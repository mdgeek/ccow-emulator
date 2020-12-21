unit ContextManagerService;

interface

uses
  CCOW_TLB, StdVcl, Classes, SysUtils, StrUtils, Variants, Windows;

type
  PParticipant = ^TParticipant;

  TParticipant = record
    contextParticipant: IContextParticipant;
    participantCoupon: Integer;
    title: WideString;
    survey: WordBool;
    wait: WordBool;
    suspended: WordBool;
  end;

  PContext = ^TContext;

  TContext = record
    participantCoupon: Integer;
    contextCoupon: Integer;
    contextItems: TStrings;
  end;

  TSurveyDecision = (Decision_Accept, Decision_ConditionallyAccept, Decision_Unknown);

  TListComponent = (ValueComponent, NameComponent, BothComponents, RawText);

  TContextException = class(Exception)
  private
    FCode: HRESULT;
  protected
    constructor Create(text: String; code: HRESULT);
  public
    property Code: HRESULT read FCode;
  end;

  TContextManagerService = class
  private
    contextManager: IContextManager;

    participants: TList;
    nextparticipantCoupon: Integer;

    currentContext: PContext;
    pendingContext: PContext;
    nextcontextCoupon: Integer;

    changed: TObject;

    function FindParticipant(participantCoupon: Integer): PParticipant;
    procedure NotifyParticipants(canceled: Boolean);
    function PollParticipants: TStrings;
    procedure TerminateAllParticipants;

    function CreateContext(participantCoupon: Integer): PContext;
    procedure DestroyContext(context: PContext);
    function GetContext(contextCoupon: Integer): PContext;
    function GetCurrentcontextCoupon: Integer;
    procedure ValidatePendingContext(contextCoupon: Integer; participantCoupon: Integer);
    procedure MergeContexts;

    function ToVarArray(items: TStrings; which: TListComponent): OleVariant;
    function FromVarArray(varArray: OleVariant): TStrings;

    procedure ClearChanged(items: TStrings);
    function ExtractItems(src: TStrings; itemName: String; onlyChanged: Boolean): TStrings;

    procedure LogActivity(participant: PParticipant; activity: String); overload;
    procedure LogActivity(participantCoupon: Integer; activity: String); overload;
    procedure LogTransaction(contextCoupon: Integer; activity: String);

    procedure Throw(text: String; code: HRESULT);

  public
    constructor Create;

    procedure Shutdown;

    procedure NotImplemented(message: String);

    procedure LogException(e: Exception);
    procedure LogInvocation(method: String);

    function CreateParticipant(contextParticipant: IDispatch;
      title: WideString; survey, wait: WordBool): Integer;
    procedure DestroyParticipant(participantCoupon: Integer);
    procedure SuspendParticipant(participantCoupon: Integer; suspend: Boolean);

    property CurrentcontextCoupon: Integer read GetCurrentContextCoupon;
    function StartContextChanges(participantCoupon: Integer): Integer;
    function EndContextChanges(contextCoupon: Integer): OleVariant;
    procedure PublishChangesDecision(contextCoupon: Integer; decision: WideString);
    procedure UndoContextChanges(contextCoupon: Integer);
    function GetItemNames(contextCoupon: Integer): OleVariant;
    function GetItemValues(participantCoupon: Integer; itemNames: OleVariant; onlyChanges: WordBool;
      contextCoupon: Integer): OleVariant;
    procedure SetItemValues(participantCoupon: Integer;
      itemNames, itemValues: OleVariant; contextCoupon: Integer);
  end;

var

  Service: TContextManagerService;

implementation

uses MainWindow;

const
  E_FAIL = HRESULT($80004005);
  E_TRANSACTION_IN_PROGRESS = HRESULT($80000209);
  E_NOT_IN_TRANSACTION = HRESULT($80000207);
  E_INVALID_TRANSACTION = HRESULT($80000211);
  E_INVALID_Integer = HRESULT($80000203);
  E_UNKNOWN_PARTICIPANT = HRESULT($8000020B);
  E_ACCEPT_NOT_POSSIBLE = HRESULT($8000020D);

var
  LOG_SEPARATOR: String;

//************************** Lifecycle **************************/

constructor TContextManagerService.Create;
begin
  LOG_SEPARATOR := StringOfChar('-', 80) + '> %d';
  LogInvocation('TContextManagerService.Create');
  participants := TList.Create;

  currentContext := nil;
  pendingContext := nil;

  changed := TObject.Create;
  contextManager := CoContextManager.Create;
end;

procedure TContextManagerService.Shutdown;
begin
  LogInvocation('TContextManagerService.Shutdown');
  TerminateAllParticipants;
  contextManager := nil;
end;

//************************** Participant **************************/

function TContextManagerService.CreateParticipant(
  contextParticipant: IDispatch; title: WideString;
  survey, wait: WordBool): Integer;
var
  participant: PParticipant;
begin
  nextParticipantCoupon := nextParticipantCoupon + 1;
  participant := New (PParticipant);
  participant^.contextParticipant := contextParticipant as IContextParticipant;
  participant^.title := title;
  participant^.survey := survey;
  participant^.wait := wait;
  participant^.suspended := False;
  participant^.participantCoupon := nextParticipantCoupon;
  participants.add(participant);
  frmMain.AddParticipant(participant);
  LogActivity(participant, 'joined the common context');
  Result := nextParticipantCoupon;
end;

procedure TContextManagerService.DestroyParticipant(participantCoupon: Integer);
var
  participant: PParticipant;
begin
  participant := FindParticipant(participantCoupon);
  LogActivity(participant, 'left the common context');
  frmMain.RemoveParticipant(participant);
  participant^.contextParticipant := nil;
  participants.Remove(participant);
end;

procedure TContextManagerService.SuspendParticipant(participantCoupon: Integer; suspend: Boolean);
var
  participant: PParticipant;
begin
  participant := FindParticipant(participantCoupon);
  participant^.suspended := suspend;
end;

function TContextManagerService.FindParticipant(participantCoupon: Integer): PParticipant;
var
  i: Integer;
  participant: PParticipant;
begin
  Result := nil;

  for i := 0 To participants.Count - 1 Do
  begin
    participant := participants[i];

    if participant^.participantCoupon = participantCoupon
    then begin
      Result := participant;
      break;
    end;
  end;

  if Result = nil
  then Throw('No participant found for coupon ' + IntToStr(participantCoupon),
    E_UNKNOWN_PARTICIPANT);
end;

procedure TContextManagerService.NotifyParticipants(canceled: Boolean);
var
  i: Integer;
  count: Integer;
  action: String;
  contextCoupon: Integer;
  participant: PParticipant;
  contextParticipant: IContextParticipant;
begin
  if canceled
  then action := 'canceled'
  else action := 'committed';

  frmMain.Log('Context change %s, notifying participants...', [action]);
  count := 0;
  contextCoupon := pendingContext^.contextCoupon;

  for i := 0 To participants.Count - 1 Do
  begin
    participant := participants[i];

    if Not(participant^.suspended)
    then begin
      count := count + 1;
      contextParticipant := participant^.contextParticipant;

      if canceled
      then contextParticipant.ContextChangesCanceled(contextCoupon)
      else contextParticipant.ContextChangesAccepted(contextCoupon);
    end;
  end;

  frmMain.Log('Notified %d out of %d participant(s)', [count, participants.Count]);
end;

function TContextManagerService.PollParticipants: TStrings;
var
  i: Integer;
  count: Integer;
  participant: PParticipant;
  contextParticipant: IContextParticipant;
  reason: WideString;
  response: WideString;
  decision: TSurveyDecision;
begin
  frmMain.Log('Polling participants...');
  Result := nil;
  count := 0;

  for i := 0 To participants.Count - 1 Do
  begin
    participant := participants[i];

    if Not(participant^.suspended) and participant^.survey
      and (participant^.participantCoupon <> pendingContext^.participantCoupon)
    then begin
      decision := Decision_Unknown;
      count := count + 1;
      contextParticipant := participant^.contextParticipant;
      LogActivity(participant, 'polled');
      response := AnsiLowerCase(contextParticipant.ContextChangesPending(pendingContext^.contextCoupon, reason));

      if response = 'accept'
      then decision := Decision_Accept
      else if response = 'conditionally_accept'
      then decision := Decision_ConditionallyAccept
      else Throw('Unknown survey response: ' + response, E_ACCEPT_NOT_POSSIBLE);

      LogActivity(participant, 'responded with: ' + reason);

      if decision = Decision_ConditionallyAccept
      then begin
        if Result = nil
        then Result := TStringList.Create;

        Result.Add(reason);
      end;
    end;
  end;

  frmMain.Log('Polled %d out of %d participant(s)', [count, participants.Count]);
end;

procedure TContextManagerService.TerminateAllParticipants;
var
  i: Integer;
  participant: PParticipant;
  contextParticipant: IContextParticipant;
begin
  frmMain.Log('Terminating all active participants...');

  for i := 0 to participants.Count - 1 do
  begin
    participant := participants[i];
    contextParticipant := participant^.contextParticipant;
    frmMain.RemoveParticipant(participant);
    participant^.contextParticipant := nil;

    Try
      contextParticipant.CommonContextTerminated;
    Except
      on E: Exception do
        LogException(E);
     end;
  end;


  participants.Clear;
end;

//************************** Context **************************/

function TContextManagerService.CreateContext(participantCoupon: Integer): PContext;
var
  context: PContext;
begin
  nextContextCoupon := nextContextCoupon + 1;
  context := New (PContext);
  context^.participantCoupon := participantCoupon;
  context^.contextCoupon := nextContextCoupon;
  context^.contextItems := TStringList.Create();
  Result := context;
end;

procedure TContextManagerService.DestroyContext(context: PContext);
begin
  if context <> nil
  then begin
    context^.contextItems.Destroy;
    context^.contextItems := nil;
  end;
end;

function TContextManagerService.GetContext(contextCoupon: Integer): PContext;
begin
  if (currentContext <> nil) and (currentContext^.contextCoupon = contextCoupon)
  then Result := currentContext
  else if (pendingContext <> nil) and (pendingContext^.contextCoupon = contextCoupon)
  then Result := pendingContext
  else Throw('Context coupon does not correspond to an active or pending context', E_INVALID_Integer);
end;

function TContextManagerService.GetCurrentcontextCoupon: Integer;
begin
  if currentContext = nil
  then Result := 0
  else Result := currentContext^.contextCoupon;

end;

function TContextManagerService.StartContextChanges(participantCoupon: Integer): Integer;
var
  participant: PParticipant;
begin
  if pendingContext <> nil
  then Throw('A context change transaction is already in progress',
    E_TRANSACTION_IN_PROGRESS);

  participant := FindParticipant(participantCoupon);

  if participant^.suspended
  then Throw('Participant is suspended', E_INVALID_TRANSACTION);

  pendingContext := CreateContext(participantCoupon);
  frmMain.pendingContext := nil;
  LogActivity(participant,
    'started context change (' + IntToStr(pendingContext^.contextCoupon) + ')');
  Result := pendingContext^.contextCoupon;
end;

function TContextManagerService.EndContextChanges(contextCoupon: Integer): OleVariant;
var
  vote: TStrings;
begin
  ValidatePendingContext(contextCoupon, -1);
  MergeContexts;
  vote := PollParticipants;
  Result := ToVarArray(vote, RawText);
end;

procedure TContextManagerService.MergeContexts;
var
  pending: TStrings;
  current: TStrings;
  subjects: TStrings;
  subject: String;
  i: Integer;

  function getSubject(item: String): String;
  var
    i: Integer;
  begin
    i := Pos('.', item);

    if i < 2
    then Result := ''
    else Result := LeftStr(item, i - 1);
  end;
begin
  if currentContext = nil
  then Exit;

  subjects := TStringList.Create;
  pending := pendingContext^.contextItems;
  current := currentContext^.contextItems;

  for i := 0 to pending.Count - 1 do
  begin
    subject := getSubject(pending.Names[i]);

    if (subject <> '') and (subjects.IndexOf(subject) = -1)
    then subjects.add(subject);
  end;

  for i := 0 to current.Count - 1 do
  begin
    subject := getSubject(current.Names[i]);

    if subjects.IndexOf(subject) = -1
    then pending.Add(current[i]);
  end;
end;

procedure TContextManagerService.PublishChangesDecision(contextCoupon: Integer; decision: WideString);
var
  accept: Boolean;
begin
  ValidatePendingContext(contextCoupon, -1);
  accept := AnsiLowerCase(decision) = 'accept';

  NotifyParticipants(Not(accept));

  if accept
  then begin
    DestroyContext(currentContext);
    currentContext := pendingContext;
    ClearChanged(currentContext^.contextItems);
    frmMain.currentContext := currentContext;
  end else begin
    DestroyContext(pendingContext);
  end;

  pendingContext := nil;
  frmMain.pendingContext := nil;
  LogTransaction(contextCoupon, 'published changes decision: ' + decision);
end;

procedure TContextManagerService.UndoContextChanges(contextCoupon: Integer);
begin
  ValidatePendingContext(contextCoupon, -1);
  LogTransaction(contextCoupon, 'undoing context changes');
  DestroyContext(pendingContext);
  pendingContext := nil;
  frmMain.pendingContext := nil;
end;

function TContextManagerService.GetItemNames(contextCoupon: Integer): OleVariant;
begin
  Result := ToVarArray(GetContext(contextCoupon)^.contextItems, NameComponent);
end;

function TContextManagerService.GetItemValues(participantCoupon: Integer;
  itemNames: OleVariant; onlyChanges: WordBool; contextCoupon: Integer): OleVariant;
var
  names: TStrings;
  itemName: String;
  items: TStrings;
  matches: TStrings;
  i: Integer;
begin
  if participantCoupon >= 0
  then LogActivity(participantCoupon, 'is retrieving item values');

  items := GetContext(contextCoupon)^.contextItems;
  matches := TStringList.Create;
  names := FromVarArray(itemNames);

  for i := 0 to names.Count - 1 do
  begin
    itemName := AnsiLowerCase(names[i]);
    matches.AddStrings(ExtractItems(items, itemName, onlyChanges));
  end;

  Result := ToVarArray(matches, BothComponents);
end;

procedure TContextManagerService.SetItemValues(participantCoupon: Integer;
  itemNames, itemValues: OleVariant; contextCoupon: Integer);
var
  i, j: Integer;
  items: TStrings;
  name: String;
  value: String;
begin
  ValidatePendingContext(contextCoupon, participantCoupon);
  LogActivity(participantCoupon, 'is setting item values');
  items := pendingContext^.contextItems;

  for i := VarArrayLowBound(itemNames, 1) to VarArrayHighBound(itemNames, 1) do begin
    name := AnsiLowerCase(itemNames[i]);
    value := itemValues[i];
    j := items.IndexOfName(name);

    if j >= 0
    then begin
      items.ValueFromIndex[j] := value;
      items.Objects[j] := Changed;
    end else begin
      items.AddObject(name + '=' + value, Changed);
    end;

    frmMain.Log('  %s=%s', [name, value]);
  end;

  frmMain.pendingContext := pendingContext;
end;

//************************** Utility **************************/

function TContextManagerService.ToVarArray(items: TStrings; which: TListComponent): OleVariant;
var
  varArray: Variant;
  i, j: Integer;
  upper: Integer;

  procedure AddItem(item: String);
  begin
    frmMain.Log('   %s', [item]);
    varArray[j] := item;
    j := j + 1;
  end;

begin
  if (items = nil) or (items.Count = 0)
  then begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Exit;
  end;

  upper := items.Count;

  if which = BothComponents
  then upper := upper * 2;

  varArray := VarArrayCreate([0, upper - 1], varOleStr);
  j := 0;

  for i := 0 to items.Count - 1 do
  begin
    Case which of
      ValueComponent: AddItem(items.ValueFromIndex[i]);
      NameComponent: AddItem(items.Names[i]);
      RawText: AddItem(items[i]);
      BothComponents:
      begin
        AddItem(items.Names[i]);
        AddItem(items.ValueFromIndex[i]);
      end;
      else Throw('Illegal string list component', E_FAIL);
    end;
  end;

  Result := varArray;
end;

function TContextManagerService.FromVarArray(varArray: OleVariant): TStrings;
var
  i: Integer;
begin
  Result := TStringList.Create;

  for i := VarArrayLowBound(varArray, 1) to VarArrayHighBound(varArray, 1) do
    Result.Add(VarToStr(varArray[i]));
end;

procedure TContextManagerService.ClearChanged(items: TStrings);
var
  i: Integer;
begin
  for i := 0 to items.Count - 1 do
    items.Objects[i] := nil;
end;

function TContextManagerService.ExtractItems(src: TStrings; itemName: String;
  onlyChanged: Boolean): TStrings;
var
  i: Integer;

  procedure AddItem(index: Integer);
  begin
    if (index >= 0) and (Not(onlyChanged) or (src.Objects[index] <> nil))
    then Result.Add(src[index]);
  end;
  
begin
  Result := TStringList.Create;

  if Not(AnsiEndsText('*', itemName))
  then begin
    AddItem(src.IndexOfName(itemName));
    Exit;
  end;

  itemName := AnsiLeftStr(itemName, Length(itemName) - 1);

  for i := 0 to src.Count - 1 do
  begin
    if AnsiStartsText(itemName, src.Names[i])
    then AddItem(i);
  end;

end;

procedure TContextManagerService.ValidatePendingContext(contextCoupon: Integer;
  participantCoupon: Integer);
begin
  if pendingContext = nil
  then Throw('No context change transaction is active', E_NOT_IN_TRANSACTION);

  if contextCoupon <> pendingContext^.contextCoupon
  then Throw('An invalid context coupon (' + IntToStr(contextCoupon)
    + ') was provided', E_INVALID_Integer);

  if (participantCoupon >= 0) and (pendingContext^.participantCoupon <> participantCoupon)
  then Throw('Participant (' + IntToStr(participantCoupon)
    + ') did not initiate this transaction (' + IntToStr(contextCoupon) + ')',
    E_INVALID_Integer);
end;

//************************** Logging **************************/

procedure TContextManagerService.LogActivity(participant: PParticipant; Activity: String);
begin
  if (participant <> nil)
  then begin
    frmMain.Log('%s(%d) %s', [participant^.title, participant^.participantCoupon, Activity]);
  end;
end;

procedure TContextManagerService.LogActivity(participantCoupon: Integer; Activity: String);
begin
  LogActivity(FindParticipant(ParticipantCoupon), Activity);
end;

procedure TContextManagerService.LogTransaction(contextCoupon: Integer; activity: String);
begin
  frmMain.Log('Transaction (%d) %s', [contextCoupon, activity]);
end;

procedure TContextManagerService.LogInvocation(method: String);
begin
  frmMain.Log(LOG_SEPARATOR, [GetCurrentThreadId]);
  frmMain.Log('Invoking method %s', [method]);
end;

procedure TContextManagerService.LogException(e: Exception);
begin
  frmMain.Log('An error occured: %s', [e.Message]);
end;

//************************** Exception Handling **************************/

procedure TContextManagerService.NotImplemented(message: String);
begin
  Throw(message, E_NOTIMPL);
end;

procedure TContextManagerService.Throw(text: String; code: HRESULT);
begin
  raise TContextException.Create(text, code);
end;

constructor TContextException.Create(text: String; code: HRESULT);
begin
  inherited Create(text);
  FCode := code;
end;

end.
