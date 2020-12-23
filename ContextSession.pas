unit ContextSession;

interface

uses
  CCOW_TLB, StdVcl, Classes, SysUtils, StrUtils, Variants, Windows, SessionForm, Common;

type
  TListComponent = (ValueComponent, NameComponent, BothComponents, RawText);

  TContextException = class(Exception)
  private
    FCode: HRESULT;
  protected
    constructor Create(text: String; code: HRESULT);
  public
    property Code: HRESULT read FCode;
  end;

  TContextSession = class
  private
    sessionId: Integer;
    sessionForm: TSessionForm;

    participants: TList;
    nextparticipantCoupon: Integer;

    currentContext: PContext;
    pendingContext: PContext;
    nextContextCoupon: Integer;

    changed: TObject;

    function FindParticipant(participantCoupon: Integer): PParticipant;
    procedure NotifyParticipants(contextCoupon: Integer; canceled: Boolean);
    function PollParticipants: TStrings;
    procedure TerminateAllParticipants;
    function CloneParticipantList: TList;

    function CreateContext(participantCoupon: Integer): PContext;
    procedure DestroyContext(var context: PContext);
    function GetContext(contextCoupon: Integer): PContext;
    function GetCurrentcontextCoupon: Integer;
    procedure ValidatePendingContext(contextCoupon: Integer; participantCoupon: Integer);
    procedure MergeContexts;

    function ToVarArray(items: TStrings; which: TListComponent): OleVariant;
    function FromVarArray(varArray: OleVariant): TStrings;

    procedure ClearChanged(items: TStrings);
    function ExtractItems(src: TStrings; itemName: String; onlyChanged: Boolean): TStrings;

    procedure SessionDestroy(Sender: TObject);

    procedure Throw(text: String; code: HRESULT); overload;
    procedure Throw(text: String; code: HRESULT; params: array of const); overload;

  public
    constructor Create;

    property id: Integer read sessionId;

    procedure NotImplemented(message: String);

    procedure LogException(e: Exception);
    procedure LogInvocation(method: String);

    function CreateParticipant(contextParticipant: IDispatch;
      title: WideString; survey, wait: WordBool): Integer;
    procedure DestroyParticipant(participantCoupon: Integer);
    procedure SuspendParticipant(participantCoupon: Integer; suspend: Boolean);
    procedure AddParticipant(participant: PParticipant);
    function RemoveParticipant(participantCoupon: Integer): PParticipant;

    property CurrentContextCoupon: Integer read GetCurrentContextCoupon;
    function StartContextChanges(participantCoupon: Integer): Integer;
    function EndContextChanges(contextCoupon: Integer): OleVariant;
    procedure PublishChangesDecision(contextCoupon: Integer; decision: WideString);
    procedure UndoContextChanges(contextCoupon: Integer);

    function GetItemNames(contextCoupon: Integer): OleVariant;
    function GetItemValues(participantCoupon: Integer; itemNames: OleVariant; onlyChanges: WordBool;
      contextCoupon: Integer): OleVariant;
    procedure SetItemValues(participantCoupon: Integer;
      itemNames, itemValues: OleVariant; contextCoupon: Integer);

    function GetSubjectsOfInterest(participantCoupon: Integer): OleVariant;
    procedure ClearFilter(participantCoupon: Integer);
    procedure SetSubjectsOfInterest(participantCoupon: Integer; subjectNames: OleVariant);

    procedure Log(text: String); overload;
    procedure Log(text: String; params: array of const); overload;
    procedure LogActivity(participant: PParticipant; activity: String); overload;
    procedure LogActivity(participantCoupon: Integer; activity: String); overload;
    procedure LogActivity(context: PContext; activity: String); overload;

  end;

var
  DefaultSession: TContextSession;

implementation

uses MainForm;

const
  E_FAIL = HRESULT($80004005);
  E_TRANSACTION_IN_PROGRESS = HRESULT($80000209);
  E_NOT_IN_TRANSACTION = HRESULT($80000207);
  E_INVALID_TRANSACTION = HRESULT($80000211);
  E_INVALID_CONTEXT_COUPON = HRESULT($80000203);
  E_UNKNOWN_PARTICIPANT = HRESULT($8000020B);
  E_ACCEPT_NOT_POSSIBLE = HRESULT($8000020D);
  E_FILTER_NOT_SET = HRESULT($80000225);

  LOG_SEPARATOR = '------------------------------------------------------------';

var
  nextSessionId: Integer;

//************************** Lifecycle **************************/

constructor TContextSession.Create;
begin
  nextSessionId := nextSessionId + 1;
  sessionId := nextSessionId;
  sessionForm := frmMain.CreateSession(sessionId);
  sessionForm.OnDestroy := Self.SessionDestroy;
  LogInvocation('TContextSession.Create');
  participants := TList.Create;

  currentContext := nil;
  pendingContext := nil;

  changed := TObject.Create;
end;

procedure TContextSession.SessionDestroy(Sender: TObject);
begin
  LogInvocation('TContextSession.SessionDestroy');
  TerminateAllParticipants;
end;

//************************** Participant **************************/

function TContextSession.CreateParticipant(
  contextParticipant: IDispatch; title: WideString; survey, wait: WordBool): Integer;
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
  sessionForm.AddParticipant(participant);
  LogActivity(participant, 'joined the common context');
  Result := nextParticipantCoupon;
end;

procedure TContextSession.DestroyParticipant(participantCoupon: Integer);
var
  participant: PParticipant;
begin
  participant := FindParticipant(participantCoupon);
  LogActivity(participant, 'left the common context');
  sessionForm.RemoveParticipant(participant);
  participant^.contextParticipant := nil;
  participants.Remove(participant);
end;

procedure TContextSession.SuspendParticipant(participantCoupon: Integer; suspend: Boolean);
var
  participant: PParticipant;
begin
  participant := FindParticipant(participantCoupon);
  participant^.suspended := suspend;
end;

function TContextSession.FindParticipant(participantCoupon: Integer): PParticipant;
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
  then Throw('No participant found for coupon [pc#%d]', E_UNKNOWN_PARTICIPANT, [participantCoupon]);
end;

procedure TContextSession.NotifyParticipants(contextCoupon: Integer; canceled: Boolean);
var
  i: Integer;
  count: Integer;
  action: String;
  pList: TList;
  participant: PParticipant;
  contextParticipant: IContextParticipant;
begin
  if canceled
  then action := 'canceled'
  else action := 'committed';

  Log(LOG_SEPARATOR);
  Log('Context change %s, notifying participants...', [action]);
  count := 0;
  pList := CloneParticipantList;

  for i := 0 To pList.Count - 1 Do
  begin
    participant := pList[i];

    if Not(participant^.suspended)
    then begin
      count := count + 1;
      contextParticipant := participant^.contextParticipant;
      LogActivity(participant, 'is being notified');

      if canceled
      then contextParticipant.ContextChangesCanceled(contextCoupon)
      else contextParticipant.ContextChangesAccepted(contextCoupon);
    end;
  end;

  Log(LOG_SEPARATOR);
  Log('Notified %d out of %d participant(s)', [count, pList.Count]);
  Log(LOG_SEPARATOR);
  pList.Free;
end;

function TContextSession.PollParticipants: TStrings;
var
  i: Integer;
  count: Integer;
  pList: TList;
  participant: PParticipant;
  contextParticipant: IContextParticipant;
  reason: WideString;
  response: WideString;
  reasons: TStrings;

  procedure AddReason(reason: String);
  begin
    if reasons = nil
    then reasons := TStringList.Create;

    reasons.Add(reason);
  end;

begin
  Log(LOG_SEPARATOR);
  Log('Polling participants...');
  count := 0;
  pList := CloneParticipantList;

  for i := 0 To pList.Count - 1 Do
  begin
    participant := pList[i];

    if Not(participant^.suspended) and participant^.survey
      and (participant <> pendingContext^.participant)
    then begin
      count := count + 1;
      contextParticipant := participant^.contextParticipant;
      LogActivity(participant, 'is being polled');
      response := AnsiLowerCase(contextParticipant.ContextChangesPending(pendingContext^.contextCoupon, reason));

      if response = 'accept'
      then begin
         LogActivity(participant, 'accepted pending change');
      end else if response = 'conditionally_accept'
      then begin
        LogActivity(participant, 'conditionally accepted pending change: ' + reason);
        AddReason(reason);
      end else begin
       LogActivity(participant, 'returned an unrecognized response: ' + response);
       AddReason('unrecognized response');
      end;
    end;
  end;

  Log(LOG_SEPARATOR);
  Log('Polled %d out of %d participant(s)', [count, pList.Count]);
  Log(LOG_SEPARATOR);
  pList.Free;
  Result := reasons;
end;

procedure TContextSession.AddParticipant(participant: PParticipant);
begin
  participants.Add(participant);
end;

function TContextSession.RemoveParticipant(participantCoupon: Integer): PParticipant;
var
  participant: PParticipant;
begin
  participant := FindParticipant(participantCoupon);
  participants.Remove(participant);
  Result := participant;
end;

procedure TContextSession.TerminateAllParticipants;
var
  i: Integer;
  pList: TList;
  participant: PParticipant;
  contextParticipant: IContextParticipant;
begin
  Log('Terminating all active participants...');
  pList := CloneParticipantList;

  for i := 0 to pList.Count - 1 do
  begin
    participant := pList[i];
    contextParticipant := participant^.contextParticipant;
    sessionForm.RemoveParticipant(participant);
    participant^.contextParticipant := nil;

    Try
      contextParticipant.CommonContextTerminated;
    Except
      on E: Exception do
        LogException(E);
     end;
  end;


  participants.Clear;
  pList.Free;
end;

function TContextSession.CloneParticipantList: TList;
begin
  Result := TList.Create;
  Result.Assign(participants, laCopy);
end;

//************************** Context **************************/

function TContextSession.CreateContext(participantCoupon: Integer): PContext;
var
  context: PContext;
  participant: PParticipant;
begin
  participant := FindParticipant(participantCoupon);
  nextContextCoupon := nextContextCoupon + 1;
  context := New (PContext);
  context^.participant := participant;
  context^.contextCoupon := nextContextCoupon;
  context^.contextItems := TStringList.Create();
  Result := context;
end;

procedure TContextSession.DestroyContext(var context: PContext);
begin
  if context <> nil
  then begin
    Dispose(context);
    context := nil;
  end;
end;

function TContextSession.GetContext(contextCoupon: Integer): PContext;
begin
  if (currentContext <> nil) and (currentContext^.contextCoupon = contextCoupon)
  then Result := currentContext
  else if (pendingContext <> nil) and (pendingContext^.contextCoupon = contextCoupon)
  then Result := pendingContext
  else Throw('Context coupon [cc#%d] does not correspond to an active or pending context',
    E_INVALID_CONTEXT_COUPON, [contextCoupon]);
end;

function TContextSession.GetCurrentcontextCoupon: Integer;
begin
  if currentContext = nil
  then Result := 0
  else Result := currentContext^.contextCoupon;

end;

function TContextSession.StartContextChanges(participantCoupon: Integer): Integer;
var
  participant: PParticipant;
begin
  if pendingContext <> nil
  then Throw('A context change transaction is already in progress', E_TRANSACTION_IN_PROGRESS);

  participant := FindParticipant(participantCoupon);

  if participant^.suspended
  then Throw('Participant is suspended', E_INVALID_TRANSACTION);

  pendingContext := CreateContext(participantCoupon);
  sessionForm.pendingContext := nil;
  LogActivity(pendingContext, 'started context change');
  Result := pendingContext^.contextCoupon;
end;

function TContextSession.EndContextChanges(contextCoupon: Integer): OleVariant;
var
  vote: TStrings;
begin
  ValidatePendingContext(contextCoupon, -1);
  MergeContexts;
  vote := PollParticipants;
  LogActivity(pendingContext, 'ended context change');
  Result := ToVarArray(vote, RawText);
end;

procedure TContextSession.MergeContexts;
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

procedure TContextSession.PublishChangesDecision(contextCoupon: Integer; decision: WideString);
var
  accept: Boolean;
begin
  ValidatePendingContext(contextCoupon, -1);
  accept := AnsiLowerCase(decision) = 'accept';

  if accept
  then begin
    DestroyContext(currentContext);
    currentContext := pendingContext;
    sessionForm.currentContext := currentContext;
  end;

  sessionForm.pendingContext := nil;
  NotifyParticipants(contextCoupon, Not(accept));
  LogActivity(pendingContext, 'published changes decision: ' + decision);

  if accept
  then ClearChanged(currentContext^.contextItems)
  else DestroyContext(pendingContext);
end;

procedure TContextSession.UndoContextChanges(contextCoupon: Integer);
begin
  ValidatePendingContext(contextCoupon, -1);
  LogActivity(pendingContext, 'is undoing context changes');
  DestroyContext(pendingContext);
  sessionForm.pendingContext := nil;
end;

function TContextSession.GetItemNames(contextCoupon: Integer): OleVariant;
begin
  Result := ToVarArray(GetContext(contextCoupon)^.contextItems, NameComponent);
end;

function TContextSession.GetItemValues(participantCoupon: Integer;
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

procedure TContextSession.SetItemValues(participantCoupon: Integer;
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
      j := items.AddObject(name + '=' + value, Changed);
    end;

    Log('  %s', [items[j]]);
  end;

  sessionForm.pendingContext := pendingContext;
end;

procedure TContextSession.ValidatePendingContext(contextCoupon: Integer; participantCoupon: Integer);
begin
  if pendingContext = nil
  then Throw('No context change transaction is active', E_NOT_IN_TRANSACTION);

  if contextCoupon <> pendingContext^.contextCoupon
  then Throw('An invalid context coupon [cc#%d] was provided', E_INVALID_CONTEXT_COUPON, [contextCoupon]);

  if (participantCoupon > -1) and (participantCoupon <> pendingContext^.participant.participantCoupon)
  then Throw('Participant [pc#%d] did not initiate this transaction [cc#%d]',
    E_INVALID_CONTEXT_COUPON, [participantCoupon, contextCoupon]);
end;

//************************** Context Filters **************************/

function TContextSession.GetSubjectsOfInterest(
  participantCoupon: Integer): OleVariant;
begin
  Result := FindParticipant(participantCoupon)^.filter;

  if Result = Null
  then Throw('A filter has not been set', E_FILTER_NOT_SET);
end;

procedure TContextSession.ClearFilter(participantCoupon: Integer);
begin
  FindParticipant(participantCoupon)^.filter := Null;
end;

procedure TContextSession.SetSubjectsOfInterest(participantCoupon: Integer;
  subjectNames: OleVariant);
begin
  FindParticipant(participantCoupon)^.filter := subjectNames;
end;

//************************** Utility **************************/

function TContextSession.ToVarArray(items: TStrings; which: TListComponent): OleVariant;
var
  varArray: Variant;
  i, j: Integer;
  upper: Integer;

  procedure AddItem(item: String);
  begin
    Log('   %s', [item]);
    varArray[j] := item;
    j := j + 1;
  end;

  procedure AddPair(name: String; value: String);
  begin
    Log('   %s=%s', [name, value]);
    varArray[j] := name;
    j := j + 1;
    varArray[j] := value;
    j := j + 1;
  end;

begin
  if (items = nil) or (items.Count = 0)
  then begin
    Result := VarArrayCreate([0, -1], varOleStr);
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
      BothComponents: AddPair(items.Names[i], items.ValueFromIndex[i]);
      else Throw('Illegal string list component', E_FAIL);
    end;
  end;

  Result := varArray;
end;

function TContextSession.FromVarArray(varArray: OleVariant): TStrings;
var
  i: Integer;
begin
  Result := TStringList.Create;

  for i := VarArrayLowBound(varArray, 1) to VarArrayHighBound(varArray, 1) do
    Result.Add(VarToStr(varArray[i]));
end;

procedure TContextSession.ClearChanged(items: TStrings);
var
  i: Integer;
begin
  for i := 0 to items.Count - 1 do
    items.Objects[i] := nil;
end;

function TContextSession.ExtractItems(src: TStrings; itemName: String;
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

//************************** Logging **************************/

procedure TContextSession.Log(text: String);
begin
  sessionForm.Log(text);
end;

procedure TContextSession.Log(text: String; params: array of const);
begin
  sessionForm.Log(text, params);
end;

procedure TContextSession.LogActivity(participant: PParticipant; activity: String);
begin
  if (participant <> nil)
  then begin
    Log('%s [pc#%d] %s', [participant^.title, participant^.participantCoupon, activity]);
  end;
end;

procedure TContextSession.LogActivity(participantCoupon: Integer; activity: String);
begin
  LogActivity(FindParticipant(ParticipantCoupon), activity);
end;

procedure TContextSession.LogActivity(context: PContext; activity: String);
var
  participant: PParticipant;
begin
  participant := context^.participant;
  Log('%s [pc#%d] %s [cc#%d]', [participant^.title, participant^.participantCoupon,
    activity, context^.contextCoupon]);
end;

procedure TContextSession.LogInvocation(method: String);
begin
  Log(LOG_SEPARATOR);
  Log('Invoking method %s [thread#%d]', [method, GetCurrentThreadId]);
end;

procedure TContextSession.LogException(e: Exception);
begin
  Log('An error occured: %s', [e.Message]);
end;

//************************** Exception Handling **************************/

procedure TContextSession.NotImplemented(message: String);
begin
  Throw(message, E_NOTIMPL);
end;

procedure TContextSession.Throw(text: String; code: HRESULT);
begin
  raise TContextException.Create(text, code);
end;

procedure TContextSession.Throw(text: String; code: HRESULT; params: array of const);
begin
  Throw(Format(text, params), code);
end;

constructor TContextException.Create(text: String; code: HRESULT);
begin
  inherited Create(text);
  FCode := code;
end;

end.
