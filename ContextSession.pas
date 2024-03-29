unit ContextSession;

interface

uses
  CCOW_TLB, Classes, SysUtils, StrUtils, Variants, SessionForm,
  Common, ContextException, Logger, Types;

type
  {
    A context session.  Each context session maintains its own current and pending
    contexts and a list of participants.  A new session form is created for each
    session.
  }
  TContextSession = class
  private
    sessionId: Integer;
    sessionForm: TSessionForm;

    FLogger: TLogger;

    participants: TList;
    nextParticipantCoupon: Integer;

    currentContext: PContext;
    pendingContext: PContext;
    nextContextCoupon: Integer;

    changed: TObject;

    function FindParticipant(participantCoupon: Integer): PParticipant;
    procedure NotifyParticipants(contextCoupon: Integer; canceled: Boolean);
    function PollParticipants: TStrings;
    procedure AddParticipant(participant: PParticipant);
    function RemoveParticipant(participantCoupon: Integer): PParticipant;
    procedure TerminateAllParticipants;
    function CloneParticipantList: TList;

    function CreateContext(participantCoupon: Integer): PContext;
    procedure DestroyContext(var context: PContext);
    function GetContext(contextCoupon: Integer): PContext;
    function GetCurrentcontextCoupon: Integer;
    procedure ValidatePendingContext(contextCoupon: Integer; participantCoupon: Integer);
    procedure MergeContexts;

    procedure ClearChanged(items: TStrings);
    function ExtractItems(src: TStrings; itemName: String; onlyChanged: Boolean): TStrings;

    procedure SessionDestroy(Sender: TObject);

    procedure Throw(text: String; code: HRESULT); overload;
    procedure Throw(text: String; code: HRESULT; params: array of const); overload;

  public
    constructor Create;
    procedure SessionActivate(participantCoupon: Integer; cmToActivate: IDispatch;
      nonce, appSignature: WideString);


    property id: Integer read sessionId;

    property Logger: TLogger read FLogger;

    procedure NotImplemented(message: String);

    function CreateParticipant(contextParticipant: IDispatch;
      title: WideString; survey, wait: WordBool): Integer;
    procedure DestroyParticipant(participantCoupon: Integer);
    procedure SuspendParticipant(participantCoupon: Integer; suspend: Boolean);

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

  end;

var
  DefaultSession: TContextSession;

implementation

uses
  MainForm, ContextManager, ComObj;

var
  nextSessionId: Integer;

//************************** Lifecycle **************************/

{
  Creates a new context session.  A default session is created during
  application startup.  This is typically all that is needed.
}
constructor TContextSession.Create;
begin
  nextSessionId := nextSessionId + 1;
  sessionId := nextSessionId;
  sessionForm := frmMain.CreateSession(sessionId);
  FLogger := sessionForm.Logger;
  sessionForm.OnDestroy := Self.SessionDestroy;
  FLogger.LogStart('TContextSession.Create');
  participants := TList.Create;

  currentContext := nil;
  pendingContext := nil;

  {$IFDEF TEST}
  currentContext := CreateContext(CreateParticipant(Participant.Create('test'), 'test', false, false));
  currentContext^.contextItems.Values['test.item1'] := 'value1';
  currentContext^.contextItems.Values['test.item2'] := 'value2';
  currentContext^.contextItems.Values['test.item3'] := 'value3';
  currentContext^.contextItems.Values['test.item4'] := 'value4';
  sessionForm.CurrentContext := currentContext;
  {$ENDIF}

  changed := TObject.Create;
end;

{
  Activates a context session for the given participant.  Essentially moves
  a participant from its current session to another.
}
procedure TContextSession.SessionActivate(participantCoupon: Integer;
  cmToActivate: IDispatch; nonce, appSignature: WideString);
  var
    contextSession: TContextSession;
    participant: PParticipant;
begin
  contextSession := TContextSession(cmToActivate);
  participant := RemoveParticipant(participantCoupon);
  contextSession.AddParticipant(participant);
end;

{
  Destroys the session, terminating any active participants.
}
procedure TContextSession.SessionDestroy(Sender: TObject);
begin
  logger.LogStart('TContextSession.SessionDestroy');
  TerminateAllParticipants;
  logger.LogEnd;
  logger.LogEnd;
end;

//************************** Participant **************************/

{
  Creates a new participant record.  The record is added to the list of
  active participants.  The newly assigned participant coupon is returned.
}
function TContextSession.CreateParticipant(contextParticipant: IDispatch;
  title: WideString; survey, wait: WordBool): Integer;
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
  participant^.filter := Null;
  AddParticipant(participant);
  sessionForm.AddParticipant(participant);
  logger.LogActivity(participant, 'joined the common context');
  Result := nextParticipantCoupon;
end;

{
  Destroys the participant record, removing it from the list of active
  participants.
}
procedure TContextSession.DestroyParticipant(participantCoupon: Integer);
var
  participant: PParticipant;
begin
  participant := RemoveParticipant(participantCoupon);
  logger.LogActivity(participant, 'left the common context');
  sessionForm.RemoveParticipant(participant);
  participant^.contextParticipant := nil;
  Dispose(participant);
end;

{
  Suspends or resumes participation in the common context.
}
procedure TContextSession.SuspendParticipant(participantCoupon: Integer; suspend: Boolean);
var
  participant: PParticipant;
begin
  participant := FindParticipant(participantCoupon);
  participant^.suspended := suspend;
  sessionForm.AddParticipant(participant);
end;

{
  Returns the record of the active participant associated with the given
  participant coupon. If one is not found, an exception is raised.
}
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

{
  Notifies all active participants of the context change decision.
}
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

  Logger.Log('Context change %s, notifying participants...', [action]);
  count := 0;
  pList := CloneParticipantList;

  for i := 0 To pList.Count - 1 Do
  begin
    participant := pList[i];

    if Not(participant^.suspended)
    then begin
      count := count + 1;
      contextParticipant := participant^.contextParticipant;

      Try
        if canceled
        then begin
          logger.LogStart('IContextParticipant.ContextChangesCanceled');
          logger.LogActivity(participant, 'is being notified');
          contextParticipant.ContextChangesCanceled(contextCoupon);
        end else begin
          logger.LogStart('IContextParticipant.ContextChangesAccepted');
          logger.LogActivity(participant, 'is being notified');
          contextParticipant.ContextChangesAccepted(contextCoupon);
        end;
      Except
        on e: Exception do
          Logger.LogException(e);
      end;

      logger.LogEnd;
    end;
  end;

  logger.Log('Notified %d out of %d participant(s)', [count, pList.Count]);
  pList.Free;
end;

{
  Polls all active participants for consent to change the context.  Returns
  a list of reasons for dissent, if any, provided by participants.  If all
  participants assent, nil will be returned.
}
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
  Logger.Log('Polling participants...');
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
      logger.LogStart('IContextParticipant.ContextChangesPending');
      logger.LogActivity(participant, 'is being polled');

      Try
        response := AnsiLowerCase(contextParticipant.ContextChangesPending(pendingContext^.contextCoupon, reason));
      Except
        on e: Exception do begin
          response := e.Message;
          reason := e.Message;
        end;
      end;
      
      logger.LogEnd;

      if response = 'accept'
      then begin
         logger.LogActivity(participant, 'accepted pending change');
      end else if response = 'conditionally_accept'
      then begin
        logger.LogActivity(participant, 'conditionally accepted pending change: ' + reason);
        AddReason(reason);
      end else begin
       logger.LogActivity(participant, 'returned an unrecognized response: ' + response);
       AddReason(reason);
      end;
    end;
  end;

  Logger.Log('Polled %d out of %d participant(s)', [count, pList.Count]);
  pList.Free;
  Result := reasons;
end;

{
  Adds a participant record to the list of active participants.
}
procedure TContextSession.AddParticipant(participant: PParticipant);
begin
  participants.Add(participant);
end;

{
  Removes a participant record from the list of active participants.
}
function TContextSession.RemoveParticipant(participantCoupon: Integer): PParticipant;
var
  participant: PParticipant;
begin
  participant := FindParticipant(participantCoupon);
  participants.Remove(participant);
  Result := participant;
end;

{
  Terminates context participation for all active participants.
}
procedure TContextSession.TerminateAllParticipants;
var
  i: Integer;
  pList: TList;
  participant: PParticipant;
  contextParticipant: IContextParticipant;
begin
  Logger.Log('Terminating all active participants...');
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
        Logger.LogException(E);
     end;
  end;


  participants.Clear;
  pList.Free;
end;

{
  Returns a clone of the active participant list.  This is used for iteration
  in case the original list is modified during the iteration loop.
}
function TContextSession.CloneParticipantList: TList;
begin
  Result := TList.Create;
  Result.Assign(participants, laCopy);
end;

//************************** Context **************************/

{
  Creates a new context record for the specified participant.
}
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

{
  Destroys a context record.
}
procedure TContextSession.DestroyContext(var context: PContext);
begin
  if context <> nil
  then begin
    Dispose(context);
    context := nil;
  end;
end;

{
  Returns the context record corresponding to the specified context coupon.
  If the context coupon does not correspond to either the current or the pending
  context, and exception is raised.
}
function TContextSession.GetContext(contextCoupon: Integer): PContext;
begin
  Result := nil;
  
  if (currentContext <> nil) and (currentContext^.contextCoupon = contextCoupon)
  then Result := currentContext
  else if (pendingContext <> nil) and (pendingContext^.contextCoupon = contextCoupon)
  then Result := pendingContext
  else Throw('Context coupon [cc#%d] does not correspond to an active or pending context',
    E_INVALID_CONTEXT_COUPON, [contextCoupon]);
end;

{
  Returns the context coupon for the current context.  If a context has not yet
  been set for this session, zero is returned.
}
function TContextSession.GetCurrentcontextCoupon: Integer;
begin
  if currentContext = nil
  then Result := 0
  else Result := currentContext^.contextCoupon;

end;

{
  Initiates a context change on behalf of the specified participant.  If a
  context change transaction is already active, an exception is raised.
  Returns the coupon for the newly created pending context.
}
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
  logger.LogActivity(pendingContext, 'started context change');
  Result := pendingContext^.contextCoupon;
end;

{
  Ends the pending context change and polls active participants for their
  assent to the change.  Returns an array of responses for each dissenting
  participant.  If no participant dissents, return an empty array.
}
function TContextSession.EndContextChanges(contextCoupon: Integer): OleVariant;
var
  vote: TStrings;
begin
  ValidatePendingContext(contextCoupon, -1);
  MergeContexts;
  vote := PollParticipants;
  Result := ToVarArray(vote, RawText);
end;

{
  Backfills the pending context with items from the current context, excluding
  items belonging to subjects already present in the pending context.
}
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

{
  Commits or rolls back the pending context and notifies active participants
  of the decision.
}
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
    pendingContext := nil;
    sessionForm.currentContext := currentContext;
    sessionForm.pendingContext := nil;
  end;

  NotifyParticipants(contextCoupon, Not(accept));
  logger.LogActivity(getContext(contextCoupon), 'published changes decision: ' + decision);

  if accept
  then ClearChanged(currentContext^.contextItems)
  else DestroyContext(pendingContext);
end;

{
  Destroys the pending context.
}
procedure TContextSession.UndoContextChanges(contextCoupon: Integer);
begin
  ValidatePendingContext(contextCoupon, -1);
  logger.LogActivity(pendingContext, 'is undoing context changes');
  DestroyContext(pendingContext);
  sessionForm.pendingContext := nil;
end;

{
  Returns item names from the specified context.  If the context coupon does
  not correspond to the current or pending context, an exception is raised.
}
function TContextSession.GetItemNames(contextCoupon: Integer): OleVariant;
begin
  Result := ToVarArray(GetContext(contextCoupon)^.contextItems, NameComponent);
end;

{
  Returns item values for the specified item names from the specified context.
  If the context coupon does not correspond to the current or pending context,
  an exception is raised.  Results are returned in an array of alternating
  item names and values.
}
function TContextSession.GetItemValues(participantCoupon: Integer;
  itemNames: OleVariant; onlyChanges: WordBool; contextCoupon: Integer): OleVariant;
var
  names: TStrings;
  items: TStrings;
  matches: TStrings;
  i: Integer;
begin
  if participantCoupon >= 0
  then logger.LogActivity(FindParticipant(participantCoupon), 'is retrieving item values');

  items := GetContext(contextCoupon)^.contextItems;
  matches := TStringList.Create;
  names := FromVarArray(itemNames);

  for i := 0 to names.Count - 1 do
    matches.AddStrings(ExtractItems(items, names[i], onlyChanges));

  Result := ToVarArray(matches, BothComponents);
end;

{
  Sets item values for the specified item names from the specified context.
  If the context coupon does not correspond to the current or pending context,
  an exception is raised.
}
procedure TContextSession.SetItemValues(participantCoupon: Integer;
  itemNames, itemValues: OleVariant; contextCoupon: Integer);
var
  i, j: Integer;
  items: TStrings;
  name: String;
  value: String;
begin
  ValidatePendingContext(contextCoupon, participantCoupon);
  logger.LogActivity(pendingContext, 'is setting item values');
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

    Logger.Log('  %s', [items[j]]);
  end;

  sessionForm.pendingContext := pendingContext;
end;

{
  Validates a pending context, raising an exception if any of the following holds true:
    - No pending context exists.
    - The pending context does not correspond to the specified context coupon.
    - If a participant coupon is specified (i.e., is a positive integer) and
      does not correspond to the participant that initiated the pending context.
}
procedure TContextSession.ValidatePendingContext(contextCoupon: Integer; participantCoupon: Integer);
begin
  if pendingContext = nil
  then Throw('No context change transaction is active', E_NOT_IN_TRANSACTION);

  if contextCoupon <> pendingContext^.contextCoupon
  then Throw('An invalid context coupon [cc#%d] was provided', E_INVALID_CONTEXT_COUPON, [contextCoupon]);

  if (participantCoupon > -1) and (participantCoupon <> pendingContext^.participant.participantCoupon)
  then Throw('Participant [pc#%d] did not initiate this transaction [cc#%d]',
    E_INVALID_CONTEXT_COUPON, [participantCoupon, contextCoupon]);

  Logger.Log('Pending context coupon [cc#%d] is valid', [contextCoupon]);
end;

//************************** Context Filters **************************/

{
  Returns subject names from the current filter, raising an exception if no
  filter has been set.
}
function TContextSession.GetSubjectsOfInterest(participantCoupon: Integer): OleVariant;
begin
  Result := FindParticipant(participantCoupon)^.filter;

  if Result = Null
  then Throw('A filter has not been set', E_FILTER_NOT_SET);
end;

{
  Clears the current filter.
}
procedure TContextSession.ClearFilter(participantCoupon: Integer);
begin
  FindParticipant(participantCoupon)^.filter := Null;
end;

{
  Sets subject names for the participant's filter.
}
procedure TContextSession.SetSubjectsOfInterest(participantCoupon: Integer; subjectNames: OleVariant);
begin
  FindParticipant(participantCoupon)^.filter := subjectNames;
end;

//************************** Utility **************************/

{
  Clears the changed flag from all items in a string list.
}
procedure TContextSession.ClearChanged(items: TStrings);
var
  i: Integer;
begin
  for i := 0 to items.Count - 1 do
    items.Objects[i] := nil;
end;

{
  Returns a list of all items in a string list that match the specified item
  name.  The item name may contain a wildcard (i.e., terminated with an
  asterisk).  If 'onlyChanged' is true, then only matching items that are
  marked as having changed are included.
}
function TContextSession.ExtractItems(src: TStrings; itemName: String; onlyChanged: Boolean): TStrings;
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
    AddItem(IndexOfName(itemName, src));
    Exit;
  end;

  itemName := AnsiLeftStr(itemName, Length(itemName) - 1);

  for i := 0 to src.Count - 1 do
  begin
    if AnsiStartsText(itemName, src.Names[i])
    then AddItem(i);
  end;

end;

//************************** Exception Handling **************************/

{
  Raises a 'not implemented' exception.
}
procedure TContextSession.NotImplemented(message: String);
begin
  Throw(message, E_NOTIMPL);
end;

{
  Raises an exception with the specified text and result code.
}
procedure TContextSession.Throw(text: String; code: HRESULT);
begin
  raise TContextException.Create(text, code);
end;

{
  Raises an exception with the specified parameterized text and result code.
}
procedure TContextSession.Throw(text: String; code: HRESULT; params: array of const);
begin
  Throw(Format(text, params), code);
end;

end.
