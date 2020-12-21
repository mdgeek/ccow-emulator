unit ContextManager;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, CCOW_TLB, Classes, SysUtils, StdVcl, ContextSession;

type
  TContextManager = class(TAutoObject, IContextManager, IContextData,
    IContextAction, ISecureBinding, ISecureContextData, IContextFilter, IContextSession)

  private
    session: TContextSession;

  protected

    //************************** IContextManager **************************/

    function IContextManager.EndContextChanges = IContextManager_EndContextChanges;
    function IContextManager_EndContextChanges(contextCoupon: Integer;
      var someBusy: WordBool): OleVariant; safecall;

    function IContextManager.Get_MostRecentContextCoupon = IContextManager_Get_MostRecentContextCoupon;
    function IContextManager_Get_MostRecentcontextCoupon: Integer; safecall;

    function IContextManager.JoinCommonContext = IContextManager_JoinCommonContext;
    function IContextManager_JoinCommonContext(const contextParticipant: IDispatch;
      const sApplicationTitle: WideString; survey,
      wait: WordBool): Integer; safecall;

    function IContextManager.StartContextChanges = IContextManager_StartContextChanges;
    function IContextManager_StartContextChanges(participantCoupon: Integer): Integer;
      safecall;

    procedure IContextManager.LeaveCommonContext = IContextManager_LeaveCommonContext;
    procedure IContextManager_LeaveCommonContext(participantCoupon: Integer); safecall;

    procedure IContextManager.PublishChangesDecision = IContextManager_PublishChangesDecision;
    procedure IContextManager_PublishChangesDecision(contextCoupon: Integer;
      const decision: WideString); safecall;

    procedure IContextManager.ResumeParticipation = IContextManager_ResumeParticipation;
    procedure IContextManager_ResumeParticipation(participantCoupon: Integer; wait: WordBool); safecall;

    procedure IContextManager.SuspendParticipation = IContextManager_SuspendParticipation;
    procedure IContextManager_SuspendParticipation(participantCoupon: Integer); safecall;

    procedure IContextManager.UndoContextChanges = IContextManager_UndoContextChanges;
    procedure IContextManager_UndoContextChanges(contextCoupon: Integer); safecall;

    //************************** IContextData **************************/

    function IContextData.GetItemNames = IContextData_GetItemNames;
    function IContextData_GetItemNames(contextCoupon: Integer): OleVariant; safecall;

    function IContextData.GetItemValues = IContextData_GetItemValues;
    function IContextData_GetItemValues(itemNames: OleVariant; onlyChanges: WordBool;
      contextCoupon: Integer): OleVariant; safecall;

    procedure IContextData.DeleteItems = IContextData_DeleteItems;
    procedure IContextData_DeleteItems(participantCoupon: Integer; names: OleVariant;
      contextCoupon: Integer); safecall;

    procedure IContextData.SetItemValues = IContextData_SetItemValues;
    procedure IContextData_SetItemValues(participantCoupon: Integer; itemNames,
      itemValues: OleVariant; contextCoupon: Integer); safecall;

    //************************** ISecureBinding **************************/

    function ISecureBinding.FinalizeBinding = ISecureBinding_FinalizeBinding;
    function ISecureBinding_FinalizeBinding(bindeeCoupon: Integer; const bindeePublicKey, mac: WideString): OleVariant; safecall;

    function ISecureBinding.InitializeBinding = ISecureBinding_InitializeBinding;
    function ISecureBinding_InitializeBinding(bindeeCoupon: Integer; propertyNames, propertyValues: OleVariant;
      var binderPublicKey: WideString): WideString; safecall;

    //************************** ISecureContextData **************************/

    function ISecureContextData.GetItemNames = ISecureContextData_GetItemNames;
    function ISecureContextData_GetItemNames(contextCoupon: Integer): OleVariant; safecall;

    function ISecureContextData.GetItemValues = ISecureContextData_GetItemValues;
    function ISecureContextData_GetItemValues(participantCoupon: Integer;
      itemNames: OleVariant; onlyChanges: WordBool; contextCoupon: Integer;
      const appSignature: WideString; var managerSignature: WideString): OleVariant; safecall;

    procedure ISecureContextData.SetItemValues = ISecureContextData_SetItemValues;
    procedure ISecureContextData_SetItemValues(participantCoupon: Integer;
      itemNames, itemValues: OleVariant; contextCoupon: Integer;
      const appSignature: WideString); safecall;

    //************************** IContextAction **************************/

    function IContextAction.Perform = IContextAction_Perform;
    function IContextAction_Perform(participantCoupon: Integer;
      itemNames, itemValues: OleVariant; const appSignature: WideString;
      var actionCoupon: Integer; var outItemNames,
      outItemValues: OleVariant): WideString; safecall;

    //************************** IContextFilter **************************/

    function IContextFilter.GetSubjectsOfInterest = IContextFilter_GetSubjectsOfInterest;
    function IContextFilter_GetSubjectsOfInterest(participantCoupon: Integer): OleVariant; safecall;

    procedure IContextFilter.ClearFilter = IContextFilter_ClearFilter;
    procedure IContextFilter_ClearFilter(participantCoupon: Integer); safecall;

    procedure IContextFilter.SetSubjectsOfInterest = IContextFilter_SetSubjectsOfInterest;
    procedure IContextFilter_SetSubjectsOfInterest(participantCoupon: Integer; subjectNames: OleVariant); safecall;

    //************************** IContextSession **************************/

    function IContextSession.Create = IContextSession_Create;
    function IContextSession_Create: IDispatch; safecall;

    procedure IContextSession.Activate = IContextSession_Activate;
    procedure IContextSession_Activate(participantCoupon: Integer;
      const cmToActivate: IDispatch; const nonce, appSignature: WideString); safecall;


  public
    procedure Initialize; override;
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HRESULT; override;

  end;

implementation

uses ComServ, Common, Variants;

procedure TContextManager.Initialize;
begin
  inherited;
  session := DefaultSession;
end;

function TContextManager.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HRESULT;
begin
  if ExceptObject is Exception
  then session.LogException(Exception(ExceptObject));

  if ExceptObject is TContextException
  then Result := TContextException(ExceptObject).Code
  else Result := inherited SafeCallException(ExceptObject, ExceptAddr);
end;

//************************** IContextManager **************************/

function TContextManager.IContextManager_JoinCommonContext(
  const contextParticipant: IDispatch; const sApplicationTitle: WideString;
  survey, wait: WordBool): Integer;
begin
  session.LogInvocation('IContextManager.JoinCommonContext');
  Result := session.CreateParticipant(contextParticipant,
    sApplicationTitle, survey, wait);
end;

procedure TContextManager.IContextManager_LeaveCommonContext(participantCoupon: Integer);
begin
  session.LogInvocation('IContextManager.LeaveCommonContext');
  session.DestroyParticipant(participantCoupon);
end;

procedure TContextManager.IContextManager_ResumeParticipation(participantCoupon: Integer;
  wait: WordBool);
begin
  session.LogInvocation('IContextManager.ResumeParticipation');
  session.SuspendParticipant(participantCoupon, False);
end;

procedure TContextManager.IContextManager_SuspendParticipation(participantCoupon: Integer);
begin
  session.LogInvocation('IContextManager.SuspendParticipation');
  session.SuspendParticipant(participantCoupon, True);
end;

function TContextManager.IContextManager_Get_MostRecentcontextCoupon: Integer;
begin
  session.LogInvocation('IContextManager.Get_MostRecentContextCoupon');
  Result := session.CurrentContextCoupon;
  session.Log('Returned: ' + IntToStr(Result));
end;

function TContextManager.IContextManager_StartContextChanges(participantCoupon: Integer): Integer;
begin
  session.LogInvocation('IContextManager.StartContextChanges');
  Result := session.StartContextChanges(participantCoupon);
end;

function TContextManager.IContextManager_EndContextChanges(contextCoupon: Integer;
  var someBusy: WordBool): OleVariant;
begin
  session.LogInvocation('IContextManager.EndContextChanges');
  Result := session.EndContextChanges(contextCoupon);
end;

procedure TContextManager.IContextManager_PublishChangesDecision(contextCoupon: Integer;
  const decision: WideString);
begin
  session.LogInvocation('IContextManager.PublishChangesDecision');
  session.PublishChangesDecision(contextCoupon, decision);
end;

procedure TContextManager.IContextManager_UndoContextChanges(contextCoupon: Integer);
begin
  session.LogInvocation('IContextManager.UndoContextChanges');
  session.UndoContextChanges(contextCoupon);
end;

//************************** IContextData **************************/

function TContextManager.IContextData_GetItemNames(contextCoupon: Integer): OleVariant;
begin
  session.LogInvocation('IContextData.GetItemNames');
  Result := session.GetItemNames(contextCoupon);
end;

function TContextManager.IContextData_GetItemValues(itemNames: OleVariant;
  onlyChanges: WordBool; contextCoupon: Integer): OleVariant;
begin
  session.LogInvocation('IContextData.GetItemValues');
  Result := session.GetItemValues(-1, itemNames, onlyChanges, contextCoupon);
end;

procedure TContextManager.IContextData_DeleteItems(participantCoupon: Integer;
  names: OleVariant; contextCoupon: Integer);
begin
  session.LogInvocation('IContextData.DeleteItems');
  session.NotImplemented('IContextData.DeleteItems is deprecated');
end;

procedure TContextManager.IContextData_SetItemValues(participantCoupon: Integer;
  itemNames, itemValues: OleVariant; contextCoupon: Integer);
begin
  session.LogInvocation('IContextData.SetItemValues');
  session.SetItemValues(participantCoupon, itemNames, itemValues, contextCoupon);
end;

//************************** ISecureContextData **************************/

function TContextManager.ISecureContextData_GetItemNames(
  contextCoupon: Integer): OleVariant;
begin
  session.LogInvocation('ISecureContextData.GetItemNames');
  Result := session.GetItemNames(contextCoupon);
end;

function TContextManager.ISecureContextData_GetItemValues(
  participantCoupon: Integer; itemNames: OleVariant; onlyChanges: WordBool;
  contextCoupon: Integer; const appSignature: WideString;
  var managerSignature: WideString): OleVariant;
begin
  session.LogInvocation('ISecureContextData.GetItemValues');
  Result := session.GetItemValues(participantCoupon, itemNames, onlyChanges, contextCoupon);
end;

procedure TContextManager.ISecureContextData_SetItemValues(
  participantCoupon: Integer; itemNames, itemValues: OleVariant;
  contextCoupon: Integer; const appSignature: WideString);
begin
  session.LogInvocation('ISecureContextData.SetItemValues');
  session.SetItemValues(participantCoupon, itemNames, itemValues, contextCoupon);
end;

//************************** ISecureBinding **************************/

function TContextManager.ISecureBinding_FinalizeBinding(bindeeCoupon: Integer;
  const bindeePublicKey, mac: WideString): OleVariant;
begin
  session.LogInvocation('ISecureBinding.FinalizeBinding');
end;

function TContextManager.ISecureBinding_InitializeBinding(bindeeCoupon: Integer;
  propertyNames, propertyValues: OleVariant;
  var binderPublicKey: WideString): WideString;
begin
  session.LogInvocation('ISecureBinding.InitializeBinding');
end;

//************************** IContextAction **************************/

function TContextManager.IContextAction_Perform(participantCoupon: Integer; itemNames,
  itemValues: OleVariant; const appSignature: WideString;
  var actionCoupon: Integer; var outItemNames,
  outItemValues: OleVariant): WideString;
begin
  session.LogInvocation('IContextAction.Perform');
  session.NotImplemented('IContextAction.Perform is not implemented');
end;

//************************** IContextFilter **************************/

function TContextManager.IContextFilter_GetSubjectsOfInterest(
  participantCoupon: Integer): OleVariant;
var
  participant: PParticipant;
begin
  session.LogInvocation('IContextFilter.GetSubjectsOfInterest');
  participant := session.FindParticipant(participantCoupon);
  Result := participant^.filter;
end;

procedure TContextManager.IContextFilter_ClearFilter(participantCoupon: Integer);
var
  participant: PParticipant;
begin
  session.LogInvocation('IContextFilter.ClearFilter');
  participant := session.FindParticipant(participantCoupon);
  participant^.filter := Null;
end;

procedure TContextManager.IContextFilter_SetSubjectsOfInterest(participantCoupon: Integer;
  subjectNames: OleVariant);
var
  participant: PParticipant;
begin
  session.LogInvocation('IContextFilter.SetSubjectsOfInterest');
  participant := session.FindParticipant(participantCoupon);
  participant^.filter := subjectNames;
end;

//************************** IContextSession **************************/

function TContextManager.IContextSession_Create: IDispatch;
var
  contextManager: IContextManager;
begin
  session.LogInvocation('IContextSession.Create');
  contextManager := CoContextManager.Create;
  TContextManager(contextManager).session := TContextSession.Create;
  Result := contextManager;
end;

procedure TContextManager.IContextSession_Activate(
  participantCoupon: Integer; const cmToActivate: IDispatch; const nonce,
  appSignature: WideString);
var
  newSession: TContextSession;
  participant: PParticipant;
begin
  session.LogInvocation('IContextSession.Activate');
  newSession := TContextManager(cmToActivate).session;
  participant := session.RemoveParticipant(participantCoupon);
  newSession.AddParticipant(participant);
end;

initialization
  TAutoObjectFactory.Create(ComServer, TContextManager, Class_ContextManager,
    ciMultiInstance, tmApartment);
end.
