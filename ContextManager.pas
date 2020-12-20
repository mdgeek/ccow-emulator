unit ContextManager;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, CCOW_TLB, Classes, SysUtils, StdVcl, ContextManagerService;

type
  TContextManager = class(TAutoObject, IContextManager, IContextData, IContextAction, ISecureBinding, ISecureContextData, IContextFilter)

  public
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HRESULT; override;

  protected

    //************* IContextManager *************//

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

    //************* IContextData *************//

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

    //************* ISecureBinding *************//

    function ISecureBinding.FinalizeBinding = ISecureBinding_FinalizeBinding;
    function ISecureBinding_FinalizeBinding(bindeeCoupon: Integer; const bindeePublicKey, mac: WideString): OleVariant; safecall;

    function ISecureBinding.InitializeBinding = ISecureBinding_InitializeBinding;
    function ISecureBinding_InitializeBinding(bindeeCoupon: Integer; propertyNames, propertyValues: OleVariant;
      var binderPublicKey: WideString): WideString; safecall;

    //************* ISecureContextData *************//

    function ISecureContextData.GetItemNames = ISecureContextData_GetItemNames;
    function ISecureContextData_GetItemNames(contextCoupon: Integer): OleVariant; safecall;

    function ISecureContextData.GetItemValues = ISecureContextData_GetItemValues;
    function ISecureContextData_GetItemValues(participantCoupon: Integer;
      names: OleVariant; onlyChanges: WordBool; contextCoupon: Integer;
      const appSignature: WideString; var managerSignature: WideString): OleVariant; safecall;

    procedure ISecureContextData.SetItemValues = ISecureContextData_SetItemValues;
    procedure ISecureContextData_SetItemValues(participantCoupon: Integer;
      itemNames, itemValues: OleVariant; contextCoupon: Integer;
      const appSignature: WideString); safecall;

    //************* IContextAction *************//

    function IContextAction.Perform = IContextAction_Perform;
    function IContextAction_Perform(participantCoupon: Integer;
      itemNames, itemValues: OleVariant; const appSignature: WideString;
      var actionCoupon: Integer; var outItemNames,
      outItemValues: OleVariant): WideString; safecall;

    //************* IContextFilter *************//

    function IContextFilter.GetSubjectsOfInterest = IContextFilter_GetSubjectsOfInterest;
    function IContextFilter_GetSubjectsOfInterest(participantCoupon: Integer): OleVariant; safecall;

    procedure IContextFilter.ClearFilter = IContextFilter_ClearFilter;
    procedure IContextFilter_ClearFilter(participantCoupon: Integer); safecall;

    procedure IContextFilter.SetSubjectsOfInterest = IContextFilter_SetSubjectsOfInterest;
    procedure IContextFilter_SetSubjectsOfInterest(participantCoupon: Integer; subjectNames: OleVariant); safecall;

  end;

implementation

uses ComServ, MainWindow;

function TContextManager.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HRESULT;
begin
  if ExceptObject is Exception
  then Service.LogException(Exception(ExceptObject));

  if ExceptObject is TContextException
  then Result := TContextException(ExceptObject).Code
  else Result := inherited SafeCallException(ExceptObject, ExceptAddr);
end;

//************* IContextManager *************//

function TContextManager.IContextManager_JoinCommonContext(
  const contextParticipant: IDispatch; const sApplicationTitle: WideString;
  survey, wait: WordBool): Integer;
begin
  Service.LogInvocation('IContextManager.JoinCommonContext');
  Result := Service.CreateParticipant(contextParticipant,
    sApplicationTitle, survey, wait);
end;

procedure TContextManager.IContextManager_LeaveCommonContext(participantCoupon: Integer);
begin
  Service.LogInvocation('IContextManager.LeaveCommonContext');
  Service.DestroyParticipant(participantCoupon);
end;

procedure TContextManager.IContextManager_ResumeParticipation(participantCoupon: Integer;
  wait: WordBool);
begin
  Service.LogInvocation('IContextManager.ResumeParticipation');
  Service.SuspendParticipant(participantCoupon, False);
end;

procedure TContextManager.IContextManager_SuspendParticipation(participantCoupon: Integer);
begin
  Service.LogInvocation('IContextManager.SuspendParticipation');
  Service.SuspendParticipant(participantCoupon, True);
end;

function TContextManager.IContextManager_Get_MostRecentcontextCoupon: Integer;
begin
  Service.LogInvocation('IContextManager.Get_MostRecentContextCoupon');
  Result := Service.CurrentContextCoupon;
  frmMain.Log('Returned: ' + IntToStr(Result));
end;

function TContextManager.IContextManager_StartContextChanges(participantCoupon: Integer): Integer;
begin
  Service.LogInvocation('IContextManager.StartContextChanges');
  Result := Service.StartContextChanges(participantCoupon);
end;

function TContextManager.IContextManager_EndContextChanges(contextCoupon: Integer;
  var someBusy: WordBool): OleVariant;
begin
  Service.LogInvocation('IContextManager.EndContextChanges');
  Result := Service.EndContextChanges(contextCoupon);
end;

procedure TContextManager.IContextManager_PublishChangesDecision(contextCoupon: Integer;
  const decision: WideString);
begin
  Service.LogInvocation('IContextManager.PublishChangesDecision');
  Service.PublishChangesDecision(contextCoupon, decision);
end;

procedure TContextManager.IContextManager_UndoContextChanges(contextCoupon: Integer);
begin
  Service.LogInvocation('IContextManager.UndoContextChanges');
  Service.UndoContextChanges(contextCoupon);
end;

//************* IContextData *************//

function TContextManager.IContextData_GetItemNames(contextCoupon: Integer): OleVariant;
begin
  Service.LogInvocation('IContextData.GetItemNames');
  Result := Service.GetItemNames(contextCoupon);
end;

function TContextManager.IContextData_GetItemValues(itemNames: OleVariant;
  onlyChanges: WordBool; contextCoupon: Integer): OleVariant;
begin
  Service.LogInvocation('IContextData.GetItemValues');
  Result := Service.GetItemValues(itemNames, onlyChanges, contextCoupon);
end;

procedure TContextManager.IContextData_DeleteItems(participantCoupon: Integer;
  names: OleVariant; contextCoupon: Integer);
begin
  Service.LogInvocation('IContextData.DeleteItems');
  Service.NotImplemented('IContextData.DeleteItems is deprecated');
end;

procedure TContextManager.IContextData_SetItemValues(participantCoupon: Integer;
  itemNames, itemValues: OleVariant; contextCoupon: Integer);
begin
  Service.LogInvocation('IContextData.SetItemValues');
  Service.SetItemValues(participantCoupon, itemNames, itemValues, contextCoupon);
end;

//************* ISecureContextData *************//

function TContextManager.ISecureContextData_GetItemNames(
  contextCoupon: Integer): OleVariant;
begin
  Service.LogInvocation('ISecureContextData.GetItemNames');
  frmMain.Log('Delegating to IContextData.GetItemNames');
  Result := IContextData_GetItemNames(contextCoupon);
end;

function TContextManager.ISecureContextData_GetItemValues(
  participantCoupon: Integer; names: OleVariant; onlyChanges: WordBool;
  contextCoupon: Integer; const appSignature: WideString;
  var managerSignature: WideString): OleVariant;
begin
  Service.LogInvocation('ISecureContextData.GetItemValues');
  frmMain.Log('Delegating to IContextData.GetItemValues');
  Result := IContextData_GetItemValues(names, onlyChanges, contextCoupon);
end;

procedure TContextManager.ISecureContextData_SetItemValues(
  participantCoupon: Integer; itemNames, itemValues: OleVariant;
  contextCoupon: Integer; const appSignature: WideString);
begin
  Service.LogInvocation('ISecureContextData.SetItemValues');
  frmMain.Log('Delegating to IContextData.SetItemValues');
  IContextData_SetItemValues(participantCoupon, itemNames, itemValues, contextCoupon);
end;

//************* ISecureBinding *************//

function TContextManager.ISecureBinding_FinalizeBinding(bindeeCoupon: Integer;
  const bindeePublicKey, mac: WideString): OleVariant;
begin
  Service.LogInvocation('ISecureBinding.FinalizeBinding');
end;

function TContextManager.ISecureBinding_InitializeBinding(bindeeCoupon: Integer;
  propertyNames, propertyValues: OleVariant;
  var binderPublicKey: WideString): WideString;
begin
  Service.LogInvocation('ISecureBinding.InitializeBinding');
end;

//************* IContextAction *************//

function TContextManager.IContextAction_Perform(participantCoupon: Integer; itemNames,
  itemValues: OleVariant; const appSignature: WideString;
  var actionCoupon: Integer; var outItemNames,
  outItemValues: OleVariant): WideString;
begin
  Service.LogInvocation('IContextAction.Perform');
  Service.NotImplemented('IContextAction.Perform is not implemented');
end;

//************* IContextFilter *************//

function TContextManager.IContextFilter_GetSubjectsOfInterest(
  participantCoupon: Integer): OleVariant;
begin
  Service.LogInvocation('IContextFilter.GetSubjectsOfInterest');

end;

procedure TContextManager.IContextFilter_ClearFilter(participantCoupon: Integer);
begin
  Service.LogInvocation('IContextFilter.ClearFilter');

end;

procedure TContextManager.IContextFilter_SetSubjectsOfInterest(participantCoupon: Integer;
  subjectNames: OleVariant);
begin
  Service.LogInvocation('IContextFilter.SetSubjectsOfInterest');

end;

initialization
  TAutoObjectFactory.Create(ComServer, TContextManager, Class_ContextManager,
    ciMultiInstance, tmApartment);
end.
