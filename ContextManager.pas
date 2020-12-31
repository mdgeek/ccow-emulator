unit ContextManager;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, CCOW_TLB, Classes, SysUtils, StdVcl, ContextSession, Forms;

type
  TContextManager = class(TAutoObject, IContextManager, IContextData,
    IContextAction, ISecureBinding, ISecureContextData, IContextFilter,
    IContextSession, IImplementationInformation)

  private
    session: TContextSession;

  public

    constructor Create(session: TContextSession);
    procedure Initialize; override;
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HRESULT; override;

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
    function IContextManager_StartContextChanges(participantCoupon: Integer): Integer; safecall;

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

    //************************** IImplementationInformation **************************/

    function IImplementationInformation.Get_ComponentName = IImplementationInformation_Get_ComponentName;
    function IImplementationInformation_Get_ComponentName: WideString; safecall;

    function IImplementationInformation.Get_Manufacturer = IImplementationInformation_Get_Manufacturer;
    function IImplementationInformation_Get_Manufacturer: WideString; safecall;

    function IImplementationInformation.Get_PartNumber = IImplementationInformation_Get_PartNumber;
    function IImplementationInformation_Get_PartNumber: WideString; safecall;

    function IImplementationInformation.Get_RevMajorNum = IImplementationInformation_Get_RevMajorNum;
    function IImplementationInformation_Get_RevMajorNum: WideString; safecall;

    function IImplementationInformation.Get_RevMinorNum = IImplementationInformation_Get_RevMinorNum;
    function IImplementationInformation_Get_RevMinorNum: WideString; safecall;

    function IImplementationInformation.Get_TargetOS = IImplementationInformation_Get_TargetOS;
    function IImplementationInformation_Get_TargetOS: WideString; safecall;

    function IImplementationInformation.Get_TargetOSRev = IImplementationInformation_Get_TargetOSRev;
    function IImplementationInformation_Get_TargetOSRev: WideString; safecall;

    function IImplementationInformation.Get_WhenInstalled = IImplementationInformation_Get_WhenInstalled;
    function IImplementationInformation_Get_WhenInstalled: WideString; safecall;

  end;

implementation

uses ComServ, Common, Variants;

constructor TContextManager.Create(session: TContextSession);
begin
  Self.session := session;
end;

procedure TContextManager.Initialize;
begin
  inherited;

  if session = nil
  then session := DefaultSession;
  
  Assert(session <> nil, 'No session!!', []);
end;

function TContextManager.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HRESULT;
begin
  if ExceptObject is Exception
  then session.LogException(Exception(ExceptObject));

  if ExceptObject is TContextException
  then Result := TContextException(ExceptObject).Code
  else Result := inherited SafeCallException(ExceptObject, ExceptAddr);

  session.LogEnd;
end;

//************************** IContextManager **************************/

function TContextManager.IContextManager_JoinCommonContext(
  const contextParticipant: IDispatch; const sApplicationTitle: WideString;
  survey, wait: WordBool): Integer;
const
  method = 'IContextManager.JoinCommonContext';
begin
  session.LogStart(method);
  Result := session.CreateParticipant(contextParticipant,
    sApplicationTitle, survey, wait);
  session.LogEnd;
end;

procedure TContextManager.IContextManager_LeaveCommonContext(participantCoupon: Integer);
const
  method = 'IContextManager.LeaveCommonContext';
begin
  session.LogStart(method);
  session.DestroyParticipant(participantCoupon);
  session.LogEnd;
end;

procedure TContextManager.IContextManager_ResumeParticipation(participantCoupon: Integer;
  wait: WordBool);
const
  method = 'IContextManager.ResumeParticipation';
begin
  session.LogStart(method);
  session.SuspendParticipant(participantCoupon, False);
  session.LogEnd;
end;

procedure TContextManager.IContextManager_SuspendParticipation(participantCoupon: Integer);
const
  method = 'IContextManager.SuspendParticipation';
begin
  session.LogStart(method);
  session.SuspendParticipant(participantCoupon, True);
  session.LogEnd;
end;

function TContextManager.IContextManager_Get_MostRecentcontextCoupon: Integer;
const
  method = 'IContextManager.Get_MostRecentContextCoupon';
begin
  session.LogStart(method);
  Result := session.CurrentContextCoupon;
  session.Log('Returned: ' + IntToStr(Result));
  session.LogEnd;
end;

function TContextManager.IContextManager_StartContextChanges(participantCoupon: Integer): Integer;
const
  method = 'IContextManager.StartContextChanges';
begin
  session.LogStart(method);
  Result := session.StartContextChanges(participantCoupon);
  session.LogEnd;
end;

function TContextManager.IContextManager_EndContextChanges(contextCoupon: Integer;
  var someBusy: WordBool): OleVariant;
const
  method = 'IContextManager.EndContextChanges';
begin
  session.LogStart(method);
  Result := session.EndContextChanges(contextCoupon);
  session.LogEnd;
end;

procedure TContextManager.IContextManager_PublishChangesDecision(contextCoupon: Integer;
  const decision: WideString);
const
  method = 'IContextManager.PublishChangesDecision';
begin
  session.LogStart(method);
  session.PublishChangesDecision(contextCoupon, decision);
  session.LogEnd;
end;

procedure TContextManager.IContextManager_UndoContextChanges(contextCoupon: Integer);
const
  method = 'IContextManager.UndoContextChanges';
begin
  session.LogStart(method);
  session.UndoContextChanges(contextCoupon);
  session.LogEnd;
end;

//************************** IContextData **************************/

function TContextManager.IContextData_GetItemNames(contextCoupon: Integer): OleVariant;
const
  method = 'IContextData.GetItemNames';
begin
  session.LogStart(method);
  Result := session.GetItemNames(contextCoupon);
  session.LogEnd;
end;

function TContextManager.IContextData_GetItemValues(itemNames: OleVariant;
  onlyChanges: WordBool; contextCoupon: Integer): OleVariant;
const
  method = 'IContextData.GetItemValues';
begin
  session.LogStart(method);
  Result := session.GetItemValues(-1, itemNames, onlyChanges, contextCoupon);
  session.LogEnd;
end;

procedure TContextManager.IContextData_DeleteItems(participantCoupon: Integer;
  names: OleVariant; contextCoupon: Integer);
const
  method = 'IContextData.DeleteItems';
begin
  session.LogStart(method);
  session.NotImplemented('IContextData.DeleteItems is deprecated');
  session.LogEnd;
end;

procedure TContextManager.IContextData_SetItemValues(participantCoupon: Integer;
  itemNames, itemValues: OleVariant; contextCoupon: Integer);
const
  method = 'IContextData.SetItemValues';
begin
  session.LogStart(method);
  session.SetItemValues(participantCoupon, itemNames, itemValues, contextCoupon);
  session.LogEnd;
end;

//************************** ISecureContextData **************************/

function TContextManager.ISecureContextData_GetItemNames(
  contextCoupon: Integer): OleVariant;
const
  method = 'ISecureContextData.GetItemNames';
begin
  session.LogStart(method);
  Result := session.GetItemNames(contextCoupon);
  session.LogEnd;
end;

function TContextManager.ISecureContextData_GetItemValues(
  participantCoupon: Integer; itemNames: OleVariant; onlyChanges: WordBool;
  contextCoupon: Integer; const appSignature: WideString;
  var managerSignature: WideString): OleVariant;
const
  method = 'ISecureContextData.GetItemValues';
begin
  session.LogStart(method);
  Result := session.GetItemValues(participantCoupon, itemNames, onlyChanges, contextCoupon);
  session.LogEnd;
end;

procedure TContextManager.ISecureContextData_SetItemValues(
  participantCoupon: Integer; itemNames, itemValues: OleVariant;
  contextCoupon: Integer; const appSignature: WideString);
const
  method = 'ISecureContextData.SetItemValues';
begin
  session.LogStart(method);
  session.SetItemValues(participantCoupon, itemNames, itemValues, contextCoupon);
  session.LogEnd;
end;

//************************** ISecureBinding **************************/

function TContextManager.ISecureBinding_FinalizeBinding(bindeeCoupon: Integer;
  const bindeePublicKey, mac: WideString): OleVariant;
const
  method = 'ISecureBinding.FinalizeBinding';
begin
  session.LogStart(method);
  session.LogEnd;
end;

function TContextManager.ISecureBinding_InitializeBinding(bindeeCoupon: Integer;
  propertyNames, propertyValues: OleVariant;
  var binderPublicKey: WideString): WideString;
const
  method = 'ISecureBinding.InitializeBinding';
begin
  session.LogStart(method);
  session.LogEnd;
end;

//************************** IContextAction **************************/

function TContextManager.IContextAction_Perform(participantCoupon: Integer; itemNames,
  itemValues: OleVariant; const appSignature: WideString;
  var actionCoupon: Integer; var outItemNames,
  outItemValues: OleVariant): WideString;
const
  method = 'IContextAction.Perform';
begin
  session.LogStart(method);
  session.NotImplemented('IContextAction.Perform is not implemented');
end;

//************************** IContextFilter **************************/

function TContextManager.IContextFilter_GetSubjectsOfInterest(
  participantCoupon: Integer): OleVariant;
const
  method = 'IContextFilter.GetSubjectsOfInterest';
begin
  session.LogStart(method);
  Result := session.GetSubjectsOfInterest(participantCoupon);
  session.LogEnd;
end;

procedure TContextManager.IContextFilter_ClearFilter(participantCoupon: Integer);
const
  method = 'IContextFilter.ClearFilter';
begin
  session.LogStart(method);
  session.ClearFilter(participantCoupon);
  session.LogEnd;
end;

procedure TContextManager.IContextFilter_SetSubjectsOfInterest(participantCoupon: Integer;
  subjectNames: OleVariant);
const
  method = 'IContextFilter.SetSubjectsOfInterest';
begin
  session.LogStart(method);
  session.SetSubjectsOfInterest(participantCoupon, subjectNames);
  session.LogEnd;
end;

//************************** IContextSession **************************/

function TContextManager.IContextSession_Create: IDispatch;
const
  method = 'IContextSession.Create';
begin
  session.LogStart(method);
  Result := TContextManager.Create(TContextSession.Create);
  session.LogEnd;
end;

procedure TContextManager.IContextSession_Activate(
  participantCoupon: Integer; const cmToActivate: IDispatch; const nonce,
  appSignature: WideString);
const
  method = 'IContextSession.Activate';
begin
  session.LogStart(method);
  session.SessionActivate(participantCoupon, cmToActivate, nonce, appSignature);
  session.LogEnd;
end;

//************************** IImplementationInformation **************************/

function TContextManager.IImplementationInformation_Get_ComponentName: WideString;
begin
  Result := Application.Title;
end;

function TContextManager.IImplementationInformation_Get_Manufacturer: WideString;
begin
  Result := 'University of Utah';
end;

function TContextManager.IImplementationInformation_Get_PartNumber: WideString;
begin
  Result := Null;
end;

function TContextManager.IImplementationInformation_Get_RevMajorNum: WideString;
begin
  Result := '1';
end;

function TContextManager.IImplementationInformation_Get_RevMinorNum: WideString;
begin
  Result := '0';
end;

function TContextManager.IImplementationInformation_Get_TargetOS: WideString;
begin
  Result := 'Windows';
end;

function TContextManager.IImplementationInformation_Get_TargetOSRev: WideString;
begin
  Result := 'All'
end;

function TContextManager.IImplementationInformation_Get_WhenInstalled: WideString;
begin
  Result := Null;
end;


initialization
  TAutoObjectFactory.Create(ComServer, TContextManager, Class_ContextManager,
    ciMultiInstance, tmApartment);
end.
