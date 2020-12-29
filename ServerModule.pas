unit ServerModule;

interface

uses
  SysUtils, Classes, StrUtils, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdCustomHTTPServer, IdHTTPServer, IdContext, IdTCPServer, IdUri, Common, CCOW_TLB,
  IdTCPConnection, IdTCPClient,
  ExtCtrls;

type
  THandlerProc = procedure(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) of object;

  THandler = class
    method: String;
    handler: THandlerProc;
    constructor Create(method: String; handler: THandlerProc);
  end;

  TRestServer = class(TDataModule)
    httpServer: TIdHTTPServer;
    Timer: TTimer;
    procedure httpServerCommandGet(context: TIdContext;
      request: TIdHTTPRequestInfo;
      response: TIdHTTPResponseInfo);
    procedure DataModuleCreate(Sender: TObject);
    procedure httpServerAfterBind(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    handlers: TList;
    procedure AddHandler(intf: String; method: String; handler: THandlerProc);
    function FindHandler(method: String): THandler;

    //************************** ContextManagementRegistry **************************/

    procedure ContextManagementRegistry_Locate(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);

    //************************** ContextManager **************************/

    procedure ContextManager_EndContextChanges(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure ContextManager_Get_MostRecentContextCoupon(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure ContextManager_JoinCommonContext(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure ContextManager_StartContextChanges(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure ContextManager_LeaveCommonContext(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure ContextManager_PublishChangesDecision(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure ContextManager_ResumeParticipation(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure ContextManager_SuspendParticipation(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure ContextManager_UndoContextChanges(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);

    //************************** ContextData **************************/

    procedure ContextData_GetItemNames(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure ContextData_GetItemValues(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure ContextData_SetItemValues(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);

    //************************** SecureContextData **************************/

    procedure SecureContextData_GetItemNames(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure SecureContextData_GetItemValues(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure SecureContextData_SetItemValues(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);

    //************************** ContextFilter **************************/

    procedure ContextFilter_GetSubjectsOfInterest(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure ContextFilter_ClearFilter(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure ContextFilter_SetSubjectsOfInterest(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
  end;

var
  RestServer: TRestServer;

implementation

{$R *.dfm}

uses
  ContextManager, ContextSession, Participant, MainForm;

const
  INTF_CONTEXT_MANAGEMENT_REGISTRY = 'ContextManagementRegistry';
  INTF_CONTEXT_MANAGER = 'ContextManager';
  INTF_CONTEXT_DATA = 'ContextData';
  INTF_SECURE_CONTEXT_DATA = 'SecureContextData';
  INTF_CONTEXT_FILTER = 'ContextFilter';

var
  ContextManager: TContextManager;

//************************** Utility Methods **************************/

{
  Logs activity on the main form.
}
procedure Log(activity: String; params: array of const);
begin
  frmMain.LogServiceActivity(Format(activity, params));
end;

{
  Returns the value of the named string parameter from a request.  If not present
  and 'required' is true, an exception is raised.
}
function GetParameter(paramName: String; request: TIdHTTPRequestInfo; required: Boolean): String; overload;
begin
  Result := ValueFromName(paramName, request.Params);
  Assert(Not(required) or (Result <> ''), 'A required parameter (%s) is missing.', [paramName]);
end;

{
  Returns the value of the named integer parameter from a request.  If not present
  and 'required' is true, an exception is raised.
}
function GetIntParameter(paramName: String; request: TIdHTTPRequestInfo; required: Boolean): Integer;
var
  s: String;
begin
  s := GetParameter(paramName, request, required);

  if s = ''
  then Result := 0
  else Result := StrToInt(s);
end;

{
  Returns the value of the named Boolean parameter from a request.  If not present
  and 'required' is true, an exception is raised.
}
function GetBooleanParameter(paramName: String; request: TIdHTTPRequestInfo; required: Boolean): Boolean;
var
  s: String;
begin
  s := AnsiLowerCase(GetParameter(paramName, request, required));
  Result := s = 'true';
end;

{
  Returns the value of the named array parameter from a request.  If not present
  and 'required' is true, an exception is raised.
}
function GetArrayParameter(paramName: String; request: TIdHTTPRequestInfo; required: Boolean): TStrings;
var
  s: String;
begin
  s := GetParameter(paramName, request, required);
  Result := TStringList.Create;
  Result.Delimiter := '|';

  if s <> ''
  then Result.DelimitedText := s;
end;

{
  Encodes and copies a form into the response.
}
procedure SaveForm(response: TIdHTTPResponseInfo; form: TStrings);
begin
  response.ContentText := EncodeForm(form);
  response.ContentType := 'application/x-www-form-urlencoded';
end;

//************************** THandler **************************/

constructor THandler.Create(method: String; handler: THandlerProc);
begin
  Self.method := method;
  Self.handler := handler;
end;

//************************** TRestServer **************************/

procedure TRestServer.DataModuleCreate(Sender: TObject);
begin
  handlers := TList.Create;

  AddHandler(INTF_CONTEXT_MANAGEMENT_REGISTRY, 'Locate', ContextManagementRegistry_Locate);

  AddHandler(INTF_CONTEXT_MANAGER, 'EndContextChanges', ContextManager_EndContextChanges);
  AddHandler(INTF_CONTEXT_MANAGER, 'Get_MostRecentContextCoupon', ContextManager_Get_MostRecentContextCoupon);
  AddHandler(INTF_CONTEXT_MANAGER, 'JoinCommonContext', ContextManager_JoinCommonContext);
  AddHandler(INTF_CONTEXT_MANAGER, 'StartContextChanges', ContextManager_StartContextChanges);
  AddHandler(INTF_CONTEXT_MANAGER, 'LeaveCommonContext', ContextManager_LeaveCommonContext);
  AddHandler(INTF_CONTEXT_MANAGER, 'PublishChangesDecision', ContextManager_PublishChangesDecision);
  AddHandler(INTF_CONTEXT_MANAGER, 'ResumeParticipation', ContextManager_ResumeParticipation);
  AddHandler(INTF_CONTEXT_MANAGER, 'SuspendParticipation', ContextManager_SuspendParticipation);
  AddHandler(INTF_CONTEXT_MANAGER, 'UndoContextChanges', ContextManager_UndoContextChanges);

  AddHandler(INTF_CONTEXT_DATA, 'GetItemNames', ContextData_GetItemNames);
  AddHandler(INTF_CONTEXT_DATA, 'GetItemValues', ContextData_GetItemValues);
  AddHandler(INTF_CONTEXT_DATA, 'SetItemValues', ContextData_SetItemValues);

  AddHandler(INTF_SECURE_CONTEXT_DATA, 'GetItemNames', SecureContextData_GetItemNames);
  AddHandler(INTF_SECURE_CONTEXT_DATA, 'GetItemValues', SecureContextData_GetItemValues);
  AddHandler(INTF_SECURE_CONTEXT_DATA, 'SetItemValues', SecureContextData_SetItemValues);

  AddHandler(INTF_CONTEXT_FILTER, 'GetSubjectsOfInterest', ContextFilter_GetSubjectsOfInterest);
  AddHandler(INTF_CONTEXT_FILTER, 'ClearFilter', ContextFilter_ClearFilter);
  AddHandler(INTF_CONTEXT_FILTER, 'SetSubjectsOfInterest', ContextFilter_SetSubjectsOfInterest);

  Try
    httpServer.Active := True;
  Except
    on e: Exception do
      Log('CCOW service initialization failed: %s', [e.Message]);
  end;
end;

procedure TRestServer.DataModuleDestroy(Sender: TObject);
begin
  httpServer.Active := False;
  ContextManager := nil;
  handlers.Clear;
end;

procedure TRestServer.httpServerCommandGet(context: TIdContext;
  request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  method: String;
  handler: THandler;
begin
  response.CacheControl := 'max-age=0, must-revalidate';
  method := getParameter('interface', request, True) + '::' + getParameter('method', request, True);
  Log('Servicing request for CCOW service %s', [method]);
  handler := FindHandler(method);

  if handler = nil
  then begin
    response.ResponseNo := 404;
    response.ContentText := 'exception=NotFound';
    Log('No CCOW service named %s is registered', [method]);
  end else Try
    response.ResponseNo := 200;
    response.ContentType := 'application/x-www-form-urlencoded';
    handler.handler(request, response);
    Log('The CCOW service %s completed successfully', [handler.method]);
  Except
    on e: Exception do begin
      response.ResponseNo := 500;
      response.ContentText := 'exception=' + EncodeParameter(e.Message);
      Log('The CCOW service %s returned an error: %s', [handler.method, e.Message]);
    end;
  end;

  response.WriteHeader;
  response.WriteContent;
end;

procedure TRestServer.AddHandler(intf: String; method: String; handler: THandlerProc);
begin
  handlers.Add(THandler.Create(intf + '::' + method, handler));
end;

function TRestServer.FindHandler(method: String): THandler;
var
  i: Integer;
  h: THandler;
begin
  Result := nil;

  for i := 0 to handlers.Count - 1 do
  begin
    h := THandler(handlers[i]);

    if CompareText(h.method, method) = 0
    then begin
      Result := h;
      break;
    end;
  end;

end;

//************************** ContextManagementRegistry **************************/

procedure TRestServer.ContextManagementRegistry_Locate(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  componentName: String;
  contextParticipant: String;
  version: String;
  form: TStrings;
begin
  componentName := GetParameter('componentName', request, True);
  Assert(CompareText('CCOW.ContextManager', componentName) = 0, 'Invalid component name: %s', [componentName]);
  contextParticipant := GetParameter('contextParticipant', request, True);
  version := GetParameter('version', request, False);
  form := TStringList.Create;
  form.Values['componentUrl'] := 'http://127.0.0.1';
  form.Values['site'] := 'ccow.org';
  SaveForm(response, form);
end;

//************************** ContextManager **************************/

procedure TRestServer.ContextManager_EndContextChanges(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  contextCoupon: Integer;
  noContinue: WordBool;
  responses: OleVariant;
  form: TStrings;
begin
  contextCoupon := GetIntParameter('contextCoupon', request, True);
  ContextManager.IContextManager_EndContextChanges(contextCoupon, noContinue);
  form := TStringList.Create;
  form.Values['noContinue'] := BoolToStr(noContinue);
  form.Values['responses'] := SerializeArray(responses);
  SaveForm(response, form);
end;

procedure TRestServer.ContextManager_Get_MostRecentContextCoupon(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  contextCoupon: Integer;
  form: TStrings;
begin
  contextCoupon := ContextManager.IContextManager_Get_MostRecentcontextCoupon;
  form := TStringList.Create;
  form.Values['contextCoupon'] := IntToStr(contextCoupon);
  SaveForm(response, form);
end;

procedure TRestServer.ContextManager_JoinCommonContext(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  applicationName: String;
  contextParticipant: IDispatch;
  survey: Boolean;
  wait: Boolean;
  participantCoupon: Integer;
  form: TStrings;
begin
  applicationName := GetParameter('applicationName', request, True);
  contextParticipant := TParticipant.Create(GetParameter('contextParticipant', request, True));
  survey := GetBooleanParameter('survey', request, True);
  wait := GetBooleanParameter('wait', request, True);
  participantCoupon := ContextManager.IContextManager_JoinCommonContext(contextParticipant, applicationName, survey, wait);
  form := TStringList.Create;
  form.Values['participantCoupon'] := IntToStr(participantCoupon);
  SaveForm(response, form);
end;

procedure TRestServer.ContextManager_StartContextChanges(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  participantCoupon: Integer;
  contextCoupon: Integer;
  form: TStrings;
begin
  participantCoupon := GetIntParameter('participantCoupon', request, True);
  contextCoupon := ContextManager.IContextManager_StartContextChanges(participantCoupon);
  form := TStringList.Create;
  form.Values['contextCoupon'] := IntToStr(contextCoupon);
  SaveForm(response, form);
end;

procedure TRestServer.ContextManager_LeaveCommonContext(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  participantCoupon: Integer;
begin
  participantCoupon := GetIntParameter('participantCoupon', request, True);
  ContextManager.IContextManager_LeaveCommonContext(participantCoupon);
end;

procedure TRestServer.ContextManager_PublishChangesDecision(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  contextCoupon: Integer;
  decision: String;
begin
  contextCoupon := GetIntParameter('contextCoupon', request, True);
  decision := GetParameter('decision', request, True);
  ContextManager.IContextManager_PublishChangesDecision(contextCoupon, decision);
end;

procedure TRestServer.ContextManager_ResumeParticipation(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  participantCoupon: Integer;
  wait: Boolean;
begin
  participantCoupon := GetIntParameter('participantCoupon', request, True);
  wait := GetBooleanParameter('wait', request, True);
  ContextManager.IContextManager_ResumeParticipation(participantCoupon, wait);
end;

procedure TRestServer.ContextManager_SuspendParticipation(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  participantCoupon: Integer;
begin
  participantCoupon := GetIntParameter('participantCoupon', request, True);
  ContextManager.IContextManager_SuspendParticipation(participantCoupon);
end;

procedure TRestServer.ContextManager_UndoContextChanges(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  contextCoupon: Integer;
begin
  contextCoupon := GetIntParameter('contextCoupon', request, True);
  ContextManager.IContextManager_UndoContextChanges(contextCoupon);
end;

//************************** ContextData **************************/

procedure TRestServer.ContextData_GetItemNames(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  itemNames: String;
  contextCoupon: Integer;
  form: TStrings;
begin
  contextCoupon := GetIntParameter('contextCoupon', request, True);
  itemNames := SerializeArray(ContextManager.IContextData_GetItemNames(contextCoupon));
  form := TStringList.Create;
  form.Values['names'] := itemNames;
  SaveForm(response, form);
end;

procedure TRestServer.ContextData_GetItemValues(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  itemValues: String;
  contextCoupon: Integer;
  itemNames: OleVariant;
  onlyChanges: Boolean;
  form: TStrings;
begin
  contextCoupon := GetIntParameter('contextCoupon', request, True);
  itemNames := ToVarArray(GetArrayParameter('itemNames', request, True), RawText);
  onlyChanges := GetBooleanParameter('onlyChanges', request, True);
  itemValues := SerializeArray(ContextManager.IContextData_GetItemValues(itemNames, onlyChanges, contextCoupon));
  form := TStringList.Create;
  form.Values['values'] := itemNames;
  SaveForm(response, form);
end;

procedure TRestServer.ContextData_SetItemValues(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  itemValues: String;
  participantCoupon: Integer;
  contextCoupon: Integer;
  itemNames: OleVariant;
begin
  participantCoupon := GetIntParameter('participantCoupon', request, True);
  contextCoupon := GetIntParameter('contextCoupon', request, True);
  itemNames := ToVarArray(GetArrayParameter('itemNames', request, True), RawText);
  itemValues := ToVarArray(GetArrayParameter('itemValues', request, True), RawText);
  ContextManager.IContextData_SetItemValues(participantCoupon, itemNames, itemValues, contextCoupon);
end;

//************************** SecureContextData **************************/

procedure TRestServer.SecureContextData_GetItemNames(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  itemNames: String;
  contextCoupon: Integer;
  form: TStrings;
begin
  contextCoupon := GetIntParameter('contextCoupon', request, True);
  itemNames := SerializeArray(ContextManager.ISecureContextData_GetItemNames(contextCoupon));
  form := TStringList.Create;
  form.Values['names'] := itemNames;
  SaveForm(response, form);
end;

procedure TRestServer.SecureContextData_GetItemValues(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  itemValues: String;
  participantCoupon: Integer;
  contextCoupon: Integer;
  itemNames: OleVariant;
  onlyChanges: Boolean;
  appSignature: String;
  managerSignature: WideString;
  form: TStrings;
begin
  participantCoupon := GetIntParameter('participantCoupon', request, True);
  contextCoupon := GetIntParameter('contextCoupon', request, True);
  itemNames := ToVarArray(GetArrayParameter('itemNames', request, True), RawText);
  onlyChanges := GetBooleanParameter('onlyChanges', request, True);
  appSignature := GetParameter('appSignature', request, True);
  itemValues := SerializeArray(ContextManager.ISecureContextData_GetItemValues(
    participantCoupon, itemNames, onlyChanges, contextCoupon, appSignature, managerSignature));
  form := TStringList.Create;
  form.Values['values'] := itemNames;
  form.Values['managerSignature'] := managerSignature;
  SaveForm(response, form);
end;

procedure TRestServer.SecureContextData_SetItemValues(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  itemValues: String;
  participantCoupon: Integer;
  contextCoupon: Integer;
  appSignature: String;
  itemNames: OleVariant;
begin
  participantCoupon := GetIntParameter('participantCoupon', request, True);
  contextCoupon := GetIntParameter('contextCoupon', request, True);
  appSignature := GetParameter('appSignature', request, True);
  itemNames := ToVarArray(GetArrayParameter('itemNames', request, True), RawText);
  itemValues := ToVarArray(GetArrayParameter('itemValues', request, True), RawText);
  ContextManager.ISecureContextData_SetItemValues(participantCoupon, itemNames, itemValues, contextCoupon, appSignature);
end;

//************************** ContextFilter **************************/

procedure TRestServer.ContextFilter_GetSubjectsOfInterest(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  participantCoupon: Integer;
  subjectNames: OleVariant;
  form: TStrings;
begin
  participantCoupon := GetIntParameter('participantCoupon', request, True);
  subjectNames := ContextManager.IContextFilter_GetSubjectsOfInterest(participantCoupon);
  form := TStringList.Create;
  form.Values['subjectNames'] := SerializeArray(subjectNames);
  SaveForm(response, form);
end;

procedure TRestServer.ContextFilter_ClearFilter(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  participantCoupon: Integer;
begin
  participantCoupon := GetIntParameter('participantCoupon', request, True);
  ContextManager.IContextFilter_ClearFilter(participantCoupon);
end;

procedure TRestServer.ContextFilter_SetSubjectsOfInterest(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  participantCoupon: Integer;
  subjectNames: OleVariant;
begin
  participantCoupon := GetIntParameter('participantCoupon', request, True);
  subjectNames := ToVarArray(GetArrayParameter('subjectNames', request, True), RawText);
  ContextManager.IContextFilter_SetSubjectsOfInterest(participantCoupon, subjectNames);
end;

procedure TRestServer.httpServerAfterBind(Sender: TObject);
begin
  Log('CCOW services available on port %d', [httpServer.DefaultPort]);
end;

procedure TRestServer.TimerTimer(Sender: TObject);
begin
  FreeAndNil(timer);
  ContextManager := TContextManager.Create(DefaultSession);
end;

end.
