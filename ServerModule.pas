unit ServerModule;

interface

uses
  SysUtils, Classes, StrUtils, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdCustomHTTPServer, IdHTTPServer, IdContext, IdTCPServer, IdUri, Common, CCOW_TLB,
  IdTCPConnection, IdTCPClient, ExtCtrls, Variants, Windows, ContextManager;

type
  TFormRequest = class
  private
    request: TIdHTTPRequestInfo;
    FMethod: String;
    function GetParamStr(name: String): String;
    function GetOptionalStr(name: String): String;
    function GetParamInt(name: String): Integer;
    function GetParamBool(name: String): Boolean;
    function GetParamList(name: String): TStrings;
    function GetParamArray(name: String): Variant;
  public
    constructor Create(request: TIdHTTPRequestInfo);
    property Method: String read FMethod;
    property Param[name: String]: String read GetParamStr;
    property ParamInt[name: String]: Integer read GetParamInt;
    property ParamBool[name: String]: Boolean read GetParamBool;
    property ParamArray[name: String]: Variant read GetParamArray;
    property Optional[name: String]: String read GetOptionalStr;
  end;

  TFormResponse = class
  private
    content: TStrings;
    response: TIdHTTPResponseInfo;
    procedure SetParamStr(name: String; value: String);
    procedure SetParamInt(name: String; value: Integer);
    procedure SetParamBool(name: String; value: Boolean);
    procedure SetParamVariant(name: String; value: Variant);
  public
    constructor Create(response: TIdHTTPResponseInfo);
    procedure SetException(code: Integer; text: String);
    procedure Write;
    property Param[name: String]: String write SetParamStr;
    property ParamInt[name: String]: Integer write SetParamInt;
    property ParamBool[name: String]: Boolean write SetParamBool;
    property ParamVariant[name: String]: Variant write SetParamVariant;
  end;

  THandlerProc = procedure(request: TFormRequest; response: TFormResponse) of object;

  THandler = class
    method: String;
    handler: THandlerProc;
    constructor Create(method: String; handler: THandlerProc);
  end;

  TRestServer = class(TDataModule)
    httpServer: TIdHTTPServer;
    Timer: TTimer;
    procedure httpServerCommandGet(context: TIdContext;
      httpRequest: TIdHTTPRequestInfo; httpResponse: TIdHTTPResponseInfo);
    procedure DataModuleCreate(Sender: TObject);
    procedure httpServerAfterBind(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    contextManager: TContextManager;
    handlers: TList;
    procedure AddHandler(intf: String; method: String; handler: THandlerProc);
    function FindHandler(method: String): THandler;

    //************************** ContextManagementRegistry **************************/

    procedure ContextManagementRegistry_Locate(request: TFormRequest; response: TFormResponse);

    //************************** ContextManager **************************/

    procedure ContextManager_EndContextChanges(request: TFormRequest; response: TFormResponse);
    procedure ContextManager_Get_MostRecentContextCoupon(request: TFormRequest; response: TFormResponse);
    procedure ContextManager_JoinCommonContext(request: TFormRequest; response: TFormResponse);
    procedure ContextManager_StartContextChanges(request: TFormRequest; response: TFormResponse);
    procedure ContextManager_LeaveCommonContext(request: TFormRequest; response: TFormResponse);
    procedure ContextManager_PublishChangesDecision(request: TFormRequest; response: TFormResponse);
    procedure ContextManager_ResumeParticipation(request: TFormRequest; response: TFormResponse);
    procedure ContextManager_SuspendParticipation(request: TFormRequest; response: TFormResponse);
    procedure ContextManager_UndoContextChanges(request: TFormRequest; response: TFormResponse);

    //************************** ContextData **************************/

    procedure ContextData_GetItemNames(request: TFormRequest; response: TFormResponse);
    procedure ContextData_GetItemValues(request: TFormRequest; response: TFormResponse);
    procedure ContextData_SetItemValues(request: TFormRequest; response: TFormResponse);

    //************************** SecureContextData **************************/

    procedure SecureContextData_GetItemNames(request: TFormRequest; response: TFormResponse);
    procedure SecureContextData_GetItemValues(request: TFormRequest; response: TFormResponse);
    procedure SecureContextData_SetItemValues(request: TFormRequest; response: TFormResponse);

    //************************** ContextFilter **************************/

    procedure ContextFilter_GetSubjectsOfInterest(request: TFormRequest; response: TFormResponse);
    procedure ContextFilter_ClearFilter(request: TFormRequest; response: TFormResponse);
    procedure ContextFilter_SetSubjectsOfInterest(request: TFormRequest; response: TFormResponse);
  end;

var
  RestServer: TRestServer;

implementation

{$R *.dfm}

uses
  ContextSession, Participant, MainForm;

const
  INTF_CONTEXT_MANAGEMENT_REGISTRY = 'ContextManagementRegistry';
  INTF_CONTEXT_MANAGER = 'ContextManager';
  INTF_CONTEXT_DATA = 'ContextData';
  INTF_SECURE_CONTEXT_DATA = 'SecureContextData';
  INTF_CONTEXT_FILTER = 'ContextFilter';

//************************** Logging **************************/

{
  Logs activity on the main form.
}
procedure Log(activity: String; params: array of const);
begin
  frmMain.LogServiceActivity(Format('[%d] ', [GetCurrentThreadId]) + Format(activity, params));
end;

//************************** TFormRequest **************************/

constructor TFormRequest.Create(request: TIdHTTPRequestInfo);
begin
  Self.request := request;
  FMethod := Param['interface'] + '::' + Param['method'];
end;

function TFormRequest.GetParamStr(name: String): String;
begin
  Result := GetOptionalStr(name);
  Assert(Result <> '', 'A required parameter (%s) is missing.', [name]);
end;

function TFormRequest.GetOptionalStr(name: String): String;
begin
  Result := ValueFromName(name, request.Params);
end;

function TFormRequest.GetParamInt(name: String): Integer;
begin
  Result := StrToIntDef(GetParamStr(name), 0);
end;

function TFormRequest.GetParamBool(name: String): Boolean;
begin
  Result := StrToBoolDef(GetParamStr(name), False);
end;

function TFormRequest.getParamList(name: String): TStrings;
begin
  Result := TStringList.Create;
  Result.Delimiter := '|';
  Result.QuoteChar := #0;
  Result.DelimitedText := GetParamStr(name);
end;

function TFormRequest.GetParamArray(name: String): Variant;
begin
  Result := ToVarArray(getParamList(name), RawText);
end;

//************************** TFormResponse **************************/

constructor TFormResponse.Create(response: TIdHTTPResponseInfo);
begin
  Self.response := response;
  response.ResponseNo := 200;
  response.CacheControl := 'max-age=0, must-revalidate';
  response.CustomHeaders.Values['Access-Control-Allow-Origin'] := '*';
  response.ContentType := 'application/x-www-form-urlencoded';
  content := TStringList.Create;
end;

procedure TFormResponse.SetParamStr(name: String; value: String);
begin
  content.Values[name] := value;
end;

procedure TFormResponse.SetParamInt(name: String; value: Integer);
begin
  SetParamStr(name, IntToStr(value));
end;

procedure TFormResponse.SetParamBool(name: String; value: Boolean);
begin
  SetParamStr(name, BoolToStr(value));
end;

procedure TFormResponse.SetParamVariant(name: String; value: Variant);
begin
  if value = Null
  then SetParamStr(name, '')
  else if VarIsArray(value)
  then SetParamStr(name, SerializeArray(value))
  else SetParamStr(name, value);
end;

procedure TFormResponse.SetException(code: Integer; text: String);
begin
  content.Clear;
  response.ResponseNo := code;
  Param['exception'] := text;
end;

{
  Encodes and copies the response form into the HTTP response.
}
procedure TFormResponse.Write;
begin
  response.ContentText := EncodeForm(content);
  response.WriteHeader;
  response.WriteContent;
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
  AddHandler(INTF_CONTEXT_MANAGER, 'GetMostRecentContextCoupon', ContextManager_Get_MostRecentContextCoupon);
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
  contextManager := nil;
  handlers.Clear;
end;

{
  Handles all GET requests.
}
procedure TRestServer.httpServerCommandGet(context: TIdContext;
  httpRequest: TIdHTTPRequestInfo; httpResponse: TIdHTTPResponseInfo);
var
  method: String;
  handler: THandler;
  request: TFormRequest;
  response: TFormResponse;
begin
  request := TFormRequest.Create(httpRequest);
  response := TFormResponse.Create(httpResponse);
  method := request.Method;
  Log('Entering %s', [method]);
  handler := FindHandler(method);

  if handler = nil
  then begin
    response.SetException(404, 'NotFound');
    Log('Unknown service %s', [method]);
  end else Try
    handler.handler(request, response);
    Log('Exited %s', [handler.method]);
  Except
    on e: Exception do begin
      response.SetException(500, e.Message);
      Log('%s returned an error: %s', [handler.method, e.Message]);
    end;
  end;

  response.Write;
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

procedure TRestServer.ContextManagementRegistry_Locate(request: TFormRequest; response: TFormResponse);
var
  componentName: String;
  contextParticipant: String;
  version: String;
begin
  componentName := request.Param['componentName'];
  Assert(CompareText('CCOW.ContextManager', componentName) = 0, 'Invalid component name: %s', [componentName]);
  contextParticipant := request.Param['contextParticipant'];
  version := request.Optional['version'];
  response.Param['componentUrl'] := 'http://127.0.0.1:2116';
  response.Param['site'] := 'test.ccow.org';
end;

//************************** ContextManager **************************/

procedure TRestServer.ContextManager_EndContextChanges(request: TFormRequest; response: TFormResponse);
var
  contextCoupon: Integer;
  noContinue: WordBool;
  responses: OleVariant;
begin
  contextCoupon := request.ParamInt['contextCoupon'];
  responses := contextManager.IContextManager_EndContextChanges(contextCoupon, noContinue);
  response.ParamBool['noContinue'] := noContinue;
  response.ParamVariant['responses'] := responses;
end;

procedure TRestServer.ContextManager_Get_MostRecentContextCoupon(request: TFormRequest; response: TFormResponse);
var
  contextCoupon: Integer;
begin
  contextCoupon := contextManager.IContextManager_Get_MostRecentcontextCoupon;
  response.ParamInt['contextCoupon'] := contextCoupon;
end;

procedure TRestServer.ContextManager_JoinCommonContext(request: TFormRequest; response: TFormResponse);
var
  applicationName: String;
  contextParticipant: IDispatch;
  survey: Boolean;
  wait: Boolean;
  participantCoupon: Integer;
begin
  applicationName := request.Param['applicationName'];
  contextParticipant := TParticipant.Create(request.Param['contextParticipant']);
  survey := request.ParamBool['survey'];
  wait := request.ParamBool['wait'];
  participantCoupon := contextManager.IContextManager_JoinCommonContext(contextParticipant, applicationName, survey, wait);
  response.ParamInt['participantCoupon'] := participantCoupon;
end;

procedure TRestServer.ContextManager_StartContextChanges(request: TFormRequest; response: TFormResponse);
var
  participantCoupon: Integer;
  contextCoupon: Integer;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  contextCoupon := contextManager.IContextManager_StartContextChanges(participantCoupon);
  response.ParamInt['contextCoupon'] := contextCoupon;
end;

procedure TRestServer.ContextManager_LeaveCommonContext(request: TFormRequest; response: TFormResponse);
var
  participantCoupon: Integer;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  contextManager.IContextManager_LeaveCommonContext(participantCoupon);
end;

procedure TRestServer.ContextManager_PublishChangesDecision(request: TFormRequest; response: TFormResponse);
var
  contextCoupon: Integer;
  decision: String;
begin
  contextCoupon := request.ParamInt['contextCoupon'];
  decision := request.Param['decision'];
  contextManager.IContextManager_PublishChangesDecision(contextCoupon, decision);
end;

procedure TRestServer.ContextManager_ResumeParticipation(request: TFormRequest; response: TFormResponse);
var
  participantCoupon: Integer;
  wait: Boolean;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  wait := request.ParamBool['wait'];
  contextManager.IContextManager_ResumeParticipation(participantCoupon, wait);
end;

procedure TRestServer.ContextManager_SuspendParticipation(request: TFormRequest; response: TFormResponse);
var
  participantCoupon: Integer;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  contextManager.IContextManager_SuspendParticipation(participantCoupon);
end;

procedure TRestServer.ContextManager_UndoContextChanges(request: TFormRequest; response: TFormResponse);
var
  contextCoupon: Integer;
begin
  contextCoupon := request.ParamInt['contextCoupon'];
  contextManager.IContextManager_UndoContextChanges(contextCoupon);
end;

//************************** ContextData **************************/

procedure TRestServer.ContextData_GetItemNames(request: TFormRequest; response: TFormResponse);
var
  itemNames: OleVariant;
  contextCoupon: Integer;
begin
  contextCoupon := request.ParamInt['contextCoupon'];
  itemNames := contextManager.IContextData_GetItemNames(contextCoupon);
  response.ParamVariant['names'] := itemNames;
end;

procedure TRestServer.ContextData_GetItemValues(request: TFormRequest; response: TFormResponse);
var
  contextCoupon: Integer;
  itemNames: OleVariant;
  itemValues: OleVariant;
  onlyChanges: Boolean;
begin
  contextCoupon := request.ParamInt['contextCoupon'];
  itemNames := request.ParamArray['itemNames'];
  onlyChanges := request.ParamBool['onlyChanges'];
  itemValues := contextManager.IContextData_GetItemValues(itemNames, onlyChanges, contextCoupon);
  response.ParamVariant['values'] := itemValues;
end;

procedure TRestServer.ContextData_SetItemValues(request: TFormRequest; response: TFormResponse);
var
  participantCoupon: Integer;
  contextCoupon: Integer;
  itemNames: OleVariant;
  itemValues: OleVariant;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  contextCoupon := request.ParamInt['contextCoupon'];
  itemNames := request.ParamArray['itemNames'];
  itemValues := request.ParamArray['itemValues'];
  contextManager.IContextData_SetItemValues(participantCoupon, itemNames, itemValues, contextCoupon);
end;

//************************** SecureContextData **************************/

procedure TRestServer.SecureContextData_GetItemNames(request: TFormRequest; response: TFormResponse);
var
  itemNames: String;
  contextCoupon: Integer;
begin
  contextCoupon := request.ParamInt['contextCoupon'];
  itemNames := SerializeArray(contextManager.ISecureContextData_GetItemNames(contextCoupon));
  response.Param['names'] := itemNames;
end;

procedure TRestServer.SecureContextData_GetItemValues(request: TFormRequest; response: TFormResponse);
var
  itemValues: OleVariant;
  participantCoupon: Integer;
  contextCoupon: Integer;
  itemNames: OleVariant;
  onlyChanges: Boolean;
  appSignature: String;
  managerSignature: WideString;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  contextCoupon := request.ParamInt['contextCoupon'];
  itemNames := request.ParamArray['itemNames'];
  onlyChanges := request.ParamBool['onlyChanges'];
  appSignature := request.Param['appSignature'];
  itemValues := contextManager.ISecureContextData_GetItemValues(
    participantCoupon, itemNames, onlyChanges, contextCoupon, appSignature, managerSignature);
  response.ParamVariant['values'] := itemValues;
  response.Param['managerSignature'] := managerSignature;
end;

procedure TRestServer.SecureContextData_SetItemValues(request: TFormRequest; response: TFormResponse);
var
  itemValues: String;
  participantCoupon: Integer;
  contextCoupon: Integer;
  appSignature: String;
  itemNames: OleVariant;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  contextCoupon := request.ParamInt['contextCoupon'];
  appSignature := request.Param['appSignature'];
  itemNames := request.ParamArray['itemNames'];
  itemValues := request.ParamArray['itemValues'];
  contextManager.ISecureContextData_SetItemValues(participantCoupon, itemNames, itemValues, contextCoupon, appSignature);
end;

//************************** ContextFilter **************************/

procedure TRestServer.ContextFilter_GetSubjectsOfInterest(request: TFormRequest; response: TFormResponse);
var
  participantCoupon: Integer;
  subjectNames: OleVariant;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  subjectNames := contextManager.IContextFilter_GetSubjectsOfInterest(participantCoupon);
  response.ParamVariant['subjectNames'] := subjectNames;
end;

procedure TRestServer.ContextFilter_ClearFilter(request: TFormRequest; response: TFormResponse);
var
  participantCoupon: Integer;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  contextManager.IContextFilter_ClearFilter(participantCoupon);
end;

procedure TRestServer.ContextFilter_SetSubjectsOfInterest(request: TFormRequest; response: TFormResponse);
var
  participantCoupon: Integer;
  subjectNames: OleVariant;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  subjectNames := request.ParamArray['subjectNames'];
  contextManager.IContextFilter_SetSubjectsOfInterest(participantCoupon, subjectNames);
end;

procedure TRestServer.httpServerAfterBind(Sender: TObject);
begin
  Log('CCOW services available on port %d', [httpServer.DefaultPort]);
end;

{
  Create the underlying context manager instance on a delay to allow
  COM initialization to complete beforehand.
}
procedure TRestServer.TimerTimer(Sender: TObject);
begin
  timer.Enabled := False;
  contextManager := TContextManager.Create(DefaultSession);
  Log('Created context manager for CCOW services', []);
end;

end.
