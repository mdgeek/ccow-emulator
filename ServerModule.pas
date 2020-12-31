unit ServerModule;

interface

uses
  SysUtils, Classes, StrUtils, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdCustomHTTPServer, IdHTTPServer, IdContext, IdTCPServer, IdUri, Common, CCOW_TLB,
  IdTCPConnection, IdTCPClient, ExtCtrls, Variants, ContextManager, Logger;

type
  TRequest = class
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

  TResponse = class
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

  THandlerProc = procedure(request: TRequest; response: TResponse) of object;

  THandler = class
    method: String;
    handler: THandlerProc;
    constructor Create(method: String; handler: THandlerProc);
  end;

  TRestServer = class(TDataModule)
    httpServer: TIdHTTPServer;
    procedure httpServerCommandGet(context: TIdContext;
      httpRequest: TIdHTTPRequestInfo; httpResponse: TIdHTTPResponseInfo);
    procedure DataModuleCreate(Sender: TObject);
    procedure httpServerAfterBind(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    contextManager: TContextManager;
    handlers: TList;
    logger: TLogger;
    procedure AddHandler(intf: String; method: String; handler: THandlerProc);
    function FindHandler(method: String): THandler;

    //************************** ContextManagementRegistry **************************/

    procedure ContextManagementRegistry_Locate(request: TRequest; response: TResponse);

    //************************** ContextManager **************************/

    procedure ContextManager_EndContextChanges(request: TRequest; response: TResponse);
    procedure ContextManager_Get_MostRecentContextCoupon(request: TRequest; response: TResponse);
    procedure ContextManager_JoinCommonContext(request: TRequest; response: TResponse);
    procedure ContextManager_StartContextChanges(request: TRequest; response: TResponse);
    procedure ContextManager_LeaveCommonContext(request: TRequest; response: TResponse);
    procedure ContextManager_PublishChangesDecision(request: TRequest; response: TResponse);
    procedure ContextManager_ResumeParticipation(request: TRequest; response: TResponse);
    procedure ContextManager_SuspendParticipation(request: TRequest; response: TResponse);
    procedure ContextManager_UndoContextChanges(request: TRequest; response: TResponse);

    //************************** ContextData **************************/

    procedure ContextData_GetItemNames(request: TRequest; response: TResponse);
    procedure ContextData_GetItemValues(request: TRequest; response: TResponse);
    procedure ContextData_SetItemValues(request: TRequest; response: TResponse);

    //************************** SecureContextData **************************/

    procedure SecureContextData_GetItemNames(request: TRequest; response: TResponse);
    procedure SecureContextData_GetItemValues(request: TRequest; response: TResponse);
    procedure SecureContextData_SetItemValues(request: TRequest; response: TResponse);

    //************************** ContextFilter **************************/

    procedure ContextFilter_GetSubjectsOfInterest(request: TRequest; response: TResponse);
    procedure ContextFilter_ClearFilter(request: TRequest; response: TResponse);
    procedure ContextFilter_SetSubjectsOfInterest(request: TRequest; response: TResponse);
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

//************************** TRequest **************************/

constructor TRequest.Create(request: TIdHTTPRequestInfo);
begin
  Self.request := request;
  FMethod := Param['interface'] + '::' + Param['method'];
end;

function TRequest.GetParamStr(name: String): String;
begin
  Result := GetOptionalStr(name);
  Assert(Result <> '', 'A required parameter (%s) is missing.', [name]);
end;

function TRequest.GetOptionalStr(name: String): String;
begin
  Result := ValueFromName(name, request.Params);
end;

function TRequest.GetParamInt(name: String): Integer;
begin
  Result := StrToIntDef(GetParamStr(name), 0);
end;

function TRequest.GetParamBool(name: String): Boolean;
begin
  Result := StrToBoolDef(GetParamStr(name), False);
end;

function TRequest.getParamList(name: String): TStrings;
begin
  Result := TStringList.Create;
  Result.Delimiter := '|';
  Result.QuoteChar := #0;
  Result.DelimitedText := GetParamStr(name);
end;

function TRequest.GetParamArray(name: String): Variant;
begin
  Result := ToVarArray(getParamList(name), RawText);
end;

//************************** TResponse **************************/

constructor TResponse.Create(response: TIdHTTPResponseInfo);
begin
  Self.response := response;
  response.ResponseNo := 200;
  response.CacheControl := 'max-age=0, must-revalidate';
  response.CustomHeaders.Values['Access-Control-Allow-Origin'] := '*';
  response.ContentType := 'application/x-www-form-urlencoded';
  content := TStringList.Create;
end;

procedure TResponse.SetParamStr(name: String; value: String);
begin
  content.Values[name] := value;
end;

procedure TResponse.SetParamInt(name: String; value: Integer);
begin
  SetParamStr(name, IntToStr(value));
end;

procedure TResponse.SetParamBool(name: String; value: Boolean);
begin
  SetParamStr(name, BoolToStr(value));
end;

procedure TResponse.SetParamVariant(name: String; value: Variant);
begin
  if VarIsNull(value)
  then SetParamStr(name, '')
  else if VarIsArray(value)
  then SetParamStr(name, SerializeArray(value))
  else SetParamStr(name, value);
end;

procedure TResponse.SetException(code: Integer; text: String);
begin
  content.Clear;
  response.ResponseNo := code;
  Param['exception'] := text;
end;

{
  Encodes and copies the response form into the HTTP response.
}
procedure TResponse.Write;
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
  contextManager := TContextManager.Create(DefaultSession);
  handlers := TList.Create;
  logger := TLogger.Create(frmMain.memoServicesLog.Lines);
  
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
      logger.LogException(e);
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
  request: TRequest;
  response: TResponse;
begin
  request := TRequest.Create(httpRequest);
  response := TResponse.Create(httpResponse);
  method := request.Method;
  logger.LogStart(method);
  handler := FindHandler(method);

  if handler = nil
  then begin
    response.SetException(404, 'NotFound');
    logger.Log('Unknown service %s', [method]);
  end else Try
    handler.handler(request, response);
  Except
    on e: Exception do begin
      response.SetException(500, e.Message);
      logger.LogException(e);
    end;
  end;

  logger.LogEnd;
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

procedure TRestServer.ContextManagementRegistry_Locate(request: TRequest; response: TResponse);
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

procedure TRestServer.ContextManager_EndContextChanges(request: TRequest; response: TResponse);
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

procedure TRestServer.ContextManager_Get_MostRecentContextCoupon(request: TRequest; response: TResponse);
var
  contextCoupon: Integer;
begin
  contextCoupon := contextManager.IContextManager_Get_MostRecentcontextCoupon;
  response.ParamInt['contextCoupon'] := contextCoupon;
end;

procedure TRestServer.ContextManager_JoinCommonContext(request: TRequest; response: TResponse);
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

procedure TRestServer.ContextManager_StartContextChanges(request: TRequest; response: TResponse);
var
  participantCoupon: Integer;
  contextCoupon: Integer;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  contextCoupon := contextManager.IContextManager_StartContextChanges(participantCoupon);
  response.ParamInt['contextCoupon'] := contextCoupon;
end;

procedure TRestServer.ContextManager_LeaveCommonContext(request: TRequest; response: TResponse);
var
  participantCoupon: Integer;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  contextManager.IContextManager_LeaveCommonContext(participantCoupon);
end;

procedure TRestServer.ContextManager_PublishChangesDecision(request: TRequest; response: TResponse);
var
  contextCoupon: Integer;
  decision: String;
begin
  contextCoupon := request.ParamInt['contextCoupon'];
  decision := request.Param['decision'];
  contextManager.IContextManager_PublishChangesDecision(contextCoupon, decision);
end;

procedure TRestServer.ContextManager_ResumeParticipation(request: TRequest; response: TResponse);
var
  participantCoupon: Integer;
  wait: Boolean;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  wait := request.ParamBool['wait'];
  contextManager.IContextManager_ResumeParticipation(participantCoupon, wait);
end;

procedure TRestServer.ContextManager_SuspendParticipation(request: TRequest; response: TResponse);
var
  participantCoupon: Integer;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  contextManager.IContextManager_SuspendParticipation(participantCoupon);
end;

procedure TRestServer.ContextManager_UndoContextChanges(request: TRequest; response: TResponse);
var
  contextCoupon: Integer;
begin
  contextCoupon := request.ParamInt['contextCoupon'];
  contextManager.IContextManager_UndoContextChanges(contextCoupon);
end;

//************************** ContextData **************************/

procedure TRestServer.ContextData_GetItemNames(request: TRequest; response: TResponse);
var
  itemNames: OleVariant;
  contextCoupon: Integer;
begin
  contextCoupon := request.ParamInt['contextCoupon'];
  itemNames := contextManager.IContextData_GetItemNames(contextCoupon);
  response.ParamVariant['names'] := itemNames;
end;

procedure TRestServer.ContextData_GetItemValues(request: TRequest; response: TResponse);
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

procedure TRestServer.ContextData_SetItemValues(request: TRequest; response: TResponse);
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

procedure TRestServer.SecureContextData_GetItemNames(request: TRequest; response: TResponse);
var
  itemNames: String;
  contextCoupon: Integer;
begin
  contextCoupon := request.ParamInt['contextCoupon'];
  itemNames := SerializeArray(contextManager.ISecureContextData_GetItemNames(contextCoupon));
  response.Param['names'] := itemNames;
end;

procedure TRestServer.SecureContextData_GetItemValues(request: TRequest; response: TResponse);
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

procedure TRestServer.SecureContextData_SetItemValues(request: TRequest; response: TResponse);
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

procedure TRestServer.ContextFilter_GetSubjectsOfInterest(request: TRequest; response: TResponse);
var
  participantCoupon: Integer;
  subjectNames: OleVariant;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  subjectNames := contextManager.IContextFilter_GetSubjectsOfInterest(participantCoupon);
  response.ParamVariant['subjectNames'] := subjectNames;
end;

procedure TRestServer.ContextFilter_ClearFilter(request: TRequest; response: TResponse);
var
  participantCoupon: Integer;
begin
  participantCoupon := request.ParamInt['participantCoupon'];
  contextManager.IContextFilter_ClearFilter(participantCoupon);
end;

procedure TRestServer.ContextFilter_SetSubjectsOfInterest(request: TRequest; response: TResponse);
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
  logger.Log('CCOW services available on port %d', [httpServer.DefaultPort]);
end;

end.
