unit ServerModule;

interface

uses
  SysUtils, Classes, StrUtils, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdCustomHTTPServer, IdHTTPServer, IdContext, IdTCPServer, IdUri, Common, CCOW_TLB,
  IdTCPConnection, IdTCPClient;

type
  THandlerProc = procedure(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) of object;

  THandler = class
    method: String;
    handler: THandlerProc;
    constructor Create(method: String; handler: THandlerProc);
  end;

  TRestServer = class(TDataModule)
    httpServer: TIdHTTPServer;
    procedure httpServerCommandGet(context: TIdContext;
      request: TIdHTTPRequestInfo;
      response: TIdHTTPResponseInfo);
    procedure DataModuleCreate(Sender: TObject);
  private
    handlers: TList;
    procedure AddHandler(intf: String; method: String; handler: THandlerProc);
    function FindHandler(params: TStrings): THandler;

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

  end;

var
  RestServer: TRestServer;

implementation

{$R *.dfm}

uses
  ContextManager, ContextSession, Participant;

const
  INTF_CONTEXT_MANAGEMENT_REGISTRY = 'ContextManagementRegistry';
  INTF_CONTEXT_MANAGER = 'ContextManager';
  INTF_CONTEXT_DATA = 'ContextData';

var
  ContextManager: TContextManager;

//************************** Utility Methods **************************/

function GetParameter(paramName: String; request: TIdHTTPRequestInfo; required: Boolean): String;
begin
  Result := request.Params.Values[paramName];
  Assert(Not(required) or (Result <> ''), 'A required parameter (%s) is missing.', [paramName]);
end;

function GetIntParameter(paramName: String; request: TIdHTTPRequestInfo; required: Boolean): Integer;
var
  s: String;
begin
  s := GetParameter(paramName, request, required);

  if s = ''
  then Result := 0
  else Result := StrToInt(s);
end;

function GetBooleanParameter(paramName: String; request: TIdHTTPRequestInfo; required: Boolean): Boolean;
var
  s: String;
begin
  s := AnsiLowerCase(GetParameter(paramName, request, required));
  Result := s = 'true';
end;

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

procedure SaveForm(response: TIdHTTPResponseInfo; form: TStrings);
begin
  response.ContentText := EncodeForm(form);
  response.ContentType := 'application/x-www-form-urlencoded';

end;

//************************** THandler **************************/

constructor THandler.Create(method: String; handler: THandlerProc);
begin
  Self.method := AnsiLowerCase(method);
  Self.handler := handler;
end;

//************************** TRestServer **************************/

procedure TRestServer.httpServerCommandGet(context: TIdContext;
  request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  handler: THandler;
begin
  handler := FindHandler(request.Params);
  response.CacheControl := 'max-age=0, must-revalidate';

  if handler = nil
  then begin
    response.ResponseNo := 404;
    response.ContentText := 'error=Bad Request';
  end else Try
    response.ResponseNo := 200;
    response.ContentType := 'application/x-www-form-urlencoded';
    handler.handler(request, response);
  Except
    on e: Exception do begin
      response.ResponseNo := 500;
      response.ContentText := e.Message;
    end;
  end;

  response.WriteHeader;
  response.WriteContent;
end;

procedure TRestServer.DataModuleCreate(Sender: TObject);
begin
  ContextManager := TContextManager.Create(DefaultSession);
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
end;

procedure TRestServer.AddHandler(intf: String; method: String; handler: THandlerProc);
begin
  handlers.Add(THandler.Create(intf + '::' + method, handler));
end;

function TRestServer.FindHandler(params: TStrings): THandler;
var
  i: Integer;
  h: THandler;
  method: String;
begin
  Result := nil;
  method := AnsiLowerCase(params.Values['interface'] + '::' + params.Values['method']);

  for i := 0 to handlers.Count - 1 do
  begin
    h := THandler(handlers[i]);

    if h.method = method
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
begin
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
end;

procedure TRestServer.ContextManager_StartContextChanges(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  contextCoupon: Integer;
  form: TStrings;
begin
  form := TStringList.Create;
end;

procedure TRestServer.ContextManager_LeaveCommonContext(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  contextCoupon: Integer;
  form: TStrings;
begin
  form := TStringList.Create;

end;

procedure TRestServer.ContextManager_PublishChangesDecision(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  contextCoupon: Integer;
  form: TStrings;
begin
  form := TStringList.Create;

end;

procedure TRestServer.ContextManager_ResumeParticipation(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  contextCoupon: Integer;
  form: TStrings;
begin
  form := TStringList.Create;

end;

procedure TRestServer.ContextManager_SuspendParticipation(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  contextCoupon: Integer;
  form: TStrings;
begin
  form := TStringList.Create;

end;

procedure TRestServer.ContextManager_UndoContextChanges(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  contextCoupon: Integer;
  form: TStrings;
begin
  form := TStringList.Create;

end;

//************************** ContextData **************************/

procedure TRestServer.ContextData_GetItemNames(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  itemNames: String;
  contextCoupon: Integer;
  form: TStrings;
begin
  contextCoupon := GetIntParameter('contextCoupon', request, True);
  itemNames := EncodeAsArray(ContextManager.IContextData_GetItemNames(contextCoupon));
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
  itemValues := EncodeAsArray(ContextManager.IContextData_GetItemValues(itemNames, onlyChanges, contextCoupon));
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

end.
