unit Participant;

interface

uses
  Classes, SysUtils, ComObj, CCOW_TLB, IdHTTP;

type

  TParticipant = class(TAutoObject, IContextParticipant)
  private
    client: TIdHttp;
    url: String;
    function Execute(method: String; params: TStrings): TStrings;
  public
    constructor Create(url: String);
    function ContextChangesPending(contextCoupon: Integer; var reason: WideString): WideString; safecall;
    procedure ContextChangesAccepted(contextCoupon: Integer); safecall;
    procedure ContextChangesCanceled(contextCoupon: Integer); safecall;
    procedure CommonContextTerminated; safecall;
    procedure Ping; safecall;
  end;

implementation

uses
  ServerModule, Common;

constructor TParticipant.Create(url: String);
begin
  Self.url := url;
  client := TIdHttp.Create(RestServer);
end;

function TParticipant.ContextChangesPending(contextCoupon: Integer; var reason: WideString): WideString;
const
  method = 'ContextChangesPending';
var
  params: TStrings;
  form: TStrings;
begin
  params := TStringList.Create;
  params.Values['contextCoupon'] := IntToStr(contextCoupon);
  form := Execute(method, params);
  reason := form.Values['reason'];
  Result := form.Values['decision'];
end;

procedure TParticipant.ContextChangesAccepted(contextCoupon: Integer);
const
  method = 'ContextChangesAccepted';
var
  params: TStrings;
begin
  params := TStringList.Create;
  params.Values['contextCoupon'] := IntToStr(contextCoupon);
  Execute(method, params);
end;

procedure TParticipant.ContextChangesCanceled(contextCoupon: Integer);
const
  method = 'ContextChangesCanceled';
var
  params: TStrings;
begin
  params := TStringList.Create;
  params.Values['contextCoupon'] := IntToStr(contextCoupon);
  Execute(method, params);
end;

procedure TParticipant.CommonContextTerminated;
const
  method = 'CommonContextTerminated';
begin
  Execute(method, nil);
end;

procedure TParticipant.Ping;
const
  method = 'Ping';
begin
  Execute(method, nil);
end;

function TParticipant.Execute(method: String; params: TStrings): TStrings;
var
  fullUrl: String;
begin
  fullUrl := Format('%s?interface=ContextParticipant&method=%s&%s',
    [url, method, encodeForm(params)]);
  Result := DecodeForm(client.Get(fullUrl));
end;

end.
