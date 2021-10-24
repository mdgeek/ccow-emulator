unit Participant;

interface

uses
  Classes, SysUtils, ComObj, CCOW_TLB;

type

  {
    Used for web participants.  Doesn't do anything at the moment.  With
    a websocket implementation, could be made fully functional.
  }
  TWebParticipant = class(TAutoObject, IContextParticipant)
  private
    url: String;
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
  Common;

constructor TWebParticipant.Create(url: String);
begin
  Self.url := url;
end;

function TWebParticipant.ContextChangesPending(contextCoupon: Integer; var reason: WideString): WideString;
begin
  reason := '';
  Result := '';
end;

procedure TWebParticipant.ContextChangesAccepted(contextCoupon: Integer);
begin
end;

procedure TWebParticipant.ContextChangesCanceled(contextCoupon: Integer);
begin
end;

procedure TWebParticipant.CommonContextTerminated;
begin
end;

procedure TWebParticipant.Ping;
begin
end;

end.
