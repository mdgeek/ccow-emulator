unit ContextException;

interface

uses
  Classes, SysUtils, Windows, SessionForm;

type
  {
    An exception that includes a result code.
  }
  TContextException = class(Exception)
  private
    FCode: HRESULT;
  public
    constructor Create(text: String; code: HRESULT);
    property Code: HRESULT read FCode;
  end;

const
  E_FAIL = HRESULT($80004005);
  E_TRANSACTION_IN_PROGRESS = HRESULT($80000209);
  E_NOT_IN_TRANSACTION = HRESULT($80000207);
  E_INVALID_TRANSACTION = HRESULT($80000211);
  E_INVALID_CONTEXT_COUPON = HRESULT($80000203);
  E_UNKNOWN_PARTICIPANT = HRESULT($8000020B);
  E_ACCEPT_NOT_POSSIBLE = HRESULT($8000020D);
  E_FILTER_NOT_SET = HRESULT($80000225);

implementation

{
  Creates an exception with the specified text and result code.
}
constructor TContextException.Create(text: String; code: HRESULT);
begin
  inherited Create(text);
  FCode := code;
end;

end.
