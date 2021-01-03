unit Logger;

interface

uses
  Classes, Windows, SysUtils, StrUtils, Common;
  
type
  TLogger = class
  private
    logger: TStrings;
    logStack: TStrings;
    FMaxLines: Integer;
    function LogIndent: String;
  public
    constructor Create(logger: TStrings);
    procedure Clear;
    procedure Log(text: String); overload;
    procedure Log(text: String; params: array of const); overload;
    procedure LogException(e: Exception);
    procedure LogActivity(participant: PParticipant; activity: String); overload;
    procedure LogActivity(context: PContext; activity: String); overload;
    procedure LogStart(method: String);
    procedure LogEnd;
    property MaxLines: Integer read FMaxLines write FMaxLines;
  end;

implementation

const
  LOG_INDENT = '> > > > > > > > > > > > > > > > > > > > ';

{ TLogger }

constructor TLogger.Create(logger: TStrings);
begin
  FMaxLines := 250;
  Self.logger := logger;
  logStack := TStringList.Create;
end;

procedure TLogger.Log(text: String);
begin
  while logger.Count >= FMaxLines do
    logger.Delete(0);

  logger.Add(LogIndent + text);
end;

procedure TLogger.Log(text: String; params: array of const);
begin
  Log(Format(text, params));
end;

{
  Logs an exception,
}
procedure TLogger.LogException(e: Exception);
begin
  Log('An error occured: %s', [e.Message]);
end;

{
  Returns a prefix string for the current indent level.
}
function TLogger.LogIndent: String;
begin
  Result := RightStr(LOG_INDENT, logStack.Count * 2);
end;

{
  Logs a participant activity.
}
procedure TLogger.LogActivity(participant: PParticipant; activity: String);
begin
  if participant <> nil
  then begin
    Log('%s [pc#%d] %s', [participant^.title, participant^.participantCoupon, activity]);
  end;
end;

{
  Logs a context activity.
}
procedure TLogger.LogActivity(context: PContext; activity: String);
var
  participant: PParticipant;
begin
  participant := context^.participant;
  Log('%s [pc#%d] %s [cc#%d]', [participant^.title, participant^.participantCoupon,
    activity, context^.contextCoupon]);
end;

{
  Logs the start of a method invocation.
}
procedure TLogger.LogStart(method: String);
begin
  Log('Entering %s [thread#%d]', [method, GetCurrentThreadId]);
  logStack.Add(method);
end;

{
` Logs the end of a method invocation.
}
procedure TLogger.LogEnd();
var
  method: String;
  i: Integer;
begin
  i := logStack.Count - 1;

  if i >= 0
  then begin
    method := logStack[i];
    logStack.Delete(i);
    Log('Exited %s', [method]);
  end else LogException(Exception.Create('Log end past end of stack'));

end;

procedure TLogger.Clear;
begin
  logger.Clear;
  logStack.Clear;
end;

end.
