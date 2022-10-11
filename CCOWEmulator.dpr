program CCOWEmulator;
uses
  Forms,
  Common in 'Common.pas',
  ContextSession in 'ContextSession.pas',
  SessionForm in 'SessionForm.pas' {frmSession},
  ContextManager in 'ContextManager.pas' {ContextManager: CoClass},
  MainForm in 'MainForm.pas' {frmMain},
  ServerModule in 'ServerModule.pas' {RestServer: TDataModule},
  Participant in 'Participant.pas',
  ContextException in 'ContextException.pas',
  Logger in 'Logger.pas',
  CCOW_TLB in 'CCOW_TLB.pas';

{$R *.TLB}
{$R *.res}
begin
  Application.Initialize;
  Application.Title := 'CCOW Emulator';
  Application.CreateForm(TfrmMain, frmMain);
  DefaultSession := TContextSession.Create;
  Application.CreateForm(TRestServer, RestServer);
  Application.Run;
end.
