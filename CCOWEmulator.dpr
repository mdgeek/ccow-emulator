program CCOWEmulator;

uses
  Forms,
  SessionForm in 'SessionForm.pas',
  CCOW_TLB in 'CCOW_TLB.pas',
  ContextManager in 'ContextManager.pas' {ContextManager: CoClass},
  ContextSession in 'ContextSession.pas',
  Common in 'Common.pas',
  MainForm in 'MainForm.pas' {frmMain};

{$R *.TLB}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'CCOW Emulator';
  Application.CreateForm(TfrmMain, frmMain);
  DefaultSession := TContextSession.Create;
  Application.Run;
end.
