program CCOWEmulator;

uses
  Forms,
  Common in 'Common.pas',
  ContextSession in 'ContextSession.pas',
  CCOW_TLB in 'CCOW_TLB.pas',
  SessionForm in 'SessionForm.pas',
  ContextManager in 'ContextManager.pas' {ContextManager: CoClass},
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
