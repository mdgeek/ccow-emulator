program CCOWEmulator;

uses
  Forms,
  Common in 'Common.pas',
  ContextSession in 'ContextSession.pas',
  CCOW_TLB in 'CCOW_TLB.pas',
  SessionForm in 'SessionForm.pas' {frmSession},
  ContextManager in 'ContextManager.pas' {ContextManager: CoClass},
  MainForm in 'MainForm.pas' {frmMain},
  ServerModule in 'ServerModule.pas' {RestServer: TDataModule};

{$R *.TLB}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'CCOW Emulator';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TRestServer, RestServer);
  DefaultSession := TContextSession.Create;
  Application.Run;
end.
