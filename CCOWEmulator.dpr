program CCOWEmulator;

uses
  Forms,
  MainWindow in 'MainWindow.pas' {frmMain},
  CCOW_TLB in 'CCOW_TLB.pas',
  ContextManager in 'ContextManager.pas' {ContextManager: CoClass},
  ContextManagerService in 'ContextManagerService.pas';

{$R *.TLB}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'CCOW Emulator';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
