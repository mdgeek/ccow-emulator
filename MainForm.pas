unit MainForm;

interface

uses
  SysUtils, Forms, ComServ, SessionForm, ComCtrls, Classes, Controls,
  StdCtrls;

type
  TfrmMain = class(TForm)
    pages: TPageControl;
  public
    function CreateSession(sessionId: Integer): TSessionForm;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  ServerModule;

function TfrmMain.CreateSession(sessionId: Integer): TSessionForm;
var
  sessionForm: TSessionForm;
  tab: TTabSheet;
begin
  tab := TTabSheet.Create(pages);
  tab.PageControl := pages;
  tab.Caption := 'Session #' + IntToStr(sessionId);
  tab.FreeNotification(Self);
  sessionForm := TSessionForm.Create(tab);
  sessionForm.Caption := tab.Caption;
  sessionForm.Parent := tab;
  sessionForm.Show;
  Result := sessionForm;
  pages.ActivePage := tab;
end;

end.
