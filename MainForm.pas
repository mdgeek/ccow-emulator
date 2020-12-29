unit MainForm;

interface

uses
  SysUtils, Forms, ComServ, SessionForm, ComCtrls, Classes, Controls;

type
  TfrmMain = class(TForm)
    pages: TPageControl;
    statusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
  private
    procedure SetStatus(status: String);
  public
    function CreateSession(sessionId: Integer): TSessionForm;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnLastRelease(var Shutdown: Boolean);
    property Status: String write SetStatus;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

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
end;

procedure TfrmMain.SetStatus(status: String);
begin
  statusBar.Panels[0].Text := status;
end;

procedure TfrmMain.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent is TTabSheet) and (pages.PageCount = 0)
  then Close;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ComServer.OnLastRelease := OnLastRelease;
end;

procedure TfrmMain.OnLastRelease(var Shutdown: Boolean);
begin
  Shutdown := False;
end;

end.
