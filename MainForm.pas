unit MainForm;

interface

uses
  SysUtils, Forms, ComServ, SessionForm, ComCtrls, Classes, Controls;

type
  TfrmMain = class(TForm)
    pages: TPageControl;
    status: TStatusBar;
    procedure FormCreate(Sender: TObject);
  public
    function CreateSession(sessionId: Integer): TSessionForm;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnLastRelease(var Shutdown: Boolean);
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
