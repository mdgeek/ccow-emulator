unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, SessionForm;

type
  TfrmMain = class(TForm)
    pages: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  public
    function CreateSession(sessionId: Integer): TSessionForm;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  CCOW_TLB;
  
var
  contextManager: IContextManager;

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
  if contextManager = nil
  then contextManager := CoContextManager.Create;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  contextManager := nil;
end;

end.
