unit MainWindow;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls,
  ContextManagerService, CCOW_TLB;

type
  TfrmMain = class(TForm)
    grpActivityLog: TGroupBox;
    grpCurrentContext: TGroupBox;
    grpParticipants: TGroupBox;
    grpPendingContext: TGroupBox;
    lbParticipants: TListBox;
    memoActivityLog: TMemo;
    memoCurrentContext: TMemo;
    memoPendingContext: TMemo;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    splitterBottom: TSplitter;
    splitterMain: TSplitter;
    splitterTop: TSplitter;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  private
    contextManager: IContextManager;

    procedure SetCurrentContext(context: PContext);
    procedure SetPendingContext(context: PContext);
    procedure SetContextItems(memo: TMemo; context: PContext);
    procedure SetContextCaption(grp: TGroupBox; context: PContext);

  public
    property CurrentContext: PContext write SetCurrentContext;
    property PendingContext: PContext write SetPendingContext;

    procedure AddParticipant(participant: PParticipant);
    procedure RemoveParticipant(participant: PParticipant);

    procedure Log(text: String);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.Log(text: String);
begin
  memoActivityLog.Lines.Add(text);
end;

procedure TfrmMain.AddParticipant(participant: PParticipant);
begin
  RemoveParticipant(participant);
  lbParticipants.AddItem(IntToStr(participant^.participantCoupon)
    + #9 + participant^.title, TObject(participant));
end;

procedure TfrmMain.RemoveParticipant(participant: PParticipant);
var
  i: Integer;
begin
  i := lbParticipants.Items.IndexOfObject(TObject(participant));

  if i >= 0
  then lbParticipants.Items.Delete(i);
end;

procedure TfrmMain.SetCurrentContext(context: PContext);
begin
  SetContextItems(memoCurrentContext, context);
  SetContextCaption(grpCurrentContext, context);
end;

procedure TfrmMain.SetPendingContext(context: PContext);
begin
  SetContextItems(memoPendingContext, context);
  SetContextCaption(grpPendingContext, context);
end;

procedure TfrmMain.SetContextItems(memo: TMemo; context: PContext);
begin
  memo.Lines.Clear;

  if context <> nil
  then memo.Lines.AddStrings(context^.contextItems);
end;

procedure TfrmMain.SetContextCaption(grp: TGroupBox; context: PContext);
begin
  if context = nil
  then grp.Caption := grp.Hint
  else grp.Caption := grp.Hint + ' (' + IntToStr(context^.contextCoupon) + ')';
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Service.Shutdown;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Service := TContextManagerService.Create;
  contextManager := CoContextManager.Create;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  contextManager := nil;
end;

end.
