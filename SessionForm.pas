unit SessionForm;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Common,
  ComCtrls, Logger, UITypes;

type
  TSessionForm = class(TForm)
    grpActivityLog: TGroupBox;
    grpCurrentContext: TGroupBox;
    grpParticipants: TGroupBox;
    grpPendingContext: TGroupBox;
    memoActivityLog: TMemo;
    memoCurrentContext: TMemo;
    memoPendingContext: TMemo;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    splitterBottom: TSplitter;
    splitterMain: TSplitter;
    splitterTop: TSplitter;
    lvParticipants: TListView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);

  private
    FLogger: TLogger;
    procedure SetCurrentContext(context: PContext);
    procedure SetPendingContext(context: PContext);
    procedure SetContextItems(memo: TMemo; context: PContext);
    procedure SetContextCaption(grp: TGroupBox; context: PContext);

  public
    property CurrentContext: PContext write SetCurrentContext;
    property PendingContext: PContext write SetPendingContext;
    procedure AddParticipant(participant: PParticipant);
    procedure RemoveParticipant(participant: PParticipant);
    property Logger: TLogger read FLogger;
  end;

implementation

uses MainForm;

{$R *.dfm}

procedure TSessionForm.AddParticipant(participant: PParticipant);
var
  item: TListItem;
begin
  RemoveParticipant(participant);
  item := lvParticipants.Items.Add;
  item.Data := participant;
  item.Caption := IntToStr(participant^.participantCoupon);
  item.SubItems.Add(participant^.title);
  item.SubItems.Add(BoolToYN(participant^.survey));
  item.SubItems.Add(BoolToYN(participant^.suspended));
end;

procedure TSessionForm.RemoveParticipant(participant: PParticipant);
var
  i: Integer;
begin
  for i := 0 to lvParticipants.Items.Count - 1 do
  begin
    if PParticipant(lvParticipants.Items[i].Data)^.participantCoupon = participant.participantCoupon
    then begin
      lvParticipants.Items.Delete(i);
      break;
    end;
  end;
end;

procedure TSessionForm.SetCurrentContext(context: PContext);
begin
  SetContextItems(memoCurrentContext, context);
  SetContextCaption(grpCurrentContext, context);
end;

procedure TSessionForm.SetPendingContext(context: PContext);
begin
  SetContextItems(memoPendingContext, context);
  SetContextCaption(grpPendingContext, context);
end;

procedure TSessionForm.SetContextItems(memo: TMemo; context: PContext);
begin
  memo.Lines.Clear;

  if context <> nil
  then memo.Lines.AddStrings(context^.contextItems);
end;

procedure TSessionForm.SetContextCaption(grp: TGroupBox; context: PContext);
begin
  if context = nil
  then grp.Caption := grp.Hint
  else grp.Caption := Format('%s [cc#%d]', [grp.Hint, context^.contextCoupon]);
end;

procedure TSessionForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Parent.Free;
  Action := caHide;
end;

procedure TSessionForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if lvParticipants.Items.Count > 0
  then begin
    if MessageDlg('Closing this tab will terminate all active participants.  Are you sure?', mtWarning, [mbYes, mbCancel], 0) = mrCancel
    then CanClose := False;
  end;
end;

procedure TSessionForm.FormCreate(Sender: TObject);
begin
  FLogger := TLogger.Create(memoActivityLog.Lines);
  grpCurrentContext.Width := frmMain.ClientWidth div 2;
  grpActivityLog.Width := frmMain.ClientWidth div 3 * 2;
  pnlTop.Height := frmMain.ClientHeight div 2;
  grpActivityLog.Caption := Format('%s (%d lines maximum)', [grpActivityLog.Caption, FLogger.MaxLines]);
end;

end.
