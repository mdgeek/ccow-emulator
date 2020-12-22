unit SessionForm;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Common;

type
  TSessionForm = class(TForm)
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);

  private
    procedure SetCurrentContext(context: PContext);
    procedure SetPendingContext(context: PContext);
    procedure SetContextItems(memo: TMemo; context: PContext);
    procedure SetContextCaption(grp: TGroupBox; context: PContext);

  public
    property CurrentContext: PContext write SetCurrentContext;
    property PendingContext: PContext write SetPendingContext;

    procedure AddParticipant(participant: PParticipant);
    procedure RemoveParticipant(participant: PParticipant);

    procedure Log(text: String); overload;
    procedure Log(text: String; params: array of const); overload;
  end;

var
  frmSession: TSessionForm;

implementation

uses MainForm;

{$R *.dfm}

procedure TSessionForm.Log(text: String);
begin
  memoActivityLog.Lines.Add(text);
end;

procedure TSessionForm.Log(text: String; params: array of const);
begin
  Log(Format(text, params));
end;

procedure TSessionForm.AddParticipant(participant: PParticipant);
begin
  RemoveParticipant(participant);
  lbParticipants.AddItem(IntToStr(participant^.participantCoupon)
    + #9 + participant^.title, TObject(participant));
end;

procedure TSessionForm.RemoveParticipant(participant: PParticipant);
var
  i: Integer;
begin
  i := lbParticipants.Items.IndexOfObject(TObject(participant));

  if i >= 0
  then lbParticipants.Items.Delete(i);
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
  else grp.Caption := grp.Hint + ' (' + IntToStr(context^.contextCoupon) + ')';
end;

procedure TSessionForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Parent.Free;
  Action := caHide;
end;

procedure TSessionForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if lbParticipants.Items.Count > 0
  then begin
    if MessageDlg('Closing this tab will terminate all active participants.  Are you sure?', mtWarning, [mbYes, mbCancel], 0) = mrCancel
    then CanClose := False;
  end;
end;

procedure TSessionForm.FormCreate(Sender: TObject);
begin
  grpCurrentContext.Width := frmMain.ClientWidth div 2;
  grpActivityLog.Width := frmMain.ClientWidth div 3 * 2;
  pnlTop.Height := frmMain.ClientHeight div 2;
end;

end.
