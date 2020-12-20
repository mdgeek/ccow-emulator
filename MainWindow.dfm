object frmMain: TfrmMain
  Left = 946
  Top = 129
  Width = 700
  Height = 454
  Caption = 'CCOW Context'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object splitterMain: TSplitter
    Left = 0
    Top = 200
    Width = 692
    Height = 3
    Cursor = crVSplit
    Align = alTop
    Beveled = True
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 692
    Height = 200
    Align = alTop
    TabOrder = 0
    object splitterTop: TSplitter
      Left = 451
      Top = 1
      Height = 198
      Beveled = True
    end
    object grpActivityLog: TGroupBox
      Left = 1
      Top = 1
      Width = 450
      Height = 198
      Align = alLeft
      Caption = 'Activity Log'
      TabOrder = 0
      object memoActivityLog: TMemo
        Left = 2
        Top = 15
        Width = 446
        Height = 181
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object grpParticipants: TGroupBox
      Left = 454
      Top = 1
      Width = 237
      Height = 198
      Align = alClient
      Caption = 'Participants'
      TabOrder = 1
      object lbParticipants: TListBox
        Left = 2
        Top = 15
        Width = 233
        Height = 181
        Align = alClient
        Columns = 2
        ItemHeight = 13
        TabOrder = 0
        TabWidth = 15
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 203
    Width = 692
    Height = 217
    Align = alClient
    TabOrder = 1
    object splitterBottom: TSplitter
      Left = 351
      Top = 1
      Height = 215
      Beveled = True
    end
    object grpCurrentContext: TGroupBox
      Left = 1
      Top = 1
      Width = 350
      Height = 215
      Hint = 'Current Context'
      Align = alLeft
      Caption = 'Current Context'
      TabOrder = 0
      object memoCurrentContext: TMemo
        Left = 2
        Top = 15
        Width = 346
        Height = 198
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object grpPendingContext: TGroupBox
      Left = 354
      Top = 1
      Width = 337
      Height = 215
      Hint = 'Pending Context'
      Align = alClient
      Caption = 'Pending Context'
      TabOrder = 1
      object memoPendingContext: TMemo
        Left = 2
        Top = 15
        Width = 333
        Height = 198
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
end
