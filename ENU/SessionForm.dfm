object SessionForm: TSessionForm
  Left = 758
  Top = 68
  Align = alClient
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  ClientHeight = 420
  ClientWidth = 692
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
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
      Left = 351
      Top = 1
      Height = 198
      Beveled = True
    end
    object grpActivityLog: TGroupBox
      Left = 1
      Top = 1
      Width = 350
      Height = 198
      Align = alLeft
      Caption = 'Activity Log'
      TabOrder = 0
      object memoActivityLog: TMemo
        Left = 2
        Top = 15
        Width = 346
        Height = 181
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Microsoft Sans Serif'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object grpParticipants: TGroupBox
      Left = 354
      Top = 1
      Width = 337
      Height = 198
      Align = alClient
      Caption = 'Participants'
      TabOrder = 1
      object lvParticipants: TListView
        Left = 2
        Top = 15
        Width = 333
        Height = 181
        Align = alClient
        Columns = <
          item
            Caption = '#'
            Width = 30
          end
          item
            Alignment = taCenter
            AutoSize = True
            Caption = 'Title'
            MinWidth = 100
          end
          item
            Alignment = taCenter
            Caption = 'Survey'
            Width = 55
          end
          item
            Alignment = taCenter
            Caption = 'Suspend'
            Width = 55
          end>
        TabOrder = 0
        ViewStyle = vsReport
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
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Microsoft Sans Serif'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
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
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
end
