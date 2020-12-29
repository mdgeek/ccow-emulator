object frmMain: TfrmMain
  Left = 454
  Top = 169
  Width = 738
  Height = 617
  Caption = 'CCOW Emulator'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultSizeOnly
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pages: TPageControl
    Left = 0
    Top = 0
    Width = 730
    Height = 564
    Align = alClient
    TabOrder = 0
  end
  object statusBar: TStatusBar
    Left = 0
    Top = 564
    Width = 730
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
end
