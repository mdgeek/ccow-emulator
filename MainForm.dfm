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
    Height = 583
    ActivePage = tabServices
    Align = alClient
    TabOrder = 0
    object tabServices: TTabSheet
      Caption = 'Services'
      object memoServicesLog: TMemo
        Left = 0
        Top = 0
        Width = 722
        Height = 555
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
end
