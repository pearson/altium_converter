object progressDlg: TProgressBarForm
  Left = 800
  Top = 500
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Please wait...'
  ClientHeight = 80
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'sans-serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object statusLabel: TLabel
    Left = 10
    Top = 10
    Width = 480
    Height = 24
    AutoSize = False
    Caption = 'Working'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'sans-serif'
    Font.Style = []
    ParentFont = False
  end
  object ProgressBar: TProgressBar
    Left = 10
    Top = 40
    Width = 480
    Height = 17
    Max = 1000
    Smooth = True
    TabOrder = 0
  end
end
