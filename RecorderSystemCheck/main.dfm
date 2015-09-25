object frmMain: TfrmMain
  Left = 488
  Top = 148
  Width = 534
  Height = 627
  Caption = 'Recorder System Checker'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object report: TRichEdit
    Left = 0
    Top = 0
    Width = 518
    Height = 548
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    Lines.Strings = (
      'Recorder 6 System Check Report'
      '')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 548
    Width = 518
    Height = 41
    Align = alBottom
    TabOrder = 1
    Visible = False
    DesignSize = (
      518
      41)
    object btnAttemptFixes: TButton
      Left = 416
      Top = 8
      Width = 91
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Attempt Fixes'
      TabOrder = 0
      OnClick = btnAttemptFixesClick
    end
  end
end
