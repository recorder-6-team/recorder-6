object frmMain: TfrmMain
  Left = 465
  Top = 207
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Recorder / Dictionary Upgrade'
  ClientHeight = 349
  ClientWidth = 496
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object imgBackdrop: TImage
    Left = 0
    Top = 0
    Width = 496
    Height = 312
    AutoSize = True
  end
  object btnCancel: TBitBtn
    Left = 414
    Top = 318
    Width = 75
    Height = 25
    Caption = '&Cancel'
    TabOrder = 0
    OnClick = btnCancelClick
  end
  object btnProceed: TBitBtn
    Left = 330
    Top = 318
    Width = 75
    Height = 25
    Caption = '&Proceed'
    TabOrder = 1
    OnClick = btnProceedClick
  end
end
