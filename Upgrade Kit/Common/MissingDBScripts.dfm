object dlgMissingScripts: TdlgMissingScripts
  Left = 519
  Top = 286
  BorderStyle = bsDialog
  Caption = 'Missing Database Upgrade Scripts'
  ClientHeight = 163
  ClientWidth = 393
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
  object lblErrorInfo: TLabel
    Left = 12
    Top = 12
    Width = 359
    Height = 65
    Caption = 
      'A different upgrade is required to upgrade your version of Recor' +
      'der. Please refer to the Recorder website (see below) for detail' +
      's of upgrades available. Note that Recorder must be upgraded to ' +
      'the latest version using the correct upgrade (or series of upgra' +
      'des run in order) and that Recorder 2002 cannot be upgraded to R' +
      'ecorder 6.'
    WordWrap = True
  end
  object lblLink: THotLabel
    Left = 108
    Top = 92
    Width = 160
    Height = 13
    Cursor = crHandPoint
    Caption = 'http://www.recordersoftware.org/'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
    OnClick = lblLinkClick
    HotColor = clPurple
  end
  object lblWebAddress: TLabel
    Left = 12
    Top = 92
    Width = 86
    Height = 13
    Caption = 'Recorder website:'
  end
  object btnOk: TButton
    Left = 156
    Top = 128
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
end
