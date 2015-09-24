object frmMain: TfrmMain
  Left = 390
  Top = 314
  AutoSize = True
  BorderStyle = bsNone
  Caption = 'Select Installation Type'
  ClientHeight = 222
  ClientWidth = 556
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 556
    Height = 222
    AutoSize = True
    BevelOuter = bvNone
    BevelWidth = 3
    BorderWidth = 2
    Color = clBlack
    TabOrder = 0
    object imgLogo: TImage
      Left = 2
      Top = 2
      Width = 552
      Height = 218
      AutoSize = True
      OnMouseMove = imgLogoMouseMove
    end
    object lblNetwork: THotLabel
      Left = 395
      Top = 118
      Width = 151
      Height = 16
      Cursor = crHandPoint
      Caption = 'Install on network server'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold, fsItalic]
      ParentColor = False
      ParentFont = False
      Transparent = True
      OnClick = lblNetworkClick
      OnMouseMove = lblStandaloneMouseMove
      HotColor = clMaroon
    end
    object lblStandalone: THotLabel
      Left = 392
      Top = 96
      Width = 154
      Height = 16
      Cursor = crHandPoint
      Caption = 'Install on single machine'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold, fsItalic]
      ParentColor = False
      ParentFont = False
      Transparent = True
      OnClick = lblStandaloneClick
      OnMouseMove = lblStandaloneMouseMove
      HotColor = clMaroon
    end
    object lblGettingStarted: THotLabel
      Left = 412
      Top = 140
      Width = 134
      Height = 16
      Cursor = crHandPoint
      Caption = 'Getting Started Guide'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold, fsItalic]
      ParentColor = False
      ParentFont = False
      Transparent = True
      OnClick = lblGettingStartedClick
      OnMouseMove = lblStandaloneMouseMove
      HotColor = clMaroon
    end
    object lblBrowse: THotLabel
      Left = 456
      Top = 162
      Width = 90
      Height = 16
      Cursor = crHandPoint
      Caption = 'Browse the CD'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold, fsItalic]
      ParentColor = False
      ParentFont = False
      Transparent = True
      OnClick = lblBrowseClick
      OnMouseMove = lblStandaloneMouseMove
      HotColor = clMaroon
    end
    object lblExit: THotLabel
      Left = 517
      Top = 192
      Width = 24
      Height = 16
      Cursor = crHandPoint
      Caption = 'Exit'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold, fsItalic]
      ParentColor = False
      ParentFont = False
      Transparent = True
      OnClick = lblExitClick
      OnMouseMove = lblStandaloneMouseMove
      HotColor = clMaroon
    end
  end
end
