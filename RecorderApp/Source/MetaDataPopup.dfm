object dlgMetaDataPopup: TdlgMetaDataPopup
  Left = 326
  Top = 224
  Width = 392
  Height = 371
  BorderIcons = [biSystemMenu]
  Caption = 'Metadata'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    384
    344)
  PixelsPerInch = 96
  TextHeight = 13
  object pnlHTMLViewer: TPanel
    Left = 6
    Top = 6
    Width = 372
    Height = 303
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object hvMetadata: THTMLViewer
      Left = 2
      Top = 2
      Width = 368
      Height = 299
      Cursor = 2
      TabOrder = 0
      Align = alClient
      PopupMenu = pmCopy
      DefBackground = clWhite
      BorderStyle = htFocused
      HistoryMaxCount = 0
      DefFontName = 'Times New Roman'
      DefPreFontName = 'Courier New'
      NoSelect = False
      CharSet = DEFAULT_CHARSET
      PrintMarginLeft = 2.000000000000000000
      PrintMarginRight = 2.000000000000000000
      PrintMarginTop = 2.000000000000000000
      PrintMarginBottom = 2.000000000000000000
      PrintScale = 1.000000000000000000
      htOptions = []
    end
  end
  object pnlButton: TPanel
    Left = 0
    Top = 312
    Width = 384
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      384
      32)
    object btnClose: TButton
      Left = 303
      Top = 2
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Close'
      Default = True
      TabOrder = 0
      OnClick = btnCloseClick
    end
  end
  object pmCopy: TPopupMenu
    Left = 57
    Top = 57
    object mnuCopy: TMenuItem
      Caption = '&Copy'
      ShortCut = 16451
      OnClick = mnuCopyClick
    end
  end
end
