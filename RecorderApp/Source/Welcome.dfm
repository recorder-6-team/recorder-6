object dlgWelcome: TdlgWelcome
  Left = 297
  Top = 209
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Welcome'
  ClientHeight = 431
  ClientWidth = 481
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object pnlQuickStart: TPanel
    Left = 0
    Top = 0
    Width = 91
    Height = 431
    Align = alLeft
    TabOrder = 0
    object llbQuickStartChoices: TLabel
      Left = 14
      Top = 8
      Width = 63
      Height = 13
      Caption = 'Quick start'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object bbSpecies: TBitBtn
      Left = 8
      Top = 28
      Width = 75
      Height = 89
      Hint = 'Enter the list of species from a sample.'
      Caption = 'Species list'
      TabOrder = 0
      OnClick = bbSpeciesClick
      Glyph.Data = {
        36080000424D3608000000000000360400002800000020000000200000000100
        0800000000000004000000000000000000000001000000010000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A600ADADAD0000420000005200000073000063633100319C3100737373008C8C
        8C009C9C9C0000CE9C00ADADAD00CEB5CE00CECECE003163FF00319CFF0000CE
        FF009CCEFF0000FFFF0031FFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FDFDFDFDFDFD
        0C0DFDFDFDFD0CFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD12121212
        0E0C120C120C0D121212120C121212121212121212FDFDFDFDFDFDFD0C120C12
        0C0D0C0E120E0C120C12120D121212121212121212FDFDFDFDFDFD000E000E0C
        0E0C0D0C000C0D0C0E000E0C000000000000121212FDFDFDFDFDFD0E0C1D1D0E
        0C0E0C0E1D0E0C0D0C1D0C0E1D1D1D1D1D00120E12FDFDFDFDFDFD0C0D0C120C
        0E0C0D0C120C0D0C120C0D120E1212121D00120C12FDFDFDFDFDFD0E0C0D0C1D
        0C0E0C0D0C0E0C0E120E0C0E0B1212121D000C0E12FDFDFDFDFDFD000E0C0D0C
        1D1D0E0B0E0C0E1D0E0C0E0B1D1D1D0C1D0C0D0C12FDFDFDFDFDFD0E0C0E0C0E
        0C0E0C0E0B0C0B0E0C0B0B12120E120E0C0D0C1212FDFDFDFDFDFD001D0C0E0C
        0E0C0D0C0C0B0B0B0B1D1D1D0E0C0D0C0D0C0E0C12FDFDFDFDFDFD0E1D121212
        1D120C0E0B0B0B0C1D12120E0C0E0C0E0C0E121212FDFDFDFDFDFD0C0E0C0E0C
        0E0C0D0C0E0B1D0B0E1D0E0C0D0C0E1D1D00121212FDFDFDFDFDFD000C0D0C0D
        0C0D0B0E0B1D1D0E0B1D0C1D1D1D1D1D0C0012120CFDFDFDFDFDFD001D1D1D0C
        0E0B1D0B121D1D1D0E0B0E0C0E0C0E0C0E000E0C0EFDFDFDFDFDFD001D0E0C0E
        0C121212121212120B0D0C0D0C0D0C0D0C0D0C0E12FDFDFDFDFDFD001D1D1D1D
        1D1D1D0C0E0C0E0C0E0B1D1D0E0C0E0C0E0C0E1212FDFDFDFDFDFD001D121212
        0C0E0C0E0C0D0C0E120E0B12121212121D00121212FDFDFDFDFDFD001D1D1D1D
        1D0C0E0C0D0C0E1D1D1D0B1D1D1D0F100F100F1212FDFDFDFDFDFD000C0D0C0D
        0C0D0C0E0C0E1D1D1D1D0B0E0F101B1B1B1B1B1012FDFDFDFDFDFD001D120E0C
        0E0C0E120E121212100F100F0F1B1B1B1B1B1B1B0FFDFDFDFDFDFD001D121212
        120E0C121212100F1B1B1B1B130A0F0A1B1B1B0A1A100FFDFDFDFD001D1D1D1D
        1D0C1D1D1D0F1B1B1B1B1B1B111918190F0A131B1B1B1B10FDFDFD001D1D1D1D
        1D1D1D1D101B1B1B1B1B1B1119181C18190A1B1B1B1B1B1B10FDFD001D121212
        1212121D0F1B1B1B1B0A1319181C181C18131B1B1B1B1B1B1B0FFD001D1D1D1D
        1D1D1D1D101B1B0A131A0A1819181C171C131B1B1B1B1B1B1B10FD001D161616
        161616160F0A131B1B1B0A19171C1719171B130A0F100F100FFDFD001D161616
        161616101B1B1B1B1B1B130F131713171B1B1B0F12FDFDFDFDFDFD001D161616
        1616160F1B1B1B1B1B1B0A1B1B1B1B1B1B1B1B1012FDFDFDFDFDFD001D161616
        161616101B1B1B1B1B0F0F1B1B1B1B1B1B1B0F1212FDFDFDFDFDFD001D1D1D1D
        1D1D1D0F1B1B1B0F101D1D101B1B1B1B1B10FDFDFDFDFDFDFDFDFD0000000000
        00000000100F1000000000000F100F100F00FDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD}
      Layout = blGlyphTop
    end
    object bbSite: TBitBtn
      Left = 8
      Top = 132
      Width = 75
      Height = 89
      Hint = 'Add or edit information about locations.'
      Caption = 'Location'
      TabOrder = 1
      OnClick = bbSiteClick
      Glyph.Data = {
        36080000424D3608000000000000360400002800000020000000200000000100
        080000000000000400000000000000000000000100000001000000000000FF00
        00000099000099993300FFFF00000099990000FFFF0099999900CCCCCC00FFFF
        FF00404040000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000000000000000000000000000000000000000000A0000000000
        0000000000000000000000000000000000000000000000000000000101010101
        0101010101010101010101010101010101010101010101010100000101010101
        0101010101010101010101010101010101010101010101010100000101010101
        0101010101010101000901010101010101010101010101010100000101010101
        0101010101010100070909010101010101010101010101010100000101010101
        0101010101010007070708090101010101010101010101010100000101010101
        0101010101000707070707080901090101010101010101010100000101010101
        0101010000070008090907070909080901010101010101010100000101010101
        0101000900080707080909090709090709010101010101010100000101010808
        0008090909000709080909090909090907090901010101010100000807080800
        0809080809090907090709090707070909080709010101010100000200000207
        0000020007020702070007000707080907070707090901010100000207020202
        0200020202000200000700070702070202080207070708090900000702070202
        0200020202020202020702020202020202020202070207020200000607020702
        0202020202020707050602070202020202060202020702050700000000000000
        0000000000000000000000000000000000000000000000000000000506050207
        0207070207020205060505070202020202050502050205060500000607020702
        0202020202020707050602070202020202060202020702050700000702070202
        0200020202020202020702020202020202020202070207020200000207020202
        0200020202000200000700070702070202080207070708090900000200000207
        0000020007020702070007000707080907070707090903040300000807080800
        0809080809090907090709090707070909080709040304030400000403040808
        0008090909000709080909090909090907090904030403040300000304030403
        0403000900080707080909090709090709030403040304030400000403040304
        0304030000070008090907070909080903040304030403040300000304030403
        0403040304000707070707080903090304030403040304030400000403040304
        0304030403040007070708090304030403040304030403040300000304030403
        0403040304030400070909030403040304030403040304030400000403040304
        0304030403040304000903040304030403040304030403040300000304030403
        0403040304030403040304030403040304030403040304030400000403040304
        0304030403040304030403040304030403040304030403040300000000000000
        0000000000000000000000000000000000000000000000000000}
      Layout = blGlyphTop
    end
    object bbReport: TBitBtn
      Left = 8
      Top = 236
      Width = 75
      Height = 89
      Hint = 'Produce a report.'
      Caption = 'Report'
      TabOrder = 2
      OnClick = bbReportClick
      Glyph.Data = {
        36080000424D3608000000000000360400002800000020000000200000000100
        0800000000000004000000000000000000000001000000010000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A600FFFFFF00FF00000000FF00009C9C9C00CECECE000000FF00FF00FF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00100D0D0D0D0D
        0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D101010101010100D0D0D0D0D
        0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D101010101010000000000000
        00000000000000000000000000000000000D0D0D101010101010000A0A0A0A0A
        0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A000D0D0D101010101010000A0D0D0D0D
        0D0D0D0D0D0D0D0D0D0000000D0D0D0A000D0D0D101010101010000A0D0D0D0D
        0D0D0D0D0D0D0D00000E0D0D00000D0A000D0D0D101010101010000A0A0A0A0A
        0A0A0A0D0A00000E0E0E0D0D0D0D0000000D0D0D101010101010000A0D0A0D0A
        0D0A0A00000E0E0E0E0D00000D0D0D0D00000D0D101010101010000A0A0A0A0A
        0A00000E0E0E0E0D0D0E0D0D00000D0D0D0D0000101010101010000A0D0D0D00
        000E0E0E0E0D0D0E0D0E0D0D0D0D00000D0D0D0D000010101010000A0A0A000E
        0E0E0E0D0D0E0E0E0D0E0D0D0D0D0D0D00000D0D0D0D00001010000A0A0A0D0E
        0E0D0D0E0E0E0E0E0D0E0D0D0D0D0D0D0D0D00000D0D0D0D0010000A0A0A0D0D
        0D0E0E0E0E0E0E0E0D0E0D0D0D0D0D0D0D0D0D0D00000D0D0000000A0D0D0D0E
        0D0E0E0E0E0E0E0E0D0E0D0D0D0D0D0D0D0D0D0D0D0D00000000000A0A0D0A0E
        0D0E0E0E0E0E0E0E0D0A0D0D0D0D0D0D0D0D0D0D0D0D0D0D0000000A0D0D0A0E
        0D0E0E0E0E0E0E0A0D0D0E0E0D0D0D0D0D0D0D0D0D0D0D0D0D00000A0A0D0A0E
        0D0E0E0E0E0A0A0E0E0E0D0D0E0E0D0D0D0D0D0D0D0D0D0D0D00000A0A0D0A0E
        0D0E0E0A0A0E0E0E0E0E0E0E0D0D0E0E0D0D0D0D0D0D0D0D0D00000A0D0D0A0E
        0D0A0A0E0E0E0F0F0E0E0D0D0E0E0D0D0E0E0D0D0D0D0D0D0D00000A0D0D0A0A
        0D0D0E0E0C0C0E0E0D0D0E0E0E0E0E000D0D0E0E0D0D0D0D0D00000A0A0D0D0A
        0E0E0D0D0E0E0D0D0E0E0E0E0E00000A0E000D0D0E0E0D0D0D00000A0A0A0A0D
        0D0A0E0E0D0D0D0D0E0E0E00000A0A0A0A000E0E0D0D0E0E0D00000A0D0D0D0D
        0D0D0D0A0E0E0D0D0D00000A0A0A0B0B0A0E000D0E0E0D0D0E0D000A0A0A0A0A
        0A0A0A0D0D0A0E0E0D0D0A0A0B0B0A0A0A0A000D0D0D0E0D0D10000A0E0E0E0E
        0E0E0E0E0E0D0D0A0E0E0D0A0A0A0A0B0B0A0E000D0D0D101010000A0E0E0E0E
        0E0E0E0E0E0E0E0D0D0A0D0A0A0B0B0A0A0A0A0E001010101010000A0E0E0E0E
        0E0E0E0E0E0E0E0E0E0D0D0D0A0A0A0A0B0B0A0A0E0010101010000A0E0E0E0E
        0E0E0E0E0E0E0E0E0E0E0E0D0A0A0B0B0A0A0A0A0A0E00101010000A0A0A0A0A
        0A0A0A0A0A0A0A0A0A0A0A0A0D0A0A0A0A0A0B0B0A0A0E000010000000000000
        000000000000000000000000000D0A0A0B0B0A0A0A0A0A0D0D00101010101010
        10101010101010101010101010100D0A0A0A0A0A0A0D0D0A0D0D101010101010
        1010101010101010101010101010100D0A0A0A0D0D0A0D0D1010}
      Layout = blGlyphTop
    end
    object ImageListButton1: TImageListButton
      Left = 8
      Top = 396
      Width = 75
      Height = 25
      Caption = '&Ok'
      ModalResult = 1
      TabOrder = 3
      ImageList = dmFormActions.ilButtons
      ImageIndex = 0
    end
  end
  object pnlHTML: TPanel
    Left = 91
    Top = 0
    Width = 390
    Height = 431
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      390
      431)
    object Bevel1: TBevel
      Left = 8
      Top = 400
      Width = 377
      Height = 26
      Anchors = [akLeft, akRight, akBottom]
    end
    object cbShowWelcomeAtStart: TCheckBox
      Left = 16
      Top = 405
      Width = 187
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Show Welcome Window at StartUp'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object pnlHTMLViewer: TPanel
      Left = 8
      Top = 8
      Width = 377
      Height = 386
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvLowered
      TabOrder = 1
      object HTMLViewer: THTMLViewer
        Left = 1
        Top = 1
        Width = 375
        Height = 384
        Cursor = 2
        OnHotSpotClick = HTMLViewerHotSpotClick
        ViewImages = False
        TabOrder = 0
        Align = alClient
        DefBackground = clWhite
        BorderStyle = htSingle
        HistoryMaxCount = 0
        DefFontName = 'Arial'
        DefPreFontName = 'Courier New'
        DefFontSize = 9
        DefOverLinkColor = clRed
        NoSelect = False
        CharSet = DEFAULT_CHARSET
        PrintMarginLeft = 2.000000000000000000
        PrintMarginRight = 2.000000000000000000
        PrintMarginTop = 2.000000000000000000
        PrintMarginBottom = 2.000000000000000000
        PrintScale = 1.000000000000000000
        htOptions = [htOverLinksActive]
      end
    end
  end
  object pmSpecies: TPopupMenu
    Images = dmFormActions.ilMenuOn
    Left = 340
    Top = 276
  end
end
