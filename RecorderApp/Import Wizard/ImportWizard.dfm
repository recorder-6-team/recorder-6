inherited frmImportWizard: TfrmImportWizard
  Left = 466
  Top = 192
  Width = 735
  Height = 545
  Caption = 'Import Wizard'
  Color = clWhite
  Constraints.MinHeight = 450
  Constraints.MinWidth = 500
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar [0]
    Left = 0
    Top = 499
    Width = 727
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 50
      end>
    ParentColor = True
    ParentFont = True
    SizeGrip = False
    UseSystemFont = False
  end
  object pnlButtons: TPanel [1]
    Left = 0
    Top = 465
    Width = 727
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object pnlButtons2: TPanel
      Left = 300
      Top = 0
      Width = 427
      Height = 34
      Align = alRight
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 1
      DesignSize = (
        427
        34)
      object btnReturn: TBitBtn
        Left = 8
        Top = 6
        Width = 89
        Height = 23
        Hint = 'Return to ... page'
        Caption = '&Return'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Visible = False
        OnClick = btnReturnClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000770606007F18
          18009B1212009E27270099333300A03D3D00A5424200A74F4F00AD5E5E00B567
          6700B9777700FF00FF00C68C8C00CE9C9C00DEB5B500FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFF1FFFFFFFFFFFFFFF11FFFFFFFFFFFFFF161FFFFFFFF1111116
          61FFFFFF1DDCCA99691FFFF1EEEEDCCA9661FF1EEEEEEECCA91FF1111111111C
          C1FFF08740FFFF1C1FFFF08870FFFF11FFFFF0CCC0FFFF1FFFFFF0ECC0FFFFFF
          FFFFF1EED1FFFFFFFFFFF11111FFFFFFFFFFFFFFFFFFFFFFFFFF}
        Spacing = 10
      end
      object btnPrev: TBitBtn
        Left = 128
        Top = 6
        Width = 89
        Height = 23
        Anchors = [akTop, akRight]
        Caption = '&Previous'
        TabOrder = 1
        OnClick = btnPrevClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000770606007F18
          18009B1212009E27270099333300A03D3D00A5424200A74F4F00AD5E5E00B567
          6700B9777700FF00FF00C68C8C00CE9C9C00DEB5B500FFFFFF00BFFFFFFFFFFF
          FFFBFC111111110000AFF14443333222220FF46543333CC3220FF5765443CFC5
          320FF577555CFFC7720FF68755CFFFC7721FF7887CFFFFC7731FF7987CFFFFC7
          731FF79887CFFFC7531FF7A9877CFFC6541FF8A98877CFC6541FF9CAA9877CC6
          531FFADCA9877755310FFEA98776543311CFBFFFFFFFFFFFFFFB}
        Spacing = 10
      end
      object btnNext: TBitBtn
        Left = 224
        Top = 6
        Width = 89
        Height = 23
        Anchors = [akTop, akRight]
        Caption = '&Next'
        TabOrder = 2
        OnClick = btnNextClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000770606007F18
          18009B1212009E27270099333300A03D3D00A5424200A74F4F00AD5E5E00B567
          6700B9777700FF00FF00C68C8C00CE9C9C00DEB5B500FFFFFF00BFFFFFFFFFFF
          FFFBFA000011111111CFF02222233334441FF0223CC33334564FF0235CFC3445
          675FF0277CFFC555775FF1277CFFFC55786FF1377CFFFFC7887FF1377CFFFFC7
          897FF1357CFFFC78897FF1456CFFC7789A7FF1456CFC77889A8FF1356CC7789A
          AC9FF0135577789ACDAFFC11334567789AEFBFFFFFFFFFFFFFFB}
        Layout = blGlyphRight
        Spacing = 20
      end
      object btnCancel: TBitBtn
        Left = 336
        Top = 6
        Width = 85
        Height = 23
        Anchors = [akTop, akRight]
        Caption = 'Cancel'
        TabOrder = 3
        OnClick = btnCancelClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000084000000
          8C000000940000009C000000A5000000AD000000B5000000BD000000C6000000
          CE000000D6000000DE000000E700FF00FF00FFFFFF00FFFFFF00EEEEEEEEEEEE
          EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE87EEEEE
          EEEEEEEE9875EE10EEEEEEEEE876E3211EEEEEEEEE86543EEEEEEEEEEEE765EE
          EEEEEEEEEE98766EEEEEEEEEEBA9E8776EEEEEEEECBEEE88EEEEEEEEEECEEEEE
          EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE}
        Spacing = 10
      end
    end
    object btnSaveTemplate: TBitBtn
      Left = 8
      Top = 6
      Width = 125
      Height = 23
      Caption = 'Save &Template...'
      TabOrder = 0
      OnClick = btnSaveTemplateClick
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000000000000000000010000000100000005A2121008418
        1800842121008C29290084313100A5313100BD4A4A00FF00FF00945A5A00AD62
        5A00A4868600BDA8A800DAB9B900D3D0D000EFEFEF00FFFFFF00777777777777
        777777044BBBBBB2244779664A9BFED2264779664B5ADFE2264779664C38BDF2
        264779664CABABC2264779666666666666477966AAAAAAAA6647796AFFFFFFFF
        A647796AFBBBBBBFA647796AFFFFFFFFA647796AFBBBBBBFA647796AFFFFFFFF
        A647794ABBBBBBBBA47777777777777777777777777777777777}
    end
  end
  object hvHelp: THTMLViewer [2]
    Left = 0
    Top = 0
    Width = 240
    Height = 465
    OnHotSpotClick = hvHelpHotSpotClick
    TabOrder = 2
    Align = alLeft
    DefBackground = clWhite
    BorderStyle = htNone
    HistoryMaxCount = 0
    DefFontName = 'Arial'
    DefPreFontName = 'Courier New'
    DefFontSize = 9
    NoSelect = False
    ScrollBars = ssVertical
    CharSet = DEFAULT_CHARSET
    PrintMarginLeft = 2.000000000000000000
    PrintMarginRight = 2.000000000000000000
    PrintMarginTop = 2.000000000000000000
    PrintMarginBottom = 2.000000000000000000
    PrintScale = 1.000000000000000000
  end
  object pnlData: TPanel [3]
    Left = 240
    Top = 0
    Width = 487
    Height = 465
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 3
    object pnlPages: TPanel
      Left = 0
      Top = 0
      Width = 487
      Height = 465
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 4
      Color = clWhite
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
    end
  end
  inherited mnuChildMerge: TMainMenu
    Tag = 999
  end
end
