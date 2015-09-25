inherited BaseMatch: TBaseMatch
  Width = 904
  Height = 419
  OnResize = dbgMatchDisplayChanged
  object pnlControls: TPanel
    Left = 0
    Top = 73
    Width = 904
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 1
    object pnlBtnSearch: TPanel
      Left = 0
      Top = 0
      Width = 145
      Height = 48
      Align = alLeft
      BevelOuter = bvNone
      Caption = 'srch'
      Color = clWhite
      TabOrder = 0
      object btnSearch: TBitBtn
        Left = 12
        Top = 12
        Width = 133
        Height = 23
        Hint = 'Browse habitats and biotopes using the Biotope Dictionary'
        Caption = '&Search'
        Enabled = False
        TabOrder = 0
        OnClick = btnSearchClick
        Glyph.Data = {
          36050000424D3605000000000000360400002800000010000000100000000100
          0800000000000001000000000000000000000001000000010000000000000000
          80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
          A600C69C8C006B636B00737373009C7373007B7B7B00AD848400B58C8400C69C
          84008C8C8C00BD948C00C69C8C00CEA58C00D6AD9400DEB59C00A57B7B00D6B5
          A500E7BDA500F7CEA500FFD6A500F7D6AD00FFDEAD00E7CEB500FFE7B500FFEF
          BD00EFE7C600FFF7C600F7EFCE00FFFFCE00FFFFD600DE8C4200FFF7E700FFFF
          E700FFC8A600FED68300FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
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
          FDFDFDFDFDFDFD0B0BFDFDFDFDFDFDFDFDFDFDFDFDFD0B18270BFDFDFDFDFDFD
          FDFDFDFDFD0B18272B2AFDFDFDFDFDFDFDFDFDFD0B18272B2AFDFDFDFDFDFDFD
          FDFDFD0B18272B2AFDFDFDFDFD120B0B0B0B0B18272B2AFDFDFDFDFD0B0C191F
          16110E272B2AFDFDFDFDFD0B0F28262625231D0F2AFDFDFDFDFD120B242C2C28
          262321090AFDFDFDFDFD0B0A262C2C2826231E230AFDFDFDFDFD0B1626292829
          25231E250AFDFDFDFDFD0B1625262625252120230AFDFDFDFDFD121023232323
          211E26250AFDFDFDFDFDFD0D1D211E1E21282C220AFDFDFDFDFDFDFD131C2321
          2528240AFDFDFDFDFDFDFDFDFD120A0A0A0A0AFDFDFDFDFDFDFD}
        Spacing = 10
      end
    end
    object pnlBtnNew: TPanel
      Left = 145
      Top = 0
      Width = 145
      Height = 48
      Align = alLeft
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 1
      object btnNew: TBitBtn
        Left = 12
        Top = 12
        Width = 133
        Height = 23
        Caption = '&Make New Entry'
        Enabled = False
        TabOrder = 0
        OnClick = btnNewClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000CE630000CE6B
          0800D6731000D67B1800D67F2900D6843100DE8C3900DE944A00DE9C5600E7AA
          6D00E7B57B00EFC8A300F7DEBD00F9E6D400FFF7EF00FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1111FFFFFFFFFFFF1531FF
          FFFFFFFFFF1551FFFFFFFFF1111751111FFFFFF1CAA775531FFFFFF1DCAA7755
          1FFFFFF1111AA1111FFFFFFFFF1CA1FFFFFFFFFFFF1DC1FFFFFFFFFFFF1111FF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        Spacing = 10
      end
    end
    object pnlBtnNewEntries: TPanel
      Left = 290
      Top = 0
      Width = 145
      Height = 48
      Align = alLeft
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 2
      object btnNewEntries: TBitBtn
        Left = 12
        Top = 12
        Width = 133
        Height = 23
        Caption = '&Make New Entries'
        Enabled = False
        TabOrder = 0
        OnClick = btnNewEntriesClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000CE630000CE6B
          0800D6731000D67B1800D67F2900D6843100DE8C3900DE944A00DE9C5600E7AA
          6D00E7B57B00EFC8A300F7DEBD00F9E6D400FFF7EF00FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1111FFFFFFFFFFFF1531FF
          FFFFFFFFFF1551FFFFFFFFF1111751111FFFFFF1CAA775531FFFFFF1DCAA7755
          1FFFFFF1111AA1111FFFFFFFFF1CA1FFFFFFFFFFFF1DC1FFFFFFFFFFFF1111FF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        Spacing = 10
      end
    end
    object pnlBtnCommit: TPanel
      Left = 435
      Top = 0
      Width = 145
      Height = 48
      Align = alLeft
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 3
      object btnCommit: TBitBtn
        Left = 12
        Top = 12
        Width = 133
        Height = 23
        Caption = '&Commit Matches'
        Enabled = False
        TabOrder = 0
        OnClick = btnCommitClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000000000000000000010000000100000005A2121008418
          1800842121008C29290084313100A5313100BD4A4A00FF00FF00945A5A00AD62
          5A00A4868600BDA8A800DAB9B900D3D0D000EFEFEF00FEFEFE00777777777777
          777777044BBBBBB2244779664A9BFED2264779664B5ADFE2264779664C38BDF2
          264779664CABABC2264779666666666666477966AAAAAAAA6647796AFFFFFFFF
          A647796AFBBBBBBFA647796AFFFFFFFFA647796AFBBBBBBFA647796AFFFFFFFF
          A647794ABBBBBBBBA47777777777777777777777777777777777}
        Spacing = 10
      end
    end
    object pnlBtnExcludeUnmatched: TPanel
      Left = 580
      Top = 0
      Width = 145
      Height = 48
      Align = alLeft
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 4
      object btnExcludeUnmatched: TBitBtn
        Left = 12
        Top = 12
        Width = 133
        Height = 23
        Hint = 
          'Exclude unmatched records from the import process and save them ' +
          'to a file'
        Caption = '&Exclude Unmatched'
        TabOrder = 0
        OnClick = btnExcludeUnmatchedClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000000000000000000010000000100000001500A0001600
          AC001A00C8001D00E0001F00EE002C0DFF004429FF006E59FF008A79FF009383
          FF009889FF00AA9DFF00C5BDFF00CBC3FF00E1DDFF00FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFF1111111111FFFFFF1CB8866321FFFFFF1ECB88663
          1FFFFFF1111111111FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        Spacing = 10
      end
    end
  end
  object dbgMatch: TImportWizardDBGrid
    Left = 0
    Top = 121
    Width = 904
    Height = 298
    Align = alClient
    Color = clWhite
    Ctl3D = False
    DataSource = dsMatchTable
    Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
    ParentCtl3D = False
    PopupMenu = pmBack
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnCellClick = dbgMatchCellClick
    OnColEnter = dbgMatchColEnter
    OnColExit = dbgMatchColExit
    OnDrawColumnCell = dbgMatchDrawColumnCell
    OnKeyDown = dbgMatchKeyDown
    ComboDropDownCount = 8
    OnColWidthsChanged = dbgMatchDisplayChanged
    OnGetCellHint = dbgMatchGetCellHint
    OnTopLeftChanged = dbgMatchDisplayChanged
  end
  object eMatchValue: TAddinLinkedEdit
    Tag = 1
    Left = 84
    Top = 168
    Width = 93
    Height = 23
    OnExit = eMatchValueExit
    TabOrder = 3
    Visible = False
    BorderStyle = bsSingle
    ImageIndex = 5
    ImageList = dmFormActions.ilButtons
    OnFindData = eMatchValueFindData
    OnGetData = eMatchValueGetData
    OnKeyDown = eMatchValueKeyDown
    PopupMenu = pmBack
  end
  object cmbMatchValue: TIDComboBox
    Left = 188
    Top = 168
    Width = 93
    Height = 19
    BevelKind = bkFlat
    Style = csOwnerDrawFixed
    Ctl3D = False
    ItemHeight = 13
    ParentCtl3D = False
    PopupMenu = pmBack
    TabOrder = 4
    Visible = False
    OnChange = cmbMatchValueChange
    OnKeyDown = cmbMatchValueKeyDown
    OnPopulate = cmbMatchValuePopulate
  end
  object pnlCheckList: TPanel
    Left = 0
    Top = 0
    Width = 904
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      904
      73)
    object lblCheckListSearch: TLabel
      Left = 28
      Top = 32
      Width = 128
      Height = 13
      Caption = 'Checklist to use for search:'
    end
    object shpCheckList: TShape
      Left = 70
      Top = 19
      Width = 822
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object lblCheckList: TLabel
      Left = 12
      Top = 12
      Width = 53
      Height = 13
      Caption = 'Checklist'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object cmbCheckLists: TIDComboBox
      Left = 28
      Top = 48
      Width = 864
      Height = 21
      BevelKind = bkFlat
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Ctl3D = False
      ItemHeight = 13
      ParentCtl3D = False
      TabOrder = 0
      OnChange = cmbCheckListsChange
      OnPopulate = cmbCheckListsPopulate
    end
  end
  object pmBack: TPopupMenu
    Left = 16
    Top = 200
    object pmSourceData: TMenuItem
      Caption = '&Show rows containing this data'
      OnClick = pmSourceDataClick
    end
  end
  object dsMatchTable: TDataSource
    DataSet = tblMatch
    OnDataChange = dsMatchTableDataChange
    Left = 144
    Top = 200
  end
  object tblMatch: TADOTable
    Left = 80
    Top = 200
  end
  object dlgSaveUnmatched: TSaveDialog
    DefaultExt = '.csv'
    Filter = 'Comma Separated Variable (*.csv)|*.csv'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 208
    Top = 200
  end
end
