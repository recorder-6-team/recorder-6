inherited fraColumnTypes: TfraColumnTypes
  Width = 470
  Height = 429
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 470
    Height = 125
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      470
      125)
    object lblErrors: TLabel
      Left = 332
      Top = 36
      Width = 6
      Height = 13
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 268
      Top = 36
      Width = 60
      Height = 13
      Caption = 'Errors found:'
    end
    object Label4: TLabel
      Left = 12
      Top = 12
      Width = 28
      Height = 13
      Caption = 'Data'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Shape1: TShape
      Left = 48
      Top = 19
      Width = 411
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object chkShowErrors: TCheckBox
      Left = 266
      Top = 56
      Width = 127
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Show rows with &errors:'
      Enabled = False
      TabOrder = 2
      OnClick = chkShowErrorsClick
    end
    object chkFirstRow: TCheckBox
      Left = 28
      Top = 36
      Width = 181
      Height = 17
      Hint = 'Specify if data includes column names in first row.'
      Alignment = taLeftJustify
      Caption = '&First row contains column names:'
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 0
      OnClick = chkFirstRowClick
    end
    object btnRemove: TBitBtn
      Left = 12
      Top = 88
      Width = 157
      Height = 23
      Caption = 'Remove Selected Rows'
      Enabled = False
      TabOrder = 3
      OnClick = btnRemoveClick
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000000000000000000010000000100000001500A0001600
        AC001A00C8001D00E0001F00EE002C0DFF004429FF006E59FF008A79FF009383
        FF009889FF00AA9DFF00C5BDFF00CBC3FF00E1DDFF00FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFF1111111111FFFFFF1CB8866321FFFFFF1ECB88663
        1FFFFFF1111111111FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    end
    object chkShowSelected: TCheckBox
      Left = 28
      Top = 56
      Width = 181
      Height = 17
      Alignment = taLeftJustify
      Caption = '&Show selected rows:'
      Enabled = False
      TabOrder = 1
      OnClick = chkShowSelectedClick
    end
  end
  object dbgData: TImportWizardDBGrid
    Left = 0
    Top = 125
    Width = 470
    Height = 304
    Align = alClient
    Color = clWhite
    DataSource = DataSource
    Options = [dgEditing, dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnCellClick = dbgDataCellClick
    OnKeyDown = dbgDataKeyDown
    ComboDropDownCount = 11
    OnComboChange = ColumnComboChange
    OnComboDrawItem = ColumnComboDrawItem
    OnGetSelectedCellHighlight = dbgDataGetSelectedCellHighlight
    OnPopulateCombo = dbgDataPopulateCombo
    OnSelectedRowsChanged = GridSelectedRowsChanged
  end
  object DataSource: TDataSource
    Left = 408
    Top = 92
  end
end
