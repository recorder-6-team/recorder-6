inherited fraFileSelect: TfraFileSelect
  Width = 501
  Height = 809
  object pnlDelimiters: TPanel
    Left = 0
    Top = 158
    Width = 501
    Height = 176
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 2
    DesignSize = (
      501
      176)
    object lblQualifier: TLabel
      Left = 299
      Top = 111
      Width = 65
      Height = 13
      Caption = 'Text &Qualifier:'
    end
    object lblRecSeparator: TLabel
      Left = 299
      Top = 87
      Width = 87
      Height = 13
      Caption = '&Record Separator:'
    end
    object lblDelimiterSection: TLabel
      Left = 12
      Top = 12
      Width = 105
      Height = 13
      Caption = 'Delimiter selection'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object shpDelimiterSeparator: TShape
      Left = 124
      Top = 19
      Width = 372
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object btnLess: TBitBtn
      Left = 376
      Top = 152
      Width = 85
      Height = 21
      Caption = 'L&ess'
      TabOrder = 7
      Visible = False
      OnClick = btnMoreLessClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF000000000000000000000000000000000000000000FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
        0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF000000000000000000000000000000000000000000FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
        0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      Layout = blGlyphRight
      Spacing = 10
    end
    object btnMore: TBitBtn
      Left = 377
      Top = 152
      Width = 85
      Height = 21
      Caption = '&More'
      TabOrder = 6
      OnClick = btnMoreLessClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000
        0000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000
        0000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      Layout = blGlyphRight
      Spacing = 10
    end
    object rgSeparator: TRadioGroup
      Left = 44
      Top = 76
      Width = 235
      Height = 85
      Caption = ' Delimiters '
      Columns = 2
      Ctl3D = False
      ItemIndex = 0
      Items.Strings = (
        'Tab'
        'Semicolon (;)'
        'Comma (,)'
        'Space'
        'Other Symbol:')
      ParentCtl3D = False
      TabOrder = 2
      OnClick = rbDelimitersClick
    end
    object eSymbol: TEdit
      Left = 183
      Top = 133
      Width = 33
      Height = 19
      Ctl3D = False
      MaxLength = 1
      ParentCtl3D = False
      TabOrder = 3
      OnChange = DataChanged
    end
    object cmbTextQualifier: TComboBox
      Left = 392
      Top = 106
      Width = 70
      Height = 21
      BevelKind = bkFlat
      Ctl3D = False
      ItemHeight = 13
      ItemIndex = 1
      ParentCtl3D = False
      TabOrder = 5
      Text = '"'
      OnChange = DataChanged
      Items.Strings = (
        ' '
        '"'
        #39)
    end
    object cmbRecSeparator: TComboBox
      Left = 392
      Top = 82
      Width = 70
      Height = 21
      BevelKind = bkFlat
      Style = csDropDownList
      Ctl3D = False
      ItemHeight = 13
      ItemIndex = 0
      ParentCtl3D = False
      TabOrder = 4
      Text = 'CRLF'
      OnChange = DataChanged
      Items.Strings = (
        'CRLF'
        'CR'
        'LF')
    end
    object rbDelimited: TRadioButton
      Left = 26
      Top = 54
      Width = 380
      Height = 17
      Caption = 
        '&Delimited - Characters such as comma or tab separate each filed' +
        '.'
      Checked = True
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 1
      TabStop = True
      OnClick = rbDelimitersClick
    end
    object rbFixedWidth: TRadioButton
      Left = 26
      Top = 32
      Width = 380
      Height = 17
      Caption = 
        'Fixed &Width - Fields are aligned in columns with spaces between' +
        ' each field.'
      TabOrder = 0
      OnClick = rbFixedWidthClick
    end
  end
  object pnlFormatting: TPanel
    Left = 0
    Top = 334
    Width = 501
    Height = 229
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 3
    Visible = False
    DesignSize = (
      501
      229)
    object lblAdvanced: TLabel
      Left = 12
      Top = 12
      Width = 107
      Height = 13
      Caption = 'Formatting Options'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object shpAdvanced: TShape
      Left = 124
      Top = 19
      Width = 372
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object gbDateTime: TGroupBox
      Left = 28
      Top = 137
      Width = 217
      Height = 85
      Caption = ' Dates and Times '
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 1
      object lblDateOrder: TLabel
        Left = 16
        Top = 28
        Width = 61
        Height = 13
        Caption = 'Date &Format:'
      end
      object lblDateDelimiter: TLabel
        Left = 16
        Top = 56
        Width = 69
        Height = 13
        Caption = 'Date De&limiter:'
        FocusControl = eDateDelimiter
      end
      object eDateDelimiter: TEdit
        Left = 92
        Top = 52
        Width = 21
        Height = 19
        Ctl3D = False
        MaxLength = 1
        ParentCtl3D = False
        TabOrder = 1
        Text = '/'
        OnChange = DataChanged
      end
      object cmbDateFormat: TComboBox
        Left = 92
        Top = 24
        Width = 97
        Height = 21
        BevelKind = bkFlat
        Ctl3D = False
        ItemHeight = 13
        ParentCtl3D = False
        TabOrder = 0
        Text = 'dd/mm/yyyy'
        OnChange = DataChanged
        Items.Strings = (
          'dd/mm/yyyy'
          'dd/mmmm/yyyy'
          'dd/mm/yy'
          'dd/mmm/yy'
          'mm/dd/yyyy'
          'mmm/dd/yyyy'
          'mm/dd/yy'
          'mmm/dd/yy'
          'yyyy/mm/dd')
      end
    end
    object gbNumbers: TGroupBox
      Left = 264
      Top = 137
      Width = 198
      Height = 85
      Caption = ' Numbers '
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 2
      object lblDecimalSymbol: TLabel
        Left = 20
        Top = 28
        Width = 78
        Height = 13
        Caption = 'Decimal Sym&bol:'
        FocusControl = eDecimalSymbol
      end
      object lblThousandSymbol: TLabel
        Left = 20
        Top = 56
        Width = 100
        Height = 13
        Caption = '&Thousand Separator:'
        FocusControl = eThousandSymbol
      end
      object eDecimalSymbol: TEdit
        Left = 132
        Top = 24
        Width = 25
        Height = 19
        Ctl3D = False
        MaxLength = 1
        ParentCtl3D = False
        TabOrder = 0
        Text = '.'
        OnChange = DataChanged
      end
      object eThousandSymbol: TEdit
        Left = 132
        Top = 52
        Width = 25
        Height = 19
        Ctl3D = False
        MaxLength = 1
        ParentCtl3D = False
        TabOrder = 1
        Text = ','
        OnChange = DataChanged
      end
    end
    object gbImportedRows: TGroupBox
      Left = 28
      Top = 32
      Width = 434
      Height = 97
      Caption = 'Imported Rows'
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
      object lblRowsFrom: TLabel
        Left = 48
        Top = 68
        Width = 20
        Height = 13
        Caption = 'from'
      end
      object lblRowsTo: TLabel
        Left = 152
        Top = 68
        Width = 9
        Height = 13
        Caption = 'to'
      end
      object rbImportAllRows: TRadioButton
        Left = 16
        Top = 20
        Width = 113
        Height = 17
        Caption = 'Import all rows'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbImportRowsClick
      end
      object rbImportSelectedRows: TRadioButton
        Left = 16
        Top = 44
        Width = 245
        Height = 17
        Caption = 'Import the following rows only'
        TabOrder = 1
        OnClick = rbImportRowsClick
      end
      object eRowsFrom: TNumberEdit
        Left = 72
        Top = 64
        Width = 69
        Height = 19
        TabOrder = 2
        OnChange = DataChanged
        Maximum = 9999
      end
      object eRowsTo: TNumberEdit
        Left = 168
        Top = 64
        Width = 69
        Height = 19
        TabOrder = 3
        OnChange = DataChanged
        Maximum = 9999
      end
    end
  end
  object pnlTemplate: TPanel
    Left = 0
    Top = 89
    Width = 501
    Height = 69
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 1
    DesignSize = (
      501
      69)
    object lblTemplateSection: TLabel
      Left = 12
      Top = 12
      Width = 53
      Height = 13
      Caption = 'Template'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblUseTemplate: TLabel
      Left = 28
      Top = 36
      Width = 84
      Height = 13
      Caption = 'Use this template:'
    end
    object shpTemplateSeparator: TShape
      Left = 72
      Top = 19
      Width = 419
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object cmbTemplate: TComboBox
      Left = 116
      Top = 32
      Width = 346
      Height = 21
      BevelKind = bkFlat
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Ctl3D = False
      ItemHeight = 13
      ItemIndex = 0
      ParentCtl3D = False
      TabOrder = 0
      Text = '<none>'
      OnChange = cmbTemplateChange
      Items.Strings = (
        '<none>')
    end
  end
  object pnlTableSelect: TPanel
    Left = 0
    Top = 563
    Width = 501
    Height = 204
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 4
    Visible = False
    DesignSize = (
      501
      204)
    object lblDataTableSection: TLabel
      Left = 12
      Top = 12
      Width = 64
      Height = 13
      Caption = 'Data Table'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object shpDataTableSeparator: TShape
      Left = 84
      Top = 19
      Width = 412
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object lblSelectTable: TLabel
      Left = 28
      Top = 36
      Width = 322
      Height = 13
      Caption = 
        'Please select the table that will be used for import from the li' +
        'st below:'
    end
    object lbTables: TListBox
      Left = 28
      Top = 56
      Width = 401
      Height = 134
      ItemHeight = 13
      Items.Strings = (
        'ADDRESS'
        'BIOTOPE_DETERMINATION'
        'BIOTOPE_OCCURRENCE'
        'BIOTOPE_OCCURRENCE_DATA'
        'CONTACT_NUMBER'
        'GRID_SQUARE'
        'INDIVIDUAL'
        'LOCATION'
        'LOCATION_NAME'
        'MEASUREMENT_QUALIFIER'
        'NAME'
        'RECORD_TYPE'
        'REFERENCE'
        'SAMPLE'
        'SOURCE'
        'SURVEY'
        'SURVEY_EVENT'
        'TAXON_DETERMINATION'
        'TAXON_OCCURRENCE'
        'TAXON_OCCURRENCE_DATA')
      TabOrder = 0
      OnClick = DataChanged
    end
  end
  object pnlSurvey: TPanel
    Left = 0
    Top = 0
    Width = 501
    Height = 89
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      501
      89)
    object lblSurveyName: TLabel
      Left = 76
      Top = 60
      Width = 36
      Height = 13
      Caption = '&Survey:'
    end
    object shpSurveySeparator: TShape
      Left = 60
      Top = 19
      Width = 413
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object lblSurvey: TLabel
      Left = 12
      Top = 12
      Width = 40
      Height = 13
      Caption = 'Survey'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSelectSurvey: TLabel
      Left = 20
      Top = 36
      Width = 293
      Height = 13
      Caption = 'Please select the survey that will receive the imported records.'
    end
    object cmbSurvey: TIDComboBox
      Left = 116
      Top = 56
      Width = 346
      Height = 21
      BevelKind = bkFlat
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Ctl3D = False
      ItemHeight = 13
      ParentCtl3D = False
      TabOrder = 0
      OnChange = DataChanged
      OnPopulate = cmbSurveyPopulate
    end
  end
end
