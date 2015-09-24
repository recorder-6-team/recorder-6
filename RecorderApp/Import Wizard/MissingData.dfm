inherited fraMissingData: TfraMissingData
  Width = 542
  Height = 628
  object pnlObserver: TPanel
    Left = 0
    Top = 334
    Width = 542
    Height = 81
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 3
    DesignSize = (
      542
      81)
    object lblObserverName: TLabel
      Left = 84
      Top = 51
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object lblSingleObserver: TLabel
      Left = 12
      Top = 4
      Width = 91
      Height = 13
      Caption = 'Single Observer'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblNoObserverColumn: TLabel
      Left = 20
      Top = 28
      Width = 475
      Height = 13
      Caption = 
        'No "Observer" column was identified.  Please enter the name to u' +
        'se as the observer for ALL records.'
    end
    object shpObserverSeparator: TShape
      Left = 108
      Top = 11
      Width = 406
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object eObserver: TNameLinkedEdit
      Tag = 2
      Left = 120
      Top = 48
      Width = 305
      Height = 23
      TabOrder = 0
      BorderStyle = bsNone
      ImageIndex = 5
      ImageList = dmFormActions.ilButtons
      OnChange = DataChange
      OnFindData = NameFindData
      OnGetData = NameGetData
    end
  end
  object pnlLocation: TPanel
    Left = 0
    Top = 89
    Width = 542
    Height = 164
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    Constraints.MinWidth = 425
    TabOrder = 1
    DesignSize = (
      542
      164)
    object lblSingleLocation: TLabel
      Left = 12
      Top = 4
      Width = 89
      Height = 13
      Caption = 'Single Location'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSelectLocation: TLabel
      Left = 20
      Top = 28
      Width = 405
      Height = 26
      Caption = 
        'No location details columns were identified.  Please enter data ' +
        'in at least one of these fields to use as the location for ALL r' +
        'ecords.'
      WordWrap = True
    end
    object shpLocationSeparator: TShape
      Left = 108
      Top = 11
      Width = 406
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    inline fraLocationInfo: TfraLocationInfo
      Left = 38
      Top = 60
      Width = 411
      Height = 88
      HorzScrollBar.Visible = False
      VertScrollBar.Visible = False
      TabOrder = 0
      inherited eSpatialRef: TSpatialRef
        Width = 275
        Height = 31
        Ctl3D = False
        OnChange = DataChange
      end
      inherited eLocationName: TEdit
        Width = 275
        Height = 19
        Ctl3D = False
        OnChange = DataChange
      end
      inherited eLocation: TAddinLinkedEdit
        Width = 275
        Height = 21
        Ctl3D = False
        ParentCtl3D = False
        OnChange = DataChange
      end
    end
  end
  object pnlSurvey: TPanel
    Left = 0
    Top = 0
    Width = 542
    Height = 89
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      542
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
      Width = 454
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
      Left = 120
      Top = 56
      Width = 305
      Height = 21
      BevelKind = bkFlat
      Style = csDropDownList
      Ctl3D = False
      ItemHeight = 13
      ParentCtl3D = False
      TabOrder = 0
      OnChange = DataChange
      OnPopulate = cmbSurveyPopulate
    end
  end
  object pnlTermLists: TPanel
    Left = 0
    Top = 538
    Width = 542
    Height = 63
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 5
    DesignSize = (
      542
      63)
    object shpOtherValuesSeparator: TShape
      Left = 96
      Top = 11
      Width = 418
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object lblOtherValues: TLabel
      Left = 12
      Top = 4
      Width = 74
      Height = 13
      Caption = 'Other Values'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSelectValues: TLabel
      Left = 20
      Top = 28
      Width = 269
      Height = 13
      Caption = 'Please select the following values to use for ALL records.'
    end
  end
  object pnlDate: TPanel
    Left = 0
    Top = 253
    Width = 542
    Height = 81
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 2
    DesignSize = (
      542
      81)
    object shpDateSeparator: TShape
      Left = 45
      Top = 11
      Width = 469
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object lblDate: TLabel
      Left = 12
      Top = 4
      Width = 28
      Height = 13
      Caption = 'Date'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSelectDate: TLabel
      Left = 20
      Top = 28
      Width = 215
      Height = 13
      Caption = 'Please select the date to use for ALL records.'
    end
    object lblDateValue: TLabel
      Left = 86
      Top = 50
      Width = 26
      Height = 13
      Caption = 'Date:'
    end
    object eDate: TVagueDateEdit
      Left = 120
      Top = 48
      Width = 221
      Height = 19
      Ctl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
      OnChange = DataChange
      OnExit = eDateExit
    end
  end
  object pnlDeterminer: TPanel
    Left = 0
    Top = 415
    Width = 542
    Height = 123
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 4
    DesignSize = (
      542
      123)
    object lblDeterminerName: TLabel
      Left = 84
      Top = 95
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object lblDeterminer: TLabel
      Left = 12
      Top = 4
      Width = 62
      Height = 13
      Caption = 'Determiner'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblNoDeterminerColumn: TLabel
      Left = 20
      Top = 28
      Width = 185
      Height = 13
      Caption = 'No "Determiner" column was identified.'
    end
    object shpDeterminerSeparator: TShape
      Left = 80
      Top = 11
      Width = 434
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object eDeterminer: TNameLinkedEdit
      Tag = 2
      Left = 120
      Top = 92
      Width = 305
      Height = 23
      TabOrder = 2
      BorderStyle = bsNone
      ImageIndex = 5
      ImageList = dmFormActions.ilButtons
      OnChange = DataChange
      OnFindData = NameFindData
      OnGetData = NameGetData
    end
    object rbFirstObserverAsDeterminer: TRadioButton
      Left = 66
      Top = 48
      Width = 217
      Height = 17
      Caption = 'Use the first Observer as the determiner'
      TabOrder = 0
      OnClick = DataChange
    end
    object rbSpecifyDeterminer: TRadioButton
      Left = 66
      Top = 72
      Width = 305
      Height = 17
      Caption = 'Use the following person as the determiner for ALL records'
      TabOrder = 1
      OnClick = DataChange
    end
  end
end
