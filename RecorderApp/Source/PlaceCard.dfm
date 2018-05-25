inherited frmPlaceCard: TfrmPlaceCard
  Left = 445
  Top = 218
  Width = 636
  Height = 520
  Caption = 'Recording Card'
  Constraints.MinWidth = 300
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel [0]
    Left = 0
    Top = 440
    Width = 628
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 2
    object lblTotalSpecies: TLabel
      Left = 324
      Top = 4
      Width = 85
      Height = 29
      AutoSize = False
      Caption = '0 species present.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object shpTaxonAdd: TShape
      Tag = 2
      Left = 88
      Top = 5
      Width = 206
      Height = 24
      Pen.Color = clRed
    end
    object lblAddSpecies: TLabel
      Left = 8
      Top = 10
      Width = 63
      Height = 13
      Caption = 'Add Species:'
    end
    object pnlButtons2: TPanel
      Left = 450
      Top = 0
      Width = 178
      Height = 34
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object bbSave: TImageListButton
        Left = 2
        Top = 6
        Width = 83
        Height = 25
        Hint = 'Save observation details'
        Caption = 'Save'
        TabOrder = 0
        OnClick = bbSaveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
      end
      object bbReset: TImageListButton
        Left = 92
        Top = 6
        Width = 83
        Height = 25
        Hint = 'Clear all fields on the card'
        Caption = 'Reset'
        Enabled = False
        TabOrder = 1
        OnClick = bbResetClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 1
      end
    end
    object eTaxon: TEdit
      Left = 89
      Top = 6
      Width = 204
      Height = 22
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsItalic]
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
      TabOrder = 1
      OnChange = eTaxonChange
      OnEnter = eTaxonEnter
      OnExit = eTaxonExit
      OnKeyPress = eTaxonKeyPress
    end
    object bbFindTaxon: TImageListButton
      Left = 294
      Top = 6
      Width = 23
      Height = 23
      Hint = 'Get taxon'
      TabOrder = 2
      OnClick = bbFindTaxonClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 5
    end
  end
  object pnlHeader: TPanel [1]
    Left = 0
    Top = 0
    Width = 628
    Height = 238
    Align = alTop
    TabOrder = 0
    OnResize = pnlHeaderResize
    DesignSize = (
      628
      238)
    object shpReference: TShape
      Tag = 2
      Left = 74
      Top = 210
      Width = 523
      Height = 23
      Anchors = [akLeft, akRight, akBottom]
      Pen.Color = clRed
    end
    object lblReference: TLabel
      Left = 7
      Top = 215
      Width = 52
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Document:'
    end
    object lblSurvey: TLabel
      Left = 8
      Top = 7
      Width = 36
      Height = 13
      Caption = 'Survey:'
    end
    object eReference: TEdit
      Left = 75
      Top = 211
      Width = 521
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 3
      OnChange = ChangeEditState
      OnKeyPress = eReferenceKeyPress
    end
    object cmbSurvey: TDBListCombo
      Left = 75
      Top = 4
      Width = 545
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      ListField = 'ITEM_NAME'
      KeyField = 'SURVEY_KEY'
      Datasource = dmFormActions.dsSurvey
      Active = False
      EmptyItem = False
      ReadOnly = False
    end
    object bbReferenceFind: TImageListButton
      Left = 597
      Top = 210
      Width = 23
      Height = 23
      Hint = 'Get reference'
      Anchors = [akRight, akBottom]
      TabOrder = 4
      OnClick = bbReferenceFindClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 5
    end
    object pnlLeft: TPanel
      Left = 8
      Top = 27
      Width = 295
      Height = 180
      Anchors = [akLeft, akTop, akBottom]
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        295
        180)
      object shpRecorders: TShape
        Tag = 2
        Left = 66
        Top = 110
        Width = 205
        Height = 70
        Anchors = [akLeft, akTop, akRight, akBottom]
        Pen.Color = clRed
      end
      object lblRecorders: TLabel
        Left = 0
        Top = 115
        Width = 58
        Height = 13
        Caption = 'Recorder(s):'
      end
      object lblSampleType: TLabel
        Left = 0
        Top = 89
        Width = 65
        Height = 13
        Caption = 'Sample Type:'
      end
      object lbRecorders: TListBox
        Left = 67
        Top = 111
        Width = 203
        Height = 68
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 2
        OnKeyDown = lbRecordersKeyDown
        OnKeyPress = lbRecordersKeyPress
      end
      object bbRecorderRemove: TImageListButton
        Left = 271
        Top = 157
        Width = 24
        Height = 23
        Hint = 'Remove selected recorder(s)'
        Anchors = [akTop, akRight]
        TabOrder = 5
        OnClick = bbRecorderRemoveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 4
      end
      object bbRecorderAdd: TImageListButton
        Left = 271
        Top = 134
        Width = 24
        Height = 23
        Hint = 'Find name'
        Anchors = [akTop, akRight]
        TabOrder = 4
        OnClick = bbRecorderAddClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 2
      end
      object bbRecorderFind: TImageListButton
        Left = 271
        Top = 111
        Width = 24
        Height = 23
        Hint = 'Get name(s)'
        Anchors = [akTop, akRight]
        TabOrder = 3
        OnClick = bbRecorderFindClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 5
      end
      object cmbSampleType: TDBListCombo
        Left = 67
        Top = 85
        Width = 228
        Height = 22
        Style = csOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
        Sorted = True
        TabOrder = 1
        OnChange = cmbSampleTypeChange
        OnDrawItem = cmbSampleTypeDrawItem
        ListField = 'SHORT_NAME'
        KeyField = 'SAMPLE_TYPE_KEY'
        Datasource = dmPlaceCard.dsSampleType
        Active = False
        EmptyItem = False
        ReadOnly = False
      end
      inline fraLocationInfo: TfraLocationInfo
        Left = -8
        Top = 0
        Width = 303
        Height = 84
        HorzScrollBar.Visible = False
        VertScrollBar.Visible = False
        Anchors = [akLeft, akTop, akRight]
        Color = clBtnFace
        ParentColor = False
        TabOrder = 0
        inherited Label6: TLabel
          Left = 8
        end
        inherited Label7: TLabel
          Left = 8
        end
        inherited Label8: TLabel
          Left = 8
          Top = 22
          Width = 44
          Height = 26
          WordWrap = True
        end
        inherited eSpatialRef: TSpatialRef
          Left = 75
          Top = 49
          Width = 228
        end
        inherited eLocationName: TEdit
          Left = 75
          Top = 26
          Width = 228
        end
        inherited eLocation: TAddinLinkedEdit
          Left = 75
          Width = 228
        end
        inherited pmMaps: TPopupMenu
          Left = 42
          Top = 37
        end
      end
    end
    object pnlRight: TPanel
      Left = 311
      Top = 27
      Width = 295
      Height = 180
      Anchors = [akLeft, akTop, akBottom]
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        295
        180)
      object shpBiotope: TShape
        Tag = 2
        Left = 66
        Top = 84
        Width = 206
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        Pen.Color = clRed
      end
      object lblBiotope: TLabel
        Left = 0
        Top = 89
        Width = 39
        Height = 13
        Caption = 'Biotope:'
      end
      object lblComments: TLabel
        Left = 0
        Top = 115
        Width = 52
        Height = 13
        Caption = 'Comments:'
      end
      object lblAdminArea: TLabel
        Left = 0
        Top = 39
        Width = 68
        Height = 13
        Caption = 'Admin Area(s):'
      end
      object lblDate: TLabel
        Left = 0
        Top = 4
        Width = 26
        Height = 13
        Caption = 'Date:'
      end
      object eBiotope: TEdit
        Left = 67
        Top = 85
        Width = 204
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = ChangeEditState
        OnKeyPress = eBiotopeKeyPress
      end
      object bbBiotopeFind: TImageListButton
        Left = 272
        Top = 84
        Width = 23
        Height = 23
        Hint = 'Get location'
        Anchors = [akTop, akRight]
        TabOrder = 3
        OnClick = bbBiotopeFindClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 5
      end
      object reComments: TRichEdit
        Left = 67
        Top = 111
        Width = 228
        Height = 69
        Anchors = [akLeft, akTop, akRight, akBottom]
        PopupMenu = dmFormActions.pmRTF
        ScrollBars = ssVertical
        TabOrder = 4
        OnChange = reCommentsChange
        OnEnter = reCommentsEnter
        OnExit = reCommentsExit
      end
      object eAdminArea: TEdit
        Left = 0
        Top = 54
        Width = 295
        Height = 21
        TabStop = False
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 1
      end
      object eDate: TVagueDateEdit
        Left = 67
        Top = 0
        Width = 137
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnChange = ChangeEditState
        OnExit = eDateExit
      end
    end
  end
  object pnlList: TPanel [2]
    Tag = 2
    Left = 0
    Top = 238
    Width = 628
    Height = 202
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    Color = clRed
    TabOrder = 1
    object sgSpecies: TStringGrid
      Left = 1
      Top = 1
      Width = 626
      Height = 200
      Align = alClient
      ColCount = 3
      Constraints.MinHeight = 40
      DefaultRowHeight = 19
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goThumbTracking]
      PopupMenu = pmReOrder
      TabOrder = 0
      OnClick = sgSpeciesClick
      OnDblClick = sgSpeciesDblClick
      OnDrawCell = sgSpeciesDrawCell
      OnKeyDown = sgSpeciesKeyDown
      OnKeyPress = sgSpeciesKeyPress
      OnMouseDown = sgSpeciesMouseDown
      OnMouseUp = sgSpeciesMouseUp
      OnSelectCell = sgSpeciesSelectCell
      OnSetEditText = sgSpeciesSetEditText
      OnTopLeftChanged = sgSpeciesTopLeftChanged
      ColWidths = (
        20
        20
        119)
    end
    object cmbAccuracy: TComboBox
      Left = 12
      Top = 76
      Width = 105
      Height = 21
      BevelInner = bvNone
      BevelKind = bkFlat
      Ctl3D = False
      ItemHeight = 13
      ParentCtl3D = False
      TabOrder = 1
      Visible = False
      OnChange = cmbAccuracyChange
      OnKeyDown = sgSpeciesComboKeyDown
      OnKeyPress = cmbKeyPress
      Items.Strings = (
        '<None>'
        'Exact'
        '+/- 5%'
        'Estimate')
    end
    object cmbSubstrate: TDBListCombo
      Left = 232
      Top = 76
      Width = 101
      Height = 21
      BevelInner = bvNone
      BevelKind = bkFlat
      Style = csDropDownList
      Ctl3D = False
      ItemHeight = 13
      ParentCtl3D = False
      Sorted = True
      TabOrder = 2
      Visible = False
      OnChange = cmbSubstrateChange
      OnKeyDown = sgSpeciesComboKeyDown
      OnKeyPress = cmbKeyPress
      ListField = 'Short_Name'
      KeyField = 'Substrate_Key'
      Datasource = dmPlaceCard.dsSubstrate
      Active = False
      EmptyItem = False
      ReadOnly = False
    end
    object cmbProvenance: TComboBox
      Left = 128
      Top = 76
      Width = 93
      Height = 21
      BevelInner = bvNone
      BevelKind = bkFlat
      Ctl3D = False
      ItemHeight = 13
      ParentCtl3D = False
      TabOrder = 3
      Visible = False
      OnChange = cmbProvenanceChange
      OnKeyDown = sgSpeciesComboKeyDown
      OnKeyPress = cmbKeyPress
      Items.Strings = (
        '<None>'
        'Alien'
        'Native'
        'Naturalised'
        'Vagrant')
    end
    object cmbRecordtype: TDBListCombo
      Left = 344
      Top = 76
      Width = 97
      Height = 21
      BevelInner = bvNone
      BevelKind = bkFlat
      Style = csDropDownList
      Ctl3D = False
      ItemHeight = 13
      ParentCtl3D = False
      Sorted = True
      TabOrder = 4
      Visible = False
      OnChange = cmbRecordTypeChange
      OnKeyDown = sgSpeciesComboKeyDown
      OnKeyPress = cmbKeyPress
      ListField = 'Short_Name'
      KeyField = 'Record_Type_Key'
      Datasource = dmPlaceCard.dsRecordType
      Active = False
      EmptyItem = False
      ReadOnly = False
    end
    object cmbQualifier: TComboBox
      Left = 448
      Top = 76
      Width = 65
      Height = 21
      BevelInner = bvNone
      BevelKind = bkFlat
      Style = csDropDownList
      Ctl3D = False
      ItemHeight = 13
      ParentCtl3D = False
      Sorted = True
      TabOrder = 5
      Visible = False
      OnChange = cmbQualifierChange
      OnKeyDown = sgSpeciesComboKeyDown
      OnKeyPress = cmbKeyPress
    end
    object cmbRestrictedData: TComboBox
      Left = 520
      Top = 76
      Width = 89
      Height = 21
      BevelInner = bvNone
      BevelKind = bkFlat
      Style = csDropDownList
      Ctl3D = False
      ItemHeight = 13
      ParentCtl3D = False
      TabOrder = 6
      Visible = False
      OnChange = cmbRestrictedDataChange
      OnKeyDown = sgSpeciesComboKeyDown
      OnKeyPress = cmbKeyPress
    end
  end
  inherited mnuChildMerge: TMainMenu
    Left = 320
    Top = 244
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 2
      object mnuEditCut: TMenuItem
        Action = dmFormActions.actCut
      end
      object mnuEditCopy: TMenuItem
        Action = dmFormActions.actCopy
      end
      object mnuEditPaste: TMenuItem
        Action = dmFormActions.actPaste
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuEditBold: TMenuItem
        Action = dmFormActions.actBold
      end
      object mnuEditItalic: TMenuItem
        Action = dmFormActions.actItalic
      end
      object mnuEditUnderline: TMenuItem
        Action = dmFormActions.actUnderline
      end
    end
  end
  object pmReOrder: TPopupMenu
    Left = 244
    Top = 244
    object mnuOriginalOrder: TMenuItem
      Caption = 'Original Order'
      OnClick = mnuOriginalOrderClick
    end
    object mnuRemoveRow: TMenuItem
      Caption = 'Remove Row'
      OnClick = mnuRemoveRowClick
    end
  end
  object ActionList1: TActionList
    Left = 304
    Top = 456
  end
end
