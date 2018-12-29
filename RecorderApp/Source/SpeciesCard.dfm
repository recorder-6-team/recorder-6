inherited frmSpeciesCard: TfrmSpeciesCard
  Left = 338
  Top = 217
  Width = 678
  Height = 545
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Species record'
  Constraints.MinHeight = 545
  Constraints.MinWidth = 200
  OldCreateOrder = True
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlSpecies: TPanel [0]
    Left = 0
    Top = 0
    Width = 670
    Height = 463
    Align = alClient
    TabOrder = 0
    OnResize = pnlSpeciesResize
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 662
      Height = 455
      TabOrder = 0
      DesignSize = (
        662
        455)
      object pnlRight: TPanel
        Left = 332
        Top = 26
        Width = 328
        Height = 359
        BevelOuter = bvNone
        TabOrder = 2
        object pnlCtrlRecorders: TPanel
          Left = 0
          Top = 0
          Width = 328
          Height = 86
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            328
            86)
          object shpRecorders: TShape
            Tag = 2
            Left = 75
            Top = 2
            Width = 223
            Height = 81
            Anchors = [akLeft, akTop, akRight]
            Pen.Color = clRed
          end
          object Label6: TLabel
            Left = 6
            Top = 5
            Width = 58
            Height = 13
            Caption = 'Recorder(s):'
          end
          object lbRecorders: TListBox
            Left = 76
            Top = 3
            Width = 221
            Height = 79
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            MultiSelect = True
            TabOrder = 0
            OnKeyDown = lbRecordersKeyDown
            OnKeyPress = lbRecordersKeyPress
          end
          object bbRecorderFind: TImageListButton
            Left = 298
            Top = 2
            Width = 24
            Height = 23
            Hint = 'Get name(s)'
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnClick = bbRecorderFindClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 5
          end
          object bbRecorderAdd: TImageListButton
            Left = 298
            Top = 25
            Width = 24
            Height = 23
            Hint = 'Find name'
            Anchors = [akTop, akRight]
            TabOrder = 2
            OnClick = bbRecorderAddClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 2
          end
          object bbRecorderRemove: TImageListButton
            Left = 298
            Top = 48
            Width = 24
            Height = 23
            Hint = 'Remove the selected recorder(s)'
            Anchors = [akTop, akRight]
            TabOrder = 3
            OnClick = bbRecorderRemoveClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 4
          end
        end
        object pnlCtrlDate: TPanel
          Left = 0
          Top = 86
          Width = 328
          Height = 25
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object Label1: TLabel
            Left = 6
            Top = 8
            Width = 26
            Height = 13
            Caption = 'Date:'
          end
          object eDate: TVagueDateEdit
            Left = 75
            Top = 3
            Width = 190
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
        object pnlCtrlAdminAreas: TPanel
          Left = 0
          Top = 139
          Width = 328
          Height = 25
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 2
          DesignSize = (
            328
            25)
          object Label5: TLabel
            Left = 6
            Top = 5
            Width = 68
            Height = 13
            Caption = 'Admin Area(s):'
            WordWrap = True
          end
          object eAdminArea: TEdit
            Left = 75
            Top = 2
            Width = 247
            Height = 21
            TabStop = False
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
          end
        end
        object pnlCtrlBiotope: TPanel
          Left = 0
          Top = 111
          Width = 328
          Height = 28
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 3
          DesignSize = (
            328
            28)
          object lblBiotope: TLabel
            Left = 6
            Top = 8
            Width = 39
            Height = 13
            Caption = 'Biotope:'
          end
          object shpBiotope: TShape
            Tag = 2
            Left = 75
            Top = 2
            Width = 226
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            Pen.Color = clRed
          end
          object eBiotope: TEdit
            Left = 76
            Top = 3
            Width = 224
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = ChangeEditState
            OnKeyPress = eBiotopeKeyPress
          end
          object bbBiotopeFind: TImageListButton
            Left = 301
            Top = 2
            Width = 21
            Height = 23
            Hint = 'Get biotope'
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnClick = bbBiotopeFindClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 5
          end
        end
        object pnlCtrlProvenance: TPanel
          Left = 0
          Top = 164
          Width = 328
          Height = 26
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 4
          DesignSize = (
            328
            26)
          object Label11: TLabel
            Left = 6
            Top = 5
            Width = 61
            Height = 13
            Caption = 'Provenance:'
          end
          object cmbProvenance: TComboBox
            Left = 75
            Top = 2
            Width = 247
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            Sorted = True
            TabOrder = 0
            OnChange = cmbProvenanceChange
            Items.Strings = (
              '<None>'
              'Alien'
              'Escaped'
              'Introduced'
              'Migrant'
              'Native'
              'Naturalised'
              'Vagrant')
          end
        end
        object pnlCtrlRecordType: TPanel
          Left = 0
          Top = 190
          Width = 328
          Height = 26
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 5
          DesignSize = (
            328
            26)
          object Label14: TLabel
            Left = 6
            Top = 5
            Width = 65
            Height = 13
            Caption = 'Record Type:'
          end
          object cmbRecordType: TDBListCombo
            Left = 75
            Top = 2
            Width = 247
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            Sorted = True
            TabOrder = 0
            OnChange = cmbRecordTypeChange
            ListField = 'SHORT_NAME'
            KeyField = 'RECORD_TYPE_KEY'
            Datasource = dmPlaceCard.dsRecordType
            Active = False
            EmptyItem = False
            ReadOnly = False
          end
        end
        object pnlCtrlSubstrate: TPanel
          Left = 0
          Top = 216
          Width = 328
          Height = 26
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 6
          DesignSize = (
            328
            26)
          object Label15: TLabel
            Left = 6
            Top = 5
            Width = 48
            Height = 13
            Caption = 'Substrate:'
          end
          object cmbSubstrate: TDBListCombo
            Left = 75
            Top = 2
            Width = 247
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            Sorted = True
            TabOrder = 0
            OnChange = cmbSubstrateChange
            ListField = 'SHORT_NAME'
            KeyField = 'SUBSTRATE_KEY'
            Datasource = dmPlaceCard.dsSubstrate
            Active = False
            EmptyItem = False
            ReadOnly = False
          end
        end
        object pnlCtrlDateOfDetermination: TPanel
          Left = 0
          Top = 242
          Width = 328
          Height = 26
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 7
          DesignSize = (
            328
            26)
          object lblDeterminationDate: TLabel
            Left = 6
            Top = 5
            Width = 106
            Height = 13
            Caption = 'Date of Determination:'
          end
          object eDeterminationDate: TVagueDateEdit
            Left = 116
            Top = 2
            Width = 205
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnChange = ChangeEditState
            OnExit = eDeterminationDateExit
          end
        end
        object pnlCtrlSpecimenComment: TPanel
          Left = 0
          Top = 268
          Width = 328
          Height = 88
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 8
          DesignSize = (
            328
            88)
          object lblSpecimenComment: TLabel
            Left = 6
            Top = 5
            Width = 97
            Height = 13
            Caption = 'Specimen Comment:'
          end
          object reSpecimenComment: TRichEdit
            Left = 116
            Top = 2
            Width = 205
            Height = 83
            Anchors = [akLeft, akTop, akRight, akBottom]
            PopupMenu = dmFormActions.pmRTF
            ScrollBars = ssVertical
            TabOrder = 0
            OnChange = ChangeEditState
            OnEnter = reCommentsEnter
            OnExit = reCommentsExit
          end
        end
      end
      object pnlLeft: TPanel
        Left = 1
        Top = 26
        Width = 331
        Height = 359
        BevelOuter = bvNone
        TabOrder = 1
        object pnlCtrlTaxon: TPanel
          Left = 0
          Top = 0
          Width = 331
          Height = 28
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            331
            28)
          object Label16: TLabel
            Left = 6
            Top = 6
            Width = 33
            Height = 13
            Caption = 'Taxon:'
          end
          object shpTaxon: TShape
            Tag = 2
            Left = 102
            Top = 2
            Width = 200
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            Pen.Color = clRed
          end
          object eTaxon: TEdit
            Left = 103
            Top = 3
            Width = 198
            Height = 22
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsItalic]
            ParentFont = False
            TabOrder = 0
            OnChange = ChangeEditState
            OnKeyPress = eTaxonKeyPress
          end
          object bbTaxonFind: TImageListButton
            Left = 302
            Top = 2
            Width = 23
            Height = 23
            Hint = 'Get taxon'
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnClick = bbTaxonFindClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 5
          end
        end
        object pnlCtrlDocument: TPanel
          Left = 0
          Top = 28
          Width = 331
          Height = 28
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          DesignSize = (
            331
            28)
          object Label7: TLabel
            Left = 6
            Top = 6
            Width = 52
            Height = 13
            Caption = 'Document:'
          end
          object shpReference: TShape
            Tag = 2
            Left = 102
            Top = 2
            Width = 200
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            Pen.Color = clRed
          end
          object bbReferenceFind: TImageListButton
            Left = 302
            Top = 2
            Width = 23
            Height = 23
            Hint = 'Get reference'
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnClick = bbReferenceFindClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 5
          end
          object eReference: TEdit
            Left = 103
            Top = 3
            Width = 198
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = ChangeEditState
            OnDblClick = eReferenceDblClick
            OnKeyPress = eReferenceKeyPressed
          end
        end
        object pnlCtrlLocationInfo: TPanel
          Left = 0
          Top = 56
          Width = 331
          Height = 89
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 2
          inline fraLocationInfo: TfraLocationInfo
            Left = 0
            Top = 0
            Width = 331
            Height = 89
            HorzScrollBar.Visible = False
            VertScrollBar.Visible = False
            Align = alClient
            TabOrder = 0
            inherited Label6: TLabel
              Left = 6
              Top = 6
            end
            inherited Label7: TLabel
              Left = 6
              Top = 61
            end
            inherited Label8: TLabel
              Left = 6
              Top = 32
              WordWrap = True
            end
            inherited eSpatialRef: TSpatialRef
              Left = 102
              Top = 53
              Width = 223
            end
            inherited eLocationName: TEdit
              Left = 103
              Top = 29
              Width = 222
            end
            inherited eLocation: TAddinLinkedEdit
              Left = 102
              Top = 2
              Width = 223
            end
            inherited pmMaps: TPopupMenu
              Left = 58
              Top = 41
            end
          end
        end
        object pnlCtrlSampleType: TPanel
          Left = 0
          Top = 145
          Width = 331
          Height = 27
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 3
          object Label17: TLabel
            Left = 6
            Top = 5
            Width = 65
            Height = 13
            Caption = 'Sample Type:'
          end
          object cmbSampleType: TDBListCombo
            Left = 102
            Top = 2
            Width = 145
            Height = 22
            Style = csOwnerDrawFixed
            ItemHeight = 16
            Sorted = True
            TabOrder = 0
            OnChange = ChangeEditState
            OnDrawItem = cmbSampleTypeDrawItem
            ListField = 'SHORT_NAME'
            KeyField = 'SAMPLE_TYPE_KEY'
            Datasource = dmPlaceCard.dsSampleType
            Active = False
            EmptyItem = False
            ReadOnly = False
          end
        end
        object pnlCtrlCount: TPanel
          Left = 0
          Top = 172
          Width = 331
          Height = 78
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 4
          DesignSize = (
            331
            78)
          object Label10: TLabel
            Left = 6
            Top = 8
            Width = 31
            Height = 13
            Caption = 'Count:'
          end
          object sgCount: TStringGrid
            Left = 102
            Top = 2
            Width = 199
            Height = 73
            Anchors = [akLeft, akTop, akRight]
            ColCount = 3
            DefaultRowHeight = 18
            FixedCols = 0
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
            TabOrder = 0
            OnClick = sgCountClick
            OnDrawCell = sgCountDrawCell
            OnKeyPress = sgCountKeyPress
            OnSelectCell = sgCountSelectCell
            OnSetEditText = sgCountSetEditText
            ColWidths = (
              96
              66
              60)
          end
          object bbDetailsAdd: TImageListButton
            Left = 301
            Top = 2
            Width = 24
            Height = 23
            Hint = 'Add new details'
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnClick = bbDetailsAddClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 2
          end
          object bbDetailsDel: TImageListButton
            Left = 301
            Top = 26
            Width = 24
            Height = 23
            Hint = 'Delete selected details'
            Anchors = [akTop, akRight]
            TabOrder = 2
            OnClick = bbDetailsDelClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 4
          end
          object cmbQualifier: TComboBox
            Left = 193
            Top = 24
            Width = 68
            Height = 21
            BevelInner = bvNone
            BevelOuter = bvNone
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 3
            Visible = False
            OnChange = cmbCountAttributeChange
            OnKeyPress = sgCountKeyPress
            Items.Strings = (
              'Hello '
              'goodbye')
          end
          object cmbAccuracy: TComboBox
            Left = 258
            Top = 24
            Width = 61
            Height = 21
            BevelInner = bvNone
            BevelOuter = bvNone
            ItemHeight = 13
            TabOrder = 4
            Visible = False
            OnChange = cmbCountAttributeChange
            OnExit = cmdGridExit
            OnKeyPress = sgCountKeyPress
            Items.Strings = (
              '<None>'
              'Exact'
              '+/- 5%'
              'Estimate')
          end
        end
        object pnlCtrlDeterminer: TPanel
          Left = 0
          Top = 250
          Width = 331
          Height = 28
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 5
          object Label12: TLabel
            Left = 6
            Top = 6
            Width = 54
            Height = 13
            Caption = 'Determiner:'
          end
          object eDeterminer: TNameLinkedEdit
            Tag = 1
            Left = 102
            Top = 2
            Width = 222
            Height = 23
            TabOrder = 0
            BorderStyle = bsSingle
            ImageIndex = 5
            ImageList = dmFormActions.ilButtons
            OnChange = ChangeEditState
            OnFindData = eDeterminerFindData
            OnGetData = eDeterminerGetData
          end
        end
        object pnlCtrlSpecimenNumber: TPanel
          Left = 0
          Top = 278
          Width = 331
          Height = 26
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 6
          object lblSpecimenNumber: TLabel
            Left = 6
            Top = 5
            Width = 90
            Height = 13
            Caption = 'Specimen Number:'
          end
          object eSpecimenNumber: TEdit
            Left = 103
            Top = 2
            Width = 145
            Height = 21
            MaxLength = 30
            TabOrder = 0
            OnChange = eSpecimenNumberChange
          end
        end
        object pnlCtrlSpecimenType: TPanel
          Left = 0
          Top = 304
          Width = 331
          Height = 26
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 7
          object lblSpecimenType: TLabel
            Left = 6
            Top = 5
            Width = 77
            Height = 13
            Caption = 'Specimen Type:'
          end
          object cmbSpecimenType: TDBListCombo
            Left = 103
            Top = 3
            Width = 145
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            OnChange = ChangeEditState
            ListField = 'SHORT_NAME'
            KeyField = 'SPECIMEN_TYPE_KEY'
            Datasource = dmTaxonOccurrences.dsSpecType
            Active = False
            EmptyItem = False
            ReadOnly = False
          end
        end
        object pnlCtrlSpecimenLocation: TPanel
          Left = 0
          Top = 330
          Width = 331
          Height = 26
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 8
          DesignSize = (
            331
            26)
          object lblSpecimenLocation: TLabel
            Left = 6
            Top = 5
            Width = 94
            Height = 13
            Caption = 'Specimen Location:'
          end
          object eSpecimenLocation: TEdit
            Left = 103
            Top = 2
            Width = 222
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 100
            TabOrder = 0
            OnChange = ChangeEditState
          end
        end
      end
      object pnlCtrlSurvey: TPanel
        Left = 1
        Top = 1
        Width = 660
        Height = 24
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          660
          24)
        object Label18: TLabel
          Left = 6
          Top = 5
          Width = 36
          Height = 13
          Caption = 'Survey:'
        end
        object cmbSurvey: TDBListCombo
          Left = 103
          Top = 2
          Width = 551
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          Sorted = True
          TabOrder = 0
          ListField = 'ITEM_NAME'
          KeyField = 'SURVEY_KEY'
          Datasource = dmFormActions.dsSurvey
          Active = False
          EmptyItem = False
          ReadOnly = False
        end
      end
      object pnlCtrlComments: TPanel
        Left = 1
        Top = 384
        Width = 660
        Height = 73
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 3
        DesignSize = (
          660
          73)
        object Label8: TLabel
          Left = 6
          Top = 5
          Width = 52
          Height = 13
          Caption = 'Comments:'
        end
        object reComments: TRichEdit
          Left = 102
          Top = 2
          Width = 551
          Height = 63
          Anchors = [akLeft, akTop, akRight, akBottom]
          PopupMenu = dmFormActions.pmRTF
          ScrollBars = ssVertical
          TabOrder = 0
          OnChange = reCommentsChange
          OnEnter = reCommentsEnter
          OnExit = reCommentsExit
        end
      end
    end
  end
  object pnlButtons: TPanel [1]
    Left = 0
    Top = 463
    Width = 670
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlButtons2: TPanel
      Left = 500
      Top = 0
      Width = 170
      Height = 36
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object bbReset: TImageListButton
        Left = 88
        Top = 6
        Width = 75
        Height = 25
        Hint = 'Clear all fields on the card'
        Cancel = True
        Caption = 'Reset'
        Enabled = False
        TabOrder = 0
        OnClick = bbResetClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 1
      end
      object bbSave: TImageListButton
        Left = 4
        Top = 6
        Width = 75
        Height = 25
        Hint = 'Save observation details'
        Caption = 'Save'
        TabOrder = 1
        OnClick = bbSaveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
      end
    end
  end
  inherited mnuChildMerge: TMainMenu
    Left = 132
    Top = 396
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
      object N2: TMenuItem
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
  object pmMapWindow: TPopupMenu
    Images = dmFormActions.ilMenuOn
    Left = 210
    Top = 398
  end
end
