object frmTaxonOccurrences: TfrmTaxonOccurrences
  Left = 530
  Top = 405
  Width = 412
  Height = 429
  Caption = 'Taxon Occurrences'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object pnlDetails: TPanel
    Left = 0
    Top = 0
    Width = 404
    Height = 402
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
    OnResize = pnlDetailsResize
    object pnlInner: TPanel
      Left = -8
      Top = 0
      Width = 385
      Height = 361
      BevelOuter = bvLowered
      TabOrder = 0
      DesignSize = (
        385
        361)
      object lblTaxonName: TLabel
        Left = 8
        Top = 4
        Width = 365
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object pcTaxonOccurrence: TPageControl
        Left = 16
        Top = 24
        Width = 373
        Height = 301
        ActivePage = tsGeneral
        Anchors = [akLeft, akTop, akRight, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnChange = pcTaxonOccurrenceChange
        OnChanging = pcTaxonOccurrenceChanging
        object tsGeneral: TTabSheet
          Caption = 'General'
          DesignSize = (
            365
            273)
          object bvlGeneral: TBevel
            Left = 4
            Top = 4
            Width = 357
            Height = 265
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object Label2: TLabel
            Left = 12
            Top = 40
            Width = 65
            Height = 13
            Caption = 'Record Type:'
          end
          object Label3: TLabel
            Left = 12
            Top = 62
            Width = 48
            Height = 13
            Caption = 'Substrate:'
          end
          object Label8: TLabel
            Left = 12
            Top = 84
            Width = 52
            Height = 13
            Caption = 'Comments:'
          end
          object Label28: TLabel
            Left = 12
            Top = 14
            Width = 72
            Height = 13
            Caption = 'Surveyor'#39's Ref:'
          end
          object lblVerificationStatus: TLabel
            Left = 168
            Top = 245
            Width = 63
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Not validated'
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object lblProvenance: TLabel
            Left = 204
            Top = 62
            Width = 61
            Height = 13
            Caption = 'Provenance:'
          end
          object Label1: TLabel
            Left = 12
            Top = 156
            Width = 31
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Count:'
          end
          object Label18: TLabel
            Left = 12
            Top = 220
            Width = 50
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Det. Type:'
          end
          object cmbProvenance: TComboBox
            Left = 272
            Top = 60
            Width = 81
            Height = 21
            ItemHeight = 13
            Sorted = True
            TabOrder = 3
            Items.Strings = (
              'Alien'
              'Escaped'
              'Introduced'
              'Migrant'
              'Native'
              'Naturalised'
              'Vagrant')
          end
          object dbeSurveyorRef: TDBEdit
            Left = 92
            Top = 12
            Width = 261
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'SURVEYORS_REF'
            DataSource = dmTaxonOccurrences.dsTaxonOcc
            TabOrder = 0
          end
          object dbreComments: TDBRichEdit
            Left = 92
            Top = 84
            Width = 261
            Height = 69
            Anchors = [akLeft, akTop, akRight, akBottom]
            DataField = 'COMMENT'
            DataSource = dmTaxonOccurrences.dsTaxonOcc
            PopupMenu = dmFormActions.pmRTF
            ScrollBars = ssVertical
            TabOrder = 4
            OnEnter = EnterRTF
            OnExit = ExitRTF
          end
          object dbcbChecked: TDBCheckBox
            Left = 96
            Top = 244
            Width = 69
            Height = 17
            Anchors = [akLeft, akBottom]
            Caption = 'Checked'
            DataField = 'CHECKED'
            DataSource = dmTaxonOccurrences.dsTaxonOcc
            TabOrder = 6
            ValueChecked = 'True'
            ValueUnchecked = 'False'
            OnClick = dbcbCheckedClick
            OnMouseUp = dbCheckBoxMouseUp
          end
          object dbcbConfidential: TDBCheckBox
            Left = 12
            Top = 244
            Width = 81
            Height = 17
            Anchors = [akLeft, akBottom]
            Caption = 'Confidential'
            DataField = 'CONFIDENTIAL'
            DataSource = dmTaxonOccurrences.dsTaxonOcc
            TabOrder = 5
            ValueChecked = 'True'
            ValueUnchecked = 'False'
            OnMouseUp = dbCheckBoxMouseUp
          end
          object mmCount: TMemo
            Left = 92
            Top = 156
            Width = 261
            Height = 56
            TabStop = False
            Anchors = [akLeft, akRight, akBottom]
            Color = clBtnFace
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 7
          end
          object dbcmbRecordType: TDBListCombo
            Left = 92
            Top = 36
            Width = 261
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            Sorted = True
            TabOrder = 1
            ListField = 'SHORT_NAME'
            KeyField = 'RECORD_TYPE_KEY'
            Datasource = dmTaxonOccurrences.dsRecordType
            Active = False
            EmptyItem = False
            ReadOnly = False
          end
          object dbcmbSubstrate: TDBListCombo
            Left = 92
            Top = 60
            Width = 105
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            Sorted = True
            TabOrder = 2
            ListField = 'SHORT_NAME'
            KeyField = 'SUBSTRATE_KEY'
            Datasource = dmTaxonOccurrences.dsSubstrate
            Active = False
            EmptyItem = False
            ReadOnly = False
          end
          object cmbDeterminationType: TDBListCombo
            Left = 92
            Top = 216
            Width = 261
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akRight, akBottom]
            ItemHeight = 13
            Sorted = True
            TabOrder = 8
            OnChange = cmbDeterminationTypeChange
            ListField = 'SHORT_NAME'
            KeyField = 'DETERMINATION_TYPE_KEY'
            Datasource = dmDetermination.dsDetType
            Active = False
            EmptyItem = False
            ReadOnly = False
          end
        end
        object tsDeterminations: TTabSheet
          Hint = 'Determinations'
          BorderWidth = 4
          Caption = 'Dets.'
          ImageIndex = 4
          object splDets: TSplitter
            Left = 0
            Top = 85
            Width = 357
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            Beveled = True
            MinSize = 73
          end
          object pnlDetsTop: TPanel
            Left = 0
            Top = 0
            Width = 357
            Height = 85
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              357
              85)
            object sgDeterminations: TStringGrid
              Left = 0
              Top = 0
              Width = 333
              Height = 81
              Anchors = [akLeft, akTop, akRight, akBottom]
              ColCount = 6
              DefaultColWidth = 190
              DefaultRowHeight = 18
              FixedCols = 0
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
              TabOrder = 0
              OnClick = sgDeterminationsClick
              OnDrawCell = sgDeterminationsDrawCell
              ColWidths = (
                16
                100
                58
                46
                46
                59)
            end
            object bbDetAdd: TImageListButton
              Left = 333
              Top = 0
              Width = 24
              Height = 23
              Hint = 'Add new determination'
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = bbDetAddClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 2
            end
            object bbDetEdit: TImageListButton
              Left = 333
              Top = 23
              Width = 24
              Height = 23
              Hint = 'Edit selected determination'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = bbDetEditClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 3
            end
            object bbDetDel: TImageListButton
              Left = 333
              Top = 46
              Width = 24
              Height = 23
              Hint = 'Delete selected determination'
              Anchors = [akTop, akRight]
              TabOrder = 3
              OnClick = bbDetDelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 4
            end
          end
          object pnlDetsBottom: TPanel
            Left = 0
            Top = 88
            Width = 357
            Height = 177
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            object gbDetDetails: TGroupBox
              Left = 0
              Top = 0
              Width = 357
              Height = 177
              Align = alClient
              Caption = 'Details'
              Constraints.MinHeight = 176
              TabOrder = 0
              DesignSize = (
                357
                177)
              object Label5: TLabel
                Left = 8
                Top = 20
                Width = 33
                Height = 13
                Caption = 'Taxon:'
              end
              object Label6: TLabel
                Left = 8
                Top = 46
                Width = 54
                Height = 13
                Caption = 'Determiner:'
              end
              object Label7: TLabel
                Left = 8
                Top = 148
                Width = 52
                Height = 13
                Caption = 'Comments:'
              end
              object Shape1: TShape
                Tag = 2
                Left = 63
                Top = 15
                Width = 263
                Height = 24
                Anchors = [akLeft, akTop, akRight]
                Pen.Color = clRed
              end
              object Label9: TLabel
                Left = 8
                Top = 72
                Width = 25
                Height = 13
                Caption = 'Role:'
              end
              object Label10: TLabel
                Left = 8
                Top = 98
                Width = 27
                Height = 13
                Caption = 'Type:'
              end
              object Label14: TLabel
                Left = 8
                Top = 124
                Width = 29
                Height = 13
                Caption = 'Work:'
              end
              object Label15: TLabel
                Left = 164
                Top = 98
                Width = 26
                Height = 13
                Anchors = [akTop, akRight]
                Caption = 'Date:'
              end
              object Shape4: TShape
                Tag = 2
                Left = 63
                Top = 119
                Width = 263
                Height = 23
                Anchors = [akLeft, akTop, akRight]
                Pen.Color = clRed
              end
              object lblReviewSummary: TLabel
                Left = 264
                Top = 72
                Width = 8
                Height = 13
                Anchors = [akLeft, akBottom]
                Caption = 'N'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlue
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object bbDetAccept: TImageListButton
                Left = 301
                Top = 147
                Width = 24
                Height = 23
                Hint = 'Accept changes'
                Anchors = [akRight, akBottom]
                TabOrder = 10
                OnClick = bbDetAcceptClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 6
              end
              object bbDetDiscard: TImageListButton
                Left = 325
                Top = 147
                Width = 24
                Height = 23
                Hint = 'Discard changes'
                Anchors = [akRight, akBottom]
                TabOrder = 11
                OnClick = bbDetDiscardClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 7
              end
              object reDetComments: TRichEdit
                Left = 64
                Top = 146
                Width = 235
                Height = 24
                Anchors = [akLeft, akTop, akRight, akBottom]
                PopupMenu = dmFormActions.pmRTF
                ScrollBars = ssVertical
                TabOrder = 9
                OnEnter = EnterRTF
                OnExit = ExitRTF
              end
              object eTaxon: TEdit
                Left = 64
                Top = 16
                Width = 261
                Height = 22
                Anchors = [akLeft, akTop, akRight]
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Arial'
                Font.Style = [fsItalic]
                ParentFont = False
                TabOrder = 0
                OnKeyPress = eTaxonKeyPress
              end
              object eWork: TEdit
                Left = 64
                Top = 120
                Width = 261
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 7
                OnDblClick = eWorkDblClick
                OnKeyPress = eWorkKeyPress
              end
              object cbPreferred: TCheckBox
                Left = 196
                Top = 70
                Width = 65
                Height = 17
                Caption = 'Preferred'
                TabOrder = 4
              end
              object eDetDate: TVagueDateEdit
                Left = 196
                Top = 94
                Width = 129
                Height = 21
                Anchors = [akTop, akRight]
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clGreen
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentFont = False
                TabOrder = 6
                OnExit = eDetDateExit
              end
              object dbcmbRole: TDBListCombo
                Left = 64
                Top = 68
                Width = 129
                Height = 21
                Style = csDropDownList
                ItemHeight = 0
                Sorted = True
                TabOrder = 3
                ListField = 'SHORT_NAME'
                KeyField = 'DETERMINER_ROLE_KEY'
                Datasource = dmDetermination.dsDetRole
                Active = False
                EmptyItem = False
                ReadOnly = False
              end
              object dbcmbType: TDBListCombo
                Left = 64
                Top = 94
                Width = 97
                Height = 21
                Style = csDropDownList
                Anchors = [akLeft, akTop, akRight]
                ItemHeight = 0
                Sorted = True
                TabOrder = 5
                ListField = 'SHORT_NAME'
                KeyField = 'DETERMINATION_TYPE_KEY'
                Datasource = dmDetermination.dsDetType
                Active = False
                EmptyItem = False
                ReadOnly = False
              end
              object bbFindTaxon: TImageListButton
                Left = 327
                Top = 15
                Width = 22
                Height = 24
                Hint = 'Get taxon'
                Anchors = [akTop, akRight]
                TabOrder = 1
                OnClick = bbFindTaxonClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 5
              end
              object bbFindRef: TImageListButton
                Left = 327
                Top = 118
                Width = 22
                Height = 24
                Hint = 'Get document'
                Anchors = [akTop, akRight]
                TabOrder = 8
                OnClick = bbFindRefClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 5
              end
              object eDeterminer: TNameLinkedEdit
                Tag = 1
                Left = 63
                Top = 42
                Width = 286
                Height = 23
                TabOrder = 2
                BorderStyle = bsSingle
                ImageIndex = 5
                ImageList = dmFormActions.ilButtons
                OnFindData = eDeterminerFindData
                OnGetData = eDeterminerGetData
              end
            end
          end
        end
        object tsMeasurements: TTabSheet
          Caption = 'Measurements'
          ImageIndex = 1
          object Measurements: TMeasurements
            Left = 4
            Top = 4
            Width = 355
            Height = 265
            SourceCol = clBlack
            DestCol = clBlack
            TabOrder = 0
            DesignSize = (
              355
              265)
          end
        end
        object tsRelatedOccurrences: TTabSheet
          Hint = 'Related Occurrences'
          BorderWidth = 4
          Caption = 'Related Occs.'
          ImageIndex = 2
          object splRelated: TSplitter
            Left = 0
            Top = 121
            Width = 357
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            Beveled = True
            MinSize = 73
          end
          object pnlRelatedTop: TPanel
            Left = 0
            Top = 0
            Width = 357
            Height = 121
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              357
              121)
            object Shape5: TShape
              Tag = 2
              Left = 0
              Top = 0
              Width = 333
              Height = 117
              Anchors = [akLeft, akTop, akRight, akBottom]
              Pen.Color = clRed
            end
            object sgRelOcc: TStringGrid
              Left = 1
              Top = 1
              Width = 331
              Height = 115
              Anchors = [akLeft, akTop, akRight, akBottom]
              ColCount = 2
              DefaultColWidth = 190
              DefaultRowHeight = 18
              FixedCols = 0
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goRowSelect]
              TabOrder = 0
              OnClick = sgRelOccClick
              OnDrawCell = sgRelOccDrawCell
              ColWidths = (
                229
                99)
            end
            object bbRelOccAdd: TImageListButton
              Left = 333
              Top = 0
              Width = 24
              Height = 23
              Hint = 'Add new related occurrence'
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = bbRelOccAddClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 2
            end
            object bbRelOccEdit: TImageListButton
              Left = 333
              Top = 23
              Width = 24
              Height = 23
              Hint = 'Edit selected related occurrence'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = bbRelOccEditClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 3
            end
            object bbRelOccDel: TImageListButton
              Left = 333
              Top = 46
              Width = 24
              Height = 23
              Hint = 'Delete selected related occurrence'
              Anchors = [akTop, akRight]
              TabOrder = 3
              OnClick = bbRelOccDelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 4
            end
          end
          object pnlRelatedBottom: TPanel
            Left = 0
            Top = 124
            Width = 357
            Height = 141
            Align = alBottom
            BevelOuter = bvNone
            Constraints.MinHeight = 141
            TabOrder = 1
            object gbRelOccDetails: TGroupBox
              Left = 0
              Top = 0
              Width = 357
              Height = 141
              Align = alClient
              Caption = 'Details'
              TabOrder = 0
              DesignSize = (
                357
                141)
              object Label11: TLabel
                Left = 8
                Top = 20
                Width = 99
                Height = 13
                Caption = 'Related Occurrence:'
              end
              object Label12: TLabel
                Left = 8
                Top = 48
                Width = 88
                Height = 13
                Caption = 'Relationship Type:'
              end
              object Label4: TLabel
                Left = 8
                Top = 70
                Width = 52
                Height = 13
                Caption = 'Comments:'
              end
              object Shape3: TShape
                Tag = 2
                Left = 115
                Top = 15
                Width = 235
                Height = 24
                Anchors = [akLeft, akTop, akRight]
                Pen.Color = clRed
              end
              object bbRelOccDiscard: TImageListButton
                Left = 325
                Top = 112
                Width = 24
                Height = 23
                Hint = 'Discard changes'
                Anchors = [akRight, akBottom]
                TabOrder = 4
                OnClick = bbRelOccDiscardClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 7
              end
              object bbRelOccAccept: TImageListButton
                Left = 301
                Top = 112
                Width = 24
                Height = 23
                Hint = 'Accept changes'
                Anchors = [akRight, akBottom]
                TabOrder = 3
                OnClick = bbRelOccAcceptClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 6
              end
              object eRelatedTaxon: TEdit
                Left = 116
                Top = 16
                Width = 233
                Height = 22
                Anchors = [akLeft, akTop, akRight]
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Arial'
                Font.Style = [fsItalic]
                ParentFont = False
                TabOrder = 0
                OnKeyPress = eRelatedTaxonKeyPress
              end
              object reOccComments: TRichEdit
                Left = 8
                Top = 84
                Width = 341
                Height = 23
                Anchors = [akLeft, akTop, akRight, akBottom]
                PopupMenu = dmFormActions.pmRTF
                ScrollBars = ssVertical
                TabOrder = 2
                OnEnter = EnterRTF
                OnExit = ExitRTF
              end
              object dbcmbRelType: TDBListCombo
                Left = 116
                Top = 44
                Width = 233
                Height = 21
                Style = csDropDownList
                Anchors = [akLeft, akTop, akRight]
                ItemHeight = 0
                Sorted = True
                TabOrder = 1
                ListField = 'SHORT_NAME'
                KeyField = 'RELATIONSHIP_TYPE_KEY'
                Datasource = dmTaxonOccurrences.dsRelationType
                Active = False
                EmptyItem = False
                ReadOnly = False
              end
            end
          end
        end
        object tsSpecimens: TTabSheet
          BorderWidth = 4
          Caption = 'Specimens'
          ImageIndex = 3
          object splSpecimens: TSplitter
            Left = 0
            Top = 89
            Width = 357
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            Beveled = True
            MinSize = 73
          end
          object gbSpecimenDetails: TGroupBox
            Left = 0
            Top = 92
            Width = 357
            Height = 173
            Align = alBottom
            Caption = 'Details'
            Constraints.MinHeight = 155
            TabOrder = 0
            DesignSize = (
              357
              173)
            object Label26: TLabel
              Left = 8
              Top = 64
              Width = 27
              Height = 13
              Caption = 'Type:'
            end
            object Label27: TLabel
              Left = 8
              Top = 108
              Width = 52
              Height = 13
              Caption = 'Comments:'
            end
            object Label13: TLabel
              Left = 8
              Top = 40
              Width = 78
              Height = 13
              Caption = 'Accession Date:'
            end
            object Label29: TLabel
              Left = 8
              Top = 16
              Width = 90
              Height = 13
              Caption = 'Specimen Number:'
            end
            object Label16: TLabel
              Left = 8
              Top = 88
              Width = 44
              Height = 13
              Caption = 'Location:'
            end
            object bbSpecimenAccept: TImageListButton
              Left = 301
              Top = 142
              Width = 24
              Height = 23
              Hint = 'Accept changes'
              Anchors = [akRight, akBottom]
              TabOrder = 5
              OnClick = bbSpecimenAcceptClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 6
            end
            object bbSpecimenDiscard: TImageListButton
              Left = 325
              Top = 142
              Width = 24
              Height = 23
              Hint = 'Discard changes'
              Anchors = [akRight, akBottom]
              TabOrder = 6
              OnClick = bbSpecimenDiscardClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 7
            end
            object eSpecNumber: TEdit
              Left = 116
              Top = 12
              Width = 133
              Height = 21
              TabOrder = 0
            end
            object eSpecDate: TVagueDateEdit
              Left = 116
              Top = 36
              Width = 133
              Height = 21
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGreen
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
              OnExit = eSpecDateExit
            end
            object reSpecComments: TRichEdit
              Left = 8
              Top = 124
              Width = 289
              Height = 41
              Anchors = [akLeft, akTop, akRight, akBottom]
              PopupMenu = dmFormActions.pmRTF
              ScrollBars = ssVertical
              TabOrder = 4
              OnEnter = EnterRTF
              OnExit = ExitRTF
            end
            object dbcmbSpecType: TDBListCombo
              Left = 116
              Top = 60
              Width = 181
              Height = 21
              Style = csDropDownList
              Anchors = [akLeft, akTop, akRight]
              ItemHeight = 0
              Sorted = True
              TabOrder = 2
              ListField = 'SHORT_NAME'
              KeyField = 'SPECIMEN_TYPE_KEY'
              Datasource = dmTaxonOccurrences.dsSpecType
              Active = False
              EmptyItem = False
              ReadOnly = False
            end
            object eSpecLocation: TEdit
              Left = 116
              Top = 84
              Width = 181
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 3
            end
          end
          object pnlSpecimensTop: TPanel
            Left = 0
            Top = 0
            Width = 357
            Height = 89
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              357
              89)
            object sgSpecimens: TStringGrid
              Left = 8
              Top = 0
              Width = 333
              Height = 85
              Anchors = [akLeft, akTop, akRight, akBottom]
              ColCount = 2
              DefaultColWidth = 190
              DefaultRowHeight = 18
              FixedCols = 0
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
              TabOrder = 0
              OnClick = sgSpecimensClick
              OnDrawCell = sgSpecimensDrawCell
              ColWidths = (
                142
                187)
            end
            object bbSpecimenAdd: TImageListButton
              Left = 333
              Top = 0
              Width = 24
              Height = 23
              Hint = 'Add new specimen'
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = bbSpecimenAddClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 2
            end
            object bbSpecimenEdit: TImageListButton
              Left = 333
              Top = 23
              Width = 24
              Height = 23
              Hint = 'Edit selected specimen'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = bbSpecimenEditClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 3
            end
            object bbSpecimenDel: TImageListButton
              Left = 333
              Top = 46
              Width = 24
              Height = 23
              Hint = 'Delete selected specimen'
              Anchors = [akTop, akRight]
              TabOrder = 3
              OnClick = bbSpecimenDelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 4
            end
          end
        end
        object tsSources: TTabSheet
          Caption = 'Sources'
          ImageIndex = 5
          object Sources: TSources
            Left = 4
            Top = 4
            Width = 355
            Height = 265
            SourceCol = clRed
            DestCol = clRed
            TabOrder = 0
            DesignSize = (
              355
              265)
          end
        end
        object tsPrivate: TTabSheet
          Caption = 'Private'
          ImageIndex = 6
          DesignSize = (
            365
            273)
          object sgPrivateOcc: TStringGrid
            Left = 0
            Top = 0
            Width = 337
            Height = 97
            Anchors = [akLeft, akTop, akRight, akBottom]
            ColCount = 3
            DefaultColWidth = 190
            DefaultRowHeight = 18
            FixedCols = 0
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
            TabOrder = 0
            OnClick = sgPrivateOccClick
            OnDrawCell = sgPrivateOccDrawCell
            ColWidths = (
              100
              93
              205)
          end
          object gbPrivateDetails: TGroupBox
            Left = 0
            Top = 112
            Width = 365
            Height = 161
            Align = alBottom
            Caption = 'Details'
            Constraints.MinHeight = 155
            TabOrder = 1
            DesignSize = (
              365
              161)
            object Label20: TLabel
              Left = 8
              Top = 72
              Width = 30
              Height = 13
              Caption = 'Detail:'
            end
            object Label21: TLabel
              Left = 184
              Top = 16
              Width = 23
              Height = 13
              Caption = 'Item:'
            end
            object Label17: TLabel
              Left = 8
              Top = 100
              Width = 52
              Height = 13
              Caption = 'Comments:'
            end
            object Label19: TLabel
              Left = 8
              Top = 16
              Width = 27
              Height = 13
              Caption = 'Type:'
            end
            object Label22: TLabel
              Left = 8
              Top = 44
              Width = 26
              Height = 13
              Caption = 'Date:'
            end
            object Label23: TLabel
              Left = 184
              Top = 44
              Width = 30
              Height = 13
              Caption = 'Value:'
            end
            object lblMetadata: TLabel
              Left = 8
              Top = 128
              Width = 274
              Height = 24
              Anchors = [akLeft, akBottom]
              AutoSize = False
              Font.Charset = ARABIC_CHARSET
              Font.Color = clWindowText
              Font.Height = -9
              Font.Name = 'Small Fonts'
              Font.Style = []
              ParentFont = False
              WordWrap = True
            end
            object bbPrivateAccept: TImageListButton
              Left = 297
              Top = 130
              Width = 24
              Height = 23
              Hint = 'Accept changes'
              Anchors = [akRight, akBottom]
              TabOrder = 2
              OnClick = bbPrivateAcceptClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 6
            end
            object bbPrivateDiscard: TImageListButton
              Left = 321
              Top = 130
              Width = 24
              Height = 23
              Hint = 'Discard changes'
              Anchors = [akRight, akBottom]
              TabOrder = 3
              OnClick = bbPrivateDiscardClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 7
            end
            object ePrivateItemName: TEdit
              Left = 216
              Top = 12
              Width = 137
              Height = 21
              AutoSize = False
              MaxLength = 30
              TabOrder = 0
            end
            object ePrivateDetail: TEdit
              Left = 52
              Top = 68
              Width = 301
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              MaxLength = 100
              TabOrder = 1
            end
            object rePrivateComment: TRichEdit
              Left = 64
              Top = 96
              Width = 289
              Height = 25
              Anchors = [akLeft, akTop, akRight, akBottom]
              PopupMenu = dmFormActions.pmRTF
              ScrollBars = ssVertical
              TabOrder = 4
              OnEnter = EnterRTF
              OnExit = ExitRTF
            end
            object dbcmbPrivateType: TDBListCombo
              Left = 52
              Top = 12
              Width = 109
              Height = 21
              Style = csDropDownList
              ItemHeight = 0
              Sorted = True
              TabOrder = 5
              OnChange = dbcmbPrivateTypeChange
              ListField = 'SHORT_NAME'
              KeyField = 'TAXON_PRIVATE_TYPE_KEY'
              Datasource = dmTaxonOccurrences.dsPrivateType
              Active = False
              EmptyItem = False
              ReadOnly = False
            end
            object ePrivateItemDate: TEdit
              Left = 52
              Top = 42
              Width = 97
              Height = 21
              MaxLength = 10
              TabOrder = 6
            end
            object ePrivateItemValue: TEdit
              Left = 216
              Top = 42
              Width = 81
              Height = 21
              MaxLength = 16
              TabOrder = 7
            end
          end
          object bbPrivateAdd: TImageListButton
            Left = 333
            Top = 0
            Width = 24
            Height = 23
            Hint = 'Add new specimen'
            Anchors = [akTop, akRight]
            TabOrder = 2
            OnClick = bbPrivateAddClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 2
          end
          object bbPrivateEdit: TImageListButton
            Left = 333
            Top = 23
            Width = 24
            Height = 23
            Hint = 'Edit selected specimen'
            Anchors = [akTop, akRight]
            TabOrder = 3
            OnClick = bbPrivateEditClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 3
          end
          object bbPrivateDel: TImageListButton
            Left = 333
            Top = 46
            Width = 24
            Height = 23
            Hint = 'Delete selected specimen'
            Anchors = [akTop, akRight]
            TabOrder = 4
            OnClick = bbPrivateDelClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 4
          end
        end
      end
      object bbSave: TImageListButton
        Left = 222
        Top = 328
        Width = 75
        Height = 25
        Hint = 'Save taxon occurrence details'
        Anchors = [akRight, akBottom]
        Caption = '&Save'
        Enabled = False
        TabOrder = 1
        OnClick = bbSaveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
      end
      object bbCancel: TImageListButton
        Left = 306
        Top = 328
        Width = 75
        Height = 25
        Hint = 'Cancel changes'
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = '&Cancel'
        Enabled = False
        TabOrder = 2
        OnClick = bbCancelClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 1
      end
    end
  end
end
