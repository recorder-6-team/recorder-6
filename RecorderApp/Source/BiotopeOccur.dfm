object frmBiotopeOccurrences: TfrmBiotopeOccurrences
  Left = 770
  Top = 116
  Width = 393
  Height = 395
  Hint = 'Save Biotope Occurrence Details'
  Caption = 'Biotope Occurrences'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  ShowHint = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object pnlDetails: TPanel
    Left = 0
    Top = 0
    Width = 385
    Height = 361
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
    OnResize = pnlDetailsResize
    object pnlInner: TPanel
      Left = 0
      Top = 0
      Width = 385
      Height = 361
      BevelOuter = bvLowered
      TabOrder = 0
      DesignSize = (
        385
        361)
      object lblBiotope: TLabel
        Left = 8
        Top = 4
        Width = 298
        Height = 13
        AutoSize = False
        Caption = 'C43.71, Mixed Western white oak (Quercus pubecens) [Corine]'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object pcBiotopeOccurrence: TPageControl
        Left = 8
        Top = 24
        Width = 373
        Height = 301
        ActivePage = tsGeneral
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        OnChange = pcBiotopeOccurrenceChange
        OnChanging = pcBiotopeOccurrenceChanging
        object tsGeneral: TTabSheet
          Caption = 'General'
          DesignSize = (
            365
            273)
          object Bevel1: TBevel
            Left = 4
            Top = 4
            Width = 357
            Height = 265
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object Label5: TLabel
            Left = 12
            Top = 16
            Width = 67
            Height = 13
            Caption = 'Surveyor'#39's ref:'
          end
          object Label2: TLabel
            Left = 12
            Top = 40
            Width = 52
            Height = 13
            Caption = 'Comments:'
          end
          object lblVerificationStatus: TLabel
            Left = 288
            Top = 237
            Width = 63
            Height = 13
            Alignment = taRightJustify
            Anchors = [akRight, akBottom]
            BiDiMode = bdLeftToRight
            Caption = 'Not validated'
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentBiDiMode = False
            ParentColor = False
            ParentFont = False
          end
          object Label18: TLabel
            Left = 12
            Top = 216
            Width = 50
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Det. Type:'
          end
          object dbreComments: TDBRichEdit
            Left = 12
            Top = 56
            Width = 341
            Height = 153
            Anchors = [akLeft, akTop, akRight, akBottom]
            DataField = 'COMMENT'
            DataSource = dmBiotopeOccurrences.dsBiotopeOcc
            PopupMenu = dmFormActions.pmRTF
            ScrollBars = ssVertical
            TabOrder = 1
            OnEnter = EnterRTF
            OnExit = ExitRTF
          end
          object dbeSurveyorRef: TDBEdit
            Left = 88
            Top = 12
            Width = 265
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'SURVEYORS_REF'
            DataSource = dmBiotopeOccurrences.dsBiotopeOcc
            TabOrder = 0
          end
          object dbcbChecked: TDBCheckBox
            Left = 12
            Top = 236
            Width = 69
            Height = 17
            Anchors = [akLeft, akBottom]
            Caption = 'Checked'
            DataField = 'CHECKED'
            DataSource = dmBiotopeOccurrences.dsBiotopeOcc
            TabOrder = 2
            ValueChecked = 'True'
            ValueUnchecked = 'False'
            OnMouseUp = dbCheckBoxMouseUp
          end
          object dbcbDigitised: TDBCheckBox
            Left = 96
            Top = 236
            Width = 69
            Height = 17
            Anchors = [akLeft, akBottom]
            Caption = 'Digitised'
            DataField = 'DIGITISED'
            DataSource = dmBiotopeOccurrences.dsBiotopeOcc
            TabOrder = 3
            ValueChecked = 'True'
            ValueUnchecked = 'False'
            OnMouseUp = dbCheckBoxMouseUp
          end
          object cmbDeterminationType: TDBListCombo
            Left = 92
            Top = 212
            Width = 261
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akRight, akBottom]
            ItemHeight = 13
            Sorted = True
            TabOrder = 4
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
          BorderWidth = 4
          Caption = 'Determinations'
          ImageIndex = 2
          object splDeterminations: TSplitter
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
          object gbDetDetails: TGroupBox
            Left = 0
            Top = 88
            Width = 357
            Height = 177
            Align = alBottom
            Caption = 'Details'
            Constraints.MinHeight = 176
            TabOrder = 0
            DesignSize = (
              357
              177)
            object Label1: TLabel
              Left = 8
              Top = 20
              Width = 39
              Height = 13
              Caption = 'Biotope:'
            end
            object Label6: TLabel
              Left = 8
              Top = 46
              Width = 54
              Height = 13
              Caption = 'Determiner:'
            end
            object Label3: TLabel
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
            object Label4: TLabel
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
            object bbFindBiotope: TImageListButton
              Left = 327
              Top = 16
              Width = 22
              Height = 22
              Hint = 'Get biotope'
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = bbFindBiotopeClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 5
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
            object reDetComments: TRichEdit
              Left = 64
              Top = 146
              Width = 233
              Height = 24
              Anchors = [akLeft, akTop, akRight, akBottom]
              Lines.Strings = (
                'reDetComments')
              PopupMenu = dmFormActions.pmRTF
              ScrollBars = ssVertical
              TabOrder = 9
              OnEnter = EnterRTF
              OnExit = ExitRTF
            end
            object eBiotope: TEdit
              Left = 64
              Top = 16
              Width = 261
              Height = 22
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
              Text = 'eBiotope'
              OnKeyPress = eBiotopeKeyPress
            end
            object eWork: TEdit
              Left = 64
              Top = 120
              Width = 261
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 7
              Text = 'eWork'
              OnDblClick = eWorkDblClick
              OnKeyPress = eWorkKeyPress
            end
            object cbPreferred: TCheckBox
              Left = 252
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
              Width = 145
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
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
              ItemHeight = 13
              Sorted = True
              TabOrder = 5
              ListField = 'SHORT_NAME'
              KeyField = 'DETERMINATION_TYPE_KEY'
              Datasource = dmDetermination.dsDetType
              Active = False
              EmptyItem = False
              ReadOnly = False
            end
            object bbFindRef: TImageListButton
              Left = 327
              Top = 120
              Width = 22
              Height = 22
              Hint = 'Get document'
              Anchors = [akTop, akRight]
              TabOrder = 8
              OnClick = bbFindWorkClick
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
          object pnlDetTop: TPanel
            Left = 0
            Top = 0
            Width = 357
            Height = 85
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
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
          end
        end
        object tsSources: TTabSheet
          Caption = 'Sources'
          ImageIndex = 3
          object Sources: TSources
            Left = 4
            Top = 4
            Width = 355
            Height = 265
            SourceCol = clBlue
            DestCol = clRed
            TabOrder = 0
          end
        end
      end
      object bbSave: TImageListButton
        Left = 222
        Top = 330
        Width = 75
        Height = 25
        Hint = 'Save biotope occurrence details'
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
        Top = 330
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
