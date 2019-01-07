inherited frmFeatureDetails: TfrmFeatureDetails
  Left = 722
  Top = 274
  Width = 393
  Height = 395
  Caption = 'Features'
  OldCreateOrder = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object pnlDetails: TPanel
    Left = 0
    Top = 0
    Width = 385
    Height = 368
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
      object lblFeature: TLabel
        Left = 56
        Top = 4
        Width = 258
        Height = 13
        Caption = 
          '<Feature                                                        ' +
          '              >'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label2: TLabel
        Left = 8
        Top = 4
        Width = 39
        Height = 13
        Caption = 'Feature:'
      end
      object bbSave: TImageListButton
        Left = 222
        Top = 330
        Width = 75
        Height = 25
        Hint = 'Save feature details'
        Anchors = [akRight, akBottom]
        Caption = '&Save'
        Enabled = False
        TabOrder = 0
        OnClick = bbSaveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
      end
      object bbCancel: TImageListButton
        Left = 304
        Top = 330
        Width = 75
        Height = 25
        Hint = 'Cancel changes'
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = '&Cancel'
        Enabled = False
        TabOrder = 1
        OnClick = bbCancelClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 1
      end
      object pcFeatureDetails: TPageControl
        Left = 8
        Top = 24
        Width = 371
        Height = 301
        ActivePage = tsGeneral
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 2
        OnChange = pcFeatureDetailsChange
        OnChanging = pcFeatureDetailsChanging
        object tsGeneral: TTabSheet
          Caption = 'General'
          ImageIndex = 3
          DesignSize = (
            363
            273)
          object Bevel1: TBevel
            Left = 4
            Top = 4
            Width = 355
            Height = 265
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object Label9: TLabel
            Left = 16
            Top = 50
            Width = 27
            Height = 13
            Caption = 'Type:'
          end
          object Label10: TLabel
            Left = 16
            Top = 136
            Width = 52
            Height = 13
            Caption = 'Comments:'
          end
          object Label29: TLabel
            Left = 16
            Top = 82
            Width = 40
            Height = 13
            Caption = 'Grading:'
          end
          object Label44: TLabel
            Left = 16
            Top = 20
            Width = 31
            Height = 13
            Caption = 'Name:'
          end
          object Label32: TLabel
            Left = 20
            Top = 109
            Width = 26
            Height = 13
            Caption = 'From:'
          end
          object Label31: TLabel
            Left = 164
            Top = 109
            Width = 16
            Height = 13
            Caption = 'To:'
          end
          object dbreComments: TDBRichEdit
            Left = 72
            Top = 136
            Width = 273
            Height = 121
            Anchors = [akLeft, akTop, akRight, akBottom]
            DataField = 'COMMENT'
            DataSource = dmFeatureDetails.dsFeature
            PopupMenu = dmFormActions.pmRTF
            ScrollBars = ssVertical
            TabOrder = 3
            OnEnter = EnterRTF
            OnExit = ExitRTF
          end
          object dbeFeatureName: TDBEdit
            Left = 72
            Top = 16
            Width = 273
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'ITEM_NAME'
            DataSource = dmFeatureDetails.dsFeature
            TabOrder = 0
          end
          object dblcFeatureGrading: TDBLookupComboBox
            Left = 72
            Top = 80
            Width = 273
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'FEATURE_GRADING_KEY'
            DataSource = dmFeatureDetails.dsFeature
            KeyField = 'FEATURE_GRADING_KEY'
            ListField = 'SHORT_NAME'
            ListSource = dmFeatureDetails.dsGrading
            TabOrder = 2
            OnClick = dbComboClick
            OnKeyDown = dbComboKeyDown
            OnKeyUp = dbComboKeyUp
          end
          object cmbFeatureType: TDBListCombo
            Left = 72
            Top = 48
            Width = 273
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            Sorted = True
            TabOrder = 1
            OnChange = cmbFeatureTypeChange
            ListField = 'SHORT_NAME'
            KeyField = 'LOCATION_FEATURE_TYPE_KEY'
            Datasource = dmFeatureDetails.dsType
            Active = False
            EmptyItem = False
            ReadOnly = False
          end
          object eFeatureFrom: TEdit
            Left = 72
            Top = 106
            Width = 77
            Height = 21
            TabOrder = 4
            OnExit = eFeatureFromExit
          end
          object eFeatureTo: TEdit
            Left = 188
            Top = 106
            Width = 77
            Height = 21
            TabOrder = 5
            OnExit = eFeatureToExit
          end
        end
        object tsAims: TTabSheet
          BorderWidth = 4
          Caption = 'Management Aims'
          ImageIndex = 1
          object splManagment: TSplitter
            Left = 0
            Top = 101
            Width = 355
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            Beveled = True
            MinSize = 73
          end
          object gbAimDetails: TGroupBox
            Left = 0
            Top = 104
            Width = 355
            Height = 161
            Align = alBottom
            Caption = 'Details'
            Constraints.MinHeight = 145
            TabOrder = 0
            DesignSize = (
              355
              161)
            object Label21: TLabel
              Left = 8
              Top = 72
              Width = 56
              Height = 13
              Caption = 'Description:'
            end
            object Label19: TLabel
              Left = 8
              Top = 113
              Width = 94
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Next appraisal date:'
            end
            object Label40: TLabel
              Left = 8
              Top = 138
              Width = 78
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Agreement date:'
            end
            object Label41: TLabel
              Left = 8
              Top = 46
              Width = 44
              Height = 13
              Caption = 'Authority:'
            end
            object Label42: TLabel
              Left = 8
              Top = 20
              Width = 31
              Height = 13
              Caption = 'Name:'
            end
            object bbAimAccept: TImageListButton
              Left = 299
              Top = 132
              Width = 24
              Height = 23
              Hint = 'Accept changes'
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 5
              OnClick = bbAimAcceptClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 6
            end
            object bbAimDiscard: TImageListButton
              Left = 323
              Top = 132
              Width = 24
              Height = 23
              Hint = 'Discard changes'
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 6
              OnClick = bbAimDiscardClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 7
            end
            object eAimName: TEdit
              Left = 68
              Top = 16
              Width = 277
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Enabled = False
              TabOrder = 0
            end
            object eNextAppraisal: TEdit
              Left = 108
              Top = 109
              Width = 89
              Height = 21
              Anchors = [akLeft, akBottom]
              TabOrder = 3
              OnExit = eNextAppraisalExit
            end
            object eAgreementDate: TEdit
              Left = 108
              Top = 134
              Width = 89
              Height = 21
              Anchors = [akLeft, akBottom]
              TabOrder = 4
              OnExit = eAgreementDateExit
            end
            object reAimDescription: TRichEdit
              Left = 68
              Top = 68
              Width = 277
              Height = 37
              Anchors = [akLeft, akTop, akRight, akBottom]
              PopupMenu = dmFormActions.pmRTF
              ScrollBars = ssVertical
              TabOrder = 2
              OnEnter = EnterRTF
              OnExit = ExitRTF
            end
            object eAimAuthority: TNameLinkedEdit
              Tag = 1
              Left = 68
              Top = 41
              Width = 277
              Height = 23
              TabOrder = 1
              BorderStyle = bsSingle
              ImageIndex = 5
              ImageList = dmFormActions.ilButtons
              OnFindData = eAimAuthorityFindData
              OnGetData = eAimAuthorityGetData
            end
          end
          object pnlManagementTop: TPanel
            Left = 0
            Top = 0
            Width = 355
            Height = 101
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              355
              101)
            object sgAims: TStringGrid
              Left = 0
              Top = 0
              Width = 331
              Height = 97
              Anchors = [akLeft, akTop, akRight, akBottom]
              ColCount = 2
              DefaultRowHeight = 18
              FixedCols = 0
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
              TabOrder = 0
              OnClick = sgAimsClick
              OnDrawCell = sgAimsDrawCell
              ColWidths = (
                231
                94)
            end
            object bbAimAdd: TImageListButton
              Left = 331
              Top = 0
              Width = 24
              Height = 23
              Hint = 'Add new management aim'
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = bbAimAddClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 2
            end
            object bbAimEdit: TImageListButton
              Left = 331
              Top = 23
              Width = 24
              Height = 23
              Hint = 'Edit selected management aim'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = bbAimEditClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 3
            end
            object bbAimDel: TImageListButton
              Left = 331
              Top = 46
              Width = 24
              Height = 23
              Hint = 'Delete selected management aim'
              Anchors = [akTop, akRight]
              TabOrder = 3
              OnClick = bbAimDelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 4
            end
          end
        end
        object tsPotentialThreats: TTabSheet
          BorderWidth = 4
          Caption = 'Pot.Threats'
          ImageIndex = 2
          object splThreats: TSplitter
            Left = 0
            Top = 101
            Width = 355
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            Beveled = True
            MinSize = 73
          end
          object gbPotentialDetails: TGroupBox
            Left = 0
            Top = 104
            Width = 355
            Height = 161
            Align = alBottom
            Caption = 'Details'
            Constraints.MinHeight = 124
            TabOrder = 0
            DesignSize = (
              355
              161)
            object Label25: TLabel
              Left = 8
              Top = 20
              Width = 27
              Height = 13
              Caption = 'Type:'
            end
            object Label26: TLabel
              Left = 8
              Top = 67
              Width = 52
              Height = 13
              Caption = 'Comments:'
            end
            object Label12: TLabel
              Left = 8
              Top = 44
              Width = 34
              Height = 13
              Caption = 'Threat:'
            end
            object bbPotentialAccept: TImageListButton
              Left = 295
              Top = 128
              Width = 24
              Height = 23
              Hint = 'Accept changes'
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 3
              OnClick = bbPotentialAcceptClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 6
            end
            object bbPotentialDiscard: TImageListButton
              Left = 319
              Top = 128
              Width = 24
              Height = 23
              Hint = 'Discard changes'
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 4
              OnClick = bbPotentialDiscardClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 7
            end
            object eThreat: TEdit
              Left = 64
              Top = 40
              Width = 279
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 1
            end
            object reThreatComments: TRichEdit
              Left = 64
              Top = 64
              Width = 279
              Height = 58
              Anchors = [akLeft, akTop, akRight, akBottom]
              PopupMenu = dmFormActions.pmRTF
              ScrollBars = ssVertical
              TabOrder = 2
              OnEnter = EnterRTF
              OnExit = ExitRTF
            end
            object cmbThreatType: TDBListCombo
              Left = 64
              Top = 16
              Width = 279
              Height = 21
              Style = csDropDownList
              Anchors = [akLeft, akTop, akRight]
              ItemHeight = 0
              Sorted = True
              TabOrder = 0
              ListField = 'SHORT_NAME'
              KeyField = 'THREAT_TYPE_KEY'
              Datasource = dmFeatureDetails.dsThreatType
              Active = False
              EmptyItem = False
              ReadOnly = False
            end
          end
          object pnlThreatsTop: TPanel
            Left = 0
            Top = 0
            Width = 355
            Height = 101
            Align = alClient
            Alignment = taLeftJustify
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              355
              101)
            object sgPotentialThreats: TStringGrid
              Left = 0
              Top = 0
              Width = 331
              Height = 97
              Anchors = [akLeft, akTop, akRight, akBottom]
              ColCount = 2
              DefaultRowHeight = 18
              FixedCols = 0
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
              TabOrder = 0
              OnClick = sgPotentialThreatsClick
              OnDrawCell = sgPotentialThreatsDrawCell
              ColWidths = (
                156
                168)
            end
            object bbPotentialAdd: TImageListButton
              Left = 331
              Top = 0
              Width = 24
              Height = 23
              Hint = 'Add new potential threat'
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = bbPotentialAddClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 2
            end
            object bbPotentialEdit: TImageListButton
              Left = 331
              Top = 23
              Width = 24
              Height = 23
              Hint = 'Edit selected potential threat'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = bbPotentialEditClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 3
            end
            object bbPotentialDel: TImageListButton
              Left = 331
              Top = 46
              Width = 24
              Height = 23
              Hint = 'Delete selected potential threat'
              Anchors = [akTop, akRight]
              TabOrder = 3
              OnClick = bbPotentialDelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 4
            end
          end
        end
        object tsDamages: TTabSheet
          BorderWidth = 4
          Caption = 'Damage Occs'
          ImageIndex = 3
          object splDamage: TSplitter
            Left = 0
            Top = 101
            Width = 355
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            Beveled = True
            MinSize = 73
          end
          object gbDamageDetails: TGroupBox
            Left = 0
            Top = 104
            Width = 355
            Height = 161
            Align = alBottom
            Caption = 'Details'
            Constraints.MinHeight = 105
            TabOrder = 0
            DesignSize = (
              355
              161)
            object Label35: TLabel
              Left = 12
              Top = 47
              Width = 47
              Height = 13
              Caption = 'Comment:'
            end
            object Label43: TLabel
              Left = 12
              Top = 20
              Width = 26
              Height = 13
              Caption = 'Date:'
            end
            object bbDamageAccept: TImageListButton
              Left = 295
              Top = 128
              Width = 24
              Height = 23
              Hint = 'Accept changes'
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 2
              OnClick = bbDamageAcceptClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 6
            end
            object bbDamageDiscard: TImageListButton
              Left = 319
              Top = 128
              Width = 24
              Height = 23
              Hint = 'Discard changes'
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 3
              OnClick = bbDamageDiscardClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 7
            end
            object eOccurrenceDate: TVagueDateEdit
              Left = 64
              Top = 16
              Width = 133
              Height = 21
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGreen
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
              OnExit = eOccurrenceDateExit
            end
            object reDamageComments: TRichEdit
              Left = 64
              Top = 44
              Width = 280
              Height = 77
              Anchors = [akLeft, akTop, akRight, akBottom]
              PopupMenu = dmFormActions.pmRTF
              ScrollBars = ssVertical
              TabOrder = 1
              OnEnter = EnterRTF
              OnExit = ExitRTF
            end
          end
          object pnlDamageTop: TPanel
            Left = 0
            Top = 0
            Width = 355
            Height = 101
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              355
              101)
            object bbDamageAdd: TImageListButton
              Left = 331
              Top = 0
              Width = 24
              Height = 23
              Hint = 'Add new damage occurrence'
              Anchors = [akTop, akRight]
              TabOrder = 0
              OnClick = bbDamageAddClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 2
            end
            object bbDamageEdit: TImageListButton
              Left = 331
              Top = 23
              Width = 24
              Height = 23
              Hint = 'Edit selected damage occurrence'
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = bbDamageEditClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 3
            end
            object bbDamageDel: TImageListButton
              Left = 331
              Top = 46
              Width = 24
              Height = 23
              Hint = 'Delete selected damage occurrence'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = bbDamageDelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 4
            end
            object sgDamageOccurrences: TStringGrid
              Left = 0
              Top = 0
              Width = 331
              Height = 97
              Anchors = [akLeft, akTop, akRight, akBottom]
              ColCount = 2
              DefaultRowHeight = 18
              FixedCols = 0
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
              TabOrder = 3
              OnClick = sgDamageOccurrencesClick
              OnDrawCell = sgDamageOccurrencesDrawCell
              ColWidths = (
                69
                256)
            end
          end
        end
        object tsSources: TTabSheet
          Caption = 'Sources'
          ImageIndex = 4
          object Sources: TSources
            Left = 4
            Top = 4
            Width = 355
            Height = 265
            SourceCol = clBlue
            DestCol = clRed
            TabOrder = 0
            DesignSize = (
              355
              265)
          end
        end
      end
    end
  end
end
