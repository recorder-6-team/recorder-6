object frmLocationDetails: TfrmLocationDetails
  Left = 544
  Top = 227
  Width = 393
  Height = 395
  Hint = 'View and manage locations list'
  Caption = 'Location Details'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
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
      object lblLocationDisp: TLabel
        Left = 8
        Top = 4
        Width = 53
        Height = 13
        Caption = '<Location>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object bbSave: TImageListButton
        Left = 222
        Top = 330
        Width = 75
        Height = 25
        Hint = 'Save location details'
        Anchors = [akRight, akBottom]
        Caption = '&Save'
        Enabled = False
        TabOrder = 0
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
        TabOrder = 1
        OnClick = bbCancelClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 1
      end
      object pcLocationDetails: TPageControl
        Left = 8
        Top = 24
        Width = 373
        Height = 301
        ActivePage = tsGeneral
        Anchors = [akLeft, akTop, akRight, akBottom]
        Constraints.MinHeight = 140
        TabOrder = 2
        OnChange = pcLocationDetailsChange
        OnChanging = pcDetailsChanging
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
          object Label3: TLabel
            Left = 188
            Top = 112
            Width = 27
            Height = 13
            Caption = 'Type:'
          end
          object Label4: TLabel
            Left = 12
            Top = 179
            Width = 56
            Height = 13
            Caption = 'Description:'
          end
          object lblPreferredNamePrompt: TLabel
            Left = 12
            Top = 12
            Width = 46
            Height = 26
            Caption = 'Preferred Name:'
            WordWrap = True
          end
          object Label22: TLabel
            Left = 12
            Top = 112
            Width = 47
            Height = 13
            Caption = 'File Code:'
          end
          object Label13: TLabel
            Left = 12
            Top = 140
            Width = 55
            Height = 26
            Caption = 'Central Spatial Ref:'
            WordWrap = True
          end
          object lblPreferredName: TLabel
            Left = 72
            Top = 12
            Width = 86
            Height = 13
            Caption = '<Preferred Name>'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object lblLocationNames: TLabel
            Left = 12
            Top = 38
            Width = 42
            Height = 13
            Caption = 'Name(s):'
          end
          object Label7: TLabel
            Left = 12
            Top = 232
            Width = 59
            Height = 26
            Anchors = [akLeft, akBottom]
            Caption = 'Last Survey Date:'
            WordWrap = True
          end
          object lblLastSurveyDate: TLabel
            Left = 72
            Top = 232
            Width = 3
            Height = 13
            Anchors = [akLeft, akBottom]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object lblStatus: TLabel
            Left = 212
            Top = 232
            Width = 133
            Height = 25
            Anchors = [akLeft, akBottom]
            AutoSize = False
            Caption = 'Status  '
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object dbreLocDescription: TDBRichEdit
            Left = 72
            Top = 176
            Width = 277
            Height = 49
            Anchors = [akLeft, akTop, akRight, akBottom]
            DataField = 'DESCRIPTION'
            DataSource = dmLocationDetails.dsLocation
            PopupMenu = dmFormActions.pmRTF
            ScrollBars = ssVertical
            TabOrder = 7
            OnEnter = EnterRTF
            OnExit = ExitRTF
          end
          object SpatialRef: TSpatialRef
            Left = 72
            Top = 136
            Width = 277
            Height = 33
            TabOrder = 6
            DropDownMenu = pmMapWindow
            ImageIndex = 5
            ImageList = dmFormActions.ilButtons
            OnExit = SpatialRefExit
            OnGetFromMap = SpatialRefGetFromMap
            OnInvalidSpatialRef = SpatialRefInvalidSpatialRef
          end
          object clbLocationNames: TCheckListBox
            Left = 72
            Top = 36
            Width = 253
            Height = 69
            OnClickCheck = clbLocationNamesClickCheck
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            Sorted = True
            TabOrder = 0
            OnClick = clbLocationNamesClick
          end
          object dbcmbLocationType: TDBLookupComboBox
            Left = 220
            Top = 108
            Width = 129
            Height = 21
            DataField = 'LOCATION_TYPE_KEY'
            DataSource = dmLocationDetails.dsLocation
            KeyField = 'LOCATION_TYPE_KEY'
            ListField = 'SHORT_NAME'
            ListSource = dmLocationDetails.dsLocationType
            TabOrder = 5
          end
          object dbeFileCode: TDBEdit
            Left = 72
            Top = 108
            Width = 105
            Height = 21
            DataField = 'FILE_CODE'
            DataSource = dmLocationDetails.dsLocation
            TabOrder = 4
          end
          object bbLocNameAdd: TImageListButton
            Left = 325
            Top = 36
            Width = 24
            Height = 23
            Hint = 'Add new location name'
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnClick = bbLocNameAddClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 2
          end
          object bbLocNameEdit: TImageListButton
            Left = 325
            Top = 59
            Width = 24
            Height = 23
            Hint = 'Edit selected location name'
            Anchors = [akTop, akRight]
            TabOrder = 2
            OnClick = bbLocNameEditClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 3
          end
          object bbLocNameDel: TImageListButton
            Left = 325
            Top = 82
            Width = 24
            Height = 23
            Hint = 'Delete selected location name'
            Anchors = [akTop, akRight]
            TabOrder = 3
            OnClick = bbLocNameDelClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 4
          end
        end
        object tsDesignations: TTabSheet
          Caption = 'Designations'
          ImageIndex = 5
          object splDesignations: TSplitter
            Left = 0
            Top = 101
            Width = 365
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            Beveled = True
            MinSize = 77
          end
          object pnlDesignationsTop: TPanel
            Left = 0
            Top = 0
            Width = 365
            Height = 101
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              365
              101)
            object bbDesignationAdd: TImageListButton
              Left = 335
              Top = 4
              Width = 24
              Height = 23
              Hint = 'Add new designation'
              Anchors = [akTop, akRight]
              TabOrder = 0
              OnClick = bbDesignationAddClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 2
            end
            object sgDesignations: TStringGrid
              Left = 4
              Top = 4
              Width = 331
              Height = 93
              Anchors = [akLeft, akTop, akRight, akBottom]
              ColCount = 4
              DefaultRowHeight = 18
              FixedCols = 0
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
              TabOrder = 1
              OnClick = sgDesignationsClick
              OnDrawCell = DrawCellChoppedText
              ColWidths = (
                94
                106
                66
                61)
            end
            object bbDesignationEdit: TImageListButton
              Left = 335
              Top = 27
              Width = 24
              Height = 23
              Hint = 'Edit the selected designation'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = bbDesignationEditClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 3
            end
            object bbDesignationDel: TImageListButton
              Left = 335
              Top = 50
              Width = 24
              Height = 23
              Hint = 'Delete the selected designation'
              Anchors = [akTop, akRight]
              TabOrder = 3
              OnClick = bbDesignationDelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 4
            end
          end
          object pnlDesignationsBottom: TPanel
            Left = 0
            Top = 104
            Width = 365
            Height = 169
            Align = alBottom
            BevelOuter = bvNone
            BorderWidth = 3
            Constraints.MinHeight = 163
            TabOrder = 1
            object gbDesignationDetails: TGroupBox
              Left = 3
              Top = 3
              Width = 359
              Height = 163
              Align = alClient
              Caption = 'Details'
              TabOrder = 0
              DesignSize = (
                359
                163)
              object Label31: TLabel
                Left = 172
                Top = 93
                Width = 16
                Height = 13
                Caption = 'To:'
              end
              object Label32: TLabel
                Left = 12
                Top = 93
                Width = 26
                Height = 13
                Caption = 'From:'
              end
              object Label33: TLabel
                Left = 12
                Top = 112
                Width = 52
                Height = 13
                Caption = 'Comments:'
              end
              object Label34: TLabel
                Left = 12
                Top = 66
                Width = 44
                Height = 13
                Caption = 'Authority:'
                WordWrap = True
              end
              object Label35: TLabel
                Left = 12
                Top = 15
                Width = 59
                Height = 13
                Caption = 'Designation:'
              end
              object Label1: TLabel
                Left = 12
                Top = 40
                Width = 47
                Height = 13
                Caption = 'Ref code:'
              end
              object eDesRefCode: TEdit
                Left = 76
                Top = 37
                Width = 117
                Height = 21
                TabOrder = 1
              end
              object reDesignComments: TRichEdit
                Left = 12
                Top = 128
                Width = 287
                Height = 29
                Anchors = [akLeft, akTop, akRight, akBottom]
                PopupMenu = dmFormActions.pmRTF
                ScrollBars = ssVertical
                TabOrder = 5
                OnEnter = EnterRTF
                OnExit = ExitRTF
              end
              object dbcmbSiteStatus: TDBListCombo
                Left = 76
                Top = 12
                Width = 271
                Height = 21
                Style = csDropDownList
                Anchors = [akLeft, akTop, akRight]
                ItemHeight = 0
                Sorted = True
                TabOrder = 0
                ListField = 'SHORT_NAME'
                KeyField = 'SITE_STATUS_KEY'
                Datasource = dmLocationDetails.dsSiteStatus
                Active = False
                EmptyItem = False
                ReadOnly = False
              end
              object eDesignationFrom: TEdit
                Left = 76
                Top = 90
                Width = 77
                Height = 21
                TabOrder = 3
                OnExit = eDesignationFromExit
              end
              object eDesignationTo: TEdit
                Left = 196
                Top = 90
                Width = 77
                Height = 21
                TabOrder = 4
                OnExit = eDesignationToExit
              end
              object bbDesignationDiscard: TImageListButton
                Left = 329
                Top = 134
                Width = 24
                Height = 23
                Hint = 'Discard changes'
                Anchors = [akRight, akBottom]
                Enabled = False
                TabOrder = 7
                OnClick = bbDesignationDiscardClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 7
              end
              object bbDesignationAccept: TImageListButton
                Left = 305
                Top = 134
                Width = 24
                Height = 23
                Hint = 'Accept changes'
                Anchors = [akRight, akBottom]
                Enabled = False
                TabOrder = 6
                OnClick = bbDesignationAcceptClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 6
              end
              object eDesAuthority: TNameLinkedEdit
                Tag = 1
                Left = 76
                Top = 62
                Width = 269
                Height = 23
                TabOrder = 2
                BorderStyle = bsSingle
                ImageIndex = 5
                ImageList = dmFormActions.ilButtons
                OnFindData = eDesAuthorityFindData
                OnGetData = eDesAuthorityGetData
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
        object tsGeoInfo: TTabSheet
          Caption = 'Geo Info'
          ImageIndex = 2
          DesignSize = (
            365
            273)
          object pcGeoInfo: TPageControl
            Left = 4
            Top = 4
            Width = 355
            Height = 265
            ActivePage = tsBoundaries
            Anchors = [akLeft, akTop, akRight, akBottom]
            TabOrder = 0
            OnChange = pcGeoInfoChange
            OnChanging = pcDetailsChanging
            object tsAdminAreas: TTabSheet
              Caption = 'Administrative Areas'
              ImageIndex = 4
              DesignSize = (
                347
                237)
              object Shape3: TShape
                Tag = 2
                Left = 3
                Top = 3
                Width = 316
                Height = 231
                Anchors = [akLeft, akTop, akRight, akBottom]
                Pen.Color = clRed
              end
              object sgAdminAreas: TStringGrid
                Left = 4
                Top = 4
                Width = 314
                Height = 229
                Anchors = [akLeft, akTop, akRight, akBottom]
                ColCount = 2
                DefaultRowHeight = 18
                FixedCols = 0
                RowCount = 2
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
                TabOrder = 0
                OnClick = sgAdminAreasClick
                OnDrawCell = DrawCellChoppedText
                ColWidths = (
                  216
                  83)
              end
              object bbAdminDel: TImageListButton
                Left = 319
                Top = 27
                Width = 24
                Height = 23
                Hint = 'Remove the selected administrative area'
                Anchors = [akTop, akRight]
                TabOrder = 2
                OnClick = bbAdminDelClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 4
              end
              object bbAdminAdd: TImageListButton
                Left = 319
                Top = 4
                Width = 24
                Height = 23
                Hint = 'Add an administrative area'
                Anchors = [akTop, akRight]
                TabOrder = 1
                OnClick = bbAdminAddClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 2
              end
            end
            object tsGridSquares: TTabSheet
              Caption = 'Grid Squares'
              ImageIndex = 3
              DesignSize = (
                347
                237)
              object pnlExtractingGridSquares: TPanel
                Left = 4
                Top = 4
                Width = 315
                Height = 229
                Anchors = [akLeft, akTop, akRight, akBottom]
                BevelInner = bvRaised
                BevelOuter = bvLowered
                Caption = 'Extracting Grid Squares'
                TabOrder = 5
              end
              object sgGridSquares: TStringGrid
                Left = 4
                Top = 4
                Width = 315
                Height = 229
                Anchors = [akLeft, akTop, akRight, akBottom]
                ColCount = 2
                DefaultRowHeight = 18
                FixedCols = 0
                RowCount = 2
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
                TabOrder = 0
                OnClick = sgGridSquaresClick
                OnDrawCell = sgGridSquaresDrawCell
                OnSetEditText = sgGridSquaresSetEditText
                ColWidths = (
                  141
                  142)
              end
              object cmbSquareType: TComboBox
                Left = 147
                Top = 23
                Width = 146
                Height = 21
                Style = csDropDownList
                ItemHeight = 13
                Sorted = True
                TabOrder = 1
                Visible = False
                OnChange = cmbSquareTypeChange
                OnExit = ComboBoxExit
                Items.Strings = (
                  '100km'
                  '10km'
                  '1km'
                  'Hectare'
                  'Tetrad')
              end
              object bbSquareAdd: TImageListButton
                Left = 319
                Top = 4
                Width = 24
                Height = 23
                Hint = 'Add a grid square'
                Anchors = [akTop, akRight]
                TabOrder = 2
                OnClick = bbSquareAddClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 2
              end
              object bbSquareDel: TImageListButton
                Left = 319
                Top = 27
                Width = 24
                Height = 23
                Hint = 'Remove the selected grid square'
                Anchors = [akTop, akRight]
                TabOrder = 3
                OnClick = bbSquareDelClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 4
              end
              object btnExtractGridSquares: TImageListButton
                Left = 319
                Top = 210
                Width = 24
                Height = 23
                Hint = 
                  'Automatically extract grid squares from the associated location ' +
                  'boundaries'
                Anchors = [akRight, akBottom]
                TabOrder = 4
                OnClick = btnExtractGridSquaresClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 23
              end
            end
            object tsLandParcel: TTabSheet
              Caption = 'Land Parcel'
              ImageIndex = 2
              DesignSize = (
                347
                237)
              object sgLandParcels: TStringGrid
                Left = 4
                Top = 4
                Width = 315
                Height = 229
                Anchors = [akLeft, akTop, akRight, akBottom]
                ColCount = 2
                DefaultRowHeight = 18
                FixedCols = 0
                RowCount = 2
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
                TabOrder = 0
                OnClick = sgLandParcelsClick
                OnDrawCell = sgLandParcelsDrawCell
                ColWidths = (
                  87
                  212)
              end
              object bbLandDel: TImageListButton
                Left = 319
                Top = 27
                Width = 24
                Height = 23
                Hint = 'Remove the selected land parcel'
                Anchors = [akTop, akRight]
                TabOrder = 2
                OnClick = bbLandDelClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 4
              end
              object bbLandAdd: TImageListButton
                Left = 319
                Top = 4
                Width = 24
                Height = 23
                Hint = 'Add new land parcel'
                Anchors = [akTop, akRight]
                TabOrder = 1
                OnClick = bbLandAddClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 2
              end
            end
            object tsBoundaries: TTabSheet
              Caption = 'Boundaries'
              DesignSize = (
                347
                237)
              object sgBoundaries: TStringGrid
                Left = 4
                Top = 4
                Width = 315
                Height = 101
                Anchors = [akLeft, akTop, akRight, akBottom]
                ColCount = 3
                DefaultRowHeight = 18
                FixedCols = 0
                RowCount = 2
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
                TabOrder = 0
                OnClick = sgBoundariesClick
                OnDrawCell = DrawCellChoppedText
                ColWidths = (
                  137
                  85
                  84)
              end
              object gbBoundaryDetails: TGroupBox
                Left = 4
                Top = 96
                Width = 339
                Height = 137
                Anchors = [akLeft, akRight, akBottom]
                Caption = 'Details'
                TabOrder = 4
                DesignSize = (
                  339
                  137)
                object Label21: TLabel
                  Left = 196
                  Top = 19
                  Width = 16
                  Height = 13
                  Caption = 'To:'
                end
                object Label20: TLabel
                  Left = 12
                  Top = 19
                  Width = 26
                  Height = 13
                  Caption = 'From:'
                end
                object Label5: TLabel
                  Left = 12
                  Top = 43
                  Width = 38
                  Height = 13
                  Caption = 'Version:'
                end
                object Label2: TLabel
                  Left = 12
                  Top = 67
                  Width = 43
                  Height = 13
                  Caption = 'Map File:'
                end
                object Label14: TLabel
                  Left = 12
                  Top = 111
                  Width = 48
                  Height = 13
                  Anchors = [akLeft, akBottom]
                  Caption = 'Object ID:'
                end
                object lblLinkedField: TLabel
                  Left = 12
                  Top = 89
                  Width = 64
                  Height = 13
                  Caption = 'lblLinkedField'
                  Visible = False
                end
                object eBoundaryVersion: TEdit
                  Left = 72
                  Top = 40
                  Width = 113
                  Height = 21
                  Enabled = False
                  TabOrder = 2
                end
                object eBoundaryFrom: TVagueDateEdit
                  Left = 72
                  Top = 16
                  Width = 113
                  Height = 21
                  Enabled = False
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clGreen
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 0
                  OnExit = eBoundaryFromExit
                end
                object eBoundaryTo: TVagueDateEdit
                  Left = 218
                  Top = 16
                  Width = 114
                  Height = 21
                  Enabled = False
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clGreen
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 1
                  OnExit = eBoundaryToExit
                end
                object bbBoundaryAccept: TImageListButton
                  Left = 284
                  Top = 108
                  Width = 24
                  Height = 23
                  Hint = 'Accept changes'
                  Anchors = [akRight, akBottom]
                  Enabled = False
                  TabOrder = 5
                  OnClick = bbBoundaryAcceptClick
                  ImageList = dmFormActions.ilButtons
                  ImageIndex = 6
                end
                object bbBoundaryDiscard: TImageListButton
                  Left = 308
                  Top = 108
                  Width = 24
                  Height = 23
                  Hint = 'Discard changes'
                  Anchors = [akRight, akBottom]
                  Enabled = False
                  TabOrder = 6
                  OnClick = bbBoundaryDiscardClick
                  ImageList = dmFormActions.ilButtons
                  ImageIndex = 7
                end
                object btnGetGISObject: TImageListButton
                  Left = 239
                  Top = 108
                  Width = 24
                  Height = 23
                  Anchors = [akRight, akBottom]
                  TabOrder = 3
                  OnClick = btnGetGISObjectClick
                  ImageList = dmFormActions.ilButtons
                  ImageIndex = 5
                end
                object btnGetGISObjectDropDown: TButton
                  Left = 262
                  Top = 108
                  Width = 16
                  Height = 23
                  Anchors = [akRight, akBottom]
                  Caption = '6'
                  Font.Charset = SYMBOL_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'Marlett'
                  Font.Style = [fsBold]
                  ParentFont = False
                  TabOrder = 4
                  OnClick = btnGetGISObjectDropDownClick
                end
                object cmbMapFile: TComboBox
                  Left = 72
                  Top = 64
                  Width = 239
                  Height = 21
                  Style = csDropDownList
                  Anchors = [akLeft, akTop, akRight]
                  Enabled = False
                  ItemHeight = 0
                  TabOrder = 7
                  OnChange = cmbMapFileChange
                end
                object btnMapFileBrowse: TButton
                  Left = 311
                  Top = 64
                  Width = 21
                  Height = 21
                  Anchors = [akTop, akRight]
                  Caption = '...'
                  TabOrder = 8
                  OnClick = btnMapFileBrowseClick
                end
                object cmbGISObjectID: TComboBox
                  Left = 72
                  Top = 108
                  Width = 163
                  Height = 21
                  Style = csDropDownList
                  Anchors = [akLeft, akRight, akBottom]
                  ItemHeight = 0
                  TabOrder = 9
                end
              end
              object bbBoundaryAdd: TImageListButton
                Left = 319
                Top = 4
                Width = 24
                Height = 23
                Hint = 'Add new boundary'
                Anchors = [akTop, akRight]
                TabOrder = 1
                OnClick = bbBoundaryAddClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 2
              end
              object bbBoundaryEdit: TImageListButton
                Left = 319
                Top = 27
                Width = 24
                Height = 23
                Hint = 'Edit the selected boundary'
                Anchors = [akTop, akRight]
                TabOrder = 2
                OnClick = bbBoundaryEditClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 3
              end
              object bbBoundaryDel: TImageListButton
                Left = 319
                Top = 50
                Width = 24
                Height = 23
                Hint = 'Delete the selected boundary'
                Anchors = [akTop, akRight]
                TabOrder = 3
                OnClick = bbBoundaryDelClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 4
              end
            end
          end
        end
        object tsOtherInfo: TTabSheet
          Caption = 'Other Info'
          ImageIndex = 4
          DesignSize = (
            365
            273)
          object pcOtherInfo: TPageControl
            Left = 4
            Top = 4
            Width = 355
            Height = 265
            ActivePage = tsRelations
            Anchors = [akLeft, akTop, akRight, akBottom]
            TabOrder = 0
            OnChange = pcOtherInfoChange
            OnChanging = pcDetailsChanging
            object tsRelations: TTabSheet
              Caption = 'Relations'
              ImageIndex = 1
              DesignSize = (
                347
                237)
              object Shape2: TShape
                Tag = 2
                Left = 3
                Top = 3
                Width = 316
                Height = 231
                Anchors = [akLeft, akTop, akRight, akBottom]
                Brush.Style = bsClear
                Pen.Color = clRed
              end
              object sgRelations: TStringGrid
                Left = 4
                Top = 4
                Width = 314
                Height = 229
                Anchors = [akLeft, akTop, akRight, akBottom]
                ColCount = 2
                DefaultRowHeight = 18
                FixedCols = 0
                RowCount = 2
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
                TabOrder = 0
                OnClick = sgRelationsClick
                OnDrawCell = sgRelationsDrawCell
                OnSetEditText = sgRelationsSetEditText
                ColWidths = (
                  168
                  142)
              end
              object cmbRelation: TComboBox
                Left = 174
                Top = 23
                Width = 145
                Height = 21
                Style = csDropDownList
                ItemHeight = 13
                Sorted = True
                TabOrder = 1
                Visible = False
                OnChange = cmbRelationChange
                OnExit = ComboBoxExit
                Items.Strings = (
                  'Adjacent'
                  'Adjoining'
                  'Contiguous'
                  'Overlapping'
                  'Within')
              end
              object bbRelationAdd: TImageListButton
                Left = 319
                Top = 4
                Width = 24
                Height = 23
                Hint = 'Add new relation'
                Anchors = [akTop, akRight]
                TabOrder = 2
                OnClick = bbRelationAddClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 2
              end
              object bbRelationDel: TImageListButton
                Left = 319
                Top = 27
                Width = 24
                Height = 23
                Hint = 'Remove the selected relation'
                Anchors = [akTop, akRight]
                TabOrder = 3
                OnClick = bbRelationDelClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 4
              end
            end
            object tsUses: TTabSheet
              BorderWidth = 4
              Caption = 'Uses'
              ImageIndex = 2
              object splUses: TSplitter
                Left = 0
                Top = 89
                Width = 339
                Height = 3
                Cursor = crVSplit
                Align = alBottom
                AutoSnap = False
                Beveled = True
                MinSize = 73
              end
              object pnlUsesTop: TPanel
                Left = 0
                Top = 0
                Width = 339
                Height = 89
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                DesignSize = (
                  339
                  89)
                object sgUses: TStringGrid
                  Left = 0
                  Top = 0
                  Width = 315
                  Height = 85
                  Anchors = [akLeft, akTop, akRight, akBottom]
                  ColCount = 3
                  DefaultRowHeight = 18
                  FixedCols = 0
                  RowCount = 2
                  Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
                  TabOrder = 0
                  OnClick = sgUsesClick
                  OnDrawCell = DrawCellChoppedText
                  ColWidths = (
                    177
                    58
                    59)
                end
                object bbUseAdd: TImageListButton
                  Left = 315
                  Top = 0
                  Width = 24
                  Height = 23
                  Hint = 'Add new land use'
                  Anchors = [akTop, akRight]
                  TabOrder = 1
                  OnClick = bbUseAddClick
                  ImageList = dmFormActions.ilButtons
                  ImageIndex = 2
                end
                object bbUseEdit: TImageListButton
                  Left = 315
                  Top = 23
                  Width = 24
                  Height = 23
                  Hint = 'Edit the selected land use'
                  Anchors = [akTop, akRight]
                  TabOrder = 2
                  OnClick = bbUseEditClick
                  ImageList = dmFormActions.ilButtons
                  ImageIndex = 3
                end
                object bbUseDel: TImageListButton
                  Left = 315
                  Top = 46
                  Width = 24
                  Height = 23
                  Hint = 'Delete the selected land use'
                  Anchors = [akTop, akRight]
                  TabOrder = 3
                  OnClick = bbUseDelClick
                  ImageList = dmFormActions.ilButtons
                  ImageIndex = 4
                end
              end
              object pnlUsesBottom: TPanel
                Left = 0
                Top = 92
                Width = 339
                Height = 137
                Align = alBottom
                BevelOuter = bvNone
                Constraints.MinHeight = 117
                TabOrder = 1
                OnResize = pnlUsesBottomResize
                object gbUseDetails: TGroupBox
                  Left = 0
                  Top = 0
                  Width = 339
                  Height = 137
                  Align = alClient
                  Caption = 'Details'
                  Constraints.MinHeight = 116
                  TabOrder = 0
                  DesignSize = (
                    339
                    137)
                  object Label45: TLabel
                    Left = 12
                    Top = 15
                    Width = 22
                    Height = 13
                    Caption = 'Use:'
                  end
                  object Label46: TLabel
                    Left = 12
                    Top = 65
                    Width = 44
                    Height = 13
                    Caption = 'Potential:'
                  end
                  object Label47: TLabel
                    Left = 196
                    Top = 40
                    Width = 16
                    Height = 13
                    Caption = 'To:'
                  end
                  object Label48: TLabel
                    Left = 12
                    Top = 40
                    Width = 26
                    Height = 13
                    Caption = 'From:'
                  end
                  object lblUsesComments: TLabel
                    Left = 12
                    Top = 102
                    Width = 52
                    Height = 13
                    Caption = 'Comments:'
                  end
                  object eUse: TEdit
                    Left = 64
                    Top = 12
                    Width = 265
                    Height = 21
                    Anchors = [akLeft, akTop, akRight]
                    Enabled = False
                    TabOrder = 0
                  end
                  object eUseFrom: TVagueDateEdit
                    Left = 64
                    Top = 37
                    Width = 109
                    Height = 21
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clGreen
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = []
                    ParentFont = False
                    TabOrder = 1
                    OnExit = eUseFromExit
                  end
                  object eUseTo: TVagueDateEdit
                    Left = 216
                    Top = 37
                    Width = 113
                    Height = 21
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clGreen
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = []
                    ParentFont = False
                    TabOrder = 2
                    OnExit = eUseToExit
                  end
                  object reUsePotential: TRichEdit
                    Left = 64
                    Top = 62
                    Width = 265
                    Height = 33
                    Anchors = [akLeft, akTop, akRight]
                    PopupMenu = dmFormActions.pmRTF
                    ScrollBars = ssVertical
                    TabOrder = 3
                    OnEnter = EnterRTF
                    OnExit = ExitRTF
                  end
                  object reUseComments: TRichEdit
                    Left = 64
                    Top = 99
                    Width = 215
                    Height = 32
                    Anchors = [akLeft, akTop, akRight]
                    PopupMenu = dmFormActions.pmRTF
                    ScrollBars = ssVertical
                    TabOrder = 4
                    OnEnter = EnterRTF
                    OnExit = ExitRTF
                  end
                  object bbUseAccept: TImageListButton
                    Left = 283
                    Top = 108
                    Width = 24
                    Height = 23
                    Hint = 'Accept changes'
                    Anchors = [akRight, akBottom]
                    Enabled = False
                    TabOrder = 5
                    OnClick = bbUseAcceptClick
                    ImageList = dmFormActions.ilButtons
                    ImageIndex = 6
                  end
                  object bbUseDiscard: TImageListButton
                    Left = 307
                    Top = 108
                    Width = 24
                    Height = 23
                    Hint = 'Discard changes'
                    Anchors = [akRight, akBottom]
                    Enabled = False
                    TabOrder = 6
                    OnClick = bbUseDiscardClick
                    ImageList = dmFormActions.ilButtons
                    ImageIndex = 7
                  end
                end
              end
            end
            object tsTenure: TTabSheet
              Caption = 'Tenure'
              ImageIndex = 3
              DesignSize = (
                347
                237)
              object sgTenure: TStringGrid
                Left = 4
                Top = 4
                Width = 315
                Height = 89
                Anchors = [akLeft, akTop, akRight, akBottom]
                ColCount = 4
                DefaultRowHeight = 18
                FixedCols = 0
                RowCount = 2
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
                TabOrder = 0
                OnClick = sgTenureClick
                OnDrawCell = DrawCellChoppedText
                ColWidths = (
                  100
                  81
                  58
                  54)
              end
              object gbTenureDetails: TGroupBox
                Left = 4
                Top = 96
                Width = 339
                Height = 137
                Anchors = [akLeft, akRight, akBottom]
                Caption = 'Details'
                TabOrder = 4
                DesignSize = (
                  339
                  137)
                object Label50: TLabel
                  Left = 12
                  Top = 52
                  Width = 27
                  Height = 13
                  Caption = 'Type:'
                end
                object Label15: TLabel
                  Left = 12
                  Top = 20
                  Width = 31
                  Height = 13
                  Caption = 'Name:'
                end
                object Label16: TLabel
                  Left = 12
                  Top = 79
                  Width = 26
                  Height = 13
                  Caption = 'From:'
                end
                object Label25: TLabel
                  Left = 184
                  Top = 79
                  Width = 16
                  Height = 13
                  Anchors = [akLeft, akBottom]
                  Caption = 'To:'
                end
                object eTenureFrom: TVagueDateEdit
                  Left = 52
                  Top = 76
                  Width = 121
                  Height = 21
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clGreen
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 2
                  OnExit = eTenureFromExit
                end
                object eTenureTo: TVagueDateEdit
                  Left = 208
                  Top = 76
                  Width = 121
                  Height = 21
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clGreen
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 3
                  OnExit = eTenureToExit
                end
                object cmbTenureType: TDBListCombo
                  Left = 52
                  Top = 48
                  Width = 277
                  Height = 21
                  Style = csDropDownList
                  Anchors = [akLeft, akTop, akRight]
                  ItemHeight = 0
                  Sorted = True
                  TabOrder = 1
                  ListField = 'SHORT_NAME'
                  KeyField = 'TENURE_TYPE_KEY'
                  Datasource = dmLocationDetails.dsTenureType
                  Active = False
                  EmptyItem = False
                  ReadOnly = False
                end
                object bbTenureAccept: TImageListButton
                  Left = 283
                  Top = 108
                  Width = 24
                  Height = 23
                  Hint = 'Accept changes'
                  Anchors = [akRight, akBottom]
                  Enabled = False
                  TabOrder = 4
                  OnClick = bbTenureAcceptClick
                  ImageList = dmFormActions.ilButtons
                  ImageIndex = 6
                end
                object bbTenureDiscard: TImageListButton
                  Left = 307
                  Top = 108
                  Width = 24
                  Height = 23
                  Hint = 'Discard changes'
                  Anchors = [akRight, akBottom]
                  Enabled = False
                  TabOrder = 5
                  OnClick = bbTenureDiscardClick
                  ImageList = dmFormActions.ilButtons
                  ImageIndex = 7
                end
                object eTenureName: TNameLinkedEdit
                  Tag = 1
                  Left = 51
                  Top = 16
                  Width = 277
                  Height = 23
                  TabOrder = 0
                  BorderStyle = bsSingle
                  ImageIndex = 5
                  ImageList = dmFormActions.ilButtons
                  OnFindData = eTenureNameFindData
                  OnGetData = eTenureNameGetData
                end
              end
              object bbTenureAdd: TImageListButton
                Left = 319
                Top = 4
                Width = 24
                Height = 23
                Hint = 'Add new tenure'
                Anchors = [akTop, akRight]
                TabOrder = 1
                OnClick = bbTenureAddClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 2
              end
              object bbTenureEdit: TImageListButton
                Left = 319
                Top = 27
                Width = 24
                Height = 23
                Hint = 'Edit the selected tenure'
                Anchors = [akTop, akRight]
                TabOrder = 2
                OnClick = bbTenureEditClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 3
              end
              object bbTenureDel: TImageListButton
                Left = 319
                Top = 50
                Width = 24
                Height = 23
                Hint = 'Delete the selected tenure'
                Anchors = [akTop, akRight]
                TabOrder = 3
                OnClick = bbTenureDelClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 4
              end
            end
            object tsApproach: TTabSheet
              Caption = 'Approach'
              ImageIndex = 3
              OnShow = tsApproachShow
              DesignSize = (
                347
                237)
              object lblAccessRestrict: TLabel
                Left = 4
                Top = 120
                Width = 91
                Height = 13
                Caption = 'Access restrictions:'
              end
              object lblHowToGet: TLabel
                Left = 4
                Top = 4
                Width = 82
                Height = 13
                Caption = 'How to get there:'
              end
              object dbreApproach: TDBRichEdit
                Left = 4
                Top = 20
                Width = 339
                Height = 96
                Anchors = [akLeft, akTop, akRight]
                DataField = 'APPROACH'
                DataSource = dmLocationDetails.dsLocation
                PopupMenu = dmFormActions.pmRTF
                ScrollBars = ssVertical
                TabOrder = 0
                OnEnter = EnterRTF
                OnExit = ExitRTF
              end
              object dbreRestrictions: TDBRichEdit
                Left = 4
                Top = 136
                Width = 339
                Height = 96
                Anchors = [akLeft, akTop, akRight]
                DataField = 'RESTRICTION'
                DataSource = dmLocationDetails.dsLocation
                PopupMenu = dmFormActions.pmRTF
                ScrollBars = ssVertical
                TabOrder = 1
                OnEnter = EnterRTF
                OnExit = ExitRTF
              end
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
  object dlgOpen: TOpenDialog
    Filter = 
      'All supported vector file formats [*.gsf, *.dxf, *.bna, *.vpf, *' +
      '.ntf, *.mif, *.shp, *.opt, *.std]|*.gsf;*.dxf;*.bna;*.vpf;*.ntf;' +
      '*.mif;*.shp;*.opt;*.std|MapServer Sheets (*.gsf)|*.gsf|AutoCAD (' +
      '*.dxf)|*.dxf|Atlas GIS (*.bna)|*.bna|Digital Chart of the World ' +
      '(*.vpf)|*.vpf|OS Version 2 Levels 1,2,3 (*.ntf)|*.ntf|Map Info f' +
      'ile (*.mif)|*.mif|ERSI Shape file (*.shp)|*.shp|Digital Line Gra' +
      'ph (*.opt, *.std)|*.opt;*.std'
    Title = 'Select map file'
    Left = 14
    Top = 322
  end
  object pmMapWindow: TPopupMenu
    Images = dmFormActions.ilMenuOn
    Left = 84
    Top = 324
  end
  object pmMapWindowForBoundary: TPopupMenu
    Images = dmFormActions.ilMenuOn
    Left = 124
    Top = 324
  end
end
