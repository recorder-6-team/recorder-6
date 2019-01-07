object dlgOptions: TdlgOptions
  Left = 439
  Top = 295
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 479
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object bbCancel: TImageListButton
    Left = 456
    Top = 442
    Width = 83
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = bbCancelClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbOK: TImageListButton
    Left = 364
    Top = 442
    Width = 83
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = bbOkClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object btnDefault: TButton
    Left = 4
    Top = 442
    Width = 117
    Height = 25
    Caption = 'Default Settings'
    TabOrder = 0
    OnClick = btnDefaultClick
  end
  object pcOptionsPages: TPageControl
    Left = 0
    Top = 0
    Width = 545
    Height = 433
    ActivePage = tsSettingTable
    Align = alTop
    TabOrder = 1
    OnChange = pcOptionsPagesChange
    object tsGeneral: TTabSheet
      Caption = 'General'
      object grbMenusWindows: TGroupBox
        Left = 4
        Top = 4
        Width = 257
        Height = 121
        Caption = ' Display '
        TabOrder = 0
        object cbShowLastSessionWindows: TCheckBox
          Left = 6
          Top = 22
          Width = 243
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Open with windows from last Session:'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object cbShowWelcomeAtStart: TCheckBox
          Left = 6
          Top = 44
          Width = 243
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Show Welcome Window at StartUp:'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object cbShowMenuIcons: TCheckBox
          Left = 6
          Top = 66
          Width = 243
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Show Menu icons:'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = cbShowMenuIconsClick
        end
        object cbGraduatedMenus: TCheckBox
          Left = 6
          Top = 88
          Width = 243
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Graduated Menus:'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
      end
      object grbConfidentialData: TGroupBox
        Left = 4
        Top = 164
        Width = 265
        Height = 77
        Caption = ' Confidential Data '
        TabOrder = 4
        DesignSize = (
          265
          77)
        object lblMinimumAccess: TLabel
          Left = 8
          Top = 50
          Width = 106
          Height = 13
          Caption = 'Minimum access level:'
        end
        object cbExportConfidentialOcc: TCheckBox
          Left = 6
          Top = 22
          Width = 247
          Height = 17
          Hint = 'Allow export of confidential occurrences'
          Alignment = taLeftJustify
          Caption = 'Enable export of confidential occurrences:'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object cmbConfAccessLevel: TComboBox
          Left = 128
          Top = 46
          Width = 127
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          Enabled = False
          ItemHeight = 13
          TabOrder = 1
          Items.Strings = (
            'Read Only'
            'Record Cards Only'
            'Add Only'
            'Full Edit (Own data only)'
            'Full Edit'
            'System Manager')
        end
      end
      object gbLocationNodes: TGroupBox
        Left = 4
        Top = 266
        Width = 525
        Height = 73
        Caption = 'Location Hierarchy Options'
        TabOrder = 3
        object lblExtraLocationSearchColumns: TLabel
          Left = 264
          Top = 12
          Width = 151
          Height = 13
          Caption = 'Extra Location Search Columns:'
        end
        object lblExtraLocationSearchColumnList: TLabel
          Left = 272
          Top = 31
          Width = 177
          Height = 28
          AutoSize = False
          Caption = 'Location Type, Spatial Reference, File Code'
          WordWrap = True
        end
        object chkIncludeLocationSpatialRef: TCheckBox
          Left = 6
          Top = 44
          Width = 243
          Height = 17
          Hint = 
            'In the Locations window, should the spatial reference be shown a' +
            's part of the location name in the hierarchy?'
          Alignment = taLeftJustify
          Caption = 'Include spatial reference in location hierarchy:'
          TabOrder = 1
        end
        object chkIncludeLocationFileCode: TCheckBox
          Left = 6
          Top = 20
          Width = 243
          Height = 17
          Hint = 
            'In the Locations window, should the file code be shown as part o' +
            'f the location name in the hierarchy?'
          Alignment = taLeftJustify
          Caption = 'Include file code in location hierarchy:'
          TabOrder = 0
        end
        object btnExtraLocationSearchColumns: TButton
          Left = 456
          Top = 34
          Width = 61
          Height = 25
          Caption = '&Choose...'
          TabOrder = 2
          OnClick = btnExtraLocationSearchColumnsClick
        end
      end
      object grbTaxonNameOptions: TGroupBox
        Left = 276
        Top = 12
        Width = 257
        Height = 49
        Caption = 'Taxon Name Display Options'
        TabOrder = 1
        object cbShowCommonNames: TCheckBox
          Left = 6
          Top = 22
          Width = 97
          Height = 17
          Hint = 'Use common names on screen for taxa where available'
          Alignment = taLeftJustify
          Caption = 'Common names:'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object cbShowAuthors: TCheckBox
          Left = 106
          Top = 22
          Width = 53
          Height = 17
          Alignment = taLeftJustify
          Caption = 'authors:'
          TabOrder = 1
        end
        object cbShowNamesAsEntered: TCheckBox
          Left = 160
          Top = 22
          Width = 89
          Height = 17
          Alignment = taLeftJustify
          Caption = 'names as input:'
          TabOrder = 2
        end
      end
      object grbTaxonOptions: TGroupBox
        Left = 276
        Top = 84
        Width = 257
        Height = 149
        Caption = 'Other Taxon Options '
        TabOrder = 2
        object lblTaxonSearches: TLabel
          Left = 8
          Top = 42
          Width = 132
          Height = 13
          Caption = 'Restrict Taxon Searches to:'
        end
        object cmbTaxonRestriction: TComboBox
          Left = 8
          Top = 58
          Width = 241
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          OnChange = cmbTaxonRestrictionChange
          Items.Strings = (
            '')
        end
        object cbFullTranslation: TCheckBox
          Left = 6
          Top = 20
          Width = 243
          Height = 17
          Hint = 'Use Full Translation to Currently Preferred Term'
          Alignment = taLeftJustify
          Caption = 'Use Full Translation to Preferred Term:'
          TabOrder = 0
        end
        object chkPartialTaxonSearch: TCheckBox
          Left = 6
          Top = 82
          Width = 243
          Height = 17
          Hint = 'Enable partial name search for taxa'
          Alignment = taLeftJustify
          Caption = 'Enable partial name search for taxa:'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object chkAutoCompleteSearch: TCheckBox
          Left = 6
          Top = 104
          Width = 243
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Auto-complete search in Import Wizard:'
          TabOrder = 3
        end
        object chkUsePreferredTaxa: TCheckBox
          Left = 6
          Top = 128
          Width = 243
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Use preferred taxa for Import Wizard'
          TabOrder = 4
        end
      end
    end
    object tsMainToolbar: TTabSheet
      Caption = 'Toolbars'
      ImageIndex = 1
      object bvlToolbars: TBevel
        Left = 4
        Top = 4
        Width = 529
        Height = 341
        Shape = bsFrame
      end
      object lblToolbarButtons: TLabel
        Left = 288
        Top = 16
        Width = 78
        Height = 13
        Caption = '&Toolbar Buttons:'
        FocusControl = lbOnToolbar
      end
      object lblAvailableActions: TLabel
        Left = 12
        Top = 16
        Width = 84
        Height = 13
        Caption = '&Available Actions:'
        FocusControl = lbAvailable
      end
      object bbMoveDown: TImageListButton
        Left = 257
        Top = 292
        Width = 23
        Height = 22
        Hint = 'Move button in selection down'
        TabOrder = 6
        OnClick = bbMoveDownClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 13
      end
      object bbMoveUp: TImageListButton
        Left = 257
        Top = 268
        Width = 23
        Height = 22
        Hint = 'Move button in selection up'
        TabOrder = 5
        OnClick = bbMoveUpClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 12
      end
      object bbClearAll: TImageListButton
        Left = 257
        Top = 92
        Width = 23
        Height = 22
        Hint = 'Clear selection'
        TabOrder = 4
        OnClick = bbClearAllClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 8
      end
      object bbRemove: TImageListButton
        Left = 257
        Top = 56
        Width = 23
        Height = 22
        Hint = 'Remove button from selection'
        TabOrder = 3
        OnClick = bbRemoveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 10
      end
      object bbAdd: TImageListButton
        Left = 257
        Top = 32
        Width = 23
        Height = 22
        Hint = 'Add button to selection'
        TabOrder = 2
        OnClick = bbAddClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 11
      end
      object lbOnToolbar: TListBox
        Tag = -1
        Left = 288
        Top = 32
        Width = 237
        Height = 281
        Style = lbOwnerDrawFixed
        DragMode = dmAutomatic
        ItemHeight = 20
        TabOrder = 7
        OnClick = lbOnToolbarClick
        OnDblClick = bbRemoveClick
        OnDragOver = lbOnToolbarDragOver
        OnDrawItem = DrawListItem
      end
      object lbAvailable: TListBox
        Tag = -1
        Left = 12
        Top = 32
        Width = 237
        Height = 281
        Style = lbOwnerDrawFixed
        ItemHeight = 20
        TabOrder = 0
        OnDblClick = bbAddClick
        OnDrawItem = DrawListItem
      end
      object cbShowMainToolbar: TCheckBox
        Left = 12
        Top = 319
        Width = 125
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Show Main Toolbar:'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object cbShowActiveWindowToolbar: TCheckBox
        Left = 288
        Top = 319
        Width = 173
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Show Active Window Toolbar:'
        Checked = True
        State = cbChecked
        TabOrder = 8
        OnClick = cbShowActiveWindowToolbarClick
      end
    end
    object tsAppearance: TTabSheet
      Caption = 'Appearance'
      ImageIndex = 3
      object bvlAppearance: TBevel
        Left = 4
        Top = 4
        Width = 529
        Height = 341
        Shape = bsFrame
      end
      object lblInformationSource: TLabel
        Left = 12
        Top = 52
        Width = 92
        Height = 13
        Caption = 'Information Source:'
      end
      object lblInformationDest: TLabel
        Left = 12
        Top = 84
        Width = 111
        Height = 13
        Caption = 'Information Destination:'
      end
      object lblMandatoryFields: TLabel
        Left = 12
        Top = 20
        Width = 83
        Height = 13
        Caption = 'Mandatory Fields:'
      end
      object bvlPreview: TBevel
        Left = 288
        Top = 16
        Width = 233
        Height = 285
      end
      object ShapeSource: TShape
        Left = 295
        Top = 53
        Width = 219
        Height = 23
        Pen.Color = clBlue
      end
      object ShapeDest: TShape
        Left = 295
        Top = 83
        Width = 219
        Height = 23
        Pen.Color = clRed
      end
      object ShapeSourceDest: TShape
        Left = 295
        Top = 113
        Width = 219
        Height = 23
        Brush.Color = clBlue
        Pen.Color = clRed
        Pen.Style = psDash
      end
      object lblBackgroundPreview: TLabel
        Left = 296
        Top = 152
        Width = 102
        Height = 13
        Caption = 'Background Preview:'
      end
      object colMandatoryField: TColorButton
        Left = 132
        Top = 16
        Width = 41
        Height = 20
        ActiveColor = clYellow
        TabOrder = 0
        TabStop = True
        OnChange = colMandatoryFieldChange
      end
      object colDragSource: TColorButton
        Left = 132
        Top = 48
        Width = 41
        Height = 20
        ActiveColor = clBlue
        TabOrder = 1
        TabStop = True
        OnChange = colDragSourceChange
      end
      object colDragDest: TColorButton
        Left = 132
        Top = 80
        Width = 41
        Height = 20
        ActiveColor = clRed
        TabOrder = 2
        TabStop = True
        OnChange = colDragDestChange
      end
      object btnUndoAppearance: TButton
        Left = 448
        Top = 310
        Width = 75
        Height = 25
        Caption = '&Undo'
        TabOrder = 6
        OnClick = btnUndoAppearanceClick
      end
      object gbBackground: TGroupBox
        Left = 12
        Top = 116
        Width = 269
        Height = 93
        Caption = ' Application Background '
        TabOrder = 3
        object rbBackPlain: TRadioButton
          Left = 16
          Top = 20
          Width = 221
          Height = 17
          Caption = 'Plain'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = rbBackPlainClick
        end
        object rbBackBitmap: TRadioButton
          Left = 16
          Top = 40
          Width = 125
          Height = 17
          Caption = 'Bitmap'
          TabOrder = 1
          OnClick = rbBackBitmapClick
        end
        object eBackBitmap: TEdit
          Left = 32
          Top = 60
          Width = 209
          Height = 21
          Enabled = False
          TabOrder = 2
          Text = '<None>'
          OnExit = eBackBitmapExit
          OnKeyPress = eBackBitmapKeyPress
        end
        object btnBitmapBrowse: TButton
          Left = 240
          Top = 60
          Width = 21
          Height = 21
          Caption = '...'
          Enabled = False
          TabOrder = 3
          OnClick = btnBitmapBrowseClick
        end
      end
      object cbNoDragDropFrame: TCheckBox
        Left = 12
        Top = 224
        Width = 249
        Height = 17
        Caption = 'Disable Information Transfer Colours'
        TabOrder = 4
      end
      object cbShowToolTips: TCheckBox
        Left = 12
        Top = 248
        Width = 249
        Height = 17
        Caption = 'Show Tool Tips'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
      object pnlBackBitmap: TPanel
        Left = 296
        Top = 172
        Width = 217
        Height = 119
        BevelOuter = bvLowered
        Enabled = False
        TabOrder = 7
        object imgBackBitmap: TImage
          Left = 1
          Top = 1
          Width = 215
          Height = 117
          Center = True
          Stretch = True
        end
      end
      object eMandatory: TEdit
        Left = 296
        Top = 24
        Width = 217
        Height = 21
        TabStop = False
        Color = clYellow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 8
        Text = 'Mandatory field'
      end
      object eDragSource: TEdit
        Left = 296
        Top = 54
        Width = 217
        Height = 21
        TabStop = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 9
        Text = 'Field you can transfer information from... '
      end
      object eDragDest: TEdit
        Left = 296
        Top = 84
        Width = 217
        Height = 21
        TabStop = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 10
        Text = 'Field you can transfer information to...'
      end
      object eDragSourceDest: TEdit
        Left = 296
        Top = 114
        Width = 217
        Height = 21
        TabStop = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 11
        Text = 'Field you can transfer info from and to...'
      end
      object cbUseOriginalicons: TCheckBox
        Left = 12
        Top = 272
        Width = 249
        Height = 17
        Caption = 'Use Original Icons'
        TabOrder = 12
      end
    end
    object tsSpatialRef: TTabSheet
      Caption = 'Spatial References'
      ImageIndex = 5
      object bvlSpatialRefs: TBevel
        Left = 4
        Top = 4
        Width = 529
        Height = 341
        Shape = bsFrame
      end
      object lblSystemSelect: TLabel
        Left = 16
        Top = 20
        Width = 190
        Height = 26
        Caption = '&Select your preferred system for viewing spatial references:'
        FocusControl = cblbSpatialRefs
        WordWrap = True
      end
      object cblbSpatialRefs: TCheckListBox
        Left = 224
        Top = 20
        Width = 297
        Height = 285
        OnClickCheck = cblbSpatialRefsClickCheck
        ItemHeight = 13
        TabOrder = 0
        OnClick = cblbSpatialRefsClick
        OnMouseDown = cblbSpatialRefsMouseDown
      end
      object cbGridRefsAsSquares: TCheckBox
        Left = 14
        Top = 314
        Width = 223
        Height = 17
        Hint = 'Display grid references as squares on map'
        Alignment = taLeftJustify
        Caption = '&Display grid references as squares on map:'
        TabOrder = 1
      end
    end
    object tsFileLocations: TTabSheet
      Caption = 'File Locations'
      ImageIndex = 5
      object bvlFiles: TBevel
        Left = 4
        Top = 4
        Width = 529
        Height = 341
        Shape = bsFrame
      end
      object lblRucksacks: TLabel
        Left = 16
        Top = 21
        Width = 76
        Height = 13
        Caption = 'Rucksack Files:'
      end
      object lblTemplates: TLabel
        Left = 16
        Top = 53
        Width = 87
        Height = 13
        Caption = 'Report Templates:'
      end
      object lblQueries: TLabel
        Left = 16
        Top = 150
        Width = 39
        Height = 13
        Caption = 'Queries:'
      end
      object lblCards: TLabel
        Left = 16
        Top = 182
        Width = 82
        Height = 13
        Caption = 'Recording Cards:'
      end
      object lblImages: TLabel
        Left = 16
        Top = 214
        Width = 56
        Height = 13
        Caption = 'Image Files:'
      end
      object lblFilters: TLabel
        Left = 16
        Top = 247
        Width = 71
        Height = 13
        Caption = 'Polygon Filters:'
      end
      object lblSnapshotFiles: TLabel
        Left = 16
        Top = 117
        Width = 72
        Height = 13
        Caption = 'Snapshot Files:'
      end
      object lblBatches: TLabel
        Left = 16
        Top = 280
        Width = 74
        Height = 13
        Caption = 'Batch Updates:'
      end
      object lblExternalFilePath: TLabel
        Left = 16
        Top = 313
        Width = 82
        Height = 13
        Caption = 'External File Path'
      end
      object lblExportTemplates: TLabel
        Left = 16
        Top = 85
        Width = 85
        Height = 13
        Caption = 'Export Templates:'
      end
      object eRucksackFiles: TEdit
        Left = 116
        Top = 18
        Width = 381
        Height = 21
        TabOrder = 0
      end
      object eTemplates: TEdit
        Left = 116
        Top = 50
        Width = 381
        Height = 21
        TabOrder = 2
      end
      object btnRucksackBrowse: TButton
        Left = 496
        Top = 18
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 1
        OnClick = btnRucksackBrowseClick
      end
      object btnTemplateBrowse: TButton
        Left = 496
        Top = 50
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 3
        OnClick = btnTemplateBrowseClick
      end
      object eReports: TEdit
        Left = 116
        Top = 147
        Width = 381
        Height = 21
        TabOrder = 8
      end
      object btnReportBrowse: TButton
        Left = 496
        Top = 147
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 9
        OnClick = btnReportBrowseClick
      end
      object btnRecordingCardBrowse: TButton
        Left = 496
        Top = 179
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 11
        OnClick = btnRecordingCardBrowseClick
      end
      object eRecordingCards: TEdit
        Left = 116
        Top = 179
        Width = 381
        Height = 21
        TabOrder = 10
      end
      object eImageFiles: TEdit
        Left = 116
        Top = 211
        Width = 381
        Height = 21
        TabOrder = 12
      end
      object btnImageFilesBrowse: TButton
        Left = 496
        Top = 211
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 13
        OnClick = btnImageFilesBrowseClick
      end
      object ePolygonFilters: TEdit
        Left = 116
        Top = 244
        Width = 381
        Height = 21
        TabOrder = 14
      end
      object btnPolygonFiltersBrowse: TButton
        Left = 496
        Top = 244
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 15
        OnClick = btnPolygonFiltersBrowseClick
      end
      object eSnapshots: TEdit
        Left = 116
        Top = 114
        Width = 381
        Height = 21
        TabOrder = 6
      end
      object btnSnapshotBrowse: TButton
        Left = 496
        Top = 114
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 7
        OnClick = btnSnapshotBrowseClick
      end
      object eBatchUpdates: TEdit
        Left = 116
        Top = 277
        Width = 381
        Height = 21
        TabOrder = 16
      end
      object btnBatchUpdatesBrowse: TButton
        Left = 496
        Top = 277
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 17
        OnClick = btnBatchUpdatesBrowseClick
      end
      object eExternalFiles: TEdit
        Left = 116
        Top = 310
        Width = 381
        Height = 21
        TabOrder = 18
      end
      object btnExternalFilePath: TButton
        Left = 496
        Top = 310
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 19
        OnClick = btnExternalFilePathClick
      end
      object eExportTemplates: TEdit
        Left = 116
        Top = 82
        Width = 381
        Height = 21
        TabOrder = 4
      end
      object btnExportTemplateBrowse: TButton
        Left = 496
        Top = 82
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 5
        OnClick = btnExportTemplateBrowseClick
      end
    end
    object tsSettingTable: TTabSheet
      Caption = 'Setting Table'
      ImageIndex = 5
      object lblSetting1: TLabel
        Left = 8
        Top = 40
        Width = 150
        Height = 13
        Caption = 'Master Workstation for mapping'
      end
      object lblMapMaster: TLabel
        Left = 424
        Top = 0
        Width = 99
        Height = 33
        Caption = 'Makes this workstation the master for managing mapping.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Small Fonts'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblThisWorkStation: TLabel
        Left = 168
        Top = 8
        Width = 153
        Height = 17
        AutoSize = False
        Caption = 'Current Workstation'
        Color = clWindow
        ParentColor = False
      end
      object lblSetting2: TLabel
        Left = 8
        Top = 72
        Width = 145
        Height = 13
        Caption = 'Competency Level for Review '
      end
      object lblSetting3: TLabel
        Left = 8
        Top = 296
        Width = 56
        Height = 13
        Caption = 'Gateway url'
      end
      object lblSetting4: TLabel
        Left = 8
        Top = 328
        Width = 85
        Height = 13
        Caption = 'R6 on line help url'
      end
      object lblSetting5: TLabel
        Left = 8
        Top = 104
        Width = 98
        Height = 13
        Caption = 'Preferred Locations  '
      end
      object lblSetting6: TLabel
        Left = 8
        Top = 136
        Width = 79
        Height = 13
        Caption = 'Preferred Names'
      end
      object lblSetting7: TLabel
        Left = 8
        Top = 168
        Width = 58
        Height = 13
        Caption = 'Sort Method'
      end
      object lblSetting8: TLabel
        Left = 8
        Top = 200
        Width = 127
        Height = 13
        Caption = 'Taxon Designmation Lists  '
      end
      object lblSetting10: TLabel
        Left = 8
        Top = 232
        Width = 121
        Height = 13
        Caption = 'Licence Key - Temp Data'
      end
      object lblSetting11: TLabel
        Left = 8
        Top = 262
        Width = 85
        Height = 13
        Caption = 'Temp Name Key  '
      end
      object lblWorksattion: TLabel
        Left = 8
        Top = 8
        Width = 94
        Height = 13
        Caption = 'Current Workstation'
      end
      object lblSettingWarning: TLabel
        Left = 8
        Top = 384
        Width = 53
        Height = 13
        Caption = 'WARNING'
        Color = clBtnHighlight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object lblFixOS: TLabel
        Left = 424
        Top = 40
        Width = 76
        Height = 44
        Caption = 
          'Brings the object sheet path for this workstation in line with M' +
          'aster '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Small Fonts'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblSetting12: TLabel
        Left = 8
        Top = 360
        Width = 138
        Height = 13
        Caption = 'Dictionary Update Block Size'
      end
      object edMaster: TEdit
        Left = 168
        Top = 38
        Width = 153
        Height = 21
        ReadOnly = True
        TabOrder = 0
      end
      object btnMaster: TButton
        Left = 328
        Top = 6
        Width = 89
        Height = 25
        Caption = 'Make Master'
        TabOrder = 1
        OnClick = btnMasterClick
      end
      object edCompetency: TEdit
        Left = 168
        Top = 70
        Width = 41
        Height = 21
        MaxLength = 1
        TabOrder = 2
        OnKeyPress = edCompetencyKeyPress
      end
      object edPreLocs: TEdit
        Left = 168
        Top = 102
        Width = 353
        Height = 21
        MaxLength = 250
        TabOrder = 3
      end
      object edPrefnames: TEdit
        Left = 168
        Top = 134
        Width = 353
        Height = 21
        Color = clCaptionText
        MaxLength = 250
        TabOrder = 4
      end
      object edSortMethod: TEdit
        Left = 168
        Top = 166
        Width = 89
        Height = 21
        MaxLength = 250
        TabOrder = 5
      end
      object edTaxDesList: TEdit
        Left = 168
        Top = 198
        Width = 353
        Height = 21
        MaxLength = 250
        TabOrder = 6
      end
      object edTempLicence: TEdit
        Left = 168
        Top = 230
        Width = 121
        Height = 21
        MaxLength = 250
        TabOrder = 7
      end
      object edTempNames: TEdit
        Left = 168
        Top = 262
        Width = 353
        Height = 21
        MaxLength = 250
        TabOrder = 8
      end
      object edGatewayURL: TEdit
        Left = 168
        Top = 294
        Width = 353
        Height = 21
        MaxLength = 250
        TabOrder = 9
      end
      object edHelpUrl: TEdit
        Left = 168
        Top = 326
        Width = 353
        Height = 21
        MaxLength = 250
        TabOrder = 10
      end
      object btnFixObjectSheet: TButton
        Left = 328
        Top = 40
        Width = 89
        Height = 25
        Caption = 'Fix Object Sheet'
        TabOrder = 11
      end
      object edBlockSize: TEdit
        Left = 168
        Top = 358
        Width = 41
        Height = 21
        MaxLength = 2
        TabOrder = 12
        Text = '50'
        OnKeyPress = edBlockSizeKeyPress
      end
    end
    object tsSundry: TTabSheet
      Caption = 'Other'
      ImageIndex = 6
      object grbOther: TGroupBox
        Left = 140
        Top = 30
        Width = 265
        Height = 275
        Caption = ' Other '
        TabOrder = 0
        object lblCenturyCutOff: TLabel
          Left = 8
          Top = 109
          Width = 127
          Height = 13
          Hint = 'Century cut-off Year (0-99)'
          Caption = 'Century cut-off Year (0-99):'
        end
        object lblCenturyCutOffInfo: TLabel
          Left = 8
          Top = 136
          Width = 248
          Height = 33
          Caption = 
            'This value will be used to automatically work out the century fo' +
            'r a date where only the last two digits of the year have been sp' +
            'ecified.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Small Fonts'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object lblDelimiter: TLabel
          Left = 8
          Top = 177
          Width = 101
          Height = 13
          Hint = 
            'This must not be alphanumeric, or one of the characters <, >, ~,' +
            ' -, / or a space as these clash with other data entry.'
          Caption = 'Rapid Entry Delimiter:'
        end
        object cbAutoEmail: TCheckBox
          Left = 6
          Top = 18
          Width = 247
          Height = 17
          Hint = 
            'Automatically create emails when contributing to Recording Schem' +
            'es'
          Alignment = taLeftJustify
          Caption = 'Auto-create emails for Recording Schemes:'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object eDateCutYear: TEdit
          Left = 144
          Top = 106
          Width = 33
          Height = 21
          Hint = 'Century cut-off Year (0-99)'
          MaxLength = 2
          TabOrder = 4
          OnExit = eDateCutYearExit
          OnKeyPress = eDateCutYearKeyPress
        end
        object chkRememberFilters: TCheckBox
          Left = 6
          Top = 40
          Width = 247
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Remember applied filters:'
          TabOrder = 1
        end
        object eRapidEntryDelimiter: TEdit
          Left = 120
          Top = 174
          Width = 33
          Height = 21
          Hint = 
            'This must not be alphanumeric, or one of the characters <, >, ~,' +
            ' -, / or a space as these clash with other data entry.'
          MaxLength = 1
          TabOrder = 5
          OnKeyPress = eRapidEntryDelimiterKeyPress
        end
        object chkOrganiseSurveysByTag: TCheckBox
          Left = 6
          Top = 62
          Width = 247
          Height = 17
          Hint = 'Organise Surveys by Tag'
          Alignment = taLeftJustify
          Caption = 'Organise Surveys by Tag:'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object chkUseOldImportWizard: TCheckBox
          Left = 8
          Top = 252
          Width = 245
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Use the v6.13 Import Wizard:'
          TabOrder = 3
          Visible = False
        end
        object chkIgnoreRememberedMatches: TCheckBox
          Left = 8
          Top = 84
          Width = 245
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Ignore Previous Import Wizard Matches:'
          TabOrder = 6
        end
        object cbPlaceCardDocs: TCheckBox
          Left = 6
          Top = 214
          Width = 243
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Documents - add to Taxon Occurrence'
          TabOrder = 7
        end
      end
    end
  end
  object dlgOpen: TOpenPictureDialog
    Left = 144
    Top = 438
  end
  object dlgFolder: TFolderBrowser
    BrowseFlags = [bfDirsOnly, bfStatusText]
    NewDialogStyle = False
    Title = 'Select folder'
    Left = 176
    Top = 438
  end
end
