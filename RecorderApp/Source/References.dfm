inherited frmReferences: TfrmReferences
  Left = 595
  Top = 467
  Width = 626
  Height = 457
  Caption = 'Documents'
  Constraints.MinHeight = 446
  Constraints.MinWidth = 446
  KeyPreview = False
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object ReferenceSplitter: TSplitter [0]
    Left = 247
    Top = 0
    Width = 10
    Height = 365
    Align = alRight
    MinSize = 100
    OnCanResize = ReferenceSplitterCanResize
    OnMoved = FormResize
    OnPaint = ReferenceSplitterPaint
  end
  object pnlButtons: TPanel [1]
    Left = 0
    Top = 365
    Width = 610
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlButtons2: TPanel
      Left = 427
      Top = 0
      Width = 183
      Height = 34
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object bbShowAll: TButton
        Left = 4
        Top = 6
        Width = 75
        Height = 25
        Hint = 'Remove Filter on Hierarchy'
        Caption = '&Show All'
        TabOrder = 0
        Visible = False
        OnClick = bbShowAllClick
      end
      object bbRelatedData: TBitBtn
        Left = 84
        Top = 6
        Width = 93
        Height = 25
        Hint = 'Access data related to the selected reference'
        Caption = 'Re&lated Data'
        TabOrder = 1
        OnClick = bbRelatedDataClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          0400000000000001000000000000000000001000000010000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7777777777777777777777777777777777777777777777777777777777777777
          7777777777777777777777777777777777777777777777777777777777777777
          7777777777FF77777777777777700777777777777877F7777777777777000077
          7777777787777F77777777777000000777777778777777F77777777700000000
          777777877777777F77777770000000000777787777777777F777777777777777
          7777778888888888F77777777777777777777777777777777777777777777777
          7777777777777777777777777777777777777777777777777777777777777777
          7777777777777777777777777777777777777777777777777777}
        Layout = blGlyphRight
        NumGlyphs = 2
        Spacing = -1
      end
    end
    object bbAdd: TImageListButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Add new document'
      Caption = '&Add'
      TabOrder = 1
      OnClick = bbAddClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 2
    end
    object bbEdit: TImageListButton
      Left = 88
      Top = 5
      Width = 75
      Height = 26
      Hint = 'Edit selected document'
      Caption = 'Ed&it'
      TabOrder = 2
      OnClick = bbEditClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 3
    end
    object bbDelete: TImageListButton
      Left = 168
      Top = 5
      Width = 75
      Height = 26
      Hint = 'Delete selected document'
      Caption = 'Delete'
      TabOrder = 3
      OnClick = bbDeleteClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 4
    end
  end
  object pnlReferences: TPanel [2]
    Left = 0
    Top = 0
    Width = 247
    Height = 365
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlList: TPanel
      Tag = 1
      Left = 0
      Top = 17
      Width = 247
      Height = 348
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 1
      Color = clLime
      TabOrder = 0
      object dbgReferences: TDBJNCCGrid
        Left = 1
        Top = 1
        Width = 245
        Height = 346
        Align = alClient
        DataSource = dsReferences
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
        PopupMenu = pmGrid
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        OnKeyDown = dbgReferencesKeyDown
        OnKeyPress = dbgReferencesKeyPress
      end
    end
    object pnlLabel: TPanel
      Left = 0
      Top = 0
      Width = 247
      Height = 17
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object lblDocuments: TLabel
        Left = 8
        Top = 2
        Width = 57
        Height = 13
        Caption = 'Documents:'
      end
    end
  end
  object pnlReferenceDetails: TPanel [3]
    Left = 257
    Top = 0
    Width = 353
    Height = 365
    Align = alRight
    BevelOuter = bvLowered
    Constraints.MinWidth = 353
    TabOrder = 2
    OnResize = pnlReferenceDetailsResize
    object pnlInner: TPanel
      Left = 0
      Top = 0
      Width = 353
      Height = 369
      BevelOuter = bvLowered
      TabOrder = 0
      DesignSize = (
        353
        369)
      object lblReference: TLabel
        Left = 68
        Top = 4
        Width = 3
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblReferencePrompt: TLabel
        Left = 8
        Top = 4
        Width = 52
        Height = 13
        Caption = 'Document:'
      end
      object pcReferenceDetails: TPageControl
        Left = 8
        Top = 24
        Width = 333
        Height = 305
        ActivePage = tsKeywords
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        OnChange = pcReferenceDetailsChange
        OnChanging = pcReferenceDetailsChanging
        object tsGeneral: TTabSheet
          Caption = 'General'
          DesignSize = (
            325
            277)
          object bvlGeneralFrame: TBevel
            Left = 4
            Top = 4
            Width = 317
            Height = 269
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object lblAuthor: TLabel
            Left = 16
            Top = 16
            Width = 45
            Height = 13
            Caption = 'Author(s):'
          end
          object lblYear: TLabel
            Left = 16
            Top = 107
            Width = 25
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Year:'
          end
          object lblFullReference: TLabel
            Left = 16
            Top = 128
            Width = 72
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Full Reference:'
          end
          object lblRefType: TLabel
            Left = 171
            Top = 108
            Width = 27
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Type:'
          end
          object Label1: TLabel
            Left = 16
            Top = 241
            Width = 44
            Height = 26
            Anchors = [akLeft, akBottom]
            Caption = 'Storage Location:'
            WordWrap = True
          end
          object lbAuthors: TListBox
            Left = 16
            Top = 32
            Width = 273
            Height = 69
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 13
            TabOrder = 0
            OnClick = lbAuthorsClick
          end
          object dbreReference: TDBRichEdit
            Left = 16
            Top = 144
            Width = 297
            Height = 81
            TabStop = False
            Anchors = [akLeft, akRight, akBottom]
            Color = clBtnFace
            DataField = 'FULL_REFERENCE'
            DataSource = dmReferences.dsReference
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 6
          end
          object bbAuthorAdd: TImageListButton
            Left = 288
            Top = 32
            Width = 24
            Height = 23
            Hint = 'Add author'
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnClick = bbAuthorAddClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 2
          end
          object bbAuthorEdit: TImageListButton
            Left = 288
            Top = 54
            Width = 24
            Height = 23
            Hint = 'Edit selected author'
            Anchors = [akTop, akRight]
            TabOrder = 2
            OnClick = bbAuthorEditClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 3
          end
          object bbAuthorDel: TImageListButton
            Left = 288
            Top = 76
            Width = 24
            Height = 23
            Hint = 'Delete selected author'
            Anchors = [akTop, akRight]
            TabOrder = 3
            OnClick = bbAuthorDelClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 4
          end
          object cmbReferenceType: TComboBox
            Left = 200
            Top = 104
            Width = 113
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akBottom]
            ItemHeight = 0
            Sorted = True
            TabOrder = 5
            OnChange = cmbReferenceTypeChange
          end
          object eStorageLocation: TDBEdit
            Left = 64
            Top = 244
            Width = 249
            Height = 21
            Anchors = [akLeft, akRight, akBottom]
            DataField = 'PHYSICAL_LOCATION'
            DataSource = dmReferences.dsReference
            TabOrder = 7
          end
          object eYear: TVagueDateEdit
            Left = 48
            Top = 104
            Width = 105
            Height = 21
            Hint = '"" is not a valid month or season name.'
            Anchors = [akLeft, akBottom]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 4
          end
        end
        object tsDetails: TTabSheet
          Caption = 'Details'
          ImageIndex = 1
          DesignSize = (
            325
            277)
          object bvlDetailsFrame: TBevel
            Left = 4
            Top = 4
            Width = 317
            Height = 269
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object lblJournalDesc: TLabel
            Left = 12
            Top = 64
            Width = 37
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Journal:'
          end
          object lblVolume: TLabel
            Left = 12
            Top = 87
            Width = 38
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Volume:'
          end
          object lblPart: TLabel
            Left = 131
            Top = 87
            Width = 22
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Part:'
          end
          object lblNumber: TLabel
            Left = 223
            Top = 87
            Width = 40
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Number:'
          end
          object lblEdition: TLabel
            Left = 228
            Top = 111
            Width = 35
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Edition:'
          end
          object lblSupplement: TLabel
            Left = 120
            Top = 111
            Width = 33
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Suppl.:'
          end
          object lblPages: TLabel
            Left = 12
            Top = 111
            Width = 33
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Pages:'
          end
          object lblSymposium: TLabel
            Left = 12
            Top = 135
            Width = 75
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Symposium title:'
          end
          object lblEditor: TLabel
            Left = 12
            Top = 158
            Width = 41
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Editor(s):'
          end
          object lblPublisher: TLabel
            Left = 12
            Top = 227
            Width = 46
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Publisher:'
            WordWrap = True
          end
          object lblPlace: TLabel
            Left = 12
            Top = 251
            Width = 96
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Place of publication:'
          end
          object lblTitle: TLabel
            Left = 12
            Top = 15
            Width = 23
            Height = 13
            Caption = 'Title:'
          end
          object lbEditors: TListBox
            Left = 60
            Top = 156
            Width = 173
            Height = 65
            Anchors = [akLeft, akRight, akBottom]
            ItemHeight = 13
            TabOrder = 9
            OnClick = lbEditorsClick
          end
          object dbreTitle: TDBRichEdit
            Left = 60
            Top = 12
            Width = 253
            Height = 45
            Anchors = [akLeft, akTop, akRight, akBottom]
            DataField = 'TITLE'
            DataSource = dmReferences.dsReference
            PopupMenu = dmFormActions.pmRTF
            ScrollBars = ssVertical
            TabOrder = 0
            OnEnter = reEnter
            OnExit = reExit
          end
          object dblcJournal: TDBLookupComboBox
            Left = 60
            Top = 60
            Width = 253
            Height = 21
            Anchors = [akLeft, akRight, akBottom]
            DataField = 'JOURNAL_KEY'
            DataSource = dmReferences.dsReference
            KeyField = 'JOURNAL_KEY'
            ListField = 'SHORT_NAME'
            ListSource = dmReferences.dsJournal
            TabOrder = 1
            OnClick = dbComboClick
            OnKeyDown = dbComboKeyDown
            OnKeyUp = dbComboKeyUp
          end
          object dbeVolume: TDBEdit
            Left = 60
            Top = 84
            Width = 57
            Height = 21
            Anchors = [akLeft, akBottom]
            DataField = 'VOLUME'
            DataSource = dmReferences.dsReference
            TabOrder = 2
            OnChange = dbeVolumePartNumberChange
          end
          object dbePart: TDBEdit
            Left = 154
            Top = 84
            Width = 65
            Height = 21
            Anchors = [akLeft, akBottom]
            DataField = 'PART'
            DataSource = dmReferences.dsReference
            TabOrder = 3
            OnChange = dbeVolumePartNumberChange
          end
          object dbeNumber: TDBEdit
            Left = 264
            Top = 84
            Width = 49
            Height = 21
            Anchors = [akLeft, akBottom]
            DataField = 'NUMBER'
            DataSource = dmReferences.dsReference
            TabOrder = 4
            OnChange = dbeVolumePartNumberChange
          end
          object dbeSupplement: TDBEdit
            Left = 154
            Top = 108
            Width = 65
            Height = 21
            Anchors = [akLeft, akBottom]
            DataField = 'SUPPLEMENT'
            DataSource = dmReferences.dsReference
            TabOrder = 6
          end
          object dbeEdition: TDBEdit
            Left = 264
            Top = 108
            Width = 49
            Height = 21
            Anchors = [akLeft, akBottom]
            DataField = 'EDITION'
            DataSource = dmReferences.dsReference
            TabOrder = 7
          end
          object dbeSymposium: TDBEdit
            Left = 96
            Top = 132
            Width = 217
            Height = 21
            Anchors = [akLeft, akRight, akBottom]
            DataField = 'SYMPOSIUM_TITLE'
            DataSource = dmReferences.dsReference
            TabOrder = 8
          end
          object dbePublisher: TDBEdit
            Left = 60
            Top = 224
            Width = 253
            Height = 21
            Anchors = [akLeft, akRight, akBottom]
            DataField = 'PUBLISHER'
            DataSource = dmReferences.dsReference
            TabOrder = 13
          end
          object dbePublicationPlace: TDBEdit
            Left = 112
            Top = 248
            Width = 201
            Height = 21
            Anchors = [akLeft, akRight, akBottom]
            DataField = 'PLACE_OF_PUBLICATION'
            DataSource = dmReferences.dsReference
            TabOrder = 14
          end
          object dbePages: TDBEdit
            Left = 60
            Top = 108
            Width = 57
            Height = 21
            Anchors = [akLeft, akBottom]
            DataField = 'PAGES'
            DataSource = dmReferences.dsReference
            TabOrder = 5
          end
          object bbEditorAdd: TImageListButton
            Left = 232
            Top = 156
            Width = 23
            Height = 22
            Hint = 'Add editor'
            Anchors = [akRight, akBottom]
            TabOrder = 10
            OnClick = bbEditorAddClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 2
          end
          object bbEditorEdit: TImageListButton
            Left = 232
            Top = 178
            Width = 23
            Height = 22
            Hint = 'Edit selected editor'
            Anchors = [akRight, akBottom]
            TabOrder = 11
            OnClick = bbEditorEditClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 3
          end
          object bbEditorDel: TImageListButton
            Left = 232
            Top = 200
            Width = 23
            Height = 22
            Hint = 'Delete selected editor'
            Anchors = [akRight, akBottom]
            TabOrder = 12
            OnClick = bbEditorDelClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 4
          end
        end
        object tsKeywords: TTabSheet
          Caption = 'Keywords'
          ImageIndex = 3
          OnShow = tsKeywordsShow
          DesignSize = (
            325
            277)
          object btnAddKeyword: TImageListButton
            Left = 296
            Top = 4
            Width = 24
            Height = 23
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnClick = btnAddKeywordClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 2
          end
          object btnRemoveKeyword: TImageListButton
            Left = 296
            Top = 26
            Width = 24
            Height = 23
            Anchors = [akTop, akRight]
            TabOrder = 2
            OnClick = btnRemoveKeywordClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 4
          end
          object sgKeywords: TControlStringGrid
            Left = 4
            Top = 4
            Width = 293
            Height = 269
            Anchors = [akLeft, akTop, akRight, akBottom]
            ColCount = 1
            DefaultColWidth = 284
            DefaultRowHeight = 19
            FixedCols = 0
            RowCount = 2
            ScrollBars = ssVertical
            TabOrder = 0
            OnClick = sgKeywordsClick
            ColWidths = (
              282)
          end
        end
        object tsNumbers: TTabSheet
          Caption = 'Other'
          ImageIndex = 2
          DesignSize = (
            325
            277)
          object bvlNumbersFrame: TBevel
            Left = 4
            Top = 4
            Width = 317
            Height = 269
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object lblNumbers: TLabel
            Left = 16
            Top = 12
            Width = 45
            Height = 13
            Caption = 'Numbers:'
          end
          object lblOriginalFile: TLabel
            Left = 16
            Top = 208
            Width = 57
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Original File:'
          end
          object sgNumbers: TStringGrid
            Left = 16
            Top = 28
            Width = 275
            Height = 177
            Anchors = [akLeft, akTop, akRight, akBottom]
            ColCount = 2
            DefaultRowHeight = 18
            FixedCols = 0
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
            TabOrder = 0
            OnClick = sgNumbersClick
            OnDrawCell = sgNumbersDrawCell
            ColWidths = (
              142
              126)
          end
          object cmbNumberType: TComboBox
            Left = 160
            Top = 48
            Width = 129
            Height = 21
            ItemHeight = 13
            TabOrder = 3
            Visible = False
            OnExit = cmbNumberTypeExit
            Items.Strings = (
              'ISBN'
              'ISSN')
          end
          object eOriginalFile: TEdit
            Left = 16
            Top = 222
            Width = 293
            Height = 21
            Anchors = [akLeft, akRight, akBottom]
            ReadOnly = True
            TabOrder = 4
            OnChange = eOriginalFileChange
          end
          object bbNumberAdd: TImageListButton
            Left = 290
            Top = 28
            Width = 24
            Height = 23
            Hint = 'Add new number'
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnClick = bbNumberAddClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 2
          end
          object bbNumberDel: TImageListButton
            Left = 290
            Top = 50
            Width = 24
            Height = 23
            Hint = 'Delete selected number'
            Anchors = [akTop, akRight]
            TabOrder = 2
            OnClick = bbNumberDelClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 4
          end
          object bbOriginalFileView: TImageListButton
            Left = 262
            Top = 244
            Width = 24
            Height = 23
            Hint = 'View file content'
            Anchors = [akRight, akBottom]
            Caption = '...'
            Enabled = False
            TabOrder = 5
            OnClick = bbOriginalFileViewClick
          end
          object bbOriginalFileAdd: TImageListButton
            Left = 285
            Top = 244
            Width = 24
            Height = 23
            Hint = 'Get file name'
            Anchors = [akRight, akBottom]
            TabOrder = 6
            OnClick = bbOriginalFileAddClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 2
          end
        end
      end
      object bbSave: TImageListButton
        Left = 182
        Top = 334
        Width = 75
        Height = 26
        Hint = 'Save document details'
        Anchors = [akRight, akBottom]
        Caption = '&Save'
        Enabled = False
        TabOrder = 1
        OnClick = bbSaveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
      end
      object bbCancel: TImageListButton
        Left = 266
        Top = 334
        Width = 75
        Height = 26
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
  inherited mnuChildMerge: TMainMenu
    Left = 44
    Top = 68
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 2
      object mnuEditAdd: TMenuItem
        Caption = '&Add'
        Hint = 'Add new document'
        OnClick = bbAddClick
      end
      object mnuEditEdit: TMenuItem
        Caption = '&Edit'
        Hint = 'Edit selected document'
        OnClick = bbEditClick
      end
      object mnuEditDelete: TMenuItem
        Caption = '&Delete'
        Hint = 'Delete selected document'
        OnClick = bbDeleteClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuEditCut: TMenuItem
        Action = dmFormActions.actCut
      end
      object mnuEditCopy: TMenuItem
        Action = dmFormActions.actCopy
      end
      object mnuEditPaste: TMenuItem
        Action = dmFormActions.actPaste
      end
      object mnuEditTransferData: TMenuItem
        Action = dmFormActions.actTransferData
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuEditFind: TMenuItem
        Action = actFind
      end
      object mnuEditSimpleFilter: TMenuItem
        Action = actFilter
      end
      object actEditShowMetadata: TMenuItem
        Action = actShowMetadata
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
  object pmRelatedData: TPopupMenu
    Left = 44
    Top = 120
    object mnuRelSurveys: TMenuItem
      Caption = '&Surveys...'
      Hint = 'Show related Surveys for the selected Reference'
      ImageIndex = 14
      OnClick = mnuRelSurveysClick
    end
    object mnuRelEvents: TMenuItem
      Caption = '&Events...'
      Hint = 'Show related Survey Events for the selected Reference'
      OnClick = mnuRelEventsClick
    end
    object mnuRelSamples: TMenuItem
      Caption = 'S&amples...'
      Hint = 'Show related Samples for the selected Reference'
      OnClick = mnuRelSamplesClick
    end
    object mnuRelOccur: TMenuItem
      Caption = 'O&ccurrences...'
      Hint = 'Show related Occurrences for the selected Reference'
      OnClick = mnuRelOccurClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object mnuRelLocations: TMenuItem
      Caption = '&Locations...'
      Hint = 'Show related Locations for the selected Reference'
      ImageIndex = 13
      OnClick = mnuRelLocationsClick
    end
    object mnuRelIndivOrg: TMenuItem
      Caption = '&Names and Addresses...'
      Hint = 'Show related Individuals/Organisations for the selected Referene'
      ImageIndex = 15
      OnClick = mnuRelIndivOrgClick
    end
  end
  object pmGrid: TPopupMenu
    Left = 44
    Top = 173
    object pmAdd: TMenuItem
      Caption = '&Add'
      Hint = 'Add new document'
      OnClick = bbAddClick
    end
    object pmHValidateItem: TMenuItem
      Action = dmFormActions.actDatabaseValidateSelected
    end
  end
  object dlgOpen: TOpenDialog
    Left = 44
    Top = 229
  end
  object alDocuments: TActionList
    Images = dmFormActions.ilMenuOn
    Left = 136
    Top = 69
    object actFind: TAction
      Caption = '&Find...'
      Hint = 'Find a document'
      ImageIndex = 5
      ShortCut = 16454
      OnExecute = actFindExecute
    end
    object actFilter: TAction
      Caption = '&Simple Filter...'
      Hint = 'Apply a filter on the documents'
      ImageIndex = 6
      OnExecute = actFilterExecute
    end
    object actShowMetadata: TAction
      Caption = 'Show &Metadata'
      Hint = 'Show Metadata'
      ImageIndex = 52
      OnExecute = actShowMetadataExecute
    end
  end
  object dsReferences: TDataSource
    DataSet = dmReferences.qryPopulate
    Left = 136
    Top = 121
  end
end
