object frmMain: TfrmMain
  Left = 446
  Top = 359
  Width = 655
  Height = 501
  Anchors = []
  Caption = 'Recorder'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIForm
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  WindowMenu = mnuWindow
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  OnShortCut = FormShortCut
  PixelsPerInch = 96
  TextHeight = 13
  object Status: TStatusBar
    Left = 0
    Top = 453
    Width = 647
    Height = 21
    AutoHint = True
    Panels = <
      item
        Width = 500
      end
      item
        Width = 100
      end
      item
        Style = psOwnerDraw
        Width = 50
      end>
    OnResize = StatusResize
  end
  object CoolBarMain: TCoolBar
    Left = 0
    Top = 0
    Width = 647
    Height = 72
    AutoSize = True
    BandMaximize = bmDblClick
    Bands = <
      item
        Break = False
        Control = pnlMenuContainer
        ImageIndex = -1
        MinHeight = 22
        Width = 647
      end
      item
        Control = tbMainToolbar
        ImageIndex = -1
        MinHeight = 22
        Width = 647
      end
      item
        Control = tbContext
        ImageIndex = -1
        MinHeight = 22
        Visible = False
        Width = 647
      end>
    EdgeBorders = [ebBottom]
    PopupMenu = pmMainToolbar
    object tbMainToolbar: TXPToolbar
      Left = 9
      Top = 24
      Width = 634
      Height = 22
      Align = alLeft
      AutoSize = True
      Caption = 'Main Toolbar'
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmFormActions.ilMenuOn
      PopupMenu = pmMainToolbar
      TabOrder = 0
      Wrapable = False
      OnResize = ToolbarResize
    end
    object tbContext: TXPToolbar
      Left = 9
      Top = 48
      Width = 634
      Height = 22
      Align = alLeft
      AutoSize = True
      Caption = 'Context sensitive'
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmFormActions.ilMenuOn
      PopupMenu = pmMainToolbar
      TabOrder = 1
      Transparent = True
      Visible = False
      Wrapable = False
      OnResize = ToolbarResize
    end
    object pnlMenuContainer: TPanel
      Left = 9
      Top = 0
      Width = 634
      Height = 22
      BevelOuter = bvNone
      PopupMenu = pmMainToolbar
      TabOrder = 2
      object tbMenu: TXPToolbar
        Left = 0
        Top = 0
        Width = 680
        Height = 22
        Align = alLeft
        AutoSize = True
        ButtonHeight = 21
        ButtonWidth = 68
        Caption = 'tbMenu'
        EdgeBorders = []
        Flat = True
        ShowCaptions = True
        TabOrder = 0
        Wrapable = False
        object tbtnMnuFile: TXPToolbutton
          Left = 0
          Top = 0
          AutoSize = True
          Caption = '&File'
          Grouped = True
          MenuItem = mnuFile
        end
        object tbtnMnuMerge1: TXPToolbutton
          Left = 27
          Top = 0
          AutoSize = True
          Grouped = True
          Visible = False
        end
        object tbtnMnuDataEntry: TXPToolbutton
          Tag = 1
          Left = 41
          Top = 0
          AutoSize = True
          Caption = '&Data Entry'
          Grouped = True
          MenuItem = mnuData
        end
        object tbtnMnuMerge2: TXPToolbutton
          Left = 102
          Top = 0
          AutoSize = True
          Grouped = True
          Visible = False
        end
        object tbtnMnuDictionaries: TXPToolbutton
          Tag = 2
          Left = 116
          Top = 0
          AutoSize = True
          Caption = 'D&ictionaries'
          Grouped = True
          MenuItem = mnuDictionary
        end
        object tbtnMnuMap: TXPToolbutton
          Tag = 3
          Left = 182
          Top = 0
          AutoSize = True
          Caption = '&Map'
          Grouped = True
          MenuItem = mnuMainMap
        end
        object tbtnMnuReports: TXPToolbutton
          Tag = 4
          Left = 214
          Top = 0
          AutoSize = True
          Caption = '&Reports'
          Grouped = True
          MenuItem = mnuReports
        end
        object tbtnMnuTools: TXPToolbutton
          Tag = 5
          Left = 262
          Top = 0
          AutoSize = True
          Caption = '&Tools'
          Grouped = True
          MenuItem = mnuTools
        end
        object tbtnMnuWindow: TXPToolbutton
          Tag = 6
          Left = 299
          Top = 0
          AutoSize = True
          Caption = '&Window'
          Grouped = True
          MenuItem = mnuWindow
        end
        object tbtnMnuHelp: TXPToolbutton
          Tag = 7
          Left = 349
          Top = 0
          AutoSize = True
          Caption = '&Help'
          Grouped = True
          MenuItem = mnuHelp
        end
      end
      object tbMDIButtons: TXPToolbar
        Left = 565
        Top = 0
        Width = 69
        Height = 22
        Align = alRight
        AutoSize = True
        Caption = 'tbMDIButtons'
        EdgeBorders = []
        Flat = True
        Images = ilMDI
        TabOrder = 1
        Visible = False
        object tbtnMDIMinimise: TXPToolbutton
          Left = 0
          Top = 0
          Caption = 'tbtnMDIMinimise'
          ImageIndex = 0
          OnClick = tbtnMDIMinimiseClick
          Images = ilMDI
        end
        object tbtnMDINormalise: TXPToolbutton
          Left = 23
          Top = 0
          Caption = 'tbtnMDINormalise'
          ImageIndex = 1
          OnClick = tbtnMDINormaliseClick
          Images = ilMDI
        end
        object tbtnMDIClose: TXPToolbutton
          Left = 46
          Top = 0
          Caption = 'tbtnMDIClose'
          ImageIndex = 2
          OnClick = tbtnMDICloseClick
          Images = ilMDI
        end
      end
    end
  end
  object mnuMain: TMainMenu
    BiDiMode = bdLeftToRight
    Images = dmFormActions.ilMenuOn
    ParentBiDiMode = False
    Left = 36
    Top = 92
    object mnuFile: TMenuItem
      Caption = '&File'
      GroupIndex = 1
      object mnuFileOpen: TMenuItem
        Caption = '&Open'
        ImageIndex = 59
        Visible = False
        OnClick = mnuFileOpenClick
      end
      object mnuFileSave: TMenuItem
        Caption = '&Save'
        Enabled = False
        OnClick = mnuFileSaveClick
      end
      object mnuFileSaveAs: TMenuItem
        Caption = 'Save &As...'
        Enabled = False
        ImageIndex = 0
        OnClick = mnuFileSaveAsClick
      end
      object mnuFileClose: TMenuItem
        Caption = '&Close'
        Enabled = False
        OnClick = mnuFileCloseClick
      end
      object mnuFileCloseAll: TMenuItem
        Caption = 'C&lose All'
        Enabled = False
        OnClick = mnuFileCloseAllClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object mnuPrintPreview: TMenuItem
        Action = dmFormActions.actPreview
      end
      object mnuFilePrint: TMenuItem
        Action = dmFormActions.actPrint
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuFileExit: TMenuItem
        Caption = 'E&xit'
        ShortCut = 32883
        OnClick = mnuFileExitClick
      end
    end
    object mnuData: TMenuItem
      Caption = '&Data Entry'
      GroupIndex = 3
      object mnuDataEntrySpecies: TMenuItem
        Caption = 'Enter &Species for a place'
        Hint = 'Enter a list of species recorded at a given place'
        ImageIndex = 12
        object mnuDataEntryCardSplitter: TMenuItem
          Caption = '-'
        end
        object mnuDataEntryNewCard: TMenuItem
          Caption = 'Add or Edit Recording &Card'
          Hint = 'Design a new  recording card or edit an existing card. '
          ImageIndex = 12
          OnClick = mnuDataEntryNewCardClick
        end
      end
      object mnuDataEntrySurvey: TMenuItem
        Action = dmFormActions.actObservations
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuDataEntryLocations: TMenuItem
        Action = dmFormActions.actLocations
      end
      object mnuDataEntryIndOrg: TMenuItem
        Action = dmFormActions.actNames
      end
      object mnuDataEntryReferences: TMenuItem
        Action = dmFormActions.actDocuments
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object mnuDataEntryPlaces: TMenuItem
        Action = dmFormActions.actSpeciesRecord
      end
      object mnuDataEntryCustomSpeciesCards: TMenuItem
        Caption = 'Custom species cards'
        Visible = False
      end
    end
    object mnuDictionary: TMenuItem
      Caption = 'D&ictionaries'
      GroupIndex = 5
      object mnuDictionaryTaxon: TMenuItem
        Action = dmFormActions.actTaxonDiction
      end
      object mnuDictionaryBiotope: TMenuItem
        Action = dmFormActions.actBiotopeDiction
      end
      object mnuDictionaryAdminArea: TMenuItem
        Action = dmFormActions.actAdminAreaDiction
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object mnuDictionariesNewTaxa: TMenuItem
        Action = dmFormActions.actNewTaxa
      end
      object mnuDictionariesNewBiotopes: TMenuItem
        Action = dmFormActions.actNewBiotopes
      end
    end
    object mnuMainMap: TMenuItem
      Caption = '&Map'
      GroupIndex = 6
      object mnuMapWindow: TMenuItem
        Caption = 'View &Map'
        Hint = 'Display Map window'
        ImageIndex = 21
      end
      object mnuMapOptions: TMenuItem
        Action = dmFormActions.actMapOptions
      end
      object mnuMapBrowser: TMenuItem
        Action = dmFormActions.actMapBrowser
      end
    end
    object mnuReports: TMenuItem
      Caption = '&Reports'
      GroupIndex = 7
      object mnuReportsOpen: TMenuItem
        Action = dmFormActions.actOpenReport
      end
      object mnuReportsWizard: TMenuItem
        Action = dmFormActions.actReportWizard
      end
      object mnuReportsQuickReport: TMenuItem
        Caption = '&Quick Report'
        OnClick = mnuReportsQuickReportClick
        object mnuReportsLocationReport: TMenuItem
          Action = dmFormActions.actOccurrencesForPlacesReport
        end
        object mnuReportsTaxonOccurrences: TMenuItem
          Action = dmFormActions.actPlacesForOccurrencesReport
        end
      end
    end
    object mnuTools: TMenuItem
      Caption = '&Tools'
      GroupIndex = 8
      object mnuToolsRucksack: TMenuItem
        Action = dmFormActions.actRuckSack
        GroupIndex = 1
      end
      object mnuToolsTermLists: TMenuItem
        Action = dmFormActions.actTermsLists
        GroupIndex = 2
      end
      object mnuEnhancedTermlists: TMenuItem
        Action = dmFormActions.actEnhancedTermLists
        GroupIndex = 2
      end
      object mnuToolsLoadExternalFilter: TMenuItem
        Action = dmFormActions.actLoadExternalFilter
        GroupIndex = 2
      end
      object mnuToolsRunBatchUpdates: TMenuItem
        Action = dmFormActions.actRunBatchUpdates
        GroupIndex = 2
      end
      object mnuToolsBatchUpdates: TMenuItem
        Caption = '&Batch Updates'
        GroupIndex = 2
        OnClick = mnuToolsBatchUpdatesClick
        object mnuEmpty: TMenuItem
          Caption = '<No updates available>'
          Enabled = False
          Visible = False
        end
      end
      object mnuToolsCommitUpdates: TMenuItem
        Action = dmFormActions.actCommitUpdates
        GroupIndex = 2
      end
      object mnuToolsCancelUpdates: TMenuItem
        Action = dmFormActions.actCancelUpdates
        GroupIndex = 2
      end
      object mnuToolsSep1: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object mnuToolsUserConfig: TMenuItem
        Action = dmFormActions.actUserConfig
        GroupIndex = 3
      end
      object mnuToolsPassword: TMenuItem
        Caption = 'Change &Password...'
        GroupIndex = 4
        OnClick = mnuToolsPasswordClick
      end
      object mnuToolsSep2: TMenuItem
        Caption = '-'
        GroupIndex = 8
      end
      object mnuToolsInstall: TMenuItem
        Caption = '&Install'
        GroupIndex = 8
        object mnuToolsInstallModule: TMenuItem
          Action = dmFormActions.actConfigAddIn
        end
      end
      object mnuToolsSep3: TMenuItem
        Caption = '-'
        GroupIndex = 8
      end
      object mnuToolsExportManagement: TMenuItem
        Caption = 'E&xport Management'
        GroupIndex = 8
        object mnuToolsContribute: TMenuItem
          Caption = '&Contribute to Scheme'
        end
        object mnuToolsExportUsingFilter: TMenuItem
          Caption = '&Export Using Filter'
        end
        object mnuToolsManageRecordingSchemes: TMenuItem
          Action = dmFormActions.actManageSchemes
        end
        object mnuToolsManageExportFilters: TMenuItem
          Action = dmFormActions.actManageExportFilters
          GroupIndex = 8
        end
        object N8: TMenuItem
          Caption = '-'
          GroupIndex = 8
        end
        object mnuToolsEditMetadata: TMenuItem
          Action = dmFormActions.actEditMetadata
          GroupIndex = 9
        end
      end
      object mnuToolsImport: TMenuItem
        Action = dmFormActions.actImport
        GroupIndex = 8
      end
      object mnuToolsDatabaseExport: TMenuItem
        Action = dmFormActions.actExport
        GroupIndex = 8
      end
      object mnuToolsDatabase: TMenuItem
        Caption = 'D&atabase Tools'
        GroupIndex = 9
        object mnuToolsMergeDataItems: TMenuItem
          Action = dmFormActions.actMergeData
        end
        object mnuNewCustodian: TMenuItem
          Action = dmFormActions.actNewCustodian
        end
        object mnuToolsRevalidateDatabase: TMenuItem
          Action = dmFormActions.actDatabaseValidateAll
        end
        object mnuToolsRevalidateSelectedRecords: TMenuItem
          Action = dmFormActions.actDatabaseValidateSelected
        end
        object N4: TMenuItem
          Caption = '-'
        end
        object mnuToolsDatabaseBackup: TMenuItem
          Action = dmFormActions.actBackup
        end
        object mnuToolsDatabaseRestore: TMenuItem
          Action = dmFormActions.actRestore
        end
        object mnuToolsChangeBackupLocation: TMenuItem
          Caption = 'View or Change Backup Location...'
          OnClick = mnuToolsChangeBackupLocationClick
        end
        object N7: TMenuItem
          Caption = '-'
        end
        object MnuToolsUpdateDictionary1: TMenuItem
          Caption = 'Update Dictionary'
          OnClick = MnuToolsUpdateDictionary1Click
        end
        object mnuToolsDatabaseRebuildNameIndex: TMenuItem
          Caption = 'Rebuild Taxon &Name Index'
          OnClick = mnuToolsDatabaseRebuildNameIndexClick
        end
        object mnuToolsDatabaseRebuildGroupIndex: TMenuItem
          Caption = 'Rebuild &Taxon Group Index'
          OnClick = mnuToolsDatabaseRebuildGroupIndexClick
        end
        object mnuToolsDatabaseRebuildSynonymIndex: TMenuItem
          Caption = 'Rebuild Taxon &Synonym Index'
          OnClick = mnuToolsDatabaseRebuildSynonymIndexClick
        end
        object mnuToolsDatabaseRebuildDesignationIndex: TMenuItem
          Caption = 'Rebuild Taxon &Designation Index'
          OnClick = mnuToolsDatabaseRebuildDesignationIndexClick
        end
      end
      object mnuToolsSep4: TMenuItem
        Caption = '-'
        GroupIndex = 9
      end
      object mnuToolsOptions: TMenuItem
        Action = dmFormActions.actOptions
        GroupIndex = 9
      end
    end
    object mnuWindow: TMenuItem
      Caption = '&Window'
      GroupIndex = 9
      OnClick = mnuWindowClick
      object mnuWindowsCascade: TMenuItem
        Caption = '&Cascade'
        Enabled = False
        OnClick = mnuWindowsCascadeClick
      end
      object mnuWindowSeparator: TMenuItem
        Caption = '-'
        Visible = False
      end
    end
    object mnuHelp: TMenuItem
      Caption = '&Help'
      GroupIndex = 10
      object mnuHelpContents: TMenuItem
        Action = dmFormActions.actHelp
      end
      object mnuHelpContext: TMenuItem
        Caption = '&Context Sensitive'
        ShortCut = 112
        OnClick = mnuHelpContextClick
      end
      object mnuOnLineHelp: TMenuItem
        Caption = '&On Line Help'
        OnClick = mnuOnLineHelpClick
      end
      object mnuHelpSummary: TMenuItem
        Caption = '&Summary Info...'
        OnClick = mnuHelpSummaryClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuHelpAbout: TMenuItem
        Caption = '&About Recorder 6...'
        Hint = 'About Recorder 2002...'
        ImageIndex = 60
        OnClick = mnuHelpAboutClick
      end
    end
  end
  object pmMainToolbar: TPopupMenu
    Left = 36
    Top = 152
    object pmMainToolbarConfigure: TMenuItem
      Caption = '&Configure...'
      OnClick = pmMainToolbarConfigureClick
    end
  end
  object pmRecordCards: TPopupMenu
    Images = dmFormActions.ilMenuOn
    Left = 36
    Top = 208
    object pmRecordCardsSplitter: TMenuItem
      Caption = '-'
    end
    object pmRecordCardsNew: TMenuItem
      Caption = 'New Recording &Card'
      Default = True
      ImageIndex = 12
      OnClick = mnuDataEntryNewCardClick
    end
  end
  object ilMDI: TImageList
    Left = 472
    Top = 148
    Bitmap = {
      494C010103000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFFFFFFFFF0000
      FFFFFFFFFFFF0000FFFFFFFFFFFF0000F03FF03FFFFF0000F03FF7BFF3CF0000
      FFFFF7BFF99F0000FFFFF78FFC3F0000FFFFF02FFE7F0000FFFFF02FFC3F0000
      FFFFFDEFF99F0000FFFFFC0FF3CF0000FFFFFC0FFFFF0000FFFFFFFFFFFF0000
      FFFFFFFFFFFF0000FFFFFFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
  object pmMapWindow: TPopupMenu
    Images = dmFormActions.ilMenuOn
    Left = 36
    Top = 264
  end
end
