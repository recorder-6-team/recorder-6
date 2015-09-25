inherited frmFilterResult: TfrmFilterResult
  Left = 182
  Top = 153
  Width = 640
  Height = 397
  Caption = 'Filter Result'
  Font.Name = 'Arial'
  Scaled = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 14
  object dbgResults: TDBFilterGrid [0]
    Left = 0
    Top = 21
    Width = 632
    Height = 296
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Arial'
    TitleFont.Style = [fsBold]
    OnCellClick = dbgResultsCellClick
    OnColumnMoved = dbgResultsColumnMoved
    OnDrawColumnCell = dbgResultsDrawColumnCell
    OnMouseDown = dbgResultsMouseDown
    OnMouseMove = dbgResultsMouseMove
    FilterOperators.Strings = (
      '~:LIKE'
      '=:='
      '<:<'
      '>:>'
      '<=:<='
      '>=:>='
      #216':IS NULL')
    UnaryOperators.Strings = (
      'IS NULL')
    OnFilterChanged = dbgResultsFilterChanged
    OnFilterOperator = dbgResultsFilterOperator
  end
  object pnlPreparingReport: TPanel [1]
    Left = 0
    Top = 21
    Width = 632
    Height = 296
    Align = alClient
    Caption = 'pnlPreparingReport'
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    Visible = False
  end
  object pnlButtons: TPanel [2]
    Left = 0
    Top = 317
    Width = 632
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnOutput: TImageListButton
      Left = 8
      Top = 6
      Width = 113
      Height = 25
      Hint = 'Print the grid or a report'
      Caption = 'Re&port Output'
      TabOrder = 0
      OnClick = btnOutputClick
      ImageList = dmFormActions.ilMenuOn
      ImageIndex = 43
      Spacing = -1
    end
    object pnlButtons2: TPanel
      Left = 467
      Top = 0
      Width = 165
      Height = 34
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object bbFinish: TImageListButton
        Left = 84
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Fi&nish'
        TabOrder = 1
        OnClick = bbFinishClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
        Spacing = -1
      end
      object bbBack: TImageListButton
        Left = 4
        Top = 6
        Width = 75
        Height = 25
        Hint = 'Go back to the wizard'
        Caption = '&Back'
        TabOrder = 0
        OnClick = bbBackClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 10
        Spacing = -1
      end
    end
  end
  object pnlTitle: TPanel [3]
    Left = 0
    Top = 0
    Width = 632
    Height = 21
    Align = alTop
    TabOrder = 1
    DesignSize = (
      632
      21)
    object lblOccCount: TLabel
      Left = 535
      Top = 3
      Width = 89
      Height = 13
      Alignment = taRightJustify
      Anchors = [akRight]
      Caption = 'occurrences found'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object lblClickInstruction: TLabel
      Left = 4
      Top = 4
      Width = 177
      Height = 13
      Caption = 'Click on an item to view or edit details'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
  inherited mnuChildMerge: TMainMenu
    Left = 156
    Top = 52
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 2
      object mnuEditCopy: TMenuItem
        Action = actResultCopy
      end
      object mnuEditCopyAll: TMenuItem
        Action = actResultCopyAll
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuEditView: TMenuItem
        Action = actResultView
      end
    end
    object Reports1: TMenuItem
      Caption = '&Reports'
      GroupIndex = 7
      object mnuReportOpen: TMenuItem
        Action = dmFormActions.actOpenReport
      end
      object mnuReportWizard: TMenuItem
        Action = dmFormActions.actReportWizard
      end
      object OccurrencesforPlacesReport1: TMenuItem
        Action = dmFormActions.actOccurrencesForPlacesReport
      end
      object PlacesforOccurrencesReport1: TMenuItem
        Action = dmFormActions.actPlacesForOccurrencesReport
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuReportMap: TMenuItem
        Caption = 'Send To &Map'
        Hint = 'Plot the results as distribution points on the map'
        ImageIndex = 21
      end
    end
  end
  object pmOutput: TPopupMenu
    Images = dmFormActions.ilMenuOn
    Left = 156
    Top = 156
    object pmPrintGrid: TMenuItem
      Action = actPrintGrid
    end
    object pmOutputCopy: TMenuItem
      Action = actResultCopy
      Hint = 'Copy the content of the currently selected cell to the clipboard'
    end
    object pmOutputResultCopyAll: TMenuItem
      Action = actResultCopyAll
    end
    object pmOutputMap: TMenuItem
      Caption = 'Send To &Map'
      Hint = 'Plot the results as distribution points on the map'
      ImageIndex = 21
    end
    object pmOutputRevalidateItems: TMenuItem
      Caption = 'Revalidate Items'
      OnClick = pmOutputRevalidateItemsClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object pmOutputExportData: TMenuItem
      Action = dmFormActions.actExport
    end
    object mnuOutputExportExcel: TMenuItem
      Caption = 'Export to Excel...'
      ImageIndex = 71
      OnClick = mnuOutputExportExcelClick
    end
    object pmOutputExportGoogleEarth: TMenuItem
      Caption = 'Export to Google Earth...'
      OnClick = pmOutputExportGoogleEarthClick
    end
    object mnuOutputExportSHP: TMenuItem
      Caption = 'Export to ESRI Shape File...'
      OnClick = mnuOutputExportSHPClick
    end
    object mnuOutputExportOther: TMenuItem
      Caption = 'Export to Other Formats...'
      ImageIndex = 72
      OnClick = mnuOutputExportOtherClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object pmRunReportTemplate: TMenuItem
      Action = actRunReportTemplate
    end
    object pmNewReportTemplate: TMenuItem
      Action = actNewReportTemplate
    end
    object pmEditReportTemplate: TMenuItem
      Action = actEditReportTemplate
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object pmRunSnapshot: TMenuItem
      Action = actRunSnapshot
    end
    object pmNewSnapshot: TMenuItem
      Action = actNewSnapshot
    end
    object pmEditSnapshot: TMenuItem
      Action = actEditSnapshot
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'wzd'
    Filter = 'Report Files (*.wzd)|*.wzd'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save As...'
    Left = 36
    Top = 156
  end
  object alResult: TActionList
    Images = dmFormActions.ilMenuOn
    Left = 156
    Top = 104
    object actResultCopy: TAction
      Caption = '&Copy'
      Hint = 'Copy the grid content to the clipboard'
      ImageIndex = 3
      ShortCut = 16451
      OnExecute = actResultCopyExecute
    end
    object actResultCopyAll: TAction
      Caption = 'Copy All'
      ImageIndex = 3
      OnExecute = actResultCopyAllExecute
    end
    object actResultView: TAction
      Caption = '&View'
      Hint = 'View details for the selected cell'
      ImageIndex = 35
    end
    object actPrintGrid: TAction
      Category = 'Output'
      Caption = '&Print Grid'
      ImageIndex = 1
      OnExecute = actPrintGridExecute
    end
    object actNewSnapshot: TAction
      Category = 'Output'
      Caption = 'New &Snapshot'
      Hint = 'Create a new snapshot of the results'
      OnExecute = actNewSnapshotExecute
    end
    object actEditSnapshot: TAction
      Category = 'Output'
      Caption = 'Edit S&napshot'
      Hint = 'Edit the existing snapshot template'
      OnExecute = actEditSnapshotExecute
    end
    object actRunSnapshot: TAction
      Category = 'Output'
      Caption = '&Run Snapshot'
      Hint = 'Run the existing snapshot'
      OnExecute = actRunSnapshotExecute
    end
    object actNewReportTemplate: TAction
      Category = 'Output'
      Caption = 'New Report &Template'
      Hint = 'Create a new report Template'
      OnExecute = actNewReportTemplateExecute
    end
    object actEditReportTemplate: TAction
      Category = 'Output'
      Caption = 'Edit Report Templ&ate'
      Hint = 'Edit the existing report template'
      OnExecute = actEditReportTemplateExecute
    end
    object actRunReportTemplate: TAction
      Category = 'Output'
      Caption = 'R&un Report Template'
      Hint = 'Display the results using the existing report template'
      OnExecute = actRunReportTemplateExecute
    end
  end
  object DataSource: TDataSource
    DataSet = qryCustomReport
    Left = 320
    Top = 96
  end
  object qryCustomReport: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 392
    Top = 96
  end
end
