inherited frmTaxonDictBrowser: TfrmTaxonDictBrowser
  Caption = 'Taxon Dictionary Browser'
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited pcBrowser: TPageControl
    OnChanging = pcBrowserChanging
    inherited tsTree: TTabSheet
      TabVisible = True
      inherited tvDictionary: TKeyboardRapidTree
        Height = 307
        Data = {0400000000000000}
      end
    end
    object tsList: TTabSheet
      Tag = 1
      BorderWidth = 1
      Caption = 'List'
      ImageIndex = 1
      object lvTaxonList: TListView
        Left = 0
        Top = 0
        Width = 350
        Height = 299
        Align = alClient
        Columns = <
          item
            Caption = 'Scientific Name'
            Width = 150
          end
          item
            Caption = 'Common Name'
            Width = 150
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        SmallImages = dmFormActions.ilTaxon
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = lvTaxonListColumnClick
        OnCompare = lvTaxonListCompare
        OnDblClick = lvTaxonListDblClick
        OnSelectItem = lvTaxonListSelectItem
      end
    end
  end
  inherited pnlDetails: TPanel
    object hvTaxonDetails: THTMLViewer
      Left = 0
      Top = 0
      Width = 185
      Height = 337
      Cursor = 2
      OnHotSpotCovered = hvTaxonDetailsHotSpotCovered
      OnHotSpotClick = hvTaxonDetailsHotSpotClick
      TabOrder = 0
      Align = alClient
      DefBackground = clWhite
      BorderStyle = htFocused
      HistoryMaxCount = 0
      DefFontName = 'Arial'
      DefPreFontName = 'Courier New'
      DefFontSize = 10
      NoSelect = False
      CharSet = DEFAULT_CHARSET
      PrintMarginLeft = 2.000000000000000000
      PrintMarginRight = 2.000000000000000000
      PrintMarginTop = 2.000000000000000000
      PrintMarginBottom = 2.000000000000000000
      PrintScale = 1.000000000000000000
      htOptions = []
    end
  end
  inherited mnuChildMerge: TMainMenu
    inherited mnuEdit: TMenuItem
      object mnuEditReturnData: TMenuItem [1]
        Action = dmFormActions.actTransferData
      end
      object mnuEditShowMetadata: TMenuItem
        Action = actShowMetaData
      end
      object mnuEditShowDetails: TMenuItem
        Action = actShowItemDetails
        Caption = 'Show Details'
      end
    end
  end
  inherited alTaxonDict: TActionList
    object actShowMetaData: TAction
      Caption = 'Show &Metadata'
      Hint = 'Show Metadata'
      ImageIndex = 52
      OnExecute = actShowMetaDataExecute
    end
    object actShowItemDetails: TAction
      Caption = 'actShowItemDetails'
      ImageIndex = 55
      OnExecute = actShowItemDetailsExecute
    end
  end
  inherited pmHierarchy: TPopupMenu
    object pmHQuickReports: TMenuItem
      Caption = '&Quick Report'
      OnClick = pmHQuickReportsClick
      object mnuOccurrencesForPlacesReport: TMenuItem
        Action = dmFormActions.actOccurrencesForPlacesReport
      end
      object mnuHReport: TMenuItem
        Action = dmFormActions.actPlacesForOccurrencesReport
      end
    end
    object pmHBatchUpdate: TMenuItem
      Caption = '&Batch Updates'
      OnClick = pmHBatchUpdateClick
      object mnuPlaceHolder: TMenuItem
        Caption = 'PlaceHolder'
        Visible = False
      end
    end
  end
end
