inherited frmBiotopeDictBrowser: TfrmBiotopeDictBrowser
  Left = 298
  Top = 174
  Caption = 'Biotope Dictionary Browser'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pcBrowser: TPageControl
    inherited tsTree: TTabSheet
      inherited tvDictionary: TKeyboardRapidTree
        Data = {0400000000000000}
      end
    end
  end
  inherited pnlDetails: TPanel
    object hvBiotopeDetails: THTMLViewer
      Left = 0
      Top = 0
      Width = 185
      Height = 337
      Cursor = 2
      OnHotSpotCovered = hvBiotopeDetailsHotSpotCovered
      OnHotSpotClick = hvBiotopeDetailsHotSpotClick
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
      object ReturnData1: TMenuItem [1]
        Action = dmFormActions.actTransferData
      end
      object mnuEditShowMetadata: TMenuItem
        Action = actShowMetadata
      end
      object mnuEditShowItemDetails: TMenuItem
        Action = actShowItemDetails
        Caption = 'Show Details'
      end
    end
  end
  inherited pmHierarchy: TPopupMenu
    object pmHQuickReports: TMenuItem
      Caption = '&Quick Reports'
      OnClick = pmHQuickReportsClick
      object pmHierarchyOccurrencesForPlacesReport: TMenuItem
        Action = dmFormActions.actOccurrencesForPlacesReport
      end
      object PlacesforOccurrencesReport2: TMenuItem
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
      object TMenuItem
      end
    end
  end
  inherited alBiotopeDiction: TActionList
    object actShowMetadata: TAction
      Caption = 'Show &Metadata'
      Hint = 'Show Metadata'
      ImageIndex = 52
      OnExecute = actShowMetadataExecute
    end
    object actShowItemDetails: TAction
      Caption = 'actShowItemDetails'
      ImageIndex = 55
      OnExecute = actShowItemDetailsExecute
    end
  end
end
