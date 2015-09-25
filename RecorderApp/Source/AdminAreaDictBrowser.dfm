inherited frmAdminAreaDictBrowser: TfrmAdminAreaDictBrowser
  Width = 498
  Caption = 'Administrative Area Browser'
  PixelsPerInch = 96
  TextHeight = 13
  inherited DictSplitter: TSplitter
    Left = 295
    Height = 329
  end
  inherited pnlSelection: TPanel
    Width = 490
    inherited lblListName: TLabel
      Width = 27
      Caption = 'Type:'
    end
    inherited cmbList: TComboBox
      Left = 40
      Width = 355
    end
    inherited btnShowAll: TButton
      Left = 406
    end
  end
  inherited pcBrowser: TPageControl
    Width = 295
    Height = 329
    inherited tsTree: TTabSheet
      inherited tvDictionary: TKeyboardRapidTree
        Width = 285
        Height = 317
        PopupMenu = pmHierarchy
        Data = {0400000000000000}
      end
    end
  end
  inherited pnlDetails: TPanel
    Left = 305
    Height = 329
    object hvAdminAreaDetails: THTMLViewer
      Left = 0
      Top = 0
      Width = 185
      Height = 329
      Cursor = 2
      OnHotSpotClick = hvAdminAreaDetailsHotSpotClick
      ViewImages = False
      TabOrder = 0
      Align = alClient
      DefBackground = clWhite
      BorderStyle = htFocused
      HistoryMaxCount = 0
      DefFontName = 'Times New Roman'
      DefPreFontName = 'Courier New'
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
    Left = 36
    Top = 88
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 2
      Hint = 'Sort Administrative Areas'
      object mnuEditCopy: TMenuItem
        Action = dmFormActions.actCopy
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
      object mnuEditSort: TMenuItem
        Caption = '&Sort By'
        ImageIndex = 7
        object mnuEditSortCode: TMenuItem
          Action = actSortCode
        end
        object mnuEditSortName: TMenuItem
          Action = actSortName
        end
      end
      object mnuEditFilter: TMenuItem
        Action = actFilter
      end
      object mnuEditShowMetadata: TMenuItem
        Action = actShowMetadata
      end
      object mnuEditShowItemDetails: TMenuItem
        Action = actShowItemDetails
        Caption = 'Show Details'
        GroupIndex = 1
      end
    end
  end
  object pmHierarchy: TPopupMenu
    Left = 36
    Top = 149
    object pmHSortBy: TMenuItem
      Caption = '&Sort By'
      object pmHSortCode: TMenuItem
        Action = actSortCode
      end
      object pmHSortName: TMenuItem
        Action = actSortName
      end
    end
    object pmHQuickReports: TMenuItem
      Caption = '&Quick Report'
      OnClick = pmHQuickReportsClick
      object mnuDummy: TMenuItem
        Caption = 'Sub Menu'
        Visible = False
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
  object pmSort: TPopupMenu
    Left = 36
    Top = 213
    object pmSortCode: TMenuItem
      Action = actSortCode
    end
    object pmSortName: TMenuItem
      Action = actSortName
      Default = True
    end
  end
  object alAdminArea: TActionList
    Images = dmFormActions.ilMenuOn
    Left = 112
    Top = 89
    object actSortCode: TAction
      Caption = '&Code'
      Hint = 'Sort Administrative Areas by Code, where applicable'
      OnExecute = actSortCodeExecute
    end
    object actSortName: TAction
      Caption = '&Name'
      Hint = 'Sort Administrative Areas by Name'
      OnExecute = actSortNameExecute
    end
    object actFind: TAction
      Caption = '&Find...'
      Hint = 'Find an item'
      ImageIndex = 5
      ShortCut = 16454
      OnExecute = actFindExecute
    end
    object actFilter: TAction
      Caption = '&Simple Filter...'
      Hint = 'Apply a filter on the Administrative Areas'
      ImageIndex = 6
      OnExecute = actFilterExecute
    end
    object actShowMetadata: TAction
      Caption = 'Show &Metadata'
      Hint = 'Show Metadata'
      ImageIndex = 52
      OnExecute = actShowMetadataExecute
    end
    object actShowItemDetails: TAction
      Caption = 'actShowHTMLViewer'
      Hint = 'Show details for selected item in HTML pane'
      ImageIndex = 55
      OnExecute = actShowItemDetailsExecute
    end
  end
end
