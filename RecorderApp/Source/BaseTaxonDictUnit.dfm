inherited BaseTaxonDict: TBaseTaxonDict
  Caption = 'BaseTaxonDict'
  PixelsPerInch = 96
  TextHeight = 13
  inherited DictSplitter: TSplitter
    Height = 337
  end
  inherited pnlSelection: TPanel
    inherited lblListName: TLabel
      Width = 19
      Caption = 'List:'
    end
    inherited cmbList: TComboBox
      Left = 32
      Width = 428
    end
  end
  inherited pcBrowser: TPageControl
    Height = 337
    inherited tsTree: TTabSheet
      inherited tvDictionary: TKeyboardRapidTree
        Height = 325
        PopupMenu = pmHierarchy
        OnDblClick = tvDictionaryDblClick
        Indent = 32
        Images = dmFormActions.ilTaxon
        Data = {0400000000000000}
      end
    end
  end
  inherited pnlDetails: TPanel
    Height = 337
  end
  inherited mnuChildMerge: TMainMenu
    Images = dmFormActions.ilMenuOn
    Left = 44
    Top = 80
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 2
      object mnuEditCopy: TMenuItem
        Action = dmFormActions.actCopy
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuEditFind: TMenuItem
        Action = actFind
      end
      object mnuEditSort: TMenuItem
        Caption = 'S&ort By'
        Hint = 'Sort taxa'
        ImageIndex = 7
        object mnuEditSortDefault: TMenuItem
          Action = actSortDefault
        end
        object mnuEditSortTaxonName: TMenuItem
          Action = actSortSciName
        end
        object mnuEditSortAuthority: TMenuItem
          Action = actSortAuthority
        end
        object mnuEditSortCommonName: TMenuItem
          Action = actSortComName
        end
      end
      object mnuEditFilter: TMenuItem
        Action = actFilter
      end
    end
  end
  object alTaxonDict: TActionList
    Images = dmFormActions.ilMenuOn
    Left = 140
    Top = 80
    object actSortDefault: TAction
      Caption = '< Default >'
      Hint = 'Use the default sort order'
      OnExecute = actSortDefaultExecute
    end
    object actSortSciName: TAction
      Caption = '&Taxon Name'
      Hint = 'Sort taxa by name'
      OnExecute = actSortSciNameExecute
    end
    object actSortAuthority: TAction
      Caption = '&Authority'
      Hint = 'Sort taxa by authority'
      OnExecute = actSortAuthorityExecute
    end
    object actSortComName: TAction
      Caption = '&Common Name'
      Hint = 'Sort taxa by common name'
      OnExecute = actSortComNameExecute
    end
    object actFind: TAction
      Caption = '&Find...'
      Hint = 'Find a taxon'
      ImageIndex = 5
      ShortCut = 16454
      OnExecute = actFindExecute
    end
    object actFilter: TAction
      Caption = '&Simple Filter...'
      Hint = 'Apply a filter on the taxa'
      ImageIndex = 6
      OnExecute = actFilterExecute
    end
  end
  object pmSort: TPopupMenu
    Left = 44
    Top = 204
    object pmSortDefault: TMenuItem
      Action = actSortDefault
      Default = True
    end
    object pmSortName: TMenuItem
      Action = actSortSciName
    end
    object pmSortAuthority: TMenuItem
      Action = actSortAuthority
    end
    object pmSortCommonName: TMenuItem
      Action = actSortComName
    end
  end
  object pmHierarchy: TPopupMenu
    Left = 44
    Top = 140
    object pmHSortBy: TMenuItem
      Caption = '&Sort By'
      object pmHSortDefault: TMenuItem
        Action = actSortDefault
      end
      object pmHSortSciName: TMenuItem
        Action = actSortSciName
      end
      object pmHSortAuthority: TMenuItem
        Action = actSortAuthority
      end
      object pmHSortComName: TMenuItem
        Action = actSortComName
      end
    end
  end
end
