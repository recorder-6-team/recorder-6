inherited BaseBiotopeDict: TBaseBiotopeDict
  Top = 326
  Width = 559
  Height = 411
  Caption = 'BaseBiotopeDict'
  PixelsPerInch = 96
  TextHeight = 13
  inherited DictSplitter: TSplitter
    Left = 356
    Height = 336
  end
  inherited pnlSelection: TPanel
    Width = 551
    inherited cmbList: TComboBox
      Width = 376
    end
    inherited btnShowAll: TButton
      Left = 467
    end
  end
  inherited pcBrowser: TPageControl
    Width = 356
    Height = 336
    inherited tsTree: TTabSheet
      inherited tvDictionary: TKeyboardRapidTree
        Width = 346
        Height = 324
        PopupMenu = pmHierarchy
        Data = {0400000000000000}
      end
    end
  end
  inherited pnlDetails: TPanel
    Left = 366
    Height = 336
  end
  inherited mnuChildMerge: TMainMenu
    Images = dmFormActions.ilMenuOn
    Left = 48
    Top = 84
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
        Hint = 'Sort biotopes'
        ImageIndex = 7
        object mnuEditSortDefault: TMenuItem
          Action = actSortDefault
        end
        object mnuEditSortName: TMenuItem
          Action = actSortName
        end
        object mnuEditSortOriginalCode: TMenuItem
          Action = actSortCode
        end
        object mnuEditSortLongName: TMenuItem
          Action = actSortLongName
        end
      end
      object mnuEditFilter: TMenuItem
        Action = actFilter
      end
    end
  end
  object pmHierarchy: TPopupMenu
    Left = 48
    Top = 144
    object pmHSortBy: TMenuItem
      Caption = '&Sort By'
      object pmHSortDefault: TMenuItem
        Action = actSortDefault
      end
      object pmHSortName: TMenuItem
        Action = actSortName
      end
      object pmHSortCode: TMenuItem
        Action = actSortCode
      end
      object pmHSortLongName: TMenuItem
        Action = actSortLongName
      end
    end
  end
  object pmSort: TPopupMenu
    Left = 48
    Top = 204
    object pmSortDefault: TMenuItem
      Action = actSortDefault
      Default = True
    end
    object pmSortName: TMenuItem
      Action = actSortName
    end
    object pmSortOriginalCode: TMenuItem
      Action = actSortCode
    end
    object pmSortLongName: TMenuItem
      Action = actSortLongName
    end
  end
  object alBiotopeDiction: TActionList
    Images = dmFormActions.ilMenuOn
    Left = 144
    Top = 84
    object actSortDefault: TAction
      Caption = '< Default >'
      Hint = 'Use default sort order'
      OnExecute = actSortDefaultExecute
    end
    object actSortName: TAction
      Caption = '&Name'
      Hint = 'Sort biotopes by name'
      OnExecute = actSortNameExecute
    end
    object actSortCode: TAction
      Caption = '&Original Code'
      Hint = 'Sort biotopes by code'
      OnExecute = actSortCodeExecute
    end
    object actSortLongName: TAction
      Caption = '&Long Name'
      Hint = 'Sort biotopes by long name'
      OnExecute = actSortLongNameExecute
    end
    object actFind: TAction
      Caption = '&Find...'
      Hint = 'Find a biotope'
      ImageIndex = 5
      ShortCut = 16454
      OnExecute = actFindExecute
    end
    object actFilter: TAction
      Caption = '&Simple Filter...'
      Hint = 'Apply a filter on the Biotopes'
      ImageIndex = 6
      OnExecute = actFilterExecute
    end
  end
end
