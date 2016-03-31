inherited frmEnhancedTermLists: TfrmEnhancedTermLists
  Left = 234
  Top = 297
  Width = 838
  Height = 605
  Caption = 'Enhanced Term Lists'
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter [0]
    Left = 321
    Top = 0
    Height = 522
  end
  object pnlTerms: TPanel [1]
    Left = 0
    Top = 0
    Width = 321
    Height = 522
    Align = alLeft
    TabOrder = 0
    DesignSize = (
      321
      522)
    object Label1: TLabel
      Left = 4
      Top = 48
      Width = 78
      Height = 13
      Caption = 'Available Terms:'
    end
    object Label5: TLabel
      Left = 4
      Top = 4
      Width = 46
      Height = 13
      Caption = 'Term List:'
    end
    object tvTerms: TKeyboardRapidTree
      Left = 2
      Top = 64
      Width = 315
      Height = 450
      SmoothExpandCollapse = False
      FitColumnToClientWidth = True
      NodeSelectionStyle = tnsTextOnly
      FitToHeight = False
      RightClickSelect = True
      DoubleBuffered = True
      Anchors = [akLeft, akTop, akRight, akBottom]
      TransparentMode = True
      DefaultRowHeight = 16
      DragMode = dmAutomatic
      RowCount = 0
      FixedRows = 0
      GridLineWidth = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
      PopupMenu = pmTreeview
      TabOrder = 0
      OnExit = tvTermsExit
      OnKeyDown = tvTermsKeyDown
      OnKeyUp = tvTermsKeyUp
      OnMouseDown = tvTermsMouseDown
      OnMouseUp = tvTermsMouseUp
      OnSelectCell = tvTermsSelectCell
      OnExpanding = tvTermsExpanding
      OnChange = tvTermsChange
      HideSelection = False
      Showlines = True
      ShowRoot = True
      ShowButtons = True
      ShowImages = False
      ShowLogic = False
      SortType = stNone
      WordWrap = False
      AutoMove = False
      ToolTips = False
      AutoExpand = False
      TooltipColor = clInfoBk
      ToolTipPause = 1000
      StatesDrawed = False
      HTMLDrawing = True
      ColWidths = (
        311)
      Data = {0400000000000000}
    end
    object cmbConceptGroup: TIDComboBox
      Left = 2
      Top = 20
      Width = 315
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      OnChange = cmbConceptGroupChange
      OnPopulate = cmbConceptGroupPopulate
    end
  end
  object Panel2: TPanel [2]
    Left = 0
    Top = 522
    Width = 830
    Height = 37
    Align = alBottom
    TabOrder = 1
    object btnDelete: TImageListButton
      Left = 168
      Top = 6
      Width = 75
      Height = 25
      Action = actDelete
      TabOrder = 2
      ImageList = dmFormActions.ilButtons
      ImageIndex = 4
    end
    object btnEdit: TImageListButton
      Left = 88
      Top = 6
      Width = 75
      Height = 25
      Action = actEdit
      TabOrder = 1
      ImageList = dmFormActions.ilButtons
      ImageIndex = 3
    end
    object btnAdd: TImageListButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Add a new term'
      Caption = '&Add'
      TabOrder = 0
      OnClick = btnAddClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 2
    end
  end
  object pnlDetails: TPanel [3]
    Left = 324
    Top = 0
    Width = 506
    Height = 522
    Align = alClient
    TabOrder = 2
    OnResize = pnlDetailsResize
    DesignSize = (
      506
      522)
    object Label2: TLabel
      Left = 12
      Top = 15
      Width = 27
      Height = 13
      Caption = 'Term:'
    end
    object Label3: TLabel
      Left = 12
      Top = 47
      Width = 51
      Height = 13
      Caption = 'Language:'
    end
    object Label4: TLabel
      Left = 12
      Top = 112
      Width = 51
      Height = 13
      Caption = 'Synonyms:'
    end
    object Label6: TLabel
      Left = 12
      Top = 84
      Width = 47
      Height = 21
      Caption = 'Sort Code:'
    end
    object btnSave: TImageListButton
      Left = 338
      Top = 482
      Width = 75
      Height = 26
      Hint = 'Save term details'
      Anchors = [akRight, akBottom]
      Caption = '&Save'
      Enabled = False
      TabOrder = 5
      OnClick = btnSaveClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 0
    end
    object btnCancel: TImageListButton
      Left = 422
      Top = 482
      Width = 75
      Height = 26
      Hint = 'Cancel changes'
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      Enabled = False
      TabOrder = 4
      OnClick = btnCancelClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 1
    end
    object eTerm: TLinkedEdit
      Tag = 1
      Left = 76
      Top = 12
      Width = 245
      Height = 21
      OnExit = eTermExit
      TabOrder = 0
      BorderStyle = bsSingle
      ShowButton = False
      ShowDragDropBorder = False
    end
    object cmbLanguage: TIDComboBox
      Left = 76
      Top = 44
      Width = 421
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbLanguageChange
      OnPopulate = cmbLanguagePopulate
    end
    object sgSynonyms: TControlStringGrid
      Left = 76
      Top = 112
      Width = 396
      Height = 361
      Anchors = [akLeft, akTop, akRight, akBottom]
      ColCount = 3
      Ctl3D = True
      DefaultRowHeight = 18
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
      ParentCtl3D = False
      TabOrder = 1
      ColWidths = (
        20
        114
        92)
    end
    object btnAddSynonym: TImageListButton
      Left = 471
      Top = 116
      Width = 23
      Height = 24
      Hint = 'Add a new synonym'
      Anchors = [akTop, akRight]
      TabOrder = 2
      ImageList = dmFormActions.ilButtons
      ImageIndex = 2
    end
    object btnRemoveSynonym: TImageListButton
      Left = 471
      Top = 147
      Width = 23
      Height = 24
      Hint = 'Delete the selected synonym'
      Anchors = [akTop, akRight]
      TabOrder = 3
      ImageList = dmFormActions.ilButtons
      ImageIndex = 4
    end
    object eSortCode: TEdit
      Left = 76
      Top = 80
      Width = 80
      Height = 21
      MaxLength = 8
      TabOrder = 6
      OnExit = eSortCodeExit
      OnKeyPress = eSortCodeKeyPress
    end
  end
  inherited mnuChildMerge: TMainMenu
    Images = dmFormActions.ilMenuOn
    Left = 212
    Top = 0
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 2
      object mnuEditAdd: TMenuItem
        Caption = '&Add'
        Hint = 'Add a term to the currently selected list'
        object mnuMainAddSibling: TMenuItem
          Action = actAddTerm
        end
        object mnuMainAddChild: TMenuItem
          Action = actAddChildTerm
        end
      end
      object mnuEditEdit: TMenuItem
        Action = actEdit
      end
      object mnuEditDelete: TMenuItem
        Action = actDelete
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuEditReturnData: TMenuItem
        Action = dmFormActions.actTransferData
      end
      object mnuEditRename: TMenuItem
        Action = actRename
      end
    end
  end
  object pmAdd: TPopupMenu
    Left = 20
    Top = 328
    object pmAddAddTerm: TMenuItem
      Action = actAddTerm
    end
    object pmAddAddChildTerm: TMenuItem
      Action = actAddChildTerm
    end
  end
  object alEnhancedTermLists: TActionList
    Images = dmFormActions.ilMenuOn
    Left = 124
    Top = 328
    object actAddTerm: TAction
      Caption = '&Add Term'
      Hint = 'Add a top level term'
      OnExecute = actAddTermExecute
    end
    object actAddChildTerm: TAction
      Caption = 'Add &Child Term'
      Hint = 'Add a term as a child of the selected term'
      OnExecute = actAddTermExecute
    end
    object actEdit: TAction
      Caption = '&Edit'
      Hint = 'Edit the selected term'
      ImageIndex = 49
      OnExecute = actEditExecute
    end
    object actDelete: TAction
      Caption = '&Delete'
      Hint = 'Delete the selected term'
      ImageIndex = 50
      OnExecute = actDeleteExecute
    end
    object actRename: TAction
      Caption = '&Rename'
      ShortCut = 113
      OnExecute = actRenameExecute
    end
  end
  object pmTreeview: TPopupMenu
    Images = dmFormActions.ilButtons
    Left = 20
    Top = 284
    object mnuTreeViewAddTerm: TMenuItem
      Action = actAddTerm
    end
    object mnuTreeViewAddChildTerm: TMenuItem
      Action = actAddChildTerm
    end
    object mnuTreeViewEdit: TMenuItem
      Action = actEdit
    end
    object mnuTreeViewDelete: TMenuItem
      Action = actDelete
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object mnuTreeViewRename: TMenuItem
      Action = actRename
    end
  end
end
