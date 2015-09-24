inherited frmRuckSack: TfrmRuckSack
  Left = 690
  Top = 129
  Width = 340
  Height = 430
  Caption = 'My Rucksack'
  Constraints.MinHeight = 160
  Constraints.MinWidth = 340
  Position = poDefaultPosOnly
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pcRuckSack: TPageControl [0]
    Left = 0
    Top = 37
    Width = 332
    Height = 347
    ActivePage = tsTaxa
    Align = alClient
    TabOrder = 1
    OnChange = pcRuckSackChange
    OnEnter = pcRuckSackEnter
    object tsTaxa: TTabSheet
      Caption = 'Taxa'
      object shpTaxa: TShape
        Tag = 3
        Left = 0
        Top = 0
        Width = 324
        Height = 319
        Align = alClient
        Brush.Color = clLime
        Pen.Color = clRed
        Pen.Style = psDash
      end
      object sgRucksack: TStringGrid
        Left = 1
        Top = 1
        Width = 322
        Height = 317
        ColCount = 2
        DefaultRowHeight = 18
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goRowSelect]
        TabOrder = 0
        OnDrawCell = sgRucksackDrawCell
        OnKeyUp = sgRucksackKeyUp
        OnMouseUp = sgRucksackMouseUp
        OnSelectCell = sgRucksackSelectCell
        OnSetEditText = sgRucksackSetEditText
        ColWidths = (
          152
          145)
      end
    end
    object tsBiotopes: TTabSheet
      Caption = 'Biotopes'
      ImageIndex = 1
      object shpBiotopes: TShape
        Tag = 3
        Left = 0
        Top = 0
        Width = 324
        Height = 319
        Align = alClient
        Brush.Color = clLime
        Pen.Color = clRed
        Pen.Style = psDash
      end
      object lbBiotopes: TListBox
        Left = 1
        Top = 1
        Width = 322
        Height = 317
        DragMode = dmAutomatic
        ItemHeight = 13
        MultiSelect = True
        PopupMenu = pmListOptions
        TabOrder = 0
        OnClick = ListBoxClick
        OnKeyDown = RuckSackKeyDown
      end
    end
    object tsLocations: TTabSheet
      Caption = 'Locations'
      ImageIndex = 3
      object shpLocations: TShape
        Tag = 3
        Left = 0
        Top = 0
        Width = 324
        Height = 319
        Align = alClient
        Brush.Color = clLime
        Pen.Color = clRed
        Pen.Style = psDash
      end
      object lbLocations: TListBox
        Left = 1
        Top = 1
        Width = 322
        Height = 317
        DragMode = dmAutomatic
        ItemHeight = 13
        MultiSelect = True
        PopupMenu = pmListOptions
        TabOrder = 0
        OnClick = ListBoxClick
        OnKeyDown = RuckSackKeyDown
      end
    end
    object tsPeople: TTabSheet
      Caption = 'People'
      ImageIndex = 4
      object shpPeople: TShape
        Tag = 3
        Left = 0
        Top = 0
        Width = 324
        Height = 319
        Align = alClient
        Brush.Color = clLime
        Pen.Color = clRed
        Pen.Style = psDash
      end
      object lbPeople: TListBox
        Left = 1
        Top = 1
        Width = 322
        Height = 317
        DragMode = dmAutomatic
        ItemHeight = 13
        MultiSelect = True
        PopupMenu = pmListOptions
        TabOrder = 0
        OnClick = ListBoxClick
        OnKeyDown = RuckSackKeyDown
      end
    end
    object tsDocuments: TTabSheet
      Caption = 'Documents'
      ImageIndex = 5
      object shpReferences: TShape
        Tag = 3
        Left = 0
        Top = 0
        Width = 324
        Height = 319
        Align = alClient
        Brush.Color = clLime
        Pen.Color = clRed
        Pen.Style = psDash
      end
      object lbReferences: TListBox
        Left = 1
        Top = 1
        Width = 322
        Height = 317
        DragMode = dmAutomatic
        ItemHeight = 13
        MultiSelect = True
        PopupMenu = pmListOptions
        TabOrder = 0
        OnClick = ListBoxClick
        OnKeyDown = RuckSackKeyDown
      end
    end
  end
  object pnlButtons: TPanel [1]
    Left = 0
    Top = 0
    Width = 332
    Height = 37
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblRucksack: TLabel
      Left = 4
      Top = 12
      Width = 52
      Height = 13
      Caption = 'Rucksack:'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object cmbRucksacks: TComboBox
      Left = 60
      Top = 8
      Width = 233
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnChange = cmbRucksacksChange
    end
    object bbBrowser: TButton
      Left = 292
      Top = 7
      Width = 23
      Height = 22
      Hint = 'Select another folder'
      Caption = '...'
      TabOrder = 1
      OnClick = bbBrowseClick
    end
  end
  inherited mnuChildMerge: TMainMenu
    Left = 128
    Top = 76
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 2
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
      object mnuEditRemove: TMenuItem
        Action = actRemove
      end
      object mnuEditClearRucksack: TMenuItem
        Action = actClearRucksack
      end
    end
  end
  object pmListOptions: TPopupMenu
    Images = dmFormActions.ilMenuOn
    Left = 128
    Top = 176
    object mnuRemove: TMenuItem
      Action = actRemove
    end
    object ClearRucksack1: TMenuItem
      Action = actClearRucksack
    end
  end
  object dlgFolder: TFolderBrowser
    BrowseFlags = [bfDirsOnly, bfStatusText]
    NewDialogStyle = False
    Title = 'Select folder'
    Left = 128
    Top = 125
  end
  object alRucksack: TActionList
    Images = dmFormActions.ilMenuOn
    Left = 212
    Top = 77
    object actRemove: TAction
      Caption = 'Re&move'
      Hint = 'Remove the selected item(s) from the Rucksack'
      ImageIndex = 50
      ShortCut = 46
      OnExecute = actRemoveExecute
    end
    object actClearRucksack: TAction
      Caption = 'C&lear Rucksack'
      Hint = 'Clear the content of the current rucksack'
      ImageIndex = 33
      OnExecute = actClearRucksackExecute
    end
  end
end
