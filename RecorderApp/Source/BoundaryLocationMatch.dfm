object dlgBoundaryLocationMatch: TdlgBoundaryLocationMatch
  Left = 244
  Top = 193
  Width = 709
  Height = 349
  Caption = 'Location Match'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object plContainer: TPanel
    Left = 0
    Top = 0
    Width = 701
    Height = 319
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      701
      319)
    object Bevel1: TBevel
      Left = 0
      Top = 1
      Width = 701
      Height = 286
      Anchors = [akLeft, akTop, akRight, akBottom]
      Shape = bsFrame
    end
    object lblBoundaryCount: TLabel
      Left = 12
      Top = 12
      Width = 76
      Height = 13
      Caption = 'Boundary Count'
    end
    object lblMatchedLocationsCount: TLabel
      Left = 388
      Top = 12
      Width = 91
      Height = 13
      Anchors = [akTop]
      Caption = 'Matched Locations'
    end
    object lblBoundaryAttribute: TLabel
      Left = 12
      Top = 36
      Width = 115
      Height = 13
      Caption = 'Boundary Attribute Field:'
    end
    object btnCancel: TImageListButton
      Left = 626
      Top = 294
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      TabOrder = 0
      OnClick = btnCancelClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 1
    end
    object btnImport: TImageListButton
      Left = 546
      Top = 294
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Import'
      ModalResult = 1
      TabOrder = 1
      OnClick = btnImportClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 0
    end
    object cmbBoundaryAttribute: TComboBox
      Left = 132
      Top = 32
      Width = 561
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 2
      OnChange = cmbBoundaryAttributeChange
    end
    object btnNewEntry: TImageListButton
      Left = 12
      Top = 60
      Width = 125
      Height = 25
      Caption = '&Make new Entry'
      TabOrder = 3
      OnClick = btnNewEntryClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 2
    end
    object btnNewEntries: TImageListButton
      Left = 144
      Top = 60
      Width = 125
      Height = 25
      Cancel = True
      Caption = 'Make New Entrie&s'
      TabOrder = 4
      OnClick = btnNewEntriesClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 2
    end
    object sgMatchedObjects: TStringGrid
      Left = 12
      Top = 96
      Width = 681
      Height = 183
      Anchors = [akLeft, akTop, akRight, akBottom]
      ColCount = 3
      Ctl3D = False
      DefaultColWidth = 134
      DefaultRowHeight = 17
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing]
      ParentCtl3D = False
      TabOrder = 5
      OnClick = sgMatchedObjectsClick
      OnDrawCell = sgMatchedObjectsDrawCell
      OnKeyDown = sgMatchedObjectsKeyDown
      OnMouseDown = sgMatchedObjectsMouseDown
      ColWidths = (
        80
        290
        301)
    end
    object eMatchValue: TAddinLinkedEdit
      Tag = 1
      Left = 388
      Top = 115
      Width = 301
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 6
      Visible = False
      BorderStyle = bsSingle
      ImageIndex = 5
      ImageList = dmFormActions.ilButtons
      OnFindData = eMatchValueFindData
      OnGetData = eMatchValueGetData
      OnKeyDown = eMatchValueKeyDown
      ShowDragDropBorder = False
    end
  end
end
