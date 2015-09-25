inherited frmTermLists: TfrmTermLists
  Left = 417
  Top = 202
  Width = 540
  Height = 474
  ActiveControl = cmbTermLists
  Caption = 'Configure Term Lists'
  Constraints.MinWidth = 436
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object TermListSplitter: TSplitter [0]
    Left = 131
    Top = 29
    Width = 10
    Height = 353
    Align = alRight
    Color = clBtnFace
    MinSize = 1
    ParentColor = False
    ResizeStyle = rsUpdate
    OnCanResize = TermListSplitterCanResize
    OnMoved = FormResize
    OnPaint = TermListSplitterPaint
  end
  object pnlButtons: TPanel [1]
    Left = 0
    Top = 382
    Width = 524
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object bbAdd: TImageListButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Add a term to the currently selected list'
      Caption = '&Add'
      TabOrder = 0
      OnClick = bbAddClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 2
    end
    object bbEdit: TImageListButton
      Left = 88
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Edit the selected term in the currently selected list'
      Caption = '&Edit'
      TabOrder = 1
      OnClick = actEditExecute
      ImageList = dmFormActions.ilButtons
      ImageIndex = 3
    end
    object bbDelete: TImageListButton
      Left = 168
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Delete the selected term from the currently selected list'
      Caption = '&Delete'
      TabOrder = 2
      OnClick = actDeleteExecute
      ImageList = dmFormActions.ilButtons
      ImageIndex = 4
    end
  end
  object pnlLabel: TPanel [2]
    Left = 0
    Top = 0
    Width = 524
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 52
      Height = 13
      Caption = 'Select List:'
    end
    object cmbTermLists: TComboBox
      Left = 64
      Top = 4
      Width = 193
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbTermListsChange
    end
  end
  object scbTermDetails: TScrollBox [3]
    Left = 141
    Top = 29
    Width = 383
    Height = 353
    Align = alRight
    Constraints.MinHeight = 337
    Constraints.MinWidth = 380
    TabOrder = 2
    object bbCancel: TImageListButton
      Left = 294
      Top = 319
      Width = 75
      Height = 25
      Hint = 'Cancel changes'
      Cancel = True
      Caption = '&Cancel'
      Enabled = False
      ModalResult = 2
      TabOrder = 3
      OnClick = bbCancelClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 1
    end
    object bbSave: TImageListButton
      Left = 211
      Top = 319
      Width = 75
      Height = 25
      Hint = 'Save term details'
      Caption = '&Save'
      Enabled = False
      ModalResult = 1
      TabOrder = 2
      OnClick = bbSaveClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 0
    end
    object gbDetails: TGroupBox
      Left = 8
      Top = 7
      Width = 361
      Height = 166
      Caption = 'Details'
      TabOrder = 0
      object Label3: TLabel
        Left = 8
        Top = 22
        Width = 59
        Height = 13
        Caption = 'Short Name:'
      end
      object Label4: TLabel
        Left = 8
        Top = 72
        Width = 56
        Height = 13
        Caption = 'Description:'
      end
      object Label2: TLabel
        Left = 8
        Top = 48
        Width = 58
        Height = 13
        Caption = 'Long Name:'
      end
      object eShortName: TEdit
        Left = 80
        Top = 19
        Width = 121
        Height = 21
        MaxLength = 20
        TabOrder = 0
        OnChange = eShortNameChange
      end
      object eLongName: TEdit
        Left = 80
        Top = 45
        Width = 273
        Height = 21
        MaxLength = 100
        TabOrder = 1
      end
      object dbreDescription: TDBRichEdit
        Left = 8
        Top = 89
        Width = 345
        Height = 68
        DataSource = dmTermLists.dsTermList
        PopupMenu = dmFormActions.pmRTF
        ScrollBars = ssVertical
        TabOrder = 2
        OnEnter = reDescriptionEnter
        OnExit = reDescriptionExit
      end
    end
    object gbAdditionalInfo: TGroupBox
      Left = 8
      Top = 180
      Width = 361
      Height = 131
      Caption = 'Additional Information'
      TabOrder = 1
      Visible = False
      object lblTextField1: TLabel
        Left = 142
        Top = 20
        Width = 44
        Height = 13
        Caption = 'Authority:'
      end
      object lblTextField2: TLabel
        Left = 142
        Top = 43
        Width = 52
        Height = 13
        Caption = 'Date From:'
      end
      object lblTextField3: TLabel
        Left = 142
        Top = 66
        Width = 42
        Height = 13
        Caption = 'Date To:'
      end
      object bvlImage: TBevel
        Left = 8
        Top = 26
        Width = 54
        Height = 54
      end
      object lblAssocRecordCard: TLabel
        Left = 9
        Top = 103
        Width = 118
        Height = 13
        Caption = 'Associated Record Card:'
      end
      object rgDeterminationVerification: TRadioGroup
        Left = 148
        Top = 16
        Width = 205
        Height = 81
        Caption = 'Determination Verification'
        Items.Strings = (
          'Not Verified'
          'Failed/Pending Verification'
          'Passed Verification')
        TabOrder = 9
        Visible = False
      end
      object chklbScreens: TCheckListBox
        Left = 200
        Top = 16
        Width = 153
        Height = 105
        ItemHeight = 13
        TabOrder = 7
        Visible = False
      end
      object cmbDataTypes: TComboBox
        Left = 224
        Top = 16
        Width = 129
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 8
        Visible = False
        Items.Strings = (
          'Boolean'
          'Memo'
          'Number'
          'Spatial Reference'
          'Text'
          'Vague Date')
      end
      object cmbAssocRecordCard: TComboBox
        Left = 141
        Top = 99
        Width = 208
        Height = 21
        Style = csDropDownList
        Enabled = False
        ItemHeight = 13
        TabOrder = 2
        Items.Strings = (
          'Hoverfly Recording Card'
          'MyCOM Recording Card')
      end
      object dbeAddField1: TDBEdit
        Left = 224
        Top = 17
        Width = 129
        Height = 21
        DataSource = dmTermLists.dsTermList
        TabOrder = 3
        OnChange = dbeAddField1Change
      end
      object dbeAddField2: TDBEdit
        Left = 224
        Top = 40
        Width = 129
        Height = 21
        DataSource = dmTermLists.dsTermList
        TabOrder = 4
      end
      object dbeAddField3: TDBEdit
        Left = 224
        Top = 63
        Width = 129
        Height = 21
        DataSource = dmTermLists.dsTermList
        TabOrder = 5
      end
      object imgSample: TDBImage
        Left = 9
        Top = 27
        Width = 52
        Height = 52
        Color = clBtnFace
        DataSource = dmTermLists.dsTermList
        TabOrder = 6
      end
      object bbOpen: TImageListButton
        Left = 69
        Top = 27
        Width = 60
        Height = 25
        Caption = '&Open'
        Enabled = False
        TabOrder = 0
        OnClick = bbOpenClick
        ImageList = dmFormActions.ilMenuOn
        ImageIndex = 59
      end
      object bbDefault: TButton
        Left = 69
        Top = 55
        Width = 60
        Height = 25
        Caption = '&Default'
        TabOrder = 1
        OnClick = bbDefaultClick
      end
      object chkHide: TCheckBox
        Left = 149
        Top = 102
        Width = 84
        Height = 17
        Caption = 'Hide'
        TabOrder = 10
      end
      object btnAddValue: TImageListButton
        Left = 330
        Top = 41
        Width = 23
        Height = 24
        TabOrder = 12
        OnClick = btnAddValueClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 2
      end
      object btnDelValue: TImageListButton
        Left = 330
        Top = 64
        Width = 23
        Height = 24
        TabOrder = 13
        OnClick = btnDelValueClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 4
      end
      object sgUnitValues: TStringGrid
        Left = 168
        Top = 40
        Width = 164
        Height = 83
        ColCount = 1
        DefaultRowHeight = 17
        FixedCols = 0
        RowCount = 2
        TabOrder = 11
        OnKeyDown = sgUnitValuesKeyDown
        OnKeyPress = sgUnitValuesKeyPress
        OnSelectCell = sgUnitValuesSelectCell
        ColWidths = (
          145)
      end
    end
  end
  object tvTermList: TTreeView [4]
    Left = 0
    Top = 29
    Width = 131
    Height = 353
    Align = alClient
    HideSelection = False
    Indent = 35
    PopupMenu = pmEdit
    ReadOnly = True
    TabOrder = 1
    OnChange = tvTermListChange
    OnDragDrop = tvTermListDragDrop
    OnDragOver = tvTermListDragOver
    OnKeyDown = tvTermListKeyDown
  end
  inherited mnuChildMerge: TMainMenu
    Left = 56
    Top = 36
    object mnuChildEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 2
      object mnuEditAdd: TMenuItem
        Caption = '&Add'
        Hint = 'Add a term to the currently selected list'
        object mnuMainAddSibling: TMenuItem
          Action = actAddSibling
        end
        object mnuMainAddChild: TMenuItem
          Action = actAddChild
        end
      end
      object mnuEditEdit: TMenuItem
        Action = actEdit
      end
      object mnuEditDelete: TMenuItem
        Action = actDelete
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuEditCut: TMenuItem
        Action = dmFormActions.actCut
      end
      object mnuEditCopy: TMenuItem
        Action = dmFormActions.actCopy
      end
      object mnuEditPaste: TMenuItem
        Action = dmFormActions.actPaste
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuEditBold: TMenuItem
        Action = dmFormActions.actBold
      end
      object mnuEditItalic: TMenuItem
        Action = dmFormActions.actItalic
      end
      object mnuEditUnderline: TMenuItem
        Action = dmFormActions.actUnderline
      end
    end
  end
  object pmEdit: TPopupMenu
    Left = 56
    Top = 84
    object mnuAdd: TMenuItem
      Caption = '&Add'
      Hint = 'Add a term to the currently selected list'
      object mnuAddSibling: TMenuItem
        Action = actAddSibling
      end
      object mnuAddChild: TMenuItem
        Action = actAddChild
      end
    end
    object mnuEdit: TMenuItem
      Action = actEdit
    end
    object mnuDelete: TMenuItem
      Action = actDelete
    end
  end
  object dlgOpenPic: TOpenPictureDialog
    Filter = 
      'All (*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf)|*.jpg;*.jpeg;*.bmp;*.' +
      'ico;*.emf;*.wmf|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*' +
      '.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.ico|Enhanced' +
      ' Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf'
    Left = 52
    Top = 200
  end
  object pmAdd: TPopupMenu
    Left = 56
    Top = 137
    object mnuSibling: TMenuItem
      Action = actAddSibling
    end
    object mnuChild: TMenuItem
      Action = actAddChild
    end
  end
  object alTermLists: TActionList
    Left = 52
    Top = 248
    object actAddSibling: TAction
      Caption = '&Sibling'
      Hint = 'Add a term on the same level as the selected one'
      OnExecute = actAddSiblingExecute
    end
    object actAddChild: TAction
      Caption = '&Child'
      Hint = 'Add a term as a child to the selected one'
      OnExecute = actAddChildExecute
    end
    object actEdit: TAction
      Caption = '&Edit'
      Hint = 'Edit the selected term in the Currently selected list'
      OnExecute = actEditExecute
    end
    object actDelete: TAction
      Caption = '&Delete'
      Hint = 'Delete the selected term from the Currently selected list'
      OnExecute = actDeleteExecute
    end
  end
end
