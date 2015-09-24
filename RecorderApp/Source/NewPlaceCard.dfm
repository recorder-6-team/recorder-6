object dlgNewPlaceCard: TdlgNewPlaceCard
  Left = 511
  Top = 252
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Add or Edit Recording Card'
  ClientHeight = 425
  ClientWidth = 392
  Color = clBtnFace
  Constraints.MinHeight = 457
  Constraints.MinWidth = 401
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    392
    425)
  PixelsPerInch = 96
  TextHeight = 13
  object bbCancel: TImageListButton
    Left = 312
    Top = 395
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
    OnClick = bbCancelClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbOK: TImageListButton
    Left = 232
    Top = 395
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = bbOKClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object gbStep1: TGroupBox
    Left = 4
    Top = 1
    Width = 384
    Height = 70
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    DesignSize = (
      384
      70)
    object lblistName: TLabel
      Left = 8
      Top = 13
      Width = 297
      Height = 13
      Caption = 
        '1. Enter name for a new list, or choose an existing list to modi' +
        'fy:'
    end
    object Label3: TLabel
      Left = 24
      Top = 40
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object bbListRemove: TImageListButton
      Left = 288
      Top = 34
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Remove'
      TabOrder = 1
      OnClick = bbListRemoveClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 4
    end
    object cmbListNames: TComboBox
      Left = 60
      Top = 36
      Width = 221
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnChange = cmbListNamesChange
    end
  end
  object gbStep2: TGroupBox
    Left = 4
    Top = 76
    Width = 384
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    DesignSize = (
      384
      89)
    object lblSpeciesList: TLabel
      Left = 8
      Top = 12
      Width = 300
      Height = 13
      Caption = 
        '2. Choose a species list from the taxon dictionary or a rucksack' +
        ':'
    end
    object rbTaxonDictionary: TRadioButton
      Left = 24
      Top = 36
      Width = 109
      Height = 17
      Caption = 'Taxon dictionary:'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbListClick
    end
    object rbRucksack: TRadioButton
      Left = 24
      Top = 62
      Width = 85
      Height = 17
      Caption = 'Rucksack:'
      TabOrder = 2
      OnClick = rbListClick
    end
    object cmbRucksack: TComboBox
      Left = 132
      Top = 58
      Width = 232
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      ItemHeight = 13
      Sorted = True
      TabOrder = 3
      OnChange = cmbRucksackChange
      Items.Strings = (
        'Rusksack')
    end
    object cmbTaxonDictionary: TDBListCombo
      Left = 132
      Top = 34
      Width = 232
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      OnChange = cmbTaxonDictionaryChange
      KeyValue = '0'
      ListField = 'DisplayField'
      KeyField = 'KeyField'
      Datasource = dmTaxonDictBrowser.dsLocalLists
      Active = False
      EmptyItem = False
      ReadOnly = False
    end
  end
  object gbAddin: TGroupBox
    Left = 4
    Top = 170
    Width = 385
    Height = 49
    TabOrder = 3
    Visible = False
    object lblAddin: TLabel
      Left = 16
      Top = 18
      Width = 146
      Height = 13
      Caption = '3. Select the card header type:'
    end
    object cmbHeaderTypes: TComboBox
      Left = 176
      Top = 16
      Width = 201
      Height = 21
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object gbStep3: TGroupBox
    Left = 4
    Top = 170
    Width = 384
    Height = 221
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    DesignSize = (
      384
      221)
    object lblAddColumns: TLabel
      Left = 8
      Top = 13
      Width = 365
      Height = 13
      Caption = 
        '3. Choose any additional columns you wish to show alongside the ' +
        'species list:'
    end
    object pnlButtons: TPanel
      Left = 180
      Top = 55
      Width = 23
      Height = 145
      BevelOuter = bvNone
      TabOrder = 3
      object bbAdd: TImageListButton
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Hint = 'Add column to selection'
        TabOrder = 0
        OnClick = bbAddClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 11
      end
      object bbAddAll: TImageListButton
        Left = 0
        Top = 22
        Width = 23
        Height = 22
        Hint = 'Add all columns to selection'
        TabOrder = 1
        OnClick = bbAddAllClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 9
      end
      object bbRemove: TImageListButton
        Left = 0
        Top = 50
        Width = 23
        Height = 22
        Hint = 'Remove column from selection'
        TabOrder = 2
        OnClick = bbRemoveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 10
      end
      object bbClearAll: TImageListButton
        Left = 0
        Top = 72
        Width = 23
        Height = 22
        Hint = 'Clear selection'
        TabOrder = 3
        OnClick = bbClearAllClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 8
      end
      object bbMoveUp: TImageListButton
        Left = 0
        Top = 100
        Width = 23
        Height = 22
        Hint = 'Move column in selection up'
        TabOrder = 4
        OnClick = bbMoveUpClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 12
      end
      object bbMoveDown: TImageListButton
        Left = 0
        Top = 122
        Width = 23
        Height = 22
        Hint = 'Move column in selection down'
        TabOrder = 5
        OnClick = bbMoveDownClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 13
      end
    end
    object lbSelected: TListBox
      Left = 208
      Top = 32
      Width = 169
      Height = 179
      Anchors = [akLeft, akTop, akBottom]
      DragMode = dmAutomatic
      ItemHeight = 13
      PopupMenu = pmColNames
      TabOrder = 1
      OnClick = ColumnListsClick
      OnDblClick = bbRemoveClick
      OnMouseMove = ListBoxMouseMove
    end
    object pcColumns: TPageControl
      Left = 8
      Top = 32
      Width = 165
      Height = 179
      ActivePage = tsMeasurements
      Anchors = [akLeft, akTop, akBottom]
      TabOrder = 0
      OnChange = ColumnListsClick
      object tsStandard: TTabSheet
        Caption = 'Standard'
        object lbAvailable: TListBox
          Left = 0
          Top = 0
          Width = 157
          Height = 151
          Align = alClient
          DragMode = dmAutomatic
          ItemHeight = 13
          Items.Strings = (
            'Code Number'
            'Comment'
            'Common Name'
            'Count'
            'Date of Determination'
            'Determiner'
            'Provenance'
            'Record Type'
            'Substrate')
          Sorted = True
          TabOrder = 0
          OnClick = ColumnListsClick
          OnDblClick = bbAddClick
        end
      end
      object tsMeasurements: TTabSheet
        Caption = 'Measurements'
        ImageIndex = 1
        object lbMeasurements: TListBox
          Left = 0
          Top = 0
          Width = 157
          Height = 151
          Align = alClient
          DragMode = dmAutomatic
          ItemHeight = 13
          Sorted = True
          TabOrder = 0
          OnClick = ColumnListsClick
          OnDblClick = bbAddClick
          OnMouseMove = ListBoxMouseMove
        end
      end
    end
    object eRename: TEdit
      Left = 210
      Top = 186
      Width = 165
      Height = 22
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 2
      Text = 'eRename'
      Visible = False
      OnExit = eRenameExit
      OnKeyPress = eRenameKeyPress
    end
  end
  object pmColNames: TPopupMenu
    Left = 328
    Top = 206
    object pmColNamesRename: TMenuItem
      Caption = 'Rename'
      OnClick = pmColNamesRenameClick
    end
    object pmColNamesReset: TMenuItem
      Caption = 'Reset'
      OnClick = pmColNamesResetClick
    end
  end
end
