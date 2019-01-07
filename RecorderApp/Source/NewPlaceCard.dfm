object dlgNewPlaceCard: TdlgNewPlaceCard
  Left = 718
  Top = 215
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Add or Edit Recording Card'
  ClientHeight = 605
  ClientWidth = 391
  Color = clBtnFace
  Constraints.MinHeight = 452
  Constraints.MinWidth = 399
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
    391
    605)
  PixelsPerInch = 96
  TextHeight = 13
  object bbCancel: TImageListButton
    Left = 311
    Top = 575
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    OnClick = bbCancelClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbOK: TImageListButton
    Left = 231
    Top = 575
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = bbOKClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object gbStep2: TGroupBox
    Left = 4
    Top = 76
    Width = 383
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    DesignSize = (
      383
      89)
    object lblSpeciesList: TLabel
      Left = 8
      Top = 12
      Width = 288
      Height = 13
      Caption = 'Choose a species list from the taxon dictionary or a rucksack:'
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
      Width = 231
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
      Width = 231
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
    Top = 282
    Width = 385
    Height = 49
    TabOrder = 2
    Visible = False
    object lblAddin: TLabel
      Left = 16
      Top = 18
      Width = 134
      Height = 13
      Caption = 'Select the card header type:'
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
    Top = 288
    Width = 383
    Height = 281
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    DesignSize = (
      383
      281)
    object lblAddColumns: TLabel
      Left = 8
      Top = 13
      Width = 353
      Height = 13
      Caption = 
        'Choose any additional columns you wish to show alongside the spe' +
        'cies list:'
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
      Height = 239
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
      Height = 239
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
          Height = 211
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
          Height = 211
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
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 2
      Text = 'eRename'
      Visible = False
      OnExit = eRenameExit
      OnKeyPress = eRenameKeyPress
    end
  end
  object gbStep2a: TGroupBox
    Left = 4
    Top = 170
    Width = 383
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    DesignSize = (
      383
      49)
    object Label2: TLabel
      Left = 8
      Top = 18
      Width = 73
      Height = 13
      Caption = 'Default Survey '
    end
    object cmbSurveyList: TDBListCombo
      Left = 132
      Top = 12
      Width = 230
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      ItemHeight = 13
      TabOrder = 0
      KeyValue = '0'
      ListField = 'Item_Name'
      KeyField = 'Survey_key'
      Datasource = dmFormActions.dsSurvey
      Active = False
      EmptyItem = False
      ReadOnly = False
    end
    object cbDefaultSurvey: TCheckBox
      Left = 104
      Top = 16
      Width = 17
      Height = 17
      TabOrder = 1
      OnClick = cbDefaultSurveyClick
    end
  end
  object gbStep1: TGroupBox
    Left = 4
    Top = 1
    Width = 383
    Height = 70
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    DesignSize = (
      383
      70)
    object lblistName: TLabel
      Left = 8
      Top = 13
      Width = 285
      Height = 13
      Caption = 'Enter name for a new list, or choose an existing list to modify:'
    end
    object Label3: TLabel
      Left = 24
      Top = 40
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object bbListRemove: TImageListButton
      Left = 287
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
      Width = 220
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnChange = cmbListNamesChange
    end
  end
  object gbStep2b: TGroupBox
    Left = 4
    Top = 220
    Width = 383
    Height = 69
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    DesignSize = (
      383
      69)
    object Label1: TLabel
      Left = 8
      Top = 18
      Width = 66
      Height = 13
      Caption = 'Default Group'
    end
    object Label4: TLabel
      Left = 8
      Top = 40
      Width = 369
      Height = 22
      Caption = 
        'During data input this option will warn if an attempt is made to' +
        ' add a taxon not in the default taxon group.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object cmbTaxonGroupList: TDBListCombo
      Left = 132
      Top = 12
      Width = 230
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      ItemHeight = 13
      TabOrder = 0
      KeyValue = '0'
      ListField = 'Taxon_Group_Name'
      KeyField = 'Taxon_Group_Key'
      Datasource = dmFormActions.dsTaxonGroup
      Active = False
      EmptyItem = False
      ReadOnly = False
    end
    object cbTaxonGroup: TCheckBox
      Left = 104
      Top = 16
      Width = 17
      Height = 17
      TabOrder = 1
      OnClick = cbTaxonGroupClick
    end
  end
  object pmColNames: TPopupMenu
    Left = 288
    Top = 350
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
