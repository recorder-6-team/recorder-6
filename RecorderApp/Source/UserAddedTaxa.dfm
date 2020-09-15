object dlgUserAddedTaxa: TdlgUserAddedTaxa
  Left = 461
  Top = 225
  BorderStyle = bsDialog
  Caption = 'Manage User Added Taxa'
  ClientHeight = 572
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object lblListName: TLabel
    Left = 8
    Top = 8
    Width = 19
    Height = 13
    Caption = 'List:'
  end
  object rgProcess: TRadioGroup
    Left = 25
    Top = 512
    Width = 160
    Height = 49
    Caption = 'Process'
    Color = clBtnFace
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Selected'
      'All')
    ParentColor = False
    TabOrder = 0
  end
  object btnAction: TButton
    Left = 264
    Top = 528
    Width = 49
    Height = 25
    Caption = 'Action'
    TabOrder = 1
    OnClick = btnActionClick
  end
  object btnCancel: TButton
    Left = 320
    Top = 528
    Width = 49
    Height = 25
    Caption = 'Exit'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object cbUsedOnly: TCheckBox
    Left = 296
    Top = 0
    Width = 65
    Height = 41
    Caption = 'Show used only'
    Checked = True
    State = cbChecked
    TabOrder = 3
    WordWrap = True
    OnClick = cbUsedOnlyClick
  end
  object cmbLists: TComboBox
    Left = 32
    Top = 8
    Width = 265
    Height = 21
    ItemHeight = 13
    TabOrder = 4
    Text = 'cmbLists'
    OnClick = cmbListsClick
  end
  object lstTaxa: TListBox
    Left = 8
    Top = 48
    Width = 361
    Height = 449
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 5
  end
  object cbAuto: TCheckBox
    Left = 200
    Top = 524
    Width = 41
    Height = 33
    Caption = 'Auto'
    TabOrder = 6
  end
end
