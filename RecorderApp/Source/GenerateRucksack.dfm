object dlgGenerateRucksack: TdlgGenerateRucksack
  Left = 399
  Top = 262
  BorderStyle = bsDialog
  Caption = 'Generate Rucksack'
  ClientHeight = 239
  ClientWidth = 586
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblTaxonNameColumn: TLabel
    Left = 16
    Top = 8
    Width = 70
    Height = 26
    Caption = 'Taxon Name - column name  '
    WordWrap = True
  end
  object lblExplainSort: TLabel
    Left = 16
    Top = 112
    Width = 146
    Height = 52
    Caption = 
      'Determines the order in which taxa appear in the Rucksack and th' +
      'erefore the default order in Recording cards. '
    WordWrap = True
  end
  object lblSerachColumn: TLabel
    Left = 312
    Top = 8
    Width = 80
    Height = 26
    Caption = 'Search - column Name'
    WordWrap = True
  end
  object lblinputfile: TLabel
    Left = 16
    Top = 48
    Width = 118
    Height = 13
    Caption = 'Input File (.csv/,ruk/.crd)'
  end
  object lblOutputFile: TLabel
    Left = 16
    Top = 67
    Width = 51
    Height = 13
    Caption = 'Output File'
  end
  object edInputColumnName: TEdit
    Left = 104
    Top = 8
    Width = 177
    Height = 21
    TabOrder = 0
    Text = 'TaxonName'
    OnChange = edInputColumnNameChange
  end
  object rgSort: TRadioGroup
    Left = 168
    Top = 104
    Width = 185
    Height = 97
    Caption = 'Output Order'
    ItemIndex = 0
    Items.Strings = (
      'Input File Order'
      'Scientific Name'
      'TaxonGroup/Scientific Name'
      'Recorder Taxonomic Order'
      'Search Code')
    TabOrder = 1
  end
  object edSearchColumnName: TEdit
    Left = 400
    Top = 8
    Width = 177
    Height = 21
    TabOrder = 2
    OnChange = edSearchColumnNameChange
  end
  object edInputFile: TEdit
    Left = 152
    Top = 40
    Width = 393
    Height = 21
    Enabled = False
    TabOrder = 3
  end
  object edOutputFile: TEdit
    Left = 152
    Top = 64
    Width = 393
    Height = 21
    Enabled = False
    TabOrder = 4
  end
  object bbBrowser: TButton
    Left = 556
    Top = 40
    Width = 23
    Height = 21
    Hint = 'Select another folder'
    Caption = '...'
    TabOrder = 5
    OnClick = bbBrowserClick
  end
  object Button1: TButton
    Left = 556
    Top = 64
    Width = 23
    Height = 21
    Hint = 'Select another folder'
    Caption = '...'
    TabOrder = 6
    OnClick = Button1Click
  end
  object btnAction: TButton
    Left = 452
    Top = 208
    Width = 57
    Height = 25
    Caption = 'OK'
    TabOrder = 7
    OnClick = btnActionClick
  end
  object bbCancel: TImageListButton
    Left = 516
    Top = 208
    Width = 57
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
    OnClick = bbCancelClick
    ImageIndex = 1
  end
  object cbTransfer: TCheckBox
    Left = 368
    Top = 112
    Width = 169
    Height = 17
    Caption = 'Transfer Tli keys '
    TabOrder = 9
    OnClick = cbTransferClick
  end
  object cbRecommended: TCheckBox
    Left = 368
    Top = 136
    Width = 169
    Height = 25
    Caption = 'Use Recommended Taxa'
    TabOrder = 10
    OnClick = cbRecommendedClick
  end
  object OpenDialog1: TOpenDialog
    Filter = '*.txt'
    Left = 552
    Top = 104
  end
  object SaveDialog1: TSaveDialog
    Filter = '*.ruk'
    Left = 552
    Top = 136
  end
end
