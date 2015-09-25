object Project3RuckForm1: TProject3RuckForm1
  Left = 860
  Top = 237
  Width = 378
  Height = 535
  ActiveControl = CheckBox1
  Caption = 'Generate Rucksack  V3.3a'
  Color = clCream
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = ActiveFormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 24
    Top = 360
    Width = 321
    Height = 113
    AutoSize = False
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object Label1: TLabel
    Left = 19
    Top = 8
    Width = 341
    Height = 49
    AutoSize = False
    WordWrap = True
  end
  object Label3: TLabel
    Left = 24
    Top = 256
    Width = 108
    Height = 13
    Caption = 'Taxon  - column name '
    WordWrap = True
  end
  object Label4: TLabel
    Left = 16
    Top = 480
    Width = 136
    Height = 13
    Alignment = taCenter
    Caption = 'Click on OK to continue'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 208
    Top = 160
    Width = 136
    Height = 65
    Caption = 
      'Determines the order in which taxa appear in the Rucksack and th' +
      'erefore the default order in Recording cards. '
    WordWrap = True
  end
  object Label6: TLabel
    Left = 24
    Top = 280
    Width = 108
    Height = 13
    Caption = 'Search - column Name'
  end
  object Shape1: TShape
    Left = 0
    Top = 352
    Width = 369
    Height = 1
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 64
    Width = 265
    Height = 17
    Caption = 'Allow allocation based on synonyms'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object CheckBox2: TCheckBox
    Left = 24
    Top = 80
    Width = 265
    Height = 25
    Caption = 'Allow allocation using Ulster Museum Marine List '
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object CheckBox3: TCheckBox
    Left = 24
    Top = 104
    Width = 265
    Height = 25
    Caption = 'Allow allocation using List of Additional Names'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object CheckBox4: TCheckBox
    Left = 24
    Top = 128
    Width = 265
    Height = 25
    Caption = 'Allow allocation using Recorder 3  List'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object Edit1: TEdit
    Left = 152
    Top = 256
    Width = 185
    Height = 21
    TabOrder = 5
    Text = 'TaxonName'
  end
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 152
    Width = 177
    Height = 97
    Caption = 'Output Order'
    ItemIndex = 0
    Items.Strings = (
      'Input File Order'
      'Scientific Name'
      'TaxonGroup/Scientific Name'
      'Recorder Taxonomic Order'
      'Search Code')
    TabOrder = 4
  end
  object Edit2: TEdit
    Left = 152
    Top = 280
    Width = 185
    Height = 21
    TabOrder = 6
  end
  object CheckBox5: TCheckBox
    Left = 192
    Top = 328
    Width = 145
    Height = 17
    Caption = 'Transfer other keys (.ruk)'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object CheckBox6: TCheckBox
    Left = 24
    Top = 328
    Width = 161
    Height = 17
    Caption = 'Transfer Search Codes (.ruk)'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
  object CheckBox7: TCheckBox
    Left = 24
    Top = 304
    Width = 273
    Height = 17
    Caption = 'Retain original keys (applies to .ruk and .crd files)'
    TabOrder = 9
  end
  object OpenDialog1: TOpenDialog
    Filter = '*.txt'
    Left = 16
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Filter = '*.ruk'
    Left = 48
    Top = 8
  end
end
