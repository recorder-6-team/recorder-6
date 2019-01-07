object GotoKey2X: TGotoKey2X
  Left = 865
  Top = 362
  Width = 240
  Height = 191
  Caption = 'Go to key '
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 64
    Width = 27
    Height = 13
    Caption = 'Table'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 18
    Height = 13
    Caption = 'Key'
  end
  object Edit1: TEdit
    Left = 8
    Top = 32
    Width = 153
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 88
    Width = 169
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Text = 'ComboBox1'
    OnClick = ComboBox1Click
    Items.Strings = (
      'Survey_Key'
      'Survey_Event_Key'
      'Sample_Key'
      'Taxon_Occurrence_Key'
      'Biotope_Occurrence_Key'
      'Location_Key'
      'Feature_Key'
      'Name_Key'
      'Source_Key (Documents)'
      'Taxon_Version_Key'
      'Taxon_List_item_Key '
      'Biotope_List_Item_Key'
      'Admini_Area_Key'
      'External_Ref')
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 128
    Width = 113
    Height = 17
    Caption = 'Expand'
    TabOrder = 2
    OnClick = CheckBox1Click
  end
end
