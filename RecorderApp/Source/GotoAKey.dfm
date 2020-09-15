object dlgGotoKey: TdlgGotoKey
  Left = 578
  Top = 328
  Width = 257
  Height = 231
  Caption = 'Goto Key'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
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
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 241
    Height = 153
    Shape = bsFrame
  end
  object edKey: TEdit
    Left = 8
    Top = 32
    Width = 161
    Height = 21
    TabOrder = 0
    Text = 'edKey'
    OnDblClick = edKeyDblClick
  end
  object cmbTable: TComboBox
    Left = 8
    Top = 88
    Width = 169
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Text = 'cmbTable'
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
  object cbExpand: TCheckBox
    Left = 8
    Top = 128
    Width = 113
    Height = 17
    Caption = 'Expand'
    TabOrder = 2
    OnClick = cbExpandClick
  end
  object btnAction: TButton
    Left = 116
    Top = 168
    Width = 57
    Height = 25
    Caption = 'OK'
    TabOrder = 3
    OnClick = btnActionClick
  end
  object bbCancel: TImageListButton
    Left = 180
    Top = 168
    Width = 57
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    OnClick = bbCancelClick
    ImageIndex = 1
  end
end
