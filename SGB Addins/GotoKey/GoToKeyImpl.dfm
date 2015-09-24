object GoToKeyX: TGoToKeyX
  Left = 0
  Top = 0
  Width = 197
  Height = 145
  Caption = 'GoToKeyX'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TBevel
    Left = 4
    Top = 8
    Width = 181
    Height = 105
    Shape = bsFrame
  end
  object labelKey: TLabel
    Left = 10
    Top = 16
    Width = 21
    Height = 13
    Caption = 'Key:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object labelType: TLabel
    Left = 8
    Top = 61
    Width = 63
    Height = 13
    Caption = 'Type of data:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object comboType: TComboBox
    Left = 10
    Top = 80
    Width = 169
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Text = 'comboType'
    OnClick = comboTypeClick
    Items.Strings = (
      'Survey'
      'Survey event'
      'Sample'
      'Taxon occurrence'
      'Biotope occurrence'
      'Location'
      'Individual'
      'Organisation'
      'Document'
      'Taxon'
      'Biotope'
      'Administrative area')
  end
  object editKey: TEdit
    Left = 8
    Top = 34
    Width = 169
    Height = 21
    CharCase = ecUpperCase
    MaxLength = 16
    TabOrder = 1
    Text = 'EDITKEY'
  end
end
