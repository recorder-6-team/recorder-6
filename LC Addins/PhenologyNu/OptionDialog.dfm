object FormpOption: TFormpOption
  Left = 1370
  Top = 166
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 236
  ClientWidth = 216
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 144
    Top = 8
    Width = 41
    Height = 13
    Caption = 'Year To '
  end
  object Shape: TShape
    Left = 43
    Top = 144
    Width = 126
    Height = 33
  end
  object Label2: TLabel
    Left = 24
    Top = 8
    Width = 59
    Height = 13
    Caption = 'Years  From '
  end
  object Edit1: TEdit
    Left = 16
    Top = 24
    Width = 73
    Height = 21
    MaxLength = 4
    TabOrder = 0
    OnKeyPress = Edit1KeyPress
  end
  object Edit2: TEdit
    Left = 128
    Top = 24
    Width = 73
    Height = 21
    MaxLength = 4
    TabOrder = 1
    OnKeyPress = Edit2KeyPress
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 56
    Width = 193
    Height = 81
    Caption = 'Base Output On'
    ItemIndex = 0
    Items.Strings = (
      'Taxon (Phenology)'
      'Survey (Record Distibution)')
    TabOrder = 2
  end
  object cbApply: TCheckBox
    Left = 56
    Top = 152
    Width = 105
    Height = 17
    Caption = 'Apply options'
    TabOrder = 3
  end
  object bOk: TButton
    Left = 80
    Top = 192
    Width = 65
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 4
  end
end
