object LicenceKey: TLicenceKey
  Left = 536
  Top = 323
  Width = 284
  Height = 146
  Caption = 'Licence Key Generator '
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
    Left = 24
    Top = 24
    Width = 70
    Height = 13
    Caption = 'Financial Year '
  end
  object LicenceKey: TLabel
    Left = 112
    Top = 56
    Width = 81
    Height = 17
    AutoSize = False
  end
  object Label2: TLabel
    Left = 24
    Top = 48
    Width = 73
    Height = 25
    AutoSize = False
    Caption = 'Key'
  end
  object edYear: TEdit
    Left = 112
    Top = 24
    Width = 81
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 216
    Top = 24
    Width = 57
    Height = 25
    Caption = 'Generate'
    TabOrder = 1
    OnClick = Button1Click
  end
end
