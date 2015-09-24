object BasePage: TBasePage
  Left = 0
  Top = 0
  Width = 336
  Height = 390
  Color = clWhite
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  object pnlTitle: TPanel
    Left = 0
    Top = 0
    Width = 336
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object lblTitle: TLabel
      Left = 12
      Top = 12
      Width = 50
      Height = 19
      Caption = '<Title>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object pnlContinue: TPanel
    Left = 0
    Top = 366
    Width = 336
    Height = 24
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object lblClickNext: TLabel
      Left = 12
      Top = 4
      Width = 118
      Height = 14
      Caption = 'Click Next to continue...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsItalic]
      ParentFont = False
    end
  end
end
