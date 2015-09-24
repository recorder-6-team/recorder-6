object BaseFrame: TBaseFrame
  Left = 0
  Top = 0
  Width = 328
  Height = 312
  Color = clWhite
  ParentColor = False
  TabOrder = 0
  object pnlTitle: TPanel
    Left = 0
    Top = 0
    Width = 328
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
end
