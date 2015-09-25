inherited fraCutOffYear: TfraCutOffYear
  object Label4: TLabel
    Left = 20
    Top = 48
    Width = 283
    Height = 80
    Caption = 
      'Please enter a cut-off year for the display of hollow/solid symb' +
      'ols on the map.  This year can be changed at a later date via th' +
      'e Map Options dialog on the Map menu, but cannot be set to a fut' +
      'ure date.'
    WordWrap = True
  end
  object lblCutOffYear: TLabel
    Left = 88
    Top = 148
    Width = 70
    Height = 16
    Caption = 'Cut-off year:'
  end
  object Label1: TLabel
    Left = 20
    Top = 20
    Width = 186
    Height = 16
    Caption = 'Cut-Off Year for Map Symbols'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object eCutOffYear: TEdit
    Left = 164
    Top = 144
    Width = 49
    Height = 24
    MaxLength = 4
    TabOrder = 0
    OnChange = eCutOffYearChange
    OnKeyPress = eCutOffYearKeyPress
  end
end
