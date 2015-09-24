inherited fraSpatialRef: TfraSpatialRef
  object Label3: TLabel
    Left = 20
    Top = 48
    Width = 296
    Height = 64
    Caption = 
      'Please select the Spatial Reference System you would like to use' +
      ' as your default display system for the application.  This can b' +
      'e changed at a later date via the Options dialog off the Tools m' +
      'enu.'
    WordWrap = True
  end
  object lblSpatialRef: TLabel
    Left = 36
    Top = 124
    Width = 162
    Height = 16
    Caption = 'Spatial Reference Ssytems:'
  end
  object Label4: TLabel
    Left = 20
    Top = 20
    Width = 164
    Height = 16
    Caption = 'Spatial Reference System'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object cblbSpatialRefs: TCheckListBox
    Left = 36
    Top = 144
    Width = 257
    Height = 132
    OnClickCheck = cblbSpatialRefsClickCheck
    IntegralHeight = True
    ItemHeight = 16
    Items.Strings = (
      'Ordnance Survey - UK Grid'
      'Ordnance Survey - Irish Grid'
      'Latitude and Longitude'
      'UTM')
    TabOrder = 0
  end
end
