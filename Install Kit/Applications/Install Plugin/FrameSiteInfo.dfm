inherited fraSiteInfo: TfraSiteInfo
  object Label2: TLabel
    Left = 20
    Top = 48
    Width = 284
    Height = 64
    Caption = 
      'Enter a Site ID for the application.  This will uniquely identif' +
      'y all recordings made at this site.  You must also enter the sup' +
      'plied verification key to ensure your site ID is correct.'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 20
    Top = 124
    Width = 273
    Height = 48
    Caption = 
      'The Site ID must be 8 characters in length and can only consist ' +
      'of the letters A-Z and the numbers 0-9.'
    WordWrap = True
  end
  object lblSiteID: TLabel
    Left = 64
    Top = 188
    Width = 43
    Height = 16
    Caption = 'Site ID:'
  end
  object Label5: TLabel
    Left = 64
    Top = 220
    Width = 95
    Height = 16
    Caption = 'Verification Key:'
  end
  object Label4: TLabel
    Left = 20
    Top = 20
    Width = 80
    Height = 16
    Caption = 'Site Settings'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object eSiteID: TEdit
    Left = 168
    Top = 184
    Width = 97
    Height = 24
    CharCase = ecUpperCase
    MaxLength = 8
    TabOrder = 0
    OnChange = eSiteIDChange
    OnExit = InfoCheck
    OnKeyPress = eSiteIDKeyPress
  end
  object eVerificationKey: TEdit
    Left = 168
    Top = 216
    Width = 57
    Height = 24
    MaxLength = 4
    TabOrder = 1
    OnChange = eVerificationKeyChange
    OnExit = InfoCheck
  end
end
