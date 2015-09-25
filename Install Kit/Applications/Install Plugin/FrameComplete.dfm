inherited fraComplete: TfraComplete
  object lblRebootInfo: TLabel
    Left = 20
    Top = 48
    Width = 284
    Height = 64
    Caption = 
      'The installation of the Server version of Recorder is complete. ' +
      ' For the database to be installed on the selected SQL Server, yo' +
      'u will need to reboot your machine.'
    Visible = False
    WordWrap = True
  end
  object Label1: TLabel
    Left = 20
    Top = 20
    Width = 248
    Height = 16
    Caption = 'Recorder Installation Wizard complete.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblProceed: TLabel
    Left = 20
    Top = 188
    Width = 154
    Height = 16
    Caption = 'Click Proceed to continue.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsItalic]
    ParentFont = False
  end
  object lblSystemCompInfo: TLabel
    Left = 20
    Top = 48
    Width = 248
    Height = 48
    Caption = 
      'Additional system components need to be installed to complete th' +
      'e installation of the software. '
    WordWrap = True
  end
  object lblCDInfo: TLabel
    Left = 20
    Top = 124
    Width = 273
    Height = 32
    Caption = 
      'Please note that if you are installing from a CD, make sure to l' +
      'eave it in the CD drive.'
    WordWrap = True
  end
end
