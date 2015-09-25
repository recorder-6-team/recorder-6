inherited fraUninstallSelect: TfraUninstallSelect
  object lblTitle: TLabel
    Left = 20
    Top = 20
    Width = 105
    Height = 16
    Caption = 'Uninstall options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblInformation: TLabel
    Left = 20
    Top = 56
    Width = 301
    Height = 39
    Caption = 
      'All Recorder files and settings will be automatically removed. T' +
      'he items listed below were also installed with Recorder. Select ' +
      'those you want to completely uninstall from your computer.'
    WordWrap = True
  end
  object chkSQLInstance: TCheckBox
    Left = 28
    Top = 144
    Width = 261
    Height = 17
    Caption = 'SQL Express instance.'
    TabOrder = 0
    OnClick = chkSQLInstanceClick
  end
end
