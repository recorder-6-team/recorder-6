inherited fraNewOrExisting: TfraNewOrExisting
  object Label4: TLabel
    Left = 20
    Top = 20
    Width = 137
    Height = 16
    Caption = 'SQL Server Selection'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 24
    Top = 48
    Width = 281
    Height = 193
    Caption = 'Select Database Server Install'
    TabOrder = 0
    object rbInstallNew: TRadioButton
      Left = 16
      Top = 32
      Width = 241
      Height = 41
      Caption = 'Install a new instance of MSDE onto this machine'
      TabOrder = 0
      WordWrap = True
      OnClick = rbInstallOptionClick
    end
    object rbInstallExisting: TRadioButton
      Left = 16
      Top = 80
      Width = 249
      Height = 33
      Caption = 'Use the following existing SQL Server or MSDE instance'
      TabOrder = 1
      WordWrap = True
      OnClick = rbInstallOptionClick
    end
    object cmbServerName: TComboBox
      Left = 32
      Top = 128
      Width = 217
      Height = 24
      CharCase = ecUpperCase
      Enabled = False
      ItemHeight = 16
      TabOrder = 2
      OnChange = cmbServerNameChange
    end
  end
end
