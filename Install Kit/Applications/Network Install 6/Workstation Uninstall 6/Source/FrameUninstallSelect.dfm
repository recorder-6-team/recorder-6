inherited fraUninstallSelect: TfraUninstallSelect
  object Label1: TLabel
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
  object lblSysCompInfo: TLabel
    Left = 46
    Top = 174
    Width = 247
    Height = 32
    Caption = '(Include MDAC 2.6 and 2.7, MS Jet engine 3.51 and 4.0, XML 3.0)'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 20
    Top = 56
    Width = 300
    Height = 64
    Caption = 
      'All Recorder files and settings will be automatically removed. Th' +
      'e items listed below were also installed with Recorder. Select t' +
      'hose you want to completely uninstall from your computer.'
    WordWrap = True
  end
  object chkSysComp: TCheckBox
    Left = 28
    Top = 156
    Width = 233
    Height = 17
    Caption = 'System components for Recorder.'
    TabOrder = 0
    WordWrap = True
    OnClick = chkSysCompClick
  end
end
