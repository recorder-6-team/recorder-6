inherited fraDatabaseServerSelect: TfraDatabaseServerSelect
  object lblInfo: TLabel [0]
    Left = 12
    Top = 49
    Width = 309
    Height = 40
    Caption = 
      'Please select the SQL Server or SQL Express instance on which th' +
      'e NBN Database was installed:'
    WordWrap = True
  end
  object lblInstanceName: TLabel [1]
    Left = 24
    Top = 106
    Width = 73
    Height = 13
    Caption = 'Instance name:'
  end
  object lblWarning: TLabel [2]
    Left = 26
    Top = 130
    Width = 265
    Height = 13
    Caption = 'Warning: This instance uses Trusted Connection ONLY.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object lblInfoNoDetect: TLabel [3]
    Left = 12
    Top = 48
    Width = 309
    Height = 41
    Caption = 
      'Setup is unable to detect SQL Server or SQL Express instances in' +
      'stalled on your network. Please enter the instance on which the ' +
      'NBN Database was installed:'
    WordWrap = True
  end
  object lblDropDownInfo: TLabel [4]
    Left = 24
    Top = 150
    Width = 297
    Height = 67
    AutoSize = False
    Caption = 
      'The drop-down box above may not be an exhaustive list of the SQL' +
      ' Server instances available. If the SQL Server instance you wish' +
      ' to use is not displayed, please click in the instance name box ' +
      'and type in the full name of the instance you wish to use.'
    WordWrap = True
  end
  inherited pnlTitle: TPanel
    TabOrder = 2
    inherited lblTitle: TLabel
      Width = 201
      Caption = 'Database Server Selection'
    end
  end
  inherited pnlContinue: TPanel
    TabOrder = 3
  end
  object cmbInstances: TComboBox
    Left = 104
    Top = 102
    Width = 205
    Height = 21
    BevelKind = bkFlat
    CharCase = ecUpperCase
    Ctl3D = True
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 0
    OnChange = CheckChange
  end
  object eInstance: TEdit
    Left = 104
    Top = 103
    Width = 205
    Height = 19
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 1
    Visible = False
    OnChange = CheckChange
  end
end
