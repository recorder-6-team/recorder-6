inherited fraSelectExistingServer: TfraSelectExistingServer
  object lblInstanceName: TLabel [0]
    Left = 12
    Top = 80
    Width = 73
    Height = 13
    Caption = 'Instance name:'
  end
  object lblWarning: TLabel [1]
    Left = 14
    Top = 100
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
  object lblDropDownInfo: TLabel [2]
    Left = 12
    Top = 117
    Width = 317
    Height = 67
    AutoSize = False
    Caption = 
      'The drop-down box above may not be an exhaustive list of the SQL' +
      ' Server instances available. If the SQL Server instance you wish' +
      ' to use is not displayed, please click in the instance name box ' +
      'and type in the full name of the instance you wish to use.'
    WordWrap = True
  end
  object lblExplanation: TLabel [3]
    Left = 12
    Top = 48
    Width = 253
    Height = 26
    Caption = 
      'Select the existing SQL Server instance to attach the database t' +
      'o:'
    WordWrap = True
  end
  object Label1: TLabel [4]
    Left = 12
    Top = 176
    Width = 289
    Height = 39
    Caption = 
      'Please also specify how you want to connect to SQL Server. Unles' +
      's you specified a password for the sa account during installatio' +
      'n, you should select the first option.'
    WordWrap = True
  end
  inherited pnlTitle: TPanel
    inherited lblTitle: TLabel
      Width = 204
      Caption = 'Select Existing SQL Server'
    end
  end
  inherited pnlContinue: TPanel
    TabOrder = 2
  end
  object cmbInstances: TComboBox
    Left = 92
    Top = 76
    Width = 197
    Height = 21
    BevelKind = bkFlat
    CharCase = ecUpperCase
    Ctl3D = True
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 1
    OnChange = cmbInstancesChange
  end
  object gbLogin: TGroupBox
    Left = 8
    Top = 224
    Width = 321
    Height = 129
    Caption = ' SQL Server Login '
    TabOrder = 3
    object lblUsername: TLabel
      Left = 40
      Top = 68
      Width = 51
      Height = 13
      Caption = 'Username:'
    end
    object lblPassword: TLabel
      Left = 40
      Top = 96
      Width = 49
      Height = 13
      Caption = 'Password:'
    end
    object rbTrustedLogin: TRadioButton
      Left = 8
      Top = 12
      Width = 297
      Height = 31
      Caption = 'Use my Windows Account to login'
      Checked = True
      TabOrder = 0
      TabStop = True
      WordWrap = True
    end
    object rbSQLLogin: TRadioButton
      Left = 8
      Top = 38
      Width = 305
      Height = 31
      Caption = 'Use the following SQL Server system administrator login:'
      TabOrder = 1
      WordWrap = True
    end
    object eUsername: TEdit
      Left = 112
      Top = 64
      Width = 121
      Height = 21
      TabOrder = 2
      Text = 'sa'
    end
    object ePassword: TEdit
      Left = 112
      Top = 92
      Width = 121
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
  end
end
