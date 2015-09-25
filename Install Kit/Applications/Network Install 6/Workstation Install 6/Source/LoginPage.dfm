inherited fraLogin: TfraLogin
  object lblMigrateInfo1: TLabel [0]
    Left = 12
    Top = 44
    Width = 302
    Height = 26
    Caption = 
      'To transfer data to the SQL Server you must supply login details' +
      ' that give system administrator privileges on the server. '
    WordWrap = True
  end
  object lblMigrateInfo2: TLabel [1]
    Left = 12
    Top = 128
    Width = 294
    Height = 26
    Caption = 
      'Note that this tool must be run on the Server itself so that SQL' +
      ' Server can find the Access data files.'
    WordWrap = True
  end
  object lblCommonInfo: TLabel [2]
    Left = 12
    Top = 80
    Width = 294
    Height = 39
    Caption = 
      'The default details of "Username = sa" and no password are autom' +
      'atically created when MSDE is installed.  You may have chosen to' +
      ' change the username and password at some point.'
    WordWrap = True
  end
  inherited pnlTitle: TPanel
    TabOrder = 1
    inherited lblTitle: TLabel
      Width = 108
      Caption = 'Login Options'
    end
  end
  object gbLogin: TGroupBox
    Left = 28
    Top = 168
    Width = 281
    Height = 125
    Caption = ' Server Login '
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    object Label16: TLabel
      Left = 32
      Top = 44
      Width = 51
      Height = 13
      Caption = 'Username:'
    end
    object Label17: TLabel
      Left = 32
      Top = 66
      Width = 49
      Height = 13
      Caption = 'Password:'
    end
    object rbTrustedLogin: TRadioButton
      Left = 8
      Top = 96
      Width = 149
      Height = 17
      Caption = 'Use a trusted connection.'
      TabOrder = 0
    end
    object rbSQLLogin: TRadioButton
      Left = 8
      Top = 22
      Width = 141
      Height = 17
      Caption = 'Use the following login:'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
    object eUsername: TEdit
      Left = 104
      Top = 40
      Width = 121
      Height = 19
      TabOrder = 2
      Text = 'sa'
    end
    object ePassword: TEdit
      Left = 104
      Top = 64
      Width = 121
      Height = 19
      PasswordChar = '*'
      TabOrder = 3
    end
  end
end
