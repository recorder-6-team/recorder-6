inherited fraLogin: TfraLogin
  object lblInformation: TLabel [0]
    Left = 12
    Top = 48
    Width = 307
    Height = 65
    Caption = 
      'To transfer data to the SQL Server you must supply login details' +
      ' that give system administrator privileges on the server.  The d' +
      'efault details are Username = sa and the password which you ente' +
      'red when SQL Express was installed.  You may have chosen to chan' +
      'ge the username and password at some point.'
    WordWrap = True
  end
  object lblNote: TLabel [1]
    Left = 12
    Top = 128
    Width = 294
    Height = 26
    Caption = 
      'Note that this tool must be run on the Server itself so that SQL' +
      ' Server can find the Access data files.'
    WordWrap = True
  end
  inherited pnlTitle: TPanel
    TabOrder = 2
    inherited lblTitle: TLabel
      Width = 108
      Caption = 'Login Options'
    end
  end
  object gbLogin: TGroupBox
    Left = 28
    Top = 164
    Width = 269
    Height = 149
    Caption = ' SQL Server Login '
    TabOrder = 0
    object lblUsername: TLabel
      Left = 32
      Top = 60
      Width = 51
      Height = 13
      Caption = 'Username:'
    end
    object lblPassword: TLabel
      Left = 32
      Top = 82
      Width = 49
      Height = 13
      Caption = 'Password:'
    end
    object rbTrustedLogin: TRadioButton
      Left = 8
      Top = 108
      Width = 237
      Height = 31
      Caption = 'Use my Windows Account to login (Windows Authentication)'
      TabOrder = 0
      WordWrap = True
    end
    object rbSQLLogin: TRadioButton
      Left = 8
      Top = 22
      Width = 257
      Height = 31
      Caption = 'Use the following SQL Server system administrator login:'
      Checked = True
      TabOrder = 1
      TabStop = True
      WordWrap = True
    end
    object eUsername: TEdit
      Left = 104
      Top = 56
      Width = 121
      Height = 21
      TabOrder = 2
      Text = 'sa'
    end
    object ePassword: TEdit
      Left = 104
      Top = 80
      Width = 121
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
  end
end
