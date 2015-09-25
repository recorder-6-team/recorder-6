inherited fraLogin: TfraLogin
  object lblMigrateInfo1: TLabel [0]
    Left = 12
    Top = 56
    Width = 302
    Height = 26
    Caption = 
      'To transfer data to the SQL Server you must supply login details' +
      ' that give system administrator privileges on the server. '
    WordWrap = True
  end
  object lblMigrateInfo2: TLabel [1]
    Left = 12
    Top = 156
    Width = 294
    Height = 26
    Caption = 
      'Note that this tool must be run on the Server itself so that SQL' +
      ' Server can find the Access data files.'
    WordWrap = True
  end
  object lblCommonInfo: TLabel [2]
    Left = 12
    Top = 100
    Width = 304
    Height = 52
    Caption = 
      'The default "Username = sa" is automatically created when SQL Ex' +
      'press is installed, with the password that you specified.  You m' +
      'ay have chosen to change the username and password at some point' +
      '.'
    WordWrap = True
  end
  object lblDatabaseInfo: TLabel [3]
    Left = 12
    Top = 46
    Width = 295
    Height = 52
    Caption = 
      'To install the NBN Database onto the selected MSDE/SQL Server in' +
      'stance, you must supply login details that give system administr' +
      'ator privileges on the server.  Also, please ensure that the SQL' +
      ' Server you selected exists.'
    WordWrap = True
  end
  object lblAccessInfo: TLabel [4]
    Left = 12
    Top = 46
    Width = 290
    Height = 52
    Caption = 
      'To create the Access linked database, you must supply login deta' +
      'ils for the selected MSDE/SQL Server instance that give system a' +
      'dministrator privileges on the server.  Also, please ensure that' +
      ' the SQL Server you selected exists.'
    WordWrap = True
  end
  inherited pnlTitle: TPanel
    TabOrder = 2
    inherited lblTitle: TLabel
      Width = 108
      Caption = 'Login Options'
    end
  end
  inherited pnlContinue: TPanel
    inherited lblClickNext: TLabel
      Width = 125
      Caption = 'Click Install to continue...'
    end
  end
  object gbLogin: TGroupBox
    Left = 28
    Top = 192
    Width = 269
    Height = 149
    Caption = ' Server Login '
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    object lblUsername: TLabel
      Left = 32
      Top = 94
      Width = 51
      Height = 13
      Caption = 'Username:'
    end
    object lblPassword: TLabel
      Left = 32
      Top = 118
      Width = 49
      Height = 13
      Caption = 'Password:'
    end
    object rbTrustedLogin: TRadioButton
      Left = 8
      Top = 20
      Width = 257
      Height = 31
      Caption = 'Use a trusted connection to login (Windows authentication)'
      Checked = True
      TabOrder = 0
      TabStop = True
      WordWrap = True
    end
    object rbSQLLogin: TRadioButton
      Left = 8
      Top = 54
      Width = 257
      Height = 31
      Caption = 'Use the following SQL Server system administrator login:'
      TabOrder = 1
      TabStop = True
      WordWrap = True
      OnClick = rbSQLLoginClick
      OnEnter = rbSQLLoginEnter
    end
    object eUsername: TEdit
      Left = 104
      Top = 92
      Width = 121
      Height = 19
      TabOrder = 2
      Text = 'sa'
    end
    object ePassword: TEdit
      Left = 104
      Top = 116
      Width = 121
      Height = 19
      PasswordChar = '*'
      TabOrder = 3
    end
  end
end
