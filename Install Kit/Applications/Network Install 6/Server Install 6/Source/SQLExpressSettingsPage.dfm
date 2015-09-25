inherited fraSQLExpressSettings: TfraSQLExpressSettings
  object lblExistingInstancePlural: TLabel [0]
    Left = 12
    Top = 39
    Width = 282
    Height = 52
    Caption = 
      'There are existing instances of MSDE, SQL Express or SQL Server ' +
      'on this machine.  Using an existing instance is usually preferab' +
      'le to installing a new one as it prevents your system being clut' +
      'tered up by additional files. '
    Visible = False
    WordWrap = True
  end
  object lblExistingInstanceSingular: TLabel [1]
    Left = 12
    Top = 39
    Width = 282
    Height = 52
    Caption = 
      'There is an existing instance of MSDE, SQL Express or SQL Server' +
      ' on this machine.  Using an existing instance is usually prefera' +
      'ble to installing a new one as it prevents your system being clu' +
      'ttered up by additional files. '
    Visible = False
    WordWrap = True
  end
  object pnlControls: TPanel [2]
    Left = 0
    Top = 91
    Width = 336
    Height = 270
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 0
    object lblNewInstanceName: TLabel
      Left = 12
      Top = 64
      Width = 296
      Height = 26
      Caption = 
        'New instance name (it must start with a letter or underscore (_)' +
        ' and contain only letters, numbers, underscores or '#39'$'#39' signs):'
      WordWrap = True
    end
    object lblNameInUse: TLabel
      Left = 172
      Top = 96
      Width = 141
      Height = 13
      Caption = 'Instance name already in use.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object lblNoSpace: TLabel
      Left = 28
      Top = 43
      Width = 230
      Height = 13
      Caption = 'Not enough space for SQL Express on this drive.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object lblInstanceDir: TLabel
      Left = 12
      Top = 5
      Width = 156
      Height = 13
      Caption = 'SQL Server Express install folder:'
    end
    object lblNotLocal: TLabel
      Left = 28
      Top = 43
      Width = 111
      Height = 13
      Caption = 'This is not a local drive.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object lblEnterPassword: TLabel
      Left = 12
      Top = 136
      Width = 308
      Height = 26
      Caption = 
        'Enter a password to use for the System administrator (sa) accoun' +
        't for SQL Express.'
      WordWrap = True
    end
    object lblConditions: TLabel
      Left = 12
      Top = 167
      Width = 304
      Height = 39
      Caption = 
        'This password should be at least 8 characters long, and contain ' +
        'three or more different types of character (out of upper case le' +
        'tter, lower case letter, number and non-alphanumeric).'
      WordWrap = True
    end
    object lblPassword: TLabel
      Left = 28
      Top = 213
      Width = 49
      Height = 13
      Caption = 'Password:'
    end
    object lblPasswordConfirm: TLabel
      Left = 28
      Top = 237
      Width = 87
      Height = 13
      Caption = 'Confirm Password:'
    end
    object eNewInstanceName: TRestrictedEdit
      Left = 28
      Top = 93
      Width = 141
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 16
      TabOrder = 2
      OnChange = CheckChange
    end
    object eServerPath: TEdit
      Left = 28
      Top = 21
      Width = 273
      Height = 21
      TabOrder = 0
      OnChange = eServerPathChange
    end
    object btnServerPath: TButton
      Left = 301
      Top = 21
      Width = 21
      Height = 19
      Caption = '...'
      TabOrder = 1
      OnClick = btnServerPathClick
    end
    object ePassword: TEdit
      Left = 124
      Top = 209
      Width = 137
      Height = 21
      MaxLength = 128
      PasswordChar = '*'
      TabOrder = 3
      OnChange = CheckChange
    end
    object ePasswordConfirm: TEdit
      Left = 124
      Top = 233
      Width = 137
      Height = 21
      MaxLength = 128
      PasswordChar = '*'
      TabOrder = 4
      OnChange = CheckChange
    end
  end
  inherited pnlTitle: TPanel
    TabOrder = 2
    inherited lblTitle: TLabel
      Width = 167
      Caption = 'SQL Express Settings'
    end
  end
  inherited pnlContinue: TPanel
    inherited lblClickNext: TLabel
      Width = 125
      Caption = 'Click Install to continue...'
    end
  end
  object dlgFolder: TFolderBrowser
    BrowseFlags = [bfDirsOnly]
    NewDialogStyle = False
    Title = 'Select SQL Express installation folder:'
    Left = 284
    Top = 4
  end
end
