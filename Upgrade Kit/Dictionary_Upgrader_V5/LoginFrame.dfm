inherited fraLogin: TfraLogin
  object lblClickNext: TLabel [0]
    Left = 12
    Top = 292
    Width = 136
    Height = 14
    Caption = 'Click Proceed to continue...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsItalic]
    ParentFont = False
  end
  object pnlExplain: TPanel [1]
    Left = 0
    Top = 40
    Width = 328
    Height = 233
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 2
    Visible = False
    object lblExplain: TLabel
      Left = 16
      Top = 48
      Width = 3
      Height = 13
      WordWrap = True
    end
  end
  inherited pnlTitle: TPanel
    inherited lblTitle: TLabel
      Width = 135
      Caption = 'SQL Server Login'
    end
  end
  object pnlLogin: TPanel
    Left = 0
    Top = 40
    Width = 328
    Height = 233
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 1
    object lblPleaseSelect: TLabel
      Left = 20
      Top = 4
      Width = 284
      Height = 26
      Caption = 
        'Please select the SQL Server system administrator login you woul' +
        'd like to use to log on to the SQL Server:'
      WordWrap = True
    end
    object gbLogin: TGroupBox
      Left = 28
      Top = 44
      Width = 281
      Height = 149
      Caption = ' SQL Server Login '
      TabOrder = 0
      object lblUsername: TLabel
        Left = 32
        Top = 59
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
      object rbSQL: TRadioButton
        Left = 8
        Top = 20
        Width = 265
        Height = 31
        Caption = 'Use the following SQL Server system administrator login:'
        Checked = True
        TabOrder = 0
        TabStop = True
        WordWrap = True
        OnClick = rbSQLClick
      end
      object eUsername: TEdit
        Left = 104
        Top = 56
        Width = 121
        Height = 21
        TabOrder = 1
        Text = 'sa'
      end
      object ePassword: TEdit
        Left = 104
        Top = 80
        Width = 121
        Height = 21
        PasswordChar = '*'
        TabOrder = 2
      end
      object rbTrusted: TRadioButton
        Left = 8
        Top = 108
        Width = 265
        Height = 33
        Caption = 'Use my Windows Account to login (Windows Authentication)'
        TabOrder = 3
        WordWrap = True
        OnClick = rbSQLClick
      end
    end
  end
end
