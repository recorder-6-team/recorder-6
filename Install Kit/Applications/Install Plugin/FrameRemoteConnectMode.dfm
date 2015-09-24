inherited fraRemoteConnectMode: TfraRemoteConnectMode
  object Label1: TLabel
    Left = 20
    Top = 56
    Width = 258
    Height = 48
    Caption = 
      'Check the following box if you want users to connect to the sele' +
      'cted server using NT authentication.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 20
    Top = 116
    Width = 248
    Height = 32
    Caption = 
      'You must use this option if SQL Server authentication is not ena' +
      'bled on the server.'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 20
    Top = 20
    Width = 164
    Height = 16
    Caption = 'Remote Connection Mode'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object cbNTAuthentication: TCheckBox
    Left = 48
    Top = 160
    Width = 185
    Height = 17
    Caption = 'Use NT Authentication'
    TabOrder = 0
    OnClick = cbNTAuthenticationClick
  end
end
