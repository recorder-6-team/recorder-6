inherited fraRemoteInstructions: TfraRemoteInstructions
  object Label1: TLabel
    Left = 20
    Top = 48
    Width = 253
    Height = 32
    Caption = 
      'You must manually install the database file onto your SQL Server' +
      ' instance.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 20
    Top = 20
    Width = 179
    Height = 16
    Caption = 'Server Database Installation'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 20
    Top = 96
    Width = 253
    Height = 32
    Caption = 'Click the View button below for instructions on how to do this.'
    WordWrap = True
  end
  object btnView: TButton
    Left = 126
    Top = 152
    Width = 75
    Height = 21
    Caption = '&View'
    TabOrder = 0
    OnClick = btnViewClick
  end
end
