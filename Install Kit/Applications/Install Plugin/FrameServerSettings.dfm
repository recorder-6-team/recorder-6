inherited fraServerSettings: TfraServerSettings
  object lblName: TLabel
    Left = 28
    Top = 129
    Width = 38
    Height = 16
    Caption = 'Name:'
    Enabled = False
    Transparent = True
  end
  object lblPath: TLabel
    Left = 28
    Top = 206
    Width = 31
    Height = 16
    Caption = 'Path:'
    Enabled = False
    Transparent = True
  end
  object Label1: TLabel
    Left = 20
    Top = 48
    Width = 251
    Height = 32
    Caption = 
      'Select the following settings for your MSDE installation if requ' +
      'ired.'
    Transparent = True
    WordWrap = True
  end
  object Label4: TLabel
    Left = 20
    Top = 20
    Width = 136
    Height = 16
    Caption = 'Local Server Settings'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object cbInstanceName: TCheckBox
    Left = 20
    Top = 88
    Width = 297
    Height = 33
    Caption = 'I want to specify my own server instance name'
    TabOrder = 0
    WordWrap = True
    OnClick = cbInstanceNameClick
  end
  object eInstanceName: TRestrictedEdit
    Left = 68
    Top = 125
    Width = 185
    Height = 24
    CharCase = ecUpperCase
    Enabled = False
    MaxLength = 16
    TabOrder = 1
    OnChange = eInstanceNameChange
    OnCheckText = eInstanceNameCheckText
  end
  object cbDataDir: TCheckBox
    Left = 20
    Top = 176
    Width = 289
    Height = 17
    Caption = 'I want to specify a non-default database path'
    TabOrder = 2
    OnClick = cbDataDirClick
  end
  object eDataDir: TEdit
    Left = 68
    Top = 204
    Width = 209
    Height = 22
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnChange = eDataDirChange
  end
  object btnFolderBrowse: TButton
    Left = 280
    Top = 204
    Width = 22
    Height = 21
    Caption = '...'
    Enabled = False
    TabOrder = 4
    OnClick = btnFolderBrowseClick
  end
  object FolderBrowser: TFolderBrowser
    BrowseFlags = [bfDirsOnly]
    NewDialogStyle = False
    Title = 'Select MSDE installation folder:'
    Left = 280
    Top = 232
  end
end
