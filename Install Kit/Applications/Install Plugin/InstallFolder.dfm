inherited fraInstallFolder: TfraInstallFolder
  object Label1: TLabel
    Left = 16
    Top = 52
    Width = 286
    Height = 32
    Caption = 
      'Click Next to install to this folder, or click Change to install' +
      ' to a different folder.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 16
    Top = 96
    Width = 289
    Height = 45
    AutoSize = False
    Caption = 'C:\Program Files\JNCC\Recorder\'
    WordWrap = True
  end
  object Button1: TButton
    Left = 232
    Top = 144
    Width = 75
    Height = 21
    Caption = 'Change...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object dlgInstallFolder: TFolderBrowser
    BrowseFlags = []
    Title = 'Select folder'
    Left = 276
    Top = 172
  end
end
