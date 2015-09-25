inherited fraInstallFolder: TfraInstallFolder
  object Label1: TLabel
    Left = 20
    Top = 48
    Width = 242
    Height = 32
    Caption = 
      'Click Next to install to this folder, or click Browse to install' +
      ' to a different folder.'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 20
    Top = 20
    Width = 115
    Height = 16
    Caption = 'Installation Folder'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnBrowse: TButton
    Left = 236
    Top = 131
    Width = 75
    Height = 22
    Caption = '&Browse...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = btnBrowseClick
  end
  object eInstallFolder: TEdit
    Left = 20
    Top = 104
    Width = 293
    Height = 24
    TabOrder = 1
    OnChange = eInstallFolderChange
  end
  object FolderBrowser: TFolderBrowser
    BrowseFlags = [bfDirsOnly]
    Folder = 'C:\Program Files\JNCC\Recorder\'
    NewDialogStyle = False
    Title = 'Select installation folder:'
    Left = 236
    Top = 157
  end
end
