inherited fraInstallFolder: TfraInstallFolder
  object lblInformation: TLabel [0]
    Left = 12
    Top = 56
    Width = 302
    Height = 39
    Caption = 
      'Click Next to install the application to this folder, or click B' +
      'rowse to install to a different folder. The database may be stor' +
      'ed on a different drive.'
    WordWrap = True
  end
  object lblDisk: TLabel [1]
    Left = 21
    Top = 190
    Width = 171
    Height = 13
    Caption = 'Available disk space on local drives:'
  end
  object lblRequiredSpace: TLabel [2]
    Left = 20
    Top = 150
    Width = 182
    Height = 13
    Caption = 'Required disk space (Application files):'
  end
  object lblRequired: TLabel [3]
    Left = 222
    Top = 150
    Width = 37
    Height = 13
    Caption = '600 MB'
  end
  object lblNotLocal: TLabel [4]
    Left = 36
    Top = 122
    Width = 251
    Height = 26
    Caption = 
      'The selected drive is not a local drive and cannot be used to in' +
      'stall Recorder.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblRequiredDatabase: TLabel [5]
    Left = 20
    Top = 170
    Width = 176
    Height = 13
    Caption = 'Required disk space (Database files):'
  end
  object lblRequiredDB: TLabel [6]
    Left = 222
    Top = 170
    Width = 24
    Height = 13
    Caption = '1 GB'
  end
  object lblNoSpace: TLabel [7]
    Left = 36
    Top = 122
    Width = 232
    Height = 13
    Caption = 'There is not enough space on the selected drive.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  inherited pnlTitle: TPanel
    TabOrder = 2
    inherited lblTitle: TLabel
      Width = 291
      Caption = 'Installation Folder for Application Files'
    end
  end
  object eInstallFolder: TEdit
    Left = 12
    Top = 102
    Width = 289
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = eInstallFolderChange
  end
  object btnBrowse: TButton
    Left = 301
    Top = 102
    Width = 21
    Height = 19
    Caption = '...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object dlgFolder: TFolderBrowser
    BrowseFlags = [bfDirsOnly]
    Folder = 'C:\Recorder 6\'
    NewDialogStyle = False
    Title = 'Select installation folder:'
    Left = 296
    Top = 137
  end
end
