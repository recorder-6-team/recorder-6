inherited fraInstallFolder: TfraInstallFolder
  object lblFolderInfo: TLabel [0]
    Left = 12
    Top = 64
    Width = 302
    Height = 26
    Caption = 
      'Click Next to install the application to this folder, or click B' +
      'rowse to install to a different folder. '
    WordWrap = True
  end
  object lblDisk: TLabel [1]
    Left = 20
    Top = 184
    Width = 289
    Height = 33
    AutoSize = False
    Caption = 'Available disk space on selected drive:'
    WordWrap = True
  end
  object lblRequiredSpace: TLabel [2]
    Left = 20
    Top = 164
    Width = 100
    Height = 13
    Caption = 'Required disk space:'
  end
  object lblRequired: TLabel [3]
    Left = 212
    Top = 164
    Width = 37
    Height = 13
    Caption = '600 MB'
  end
  object lblAvailable: TLabel [4]
    Left = 212
    Top = 184
    Width = 39
    Height = 13
    Caption = '20.1 GB'
  end
  object lblDriveNotValid: TLabel [5]
    Left = 36
    Top = 220
    Width = 269
    Height = 39
    Caption = 
      'The installation of Recorder 6 Workstation must be done on a loc' +
      'al drive. The selected drive is not a valid drive destination an' +
      'd cannot be used to install Recorder.'
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
  inherited pnlContinue: TPanel
    TabOrder = 3
  end
  object btnBrowse: TButton
    Left = 234
    Top = 119
    Width = 75
    Height = 22
    Caption = '&Browse...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object eInstallFolder: TEdit
    Left = 12
    Top = 96
    Width = 297
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
  object dlgFolder: TFolderBrowser
    BrowseFlags = [bfDirsOnly]
    Folder = 'C:\Recorder 6\'
    NewDialogStyle = False
    Title = 'Select installation folder:'
    Left = 280
    Top = 141
  end
end
