inherited fraRemoveWelcome: TfraRemoveWelcome
  object lblInformation: TLabel [0]
    Left = 12
    Top = 48
    Width = 305
    Height = 52
    Caption = 
      'This process will uninstall Recorder 2002 from your machine and ' +
      'delete the Recorder 2002 folder.  Don'#39't do this until you are su' +
      're that Recorder 6 is working properly and your data has been tr' +
      'ansferred successfully from Recorder 2002.'
    WordWrap = True
  end
  object lblDB: TLabel [1]
    Left = 40
    Top = 144
    Width = 84
    Height = 13
    Caption = 'Path to database:'
  end
  object lblZip: TLabel [2]
    Left = 40
    Top = 192
    Width = 129
    Height = 13
    Caption = 'Archive file name and path:'
  end
  inherited pnlTitle: TPanel
    TabOrder = 5
    inherited lblTitle: TLabel
      Width = 182
      Caption = 'Recorder 2002 Uninstall'
    end
  end
  inherited pnlContinue: TPanel
    TabOrder = 6
    inherited lblClickNext: TLabel
      Width = 135
      Caption = 'Click Remove to continue...'
    end
  end
  object chkZip: TCheckBox
    Left = 16
    Top = 124
    Width = 225
    Height = 17
    Caption = 'Zip and archive Recorder 2002 database.'
    TabOrder = 0
    OnClick = chkZipClick
  end
  object eDBPath: TEdit
    Left = 40
    Top = 160
    Width = 261
    Height = 21
    TabOrder = 1
    OnChange = CheckChange
  end
  object btnDBFolder: TButton
    Left = 301
    Top = 160
    Width = 21
    Height = 19
    Caption = '...'
    TabOrder = 2
    OnClick = btnDBFolderClick
  end
  object eZipName: TEdit
    Left = 40
    Top = 208
    Width = 261
    Height = 21
    TabOrder = 3
    OnChange = CheckChange
  end
  object btnZipName: TButton
    Left = 301
    Top = 208
    Width = 21
    Height = 19
    Caption = '...'
    TabOrder = 4
    OnClick = btnZipNameClick
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'zip'
    Filter = 'Zip files (*.zip)|*.zip|All Files (*.*)|*.*'
    Left = 300
    Top = 4
  end
  object dlgFolder: TFolderBrowser
    BrowseFlags = []
    NewDialogStyle = False
    Title = 'Select folder'
    Left = 248
    Top = 4
  end
end
