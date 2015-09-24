inherited fraMigrateSettings1: TfraMigrateSettings1
  object lblInformation: TLabel [0]
    Left = 12
    Top = 44
    Width = 303
    Height = 26
    Caption = 
      'Data will be transfered to the Recorder 6 database into the SQL ' +
      'Server instance:'
    WordWrap = True
  end
  object lblInstance: TLabel [1]
    Left = 92
    Top = 57
    Width = 120
    Height = 14
    Caption = '<SQLExpress instance>'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsItalic]
    ParentFont = False
  end
  object lblSourceMDB: TLabel [2]
    Left = 12
    Top = 80
    Width = 163
    Height = 13
    Caption = 'Transfer Recorder 2002 data from:'
  end
  object lblAlsoTransfer: TLabel [3]
    Left = 12
    Top = 128
    Width = 262
    Height = 13
    Caption = 'Also transfer the additional data files as specified below:'
  end
  inherited pnlTitle: TPanel
    TabOrder = 11
    inherited lblTitle: TLabel
      Width = 131
      Caption = 'Transfer Settings'
    end
  end
  inherited pnlContinue: TPanel
    TabOrder = 12
  end
  object chkPolygonFilters: TCheckBox
    Left = 20
    Top = 242
    Width = 97
    Height = 17
    Caption = 'Polygon Filters:'
    TabOrder = 8
    OnClick = CheckChange
  end
  object chkObjectSheets: TCheckBox
    Left = 20
    Top = 194
    Width = 145
    Height = 17
    Caption = 'Map Polygon Layer Files:'
    TabOrder = 5
    OnClick = CheckChange
  end
  object chkMapFiles: TCheckBox
    Left = 20
    Top = 146
    Width = 89
    Height = 17
    Caption = 'Map Files:'
    TabOrder = 2
    OnClick = CheckChange
  end
  object eDBPath: TEdit
    Left = 12
    Top = 96
    Width = 289
    Height = 21
    TabOrder = 0
    OnChange = CheckChange
  end
  object btnDBFolder: TButton
    Left = 301
    Top = 96
    Width = 21
    Height = 19
    Caption = '...'
    TabOrder = 1
    OnClick = btnDBFolderClick
  end
  object btnPolygonFiltersFolder: TButton
    Left = 301
    Top = 260
    Width = 21
    Height = 19
    Caption = '...'
    TabOrder = 10
    OnClick = btnMapFilesFolderClick
  end
  object btnObjectSheetsFolder: TButton
    Left = 301
    Top = 212
    Width = 21
    Height = 19
    Caption = '...'
    TabOrder = 7
    OnClick = btnObjectSheetsFolderClick
  end
  object btnMapFilesFolder: TButton
    Left = 301
    Top = 164
    Width = 21
    Height = 19
    Caption = '...'
    TabOrder = 4
    OnClick = btnMapFilesFolderClick
  end
  object ePolygonFiltersPath: TEdit
    Left = 40
    Top = 260
    Width = 261
    Height = 21
    TabOrder = 9
    OnChange = CheckChange
  end
  object eObjectSheetsPath: TEdit
    Left = 40
    Top = 212
    Width = 261
    Height = 21
    TabOrder = 6
    OnChange = CheckChange
  end
  object eMapFilesPath: TEdit
    Left = 40
    Top = 164
    Width = 261
    Height = 21
    TabOrder = 3
    OnChange = CheckChange
  end
  object dlgFolder: TFolderBrowser
    BrowseFlags = []
    NewDialogStyle = False
    Title = 'Select folder'
    Left = 300
    Top = 8
  end
end
