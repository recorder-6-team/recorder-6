inherited fraMigrateComplete: TfraMigrateComplete
  object lblInformation: TLabel [0]
    Left = 12
    Top = 48
    Width = 306
    Height = 26
    Caption = 
      'Your data has been migrated to the latest version of Recorder.  ' +
      'A brief summary of the data transferred is given below:'
    WordWrap = True
  end
  object lblUpgradeErrors: TLabel [1]
    Left = 24
    Top = 200
    Width = 295
    Height = 56
    Caption = 
      'Errors were encountered during the upgrade process and a number ' +
      'of records failed to transfer from the Access database to SQL Se' +
      'rver.  The failed records have been copied into the UpgradeError' +
      's.mdb file in the database folder.'
    Color = clWindowText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    Visible = False
    WordWrap = True
  end
  object lblTotalRecords: TLabel [2]
    Left = 200
    Top = 80
    Width = 6
    Height = 13
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblLocations: TLabel [3]
    Left = 200
    Top = 96
    Width = 6
    Height = 13
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblNames: TLabel [4]
    Left = 200
    Top = 112
    Width = 6
    Height = 13
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblReferences: TLabel [5]
    Left = 200
    Top = 128
    Width = 6
    Height = 13
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblSurveys: TLabel [6]
    Left = 200
    Top = 144
    Width = 6
    Height = 13
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblTaxOccs: TLabel [7]
    Left = 200
    Top = 160
    Width = 6
    Height = 13
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblBioOccs: TLabel [8]
    Left = 200
    Top = 176
    Width = 6
    Height = 13
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblInfoRecords: TLabel [9]
    Left = 88
    Top = 80
    Width = 100
    Height = 13
    Caption = 'Records Transferred:'
  end
  object lblInfoLocations: TLabel [10]
    Left = 88
    Top = 96
    Width = 49
    Height = 13
    Caption = 'Locations:'
  end
  object lblInfoNames: TLabel [11]
    Left = 88
    Top = 112
    Width = 36
    Height = 13
    Caption = 'Names:'
  end
  object lblInfoReferences: TLabel [12]
    Left = 88
    Top = 128
    Width = 58
    Height = 13
    Caption = 'References:'
  end
  object lblInfoSurveys: TLabel [13]
    Left = 88
    Top = 144
    Width = 41
    Height = 13
    Caption = 'Surveys:'
  end
  object lblInfoTaxa: TLabel [14]
    Left = 88
    Top = 160
    Width = 106
    Height = 13
    Caption = 'Species Observations:'
  end
  object lblInfoBiotopes: TLabel [15]
    Left = 88
    Top = 176
    Width = 102
    Height = 13
    Caption = 'Habitat Observations:'
  end
  inherited pnlTitle: TPanel
    TabOrder = 2
    inherited lblTitle: TLabel
      Width = 187
      Caption = 'Data Migration Complete'
    end
  end
  inherited pnlContinue: TPanel
    inherited lblClickNext: TLabel
      Visible = False
    end
  end
  object chkLaunch: TCheckBox
    Left = 80
    Top = 268
    Width = 141
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Launch Recorder 6 now:'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
end
