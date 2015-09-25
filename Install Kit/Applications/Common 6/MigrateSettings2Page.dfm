inherited fraMigrateSettings2: TfraMigrateSettings2
  object lblInformation: TLabel [0]
    Left = 12
    Top = 48
    Width = 237
    Height = 13
    Caption = 'Additional data files to transfer, as specified below:'
  end
  inherited pnlTitle: TPanel
    TabOrder = 12
    inherited lblTitle: TLabel
      Width = 234
      Caption = 'Transfer Settings (...continued)'
    end
  end
  inherited pnlContinue: TPanel
    TabOrder = 13
    inherited lblClickNext: TLabel
      Width = 137
      Caption = 'Click Transfer to continue...'
    end
  end
  object eRucksacksPath: TEdit
    Left = 40
    Top = 88
    Width = 261
    Height = 21
    TabOrder = 1
    OnChange = CheckChange
  end
  object btnRucksacksFolder: TButton
    Left = 301
    Top = 88
    Width = 21
    Height = 19
    Caption = '...'
    TabOrder = 2
    OnClick = FolderClick
  end
  object eImagesPath: TEdit
    Left = 40
    Top = 232
    Width = 261
    Height = 21
    TabOrder = 10
    OnChange = CheckChange
  end
  object eTemplatesPath: TEdit
    Left = 40
    Top = 184
    Width = 261
    Height = 21
    TabOrder = 7
    OnChange = CheckChange
  end
  object eCardsPath: TEdit
    Left = 40
    Top = 136
    Width = 261
    Height = 21
    TabOrder = 4
    OnChange = CheckChange
  end
  object btnCardsFolder: TButton
    Left = 301
    Top = 136
    Width = 21
    Height = 19
    Caption = '...'
    TabOrder = 5
    OnClick = FolderClick
  end
  object btnTemplatesFolder: TButton
    Left = 301
    Top = 184
    Width = 21
    Height = 19
    Caption = '...'
    TabOrder = 8
    OnClick = FolderClick
  end
  object btnImagesFolder: TButton
    Left = 301
    Top = 232
    Width = 21
    Height = 19
    Caption = '...'
    TabOrder = 11
    OnClick = FolderClick
  end
  object chkRucksacks: TCheckBox
    Left = 20
    Top = 70
    Width = 89
    Height = 17
    Caption = 'Rucksacks:'
    TabOrder = 0
    OnClick = CheckChange
  end
  object chkCards: TCheckBox
    Left = 20
    Top = 118
    Width = 117
    Height = 17
    Caption = 'Recording Cards:'
    TabOrder = 3
    OnClick = CheckChange
  end
  object chkTemplates: TCheckBox
    Left = 20
    Top = 166
    Width = 117
    Height = 17
    Caption = 'Report Templates:'
    TabOrder = 6
    OnClick = CheckChange
  end
  object chkImages: TCheckBox
    Left = 20
    Top = 214
    Width = 117
    Height = 17
    Caption = 'Local Image Files:'
    TabOrder = 9
    OnClick = CheckChange
  end
  object dlgFolder: TFolderBrowser
    BrowseFlags = []
    NewDialogStyle = False
    Title = 'Select folder'
    Left = 300
    Top = 8
  end
end
