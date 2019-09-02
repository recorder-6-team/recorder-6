inherited fraDicUpdater: TfraDicUpdater
  Width = 326
  Height = 240
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Color = clBtnHighlight
  object Label2: TLabel [0]
    Left = 16
    Top = 28
    Width = 63
    Height = 13
    Caption = 'File Location:'
  end
  object lblFileLocation: TLabel [1]
    Left = 8
    Top = 44
    Width = 63
    Height = 13
    Caption = 'File Location:'
  end
  object lblCurrentDict: TLabel [2]
    Left = 16
    Top = 123
    Width = 90
    Height = 13
    Caption = 'Current Dictionary: '
  end
  object lblCurrentKey: TLabel [3]
    Left = 112
    Top = 123
    Width = 46
    Height = 13
    Caption = 'Unknown'
  end
  object lblDictStatus: TLabel [4]
    Left = 16
    Top = 171
    Width = 86
    Height = 13
    Caption = 'Dictionary Status: '
  end
  object lblDictCurrentStatus: TLabel [5]
    Left = 112
    Top = 171
    Width = 46
    Height = 13
    Caption = 'Unknown'
  end
  object lblBlockCaption: TLabel [6]
    Left = 16
    Top = 195
    Width = 56
    Height = 13
    Caption = 'Block Size: '
  end
  object lblUpdate: TLabel [7]
    Left = 16
    Top = 147
    Width = 57
    Height = 13
    Caption = 'Update File '
  end
  object lblUpdateFile: TLabel [8]
    Left = 112
    Top = 147
    Width = 61
    Height = 13
    Caption = 'lblUpdateFile'
  end
  object lLicenceKey: TLabel [9]
    Left = 16
    Top = 91
    Width = 58
    Height = 13
    Caption = 'Licence key'
  end
  inherited pnlTitle: TPanel
    Width = 326
    Color = clBtnHighlight
    ParentColor = False
    inherited lblTitle: TLabel
      Left = 4
      Top = 16
      Width = 137
      Caption = 'Dictionary Update'
    end
  end
  object edFileLocation: TEdit
    Left = 13
    Top = 64
    Width = 260
    Height = 21
    TabOrder = 1
  end
  object btnDictionaryUpgradeFolder: TButton
    Left = 281
    Top = 64
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 2
    OnClick = btnDictionaryUpgradeFolderClick
  end
  object edLicenceKey: TEdit
    Left = 112
    Top = 91
    Width = 73
    Height = 21
    MaxLength = 8
    TabOrder = 3
  end
  object edBlockSize: TEdit
    Left = 112
    Top = 195
    Width = 41
    Height = 21
    MaxLength = 2
    TabOrder = 4
    Text = '50'
    OnKeyPress = edBlockSizeKeyPress
  end
  object dlgFolder: TFolderBrowser
    BrowseFlags = [bfDirsOnly, bfStatusText]
    NewDialogStyle = False
    Title = 'Select folder'
    Left = 176
    Top = 438
  end
end
