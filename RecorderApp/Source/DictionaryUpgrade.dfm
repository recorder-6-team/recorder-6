object dlgDictionaryUpgrade: TdlgDictionaryUpgrade
  Left = 549
  Top = 476
  BorderStyle = bsDialog
  Caption = 'Dictionary Update'
  ClientHeight = 162
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 8
    Width = 413
    Height = 113
    Shape = bsFrame
  end
  object lblFileLocation: TLabel
    Left = 8
    Top = 24
    Width = 63
    Height = 13
    Caption = 'File Location:'
  end
  object lblCurrentDict: TLabel
    Left = 8
    Top = 48
    Width = 90
    Height = 13
    Caption = 'Current Dictionary: '
  end
  object lblCurrentKey: TLabel
    Left = 112
    Top = 48
    Width = 46
    Height = 13
    Caption = 'Unknown'
  end
  object lblDictStatus: TLabel
    Left = 8
    Top = 72
    Width = 86
    Height = 13
    Caption = 'Dictionary Status: '
  end
  object lblDictCurrentStatus: TLabel
    Left = 112
    Top = 72
    Width = 46
    Height = 13
    Caption = 'Unknown'
  end
  object lblBlockCaption: TLabel
    Left = 8
    Top = 96
    Width = 56
    Height = 13
    Caption = 'Block Size: '
  end
  object lblBlockSize: TLabel
    Left = 136
    Top = 96
    Width = 6
    Height = 13
    Caption = '1'
  end
  object lblUpdate: TLabel
    Left = 192
    Top = 48
    Width = 57
    Height = 13
    Caption = 'Update File '
  end
  object lblUpdateFile: TLabel
    Left = 272
    Top = 48
    Width = 61
    Height = 13
    Caption = 'lblUpdateFile'
  end
  object btnAction: TButton
    Left = 288
    Top = 128
    Width = 57
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = btnActionClick
  end
  object bbCancel: TImageListButton
    Left = 350
    Top = 128
    Width = 57
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = bbCancelClick
    ImageIndex = 1
  end
  object edFileLocation: TEdit
    Left = 112
    Top = 16
    Width = 273
    Height = 21
    TabOrder = 2
  end
  object btnDictionaryUpgradeFolder: TButton
    Left = 390
    Top = 18
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 3
    OnClick = btnDictionaryUpgradeFolderClick
  end
  object dlgFolder: TFolderBrowser
    BrowseFlags = [bfDirsOnly, bfStatusText]
    NewDialogStyle = False
    Title = 'Select folder'
    Left = 176
    Top = 438
  end
  object FolderBrowser1: TFolderBrowser
    BrowseFlags = [bfDirsOnly, bfStatusText]
    NewDialogStyle = False
    Title = 'Select folder'
    Left = 176
    Top = 438
  end
end
