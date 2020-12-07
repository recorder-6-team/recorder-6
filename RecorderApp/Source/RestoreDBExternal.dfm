object dlgRestoreDBEXternal: TdlgRestoreDBEXternal
  Left = 412
  Top = 425
  BorderStyle = bsDialog
  Caption = 'Restore Specified Backup'
  ClientHeight = 118
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblDBLocation: TLabel
    Left = 8
    Top = 52
    Width = 81
    Height = 13
    Caption = 'Database Folder:'
  end
  object Label1: TLabel
    Left = 8
    Top = 20
    Width = 56
    Height = 13
    Caption = 'Backup file '
  end
  object btnAction: TButton
    Left = 288
    Top = 80
    Width = 57
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = btnActionClick
  end
  object bbCancel: TImageListButton
    Left = 350
    Top = 80
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
    Left = 96
    Top = 48
    Width = 297
    Height = 21
    TabOrder = 2
  end
  object btnDatabaseFolder: TButton
    Left = 398
    Top = 50
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 3
    OnClick = btnDatabaseFolderClick
  end
  object edBackupFile: TEdit
    Left = 96
    Top = 16
    Width = 177
    Height = 21
    TabOrder = 4
    Text = 'NBNdata.bak'
  end
  object dlgFolder: TFolderBrowser
    BrowseFlags = [bfDirsOnly, bfStatusText]
    NewDialogStyle = False
    Title = 'Select report folder'
    Left = 92
    Top = 72
  end
end
