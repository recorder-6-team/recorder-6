object dlgRucksackSave: TdlgRucksackSave
  Left = 305
  Top = 126
  ActiveControl = eRucksackName
  BorderStyle = bsDialog
  Caption = 'Save Rucksack'
  ClientHeight = 172
  ClientWidth = 376
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 8
    Width = 365
    Height = 125
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 181
    Height = 13
    Caption = 'Please enter a &name for this rucksack:'
    FocusControl = eRucksackName
  end
  object Label2: TLabel
    Left = 16
    Top = 60
    Width = 88
    Height = 13
    Caption = 'Destination Folder:'
  end
  object eRucksackName: TEdit
    Left = 16
    Top = 32
    Width = 341
    Height = 21
    TabOrder = 0
    OnChange = eRucksackNameChange
  end
  object btnBrowse: TButton
    Left = 282
    Top = 100
    Width = 75
    Height = 25
    Caption = '&Browse...'
    TabOrder = 2
    OnClick = btnBrowseClick
  end
  object eDestFolder: TEdit
    Left = 16
    Top = 76
    Width = 341
    Height = 21
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    Text = 'Destination Folder'
  end
  object bbOk: TImageListButton
    Left = 210
    Top = 140
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 3
    Down = False
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object bbCancel: TImageListButton
    Left = 294
    Top = 140
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    Down = False
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object dlgFolder: TFolderBrowser
    BrowseFlags = [bfDirsOnly, bfStatusText]
    NewDialogStyle = False
    Title = 'Select the destination folder'
    Left = 248
    Top = 100
  end
end
