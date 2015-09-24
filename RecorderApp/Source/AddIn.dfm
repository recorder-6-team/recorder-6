object dlgAddIn: TdlgAddIn
  Left = 246
  Top = 143
  BorderStyle = bsDialog
  Caption = 'Add-Ins Configuration'
  ClientHeight = 346
  ClientWidth = 413
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
    Left = 8
    Top = 8
    Width = 397
    Height = 301
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 20
    Top = 220
    Width = 56
    Height = 13
    Caption = 'Description:'
  end
  object bbShieldInstall: TBitBtn
    Left = 316
    Top = 20
    Width = 75
    Height = 25
    Caption = '&Install'
    TabOrder = 2
    Visible = False
    OnClick = bbInstallClick
  end
  object bbShieldRemove: TBitBtn
    Left = 316
    Top = 56
    Width = 75
    Height = 25
    Caption = '&Remove'
    TabOrder = 4
    Visible = False
    OnClick = bbRemoveClick
  end
  object bbAbout: TImageListButton
    Left = 316
    Top = 184
    Width = 75
    Height = 25
    Caption = '&About'
    TabOrder = 5
    OnClick = bbAboutClick
    ImageList = dmFormActions.ilMenuOn
    ImageIndex = 60
    Spacing = -1
  end
  object bbRemove: TImageListButton
    Left = 316
    Top = 56
    Width = 75
    Height = 25
    Caption = '&Remove'
    TabOrder = 3
    OnClick = bbRemoveClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
    Spacing = -1
  end
  object bbInstall: TImageListButton
    Left = 316
    Top = 20
    Width = 75
    Height = 25
    Caption = '&Install'
    TabOrder = 1
    OnClick = bbInstallClick
    ImageList = dmFormActions.ilMenuOn
    ImageIndex = 59
    Spacing = -1
  end
  object lbAddIns: TListBox
    Left = 20
    Top = 20
    Width = 285
    Height = 189
    Style = lbOwnerDrawFixed
    ItemHeight = 18
    TabOrder = 0
    OnClick = lbAddInsClick
    OnDrawItem = lbAddInsDrawItem
  end
  object mmDescription: TMemo
    Left = 20
    Top = 236
    Width = 373
    Height = 65
    Color = clBtnFace
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object bbClose: TButton
    Left = 328
    Top = 316
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Close'
    TabOrder = 7
    OnClick = bbCloseClick
  end
  object dlgOpenFile: TOpenDialog
    Filter = 'Recorder Addin|*.dll;*.ocx'
    Left = 336
    Top = 96
  end
end
