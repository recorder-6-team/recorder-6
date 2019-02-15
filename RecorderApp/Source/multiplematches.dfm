object dlgMultipleMatches: TdlgMultipleMatches
  Left = 454
  Top = 255
  BorderStyle = bsDialog
  Caption = 'Match Selection'
  ClientHeight = 472
  ClientWidth = 531
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    531
    472)
  PixelsPerInch = 96
  TextHeight = 13
  object lblInputValue: TLabel
    Left = 16
    Top = 8
    Width = 59
    Height = 13
    Caption = 'Import Value'
  end
  object lblImportValue: TLabel
    Left = 88
    Top = 8
    Width = 3
    Height = 13
    Caption = '-'
    Color = clBtnHighlight
    ParentColor = False
  end
  object lbMultiMatches: TListBox
    Left = 11
    Top = 32
    Width = 510
    Height = 169
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = lbMultiMatchesClick
  end
  object btnAction: TButton
    Left = 400
    Top = 440
    Width = 57
    Height = 25
    Caption = 'Accept'
    TabOrder = 1
    OnClick = btnActionClick
  end
  object bbCancel: TImageListButton
    Left = 462
    Top = 440
    Width = 57
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = bbCancelClick
    ImageIndex = 1
  end
  object memDetailedNotes: TMemo
    Left = 11
    Top = 216
    Width = 510
    Height = 209
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 3
    WordWrap = False
  end
  object cbExtraInfo: TCheckBox
    Left = 16
    Top = 443
    Width = 113
    Height = 17
    Caption = 'Extra information'
    TabOrder = 4
    OnClick = cbExtraInfoClick
  end
end
