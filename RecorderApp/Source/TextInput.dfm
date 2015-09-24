object dlgTextInput: TdlgTextInput
  Left = 396
  Top = 122
  BorderStyle = bsDialog
  Caption = 'Text Input'
  ClientHeight = 148
  ClientWidth = 313
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 8
    Width = 305
    Height = 101
    Shape = bsFrame
  end
  object lblSubject: TLabel
    Left = 16
    Top = 16
    Width = 72
    Height = 13
    Caption = 'Author'#39's Name:'
  end
  object lblInitials: TLabel
    Left = 16
    Top = 64
    Width = 32
    Height = 13
    Caption = 'Initials:'
  end
  object bbCancel: TImageListButton
    Left = 232
    Top = 116
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbOK: TImageListButton
    Left = 148
    Top = 116
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = bbOkClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object eName: TEdit
    Left = 16
    Top = 32
    Width = 277
    Height = 21
    TabOrder = 0
    OnChange = eNameChange
  end
  object eInitials: TEdit
    Left = 16
    Top = 80
    Width = 57
    Height = 21
    TabOrder = 1
    OnChange = eNameChange
  end
end
