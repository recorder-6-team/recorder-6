object dlgPassword: TdlgPassword
  Left = 405
  Top = 164
  BorderStyle = bsDialog
  Caption = 'DatabasePassword'
  ClientHeight = 105
  ClientWidth = 263
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
  object lblPasswordInstruct: TLabel
    Left = 8
    Top = 8
    Width = 180
    Height = 13
    Caption = 'Please enter your database password.'
  end
  object ePassword: TEdit
    Left = 8
    Top = 40
    Width = 249
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object btnOk: TImageListButton
    Left = 104
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
    Down = False
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object btnCancel: TImageListButton
    Left = 184
    Top = 72
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    Down = False
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
end
