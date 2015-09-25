object dlgChangePassword: TdlgChangePassword
  Left = 550
  Top = 291
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Change Password'
  ClientHeight = 175
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 8
    Width = 373
    Height = 129
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 20
    Top = 48
    Width = 68
    Height = 13
    Caption = '&Old Password:'
    FocusControl = eOldPassword
  end
  object Label2: TLabel
    Left = 20
    Top = 76
    Width = 74
    Height = 13
    Caption = '&New Password:'
    FocusControl = eNewPassword
  end
  object Label3: TLabel
    Left = 20
    Top = 104
    Width = 112
    Height = 13
    Caption = '&Confirm New Password:'
    FocusControl = eConfirmPassword
  end
  object Label4: TLabel
    Left = 20
    Top = 20
    Width = 56
    Height = 13
    Caption = 'User Name:'
  end
  object lblUserName: TLabel
    Left = 140
    Top = 20
    Width = 221
    Height = 13
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object bbOK: TImageListButton
    Left = 215
    Top = 145
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = bbOKClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object bbCancel: TImageListButton
    Left = 303
    Top = 145
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object eOldPassword: TEdit
    Left = 140
    Top = 44
    Width = 221
    Height = 21
    MaxLength = 20
    PasswordChar = '*'
    TabOrder = 0
  end
  object eNewPassword: TEdit
    Left = 140
    Top = 72
    Width = 221
    Height = 21
    MaxLength = 20
    PasswordChar = '*'
    TabOrder = 1
  end
  object eConfirmPassword: TEdit
    Left = 140
    Top = 100
    Width = 221
    Height = 21
    MaxLength = 20
    PasswordChar = '*'
    TabOrder = 2
  end
end
