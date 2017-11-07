object dlgGetAdminPwd: TdlgGetAdminPwd
  Left = 505
  Top = 295
  Width = 311
  Height = 294
  Caption = 'Enter Login Details'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 132
    Width = 289
    Height = 94
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 13
    Top = 148
    Width = 51
    Height = 13
    Caption = 'Username:'
  end
  object Label2: TLabel
    Left = 13
    Top = 176
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object Label3: TLabel
    Left = 8
    Top = 4
    Width = 260
    Height = 52
    Caption = 
      'To install this addin, updates are required to the database.  Pl' +
      'ease enter details of the SQL Server '#39'sa'#39' (system administrator)' +
      ' login to use when applying these updates.'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 8
    Top = 60
    Width = 277
    Height = 65
    Caption = 
      'If you are not sure what these login details are, then please as' +
      'k your network or database administrator.  If you are using a in' +
      'stallation of MSDE, then the username '#39'sa'#39' and blank password ar' +
      'e the login details setup by default, unless you have subsequent' +
      'ly changed them.'
    WordWrap = True
  end
  object btnOK: TImageListButton
    Left = 136
    Top = 235
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = btnOKClick
    ImageIndex = 6
  end
  object btnCancel: TImageListButton
    Left = 221
    Top = 235
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    OnClick = btnCancelClick
    ImageIndex = 4
  end
  object ePassword: TMaskEdit
    Left = 79
    Top = 172
    Width = 208
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object eUsername: TEdit
    Left = 79
    Top = 144
    Width = 208
    Height = 21
    TabOrder = 0
  end
  object chkTrustedLogin: TCheckBox
    Left = 79
    Top = 200
    Width = 97
    Height = 17
    Caption = 'Trusted login'
    TabOrder = 2
  end
end
