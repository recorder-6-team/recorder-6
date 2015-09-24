object formLogin: TformLogin
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Database Login'
  ClientHeight = 262
  ClientWidth = 395
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clNavy
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 377
    Height = 201
    Shape = bsFrame
  end
  object labelUserName: TLabel
    Left = 70
    Top = 143
    Width = 52
    Height = 13
    Caption = 'Username:'
  end
  object labelPassword: TLabel
    Left = 72
    Top = 169
    Width = 50
    Height = 13
    Caption = 'Password:'
  end
  object StaticText: TStaticText
    Left = 24
    Top = 16
    Width = 353
    Height = 58
    AutoSize = False
    Caption = 
      'This process needs to install some stored procedures in your dat' +
      'abase. To do this, you need to be logged in as a system administ' +
      'rator. Please login using a system administrator account.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object rbTrusted: TRadioButton
    Left = 64
    Top = 88
    Width = 177
    Height = 17
    Caption = 'Use my Windows account'
    Checked = True
    TabOrder = 1
    TabStop = True
    OnClick = rbTrustedClick
  end
  object rbSQL: TRadioButton
    Left = 64
    Top = 118
    Width = 257
    Height = 17
    Caption = 'Use the following SQL Server account:'
    TabOrder = 2
    OnClick = rbSQLClick
  end
  object edUsername: TEdit
    Left = 152
    Top = 141
    Width = 137
    Height = 19
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 3
    Text = 'sa'
  end
  object edPassword: TEdit
    Left = 152
    Top = 167
    Width = 137
    Height = 19
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    PasswordChar = '*'
    TabOrder = 4
    Text = 'edPassword'
  end
  object bLogin: TButton
    Left = 104
    Top = 223
    Width = 75
    Height = 25
    Caption = 'Login'
    ModalResult = 1
    TabOrder = 5
    OnClick = bLoginClick
  end
  object bCancel: TButton
    Left = 198
    Top = 223
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
end
