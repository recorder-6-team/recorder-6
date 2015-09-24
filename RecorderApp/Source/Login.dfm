object dlgLogin: TdlgLogin
  Left = 599
  Top = 284
  ActiveControl = ePassword
  Anchors = []
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = '%s - Login'
  ClientHeight = 199
  ClientWidth = 265
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 6
    Top = 6
    Width = 253
    Height = 73
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 16
    Top = 20
    Width = 51
    Height = 13
    Caption = 'Username:'
  end
  object Label2: TLabel
    Left = 16
    Top = 49
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object lblPasswordInstruct: TLabel
    Left = 12
    Top = 120
    Width = 249
    Height = 73
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHotLight
    Font.Height = -8
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object ePassword: TEdit
    Left = 76
    Top = 46
    Width = 173
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object bbOk: TBitBtn
    Left = 102
    Top = 85
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = bbOkClick
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      040000000000800000000000000000000000100000001000000000840000008C
      00000094000008940800089C0800109C100010A5100018A5180021A5210021AD
      2100FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00AAAAAAAAAAAA
      AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA0AAAAAAAAAAAAAA111AAAAA
      AAAAAAAA3333AAAAAAAAAAA55555AAAAAAAAAA7777A77AAAAAAAAAA88AA887AA
      AAAAAAAAAAAA87AAAAAAAAAAAAAAA75AAAAAAAAAAAAAAA55AAAAAAAAAAAAAAA5
      3AAAAAAAAAAAAAAA31AAAAAAAAAAAAAAAA0AAAAAAAAAAAAAAAAA}
  end
  object bbCancel: TBitBtn
    Left = 184
    Top = 85
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000084000000
      8C000000940000009C000000A5000000AD000000B5000000BD000000C6000000
      CE000000D6000000DE000000E700FF00FF00FFFFFF00FFFFFF00EEEEEEEEEEEE
      EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE87EEEEE
      EEEEEEEE9875EE10EEEEEEEEE876E3211EEEEEEEEE86543EEEEEEEEEEEE765EE
      EEEEEEEEEE98766EEEEEEEEEEBA9E8776EEEEEEEECBEEE88EEEEEEEEEECEEEEE
      EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE}
  end
  object dblcUsers: TDBListCombo
    Left = 76
    Top = 16
    Width = 173
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = dblcUsersChange
    ListField = 'DISPLAY_NAME'
    KeyField = 'NAME_KEY'
    Datasource = dmGeneralData.dsUsers
    Active = False
    EmptyItem = False
    ReadOnly = False
  end
end
