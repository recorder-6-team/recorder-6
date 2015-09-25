object dlgFirstRun: TdlgFirstRun
  Left = 491
  Top = 268
  Anchors = []
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Recorder Setup Wizard'
  ClientHeight = 474
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object bvlFrame: TBevel
    Left = 6
    Top = 6
    Width = 387
    Height = 427
    Shape = bsFrame
  end
  object lblUsernameDesc: TLabel
    Left = 16
    Top = 16
    Width = 348
    Height = 26
    Caption = 
      'Please enter your name (which will be used to customise some asp' +
      'ects of Recorder) and a password (optional):'
    WordWrap = True
  end
  object lblFirstname: TLabel
    Left = 28
    Top = 55
    Width = 53
    Height = 13
    Caption = 'First Name:'
  end
  object lblSurname: TLabel
    Left = 28
    Top = 83
    Width = 45
    Height = 13
    Caption = 'Surname:'
  end
  object lblPassword: TLabel
    Left = 28
    Top = 111
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object lblConfirmPassword: TLabel
    Left = 28
    Top = 139
    Width = 87
    Height = 13
    Caption = 'Confirm Password:'
  end
  object lblSurveyName: TLabel
    Left = 28
    Top = 195
    Width = 67
    Height = 13
    Caption = 'Survey Name:'
    Enabled = False
  end
  object lblTerms: TLabel
    Left = 16
    Top = 228
    Width = 365
    Height = 26
    Caption = 
      'Please tick the checkbox below to agree to the following licence' +
      ' agreements for the use of the maps installed in Recorder:'
    WordWrap = True
  end
  object btnOk: TBitBtn
    Left = 236
    Top = 441
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    TabOrder = 8
    OnClick = btnOkClick
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
  object btnCancel: TBitBtn
    Left = 318
    Top = 441
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
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
  object eFirstname: TEdit
    Left = 120
    Top = 52
    Width = 177
    Height = 21
    MaxLength = 20
    TabOrder = 0
  end
  object eSurname: TEdit
    Left = 120
    Top = 80
    Width = 177
    Height = 21
    MaxLength = 30
    TabOrder = 1
  end
  object ePassword: TEdit
    Left = 120
    Top = 108
    Width = 177
    Height = 21
    MaxLength = 20
    PasswordChar = '*'
    TabOrder = 2
  end
  object eConfirmPassword: TEdit
    Left = 120
    Top = 136
    Width = 177
    Height = 21
    MaxLength = 20
    PasswordChar = '*'
    TabOrder = 3
  end
  object chkCreateSurvey: TCheckBox
    Left = 16
    Top = 172
    Width = 193
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Automatically create the first survey:'
    TabOrder = 4
    OnClick = chkCreateSurveyClick
  end
  object eSurveyName: TEdit
    Left = 100
    Top = 192
    Width = 281
    Height = 21
    Enabled = False
    MaxLength = 100
    TabOrder = 5
  end
  object chkAcceptTerms: TCheckBox
    Left = 16
    Top = 408
    Width = 193
    Height = 17
    Alignment = taLeftJustify
    Caption = 'I accept these terms and conditions:'
    TabOrder = 7
    OnClick = chkAcceptTermsClick
  end
  object mmTerms: TMemo
    Left = 16
    Top = 256
    Width = 365
    Height = 149
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 6
  end
end
