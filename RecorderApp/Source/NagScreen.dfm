object dlgNagScreen: TdlgNagScreen
  Left = 379
  Top = 181
  BorderStyle = bsDialog
  Caption = 'Important Information'
  ClientHeight = 267
  ClientWidth = 405
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 4
    Width = 397
    Height = 225
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 32
    Top = 20
    Width = 329
    Height = 48
    Caption = 
      'You currently have both Recorder 6 and Recorder 2002 installed o' +
      'n this machine. This is only for evaluation purposes.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 32
    Top = 76
    Width = 321
    Height = 48
    Caption = 
      'YOU MUST NOT ENTER LIVE DATA INTO BOTH SYSTEMS SINCE YOU CANNOT ' +
      'MERGE THE TWO DATABASES.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label3: TLabel
    Left = 32
    Top = 132
    Width = 345
    Height = 80
    Caption = 
      'Transferring data from Recorder 2002 overwrites any data that ha' +
      's been put in to Recorder 6. Once you are satisfied that Recorde' +
      'r 6 has been installed correctly and your data has been successf' +
      'ully transfered, then you should UNINSTALL RECORDER 2002!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object btnOk: TBitBtn
    Left = 168
    Top = 236
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
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
end
