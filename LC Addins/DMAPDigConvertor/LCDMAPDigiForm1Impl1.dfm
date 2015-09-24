object LCDMAPDigiForm1: TLCDMAPDigiForm1
  Left = 884
  Top = 261
  Width = 303
  Height = 372
  Caption = 'LCDMAPDigiForm1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 56
    Width = 111
    Height = 13
    Caption = 'File From Digitizer [.bdy)'
  end
  object Label2: TLabel
    Left = 24
    Top = 80
    Width = 145
    Height = 17
    AutoSize = False
    Color = clYellow
    ParentColor = False
  end
  object Label3: TLabel
    Left = 24
    Top = 120
    Width = 54
    Height = 13
    Caption = 'Output File '
  end
  object Label4: TLabel
    Left = 24
    Top = 136
    Width = 145
    Height = 17
    AutoSize = False
    Color = clYellow
    ParentColor = False
  end
  object Label5: TLabel
    Left = 32
    Top = 200
    Width = 229
    Height = 78
    Caption = 
      'Browse to choose the file to process and the file to output. Not' +
      'e that the process is only designed to work on .bdy files genera' +
      'ted by the DMAP digitizer. Results from other DMAP boundary file' +
      's are unilkely to be useable. Click on OK to process the file. '
    WordWrap = True
  end
  object Button1: TButton
    Left = 192
    Top = 80
    Width = 65
    Height = 17
    Caption = 'Browse'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 192
    Top = 136
    Width = 65
    Height = 17
    Caption = 'Browse'
    TabOrder = 1
    OnClick = Button2Click
  end
  object OpenDialog1: TOpenDialog
    Filter = '*.txt'
    Left = 16
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Filter = '*.ruk'
    Left = 48
    Top = 8
  end
end
