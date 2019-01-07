object dlgAbout: TdlgAbout
  Left = 275
  Top = 300
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 320
  ClientWidth = 413
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    413
    320)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TImageListButton
    Left = 332
    Top = 288
    Width = 75
    Height = 23
    Anchors = [akTop, akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object pcAbout: TPageControl
    Left = 4
    Top = 8
    Width = 403
    Height = 271
    ActivePage = tsVersion
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsVersion: TTabSheet
      Caption = 'Version'
      object Bevel2: TBevel
        Left = 4
        Top = 139
        Width = 389
        Height = 9
        Shape = bsTopLine
      end
      object Label1: TLabel
        Left = 172
        Top = 8
        Width = 51
        Height = 16
        Caption = 'Version:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Version: TLabel
        Left = 250
        Top = 8
        Width = 47
        Height = 16
        Caption = 'Version'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label4: TLabel
        Left = 172
        Top = 28
        Width = 16
        Height = 16
        Caption = 'of:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lDate: TLabel
        Left = 250
        Top = 28
        Width = 33
        Height = 16
        Caption = 'lDate'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 8
        Top = 151
        Width = 84
        Height = 13
        Caption = 'Operating system:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object OS: TLabel
        Left = 96
        Top = 151
        Width = 15
        Height = 13
        Caption = 'OS'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label3: TLabel
        Left = 8
        Top = 169
        Width = 40
        Height = 13
        Caption = 'Memory:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object PhysMem: TLabel
        Left = 96
        Top = 169
        Width = 46
        Height = 13
        Caption = 'PhysMem'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label6: TLabel
        Left = 8
        Top = 119
        Width = 136
        Height = 14
        Caption = 'Based on Recorder 6 v6.28 '
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 8
        Top = 79
        Width = 134
        Height = 14
        Caption = 'Developed by Biodiverse IT '
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Image: TImage
        Left = 8
        Top = 8
        Width = 81
        Height = 73
      end
      object Label7: TLabel
        Left = 8
        Top = 187
        Width = 69
        Height = 13
        Caption = 'Map Software:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object MapServer: TLabel
        Left = 96
        Top = 187
        Width = 92
        Height = 13
        Caption = 'MapServer 5 Turbo'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label8: TLabel
        Left = 8
        Top = 223
        Width = 51
        Height = 13
        Caption = 'Username:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblUsername: TLabel
        Left = 96
        Top = 223
        Width = 50
        Height = 13
        Caption = 'UserName'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblDictionaryVersionCaption: TLabel
        Left = 172
        Top = 56
        Width = 64
        Height = 16
        Caption = 'Dictionary'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblDictionaryVersion: TLabel
        Left = 250
        Top = 57
        Width = 113
        Height = 16
        Caption = 'WWWWWWWW'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblDictionaryVersionCaption2: TLabel
        Left = 172
        Top = 72
        Width = 51
        Height = 16
        Caption = 'Version:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label9: TLabel
        Left = 172
        Top = 96
        Width = 59
        Height = 16
        Caption = 'Database'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label10: TLabel
        Left = 172
        Top = 112
        Width = 51
        Height = 16
        Caption = 'Version:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblDatabaseVersion: TLabel
        Left = 250
        Top = 97
        Width = 113
        Height = 16
        Caption = 'WWWWWWWW'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label12: TLabel
        Left = 96
        Top = 205
        Width = 295
        Height = 13
        Caption = 'Shape Viewer Objects GIS from  Ecological Software Solutions'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object StaticText1: TStaticText
        Left = 8
        Top = 94
        Width = 133
        Height = 18
        Caption = 'and Littlefield Consultancy '
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    object tsMoreInfo: TTabSheet
      Caption = 'More'
      object mmMoreInfo: TMemo
        Left = 0
        Top = 0
        Width = 395
        Height = 243
        Align = alClient
        Color = clBtnFace
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
end
