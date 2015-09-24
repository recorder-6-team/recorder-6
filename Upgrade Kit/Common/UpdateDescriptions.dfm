object frmUpdateDescriptions: TfrmUpdateDescriptions
  Left = 262
  Top = 251
  Width = 607
  Height = 410
  Caption = 'Updates Applied'
  Color = clBtnFace
  Constraints.MinHeight = 190
  Constraints.MinWidth = 440
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButton: TPanel
    Left = 0
    Top = 350
    Width = 599
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      599
      33)
    object btnClose: TButton
      Left = 519
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Close'
      ModalResult = 1
      TabOrder = 0
    end
  end
  object mmDescriptions: TMemo
    Left = 0
    Top = 25
    Width = 599
    Height = 325
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 599
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object lblUpgradeApplied: TLabel
      Left = 8
      Top = 8
      Width = 410
      Height = 13
      Caption = 
        'The upgrade you have applied contains the following database and' +
        ' dictionary updates:'
    end
  end
end
