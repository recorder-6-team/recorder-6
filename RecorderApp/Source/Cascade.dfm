object dlgCascade: TdlgCascade
  Left = 355
  Top = 664
  BorderStyle = bsDialog
  Caption = 'Cascade Event Changes'
  ClientHeight = 227
  ClientWidth = 490
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
  object pnlChoose: TGroupBox
    Left = 5
    Top = 0
    Width = 481
    Height = 193
    TabOrder = 0
    object rbDoNothing: TRadioButton
      Left = 24
      Top = 8
      Width = 425
      Height = 49
      TabOrder = 0
      OnClick = RadioButtonClick
    end
    object rbCascadeAll: TRadioButton
      Left = 24
      Top = 54
      Width = 425
      Height = 49
      TabOrder = 1
      WordWrap = True
      OnClick = RadioButtonClick
    end
    object rbCascadeEqual: TRadioButton
      Left = 24
      Top = 100
      Width = 425
      Height = 49
      TabOrder = 2
      WordWrap = True
      OnClick = RadioButtonClick
    end
    object ckbUpdateSamples: TCheckBox
      Left = 24
      Top = 160
      Width = 449
      Height = 25
      Caption = 
        'Also update all samples and determinations within the same surve' +
        'y event using the same rules'
      Color = clBtnFace
      Enabled = False
      ParentColor = False
      TabOrder = 3
      WordWrap = True
    end
  end
  object bbOK: TImageListButton
    Left = 411
    Top = 197
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = bbOKClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
end
