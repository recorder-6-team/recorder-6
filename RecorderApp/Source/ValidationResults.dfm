object dlgValidationResults: TdlgValidationResults
  Left = 335
  Top = 299
  Width = 544
  Height = 291
  BorderIcons = [biSystemMenu]
  Caption = 'Database Validation Results'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  DesignSize = (
    536
    264)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 519
    Height = 209
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
  end
  object lblSummary: TLabel
    Left = 20
    Top = 20
    Width = 53
    Height = 13
    Caption = 'lblSummary'
  end
  object btnClose: TImageListButton
    Left = 418
    Top = 224
    Width = 109
    Height = 25
    Hint = 'Close'
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'View Results'
    ModalResult = 2
    TabOrder = 2
  end
  object btnSave: TImageListButton
    Left = 298
    Top = 224
    Width = 109
    Height = 25
    Hint = 'Save'
    Anchors = [akRight, akBottom]
    Caption = 'Save Details'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnSaveClick
    ImageList = dmFormActions.ilMenuOn
    ImageIndex = 0
  end
  object mmDetails: TMemo
    Left = 20
    Top = 40
    Width = 499
    Height = 165
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object dlgSaveDetails: TSaveDialog
    DefaultExt = 'ref'
    Filter = 'Recorder External Filter (*.ref)|*.ref'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 144
    Top = 72
  end
end
