inherited dlgContainer: TdlgContainer
  Left = 259
  Top = 128
  BorderStyle = bsDialog
  Caption = 'dlgContainer'
  ClientHeight = 310
  ClientWidth = 483
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 276
    Width = 483
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object bbCancel: TImageListButton
      Left = 400
      Top = 4
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = bbCancelClick
      Down = False
      ImageList = dmFormActions.ilButtons
      ImageIndex = 1
    end
    object bbOk: TImageListButton
      Left = 316
      Top = 4
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = bbOkClick
      Down = False
      ImageList = dmFormActions.ilButtons
      ImageIndex = 0
    end
  end
end
