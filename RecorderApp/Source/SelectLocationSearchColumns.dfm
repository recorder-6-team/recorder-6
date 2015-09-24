object dlgSelectLocationSearchColumns: TdlgSelectLocationSearchColumns
  Left = 736
  Top = 469
  BorderStyle = bsDialog
  Caption = 'Extra Location Search Columns'
  ClientHeight = 249
  ClientWidth = 265
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    265
    249)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 249
    Height = 33
    AutoSize = False
    Caption = 
      'Select the additional columns that are to be displayed when sear' +
      'ching for locations.'
    WordWrap = True
  end
  object chklbColumns: TCheckListBox
    Left = 8
    Top = 48
    Width = 249
    Height = 157
    OnClickCheck = chklbColumnsClickCheck
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object bbCancel: TImageListButton
    Left = 176
    Top = 218
    Width = 83
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbOK: TImageListButton
    Left = 83
    Top = 218
    Width = 83
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
end
