object dlgDeleteDatasets: TdlgDeleteDatasets
  Left = 310
  Top = 184
  BorderStyle = bsDialog
  Caption = 'Map Datasets'
  ClientHeight = 256
  ClientWidth = 289
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblDatasets: TLabel
    Left = 8
    Top = 8
    Width = 244
    Height = 39
    Caption = 
      'There are already 10 datasets present on the map.  Please select' +
      ' a dataset to remove before additional datasets can be added:'
    WordWrap = True
  end
  object bbCancel: TImageListButton
    Left = 208
    Top = 224
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    Down = False
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbOK: TImageListButton
    Left = 120
    Top = 224
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    Down = False
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object lbDatasets: TListBox
    Left = 8
    Top = 52
    Width = 273
    Height = 165
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbDatasetsClick
  end
end
