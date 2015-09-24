object dlgPrintTitle: TdlgPrintTitle
  Left = 395
  Top = 248
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Title'
  ClientHeight = 133
  ClientWidth = 399
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
  object bvlPrintTitle: TBevel
    Left = 4
    Top = 8
    Width = 389
    Height = 81
    Shape = bsFrame
  end
  object lblPrintTiltle: TLabel
    Left = 16
    Top = 24
    Width = 152
    Height = 13
    Caption = 'Enter a title for the Map Print-out'
  end
  object bbOK: TImageListButton
    Left = 316
    Top = 100
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = bbOkClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object ePrintTitle: TEdit
    Left = 16
    Top = 48
    Width = 369
    Height = 21
    MaxLength = 36
    TabOrder = 0
  end
end
