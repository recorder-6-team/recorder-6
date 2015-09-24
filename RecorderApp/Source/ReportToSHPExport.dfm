object dlgReportToSHPExport: TdlgReportToSHPExport
  Left = 625
  Top = 338
  BorderStyle = bsDialog
  Caption = 'ESRI Shape File Export'
  ClientHeight = 304
  ClientWidth = 235
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
  object Label1: TLabel
    Left = 12
    Top = 8
    Width = 202
    Height = 13
    Caption = 'Select the spatial reference system to use: '
  end
  object Label2: TLabel
    Left = 12
    Top = 52
    Width = 207
    Height = 26
    Caption = 'Select the attributes to include in the shape file:'
    WordWrap = True
  end
  object cmbReferenceSystem: TComboBox
    Left = 8
    Top = 24
    Width = 217
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object lbAttributeFields: TCheckListBox
    Left = 8
    Top = 80
    Width = 217
    Height = 185
    ItemHeight = 13
    TabOrder = 1
  end
  object btnOK: TImageListButton
    Left = 68
    Top = 272
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = btnOKClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object btnCancel: TImageListButton
    Left = 152
    Top = 272
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = btnCancelClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object dlgSaveSHP: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 176
    Top = 216
  end
end
