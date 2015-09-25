object dlgGoogleEarthExport: TdlgGoogleEarthExport
  Left = 625
  Top = 338
  BorderStyle = bsDialog
  Caption = 'Google Earth Export'
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
    Width = 201
    Height = 13
    Caption = 'Select the attribute to use as a marker title:'
  end
  object Label2: TLabel
    Left = 12
    Top = 52
    Width = 210
    Height = 26
    Caption = 'Select the attributes to include in the marker descriptions:'
    WordWrap = True
  end
  object cmbPlacemarkName: TComboBox
    Left = 8
    Top = 24
    Width = 217
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object clbDescriptionFields: TCheckListBox
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
  object dlgSaveKML: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 176
    Top = 216
  end
end
