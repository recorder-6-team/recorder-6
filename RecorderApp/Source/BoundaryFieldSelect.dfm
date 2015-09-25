object dlgBoundaryFieldSelect: TdlgBoundaryFieldSelect
  Left = 682
  Top = 276
  BorderStyle = bsDialog
  Caption = 'Boundary Field Select'
  ClientHeight = 68
  ClientWidth = 293
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    293
    68)
  PixelsPerInch = 96
  TextHeight = 13
  object lblBoundaryAttribute: TLabel
    Left = 8
    Top = 12
    Width = 115
    Height = 13
    Caption = 'Boundary Attribute Field:'
  end
  object cmbBoundaryAttribute: TComboBox
    Left = 128
    Top = 8
    Width = 159
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnCancel: TImageListButton
    Left = 212
    Top = 37
    Width = 75
    Height = 25
    Hint = 'Cancel changes'
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object btnOK: TImageListButton
    Left = 128
    Top = 37
    Width = 75
    Height = 25
    Hint = 'Select this boundary attribute field'
    Anchors = [akTop, akRight]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
end
