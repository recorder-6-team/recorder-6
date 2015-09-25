object dlgPolygonLayerDetails: TdlgPolygonLayerDetails
  Left = 472
  Top = 270
  BorderStyle = bsDialog
  Caption = 'Polygon Layer Display Details'
  ClientHeight = 179
  ClientWidth = 222
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
  object Bevel1: TBevel
    Left = 4
    Top = 8
    Width = 213
    Height = 133
    Shape = bsFrame
  end
  object Label9: TLabel
    Left = 16
    Top = 112
    Width = 90
    Height = 13
    Caption = 'Unselected Colour:'
  end
  object lblPattern: TLabel
    Left = 16
    Top = 48
    Width = 37
    Height = 13
    Caption = 'Pattern:'
  end
  object Label8: TLabel
    Left = 16
    Top = 80
    Width = 78
    Height = 13
    Caption = 'Selected Colour:'
  end
  object Label1: TLabel
    Left = 16
    Top = 20
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object btnOk: TImageListButton
    Left = 58
    Top = 148
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btnOKClick
    Down = False
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object btnCancel: TImageListButton
    Left = 142
    Top = 148
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
    Down = False
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object cmbPattern: TComboBox
    Left = 56
    Top = 44
    Width = 149
    Height = 22
    Style = csOwnerDrawFixed
    ItemHeight = 16
    TabOrder = 1
    OnDrawItem = cmbPatternDrawItem
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8')
  end
  object cbSelected: TColorButton
    Left = 164
    Top = 76
    Width = 41
    Height = 20
    ActiveColor = clRed
    TabOrder = 2
    TabStop = True
  end
  object cbUnselected: TColorButton
    Left = 164
    Top = 108
    Width = 41
    Height = 20
    ActiveColor = clBlue
    TabOrder = 3
    TabStop = True
    OnChange = cbUnselectedChange
  end
  object eName: TEdit
    Left = 56
    Top = 16
    Width = 149
    Height = 21
    MaxLength = 20
    TabOrder = 0
    Text = '<new polygon layer>'
  end
end
