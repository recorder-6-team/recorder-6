object frmMapBrowser: TfrmMapBrowser
  Left = 587
  Top = 515
  Width = 265
  Height = 321
  BorderStyle = bsSizeToolWin
  Caption = 'Map Navigator Tool'
  Color = clBtnFace
  Constraints.MinHeight = 170
  Constraints.MinWidth = 258
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Visible = True
  OnClose = FormClose
  DesignSize = (
    257
    287)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TBevel
    Left = 4
    Top = 32
    Width = 246
    Height = 5
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object lblLayer: TLabel
    Left = 8
    Top = 44
    Width = 29
    Height = 13
    Caption = 'Layer:'
  end
  object lblMap: TLabel
    Left = 8
    Top = 8
    Width = 24
    Height = 13
    Caption = 'Map:'
  end
  object cmbLayers: TComboBox
    Left = 40
    Top = 40
    Width = 210
    Height = 22
    Style = csOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    Sorted = True
    TabOrder = 2
    OnChange = cmbLayersChange
    OnDrawItem = cmbLayersDrawItem
  end
  object lbPolygons: TListBox
    Left = 8
    Top = 68
    Width = 242
    Height = 185
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 16
    Sorted = True
    TabOrder = 3
    OnClick = lbPolygonsClick
    OnDrawItem = lbPolygonsDrawItem
  end
  object btnCentre: TImageListButton
    Left = 8
    Top = 258
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Centre'
    TabOrder = 4
    OnClick = btnCentreClick
    ImageList = dmFormActions.ilMenuOn
    ImageIndex = 24
    Spacing = -1
  end
  object btnZoom: TImageListButton
    Left = 88
    Top = 258
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Zoom'
    TabOrder = 5
    OnClick = btnZoomClick
    ImageList = dmFormActions.ilMenuOn
    ImageIndex = 25
    Spacing = -1
  end
  object cmbMaps: TComboBox
    Left = 40
    Top = 4
    Width = 186
    Height = 22
    Style = csOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    Sorted = True
    TabOrder = 0
    OnChange = cmbMapsChange
    OnDrawItem = cmbMapsDrawItem
  end
  object btnGo: TImageListButton
    Left = 225
    Top = 4
    Width = 24
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 1
    OnClick = btnGoClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 5
  end
  object btnSelect: TImageListButton
    Left = 168
    Top = 258
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Select'
    TabOrder = 6
    OnClick = btnSelectClick
    ImageList = dmFormActions.ilMenuOn
    ImageIndex = 23
  end
end
