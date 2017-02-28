object dlgMapSelection: TdlgMapSelection
  Left = 408
  Top = 277
  Width = 385
  Height = 265
  BorderIcons = [biSystemMenu]
  Caption = 'Map Selection'
  Color = clBtnFace
  Constraints.MinHeight = 265
  Constraints.MinWidth = 385
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    377
    238)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 4
    Width = 369
    Height = 189
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
  end
  object btnOk: TImageListButton
    Left = 214
    Top = 200
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object btnCancel: TImageListButton
    Left = 298
    Top = 200
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object Panel1: TPanel
    Left = 12
    Top = 8
    Width = 353
    Height = 179
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 2
    object lblInstruct: TLabel
      Left = 0
      Top = 0
      Width = 353
      Height = 26
      Align = alTop
      Caption = 
        'The system is unable to determine a specific map to use for this' +
        ' operation. Please select a map from the ones available listed b' +
        'elow.'
      WordWrap = True
    end
    object Panel2: TPanel
      Left = 0
      Top = 26
      Width = 353
      Height = 153
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        353
        153)
      object Label1: TLabel
        Left = 0
        Top = 8
        Width = 74
        Height = 13
        Caption = 'Available maps:'
      end
      object Label2: TLabel
        Left = 203
        Top = 8
        Width = 81
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Contained layers:'
      end
      object lbMaps: TListBox
        Left = 0
        Top = 20
        Width = 188
        Height = 132
        Style = lbOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 18
        TabOrder = 0
        OnClick = lbMapsClick
        OnDrawItem = lbMapsDrawItem
      end
      object lbLayers: TListBox
        Left = 203
        Top = 20
        Width = 146
        Height = 132
        Style = lbOwnerDrawFixed
        Anchors = [akTop, akRight, akBottom]
        Color = clBtnFace
        ItemHeight = 18
        TabOrder = 1
        OnDrawItem = lbLayersDrawItem
      end
    end
  end
end
