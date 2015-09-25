object dlgSelectDistributionPoint: TdlgSelectDistributionPoint
  Left = 417
  Top = 207
  Width = 561
  Height = 300
  BorderIcons = [biSystemMenu]
  Caption = 'Select Distribution Point'
  Color = clBtnFace
  Constraints.MaxWidth = 561
  Constraints.MinWidth = 561
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  DesignSize = (
    553
    273)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 4
    Width = 545
    Height = 233
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
  end
  object lblPleaseSelect: TLabel
    Left = 12
    Top = 12
    Width = 217
    Height = 13
    Caption = 'Please Select the Distribution Point to Display:'
  end
  object bbCancel: TImageListButton
    Left = 472
    Top = 242
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
  object bbOK: TImageListButton
    Left = 388
    Top = 242
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object sgPoints: TStringGrid
    Left = 12
    Top = 28
    Width = 529
    Height = 201
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 6
    Ctl3D = True
    DefaultRowHeight = 21
    FixedCols = 0
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 2
    OnDblClick = sgPointsDblClick
    OnDrawCell = sgPointsDrawCell
    ColWidths = (
      79
      97
      64
      114
      64
      101)
  end
end
