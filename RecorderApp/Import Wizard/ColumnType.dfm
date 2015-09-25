object dlgColumnType: TdlgColumnType
  Left = 613
  Top = 380
  Width = 321
  Height = 296
  Caption = 'Column Type Selection'
  Color = clBtnFace
  Constraints.MinHeight = 100
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object tvTypes: TRapidTree
    Left = 0
    Top = 0
    Width = 313
    Height = 233
    SmoothExpandCollapse = False
    FitColumnToClientWidth = True
    FitToHeight = False
    DoubleBuffered = False
    Align = alClient
    Ctl3D = True
    TransparentMode = False
    DefaultRowHeight = 18
    RowCount = 0
    FixedRows = 0
    GridLineWidth = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    ParentCtl3D = False
    TabOrder = 0
    OnDblClick = tvTypesDblClick
    Indent = 18
    HideSelection = False
    Showlines = True
    ShowRoot = True
    ShowButtons = True
    ShowImages = False
    ShowLogic = False
    SortType = stText
    WordWrap = False
    AutoMove = False
    ToolTips = False
    AutoExpand = False
    TooltipColor = clInfoBk
    ToolTipPause = 1000
    StatesDrawed = True
    Data = {0400000000000000}
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 233
    Width = 313
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlButtonsAlign: TPanel
      Left = 139
      Top = 0
      Width = 174
      Height = 36
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnCancel: TImageListButton
        Left = 92
        Top = 6
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
        ImageList = dmFormActions.ilButtons
        ImageIndex = 1
      end
      object btnOk: TImageListButton
        Left = 8
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Ok'
        Default = True
        ModalResult = 1
        TabOrder = 0
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
      end
    end
  end
end
