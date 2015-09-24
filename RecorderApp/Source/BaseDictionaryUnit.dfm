inherited BaseDictionary: TBaseDictionary
  Left = 461
  Top = 325
  Width = 563
  Height = 412
  ActiveControl = tvDictionary
  Caption = 'BaseDictionary'
  Constraints.MinHeight = 140
  Constraints.MinWidth = 360
  OldCreateOrder = True
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DictSplitter: TSplitter [0]
    Left = 360
    Top = 29
    Width = 10
    Height = 356
    Align = alRight
    ResizeStyle = rsUpdate
    OnMoved = FormResize
    OnPaint = DictSplitterPaint
  end
  object pnlSelection: TPanel [1]
    Left = 0
    Top = 0
    Width = 555
    Height = 29
    Align = alTop
    TabOrder = 0
    DesignSize = (
      555
      29)
    object lblListName: TLabel
      Left = 8
      Top = 8
      Width = 64
      Height = 13
      Caption = 'Classification:'
    end
    object cmbList: TComboBox
      Left = 80
      Top = 4
      Width = 380
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbListChange
    end
    object btnShowAll: TButton
      Left = 471
      Top = 2
      Width = 75
      Height = 25
      Hint = 'Remove Filter'
      Anchors = [akTop, akRight]
      Caption = '&Show All'
      TabOrder = 1
      Visible = False
      OnClick = btnShowAllClick
    end
  end
  object pcBrowser: TPageControl [2]
    Left = 0
    Top = 29
    Width = 360
    Height = 356
    ActivePage = tsTree
    Align = alClient
    TabOrder = 1
    TabStop = False
    object tsTree: TTabSheet
      Tag = 1
      BorderWidth = 1
      Caption = 'Hierarchy'
      TabVisible = False
      object tvDictionary: TKeyboardRapidTree
        Left = 0
        Top = 0
        Width = 350
        Height = 344
        SmoothExpandCollapse = False
        FitColumnToClientWidth = True
        OutlineColor = clBtnShadow
        FitToHeight = False
        DoubleBuffered = False
        Align = alClient
        TransparentMode = False
        DefaultRowHeight = 16
        RowCount = 0
        FixedRows = 0
        GridLineWidth = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
        TabOrder = 0
        OnDrawCell = tvDictionaryDrawCell
        OnExpanding = tvDictionaryExpanding
        OnCollapsing = tvDictionaryCollapsing
        OnCollapsed = tvDictionaryCollapsed
        OnChange = tvDictionaryChange
        OnCompare = tvDictionaryCompare
        HideSelection = False
        Showlines = True
        ShowRoot = True
        ShowButtons = True
        ShowImages = True
        ShowLogic = True
        SortType = stNone
        WordWrap = False
        AutoMove = False
        ToolTips = False
        AutoExpand = False
        TooltipColor = clInfoBk
        ToolTipPause = 1000
        StatesDrawed = True
        Data = {0400000000000000}
      end
    end
  end
  object pnlDetails: TPanel [3]
    Left = 370
    Top = 29
    Width = 185
    Height = 356
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
  end
end
