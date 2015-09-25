inherited fraImportAnalysis: TfraImportAnalysis
  Width = 560
  Height = 479
  object Panel1: TPanel
    Left = 0
    Top = 137
    Width = 560
    Height = 19
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
  end
  object pnlDuplicateTab: TPanel
    Left = 0
    Top = 137
    Width = 97
    Height = 20
    BevelOuter = bvNone
    Caption = 'Duplicate Items'
    Ctl3D = False
    ParentColor = True
    ParentCtl3D = False
    TabOrder = 2
    OnClick = pnlDuplicateTabClick
    object Shape1: TShape
      Left = 0
      Top = 0
      Width = 97
      Height = 1
      Align = alTop
    end
    object Shape2: TShape
      Left = 0
      Top = 1
      Width = 1
      Height = 19
      Align = alLeft
    end
    object Shape3: TShape
      Left = 96
      Top = 1
      Width = 1
      Height = 19
      Align = alRight
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 156
    Width = 560
    Height = 323
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    Ctl3D = False
    ParentColor = True
    ParentCtl3D = False
    TabOrder = 1
    object pcValidation: TPageControl
      Left = 1
      Top = 1
      Width = 556
      Height = 319
      ActivePage = tsDuplicateItems
      Align = alClient
      Style = tsButtons
      TabOrder = 0
      object tsDuplicateItems: TTabSheet
        Caption = 'Duplicate Items'
        TabVisible = False
        object Panel6: TPanel
          Left = 0
          Top = 0
          Width = 548
          Height = 268
          Align = alClient
          BevelOuter = bvNone
          Caption = 'Panel6'
          Color = clWhite
          TabOrder = 0
          object SplitDuplicateImported: TSplitter
            Left = 204
            Top = 0
            Width = 4
            Height = 268
            Align = alRight
            MinSize = 100
            ResizeStyle = rsUpdate
            OnMoved = SplitDuplicateImportedMoved
          end
          object SplitImportedOriginal: TSplitter
            Left = 376
            Top = 0
            Width = 4
            Height = 268
            Align = alRight
            MinSize = 100
            ResizeStyle = rsUpdate
          end
          object pnlDuplicateDataTree: TPanel
            Left = 0
            Top = 0
            Width = 204
            Height = 268
            Align = alClient
            BevelOuter = bvNone
            Constraints.MinWidth = 30
            ParentColor = True
            TabOrder = 2
            OnResize = DuplicateItemsResize
            object Label3: TLabel
              Left = 0
              Top = 0
              Width = 204
              Height = 17
              Align = alTop
              AutoSize = False
              Caption = 'Duplicate Data:'
            end
          end
          object pnlOriginalDataTree: TPanel
            Left = 380
            Top = 0
            Width = 168
            Height = 268
            Align = alRight
            BevelOuter = bvNone
            Caption = 'pnlOriginalDataTree'
            ParentColor = True
            TabOrder = 0
            OnResize = DuplicateItemsResize
            object Label1: TLabel
              Left = 0
              Top = 0
              Width = 168
              Height = 17
              Align = alTop
              AutoSize = False
              Caption = 'Original Data:'
            end
            object tvOriginalData: TRapidTree
              Left = 0
              Top = 17
              Width = 168
              Height = 251
              SmoothExpandCollapse = False
              FitColumnToClientWidth = True
              FitToHeight = False
              DoubleBuffered = False
              Align = alClient
              Ctl3D = False
              Constraints.MinWidth = 100
              TransparentMode = False
              DefaultRowHeight = 18
              RowCount = 0
              FixedRows = 0
              GridLineWidth = 0
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
              ParentCtl3D = False
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              Indent = 18
              HideSelection = False
              Showlines = True
              ShowRoot = True
              ShowButtons = True
              ShowImages = False
              ShowLogic = False
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
          object pnlImportedDataTree: TPanel
            Left = 208
            Top = 0
            Width = 168
            Height = 268
            Align = alRight
            BevelOuter = bvNone
            Caption = 'pnlImportedDataTree'
            ParentColor = True
            TabOrder = 1
            OnResize = DuplicateItemsResize
            object Label2: TLabel
              Left = 0
              Top = 0
              Width = 168
              Height = 17
              Align = alTop
              AutoSize = False
              Caption = 'Imported Data:'
            end
            object tvImportedData: TRapidTree
              Left = 0
              Top = 17
              Width = 168
              Height = 251
              SmoothExpandCollapse = False
              FitColumnToClientWidth = True
              FitToHeight = False
              DoubleBuffered = False
              Align = alClient
              Ctl3D = False
              Constraints.MinWidth = 100
              TransparentMode = False
              DefaultRowHeight = 18
              RowCount = 0
              FixedRows = 0
              GridLineWidth = 0
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
              ParentCtl3D = False
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              Indent = 18
              HideSelection = False
              Showlines = True
              ShowRoot = True
              ShowButtons = True
              ShowImages = False
              ShowLogic = False
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
        object Panel5: TPanel
          Left = 0
          Top = 268
          Width = 548
          Height = 41
          Align = alBottom
          BevelOuter = bvNone
          Color = clWhite
          TabOrder = 1
          DesignSize = (
            548
            41)
          object btnAllImported: TButton
            Left = 222
            Top = 9
            Width = 97
            Height = 23
            Anchors = [akLeft, akBottom]
            Caption = 'All &Imported'
            TabOrder = 0
            OnClick = btnAllImportedClick
          end
          object btnReject: TButton
            Left = 118
            Top = 9
            Width = 97
            Height = 23
            Anchors = [akLeft, akBottom]
            Caption = '&Reject Import'
            Enabled = False
            TabOrder = 1
            OnClick = btnRejectClick
          end
          object btnAccept: TButton
            Left = 13
            Top = 9
            Width = 97
            Height = 23
            Anchors = [akLeft, akBottom]
            Caption = '&Accept Import'
            Enabled = False
            TabOrder = 2
            OnClick = btnAcceptClick
          end
          object btnAllOriginal: TButton
            Left = 326
            Top = 9
            Width = 97
            Height = 23
            Anchors = [akLeft, akBottom]
            Caption = 'All &Original'
            TabOrder = 3
            OnClick = btnAllOriginalClick
          end
          object btnAllLatest: TButton
            Left = 430
            Top = 9
            Width = 97
            Height = 23
            Anchors = [akLeft, akBottom]
            Caption = 'All &Latest'
            TabOrder = 4
            OnClick = btnAllLatestClick
          end
        end
      end
      object tsInvalidItems: TTabSheet
        Caption = 'Invalid Items'
        ImageIndex = 1
        TabVisible = False
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 548
          Height = 309
          Align = alClient
          BevelOuter = bvNone
          Caption = 'Panel3'
          Color = clWhite
          TabOrder = 0
          object SplitInvalidData: TSplitter
            Left = 201
            Top = 0
            Height = 309
            Align = alRight
            MinSize = 100
            ResizeStyle = rsUpdate
          end
          object pnlInvalidDataTree: TPanel
            Left = 0
            Top = 0
            Width = 201
            Height = 309
            Align = alClient
            BevelOuter = bvNone
            Caption = 'pnlInvalidDataTree'
            Color = clWhite
            Constraints.MinWidth = 100
            TabOrder = 0
            OnResize = InvalidItemsResize
            object lblInvalidData: TLabel
              Left = 0
              Top = 0
              Width = 57
              Height = 13
              Align = alTop
              Caption = 'Invalid Data'
            end
            object tvInvalidData: TRapidTree
              Left = 0
              Top = 13
              Width = 201
              Height = 296
              SmoothExpandCollapse = False
              FitColumnToClientWidth = True
              FitToHeight = False
              DoubleBuffered = False
              Align = alClient
              Ctl3D = False
              Constraints.MinWidth = 100
              TransparentMode = False
              DefaultRowHeight = 18
              RowCount = 0
              FixedRows = 0
              GridLineWidth = 0
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
              ParentCtl3D = False
              TabOrder = 0
              OnChange = tvInvalidDataChange
              Indent = 18
              HideSelection = False
              Showlines = True
              ShowRoot = True
              ShowButtons = True
              ShowImages = False
              ShowLogic = False
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
          object pnlInvalidDataDetails: TPanel
            Left = 204
            Top = 0
            Width = 344
            Height = 309
            Align = alRight
            BevelOuter = bvNone
            Color = clWhite
            Constraints.MinWidth = 100
            TabOrder = 1
            OnResize = InvalidItemsResize
            object lblInvalidDetails: TLabel
              Left = 0
              Top = 0
              Width = 35
              Height = 13
              Align = alTop
              Caption = 'Details:'
            end
            object SplitInvalidDetails: TSplitter
              Left = 0
              Top = 149
              Width = 344
              Height = 3
              Cursor = crVSplit
              Align = alTop
              MinSize = 100
            end
            object mmInvalidDetails: TMemo
              Left = 0
              Top = 13
              Width = 344
              Height = 136
              Align = alTop
              Constraints.MinHeight = 100
              Constraints.MinWidth = 100
              Ctl3D = False
              ParentCtl3D = False
              ReadOnly = True
              ScrollBars = ssVertical
              TabOrder = 0
            end
            object reXML: TRichEdit
              Left = 0
              Top = 152
              Width = 344
              Height = 124
              Align = alClient
              BevelEdges = []
              BevelInner = bvNone
              BevelOuter = bvNone
              BorderWidth = 1
              Ctl3D = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -9
              Font.Name = 'Small Fonts'
              Font.Style = []
              Constraints.MinHeight = 100
              Constraints.MinWidth = 100
              ParentCtl3D = False
              ParentFont = False
              ReadOnly = True
              ScrollBars = ssBoth
              TabOrder = 1
              WordWrap = False
            end
            object pnlSaveInvalidData: TPanel
              Left = 0
              Top = 276
              Width = 344
              Height = 33
              Align = alBottom
              BevelOuter = bvNone
              Color = clWhite
              TabOrder = 2
              DesignSize = (
                344
                33)
              object btnSaveDetails: TImageListButton
                Left = 244
                Top = 4
                Width = 99
                Height = 25
                Anchors = [akTop, akRight]
                Caption = '&Save Details'
                TabOrder = 0
                OnClick = btnSaveDetailsClick
                ImageList = dmFormActions.ilMenuOn
                ImageIndex = 0
              end
            end
          end
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 560
    Height = 137
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel2'
    ParentColor = True
    TabOrder = 4
    DesignSize = (
      560
      137)
    object lblImportDetails: TLabel
      Left = 8
      Top = 12
      Width = 76
      Height = 13
      Caption = 'Analysis Details:'
    end
    object mmDetails: TMemo
      Left = 88
      Top = 8
      Width = 472
      Height = 122
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object pnlInvalidTab: TPanel
    Left = 98
    Top = 138
    Width = 97
    Height = 18
    BevelOuter = bvNone
    Caption = 'Invalid Items'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 3
    OnClick = pnlInvalidTabClick
    object Shape4: TShape
      Left = 0
      Top = 0
      Width = 97
      Height = 1
      Align = alTop
    end
    object Shape5: TShape
      Left = 0
      Top = 1
      Width = 1
      Height = 17
      Align = alLeft
    end
    object Shape6: TShape
      Left = 96
      Top = 1
      Width = 1
      Height = 17
      Align = alRight
    end
  end
  object dlgSaveDetails: TSaveDialog
    DefaultExt = '.ref'
    Filter = 'Recorder External Filter (*.ref)|*.ref'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 492
    Top = 344
  end
end
