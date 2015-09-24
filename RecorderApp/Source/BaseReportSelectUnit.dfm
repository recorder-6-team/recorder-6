object BaseReportSelect: TBaseReportSelect
  Left = 283
  Top = 334
  Width = 684
  Height = 404
  Caption = 'BaseReportSelect'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    676
    377)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 660
    Height = 333
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
  end
  object Label2: TLabel
    Left = 20
    Top = 16
    Width = 64
    Height = 13
    Caption = 'File Directory:'
  end
  object Label1: TLabel
    Left = 20
    Top = 64
    Width = 52
    Height = 13
    Caption = '&Select File:'
  end
  object pnlFiles: TPanel
    Left = 20
    Top = 80
    Width = 638
    Height = 249
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 4
    object Splitter: TSplitter
      Left = 323
      Top = 0
      Width = 5
      Height = 249
      ResizeStyle = rsUpdate
      OnMoved = SplitterMoved
    end
    object tvSelectFile: TKeyboardRapidTree
      Left = 0
      Top = 0
      Width = 323
      Height = 249
      SmoothExpandCollapse = False
      FitColumnToClientWidth = True
      FitToHeight = False
      DoubleBuffered = True
      Align = alLeft
      Constraints.MinWidth = 50
      TransparentMode = True
      DefaultRowHeight = 17
      RowCount = 1
      FixedRows = 0
      GridLineWidth = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
      TabOrder = 0
      OnDblClick = tvSelectFileDblClick
      OnChange = tvSelectFileChange
      HideSelection = False
      Showlines = True
      ShowRoot = True
      ShowButtons = True
      ShowImages = True
      ShowLogic = True
      Images = dmFormActions.ilMenuOn
      SortType = stNone
      WordWrap = False
      AutoMove = False
      ToolTips = False
      AutoExpand = False
      TooltipColor = clInfoBk
      ToolTipPause = 1000
      StatesDrawed = True
      Data = {
        9C00000001000000060854466C794E6F64658A000000060943656C6C73546578
        741300000006020D0A060648696464656E2000000000060A496D616765496E64
        65783400000000000000060D53656C6563746564496E6465784B000000000000
        00060A5374617465496E6465785F000000000000000604546578747300000006
        0854466C794E6F64650609556E6971756554616786000000FFFFFFFF00000000}
    end
    object pnlDescription: TPanel
      Left = 328
      Top = 0
      Width = 310
      Height = 249
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object rtfDescription: TRichEdit
        Left = 0
        Top = 0
        Width = 310
        Height = 249
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        Color = cl3DLight
        Ctl3D = False
        ParentCtl3D = False
        ReadOnly = True
        TabOrder = 0
      end
    end
  end
  object btnCancel: TImageListButton
    Left = 593
    Top = 347
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
  object btnOK: TImageListButton
    Left = 509
    Top = 347
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object eDirectory: TEdit
    Left = 20
    Top = 32
    Width = 613
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
    Text = 'C:\JNCC\Reports'
  end
  object btnBrowse: TButton
    Left = 634
    Top = 32
    Width = 23
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = btnBrowseClick
  end
  object dlgFolder: TFolderBrowser
    BrowseFlags = [bfDirsOnly, bfStatusText]
    NewDialogStyle = False
    Title = 'Select report folder'
    Left = 276
    Top = 32
  end
end
