object FastReportingX: TFastReportingX
  Left = 687
  Top = 209
  Width = 348
  Height = 587
  ActiveControl = lstReports
  Caption = 'Fast Reporting'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = ActiveFormDestroy
  OnDeactivate = ActiveFormDeactivate
  DesignSize = (
    340
    560)
  PixelsPerInch = 96
  TextHeight = 13
  object lblReportId: TLabel
    Left = 72
    Top = 8
    Width = 121
    Height = 17
    AutoSize = False
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 47
    Height = 13
    Caption = 'Report Id '
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 68
    Height = 13
    Caption = 'Select Report '
  end
  object Label3: TLabel
    Left = 8
    Top = 536
    Width = 321
    Height = 14
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = 'Label3'
  end
  object btnDesignReport: TButton
    Left = 6
    Top = 504
    Width = 107
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Design New Report'
    TabOrder = 0
    OnClick = btnDesignReportClick
  end
  object lstReports: TListBox
    Left = 32
    Top = 72
    Width = 265
    Height = 417
    ItemHeight = 13
    Sorted = True
    TabOrder = 1
  end
  object btnRunReport: TButton
    Left = 254
    Top = 504
    Width = 81
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Run Report'
    TabOrder = 2
    OnClick = btnRunReportClick
  end
  object cbAllReports: TCheckBox
    Left = 32
    Top = 56
    Width = 89
    Height = 17
    Caption = 'All Reports '
    TabOrder = 3
    OnClick = cbAllReportsClick
  end
  object btnDesignSelected: TButton
    Left = 126
    Top = 504
    Width = 123
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Design Selected Report'
    TabOrder = 4
    OnClick = btnDesignSelectedClick
  end
  object btnRefresh: TButton
    Left = 240
    Top = 56
    Width = 57
    Height = 17
    Caption = 'Refresh'
    TabOrder = 5
    OnClick = btnRefreshClick
  end
  object frxReport1: TfrxReport
    Version = '6.0.5'
    DotMatrixReport = False
    IniFile = '\Software\Fast Reports'
    PreviewOptions.Buttons = [pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind, pbOutline, pbPageSetup, pbTools, pbEdit, pbNavigator, pbExportQuick]
    PreviewOptions.Zoom = 1.000000000000000000
    PrintOptions.Printer = 'Default'
    PrintOptions.PrintOnSheet = 0
    ReportOptions.CreateDate = 43199.834823969900000000
    ReportOptions.LastChange = 43199.834823969900000000
    ScriptLanguage = 'PascalScript'
    ScriptText.Strings = (
      'begin'
      ''
      'end.')
    Left = 120
    Top = 440
    Datasets = <>
    Variables = <>
    Style = <>
  end
  object frxDialogControls1: TfrxDialogControls
    Left = 152
    Top = 440
  end
  object frxDesigner1: TfrxDesigner
    DefaultScriptLanguage = 'PascalScript'
    DefaultFont.Charset = DEFAULT_CHARSET
    DefaultFont.Color = clWindowText
    DefaultFont.Height = -13
    DefaultFont.Name = 'Arial'
    DefaultFont.Style = []
    DefaultLeftMargin = 10.000000000000000000
    DefaultRightMargin = 10.000000000000000000
    DefaultTopMargin = 10.000000000000000000
    DefaultBottomMargin = 10.000000000000000000
    DefaultPaperSize = 9
    DefaultOrientation = poPortrait
    TemplatesExt = 'fr3'
    Restrictions = []
    RTLLanguage = False
    MemoParentFont = False
    Left = 184
    Top = 440
  end
  object frxADOComponents1: TfrxADOComponents
    Left = 216
    Top = 440
  end
  object frxDOCXExport1: TfrxDOCXExport
    UseFileCache = True
    ShowProgress = True
    OverwritePrompt = False
    DataOnly = False
    OpenAfterExport = False
    PictureType = gpPNG
    Left = 248
    Top = 440
  end
  object fsPascal1: TfsPascal
    Left = 88
    Top = 440
  end
  object frxMapObject1: TfrxMapObject
    Left = 88
    Top = 392
  end
  object frxPPTXExport1: TfrxPPTXExport
    UseFileCache = True
    ShowProgress = True
    OverwritePrompt = False
    DataOnly = False
    OpenAfterExport = False
    PictureType = gpPNG
    Left = 128
    Top = 392
  end
  object frxXLSExport1: TfrxXLSExport
    UseFileCache = True
    ShowProgress = True
    OverwritePrompt = False
    DataOnly = False
    ExportEMF = True
    AsText = False
    Background = True
    FastExport = True
    PageBreaks = True
    EmptyLines = True
    SuppressPageHeadersFooters = False
    Left = 168
    Top = 392
  end
  object frxPDFExport1: TfrxPDFExport
    UseFileCache = True
    ShowProgress = True
    OverwritePrompt = False
    DataOnly = False
    OpenAfterExport = False
    PrintOptimized = False
    Outline = False
    Background = False
    HTMLTags = True
    Quality = 95
    Transparency = False
    Author = 'FastReport'
    Subject = 'FastReport PDF export'
    ProtectionFlags = [ePrint, eModify, eCopy, eAnnot]
    HideToolbar = False
    HideMenubar = False
    HideWindowUI = False
    FitWindow = False
    CenterWindow = False
    PrintScaling = False
    PdfA = False
    Left = 208
    Top = 392
  end
  object Copyro: TfrxReportTableObject
    Left = 248
    Top = 392
  end
end
