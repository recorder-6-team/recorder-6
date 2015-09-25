inherited frmReportPreview: TfrmReportPreview
  Left = 284
  Top = 123
  Width = 428
  Height = 352
  Caption = 'Report Preview'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object QRPreview: TQRPreview [0]
    Left = 0
    Top = 8
    Width = 420
    Height = 298
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    Align = alClient
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    OnPageAvailable = QRPreviewPageAvailable
    OnProgressUpdate = QRPreviewProgressUpdate
    PageNumber = 1
    Zoom = 100
  end
  object tbPreview: TToolBar [1]
    Left = 0
    Top = 0
    Width = 420
    Height = 8
    AutoSize = True
    BorderWidth = 2
    EdgeBorders = []
    Flat = True
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 12
      Style = tbsDivider
    end
    object Separator1: TToolButton
      Left = 8
      Top = 0
      Width = 8
      Caption = 'Separator1'
      ImageIndex = 3
      Style = tbsDivider
    end
    object Separator2: TToolButton
      Left = 16
      Top = 0
      Width = 8
      Caption = 'Separator2'
      ImageIndex = 7
      Style = tbsDivider
    end
  end
  inherited mnuChildMerge: TMainMenu
    Images = dmFormActions.ilMenuOn
    Left = 38
    Top = 24
    object mnuReports: TMenuItem
      Caption = '&Reports'
      GroupIndex = 7
      object mnuReportRun: TMenuItem
        Action = dmFormActions.actOpenReport
      end
      object mnuRpeortWizard: TMenuItem
        Action = dmFormActions.actReportWizard
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuReportZoomToFit: TMenuItem
        Action = actZoomToFit
      end
      object mnuReportZoom100: TMenuItem
        Action = actZoom100
      end
      object mnuReportZoomToWidth: TMenuItem
        Action = actZoomToWidth
      end
      object mnuReportZoomIn: TMenuItem
        Action = actZoomIn
      end
      object mnuReportZoomOut: TMenuItem
        Action = actZoomOut
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuReportFirst: TMenuItem
        Action = actFirstPage
      end
      object mnuReportPrev: TMenuItem
        Action = actPrevPage
      end
      object mnuReportNext: TMenuItem
        Action = actNextPage
      end
      object mnuReportLast: TMenuItem
        Action = actLastPage
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuReportExport: TMenuItem
        Action = actExport
      end
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'TXT'
    Filter = 
      'Ascii text file (*.txt)|*.TXT|Comma Separated file (*.csv)|*.CSV' +
      '|Rich-text formatted file (*.rtf)|*.RTF|HTML Document (*.htm)|*.' +
      'HTM|Windows metafile (*.wmf)|*.WMF|Excel Spreadsheet (*.xls)|*.X' +
      'LS'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 38
    Top = 74
  end
  object alPreview: TActionList
    Images = dmFormActions.ilMenuOn
    Left = 38
    Top = 130
    object actZoomToFit: TAction
      Caption = '&Zoom To Fit'
      Hint = 'Zoom to fit'
      ImageIndex = 41
      OnExecute = actZoomToFitClick
    end
    object actZoom100: TAction
      Caption = 'Zoom To &100%'
      Hint = 'Zoom to 100%'
      ImageIndex = 42
      OnExecute = actZoom100Click
    end
    object actZoomToWidth: TAction
      Caption = 'Zoom To Width'
      Hint = 'Zoom to width'
      ImageIndex = 43
      OnExecute = actZoomToWidthClick
    end
    object actZoomIn: TAction
      Caption = 'Zoom &In'
      Hint = 'Zoom in'
      ImageIndex = 25
      OnExecute = actZoomInClick
    end
    object actZoomOut: TAction
      Caption = 'Zoom &Out'
      Hint = 'Zoom out'
      ImageIndex = 26
      OnExecute = actZoomOutClick
    end
    object actFirstPage: TAction
      Caption = '&First Page'
      Hint = 'Go to first page'
      ImageIndex = 44
      OnExecute = actFirstPageClick
    end
    object actPrevPage: TAction
      Caption = '&Previous Page'
      Hint = 'Go to previous page'
      ImageIndex = 45
      OnExecute = actPrevPageClick
    end
    object actNextPage: TAction
      Caption = '&Next Page'
      Hint = 'Go to next page'
      ImageIndex = 46
      OnExecute = actNextPageClick
    end
    object actLastPage: TAction
      Caption = '&Last Page'
      Hint = 'Go to last page'
      ImageIndex = 47
      OnExecute = actLastPageClick
    end
    object actExport: TAction
      Caption = '&Export...'
      Hint = 'Export report'
      ImageIndex = 37
      OnExecute = actExportExecute
    end
  end
end
