inherited frmMap: TfrmMap
  Left = 280
  Top = 185
  Width = 635
  Height = 594
  ActiveControl = sgLayers
  Caption = 'Map Window'
  Constraints.MinHeight = 135
  Constraints.MinWidth = 290
  Menu = nil
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMapPanel: TPanel [0]
    Left = 219
    Top = 0
    Width = 400
    Height = 519
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = pnlMapPanelClick
    OnMouseDown = pnlMapPanelMouseDown
    OnMouseMove = pnlMapPanelMouseMove
    OnMouseUp = pnlMapPanelMouseUp
    OnResize = pnlMapPanelResize
  end
  object pnlDockSiteRight: TPanel [1]
    Left = 619
    Top = 0
    Width = 0
    Height = 519
    Align = alRight
    AutoSize = True
    BevelOuter = bvNone
    DockSite = True
    TabOrder = 1
    OnDockDrop = pnlDockSiteDockDrop
    OnDockOver = pnlDockSiteLeftDockOver
    OnGetSiteInfo = pnlDockSiteLeftGetSiteInfo
  end
  object pnlDockSiteLeft: TPanel [2]
    Left = 0
    Top = 0
    Width = 219
    Height = 519
    Align = alLeft
    AutoSize = True
    BevelOuter = bvNone
    DockSite = True
    TabOrder = 2
    OnDockDrop = pnlDockSiteDockDrop
    OnDockOver = pnlDockSiteLeftDockOver
    OnGetSiteInfo = pnlDockSiteLeftGetSiteInfo
    object pnlDockableLegend: TPanel
      Left = 0
      Top = 0
      Width = 219
      Height = 519
      Align = alLeft
      TabOrder = 0
      object HorizontalSplitter: TSplitter
        Left = 1
        Top = 216
        Width = 217
        Height = 3
        Cursor = crVSplit
        Align = alTop
        AutoSnap = False
        OnMoved = HorizontalSplitterMoved
      end
      object pnlDistPointsLabel: TPanel
        Left = 1
        Top = 45
        Width = 217
        Height = 20
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        Caption = ' Distribution Points'
        TabOrder = 1
      end
      object pnlMapLayerLabel: TPanel
        Left = 1
        Top = 219
        Width = 217
        Height = 25
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        Caption = ' Map Layers'
        TabOrder = 3
        DesignSize = (
          217
          25)
        object btnLayerDown: TImageListButton
          Left = 191
          Top = 1
          Width = 24
          Height = 23
          Hint = 'Move the selected layer down'
          Anchors = [akTop, akRight]
          TabOrder = 0
          OnClick = btnLayerDownClick
          ImageList = dmFormActions.ilButtons
          ImageIndex = 13
        end
        object btnLayerUp: TImageListButton
          Left = 164
          Top = 1
          Width = 24
          Height = 23
          Hint = 'Move the selected layer up'
          Anchors = [akTop, akRight]
          Enabled = False
          TabOrder = 1
          OnClick = btnLayerUpClick
          ImageList = dmFormActions.ilButtons
          ImageIndex = 12
        end
      end
      object pnlSelectedPolySheet: TPanel
        Left = 1
        Top = 477
        Width = 217
        Height = 41
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 5
        object lblCurrentPolygonLayer: TLabel
          Left = 2
          Top = 2
          Width = 107
          Height = 13
          Caption = 'Current Polygon Layer:'
        end
        object cmbCurrentLayer: TComboBox
          Left = 2
          Top = 16
          Width = 209
          Height = 22
          Style = csOwnerDrawFixed
          ItemHeight = 16
          TabOrder = 0
          OnChange = cmbCurrentLayerChange
          OnDrawItem = cmbCurrentLayerDrawItem
        end
      end
      object sgDatasets: TStringGrid
        Left = 1
        Top = 65
        Width = 217
        Height = 151
        Hint = 'List of datasets dropped on the map'
        Align = alTop
        ColCount = 3
        DefaultRowHeight = 18
        FixedCols = 0
        RowCount = 1
        FixedRows = 0
        GridLineWidth = 0
        TabOrder = 2
        ColWidths = (
          16
          30
          143)
      end
      object sgLayers: TStringGrid
        Left = 1
        Top = 244
        Width = 217
        Height = 233
        Hint = 'List of layers for displayed map'
        Align = alClient
        ColCount = 3
        DefaultRowHeight = 18
        FixedCols = 0
        RowCount = 1
        FixedRows = 0
        GridLineWidth = 0
        TabOrder = 4
        OnSelectCell = sgLayersSelectCell
        ColWidths = (
          16
          30
          143)
      end
      object pnlMapSelector: TPanel
        Left = 1
        Top = 1
        Width = 217
        Height = 44
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Bevel1: TBevel
          Left = 0
          Top = 42
          Width = 217
          Height = 2
          Align = alBottom
          Shape = bsBottomLine
        end
        object Label1: TLabel
          Left = 2
          Top = 2
          Width = 57
          Height = 13
          Caption = 'Active Map:'
        end
        object cmbActiveMap: TComboBox
          Left = 2
          Top = 16
          Width = 209
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          Sorted = True
          TabOrder = 0
          OnChange = cmbActiveMapChange
        end
      end
    end
  end
  object pnlBottom: TPanel [3]
    Left = 0
    Top = 519
    Width = 619
    Height = 37
    Align = alBottom
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 3
    object shpDrag: TShape
      Tag = 1
      Left = 1
      Top = 1
      Width = 617
      Height = 35
      Align = alClient
      Brush.Style = bsClear
      Pen.Color = clBlue
    end
    object pnlDrag: TPanel
      Left = 2
      Top = 2
      Width = 623
      Height = 31
      BevelOuter = bvNone
      TabOrder = 0
      object lblXPThemeWorkaround: TLabel
        Left = 0
        Top = -50
        Width = 340
        Height = 13
        Caption = 
          'This label is there to make XP theming work with TStaticText! Do' +
          'n'#39't ask.'
        Visible = False
      end
      object eSpatialRef: TEdit
        Left = 72
        Top = 6
        Width = 129
        Height = 21
        ReadOnly = True
        TabOrder = 0
      end
      object eLocation: TEdit
        Left = 264
        Top = 6
        Width = 229
        Height = 21
        ReadOnly = True
        TabOrder = 1
      end
      object staticSpatialRef: TStaticText
        Left = 8
        Top = 10
        Width = 62
        Height = 17
        Caption = 'Spatial Ref.:'
        TabOrder = 2
      end
      object staticLocation: TStaticText
        Left = 216
        Top = 10
        Width = 48
        Height = 17
        Caption = 'Location:'
        TabOrder = 3
      end
    end
  end
  inherited mnuChildMerge: TMainMenu
    Left = 266
    Top = 16
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 2
      object mnuEditReturnData: TMenuItem
        Action = dmFormActions.actTransferData
      end
      object mnuEditSep1: TMenuItem
        Caption = '-'
      end
      object mnuEditFinishPolygonCurrent: TMenuItem
        Action = actFinishPolygonCurrent
      end
      object mnuEditFinishPolygonAny: TMenuItem
        Caption = 'Finish Polygon && add to layer'
        Enabled = False
      end
      object mnuEditFinishLineCurrent: TMenuItem
        Action = actFinishLineCurrent
      end
      object mnuEditFinishLineAny: TMenuItem
        Caption = 'Finish Line && add &to layer'
        Enabled = False
        Hint = 'Adds the line to a specific layer'
      end
      object mnuEditCancel: TMenuItem
        Action = actCancelDraw
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuEditCopyMap: TMenuItem
        Action = actCopyMapToClipboard
      end
    end
    object mnuChildMap: TMenuItem
      Caption = '&Map'
      GroupIndex = 6
      object mnuMapWindow: TMenuItem
        Caption = '&Map'
        Hint = 'Display Map window'
        ImageIndex = 21
      end
      object mnuMapOptions: TMenuItem
        Action = dmFormActions.actMapOptions
      end
      object mnuMapBrowser: TMenuItem
        Action = dmFormActions.actMapBrowser
      end
      object mnuMapSep1: TMenuItem
        Caption = '-'
      end
      object mnuMapPointer: TMenuItem
        Action = actPointer
        GroupIndex = 7
        RadioItem = True
      end
      object mnuMapZoom: TMenuItem
        Action = actZoom
        GroupIndex = 7
        RadioItem = True
      end
      object mnuMapUnZoom: TMenuItem
        Action = actUnZoom
        GroupIndex = 7
        RadioItem = True
      end
      object mnuMapZoomExtents: TMenuItem
        Caption = 'Zoom to E&xtents'
        GroupIndex = 7
        Hint = 'Change the scale to display the entire map'
        ImageIndex = 27
        OnClick = mnuMapZoomExtentsClick
      end
      object mnuMapPan: TMenuItem
        Action = actPan
        GroupIndex = 7
        RadioItem = True
      end
      object mnuMapFindSourceData: TMenuItem
        Action = actFindSourceData
        GroupIndex = 7
        RadioItem = True
      end
      object mnuMapAddBoundary: TMenuItem
        Caption = 'Add B&oundary'
        GroupIndex = 7
        Hint = 'Draw a boundary on the map'
        ImageIndex = 30
        RadioItem = True
        object mnuMapBoundaryDraw: TMenuItem
          Action = actDraw
        end
        object mnuMapBoundaryImport: TMenuItem
          Caption = '&Import'
          OnClick = mnuMapBoundaryImportClick
        end
      end
      object mnuMapDeletePolygon: TMenuItem
        Action = actDeletePolygon
        GroupIndex = 7
        RadioItem = True
      end
      object mnuMapSubtractBoundary: TMenuItem
        Action = actSubtractBoundary
        GroupIndex = 7
      end
      object mnuMapCancelSubtract: TMenuItem
        Action = actCancelSubtract
        GroupIndex = 7
      end
      object mnuMapAssociateBoundary: TMenuItem
        Action = actAssociateBoundary
        GroupIndex = 7
      end
      object mnuMapMovePolygon: TMenuItem
        Action = actMovePolygon
        GroupIndex = 7
      end
      object mnuMapExtractGridSquares: TMenuItem
        Action = actExtractGridSquares
        GroupIndex = 7
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 7
      end
      object mnuMapExportDataset: TMenuItem
        Caption = 'Export Distribution Points...'
        GroupIndex = 7
      end
      object N5: TMenuItem
        Caption = '-'
        GroupIndex = 7
      end
      object mnuMapAddPolygonLayer: TMenuItem
        Caption = 'Add Polygon Layer'
        GroupIndex = 7
        OnClick = actAddPolygonLayerExecute
      end
      object mnuMapAddBackgroundLayer: TMenuItem
        Caption = 'Add Background Layer'
        GroupIndex = 7
        OnClick = actAddBackgroundLayerExecute
      end
      object mnuMapExportPolygonLayer: TMenuItem
        Caption = 'Export Current Polygon Layer'
        GroupIndex = 7
        OnClick = mnuMapExportPolygonLayerClick
      end
      object mnuMapExportAllPolygonLayers: TMenuItem
        Action = actExportAllPolygonLayers
        GroupIndex = 7
      end
      object mnuMapSep2: TMenuItem
        Caption = '-'
        GroupIndex = 7
      end
      object mnuMapDistributionPoints: TMenuItem
        Caption = '&Distribution Points'
        GroupIndex = 7
        Hint = 'Set the scale for displaying distribution points'
        ImageIndex = 29
        OnClick = mnuMapDistributionPointsClick
        object mnuMapPoints10kms: TMenuItem
          Caption = '10 kms'
          Hint = 'Display distribution points centred on 10 km squares'
          OnClick = mnuMapDistributionPointsClick
        end
        object mnuMapPoints5kms: TMenuItem
          Caption = '5 kms'
          OnClick = mnuMapDistributionPointsClick
        end
        object mnuMapPoints2kms: TMenuItem
          Caption = '2 kms'
          OnClick = mnuMapDistributionPointsClick
        end
        object mnuMapPoints1km: TMenuItem
          Caption = '1 km'
          OnClick = mnuMapDistributionPointsClick
        end
        object mnuMapPoints100m: TMenuItem
          Caption = '100 m'
          OnClick = mnuMapDistributionPointsClick
        end
        object mnuMapDistPointsSep: TMenuItem
          Caption = '-'
        end
        object mnuMapPointsDefault: TMenuItem
          Caption = 'Default'
          Checked = True
          OnClick = mnuMapDistributionPointsClick
        end
      end
      object mnuMapGridLines: TMenuItem
        Caption = '&Grid Lines'
        GroupIndex = 7
        Hint = 'Set the gridline spacing interval'
        ImageIndex = 28
        object mnuMapLines100kms: TMenuItem
          Caption = '100 kms'
          OnClick = mnuMapGridLinesClick
        end
        object mnuMapLines50kms: TMenuItem
          Caption = '50 kms'
          OnClick = mnuMapGridLinesClick
        end
        object mnuMapLines20kms: TMenuItem
          Caption = '10 kms'
          OnClick = mnuMapGridLinesClick
        end
        object mnuMapLines10kms: TMenuItem
          Caption = '1 km'
          OnClick = mnuMapGridLinesClick
        end
        object mnuMapGridLinesSep: TMenuItem
          Caption = '-'
        end
        object mnuMapLinesNone: TMenuItem
          Caption = '&None'
          Checked = True
          OnClick = mnuMapGridLinesClick
        end
      end
      object mnuMapSep3: TMenuItem
        Caption = '-'
        GroupIndex = 7
      end
      object mnuMapAddSample: TMenuItem
        Caption = 'Add &Sample'
        GroupIndex = 7
        Hint = 'Add a sample icon to the map'
        ImageIndex = 12
      end
      object mnuMapClear: TMenuItem
        Action = actClear
        GroupIndex = 7
      end
    end
  end
  object pmDistributionPoints: TPopupMenu
    Left = 266
    Top = 74
    object pmPoints10kms: TMenuItem
      Caption = '10 kms'
      Hint = 'Display points centred on a 10 km grid square'
      OnClick = mnuMapDistributionPointsClick
    end
    object pmPoints5kms: TMenuItem
      Caption = '5 kms'
      Hint = 'Display points centred on a 5 km grid square'
      OnClick = mnuMapDistributionPointsClick
    end
    object pmPoints2kms: TMenuItem
      Caption = '2 kms'
      Hint = 'Display points centred on a 2 km grid square'
      OnClick = mnuMapDistributionPointsClick
    end
    object pmPoints1km: TMenuItem
      Caption = '1 km'
      Hint = 'Display points centred on a 1 km grid square'
      OnClick = mnuMapDistributionPointsClick
    end
    object pmPoints100m: TMenuItem
      Caption = '100 m'
      Hint = 'Display points centred on a 100 m grid square'
      OnClick = mnuMapDistributionPointsClick
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object pmPointsDefault: TMenuItem
      Caption = 'Default'
      Checked = True
      Default = True
      Hint = 'Display points centred as default'
      OnClick = mnuMapDistributionPointsClick
    end
  end
  object pmGridLines: TPopupMenu
    Left = 266
    Top = 126
    object pmLines100kms: TMenuItem
      Caption = '100 kms'
      Hint = 'Display a 100 km grid'
      OnClick = mnuMapGridLinesClick
    end
    object pmLines50kms: TMenuItem
      Tag = 50
      Caption = '50 kms'
      Hint = 'Display a 50 km grid'
      OnClick = mnuMapGridLinesClick
    end
    object pmLines20kms: TMenuItem
      Tag = 10
      Caption = '10 kms'
      Hint = 'Display a 10 km grid'
      OnClick = mnuMapGridLinesClick
    end
    object pmLines10kms: TMenuItem
      Tag = 1
      Caption = '1 km'
      Hint = 'Display a 1 km grid'
      OnClick = mnuMapGridLinesClick
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object pmLinesNone: TMenuItem
      Caption = '&None'
      Checked = True
      Default = True
      OnClick = mnuMapGridLinesClick
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 
      'All supported vector file formats [*.gsf, *.dxf, *.bna, *.vpf, *' +
      '.ntf, *.mif, *.shp, *.opt, *.std]|*.gsf;*.dxf;*.bna;*.vpf;*.ntf;' +
      '*.mif;*.shp;*.opt;*.std|MapServer Sheets (*.gsf)|*.gsf|AutoCAD (' +
      '*.dxf)|*.dxf|Atlas GIS (*.bna)|*.bna|Digital Chart of the World ' +
      '(*.vpf)|*.vpf|OS Version 2 Levels 1,2,3 (*.ntf)|*.ntf|Map Info f' +
      'ile (*.mif)|*.mif|ERSI Shape file (*.shp)|*.shp|Digital Line Gra' +
      'ph (*.opt, *.std)|*.opt;*.std'
    Title = 'Import Boundary'
    Left = 348
    Top = 16
  end
  object pmBoundary: TPopupMenu
    Left = 266
    Top = 242
    object pmBoundaryDraw: TMenuItem
      Action = actDraw
      Default = True
    end
    object pmBoundaryImport: TMenuItem
      Caption = '&Import onto layer'
      OnClick = mnuMapBoundaryImportClick
    end
  end
  object pmPolygonDrawing: TPopupMenu
    Left = 266
    Top = 188
    object pmDrawingFinishPolygonCurrent: TMenuItem
      Action = actFinishPolygonCurrent
    end
    object pmDrawingFinishPolygonAny: TMenuItem
      Caption = 'Finish Polygon && add &to layer'
    end
    object pmDrawingFinishLineCurrent: TMenuItem
      Action = actFinishLineCurrent
    end
    object pmDrawingFinishLineAny: TMenuItem
      Caption = 'Finish Line && &add to layer'
    end
    object pmDrawingCancel: TMenuItem
      Action = actCancelDraw
    end
  end
  object pmSampleTypes: TPopupMenu
    Images = dmFormActions.ilMenuOn
    Left = 266
    Top = 296
  end
  object alMap: TActionList
    Left = 348
    Top = 74
    object actDeletePolygon: TAction
      Caption = 'Delete Pol&ygon'
      Enabled = False
      Hint = 'Delete the selected boundary polygon'
      ImageIndex = 31
      OnExecute = mnuMapDeletePolygonClick
    end
    object actAssociateBoundary: TAction
      Caption = 'Associate Boundar&y'
      Enabled = False
      Hint = 'Associated Locations'
      ImageIndex = 51
      OnExecute = actAssociateBoundaryExecute
    end
    object actClear: TAction
      Caption = 'Cl&ear'
      Enabled = False
      Hint = 'Clears all symbols from the current map'
      ImageIndex = 33
      OnExecute = mnuMapClearClick
    end
    object actFinishPolygonCurrent: TAction
      Category = 'Drawing'
      Caption = 'Finish &Polygon && add to current layer'
      OnExecute = actFinishPolygonCurrentExecute
    end
    object actFinishLineCurrent: TAction
      Category = 'Drawing'
      Caption = 'Finish &Line && add to current drawing layer'
      OnExecute = actFinishLineCurrentExecute
    end
    object actCancelDraw: TAction
      Category = 'Drawing'
      Caption = '&Cancel'
      OnExecute = actCancelDrawExecute
    end
    object actCopyMapToClipboard: TAction
      Caption = 'C&opy Map to Clipboard'
      Hint = 'Copy the portion of the map currently displayed to the clipboard'
      ImageIndex = 3
      ShortCut = 16461
      OnExecute = actCopyMapToClipboardExecute
    end
    object actMovePolygon: TAction
      Caption = 'Move Polygon to &Layer'
      Hint = 'Move Polygon to Layer'
      ImageIndex = 58
      OnExecute = actMovePolygonExecute
    end
    object actCancelSubtract: TAction
      Caption = 'Cancel Subtract Boundary from Boundary'
      Enabled = False
      Hint = 'Cancel Subtract Boundary from Boundary'
      ImageIndex = 62
      ShortCut = 27
      Visible = False
      OnExecute = actCancelSubtractExecute
    end
    object actSubtractBoundary: TAction
      Caption = '&Create Hole in Polygon'
      Hint = 'Create Hole in Polygon'
      ImageIndex = 61
      ShortCut = 119
      OnExecute = actSubtractBoundaryExecute
    end
    object actAddPolygonLayer: TAction
      Category = 'Layer'
      Caption = 'Add Polygon Layer'
      OnExecute = actAddPolygonLayerExecute
    end
    object actAddBackgroundLayer: TAction
      Category = 'Layer'
      Caption = 'Add Background Layer'
      OnExecute = actAddBackgroundLayerExecute
    end
    object actExtractGridSquares: TAction
      Caption = 'E&xtract Grid Squares'
      Hint = 'Extract grid squares for boundaries associated with locations'
      ImageIndex = 73
      OnExecute = actExtractGridSquaresExecute
    end
    object actExportAllPolygonLayers: TAction
      Category = 'Layer'
      Caption = 'Export All Polygon Layers'
      OnExecute = actExportAllPolygonLayersClick
    end
  end
  object tmRefresh: TTimer
    Interval = 5000
    OnTimer = tmRefreshTimer
    Left = 422
    Top = 16
  end
  object alMapTools: TActionList
    Left = 348
    Top = 120
    object actPointer: TAction
      Caption = '&Pointer'
      Hint = 'Switch map cursor to a pointer'
      ImageIndex = 23
      ShortCut = 116
      OnExecute = actPointerExecute
    end
    object actZoom: TAction
      Caption = '&Zoom'
      Hint = 'Switch map pointer to zoom in'
      ImageIndex = 25
      ShortCut = 113
      OnExecute = actZoomExecute
    end
    object actUnZoom: TAction
      Caption = '&UnZoom'
      Hint = 'Switch map pointer to zoom out'
      ImageIndex = 26
      ShortCut = 114
      OnExecute = actUnZoomExecute
    end
    object actPan: TAction
      Caption = 'P&an'
      Hint = 'Switch map pointer to pan the map'
      ImageIndex = 24
      ShortCut = 117
      OnExecute = actPanExecute
    end
    object actFindSourceData: TAction
      Caption = '&Find Source Data'
      Hint = 'Finds source data for observations'
      ImageIndex = 57
      OnExecute = actFindSourceDataExecute
    end
    object actDraw: TAction
      Caption = '&Draw'
      OnExecute = actDrawExecute
      OnUpdate = actDrawUpdate
    end
  end
  object pmSubtractBoundary: TPopupMenu
    Left = 266
    Top = 348
    object mnuSubtractBoundary: TMenuItem
      Action = actSubtractBoundary
    end
    object mnuSubtract: TMenuItem
      Action = actCancelSubtract
    end
  end
  object pmPolygonToLayer: TPopupMenu
    Left = 266
    Top = 400
  end
  object pmPolygonReport: TPopupMenu
    Left = 348
    Top = 172
    object pmDeletePolygon: TMenuItem
      Action = actDeletePolygon
    end
    object pmAssociateBoundary: TMenuItem
      Action = actAssociateBoundary
    end
    object pmExtractGridSquares: TMenuItem
      Action = actExtractGridSquares
    end
    object pmDivider: TMenuItem
      Caption = '-'
    end
    object pmQuickReports: TMenuItem
      Caption = 'Quick Report'
      Enabled = False
      OnClick = pmQuickReportsClick
      object OccurrencesForPlaces1: TMenuItem
        Action = dmFormActions.actOccurrencesForPlacesReport
      end
    end
    object pmBatchUpdate: TMenuItem
      Caption = 'Batch Update'
      Enabled = False
      OnClick = pmBatchUpdateClick
      object pmPlaceHolder: TMenuItem
        Caption = 'Place Holder'
        Enabled = False
        Visible = False
      end
    end
  end
  object tmBoundingBox: TTimer
    OnTimer = tmBoundingBoxTimer
    Left = 419
    Top = 64
  end
end
