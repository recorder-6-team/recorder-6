inherited frmLocations: TfrmLocations
  Left = 389
  Top = 316
  Width = 670
  Height = 447
  ActiveControl = tvLocations
  Caption = 'Locations'
  Constraints.MinHeight = 446
  Constraints.MinWidth = 500
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object LocationSplitter: TSplitter [0]
    Left = 253
    Top = 0
    Width = 10
    Height = 355
    Align = alRight
    MinSize = 100
    OnMoved = FormResize
    OnPaint = LocationSplitterPaint
  end
  object pnlDetails: TPanel [1]
    Left = 263
    Top = 0
    Width = 391
    Height = 355
    Align = alRight
    BevelOuter = bvNone
    Constraints.MinWidth = 391
    TabOrder = 0
  end
  object pnlButtons: TPanel [2]
    Left = 0
    Top = 355
    Width = 654
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlButtons2: TPanel
      Left = 471
      Top = 0
      Width = 183
      Height = 34
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 3
      object btnShowAll: TButton
        Left = 4
        Top = 6
        Width = 75
        Height = 25
        Hint = 'Remove Filter on Hierarchy.'
        Caption = '&Show All'
        TabOrder = 0
        Visible = False
        OnClick = btnShowAllClick
      end
      object bbRelatedData: TImageListButton
        Left = 84
        Top = 6
        Width = 93
        Height = 25
        Hint = 'Access data related to the selected location'
        Caption = 'Re&lated Data'
        TabOrder = 1
        OnClick = bbRelatedDataClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 14
        Layout = blGlyphRight
      end
    end
    object bbEdit: TImageListButton
      Left = 88
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Edit the selected location/feature'
      Caption = 'Ed&it'
      TabOrder = 1
      OnClick = bbEditClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 3
    end
    object bbDelete: TImageListButton
      Left = 168
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Delete the selected location/feature'
      Caption = 'Delete'
      TabOrder = 2
      OnClick = bbDeleteClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 4
    end
    object bbAdd: TImageListButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Add locations and features'
      Caption = '&Add'
      TabOrder = 0
      OnClick = bbAddClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 2
    end
  end
  object pnlDrill: TPanel [3]
    Left = 0
    Top = 0
    Width = 253
    Height = 355
    Align = alClient
    TabOrder = 2
    object shpLocations: TShape
      Tag = 3
      Left = 1
      Top = 18
      Width = 251
      Height = 336
      Align = alClient
      Brush.Color = clLime
      Pen.Color = clRed
      Pen.Style = psDash
    end
    object pnlLabel: TPanel
      Left = 1
      Top = 1
      Width = 251
      Height = 17
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 2
        Width = 48
        Height = 13
        Caption = 'Hierarchy:'
      end
    end
    object tvLocations: TKeyboardRapidTree
      Left = 2
      Top = 19
      Width = 262
      Height = 363
      SmoothExpandCollapse = False
      FitColumnToClientWidth = True
      FitToHeight = False
      DoubleBuffered = True
      TransparentMode = True
      DefaultRowHeight = 16
      DragMode = dmAutomatic
      RowCount = 0
      FixedRows = 0
      GridLineWidth = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
      PopupMenu = pmHierarchy
      TabOrder = 1
      OnDrawCell = tvLocationsDrawCell
      OnExit = tvLocationsExit
      OnKeyDown = tvLocationsKeyDown
      OnExpanding = tvLocationsExpanding
      OnChange = tvLocationsChange
      OnCompare = tvLocationsCompare
      HideSelection = False
      Showlines = True
      ShowRoot = True
      ShowButtons = True
      ShowImages = True
      ShowLogic = False
      Images = ilLocation
      SortType = stNone
      WordWrap = False
      AutoMove = False
      ToolTips = False
      AutoExpand = False
      TooltipColor = clInfoBk
      ToolTipPause = 1000
      StatesDrawed = True
      ColWidths = (
        258)
      Data = {0400000000000000}
    end
  end
  inherited mnuChildMerge: TMainMenu
    Left = 302
    object mnuEdit: TMenuItem
      Caption = 'Edit'
      GroupIndex = 2
      object mnuEditAdd: TMenuItem
        Caption = '&Add'
        Hint = 'Add Locations and Features'
        object mnuEditAddSite: TMenuItem
          Action = actAddSite
        end
        object mnuEditAddSubSite: TMenuItem
          Action = actAddSubSite
        end
        object mnuEditAddFeature: TMenuItem
          Action = actAddFeature
        end
      end
      object mnuEditEdit: TMenuItem
        Caption = '&Edit'
        Hint = 'Edit the selected location/feature'
        OnClick = bbEditClick
      end
      object mnuEditDelete: TMenuItem
        Caption = '&Delete'
        Hint = 'Delete the selected location/feature'
        OnClick = bbDeleteClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuEditCut: TMenuItem
        Action = dmFormActions.actCut
      end
      object mnuEditCopy: TMenuItem
        Action = dmFormActions.actCopy
      end
      object mnuEditPaste: TMenuItem
        Action = dmFormActions.actPaste
      end
      object mnuEditPromoteNode: TMenuItem
        Action = actPromoteNode
      end
      object mnuEditMoveto: TMenuItem
        Action = actMoveTo
      end
      object mnuEditReturnData: TMenuItem
        Action = dmFormActions.actTransferData
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuEditFind: TMenuItem
        Action = actFind
      end
      object mnuEditSortBy: TMenuItem
        Caption = 'S&ort By'
        Hint = 'Sort locations/features'
        ImageIndex = 7
        object mnuEditSortLocation: TMenuItem
          Action = actSortSiteName
        end
        object mnuEditSortFeatureName: TMenuItem
          Action = actSortFeatName
        end
        object mnuEditSortFeatureType: TMenuItem
          Action = actSortFeatType
        end
      end
      object mnuEditFilter: TMenuItem
        Action = actFilter
      end
      object mnuEditFindOnMap: TMenuItem
        Action = actFindOnMap
      end
      object mnuEditShowMetadata: TMenuItem
        Action = actShowMetadata
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuEditBold: TMenuItem
        Action = dmFormActions.actBold
      end
      object mnuEditItalic: TMenuItem
        Action = dmFormActions.actItalic
      end
      object mnuEditUnderline: TMenuItem
        Action = dmFormActions.actUnderline
      end
    end
  end
  object pmRelatedData: TPopupMenu
    Left = 302
    Top = 220
    object mnuRelSurveys: TMenuItem
      Caption = '&Surveys...'
      Hint = 'Show related Surveys for the selected Location'
      ImageIndex = 14
      OnClick = mnuRelSurveysClick
    end
    object mnuRelEvents: TMenuItem
      Caption = '&Events...'
      Hint = 'Show related Survey Events for the selected Location '
      OnClick = mnuRelEventsClick
    end
    object mnuRelSamples: TMenuItem
      Caption = 'S&amples...'
      Hint = 'Show related Samples for the selected Location'
      OnClick = mnuRelSamplesClick
    end
    object mnuRelOccur: TMenuItem
      Caption = '&Occurrences...'
      Hint = 'Show related Occurrences for the selected Location'
      OnClick = mnuRelOccurClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object mnuRelIndivOrg: TMenuItem
      Caption = '&Names and Addresses...'
      Hint = 
        'Show related Individuals and Organisations for the selected Loca' +
        'tion'
      ImageIndex = 15
      OnClick = mnuRelIndivOrgClick
    end
    object mnuRelDocuments: TMenuItem
      Caption = '&Documents...'
      Hint = 'Show related References for the selected Location'
      ImageIndex = 16
      OnClick = mnuRelDocumentsClick
    end
  end
  object pmAdd: TPopupMenu
    Left = 302
    Top = 164
    object pmAddSite: TMenuItem
      Action = actAddSite
    end
    object pmAddSubSite: TMenuItem
      Action = actAddSubSite
    end
    object pmAddFeature: TMenuItem
      Action = actAddFeature
    end
  end
  object pmSortBy: TPopupMenu
    Left = 302
    Top = 108
    object pmSortLocation: TMenuItem
      Action = actSortSiteName
    end
    object pmSortFileCode: TMenuItem
      Action = actSortFileCode
    end
    object pnSortSpatialRef: TMenuItem
      Action = actSortSpatialRef
    end
    object pmSortFeatureName: TMenuItem
      Action = actSortFeatName
    end
    object pmSortFeatureType: TMenuItem
      Action = actSortFeatType
    end
  end
  object pmHierarchy: TPopupMenu
    Images = dmFormActions.ilMenuOn
    OnPopup = pmHierarchyPopup
    Left = 302
    Top = 56
    object pmHAdd: TMenuItem
      Caption = '&Add'
      Hint = 'Add Locations and Features'
      object pmHAddSite: TMenuItem
        Action = actAddSite
      end
      object pmHAddSubSite: TMenuItem
        Action = actAddSubSite
      end
      object pmHAddFeature: TMenuItem
        Action = actAddFeature
      end
    end
    object pmHSortBy: TMenuItem
      Caption = 'S&ort'
      Hint = 'Sort locations/features'
      ImageIndex = 7
      object pmHSortLocationName: TMenuItem
        Action = actSortSiteName
      end
      object pmHSortFileCode: TMenuItem
        Action = actSortFileCode
      end
      object pmHSortSpatialRef: TMenuItem
        Action = actSortSpatialRef
      end
      object pmHSortFeatureName: TMenuItem
        Action = actSortFeatName
      end
      object pmHSortFeatureType: TMenuItem
        Action = actSortFeatType
      end
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object pmHCut: TMenuItem
      Action = dmFormActions.actCut
    end
    object pmHCopy: TMenuItem
      Action = dmFormActions.actCopy
    end
    object pmHPaste: TMenuItem
      Action = dmFormActions.actPaste
    end
    object pmHPromoteNode: TMenuItem
      Action = actPromoteNode
    end
    object pmHMoveTo: TMenuItem
      Action = actMoveTo
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object pmHFindOnMap: TMenuItem
      Action = actFindOnMap
    end
    object pmHQuickReports: TMenuItem
      Caption = '&Quick Report'
      OnClick = pmHQuickReportsClick
      object pmHOccurrencesForPlacesReport: TMenuItem
        Action = dmFormActions.actOccurrencesForPlacesReport
      end
    end
    object pmHBatchUpdate: TMenuItem
      Caption = '&Batch Updates'
      OnClick = pmHBatchUpdateClick
      object mnuPlaceHolder: TMenuItem
        Caption = 'PlaceHolder'
        Visible = False
      end
      object TMenuItem
      end
    end
    object pmHValidateItem: TMenuItem
      Action = dmFormActions.actDatabaseValidateSelected
    end
  end
  object ilLocation: TImageList
    Left = 396
    Top = 12
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000066
      FF00000099000066FF0000009900000099000033660000009900003366000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000066FF00000099000066FF00000099000000990000336600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000066660000669933006699
      3300669933000000000000660000009900000099000000990000009900000099
      0000009900000099000000000000000000000000000000000000000000000000
      0000000000000066FF0000009900000099000033660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000066330000666600006699
      3300669933006699330000000000006600000099000000990000009900000099
      0000009900000099000000000000000000000000000000000000000000000000
      000000000000000000000066FF00000099000033660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF993300663300006666
      0000669933006699330099996600000000000066000000990000009900000099
      00000099000066CC000000000000000000000000000000000000000000000000
      000000000000000000000066FF00000099000033660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF993300FF9933006633
      0000666600009999660099CC9900FF9933000000000000660000009900000099
      000066CC000066FF99000000000000000000000000000099000000CC00000099
      000000990000006600000066FF00000099000033660000660000006600000066
      0000006600000066000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF993300FF993300FF99
      33009999660099CC9900FF993300FF993300FF9933000000000066CC000066CC
      000066FF9900FF993300000000000000000000CC000000CC00000099000000CC
      0000009900000099000000990000006600000099000000660000009900000066
      0000006600000066000000660000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF993300FF993300FF99
      3300FF993300FF993300FF993300FF993300FF993300FF99330066FF990066FF
      9900FF993300FF993300000000000000000000CC00000099000000CC00000099
      0000009900000099000000990000009900000099000000990000006600000099
      0000006600000066000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF993300FFCC6600FF99
      3300FFCC6600FF993300FFCC6600FF993300FFCC6600FF993300FFCC6600FF99
      3300FFCC6600FF993300000000000000000000CC000000CC0000009900000099
      000000CC00000099000000990000009900000066000000990000009900000066
      0000009900000066000000660000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFCC6600FF993300FFCC
      6600FF993300FFCC6600FF993300FFCC6600FF993300FFCC660000CCCC0033FF
      FF00FF993300FFCC6600000000000000000000CC00000099000000CC00000099
      0000009900000099000000990000009900000099000000990000009900000099
      0000006600000066000000660000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFCC6600FFCC6600FFCC
      6600FFCC6600FFCC6600FFCC6600FFCC6600FFCC660000CCCC0033FFFF0033FF
      FF0033FFFF00FFCC6600000000000000000000CC000000CC00000099000000CC
      0000009900000099000000CC0000009900000099000000990000006600000099
      0000009900000066000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFCC6600FFCCCC00FFCC
      6600FFCCCC00FFCC6600FFCCCC00FFCC6600FFCCCC0033FFFF0033FFFF0033FF
      FF0033FFFF00FFCC660000000000000000000000000000CC000000CC00000099
      000000CC000000990000009900000099000000CC000000990000009900000099
      0000006600000066000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFCCCC00FFCC6600FFCC
      CC00FFCC6600FFCCCC00FFCC6600FFCCCC00FFCC6600FFCCCC0033FFFF0033FF
      FF00FFCC6600FFCCCC000000000000000000000000000000000000CC000000CC
      00000099000000CC000000990000009900000099000000990000009900000066
      0000009900000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFCCCC00FFCCCC00FFCC
      CC00FFCCCC00FFCCCC00FFCCCC00FFCCCC00FFCCCC00FFCCCC00FFCCCC00FFCC
      CC00FFCCCC00FFCCCC00000000000000000000000000000000000000000000CC
      000000CC00000099000000CC00000099000000CC000000990000009900000099
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000CC000000CC00000099000000CC00000099000000CC0000009900000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFE01F000000000001F03F00000000
      0001F87F000000000001FC7F000000000001FC7F000000000001800300000000
      0001000100000000000100030000000000010001000000000001000100000000
      000100030000000000018003000000000001C007000000000001E00F00000000
      0001F01F00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object alLocations: TActionList
    Images = dmFormActions.ilMenuOn
    Left = 396
    Top = 56
    object actAddSite: TAction
      Caption = '&Site'
      Hint = 'Add new Location on the same level as the selected one'
      OnExecute = actAddSiteExecute
    end
    object actAddSubSite: TAction
      Caption = 'S&ub Site'
      Hint = 'Add new Location as a child to the selected one'
      OnExecute = actAddSubSiteExcute
    end
    object actAddFeature: TAction
      Caption = '&Feature'
      Hint = 'Add new Feature to the selected location'
      OnExecute = actAddFeatureExecute
    end
    object actSortSiteName: TAction
      Caption = '&Location Name'
      Hint = 'Sort Locations by Name'
      OnExecute = actSortSiteNameExecute
    end
    object actSortFileCode: TAction
      Caption = 'File &Code'
      Hint = 'Sort Locations by Code'
      OnExecute = actSortFileCodeExecute
    end
    object actSortSpatialRef: TAction
      Caption = '&Spatial Ref'
      Hint = 'Sort Locations by Spatial Ref'
      OnExecute = actSortSpatialRefExecute
    end
    object actSortFeatName: TAction
      Caption = 'Feature &Name'
      Hint = 'Sort Features by Name'
      OnExecute = actSortFeatNameExecute
    end
    object actSortFeatType: TAction
      Caption = 'Feature &Type'
      Hint = 'Sort Features by Type'
      OnExecute = actSortFeatTypeExecute
    end
    object actFind: TAction
      Caption = '&Find'
      Hint = 'Find an Item'
      ImageIndex = 5
      ShortCut = 16454
      OnExecute = actFindExecute
    end
    object actFilter: TAction
      Caption = '&Simple Filter...'
      Hint = 'Apply a filter on the locations'
      ImageIndex = 6
      OnExecute = actFilterExecute
    end
    object actShowMetadata: TAction
      Caption = 'Show &Metadata'
      Hint = 'Show Metadata'
      ImageIndex = 52
      OnExecute = actShowMetadataExecute
    end
    object actFindOnMap: TAction
      Caption = 'Find On Map'
      Hint = 'Find On Map'
      OnExecute = actFindOnMapExecute
    end
    object actPromoteNode: TAction
      Caption = 'Pro&mote to Top-Level'
      Hint = 'Move selected location(s) to top level'
      OnExecute = actPromoteNodeExecute
    end
    object actMoveTo: TAction
      Caption = 'Mo&ve to...'
      Hint = 'Move selected location(s) under another location'
      OnExecute = actMoveToExecute
    end
  end
end
