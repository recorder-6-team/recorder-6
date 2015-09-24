inherited fraMatchLocations: TfraMatchLocations
  Height = 348
  inherited dbgMatch: TImportWizardDBGrid [0]
    Top = 151
    Width = 409
    Height = 22
    Align = alNone
    TabOrder = 3
    Visible = False
  end
  inherited pnlCheckList: TPanel [1]
    inherited shpCheckList: TShape
      Width = 342
    end
    inherited cmbCheckLists: TIDComboBox
      Width = 384
    end
  end
  object pnlLocation: TPanel [2]
    Left = 0
    Top = 73
    Width = 603
    Height = 70
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    TabOrder = 5
    DesignSize = (
      603
      70)
    object shpLocation: TShape
      Left = 148
      Top = 19
      Width = 267
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object lblLocation: TLabel
      Left = 12
      Top = 12
      Width = 128
      Height = 13
      Caption = 'Location Specification'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object chkVagueLocations: TCheckBox
      Left = 28
      Top = 32
      Width = 329
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Place any remaining unmatched locations into the location name:'
      TabOrder = 0
      OnClick = chkVagueLocationsClick
    end
    object chkUseFirstCentroid: TCheckBox
      Left = 28
      Top = 52
      Width = 329
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Use the first listed grid reference when making new entry:'
      TabOrder = 1
      OnClick = chkVagueLocationsClick
    end
  end
  object eCentroid: TAddinLinkedEdit [3]
    Tag = 1
    Left = 292
    Top = 168
    Width = 93
    Height = 21
    TabOrder = 6
    BorderStyle = bsSingle
    ImageIndex = 14
    ImageList = dmFormActions.ilButtons
    OnGetData = eCentroidGetData
    OnKeyDown = eCentroidKeyDown
    ShowDragDropBorder = False
  end
  inherited eMatchValue: TAddinLinkedEdit
    TabOrder = 2
  end
  inherited pnlControls: TPanel [6]
    Top = 143
    inherited btnSearch: TBitBtn
      TabOrder = 1
      Visible = False
    end
    inherited btnNew: TBitBtn
      TabOrder = 0
    end
  end
  object sgSurrogateMatch: TStringGrid [7]
    Left = 0
    Top = 187
    Width = 603
    Height = 161
    Align = alClient
    Ctl3D = False
    DefaultRowHeight = 19
    DefaultDrawing = False
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    ParentCtl3D = False
    TabOrder = 7
    OnClick = sgSurrogateMatchClick
    OnDrawCell = sgSurrogateMatchDrawCell
    OnExit = sgSurrogateMatchExit
    OnMouseUp = sgSurrogateMatchMouseUp
    OnSelectCell = sgSurrogateMatchSelectCell
    OnTopLeftChanged = sgSurrogateMatchTopLeftChanged
    ColWidths = (
      64
      64
      64
      64
      64)
  end
  object pmCentroid: TPopupMenu
    Images = dmFormActions.ilMenuOn
    Left = 384
    Top = 168
  end
end
