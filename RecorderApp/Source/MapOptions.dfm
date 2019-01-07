object dlgMapOptions: TdlgMapOptions
  Left = 707
  Top = 339
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Map Options'
  ClientHeight = 328
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TButton
    Left = 386
    Top = 298
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object pcMapOptions: TPageControl
    Left = 4
    Top = 4
    Width = 457
    Height = 289
    ActivePage = tsRecover
    TabOrder = 0
    OnChange = pcMapOptionsChange
    OnChanging = pcMapOptionsChanging
    object tsBaseMap: TTabSheet
      Caption = 'Enabled Maps'
      object bvlBaseMapFrame: TBevel
        Left = -12
        Top = 4
        Width = 441
        Height = 253
        Shape = bsFrame
      end
      object sgMaps: TStringGrid
        Left = 16
        Top = 16
        Width = 417
        Height = 201
        ColCount = 4
        DefaultRowHeight = 19
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing]
        TabOrder = 0
        OnClick = sgMapsClick
        OnDrawCell = sgMapsDrawCell
        OnKeyPress = sgMapsKeyPress
        OnMouseWheelDown = sgMapsMouseWheel
        OnMouseWheelUp = sgMapsMouseWheel
        OnSelectCell = sgMapsSelectCell
        OnTopLeftChanged = sgMapsTopLeftChanged
        ColWidths = (
          42
          26
          103
          237)
      end
      object btnReset: TImageListButton
        Left = 344
        Top = 224
        Width = 87
        Height = 25
        Hint = 'Reset selected map'
        Caption = '&Reset'
        TabOrder = 4
        OnClick = btnResetClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 22
      end
      object btnRemoveMap: TImageListButton
        Left = 112
        Top = 224
        Width = 87
        Height = 25
        Hint = 'Remove selected map'
        Caption = '&Delete'
        TabOrder = 3
        OnClick = btnRemoveMapClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 4
      end
      object btnAddMap: TImageListButton
        Left = 16
        Top = 224
        Width = 87
        Height = 25
        Hint = 'Add new map'
        Caption = '&Add'
        TabOrder = 2
        OnClick = btnAddMapClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 2
      end
      object cmbBaseMaps: TComboBox
        Left = 192
        Top = 36
        Width = 240
        Height = 21
        AutoCloseUp = True
        Style = csDropDownList
        ItemHeight = 0
        Sorted = True
        TabOrder = 1
        Visible = False
        OnChange = cmbBaseMapsChange
        OnExit = cmbBaseMapsExit
      end
    end
    object tsMapLayers: TTabSheet
      Caption = 'Map Layers'
      object lblMap: TLabel
        Left = 16
        Top = 16
        Width = 24
        Height = 13
        Caption = 'Map:'
      end
      object Bevel2: TBevel
        Left = 4
        Top = 4
        Width = 441
        Height = 253
        Shape = bsFrame
      end
      object cmbMaps: TComboBox
        Left = 44
        Top = 12
        Width = 193
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
        OnChange = cmbMapsChange
      end
      object btnMoveDown: TImageListButton
        Left = 228
        Top = 224
        Width = 95
        Height = 25
        Caption = 'Move &Down'
        TabOrder = 4
        OnClick = btnMoveDownClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 13
      end
      object btnMoveUp: TImageListButton
        Left = 128
        Top = 224
        Width = 95
        Height = 25
        Caption = 'Move &Up'
        Enabled = False
        TabOrder = 3
        OnClick = btnMoveUpClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 12
      end
      object btnDeleteLayer: TImageListButton
        Left = 338
        Top = 224
        Width = 95
        Height = 25
        Caption = 'Delete'
        TabOrder = 5
        OnClick = btnDeleteLayerClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 4
      end
      object btnLayerProperties: TImageListButton
        Left = 16
        Top = 224
        Width = 95
        Height = 25
        Caption = '&Properties...'
        TabOrder = 2
        OnClick = btnLayerPropertiesClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 3
      end
      object sgLayers: TStringGrid
        Left = 16
        Top = 36
        Width = 417
        Height = 181
        ColCount = 3
        DefaultRowHeight = 18
        FixedCols = 0
        FixedRows = 0
        GridLineWidth = 0
        TabOrder = 1
        OnSelectCell = sgLayersSelectCell
        ColWidths = (
          20
          24
          369)
      end
    end
    object tsDistributionSymbols: TTabSheet
      Caption = 'Distribution Symbols'
      object lblCutoff: TLabel
        Left = 220
        Top = 238
        Width = 171
        Height = 13
        Caption = 'Cut off year for hollow/solid symbols:'
      end
      object Bevel1: TBevel
        Left = 4
        Top = 4
        Width = 441
        Height = 225
        Shape = bsFrame
      end
      object btnDeleteDataset: TImageListButton
        Left = 348
        Top = 196
        Width = 87
        Height = 25
        Caption = 'Delete'
        TabOrder = 1
        OnClick = btnDeleteDatasetClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 4
      end
      object eCutOffYear: TEdit
        Left = 396
        Top = 236
        Width = 49
        Height = 21
        TabOrder = 3
        Text = '1980'
      end
      object chkUniqueDataset: TCheckBox
        Left = 4
        Top = 236
        Width = 185
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Allow only one dataset at a time:'
        TabOrder = 2
        OnClick = chkUniqueDatasetClick
      end
      object sgDatasets: TStringGrid
        Left = 16
        Top = 36
        Width = 417
        Height = 153
        ColCount = 3
        DefaultRowHeight = 18
        FixedCols = 0
        FixedRows = 0
        GridLineWidth = 0
        TabOrder = 0
        OnSelectCell = sgDatasetsSelectCell
        ColWidths = (
          21
          31
          361)
      end
    end
    object tsRecover: TTabSheet
      Caption = 'Admin'
      ImageIndex = 3
      object lblCurrentObjectSheet: TLabel
        Left = 104
        Top = 176
        Width = 59
        Height = 13
        Caption = 'ObjectSheet'
        Color = clWindow
        ParentColor = False
      end
      object lblCurrentMapSheetPath: TLabel
        Left = 8
        Top = 200
        Width = 80
        Height = 13
        Caption = 'Map Sheet Path '
        Color = clBtnFace
        ParentColor = False
      end
      object lblMapSheet: TLabel
        Left = 104
        Top = 200
        Width = 52
        Height = 13
        Caption = 'Map Sheet'
        Color = clWindow
        ParentColor = False
      end
      object lblObjectSheetPath: TLabel
        Left = 8
        Top = 176
        Width = 90
        Height = 13
        Caption = 'Object Sheet Path '
        Color = clBtnFace
        ParentColor = False
      end
      object lblWorkStation: TLabel
        Left = 8
        Top = 138
        Width = 60
        Height = 13
        Caption = 'Workstation '
      end
      object lblUserFile: TLabel
        Left = 8
        Top = 224
        Width = 74
        Height = 13
        Caption = 'User Files Path '
      end
      object lblUserFilePath: TLabel
        Left = 104
        Top = 224
        Width = 60
        Height = 13
        Caption = 'UserFilePath'
        Color = clBtnHighlight
        ParentColor = False
      end
      object lblBaseMap: TLabel
        Left = 8
        Top = 8
        Width = 51
        Height = 13
        Caption = 'Base Map '
      end
      object lblCurrentBasemap: TLabel
        Left = 64
        Top = 8
        Width = 84
        Height = 13
        Caption = 'Current Base map'
        Color = clWindow
        ParentColor = False
      end
      object rgSecure: TRadioGroup
        Left = 8
        Top = 32
        Width = 425
        Height = 97
        Caption = 'Actions'
        Color = clBtnFace
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Backup Map Data'
          'Archive Backups'
          'Restore Map Data'
          'Restore Background Map Data'
          'Relink  All Map Data'
          'Relink  Background Map Data')
        ParentColor = False
        TabOrder = 0
        OnClick = rgSecureClick
      end
      object btnSecure: TButton
        Left = 360
        Top = 136
        Width = 73
        Height = 25
        Caption = 'Go'
        TabOrder = 1
        OnClick = btnSecureClick
      end
      object edWorkstation: TEdit
        Left = 88
        Top = 136
        Width = 161
        Height = 21
        Enabled = False
        TabOrder = 2
        Text = 'edWorkstation'
      end
      object cbRetain: TCheckBox
        Left = 264
        Top = 136
        Width = 65
        Height = 25
        Caption = 'Retain'
        TabOrder = 3
      end
      object btnFixMaster: TButton
        Left = 408
        Top = 168
        Width = 33
        Height = 25
        Caption = 'Fix'
        TabOrder = 4
        OnClick = btnFixMasterClick
      end
    end
  end
end
