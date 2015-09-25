object dlgDataImport: TdlgDataImport
  Left = 410
  Top = 181
  BorderStyle = bsDialog
  Caption = 'Data Import'
  ClientHeight = 442
  ClientWidth = 538
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object gbImportDetails: TGroupBox
    Left = 4
    Top = 0
    Width = 529
    Height = 125
    TabOrder = 1
    object lblImportDetails: TLabel
      Left = 8
      Top = 12
      Width = 76
      Height = 13
      Caption = 'Analysis Details:'
    end
    object mmDetails: TMemo
      Left = 88
      Top = 10
      Width = 437
      Height = 111
      Color = clBtnFace
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object bbAbort: TImageListButton
    Left = 457
    Top = 412
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Abort'
    ModalResult = 2
    TabOrder = 6
    OnClick = bbCancelClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object gbImportType: TGroupBox
    Left = 4
    Top = 4
    Width = 529
    Height = 105
    TabOrder = 0
    object Label1: TLabel
      Left = 12
      Top = 12
      Width = 59
      Height = 13
      Caption = 'Import Type:'
    end
    object Label2: TLabel
      Left = 12
      Top = 56
      Width = 37
      Height = 13
      Caption = 'Source:'
    end
    object cmbImportType: TComboBox
      Left = 12
      Top = 28
      Width = 181
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbImportTypeChange
    end
    object eSource: TEdit
      Left = 12
      Top = 72
      Width = 453
      Height = 21
      TabOrder = 1
      OnChange = eSourceChange
    end
    object bbImportFrom: TButton
      Left = 464
      Top = 72
      Width = 23
      Height = 22
      Caption = '...'
      TabOrder = 2
      OnClick = bbImportFromClick
    end
  end
  object pcValidation: TPageControl
    Left = 0
    Top = 132
    Width = 538
    Height = 273
    ActivePage = tsDuplicateItems
    TabOrder = 4
    Visible = False
    object tsDuplicateItems: TTabSheet
      Caption = 'Duplicate Items'
      object Label4: TLabel
        Left = 8
        Top = 0
        Width = 74
        Height = 13
        Caption = 'Duplicate Data:'
      end
      object Label5: TLabel
        Left = 180
        Top = 0
        Width = 70
        Height = 13
        Caption = 'Imported Data:'
      end
      object Label6: TLabel
        Left = 352
        Top = 0
        Width = 64
        Height = 13
        Caption = 'Original Data:'
      end
      object bbAllLatest: TButton
        Left = 424
        Top = 216
        Width = 97
        Height = 25
        Caption = 'All &Latest'
        TabOrder = 7
        OnClick = bbAllLatestClick
      end
      object bbAllOriginal: TButton
        Left = 320
        Top = 216
        Width = 97
        Height = 25
        Caption = 'All &Original'
        TabOrder = 6
        OnClick = bbAllOriginalClick
      end
      object bbAllImported: TButton
        Left = 216
        Top = 216
        Width = 97
        Height = 25
        Caption = 'All &Imported'
        TabOrder = 5
        OnClick = bbAllImportedClick
      end
      object bbReject: TButton
        Left = 112
        Top = 216
        Width = 97
        Height = 25
        Caption = '&Reject Import'
        Enabled = False
        TabOrder = 4
        OnClick = bbRejectClick
      end
      object bbAccept: TButton
        Left = 8
        Top = 216
        Width = 97
        Height = 25
        Caption = '&Accept Import'
        Enabled = False
        TabOrder = 3
        OnClick = bbAcceptClick
      end
      object tvDuplicateData: TTreeView
        Left = 8
        Top = 16
        Width = 169
        Height = 193
        HideSelection = False
        Indent = 19
        ReadOnly = True
        TabOrder = 0
        OnChange = tvDuplicateDataChange
        OnCustomDrawItem = TreeViewDataCustomDrawItem
      end
      object tvImportedData: TTreeView
        Left = 180
        Top = 16
        Width = 169
        Height = 193
        HideSelection = False
        Indent = 19
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 1
        OnClick = tvImportedDataClick
        OnCustomDrawItem = TreeViewDataCustomDrawItem
      end
      object tvOriginalData: TTreeView
        Left = 352
        Top = 16
        Width = 169
        Height = 193
        HideSelection = False
        Indent = 19
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 2
        OnClick = tvOriginalDataClick
        OnCustomDrawItem = TreeViewDataCustomDrawItem
      end
    end
    object tsInvalidItems: TTabSheet
      Caption = 'Invalid Items'
      ImageIndex = 1
      object lblInvalidData: TLabel
        Left = 8
        Top = 0
        Width = 57
        Height = 13
        Caption = 'Invalid Data'
      end
      object lblInvalidDetails: TLabel
        Left = 196
        Top = 0
        Width = 35
        Height = 13
        Caption = 'Details:'
      end
      object tvInvalidData: TTreeView
        Left = 8
        Top = 16
        Width = 173
        Height = 221
        Indent = 19
        ReadOnly = True
        TabOrder = 0
        OnChange = tvInvalidDataChange
        OnCustomDrawItem = TreeViewDataCustomDrawItem
      end
      object mmInvalidDetails: TMemo
        Left = 184
        Top = 16
        Width = 337
        Height = 221
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object reXML: TRichEdit
        Left = 184
        Top = 76
        Width = 337
        Height = 161
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Small Fonts'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 2
        Visible = False
        WordWrap = False
      end
    end
  end
  object bbCancel: TImageListButton
    Left = 457
    Top = 116
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = bbCancelClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbAnalyse: TButton
    Left = 374
    Top = 116
    Width = 75
    Height = 25
    Caption = '&Analyse'
    Default = True
    Enabled = False
    TabOrder = 2
    OnClick = bbAnalyseClick
  end
  object bbImport: TImageListButton
    Left = 372
    Top = 412
    Width = 75
    Height = 25
    Caption = 'Import'
    Enabled = False
    TabOrder = 5
    OnClick = bbImportClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object dlgOpen: TOpenDialog
    Left = 4
    Top = 396
  end
end
