object frmFilterManager: TfrmFilterManager
  Left = 336
  Top = 147
  Width = 647
  Height = 429
  Caption = 'Filter Manager'
  Color = clBtnFace
  Constraints.MinHeight = 429
  Constraints.MinWidth = 647
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    631
    391)
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 353
    Width = 631
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnOpen: TImageListButton
      Left = 8
      Top = 7
      Width = 75
      Height = 25
      Caption = '&Open'
      TabOrder = 0
      OnClick = btnOpenClick
      ImageList = dmFormActions.ilMenuOn
      ImageIndex = 59
    end
    object btnSave: TImageListButton
      Left = 92
      Top = 7
      Width = 75
      Height = 25
      Caption = '&Save'
      TabOrder = 1
      OnClick = btnSaveClick
      ImageList = dmFormActions.ilMenuOn
      ImageIndex = 0
    end
    object pnlClose: TPanel
      Left = 536
      Top = 0
      Width = 95
      Height = 38
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object btnClose: TButton
        Left = 12
        Top = 8
        Width = 75
        Height = 25
        Caption = 'C&lose'
        TabOrder = 0
        OnClick = btnCloseClick
      end
    end
  end
  object gbFilterList: TGroupBox
    Left = 8
    Top = 8
    Width = 259
    Height = 357
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Export Filters'
    TabOrder = 0
    DesignSize = (
      259
      357)
    object lbFilters: TListBox
      Left = 8
      Top = 20
      Width = 243
      Height = 297
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnClick = lbFiltersClick
      OnEnter = lbFiltersClick
      OnKeyUp = lbFiltersKeyUp
    end
    object btnAdd: TImageListButton
      Left = 8
      Top = 324
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Add'
      TabOrder = 1
      OnClick = btnAddClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 2
    end
    object btnEdit: TImageListButton
      Left = 92
      Top = 324
      Width = 75
      Height = 25
      Anchors = [akBottom]
      Caption = '&Edit'
      TabOrder = 2
      OnClick = btnEditClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 3
    end
    object btnDelete: TImageListButton
      Left = 176
      Top = 324
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Delete'
      TabOrder = 3
      OnClick = btnDeleteClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 4
    end
  end
  object pcFilterDetails: TPageControl
    Left = 276
    Top = 8
    Width = 353
    Height = 357
    ActivePage = tsGeneral
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    OnChange = pcFilterDetailsChange
    object tsGeneral: TTabSheet
      Caption = 'General'
      DesignSize = (
        345
        329)
      object lblFilterName: TLabel
        Left = 8
        Top = 4
        Width = 53
        Height = 13
        Caption = 'Filter Name'
      end
      object lblFilterSurveys: TLabel
        Left = 8
        Top = 44
        Width = 78
        Height = 13
        Caption = 'Filter on Surveys'
      end
      object lblFilterTaxa: TLabel
        Left = 8
        Top = 212
        Width = 98
        Height = 13
        Caption = 'Filter on Higher Taxa'
      end
      object lblFilterSurveyTags: TLabel
        Left = 8
        Top = 128
        Width = 100
        Height = 13
        Caption = 'Filter on Survey Tags'
      end
      object eFilterName: TEdit
        Left = 8
        Top = 20
        Width = 330
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 40
        TabOrder = 0
      end
      object lbFilterSurveys: TListBox
        Left = 8
        Top = 60
        Width = 310
        Height = 65
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 1
      end
      object btnAddSurvey: TImageListButton
        Left = 317
        Top = 60
        Width = 24
        Height = 23
        Anchors = [akTop, akRight]
        TabOrder = 2
        OnClick = btnAddSurveyClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 2
      end
      object btnDeleteSurvey: TImageListButton
        Left = 317
        Top = 82
        Width = 24
        Height = 23
        Anchors = [akTop, akRight]
        TabOrder = 3
        OnClick = btnDeleteSurveyClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 4
      end
      object lbFilterTaxa: TListBox
        Left = 8
        Top = 228
        Width = 310
        Height = 65
        Style = lbOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGrayText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 14
        MultiSelect = True
        ParentFont = False
        TabOrder = 7
        OnDrawItem = lbFilterTaxaDrawItem
      end
      object btnAddTaxon: TImageListButton
        Left = 317
        Top = 228
        Width = 24
        Height = 23
        Anchors = [akTop, akRight]
        TabOrder = 8
        OnClick = btnAddTaxonClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 2
      end
      object btnDeleteTaxon: TImageListButton
        Left = 317
        Top = 250
        Width = 24
        Height = 23
        Anchors = [akTop, akRight]
        TabOrder = 9
        OnClick = btnDeleteTaxonClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 4
      end
      object btnCancel: TImageListButton
        Left = 264
        Top = 301
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = '&Cancel'
        TabOrder = 11
        OnClick = btnCancelClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 1
      end
      object btnOk: TImageListButton
        Left = 180
        Top = 301
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&OK'
        TabOrder = 10
        OnClick = btnOKClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
      end
      object lbFilterSurveyTags: TListBox
        Left = 8
        Top = 144
        Width = 310
        Height = 65
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 4
      end
      object btnAddSurveyTag: TImageListButton
        Left = 317
        Top = 144
        Width = 24
        Height = 23
        Anchors = [akTop, akRight]
        TabOrder = 5
        OnClick = btnAddSurveyTagClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 2
      end
      object btnDeleteSurveyTag: TImageListButton
        Left = 317
        Top = 166
        Width = 24
        Height = 23
        Anchors = [akTop, akRight]
        TabOrder = 6
        OnClick = btnDeleteSurveyTagClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 4
      end
    end
    object tsSamples: TTabSheet
      Caption = 'Samples'
      ImageIndex = 1
      object bvlObservationDates: TBevel
        Left = 8
        Top = 148
        Width = 329
        Height = 105
        Shape = bsFrame
      end
      object lblObsDateStart: TLabel
        Left = 16
        Top = 156
        Width = 131
        Height = 13
        Caption = 'Filter For Observations From'
      end
      object lblObsDateEnd: TLabel
        Left = 16
        Top = 204
        Width = 121
        Height = 13
        Caption = 'Filter For Observations To'
      end
      object gbSpatialRefCorners: TGroupBox
        Left = 8
        Top = 8
        Width = 329
        Height = 129
        Caption = 'Bounding Box:'
        TabOrder = 0
        object bvlInstructions: TBevel
          Left = 56
          Top = 42
          Width = 229
          Height = 51
          Shape = bsFrame
        end
        object lblSWCorner: TLabel
          Left = 148
          Top = 104
          Width = 98
          Height = 13
          Caption = '<- South-west corner'
        end
        object lblNECorner: TLabel
          Left = 92
          Top = 20
          Width = 91
          Height = 13
          Caption = 'North-east corner->'
        end
        object lblInstructions: TLabel
          Left = 64
          Top = 48
          Width = 215
          Height = 39
          Caption = 
            'Specify the grid-reference of the north-east and south-west corn' +
            'ers of a box enclosing all the samples that the filter should in' +
            'clude.'
          WordWrap = True
        end
        object btnMap: TImageListButton
          Left = 297
          Top = 99
          Width = 24
          Height = 23
          Hint = 'Get bounding box from map'
          TabOrder = 2
          OnClick = btnMapClick
          ImageList = dmFormActions.ilButtons
          ImageIndex = 5
        end
        object eNECorner: TEdit
          Left = 192
          Top = 16
          Width = 133
          Height = 21
          CharCase = ecUpperCase
          TabOrder = 0
          OnExit = eNECornerExit
        end
        object eSWCorner: TEdit
          Left = 8
          Top = 100
          Width = 133
          Height = 21
          CharCase = ecUpperCase
          TabOrder = 1
          OnExit = eSWCornerExit
        end
      end
      object meObsDateStart: TMaskEdit
        Left = 16
        Top = 172
        Width = 73
        Height = 21
        EditMask = '!99/99/9999;1;_'
        MaxLength = 10
        TabOrder = 1
        Text = '  /  /    '
      end
      object meObsDateEnd: TMaskEdit
        Left = 16
        Top = 220
        Width = 73
        Height = 21
        EditMask = '!99/99/9999;1;_'
        MaxLength = 10
        TabOrder = 2
        Text = '  /  /    '
      end
    end
    object tsOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 2
      DesignSize = (
        345
        329)
      object bvlCheckBoxes: TBevel
        Left = 8
        Top = 44
        Width = 329
        Height = 117
        Anchors = [akLeft, akTop, akRight]
        Shape = bsFrame
      end
      object cbIncludeOccurrences: TCheckBox
        Left = 20
        Top = 60
        Width = 249
        Height = 17
        Caption = 'Include all observations that match filter'
        TabOrder = 0
      end
      object cbIncludeLocations: TCheckBox
        Left = 20
        Top = 88
        Width = 305
        Height = 25
        Caption = 
          'Include all locations that match the filter'#39's bounding box and s' +
          'urvey criteria'
        TabOrder = 1
        WordWrap = True
      end
      object cbIncludeNames: TCheckBox
        Left = 20
        Top = 124
        Width = 261
        Height = 17
        Caption = 'Include all individuals and organisations'
        TabOrder = 2
      end
    end
  end
  object odFilter: TOpenDialog
    DefaultExt = 'exf'
    Filter = 'Export Filters|*.exf|All Files|*.*'
    Title = 'Open Export Filter'
    Left = 40
    Top = 52
  end
  object sdFilter: TSaveDialog
    DefaultExt = 'exf'
    Filter = 'Export Filters|*.exf|All Files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save Export Filter'
    Left = 76
    Top = 52
  end
end
