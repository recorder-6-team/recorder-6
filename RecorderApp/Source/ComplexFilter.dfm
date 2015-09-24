object dlgComplexFilter: TdlgComplexFilter
  Left = 439
  Top = 218
  BorderStyle = bsDialog
  Caption = 'Complex Filter'
  ClientHeight = 463
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object bbCancel: TImageListButton
    Left = 358
    Top = 432
    Width = 75
    Height = 25
    Hint = 'Cancel changes'
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = bbCancelClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbOK: TImageListButton
    Left = 274
    Top = 432
    Width = 75
    Height = 25
    Hint = 'Accept filters'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = bbOKClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object pcComplexFilter: TPageControl
    Left = 4
    Top = 8
    Width = 429
    Height = 417
    ActivePage = tsConditions
    TabOrder = 0
    OnChange = pcComplexFilterChange
    OnChanging = pcComplexFilterChanging
    object tsConditions: TTabSheet
      Caption = 'Conditions'
      object lblRecordCount: TLabel
        Left = 204
        Top = 357
        Width = 40
        Height = 13
        Caption = '<Count>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object bbMoveUp: TImageListButton
        Left = 394
        Top = 302
        Width = 22
        Height = 22
        Hint = 'Increase sorting priority of selected field'
        TabOrder = 6
        OnClick = bbMoveUpClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 12
      end
      object bbAdd: TImageListButton
        Left = 394
        Top = 233
        Width = 22
        Height = 22
        Hint = 'Add new filter condition'
        TabOrder = 3
        OnClick = bbAddClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 2
      end
      object gbFilterDetails: TGroupBox
        Left = 4
        Top = 4
        Width = 413
        Height = 221
        Caption = 'Details'
        TabOrder = 0
        object Label1: TLabel
          Left = 12
          Top = 16
          Width = 76
          Height = 13
          Caption = '&Available Fields:'
          FocusControl = lbAvailableFields
        end
        object Label3: TLabel
          Left = 12
          Top = 192
          Width = 41
          Height = 13
          Caption = 'Criterion:'
          FocusControl = eCriteria1
        end
        object Label2: TLabel
          Left = 180
          Top = 192
          Width = 18
          Height = 13
          Caption = 'and'
        end
        object bbFilterAccept: TImageListButton
          Left = 357
          Top = 188
          Width = 22
          Height = 22
          Hint = 'Accept filter attributes'
          Enabled = False
          TabOrder = 4
          OnClick = bbFilterAcceptClick
          ImageList = dmFormActions.ilButtons
          ImageIndex = 6
        end
        object bbFilterCancel: TImageListButton
          Left = 379
          Top = 188
          Width = 22
          Height = 22
          Hint = 'Discard changes'
          Enabled = False
          TabOrder = 5
          OnClick = bbFilterCancelClick
          ImageList = dmFormActions.ilButtons
          ImageIndex = 7
        end
        object eCriteria2: TEdit
          Left = 208
          Top = 188
          Width = 121
          Height = 21
          Enabled = False
          TabOrder = 3
          OnExit = eCriteria2Exit
          OnKeyPress = CriteriaKeyPress
        end
        object eCriteria1: TEdit
          Left = 56
          Top = 188
          Width = 273
          Height = 21
          Enabled = False
          TabOrder = 2
          OnExit = eCriteria1Exit
          OnKeyPress = CriteriaKeyPress
        end
        object lbAvailableFields: TListBox
          Left = 12
          Top = 32
          Width = 229
          Height = 149
          Enabled = False
          ItemHeight = 13
          Sorted = True
          TabOrder = 0
          OnClick = lbAvailableFieldsClick
        end
        object rgConditions: TRadioGroup
          Left = 248
          Top = 16
          Width = 153
          Height = 165
          Caption = 'Conditions'
          Items.Strings = (
            'is &Equal to'
            'is &Not equal to'
            'is &Greater than'
            'is &Less than'
            'is &Between')
          TabOrder = 1
          OnClick = rgConditionsClick
        end
      end
      object cbRecordCount: TCheckBox
        Left = 112
        Top = 356
        Width = 93
        Height = 17
        Caption = 'Record Count:'
        TabOrder = 9
        Visible = False
      end
      object sgFilters: TStringGrid
        Left = 4
        Top = 232
        Width = 389
        Height = 114
        ColCount = 2
        DefaultRowHeight = 18
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing]
        TabOrder = 1
        OnClick = sgFiltersClick
        OnDrawCell = sgFiltersDrawCell
        ColWidths = (
          47
          319)
      end
      object cmbAndOr: TComboBox
        Left = 12
        Top = 316
        Width = 73
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        Visible = False
        OnChange = cmbAndOrChange
        Items.Strings = (
          'AND'
          'OR')
      end
      object bbClearAll: TImageListButton
        Left = 4
        Top = 352
        Width = 81
        Height = 25
        Caption = '&Clear All'
        TabOrder = 8
        OnClick = bbClearAllClick
        ImageList = dmFormActions.ilMenuOn
        ImageIndex = 33
      end
      object bbMoveDown: TImageListButton
        Left = 394
        Top = 324
        Width = 22
        Height = 22
        Hint = 'Decrease sorting priority of selected field'
        TabOrder = 7
        OnClick = bbMoveDownClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 13
      end
      object bbDelete: TImageListButton
        Left = 394
        Top = 277
        Width = 22
        Height = 22
        Hint = 'Delete selected filter condition'
        TabOrder = 5
        OnClick = bbDeleteClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 4
      end
      object bbEdit: TImageListButton
        Left = 394
        Top = 255
        Width = 22
        Height = 22
        Hint = 'Edit selected filter condition'
        TabOrder = 4
        OnClick = bbEditClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 3
      end
    end
    object tsConstraints: TTabSheet
      Caption = 'Constraints'
      ImageIndex = 1
      object Bevel1: TBevel
        Left = 4
        Top = 8
        Width = 413
        Height = 377
        Shape = bsFrame
      end
      object cbExcConfidential: TCheckBox
        Left = 124
        Top = 64
        Width = 161
        Height = 17
        Caption = 'Exclude Confidential Data'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object cbExcUnChecked: TCheckBox
        Left = 124
        Top = 112
        Width = 153
        Height = 17
        Caption = 'Exclude Unchecked Data'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object cbExcUnverified: TCheckBox
        Left = 124
        Top = 160
        Width = 245
        Height = 17
        Caption = 'Exclude Failed/Pending Verification Data'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object cbExcZero: TCheckBox
        Left = 124
        Top = 208
        Width = 177
        Height = 17
        Caption = 'Exclude Zero Abundance Data'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
    end
  end
end
