inherited frmBiotopeDictEditor: TfrmBiotopeDictEditor
  Left = 464
  Top = 142
  Width = 768
  Height = 511
  Caption = 'Biotope Dictionary Details'
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited DictSplitter: TSplitter
    Height = 393
  end
  inherited pnlSelection: TPanel
    Width = 760
    inherited cmbList: TComboBox
      Width = 585
    end
    inherited btnShowAll: TButton
      Left = 676
    end
  end
  inherited pcBrowser: TPageControl
    Height = 393
    inherited tsTree: TTabSheet
      inherited tvDictionary: TKeyboardRapidTree
        Height = 381
        OnKeyDown = tvDictionaryKeyDown
        Data = {0400000000000000}
      end
    end
  end
  inherited pnlDetails: TPanel
    Width = 390
    Height = 393
    Constraints.MinHeight = 376
    Constraints.MinWidth = 390
    OnResize = pnlDetailsResize
    object scbBiotopeDetails: TScrollBox
      Left = 0
      Top = 0
      Width = 390
      Height = 376
      Constraints.MinHeight = 376
      Constraints.MinWidth = 390
      TabOrder = 0
      DesignSize = (
        386
        372)
      object lblNamePrompt: TLabel
        Left = 8
        Top = 8
        Width = 76
        Height = 13
        Caption = 'Selected Name:'
      end
      object lblSelectedName: TLabel
        Left = 93
        Top = 8
        Width = 3
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object btnSave: TImageListButton
        Left = 222
        Top = 342
        Width = 75
        Height = 25
        Hint = 'Save biotope details'
        Anchors = [akRight, akBottom]
        Caption = '&Save'
        Enabled = False
        TabOrder = 1
        OnClick = btnSaveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
      end
      object btnCancel: TImageListButton
        Left = 305
        Top = 342
        Width = 75
        Height = 25
        Hint = 'Cancel changes'
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = '&Cancel'
        Enabled = False
        TabOrder = 2
        OnClick = btnCancelClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 1
      end
      object pcBiotopeDetails: TPageControl
        Left = 8
        Top = 27
        Width = 372
        Height = 310
        ActivePage = tsFacts
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        OnChange = pcBiotopeDetailsChange
        OnChanging = pcBiotopeDetailsChanging
        object tsGeneral: TTabSheet
          Caption = 'General'
          DesignSize = (
            364
            282)
          object Bevel1: TBevel
            Left = 5
            Top = 4
            Width = 355
            Height = 273
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object lblOriginalCode: TLabel
            Left = 16
            Top = 176
            Width = 66
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Original Code:'
          end
          object lblFullTerm: TLabel
            Left = 16
            Top = 100
            Width = 46
            Height = 13
            Caption = 'Full Term:'
          end
          object lblShortTerm: TLabel
            Left = 16
            Top = 48
            Width = 55
            Height = 13
            Caption = 'Short Term:'
          end
          object dblblEntryDate: TDBText
            Left = 84
            Top = 227
            Width = 61
            Height = 13
            Anchors = [akLeft, akBottom]
            DataField = 'BLI_ENTRY_DATE'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object dblblChangedDate: TDBText
            Left = 84
            Top = 251
            Width = 61
            Height = 13
            Anchors = [akLeft, akBottom]
            DataField = 'BLI_CHANGED_DATE'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object lblEntryDatePrompt: TLabel
            Left = 16
            Top = 227
            Width = 51
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Entry date:'
          end
          object lblChangeDatePrompt: TLabel
            Left = 16
            Top = 251
            Width = 64
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Change date:'
          end
          object lblChangeByPrompt: TLabel
            Left = 152
            Top = 251
            Width = 15
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'By:'
          end
          object lblEntryByPrompt: TLabel
            Left = 152
            Top = 227
            Width = 15
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'By:'
          end
          object lblEnteredBy: TLabel
            Left = 172
            Top = 227
            Width = 178
            Height = 13
            Anchors = [akLeft, akBottom]
            AutoSize = False
            Caption = '<Entered  By                                     >'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object lblChangedBy: TLabel
            Left = 172
            Top = 251
            Width = 178
            Height = 13
            Anchors = [akLeft, akBottom]
            AutoSize = False
            Caption = '<Entered  By                                     >'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object dbeShortTerm: TDBEdit
            Left = 96
            Top = 48
            Width = 241
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'B_SHORT_TERM'
            TabOrder = 0
            OnChange = dbeTermCodeChange
          end
          object dbmmFullTerm: TDBMemo
            Left = 96
            Top = 96
            Width = 241
            Height = 49
            Anchors = [akLeft, akTop, akRight, akBottom]
            DataField = 'B_FULL_TERM'
            ScrollBars = ssVertical
            TabOrder = 1
          end
          object dbeOriginalCode: TDBEdit
            Left = 96
            Top = 174
            Width = 241
            Height = 21
            Anchors = [akLeft, akRight, akBottom]
            DataField = 'B_ORIGINAL_CODE'
            TabOrder = 2
            OnChange = dbeTermCodeChange
          end
        end
        object tsFacts: TTabSheet
          BorderWidth = 4
          Caption = 'Facts'
          ImageIndex = 2
          object splFacts: TSplitter
            Left = 0
            Top = 94
            Width = 356
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            Beveled = True
            MinSize = 73
          end
          object gbFactDetails: TGroupBox
            Left = 0
            Top = 97
            Width = 356
            Height = 177
            Align = alBottom
            Caption = 'Details:'
            Constraints.MinHeight = 147
            TabOrder = 0
            DesignSize = (
              356
              177)
            object lblFact: TLabel
              Left = 10
              Top = 60
              Width = 36
              Height = 26
              Caption = 'Facts: (HTML)'
              WordWrap = True
            end
            object lblFactTitle: TLabel
              Left = 10
              Top = 16
              Width = 23
              Height = 13
              Caption = 'Title:'
            end
            object lblFactType: TLabel
              Left = 10
              Top = 40
              Width = 27
              Height = 13
              Caption = 'Type:'
            end
            object lblFactDate: TLabel
              Left = 193
              Top = 40
              Width = 26
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'Date:'
            end
            object Shape1: TShape
              Tag = 2
              Left = 69
              Top = 119
              Width = 258
              Height = 23
              Anchors = [akLeft, akRight, akBottom]
              Pen.Color = clRed
            end
            object lblFactsReference: TLabel
              Left = 10
              Top = 123
              Width = 52
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Document:'
            end
            object eFactTitle: TEdit
              Left = 48
              Top = 12
              Width = 300
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
            end
            object cmbFactType: TComboBox
              Left = 48
              Top = 36
              Width = 86
              Height = 21
              Style = csDropDownList
              Anchors = [akLeft, akTop, akRight]
              ItemHeight = 13
              Sorted = True
              TabOrder = 1
              Items.Strings = (
                'AVI Movie'
                'Bitmap Image'
                'JPEG Image'
                'Text'
                'WAV Sound')
            end
            object vdeFactDate: TVagueDateEdit
              Left = 225
              Top = 36
              Width = 123
              Height = 21
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGreen
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              OnExit = vdeFactDateExit
            end
            object eFactReference: TEdit
              Tag = 1
              Left = 70
              Top = 120
              Width = 256
              Height = 21
              Anchors = [akLeft, akRight, akBottom]
              TabOrder = 4
              OnDblClick = eFactReferenceDblClick
              OnKeyPress = eFactReferenceKeyPress
            end
            object reFact: TMemo
              Left = 48
              Top = 60
              Width = 300
              Height = 54
              Anchors = [akLeft, akTop, akRight, akBottom]
              Lines.Strings = (
                '')
              TabOrder = 3
            end
            object btnFactReferenceFind: TImageListButton
              Left = 327
              Top = 119
              Width = 20
              Height = 22
              Hint = 'Get document'
              Anchors = [akRight, akBottom]
              TabOrder = 7
              OnClick = btnFactReferenceFindClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 5
            end
            object btnFactCancel: TImageListButton
              Left = 324
              Top = 147
              Width = 24
              Height = 23
              Hint = 'Discard changes'
              Anchors = [akRight, akBottom]
              TabOrder = 6
              OnClick = btnFactCancelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 7
            end
            object btnFactOK: TImageListButton
              Left = 300
              Top = 147
              Width = 24
              Height = 23
              Hint = 'Accept changes'
              Anchors = [akRight, akBottom]
              TabOrder = 5
              OnClick = btnFactOKClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 6
            end
          end
          object pnlFactsTop: TPanel
            Left = 0
            Top = 0
            Width = 356
            Height = 94
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              356
              94)
            object btnFactDelete: TImageListButton
              Left = 332
              Top = 46
              Width = 24
              Height = 23
              Hint = 'Delete selected fact'
              Anchors = [akTop, akRight]
              TabOrder = 0
              OnClick = btnFactDeleteClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 4
            end
            object btnFactEdit: TImageListButton
              Left = 332
              Top = 23
              Width = 24
              Height = 23
              Hint = 'Edit selected fact'
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = btnFactEditClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 3
            end
            object btnFactAdd: TImageListButton
              Left = 332
              Top = 0
              Width = 24
              Height = 23
              Hint = 'Add new fact'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = btnFactAddClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 2
            end
            object sgFacts: TStringGrid
              Left = 0
              Top = 0
              Width = 332
              Height = 90
              Anchors = [akLeft, akTop, akRight, akBottom]
              ColCount = 3
              DefaultRowHeight = 18
              FixedCols = 0
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSizing, goRowSelect]
              TabOrder = 3
              OnClick = sgFactsClick
              ColWidths = (
                64
                70
                191)
            end
          end
        end
        object tsSources: TTabSheet
          Caption = 'Sources'
          ImageIndex = 3
          object Sources: TSources
            Left = 5
            Top = 5
            Width = 355
            Height = 272
            SourceCol = clBlue
            DestCol = clRed
            TabOrder = 0
          end
        end
      end
    end
  end
  object pnlButtons: TPanel [4]
    Left = 0
    Top = 422
    Width = 760
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object btnAdd: TImageListButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Add a new biotope to the selected classification'
      Caption = '&Add'
      TabOrder = 0
      OnClick = btnAddClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 2
    end
    object btnEdit: TImageListButton
      Left = 88
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Edit the selected biotope'
      Caption = 'Ed&it'
      TabOrder = 1
      OnClick = btnEditClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 3
    end
    object btnDelete: TImageListButton
      Left = 168
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Delete the selected biotope'
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btnDeleteClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 4
    end
  end
  inherited mnuChildMerge: TMainMenu
    inherited mnuEdit: TMenuItem
      object mnuEditAdd: TMenuItem [0]
        Caption = '&Add'
        Hint = 'Add a new biotope to the selected classification'
        object mnuEditAddSibling: TMenuItem
          Action = actAddSibling
        end
        object mnuEditAddChild: TMenuItem
          Action = actAddChild
        end
      end
      object mnuEditEdit: TMenuItem [1]
        Caption = '&Edit'
        Hint = 'Edit the selected biotope'
        OnClick = btnEditClick
      end
      object mnuEditDelete: TMenuItem [2]
        Caption = '&Delete'
        Hint = 'Delete the selected biotope'
        OnClick = btnDeleteClick
      end
      object N2: TMenuItem [3]
        Caption = '-'
      end
      object mnuEditCut: TMenuItem [4]
        Action = dmFormActions.actCut
      end
      object mnuEditPaste: TMenuItem [6]
        Action = dmFormActions.actPaste
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
  inherited pmHierarchy: TPopupMenu
    object pmHAdd: TMenuItem [0]
      Caption = '&Add'
      object pmHAddSibling: TMenuItem
        Action = actAddSibling
      end
      object pmHAddChild: TMenuItem
        Action = actAddChild
      end
    end
  end
  inherited alBiotopeDiction: TActionList
    object actAddSibling: TAction [0]
      Caption = '&Sibling'
      Hint = 'Add a biotope on the same level as the selected one'
      OnExecute = actAddSiblingExecute
    end
    object actAddChild: TAction [1]
      Caption = '&Child'
      Hint = 'Add a biotope as a child to the same level as the selected one'
      OnExecute = actAddChildExecute
    end
  end
  object pmAdd: TPopupMenu
    Left = 104
    Top = 204
    object pmAddSibling: TMenuItem
      Action = actAddSibling
    end
    object pmAddChild: TMenuItem
      Action = actAddChild
    end
  end
end
