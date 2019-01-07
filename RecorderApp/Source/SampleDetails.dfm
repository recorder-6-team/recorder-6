inherited frmSampleDetails: TfrmSampleDetails
  Left = 632
  Top = 226
  Width = 390
  Height = 395
  Caption = 'Sample Details'
  OldCreateOrder = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object pnlDetails: TPanel
    Left = 0
    Top = 0
    Width = 382
    Height = 368
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
    OnResize = pnlDetailsResize
    object pnlInner: TPanel
      Left = 0
      Top = 0
      Width = 385
      Height = 361
      BevelOuter = bvLowered
      TabOrder = 0
      DesignSize = (
        385
        361)
      object lblSample: TLabel
        Left = 8
        Top = 4
        Width = 35
        Height = 13
        Caption = 'Sample'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object bbCancel: TImageListButton
        Left = 306
        Top = 330
        Width = 75
        Height = 25
        Hint = 'Cancel changes'
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = '&Cancel'
        Enabled = False
        TabOrder = 0
        OnClick = bbCancelClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 1
      end
      object bbSave: TImageListButton
        Left = 222
        Top = 330
        Width = 75
        Height = 25
        Hint = 'Save sample details'
        Anchors = [akRight, akBottom]
        Caption = '&Save'
        Enabled = False
        TabOrder = 1
        OnClick = bbSaveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
      end
      object pcSampleDetails: TPageControl
        Left = 8
        Top = 24
        Width = 373
        Height = 301
        ActivePage = tsPrivate
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 2
        OnChange = pcSampleDetailsChange
        object tsGeneral: TTabSheet
          Caption = 'General'
          DesignSize = (
            365
            273)
          object Bevel1: TBevel
            Left = 4
            Top = 4
            Width = 357
            Height = 265
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object lblSampleRefDisp: TLabel
            Left = 12
            Top = 16
            Width = 58
            Height = 13
            Caption = 'Sample Ref:'
          end
          object lblSampleType: TLabel
            Left = 12
            Top = 165
            Width = 27
            Height = 13
            Caption = 'Type:'
          end
          object lblSampleDate: TLabel
            Left = 12
            Top = 138
            Width = 26
            Height = 13
            Caption = 'Date:'
          end
          object lblStartTime: TLabel
            Left = 12
            Top = 191
            Width = 51
            Height = 13
            Caption = 'Start Time:'
          end
          object lblDuration: TLabel
            Left = 152
            Top = 191
            Width = 43
            Height = 13
            Caption = 'Duration:'
          end
          object lblComment: TLabel
            Left = 12
            Top = 218
            Width = 47
            Height = 13
            Caption = 'Comment:'
          end
          object dbeSampleRef: TDBEdit
            Left = 80
            Top = 12
            Width = 273
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'SAMPLE_REFERENCE'
            DataSource = dmSample.dsSample
            TabOrder = 0
          end
          object dbcmbSampleType: TDBGlyphLookupComboBox
            Left = 80
            Top = 161
            Width = 273
            Height = 22
            Anchors = [akLeft, akTop, akRight]
            DataField = 'SAMPLE_TYPE_KEY'
            DataSource = dmSample.dsSample
            GlyphField = 'IMAGE'
            ItemHeight = 16
            KeyField = 'SAMPLE_TYPE_KEY'
            ListField = 'SHORT_NAME'
            ListSource = dmSample.dsSampleType
            TabOrder = 3
            OnClick = dbComboClick
            OnKeyDown = dbComboKeyDown
            OnKeyUp = dbComboKeyUp
          end
          object dbreComments: TDBRichEdit
            Left = 80
            Top = 215
            Width = 273
            Height = 44
            Anchors = [akLeft, akTop, akRight, akBottom]
            DataField = 'COMMENT'
            DataSource = dmSample.dsSample
            PopupMenu = dmFormActions.pmRTF
            ScrollBars = ssVertical
            TabOrder = 6
            OnEnter = EnterRTF
            OnExit = ExitRTF
          end
          object dbeDuration: TDBEdit
            Left = 208
            Top = 188
            Width = 145
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'DURATION'
            DataSource = dmSample.dsSample
            TabOrder = 5
          end
          object eStartTime: TEdit
            Left = 80
            Top = 188
            Width = 49
            Height = 21
            TabOrder = 4
            OnExit = eStartTimeExit
          end
          object eSampleDate: TVagueDateEdit
            Left = 80
            Top = 134
            Width = 153
            Height = 21
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnExit = eSampleDateExit
          end
          inline fraLocationInfo: TfraLocationInfo
            Left = 8
            Top = 36
            Width = 345
            Height = 97
            HorzScrollBar.Visible = False
            VertScrollBar.Visible = False
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
            inherited Label6: TLabel
              Left = 4
              Top = 6
            end
            inherited Label7: TLabel
              Left = 4
              Top = 66
            end
            inherited Label8: TLabel
              Left = 4
              Width = 44
              Height = 26
              WordWrap = True
            end
            inherited eSpatialRef: TSpatialRef
              Left = 72
              Top = 58
              Width = 273
            end
            inherited eLocationName: TEdit
              Left = 72
              Top = 32
              Width = 273
              Anchors = [akLeft, akTop, akRight, akBottom]
              OnChange = nil
            end
            inherited eLocation: TAddinLinkedEdit
              Left = 72
              Top = 2
              Width = 273
              OnChange = fraLocationInfoeLocationChange
            end
          end
        end
        object tsSampleRecorder: TTabSheet
          Caption = 'Recorder(s)'
          ImageIndex = 3
          DesignSize = (
            365
            273)
          object Bevel2: TBevel
            Left = 4
            Top = 4
            Width = 355
            Height = 265
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object lblRecorders: TLabel
            Left = 12
            Top = 12
            Width = 52
            Height = 13
            Caption = 'Recorders:'
          end
          object clbRecorders: TCheckListBox
            Left = 12
            Top = 28
            Width = 337
            Height = 233
            OnClickCheck = clbRecordersClickCheck
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 13
            TabOrder = 0
          end
        end
        object tsMeasurements: TTabSheet
          Caption = 'Measurements'
          ImageIndex = 2
          object Measurements: TMeasurements
            Left = 4
            Top = 4
            Width = 355
            Height = 265
            SourceCol = clBlack
            DestCol = clBlack
            TabOrder = 0
            DesignSize = (
              355
              265)
          end
        end
        object tsRelations: TTabSheet
          Caption = 'Related Samples'
          ImageIndex = 1
          DesignSize = (
            365
            273)
          object Bevel3: TBevel
            Left = 4
            Top = 4
            Width = 357
            Height = 265
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object Label16: TLabel
            Left = 12
            Top = 12
            Width = 83
            Height = 13
            Caption = 'Related Samples:'
          end
          object shpRelatedSamples: TShape
            Tag = 2
            Left = 11
            Top = 27
            Width = 315
            Height = 231
            Anchors = [akLeft, akTop, akRight, akBottom]
            Pen.Color = clRed
          end
          object bbSampleAdd: TImageListButton
            Left = 326
            Top = 27
            Width = 24
            Height = 23
            Hint = 'Add new related sample'
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnClick = bbSampleAddClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 2
          end
          object bbSampleDel: TImageListButton
            Left = 326
            Top = 50
            Width = 24
            Height = 24
            Hint = 'Remove selected related sample'
            Anchors = [akTop, akRight]
            TabOrder = 2
            OnClick = bbSampleDelClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 4
          end
          object sgRelatedSamples: TStringGrid
            Left = 12
            Top = 28
            Width = 313
            Height = 229
            Anchors = [akLeft, akTop, akRight, akBottom]
            ColCount = 3
            DefaultRowHeight = 18
            FixedCols = 0
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
            TabOrder = 0
            OnClick = sgRelatedSamplesClick
            ColWidths = (
              106
              70
              130)
          end
        end
        object tsSources: TTabSheet
          Caption = 'Sources'
          ImageIndex = 4
          object Sources: TSources
            Left = 4
            Top = 4
            Width = 355
            Height = 265
            SourceCol = clBlue
            DestCol = clRed
            TabOrder = 0
            DesignSize = (
              355
              265)
          end
        end
        object tsAdminAreas: TTabSheet
          Caption = 'Administrative Areas'
          ImageIndex = 5
          DesignSize = (
            365
            273)
          object Shape3: TShape
            Tag = 2
            Left = 3
            Top = 3
            Width = 334
            Height = 265
            Anchors = [akLeft, akTop, akRight, akBottom]
            Pen.Color = clRed
          end
          object sgAdminAreas: TStringGrid
            Left = 4
            Top = 4
            Width = 332
            Height = 263
            Anchors = [akLeft, akTop, akRight, akBottom]
            ColCount = 2
            DefaultRowHeight = 18
            FixedCols = 0
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
            TabOrder = 0
            OnClick = sgAdminAreasClick
            ColWidths = (
              216
              83)
          end
          object bbAdminAdd: TImageListButton
            Left = 337
            Top = 4
            Width = 24
            Height = 23
            Hint = 'Add an administrative area'
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnClick = bbAdminAddClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 2
          end
          object bbAdminDel: TImageListButton
            Left = 337
            Top = 27
            Width = 24
            Height = 23
            Hint = 'Remove the selected administrative area'
            Anchors = [akTop, akRight]
            TabOrder = 2
            OnClick = bbAdminDelClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 4
          end
        end
        object tsPrivate: TTabSheet
          Caption = 'Private'
          ImageIndex = 6
          DesignSize = (
            365
            273)
          object Bevel4: TBevel
            Left = 4
            Top = 12
            Width = 355
            Height = 265
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object Label8: TLabel
            Left = 12
            Top = 51
            Width = 80
            Height = 13
            Caption = 'Private Location:'
            WordWrap = True
          end
          object Label1: TLabel
            Left = 12
            Top = 81
            Width = 64
            Height = 13
            Caption = 'Private Code:'
            WordWrap = True
          end
          object Label2: TLabel
            Left = 12
            Top = 16
            Width = 309
            Height = 26
            Anchors = [akLeft, akTop, akRight]
            Caption = 
              'Use the following fields to store details such as house names an' +
              'd numbers. This data should not be exported. '
            WordWrap = True
          end
          object Label3: TLabel
            Left = 12
            Top = 161
            Width = 52
            Height = 26
            Caption = 'Unparsed Recorders:'
            WordWrap = True
          end
          object Label4: TLabel
            Left = 12
            Top = 128
            Width = 313
            Height = 26
            Anchors = [akLeft, akTop, akRight]
            Caption = 
              'Used for unparsed recorders held for temporary surveys. This dat' +
              'a must not be exported. '
            WordWrap = True
          end
          object ePrivateLocation: TEdit
            Left = 96
            Top = 48
            Width = 249
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 100
            TabOrder = 0
          end
          object ePrivateCode: TEdit
            Left = 96
            Top = 78
            Width = 169
            Height = 21
            MaxLength = 20
            TabOrder = 1
          end
          object eUnparsed: TEdit
            Left = 96
            Top = 166
            Width = 249
            Height = 21
            MaxLength = 255
            TabOrder = 2
          end
        end
      end
    end
  end
  object pmMapWindow: TPopupMenu
    Images = dmFormActions.ilMenuOn
    Left = 40
    Top = 312
  end
end
