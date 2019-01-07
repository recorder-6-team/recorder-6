inherited frmSurveyDetails: TfrmSurveyDetails
  Left = 416
  Top = 374
  Width = 780
  Height = 395
  Caption = 'Survey Details'
  OldCreateOrder = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlDetails: TPanel
    Left = 0
    Top = 0
    Width = 772
    Height = 368
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
    OnResize = pnlDetailsResize
    object pnlInner: TPanel
      Left = 8
      Top = 8
      Width = 385
      Height = 353
      BevelOuter = bvLowered
      TabOrder = 0
      DesignSize = (
        385
        353)
      object lblRunbyPrompt: TLabel
        Left = 176
        Top = 4
        Width = 38
        Height = 13
        Caption = 'Run By:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblRunbyDisp: TLabel
        Left = 220
        Top = 4
        Width = 83
        Height = 13
        Caption = '<Survey Run By>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblSurveyNameDisp: TLabel
        Left = 8
        Top = 4
        Width = 165
        Height = 13
        AutoSize = False
        Caption = '<Survey Name>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object bbCancel: TImageListButton
        Left = 306
        Top = 314
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
        Top = 314
        Width = 75
        Height = 25
        Hint = 'Save survey details'
        Anchors = [akRight, akBottom]
        Caption = '&Save'
        Enabled = False
        TabOrder = 1
        OnClick = bbSaveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
      end
      object pcSurveyDetails: TPageControl
        Left = 8
        Top = 24
        Width = 373
        Height = 281
        ActivePage = Licence
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 2
        OnChange = pcSurveyDetailsChange
        OnResize = TagColumnResize
        object tsGeneral: TTabSheet
          Caption = 'General'
          OnResize = tsGeneralResize
          DesignSize = (
            365
            253)
          object bvlGeneral: TBevel
            Left = 4
            Top = 4
            Width = 357
            Height = 245
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object lblSurveyName: TLabel
            Left = 12
            Top = 14
            Width = 31
            Height = 13
            Caption = 'Name:'
          end
          object lblType: TLabel
            Left = 12
            Top = 64
            Width = 27
            Height = 13
            Caption = 'Type:'
          end
          object lblStartDate: TLabel
            Left = 12
            Top = 86
            Width = 66
            Height = 26
            Caption = 'Records Allowed From:'
            WordWrap = True
          end
          object lblEndDate: TLabel
            Left = 220
            Top = 94
            Width = 16
            Height = 13
            Caption = 'To:'
          end
          object lblSurveyStatus: TLabel
            Left = 12
            Top = 145
            Width = 33
            Height = 13
            Caption = 'Status:'
          end
          object lblSurveyRunBy: TLabel
            Left = 12
            Top = 40
            Width = 38
            Height = 13
            Caption = 'Run By:'
          end
          object lblSurveyMedia: TLabel
            Left = 212
            Top = 145
            Width = 32
            Height = 13
            Caption = 'Media:'
          end
          object lblDescription: TLabel
            Left = 12
            Top = 193
            Width = 56
            Height = 13
            Caption = 'Description:'
          end
          object Label4: TLabel
            Left = 12
            Top = 169
            Width = 51
            Height = 13
            Caption = 'Periodicity:'
          end
          object lblOperatingFrom: TLabel
            Left = 12
            Top = 118
            Width = 75
            Height = 13
            Alignment = taCenter
            Caption = 'Operating From:'
          end
          object lblOperatingTo: TLabel
            Left = 220
            Top = 118
            Width = 16
            Height = 13
            Alignment = taCenter
            Caption = 'To:'
            WordWrap = True
          end
          object Label2: TLabel
            Left = 204
            Top = 169
            Width = 70
            Height = 13
            Caption = 'Date of Import:'
          end
          object dbcmbSurveyStatus: TDBLookupComboBox
            Left = 76
            Top = 140
            Width = 129
            Height = 21
            DataField = 'SURVEY_STATUS_KEY'
            DataSource = dmSurvey.dsSurvey
            KeyField = 'Survey_Status_Key'
            ListField = 'Short_Name'
            ListSource = dmSurvey.dsSurveyStatus
            TabOrder = 7
            OnClick = dbComboClick
            OnKeyDown = dbComboKeyDown
            OnKeyUp = dbComboKeyUp
          end
          object dbcmbSurveyMedia: TDBLookupComboBox
            Left = 248
            Top = 140
            Width = 105
            Height = 21
            DataField = 'SURVEY_MEDIA_KEY'
            DataSource = dmSurvey.dsSurvey
            KeyField = 'Survey_Media_Key'
            ListField = 'Short_Name'
            ListSource = dmSurvey.dsSurveyMedia
            TabOrder = 8
            OnClick = dbComboClick
            OnKeyDown = dbComboKeyDown
            OnKeyUp = dbComboKeyUp
          end
          object dbcmbSurveyType: TDBLookupComboBox
            Left = 76
            Top = 64
            Width = 277
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'SURVEY_TYPE_KEY'
            DataSource = dmSurvey.dsSurvey
            KeyField = 'Survey_Type_Key'
            ListField = 'Short_Name'
            ListSource = dmSurvey.dsSurveyType
            TabOrder = 2
            OnClick = dbComboClick
            OnKeyDown = dbComboKeyDown
            OnKeyUp = dbComboKeyUp
          end
          object dbeSurveyName: TDBEdit
            Left = 76
            Top = 12
            Width = 277
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'ITEM_NAME'
            DataSource = dmSurvey.dsSurvey
            TabOrder = 0
          end
          object dbePeriodicity: TDBEdit
            Left = 76
            Top = 165
            Width = 125
            Height = 21
            DataField = 'PERIODICITY'
            DataSource = dmSurvey.dsSurvey
            TabOrder = 9
          end
          object dbreDescription: TDBRichEdit
            Left = 76
            Top = 193
            Width = 277
            Height = 48
            Anchors = [akLeft, akTop, akRight, akBottom]
            DataField = 'DESCRIPTION'
            DataSource = dmSurvey.dsSurvey
            PopupMenu = dmFormActions.pmRTF
            ScrollBars = ssVertical
            TabOrder = 10
            OnEnter = EnterRTF
            OnExit = ExitRTF
          end
          object eSurveyFrom: TVagueDateEdit
            Left = 100
            Top = 88
            Width = 109
            Height = 21
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            OnExit = SurveyDateValidate
          end
          object eSurveyTo: TVagueDateEdit
            Left = 244
            Top = 88
            Width = 109
            Height = 21
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 4
            OnExit = SurveyDateValidate
          end
          object eSurveyOpFrom: TVagueDateEdit
            Left = 100
            Top = 114
            Width = 109
            Height = 21
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 5
            OnExit = SurveyDateValidate
          end
          object eSurveyOpTo: TVagueDateEdit
            Left = 244
            Top = 114
            Width = 109
            Height = 21
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 6
            OnExit = SurveyDateValidate
          end
          object eSurveyRunBy: TNameLinkedEdit
            Tag = 1
            Left = 76
            Top = 36
            Width = 277
            Height = 23
            TabOrder = 1
            BorderStyle = bsSingle
            ImageIndex = 5
            ImageList = dmFormActions.ilButtons
            OnFindData = eSurveyRunByFindData
            OnGetData = eSurveyRunByGetData
          end
          object eImportDate: TEdit
            Left = 280
            Top = 165
            Width = 73
            Height = 21
            TabOrder = 11
            OnExit = eImportDateExit
          end
        end
        object tsGeography: TTabSheet
          Caption = 'Geography'
          ImageIndex = 1
          DesignSize = (
            365
            253)
          object Bevel2: TBevel
            Left = 4
            Top = 4
            Width = 355
            Height = 245
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object Label7: TLabel
            Left = 12
            Top = 152
            Width = 106
            Height = 13
            Caption = 'Geographic coverage:'
          end
          object gbSpatialRefCorners: TGroupBox
            Left = 12
            Top = 12
            Width = 341
            Height = 129
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Bounding Box:'
            TabOrder = 0
            DesignSize = (
              341
              129)
            object Label5: TLabel
              Left = 148
              Top = 104
              Width = 98
              Height = 13
              Caption = '<- South-west corner'
            end
            object Label30: TLabel
              Left = 104
              Top = 20
              Width = 91
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'North-east corner->'
            end
            object Label1: TLabel
              Left = 64
              Top = 48
              Width = 225
              Height = 39
              Anchors = [akTop]
              Caption = 
                'Specify the grid-reference of the north-east and south-west corn' +
                'ers of a box enclosing all the locations that the survey covers.'
              WordWrap = True
            end
            object Bevel3: TBevel
              Left = 62
              Top = 42
              Width = 229
              Height = 51
              Anchors = [akTop]
              Shape = bsFrame
            end
            object btnMap: TImageListButton
              Left = 292
              Top = 97
              Width = 24
              Height = 23
              Hint = 'Get bounding box from map'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = btnMapClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 5
            end
            object eNECorner: TEdit
              Left = 200
              Top = 16
              Width = 133
              Height = 21
              Anchors = [akTop, akRight]
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
            object btnMapDropDown: TButton
              Left = 315
              Top = 97
              Width = 16
              Height = 23
              Anchors = [akTop, akRight]
              Caption = '6'
              Font.Charset = SYMBOL_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Marlett'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 3
              OnClick = btnMapDropDownClick
            end
          end
          object dbreGeoCoverage: TDBRichEdit
            Left = 12
            Top = 172
            Width = 341
            Height = 69
            Anchors = [akLeft, akTop, akRight, akBottom]
            DataField = 'GEOGRAPHIC_COVERAGE'
            DataSource = dmSurvey.dsSurvey
            PopupMenu = dmFormActions.pmRTF
            ScrollBars = ssVertical
            TabOrder = 1
            OnEnter = EnterRTF
            OnExit = ExitRTF
          end
        end
        object tsSurveyTags: TTabSheet
          Caption = 'Tags'
          ImageIndex = 3
          OnShow = tsSurveyTagsShow
          DesignSize = (
            365
            253)
          object Shape2: TShape
            Tag = 2
            Left = 3
            Top = 3
            Width = 335
            Height = 247
            Anchors = [akLeft, akTop, akRight, akBottom]
            Pen.Color = clRed
          end
          object btnAddTag: TImageListButton
            Left = 338
            Top = 4
            Width = 24
            Height = 23
            Hint = 'Add Tag'
            Anchors = [akTop, akRight]
            TabOrder = 0
            OnClick = btnAddTagClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 2
          end
          object btnRemoveTag: TImageListButton
            Left = 338
            Top = 26
            Width = 24
            Height = 23
            Hint = 'Delete selected tag'
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnClick = btnRemoveTagClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 4
          end
          object sgSurveyTags: TControlStringGrid
            Left = 4
            Top = 4
            Width = 333
            Height = 245
            Anchors = [akLeft, akTop, akRight, akBottom]
            ColCount = 1
            DefaultRowHeight = 19
            FixedCols = 0
            RowCount = 2
            ScrollBars = ssVertical
            TabOrder = 2
            ColWidths = (
              332)
          end
        end
        object tsSources: TTabSheet
          Caption = 'Sources'
          ImageIndex = 2
          object Sources: TSources
            Left = 4
            Top = 4
            Width = 357
            Height = 265
            SourceCol = clBlue
            DestCol = clRed
            TabOrder = 0
            DesignSize = (
              357
              265)
          end
        end
        object Licence: TTabSheet
          Caption = 'Licence'
          ImageIndex = 4
          DesignSize = (
            365
            253)
          object lblLicence: TLabel
            Left = 12
            Top = 8
            Width = 41
            Height = 13
            Caption = 'Licence:'
          end
          object lblAttribution: TLabel
            Left = 12
            Top = 49
            Width = 50
            Height = 13
            Caption = 'Attribution:'
          end
          object lblNotes: TLabel
            Left = 12
            Top = 153
            Width = 73
            Height = 13
            Caption = 'Notes (Private):'
          end
          object dbcmbLicence: TDBLookupComboBox
            Left = 100
            Top = 8
            Width = 241
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'LICENCE_KEY'
            DataSource = dmSurvey.dsSurvey
            KeyField = 'Licence_Key'
            ListField = 'Long_Name'
            ListSource = dmSurvey.dsSurveyLicence
            TabOrder = 0
            OnClick = dbComboClick
            OnKeyDown = dbComboKeyDown
            OnKeyUp = dbComboKeyUp
          end
          object dbreAttribution: TDBRichEdit
            Left = 100
            Top = 49
            Width = 261
            Height = 88
            Anchors = [akLeft, akTop, akRight]
            DataField = 'ATTRIBUTION'
            DataSource = dmSurvey.dsSurvey
            PopupMenu = dmFormActions.pmRTF
            ScrollBars = ssVertical
            TabOrder = 1
            OnEnter = EnterRTF
            OnExit = ExitRTF
          end
          object dbreNotes: TDBRichEdit
            Left = 100
            Top = 153
            Width = 261
            Height = 64
            Anchors = [akLeft, akTop, akRight, akBottom]
            DataField = 'PRIVATE_NOTES'
            DataSource = dmSurvey.dsSurvey
            PopupMenu = dmFormActions.pmRTF
            ScrollBars = ssVertical
            TabOrder = 2
            OnEnter = EnterRTF
            OnExit = ExitRTF
          end
          object dbcbTemporary: TDBCheckBox
            Left = 100
            Top = 228
            Width = 189
            Height = 17
            Anchors = [akLeft, akBottom]
            Caption = 'Survey Holds Temporary Data'
            DataField = 'TEMPORARY_SURVEY'
            DataSource = dmSurvey.dsSurvey
            TabOrder = 3
            ValueChecked = 'True'
            ValueUnchecked = 'False'
          end
        end
      end
    end
  end
  object dlgOpen: TOpenDialog
    Left = 12
    Top = 324
  end
  object pmMapWindow: TPopupMenu
    Images = dmFormActions.ilMenuOn
    Left = 104
    Top = 324
  end
end
