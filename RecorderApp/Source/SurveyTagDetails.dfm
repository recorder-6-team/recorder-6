inherited frmSurveyTagDetails: TfrmSurveyTagDetails
  Left = 388
  Top = 175
  Width = 393
  Height = 395
  Caption = 'Survey Tag Details'
  OldCreateOrder = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object pnlDetails: TPanel
    Left = 0
    Top = 0
    Width = 385
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
      object Bevel1: TBevel
        Left = 8
        Top = 24
        Width = 373
        Height = 301
        Anchors = [akLeft, akTop, akRight, akBottom]
        Shape = bsFrame
      end
      object Shape2: TShape
        Tag = 2
        Left = 17
        Top = 199
        Width = 332
        Height = 116
        Anchors = [akLeft, akTop, akRight, akBottom]
        Pen.Color = clRed
      end
      object Label1: TLabel
        Left = 16
        Top = 180
        Width = 41
        Height = 13
        Caption = 'Surveys:'
      end
      object Label2: TLabel
        Left = 16
        Top = 35
        Width = 58
        Height = 13
        Caption = 'Survey Tag:'
      end
      object Label3: TLabel
        Left = 16
        Top = 63
        Width = 56
        Height = 13
        Caption = 'Description:'
      end
      object lblSurveyTagDisp: TLabel
        Left = 8
        Top = 4
        Width = 67
        Height = 13
        Caption = '<Survey Tag>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblSortCode: TLabel
        Left = 16
        Top = 160
        Width = 47
        Height = 13
        Caption = 'Sort Code'
      end
      object eTagName: TEdit
        Left = 80
        Top = 32
        Width = 293
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 150
        TabOrder = 0
      end
      object btnSave: TImageListButton
        Left = 222
        Top = 330
        Width = 75
        Height = 25
        Hint = 'Save survey details'
        Anchors = [akRight, akBottom]
        Caption = '&Save'
        Enabled = False
        TabOrder = 1
        OnClick = btnSaveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
      end
      object btnCancel: TImageListButton
        Left = 306
        Top = 330
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
      object sgSurveys: TControlStringGrid
        Left = 18
        Top = 200
        Width = 330
        Height = 114
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColCount = 1
        DefaultRowHeight = 19
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 3
        ColWidths = (
          317)
      end
      object btnDeleteSurvey: TImageListButton
        Left = 349
        Top = 222
        Width = 24
        Height = 23
        Hint = 'Delete selected survey'
        Anchors = [akTop, akRight]
        TabOrder = 4
        ImageList = dmFormActions.ilButtons
        ImageIndex = 4
      end
      object btnAddSurvey: TImageListButton
        Left = 349
        Top = 200
        Width = 24
        Height = 23
        Hint = 'Add survey'
        Anchors = [akTop, akRight]
        TabOrder = 5
        ImageList = dmFormActions.ilButtons
        ImageIndex = 2
      end
      object mmDescription: TMemo
        Left = 80
        Top = 60
        Width = 293
        Height = 93
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 6
      end
      object eTagSortCode: TEdit
        Left = 80
        Top = 160
        Width = 73
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 10
        TabOrder = 7
        OnExit = eTagSortCodeExit
        OnKeyPress = eTagSortCodeKeyPress
      end
    end
  end
  object qrySurveys: TJNCCQuery
    ConnectionString = 
      'Provider=SQLOLEDB.1;Integrated Security=SSPI;Persist Security In' +
      'fo=False;Initial Catalog=NBNData;Data Source=DSWKS75\RECORDER'
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'UserNameKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'EXEC usp_Surveys_Select_ForSurveyTag :Key, 0, :UserNameKey')
    ParseSQL = False
    Left = 36
    Top = 240
    object qrySurveysSurvey_Tag_Key: TStringField
      FieldName = 'Survey_Tag_Key'
      Visible = False
      FixedChar = True
      Size = 16
    end
    object qrySurveysConcept_Key: TStringField
      FieldName = 'Concept_Key'
      Visible = False
      FixedChar = True
      Size = 16
    end
    object qrySurveysSurvey_Key: TStringField
      FieldName = 'Survey_Key'
      Visible = False
      FixedChar = True
      Size = 16
    end
    object qrySurveysItemName: TStringField
      DisplayLabel = 'Survey Name'
      DisplayWidth = 300
      FieldName = 'ItemName'
      Size = 203
    end
    object qrySurveysEntered_By: TStringField
      FieldName = 'Entered_By'
      Visible = False
      FixedChar = True
      Size = 16
    end
    object qrySurveysCustodian: TStringField
      FieldName = 'Custodian'
      Visible = False
      FixedChar = True
      Size = 8
    end
  end
end
