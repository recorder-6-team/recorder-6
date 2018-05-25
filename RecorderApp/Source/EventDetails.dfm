inherited frmEventDetails: TfrmEventDetails
  Left = 856
  Top = 156
  Width = 404
  Height = 446
  Caption = 'Event Details'
  OldCreateOrder = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object pnlDetails: TPanel
    Left = 0
    Top = 0
    Width = 388
    Height = 407
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
    OnResize = pnlDetailsResize
    object pnlInner: TPanel
      Left = 0
      Top = 0
      Width = 385
      Height = 409
      BevelOuter = bvLowered
      Caption = 'pnlInner'
      TabOrder = 0
      DesignSize = (
        385
        409)
      object dblblEventDate: TDBText
        Left = 8
        Top = 4
        Width = 73
        Height = 13
        AutoSize = True
        DataField = 'VAGUE_DATE_START'
        DataSource = dmEvent.dsEvent
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblEvent: TLabel
        Left = 176
        Top = 4
        Width = 53
        Height = 13
        Caption = '<Location>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object pcEventDetails: TPageControl
        Left = 8
        Top = 24
        Width = 373
        Height = 349
        ActivePage = tsGeneral
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        OnChange = pcEventDetailsChange
        object tsGeneral: TTabSheet
          Caption = 'General'
          DesignSize = (
            365
            321)
          object Bevel1: TBevel
            Left = 4
            Top = 4
            Width = 357
            Height = 313
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object lblEventDate: TLabel
            Left = 12
            Top = 100
            Width = 26
            Height = 13
            Caption = 'Date:'
          end
          object lblEventWeather: TLabel
            Left = 12
            Top = 124
            Width = 44
            Height = 13
            Caption = 'Weather:'
          end
          object shpLocationName: TShape
            Tag = 2
            Left = 97
            Top = 38
            Width = 236
            Height = 23
            Pen.Color = clRed
          end
          object pnlCommentsRecorders: TPanel
            Left = 8
            Top = 145
            Width = 349
            Height = 160
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelOuter = bvNone
            TabOrder = 3
            DesignSize = (
              349
              160)
            object pnlComment: TPanel
              Left = 0
              Top = 0
              Width = 349
              Height = 28
              Anchors = [akLeft, akTop, akRight]
              BevelOuter = bvNone
              TabOrder = 0
              DesignSize = (
                349
                28)
              object lblComment: TLabel
                Left = 4
                Top = 0
                Width = 47
                Height = 13
                Caption = 'Comment:'
              end
              object dbreComments: TDBRichEdit
                Left = 56
                Top = 0
                Width = 289
                Height = 28
                Anchors = [akLeft, akTop, akRight, akBottom]
                DataField = 'COMMENT'
                DataSource = dmEvent.dsEvent
                PopupMenu = dmFormActions.pmRTF
                ScrollBars = ssVertical
                TabOrder = 0
                OnEnter = EnterRTF
                OnExit = ExitRTF
              end
            end
            object pnlRecorders: TPanel
              Left = 0
              Top = 32
              Width = 349
              Height = 121
              Anchors = [akLeft, akTop, akRight]
              BevelOuter = bvNone
              TabOrder = 1
              DesignSize = (
                349
                121)
              object shpRecorders: TShape
                Tag = 2
                Left = 3
                Top = 15
                Width = 318
                Height = 106
                Anchors = [akLeft, akTop, akRight, akBottom]
                Pen.Color = clRed
              end
              object Label4: TLabel
                Left = 4
                Top = 0
                Width = 58
                Height = 13
                Caption = 'Recorder(s):'
              end
              object sgRecorders: TStringGrid
                Left = 4
                Top = 24
                Width = 316
                Height = 104
                Anchors = [akLeft, akTop, akRight, akBottom]
                ColCount = 2
                DefaultRowHeight = 18
                FixedCols = 0
                RowCount = 2
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing]
                TabOrder = 0
                OnClick = sgRecordersClick
                OnDrawCell = sgRecordersDrawCell
                OnKeyDown = sgRecordersKeyDown
                OnTopLeftChanged = sgRecordersTopLeftChanged
                ColWidths = (
                  181
                  116)
              end
              object bbRecorderFind: TImageListButton
                Left = 321
                Top = 15
                Width = 24
                Height = 23
                Hint = 'Get name(s)'
                Anchors = [akTop, akRight]
                TabOrder = 1
                OnClick = bbRecorderFindClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 5
              end
              object bbRecorderAdd: TImageListButton
                Left = 321
                Top = 38
                Width = 24
                Height = 23
                Hint = 'Find name'
                Anchors = [akTop, akRight]
                TabOrder = 2
                OnClick = bbRecorderAddClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 2
              end
              object bbRecorderRemove: TImageListButton
                Left = 321
                Top = 61
                Width = 24
                Height = 23
                Hint = 'Remove the selected name(s)'
                Anchors = [akTop, akRight]
                TabOrder = 3
                OnClick = bbRecorderRemoveClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 4
              end
              object cmbRecorderRole: TComboBox
                Left = 186
                Top = 36
                Width = 121
                Height = 21
                Style = csDropDownList
                ItemHeight = 13
                TabOrder = 4
                Visible = False
                OnChange = cmbRecorderRoleChange
                OnExit = cmbRecorderRoleExit
                Items.Strings = (
                  'Recorder'
                  'Surveyor')
              end
              object bbRecorderReplace: TImageListButton
                Left = 321
                Top = 85
                Width = 24
                Height = 23
                Hint = 'Replace the selected name(s) with another person.'
                Anchors = [akTop, akRight]
                TabOrder = 5
                OnClick = bbRecorderReplaceClick
                ImageList = dmFormActions.ilButtons
                ImageIndex = 3
              end
            end
          end
          object dbeEventWeather: TDBEdit
            Left = 64
            Top = 121
            Width = 289
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'SURVEY_EVENT_WEATHER'
            DataSource = dmEvent.dsEvent
            TabOrder = 2
          end
          object eEventDate: TVagueDateEdit
            Left = 64
            Top = 98
            Width = 133
            Height = 21
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnExit = eEventDateExit
          end
          inline fraLocationInfo: TfraLocationInfo
            Left = 8
            Top = 9
            Width = 345
            Height = 85
            HorzScrollBar.Visible = False
            VertScrollBar.Visible = False
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            inherited Label6: TLabel
              Left = 4
              Width = 75
              Caption = 'Event Location:'
            end
            inherited Label7: TLabel
              Left = 4
            end
            inherited Label8: TLabel
              Left = 4
            end
            inherited eSpatialRef: TSpatialRef
              Width = 263
            end
            inherited eLocationName: TEdit
              Top = 27
              Width = 263
            end
            inherited eLocation: TAddinLinkedEdit
              Width = 263
            end
          end
        end
        object tsOwnership: TTabSheet
          Caption = 'Ownership'
          ImageIndex = 2
          OnShow = tsOwnershipShow
          DesignSize = (
            365
            321)
          object Label2: TLabel
            Left = 12
            Top = 12
            Width = 230
            Height = 13
            Caption = 'Individuals && Organisations responsible for event:'
          end
          object Bevel2: TBevel
            Left = 4
            Top = 4
            Width = 357
            Height = 313
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
        end
        object tsSources: TTabSheet
          Caption = 'Sources'
          ImageIndex = 1
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
      end
      object bbSave: TImageListButton
        Left = 222
        Top = 378
        Width = 75
        Height = 25
        Hint = 'Save survey event details'
        Anchors = [akRight, akBottom]
        Caption = '&Save'
        Enabled = False
        TabOrder = 1
        OnClick = bbSaveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
      end
      object bbCancel: TImageListButton
        Left = 306
        Top = 378
        Width = 75
        Height = 25
        Hint = 'Cancel changes'
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = '&Cancel'
        Enabled = False
        TabOrder = 2
        OnClick = bbCancelClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 1
      end
    end
  end
  object pmMapWindow: TPopupMenu
    Images = dmFormActions.ilMenuOn
    Left = 48
    Top = 316
  end
end
