inherited frmIndOrg: TfrmIndOrg
  Left = 270
  Top = 266
  Width = 714
  Height = 455
  Caption = 'Names & Addresses'
  Constraints.MinHeight = 446
  Constraints.MinWidth = 550
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object splIndOrg: TSplitter [0]
    Left = 273
    Top = 0
    Width = 10
    Height = 375
    Align = alRight
    MinSize = 100
    OnCanResize = splIndOrgCanResize
    OnMoved = FormResize
    OnPaint = splIndOrgPaint
  end
  object pnlButtons: TPanel [1]
    Left = 0
    Top = 375
    Width = 706
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object pnlButtons2: TPanel
      Left = 512
      Top = 0
      Width = 194
      Height = 34
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 3
      object bbShowAll: TButton
        Left = -4
        Top = 6
        Width = 75
        Height = 25
        Hint = 'Remove Filter on Hierarchy'
        Caption = '&Show All'
        TabOrder = 0
        Visible = False
        OnClick = bbShowAllClick
      end
      object bbRelatedData: TImageListButton
        Left = 100
        Top = 6
        Width = 93
        Height = 25
        Hint = 'Access data related to the selected Individual/Organisation'
        Caption = 'Re&lated Data'
        TabOrder = 1
        OnClick = bbRelatedDataClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 14
        Layout = blGlyphRight
        Spacing = -1
      end
    end
    object bbEdit: TImageListButton
      Left = 88
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Edit the selected Individual/Organisation'
      Caption = 'Ed&it'
      TabOrder = 1
      OnClick = bbEditClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 3
      Spacing = -1
    end
    object bbDelete: TImageListButton
      Left = 168
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Delete the selected Individual/Organisation'
      Caption = 'Delete'
      TabOrder = 2
      OnClick = bbDeleteClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 4
      Spacing = -1
    end
    object bbAdd: TImageListButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Add an Individual/Organisation'
      Caption = '&Add'
      TabOrder = 0
      OnClick = bbAddClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 2
      Spacing = -1
    end
  end
  object pnlIndOrg: TPanel [2]
    Left = 0
    Top = 0
    Width = 273
    Height = 375
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlLabel: TPanel
      Left = 0
      Top = 0
      Width = 273
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblName: TLabel
        Left = 68
        Top = 6
        Width = 55
        Height = 13
        Caption = 'Names List:'
      end
      object sbIndividuals: TSpeedButton
        Left = 0
        Top = 1
        Width = 25
        Height = 22
        Hint = 'List Individuals'
        GroupIndex = 1
        Down = True
        Glyph.Data = {
          36050000424D3605000000000000360400002800000010000000100000000100
          0800000000000001000000000000000000000001000000010000000000000000
          330000006600000099000000CC000000FF000033000000333300003366000033
          99000033CC000033FF00006600000066330000666600006699000066CC000066
          FF00009900000099330000996600009999000099CC000099FF0000CC000000CC
          330000CC660000CC990000CCCC0000CCFF0000FF000000FF330000FF660000FF
          990000FFCC0000FFFF00330000003300330033006600330099003300CC003300
          FF00333300003333330033336600333399003333CC003333FF00336600003366
          330033666600336699003366CC003366FF003399000033993300339966003399
          99003399CC003399FF0033CC000033CC330033CC660033CC990033CCCC0033CC
          FF0033FF000033FF330033FF660033FF990033FFCC0033FFFF00660000006600
          330066006600660099006600CC006600FF006633000066333300663366006633
          99006633CC006633FF00666600006666330066666600666699006666CC006666
          FF00669900006699330066996600669999006699CC006699FF0066CC000066CC
          330066CC660066CC990066CCCC0066CCFF0066FF000066FF330066FF660066FF
          990066FFCC0066FFFF00990000009900330099006600990099009900CC009900
          FF00993300009933330099336600993399009933CC009933FF00996600009966
          330099666600996699009966CC009966FF009999000099993300999966009999
          99009999CC009999FF0099CC000099CC330099CC660099CC990099CCCC0099CC
          FF0099FF000099FF330099FF660099FF990099FFCC0099FFFF00CC000000CC00
          3300CC006600CC009900CC00CC00CC00FF00CC330000CC333300CC336600CC33
          9900CC33CC00CC33FF00CC660000CC663300CC666600CC669900CC66CC00CC66
          FF00CC990000CC993300CC996600CC999900CC99CC00CC99FF00CCCC0000CCCC
          3300CCCC6600CCCC9900CCCCCC00CCCCFF00CCFF0000CCFF3300CCFF6600CCFF
          9900CCFFCC00CCFFFF00FF000000FF003300FF006600FF009900FF00CC00FF00
          FF00FF330000FF333300FF336600FF339900FF33CC00FF33FF00FF660000FF66
          3300FF666600FF669900FF66CC00FF66FF00FF990000FF993300FF996600FF99
          9900FF99CC00FF99FF00FFCC0000FFCC3300FFCC6600FFCC9900FFCCCC00FFCC
          FF00FFFF0000FFFF3300FFFF6600FFFF9900FFFFCC00FFFFFF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000B9B9B9000000
          00B900000000B9B9B9B9B9B9B900906C00B900486C00B9B9B9B9B9B9B9009090
          6C006C6C4800B9B9B9B9B9B9B9B900909000906C00B9B9B9B9B9B9B900B900B4
          90906C9000B900B9B9B9B900BA000090B490906C00006C00B9B9B900B4BA0000
          90B4900000906C00B9B9B9B900B4BA00B490B400906C00B9B9B9B9B9B900B4BA
          B4B490B49000B9B9B9B9B9B9B9B900B4BAB4B49000B9B9B9B9B9B9B9B9B9B900
          B4BAB400B9B9B9B9B9B9B9B9B9B9B9B900B400B9B9B9B9B9B9B9B9B9B9B9B900
          BABAB400B9B9B9B9B9B9B9B9B9B900BAC0BABAB400B9B9B9B9B9B9B9B9B900C0
          BAC0BABA00B9B9B9B9B9B9B9B9B9B90000000000B9B9B9B9B9B9}
        Spacing = -1
        OnClick = sbIndividualsClick
      end
      object sbOrganisations: TSpeedButton
        Left = 24
        Top = 1
        Width = 25
        Height = 22
        Hint = 'List Organisations'
        GroupIndex = 1
        Glyph.Data = {
          36050000424D3605000000000000360400002800000010000000100000000100
          0800000000000001000000000000000000000001000000010000000000000000
          330000006600000099000000CC000000FF000033000000333300003366000033
          99000033CC000033FF00006600000066330000666600006699000066CC000066
          FF00009900000099330000996600009999000099CC000099FF0000CC000000CC
          330000CC660000CC990000CCCC0000CCFF0000FF000000FF330000FF660000FF
          990000FFCC0000FFFF00330000003300330033006600330099003300CC003300
          FF00333300003333330033336600333399003333CC003333FF00336600003366
          330033666600336699003366CC003366FF003399000033993300339966003399
          99003399CC003399FF0033CC000033CC330033CC660033CC990033CCCC0033CC
          FF0033FF000033FF330033FF660033FF990033FFCC0033FFFF00660000006600
          330066006600660099006600CC006600FF006633000066333300663366006633
          99006633CC006633FF00666600006666330066666600666699006666CC006666
          FF00669900006699330066996600669999006699CC006699FF0066CC000066CC
          330066CC660066CC990066CCCC0066CCFF0066FF000066FF330066FF660066FF
          990066FFCC0066FFFF00990000009900330099006600990099009900CC009900
          FF00993300009933330099336600993399009933CC009933FF00996600009966
          330099666600996699009966CC009966FF009999000099993300999966009999
          99009999CC009999FF0099CC000099CC330099CC660099CC990099CCCC0099CC
          FF0099FF000099FF330099FF660099FF990099FFCC0099FFFF00CC000000CC00
          3300CC006600CC009900CC00CC00CC00FF00CC330000CC333300CC336600CC33
          9900CC33CC00CC33FF00CC660000CC663300CC666600CC669900CC66CC00CC66
          FF00CC990000CC993300CC996600CC999900CC99CC00CC99FF00CCCC0000CCCC
          3300CCCC6600CCCC9900CCCCCC00CCCCFF00CCFF0000CCFF3300CCFF6600CCFF
          9900CCFFCC00CCFFFF00FF000000FF003300FF006600FF009900FF00CC00FF00
          FF00FF330000FF333300FF336600FF339900FF33CC00FF33FF00FF660000FF66
          3300FF666600FF669900FF66CC00FF66FF00FF990000FF993300FF996600FF99
          9900FF99CC00FF99FF00FFCC0000FFCC3300FFCC6600FFCC9900FFCCCC00FFCC
          FF00FFFF0000FFFF3300FFFF6600FFFF9900FFFFCC00FFFFFF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000B90000000000
          0000B900000000000000B900486C00906C00B900486C00906C00006C6C480090
          906C006C6C480090906C00906C00B900909000906C00B9009090906C9000B900
          B490906C9000B900B490909000BA000090B490906C00006C00B4B49000B4BA00
          0090B4900000906C009090B40000B4BA00B490B400906C0000B4B490B49000B4
          BAB4B490B49000B4BAB4B4B49000B900B4BAB4B49000B900B4BABAB400B9B9B9
          00B4BAB400B9B9B900B4B400B9B9B9B9B900B400B9B9B9B9B900BAB400B9B9B9
          00BABAB400B9B9B900BABABAB400B900BAC0BABAB400B900BAC0C0BABA00B900
          C0BAC0BABA00B900C0BA000000B9B9B90000000000B9B9B90000}
        Spacing = -1
        OnClick = sbOrganisationsClick
      end
    end
    object pnlLists: TPanel
      Tag = 1
      Left = 0
      Top = 25
      Width = 273
      Height = 350
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 1
      Color = clLime
      TabOrder = 1
      object tvOrganisations: TKeyboardRapidTree
        Left = 1
        Top = 1
        Width = 271
        Height = 348
        SmoothExpandCollapse = False
        FitColumnToClientWidth = True
        FitToHeight = False
        DoubleBuffered = True
        Align = alClient
        TransparentMode = True
        DefaultRowHeight = 16
        DragMode = dmAutomatic
        RowCount = 0
        FixedRows = 0
        GridLineWidth = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
        PopupMenu = pmHierarchy
        TabOrder = 1
        OnExpanding = tvExpanding
        OnChange = tvChange
        OnCompare = tvCompare
        HideSelection = False
        Showlines = True
        ShowRoot = True
        ShowButtons = True
        ShowImages = True
        ShowLogic = False
        Images = ilNames
        SortType = stNone
        WordWrap = False
        AutoMove = False
        ToolTips = False
        AutoExpand = False
        TooltipColor = clInfoBk
        ToolTipPause = 1000
        StatesDrawed = True
        Data = {0400000000000000}
      end
      object tvIndividuals: TKeyboardRapidTree
        Left = 1
        Top = 1
        Width = 271
        Height = 348
        SmoothExpandCollapse = False
        FitColumnToClientWidth = True
        FitToHeight = False
        DoubleBuffered = True
        Align = alClient
        TransparentMode = True
        DefaultRowHeight = 16
        DragMode = dmAutomatic
        RowCount = 0
        FixedRows = 0
        GridLineWidth = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
        PopupMenu = pmHierarchy
        TabOrder = 0
        OnKeyDown = tvIndividualsKeyDown
        OnExpanding = tvExpanding
        OnChange = tvChange
        OnCompare = tvCompare
        HideSelection = False
        Showlines = True
        ShowRoot = True
        ShowButtons = True
        ShowImages = True
        ShowLogic = False
        Images = ilNames
        SortType = stNone
        WordWrap = False
        AutoMove = False
        ToolTips = False
        AutoExpand = False
        TooltipColor = clInfoBk
        ToolTipPause = 1000
        StatesDrawed = True
        ColWidths = (
          267)
        Data = {0400000000000000}
      end
    end
  end
  object pnlIndOrgDetails: TPanel [3]
    Left = 283
    Top = 0
    Width = 423
    Height = 375
    Align = alRight
    BevelOuter = bvLowered
    Constraints.MinWidth = 423
    TabOrder = 2
    OnResize = pnlIndOrgDetailsResize
    object pnlInner: TPanel
      Left = 0
      Top = 0
      Width = 423
      Height = 367
      BevelOuter = bvLowered
      TabOrder = 0
      DesignSize = (
        423
        367)
      object lblNamePrompt: TLabel
        Left = 8
        Top = 4
        Width = 76
        Height = 13
        Caption = 'Selected Name:'
      end
      object lblSelectedName: TLabel
        Left = 90
        Top = 4
        Width = 163
        Height = 13
        Caption = '<Selected individual/organisation>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object bbSave: TImageListButton
        Left = 255
        Top = 334
        Width = 75
        Height = 25
        Hint = 'Save Individual/Organisation Details'
        Anchors = [akRight, akBottom]
        Caption = '&Save'
        Enabled = False
        ModalResult = 1
        TabOrder = 0
        OnClick = bbSaveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
      end
      object bbCancel: TImageListButton
        Left = 339
        Top = 334
        Width = 75
        Height = 25
        Hint = 'Cancel Changes'
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = '&Cancel'
        Enabled = False
        ModalResult = 2
        TabOrder = 1
        OnClick = bbCancelClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 1
      end
      object pcNameDetails: TPageControl
        Left = 8
        Top = 24
        Width = 405
        Height = 305
        ActivePage = tsBiography
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 2
        OnChange = pcNameDetailsChange
        OnChanging = pcNameDetailsChanging
        object tsIndividual: TTabSheet
          Caption = 'Individual'
          DesignSize = (
            397
            277)
          object bvlIndividual: TBevel
            Left = 4
            Top = 3
            Width = 389
            Height = 270
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object lblITitle: TLabel
            Left = 16
            Top = 16
            Width = 23
            Height = 13
            Caption = 'Title:'
          end
          object lblIForenames: TLabel
            Left = 16
            Top = 40
            Width = 55
            Height = 13
            Caption = 'Forenames:'
          end
          object lblIIntitials: TLabel
            Left = 276
            Top = 40
            Width = 32
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Initials:'
          end
          object lblISurname: TLabel
            Left = 16
            Top = 64
            Width = 45
            Height = 13
            Caption = 'Surname:'
          end
          object lblIAddress: TLabel
            Left = 16
            Top = 90
            Width = 41
            Height = 13
            Caption = 'Address:'
          end
          object Label1: TLabel
            Left = 16
            Top = 225
            Width = 26
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Dept:'
          end
          object mmIAddress: TMemo
            Left = 72
            Top = 88
            Width = 297
            Height = 129
            TabStop = False
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clBtnFace
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 4
          end
          object dbeIForenames: TDBEdit
            Left = 72
            Top = 38
            Width = 193
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'FORENAME'
            DataSource = dmName.dsIndividual
            TabOrder = 1
          end
          object dbeIInitials: TDBEdit
            Left = 312
            Top = 38
            Width = 57
            Height = 21
            Anchors = [akTop, akRight]
            DataField = 'INITIALS'
            DataSource = dmName.dsIndividual
            TabOrder = 2
          end
          object dbeISurname: TDBEdit
            Left = 72
            Top = 62
            Width = 297
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'SURNAME'
            DataSource = dmName.dsIndividual
            TabOrder = 3
          end
          object dbcbTitle: TDBComboBox
            Left = 72
            Top = 14
            Width = 57
            Height = 21
            DataField = 'TITLE'
            DataSource = dmName.dsIndividual
            ItemHeight = 13
            Items.Strings = (
              'Col'
              'Dr'
              'Miss'
              'Mr'
              'Mrs'
              'Ms'
              'Prof'
              'Rev')
            Sorted = True
            TabOrder = 0
          end
          object cmbDept: TIDComboBox
            Left = 72
            Top = 222
            Width = 297
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akRight, akBottom]
            ItemHeight = 0
            TabOrder = 5
            OnPopulate = cmbDeptPopulate
          end
        end
        object tsOrganisation: TTabSheet
          Caption = 'Organisation'
          ImageIndex = 5
          TabVisible = False
          DesignSize = (
            397
            277)
          object bvlOrganisation: TBevel
            Left = 4
            Top = 4
            Width = 389
            Height = 269
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object lblOAcronym: TLabel
            Left = 16
            Top = 9
            Width = 52
            Height = 26
            Caption = 'Acronym / Full name:'
            WordWrap = True
          end
          object lblType: TLabel
            Left = 16
            Top = 39
            Width = 27
            Height = 13
            Caption = 'Type:'
          end
          object lblOFounded: TLabel
            Left = 16
            Top = 64
            Width = 45
            Height = 13
            Caption = 'Founded:'
          end
          object lblEnded: TLabel
            Left = 196
            Top = 64
            Width = 59
            Height = 13
            Caption = 'Date ended:'
          end
          object Label40: TLabel
            Left = 16
            Top = 89
            Width = 47
            Height = 13
            Caption = 'Comment:'
          end
          object lblAddress: TLabel
            Left = 16
            Top = 131
            Width = 41
            Height = 13
            Caption = 'Address:'
          end
          object mmOAddress: TMemo
            Left = 68
            Top = 128
            Width = 301
            Height = 137
            TabStop = False
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clBtnFace
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 6
          end
          object dbeOAcronym: TDBEdit
            Tag = 1
            Left = 68
            Top = 12
            Width = 65
            Height = 21
            DataField = 'ACRONYM'
            DataSource = dmName.dsOrganisation
            TabOrder = 0
          end
          object dbeOFullName: TDBEdit
            Tag = 1
            Left = 136
            Top = 12
            Width = 233
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'FULL_NAME'
            DataSource = dmName.dsOrganisation
            TabOrder = 1
          end
          object dbeODateFounded: TDBEdit
            Tag = 1
            Left = 68
            Top = 62
            Width = 121
            Height = 21
            DataField = 'FOUNDED_VAGUE_DATE_START'
            DataSource = dmName.dsOrganisation
            TabOrder = 3
            OnExit = dbeODateFoundedExit
          end
          object dbeODateEnded: TDBEdit
            Tag = 1
            Left = 256
            Top = 62
            Width = 113
            Height = 21
            DataField = 'ENDED_VAGUE_DATE_START'
            DataSource = dmName.dsOrganisation
            TabOrder = 4
            OnExit = dbeODateEndedExit
          end
          object dbreComment: TDBRichEdit
            Tag = 1
            Left = 68
            Top = 87
            Width = 301
            Height = 37
            Anchors = [akLeft, akTop, akRight]
            DataField = 'COMMENT'
            DataSource = dmName.dsOrganisation
            PopupMenu = dmFormActions.pmRTF
            ScrollBars = ssVertical
            TabOrder = 5
            OnEnter = reEnter
            OnExit = reExit
          end
          object dblcOOrgType: TDBLookupComboBox
            Tag = 1
            Left = 68
            Top = 37
            Width = 301
            Height = 21
            DataField = 'ORGANISATION_TYPE_KEY'
            DataSource = dmName.dsOrganisation
            KeyField = 'ORGANISATION_TYPE_KEY'
            ListField = 'SHORT_NAME'
            ListSource = dmName.dsOrganisationType
            TabOrder = 2
          end
        end
        object tsAddresses: TTabSheet
          BorderWidth = 4
          Caption = 'Addresses'
          ImageIndex = 1
          object splAddresses: TSplitter
            Left = 0
            Top = 81
            Width = 389
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            Beveled = True
            MinSize = 73
          end
          object gbAddressDetails: TGroupBox
            Left = 0
            Top = 84
            Width = 389
            Height = 185
            Align = alBottom
            Caption = 'Details'
            Constraints.MinHeight = 169
            TabOrder = 0
            DesignSize = (
              389
              185)
            object Label19: TLabel
              Left = 8
              Top = 85
              Width = 42
              Height = 26
              Caption = 'Post/zip Code:'
              WordWrap = True
            end
            object Label18: TLabel
              Left = 160
              Top = 90
              Width = 39
              Height = 13
              Caption = 'Country:'
            end
            object Label21: TLabel
              Left = 248
              Top = 42
              Width = 16
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'To:'
            end
            object Label20: TLabel
              Left = 248
              Top = 18
              Width = 26
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'From:'
            end
            object Label22: TLabel
              Left = 8
              Top = 116
              Width = 52
              Height = 13
              Caption = 'Comments:'
            end
            object Label38: TLabel
              Left = 8
              Top = 16
              Width = 41
              Height = 13
              Caption = 'Address:'
            end
            object eACountry: TEdit
              Left = 204
              Top = 88
              Width = 177
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 4
            end
            object rbAHome: TRadioButton
              Left = 8
              Top = 160
              Width = 93
              Height = 17
              Anchors = [akLeft, akBottom]
              Caption = '&Home Address'
              Checked = True
              Enabled = False
              TabOrder = 6
              TabStop = True
            end
            object rbAWork: TRadioButton
              Left = 112
              Top = 160
              Width = 93
              Height = 17
              Anchors = [akLeft, akBottom]
              Caption = '&Work Address'
              Enabled = False
              TabOrder = 7
            end
            object cbAPreferred: TCheckBox
              Left = 232
              Top = 160
              Width = 65
              Height = 17
              Anchors = [akLeft, akBottom]
              Caption = 'Preferred'
              TabOrder = 8
            end
            object mmAAddress: TMemo
              Left = 64
              Top = 16
              Width = 181
              Height = 69
              Anchors = [akLeft, akTop, akRight]
              ScrollBars = ssVertical
              TabOrder = 0
              OnChange = mmAAddressChange
            end
            object reAComments: TRichEdit
              Left = 64
              Top = 112
              Width = 317
              Height = 37
              Anchors = [akLeft, akTop, akRight, akBottom]
              PopupMenu = dmFormActions.pmRTF
              ScrollBars = ssVertical
              TabOrder = 5
              OnEnter = reEnter
              OnExit = reExit
            end
            object eAFrom: TVagueDateEdit
              Left = 276
              Top = 16
              Width = 105
              Height = 21
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGreen
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              OnExit = eAFromExit
            end
            object eATo: TVagueDateEdit
              Left = 276
              Top = 40
              Width = 105
              Height = 21
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGreen
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              OnExit = eAToExit
            end
            object bbAAccept: TImageListButton
              Left = 332
              Top = 154
              Width = 24
              Height = 23
              Hint = 'Accept changes'
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 9
              OnClick = bbAAcceptClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 6
            end
            object bbACancel: TImageListButton
              Left = 356
              Top = 154
              Width = 24
              Height = 23
              Hint = 'Discard changes'
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 10
              OnClick = bbACancelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 7
            end
            object eAPostCode: TEdit
              Left = 64
              Top = 88
              Width = 77
              Height = 21
              MaxLength = 10
              TabOrder = 3
            end
          end
          object pnlAddressesTop: TPanel
            Left = 0
            Top = 0
            Width = 389
            Height = 81
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              389
              81)
            object sgAAddresses: TStringGrid
              Left = 0
              Top = 0
              Width = 365
              Height = 77
              Anchors = [akLeft, akTop, akRight, akBottom]
              ColCount = 3
              DefaultRowHeight = 18
              FixedCols = 0
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
              TabOrder = 0
              OnClick = sgAAddressesClick
              OnDrawCell = GridDrawCell
              ColWidths = (
                19
                271
                70)
            end
            object bbAAdd: TImageListButton
              Tag = 1
              Left = 365
              Top = 0
              Width = 24
              Height = 23
              Hint = 'Add new address'
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = bbAAddClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 2
            end
            object bbAEdit: TImageListButton
              Tag = 1
              Left = 365
              Top = 23
              Width = 24
              Height = 23
              Hint = 'Edit selected address'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = bbAEditClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 3
            end
            object bbADel: TImageListButton
              Tag = 1
              Left = 365
              Top = 46
              Width = 24
              Height = 23
              Hint = 'Delete selected address'
              Anchors = [akTop, akRight]
              TabOrder = 3
              OnClick = bbADelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 4
            end
          end
        end
        object tsContacts: TTabSheet
          BorderWidth = 4
          Caption = 'Contact Nos.'
          ImageIndex = 2
          object splContactNos: TSplitter
            Left = 0
            Top = 97
            Width = 389
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            Beveled = True
            MinSize = 73
          end
          object gbContactDetails: TGroupBox
            Left = 0
            Top = 100
            Width = 389
            Height = 169
            Align = alBottom
            Caption = 'Details'
            Constraints.MinHeight = 137
            TabOrder = 0
            DesignSize = (
              389
              169)
            object Label23: TLabel
              Left = 12
              Top = 16
              Width = 27
              Height = 13
              Caption = 'Type:'
            end
            object Label24: TLabel
              Left = 260
              Top = 14
              Width = 29
              Height = 13
              Caption = 'Prefix:'
            end
            object Label25: TLabel
              Left = 12
              Top = 40
              Width = 40
              Height = 13
              Caption = 'Number:'
            end
            object Label26: TLabel
              Left = 12
              Top = 64
              Width = 55
              Height = 13
              Caption = 'Constraints:'
            end
            object eCPrefix: TEdit
              Left = 296
              Top = 12
              Width = 81
              Height = 21
              TabOrder = 1
            end
            object eCNumber: TEdit
              Left = 56
              Top = 38
              Width = 321
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 2
            end
            object cbCPreferred: TCheckBox
              Left = 12
              Top = 144
              Width = 65
              Height = 17
              Anchors = [akLeft, akBottom]
              Caption = 'Preferred'
              TabOrder = 4
            end
            object cmbCType: TComboBox
              Left = 56
              Top = 12
              Width = 165
              Height = 21
              ItemHeight = 13
              Sorted = True
              TabOrder = 0
              Items.Strings = (
                'Email'
                'Fax'
                'Mobile'
                'Phone'
                'Web-Site')
            end
            object reCConstraints: TRichEdit
              Left = 12
              Top = 80
              Width = 369
              Height = 53
              Anchors = [akLeft, akTop, akRight, akBottom]
              PopupMenu = dmFormActions.pmRTF
              ScrollBars = ssVertical
              TabOrder = 3
              OnEnter = reEnter
              OnExit = reExit
            end
            object bbCAccept: TImageListButton
              Left = 332
              Top = 138
              Width = 24
              Height = 23
              Hint = 'Accept changes'
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 5
              OnClick = bbCAcceptClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 6
            end
            object bbCCancel: TImageListButton
              Left = 356
              Top = 138
              Width = 24
              Height = 23
              Hint = 'Discard changes'
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 6
              OnClick = bbCCancelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 7
            end
          end
          object pnlContactNosTop: TPanel
            Left = 0
            Top = 0
            Width = 389
            Height = 97
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              389
              97)
            object sgCContactNumbers: TStringGrid
              Left = 0
              Top = 0
              Width = 365
              Height = 93
              Anchors = [akLeft, akTop, akRight, akBottom]
              ColCount = 4
              DefaultRowHeight = 18
              FixedCols = 0
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
              TabOrder = 0
              OnClick = sgCContactNumbersClick
              OnDrawCell = GridDrawCell
              ColWidths = (
                21
                91
                55
                193)
            end
            object bbCAdd: TImageListButton
              Tag = 1
              Left = 365
              Top = 0
              Width = 24
              Height = 23
              Hint = 'Add new contact number'
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = bbCAddClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 2
            end
            object bbCEdit: TImageListButton
              Tag = 1
              Left = 365
              Top = 23
              Width = 24
              Height = 23
              Hint = 'Edit selected contact number'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = bbCEditClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 3
            end
            object bbCDel: TImageListButton
              Tag = 1
              Left = 365
              Top = 46
              Width = 24
              Height = 23
              Hint = 'Delete selected contact'
              Anchors = [akTop, akRight]
              TabOrder = 3
              OnClick = bbCDelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 4
            end
          end
        end
        object tsComms: TTabSheet
          BorderWidth = 4
          Caption = 'Comms.'
          ImageIndex = 3
          object splComms: TSplitter
            Left = 0
            Top = 97
            Width = 389
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            Beveled = True
            MinSize = 73
          end
          object gbCODetails: TGroupBox
            Left = 0
            Top = 100
            Width = 389
            Height = 169
            Align = alBottom
            Caption = 'Details'
            Constraints.MinHeight = 141
            TabOrder = 0
            DesignSize = (
              389
              169)
            object Label27: TLabel
              Left = 12
              Top = 19
              Width = 25
              Height = 13
              Caption = 'With:'
            end
            object Label29: TLabel
              Left = 12
              Top = 45
              Width = 27
              Height = 13
              Caption = 'Type:'
            end
            object lblCommDate: TLabel
              Left = 212
              Top = 45
              Width = 26
              Height = 13
              Caption = 'Date:'
            end
            object Label30: TLabel
              Left = 12
              Top = 68
              Width = 40
              Height = 13
              Caption = 'Content:'
            end
            object Label31: TLabel
              Left = 12
              Top = 143
              Width = 72
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'File Reference:'
            end
            object cmbCOType: TComboBox
              Left = 48
              Top = 41
              Width = 137
              Height = 21
              Enabled = False
              ItemHeight = 13
              Sorted = True
              TabOrder = 1
              Items.Strings = (
                'Agreement'
                'EMail'
                'Fax'
                'Letter'
                'Telephone')
            end
            object eCORef: TEdit
              Left = 88
              Top = 140
              Width = 213
              Height = 21
              Anchors = [akLeft, akRight, akBottom]
              TabOrder = 4
            end
            object reCOContent: TRichEdit
              Left = 12
              Top = 84
              Width = 365
              Height = 49
              Anchors = [akLeft, akTop, akRight, akBottom]
              PopupMenu = dmFormActions.pmRTF
              ScrollBars = ssVertical
              TabOrder = 3
              OnEnter = reEnter
              OnExit = reExit
            end
            object eCODate: TVagueDateEdit
              Left = 248
              Top = 41
              Width = 129
              Height = 21
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGreen
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              OnExit = eCODateExit
            end
            object bbCOAccept: TImageListButton
              Left = 329
              Top = 138
              Width = 24
              Height = 23
              Hint = 'Accept changes'
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 5
              OnClick = bbCOAcceptClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 6
            end
            object bbCOCancel: TImageListButton
              Left = 353
              Top = 138
              Width = 24
              Height = 23
              Hint = 'Discard changes'
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 6
              OnClick = bbCOCancelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 7
            end
            object eCOName: TNameLinkedEdit
              Tag = 1
              Left = 48
              Top = 15
              Width = 329
              Height = 23
              TabOrder = 0
              BorderStyle = bsSingle
              ImageIndex = 5
              ImageList = dmFormActions.ilButtons
              OnFindData = eCONameFindData
              ShowButton = False
            end
          end
          object pnlCommsTop: TPanel
            Left = 0
            Top = 0
            Width = 389
            Height = 97
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              389
              97)
            object Shape3: TShape
              Tag = 2
              Left = 0
              Top = 0
              Width = 365
              Height = 94
              Anchors = [akLeft, akTop, akRight, akBottom]
              Pen.Color = clRed
            end
            object sgCOComms: TStringGrid
              Left = 1
              Top = 1
              Width = 363
              Height = 92
              Anchors = [akLeft, akTop, akRight, akBottom]
              ColCount = 3
              DefaultRowHeight = 18
              FixedCols = 0
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
              TabOrder = 0
              OnClick = sgCOCommsClick
              OnDrawCell = DrawCellChoppedText
              ColWidths = (
                79
                215
                66)
            end
            object bbCOAdd: TImageListButton
              Tag = 1
              Left = 365
              Top = 0
              Width = 24
              Height = 23
              Hint = 'Add new communication'
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = bbCOAddClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 2
            end
            object bbCOEdit: TImageListButton
              Tag = 1
              Left = 365
              Top = 23
              Width = 24
              Height = 23
              Hint = 'Edit selected communication'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = bbCOEditClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 3
            end
            object bbCODel: TImageListButton
              Tag = 1
              Left = 365
              Top = 46
              Width = 24
              Height = 23
              Hint = 'Delete selected communication'
              Anchors = [akTop, akRight]
              TabOrder = 3
              OnClick = bbCODelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 4
            end
          end
        end
        object tsAssoc: TTabSheet
          BorderWidth = 4
          Caption = 'Assocs.'
          ImageIndex = 4
          object splAssocs: TSplitter
            Left = 0
            Top = 97
            Width = 389
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            Beveled = True
            MinSize = 73
          end
          object gbASDetails: TGroupBox
            Left = 0
            Top = 100
            Width = 389
            Height = 169
            Align = alBottom
            Caption = 'Details'
            Constraints.MinHeight = 141
            TabOrder = 0
            DesignSize = (
              389
              169)
            object Label34: TLabel
              Left = 12
              Top = 19
              Width = 25
              Height = 13
              Caption = 'With:'
            end
            object Label37: TLabel
              Left = 12
              Top = 44
              Width = 25
              Height = 13
              Caption = 'Role:'
            end
            object Label32: TLabel
              Left = 12
              Top = 143
              Width = 26
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'From:'
            end
            object Label33: TLabel
              Left = 176
              Top = 143
              Width = 16
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'To:'
            end
            object Label28: TLabel
              Left = 12
              Top = 68
              Width = 52
              Height = 13
              Caption = 'Comments:'
            end
            object Label36: TLabel
              Left = 188
              Top = 44
              Width = 59
              Height = 13
              Caption = 'Name Code:'
            end
            object cmbASRole: TComboBox
              Left = 48
              Top = 40
              Width = 121
              Height = 21
              ItemHeight = 13
              Sorted = True
              TabOrder = 1
            end
            object eASNameCode: TEdit
              Left = 252
              Top = 40
              Width = 125
              Height = 21
              TabOrder = 2
            end
            object reASComment: TRichEdit
              Left = 12
              Top = 84
              Width = 365
              Height = 49
              Anchors = [akLeft, akTop, akRight, akBottom]
              PopupMenu = dmFormActions.pmRTF
              ScrollBars = ssVertical
              TabOrder = 3
              OnEnter = reEnter
              OnExit = reExit
            end
            object eASDateFrom: TVagueDateEdit
              Left = 48
              Top = 140
              Width = 121
              Height = 21
              Anchors = [akLeft, akBottom]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGreen
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 4
              OnExit = eASDateFromExit
            end
            object eASDateTo: TVagueDateEdit
              Left = 200
              Top = 140
              Width = 121
              Height = 21
              Anchors = [akLeft, akBottom]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGreen
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 5
              OnExit = eASDateToExit
            end
            object bbASAccept: TImageListButton
              Left = 332
              Top = 138
              Width = 24
              Height = 23
              Hint = 'Accept changes'
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 6
              OnClick = bbASAcceptClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 6
            end
            object bbASCancel: TImageListButton
              Left = 356
              Top = 138
              Width = 24
              Height = 23
              Hint = 'Discard changes'
              Anchors = [akRight, akBottom]
              Enabled = False
              TabOrder = 7
              OnClick = bbASCancelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 7
            end
            object eASName: TNameLinkedEdit
              Tag = 1
              Left = 48
              Top = 15
              Width = 329
              Height = 23
              TabOrder = 0
              BorderStyle = bsSingle
              ImageIndex = 5
              ImageList = dmFormActions.ilButtons
              OnFindData = eASNameFindData
              ShowButton = False
            end
          end
          object pnlAssocsTop: TPanel
            Left = 0
            Top = 0
            Width = 389
            Height = 97
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              389
              97)
            object Shape4: TShape
              Tag = 2
              Left = 0
              Top = 0
              Width = 365
              Height = 94
              Anchors = [akLeft, akTop, akRight, akBottom]
              Pen.Color = clRed
            end
            object sgASAssocs: TStringGrid
              Left = 1
              Top = 1
              Width = 363
              Height = 92
              Anchors = [akLeft, akTop, akRight, akBottom]
              DefaultRowHeight = 18
              FixedCols = 0
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
              TabOrder = 0
              OnClick = sgASAssocsClick
              OnDrawCell = DrawCellChoppedText
              ColWidths = (
                136
                53
                61
                55
                55)
            end
            object bbASDel: TImageListButton
              Tag = 1
              Left = 365
              Top = 47
              Width = 24
              Height = 23
              Hint = 'Delete selected association'
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = bbASDelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 4
            end
            object bbASEdit: TImageListButton
              Tag = 1
              Left = 365
              Top = 24
              Width = 24
              Height = 23
              Hint = 'Edit selected association'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = bbASEditClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 3
            end
            object bbASAdd: TImageListButton
              Tag = 1
              Left = 365
              Top = 1
              Width = 24
              Height = 23
              Hint = 'Add new association'
              Anchors = [akTop, akRight]
              TabOrder = 3
              OnClick = bbASAddClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 2
            end
          end
        end
        object tsBiography: TTabSheet
          Caption = 'Biography'
          ImageIndex = 7
          DesignSize = (
            397
            277)
          object Bevel3: TBevel
            Left = 4
            Top = 4
            Width = 389
            Height = 269
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object lblHHonorifics: TLabel
            Left = 12
            Top = 18
            Width = 50
            Height = 13
            Caption = 'Honorifics:'
          end
          object lblHDateOfBirth: TLabel
            Left = 12
            Top = 50
            Width = 62
            Height = 13
            Caption = 'Date of Birth:'
          end
          object lblHDateOfDeath: TLabel
            Left = 204
            Top = 50
            Width = 70
            Height = 13
            Caption = 'Date of Death:'
          end
          object lblHFloreat: TLabel
            Left = 12
            Top = 114
            Width = 35
            Height = 13
            Caption = 'Floreat:'
          end
          object lblHGeneralComments: TLabel
            Left = 12
            Top = 144
            Width = 92
            Height = 13
            Caption = 'General Comments:'
          end
          object lblHActiveFrom: TLabel
            Left = 12
            Top = 82
            Width = 66
            Height = 13
            Caption = 'Active Period:'
          end
          object dbeHHonorifics: TDBEdit
            Left = 88
            Top = 16
            Width = 121
            Height = 21
            DataField = 'HONORIFICS'
            DataSource = dmName.dsIndividual
            TabOrder = 0
          end
          object dbeHFloreat: TDBEdit
            Left = 88
            Top = 112
            Width = 121
            Height = 21
            DataField = 'PERSON_FLOREAT'
            DataSource = dmName.dsIndividual
            TabOrder = 3
          end
          object dbreComments: TDBRichEdit
            Left = 12
            Top = 160
            Width = 373
            Height = 105
            Anchors = [akLeft, akTop, akRight, akBottom]
            DataField = 'COMMENT'
            DataSource = dmName.dsIndividual
            PopupMenu = dmFormActions.pmRTF
            ScrollBars = ssVertical
            TabOrder = 4
            OnEnter = reEnter
            OnExit = reExit
          end
          object eHDateOfBirth: TVagueDateEdit
            Left = 88
            Top = 48
            Width = 105
            Height = 21
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnExit = eHDateOfBirthExit
          end
          object eHDateOfDeath: TVagueDateEdit
            Left = 280
            Top = 48
            Width = 105
            Height = 21
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnExit = eHDateOfDeathExit
          end
          object eHActivePeriod: TVagueDateEdit
            Left = 88
            Top = 80
            Width = 105
            Height = 21
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 5
            OnExit = eHActivePeriodExit
          end
        end
        object tsDepartments: TTabSheet
          Caption = 'Depts.'
          ImageIndex = 8
          TabVisible = False
          OnShow = tsDepartmentsShow
          DesignSize = (
            397
            277)
          object Label2: TLabel
            Left = 4
            Top = 4
            Width = 159
            Height = 13
            Caption = 'Departments for this Organisation:'
          end
          object sgDepartments: TControlStringGrid
            Left = 4
            Top = 20
            Width = 365
            Height = 253
            Anchors = [akLeft, akTop, akRight, akBottom]
            ColCount = 2
            DefaultRowHeight = 18
            FixedCols = 0
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
            TabOrder = 0
            OnClick = sgDepartmentsClick
            ColWidths = (
              113
              236)
          end
          object btnDeptDel: TImageListButton
            Tag = 1
            Left = 369
            Top = 44
            Width = 24
            Height = 23
            Hint = 'Delete selected department'
            Anchors = [akTop, akRight]
            Enabled = False
            TabOrder = 1
            OnClick = btnDeptDelClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 4
          end
          object btnDeptAdd: TImageListButton
            Tag = 1
            Left = 369
            Top = 21
            Width = 24
            Height = 23
            Hint = 'Add new department'
            Anchors = [akTop, akRight]
            Enabled = False
            TabOrder = 2
            OnClick = btnDeptAddClick
            ImageList = dmFormActions.ilButtons
            ImageIndex = 2
          end
        end
        object tsSources: TTabSheet
          Caption = 'Sources'
          ImageIndex = 6
          object Sources: TSources
            Left = 8
            Top = 8
            Width = 385
            Height = 265
            SourceCol = clBlue
            DestCol = clRed
            TabOrder = 0
            DesignSize = (
              385
              265)
          end
        end
      end
    end
  end
  inherited mnuChildMerge: TMainMenu
    Left = 44
    Top = 40
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 2
      object mnuEditAdd: TMenuItem
        Caption = '&Add'
        Hint = 'Add an Individual/Organisation'
        object mnuEditAddIndividual: TMenuItem
          Action = actAddIndividual
        end
        object mnuEditAddOrganisation: TMenuItem
          Action = actAddOrganisation
        end
      end
      object mnuEditEdit: TMenuItem
        Caption = '&Edit'
        Hint = 'Edit the selected Individual/Organisation'
        OnClick = bbEditClick
      end
      object mnuEditDelete: TMenuItem
        Caption = '&Delete'
        Hint = 'Delete the selected Individual/Organisation'
        OnClick = bbDeleteClick
      end
      object mnuEditSep1: TMenuItem
        Caption = '-'
      end
      object mnuEditCut: TMenuItem
        Action = dmFormActions.actCut
      end
      object mnuEditCopy: TMenuItem
        Action = dmFormActions.actCopy
      end
      object mnuEditPaste: TMenuItem
        Action = dmFormActions.actPaste
      end
      object mnuEditTransferData: TMenuItem
        Action = dmFormActions.actTransferData
      end
      object mnuEditSep2: TMenuItem
        Caption = '-'
      end
      object mnuEditFind: TMenuItem
        Action = actFind
      end
      object mnuEditSortBy: TMenuItem
        Caption = 'S&ort By'
        Hint = 'Sort the Individuals/Organisations'
        ImageIndex = 7
        object mnuEditSortForename: TMenuItem
          Action = actSortForename
        end
        object mnuEditSortSurname: TMenuItem
          Action = actSortSurname
        end
        object mnuEditSortAcronym: TMenuItem
          Action = actSortAcronym
        end
        object mnuEditSortFullName: TMenuItem
          Action = actSortFullName
        end
      end
      object mnuEditFilter: TMenuItem
        Action = actFilter
      end
      object mnuEditShowMetadata: TMenuItem
        Action = actShowMetadata
      end
      object mnuEditSep3: TMenuItem
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
  object pmRelatedData: TPopupMenu
    Left = 44
    Top = 244
    object mnuRelSurveys: TMenuItem
      Caption = '&Surveys...'
      Hint = 'Show related Surveys for the selected Individual/Organisation'
      ImageIndex = 14
      OnClick = mnuRelSurveysClick
    end
    object mnuRelEvents: TMenuItem
      Caption = '&Events...'
      Hint = 
        'Show related Survey Events for the selected Individual/Organisat' +
        'ion'
      OnClick = mnuRelEventsClick
    end
    object mnuRelSamples: TMenuItem
      Caption = 'S&amples...'
      Hint = 'Show related Samples for the selected Individual/Organisation'
      OnClick = mnuRelSamplesClick
    end
    object mnuRelOccur: TMenuItem
      Caption = '&Occurrences...'
      Hint = 
        'Show related Occurrences for the selected Individual/Organisatio' +
        'n'
      OnClick = mnuRelOccurClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object mnuRelLocations: TMenuItem
      Caption = '&Locations...'
      Hint = 'Show related Locations for the selected Individual/Organisation'
      ImageIndex = 13
      OnClick = mnuRelLocationsClick
    end
    object mnuRelDocuments: TMenuItem
      Caption = '&Documents...'
      Hint = 
        'Show related Documents/References for the selected Individual/Or' +
        'ganisation'
      ImageIndex = 16
      OnClick = mnuRelDocumentsClick
    end
  end
  object pmAdd: TPopupMenu
    Left = 43
    Top = 142
    object pmAddIndividual: TMenuItem
      Action = actAddIndividual
    end
    object pmAddOrganisation: TMenuItem
      Action = actAddOrganisation
    end
  end
  object pmSortBy: TPopupMenu
    Left = 43
    Top = 90
    object pmSortForename: TMenuItem
      Action = actSortForename
    end
    object pmSortSurname: TMenuItem
      Action = actSortSurname
    end
    object pmSortAcronym: TMenuItem
      Action = actSortAcronym
    end
    object pmSortFullName: TMenuItem
      Action = actSortFullName
    end
  end
  object pmHierarchy: TPopupMenu
    Left = 44
    Top = 196
    object pmHAdd: TMenuItem
      Caption = '&Add'
      Hint = 'Add an Individual/Organisation'
      object pmHAddIndividual: TMenuItem
        Action = actAddIndividual
      end
      object pmHAddOrganisation: TMenuItem
        Action = actAddOrganisation
      end
    end
    object pmHSortBy: TMenuItem
      Caption = 'S&ort'
      ImageIndex = 7
      object pmHSortForename: TMenuItem
        Action = actSortForename
      end
      object pmHSortSurname: TMenuItem
        Action = actSortSurname
      end
      object pmHSortAcronym: TMenuItem
        Action = actSortAcronym
      end
      object pmHSortFullName: TMenuItem
        Action = actSortFullName
      end
    end
    object pmHQuickReports: TMenuItem
      Caption = '&Quick Report'
      OnClick = pmHQuickReportsClick
      object mnuDummy: TMenuItem
        Caption = 'Dummy Sub Menu'
        ImageIndex = 6
        Visible = False
        OnClick = actFilterExecute
      end
    end
    object pmHBatchUpdate: TMenuItem
      Caption = '&Batch Updates'
      OnClick = pmHBatchUpdateClick
      object mnuPlaceHolder: TMenuItem
        Caption = 'PlaceHolder'
        Visible = False
      end
      object TMenuItem
      end
    end
    object pmHValidateItem: TMenuItem
      Action = dmFormActions.actDatabaseValidateSelected
    end
  end
  object alNames: TActionList
    Images = dmFormActions.ilMenuOn
    Left = 104
    Top = 41
    object actAddIndividual: TAction
      Caption = '&Individual'
      Hint = 'Add an individual'
      OnExecute = actAddIndividualExecute
    end
    object actAddOrganisation: TAction
      Caption = '&Organisation'
      Hint = 'Add an organisation'
      OnExecute = actAddOrganisationExecute
    end
    object actSortForename: TAction
      Caption = '&Forename'
      Hint = 'Sort individuals by forename'
      OnExecute = actSortForenameExecute
    end
    object actSortSurname: TAction
      Caption = '&Surname'
      Hint = 'Sort individuals by surname'
      OnExecute = actSortSurnameExecute
    end
    object actSortAcronym: TAction
      Caption = '&Acronym'
      Hint = 'Sort organisations by acronym'
      OnExecute = actSortAcronymExecute
    end
    object actSortFullName: TAction
      Caption = '&Full Name'
      Hint = 'Sort organisations by full name'
      OnExecute = actSortFullNameExecute
    end
    object actFind: TAction
      Caption = '&Find...'
      Hint = 'Find an Individual/Organisation'
      ImageIndex = 5
      ShortCut = 16454
      OnExecute = actFindExecute
    end
    object actFilter: TAction
      Caption = '&Simple Filter...'
      Hint = 'Apply a filter on the Names'
      ImageIndex = 6
      OnExecute = actFilterExecute
    end
    object actShowMetadata: TAction
      Caption = 'Show &Metadata'
      Hint = 'Show Metadata'
      ImageIndex = 52
      OnExecute = actShowMetadataExecute
    end
  end
  object ilNames: TImageList
    Left = 396
    Top = 12
    Bitmap = {
      494C010104000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FEFEFE00FDFDFD00FBFBFB00FAFAFA00FCFCFC00FEFEFE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FEFEFE00FDFDFD00FBFBFB00FAFAFA00FCFCFC00FEFEFE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBFBFB00DFE7DF0096CB96006EBE6E006CBC6C008FC48F00D2DAD200F4F4
      F400FEFEFE0000000000000000000000000000000000FDFDFC00CBCBAC00B0B0
      8000AEAE7E00AEAE7E00AEAE7E00AEAE7E00AEAE7E00AEAE7E00AEAE7E00AEAE
      7E00B0B08000CBCBAC00FDFDFC00000000000000000000000000000000000000
      0000FBFBFB00DFE7DF0096CB96006EBE6E006CBC6C008FC48F00D2DAD200F4F4
      F400FEFEFE0000000000000000000000000000000000CCD3BA00BBC5A400BBC5
      A400BBC5A400BBC5A400BBC5A400BBC5A400BBC5A400BBC5A400BBC5A400BBC5
      A400BBC5A400BBC5A400CCD3BA00000000000000000000000000FEFEFE00F4F5
      F4007BC77B000CAB0C0000AA000000AA000000AA000000AA00000BAA0B006CB8
      6C00E3E4E300FDFDFD00000000000000000000000000CBCBAC00949454009494
      5400949454009494540094945400949454009494540094945400949454009494
      54009494540094945400CBCBAC00000000000000000000000000FEFEFE00F4F5
      F4007BC77B000CAB0C0000AA000000AA000000AA000000AA00000BAA0B006CB8
      6C00E3E4E300FDFDFD00000000000000000000000000BBC5A400A5B28600A5B2
      8600A5B28600A5B28600A5B28600A5B28600A5B28600A5B28600A5B28600A5B2
      8600A5B28600A5B28600BBC5A400000000000000000000000000F7F8F7008BCD
      77007FD47F007FD47F007FD47F007FD47F007FD47F007FD47F007FD47F007FD4
      7F0063BF7A00E3E4E300FEFEFE000000000000000000B0B0800094945400DFD7
      B100000000000000000000000000000000000000000000000000000000000000
      0000C9D2C60094945400B0B08000000000000000000000000000F7F8F7006DC5
      63003FBF3F003FBF3F003FBF3F003FBF3F003FBF3F003DBF3F000FAE0F003FBF
      3F0076C37F00E3E4E300FEFEFE000000000000000000BBC5A400A5B28600E4E2
      C700000000000000000000000000000000000000000000000000B8C2A400FFFF
      FA0000000000A9BB9F00BBC5A4000000000000000000FEFEFE0082CE82005DC1
      3400FFFFFC00000000000000000000000000000000000000000000000000FAFF
      FF002DBE55006DB86D00F4F4F4000000000000000000AEAE7E0094945400B3AD
      7600F2EFDF00000000000000000000000000000000000000000000000000E9EE
      E900A6AB820094945400AEAE7E000000000000000000FEFEFE0082CE820076C9
      4E000000000000000000000000000000000000000000F6FFFF003FBB3D000000
      0000CDF4F6006DB86D00F4F4F4000000000000000000BBC5A400A5B28600E3E1
      C6000000000000000000000000000000000000000000F3F8F700B6BD9900FFFF
      FB00FEFFFF00A5B89A00BBC5A4000000000000000000ECF4EC000EAD0E0000AA
      000034B71F009ADB8D00F7FAEC000000000000000000EAF9F5008ADB9A001FB6
      310000AA00000BAA0B00D2DAD200FEFEFE0000000000AEAE7E00949454009494
      540094945400B2AD7600E8E4CD000000000000000000DEE3D900A7AB80009494
      54009494540094945400AEAE7E000000000000000000ECF4EC000EAD0E0064C3
      3A0000000000000000000000000000000000FCFFFF0088DCAA005FC23E000000
      0000ADEBDA000BAA0B00D2DAD200FEFEFE0000000000BBC5A400A5B28600B4BC
      9100E3E4D0000000000000000000F5F8F700CAD5C200B4BE9A00E7E7D100F2F6
      F300C4D1BE00A5B28600BBC5A4000000000000000000A8DDA80000AA000000AA
      000000AA000000AA00003CB61900FFFFF800F3FEFF0017B4350000AA000000AA
      000000AA000000AA00008FC48F00FCFCFC0000000000AEAE7E00949454009494
      54009494540094945400A69D5700FFFFFB00F9FFFF0095986A00949454009494
      54009494540094945400AEAE7E000000000000000000A8DDA80000AA000000AA
      000043BC2F00E1F0CA0000000000A6E6C3001BB4290087D16700E8F7E8006ED1
      830008AE130000AA00008FC48F00FCFCFC0000000000BBC5A400A5B28600A5B2
      8600A5B28600E3E1C60000000000AFC1A800A5B28600E0DEC100EEF5F700A5B4
      8C00A5B28600A5B28600BBC5A400000000000000000082D2820000AA000000AA
      000000AA000000AA000045B91E00FFFFFA00F5FEFF001EB8400000AA000000AA
      000000AA000000AA00006CBC6C00FBFBFB0000000000AEAE7E00949454009494
      54009494540094945400C5BC86000000000000000000B1BA9F00949454009494
      54009494540094945400AEAE7E00000000000000000082D2820000AA000000AA
      000000AA00007ECC56000000000021BA490000AA0000ADDB8100A5E7CE0000AA
      000000AA000000AA00006CBC6C00FBFBFB0000000000BBC5A400A5B28600A5B2
      8600A5B28600EEEAD40000000000BACAB700A5B28600ECE8D200F8FCFD00A9B9
      9900A5B28600A5B28600BBC5A400000000000000000083D2830000AA000000AA
      000000AA000000AA0000C9E59E00000000000000000099E3C50000AA000000AA
      000000AA000000AA00006EBE6E00FBFBFB0000000000AEAE7E00949454009494
      54009494540098945400F8F3DF000000000000000000E6EFF000949458009494
      54009494540094945400AEAE7E00000000000000000083D2830000AA000000AA
      000006AA0000D9ECB500000000007FDAAA0014AC0000F5F8DE00F3FDFF0011B3
      300000AA000000AA00006EBE6E00FBFBFB0000000000BBC5A400A5B28600A5B2
      8600BDC19600FFFEFB0000000000E3EEEE00AAB28600FFFCF20000000000BACB
      B700A5B28600A5B28600BBC5A4000000000000000000AAE0AA0000AA000000AA
      000000AA000000AA0000E5EEB9000000000000000000B5ECDF0000AA000000AA
      000000AA000000AA000096CB9600FDFDFD0000000000AEAE7E00949454009494
      54009494540094945400FCF4DF000000000000000000E5EEF300949454009494
      54009494540094945400AEAE7E000000000000000000AAE0AA0000AA000000AA
      000028AF0300FFFFF80000000000C7F2F1001BAB0000FFFEEE000000000010B5
      380000AA000000AA000096CB9600FDFDFD0000000000BBC5A400A5B28600A5B2
      8600BFC295000000000000000000E7F1F400A7B28600FAF6E80000000000B4C5
      AE00A5B28600A5B28600BBC5A4000000000000000000F0F8F0000FAE0F0000AA
      000000AA000000AA0000DEECB3000000000000000000B1EBDC0000AA000000AA
      000000AA00000CAB0C00DFE7DF00FEFEFE0000000000AEAE7E00949454009494
      54009494540095945400F9F1DC000000000000000000E5EEF200949454009494
      54009494540094945400AEAE7E000000000000000000F0F8F0000FAE0F0000AA
      000028AF0000FFFFFA0000000000C6F2F10003AA0000A3DB86009EE1AF0000AB
      0E0000AA00000CAB0C00DFE7DF00FEFEFE0000000000BBC5A400A5B28600A5B2
      8600BCBF9200FFFFFD0000000000E4EFF000A5B28600B1BA9100B4C1A000A5B2
      8600A5B28600A5B28600BBC5A4000000000000000000FEFEFE008AD58A0000AA
      000000AA000000AA000041BB2B00BCE7B200AFE6BA0028BA3F0000AA000000AA
      000000AA00007BC77B00FBFBFB000000000000000000AEAE7E00949454009494
      54009494540094945400B1AC7400E3E0CB00DDDFD000A6AB8000949454009494
      54009494540094945400AEAE7E000000000000000000FEFEFE008AD58A0000AA
      000003AA0000A1DA8300C2EAC2005BCD7C0000AA000000AA000000AA000000AA
      000000AA00007BC77B00FBFBFB000000000000000000BBC5A400A5B28600A5B2
      8600A5B28600CBCFAF00D2D9C300B2C0A200A5B28600A5B28600A5B28600A5B2
      8600A5B28600A5B28600BBC5A400000000000000000000000000FCFDFC0057C5
      570000AA000000AA000000AA000000AA000000AA000000AA000000AA000000AA
      000050BD5000F4F5F400000000000000000000000000B0B08000949454009494
      5400949454009494540094945400949454009494540094945400949454009494
      54009494540094945400B0B08000000000000000000000000000FCFDFC0057C5
      570000AA000000AA000000AA000000AA000000AA000000AA000000AA000000AA
      000050BD5000F4F5F400000000000000000000000000BBC5A400A5B28600A5B2
      8600A5B28600A5B28600A5B28600A5B28600A5B28600A5B28600A5B28600A5B2
      8600A5B28600A5B28600BBC5A40000000000000000000000000000000000FCFD
      FC0089D589000FAE0F0000AA000000AA000000AA000000AA00000EAD0E0082CE
      8200F7F8F700FEFEFE00000000000000000000000000CBCBAC00949454009494
      5400949454009494540094945400949454009494540094945400949454009494
      54009494540094945400CBCBAC0000000000000000000000000000000000FCFD
      FC0089D589000FAE0F0000AA000000AA000000AA000000AA00000EAD0E0082CE
      8200F7F8F700FEFEFE00000000000000000000000000BBC5A400A5B28600A5B2
      8600A5B28600A5B28600A5B28600A5B28600A5B28600A5B28600A5B28600A5B2
      8600A5B28600A5B28600BBC5A400000000000000000000000000000000000000
      0000FEFEFE00F0F8F000AAE0AA0082D2820082D18200A7DDA700ECF4EC00FDFD
      FD000000000000000000000000000000000000000000FDFDFC00CBCBAC00B0B0
      8000AEAE7E00AEAE7E00AEAE7E00AEAE7E00AEAE7E00AEAE7E00AEAE7E00AEAE
      7E00B0B08000CBCBAC00FDFDFC00000000000000000000000000000000000000
      0000FEFEFE00F0F8F000AAE0AA0082D2820082D18200A7DDA700ECF4EC00FDFD
      FD000000000000000000000000000000000000000000CCD3BA00BBC5A400BBC5
      A400BBC5A400BBC5A400BBC5A400BBC5A400BBC5A400BBC5A400BBC5A400BBC5
      A400BBC5A400BBC5A400CCD3BA00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000F81FFFFFF81FFFFFF0078001F0078001
      C0038001C0038001C0018FF1C0018FC987E187E18F918F81818081818F108601
      8000800182008201800081818200820181808181820082218180818182208621
      81808181820082018001800180018001C0038001C0038001E0038001E0038001
      F00F8001F00F8001FFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
end
