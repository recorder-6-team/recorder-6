object dlgFind: TdlgFind
  Left = 341
  Top = 243
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Find <STUFF>'
  ClientHeight = 340
  ClientWidth = 392
  Color = clBtnFace
  Constraints.MinHeight = 310
  Constraints.MinWidth = 200
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 392
    Height = 195
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      392
      195)
    object bvlFrame: TBevel
      Left = 4
      Top = 8
      Width = 384
      Height = 183
      Anchors = [akLeft, akTop, akRight, akBottom]
      Shape = bsFrame
    end
    object Label1: TLabel
      Left = 12
      Top = 12
      Width = 61
      Height = 13
      Caption = 'Search &Text:'
    end
    object Label2: TLabel
      Left = 12
      Top = 56
      Width = 44
      Height = 13
      Caption = '&Matches:'
      FocusControl = lbMatches
    end
    object lbMatches: TListBox
      Left = 11
      Top = 72
      Width = 370
      Height = 111
      Style = lbOwnerDrawFixed
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 14
      ParentShowHint = False
      ShowHint = True
      Sorted = True
      TabOrder = 2
      OnDblClick = lbMatchesDblClick
      OnDrawItem = lbMatchesDrawItem
    end
    object eSearchText: TFinder
      Left = 11
      Top = 28
      Width = 370
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      OutputList = lbMatches
      TabOrder = 0
      OnEnter = eSearchTextEnter
      OnPopulateList = eSearchTextPopulateList
      OnPopulateStop = eSearchTextPopulateStop
    end
    object btnGo: TButton
      Left = 360
      Top = 28
      Width = 21
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '&Go'
      TabOrder = 1
      Visible = False
      OnClick = btnGoClick
    end
  end
  object pnlSearchAbbr: TPanel
    Left = 0
    Top = 195
    Width = 392
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      392
      46)
    object rgSearchAbbr: TRadioGroup
      Left = 4
      Top = 1
      Width = 384
      Height = 41
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Search By:'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        '&Name'
        '&Abbreviation')
      TabOrder = 0
      OnClick = rgSearchAbbrClick
    end
  end
  object pnlRestrict: TPanel
    Left = 0
    Top = 241
    Width = 392
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      392
      41)
    object lblTaxonSearches: TLabel
      Left = 8
      Top = 0
      Width = 99
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Restrict Searches to:'
    end
    object cmbTaxonRestriction: TComboBox
      Left = 4
      Top = 16
      Width = 383
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbTaxonRestrictionChange
      Items.Strings = (
        '')
    end
  end
  object pnlReferenceKeywords: TPanel
    Left = 0
    Top = 282
    Width = 392
    Height = 21
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    Visible = False
    object chkSearchKeywords: TCheckBox
      Left = 8
      Top = 2
      Width = 201
      Height = 17
      Caption = 'Search for Reference Keywords'
      TabOrder = 0
      OnClick = chkSearchKeywordsClick
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 303
    Width = 392
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    DesignSize = (
      392
      37)
    object bbOK: TImageListButton
      Left = 226
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = bbOKClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 0
    end
    object bbCancel: TImageListButton
      Left = 309
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = bbCancelClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 1
    end
  end
end
