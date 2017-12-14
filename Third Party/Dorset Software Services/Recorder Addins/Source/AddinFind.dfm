object dlgAddinFind: TdlgAddinFind
  Left = 459
  Top = 285
  Width = 392
  Height = 347
  BorderIcons = [biSystemMenu]
  Caption = 'Find'
  Color = clBtnFace
  Constraints.MinHeight = 235
  Constraints.MinWidth = 247
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    384
    320)
  PixelsPerInch = 96
  TextHeight = 14
  object bvlFrame: TBevel
    Left = 4
    Top = 8
    Width = 374
    Height = 270
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 12
    Top = 16
    Width = 62
    Height = 14
    Caption = 'Search &Text:'
  end
  object Label2: TLabel
    Left = 12
    Top = 68
    Width = 44
    Height = 14
    Caption = '&Matches:'
  end
  object btnOk: TImageListButton
    Left = 218
    Top = 286
    Width = 74
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Enabled = False
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOkClick
    ImageList = dmAddinInterface.ilButtons
    ImageIndex = 3
  end
  object btnCancel: TImageListButton
    Left = 304
    Top = 286
    Width = 74
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ImageList = dmAddinInterface.ilButtons
    ImageIndex = 4
  end
  object eSearchText: TEdit
    Left = 12
    Top = 32
    Width = 358
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = eSearchTextChange
    OnKeyDown = eSearchTextKeyDown
  end
  object lbMatches: TIDListBox
    Left = 12
    Top = 84
    Width = 358
    Height = 185
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 14
    Sorted = True
    Style = lbOwnerDrawFixed
    TabOrder = 1
    OnClick = lbMatchesClick
    OnDblClick = lbMatchesDblClick
    OnDrawItem = lbMatchesDrawItem
    OnKeyDown = lbMatchesKeyDown
  end
  object Animation: TAnimate
    Left = 8
    Top = 290
    Width = 16
    Height = 16
    Anchors = [akLeft, akBottom]
    CommonAVI = aviFindFile
    StopFrame = 8
    Visible = False
  end
end
