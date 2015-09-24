object dlgExternalSourceDetails: TdlgExternalSourceDetails
  Left = 443
  Top = 324
  BorderStyle = bsDialog
  Caption = 'External Source Details'
  ClientHeight = 135
  ClientWidth = 738
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    738
    135)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 7
    Top = 8
    Width = 725
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 123
    Height = 13
    Caption = 'Website URL or File Path:'
  end
  object Label2: TLabel
    Left = 24
    Top = 56
    Width = 97
    Height = 13
    Caption = 'Display as (optional):'
  end
  object bbCancel: TImageListButton
    Left = 656
    Top = 97
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbOK: TImageListButton
    Left = 571
    Top = 97
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object ePath: TEdit
    Left = 160
    Top = 20
    Width = 537
    Height = 21
    TabOrder = 0
  end
  object eTitle: TEdit
    Left = 160
    Top = 52
    Width = 561
    Height = 21
    MaxLength = 100
    TabOrder = 2
  end
  object btnOpen: TButton
    Left = 696
    Top = 18
    Width = 25
    Height = 23
    Hint = 'Click to find a file to use as an external reference.'
    Caption = '...'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = btnOpenClick
  end
end
