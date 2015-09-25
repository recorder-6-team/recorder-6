object dlgFilter: TdlgFilter
  Left = 332
  Top = 112
  BorderStyle = bsDialog
  Caption = 'Filter'
  ClientHeight = 256
  ClientWidth = 415
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 8
    Width = 405
    Height = 209
    Shape = bsFrame
  end
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
    Caption = 'C&riterion:'
    FocusControl = eCriteria1
  end
  object Label2: TLabel
    Left = 180
    Top = 192
    Width = 18
    Height = 13
    Caption = 'and'
  end
  object bbCancel: TImageListButton
    Left = 334
    Top = 224
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbAccept: TImageListButton
    Left = 250
    Top = 224
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = bbAcceptClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object lbAvailableFields: TListBox
    Left = 12
    Top = 32
    Width = 229
    Height = 149
    ItemHeight = 13
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
      'is &Less then'
      '&Between')
    TabOrder = 1
    OnClick = rgConditionsClick
  end
  object eCriteria2: TEdit
    Left = 208
    Top = 188
    Width = 121
    Height = 21
    TabOrder = 3
    OnExit = eCriteria2Exit
    OnKeyPress = CriteriaKeyPress
  end
  object eCriteria1: TEdit
    Left = 56
    Top = 188
    Width = 273
    Height = 21
    TabOrder = 2
    OnExit = eCriteria1Exit
    OnKeyPress = CriteriaKeyPress
  end
end
