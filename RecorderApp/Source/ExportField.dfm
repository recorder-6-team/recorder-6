object dlgExportField: TdlgExportField
  Left = 346
  Top = 123
  BorderStyle = bsDialog
  Caption = 'Select Data Column'
  ClientHeight = 392
  ClientWidth = 269
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 8
    Width = 261
    Height = 261
    Shape = bsFrame
  end
  object lblTitle: TLabel
    Left = 16
    Top = 16
    Width = 109
    Height = 13
    Caption = 'Select data column for:'
  end
  object Bevel2: TBevel
    Left = 4
    Top = 280
    Width = 261
    Height = 73
    Shape = bsFrame
  end
  object Label2: TLabel
    Left = 16
    Top = 296
    Width = 116
    Height = 13
    Caption = 'Number of Ranges (2-9):'
  end
  object lblMax: TLabel
    Left = 144
    Top = 328
    Width = 65
    Height = 13
    AutoSize = False
    Caption = 'Max. Value:'
  end
  object lblMin: TLabel
    Left = 16
    Top = 328
    Width = 73
    Height = 13
    AutoSize = False
    Caption = 'Min. Value:'
  end
  object bbCancel: TImageListButton
    Left = 142
    Top = 360
    Width = 123
    Height = 25
    Cancel = True
    Caption = 'Exclude Quantities'
    ModalResult = 2
    TabOrder = 5
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbOK: TImageListButton
    Left = 4
    Top = 360
    Width = 125
    Height = 25
    Caption = 'Include Quantities'
    Default = True
    TabOrder = 4
    OnClick = bbOkClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object cblbColumns: TCheckListBox
    Left = 16
    Top = 32
    Width = 241
    Height = 229
    OnClickCheck = cblbColumnsClickCheck
    ItemHeight = 13
    Items.Strings = (
      'Abundance'
      'Feeding Habits'
      'Location'
      'Recorder'
      'Sex'
      'Weight'
      'Wing Length')
    Sorted = True
    TabOrder = 0
    OnClick = cblbColumnsClick
    OnDblClick = cblbColumnsDblClick
    OnMouseDown = cblbColumnsMouseDown
  end
  object eRanges: TEdit
    Left = 216
    Top = 292
    Width = 41
    Height = 21
    MaxLength = 1
    TabOrder = 1
    OnExit = eRangesExit
  end
  object eMax: TEdit
    Left = 216
    Top = 324
    Width = 41
    Height = 21
    MaxLength = 5
    TabOrder = 3
    OnExit = eMaxExit
  end
  object eMin: TEdit
    Left = 88
    Top = 324
    Width = 41
    Height = 21
    MaxLength = 5
    TabOrder = 2
    OnExit = eMinExit
  end
end
