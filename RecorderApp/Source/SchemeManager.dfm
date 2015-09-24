object dlgSchemeManager: TdlgSchemeManager
  Left = 296
  Top = 182
  HelpContext = 152
  BorderStyle = bsDialog
  Caption = 'Recording Scheme Manager'
  ClientHeight = 238
  ClientWidth = 686
  Color = clBtnFace
  Constraints.MinHeight = 148
  Constraints.MinWidth = 381
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    686
    238)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSchemes: TLabel
    Left = 8
    Top = 8
    Width = 145
    Height = 13
    Caption = 'Recording Schemes Available:'
  end
  object btnOk: TImageListButton
    Left = 520
    Top = 208
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = btnOkClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object sgSchemes: TControlStringGrid
    Left = 8
    Top = 24
    Width = 672
    Height = 179
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultColWidth = 32
    DefaultRowHeight = 18
    FixedCols = 0
    RowCount = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goTabs]
    ParentFont = False
    ParentShowHint = False
    PopupMenu = pmSchemeGrid
    ShowHint = True
    TabOrder = 0
  end
  object btnCancel: TImageListButton
    Left = 604
    Top = 208
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object btnOpen: TImageListButton
    Left = 8
    Top = 208
    Width = 75
    Height = 25
    Caption = '&Open'
    TabOrder = 1
    OnClick = btnOpenClick
    ImageList = dmFormActions.ilMenuOn
    ImageIndex = 59
  end
  object btnSave: TImageListButton
    Left = 92
    Top = 208
    Width = 75
    Height = 25
    Caption = '&Save'
    TabOrder = 2
    OnClick = btnSaveClick
    ImageList = dmFormActions.ilMenuOn
    ImageIndex = 0
  end
  object pmSchemeGrid: TPopupMenu
    Left = 344
    Top = 76
    object mnuNewRecordingScheme: TMenuItem
      Caption = '&New Recording Scheme'
      ShortCut = 45
    end
    object mnuDelete: TMenuItem
      Caption = '&Delete'
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.sch'
    Filter = 'Scheme Files|*.sch|All Files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 588
    Top = 36
  end
  object SaveDialog: TSaveDialog
    Filter = 'Scheme Files|*.sch|All Files|*.*'
    Left = 524
    Top = 36
  end
end
