object dlgImportComplete: TdlgImportComplete
  Left = 399
  Top = 306
  BorderStyle = bsDialog
  Caption = 'Import Complete'
  ClientHeight = 291
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object bevel: TBevel
    Left = 8
    Top = 8
    Width = 385
    Height = 241
    Shape = bsFrame
  end
  object lblImported: TLabel
    Left = 16
    Top = 20
    Width = 370
    Height = 20
    AutoSize = False
    Caption = '%d items were successfully imported'
  end
  object lblRejected: TLabel
    Left = 16
    Top = 68
    Width = 370
    Height = 20
    AutoSize = False
    Caption = '%d items were rejected during import'
  end
  object lblFailedValidation: TLabel
    Left = 16
    Top = 44
    Width = 370
    Height = 20
    AutoSize = False
    Caption = '%d items failed validation'
  end
  object gbRejects: TGroupBox
    Left = 32
    Top = 92
    Width = 346
    Height = 81
    Caption = 'Rejection Details'
    TabOrder = 0
    object lblRejectFile: TLabel
      Left = 8
      Top = 20
      Width = 329
      Height = 26
      AutoSize = False
      Caption = 'Details of the rejected records are in %s.'
      WordWrap = True
    end
    object chkViewRejectDetails: TCheckBox
      Left = 8
      Top = 56
      Width = 265
      Height = 17
      Caption = 'Check here to view the details of rejected records'
      TabOrder = 0
    end
  end
  object chkReviewImports: TCheckBox
    Left = 16
    Top = 188
    Width = 250
    Height = 17
    Caption = 'Review the imported records'
    TabOrder = 1
  end
  object chkSaveFilter: TCheckBox
    Left = 16
    Top = 212
    Width = 362
    Height = 25
    Caption = 
      'Save the list of imported records as a filter file so that you c' +
      'an review them later'
    Checked = True
    State = cbChecked
    TabOrder = 2
    WordWrap = True
  end
  object btnOk: TImageListButton
    Left = 318
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 3
    OnClick = btnOkClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object dlgSaveFilter: TSaveDialog
    DefaultExt = '.ref'
    Filter = 'Recorder External Filter (*.ref)|*.ref'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 328
    Top = 12
  end
end
