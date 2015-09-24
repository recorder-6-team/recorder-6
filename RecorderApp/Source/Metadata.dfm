object dlgMetadata: TdlgMetadata
  Left = 387
  Top = 139
  BorderStyle = bsDialog
  Caption = 'Specify Metadata'
  ClientHeight = 383
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 208
    Width = 325
    Height = 137
    Shape = bsFrame
  end
  object lblDataItem: TLabel
    Left = 12
    Top = 218
    Width = 49
    Height = 13
    Caption = 'Data Item:'
  end
  object lblData: TLabel
    Left = 12
    Top = 238
    Width = 100
    Height = 13
    Caption = 'Exported Information:'
  end
  object lblInstruct: TLabel
    Left = 4
    Top = 4
    Width = 317
    Height = 41
    AutoSize = False
    Caption = 
      'Before you can export data, please enter the following informati' +
      'on which describes your dataset.  This information is provided w' +
      'ithin the export file to other users.'
    WordWrap = True
  end
  object lblSelect: TLabel
    Left = 4
    Top = 48
    Width = 102
    Height = 13
    Caption = 'Select the item to edit'
  end
  object lblMetaItem: TLabel
    Left = 72
    Top = 218
    Width = 3
    Height = 13
  end
  object bbDiscard: TImageListButton
    Left = 299
    Top = 315
    Width = 24
    Height = 23
    Hint = 'Discard changes'
    Enabled = False
    TabOrder = 4
    OnClick = bbDiscardClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 7
  end
  object bbAccept: TImageListButton
    Left = 275
    Top = 315
    Width = 24
    Height = 23
    Hint = 'Accept changes'
    Enabled = False
    TabOrder = 3
    OnClick = bbAcceptClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 6
  end
  object bbMetaDataEdit: TImageListButton
    Left = 304
    Top = 176
    Width = 24
    Height = 23
    Hint = 'Edit selected item'
    TabOrder = 1
    OnClick = bbMetaDataEditClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 3
  end
  object bbCancel: TImageListButton
    Left = 254
    Top = 350
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Abort'
    ModalResult = 2
    TabOrder = 6
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbContinue: TImageListButton
    Left = 170
    Top = 350
    Width = 75
    Height = 25
    Caption = '&Continue'
    Default = True
    ModalResult = 1
    TabOrder = 5
    OnClick = bbContinueClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object dbgMetadataItems: TDBRTFGrid
    Left = 4
    Top = 64
    Width = 299
    Height = 134
    DataSource = dsMetadata
    Options = [dgTitles, dgIndicator, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnDrawColumnCell = dbgMetadataItemsDrawColumnCell
    OnDblClick = bbMetaDataEditClick
  end
  object mmInfo: TMemo
    Left = 12
    Top = 252
    Width = 309
    Height = 57
    TabOrder = 2
  end
  object dsMetadata: TDataSource
    AutoEdit = False
    DataSet = dmGeneralData.qryMetadata
    OnDataChange = dsMetadataDataChange
    Left = 248
    Top = 72
  end
end
