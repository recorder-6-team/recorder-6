object dlgConnStringEditor: TdlgConnStringEditor
  Left = 196
  Top = 114
  BorderStyle = bsDialog
  Caption = 'ADO Connection String Editor'
  ClientHeight = 177
  ClientWidth = 472
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 302
    Top = 147
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 390
    Top = 147
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object gbConnectionOptions: TGroupBox
    Tag = 2201
    Left = 5
    Top = 4
    Width = 461
    Height = 136
    Caption = ' Source of Connection '
    TabOrder = 2
    object rbUseDataLinkFile: TRadioButton
      Tag = 2203
      Left = 10
      Top = 21
      Width = 119
      Height = 20
      Caption = 'Use Data &Link File:'
      TabOrder = 0
      OnClick = SourceChange
    end
    object cmbDataLinkFile: TComboBox
      Tag = 2206
      Left = 28
      Top = 42
      Width = 332
      Height = 21
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
    end
    object btnBrowse: TButton
      Tag = 2209
      Left = 370
      Top = 39
      Width = 75
      Height = 25
      Caption = '&Browse...'
      TabOrder = 2
      OnClick = btnBrowseClick
    end
    object rbUseConnectionString: TRadioButton
      Tag = 2202
      Left = 10
      Top = 73
      Width = 131
      Height = 20
      Caption = 'Use &Connection String:'
      TabOrder = 3
      OnClick = SourceChange
    end
    object eConnectionString: TEdit
      Tag = 2205
      Left = 28
      Top = 94
      Width = 332
      Height = 21
      TabOrder = 4
    end
    object btnBuild: TButton
      Tag = 2208
      Left = 370
      Top = 92
      Width = 75
      Height = 25
      Caption = 'B&uild...'
      TabOrder = 5
      OnClick = btnBuildClick
    end
  end
end
