object dlgExtractGridSquares: TdlgExtractGridSquares
  Left = 582
  Top = 190
  BorderStyle = bsDialog
  Caption = 'Extract Grid Squares'
  ClientHeight = 388
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    337
    388)
  PixelsPerInch = 96
  TextHeight = 13
  object lblInformation: TLabel
    Left = 8
    Top = 8
    Width = 318
    Height = 39
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'This dialog allows you to automatically scan a boundary linked t' +
      'o a location, or to scan all boundaries linked to locations in a' +
      ' layer, and extract the required grid square records into the lo' +
      'cation'#39's details.'
    WordWrap = True
  end
  object gbPolygonOrLayer: TGroupBox
    Left = 8
    Top = 64
    Width = 321
    Height = 113
    Caption = 'Select boundaries to scan'
    TabOrder = 0
    object lblCurrentLocation: TLabel
      Left = 36
      Top = 40
      Width = 166
      Height = 13
      Caption = '(Polygon not selected or not linked)'
      Enabled = False
    end
    object lblLayer: TLabel
      Left = 36
      Top = 84
      Width = 62
      Height = 13
      Caption = 'Select Layer:'
      Enabled = False
    end
    object rbCurrentPolygon: TRadioButton
      Left = 12
      Top = 20
      Width = 253
      Height = 17
      Caption = 'Current boundary only'
      Enabled = False
      TabOrder = 0
      OnClick = rbPolygonOrLayerClick
    end
    object rbLayer: TRadioButton
      Left = 12
      Top = 64
      Width = 257
      Height = 17
      Caption = 'All boundaries in layer'
      Enabled = False
      TabOrder = 1
      OnClick = rbPolygonOrLayerClick
    end
    object cmbLayer: TComboBox
      Left = 100
      Top = 81
      Width = 209
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 2
      OnChange = cmbLayerChange
    end
  end
  object gbSize: TGroupBox
    Left = 8
    Top = 184
    Width = 321
    Height = 81
    Caption = 'Select grid square size to generate'
    TabOrder = 1
    object chk10km: TCheckBox
      Left = 28
      Top = 24
      Width = 97
      Height = 17
      Caption = '10km'
      TabOrder = 0
      OnClick = chkSizeClick
    end
    object chk2km: TCheckBox
      Left = 28
      Top = 52
      Width = 97
      Height = 17
      Caption = '2km'
      TabOrder = 1
      OnClick = chkSizeClick
    end
    object chk1km: TCheckBox
      Left = 164
      Top = 24
      Width = 97
      Height = 17
      Caption = '1km'
      TabOrder = 2
      OnClick = chkSizeClick
    end
    object chk100m: TCheckBox
      Left = 164
      Top = 52
      Width = 97
      Height = 17
      Caption = '100m'
      TabOrder = 3
      OnClick = chkSizeClick
    end
  end
  object btnOk: TImageListButton
    Left = 168
    Top = 356
    Width = 75
    Height = 25
    Caption = 'Ok'
    Enabled = False
    TabOrder = 3
    OnClick = btnOkClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object btnCancel: TImageListButton
    Left = 256
    Top = 356
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    OnClick = btnCancelClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object gbGridSquares: TGroupBox
    Left = 8
    Top = 272
    Width = 321
    Height = 77
    Caption = 'Select grid squares selection'
    TabOrder = 2
    object rbIncludeSquares: TRadioButton
      Left = 12
      Top = 20
      Width = 249
      Height = 17
      Caption = 'Include boundary-overlapping grid squares'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbExcludeSquares: TRadioButton
      Left = 12
      Top = 48
      Width = 245
      Height = 17
      Caption = 'Exclude boundary-overlapping grid squares'
      TabOrder = 1
    end
  end
end
