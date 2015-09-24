object formOptions: TformOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'NBN Export options'
  ClientHeight = 390
  ClientWidth = 208
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clNavy
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 192
    Height = 54
    Shape = bsFrame
  end
  object Bevel2: TBevel
    Left = 8
    Top = 72
    Width = 192
    Height = 126
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 8
    Top = 204
    Width = 147
    Height = 13
    Caption = 'Measurement types to export:'
  end
  object cbZeroAbundance: TCheckBox
    Left = 16
    Top = 16
    Width = 177
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Include zero abundance records?'
    TabOrder = 0
  end
  object cbConfidential: TCheckBox
    Left = 16
    Top = 40
    Width = 177
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Include confidential records?'
    TabOrder = 1
  end
  object cbRecorders: TCheckBox
    Left = 16
    Top = 79
    Width = 177
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Export Recorder'#39's names?'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object cbDeterminer: TCheckBox
    Left = 16
    Top = 102
    Width = 177
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Export Determiner'#39's name?'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object cbSampleType: TCheckBox
    Left = 16
    Top = 125
    Width = 177
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Export Sample type?'
    TabOrder = 4
  end
  object bOK: TButton
    Left = 8
    Top = 357
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 5
  end
  object bCancel: TButton
    Left = 125
    Top = 357
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object clbMeasurements: TCheckListBox
    Left = 8
    Top = 223
    Width = 192
    Height = 56
    ItemHeight = 13
    TabOrder = 7
  end
  object cbComment: TCheckBox
    Left = 16
    Top = 148
    Width = 177
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Export Comment?'
    TabOrder = 8
  end
  object cbSubstrate: TCheckBox
    Left = 16
    Top = 171
    Width = 177
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Export Substrate?'
    TabOrder = 9
  end
  object rgSiteName: TRadioGroup
    Left = 8
    Top = 285
    Width = 192
    Height = 60
    Hint = 
      'The "SiteName" field is obtained either from Sample_Location_Nam' +
      'e or from the preferred Location_Name. If  the first of these fi' +
      'elds has an entry it is used, otherwise the second field is used' +
      ' instead. '
    BiDiMode = bdLeftToRight
    Caption = 'SiteName options: '
    ItemIndex = 0
    Items.Strings = (
      'Sample Location Name first'
      'preferred Location Name first')
    ParentBiDiMode = False
    TabOrder = 10
  end
end
