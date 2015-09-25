object formBulkLoadX: TformBulkLoadX
  Left = 255
  Top = 107
  Width = 231
  Height = 355
  AxBorderStyle = afbNone
  Caption = 'formBulkLoadX'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TBevel
    Left = 8
    Top = 272
    Width = 209
    Height = 49
    Shape = bsFrame
  end
  object bBrowse: TSpeedButton
    Left = 192
    Top = 67
    Width = 23
    Height = 22
    Hint = 'Browse'
    Caption = '...'
    OnClick = bBrowseClick
  end
  object lbMap: TLabel
    Left = 8
    Top = 6
    Width = 50
    Height = 13
    Caption = 'Base map:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lbDirectory: TLabel
    Left = 8
    Top = 52
    Width = 153
    Height = 13
    Caption = 'Directory containing files to load:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object labFiles: TLabel
    Left = 8
    Top = 98
    Width = 89
    Height = 13
    Caption = 'Select files to load:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lbCutIn: TLabel
    Left = 16
    Top = 276
    Width = 58
    Height = 13
    Caption = 'Cut-in scale:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lbCutOut: TLabel
    Left = 112
    Top = 278
    Width = 65
    Height = 13
    Caption = 'Cut-out scale:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object cbMap: TComboBox
    Left = 8
    Top = 22
    Width = 209
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Text = 'cbMap'
  end
  object edDirectory: TEdit
    Left = 8
    Top = 68
    Width = 184
    Height = 21
    Color = clInactiveCaptionText
    ReadOnly = True
    TabOrder = 1
    Text = 'edDirectory'
  end
  object listFiles: TListBox
    Left = 8
    Top = 114
    Width = 209
    Height = 153
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 2
  end
  object cbCutIn: TComboBox
    Left = 16
    Top = 292
    Width = 81
    Height = 21
    ItemHeight = 13
    ItemIndex = 5
    TabOrder = 3
    Text = '1:250,000'
    Items.Strings = (
      '1:10,000,000'
      '1:5,000,000'
      '1:2,000,000'
      '1:1,000,000'
      '1:500,000'
      '1:250,000'
      '1:100,000'
      '1:50,000'
      '1:25,000'
      '1:10,000'
      '1:5,000'
      '1:2,500'
      '1:1,000'
      '1:500'
      '1:250'
      '1:100'
      '1:10'
      '1:1')
  end
  object cbCutOut: TComboBox
    Left = 112
    Top = 292
    Width = 81
    Height = 21
    ItemHeight = 13
    ItemIndex = 12
    TabOrder = 4
    Text = '1:100'
    Items.Strings = (
      '1:1,000,000'
      '1:500,000'
      '1:250,000'
      '1:100,000'
      '1:50,000'
      '1:25,000'
      '1:10,000'
      '1:5,000'
      '1:2,500'
      '1:1,000'
      '1:500'
      '1:250'
      '1:100'
      '1:10'
      '1:1')
  end
end
