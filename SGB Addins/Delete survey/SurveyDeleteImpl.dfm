object SurveyDeleteX: TSurveyDeleteX
  Left = 0
  Top = 0
  Width = 276
  Height = 246
  Caption = 'SurveyDeleteX'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 257
    Height = 225
    Shape = bsFrame
  end
  object labelSurvey: TLabel
    Left = 16
    Top = 14
    Width = 80
    Height = 17
    Caption = 'Select survey:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -12
    Font.Name = 'Comic Sans MS'
    Font.Style = []
    ParentFont = False
  end
  object cbSurveys: TComboBox
    Left = 16
    Top = 32
    Width = 241
    Height = 21
    Ctl3D = False
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 0
    Text = 'cbSurveys'
    OnChange = cbSurveysChange
  end
  object memStatistics: TMemo
    Left = 16
    Top = 60
    Width = 241
    Height = 141
    Color = 15987675
    Ctl3D = False
    Lines.Strings = (
      'memStatistics')
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 1
  end
  object cbSurveyRow: TCheckBox
    Left = 16
    Top = 208
    Width = 161
    Height = 17
    Caption = 'Delete the Survey entry'
    Checked = True
    Ctl3D = False
    ParentCtl3D = False
    State = cbChecked
    TabOrder = 2
  end
end
