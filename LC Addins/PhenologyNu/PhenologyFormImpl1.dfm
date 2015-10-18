object PhenologyFormX: TPhenologyFormX
  Left = 726
  Top = 246
  Width = 1142
  Height = 668
  Caption = 'PhenologyFormX'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Chart: TChart
    Left = 0
    Top = 0
    Width = 1126
    Height = 630
    AllowZoom = False
    BackWall.Brush.Color = clWhite
    BackWall.Color = clWhite
    MarginBottom = 2
    MarginLeft = 2
    MarginRight = 2
    MarginTop = 2
    Title.Font.Charset = DEFAULT_CHARSET
    Title.Font.Color = clBlue
    Title.Font.Height = -13
    Title.Font.Name = 'Arial'
    Title.Font.Style = [fsBold]
    Title.Text.Strings = (
      'Phenology')
    BackColor = clWhite
    BottomAxis.Axis.Width = 1
    BottomAxis.AxisValuesFormat = '#,##0'
    BottomAxis.Grid.Visible = False
    BottomAxis.Title.Caption = 'Week'
    BottomAxis.Title.Font.Charset = DEFAULT_CHARSET
    BottomAxis.Title.Font.Color = clBlack
    BottomAxis.Title.Font.Height = -12
    BottomAxis.Title.Font.Name = 'Arial'
    BottomAxis.Title.Font.Style = []
    LeftAxis.Axis.Width = 1
    LeftAxis.Title.Caption = 'Number or records per week'
    Legend.Visible = False
    View3D = False
    View3DWalls = False
    Align = alClient
    PopupMenu = PopupMenu
    TabOrder = 0
    AutoSize = True
    object Series1: TBarSeries
      Marks.ArrowLength = 20
      Marks.Visible = False
      SeriesColor = 8421440
      BarPen.Visible = False
      BarWidthPercent = 80
      MultiBar = mbNone
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Bar'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
  end
  object PopupMenu: TPopupMenu
    Left = 40
    Top = 584
    object mPeriod: TMenuItem
      Caption = 'Period'
      object mWeek: TMenuItem
        Caption = 'Week'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = mWeekClick
      end
      object mFortnight: TMenuItem
        Caption = 'Fortnight'
        GroupIndex = 1
        RadioItem = True
        OnClick = mFortnightClick
      end
      object mMonth: TMenuItem
        Caption = 'Month'
        GroupIndex = 1
        RadioItem = True
        OnClick = mMonthClick
      end
    end
    object mColour: TMenuItem
      Caption = 'Colour ...'
      OnClick = mColourClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mPrint: TMenuItem
      Caption = 'Print'
      OnClick = aPrintExecute
    end
    object mSave: TMenuItem
      Caption = 'Save'
      OnClick = aSaveExecute
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object SetOptiions: TMenuItem
      Caption = 'Options'
      object SetOptions: TMenuItem
        Caption = 'Set Options'
        OnClick = SetOptionsClick
      end
      object ApplyOptions: TMenuItem
        Action = aApply
        Caption = 'Apply Options'
        RadioItem = True
      end
    end
  end
  object ColorDialog: TColorDialog
    Left = 80
    Top = 584
  end
  object PrintDialog: TPrintDialog
    Left = 120
    Top = 584
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 
      'Windows bitmap (*.bmp)|*.bmp|Windows Metafile (*.wmf)|*.wmf|Wind' +
      'ows enhanced Metafile (*.emf)|*.emf|Excel 97 spreadsheet (*.xls)' +
      '|*.xls'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 168
    Top = 584
  end
  object ActionList: TActionList
    Left = 216
    Top = 584
    object aSave: TAction
      Caption = 'Save as ...'
    end
    object aPrint: TAction
      Caption = 'Print ...'
    end
    object aApply: TAction
      Caption = 'Apply constraints'
      OnExecute = mApplyClick
      OnUpdate = aApplyUpdate
    end
  end
end
