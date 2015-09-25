inherited fraFixedWidths: TfraFixedWidths
  Width = 338
  Height = 267
  object pbBreaks: TPaintBox
    Left = 17
    Top = 97
    Width = 290
    Height = 131
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
    OnClick = pbBreaksClick
    OnMouseDown = pbBreaksMouseDown
    OnMouseMove = pbBreaksMouseMove
    OnMouseUp = pbBreaksMouseUp
    OnPaint = pbBreaksPaint
  end
  object lblArrowLines: TLabel
    Left = 16
    Top = 16
    Width = 177
    Height = 13
    Caption = 'Lines with arrows are the field breaks.'
  end
  object lblAddingBreak: TLabel
    Left = 16
    Top = 32
    Width = 210
    Height = 13
    Caption = 'To add a break, click at the desired position.'
  end
  object lblRemovingBreak: TLabel
    Left = 16
    Top = 48
    Width = 208
    Height = 13
    Caption = 'To remove a break, double-click on the line.'
  end
  object lblMovingBreak: TLabel
    Left = 16
    Top = 64
    Width = 228
    Height = 13
    Caption = 'To move an existing break line, click and drag it.'
  end
  object sbBreakVert: TScrollBar
    Left = 308
    Top = 117
    Width = 14
    Height = 111
    Anchors = [akTop, akRight, akBottom]
    Kind = sbVertical
    PageSize = 0
    TabOrder = 0
    TabStop = False
    OnScroll = sbBreakVertScroll
  end
  object sbBreakHorz: TScrollBar
    Left = 17
    Top = 229
    Width = 289
    Height = 14
    Anchors = [akLeft, akRight, akBottom]
    PageSize = 0
    TabOrder = 1
    TabStop = False
    OnScroll = sbBreakHorzScroll
  end
end
