inherited fraKeyCheck: TfraKeyCheck
  Width = 326
  Height = 240
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Color = clBtnHighlight
  object Panel1: TPanel [0]
    Left = 8
    Top = 0
    Width = 320
    Height = 229
    Alignment = taLeftJustify
    Caption = '    '
    Color = clWhite
    TabOrder = 0
    object lblLiicenceKey: TLabel
      Left = 16
      Top = 88
      Width = 59
      Height = 13
      Caption = 'Licence Key'
    end
    object edLicenceKey: TEdit
      Left = 104
      Top = 88
      Width = 97
      Height = 21
      MaxLength = 8
      TabOrder = 0
    end
    object edVersion: TEdit
      Left = 16
      Top = 192
      Width = 289
      Height = 21
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 1
      Text = 'A licence key is required to install this upgrade'
    end
  end
  inherited pnlTitle: TPanel
    Color = clBtnHighlight
    ParentColor = False
    TabOrder = 1
    inherited lblTitle: TLabel
      Width = 94
      Caption = 'Licence Key'
    end
  end
end
