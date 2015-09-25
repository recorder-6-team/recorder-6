inherited fraServerFiles: TfraServerFiles
  object Label6: TLabel
    Left = 20
    Top = 48
    Width = 294
    Height = 48
    Caption = 
      'Please select which of the following file type are to be install' +
      'ed on the Server, and therefore are shared by all users of this ' +
      'network:'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 20
    Top = 20
    Width = 133
    Height = 16
    Caption = 'File Sharing Settings'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object cbShareReports: TCheckBox
    Left = 32
    Top = 116
    Width = 129
    Height = 17
    Caption = 'Reports'
    TabOrder = 0
    OnClick = cbShareClick
  end
  object cbShareTemplates: TCheckBox
    Left = 32
    Top = 144
    Width = 129
    Height = 17
    Caption = 'Report Templates'
    TabOrder = 1
    OnClick = cbShareClick
  end
  object cbShareReportSnapshots: TCheckBox
    Left = 32
    Top = 172
    Width = 129
    Height = 17
    Caption = 'Report Snapshots'
    TabOrder = 2
    OnClick = cbShareClick
  end
  object cbShareRecordCards: TCheckBox
    Left = 180
    Top = 116
    Width = 129
    Height = 17
    Caption = 'Record Cards'
    TabOrder = 3
    OnClick = cbShareClick
  end
  object cbSharePolygonFilters: TCheckBox
    Left = 180
    Top = 144
    Width = 129
    Height = 17
    Caption = 'Polygon Filters'
    TabOrder = 4
    OnClick = cbShareClick
  end
  object cbShareRucksacks: TCheckBox
    Left = 180
    Top = 172
    Width = 129
    Height = 17
    Caption = 'Rucksacks'
    TabOrder = 5
    OnClick = cbShareClick
  end
end
