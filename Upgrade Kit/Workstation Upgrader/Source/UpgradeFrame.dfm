inherited fraUpgrade: TfraUpgrade
  object lblWait: TLabel
    Left = 24
    Top = 46
    Width = 256
    Height = 16
    Caption = 'Please wait, upgrading Recorder database...'
  end
  object lblZipFileMerge: TLabel
    Left = 24
    Top = 101
    Width = 114
    Height = 16
    Caption = 'Merging Data File...'
    Visible = False
  end
  object pbProgress: TProgressBar
    Left = 23
    Top = 70
    Width = 266
    Height = 16
    TabOrder = 0
  end
  object pbZipFileProgress: TProgressBar
    Left = 24
    Top = 124
    Width = 265
    Height = 17
    TabOrder = 1
    Visible = False
  end
end
