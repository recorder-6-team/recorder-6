inherited fraUpgrade: TfraUpgrade
  object lblWait: TLabel [0]
    Left = 24
    Top = 46
    Width = 210
    Height = 13
    Caption = 'Please wait, upgrading Recorder database...'
  end
  object lblZipFileMerge: TLabel [1]
    Left = 24
    Top = 101
    Width = 92
    Height = 13
    Caption = 'Merging Data File...'
    Visible = False
  end
  inherited pnlTitle: TPanel
    TabOrder = 2
    inherited lblTitle: TLabel
      Width = 93
      Caption = 'Upgrading...'
    end
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
