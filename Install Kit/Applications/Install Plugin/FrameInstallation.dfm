inherited fraInstallation: TfraInstallation
  object lblFile: TLabel
    Left = 36
    Top = 144
    Width = 265
    Height = 14
    AutoSize = False
    Caption = 'Copying file: '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object lblInstallStep: TLabel
    Left = 28
    Top = 120
    Width = 138
    Height = 16
    Caption = 'Installing specific files...'
  end
  object lblOverall: TLabel
    Left = 36
    Top = 184
    Width = 84
    Height = 14
    Caption = 'Overall progress:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object lblCancel: TLabel
    Left = 52
    Top = 240
    Width = 201
    Height = 16
    Caption = 'Cancelling operation, please wait...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsItalic]
    ParentFont = False
    Visible = False
  end
  object Animation: TAnimate
    Left = 28
    Top = 36
    Width = 272
    Height = 60
    CommonAVI = aviCopyFiles
    StopFrame = 31
  end
  object pbFiles: TProgressBar
    Left = 36
    Top = 160
    Width = 261
    Height = 13
    TabOrder = 1
  end
  object pbOverall: TProgressBar
    Left = 36
    Top = 200
    Width = 261
    Height = 13
    TabOrder = 2
  end
  object tmrMSDE: TTimer
    Enabled = False
    OnTimer = tmrMSDETimer
    Left = 16
    Top = 240
  end
end
