inherited fraInstallation: TfraInstallation
  object lblFile: TLabel [0]
    Left = 36
    Top = 148
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
  object lblInstallStep: TLabel [1]
    Left = 28
    Top = 124
    Width = 110
    Height = 13
    Caption = 'Installing specific files...'
  end
  object lblOverall: TLabel [2]
    Left = 36
    Top = 188
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
  object lblCancel: TLabel [3]
    Left = 52
    Top = 244
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
  object lblInstallRecorder: TLabel [4]
    Left = 12
    Top = 12
    Width = 157
    Height = 19
    Caption = 'Installing Recorder 6'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblInstallDatabase: TLabel [5]
    Left = 12
    Top = 12
    Width = 185
    Height = 19
    Caption = 'Installing NBN Database'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  inherited pnlTitle: TPanel
    inherited lblTitle: TLabel
      Width = 69
      Caption = 'Installing'
    end
  end
  inherited pnlContinue: TPanel
    TabOrder = 3
    inherited lblClickNext: TLabel
      Visible = False
    end
  end
  object Animation: TAnimate
    Left = 28
    Top = 48
    Width = 272
    Height = 60
    CommonAVI = aviCopyFiles
    StopFrame = 31
  end
  object pbFiles: TProgressBar
    Left = 36
    Top = 164
    Width = 261
    Height = 13
    TabOrder = 1
  end
  object pbOverall: TProgressBar
    Left = 36
    Top = 204
    Width = 261
    Height = 13
    TabOrder = 2
  end
end
