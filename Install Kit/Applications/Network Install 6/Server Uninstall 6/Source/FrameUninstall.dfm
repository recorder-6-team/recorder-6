inherited fraUninstall: TfraUninstall
  object pnlFinish: TPanel
    Left = 4
    Top = 4
    Width = 321
    Height = 285
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    Visible = False
    object Label2: TLabel
      Left = 20
      Top = 16
      Width = 257
      Height = 16
      Caption = 'Uninstall of Recorder 6 Server complete.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 20
      Top = 64
      Width = 274
      Height = 48
      Caption = 
        'All installed Recorder files and settings have been removed. You' +
        'r system has been restored to its original state.'
      WordWrap = True
    end
    object Label4: TLabel
      Left = 20
      Top = 188
      Width = 116
      Height = 16
      Caption = 'Press Finish to exit.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsItalic]
      ParentFont = False
    end
  end
  object pnlExecute: TPanel
    Left = 4
    Top = 4
    Width = 321
    Height = 285
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object Label1: TLabel
      Left = 20
      Top = 92
      Width = 262
      Height = 32
      Caption = 
        'Removing installed files and settings. Please wait, this can tak' +
        'e several minutes...'
      WordWrap = True
    end
    object lblFiles: TLabel
      Left = 96
      Top = 148
      Width = 107
      Height = 16
      Caption = 'Files and folders...'
    end
    object lblStepFiles: TLabel
      Left = 76
      Top = 148
      Width = 14
      Height = 17
      AutoSize = False
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Wingdings'
      Font.Style = []
      ParentFont = False
    end
    object lblRegistry: TLabel
      Left = 96
      Top = 172
      Width = 103
      Height = 16
      Caption = 'Registry entries...'
    end
    object lblStepRegistry: TLabel
      Left = 76
      Top = 172
      Width = 14
      Height = 17
      AutoSize = False
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Wingdings'
      Font.Style = []
      ParentFont = False
    end
    object Animation: TAnimate
      Left = 20
      Top = 16
      Width = 272
      Height = 60
      CommonAVI = aviRecycleFile
      StopFrame = 23
      Timers = True
    end
  end
  object tmrAnim: TTimer
    Interval = 100
    OnTimer = tmrAnimTimer
    Left = 268
    Top = 20
  end
end
