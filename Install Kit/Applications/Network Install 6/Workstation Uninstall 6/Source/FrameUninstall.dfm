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
    object lblUninstallComplete: TLabel
      Left = 20
      Top = 16
      Width = 122
      Height = 16
      Caption = 'Uninstall complete.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblUninstallInfo: TLabel
      Left = 20
      Top = 64
      Width = 273
      Height = 48
      Caption = 
        'All installed Recorder files and settings have been removed. You' +
        'r system has been restored to its original state.'
      WordWrap = True
    end
    object lblContinue: TLabel
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
    object lblInformation: TLabel
      Left = 20
      Top = 92
      Width = 262
      Height = 32
      Caption = 
        'Removing installed files and settings. Please wait, this can tak' +
        'e several minutes...'
      WordWrap = True
    end
    object lblStepShortcuts: TLabel
      Left = 76
      Top = 160
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
    object lblStepAddins: TLabel
      Left = 76
      Top = 136
      Width = 13
      Height = 17
      AutoSize = False
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Wingdings'
      Font.Style = []
      ParentFont = False
    end
    object lblAddins: TLabel
      Left = 96
      Top = 136
      Width = 52
      Height = 16
      Caption = 'Addins...'
    end
    object lblShortcuts: TLabel
      Left = 96
      Top = 160
      Width = 68
      Height = 16
      Caption = 'Shortcuts...'
    end
    object lblFiles: TLabel
      Left = 96
      Top = 184
      Width = 107
      Height = 16
      Caption = 'Files and folders...'
    end
    object lblRegistry: TLabel
      Left = 96
      Top = 208
      Width = 103
      Height = 16
      Caption = 'Registry entries...'
    end
    object lblStepFiles: TLabel
      Left = 76
      Top = 184
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
    object lblStepRegistry: TLabel
      Left = 76
      Top = 208
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
    object lblSysComp: TLabel
      Left = 96
      Top = 232
      Width = 134
      Height = 16
      Caption = 'System Components...'
    end
    object lblStepSysComp: TLabel
      Left = 76
      Top = 232
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
