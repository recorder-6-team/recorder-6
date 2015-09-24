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
    object lblAllRemoved: TLabel
      Left = 20
      Top = 64
      Width = 256
      Height = 39
      Caption = 
        'All installed Recorder files and settings have been removed. You' +
        'r system has been restored to its original state.'
      WordWrap = True
    end
    object lblFinishToExit: TLabel
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
      Width = 255
      Height = 26
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
      Width = 41
      Height = 13
      Caption = 'Addins...'
    end
    object lblShortcuts: TLabel
      Left = 96
      Top = 160
      Width = 54
      Height = 13
      Caption = 'Shortcuts...'
    end
    object lblFiles: TLabel
      Left = 96
      Top = 184
      Width = 85
      Height = 13
      Caption = 'Files and folders...'
    end
    object lblSQLInstance: TLabel
      Left = 96
      Top = 232
      Width = 113
      Height = 13
      Caption = 'SQL Express instance...'
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
    object lblStepSQLInstance: TLabel
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
    object lblRegistry: TLabel
      Left = 96
      Top = 208
      Width = 81
      Height = 13
      Caption = 'Registry entries...'
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
