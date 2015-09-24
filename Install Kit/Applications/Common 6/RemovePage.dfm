inherited fraRemove: TfraRemove
  object lblArchiving: TLabel [0]
    Left = 112
    Top = 164
    Width = 174
    Height = 13
    Caption = 'Archiving Recorder 2002 database...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblInformation: TLabel [1]
    Left = 16
    Top = 128
    Width = 286
    Height = 26
    Caption = 
      'Proceeding with the removal of Recorder 2002. Please wait, this ' +
      'can take several minutes...'
    WordWrap = True
  end
  object lblStepArchive: TLabel [2]
    Left = 92
    Top = 164
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
  inherited pnlTitle: TPanel
    inherited lblTitle: TLabel
      Width = 193
      Caption = 'Removing Recorder 2002'
    end
  end
  inherited pnlContinue: TPanel
    TabOrder = 3
    Visible = False
  end
  object Animation: TAnimate
    Left = 32
    Top = 48
    Width = 272
    Height = 60
    CommonAVI = aviCopyFile
    StopFrame = 20
  end
  object pbZip: TProgressBar
    Left = 112
    Top = 180
    Width = 209
    Height = 13
    TabOrder = 2
  end
  object pnlFiles: TPanel
    Left = 84
    Top = 196
    Width = 249
    Height = 85
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object lblStepShortcuts: TLabel
      Left = 8
      Top = 8
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
    object lblShortcuts: TLabel
      Left = 28
      Top = 8
      Width = 54
      Height = 13
      Caption = 'Shortcuts...'
    end
    object lblFiles: TLabel
      Left = 28
      Top = 32
      Width = 85
      Height = 13
      Caption = 'Files and folders...'
    end
    object lblRegistry: TLabel
      Left = 28
      Top = 56
      Width = 81
      Height = 13
      Caption = 'Registry entries...'
    end
    object lblStepFiles: TLabel
      Left = 8
      Top = 32
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
      Left = 8
      Top = 56
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
  end
end
