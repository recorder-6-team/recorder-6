inherited fraMigrate: TfraMigrate
  inherited pnlTitle: TPanel
    TabOrder = 2
    inherited lblTitle: TLabel
      Width = 199
      Caption = 'Transfering Data and Files'
    end
  end
  inherited pnlContinue: TPanel
    Visible = False
  end
  object pnlControls: TPanel
    Left = 0
    Top = 41
    Width = 336
    Height = 325
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 0
    object lblTotalRecords: TLabel
      Left = 36
      Top = 174
      Width = 109
      Height = 13
      Caption = 'Records Transferred: 0'
      Color = clWhite
      ParentColor = False
      Transparent = False
      Visible = False
    end
    object lblOverall: TLabel
      Left = 36
      Top = 172
      Width = 84
      Height = 14
      Caption = 'Overall progress:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = False
      Visible = False
    end
    object lblInstallStep: TLabel
      Left = 28
      Top = 108
      Width = 110
      Height = 13
      Caption = 'Installing specific files...'
      Transparent = False
    end
    object lblFile: TLabel
      Left = 36
      Top = 132
      Width = 265
      Height = 14
      AutoSize = False
      Caption = 'Copying file: '
      Transparent = False
      Visible = False
    end
    object lblCurrentTable: TLabel
      Left = 36
      Top = 195
      Width = 127
      Height = 13
      Caption = 'Preparing to migrate data...'
      Color = clWhite
      ParentColor = False
      Transparent = False
      Visible = False
    end
    object lblRecordsOutOf: TLabel
      Left = 56
      Top = 212
      Width = 3
      Height = 13
      Color = clWhite
      ParentColor = False
      Transparent = False
    end
    object lblErrors: TLabel
      Left = 36
      Top = 230
      Width = 3
      Height = 13
      Color = clWhite
      ParentColor = False
      Transparent = False
    end
    object lblCancel: TLabel
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
      Transparent = False
      Visible = False
    end
    object lblSub: TLabel
      Left = 38
      Top = 212
      Width = 14
      Height = 17
      AutoSize = False
      Caption = #240
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Wingdings'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object pbOverall: TProgressBar
      Left = 36
      Top = 188
      Width = 261
      Height = 13
      TabOrder = 0
      Visible = False
    end
    object Animation: TAnimate
      Left = 28
      Top = 32
      Width = 272
      Height = 60
      Active = True
      CommonAVI = aviCopyFiles
      StopFrame = 1
      Visible = False
    end
    object pbProgress: TProgressBar
      Left = 36
      Top = 148
      Width = 261
      Height = 13
      TabOrder = 2
    end
  end
end
