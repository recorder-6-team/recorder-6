inherited fraServerSelect: TfraServerSelect
  object lblInfo: TLabel [0]
    Left = 12
    Top = 43
    Width = 302
    Height = 26
    Caption = 
      'Please select the SQL Server or SQL Express instance on which ' +
      'to install the NBN Database:'
    WordWrap = True
  end
  object lblInstanceName: TLabel [1]
    Left = 24
    Top = 78
    Width = 73
    Height = 13
    Caption = 'Instance name:'
  end
  object lblWarning: TLabel [2]
    Left = 38
    Top = 102
    Width = 265
    Height = 13
    Caption = 'Warning: This instance uses Trusted Connection ONLY.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object lblRequiredSpace: TLabel [3]
    Left = 24
    Top = 272
    Width = 100
    Height = 13
    Caption = 'Required disk space:'
  end
  object lblRequired: TLabel [4]
    Left = 156
    Top = 272
    Width = 37
    Height = 13
    Caption = '600 MB'
  end
  object lblAvailable: TLabel [5]
    Left = 156
    Top = 292
    Width = 39
    Height = 13
    Caption = '20.1 GB'
  end
  object lblDisk: TLabel [6]
    Left = 24
    Top = 292
    Width = 118
    Height = 13
    Caption = 'Available disk space on :'
  end
  object lblNotLocal: TLabel [7]
    Left = 36
    Top = 102
    Width = 270
    Height = 26
    Caption = 
      'The selected instance is not a local instance and cannot be used' +
      ' to install Recorder.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblDropDownInfo: TLabel [8]
    Left = 24
    Top = 130
    Width = 297
    Height = 55
    AutoSize = False
    Caption = 
      'The drop-down box above may not be an exhaustive list of the SQL' +
      ' Server instances available. If the instance you wish to use is ' +
      'not displayed, please click in the instance name box and type in' +
      ' the full name of the instance you wish to use.'
    WordWrap = True
  end
  inherited pnlTitle: TPanel
    TabOrder = 2
    inherited lblTitle: TLabel
      Width = 162
      Caption = 'SQL Server Selection'
    end
  end
  inherited pnlContinue: TPanel
    TabOrder = 3
  end
  object cmbInstances: TComboBox
    Left = 104
    Top = 74
    Width = 205
    Height = 21
    BevelKind = bkFlat
    CharCase = ecUpperCase
    Ctl3D = True
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 0
    OnChange = CheckChange
  end
  inline fraDBFileLocationSelect: TfraDBFileLocationSelect
    Left = 0
    Top = 190
    Width = 318
    Height = 83
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Color = clWhite
    ParentColor = False
    TabOrder = 1
    inherited lblNotLocal: TLabel
      Left = 28
    end
    inherited lblNoSpace: TLabel
      Left = 28
      Width = 296
      Height = 30
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Not enough disk space for the database files on this drive.'
    end
    inherited rbDefault: TRadioButton
      Left = 12
    end
    inherited rbPickLocation: TRadioButton
      Left = 12
    end
    inherited eFilePath: TEdit
      Left = 28
      Width = 273
    end
    inherited btnFolderBrowse: TButton
      Left = 293
    end
  end
end
