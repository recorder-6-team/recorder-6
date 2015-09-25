inherited fraCompletion: TfraCompletion
  object lblUpgrading: TLabel [0]
    Left = 12
    Top = 152
    Width = 297
    Height = 13
    Caption = 'IF YOU ARE UPGRADING FROM RECORDER 2002:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object lblUpgradingAdditional: TLabel [1]
    Left = 32
    Top = 176
    Width = 260
    Height = 52
    Caption = 
      'Once you are confident of your installation of Recorder 6, you s' +
      'hould transfer your data from Recorder 2002 and then uninstall i' +
      't. Options to do this will be available next time you insert thi' +
      's CD.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblUpgradingWarning: TLabel [2]
    Left = 32
    Top = 236
    Width = 260
    Height = 65
    Caption = 
      'WARNING Any records you enter into Recorder 6 will be overwritte' +
      'n by the process of transferring data from Recorder 2002. DO NOT' +
      ' enter new data into Recorder 6 until after you have successfull' +
      'y transferred your existing data from Recorder 2002.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblCompletionInfo: TLabel [3]
    Left = 12
    Top = 40
    Width = 309
    Height = 39
    Caption = 
      'Recorder 6 is now installed. You can now use Recorder 6 on this ' +
      'workstation. To add further workstations to the network run the ' +
      'WorkstationSetup.exe on each additional workstation.'
    WordWrap = True
  end
  inherited pnlTitle: TPanel
    inherited lblTitle: TLabel
      Visible = False
    end
  end
  inherited pnlContinue: TPanel
    inherited lblClickNext: TLabel
      Width = 149
      Caption = 'Click Main Menu to continue...'
    end
  end
  object chkLaunchGuide: TCheckBox
    Left = 20
    Top = 88
    Width = 209
    Height = 17
    Caption = 'Launch Getting Started Guide now'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object chkLaunchRecorder: TCheckBox
    Left = 20
    Top = 116
    Width = 209
    Height = 17
    Caption = 'Launch Recorder 6 now'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
