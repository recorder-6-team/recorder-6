inherited fraCompletion: TfraCompletion
  object pnlRecorder: TPanel [0]
    Left = 12
    Top = 48
    Width = 313
    Height = 293
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 3
    object lblRecorderInfo: TLabel
      Left = 0
      Top = 0
      Width = 308
      Height = 52
      Caption = 
        'The Recorder 6 application and other files are now installed on ' +
        'your chosen server. Please note that you cannot use Recorder 6 a' +
        's it stands; you must run the Workstation setup first on each of' +
        ' the workstation computers.'
      WordWrap = True
    end
    object Label2: TLabel
      Left = 0
      Top = 60
      Width = 298
      Height = 52
      Caption = 
        'On the workstation navigate to the WorkstationSetup.exe file loc' +
        'ated on the Server (you may need to speak to your network admini' +
        'strator to set up network drive mapping, see the network install' +
        'ation guide for further information).'
      WordWrap = True
    end
  end
  object pnlDatabase: TPanel [1]
    Left = 12
    Top = 48
    Width = 313
    Height = 293
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 2
    object lblDatabaseInfo: TLabel
      Left = 0
      Top = 0
      Width = 313
      Height = 39
      AutoSize = False
      Caption = 
        'Your database is now set up and you can install the Recorder 6 a' +
        'pplication and other files on your server (step 2 from the Insta' +
        'll Recorder 6 menu). '
      WordWrap = True
    end
    object Label1: TLabel
      Left = 0
      Top = 47
      Width = 310
      Height = 39
      Caption = 
        'Step 2 can be run on either the server or from a  workstation wi' +
        'th rights to create the required files in the Recorder installat' +
        'ion folder on the server.'
      WordWrap = True
    end
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
end
