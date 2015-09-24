inherited fraCompletion: TfraCompletion
  object lblInformation: TLabel [0]
    Left = 12
    Top = 56
    Width = 301
    Height = 39
    Caption = 
      'The Database Setup needs to be run to complete the Recorder inst' +
      'allation.  Click Continue to proceed.'
    WordWrap = True
  end
  inherited pnlTitle: TPanel
    inherited lblTitle: TLabel
      Width = 282
      Caption = 'Recorder 6 Application Files Installed'
    end
  end
  inherited pnlContinue: TPanel
    Visible = False
  end
end
