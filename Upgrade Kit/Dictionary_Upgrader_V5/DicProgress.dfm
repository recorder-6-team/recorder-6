inherited fraProgress: TfraProgress
  Width = 320
  Height = 240
  object lblWait: TLabel [0]
    Left = 16
    Top = 54
    Width = 203
    Height = 13
    Caption = 'Please wait, Updating Recorder Dictionary '
  end
  object lblProgress: TLabel [1]
    Left = 16
    Top = 88
    Width = 185
    Height = 17
    AutoSize = False
  end
  inherited pnlTitle: TPanel
    Width = 320
    inherited lblTitle: TLabel
      Width = 82
      Caption = 'Updating...'
    end
  end
end
