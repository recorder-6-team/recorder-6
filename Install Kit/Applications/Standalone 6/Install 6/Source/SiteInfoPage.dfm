inherited fraSiteInfo: TfraSiteInfo
  object pnlConfirmSiteID: TPanel [0]
    Left = 0
    Top = 41
    Width = 336
    Height = 325
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 3
    object lblInformation: TLabel
      Left = 12
      Top = 12
      Width = 304
      Height = 26
      Caption = 
        'This is the Site ID for your copy of Recorder 2002. If you wish ' +
        'to continue using it for Recorder 6, just click the Next button.'
      WordWrap = True
    end
    object lblChangeSiteID: TLabel
      Left = 12
      Top = 52
      Width = 311
      Height = 39
      Caption = 
        'You can choose a different Site ID by clicking the Change Button' +
        '. You will have to enter the correct verification key to validat' +
        'e the new Site ID.'
      WordWrap = True
    end
    object lblSiteIDExisting: TLabel
      Left = 97
      Top = 120
      Width = 35
      Height = 13
      Caption = 'Site ID:'
    end
    object lblConfirmSiteID: TLabel
      Left = 140
      Top = 120
      Width = 57
      Height = 13
      Caption = 'TESTDATA'
    end
    object lblNote: TLabel
      Left = 40
      Top = 164
      Width = 252
      Height = 26
      Caption = 
        'Note: If you change the Site ID, you will lose the custody of th' +
        'e records you entered in Recorder 2002.'
      WordWrap = True
    end
    object btnChange: TButton
      Left = 224
      Top = 116
      Width = 75
      Height = 21
      Caption = 'C&hange'
      TabOrder = 0
      OnClick = btnChangeClick
    end
  end
  object pnlEnterSiteID: TPanel [1]
    Left = 0
    Top = 41
    Width = 336
    Height = 325
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 0
    object lblInformationNewSite: TLabel
      Left = 12
      Top = 12
      Width = 293
      Height = 65
      Caption = 
        'Enter your Site ID and corresponding verification key for this i' +
        'nstance of Recorder. These details can be found on your licence ' +
        'agreement (the Site ID is 8 characters in length and only consis' +
        'ts of the letters A-Z and the numbers 0-9). The Site ID uniquely' +
        ' identifies all the recordings held in the database.'
      WordWrap = True
    end
    object lblSiteID: TLabel
      Left = 97
      Top = 120
      Width = 35
      Height = 13
      Caption = 'Site ID:'
    end
    object lblKey: TLabel
      Left = 56
      Top = 148
      Width = 76
      Height = 13
      Caption = 'Verification Key:'
    end
    object eSiteID: TEdit
      Left = 140
      Top = 116
      Width = 97
      Height = 21
      CharCase = ecUpperCase
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 8
      ParentFont = False
      TabOrder = 0
      OnChange = eSiteIDChange
      OnExit = InfoCheck
      OnKeyPress = eSiteIDKeyPress
    end
    object eVerificationKey: TEdit
      Left = 140
      Top = 144
      Width = 57
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 4
      ParentFont = False
      TabOrder = 1
      OnChange = eVerificationKeyChange
      OnExit = InfoCheck
    end
  end
  inherited pnlTitle: TPanel
    TabOrder = 2
    inherited lblTitle: TLabel
      Width = 96
      Caption = 'Site Settings'
    end
  end
end
