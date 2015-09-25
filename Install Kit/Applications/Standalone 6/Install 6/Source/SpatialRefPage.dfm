inherited fraSpatialRef: TfraSpatialRef
  object lblInformation: TLabel [0]
    Left = 12
    Top = 48
    Width = 296
    Height = 52
    Caption = 
      'Please select the Spatial Reference System you would like to use' +
      ' as your default display system for the application.  This can b' +
      'e changed at a later date via the Options dialog off the Tools m' +
      'enu.'
    WordWrap = True
  end
  object lblSpatialRef: TLabel [1]
    Left = 36
    Top = 124
    Width = 125
    Height = 13
    Caption = 'Spatial Reference Sytems:'
  end
  inherited pnlTitle: TPanel
    TabOrder = 2
    inherited lblTitle: TLabel
      Width = 195
      Caption = 'Spatial Reference System'
    end
  end
  object cblbSpatialRefs: TCheckListBox
    Left = 36
    Top = 140
    Width = 257
    Height = 160
    OnClickCheck = cblbSpatialRefsClickCheck
    IntegralHeight = True
    ItemHeight = 13
    TabOrder = 0
  end
end
