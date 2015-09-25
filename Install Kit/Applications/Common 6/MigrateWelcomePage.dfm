inherited fraMigrateWelcome: TfraMigrateWelcome
  object lblDiskSpaceInformation: TLabel [0]
    Left = 12
    Top = 156
    Width = 251
    Height = 26
    Caption = 
      'The transfer requires about 900 MB in your database directory fo' +
      'r every 500,000 species occurrences.'
    WordWrap = True
  end
  object lblWarning: TLabel [1]
    Left = 12
    Top = 188
    Width = 53
    Height = 13
    Caption = 'WARNING'
  end
  object lblSiteIDInformation: TLabel [2]
    Left = 12
    Top = 204
    Width = 316
    Height = 91
    Caption = 
      'Since your new copy of Recorder 6 and the old copy of Recorder 2' +
      '002 have the same SiteID, any records you enter into Recorder 6 ' +
      'will have the same codes as any you enter into Recorder 2002. Th' +
      'is means that the records will overwrite each other and your dat' +
      'a will be lost  DO NOT enter new data into Recorder 6 until afte' +
      'r you have removed Recorder 2002 from this machine. DO NOT allow' +
      ' your copy of Recorder 2002 to be used by somebody else.'
    WordWrap = True
  end
  object lblInformation: TLabel [3]
    Left = 12
    Top = 48
    Width = 316
    Height = 104
    Caption = 
      'You are about to transfer your records from your old copy of Rec' +
      'order 2002 into your copy of Recorder 6.  This transfer process ' +
      'will completely overwrite the current database and any data that' +
      ' may have been transferred or input previously.  If this fails f' +
      'or any reason, you can repeat the exercise. Once you are happy t' +
      'hat your data has transferred properly, you should uninstall Rec' +
      'order 2002 using the option provided on the CD and start to use ' +
      'Recorder 6 as your master database.'
    WordWrap = True
  end
  inherited pnlTitle: TPanel
    inherited lblTitle: TLabel
      Width = 167
      Caption = 'Transfering Your Data'
    end
  end
end
