inherited fraDatabaseFileLocation: TfraDatabaseFileLocation
  object lblFileLocationInstruct: TLabel [0]
    Left = 8
    Top = 40
    Width = 308
    Height = 39
    Caption = 
      'If you need to install the database files to a custom location, ' +
      'then please select where they should be installed. Otherwise you' +
      ' should leave them to install in the default location. '
    WordWrap = True
  end
  inherited pnlTitle: TPanel
    inherited lblTitle: TLabel
      Width = 174
      Caption = 'Database File Location'
    end
  end
  inherited pnlContinue: TPanel
    inherited lblClickNext: TLabel
      Width = 125
      Caption = 'Click Install to continue...'
    end
  end
  inline fraSelectDBFileLocation: TfraDBFileLocationSelect
    Left = 0
    Top = 105
    Width = 336
    Height = 104
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Color = clWhite
    ParentColor = False
    TabOrder = 2
    inherited lblNotLocal: TLabel
      Left = 28
      Height = -47
    end
    inherited lblNoSpace: TLabel
      Left = 28
      Width = 314
      Height = -188
    end
    inherited rbDefault: TRadioButton
      Left = 12
    end
    inherited rbPickLocation: TRadioButton
      Left = 12
    end
    inherited eFilePath: TEdit
      Left = 28
      Width = 291
    end
    inherited btnFolderBrowse: TButton
      Left = 311
    end
  end
end
