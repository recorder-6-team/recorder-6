object fraDBFileLocationSelect: TfraDBFileLocationSelect
  Left = 0
  Top = 0
  Width = 279
  Height = 90
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Color = clWhite
  ParentColor = False
  TabOrder = 0
  DesignSize = (
    279
    90)
  object lblNotLocal: TLabel
    Left = 20
    Top = 64
    Width = 111
    Height = 13
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'This is not a local drive.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object lblNoSpace: TLabel
    Left = 20
    Top = 64
    Width = 257
    Height = 17
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'Not enough space for the database files on this drive.'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Visible = False
    WordWrap = True
  end
  object rbDefault: TRadioButton
    Left = 0
    Top = 0
    Width = 209
    Height = 17
    Caption = 'Install Database files in default location.'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = RadioClick
  end
  object rbPickLocation: TRadioButton
    Left = 0
    Top = 20
    Width = 217
    Height = 17
    Caption = 'Install Database files in a custom location:'
    TabOrder = 1
    OnClick = RadioClick
  end
  object eFilePath: TEdit
    Left = 20
    Top = 40
    Width = 234
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 2
    OnChange = eFilePathChange
  end
  object btnFolderBrowse: TButton
    Left = 254
    Top = 40
    Width = 21
    Height = 19
    Anchors = [akTop, akRight]
    Caption = '...'
    Enabled = False
    TabOrder = 3
    OnClick = btnFolderBrowseClick
  end
  object dlgFolder: TFolderBrowser
    BrowseFlags = [bfDirsOnly]
    Folder = 'C:\'
    NewDialogStyle = False
    Title = 'Select installation folder:'
    Left = 248
    Top = 1
  end
end
