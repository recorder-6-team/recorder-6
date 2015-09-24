object frmSnapshot: TfrmSnapshot
  Left = 267
  Top = 199
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Reporting Snapshot Tool'
  ClientHeight = 495
  ClientWidth = 786
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object pcSnapshot: TPageControl
    Left = 426
    Top = 0
    Width = 360
    Height = 460
    ActivePage = tsServer
    Align = alRight
    TabOrder = 0
    object tsServer: TTabSheet
      Caption = 'tsServer'
      TabVisible = False
      object Label1: TLabel
        Left = 36
        Top = 324
        Width = 34
        Height = 13
        Caption = 'Server:'
      end
      object Label2: TLabel
        Left = 36
        Top = 348
        Width = 49
        Height = 13
        Caption = 'Database:'
      end
      object Label15: TLabel
        Left = 28
        Top = 20
        Width = 296
        Height = 52
        Caption = 
          'This Wizard allows you to create a reporting snapshot from the r' +
          'esults set you have already created.  It allows you to create a ' +
          'complete new data model with a simplified structure, making repo' +
          'rting easier.'
        WordWrap = True
      end
      object Label16: TLabel
        Left = 32
        Top = 276
        Width = 209
        Height = 26
        Caption = 
          'Select the server and database you want to place the snapshot on' +
          'to:'
        WordWrap = True
      end
      object Label17: TLabel
        Left = 28
        Top = 80
        Width = 296
        Height = 26
        Caption = 
          'The attributes included are those you have already selected in t' +
          'he Report Wizard.'
        WordWrap = True
      end
      object Label19: TLabel
        Left = 32
        Top = 144
        Width = 230
        Height = 13
        Caption = 'Enter your SQL Server Username and Password:'
      end
      object lblUserName: TLabel
        Left = 36
        Top = 168
        Width = 51
        Height = 13
        Caption = 'Username:'
      end
      object lblPassword: TLabel
        Left = 36
        Top = 196
        Width = 49
        Height = 13
        Caption = 'Password:'
      end
      object Bevel1: TBevel
        Left = 60
        Top = 252
        Width = 233
        Height = 5
        Shape = bsBottomLine
      end
      object Bevel2: TBevel
        Left = 56
        Top = 124
        Width = 233
        Height = 5
        Shape = bsBottomLine
      end
      object cmbServer: TComboBox
        Left = 108
        Top = 320
        Width = 173
        Height = 21
        ItemHeight = 13
        TabOrder = 3
        Text = 'Select SQL Server or MSDE'
        OnChange = ControlChanged
        OnEnter = cmbServerEnter
        OnExit = cmbServerExit
      end
      object cmbDatabase: TComboBox
        Left = 108
        Top = 348
        Width = 173
        Height = 21
        ItemHeight = 13
        TabOrder = 4
        Text = 'Select Database on Server'
        OnChange = cmbDatabaseChange
      end
      object chkDatabase: TCheckBox
        Left = 36
        Top = 380
        Width = 185
        Height = 17
        Caption = 'Create database if it doesn'#39't exist'
        TabOrder = 5
        OnClick = ControlChanged
      end
      object eUserName: TEdit
        Left = 100
        Top = 164
        Width = 121
        Height = 21
        TabOrder = 0
        OnChange = ControlChanged
      end
      object ePassword: TEdit
        Left = 100
        Top = 192
        Width = 121
        Height = 21
        PasswordChar = '*'
        TabOrder = 1
        OnChange = ControlChanged
      end
      object chkTrustedSecurity: TCheckBox
        Left = 36
        Top = 224
        Width = 249
        Height = 17
        Caption = 'Use my network login to access SQL Server'
        TabOrder = 2
        OnClick = chkTrustedSecurityClick
      end
      object chkOverwriteTables: TCheckBox
        Left = 36
        Top = 408
        Width = 285
        Height = 17
        Caption = 'Overwrite existing tables without prompting'
        TabOrder = 6
        OnClick = ControlChanged
      end
    end
    object tsModel: TTabSheet
      Caption = 'tsModel'
      ImageIndex = 1
      TabVisible = False
      object Label4: TLabel
        Left = 24
        Top = 48
        Width = 245
        Height = 26
        Caption = 
          'Please select the tables you would like to include in the snapsh' +
          'ot data model:'
        WordWrap = True
      end
      object chkSeparateObs: TCheckBox
        Left = 44
        Top = 248
        Width = 261
        Height = 17
        Caption = 'Separate Taxon and Biotope Observations table'
        TabOrder = 4
        OnClick = CheckBoxClick
      end
      object chkSamples: TCheckBox
        Left = 44
        Top = 176
        Width = 197
        Height = 17
        Caption = 'Separate Sample Table'
        TabOrder = 2
        OnClick = CheckBoxClick
      end
      object chkEvents: TCheckBox
        Left = 44
        Top = 140
        Width = 197
        Height = 17
        Caption = 'Separate Events Table'
        TabOrder = 1
        OnClick = CheckBoxClick
      end
      object chkSurveys: TCheckBox
        Left = 44
        Top = 104
        Width = 197
        Height = 17
        Caption = 'Separate Survey Table'
        TabOrder = 0
        OnClick = CheckBoxClick
      end
      object chkSeparateDict: TCheckBox
        Left = 44
        Top = 320
        Width = 257
        Height = 17
        Caption = 'Separate Taxon and Biotope Dictionary table'
        TabOrder = 6
        OnClick = CheckBoxClick
      end
      object chkObsData: TCheckBox
        Left = 44
        Top = 284
        Width = 145
        Height = 17
        Caption = 'Occurrence Data table'
        Enabled = False
        TabOrder = 5
        OnClick = CheckBoxClick
      end
      object chkSampleData: TCheckBox
        Left = 44
        Top = 212
        Width = 169
        Height = 17
        Caption = 'Sample Data table'
        Enabled = False
        TabOrder = 3
        OnClick = CheckBoxClick
      end
    end
    object tsTableNames: TTabSheet
      Caption = 'Survey Table:'
      ImageIndex = 2
      TabVisible = False
      object Label3: TLabel
        Left = 8
        Top = 48
        Width = 329
        Height = 13
        Caption = 
          'Please select the names you would like each table to be created ' +
          'with:'
      end
      object lblSurveyName: TLabel
        Left = 12
        Top = 76
        Width = 66
        Height = 13
        Caption = 'Survey Table:'
        Enabled = False
      end
      object lblEventName: TLabel
        Left = 12
        Top = 104
        Width = 66
        Height = 13
        Caption = 'Events Table:'
        Enabled = False
      end
      object lblSampleName: TLabel
        Left = 12
        Top = 132
        Width = 73
        Height = 13
        Caption = 'Samples Table:'
        Enabled = False
      end
      object lblSampleDataName: TLabel
        Left = 12
        Top = 160
        Width = 94
        Height = 13
        Caption = 'Sample Data Table:'
        Enabled = False
      end
      object lblTaxObsName: TLabel
        Left = 12
        Top = 188
        Width = 95
        Height = 13
        Caption = 'Observations Table:'
      end
      object lblBioObsName: TLabel
        Left = 12
        Top = 216
        Width = 134
        Height = 13
        Caption = 'Biotope Observations Table:'
        Enabled = False
      end
      object lblTaxObsDataName: TLabel
        Left = 12
        Top = 244
        Width = 111
        Height = 13
        Caption = 'Taxon Obs Data Table:'
        Enabled = False
      end
      object lblBioObsDataName: TLabel
        Left = 12
        Top = 272
        Width = 117
        Height = 13
        Caption = 'Biotope Obs Data Table:'
        Enabled = False
      end
      object lblTaxDictName: TLabel
        Left = 12
        Top = 300
        Width = 85
        Height = 13
        Caption = 'Taxon Dict Table:'
        Enabled = False
      end
      object lblBioDictName: TLabel
        Left = 12
        Top = 328
        Width = 91
        Height = 13
        Caption = 'Biotope Dict Table:'
        Enabled = False
      end
      object Label18: TLabel
        Left = 66
        Top = 392
        Width = 192
        Height = 26
        Alignment = taCenter
        Caption = 'Click Run to create the snapshot.  This may take a few minutes.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object eBioObsName: TEdit
        Left = 152
        Top = 212
        Width = 157
        Height = 21
        Enabled = False
        MaxLength = 30
        TabOrder = 5
        Text = 'BioObs'
        OnChange = eTableNameChange
      end
      object eBioDictName: TEdit
        Left = 152
        Top = 324
        Width = 157
        Height = 21
        Enabled = False
        MaxLength = 30
        TabOrder = 9
        Text = 'BiotopeDict'
        OnChange = eTableNameChange
      end
      object eBioObsDataName: TEdit
        Left = 152
        Top = 268
        Width = 157
        Height = 21
        Enabled = False
        MaxLength = 30
        TabOrder = 7
        Text = 'BioObsData'
        OnChange = eTableNameChange
      end
      object eTaxObsDataName: TEdit
        Left = 152
        Top = 240
        Width = 157
        Height = 21
        Enabled = False
        MaxLength = 30
        TabOrder = 6
        Text = 'TaxObsData'
        OnChange = eTableNameChange
      end
      object eTaxDictName: TEdit
        Left = 152
        Top = 296
        Width = 157
        Height = 21
        Enabled = False
        MaxLength = 30
        TabOrder = 8
        Text = 'TaxonDict'
        OnChange = eTableNameChange
      end
      object eSurveyName: TEdit
        Left = 152
        Top = 72
        Width = 157
        Height = 21
        Enabled = False
        MaxLength = 30
        TabOrder = 0
        Text = 'Surveys'
        OnChange = eTableNameChange
      end
      object eEventName: TEdit
        Left = 152
        Top = 100
        Width = 157
        Height = 21
        Enabled = False
        MaxLength = 30
        TabOrder = 1
        Text = 'Events'
        OnChange = eTableNameChange
      end
      object eSampleName: TEdit
        Left = 152
        Top = 128
        Width = 157
        Height = 21
        Enabled = False
        MaxLength = 30
        TabOrder = 2
        Text = 'Samples'
        OnChange = eTableNameChange
      end
      object eSampleDataName: TEdit
        Left = 152
        Top = 156
        Width = 157
        Height = 21
        Enabled = False
        MaxLength = 30
        TabOrder = 3
        Text = 'SampleData'
        OnChange = eTableNameChange
      end
      object eTaxObsName: TEdit
        Left = 152
        Top = 184
        Width = 157
        Height = 21
        MaxLength = 30
        TabOrder = 4
        Text = 'Observations'
        OnChange = eTableNameChange
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 460
    Width = 786
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      786
      35)
    object btnCancel: TButton
      Left = 708
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Close'
      TabOrder = 2
      OnClick = btnCancelClick
    end
    object btnPrevious: TImageListButton
      Left = 532
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Previous'
      Enabled = False
      TabOrder = 0
      OnClick = btnPreviousClick
      ImageList = dmFormActions.ilMenuOn
      ImageIndex = 45
    end
    object btnNext: TImageListButton
      Left = 616
      Top = 6
      Width = 75
      Height = 25
      Caption = '&Next'
      TabOrder = 1
      OnClick = btnNextClick
      ImageList = dmFormActions.ilMenuOn
      ImageIndex = 46
    end
  end
  object pnlDiagram: TPanel
    Left = 0
    Top = 0
    Width = 426
    Height = 460
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    TabOrder = 2
    object pntDataDiagram: TPaintBox
      Left = 1
      Top = 1
      Width = 420
      Height = 454
      Align = alClient
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      OnPaint = pntDataDiagramPaint
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'snp'
    Filter = 'Recorder Snapshot Files(*.snp)|*.snp|All Files (*.*)|*.*'
    Title = 'Load Snapshot file'
    Left = 303
    Top = 384
  end
  object cnnNewDatabase: TADOConnection
    LoginPrompt = False
    Provider = 'SQLOLEDB.1'
    Left = 126
    Top = 384
  end
  object qryAllPurpose: TADOQuery
    Connection = cnnNewDatabase
    Parameters = <>
    Left = 220
    Top = 384
  end
  object qryForeignKeys: TADOQuery
    Connection = cnnNewDatabase
    Parameters = <
      item
        Name = 'TableName'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'select '#39'alter table ['#39'+user_name(uid) + '#39'].['#39' + object_Name(fkey' +
        'id) + '#39'] drop constraint ['#39' + object_Name(constid) + '#39']'#39' '
      'from sysforeignkeys  as A inner join sysobjects as B on '
      'A.rkeyid=B.id  where rkeyID = object_ID(:TableName)')
    Left = 32
    Top = 384
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'snp'
    Filter = 'Recorder Snapshot Files(*.snp)|*.snp|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save Snapshot File'
    Left = 378
    Top = 384
  end
end
