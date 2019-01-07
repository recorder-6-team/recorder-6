object DownloadDialog: TDownloadDialog
  Left = 56
  Top = 276
  Width = 721
  Height = 383
  Caption = 'DownloadDialog'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlInfo: TPanel
    Left = 437
    Top = 0
    Width = 268
    Height = 344
    Align = alClient
    TabOrder = 0
    object ProgressBar: TProgressBar
      Left = 1
      Top = 326
      Width = 266
      Height = 17
      Align = alBottom
      TabOrder = 0
    end
    object mmLog: TMemo
      Left = 1
      Top = 1
      Width = 266
      Height = 325
      Align = alClient
      TabOrder = 1
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 437
    Height = 344
    Align = alLeft
    AutoSize = True
    Caption = 'Panel1'
    TabOrder = 1
    object pnlSelectDownload: TPanel
      Left = 1
      Top = 122
      Width = 435
      Height = 221
      Align = alClient
      Enabled = False
      TabOrder = 0
      object lblSurvey: TLabel
        Left = 24
        Top = 96
        Width = 97
        Height = 13
        Caption = 'Survey to download:'
        Enabled = False
      end
      object lblStartDate: TLabel
        Left = 24
        Top = 140
        Width = 49
        Height = 13
        Caption = 'Start date:'
        Enabled = False
      end
      object lblEndDate: TLabel
        Left = 24
        Top = 166
        Width = 46
        Height = 13
        Caption = 'End date:'
        Enabled = False
      end
      object lblIntoSurvey: TLabel
        Left = 24
        Top = 191
        Width = 99
        Height = 13
        Caption = 'Survey to import into:'
        Enabled = False
      end
      object rgDownloadType: TRadioGroup
        Left = 16
        Top = 16
        Width = 401
        Height = 65
        Caption = 'Choose which records to download'
        Columns = 3
        Enabled = False
        Items.Strings = (
          'My Records')
        TabOrder = 0
      end
      object cmbSurvey: TComboBox
        Left = 144
        Top = 91
        Width = 273
        Height = 21
        Style = csDropDownList
        Enabled = False
        ItemHeight = 13
        TabOrder = 1
        OnChange = cmbSurveyChange
        Items.Strings = (
          '<all surveys>')
      end
      object dtpStartDate: TDateTimePicker
        Left = 144
        Top = 136
        Width = 186
        Height = 21
        Date = 0.653296238422626600
        Time = 0.653296238422626600
        Enabled = False
        TabOrder = 2
      end
      object dtpEndDate: TDateTimePicker
        Left = 144
        Top = 162
        Width = 186
        Height = 21
        Date = 41414.653376331020000000
        Time = 41414.653376331020000000
        Enabled = False
        TabOrder = 3
      end
      object cmbIntoSurvey: TComboBox
        Left = 144
        Top = 188
        Width = 273
        Height = 21
        Style = csDropDownList
        Enabled = False
        ItemHeight = 13
        TabOrder = 4
      end
      object cbLimitToAccepted: TCheckBox
        Left = 145
        Top = 115
        Width = 152
        Height = 17
        Caption = 'Limit to accepted records'
        Checked = True
        Enabled = False
        State = cbChecked
        TabOrder = 5
      end
    end
    object pnlLogin: TPanel
      Left = 1
      Top = 1
      Width = 435
      Height = 121
      Align = alTop
      TabOrder = 1
      object lblEmail: TLabel
        Left = 8
        Top = 32
        Width = 126
        Height = 13
        Caption = 'Email registered on Indicia:'
      end
      object lblPassword: TLabel
        Left = 8
        Top = 56
        Width = 82
        Height = 13
        Caption = 'Indicia password:'
      end
      object lblLoggedInAs: TLabel
        Left = 227
        Top = 88
        Width = 3
        Height = 13
      end
      object lblLoginInstruct: TLabel
        Left = 8
        Top = 8
        Width = 197
        Height = 13
        Caption = 'Please provide your login details to Indicia'
      end
      object eEmail: TEdit
        Left = 144
        Top = 29
        Width = 273
        Height = 21
        TabOrder = 0
      end
      object ePassword: TEdit
        Left = 144
        Top = 53
        Width = 273
        Height = 21
        PasswordChar = '*'
        TabOrder = 1
      end
      object btnLogin: TButton
        Left = 144
        Top = 80
        Width = 75
        Height = 25
        Caption = 'Login'
        TabOrder = 2
        OnClick = btnLoginClick
      end
    end
  end
  object IdHTTP1: TIdHTTP
    AllowCookies = True
    HandleRedirects = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoForceEncodeParams]
    Left = 392
    Top = 80
  end
end
