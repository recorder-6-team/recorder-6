inherited fraImport: TfraImport
  Width = 425
  Height = 166
  DesignSize = (
    425
    166)
  object lblStepProcessing: TLabel
    Left = 36
    Top = 40
    Width = 14
    Height = 17
    AutoSize = False
  end
  object lblProcessing: TLabel
    Left = 56
    Top = 40
    Width = 99
    Height = 13
    Caption = 'Processing import file'
  end
  object lblInfo: TLabel
    Left = 16
    Top = 16
    Width = 262
    Height = 13
    Caption = 'Importing data into Recorder.  This may take some time.'
  end
  object lblCreating: TLabel
    Left = 56
    Top = 64
    Width = 135
    Height = 13
    Caption = 'Creating temporary database'
  end
  object lblStepCreating: TLabel
    Left = 36
    Top = 64
    Width = 14
    Height = 17
    AutoSize = False
  end
  object btnStop: TBitBtn
    Left = 288
    Top = 136
    Width = 129
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '&Stop'
    TabOrder = 0
    OnClick = btnStopClick
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000090975001515
      87001313A2002E2E9A0042429C003D3DA5004E4EA500FF00FF006363AD006363
      B5006B6BB5007E7EB9008C8CC6009C9CCE00B5B5DE00FFFFFF007FFFFFFFFFFF
      FFF7FB000011111111CFF02222233333331FF02223333333553FF023CCCCCCCC
      565FF026CFFFFFFC664FF026CFFFFFFC685FF036CFFFFFFC886FF136CFFFFFFC
      896FF135CFFFFFFC8A6FF135CFFFFFFC9A6FF135CCCCCCCCAB8FF135566668AA
      BC9FF013546668ABCDAFFB1133345668AAEF7FFFFFFFFFFFFFF7}
    Spacing = 10
  end
  object pnlPostProcessing: TPanel
    Left = 36
    Top = 88
    Width = 381
    Height = 46
    BevelOuter = bvNone
    Color = clWhite
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
    object lblStepValidating: TLabel
      Left = 0
      Top = 0
      Width = 14
      Height = 17
      AutoSize = False
    end
    object lblStepDuplicates: TLabel
      Left = 0
      Top = 24
      Width = 14
      Height = 17
      AutoSize = False
    end
    object lblValidating: TLabel
      Left = 20
      Top = 0
      Width = 70
      Height = 13
      Caption = 'Validating data'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblDuplicates: TLabel
      Left = 20
      Top = 24
      Width = 111
      Height = 13
      Caption = 'Checking for duplicates'
    end
  end
end
