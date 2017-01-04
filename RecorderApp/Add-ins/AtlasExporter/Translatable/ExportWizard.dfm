object dlgExportWizard: TdlgExportWizard
  Left = 362
  Top = 282
  Width = 515
  Height = 344
  Caption = 'Atlas Exporter'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100001000400280100001600000028000000100000002000
    000001000400000000000000000000000000000000000000000000000000EF51
    5200EFB2B500F7DBAD00F7EBD600FFFFFF000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000004444
    4444444444444333144413331334432204440222023441000000000000144322
    0444022202344322044402220234432204440222023441000000000000144444
    0222044402344444022204440234444402220444023441000000000000144322
    0222044402344322022204440234433313331444133444444444444444440000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    507
    317)
  PixelsPerInch = 96
  TextHeight = 13
  object lblExportProgress: TLabel
    Left = 8
    Top = 115
    Width = 74
    Height = 13
    Caption = 'Export Progress'
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 130
    Width = 481
    Height = 31
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
  end
  object pcWizard: TPageControl
    Left = 0
    Top = 0
    Width = 507
    Height = 267
    ActivePage = tsOutput
    Align = alTop
    MultiLine = True
    TabOrder = 0
    OnChange = pcWizardChange
    OnChanging = pcWizardChanging
    object tsOutput: TTabSheet
      Caption = 'Output'
      ImageIndex = 2
      DesignSize = (
        499
        239)
      object Label1: TLabel
        Left = 24
        Top = 197
        Width = 167
        Height = 13
        Caption = 'Enter the directory to output files to:'
      end
      object rgOutputMode: TRadioGroup
        Left = 24
        Top = 10
        Width = 441
        Height = 55
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Please select the grid output mode:'
        Items.Strings = (
          'Export all records at the finest grid resolution available'
          'Aggregate observation data')
        TabOrder = 0
        OnClick = rgOutputModeClick
      end
      object eOutputDir: TEdit
        Left = 24
        Top = 213
        Width = 417
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object btnSelectDir: TButton
        Left = 442
        Top = 213
        Width = 21
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Caption = '...'
        TabOrder = 3
        OnClick = btnSelectDirClick
      end
      object rgSynonymHandling: TRadioGroup
        Left = 24
        Top = 70
        Width = 441
        Height = 55
        Caption = 'Handling of synonyms:'
        ItemIndex = 0
        Items.Strings = (
          
            'Treat synonyms as distinct names and include them separately in ' +
            'the output'
          'Only use the preferred names in the output')
        TabOrder = 1
      end
      object rgOutputFileFormat: TRadioGroup
        Left = 24
        Top = 128
        Width = 441
        Height = 65
        Caption = 'Please select the output file format'
        ItemIndex = 0
        Items.Strings = (
          'dBase 3 (dbf)'
          'Comma separated text (csv)'
          'ESRI Shape File (shp)')
        TabOrder = 4
      end
    end
    object tsGrid: TTabSheet
      Caption = 'Grid'
      DesignSize = (
        499
        239)
      object lblOutputFinestInstruct: TLabel
        Left = 24
        Top = 24
        Width = 195
        Height = 26
        Caption = 
          'For non MTB data, please select the grid system and output resol' +
          'ution for the data.'
        Visible = False
        WordWrap = True
      end
      object lblOutputAggregatedInstruct: TLabel
        Left = 24
        Top = 16
        Width = 195
        Height = 39
        Caption = 
          'Please select the default grid system and output resolution that' +
          ' data will be aggregated to where possible.'
        WordWrap = True
      end
      object rgGridSystem: TRadioGroup
        Left = 256
        Top = 8
        Width = 209
        Height = 201
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Grid system:'
        Items.Strings = (
          'MTB'
          'MTBQ'
          'MTBQQ'
          'MTBQQQ'
          'MTBQYX')
        TabOrder = 0
        OnClick = rgGridSystemClick
      end
    end
    object tsNonCompatibleGrids: TTabSheet
      Caption = 'Non-compatible grids'
      ImageIndex = 3
      TabVisible = False
      object gbNonCompatibleGrids: TGroupBox
        Left = 0
        Top = 0
        Width = 489
        Height = 161
        Caption = 'How should non-compatible grids be handled?'
        TabOrder = 0
        object gbGermanGridConversions: TGroupBox
          Left = 24
          Top = 84
          Width = 393
          Height = 57
          Caption = 'German grids'
          TabOrder = 2
          object chkDiscardQYX: TCheckBox
            Left = 8
            Top = 15
            Width = 353
            Height = 17
            Caption = 'Discard MTBQYX data'
            TabOrder = 0
          end
          object chkDiscardQQQ: TCheckBox
            Left = 8
            Top = 34
            Width = 361
            Height = 17
            Caption = 'Discard MTBQQ and MTBQQQ data'
            TabOrder = 1
          end
        end
        object rgNonGermanGridConversion: TRadioGroup
          Left = 8
          Top = 16
          Width = 473
          Height = 57
          Caption = 'Non-German Grids'
          ItemIndex = 1
          Items.Strings = (
            'Convert non German grid system data into the target system'
            'Discard non-German grid system data')
          TabOrder = 0
        end
        object rgGermanGridConversions: TRadioGroup
          Left = 8
          Top = 84
          Width = 473
          Height = 57
          Caption = 'German grids'
          TabOrder = 1
        end
      end
      object rgResolveConflicts: TRadioGroup
        Left = 0
        Top = 160
        Width = 489
        Height = 57
        Caption = 'Conflict Resolution'
        ItemIndex = 0
        Items.Strings = (
          
            'Keep larger grid squares when more recent than smaller grid squa' +
            'res.'
          
            'Discard larger grid squares even when more recent than smaller g' +
            'rid squares.')
        TabOrder = 1
      end
    end
    object tsRegion: TTabSheet
      Caption = 'Region'
      ImageIndex = 1
      DesignSize = (
        499
        239)
      object Label4: TLabel
        Left = 8
        Top = 8
        Width = 465
        Height = 39
        Anchors = [akLeft, akTop, akRight]
        Caption = 
          'By default only Observations that fall into a valid MTB notation' +
          ' (between upper left 0900 and lower right 8756) will be exported' +
          '. You can override this by specifying MTB squares to include in ' +
          'the following box. Each square is a 4 digit number, separated by' +
          ' a semi-colon (;).'
        WordWrap = True
      end
      object mmMTBsToInclude: TMemo
        Left = 8
        Top = 64
        Width = 473
        Height = 153
        TabOrder = 0
      end
    end
  end
  object btnCancel: TButton
    Left = 232
    Top = 278
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnPrev: TButton
    Left = 331
    Top = 278
    Width = 75
    Height = 25
    Caption = 'Previous'
    TabOrder = 2
    OnClick = btnPrevClick
  end
  object btnNext: TButton
    Left = 416
    Top = 278
    Width = 75
    Height = 25
    Caption = 'Next'
    TabOrder = 3
    OnClick = btnNextClick
  end
  object btnFinish: TButton
    Left = 416
    Top = 278
    Width = 75
    Height = 25
    Caption = 'Finish'
    TabOrder = 4
    OnClick = btnFinishClick
  end
end
