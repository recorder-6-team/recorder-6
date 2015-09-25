object dlgRegisterMap: TdlgRegisterMap
  Left = 433
  Top = 184
  BorderStyle = bsDialog
  Caption = 'Map Registration Details'
  ClientHeight = 292
  ClientWidth = 330
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object bvlFram: TBevel
    Left = 4
    Top = 8
    Width = 321
    Height = 249
    Shape = bsFrame
  end
  object lblSouthWest: TLabel
    Left = 24
    Top = 144
    Width = 59
    Height = 13
    Caption = 'South-West:'
  end
  object lblInstructions: TLabel
    Left = 20
    Top = 64
    Width = 133
    Height = 13
    Caption = 'Enter the registration details:'
  end
  object lblCutIn: TLabel
    Left = 24
    Top = 200
    Width = 31
    Height = 13
    Caption = 'Cut In:'
  end
  object lblLayerPrompt: TLabel
    Left = 20
    Top = 20
    Width = 98
    Height = 13
    Caption = 'Selected Map Layer:'
  end
  object lblLayerDisp: TLabel
    Left = 20
    Top = 40
    Width = 260
    Height = 13
    AutoSize = False
    Caption = 
      '<SelectedLayer                                                  ' +
      '          >'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object lblCutOut: TLabel
    Left = 24
    Top = 228
    Width = 39
    Height = 13
    Caption = 'Cut Out:'
  end
  object lblFileName: TLabel
    Left = 24
    Top = 116
    Width = 47
    Height = 13
    Caption = 'File Name'
  end
  object lblName: TLabel
    Left = 24
    Top = 88
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object lblNorthEast: TLabel
    Left = 24
    Top = 172
    Width = 50
    Height = 13
    Caption = 'North-East'
  end
  object bbCancel: TImageListButton
    Left = 250
    Top = 264
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbOK: TImageListButton
    Left = 168
    Top = 264
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 7
    OnClick = bbOKClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object eSouthWest: TEdit
    Left = 92
    Top = 140
    Width = 149
    Height = 21
    TabOrder = 3
    OnExit = eSouthWestExit
  end
  object cmbCutIn: TComboBox
    Left = 92
    Top = 196
    Width = 93
    Height = 21
    ItemHeight = 13
    TabOrder = 5
    Items.Strings = (
      '1:100,000,000'
      '1:10,000,000'
      '1:1,000,000'
      '1:500,000'
      '1:250,000'
      '1:100,000'
      '1:50,000'
      '1:25,000'
      '1:10,000'
      '1:5,000'
      '1:2,500')
  end
  object cmbCutOut: TComboBox
    Left = 92
    Top = 224
    Width = 93
    Height = 21
    ItemHeight = 13
    TabOrder = 6
    Items.Strings = (
      '1:10,000,000'
      '1:1,000,000'
      '1:500,000'
      '1:250,000'
      '1:100,000'
      '1:50,000'
      '1:25,000'
      '1:10,000'
      '1:5,000'
      '1:2,500'
      '1:1000'
      '1:500'
      '1:250'
      '1:100'
      '1:10'
      '1:1')
  end
  object eFileName: TEdit
    Left = 92
    Top = 112
    Width = 205
    Height = 21
    TabOrder = 1
    OnChange = eFileNameChange
    OnExit = eFileNameExit
  end
  object eSheetName: TEdit
    Left = 92
    Top = 83
    Width = 149
    Height = 21
    TabOrder = 0
  end
  object eNorthEast: TEdit
    Left = 92
    Top = 168
    Width = 149
    Height = 21
    TabOrder = 4
    OnExit = eNorthEastExit
  end
  object bbFileName: TButton
    Left = 296
    Top = 112
    Width = 23
    Height = 22
    Caption = '...'
    TabOrder = 2
    OnClick = bbFileNameClick
  end
  object dlgOpenFile: TOpenDialog
    OnClose = dlgOpenFileClose
    Filter = 
      'All supported file types|*.gsf;*.dxf;*.bna;*.vpf;*.ntf;*.mif;*.s' +
      'hp;*.opt;*.std;*.bmp;*.wmf;*.emf|All supported vector file forma' +
      'ts [*.gsf, *.dxf, *.bna, *.vpf, *.ntf, *.mif, *.shp, *.opt, *.st' +
      'd]|*.gsf;*.dxf;*.bna;*.vpf;*.ntf;*.mif;*.shp;*.opt;*.std|All sup' +
      'ported raster file formats [*.bmp, *.wmf, *.emf, *.tif]|*.bmp;*.' +
      'wmf;*.emf|Windows Bitmaps (*.bmp)|*.bmp;|Atlas GIS (*.bna)|*.bna' +
      '|USGS DLG (*.std), (*.opt)|*.std;*.opt|AutoCAD DXF (*.dxf)|*.dxf' +
      '|MapInfo (*.mif)|*.mif|NTF (*.ntf)|*.ntf|ArcInfo Shape (*.shp)|*' +
      '.shp|Digital Chart of the World (*.vpf)|*.vpf|Metafiles (*.wmf),' +
      ' (*.emf)|*.wmf;*.emf|MapServer Sheets (*.gsf)|*.gsf|All Files [*' +
      '.*]|*.*|tif|*.tif'
    Title = 'Add New Sheets'
    Left = 268
    Top = 208
  end
end
