object dlgDataExport: TdlgDataExport
  Left = 748
  Top = 251
  BorderStyle = bsDialog
  Caption = 'Data Export'
  ClientHeight = 283
  ClientWidth = 407
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  DesignSize = (
    407
    283)
  PixelsPerInch = 96
  TextHeight = 13
  object bvlFrame: TBevel
    Left = 4
    Top = 8
    Width = 399
    Height = 224
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
  end
  object lblExportType: TLabel
    Left = 16
    Top = 16
    Width = 60
    Height = 13
    Caption = 'Export Type:'
  end
  object lblDestination: TLabel
    Left = 16
    Top = 60
    Width = 56
    Height = 13
    Caption = 'Destination:'
  end
  object bbCancel: TImageListButton
    Left = 327
    Top = 252
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
    OnClick = bbCancelClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbOK: TImageListButton
    Left = 243
    Top = 252
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    TabOrder = 8
    OnClick = bbOkClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object cmbExportType: TComboBox
    Left = 16
    Top = 32
    Width = 177
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cmbExportTypeChange
  end
  object eDestination: TEdit
    Left = 16
    Top = 76
    Width = 349
    Height = 21
    TabOrder = 1
    OnChange = eDestinationChange
  end
  object cbIncludeObservations: TCheckBox
    Left = 16
    Top = 240
    Width = 129
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Include Observations'
    Checked = True
    State = cbChecked
    TabOrder = 6
    Visible = False
  end
  object cbIncludeSubSites: TCheckBox
    Left = 16
    Top = 260
    Width = 217
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Include Subsites for exported Locations'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object rgExportFilterSettings: TRadioGroup
    Left = 16
    Top = 108
    Width = 377
    Height = 93
    Caption = 'Export Filter Settings'
    TabOrder = 3
    OnClick = rgExportFilterSettingsClick
  end
  object dtpFilterDate: TDateTimePicker
    Left = 188
    Top = 156
    Width = 186
    Height = 21
    Date = 0.691639479169680300
    Time = 0.691639479169680300
    Enabled = False
    TabOrder = 4
    Visible = False
  end
  object cbReassignCustody: TCheckBox
    Left = 16
    Top = 207
    Width = 137
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Reassign Data Custody'
    Enabled = False
    TabOrder = 5
  end
  object bbExportTo: TButton
    Left = 368
    Top = 76
    Width = 23
    Height = 22
    Caption = '...'
    TabOrder = 2
    OnClick = bbExportToClick
  end
  object cbNoValidation: TCheckBox
    Left = 184
    Top = 208
    Width = 113
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Do Not Validate'
    Enabled = False
    TabOrder = 10
  end
  object dlgSave: TSaveDialog
    Left = 204
    Top = 24
  end
end
