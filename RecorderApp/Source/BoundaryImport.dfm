object dlgBoundaryImportDialog: TdlgBoundaryImportDialog
  Left = 576
  Top = 391
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Boundary Import'
  ClientHeight = 128
  ClientWidth = 302
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  DesignSize = (
    302
    128)
  PixelsPerInch = 96
  TextHeight = 13
  object bbOK: TImageListButton
    Left = 137
    Top = 98
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 0
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object bbCancel: TImageListButton
    Left = 220
    Top = 98
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object pnlOptions: TPanel
    Left = 8
    Top = 8
    Width = 287
    Height = 82
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    DesignSize = (
      287
      82)
    object lblOption: TLabel
      Left = 8
      Top = 8
      Width = 183
      Height = 13
      Caption = 'Match boundaries to existing locations:'
    end
    object lblBoundaryAttribute: TLabel
      Left = 8
      Top = 32
      Width = 115
      Height = 13
      Caption = 'Boundary Attribute Field:'
    end
    object lblLocationField: TLabel
      Left = 8
      Top = 57
      Width = 69
      Height = 13
      Caption = 'Location Field:'
    end
    object rbNo: TRadioButton
      Left = 244
      Top = 8
      Width = 37
      Height = 13
      Caption = 'No'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioClick
    end
    object rbYes: TRadioButton
      Left = 196
      Top = 8
      Width = 41
      Height = 13
      Caption = 'Yes'
      TabOrder = 1
      OnClick = RadioClick
    end
    object cmbBoundaryAttribute: TComboBox
      Left = 128
      Top = 28
      Width = 151
      Height = 21
      Hint = 
        'Identifies the attribute field to be used to match to the Record' +
        'er location field.'
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 2
      Text = '<Manual Match>'
      OnChange = cmbBoundaryAttributeChange
      Items.Strings = (
        '<Manual Match>')
    end
    object cmbLocationField: TComboBox
      Left = 128
      Top = 53
      Width = 151
      Height = 21
      Hint = 
        'Identifies which Recorder location field to be used in the match' +
        'ing process.'
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      ItemHeight = 13
      TabOrder = 3
      Items.Strings = (
        'Location Key'
        'Location Name'
        'File Code')
    end
  end
end
