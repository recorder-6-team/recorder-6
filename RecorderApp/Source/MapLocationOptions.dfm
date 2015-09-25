object dlgMapLocationOptions: TdlgMapLocationOptions
  Left = 415
  Top = 280
  BorderStyle = bsDialog
  Caption = 'Location Options'
  ClientHeight = 213
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblNoAssociation: TLabel
    Left = 8
    Top = 12
    Width = 282
    Height = 13
    Caption = 'There is no Location currently associated with this boundary'
  end
  object bbCancel: TImageListButton
    Left = 212
    Top = 180
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object bbOK: TImageListButton
    Left = 128
    Top = 180
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 28
    Width = 281
    Height = 145
    TabOrder = 0
    object lblAdminArea: TLabel
      Left = 16
      Top = 96
      Width = 84
      Height = 13
      Caption = 'Admin Area Type:'
      Enabled = False
    end
    object rbNewLocation: TRadioButton
      Left = 12
      Top = 16
      Width = 249
      Height = 17
      Caption = 'Create a new location'
      TabOrder = 0
      OnClick = rbAdminAreaClick
    end
    object rbExistingLocation: TRadioButton
      Left = 12
      Top = 44
      Width = 233
      Height = 17
      Caption = 'Link the boundary to an existing Location'
      TabOrder = 1
      OnClick = rbAdminAreaClick
    end
    object rbAdminArea: TRadioButton
      Left = 12
      Top = 72
      Width = 249
      Height = 17
      Caption = 'Link the boundary to an Admin Area'
      TabOrder = 2
      OnClick = rbAdminAreaClick
    end
    object cmbAdminAreaType: TComboBox
      Left = 16
      Top = 112
      Width = 237
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 3
    end
  end
end
