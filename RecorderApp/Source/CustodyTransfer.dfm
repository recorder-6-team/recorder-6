object dlgCustodyTransfer: TdlgCustodyTransfer
  Left = 454
  Top = 242
  BorderStyle = bsDialog
  Caption = 'Transfer Custody'
  ClientHeight = 186
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    329
    186)
  PixelsPerInch = 96
  TextHeight = 13
  object bvlExplanation: TBevel
    Left = 4
    Top = 8
    Width = 321
    Height = 69
    Anchors = [akLeft, akTop, akRight]
    Shape = bsFrame
  end
  object lblExplanation: TLabel
    Left = 12
    Top = 16
    Width = 303
    Height = 52
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'This function allows you to transfer custody of your data to ano' +
      'ther site.  Only data over which you currently have custody will' +
      ' be affected.  After the transfer only the site to which custody' +
      ' has been transferred will be able to modify the data.'
    WordWrap = True
  end
  object lblCurrentSiteID: TLabel
    Left = 36
    Top = 88
    Width = 119
    Height = 13
    Caption = 'Enter your current Site ID'
  end
  object lblNewCustodian: TLabel
    Left = 36
    Top = 120
    Width = 164
    Height = 13
    Caption = 'Enter the Site ID receiving custody'
  end
  object btnCancel: TImageListButton
    Left = 248
    Top = 156
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object btnOk: TImageListButton
    Left = 164
    Top = 156
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = btnOkClick
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object eCurrentSite: TRestrictedEdit
    Left = 224
    Top = 84
    Width = 69
    Height = 21
    CharCase = ecUpperCase
    TabOrder = 0
    OnCheckText = eCheckText
  end
  object eNewCustodian: TRestrictedEdit
    Left = 224
    Top = 116
    Width = 69
    Height = 21
    CharCase = ecUpperCase
    TabOrder = 1
    OnCheckText = eCheckText
  end
end
