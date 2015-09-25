inherited frmMDIContainer: TfrmMDIContainer
  Left = 347
  Top = 266
  Width = 459
  Height = 362
  Caption = 'OLE Container form for ActiveX forms'
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited mnuChildMerge: TMainMenu
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 2
      object mnuEditTransferData: TMenuItem
        Action = dmFormActions.actTransferData
      end
    end
  end
end
