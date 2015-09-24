inherited dlgOpenReport: TdlgOpenReport
  Left = 269
  Top = 134
  Height = 420
  Caption = 'Open Saved Report'
  Constraints.MinHeight = 400
  Constraints.MinWidth = 650
  Font.Name = 'Arial'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 14
  inherited Bevel1: TBevel
    Width = 653
    Height = 337
  end
  inherited Label2: TLabel
    Left = 16
    Width = 82
    Height = 14
    Caption = 'Report Directory:'
  end
  inherited Label1: TLabel
    Left = 16
    Width = 68
    Height = 14
    Caption = '&Select Report:'
  end
  inherited pnlFiles: TPanel
    Left = 16
    Width = 636
    Height = 253
    TabOrder = 3
    inherited Splitter: TSplitter
      Height = 253
    end
    inherited tvSelectFile: TKeyboardRapidTree
      Height = 253
      Data = {
        9C00000001000000060854466C794E6F64658A000000060943656C6C73546578
        741300000006020D0A060648696464656E2000000000060A496D616765496E64
        65783400000000000000060D53656C6563746564496E6465784B000000000000
        00060A5374617465496E6465785F000000000000000604546578747300000006
        0854466C794E6F64650609556E6971756554616786000000FFFFFFFF00000000}
    end
    inherited pnlDescription: TPanel
      Width = 308
      Height = 253
      inherited rtfDescription: TRichEdit
        Width = 308
        Height = 224
      end
      object pnlFavourite: TPanel
        Left = 0
        Top = 224
        Width = 308
        Height = 29
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object chkIsFavourite: TCheckBox
          Left = 4
          Top = 8
          Width = 293
          Height = 13
          Caption = 'Display this report directly in the Report menu'
          Enabled = False
          TabOrder = 0
          OnClick = chkIsFavouriteClick
        end
      end
    end
  end
  inherited btnCancel: TImageListButton
    Left = 576
    Top = 351
    TabOrder = 2
  end
  inherited btnOK: TImageListButton
    Left = 492
    Top = 351
  end
  inherited eDirectory: TEdit
    Left = 16
    TabOrder = 4
  end
  inherited btnBrowse: TButton
    Left = 629
    Top = 31
    Width = 22
    TabOrder = 1
  end
end
