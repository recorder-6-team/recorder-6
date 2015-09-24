inherited dlgBatchUpdate: TdlgBatchUpdate
  Left = 278
  Top = 146
  Caption = 'Run Batch Update'
  Constraints.MinHeight = 225
  Constraints.MinWidth = 465
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlFiles: TPanel
    inherited tvSelectFile: TKeyboardRapidTree
      Data = {
        9C00000001000000060854466C794E6F64658A000000060943656C6C73546578
        741300000006020D0A060648696464656E2000000000060A496D616765496E64
        65783400000000000000060D53656C6563746564496E6465784B000000000000
        00060A5374617465496E6465785F000000000000000604546578747300000006
        0854466C794E6F64650609556E6971756554616786000000FFFFFFFF00000000}
    end
  end
  object qryBatchUpdate: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 412
    Top = 24
  end
end
