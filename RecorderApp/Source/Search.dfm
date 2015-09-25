inherited dmSearch: TdmSearch
  OldCreateOrder = True
  Left = 748
  Top = 107
  Height = 126
  Width = 219
  object qrySource: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 32
    Top = 24
  end
  object qryAdmin: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'SourceParam'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT AA.Admin_Area_Key, AA.Item_Name FROM Admin_Area AS AA'
      'WHERE AA.Parent = :SourceParam')
    ParseSQL = True
    Left = 112
    Top = 24
  end
end
