inherited BaseDictionaryData: TBaseDictionaryData
  OldCreateOrder = True
  Left = 485
  Top = 206
  Height = 166
  Width = 350
  object qryList: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 20
    Top = 16
  end
  object qryTopLevel: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 92
    Top = 16
  end
  object qryChildLevel: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 168
    Top = 16
  end
  object qryParent: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 20
    Top = 68
  end
  object qryPreferredKey: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 92
    Top = 68
  end
  object ppTopLevel: TPageProducer
    OnHTMLTag = ppPopulateQueryTag
    Left = 268
    Top = 16
  end
  object ppChildLevel: TPageProducer
    OnHTMLTag = ppPopulateQueryTag
    Left = 272
    Top = 68
  end
end
