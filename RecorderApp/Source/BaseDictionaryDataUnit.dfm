inherited BaseDictionaryData: TBaseDictionaryData
  OldCreateOrder = True
  Left = 648
  Top = 208
  Height = 245
  Width = 396
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
  object ppVirtualTopLevel: TPageProducer
    HTMLDoc.Strings = (
      
        '           SELECT Taxon_List_Item_Key, Taxon_Rank_Key, Sort_Code' +
        ', Preferred_Name, System_Supplied_Data, Taxon_Version_Key, Taxon' +
        '_List_Version_Key,Parent,Has_Children'
      'INTO #Temp'
      'FROM <#TableName> Taxon_List_Item'
      '<#JoinFilter>'
      'WHERE'
      'Taxon_List_Version_Key IN ('
      '<#ListKeys>'
      ')'
      'AND Sort_Code = (Select min(sort_code) FROM <#TableName>)'
      'AND Taxon_List_Version_To IS NULL'
      'AND Preferred_Name=Taxon_List_Item_Key'
      '<#Filter>'
      ''
      
        'SELECT ITN.Taxon_List_Item_Key AS ListKey, ITN.Authority, ITN.Pr' +
        'eferred_Name AS ItemName,'
      
        'ITN.Preferred_Name_Italic AS ItemNameItalic, ITN.Preferred_Name_' +
        'Attribute as ItemNameAttribute,'
      
        'ITN.Preferred_Name_Authority AS ItemNameAuthor, ITN.Preferred_Na' +
        'me AS DisplayField,'
      'TLI.Taxon_Rank_Key AS RankKey, TLI.Sort_Code AS SortCode,'
      
        'TLI.Preferred_Name AS PrefNameKey, TLI.System_Supplied_Data AS S' +
        'ystemSupplied,'
      
        'TLI.Taxon_Version_Key AS TaxonVersionKey, ITN.Common_Name AS Com' +
        'monName, ITN.Common_Name_Italic AS CommonItalic,'
      
        'ITN.Common_Name_Attribute AS CommonNameAttribute,  TLI.Has_Child' +
        'ren as HasChildren,TLI.Parent as Parent'
      'FROM #Temp TLI'
      
        'INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI' +
        '.Taxon_List_Item_Key'
      'WHERE ITN.Actual_Name = ITN.Preferred_Name'
      '<#OrderBy>'
      ''
      'DROP TABLE #Temp')
    OnHTMLTag = ppPopulateQueryTag
    Left = 172
    Top = 80
  end
  object ppVirtualChildLevel: TPageProducer
    OnHTMLTag = ppPopulateQueryTag
    Left = 168
    Top = 140
  end
  object qryVirtualParent: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 36
    Top = 124
  end
  object ppVirtualParent: TPageProducer
    OnHTMLTag = ppPopulateQueryTag
    Left = 256
    Top = 140
  end
end
