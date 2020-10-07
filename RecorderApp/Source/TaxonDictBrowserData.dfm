inherited dmTaxonDictBrowser: TdmTaxonDictBrowser
  Left = 649
  Top = 215
  Height = 414
  Width = 348
  inherited ppTopLevel: TPageProducer
    HTMLDoc.Strings = (
      
        'SELECT Taxon_List_Item_Key, Taxon_Rank_Key, Sort_Code, Preferred' +
        '_Name, System_Supplied_Data, Taxon_Version_Key, Taxon_List_Versi' +
        'on_Key'
      'INTO #Temp'
      'FROM Taxon_List_Item'
      '<#JoinFilter>'
      'WHERE'
      'Taxon_List_Version_Key IN ('
      '<#ListKeys>'
      ')'
      'AND Parent IS NULL'
      'AND Taxon_List_Version_To IS NULL'
      'AND Preferred_Name=Taxon_List_Item_Key'
      '<#Filter>'
      ''
      
        'SELECT ITN.Taxon_List_Item_Key AS ListKey, ITN.Authority, ITN.Pr' +
        'eferred_Name AS ItemName,'
      
        'ITN.Preferred_Name_Italic AS ItemNameItalic, ITN.Preferred_Name_' +
        'Attribute as ItemNameAttribute, '
      
        'ITN.Preferred_Name_Authority AS ItemNameAuthor, ITN.Preferred_Na' +
        'me AS DisplayField,'
      'TLI.Taxon_Rank_Key AS RankKey, TLI.Sort_Code AS SortCode,'
      
        'TLI.Preferred_Name AS PrefNameKey, TLI.System_Supplied_Data AS S' +
        'ystemSupplied,'
      
        'TLI.Taxon_Version_Key AS TaxonVersionKey, ITN.Common_Name AS Com' +
        'monName, ITN.Common_Name_Italic AS CommonItalic, '
      
        'ITN.Common_Name_Attribute AS CommonNameAttribute, ITN.Has_Childr' +
        'en  AS HasChildren'
      'FROM #Temp TLI'
      
        'INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI' +
        '.Taxon_List_Item_Key'
      'WHERE ITN.Taxon_List_Version_Key = TLI.Taxon_List_Version_Key'
      'AND ITN.Actual_Name = ITN.Preferred_Name'
      '<#OrderBy>'
      ''
      'DROP TABLE #Temp'
      ''
      '')
  end
  inherited ppChildLevel: TPageProducer
    HTMLDoc.Strings = (
      
        'SELECT Taxon_List_Item_Key, Taxon_Rank_Key, Sort_Code, Preferred' +
        '_Name, System_Supplied_Data, Taxon_Version_Key, Taxon_List_Versi' +
        'on_Key'
      'INTO #Temp'
      'FROM Taxon_List_Item'
      '<#JoinFilter>'
      'WHERE Parent = '#39'<#ParentKey>'#39
      'AND Taxon_List_Version_To IS NULL'
      'AND Preferred_Name=Taxon_List_Item_Key'
      '<#Filter>'
      ''
      
        'SELECT ITN.Taxon_List_Item_Key AS ListKey, ITN.Authority, ITN.Pr' +
        'eferred_Name AS ItemName,'
      
        'ITN.Preferred_Name_Italic AS ItemNameItalic, ITN.Preferred_Name_' +
        'Attribute AS ItemNameAttribute, '
      
        'ITN.Preferred_Name_Authority AS ItemNameAuthor, ITN.Preferred_Na' +
        'me AS DisplayField,'
      'TLI.Taxon_Rank_Key AS RankKey, TLI.Sort_Code AS SortCode,'
      
        'TLI.Preferred_Name AS PrefNameKey, TLI.System_Supplied_Data AS S' +
        'ystemSupplied,'
      
        'TLI.Taxon_Version_Key AS TaxonVersionKey, ITN.Common_Name AS Com' +
        'monName, ITN.Common_Name_Italic AS CommonItalic, '
      
        'ITN.Common_Name_Attribute AS CommonNameAttribute, ITN.Has_Childr' +
        'en AS HasChildren'
      'FROM #Temp TLI'
      
        'INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI' +
        '.Taxon_List_Item_Key'
      'WHERE ITN.Taxon_List_Version_Key = TLI.Taxon_List_Version_Key'
      'AND ITN.Actual_Name = ITN.Preferred_Name'
      '<#OrderBy>'
      ''
      'DROP TABLE #Temp')
  end
  inherited ppVirtualTopLevel: TPageProducer
    HTMLDoc.Strings = (
      
        'SELECT Taxon_List_Item_Key, Taxon_Rank_Key, Sort_Code, Preferred' +
        '_Name, System_Supplied_Data, Taxon_Version_Key, Taxon_List_Versi' +
        'on_Key,Parent,Has_Children'
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
      
        'SELECT ITN.Taxon_List_Item_Key AS ListKey, ITN.Authority, ITN.Ac' +
        'tual_Name AS ItemName,'
      
        'ITN.Actual_Name_Italic AS ItemNameItalic, ITN.Actual_Name_Attrib' +
        'ute as ItemNameAttribute,'
      
        'ITN.Authority AS ItemNameAuthor, ITN.Actual_Name AS DisplayField' +
        ','
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
      '<#OrderBy>'
      ''
      'DROP TABLE #Temp')
    Left = 268
    Top = 128
  end
  inherited ppVirtualChildLevel: TPageProducer
    HTMLDoc.Strings = (
      
        'SELECT Taxon_List_Item_Key, Taxon_Rank_Key, Sort_Code, Preferred' +
        '_Name, System_Supplied_Data, Taxon_Version_Key, Taxon_List_Versi' +
        'on_Key,'
      'Has_Children'
      'INTO #Temp'
      'FROM <#TableName> Taxon_List_Item'
      '<#JoinFilter>'
      'WHERE Parent = '#39'<#ParentKey>'#39
      'AND Taxon_List_Version_To IS NULL'
      'AND Preferred_Name=Taxon_List_Item_Key'
      '<#Filter>'
      ''
      
        'SELECT ITN.Taxon_List_Item_Key AS ListKey, ITN.Authority, ITN.Ac' +
        'tual_Name AS ItemName,'
      
        'ITN.Actual_Name_Italic AS ItemNameItalic, ITN.Actual_Name_Attrib' +
        'ute AS ItemNameAttribute,'
      
        'ITN.Authority AS ItemNameAuthor, ITN.Actual_Name AS DisplayField' +
        ','
      'TLI.Taxon_Rank_Key AS RankKey, TLI.Sort_Code AS SortCode,'
      
        'TLI.Preferred_Name AS PrefNameKey, TLI.System_Supplied_Data AS S' +
        'ystemSupplied,'
      
        'TLI.Taxon_Version_Key AS TaxonVersionKey, ITN.Common_Name AS Com' +
        'monName, ITN.Common_Name_Italic AS CommonItalic,'
      
        'ITN.Common_Name_Attribute AS CommonNameAttribute, TLI.Has_Childr' +
        'en AS HasChildren'
      'FROM #Temp TLI'
      
        'INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI' +
        '.Taxon_List_Item_Key'
      '<#OrderBy>'
      ''
      'DROP TABLE #Temp')
    Left = 184
    Top = 92
  end
  inherited qryVirtualParent: TJNCCQuery
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      '')
    Left = 260
    Top = 204
  end
  inherited ppVirtualParent: TPageProducer
    HTMLDoc.Strings = (
      'SELECT Parent'
      'FROM <#TableName> Taxon_List_Item'
      'WHERE Taxon_List_Item_Key = :Key')
    Left = 264
  end
  inherited qrySynonyms: TJNCCQuery
    Parameters = <
      item
        Name = 'PrefNameKey'
        Size = -1
        Value = Null
      end
      item
        Name = 'ListVersionKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT DISTINCT ITN2.Actual_Name, TV.Attribute, ITN2.Authority, ' +
        'ITN2.Actual_Name_Italic, T.Language, ITN2.Sort_Order, TR.Short_N' +
        'ame,'
      #9#9#9'CASE TLV1.Taxon_List_Key'
      #9#9#9#9'WHEN TLV2.Taxon_List_Key THEN 1'
      #9#9#9#9'ELSE 0'
      '      END AS CurrentList'
      'FROM Index_Taxon_Name ITN1'
      
        'INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_' +
        'Item_Key = ITN1.Recommended_Taxon_List_Item_Key'
      
        'INNER JOIN Taxon_List_Item TLI1 ON TLI1.Taxon_List_Item_Key = IT' +
        'N1.Taxon_List_Item_Key'
      
        'INNER JOIN Taxon_List_Item TLI2 ON TLI2.Taxon_List_Item_Key = IT' +
        'N2.Taxon_List_Item_Key'
      
        'INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key = TLI2.Taxon' +
        '_Version_Key'
      'INNER JOIN Taxon T ON T.Taxon_Key = TV.Taxon_Key'
      
        'INNER JOIN Taxon_List_Version TLV1 ON TLI1.Taxon_List_Version_Ke' +
        'y = TLV1.Taxon_List_Version_Key'
      
        'INNER JOIN Taxon_List_Version TLV2 ON TLI2.Taxon_List_Version_Ke' +
        'y = TLV2.Taxon_List_Version_Key'
      
        'INNER JOIN Taxon_Rank TR ON TR.Taxon_Rank_Key = TLI2.Taxon_Rank_' +
        'Key'
      ''
      'WHERE ITN1.Taxon_List_Item_Key = :PrefNameKey'
      'AND ITN2.Taxon_List_Item_Key<>ITN1.Taxon_List_Item_Key'
      'AND (ITN1.Common_Name <> ITN2.Actual_Name OR T.Language = '#39'La'#39')'
      'ORDER BY T.Language, ITN2.Sort_Order')
  end
  inherited qryFacts: TJNCCQuery
    Left = 88
    Top = 276
  end
  inherited qryStatus: TJNCCQuery
    Left = 108
  end
  inherited qryCodes: TJNCCQuery
    SQL.Strings = (
      'SELECT TAXON_LIST_ITEM.LST_ITM_CODE'
      'FROM TAXON_LIST INNER JOIN (TAXON_LIST_VERSION INNER JOIN'
      '((TAXON INNER JOIN TAXON_VERSION ON'
      'TAXON.TAXON_KEY = TAXON_VERSION.TAXON_KEY)'
      'INNER JOIN TAXON_LIST_ITEM ON'
      
        'TAXON_VERSION.TAXON_VERSION_KEY = TAXON_LIST_ITEM.TAXON_VERSION_' +
        'KEY)'
      
        'ON TAXON_LIST_VERSION.TAXON_LIST_VERSION_KEY = TAXON_LIST_ITEM.T' +
        'AXON_LIST_VERSION_KEY)'
      'ON TAXON_LIST.TAXON_LIST_KEY = TAXON_LIST_VERSION.TAXON_LIST_KEY'
      
        'WHERE TAXON_VERSION.TAXON_VERSION_KEY =:TaxonKey AND TAXON_LIST_' +
        'VERSION.TAXON_LIST_KEY=:ListKey;')
  end
  inherited qryCheckLists: TJNCCQuery
    SQL.Strings = (
      
        'SELECT DISTINCT TL.Item_Name, TL.Taxon_List_Key, TLI.Preferred_N' +
        'ame, TLI.Taxon_List_Item_Key, TLV.Taxon_List_Version_Key'
      'FROM INDEX_TAXON_SYNONYM ITS'
      
        'INNER JOIN Taxon_List_Item TLI ON ITS.SYNONYM_LIST_ITEM_KEY = TL' +
        'I.TAXON_LIST_ITEM_KEY'
      
        'INNER JOIN TAXON_LIST_VERSION TLV ON TLI.TAXON_LIST_VERSION_KEY=' +
        ' TLV.TAXON_LIST_VERSION_KEY'
      'AND TLI.TAXON_LIST_VERSION_TO IS NULL'
      
        'INNER JOIN TAXON_LIST TL ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_K' +
        'EY'
      
        'INNER JOIN TAXON_LIST_ITEM TLI2 ON TLI2.TAXON_LIST_ITEM_KEY = IT' +
        'S.TAXON_LIST_ITEM_KEY'
      
        'INNER JOIN TAXON_LIST_VERSION TLV2 ON TLI2.TAXON_LIST_VERSION_KE' +
        'Y = TLV2.TAXON_LIST_VERSION_KEY'
      'AND TLI2.TAXON_LIST_VERSION_TO IS NULL'
      
        'INNER JOIN TAXON_LIST TL2 on TLV2.TAXON_LIST_KEY = TL2.TAXON_LIS' +
        'T_KEY'
      'WHERE ITS.Taxon_List_Item_Key = :ItemKey'
      'AND TL.Taxon_List_Key <> TL2.Taxon_List_Key'
      'AND TLI.TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME')
  end
  object qryLocalLists: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      
        'SELECT TL.TAXON_LIST_KEY AS KeyField, TL.ITEM_NAME AS DisplayFie' +
        'ld, TL.LOCAL_DISK AS IsLocal'
      'FROM TAXON_LIST AS TL, TAXON_LIST_TYPE AS TLT'
      'WHERE TL.LOCAL_DISK = True AND TLT.Allow_Data_Entry = True'
      'AND TLT.TAXON_LIST_TYPE_KEY = TL.TAXON_LIST_TYPE_KEY'
      'ORDER BY TL.ITEM_NAME;')
    ParseSQL = True
    Left = 168
    Top = 292
  end
  object dsLocalLists: TDataSource
    DataSet = qryLocalLists
    Left = 212
    Top = 316
  end
  object qryCheckListsITN: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'ItemKey'
        DataType = ftString
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT DISTINCT TL.Item_Name, TL.Taxon_List_Key, TLI.Preferred_N' +
        'ame, TLI.Taxon_List_Item_Key, TLV2.Taxon_List_Version_Key'
      'FROM Index_Taxon_Name ITN'
      
        'INNER JOIN TAXON_LIST_VERSION TLV ON TLV.TAXON_LIST_VERSION_KEY=' +
        ' ITN.TAXON_LIST_VERSION_KEY'
      'INNER JOIN INDEX_TAXON_NAME ITN2 '
      
        'ON ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = ITN2.RECOMMENDED_TAXON_' +
        'LIST_ITEM_KEY'
      
        'INNER JOIN TAXON_LIST_VERSION TLV2 ON TLV2.TAXON_LIST_VERSION_KE' +
        'Y = ITN2.TAXON_LIST_VERSION_KEY'
      
        'INNER JOIN TAXON_LIST TL ON TL.TAXON_LIST_KEY = TLV2.TAXON_LIST_' +
        'KEY'
      
        'INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY = ITN2' +
        '.TAXON_LIST_ITEM_KEY'
      'WHERE ITN.Taxon_List_Item_Key = :ItemKey'
      'AND TLV.Taxon_List_Key <> TLV2.Taxon_List_Key'
      'AND TLI.TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME AND'
      'ITN2.DEPRECATED = 0  ')
    ParseSQL = True
    Left = 92
    Top = 336
  end
end
