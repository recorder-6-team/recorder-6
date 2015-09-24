inherited BaseTaxonDictData: TBaseTaxonDictData
  Left = 295
  Top = 226
  Height = 371
  Width = 396
  inherited qryList: TJNCCQuery
    SQL.Strings = (
      'SELECT TL.Taxon_List_Key AS KeyField,'
      '       TL.Item_Name AS DisplayField,'
      '       TL.Local_Disk AS IsLocal'
      'FROM Taxon_List AS TL'
      'ORDER BY TL.Item_Name;')
  end
  inherited qryParent: TJNCCQuery
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Parent'
      'FROM Taxon_List_Item'
      'WHERE Taxon_List_Item_Key = '
      '  (SELECT Preferred_Name '
      '   FROM Taxon_List_Item'
      '   WHERE Taxon_List_Item_Key = :Key)')
  end
  inherited qryPreferredKey: TJNCCQuery
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Preferred_Name'
      'FROM Taxon_List_Item'
      'WHERE Taxon_List_Item_Key = :Key')
  end
  inherited ppTopLevel: TPageProducer
    HTMLDoc.Strings = (
      
        'SELECT DISTINCT ITN.Taxon_List_Item_Key AS ListKey, ITN.Authorit' +
        'y, ITN.Preferred_Name AS ItemName, '
      
        '  ITN.Preferred_Name_Attribute AS ItemNameAttribute, ITN.Preferr' +
        'ed_Name_Authority AS ItemNameAuthor,'
      
        '  ITN.Preferred_Name_Italic AS ItemNameItalic, TV.Taxon_Key AS T' +
        'axonKey, ITN.Preferred_Name AS DisplayField,'
      
        '  TLI.Taxon_Rank_Key AS RankKey, TLI.Sort_Code AS SortCode, TLI.' +
        'Preferred_Name AS PrefNameKey,'
      
        '  TLI.System_Supplied_Data AS SystemSupplied, TV.Taxon_Version_K' +
        'ey AS TaxonVersionKey,'
      
        '  ITN.Common_Name AS CommonName,  ITN.Common_Name_Attribute AS C' +
        'ommonNameAttribute,'
      
        '  ITN.Common_Name_Italic AS CommonItalic, ITN.Has_Children AS Ha' +
        'sChildren'
      'FROM Taxon_List_Item TLI'
      
        'INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI' +
        '.Taxon_List_Item_Key'
      
        'INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_' +
        'Version_Key'
      '<#JoinFilter>'
      'WHERE TLI.Taxon_List_Version_Key IN ('
      '<#ListKeys>'
      ')'
      'AND TLI.Taxon_List_Item_Key = TLI.Preferred_Name'
      'AND ITN.Taxon_List_Version_Key = TLI.Taxon_List_VERSION_Key'
      'AND ITN.Actual_Name = ITN.Preferred_Name'
      'AND TLI.Parent IS NULL'
      'AND TLI.Taxon_List_Version_To IS NULL'
      '<#Filter>'
      '<#OrderBy>'
      '')
    Left = 272
  end
  inherited ppChildLevel: TPageProducer
    HTMLDoc.Strings = (
      
        'SELECT DISTINCT ITN.Taxon_List_Item_Key AS ListKey, ITN.Authorit' +
        'y, ITN.Preferred_Name AS ItemName, '
      
        '  ITN.Preferred_Name_Attribute AS ItemNameAttribute, ITN.Preferr' +
        'ed_Name_Authority AS ItemNameAuthor,'
      
        '  ITN.Preferred_Name_Italic AS ItemNameItalic, TV.Taxon_Key AS T' +
        'axonKey, ITN.Preferred_Name AS DisplayField,'
      
        '  TLI.Taxon_Rank_Key AS RankKey, TLI.Sort_Code AS SortCode, TLI.' +
        'Preferred_Name AS PrefNameKey,'
      
        '  TLI.System_Supplied_Data AS SystemSupplied, TV.Taxon_Version_K' +
        'ey AS TaxonVersionKey,'
      
        '  TV.Validation_Level AS ValidationLevel, ITN.Common_Name AS Com' +
        'monName, '
      
        '   ITN.Common_Name_Attribute AS CommonNameAttribute, ITN.Common_' +
        'Name_Italic AS CommonItalic,'
      '  ITN.Has_Children AS HasChildren'
      'FROM ((Taxon_List_Item TLI'
      
        'INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI' +
        '.Taxon_List_Item_Key)'
      
        'INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_' +
        'Version_Key)'
      '<#JoinFilter>'
      'WHERE TLI.Parent = '#39'<#ParentKey>'#39
      'AND TLI.Taxon_List_Item_Key = TLI.Preferred_Name'
      'AND ITN.Actual_Name = ITN.Preferred_Name'
      'AND TLI.Taxon_List_Version_To IS NULL'
      '<#Filter>')
    Left = 268
  end
  object qryMain: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT TAXON_SOURCES.SOURCE_KEY AS SourceKey'
      ''
      'FROM '
      '(TAXON INNER JOIN '
      
        '(TAXON_VERSION INNER JOIN TAXON_LIST_ITEM ON TAXON_VERSION.TAXON' +
        '_VERSION_KEY = TAXON_LIST_ITEM.TAXON_VERSION_KEY) ON TAXON.TAXON' +
        '_KEY = TAXON_VERSION.TAXON_KEY) INNER JOIN TAXON_SOURCES ON TAXO' +
        'N.TAXON_KEY = TAXON_SOURCES.TAXON_KEY'
      'WHERE TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY = :Key;')
    ParseSQL = True
    Left = 92
    Top = 232
  end
  object qryUKNative: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT '
      'TAXON_VERSION.UK_NATIVE AS UKNative '
      '     '
      
        'FROM TAXON_VERSION INNER JOIN TAXON_LIST_ITEM ON TAXON_VERSION.T' +
        'AXON_VERSION_KEY = TAXON_LIST_ITEM.TAXON_VERSION_KEY'
      'WHERE (((TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY)=:Key))')
    ParseSQL = True
    Left = 92
    Top = 180
  end
  object qryAssociated: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT'
      'TLI.TAXON_LIST_ITEM_KEY, '
      'TV.TAXON_VERSION_KEY, '
      'TBA.BIOTOPE_KEY, '
      'B.ORIGINAL_CODE + '#39', '#39' + B.SHORT_TERM AS Name,  '
      'TBA.SOURCE_KEY AS SourceKey, '
      'TBA.ASSOCIATION AS Data'
      ''
      
        'FROM (TAXON_VERSION AS TV INNER JOIN (BIOTOPE AS B INNER JOIN TA' +
        'XON_BIOTOPE_ASSOCIATION AS TBA ON B.BIOTOPE_KEY = TBA.BIOTOPE_KE' +
        'Y) ON TV.TAXON_VERSION_KEY = TBA.TAXON_VERSION_KEY)'
      
        'INNER JOIN TAXON_LIST_ITEM AS TLI ON TV.TAXON_VERSION_KEY = TLI.' +
        'TAXON_VERSION_KEY'
      'WHERE (((TLI.TAXON_LIST_ITEM_KEY)=:Key));')
    ParseSQL = True
    Left = 168
    Top = 180
  end
  object qrySynonyms: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'PrefNameKey'
        Size = -1
        Value = Null
      end
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'ListVersionKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT TAXON.ITEM_NAME AS Synonym, TAXON.AUTHORITY AS Authority,' +
        ' TAXON.LANGUAGE'
      'FROM TAXON'
      '     INNER JOIN (TAXON_VERSION'
      '     INNER JOIN TAXON_LIST_ITEM '
      
        '     ON TAXON_VERSION.TAXON_VERSION_KEY = TAXON_LIST_ITEM.TAXON_' +
        'VERSION_KEY)'
      '     ON TAXON.TAXON_KEY = TAXON_VERSION.TAXON_KEY'
      'WHERE TAXON_LIST_ITEM.PREFERRED_NAME=:PrefNameKey'
      '      AND TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY<>:Key'
      
        '      AND TAXON_LIST_ITEM.TAXON_LIST_VERSION_KEY=:ListVersionKey' +
        ';')
    ParseSQL = True
    Left = 20
    Top = 180
  end
  object qryFacts: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT'
      'TF.DATA AS Data, '
      'TF.TYPE AS Type, '
      'TF.TITLE AS Title, '
      'TF.FACT_VAGUE_DATE_START, '
      'TF.FACT_VAGUE_DATE_END, '
      'TF.FACT_VAGUE_DATE_TYPE, '
      'TF.SOURCE_KEY AS SourceKey,'
      'TF.TAXON_FACT_KEY As FactKey,'
      'TF.CHANGED_BY AS ChangedBy,'
      'TF.CHANGED_DATE AS ChangedDate,'
      'TF.ENTERED_BY AS EnteredBy,'
      'TF.ENTRY_DATE AS EnteredDate,'
      'TF.SYSTEM_SUPPLIED_DATA AS SystemSuppliedData,'
      'TF.CUSTODIAN'
      ''
      'FROM '
      'TAXON_FACT AS TF'
      ''
      'WHERE TF.TAXON_VERSION_KEY = :Key'
      'ORDER BY TF.TITLE;')
    ParseSQL = True
    Left = 168
    Top = 124
  end
  object qryStatus: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT '
      'TDT.SHORT_NAME AS ShortName, '
      'TD.DATE_FROM AS DateFrom, '
      'TD.DATE_TO AS DateTo, '
      'TD.STATUS_GEOGRAPHIC_AREA AS GeogArea, '
      'TD.STATUS_CONSTRAINT AS StatusConstraint,'
      'TD.DETAIL AS Details,'
      'TD.SOURCE_KEY AS SourceKey,'
      'TD.TAXON_DESIGNATION_KEY AS DesignationKey,'
      'TD.TAXON_DESIGNATION_TYPE_KEY AS DesignationTypeKey,'
      'TLI.TAXON_LIST_ITEM_KEY AS TaxonListItemKey,'
      'TD.CHANGED_BY AS ChangedBy,'
      'TD.CHANGED_DATE AS ChangedDate,'
      'TD.ENTERED_BY AS EnteredBy,'
      'TD.ENTRY_DATE AS EnteredDate,'
      'TD.SYSTEM_SUPPLIED_DATA AS SystemSuppliedData,'
      'TD.CUSTODIAN as CUSTODIAN'
      ''
      'FROM (TAXON_LIST_ITEM AS TLI '
      'INNER JOIN (TAXON_DESIGNATION_TYPE AS TDT '
      'INNER JOIN TAXON_DESIGNATION AS TD '
      
        'ON TDT.TAXON_DESIGNATION_TYPE_KEY = TD.TAXON_DESIGNATION_TYPE_KE' +
        'Y) '
      'ON TLI.TAXON_LIST_ITEM_KEY = TD.TAXON_LIST_ITEM_KEY) '
      ''
      ''
      'WHERE (((TLI.TAXON_LIST_ITEM_KEY)=:Key));')
    ParseSQL = True
    Left = 92
    Top = 124
  end
  object qryCodes: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'TaxonKey'
        Size = -1
        Value = Null
      end
      item
        Name = 'ListKey'
        Size = -1
        Value = Null
      end>
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
      
        'WHERE TAXON.TAXON_KEY=:TaxonKey AND TAXON_LIST_VERSION.TAXON_LIS' +
        'T_KEY=:ListKey;')
    ParseSQL = True
    Left = 20
    Top = 232
  end
  object qryCheckLists: TJNCCQuery
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
        'ame, TLI.Taxon_List_Item_Key, TLV.Taxon_List_Version_Key'
      'FROM Index_Taxon_Synonym ITS'
      
        'INNER JOIN Taxon_List_Item TLI ON ITS.Synonym_List_Item_Key=TLI.' +
        'Taxon_List_Item_Key'
      
        'INNER JOIN Taxon_List_Version TLV ON TLI.Taxon_List_Version_Key=' +
        'TLV.Taxon_List_Version_Key AND TLI.Taxon_List_Version_TO IS NULL'
      'INNER JOIN Taxon_List TL ON TLV.Taxon_List_Key=TL.Taxon_List_Key'
      
        'INNER JOIN Taxon_List_Item TLI2 ON TLI2.Taxon_List_Item_Key=ITS.' +
        'Taxon_List_Item_Key'
      
        'INNER JOIN Taxon_List_Version TLV2 ON TLI2.Taxon_List_Version_Ke' +
        'y=TLV2.Taxon_List_Version_Key AND TLI2.Taxon_List_Version_TO IS ' +
        'NULL'
      
        'INNER JOIN Taxon_List TL2 ON TLV2.Taxon_List_Key=TL2.Taxon_List_' +
        'Key'
      'WHERE ITS.Taxon_List_Item_Key = :ItemKey'
      'AND TL.Taxon_List_Key <> TL2.Taxon_List_Key'
      'AND TLI.Taxon_List_Item_Key = TLI.Preferred_Name')
    ParseSQL = True
    Left = 20
    Top = 288
  end
  object qryListName: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'ItemKey'
        Size = -1
        Value = Null
      end
      item
        Name = 'ListKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT TAXON.ITEM_NAME,'
      '               TAXON_LIST.ITEM_NAME AS TaxonListName,'
      '               TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY '
      ''
      
        'FROM TAXON INNER JOIN (TAXON_VERSION INNER JOIN (TAXON_LIST INNE' +
        'R JOIN (TAXON_LIST_VERSION INNER JOIN TAXON_LIST_ITEM ON TAXON_L' +
        'IST_VERSION.TAXON_LIST_VERSION_KEY = TAXON_LIST_ITEM.TAXON_LIST_' +
        'VERSION_KEY) ON TAXON_LIST.TAXON_LIST_KEY = TAXON_LIST_VERSION.T' +
        'AXON_LIST_KEY) ON TAXON_VERSION.TAXON_VERSION_KEY = TAXON_LIST_I' +
        'TEM.TAXON_VERSION_KEY) ON TAXON.TAXON_KEY = TAXON_VERSION.TAXON_' +
        'KEY'
      ''
      'WHERE TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY=:ItemKey'
      '      AND TAXON_LIST.TAXON_LIST_KEY=:ListKey;'
      ' ')
    ParseSQL = True
    Left = 168
    Top = 232
  end
  object qryGeneral: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT '
      'TLI.TAXON_LIST_ITEM_KEY AS TLI_TAXON_LIST_ITEM_KEY, '
      'TLI.TAXON_VERSION_KEY AS TLI_TAXON_VERSION_KEY, '
      'TLI.PREFERRED_NAME AS TLI_PREFERRED_NAME,'
      'TLI.SORT_CODE AS TLI_SORT_CODE, '
      'TLI.PARENT AS TLI_PARENT, '
      'TLI.TAXON_RANK_KEY AS TLI_TAXON_RANK_KEY, '
      'TLI.ENTERED_BY AS TLI_ENTERED_BY, '
      'TLI.ENTRY_DATE AS TLI_ENTRY_DATE, '
      'TLI.CHANGED_BY AS TLI_CHANGED_BY, '
      'TLI.CHANGED_DATE AS TLI_CHANGED_DATE, '
      'TLI.TAXON_LIST_VERSION_KEY AS TLI_TAXON_LIST_VERSION_KEY, '
      'TLI.SYSTEM_SUPPLIED_DATA  AS TLI_SYSTEM_SUPPLIED_DATA, '
      'TV.TAXON_VERSION_KEY AS TV_TAXON_VERSION_KEY, '
      'TV.TAXON_KEY AS TV_TAXON_KEY, '
      'TV.DATE_FROM AS TV_DATE_FROM, '
      'TV.VALIDATION_LEVEL AS TV_VALIDATION_LEVEL, '
      'TV.UK_NATIVE AS TV_UK_NATIVE, '
      'TV.ENTERED_BY AS TV_ENTERED_BY, '
      'TV.ENTRY_DATE AS TV_ENTRY_DATE, '
      'TV.CHANGED_BY AS TV_CHANGED_BY, '
      'TV.CHANGED_DATE AS TV_CHANGED_DATE, '
      'T.TAXON_KEY AS T_TAXON_KEY, '
      'T.ITEM_NAME AS T_ITEM_NAME, '
      'T.AUTHORITY AS T_AUTHORITY, '
      'T.LANGUAGE AS T_LANGUAGE,'
      'T.ENTERED_BY AS T_ENTERED_BY, '
      'T.ENTRY_DATE AS T_ENTRY_DATE,'
      'T.CHANGED_BY AS T_CHANGED_BY, '
      'T.CHANGED_DATE AS T_CHANGED_DATE'
      ''
      'FROM (Taxon_List_Item AS TLI'
      '     INNER JOIN Taxon_Version AS TV'
      '       ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key)'
      '     INNER JOIN Taxon AS T'
      '       ON T.Taxon_Key = TV.Taxon_Key'
      ''
      'WHERE TLI.Taxon_List_Item_Key = :Key;')
    ParseSQL = True
    Left = 20
    Top = 124
  end
end
