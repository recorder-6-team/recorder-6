inherited BaseBiotopeDictData: TBaseBiotopeDictData
  Left = 554
  Top = 212
  Height = 313
  Width = 363
  inherited qryList: TJNCCQuery
    SQL.Strings = (
      
        'SELECT BC.SHORT_NAME AS DisplayField, BC.BIOTOPE_CLASSIFICATION_' +
        'KEY AS KeyField'
      'FROM BIOTOPE_CLASSIFICATION AS BC '
      ''
      'ORDER BY BC.SHORT_NAME;')
  end
  inherited qryTopLevel: TJNCCQuery
    Left = 96
  end
  inherited qryChildLevel: TJNCCQuery
    Left = 172
  end
  inherited qryParent: TJNCCQuery
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Parent '
      'FROM Biotope_List_Item '
      'WHERE Biotope_List_Item_Key = :Key')
  end
  inherited qryPreferredKey: TJNCCQuery
    Left = 96
  end
  inherited ppTopLevel: TPageProducer
    HTMLDoc.Strings = (
      
        'SELECT (B.Original_Code + '#39' - '#39' + B.Short_Term) AS DisplayField,' +
        ' BLI.Biotope_List_Item_Key AS ListKey,'
      
        'B.Full_Term AS LongName, B.Short_Term AS ShortTerm, B.Original_C' +
        'ode AS OriginalCode,'
      
        'BLI.System_Supplied_Data AS SystemSupplied, B.Biotope_Key AS Bio' +
        'topeKey,'
      
        'BLI.Sort_Code AS SortCode, COUNT(BLICount.Biotope_List_Item_Key)' +
        ' AS ChildrenCount'
      'FROM (Biotope AS B'
      
        'INNER JOIN Biotope_List_Item AS BLI ON B.Biotope_Key = BLI.Bioto' +
        'pe_Key)'
      
        'LEFT JOIN Biotope_List_Item AS BLICount ON BLI.Biotope_List_Item' +
        '_Key = BLICount.Parent'
      '<#JoinFilter>'
      'WHERE BLI.BT_CL_Version_Key IN (<#ListKeys>)'
      'AND BLI.BT_CL_Version_To IS NULL'
      'AND B.Term_Current = 1'
      'AND BLI.Parent IS NULL'
      '<#Filter>'
      'GROUP BY'
      
        'BLI.Biotope_List_Item_Key, B.Full_Term, B.Short_Term, B.Original' +
        '_Code, BLI.System_Supplied_Data, B.Biotope_Key, BLI.Sort_Code'
      '<#OrderBy>')
  end
  inherited ppChildLevel: TPageProducer
    HTMLDoc.Strings = (
      
        'SELECT (B.Original_Code + '#39' - '#39' + B.Short_Term) AS DisplayField,' +
        ' BLI.Biotope_List_Item_Key AS ListKey,'
      
        'B.Full_Term AS LongName, B.Short_Term AS ShortTerm, B.Original_C' +
        'ode AS OriginalCode,'
      
        'BLI.System_Supplied_Data AS SystemSupplied, B.Biotope_Key AS Bio' +
        'topeKey,'
      
        'BLI.Sort_Code AS SortCode, COUNT(BLICount.Biotope_List_Item_Key)' +
        ' AS ChildrenCount'
      ''
      'FROM (Biotope AS B'
      
        'INNER JOIN Biotope_List_Item AS BLI ON B.Biotope_Key = BLI.Bioto' +
        'pe_Key)'
      
        'LEFT JOIN Biotope_List_Item AS BLICount ON BLI.Biotope_List_Item' +
        '_Key = BLICount.Parent'
      '<#JoinFilter>'
      'WHERE BLI.Parent = '#39'<#ParentKey>'#39
      'AND B.Term_Current = 1'
      'AND BLI.BT_CL_Version_To Is Null'
      '<#Filter>'
      'GROUP BY'
      
        'BLI.Biotope_List_Item_Key, B.Full_Term, B.Short_Term, B.Original' +
        '_Code, BLI.System_Supplied_Data, B.Biotope_Key, BLI.Sort_Code'
      '<#OrderBy>')
  end
  inherited ppVirtualTopLevel: TPageProducer
    Left = 268
    Top = 144
  end
  inherited ppVirtualChildLevel: TPageProducer
    Left = 272
    Top = 212
  end
  object qryStatus: TJNCCQuery [10]
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT'
      'BDT.SHORT_NAME AS ShortName,'
      'BD.DATE_FROM AS DateFrom,'
      'BD.DATE_TO AS DateTo,'
      'BD.STATUS_GEOGRAPHIC_AREA AS GeogArea,'
      'BD.STATUS_CONSTRAINT AS StatusConstraint,'
      'BD.DESCRIPTION AS Details,'
      'BD.SOURCE_KEY AS SourceKey'
      ''
      'FROM'
      'BIOTOPE_DESIGNATION_TYPE AS BDT'
      'INNER JOIN'
      
        '(BIOTOPE_LIST_ITEM AS BLI INNER JOIN BIOTOPE_DESIGNATION AS BD O' +
        'N BLI.BIOTOPE_LIST_ITEM_KEY = BD.BIOTOPE_LIST_ITEM_KEY)'
      
        'ON BDT.BIOTOPE_DESIGNATION_TYPE_KEY = BD.BIOTOPE_DESIGNATION_TYP' +
        'E_KEY'
      'WHERE BLI.BIOTOPE_LIST_ITEM_KEY=:Key;'
      ' ')
    ParseSQL = True
    Left = 96
    Top = 180
  end
  object qryFacts: TJNCCQuery [11]
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT'
      'BF.BIOTOPE_FACT_KEY AS FactKey,'
      'BF.DATA AS Data,'
      'BF.SOURCE_KEY AS SourceKey,'
      'BF.TITLE AS Title,'
      'BF.TYPE AS Type,'
      'BF.FACT_VAGUE_DATE_START,'
      'BF.FACT_VAGUE_DATE_END,'
      'BF.FACT_VAGUE_DATE_TYPE,'
      'BF.CHANGED_BY AS ChangedBy,'
      'BF.CHANGED_DATE AS ChangedDate,'
      'BF.ENTERED_BY AS EnteredBy,'
      'BF.ENTRY_DATE AS EnteredDate,'
      'BF.SYSTEM_SUPPLIED_DATA AS SystemSuppliedData,'
      'BF.CUSTODIAN'
      ''
      'FROM'
      'BIOTOPE_FACT AS BF'
      ''
      'WHERE BF.BIOTOPE_KEY=:Key'
      'ORDER BY BF.TITLE;'
      ' ')
    ParseSQL = True
    Left = 172
    Top = 124
  end
  object qryAssociated: TJNCCQuery [12]
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT '
      'TAXON.ITEM_NAME+ISNULL('#39' '#39' + TAXON.AUTHORITY,'#39#39')  AS Name, '
      'TAXON_BIOTOPE_ASSOCIATION.ASSOCIATION AS Data, '
      'TAXON_BIOTOPE_ASSOCIATION.SOURCE_KEY AS SourceKey'
      ''
      'FROM '
      
        'TAXON INNER JOIN (TAXON_VERSION INNER JOIN ((BIOTOPE INNER JOIN ' +
        'BIOTOPE_LIST_ITEM ON BIOTOPE.BIOTOPE_KEY = BIOTOPE_LIST_ITEM.BIO' +
        'TOPE_KEY) INNER JOIN TAXON_BIOTOPE_ASSOCIATION ON BIOTOPE.BIOTOP' +
        'E_KEY = TAXON_BIOTOPE_ASSOCIATION.BIOTOPE_KEY) ON TAXON_VERSION.' +
        'TAXON_VERSION_KEY = TAXON_BIOTOPE_ASSOCIATION.TAXON_VERSION_KEY)' +
        ' ON TAXON.TAXON_KEY = TAXON_VERSION.TAXON_KEY'
      'WHERE BIOTOPE_LIST_ITEM.BIOTOPE_LIST_ITEM_KEY = :Key;')
    ParseSQL = False
    Left = 96
    Top = 124
  end
  object qryMain: TJNCCQuery [13]
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT DISTINCT BIOTOPE_SOURCES.SOURCE_KEY AS SourceKey'
      'FROM '
      
        '(BIOTOPE INNER JOIN BIOTOPE_LIST_ITEM ON BIOTOPE.BIOTOPE_KEY = B' +
        'IOTOPE_LIST_ITEM.BIOTOPE_KEY) '
      
        'INNER JOIN BIOTOPE_SOURCES ON BIOTOPE.BIOTOPE_KEY = BIOTOPE_SOUR' +
        'CES.BIOTOPE_KEY'
      ''
      'WHERE'
      ''
      'BIOTOPE_LIST_ITEM.BIOTOPE_LIST_ITEM_KEY = :Key;')
    ParseSQL = True
    Left = 20
    Top = 180
  end
  object qryGeneral: TJNCCQuery [14]
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT '
      'BLI.BIOTOPE_LIST_ITEM_KEY AS BLI_BIOTOPE_LIST_ITEM_KEY, '
      'BLI.BIOTOPE_KEY AS BLI_BIOTOPE_KEY, '
      'BLI.PARENT AS BLI_PARENT,'
      'BLI.SORT_CODE AS BLI_SORT_CODE,'
      'BLI.ENTERED_BY AS BLI_ENTERED_BY,'
      'BLI.ENTRY_DATE AS BLI_ENTRY_DATE,'
      'BLI.CHANGED_BY AS BLI_CHANGED_BY,'
      'BLI.CHANGED_DATE AS BLI_CHANGED_DATE,'
      'BLI.SYSTEM_SUPPLIED_DATA AS BLI_SYSTEM_SUPPLIED_DATA,'
      'BLI.BT_CL_VERSION_KEY AS BLI_BT_CL_VERSION_KEY,'
      'B.BIOTOPE_KEY AS B_BIOTOPE_KEY, '
      'B.SHORT_TERM AS B_SHORT_TERM,'
      'B.FULL_TERM AS B_FULL_TERM, '
      'B.ORIGINAL_CODE AS B_ORIGINAL_CODE,'
      'B.ENTERED_BY AS B_ENTERED_BY,'
      'B.ENTRY_DATE AS B_ENTRY_DATE,'
      'B.CHANGED_BY AS B_CHANGED_BY,'
      'B.CHANGED_DATE AS B_CHANGED_DATE,'
      'B.SYSTEM_SUPPLIED_DATA AS B_SYSTEM_SUPPLIED_DATA,'
      'B.TERM_CURRENT AS B_TERM_CURRENT'
      ''
      
        'FROM BIOTOPE_LIST_ITEM AS BLI INNER JOIN BIOTOPE AS B ON BLI.BIO' +
        'TOPE_KEY = B.BIOTOPE_KEY'
      'WHERE (((BLI.BIOTOPE_LIST_ITEM_KEY)=:Key));')
    ParseSQL = True
    Left = 20
    Top = 124
  end
end
