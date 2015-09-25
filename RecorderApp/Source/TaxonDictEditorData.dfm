inherited dmTaxonDictEditor: TdmTaxonDictEditor
  Height = 454
  Width = 346
  inherited qryCheckLists: TJNCCQuery
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
  end
  inherited qryGeneral: TJNCCQuery
    SQL.Strings = (
      'SELECT '
      'TLI.TAXON_LIST_ITEM_KEY AS TLI_TAXON_LIST_ITEM_KEY, '
      'TLI.TAXON_VERSION_KEY AS TLI_TAXON_VERSION_KEY, '
      'TLI.PREFERRED_NAME AS TLI_PREFERRED_NAME,'
      'TLI.SORT_CODE AS TLI_SORT_CODE, '
      'TLI.PARENT AS TLI_PARENT, '
      'TLI.TAXON_RANK_KEY AS TLI_TAXON_RANK_KEY,'
      'TLI.ENTERED_BY AS TLI_ENTERED_BY, '
      'TLI.ENTRY_DATE AS TLI_ENTRY_DATE, '
      'TLI.CHANGED_BY AS TLI_CHANGED_BY, '
      'TLI.CHANGED_DATE AS TLI_CHANGED_DATE, '
      'TLI.TAXON_LIST_VERSION_KEY AS TLI_TAXON_LIST_VERSION_KEY, '
      'TLI.SYSTEM_SUPPLIED_DATA  AS TLI_SYSTEM_SUPPLIED_DATA,'
      'TLI.CUSTODIAN, '
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
      'T.CHANGED_DATE AS T_CHANGED_DATE,'
      'T.TAXON_NAME_TYPE_KEY as T_TAXON_NAME_TYPE_KEY'
      ''
      'FROM (Taxon_List_Item AS TLI'
      '     INNER JOIN Taxon_Version AS TV'
      '       ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key)'
      '     INNER JOIN Taxon AS T'
      '       ON T.Taxon_Key = TV.Taxon_Key'
      ''
      'WHERE TLI.Taxon_List_Item_Key = :Key;')
  end
  object dsRank: TDataSource
    AutoEdit = False
    DataSet = qryRank
    Left = 92
    Top = 348
  end
  object dsGeneral: TDataSource
    AutoEdit = False
    DataSet = qryGeneral
    Left = 20
    Top = 348
  end
  object dsStatusType: TDataSource
    AutoEdit = False
    DataSet = qryStatusType
    Left = 168
    Top = 348
  end
  object qryStatusType: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT TAXON_DESIGNATION_TYPE_KEY, SHORT_NAME '
      ''
      'FROM TAXON_DESIGNATION_TYPE'
      'ORDER BY SHORT_NAME')
    ParseSQL = True
    Left = 168
    Top = 288
  end
  object qryRank: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Sequence_Min'
        Size = -1
        Value = Null
      end
      item
        Name = 'Sequence_Max'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT IMAGE,SEQUENCE,TAXON_RANK_KEY,LONG_NAME FROM TAXON_RANK '
      'WHERE '
      '(SEQUENCE IS NULL'
      'OR (SEQUENCE > :Sequence_Min'
      'AND SEQUENCE < :Sequence_Max))'
      'OR SHORT_NAME LIKE '#39'Unk'#39
      'ORDER BY LONG_NAME')
    ParseSQL = True
    Left = 92
    Top = 288
  end
  object qrySortcode: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 256
    Top = 124
  end
  object qryTaxonNames: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Taxon_User_Name_Key,'
      '       Taxon_List_Item_Key,'
      '       Item_Name,'
      '       Language,'
      '       Preferred,'
      '       Entered_By,'
      '       Entry_Date,'
      '       Changed_By,'
      '       Changed_Date'
      'FROM Taxon_User_Name'
      'WHERE Taxon_List_Item_Key = :Key')
    ParseSQL = True
    Left = 256
    Top = 232
  end
  object qryRankMinChild: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Parent'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT TOP 1 SEQUENCE AS MinSequence'
      'FROM '
      'TAXON_LIST_ITEM AS TLI INNER JOIN '
      'TAXON_RANK AS TR '
      'ON'
      'TLI.TAXON_RANK_KEY = TR.TAXON_RANK_KEY'
      'WHERE TLI.PARENT =:Parent And NOT TR.SHORT_NAME LIKE '#39'Unk'#39
      'ORDER BY SEQUENCE DESC')
    ParseSQL = True
    Left = 256
    Top = 180
  end
end
