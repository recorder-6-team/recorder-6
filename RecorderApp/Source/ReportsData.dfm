inherited dmReports: TdmReports
  OldCreateOrder = True
  Left = 362
  Top = 176
  Height = 419
  Width = 472
  object qryLocations: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 32
    Top = 20
  end
  object qryOtherNames: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Item_Name'
      'FROM Location_Name'
      'WHERE Location_Key = :Key'
      'AND Preferred = 0')
    ParseSQL = True
    Left = 124
    Top = 8
  end
  object qryAdminAreas: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT AA.Item_Name + '#39'('#39' + AT.Short_Name + '#39')'#39' AS Admin_Area_Na' +
        'me'
      'FROM (Admin_Type AT '
      '    INNER JOIN Admin_Area AA '
      '        ON AT.Admin_Type_Key = AA.Admin_Type_Key) '
      '    INNER JOIN Location_Admin_Areas LAA '
      '        ON AA.Admin_Area_Key = LAA.Admin_Area_Key'
      'WHERE LAA.Location_Key = :Key')
    ParseSQL = True
    Left = 200
    Top = 20
  end
  object qryMeasurements: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT LD.ACCURACY, MT.SHORT_NAME AS MType, MQ.SHORT_NAME AS Qua' +
        'lifier, LD.DATA, MU.SHORT_NAME AS MUnit'
      'FROM (((Location_Data AS LD'
      
        '     INNER JOIN Measurement_Unit AS MU ON MU.MEASUREMENT_UNIT_KE' +
        'Y = LD.MEASUREMENT_UNIT_KEY)'
      
        '     INNER JOIN Measurement_Type AS MT ON MT.MEASUREMENT_TYPE_KE' +
        'Y = MU.MEASUREMENT_TYPE_KEY)'
      
        '     INNER JOIN MEASUREMENT_QUALIFIER AS MQ ON LD.MEASUREMENT_QU' +
        'ALIFIER_KEY = MQ.MEASUREMENT_QUALIFIER_KEY)'
      'WHERE LD.Location_Key = :Key'
      ' '
      ' ')
    ParseSQL = True
    Left = 368
    Top = 20
  end
  object qryDesignations: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT LD.Authority, LD.Date_From, LD.Date_To, SS.Short_Name'
      'FROM Location_Designation LD, Site_Status SS'
      'WHERE LD.Site_Status_Key = SS.Site_Status_Key'
      'AND LD.Location_Key = :Key')
    ParseSQL = True
    Left = 284
    Top = 20
  end
  object qryBiotopes: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      
        'SELECT B.Original_Code, B.Short_Term, BO.Biotope_Occurrence_Key,' +
        ' BO.Comment AS [GeneralComment],'
      
        '              BD.Determiner, BD.Comment AS [DeterminationComment' +
        '], '
      
        '              BD.Vague_Date_Start, BD.Vague_Date_End, BD.Vague_D' +
        'ate_Type'
      'FROM Biotope_Occurrence AS BO INNER JOIN'
      '         ((Biotope AS B INNER JOIN'
      '             Biotope_List_Item AS BLI ON'
      '         B.Biotope_Key = BLI.Biotope_Key) INNER JOIN'
      '             Biotope_Determination AS BD ON'
      
        '         BLI.Biotope_List_Item_Key = BD.Biotope_List_Item_Key) O' +
        'N'
      '     BO.Biotope_Occurrence_Key = BD.Biotope_Occurrence_Key'
      'WHERE BD.Preferred = 1 '
      'AND BO.Sample_Key IN'
      '('#39' '#39')')
    ParseSQL = True
    Left = 120
    Top = 76
  end
  object qryGetSampleKeys: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Sample_Key'
      'FROM Sample'
      'WHERE Location_Key = :Key'
      '')
    ParseSQL = True
    Left = 32
    Top = 76
  end
  object qryTaxa: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT [TO].Taxon_Occurrence_Key, RT.Short_Name, '
      
        '       TLI.Taxon_List_Item_Key, [TO].Comment AS [GeneralComment]' +
        ','
      '       TD.Determiner, TD.Comment AS [DeterminationComment],'
      
        '       TD.Vague_Date_Start, TD.Vague_Date_End, TD.Vague_Date_Typ' +
        'e'
      'FROM Record_Type AS RT INNER JOIN'
      '            (Taxon_Occurrence As [TO] INNER JOIN'
      '                 (Taxon_List_Item As TLI INNER JOIN'
      '                     Taxon_Determination As TD ON'
      
        '                 TLI.Taxon_List_Item_Key = TD.Taxon_List_Item_Ke' +
        'y) ON'
      
        '             [TO].Taxon_Occurrence_Key = TD.Taxon_Occurrence_Key' +
        ') ON'
      '            RT.Record_Type_Key = [TO].Record_Type_Key'
      'WHERE TD.Preferred = 1'
      'AND [TO].Sample_Key IN'
      '('#39' '#39')')
    ParseSQL = True
    Left = 200
    Top = 76
  end
  object qryTxCommon: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KEY'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Common_Name AS Item_Name, Authority, Taxon_List_Item_Key'
      'FROM Index_Taxon_Name'
      'WHERE Taxon_List_Item_Key = :KEY'
      ' '
      ' '
      ' ')
    ParseSQL = True
    Left = 368
    Top = 76
  end
  object qryTxSci: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT Preferred_Name AS Item_Name, Authority, Taxon_List_Item_K' +
        'ey'
      'FROM Index_Taxon_Name'
      'WHERE Taxon_List_Item_Key = :Key')
    ParseSQL = True
    Left = 284
    Top = 76
  end
  object qryOccurrences: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 32
    Top = 132
  end
  object qryTaxonSynonyms: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT T.Item_Name, T.Authority'
      'FROM Taxon AS T INNER JOIN '
      '        (Taxon_Version AS TV INNER JOIN '
      '            Taxon_List_Item AS TLI ON '
      '        TV.Taxon_Version_Key = TLI.Taxon_Version_Key) ON '
      '    T.Taxon_Key = TV.Taxon_Key'
      'WHERE TLI.Taxon_List_Item_Key <> TLI.Preferred_Name'
      'AND TLI.Preferred_Name IN '
      '()')
    ParseSQL = True
    Left = 124
    Top = 132
  end
  object qryTxDes: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT Short_Name, Date_From, Date_To, Detail AS [Details]'
      'FROM Taxon_Designation_Type AS TDT INNER JOIN '
      '        Taxon_Designation AS TD ON '
      
        '    TDT.Taxon_Designation_Type_Key = TD.Taxon_Designation_Type_K' +
        'ey '
      'WHERE Taxon_List_Item_Key IN '
      '    (SELECT Taxon_List_Item_Key '
      '    FROM Index_Taxon_Synonym'
      '    WHERE Synonym_List_Item_Key IN '
      ')')
    ParseSQL = True
    Left = 200
    Top = 132
  end
  object qryTxFacts: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT Data'
      'FROM Taxon_Fact AS TF, Taxon_List_Item AS TLI'
      'WHERE TF.Taxon_Version_Key = TLI.Taxon_Version_Key '
      'AND Taxon_List_Item_Key IN '
      '    (SELECT Taxon_List_Item_Key '
      '    FROM Index_Taxon_Synonym'
      '    WHERE Synonym_List_Item_Key IN'
      ')')
    ParseSQL = True
    Left = 284
    Top = 132
  end
  object qryTxOccs: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      
        'SELECT L.Location_Key, L.Spatial_Ref, L.Lat, L.Long, XD.Determin' +
        'er, XD.Comment AS [DeterminationComment],'
      '    XD.Vague_Date_Start, XD.Vague_Date_End, XD.Vague_Date_Type,'
      
        '    XO.Taxon_Occurrence_Key, XO.Comment AS [GeneralComment], S.S' +
        'ample_Key, RT.Short_Name'
      
        'FROM ((((((TAXON_LIST_ITEM AS XLI INNER JOIN TAXON_DETERMINATION' +
        ' AS XD'
      
        'ON XLI.TAXON_LIST_ITEM_KEY in (Select Taxon_List_Item_Key from I' +
        'ndex_Taxon_Synonym where Synonym_List_Item_Key = XD.TAXON_LIST_I' +
        'TEM_KEY)'
      
        'INNER JOIN TAXON_OCCURRENCE AS XO ON XD.TAXON_OCCURRENCE_KEY = X' +
        'O.TAXON_OCCURRENCE_KEY)'
      'INNER JOIN SAMPLE AS S ON S.SAMPLE_KEY = XO.SAMPLE_KEY)'
      
        'INNER JOIN SURVEY_EVENT AS SE ON SE.SURVEY_EVENT_KEY = S.SURVEY_' +
        'EVENT_KEY)'
      'INNER JOIN LOCATION AS L ON S.LOCATION_KEY = L.LOCATION_KEY)'
      
        'LEFT JOIN TAXON_OCCURRENCE_SOURCES XOS ON XOS.TAXON_OCCURRENCE_K' +
        'EY = XO.TAXON_OCCURRENCE_KEY)'
      'WHERE XD.Preferred = 1'
      '<#taxon_filter>'
      '<#taxon_confidential>'
      '<#taxon_checked>'
      '<#taxon_verified>'
      '<#taxon_zero_abundance>)'
      'ORDER BY 1'
      
        'ORDER BY L.Location_Key, XD.Vague_Date_Start, XO.Taxon_Occurrenc' +
        'e_Key'
      ' '
      ' ')
    ParseSQL = True
    Left = 368
    Top = 132
  end
  object qryResults: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 32
    Top = 188
  end
  object qryLocationsByKey: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      
        'SELECT   L.Location_Key, LN.Item_Name, L.Description, L.Spatial_' +
        'Ref + '#39' ('#39' + L.Spatial_Ref_Qualifier + '#39')'#39' AS GridRef'
      'FROM Location L, Location_Name LN '
      'WHERE Preferred = 1 '
      'AND LN.Location_Key = L.Location_Key AND '
      'L.Location_Key IN ('#39' '#39')')
    ParseSQL = True
    Left = 116
    Top = 188
  end
  object qryBioFacts: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Data, BLI.Biotope_Key'
      'FROM Biotope_Fact AS BF, Biotope_List_Item AS BLI'
      'WHERE BF.Biotope_Key = BLI.Biotope_Key '
      'AND Biotope_List_Item_Key =:Key'
      '')
    ParseSQL = True
    Left = 284
    Top = 192
  end
  object qryBioOccs: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT [TO].Biotope_Occurrence_Key, L.Location_Key, L.Spatial_Re' +
        'f, L.Lat, L.Long,'
      '    TD.Determiner, TD.Comment AS [DeterminationComment],'
      '    TD.Vague_Date_Start, TD.Vague_Date_End, TD.Vague_Date_Type,'
      '    [TO].Comment AS [GeneralComment], S.Sample_Key'
      
        'FROM Biotope_Determination AS TD, Biotope_Occurrence AS [TO], Sa' +
        'mple AS S,'
      '    Location AS L'
      'WHERE TD.Biotope_List_Item_Key = :Key'
      'AND [TO].Biotope_Occurrence_Key = TD.Biotope_Occurrence_Key'
      'AND S.Sample_Key = [TO].Sample_Key'
      'AND L.Location_Key = S.Location_Key'
      
        'ORDER BY L.Location_Key, TD.Vague_Date_Start, [TO].Biotope_Occur' +
        'rence_Key'
      ' ')
    ParseSQL = True
    Left = 368
    Top = 192
  end
  object ppListItems: TPageProducer
    HTMLDoc.Strings = (
      
        'SELECT distinct ITN.Preferred_Name as ItemName, ITN.Authority, T' +
        'LI.TAXON_LIST_ITEM_KEY as ListItemKey,'
      '       TLI.SORT_CODE, '#39'Taxon'#39' as RecordType'
      
        'FROM (((((TAXON_LIST_ITEM AS TLI INNER JOIN TAXON_DETERMINATION ' +
        'AS XD'
      'ON TLI.TAXON_LIST_ITEM_KEY = XD.TAXON_LIST_ITEM_KEY)'
      
        'INNER JOIN TAXON_OCCURRENCE AS [TO] ON XD.TAXON_OCCURRENCE_KEY =' +
        ' [TO].TAXON_OCCURRENCE_KEY)'
      'INNER JOIN SAMPLE AS S ON S.SAMPLE_KEY = [TO].SAMPLE_KEY)'
      
        'INNER JOIN SURVEY_EVENT AS SE ON SE.SURVEY_EVENT_KEY = S.SURVEY_' +
        'EVENT_KEY)'
      
        'LEFT JOIN USER_SURVEY_RESTRICTION USR ON USR.SURVEY_KEY = SE.SUR' +
        'VEY_KEY AND USR.NAME_KEY = @UserID'
      
        'INNER JOIN INDEX_TAXON_NAME AS ITN ON ITN.TAXON_LIST_ITEM_KEY = ' +
        'XD.TAXON_LIST_ITEM_KEY)'
      
        'WHERE TLI.TAXON_List_Item_Key in (Select Taxon_List_Item_Key fro' +
        'm Index_Taxon_Synonym where Synonym_List_Item_Key = TLI.Preferre' +
        'd_Name)'
      'AND XD.PREFERRED=1'
      'AND USR.NAME_KEY IS NULL'
      'AND <#taxon_filter>'
      '<#confidential>'
      '<#checked>'
      '<#verified>'
      '<#zero_abundance>'
      'UNION'
      
        'SELECT IsNull(X.Original_Code, '#39#39') + case when X.Original_Code +' +
        ' X.Short_Term is null then '#39#39' else '#39' '#39' end  + '
      
        '            IsNull(X.Short_Term, '#39#39') as [ItemName], '#39#39' as Author' +
        'ity,'
      
        '       BLI.Biotope_List_Item_Key AS ListItemKey, BLI.SORT_CODE, ' +
        #39'Biotope'#39' as RecordType'
      'FROM ((((((BIOTOPE_LIST_ITEM AS BLI'
      
        'INNER JOIN BIOTOPE_DETERMINATION AS XD ON BLI.BIOTOPE_LIST_ITEM_' +
        'KEY = XD.BIOTOPE_LIST_ITEM_KEY)'
      'INNER JOIN BIOTOPE AS X ON BLI.BIOTOPE_KEY = X.BIOTOPE_KEY)'
      
        'INNER JOIN BIOTOPE_OCCURRENCE AS BO ON XD.BIOTOPE_OCCURRENCE_KEY' +
        ' = BO.BIOTOPE_OCCURRENCE_KEY)'
      'INNER JOIN SAMPLE AS S ON S.SAMPLE_KEY = BO.SAMPLE_KEY)'
      
        'INNER JOIN SURVEY_EVENT AS SE ON SE.SURVEY_EVENT_KEY = S.SURVEY_' +
        'EVENT_KEY'
      
        'LEFT JOIN USER_SURVEY_RESTRICTION USR ON USR.SURVEY_KEY = SE.SUR' +
        'VEY_KEY AND USR.NAME_KEY = @UserID))'
      'WHERE <#biotope_filter>'
      'AND XD.PREFERRED=1'
      'AND USR.NAME_KEY IS NULL'
      '<#checked>'
      '<#verified>'
      'ORDER BY RecordType desc, SORT_CODE'
      ' ')
    OnHTMLTag = ppListItemsHTMLTag
    Left = 32
    Top = 260
  end
  object ppLocationsForTaxon: TPageProducer
    HTMLDoc.Strings = (
      'SELECT S.Location_Key,'
      
        '  CASE WHEN S.Location_Key IS NULL AND (S.Spatial_Ref IS NULL OR' +
        ' S.Spatial_Ref = '#39#39')'
      '      THEN S.Location_Name'
      '      ELSE S.Spatial_Ref'
      '  END AS Spatial_Ref,'
      
        '  S.Lat, S.Long, XD.Determiner, XD.Comment AS [DeterminationComm' +
        'ent],'
      '  XD.Vague_Date_Start, XD.Vague_Date_End, XD.Vague_Date_Type,'
      
        '  [TO].Taxon_Occurrence_Key, [TO].Comment AS [GeneralComment], S' +
        '.Sample_Key, RT.Short_Name'
      'FROM ((((((((TAXON_LIST_ITEM AS TLI'
      
        'INNER JOIN TAXON_DETERMINATION AS XD ON TLI.TAXON_LIST_ITEM_KEY ' +
        'in (Select Taxon_List_Item_Key from Index_Taxon_Synonym where Sy' +
        'nonym_List_Item_Key = XD.TAXON_LIST_ITEM_KEY))'
      
        'INNER JOIN TAXON_OCCURRENCE AS [TO] ON XD.TAXON_OCCURRENCE_KEY =' +
        ' [TO].TAXON_OCCURRENCE_KEY)'
      'INNER JOIN SAMPLE AS S ON S.SAMPLE_KEY = [TO].SAMPLE_KEY)'
      
        'INNER JOIN SURVEY_EVENT AS SE ON SE.SURVEY_EVENT_KEY = S.SURVEY_' +
        'EVENT_KEY)'
      'LEFT JOIN LOCATION AS L ON S.LOCATION_KEY = L.LOCATION_KEY)'
      
        'LEFT JOIN LOCATION_NAME AS LN ON LN.LOCATION_KEY = L.LOCATION_KE' +
        'Y)'
      
        'INNER JOIN RECORD_TYPE AS RT ON [TO].RECORD_TYPE_KEY = RT.RECORD' +
        '_TYPE_KEY))'
      'WHERE TLI.Taxon_List_Item_Key IN (<#current_taxon>)'
      'AND XD.Preferred = 1'
      'AND ((LN.Preferred = 1) OR (S.Location_Key IS NULL))'
      'AND <#taxon_filter>'
      '<#confidential>'
      '<#checked>'
      '<#verified>'
      '<#zero_abundance>'
      
        'ORDER BY LN.ITEM_NAME, XD.Vague_Date_Start, [TO].Taxon_Occurrenc' +
        'e_Key')
    OnHTMLTag = ppListItemsHTMLTag
    Left = 136
    Top = 260
  end
  object ppLocationsForBiotope: TPageProducer
    HTMLDoc.Strings = (
      'SELECT BO.Biotope_Occurrence_Key, S.Location_Key,'
      
        '  CASE WHEN S.Location_Key IS NULL AND (S.Spatial_Ref IS NULL OR' +
        ' S.Spatial_Ref = '#39#39')'
      '      THEN S.Location_Name'
      '      ELSE S.Spatial_Ref'
      '  END AS Spatial_Ref,'
      '  S.Lat, S.Long,'
      '  XD.Determiner, XD.Comment AS [DeterminationComment],'
      '  XD.Vague_Date_Start, XD.Vague_Date_End, XD.Vague_Date_Type,'
      '  BO.Comment AS [GeneralComment], S.Sample_Key'
      'FROM (((((((BIOTOPE_LIST_ITEM AS BLI'
      
        'INNER JOIN BIOTOPE_DETERMINATION AS XD ON BLI.BIOTOPE_LIST_ITEM_' +
        'KEY = XD.BIOTOPE_LIST_ITEM_KEY)'
      
        'INNER JOIN BIOTOPE_OCCURRENCE AS BO ON XD.BIOTOPE_OCCURRENCE_KEY' +
        ' = BO.BIOTOPE_OCCURRENCE_KEY)'
      'INNER JOIN SAMPLE AS S ON S.SAMPLE_KEY = BO.SAMPLE_KEY)'
      
        'INNER JOIN SURVEY_EVENT AS SE ON SE.SURVEY_EVENT_KEY = S.SURVEY_' +
        'EVENT_KEY)'
      'LEFT JOIN LOCATION AS L ON S.LOCATION_KEY = L.LOCATION_KEY)'
      
        'LEFT JOIN LOCATION_NAME AS LN ON LN.LOCATION_KEY = L.LOCATION_KE' +
        'Y))'
      'WHERE BLI.BIOTOPE_List_Item_Key in (<#current_biotope>)'
      'AND ((LN.Preferred = 1) OR (S.Location_Key IS NULL))'
      'AND <#biotope_filter>'
      '<#checked>'
      '<#verified>'
      
        'ORDER BY LN.ITEM_NAME, XD.Vague_Date_Start, BO.Biotope_Occurrenc' +
        'e_Key'
      ' ')
    OnHTMLTag = ppListItemsHTMLTag
    Left = 136
    Top = 316
  end
  object ppLocations: TPageProducer
    HTMLDoc.Strings = (
      
        'SELECT DISTINCT L.Location_Key, LN.Item_Name, L.Spatial_Ref + '#39' ' +
        '('#39' + L.Spatial_Ref_Qualifier + '#39')'#39' AS GridRef'
      'FROM SAMPLE AS S'
      
        'LEFT JOIN TAXON_OCCURRENCE AS [TO] ON [TO].SAMPLE_KEY = S.SAMPLE' +
        '_KEY'
      
        'LEFT JOIN TAXON_DETERMINATION AS TD ON TD.TAXON_OCCURRENCE_KEY =' +
        ' [TO].TAXON_OCCURRENCE_KEY'
      
        'LEFT JOIN Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=TD.' +
        'Taxon_List_Item_Key'
      
        'LEFT JOIN TAXON_LIST_ITEM AS TLI ON ITS.SYNONYM_LIST_ITEM_KEY = ' +
        'TLI.TAXON_LIST_ITEM_KEY'
      
        'LEFT JOIN TAXON_OCCURRENCE_SOURCES TOS ON TOS.TAXON_OCCURRENCE_K' +
        'EY = [TO].TAXON_OCCURRENCE_KEY'
      
        'LEFT JOIN BIOTOPE_OCCURRENCE AS BO ON BO.SAMPLE_KEY = S.SAMPLE_K' +
        'EY'
      
        'LEFT JOIN BIOTOPE_DETERMINATION AS BD ON BD.BIOTOPE_OCCURRENCE_K' +
        'EY = BO.BIOTOPE_OCCURRENCE_KEY'
      
        'LEFT JOIN BIOTOPE_LIST_ITEM AS BLI ON BD.BIOTOPE_LIST_ITEM_KEY =' +
        ' BLI.BIOTOPE_LIST_ITEM_KEY'
      
        'LEFT JOIN BIOTOPE_OCCURRENCE_SOURCES BOS ON BOS.BIOTOPE_OCCURREN' +
        'CE_KEY = BO.BIOTOPE_OCCURRENCE_KEY'
      
        'INNER JOIN SURVEY_EVENT AS SE ON SE.SURVEY_EVENT_KEY = S.SURVEY_' +
        'EVENT_KEY'
      
        'LEFT JOIN USER_SURVEY_RESTRICTION USR ON SE.SURVEY_KEY = USR.SUR' +
        'VEY_KEY AND USR.NAME_KEY = @UserID'
      'INNER JOIN LOCATION AS L ON L.LOCATION_KEY = S.LOCATION_KEY'
      
        'INNER JOIN LOCATION_NAME AS LN ON LN.LOCATION_KEY = L.LOCATION_K' +
        'EY'
      
        'WHERE (TLI.Taxon_List_Item_Key = TLI.Preferred_Name or TLI.Taxon' +
        '_List_Item_Key  is null)'
      'AND LN.PREFERRED=1'
      'AND (BD.PREFERRED = 1 or TD.PREFERRED = 1)'
      'AND USR.NAME_KEY IS NULL'
      'AND <#filter>'
      '<#confidential>'
      '<#checked>'
      '<#verified>'
      '<#zero_abundance>'
      'ORDER BY 2')
    OnHTMLTag = ppListItemsHTMLTag
    Left = 260
    Top = 260
  end
  object ppTxOccsForLocation: TPageProducer
    HTMLDoc.Strings = (
      'SELECT [TO].Taxon_Occurrence_Key, RT.Short_Name,'
      
        '       TLI.Taxon_List_Item_Key, TLI.SORT_CODE, [TO].Comment AS [' +
        'GeneralComment],'
      '       XD.Determiner, XD.Comment AS [DeterminationComment],'
      
        '       XD.Vague_Date_Start, XD.Vague_Date_End, XD.Vague_Date_Typ' +
        'e'
      'FROM (((((((TAXON_LIST_ITEM AS TLI'
      
        'INNER JOIN TAXON_DETERMINATION AS XD ON TLI.TAXON_LIST_ITEM_KEY ' +
        ' =XD.TAXON_LIST_ITEM_KEY)'
      
        'INNER JOIN TAXON_OCCURRENCE AS [TO] ON XD.TAXON_OCCURRENCE_KEY =' +
        ' [TO].TAXON_OCCURRENCE_KEY)'
      'INNER JOIN SAMPLE AS S ON S.SAMPLE_KEY = [TO].SAMPLE_KEY)'
      
        'INNER JOIN SURVEY_EVENT AS SE ON SE.SURVEY_EVENT_KEY = S.SURVEY_' +
        'EVENT_KEY)'
      'LEFT JOIN LOCATION AS L ON S.LOCATION_KEY = L.LOCATION_KEY)'
      
        'INNER JOIN RECORD_TYPE AS RT ON [TO].RECORD_TYPE_KEY = RT.RECORD' +
        '_TYPE_KEY))'
      'WHERE '
      ' L.LOCATION_KEY in (<#current_location>)'
      'AND XD.PREFERRED = 1'
      'AND <#taxon_filter>'
      '<#confidential>'
      '<#checked>'
      '<#verified>'
      '<#zero_abundance>'
      
        'ORDER BY L.Location_Key, TLI.SORT_CODE, XD.Vague_Date_Start, [TO' +
        '].Taxon_Occurrence_Key')
    OnHTMLTag = ppListItemsHTMLTag
    Left = 368
    Top = 260
  end
  object ppBtOccsForLocation: TPageProducer
    HTMLDoc.Strings = (
      
        'SELECT BO.Biotope_Occurrence_Key, BT.Short_Term, BT.Original_Cod' +
        'e,'
      
        '       BLI.Biotope_List_Item_Key, BLI.SORT_CODE, BO.Comment AS [' +
        'GeneralComment],'
      '       XD.Determiner, XD.Comment AS [DeterminationComment],'
      
        '       XD.Vague_Date_Start, XD.Vague_Date_End, XD.Vague_Date_Typ' +
        'e'
      'FROM (((((((BIOTOPE_LIST_ITEM AS BLI'
      'INNER JOIN BIOTOPE AS BT ON BT.BIOTOPE_KEY = BLI.BIOTOPE_KEY)'
      
        'INNER JOIN BIOTOPE_DETERMINATION AS XD ON BLI.BIOTOPE_LIST_ITEM_' +
        'KEY = XD.BIOTOPE_LIST_ITEM_KEY)'
      
        'INNER JOIN BIOTOPE_OCCURRENCE AS BO ON XD.BIOTOPE_OCCURRENCE_KEY' +
        ' = BO.BIOTOPE_OCCURRENCE_KEY)'
      'INNER JOIN SAMPLE AS S ON S.SAMPLE_KEY = BO.SAMPLE_KEY)'
      
        'INNER JOIN SURVEY_EVENT AS SE ON SE.SURVEY_EVENT_KEY = S.SURVEY_' +
        'EVENT_KEY)'
      'LEFT JOIN LOCATION AS L ON S.LOCATION_KEY = L.LOCATION_KEY))'
      'WHERE L.LOCATION_KEY in (<#current_location>)'
      'AND XD.PREFERRED = 1'
      'AND <#biotope_filter>'
      '<#checked>'
      '<#verified>'
      
        'ORDER BY L.Location_Key, BLI.SORT_CODE, XD.Vague_Date_Start, BO.' +
        'Biotope_Occurrence_Key ')
    OnHTMLTag = ppListItemsHTMLTag
    Left = 368
    Top = 312
  end
end
