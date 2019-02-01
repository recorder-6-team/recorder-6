//==============================================================================
//  Unit:        SQLConstants
//
//  Implements:  Nothing
//
//  Description: SQL String constants for Recorder 2000.
//
//
//  Author:      John van Breda
//  Created:     11th March 2002
//
//  Changes:     
//
//  Last Revision Details:
//    $Revision: 58 $
//    $Date: 1/05/09 15:47 $
//    $Author: Pauldavies $
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================
unit SQLConstants;


interface

uses
  ADODB, DataClasses;

const
{$IFDEF USE_TITAN}
  ST_LIKE_PATTERN = 'LIKE ''%s*''';
{$ELSE}
  ST_LIKE_PATTERN = 'LIKE ''%s%%''';
{$ENDIF}
  SQL_TAXON_FROM_QUERY_CURR_LIST =
          'SELECT DISTINCT ITN.Taxon_List_Item_Key, ITN.Abbreviation, ' +
          '       ITN.Authority, '''' AS ListName, ' +
          '       dbo.ufn_FormattedSpeciesName( ITN.Actual_Name, ' +
          '                                     ITN.Authority,  ' +
          '                                     ITN.Preferred_Name_Authority, ' +
          '                                     ITN.Preferred_Name, ' +
          '                                     ITN.Actual_Name_Italic, ' +
          '                                     ITN.Preferred_Name_Italic,  ' +
          '                                     ITN.Actual_Name_Attribute, ' +
          '                                     TR.Short_Name, ' +
          '                                     ITN.Can_Expand, ' +
          '''%s'') AS ItemName ' +
          'FROM Taxon_List_Version TLV ' +
          'INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Version_Key = TLV.Taxon_List_Version_Key '+
          'INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key ' +
          ' AND ITN.DEPRECATED = 0 ' +
          'INNER JOIN Taxon_Rank TR ON TR.Taxon_Rank_Key = TLI.Taxon_Rank_Key ' +
          'WHERE TLV.Taxon_List_Key = ''%s''' +
          'AND (ITN.Actual_Name ' + ST_LIKE_PATTERN +
          ' OR ITN.Common_Name ' + ST_LIKE_PATTERN  +
          ' OR ITN.Abbreviation ' + ST_LIKE_PATTERN +
          ' OR ITN.Authority ' + ST_LIKE_PATTERN + ')';
      SQL_TAXON_FROM_QUERY_VIRTUAL =
          'SELECT DISTINCT ITN.Taxon_List_Item_Key, ITN.Abbreviation, ' +
          '       ITN.Authority, '''' AS ListName, ' +
          '       dbo.ufn_FormattedSpeciesName( ITN2.Actual_Name, ' +
          '                                     ITN2.Authority,  ' +
          '                                     ITN.Preferred_Name_Authority, ' +
          '                                     ITN.Preferred_Name, ' +
          '                                     ITN.Actual_Name_Italic, ' +
          '                                     ITN.Preferred_Name_Italic,  ' +
          '                                     ITN2.Actual_Name_Attribute, ' +
          '                                     TR.Short_Name, ' +
          '                                     ITN.Can_Expand, ' +
          '''%s'') AS ItemName ' +
          'FROM %s TLI INNER JOIN ' +
          'Index_Taxon_Name ITN ON ITN.taxon_List_Item_Key ' +
          '= TLI.Taxon_List_Item_Key  ' +
          'INNER JOIN Taxon_Rank TR ON TR.Taxon_Rank_Key = TLI.Taxon_Rank_Key ' +
          'INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key = ' +
          'ITN.Taxon_List_Item_Key ' +
          'WHERE ITN2.Actual_Name ' + ST_LIKE_PATTERN +
          ' OR ITN2.Common_Name ' + ST_LIKE_PATTERN  +
          ' OR ITN2.Abbreviation ' + ST_LIKE_PATTERN ;

      SQL_TAXON_FROM_QUERY_RECOMMENDED_FULL =
        'SELECT DISTINCT ' +
        '  ITN.Taxon_List_Item_Key , ' +
        '       ITN.Abbreviation, ' +
        '       ITN.Authority, TL.Item_Name AS ListName, ' +
        '       dbo.ufn_FormattedSpeciesName( ITN2.Actual_Name, ' +
        '                                     ITN2.Authority,  ' +
        '                                     ITN.Preferred_Name_Authority, ' +
        '                                     ITN.Preferred_Name, ' +
        '                                     ITN.Actual_Name_Italic, ' +
        '                                     ITN.Preferred_Name_Italic,  ' +
        '                                     ITN2.Actual_Name_Attribute, ' +
        '                                     TR.Short_Name, ' +
        '                                     ITN.Can_Expand, ' +
        '''%s'') AS ItemName ' +
        ' FROM (Index_Taxon_Name ITN ' +
        ' INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key) ' +
        ' INNER JOIN Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key ' +
        ' INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key ' +
        ' INNER JOIN Taxon_Rank TR ON TR.Taxon_Rank_Key = TLI.Taxon_Rank_Key ' +
        ' INNER JOIN Index_Taxon_name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key = ' +
        ' ITN.Taxon_List_Item_Key ' +
        ' WHERE (ITN2.Actual_Name ' + ST_LIKE_PATTERN +
        ' OR ITN2.Abbreviation ' + ST_LIKE_PATTERN + ') ' +
        ' AND ITN2.Preferred_Taxa >0 ';

    SQL_TAXON_FROM_QUERY_PREFERRED_TAXA =
          'SELECT DISTINCT ' +
          ' [dbo].[ufn_GetTLIKeyFromSearch] (ITN.Taxon_List_Item_Key, ' +
          ' TLI.Preferred_Name, ITN.Recommended_Taxon_List_Item_Key, ' +
          ' T.Language, ITN.Preferred_Taxa) AS Taxon_List_Item_Key, ' +
          '       ITN.Abbreviation, ' +
          '       ITN.Authority, TL.Item_Name AS ListName, ' +
          '       dbo.ufn_FormattedSpeciesName( ITN.Actual_Name, ' +
          '                                     ITN.Authority,  ' +
          '                                     ITN.Preferred_Name_Authority, ' +
          '                                     ITN.Preferred_Name, ' +
          '                                     ITN.Actual_Name_Italic, ' +
          '                                     ITN.Preferred_Name_Italic,  ' +
          '                                     ITN.Actual_Name_Attribute, ' +
          '                                     TR.Short_Name, ' +
          '                                     ITN.Can_Expand, ' +
          '''%s'') AS ItemName ' +
          ' FROM (Index_Taxon_Name ITN ' +
          ' INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key) ' +
          ' INNER JOIN Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key ' +
          ' INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key ' +
          ' INNER JOIN Taxon_Rank TR ON TR.Taxon_Rank_Key = TLI.Taxon_Rank_Key ' +
          ' INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key ' +
          ' INNER JOIN Taxon T ON T.Taxon_Key = TV.Taxon_Key ' +
          ' WHERE (ITN.Actual_Name ' + ST_LIKE_PATTERN +
          ' OR ITN.Abbreviation ' + ST_LIKE_PATTERN + ') ' +
          ' AND ITN.Preferred_Taxa >0 ';

    SQL_TAXON_FROM_QUERY_ALL_LISTS =
          'SELECT DISTINCT ITN.Taxon_List_Item_Key, ITN.Abbreviation, ' +
          '       ITN.Authority, TL.Item_Name AS ListName, ' +
          '       dbo.ufn_FormattedSpeciesName( ITN.Actual_Name, ' +
          '                                     ITN.Authority,  ' +
          '                                     ITN.Preferred_Name_Authority, ' +
          '                                     ITN.Preferred_Name, ' +
          '                                     ITN.Actual_Name_Italic, ' +
          '                                     ITN.Preferred_Name_Italic,  ' +
          '                                     ITN.Actual_Name_Attribute, ' +
          '                                     TR.Short_Name, ' +
          '                                     ITN.Can_Expand, ' +
          '''%s'') AS ItemName ' +
          'FROM (Index_Taxon_Name ITN ' +
          'INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key) ' +
          'INNER JOIN Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key ' +
          'INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key ' +
          'INNER JOIN Taxon_Rank TR ON TR.Taxon_Rank_Key = TLI.Taxon_Rank_Key ' +
          'WHERE (ITN.Actual_Name ' + ST_LIKE_PATTERN +
          ' OR ITN.Abbreviation ' + ST_LIKE_PATTERN +
          ' OR ITN.Authority ' + ST_LIKE_PATTERN + ') ' +
          ' AND ITN.Allow_Data_Entry=1' +
          ' AND TLI.Taxon_List_Version_To IS NULL ' +
          ' AND TLV.Version >= (SELECT MAX(Version)' +
					'	FROM Taxon_List_Version ' +
          '	WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version' +
          ' WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)' +
          ' AND Version_Is_Amendment = 0)';

  SQL_TAXON_FROM_QUERY_PREF_LISTS =
          'SELECT DISTINCT ITN.Taxon_List_Item_Key, ITN.Abbreviation, ' +
          '       ITN.Authority, TL.Item_Name AS ListName, ' +
          '       dbo.ufn_FormattedSpeciesName( ITN.Actual_Name, ' +
          '                                     ITN.Authority,  ' +
          '                                     ITN.Preferred_Name_Authority, ' +
          '                                     ITN.Preferred_Name, ' +
          '                                     ITN.Actual_Name_Italic, ' +
          '                                     ITN.Preferred_Name_Italic,  ' +
          '                                     ITN.Actual_Name_Attribute, ' +
          '                                     TR.Short_Name, ' +
          '                                     ITN.Can_Expand, ' +
          '''%s'') AS ItemName ' +
          'FROM (Index_Taxon_Name ITN ' +
          'INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key) ' +
          'INNER JOIN Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key ' +
          'INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key ' +
          'INNER JOIN Taxon_Rank TR ON TR.Taxon_Rank_Key = TLI.Taxon_Rank_Key ' +
          'WHERE (ITN.Actual_Name ' + ST_LIKE_PATTERN +
          ' OR ITN.Abbreviation ' + ST_LIKE_PATTERN +
          ' OR ITN.Authority ' + ST_LIKE_PATTERN + ') ' +
          ' AND ITN.Preferred_List=1' +
          ' AND TLI.Taxon_List_Version_To IS NULL ' +
          ' AND TLV.Version >= (SELECT MAX(Version)' +
					'	FROM Taxon_List_Version ' +
          '	WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version' +
          ' WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)' +
          ' AND Version_Is_Amendment = 0)';

  SQL_TAXON_FROM_QUERY_RUCKSACK  =
          'SELECT DISTINCT ITNCS.Taxon_List_Item_Key, ITNCS.Abbreviation, '+
          '       ITNCS.Authority, '''' AS ListName, '+
          // SearchCode only applies for a direct search in the rucksack, not an expanded search using a rucksack of higher taxa
          '       CASE WHEN ITNCS.Taxon_List_Item_Key=ITNP.Taxon_List_Item_Key THEN LSR.SearchCode ELSE NULL END AS SearchCode, ' +
          '       dbo.ufn_FormattedSpeciesName( ITNCS.Actual_Name, ' +
          '                                     ITNCS.Authority,  ' +
          '                                     ITNCS.Preferred_Name_Authority, ' +
          '                                     ITNCS.Preferred_Name, ' +
          '                                     ITNCS.Actual_Name_Italic, ' +
          '                                     ITNCS.Preferred_Name_Italic,  ' +
          '                                     ITNCS.Actual_Name_Attribute, ' +
          '                                     TR.Short_Name, ' +
          '                                     ITNCS.Can_Expand, ' +
          '''%s'') AS ItemName ' +
          'FROM Index_Taxon_Name ITNP '+ //ITN Parent
          'INNER JOIN Index_Taxon_Group ITG ON ITG.Taxon_List_Item_Key = ITNP.Recommended_Taxon_List_Item_Key '+
          'INNER JOIN Index_Taxon_Name ITNC ON ITNC.Taxon_List_Item_Key = ITG.Contained_List_Item_Key '+ // ITN Child
          'INNER JOIN Index_Taxon_Name ITNCS ON ITNCS.Recommended_Taxon_List_Item_Key = ITNC.Recommended_Taxon_List_Item_Key '+ // ITN Child Synonyms
          'INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITNCS.Taxon_List_Item_Key ' +
          'INNER JOIN Taxon_Rank TR ON TR.Taxon_Rank_Key = TLI.Taxon_Rank_Key ' +
          'INNER JOIN #LastSearchedRucksack LSR ON ITNP.Taxon_List_Item_Key = LSR.Taxon_List_Item_Key '+
          'WHERE ITNCS.Taxon_List_Version_key = ITNP.Taxon_List_Version_key '+
          'AND (ITNCS.Actual_Name ' + ST_LIKE_PATTERN +
          ' OR ITNCS.Abbreviation ' + ST_LIKE_PATTERN +
          ' OR ITNCS.Authority ' + ST_LIKE_PATTERN +
          ' OR (LSR.SearchCode ' + ST_LIKE_PATTERN +
          ' AND ITNCS.Taxon_List_Item_Key=ITNP.Taxon_List_Item_Key))';

  ST_ITEMNAME = 'ITN.Actual_Name';
  ST_ABBREVIATION = 'ITN.Abbreviation';

  SQL_SUBQUERY_TAXON_GROUPS = 'SELECT DISTINCT ITG.Contained_List_Item_Key ' +
                              'FROM Index_Taxon_Group ITG ' +
                              'JOIN Index_Taxon_Name ITN ON ITN.Recommended_Taxon_List_Item_Key=ITG.Taxon_List_Item_Key ' +
                              'WHERE ITN.Taxon_List_Item_Key IN ';

  // SQL to clean a taxon list item from the taxon group index
  SQL_CLEAN_GROUP_INDEX = 'DELETE FROM Index_Taxon_Group ' +
                          'WHERE Taxon_List_Item_Key = ''%s'' ' +
                          'OR Contained_List_Item_Key = ''%s''';

  { The following SQL Statements are used by the map to identify points clicked on }
  SQL_EVENTS_FOR_EVENTS_INTERSECTION =
          'SELECT E.Survey_Event_Key as ItemKey, E.Vague_Date_Start, E.Vague_Date_End, E.Vague_Date_Type, LN.Item_Name AS ItemName '+
          'FROM   (Survey_Event As E INNER JOIN Location As L ON E.Location_Key = L.Location_Key) INNER JOIN Location_Name AS LN ON L.Location_Key = LN.Location_Key '+
          'WHERE LN.Preferred = 1 '+
          'AND E.SURVEY_EVENT_KEY IN (%s) AND E.SURVEY_EVENT_KEY IN (%s)' +
          ' UNION '+
          'SELECT Survey_Event_Key, Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Spatial_Ref AS ItemName '+
          'FROM Survey_Event As E '+
          'WHERE Location_Key IS NULL '+
          'AND NOT Spatial_Ref IS NULL '+
          'AND E.SURVEY_EVENT_KEY IN (%s) AND E.SURVEY_EVENT_KEY IN (%s)' +
          ' UNION '+
          'SELECT Survey_Event_Key, Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Location_Name AS ItemName '+
          'FROM Survey_Event As E '+
          'WHERE Location_Key IS NULL '+
          'AND Spatial_Ref IS NULL '+
          'AND E.SURVEY_EVENT_KEY IN (%s) AND E.SURVEY_EVENT_KEY IN (%s)';

  SQL_SAMPLES_FOR_SAMPLES_INTERSECTION =
          'SELECT S.Sample_Key as ItemKey, S.Vague_Date_Start, S.Vague_Date_End, '+
          'S.Vague_Date_Type, T.Short_Name as ItemName '+
          'FROM Sample As S INNER JOIN '+
          'Sample_Type As T ON '+
          'S.Sample_Type_Key = T.Sample_Type_Key '+
          'WHERE S.SAMPLE_KEY in (%s) AND S.SAMPLE_KEY in (%s)';

  SQL_TAXA_FOR_LIST_ITEM_AND_SAMPLE_INTERSECTION =
          'SELECT TOX.Taxon_Occurrence_Key as ItemKey, T.Item_Name As ItemName '+
          'FROM SAMPLE INNER JOIN '+
          '(Taxon As T INNER JOIN ' +
          '(Taxon_Version As TV INNER JOIN '+
          '(Taxon_Occurrence As TOX INNER JOIN '+
          '(Taxon_List_Item As TLI INNER JOIN '+
          'Taxon_Determination As TD ON '+
          'TLI.Taxon_List_Item_Key = TD.Taxon_List_Item_Key) ON '+
          'TOX.Taxon_Occurrence_Key = TD.Taxon_Occurrence_Key) ON '+
          'TV.Taxon_Version_Key = TLI.Taxon_Version_Key) ON '+
          'T.Taxon_Key = TV.Taxon_Key) ON '+
          'SAMPLE.SAMPLE_KEY=TOX.SAMPLE_KEY '+
          'WHERE TD.Preferred = 1 ' +
          'AND SAMPLE.SAMPLE_KEY in (%s) AND TLI.TAXON_LIST_ITEM_KEY in (%s)';

  SQL_TAXA_FOR_GROUP_AND_SAMPLE_INTERSECTION =
          'SELECT DISTINCT TOCC.Taxon_Occurrence_Key as ItemKey, ITN.Actual_Name As ItemName '+
          'FROM SAMPLE S '+
          'INNER JOIN TAXON_OCCURRENCE TOCC ON TOCC.SAMPLE_KEY=S.SAMPLE_KEY '+
          'INNER JOIN TAXON_DETERMINATION TD ON TD.TAXON_OCCURRENCE_KEY=TOCC.TAXON_OCCURRENCE_KEY '+
          'INNER JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY=TD.TAXON_LIST_ITEM_KEY '+
          'INNER JOIN INDEX_TAXON_GROUP ITG ON ITG.CONTAINED_LIST_ITEM_KEY=ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY '+
          'WHERE TD.Preferred = 1 ' +
          'AND S.SAMPLE_KEY in (%s) AND ITG.Taxon_List_Item_Key IN (%s)';

  SQL_TAXA_FOR_OCCURRENCE_AND_SAMPLE_INTERSECTION =
          'SELECT TOX.TAXON_OCCURRENCE_KEY as ItemKey, T.ITem_Name as ItemName ' +
          'FROM SAMPLE INNER JOIN'+
          '(Taxon As T INNER JOIN ' +
          '(Taxon_Version As TV INNER JOIN '+
          '(Taxon_Occurrence As TOX INNER JOIN '+
          '(Taxon_List_Item As TLI INNER JOIN '+
          'Taxon_Determination As TD ON '+
          'TLI.Taxon_List_Item_Key = TD.Taxon_List_Item_Key) ON '+
          'TOX.Taxon_Occurrence_Key = TD.Taxon_Occurrence_Key) ON '+
          'TV.Taxon_Version_Key = TLI.Taxon_Version_Key) ON '+
          'T.Taxon_Key = TV.Taxon_Key) ON '+
          'SAMPLE.SAMPLE_KEY=TOX.SAMPLE_KEY '+
          'WHERE TD.Preferred = 1 ' +
          'AND SAMPLE.SAMPLE_KEY IN (%s) AND TOX.TAXON_OCCURRENCE_KEY IN (%s)';

  SQL_BIOTOPES_FOR_LIST_ITEM_AND_SAMPLE_INTERSECTION =
          'SELECT BO.Biotope_Occurrence_Key As ItemKey, '+
          'IsNull(B.Original_Code, IsNull(B.Short_Term, B.Full_Term)) AS ItemName, '+
          'BO.Checked '+
          'FROM SAMPLE INNER JOIN '+
          '(Biotope_Occurrence AS BO INNER JOIN '+
          '((Biotope AS B INNER JOIN '+
          'Biotope_List_Item AS BLI ON '+
          'B.Biotope_Key = BLI.Biotope_Key) INNER JOIN '+
          'Biotope_Determination AS BD ON '+
          'BLI.Biotope_List_Item_Key = BD.Biotope_List_Item_Key) ON '+
          'BO.Biotope_Occurrence_Key = BD.Biotope_Occurrence_Key) ON '+
          'BO.SAMPLE_KEY=SAMPLE.SAMPLE_KEY '+
          'WHERE BD.Preferred = 1 and '+
          'SAMPLE.SAMPLE_KEY in (%s) AND BLI.BIOTOPE_LIST_ITEM_KEY in (%s)';

  SQL_BIOTOPES_FOR_OCCURRENCE_AND_SAMPLE_INTERSECTION =
          'SELECT BO.Biotope_Occurrence_Key as ItemKey, '+
          'IsNull(B.Original_Code, IsNull(B.Short_Term, B.Full_Term)) AS ItemName '+
          'FROM SAMPLE INNER JOIN '+
          '(Biotope_Occurrence AS BO INNER JOIN '+
          '((Biotope AS B INNER JOIN '+
          'Biotope_List_Item AS BLI ON '+
          'B.Biotope_Key = BLI.Biotope_Key) INNER JOIN '+
          'Biotope_Determination AS BD ON '+
          'BLI.Biotope_List_Item_Key = BD.Biotope_List_Item_Key) ON '+
          'BO.Biotope_Occurrence_Key = BD.Biotope_Occurrence_Key) ON '+
          'BO.SAMPLE_KEY=SAMPLE.SAMPLE_KEY '+
          'WHERE BD.Preferred = 1 '+
          'and SAMPLE.SAMPLE_KEY IN (%s) AND BO.BIOTOPE_OCCURRENCE_KEY IN (%s)';

  SQL_MAP_SAMPLE_LIST =
          'SELECT S.* ' +
          'FROM (Sample S ' +
          'INNER JOIN Survey_Event SE ' +
          'ON S.Survey_Event_Key = SE.Survey_Event_Key) ' +
          'INNER JOIN Survey ' +
          'ON Se.Survey_Key = Survey.Survey_Key ' +
          'WHERE Survey.Survey_Key IN (%s) ' +
          'AND S.Spatial_Ref <> '''' ';

  SQL_MAP_TAXA_LIST =
          'SELECT DISTINCT S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type, '+
          'S.Lat, S.Long, S.Spatial_Ref, S.Spatial_Ref_System, D.Taxon_List_Item_Key '+
          'FROM Sample AS S '+
          'INNER JOIN Taxon_Occurrence AS T on T.Sample_Key=S.Sample_Key '+
          'INNER JOIN Taxon_Determination AS D ON T.Taxon_Occurrence_Key = D.Taxon_Occurrence_Key '+
          'INNER JOIN Index_Taxon_Synonym AS J on J.Synonym_List_Item_Key = D.Taxon_List_Item_Key '+
          'INNER JOIN Survey_Event SE ON SE.Survey_Event_Key = S.Survey_Event_Key ' +
          'LEFT JOIN User_Survey_Restriction USR ON USR.Survey_Key = SE.Survey_Key ' +
           'AND USR.Name_Key = ''%s'' ' +
          '%s '+
          'WHERE %s.Taxon_List_Item_Key = ''%s''' +
          'AND D.Preferred = 1 ' +
          'AND T.Zero_Abundance = 0 ' +
          'AND T.Confidential = 0 ' +
          'AND T.Checked = 1 ' +
          'AND T.Verified <> 1';

  SQL_MAP_TAXA_LIST_NAMESERVER =
          'SELECT DISTINCT S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type, '+
          'S.Lat, S.Long, S.Spatial_Ref, S.Spatial_Ref_System, D.Taxon_List_Item_Key '+
          'FROM Sample AS S '+
          'INNER JOIN Taxon_Occurrence AS T on T.Sample_Key=S.Sample_Key '+
          'INNER JOIN Taxon_Determination AS D ON T.Taxon_Occurrence_Key = D.Taxon_Occurrence_Key '+
          'INNER JOIN Index_Taxon_Name AS ITN1 on ITN1.Taxon_List_Item_Key = D.Taxon_List_Item_Key '+
          'INNER JOIN Index_Taxon_Name AS J on J.Recommended_Taxon_List_Item_Key = ITN1.Recommended_Taxon_List_Item_Key '+
          'INNER JOIN Survey_Event SE ON SE.Survey_Event_Key = S.Survey_Event_Key ' +
          'LEFT  JOIN User_Survey_Restriction USR ON USR.Survey_Key = SE.Survey_Key ' +
                  'AND USR.Name_Key = ''%s'' ' +
          '%s '+
          'WHERE %s.Taxon_List_Item_Key = ''%s''' +
          'AND D.Preferred = 1 ' +
          'AND T.Zero_Abundance = 0 ' +
          'AND T.Confidential = 0 ' +
          'AND T.Checked = 1 ' +
          'AND T.Verified <> 1' +
          'AND USR.Name_Key IS NULL';

  SQL_MAP_TAXA_CONTAINED_JOIN =
          'JOIN Index_Taxon_Group ITG ON ITG.Contained_List_Item_Key = J.Taxon_List_Item_Key ' +
          'JOIN Index_Taxon_Synonym AS JG on JG.Synonym_List_Item_Key = ITG.Taxon_List_Item_Key';

  SQL_MAP_TAXA_CONTAINED_JOIN_NAMESERVER =
          'JOIN Index_Taxon_Group ITG ON ITG.Contained_List_Item_Key = ITN1.Recommended_Taxon_List_Item_Key ' +
          'JOIN Index_Taxon_Name AS ITN2 ON ITN2.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key '+
          'JOIN Index_Taxon_Name AS JG ON JG.Recommended_Taxon_List_Item_Key = ITN2.Recommended_Taxon_List_Item_Key';

  SQL_MAP_BIOTOPES_LIST =
          'SELECT S.Lat, S.Long, S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type, ' +
          'S.Spatial_Ref, S.Spatial_Ref_System, D.Preferred, D.Biotope_List_Item_Key ' +
          'FROM ((Sample AS S ' +
          'INNER JOIN Biotope_Occurrence As B ON S.Sample_Key = B.Sample_Key) ' +
          'INNER JOIN Biotope_Determination As D ON B.Biotope_Occurrence_Key = D.Biotope_Occurrence_Key) ' +
          'INNER JOIN Survey_Event SE ON SE.Survey_Event_Key = S.Survey_Event_Key ' +
          'LEFT  JOIN User_Survey_Restriction USR ON USR.Survey_Key = SE.Survey_Key ' +
                  'AND USR.Name_Key = ''%s'' ' +
          'WHERE D.Biotope_List_Item_Key IN (%s) ' +
          'AND D.Preferred = 1 ' +
          'AND B.Checked = 1 ' +
          'AND B.Verified <> 1 ' +
          'AND USR.Name_Key IS NULL';

  SQL_REFERENCE_AUTHORS =
          'SELECT RA.Source_Key As [Source_Key], %s, ' +
          '       R.Year_Vague_Date_Start, ' +
          '       R.Year_Vague_Date_End, ' +
          '       R.Year_Vague_Date_Type, ' +
          '       R.Title As [Title], ' +
          '       R.System_Supplied_Data AS [System_Supplied_Data] ' +
          'FROM %s AS RA ' +
          'INNER JOIN Reference AS R ON RA.Source_Key = R.Source_Key ' +
          'WHERE RA.Source_Key IN (%s) ' +
          'ORDER BY %s, R.Year_Vague_Date_Start';

  SQL_REFERENCE_AUTHORS_NO_WHERE =
          'SELECT RA.Source_Key As [Source_Key], %s, ' +
          '       R.Year_Vague_Date_Start, ' +
          '       R.Year_Vague_Date_End, ' +
          '       R.Year_Vague_Date_Type, ' +
          '       R.Title As [Title], ' +
          '       R.System_Supplied_Data AS [System_Supplied_Data] ' +
          'FROM %s AS RA ' +
          'INNER JOIN Reference AS R ON RA.Source_Key = R.Source_Key ' +
          'ORDER BY %s, R.Year_Vague_Date_Start';

  { optional SQL to extract the authors string safely from Reference_Authors_Crosstab,
    depending on max number of authors any reference has }
  SQL_AUTHORS_0 = ''''' As [Author]';
  SQL_AUTHORS_1 = '[1] As [Author]';
  SQL_AUTHORS_2 = 'IIF( ISNULL( [2] ), [1], [1]+'' & ''+[2]) As [Author]';
  SQL_AUTHORS_3 = 'IIF( ISNULL( [2] ), [1], IIF( ISNULL( [3] ), [1]+'' & ''+[2], [1]+'' et al.'') ) As [Author]';

  SQL_TAXON_RESTRICTION = ' AND TOCC.VERIFIED<>1 AND TOCC.CONFIDENTIAL=0 AND '+
                        'TOCC.ZERO_ABUNDANCE=0 AND TOCC.CHECKED=1 ';
  SQL_BIOTOPE_RESTRICTION = ' AND BOCC.VERIFIED<>1 AND BOCC.CHECKED=1 ';

  // SQL to create and use a temporary list table for input into a stored proc
  SQL_TEMPLIST_CREATE =
        'CREATE TABLE #TempList ' +
        '(RecordKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)';
  SQL_TEMPLIST_CREATE_IF_REQUIRED =
        'IF OBJECT_ID(''tempdb..#TempList'') IS NULL ' + SQL_TEMPLIST_CREATE;

  SQL_TEMPLIST_CREATE_ORDERED =
        'CREATE TABLE #TempList ' +
        '(RecordKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY, '+
        'SortOrder INT)';

  SQL_TEMPLIST_INSERT = 'INSERT INTO #TempList VALUES (''%s'')';

  SQL_TEMPLIST_INSERT_DISTINCT = 'IF NOT EXISTS (SELECT 1 FROM #TempList WHERE RecordKey=''%s'') '+
        'INSERT INTO #TempList VALUES (''%s'')';

  SQL_TEMPLIST_INSERT_ORDERED = 'INSERT INTO #TempList VALUES (''%s'', %d)';

  SQL_TEMPLIST_DROP = 'IF OBJECT_ID(''tempdb..#TempList'') IS NOT NULL DROP TABLE #TempList';

  // Database Settings, read/write
  SQL_SELECT_SETTING = 'SELECT Data FROM Setting WHERE Name=''%s''';

  SQL_UPDATE_SETTING = 'UPDATE Setting SET Data=''%s'' WHERE Name=''%s''';

type
  { Define some standard pieces of SQL that can be reused in the application }
  TStandardSQL = (ssSurveysForName, ssEventsForName, ssSamplesForName,
                  ssOccurrencesForName, ssLocationsForName,
                  ssSurveysForReference, ssEventsForReference, ssSamplesForReference,
                  ssOccurrencesForReference, ssLocationsForReference, ssNamesForReference,
                  ssSurveysForLocation,
                  ssEventsForLocation, ssSamplesForLocation, ssOccurrencesForLocation,
                  ssNamesForLocation, ssNamesForSurvey, ssNamesForEvent, ssNamesForSample,
                  ssNamesForBiotopeOcc, ssNamesForTaxonOcc, ssLocationsForEvent,
                  ssLocationsForSample, ssTaxonName, ssCommonName, ssBiotopeName, ssAdminAreaName,
                  ssLocationChildren, ssNamesForFeature, ssPreferredCommonName,
                  ssTaxonListItemForOccurrence );

procedure SetStandardQuery(AQuery: TADOQuery; const AStandard: TStandardSQL;
  const AKeyValue, AUserId: TKeyString);

//==============================================================================
implementation

{-------------------------------------------------------------------------------
  Sets up the query object with a given standard SQL.
}
procedure SetStandardQuery(AQuery: TADOQuery; const AStandard: TStandardSQL;
  const AKeyValue, AUserId: TKeyString);
begin
  if AQuery.Active then AQuery.Close;
  with AQuery.SQL do begin
    Clear;
    Text := '';

    case AStandard of
      ssCommonName,
      ssTaxonName :
          begin
            // There should be only one record returned
            Add('SELECT DISTINCT TLI.Preferred_Name AS Preferred_Name_Key, ' +
                    'ITN.Preferred_Name, ITN.Preferred_Name_Italic, ITN.Preferred_Name_Attribute, ITN.Preferred_Name_Authority, ' +
                    'ITN.Common_Name, ITN.Common_Name_Italic, ITN.Common_Name_Attribute, ' +
                    'ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Actual_Name_Attribute, ITN.Preferred_Name_Authority, ' +
                    'ITN.Authority, TLV.Taxon_List_Key, ' +
                    'TV.Attribute, TR.Short_Name AS RankName ');
            Add('FROM Index_Taxon_Name ITN ' +
                'JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key ' +
                'JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key ');
            Add('JOIN Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key ' +
                'JOIN Taxon_Rank TR ON TR.Taxon_Rank_Key = TLI.Taxon_Rank_Key ');
            Add('WHERE ITN.Taxon_List_Item_Key = ''' + AKeyValue + '''');
          end;
      ssPreferredCommonName :
          begin
            Add('SELECT ISNULL(TUN.Item_Name, T2.Item_Name) AS CommonName, ' +
                       'ISNULL(TUN.Item_Name, T2.Language) AS CommonLanguage, ' +
                       'T2.Item_Name AS OfficialCommonName, T2.Language AS OfficialCommonLanguage');
            Add('FROM ((Taxon_Common_Name AS TCN ' +
                'INNER JOIN Taxon_Version AS TV2 ' +
                        'ON TV2.Taxon_Version_Key = TCN.Taxon_Version_Key) ' +
                'INNER JOIN Taxon AS T2 ' +
                        'ON T2.Taxon_Key = TV2.Taxon_Key) ' +
                 'LEFT JOIN Taxon_User_Name AS TUN ' +
                        'ON (TUN.Taxon_List_Item_Key = TCN.Taxon_List_Item_Key ' +
                       'AND TUN.Preferred=1 OR TUN.Preferred IS NULL)');
            Add('WHERE TCN.Taxon_List_Item_Key = ''' + AKeyValue + '''');
          end;
      ssBiotopeName :
          Text :='SELECT B.Original_Code, B.Short_Term '+
              'FROM Biotope AS B INNER JOIN Biotope_List_Item AS BLI ON B.Biotope_Key = BLI.Biotope_Key '+
              'WHERE BLI.Biotope_List_Item_Key = ''' + AKeyValue + '''';
      ssAdminAreaName :
          Text :='SELECT A.Short_Code, A.Item_Name '+
              'FROM Admin_Area AS A '+
              'WHERE A.Admin_Area_Key = ''' + AKeyValue + '''';
      // From Individuals/Organisations to Observations>Surveys
      ssSurveysForName :
          Add('SELECT ''Survey'' AS [TableName], S.Survey_Key AS [ItemKey] '+
              'FROM Survey S ' +
              'LEFT JOIN User_Survey_Restriction USR ON USR.Survey_Key = S.Survey_Key ' +
                  'AND USR.Name_Key = ''' + AUserId + ''' ' +
              'WHERE Run_By = ''' + AKeyValue + ''' ' +
                  'AND USR.Name_Key IS NULL');
      // From Individuals/Organisations to Observations>Events
      ssEventsForName :
          Add('SELECT ''Survey_Event'' AS [TableName], Survey_Event_Key AS [ItemKey] '+
              'FROM Survey_Event_Recorder WHERE Name_Key = ''' + AKeyValue + '''');
      // From Individuals/Organisations to Observations>Samples
      ssSamplesForName :
          Add('SELECT ''Sample'' AS [TableName], SR.Sample_Key AS [ItemKey] '+
              'FROM Survey_Event_Recorder AS SER INNER JOIN Sample_Recorder AS SR '+
              '     ON SER.SE_Recorder_Key = SR.SE_Recorder_Key '+
              'WHERE SER.Name_Key = ''' + AKeyValue + '''');
      // From Individuals/Organisations to Observations>Occurrences (Biotope AND Taxon)
      ssOccurrencesForName :
          begin
            Add('SELECT ''Taxon_Occurrence'' AS [TableName], Taxon_Occurrence_Key AS [ItemKey] '+
                'FROM Taxon_Determination WHERE Determiner = ''' + AKeyValue + '''');
            Add('UNION '+
                'SELECT ''Biotope_Occurrence'' AS [TableName], Biotope_Occurrence_Key AS [ItemKey] ' +
                'FROM Biotope_Determination WHERE Determiner = ''' + AKeyValue + '''');
            Add('UNION '+
                'SELECT ''Taxon_Occurrence'' AS [TableName], O.Taxon_Occurrence_Key AS [ItemKey] '+
                'FROM Survey_Event_Recorder AS SER INNER JOIN Sample_Recorder AS SR '+
                '     ON SER.SE_Recorder_Key = SR.SE_Recorder_Key ');
            Add('INNER JOIN Taxon_Occurrence O ON O.Sample_Key=SR.Sample_Key '+
                'WHERE SER.Name_Key = ''' + AKeyValue + '''');
            Add('UNION '+
                'SELECT ''Biotope_Occurrence'' AS [TableName], O.Biotope_Occurrence_Key AS [ItemKey] '+
                'FROM Survey_Event_Recorder AS SER INNER JOIN Sample_Recorder AS SR '+
                '     ON SER.SE_Recorder_Key = SR.SE_Recorder_Key ');
            Add('INNER JOIN Biotope_Occurrence O ON O.Sample_Key=SR.Sample_Key '+
                'WHERE SER.Name_Key = ''' + AKeyValue + '''');
          end;
      // From Individuals/Organisations to Locations
      ssLocationsForName :
          begin
            Add('SELECT ''Location'' AS [TableName], Location_Key AS [ItemKey] '+
                'FROM Location_Designation WHERE Authority = ''' + AKeyValue + ''' ');
            Add('UNION '+
                'SELECT ''Location'' AS [TableName], Location_Key AS [ItemKey] '+
                'FROM Tenure WHERE Owned_By = ''' + AKeyValue + '''');
          end;

      // From References to Observations>Surveys
      ssSurveysForReference :
          Add('SELECT ''Survey'' AS [TableName], Survey_Key AS [ItemKey] '+
              'FROM Survey_Sources WHERE Source_Key = ''' + AKeyValue + '''');
      // From References to Observations>Events
      ssEventsForReference :
          Add('SELECT ''Survey_Event'' AS [TableName], Survey_Event_Key AS [ItemKey] '+
              'FROM Survey_Event_Sources WHERE Source_Key = ''' + AKeyValue + '''');
      // From References to Observations>Samples
      ssSamplesForReference :
          Add('SELECT ''Sample'' AS [TableName], Sample_Key AS [ItemKey] '+
              'FROM Sample_Sources WHERE Source_Key = ''' + AKeyValue + '''');
      // From References to Observations>Occurrences (Biotope AND Taxon)
      ssOccurrencesForReference :
          begin
            Add('SELECT ''Taxon_Occurrence'' AS [TableName], Taxon_Occurrence_Key AS [ItemKey] '+
                'FROM Taxon_Occurrence_Sources WHERE Source_Key = ''' + AKeyValue + ''' ');
            Add('UNION ' +
                'SELECT ''Taxon_Occurrence'' AS [TableName], Taxon_Occurrence_Key AS [ItemKey] '+
                'FROM Taxon_Determination WHERE Source_Key = ''' + AKeyValue + '''');
            Add('UNION ' +
                'SELECT ''Biotope_Occurrence'' AS [TableName], Biotope_Occurrence_Key AS [ItemKey] '+
                'FROM Biotope_Occurrence_Sources WHERE Source_Key = ''' + AKeyValue + '''');
            Add('UNION ' +
                'SELECT ''Biotope_Occurrence'' AS [TableName], Biotope_Occurrence_Key AS [ItemKey] '+
                'FROM Biotope_Determination WHERE Source_Key = ''' + AKeyValue + '''');
          end;
      // From References to Locations
      ssLocationsForReference :
          Add('SELECT ''Location'' AS [TableName], Location_Key AS [ItemKey] '+
              'FROM Location_Sources WHERE Source_Key = ''' + AKeyValue + '''');
      // From References to Individual/Organisation
      ssNamesForReference :
          begin
            Add('SELECT ''Individual'' AS [TableName], N.Name_Key AS [ItemKey] '+
                'FROM Name AS N, Name_Sources AS NS WHERE NS.Source_Key = ''' + AKeyValue + ''' '+
                'AND N.Name_Key = NS.Name_Key AND N.Organisation = 0 ');
            Add('UNION '+
                'SELECT ''Organisation'' AS [TableName], N.Name_Key AS [Item_Key] '+
                'FROM Name AS N, Name_Sources AS NS WHERE NS.Source_Key = ''' + AKeyValue + ''' '+
                'AND N.Name_Key = NS.Name_Key AND N.Organisation = 1 ');
          end;

      // From Locations to Observations>Surveys
      ssSurveysForLocation :
          begin
            Add('SELECT ''Survey'' AS [TableName], Survey_Key AS [ItemKey] FROM Survey_Event ' +
                'WHERE Location_Key = ''' + AKeyValue + ''' ');
            Add('UNION ' +
                'SELECT ''Survey'' AS [TableName], Survey_Key AS [ItemKey] FROM Sample INNER JOIN Survey_Event ' +
                'ON Survey_Event.Survey_Event_Key = Sample.Survey_Event_Key ' +
                'WHERE Sample.Location_Key = ''' + AKeyValue + ''' ');
          end;
      // From Locations to Observations>Events
      ssEventsForLocation :
          Add('SELECT ''Survey_Event'' AS [TableName], Survey_Event_Key AS [ItemKey] ' +
              'FROM Survey_Event WHERE Location_Key = ''' + AKeyValue + '''');
      // From Locations to Observations>Samples
      ssSamplesForLocation :
          Add('SELECT ''Sample'' AS [TableName], Sample_Key AS [ItemKey] ' +
              'FROM Sample WHERE Location_Key = ''' + AKeyValue + '''');
      // From Locations to Observations>Occurrences (Biotope AND Taxon)
      ssOccurrencesForLocation :
          begin
            Add('SELECT ''Taxon_Occurrence'' AS [TableName], [TO].Taxon_Occurrence_Key AS [ItemKey] '+
                'FROM Taxon_Occurrence AS [TO], Sample AS S '+
                'WHERE S.Sample_Key = [TO].Sample_Key AND S.Location_Key = ''' + AKeyValue + ''' ');
            Add('UNION '+
                'SELECT ''Biotope_Occurrence'' AS [TableName], BO.Biotope_Occurrence_Key AS [ItemKey] '+
                'FROM Biotope_Occurrence AS BO, Sample AS S '+
                'WHERE S.Sample_Key = BO.Sample_Key AND S.Location_Key = ''' + AKeyValue + ''' ');
          end;
      // From Locations to Individual/Organisation
      ssNamesForLocation :
          begin
            Add('SELECT ''Individual'' AS [TableName], N.Name_Key AS [ItemKey] '+
                'FROM Name AS N, Location_Designation AS LD WHERE LD.Location_Key = ''' + AKeyValue + ''''+
                'AND N.Name_Key = LD.Authority AND N.Organisation = 0 ');
            Add('UNION '+
                'SELECT ''Organisation'' AS [TableName], N.Name_Key AS [ItemKey] '+
                'FROM Name AS N, Location_Designation AS LD WHERE LD.Location_Key = ''' + AKeyValue + ''''+
                'AND N.Name_Key = LD.Authority AND N.Organisation = 1 ');
            Add('UNION '+
                'SELECT ''Individual'' AS [TableName], N.Name_Key AS [ItemKey] '+
                'FROM Name AS N, Tenure AS T WHERE T.Location_Key = ''' + AKeyValue + ''''+
                'AND N.Name_Key = T.Owned_By AND N.Organisation = 0 ');
            Add('UNION '+
                'SELECT ''Organisation'' AS [TableName], N.Name_Key AS [ItemKey] '+
                'FROM Name AS N, Tenure AS T WHERE T.Location_Key = ''' + AKeyValue + ''''+
                'AND N.Name_Key = T.Owned_By AND N.Organisation = 1 ');
          end;

      // From Feature to Individual/Organisation
      ssNamesForFeature :
          begin
            Add('Select ''Individual'' AS [TableName], N.Name_KEY AS [ItemKey] ' +
                'FROM Name AS N, Management_Aim As M ' +
                'WHERE M.Location_Feature_Key = ''' + AKeyValue + ''' ' +
                'AND N.Name_Key = M.Authority AND N.Organisation = 0 ' );
            Add('UNION ' +
                'Select ''Organisation'' AS [TableName], N.Name_KEY AS [ItemKey] ' +
                'FROM Name AS N, Management_Aim As M ' +
                'WHERE M.Location_Feature_Key = ''' + AKeyValue + ''' ' +
                'AND N.Name_Key = M.Authority AND N.Organisation = 1 ' );
          end;

      // From Observations>Surveys to Individual/Organisation
      ssNamesForSurvey :
          begin
            Add('SELECT ''Individual'' AS [TableName], N.Name_Key AS [ItemKey] '+
                'FROM Name AS N, Survey AS S WHERE S.Survey_Key = ''' + AKeyValue + ''' '+
                'AND N.Name_Key = S.Run_By AND N.Organisation = 0 ');
            Add('UNION '+
                'SELECT ''Organisation'' AS [TableName], N.Name_Key AS [ItemKey] '+
                'FROM Name AS N, Survey AS S WHERE S.Survey_Key = ''' + AKeyValue + ''' '+
                'AND N.Name_Key = S.Run_By AND N.Organisation = 1 ');
          end;
      // From Observations>Events to Individual/Organisation
      ssNamesForEvent :
          Add('SELECT ''Individual'' AS [TableName], N.Name_Key AS [ItemKey] '+
              'FROM Name AS N, Survey_Event_Recorder AS SER WHERE SER.Survey_Event_Key = ''' + AKeyValue + ''' '+
              'AND N.Name_Key = SER.Name_Key AND N.Organisation = 0 ');
      // From Observations>Samples to Individual/Organisation
      ssNamesForSample :
          begin
            Add('SELECT ''Individual'' AS [TableName], N.Name_Key AS [ItemKey] '+
                'FROM Name AS N, Survey_Event_Recorder AS SER, Sample_Recorder AS SR ');
            Add('WHERE SR.Sample_Key = ''' + AKeyValue + ''' AND SER.SE_Recorder_Key = SR.SE_Recorder_Key '+
                'AND N.Name_Key = SER.Name_Key AND N.Organisation = 0 ');
          end;
      // From Observations>Biotope Occurrence to Individual/Organisation
      ssNamesForBiotopeOcc :
          begin
            Add('SELECT ''Individual'' AS [TableName], N.Name_Key AS [ItemKey] '+
                'FROM Name AS N, Biotope_Determination AS BD WHERE BD.Biotope_Occurrence_Key = ''' + AKeyValue + ''' '+
                'AND N.Name_Key = BD.Determiner AND N.Organisation = 0 ');
            Add('UNION '+
                'SELECT ''Organisation'' AS [TableName], N.Name_Key AS [ItemKey] '+
                'FROM Name AS N, Biotope_Determination AS BD WHERE BD.Biotope_Occurrence_Key = ''' + AKeyValue + ''' '+
                'AND N.Name_Key = BD.Determiner AND N.Organisation = 1 ');
          end;
      // From Observations>Taxon Occurrence to Individual/Organisation
      ssNamesForTaxonOcc :
          begin
            Add('SELECT ''Individual'' AS [TableName], N.Name_Key AS [ItemKey] '+
                'FROM Name AS N, Taxon_Determination AS TD WHERE TD.Taxon_Occurrence_Key = ''' + AKeyValue + ''' '+
                'AND N.Name_Key = TD.Determiner AND N.Organisation = 0 ');
            Add('UNION '+
                'SELECT ''Organisation'' AS [TableName], N.Name_Key AS [ItemKey] '+
                'FROM Name AS N, Taxon_Determination AS TD WHERE TD.Taxon_Occurrence_Key = ''' + AKeyValue + ''' '+
                'AND N.Name_Key = TD.Determiner AND N.Organisation = 1 ');
          end;
      // From Observations>Events to Locations
      ssLocationsForEvent :
          Add('SELECT ''Location'' AS [TableName], L.Location_Key AS [ItemKey] '+
              'FROM Location AS L, Survey_Event AS SE WHERE SE.Survey_Event_Key = ''' + AKeyValue + ''' '+
              'AND L.Location_Key = SE.Location_Key');
      // From Observations>Samples/Occurrences to Locations
      ssLocationsForSample :
          Add('SELECT ''Location'' AS [TableName], L.Location_Key AS [ItemKey] '+
              'FROM Location AS L, Sample AS S WHERE S.Sample_Key = ''' + AKeyValue + ''' '+
              'AND L.Location_Key = S.Location_Key');
      // From Observations>Surveys to Locations
      //   Special processing, see TfrmObservations
      // For data export
      ssLocationChildren :
          Add('SELECT Location_key as [ItemKey], ''Location'' as [TableName] ' +
              'FROM Location WHERE Parent_Key = ''' + AKeyValue + '''');

      ssTaxonListItemForOccurrence :
          Add('SELECT TAXON_LIST_ITEM_KEY '+
              'FROM TAXON_DETERMINATION '+
              'WHERE TAXON_OCCURRENCE_KEY = ''' + AKeyValue + ''' '+
              'AND PREFERRED = 1 ');
    end;
  end;
end;  // SetStandardQuery

end.
