//==============================================================================
//  Unit: ExportFilterSQL
//
//  Description: This unit contains the SQL used in the Export Filter export
//    process.
//
//  Author: Michael Bailey
//  Created: Nov 2002
//
//  Last Revision Details:
//    $Revision: 12 $
//    $Date: 8/04/09 15:43 $
//    $Author: Ericsalmon $
//
//  $History: ExportFilterSQL.pas $
//  
//  *****************  Version 12  *****************
//  User: Ericsalmon   Date: 8/04/09    Time: 15:43
//  Updated in $/JNCC/Components
//  18981 - Problem with duplicate keys coming from Name table, because
//  Individual and Organisation queries were not filtered properly.
//  
//  *****************  Version 11  *****************
//  User: Johnvanbreda Date: 3/11/06    Time: 9:24
//  Updated in $/JNCC/Components
//  IR12598
//  Fixed individuals date filtration for export filter.
//  
//  *****************  Version 10  *****************
//  User: Ericsalmon   Date: 30/05/03   Time: 16:54
//  Updated in $/JNCC/Components
//  IR576. Fixed what was supposedly already fixed!!!
//  
//  *****************  Version 9  *****************
//  User: Johnvanbreda Date: 7/05/03    Time: 10:28
//  Updated in $/JNCC/Components
//  IR576
//  Export filter performance fixed.
//  
//  *****************  Version 8  *****************
//  User: Ericsalmon   Date: 27/03/03   Time: 16:35
//  Updated in $/JNCC/Components
//  IR544, ExportFilter.
//  Problem when date not specified. SQL split and date filters added when
//  and where needed only.
//  
//  *****************  Version 7  *****************
//  User: Ericsalmon   Date: 6/02/03    Time: 11:10
//  Updated in $/JNCC/Components
//  IR 445 fixed. Improved speed and code.
//  
//  *****************  Version 6  *****************
//  User: Michaelbailey Date: 18/12/02   Time: 15:25
//  Updated in $/JNCC/Components
//  
//  *****************  Version 5  *****************
//  User: Johnvanbreda Date: 3/12/02    Time: 12:48
//  Updated in $/JNCC/Components
//  CEDaR10
//  bugfixes
//  
//  *****************  Version 4  *****************
//  User: Michaelbailey Date: 2/12/02    Time: 11:23
//  Updated in $/JNCC/Components
//  
//  *****************  Version 3  *****************
//  User: Michaelbailey Date: 29/11/02   Time: 10:44
//  Updated in $/JNCC/Components
//  Bounding Box and Date constraints added to export filters.
//  
//  *****************  Version 2  *****************
//  User: Ericsalmon   Date: 21/11/02   Time: 16:03
//  Updated in $/JNCC/Components
//  Changed table name TAXON_GROUP_INDEX to INDEX_TAXON_GROUP
//  
//  *****************  Version 1  *****************
//  User: Michaelbailey Date: 21/11/02   Time: 14:47
//  Created in $/JNCC/Source
//  CCN No: CEDaR 10
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================


unit ExportFilterSQL;

interface

const
  SQL_TOCC_SELECT =
    'SELECT DISTINCT xocc.taxon_occurrence_key as ItemKey, ''TAXON_OCCURRENCE'' as TableName '+
    'FROM taxon_occurrence AS xocc ';

  SQL_BOCC_SELECT =
    'SELECT DISTINCT xocc.biotope_occurrence_key as ItemKey, ''BIOTOPE_OCCURRENCE'' as TableName '+
    'FROM biotope_occurrence AS xocc ';

  SQL_SAMPLE_JOIN =
    'INNER JOIN Sample AS s on S.Sample_key=xocc.sample_key ';

  SQL_EVENT_JOIN =
    'INNER JOIN survey_event AS se ON se.survey_event_key = s.survey_event_key ';

  SQL_TOCC_DETERMINATION_JOIN =
    'INNER JOIN taxon_determination AS xd ON xd.taxon_occurrence_key = xocc.taxon_occurrence_key ';

  SQL_BOCC_DETERMINATION_JOIN =
    'INNER JOIN biotope_determination AS xd ON xd.biotope_occurrence_key = xocc.biotope_occurrence_key ';

  SQL_TOCC_TAXA_JOIN =
    'INNER JOIN Index_Taxon_Synonym AS its ON its.synonym_list_item_key = xd.taxon_list_item_key ' +
    'INNER JOIN Index_Taxon_Group AS itg ON itg.contained_list_item_key = its.taxon_list_item_key '+
    'INNER JOIN Index_Taxon_Synonym taxfilter ON taxfilter.synonym_list_item_key = itg.taxon_list_item_key ';

  SQL_TOCC_TAXA_JOIN_NAMESERVER =
    'INNER JOIN Index_Taxon_Name AS itn ON itn.taxon_list_item_key = xd.taxon_list_item_key ' +
    'INNER JOIN Index_Taxon_Name AS itn2 ON itn2.recommended_taxon_list_item_key = itn.recommended_taxon_list_item_key '+
    'INNER JOIN Index_Taxon_Group AS itg ON itg.contained_list_item_key = itn2.taxon_list_item_key '+
    'INNER JOIN Index_Taxon_Name AS itn3 ON itn3.taxon_list_item_key = itg.taxon_list_item_key ' +
    'INNER JOIN Index_Taxon_Name AS taxfilter ON taxfilter.recommended_taxon_list_item_key = itn3.recommended_taxon_list_item_key ';

  SQL_TOCC_DATE_JOIN =
    'LEFT JOIN taxon_occurrence_data AS xod ON xocc.taxon_occurrence_key = xod.taxon_occurrence_key ' +
    'LEFT JOIN specimen AS sp ON xocc.taxon_occurrence_key = sp.taxon_occurrence_key ' +
    'LEFT JOIN taxon_occurrence_relation AS tor1 ON xocc.taxon_occurrence_key = tor1.taxon_occurrence_key_1 ' +
    'LEFT JOIN taxon_occurrence_relation AS tor2 ON xocc.taxon_occurrence_key = tor2.taxon_occurrence_key_2 ' +
    'LEFT JOIN substrate AS su ON su.substrate_key = xocc.substrate_key ' +
    'LEFT JOIN record_type AS rt ON rt.record_type_key = xocc.record_type_key ';
  SQL_TOCC_DATE_WHERE =
    '(xocc.entry_date >= :Date_Param OR xocc.changed_date >= :Date_Param ' +
    'OR xod.entry_date >= :Date_Param OR xod.changed_date >= :Date_Param ' +
    'OR xd.entry_date >= :Date_Param OR xd.changed_date >= :Date_Param ' +
    'OR sp.entry_date >= :Date_Param OR sp.changed_date >= :Date_Param ' +
    'OR tor1.entry_date >= :Date_Param OR tor1.changed_date >= :Date_Param ' +
    'OR tor2.entry_date >= :Date_Param OR tor2.changed_date >= :Date_Param ' +
    'OR su.entry_date >= :Date_Param OR su.changed_date >= :Date_Param ' +
    'OR rt.entry_date >= :Date_Param OR rt.changed_date >= :Date_Param) ';

  SQL_BOCC_DATE_JOIN =
    'LEFT JOIN biotope_occurrence_data AS xod ON xocc.biotope_occurrence_key = xod.biotope_occurrence_key ';
  SQL_BOCC_DATE_WHERE =
    '(xocc.entry_date >= :Date_Param OR xocc.changed_date >= :Date_Param ' +
    'OR xod.entry_date >= :Date_Param OR xod.changed_date >= :Date_Param '+
    'OR xd.entry_date >= :Date_Param OR xd.changed_date >= :Date_Param) ';

  SQL_SAMPLES_DATE_JOIN =
    'LEFT JOIN sample_recorder AS sr ON s.sample_key = sr.sample_key ' +
    'LEFT JOIN sample_data AS sd ON s.sample_key = sd.sample_key ' +
    'LEFT JOIN sample_type AS st ON st.sample_type_key = s.sample_type_key ' +
    'LEFT JOIN sample_relation AS sr1 ON s.sample_key = sr1.sample_key_1 ' +
    'LEFT JOIN sample_relation AS sr2 ON s.sample_key = sr2.sample_key_2 ';
  SQL_SAMPLES_DATE_WHERE =
    '(s.entry_date >= :Date_Param OR  s.changed_date >= :Date_Param ' +
    'OR sr.entry_date >= :Date_Param ' +
    'OR sd.entry_date >= :Date_Param OR sd.changed_date >= :Date_Param ' +
    'OR st.entry_date >= :Date_Param OR st.changed_date >= :Date_Param ' +
    'OR sr1.entry_date >= :Date_Param ' +
    'OR sr2.entry_date >= :Date_Param) ';

  SQL_EVENTS_DATE_JOIN =
    'LEFT JOIN survey_event_recorder AS ser ON se.survey_event_key = ser.survey_event_key ';
  SQL_EVENTS_DATE_WHERE =
    '(ser.entry_date >= :Date_Param OR ser.changed_date >= :Date_Param ' +
    'OR se.entry_date >= :Date_Param OR se.changed_date >= :Date_Param) ';


  //----------------------------------------------------------------------------
  // preset where clauses
  SQL_SURVEY_WHERE =
    'se.survey_key IN (%s) ';

  SQL_TAXA_WHERE  =
    'taxfilter.taxon_list_item_key IN (%s) ';

  SQL_LAT_LONG_WHERE =
    '(s.lat > %s) AND (s.lat < %s) AND (s.long > %s) AND (s.long < %s) ';

  SQL_OBS_DATE_START_WHERE =
    's.vague_date_end >= %s ';

  SQL_OBS_DATE_END_WHERE =
    's.vague_date_start <= %s ';

  //----------------------------------------------------------------------------
  SQL_INDIVIDUALS_SELECT =
    'SELECT DISTINCT n.name_key AS ItemKey, ''Name'' AS TableName '
    + 'FROM name AS n '
    + 'INNER JOIN individual AS ind ON n.name_key = ind.name_key AND n.Organisation = 0';
  SQL_INDIVIDUALS_DATE_JOIN=
    'LEFT JOIN address AS adr ON adr.name_key = n.name_key ' +
    'LEFT JOIN contact_number AS cn ON cn.name_key = n.name_key ';
  SQL_INDIVIDUALS_DATE_WHERE =
    '(n.entry_date >= :Date_Param OR n.changed_date >= :Date_Param ' +
    'OR adr.entry_date >= :Date_Param OR adr.changed_date >= :Date_Param ' +
    'OR cn.entry_date >= :Date_Param OR cn.changed_date >= :Date_Param ' +
    'OR ind.entry_date >= :Date_Param OR ind.changed_date >= :Date_Param) ';

  //----------------------------------------------------------------------------
  SQL_ORGANISATIONS_SELECT =
    'SELECT DISTINCT n.name_key AS ItemKey, ''Name'' AS TableName '
    + 'FROM name AS n '
    + 'INNER JOIN organisation AS org ON n.name_key = org.name_key AND n.Organisation = 1';
  SQL_ORGANISATIONS_DATE_JOIN =
    'LEFT JOIN organisation_type AS ot ON ot.organisation_type_key = org.organisation_type_key ' +
    'LEFT JOIN address AS a ON a.name_key = n.name_key ' +
    'LEFT JOIN contact_number AS cn ON cn.name_key = n.name_key ';
  SQL_ORGANISATIONS_DATE_WHERE =
    '(n.entry_date >= :Date_Param OR n.changed_date >= :Date_Param ' +
    'OR org.entry_date >= :Date_Param OR org.changed_date >= :Date_Param ' +
    'OR a.entry_date >= :Date_Param OR a.changed_date >= :Date_Param ' +
    'OR cn.entry_date >= :Date_Param OR cn.changed_date >= :Date_Param ' +
    'OR ot.entry_date >= :Date_Param OR ot.changed_date >= :Date_Param) ';

  //----------------------------------------------------------------------------
  SQL_LOCATIONS_SELECT =
    'SELECT DISTINCT l.location_key as ItemKey, ''LOCATION'' As TableName from location as l ';
  SQL_LOCATIONS_DATE_JOIN =
    'LEFT JOIN location_name AS ln ON ln.location_key = l.location_key ' +
    'LEFT JOIN location_feature AS lf ON lf.location_key = l.location_key ' +
    'LEFT JOIN location_designation AS lde ON lde.location_key = l.location_key ' +
    'LEFT JOIN location_data AS lda ON lda.location_key = l.location_key ' +
    'LEFT JOIN grid_square AS gs ON gs.location_key = l.location_key ' +
    'LEFT JOIN location_use AS lu ON lu.location_key = l.location_key ' +
    'LEFT JOIN land_parcel AS lp ON lp.location_key = l.location_key ' +
    'LEFT JOIN location_admin_areas AS laa ON laa.location_key = l.location_key ' +
    'LEFT JOIN location_boundary AS lb ON lb.location_key = l.location_key ' +
    'LEFT JOIN tenure AS t ON t.location_key = l.location_key ' +
    'LEFT JOIN location_relation AS lr1 ON lr1.location_key_1 = l.location_key ' +
    'LEFT JOIN location_relation AS lr2 ON lr2.location_key_2 = l.location_key ' +
    'LEFT JOIN location_type AS lt ON lt.location_type_key = l.location_type_key ';
  SQL_LOCATIONS_DATE_WHERE =
    '(l.Entry_Date >= :Date_Param OR l.Changed_Date >= :Date_Param ' +
    'OR ln.Entry_Date >= :Date_Param OR ln.Changed_Date >= :Date_Param ' +
    'OR lf.Entry_Date >= :Date_Param OR lf.Changed_Date >= :Date_Param ' +
    'OR lde.Entry_Date >= :Date_Param OR lde.Changed_Date >= :Date_Param ' +
    'OR lda.Entry_Date >= :Date_Param OR lda.Changed_Date >= :Date_Param ' +
    'OR gs.Entry_Date >= :Date_Param OR gs.Changed_Date >= :Date_Param ' +
    'OR lu.Entry_Date >= :Date_Param OR lu.Changed_Date >= :Date_Param ' +
    'OR lp.Entry_Date >= :Date_Param OR lp.Changed_Date >= :Date_Param ' +
    'OR laa.Entry_Date >= :Date_Param ' +
    'OR lb.Entry_Date >= :Date_Param OR lb.Changed_Date >= :Date_Param ' +
    'OR t.Entry_Date >= :Date_Param OR t.Changed_Date >= :Date_Param ' +
    'OR lr1.Entry_Date >= :Date_Param OR lr1.Changed_Date >= :Date_Param ' +
    'OR lr2.Entry_Date >= :Date_Param OR lr2.Changed_Date >= :Date_Param ' +
    'OR lt.Entry_Date >= :Date_Param OR lt.Changed_Date >= :Date_Param) ';
  SQL_LOCATIONS_BOUNDING_BOX_WHERE =
    '(l.lat > %s) AND (l.lat < %s) AND (l.long > %s) AND (l.long < %s) ';
  SQL_LOCATIONS_BY_SAMPLE_SURVEY_JOIN =
    'INNER JOIN sample AS sm ON sm.location_key = l.location_key '+
    'INNER JOIN survey_event AS se ON se.survey_event_key = sm.survey_event_key ';
  SQL_LOCATIONS_BY_EVENT_SURVEY_JOIN =
    'INNER JOIN survey_event AS se ON se.location_key = l.location_key ';
  SQL_LOCATIONS_BY_SURVEY_WHERE =
    'se.survey_key IN (%s) ';



implementation

end.
