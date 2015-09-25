//================================================================================
// Author:  Stuart Ball     stuart.ball@jncc.gov.uk
// Date:    Feb 2009
// Purpose: Objects to maintain the list of items that are to be checked.
//          This handles the SQL queries which find SURVEY_EVENT and SAMPLEs
//          which have OSGB grid refs and VC info available and so can be
//          checked.
//--------------------------------------------------------------------------------
//================================================================================
unit ItemsList;

interface

uses
  Classes, ADODB, SysUtils, Windows;

type
  TCallBackType = (cbInit, cbPos, cbDone);
  TOnCallBack = procedure(Sender: TObject; const aType: TCallBackType; const n: Longint) of Object;

  TInfo = class(TObject)
  private
    FKey: string;
    FTable: string;
    FItemLabel: string;
    FTableIndex: integer;
    FSelected: boolean;
    FSamples: Longint;
    FEvents: Longint;
    FQuery: TADOQuery;      // Query on the database
    procedure set_Table(const Value: string);
    procedure set_Key(const Value: string);
    function QueryString(const SQL, sKey: string): string;
    function QueryNumber(const SQL, sKey: string): integer;
    function MultipleParameters(const SQL, sKey: string): string;
    function get_Events: TStrings;
    function get_Samples: TStrings;
  public
    Constructor Create(const aTable, aKey: string; aQuery: TADOQuery);
    property Key: string read FKey write set_Key;
    property Table: string read FTable write set_Table;
    property ItemLabel: string read FItemLabel;
    property nSamples: Longint read FSamples;
    property nEvents: Longint read FEvents;
    property TableIndex: integer read FTableIndex;
    property Selected: boolean read FSelected write FSelected;
    property Samples: TStrings read get_Samples;
    property Events: TStrings read get_Events;
  end;

  TInfoList = class(TList)
  private
    FQuery: TADOQuery;
    FTotalSamples: Longint;
    FTotalEvents: Longint;
    function get_EventCount(i: integer): string;
    function get_ItemLabel(i: integer): string;
    function get_SampleCount(i: integer): string;
    function get_TypeLabel(i: integer): string;
    function get_Selcted(i: integer): boolean;
    procedure set_selected(i: integer; const Value: boolean);
    function Get_Events: string;
    function Get_Samples: string;
    function CommaDelim(oString: TStrings): string;      // Query on the database
  protected
    FOnCallBack: TOnCallBack;
  public
    Constructor Create(aQuery: TADOQuery);
    Destructor Destroy; override;
    procedure Clear; override;
    property Samples: string read Get_Samples;
    property Events: string read Get_Events;
    property TotalSamples: Longint read FTotalSamples;
    property TotalEvents: Longint read FTotalEvents;
    property ItemLabel[i: integer]: string read get_ItemLabel;
    property TypeLabel[i: integer]: string read get_TypeLabel;
    property SampleCount[i: integer]: string read get_SampleCount;
    property EventCount[i: integer]: string read get_EventCount;
    property Selected[i: integer]: boolean read get_Selcted write set_selected;
    property OnCallBack: TOnCallBack read FOnCallBack write FOnCallBack;
    function AddItem(const aTable, aKey: string): integer;
    function FindItem(const aTable, aKey: string): integer;
  end;

  Function GetTableIndex(const sName: string): integer;

// this constant needs to be exposed because it is used in registering
// the drag-drop components
const
  // these are the tables we will accept the user dropping into the grid
  // Note - they MUST be sorted and the following arrays MUST be in the
  // same order!
  DROP_TABLES : array [1..12] of string = ( 'ADMIN_AREA',
                                            'BIOTOPE_LIST_ITEM',
                                            'BIOTOPE_OCCURRENCE',
                                            'INDIVIDUAL',
                                            'LOCATION',
                                            'ORGANISATION',
                                            'REFERENCE',
                                            'SAMPLE',
                                            'SURVEY',
                                            'SURVEY_EVENT',
                                            'TAXON_LIST_ITEM',
                                            'TAXON_OCCURRENCE'
                                            );

//=============================================================================
implementation

//-----------------------------------------------------------------------------
// the rest of the constants are private to these objects
//-----------------------------------------------------------------------------
const
  ALL_SAMPLES = 'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN SAMPLE ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SAMPLE.LOCATION_KEY ' +
                'WHERE (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') AND (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (LEN(SAMPLE.SPATIAL_REF) >= 4)';
  SAMPLES_SELECT = 'SELECT SAMPLE.SAMPLE_KEY ';
  SAMPLES_COUNT = 'SELECT COUNT(DISTINCT SAMPLE.SAMPLE_KEY) AS ItemName ';
  SAMPLE_GROUP = 'GROUP BY SAMPLE.SAMPLE_KEY';
  ALL_EVENTS  = 'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN SAMPLE ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SAMPLE.LOCATION_KEY ' +
                'INNER JOIN SURVEY_EVENT ON SAMPLE.SURVEY_EVENT_KEY = SURVEY_EVENT.SURVEY_EVENT_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (SURVEY_EVENT.SPATIAL_REF_SYSTEM = ''OSGB'') ' +
                'AND (LEN(SURVEY_EVENT.SPATIAL_REF) >= 4)AND (LEN(SAMPLE.SPATIAL_REF) >= 4)';
  EVENT_SELECT = 'SELECT SURVEY_EVENT.SURVEY_EVENT_KEY ';
  EVENT_COUNT = 'SELECT COUNT(DISTINCT SURVEY_EVENT.SURVEY_EVENT_KEY) AS ItemName ';
  EVENT_GROUP = 'GROUP BY SURVEY_EVENT.SURVEY_EVENT_KEY';
  // corresponding labels for "Type"
  DROP_TYPE   : array [1..12] of string = ( 'Admin. area',
                                            'Biotope',
                                            'Biotope record',
                                            'Person',
                                            'Location',
                                            'Organisation',
                                            'Document',
                                            'Sample',
                                            'Survey',
                                            'Event',
                                            'Species',
                                            'Species record'
                                            );
  //--------------------------------------------------------------------------
  // corresponding SQL statements to get the name of the item
  //--------------------------------------------------------------------------
  ITEM_SQL : array [1..12] of string =     ('SELECT ITEM_NAME as ItemName FROM ADMIN_AREA WHERE (ADMIN_AREA_KEY = ''%s'')',
                                            'SELECT BIOTOPE.SHORT_TERM as ItemName ' +
                                                'FROM BIOTOPE INNER JOIN BIOTOPE_LIST_ITEM ON BIOTOPE.BIOTOPE_KEY = BIOTOPE_LIST_ITEM.BIOTOPE_KEY ' +
                                                'WHERE (BIOTOPE_LIST_ITEM.BIOTOPE_LIST_ITEM_KEY = ''%s'')',
                                            'SELECT BIOTOPE.SHORT_TERM AS ItemName ' +
                                                'FROM BIOTOPE INNER JOIN BIOTOPE_LIST_ITEM ON BIOTOPE.BIOTOPE_KEY = BIOTOPE_LIST_ITEM.BIOTOPE_KEY INNER JOIN ' +
                                                'BIOTOPE_DETERMINATION ON BIOTOPE_LIST_ITEM.BIOTOPE_LIST_ITEM_KEY = BIOTOPE_DETERMINATION.BIOTOPE_LIST_ITEM_KEY INNER JOIN ' +
                                                'BIOTOPE_OCCURRENCE ON BIOTOPE_DETERMINATION.BIOTOPE_OCCURRENCE_KEY = BIOTOPE_OCCURRENCE.BIOTOPE_OCCURRENCE_KEY ' +
                                                'WHERE (BIOTOPE_DETERMINATION.PREFERRED = 1) AND (BIOTOPE_OCCURRENCE.BIOTOPE_OCCURRENCE_KEY = ''%s'')',
                                            'SELECT dbo.ufn_GetFormattedName(''%s'') as ItemName',
                                            'SELECT ITEM_NAME AS ItemName FROM LOCATION_NAME WHERE (PREFERRED = 1) AND (LOCATION_KEY = ''%s'')',
                                            'SELECT dbo.ufn_GetFormattedName(''%s'') as ItemName',
                                            'SELECT dbo.ufn_GetFormattedReferenceName(''%s'') as ItemName',
                                            'SELECT CASE ' +
                                                  'WHEN ITEM_NAME Is Not Null THEN dbo.LCReturnVagueDateShort(VAGUE_DATE_START,VAGUE_DATE_END, VAGUE_DATE_TYPE) + '' - '' + ITEM_NAME ' +
		                                              'WHEN LOCATION_NAME is not null THEN dbo.LCReturnVagueDateShort(VAGUE_DATE_START,VAGUE_DATE_END, VAGUE_DATE_TYPE) + '' - '' + LOCATION_NAME ' +
                                                'ELSE dbo.LCReturnVagueDateShort(VAGUE_DATE_START,VAGUE_DATE_END, VAGUE_DATE_TYPE) + '' - '' + SPATIAL_REF ' +
                                                'END as ItemName ' +
                                                'FROM SAMPLE LEFT OUTER JOIN LOCATION_NAME ON SAMPLE.LOCATION_KEY = LOCATION_NAME.LOCATION_KEY ' +
                                                'WHERE  (PREFERRED = 1 OR PREFERRED IS NULL) AND (SAMPLE_KEY = ''%s'')',
                                            'SELECT ITEM_NAME as ItemName FROM SURVEY WHERE (SURVEY_KEY = ''%s'')',
                                            'SELECT CASE ' +
                                                  'WHEN ITEM_NAME Is Not Null THEN dbo.LCReturnVagueDateShort(VAGUE_DATE_START,VAGUE_DATE_END, VAGUE_DATE_TYPE) + '' - '' + ITEM_NAME ' +
		                                              'WHEN LOCATION_NAME is not null THEN dbo.LCReturnVagueDateShort(VAGUE_DATE_START,VAGUE_DATE_END, VAGUE_DATE_TYPE) + '' - '' + LOCATION_NAME ' +
                                                'ELSE dbo.LCReturnVagueDateShort(VAGUE_DATE_START,VAGUE_DATE_END, VAGUE_DATE_TYPE) + '' - '' + SPATIAL_REF ' +
                                                'END as ItemName ' +
                                                'FROM SURVEY_EVENT LEFT OUTER JOIN LOCATION_NAME ON SURVEY_EVENT.LOCATION_KEY = LOCATION_NAME.LOCATION_KEY ' +
                                                'WHERE  (PREFERRED = 1 OR PREFERRED IS NULL) AND (SURVEY_EVENT_KEY = ''%s'')',
                                            'SELECT dbo.ufn_GetFormattedSpeciesName(''%s'') as ItemName',
                                            'SELECT dbo.ufn_GetFormattedSpeciesName(TAXON_DETERMINATION.TAXON_LIST_ITEM_KEY) AS ItemName ' +
                                                'FROM TAXON_OCCURRENCE INNER JOIN TAXON_DETERMINATION ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY ' +
                                                'WHERE (TAXON_DETERMINATION.PREFERRED = 1) AND (TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = ''%s'')'
                                            );
  //--------------------------------------------------------------------------
  // corresponding SQL to get the SAMPLE_KEYs - the SELECT clause will be added
  // to the front depending on whether we want the keys or a count
  //--------------------------------------------------------------------------
  SAMPLE_SQL : array [1..12] of string =    ('FROM ADMIN_AREA AS ADMIN_AREA_1 INNER JOIN SAMPLE INNER JOIN LOCATION_ADMIN_AREAS AS LOCATION_ADMIN_AREAS_1 ON SAMPLE.LOCATION_KEY = ' +
                                                'LOCATION_ADMIN_AREAS_1.LOCATION_KEY ON ADMIN_AREA_1.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS_1.ADMIN_AREA_KEY INNER JOIN LOCATION_ADMIN_AREAS ' +
                                                'ON SAMPLE.LOCATION_KEY = LOCATION_ADMIN_AREAS.LOCATION_KEY WHERE (ADMIN_AREA_1.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND ' +
                                                '(LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY = ''%s'') AND (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SAMPLE.SPATIAL_REF) >= 4)',
                                             'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN BIOTOPE_DETERMINATION ' +
                                                'INNER JOIN BIOTOPE_OCCURRENCE ON BIOTOPE_DETERMINATION.BIOTOPE_OCCURRENCE_KEY = BIOTOPE_OCCURRENCE.BIOTOPE_OCCURRENCE_KEY INNER JOIN SAMPLE ' +
                                                'ON BIOTOPE_OCCURRENCE.SAMPLE_KEY = SAMPLE.SAMPLE_KEY ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SAMPLE.LOCATION_KEY WHERE (LEN(SAMPLE.SPATIAL_REF) >= 4) ' +
                                                'AND (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') AND (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (BIOTOPE_DETERMINATION.BIOTOPE_LIST_ITEM_KEY = ''%s'') ' +
                                                'AND (BIOTOPE_DETERMINATION.PREFERRED = 1)',
                                             'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN BIOTOPE_OCCURRENCE INNER JOIN SAMPLE ' +
                                                'ON BIOTOPE_OCCURRENCE.SAMPLE_KEY = SAMPLE.SAMPLE_KEY ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SAMPLE.LOCATION_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') ' +
                                                'AND (BIOTOPE_OCCURRENCE.BIOTOPE_OCCURRENCE_KEY = ''%s'') AND (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SAMPLE.SPATIAL_REF) >= 4)',
                                             'FROM SAMPLE_RECORDER INNER JOIN SAMPLE ON SAMPLE_RECORDER.SAMPLE_KEY = SAMPLE.SAMPLE_KEY INNER JOIN SURVEY_EVENT_RECORDER ON SAMPLE_RECORDER.SE_RECORDER_KEY = SURVEY_EVENT_RECORDER.SE_RECORDER_KEY ' +
                                                'INNER JOIN ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY ON SAMPLE.LOCATION_KEY = LOCATION_ADMIN_AREAS.LOCATION_KEY ' +
                                                'WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SAMPLE.SPATIAL_REF) >= 4) AND (SURVEY_EVENT_RECORDER.NAME_KEY = ''%s'')',
                                             'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN SAMPLE ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SAMPLE.LOCATION_KEY ' +
                                                'WHERE (LEN(SAMPLE.SPATIAL_REF) >= 4) AND (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') AND (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (LOCATION_ADMIN_AREAS.LOCATION_KEY = ''%s'')',
                                             'FROM SAMPLE_RECORDER INNER JOIN SAMPLE ON SAMPLE_RECORDER.SAMPLE_KEY = SAMPLE.SAMPLE_KEY INNER JOIN SURVEY_EVENT_RECORDER ON SAMPLE_RECORDER.SE_RECORDER_KEY = SURVEY_EVENT_RECORDER.SE_RECORDER_KEY ' +
                                                'INNER JOIN ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY ON SAMPLE.LOCATION_KEY = LOCATION_ADMIN_AREAS.LOCATION_KEY ' +
                                                'WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SAMPLE.SPATIAL_REF) >= 4) AND (SURVEY_EVENT_RECORDER.NAME_KEY = ''%s'')',
                                             'FROM BIOTOPE_OCCURRENCE LEFT OUTER JOIN BIOTOPE_OCCURRENCE_SOURCES ON BIOTOPE_OCCURRENCE.BIOTOPE_OCCURRENCE_KEY = BIOTOPE_OCCURRENCE_SOURCES.BIOTOPE_OCCURRENCE_KEY RIGHT OUTER JOIN ADMIN_AREA ' +
                                                'INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN SAMPLE ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SAMPLE.LOCATION_KEY ' +
                                                'INNER JOIN SURVEY_EVENT ON SAMPLE.SURVEY_EVENT_KEY = SURVEY_EVENT.SURVEY_EVENT_KEY INNER JOIN SURVEY ON SURVEY_EVENT.SURVEY_KEY = SURVEY.SURVEY_KEY LEFT OUTER JOIN SURVEY_SOURCES ' +
                                                'ON SURVEY.SURVEY_KEY = SURVEY_SOURCES.SURVEY_KEY LEFT OUTER JOIN SURVEY_EVENT_SOURCES ON SURVEY_EVENT.SURVEY_EVENT_KEY = SURVEY_EVENT_SOURCES.SURVEY_EVENT_KEY LEFT OUTER JOIN SAMPLE_SOURCES ' +
                                                'ON SAMPLE.SAMPLE_KEY = SAMPLE_SOURCES.SAMPLE_KEY ON BIOTOPE_OCCURRENCE.SAMPLE_KEY = SAMPLE.SAMPLE_KEY LEFT OUTER JOIN TAXON_OCCURRENCE_SOURCES LEFT OUTER JOIN TAXON_OCCURRENCE ' +
                                                'ON TAXON_OCCURRENCE_SOURCES.TAXON_OCCURRENCE_KEY = TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY ' +
                                                'WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SAMPLE.SPATIAL_REF) >= 4) AND ((SAMPLE_SOURCES.SOURCE_LINK_KEY = ''%s'') ' +
                                                'OR (SURVEY_EVENT_SOURCES.SOURCE_LINK_KEY = ''%s'') OR (SURVEY_SOURCES.SOURCE_LINK_KEY = ''%s'') OR (BIOTOPE_OCCURRENCE_SOURCES.SOURCE_LINK_KEY = ''%s'') OR (TAXON_OCCURRENCE_SOURCES.SOURCE_LINK_KEY = ''%s''))',
                                             'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN SAMPLE ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SAMPLE.LOCATION_KEY ' +
                                                'WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (SAMPLE.SAMPLE_KEY = ''%s'') AND (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SAMPLE.SPATIAL_REF) >= 4)',
                                             'FROM SAMPLE INNER JOIN SURVEY_EVENT ON SAMPLE.SURVEY_EVENT_KEY = SURVEY_EVENT.SURVEY_EVENT_KEY INNER JOIN ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = ' +
                                                'LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY ON SAMPLE.LOCATION_KEY = LOCATION_ADMIN_AREAS.LOCATION_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (SURVEY_EVENT.SURVEY_KEY = ''%s'') ' +
                                                'AND (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SAMPLE.SPATIAL_REF) >= 4)',
                                             'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN SAMPLE ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SAMPLE.LOCATION_KEY ' +
                                                'INNER JOIN SURVEY_EVENT ON SAMPLE.SURVEY_EVENT_KEY = SURVEY_EVENT.SURVEY_EVENT_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') ' +
                                                'AND (LEN(SAMPLE.SPATIAL_REF) >= 4) AND (SURVEY_EVENT.SURVEY_EVENT_KEY = ''%s'')',
                                             'FROM SAMPLE INNER JOIN TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY INNER JOIN TAXON_DETERMINATION ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY ' +
                                                'INNER JOIN INDEX_TAXON_GROUP ON TAXON_DETERMINATION.TAXON_LIST_ITEM_KEY = INDEX_TAXON_GROUP.CONTAINED_LIST_ITEM_KEY INNER JOIN INDEX_TAXON_SYNONYM ' +
                                                'ON INDEX_TAXON_GROUP.TAXON_LIST_ITEM_KEY = INDEX_TAXON_SYNONYM.SYNONYM_LIST_ITEM_KEY INNER JOIN ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY ' +
                                                'ON SAMPLE.LOCATION_KEY = LOCATION_ADMIN_AREAS.LOCATION_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (TAXON_DETERMINATION.PREFERRED = 1) AND (INDEX_TAXON_SYNONYM.TAXON_LIST_ITEM_KEY = ''%s'') ' +
                                                'AND (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SAMPLE.SPATIAL_REF) >= 4)',
                                             'FROM SAMPLE INNER JOIN ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY ON SAMPLE.LOCATION_KEY = LOCATION_ADMIN_AREAS.LOCATION_KEY ' +
                                                'INNER JOIN TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') ' +
                                                'AND (LEN(SAMPLE.SPATIAL_REF) >= 4) AND (TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = ''%s'')'
                                            );

  //--------------------------------------------------------------------------
  // corresponding SQL to get the SURVEY_EVENT_KEYs - the SELECT clause will be added
  // to the front depending on whether we want the keys or a count
  //--------------------------------------------------------------------------
  EVENT_SQL : array [1..12] of string =     ('FROM LOCATION_ADMIN_AREAS INNER JOIN SURVEY_EVENT INNER JOIN LOCATION_ADMIN_AREAS AS LOCATION_ADMIN_AREAS_1 ON SURVEY_EVENT.LOCATION_KEY = LOCATION_ADMIN_AREAS_1.LOCATION_KEY ' +
                                               'INNER JOIN ADMIN_AREA AS ADMIN_AREA_1 ON LOCATION_ADMIN_AREAS_1.ADMIN_AREA_KEY = ADMIN_AREA_1.ADMIN_AREA_KEY ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SURVEY_EVENT.LOCATION_KEY ' +
                                               'INNER JOIN SAMPLE ON SURVEY_EVENT.SURVEY_EVENT_KEY = SAMPLE.SURVEY_EVENT_KEY WHERE (ADMIN_AREA_1.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY = ''%s'') ' +
                                               'AND (SURVEY_EVENT.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SURVEY_EVENT.SPATIAL_REF) >= 4)',
                                             'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN BIOTOPE_DETERMINATION ' +
                                               'INNER JOIN BIOTOPE_OCCURRENCE ON BIOTOPE_DETERMINATION.BIOTOPE_OCCURRENCE_KEY = BIOTOPE_OCCURRENCE.BIOTOPE_OCCURRENCE_KEY INNER JOIN SAMPLE ' +
                                               'ON BIOTOPE_OCCURRENCE.SAMPLE_KEY = SAMPLE.SAMPLE_KEY ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SAMPLE.LOCATION_KEY INNER JOIN SURVEY_EVENT ' +
                                               'ON SAMPLE.SURVEY_EVENT_KEY = SURVEY_EVENT.SURVEY_EVENT_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (BIOTOPE_DETERMINATION.BIOTOPE_LIST_ITEM_KEY = ''%s'') ' +
                                               'AND (SURVEY_EVENT.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SURVEY_EVENT.SPATIAL_REF) >= 4) AND (BIOTOPE_DETERMINATION.PREFERRED = 1)',
                                             'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN BIOTOPE_OCCURRENCE INNER JOIN SAMPLE ' +
                                               'ON BIOTOPE_OCCURRENCE.SAMPLE_KEY = SAMPLE.SAMPLE_KEY ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SAMPLE.LOCATION_KEY INNER JOIN SURVEY_EVENT ' +
                                               'ON SAMPLE.SURVEY_EVENT_KEY = SURVEY_EVENT.SURVEY_EVENT_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (SURVEY_EVENT.SPATIAL_REF_SYSTEM = ''OSGB'') ' +
                                               'AND (LEN(SURVEY_EVENT.SPATIAL_REF) >= 4) AND (BIOTOPE_OCCURRENCE.BIOTOPE_OCCURRENCE_KEY = ''%s'')',
                                             'FROM SAMPLE INNER JOIN ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY ' +
                                               'ON SAMPLE.LOCATION_KEY = LOCATION_ADMIN_AREAS.LOCATION_KEY INNER JOIN SURVEY_EVENT ON SAMPLE.SURVEY_EVENT_KEY = SURVEY_EVENT.SURVEY_EVENT_KEY INNER JOIN SURVEY_EVENT_RECORDER ' +
                                               'ON SURVEY_EVENT.SURVEY_EVENT_KEY = SURVEY_EVENT_RECORDER.SURVEY_EVENT_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (SURVEY_EVENT_RECORDER.NAME_KEY = ''%s'') ' +
                                               'AND (SURVEY_EVENT.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SURVEY_EVENT.SPATIAL_REF) >= 4)',
                                             'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN SURVEY_EVENT ' +
                                               'ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SURVEY_EVENT.LOCATION_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') ' +
                                               'AND (LOCATION_ADMIN_AREAS.LOCATION_KEY = ''%s'') AND (LEN(SURVEY_EVENT.SPATIAL_REF) >= 4) AND (SURVEY_EVENT.SPATIAL_REF_SYSTEM = ''OSGB'')',
                                             'FROM SAMPLE INNER JOIN ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY ' +
                                               'ON SAMPLE.LOCATION_KEY = LOCATION_ADMIN_AREAS.LOCATION_KEY INNER JOIN SURVEY_EVENT ON SAMPLE.SURVEY_EVENT_KEY = SURVEY_EVENT.SURVEY_EVENT_KEY INNER JOIN SURVEY_EVENT_RECORDER ' +
                                               'ON SURVEY_EVENT.SURVEY_EVENT_KEY = SURVEY_EVENT_RECORDER.SURVEY_EVENT_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (SURVEY_EVENT_RECORDER.NAME_KEY = ''%s'') ' +
                                               'AND (SURVEY_EVENT.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SURVEY_EVENT.SPATIAL_REF) >= 4)',
                                             'FROM BIOTOPE_OCCURRENCE LEFT OUTER JOIN BIOTOPE_OCCURRENCE_SOURCES ON BIOTOPE_OCCURRENCE.BIOTOPE_OCCURRENCE_KEY = BIOTOPE_OCCURRENCE_SOURCES.BIOTOPE_OCCURRENCE_KEY RIGHT OUTER JOIN SURVEY_EVENT ' +
                                                'INNER JOIN SAMPLE ON SURVEY_EVENT.SURVEY_EVENT_KEY = SAMPLE.SURVEY_EVENT_KEY INNER JOIN SURVEY ON SURVEY_EVENT.SURVEY_KEY = SURVEY.SURVEY_KEY INNER JOIN ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ' +
                                                'ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY ON SURVEY_EVENT.LOCATION_KEY = LOCATION_ADMIN_AREAS.LOCATION_KEY LEFT OUTER JOIN SURVEY_SOURCES ON SURVEY.SURVEY_KEY = SURVEY_SOURCES.SURVEY_KEY ' +
                                                'LEFT OUTER JOIN SURVEY_EVENT_SOURCES ON SURVEY_EVENT.SURVEY_EVENT_KEY = SURVEY_EVENT_SOURCES.SURVEY_EVENT_KEY LEFT OUTER JOIN SAMPLE_SOURCES ON SAMPLE.SAMPLE_KEY = SAMPLE_SOURCES.SAMPLE_KEY ' +
                                                'ON BIOTOPE_OCCURRENCE.SAMPLE_KEY = SAMPLE.SAMPLE_KEY LEFT OUTER JOIN TAXON_OCCURRENCE_SOURCES LEFT OUTER JOIN TAXON_OCCURRENCE ON TAXON_OCCURRENCE_SOURCES.TAXON_OCCURRENCE_KEY = TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY ' +
                                                'ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND  (SURVEY_EVENT.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SURVEY_EVENT.SPATIAL_REF) >= 4) ' +
                                                'AND ((SAMPLE_SOURCES.SOURCE_LINK_KEY = ''%s'') or (SURVEY_EVENT_SOURCES.SOURCE_LINK_KEY = ''%s'') or (SURVEY_SOURCES.SOURCE_LINK_KEY = ''%s'') or (BIOTOPE_OCCURRENCE_SOURCES.SOURCE_LINK_KEY = ''%s'') ' +
                                                'or (TAXON_OCCURRENCE_SOURCES.SOURCE_LINK_KEY = ''%s''))',
                                             'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN SURVEY_EVENT ' +
                                                'ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SURVEY_EVENT.LOCATION_KEY INNER JOIN SAMPLE ON SURVEY_EVENT.SURVEY_EVENT_KEY = SAMPLE.SURVEY_EVENT_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') ' +
                                                'AND (LEN(SURVEY_EVENT.SPATIAL_REF) >= 4) AND (SURVEY_EVENT.SPATIAL_REF_SYSTEM = ''OSGB'') AND (SAMPLE.SAMPLE_KEY = ''%s'')',
                                             'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN SURVEY_EVENT ' +
                                                'ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SURVEY_EVENT.LOCATION_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') ' +
                                                'AND (SURVEY_EVENT.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SURVEY_EVENT.SPATIAL_REF) >= 4) AND (SURVEY_EVENT.SURVEY_KEY = ''%s'')',
                                             'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN SURVEY_EVENT ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SURVEY_EVENT.LOCATION_KEY ' +
                                                'WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (SURVEY_EVENT.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SURVEY_EVENT.SPATIAL_REF) >= 4) AND (SURVEY_EVENT.SURVEY_EVENT_KEY = ''%s'')',
                                             'FROM SAMPLE INNER JOIN TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY INNER JOIN SURVEY_EVENT ON SAMPLE.SURVEY_EVENT_KEY = SURVEY_EVENT.SURVEY_EVENT_KEY INNER JOIN ADMIN_AREA ' +
                                                'INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY ON SURVEY_EVENT.LOCATION_KEY = LOCATION_ADMIN_AREAS.LOCATION_KEY INNER JOIN TAXON_DETERMINATION ' +
                                                'ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY INNER JOIN INDEX_TAXON_GROUP ON TAXON_DETERMINATION.TAXON_LIST_ITEM_KEY = INDEX_TAXON_GROUP.CONTAINED_LIST_ITEM_KEY ' +
                                                'INNER JOIN INDEX_TAXON_SYNONYM ON INDEX_TAXON_GROUP.TAXON_LIST_ITEM_KEY = INDEX_TAXON_SYNONYM.SYNONYM_LIST_ITEM_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') ' +
                                                'AND (SURVEY_EVENT.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SURVEY_EVENT.SPATIAL_REF) >= 4) AND (TAXON_DETERMINATION.PREFERRED = 1) AND (INDEX_TAXON_SYNONYM.TAXON_LIST_ITEM_KEY = ''%s'')',
                                             'FROM SAMPLE INNER JOIN TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY INNER JOIN SURVEY_EVENT ON SAMPLE.SURVEY_EVENT_KEY = SURVEY_EVENT.SURVEY_EVENT_KEY INNER JOIN ADMIN_AREA ' +
                                                'INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY ON SURVEY_EVENT.LOCATION_KEY = LOCATION_ADMIN_AREAS.LOCATION_KEY WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') ' +
                                                'AND (TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = ''%s'') AND (SURVEY_EVENT.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SURVEY_EVENT.SPATIAL_REF) >= 4)'
                                            );

{ TInfo }

//--------------------------------------------------------------------------------
// Initialise
//--------------------------------------------------------------------------------
constructor TInfo.Create(const aTable, aKey: string; aQuery: TADOQuery);
begin
  inherited Create;
  FItemLabel := '';
  FTableIndex := -1;
  FSamples := 0;
  FEvents := 0;
  FQuery := aQuery;
  set_Table(aTable);
  set_Key(aKey);
end;

//--------------------------------------------------------------------------------
// Get the SURVEY_EVENTs associated with an item
//--------------------------------------------------------------------------------
function TInfo.get_Events: TStrings;
var sSQL: string;
begin
  Result := TStringList.Create;;

  if (Key <> '') and (TableIndex > 0) then
    sSQL := MultipleParameters(EVENT_SELECT + EVENT_SQL[TableIndex] + EVENT_GROUP, Key)
  else
    sSQL := EVENT_SELECT + ALL_EVENTS + EVENT_GROUP;

  with FQuery do
  begin
    SQL.Clear;
    SQL.Add(sSQL);
    try
      Open;
      If RecordCount > 0 then
        while not EoF do
        begin
          Result.Add(Trim(FieldByName('SURVEY_EVENT_KEY').AsString));
          Next;
        end;
    finally
      Close;
    end;
  end;
end;

//--------------------------------------------------------------------------------
// Get the SAMPLEs associated with a given item
//--------------------------------------------------------------------------------
function TInfo.get_Samples: TStrings;
var sSQL: string;
begin
  Result := TStringList.Create;;

  if (Key <> '') and (TableIndex > 0) then
    sSQL := MultipleParameters(SAMPLES_SELECT + SAMPLE_SQL[TableIndex] + SAMPLE_GROUP, Key)
  else
    sSQL := SAMPLES_SELECT + ALL_SAMPLES + SAMPLE_GROUP;

  with FQuery do
  begin
    SQL.Clear;
    SQL.Add(sSQL);
    try
      Open;
      If RecordCount > 0 then
        while not EoF do
        begin
          Result.Add(Trim(FieldByName('SAMPLE_KEY').AsString));
          Next;
        end;
    finally
      Close;
    end;
  end;
end;

//--------------------------------------------------------------------------------
// Build the string to use in the Format() statement which is used to generate
// the SQL string from the CONST string arrays used in these source files.
// The number of parameters is variable and depends upon the particulat CONST
//--------------------------------------------------------------------------------
function TInfo.MultipleParameters(const SQL, sKey: string): string;
var param: array of TVarRec ;
    i,p: integer;
    work: string;
begin
  // find out how many times %s occurs in the SQL string
  work := SQL;
  i := 0;
  repeat
    p := Pos('%s', work);
    if p>0 then
    begin
      inc(i);
      work := Copy(work, p+2, 999);
    end;
  until (p=0) or (work = '');
  // build a parameter array with that number of instances of sKey
  if i>0 then
  begin
    setlength(param, i);
    for p:=1 to i do
    begin
      param[p-1].vType := vtPChar;
      param[p-1].VPChar :=strnew(pchar(sKey));
    end;
    // we can now call Format with the correct number of parameters
    Result := Format(SQL, param);
    param := nil;
  end
  else
    // no instances of %s found so just return tyhe unmodified SQL
    Result := SQL;
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
function TInfo.QueryNumber(const SQL, sKey: string): integer;
var workSQL: string;
begin
  if sKey <> '' then
    workSQL := MultipleParameters(SQL, sKey)
  else
    workSQL := SQL;
  with FQuery do
  begin
    SQL.Clear;
    SQL.Add(workSQL);
    try
      Open;
      // we only expect 1 row to be returned here
      If RecordCount > 0 then
        Result := FieldByName('ItemName').AsInteger
      else
        Result := 0;
    finally
      Close;
    end;
  end;
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
function TInfo.QueryString(const SQL, sKey: string): string;
var workSQL: string;
begin
  if sKey <> '' then
    workSQL := MultipleParameters(SQL, sKey)
  else
    workSQL := SQL;
  with FQuery do
  begin
    SQL.Clear;
    SQL.Add(workSQL);
    try
      Open;
      // we only expect 1 row to be returned here
      If RecordCount > 0 then
        Result := FieldByName('ItemName').AsString
      else
        Result := '';
    finally
      Close;
    end;
  end;
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
procedure TInfo.set_Key(const Value: string);
begin
  FKey := Value;
  if (FKey <> '') and (FTableIndex > 0) then
  begin
    FItemLabel := QueryString(ITEM_SQL[FTableIndex], Value);
    FSamples := QueryNumber(SAMPLES_COUNT + SAMPLE_SQL[FTableIndex], Value);
    FEvents := QueryNumber(EVENT_COUNT + EVENT_SQL[FTableIndex], Value);
  end
  else
  begin
    FItemLabel := 'All';
    FSamples := QueryNumber(SAMPLES_COUNT + ALL_SAMPLES, '');
    FEvents := QueryNumber(EVENT_COUNT + ALL_EVENTS, '');
  end;
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
procedure TInfo.set_Table(const Value: string);
var i: integer;
begin
  if CompareText(Value, 'Database') = 0 then
  begin
    FTable := Value;
    FTableIndex := 0;
  end
  else
  begin
    i := GetTableIndex(Value);
    if i > 0 then
    begin
      FTableIndex := i;
      FTable := Value;
    end;
  end;
end;

{ TInfoList }

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
function TInfoList.AddItem(const aTable, aKey: string): integer;
var oInfo: TInfo;
begin
  oInfo := TInfo.Create(aTable, aKey, FQuery);
  FTotalSamples := FTotalSamples + oInfo.nSamples;
  FTotalEvents := FTotalEvents + oInfo.nEvents;
  Result := Add(oInfo);
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
procedure TInfoList.Clear;
var i:  integer;
begin
  if Count > 0 then
  for i := Count - 1 downto 0 do
    TInfo(Items[i]).Free;
  FTotalSamples := 0;
  FTotalEvents := 0;
  inherited;
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
constructor TInfoList.Create(aQuery: TADOQuery);
begin
  inherited Create;
  FQuery := aQuery;
  FTotalSamples := 0;
  FTotalEvents := 0;
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
destructor TInfoList.Destroy;
begin
  Clear;
  inherited;
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
function TInfoList.FindItem(const aTable, aKey: string): integer;
var i: integer;
begin
  Result := -1;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if (CompareText(TInfo(ITems[i]).Table, aTable) = 0) and
         (CompareText(TInfo(ITems[i]).Key, aKey) = 0) then
      begin
        Result := i;
        Exit;
      end;
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
function TInfoList.get_EventCount(i: integer): string;
begin
  if (i>=0) and (i<Count) then
    Result := Format('%0.0n', [TInfo(Items[i]).nEvents * 1.0])
  else
    Result := '0';
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
function TInfoList.Get_Events: string;
var oKeys, oTemp: TStrings;
    i: integer;
    j: Longint;
begin
  Result := '';
  if Count > 0 then
  begin
    oKeys := TStringList.Create;
    //oKeys.Delimiter := ',';
    //oKeys.QuoteChar := Char(39); // single quote
    //oKeys.StrictDelimiter := True;
    if Assigned(FOnCallBack) then
      FOnCallBack(Self, cbInit, FTotalEvents);
    FTotalEvents := 0;
    try
      for i := 0 to Count - 1 do
        if TInfo(Items[i]).Selected then
        begin
          //oTemp := TStringList.Create;
          try
            oTemp := TInfo(Items[i]).Events;
            if oTemp.Count > 0 then
              for j  := 0 to oTemp.Count - 1 do
                if oKeys.IndexOf(oTemp[j]) < 0 then
                begin
                  oKeys.Add(oTemp[j]);
                  inc(FTotalEvents);
                  if Assigned(FOnCallBack) and (FTotalEvents mod 100 = 0) then
                    FOnCallBack(Self, cbPos, FTotalEvents);
                end;
          finally
            oTemp.Free;
          end;
        end;
      if Assigned(FOnCallBack) then
        FOnCallBack(Self, cbDone, 0);
      if oKeys.Count > 0 then
        Result := CommaDelim(oKeys);
    finally
      oKeys.Free;
    end;
  end;
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
function TInfoList.get_ItemLabel(i: integer): string;
begin
  if (i>=0) and (i<Count) then
    Result := TInfo(Items[i]).ItemLabel
  else
    Result := '';
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
function TInfoList.get_SampleCount(i: integer): string;
begin
  if (i>=0) and (i<Count) then
    Result := Format('%0.0n', [TInfo(Items[i]).nSamples * 1.0])
  else
    Result := '0';
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
function TInfoList.Get_Samples: string;
var oKeys, oTemp: TStrings;
    i: integer;
    j: Longint;
begin
  Result := '';
  if Count > 0 then
  begin
    oKeys := TStringList.Create;
    if Assigned(FOnCallBack) then
      FOnCallBack(Self, cbInit, FTotalSamples);
    FTotalSamples := 0;
    //oKeys.Delimiter := ',';
    //oKeys.QuoteChar := '"'; //Char(39); // single quote
    //oKeys.StrictDelimiter := True;
    try
      for i := 0 to Count - 1 do
        if TInfo(Items[i]).Selected then
        begin
          //oTemp := TStringList.Create;
          try
            oTemp := TInfo(Items[i]).Samples;
            if oTemp.Count > 0 then
              for j  := 0 to oTemp.Count - 1 do
                if oKeys.IndexOf(oTemp[j]) < 0 then
                begin
                  oKeys.Add(oTemp[j]);
                  inc(FTotalSamples);
                  if Assigned(FOnCallBack) and (FTotalSamples mod 100 = 0) then
                    FOnCallBack(Self, cbPos, FTotalSamples);
                end;
          finally
            oTemp.Free;
          end;
        end;
      if Assigned(FOnCallBack) then
        FOnCallBack(Self, cbDone, 0);
      if oKeys.Count > 0 then
        Result := CommaDelim(oKeys);
    finally
      oKeys.Free;
    end;
  end;
end;

function TInfoList.CommaDelim(oString: TStrings): string;
var i: Longint;
begin
  Result := '';
  if oString.Count> 0 then
  begin
    for i := 0 to oString.Count - 1 do
      Result := Result + Char(39) + oString[i] +  char(39) + ',';
    Result := Copy(Result, 1, Length(Result)-1);
  end;
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
function TInfoList.get_Selcted(i: integer): boolean;
begin
  if (i>=0) and (i<Count) then
    Result := TInfo(Items[i]).Selected
  else
    Result := False;
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
function TInfoList.get_TypeLabel(i: integer): string;
begin
  if (i>=0) and (i<Count) then
  begin
    if TInfo(Items[i]).TableIndex = 0 then
      Result := 'Database'
    else if (TInfo(Items[i]).TableIndex >=low(DROP_TYPE)) and
            (TInfo(Items[i]).TableIndex <=High((DROP_TYPE))) then
      Result := DROP_TYPE[TInfo(Items[i]).TableIndex]
    else
      Result := 'Unknown';
  end
  else
    Result := '';
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
procedure TInfoList.set_selected(i: integer; const Value: boolean);
var j: integer;
begin
  if (i>=0) and (i<Count) then
  begin
    TInfo(Items[i]).Selected := Value;
    // recalculate totals
    FTotalSamples := 0;
    FTotalEvents := 0;
    for j := 0 to Count - 1 do
      if TInfo(Items[j]).Selected then
      begin
        FTotalSamples := FTotalSamples + TInfo(Items[j]).nSamples;
        FTotalEvents := FTotalEvents + TInfo(Items[j]).nEvents;
      end;
  end;
end;

//=============================================================================
// General purpose function
//-----------------------------------------------------------------------------
// Binary search for table name in the array
// Returns the index of the table name or -1 if it is not found
//-----------------------------------------------------------------------------
Function GetTableIndex(const sName: string): integer;
var s, e, d: integer;
begin
  s := Low(DROP_TABLES);
  e := High(DROP_TABLES);
  repeat
    result:= (s+e) shr 1;
    // case insensitive comparison!
    d:= CompareText(sName, DROP_TABLES[Result]);
    if d = 0 then
      // we found the name
      EXIT
    else
      if d > 0 then
        s:= result +1
      else
        e:= result -1;
  until s > e;
  // if this happens, we didn't find the name
  Result:= -1;
end;

//--------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
function GetString(const Index: integer) : string;
var
  buffer : array[0..255] of char;
  ls : integer;
begin
  Result := '';
  ls := LoadString(hInstance,
                   Index,
                   buffer,
                   sizeof(buffer));
  if ls <> 0 then Result := buffer;
end;

end.
