//==============================================================================
//  Unit: Custody
//
//  Description: Contains functionality for transferring the Custody of data.
//  Used by dialogue boxes such as dlgCustodyTransfer and
//  dlgCustodyTransferExport.
//
//  Author: Michael Bailey
//  Created: Dec 2002
//
//  Last Revision Details:
//    $Revision: 15 $
//    $Date: 25/03/09 10:05 $
//    $Author: Pauldavies $
//
//  $History: Custody.pas $
//  
//  *****************  Version 15  *****************
//  User: Pauldavies   Date: 25/03/09   Time: 10:05
//  Updated in $/JNCC/Development/Build/Source
//  Incident 18907
//  CCN 292
//  
//  Set collation on the temporary tables to database_default to avoid
//  conflicts.
//  
//  *****************  Version 14  *****************
//  User: Pauldavies   Date: 10/03/09   Time: 12:02
//  Updated in $/JNCC/Development/Build/Source
//  Refactoring.
//  
//  *****************  Version 13  *****************
//  User: Pauldavies   Date: 3/03/09    Time: 14:05
//  Updated in $/JNCC/Development/Build/Source
//  Corrected index counts in constant arrays.
//  
//  *****************  Version 12  *****************
//  User: Pauldavies   Date: 3/03/09    Time: 13:48
//  Updated in $/JNCC/Development/Build/Source
//  Incident 18759
//  CCN 317
//  
//  Added custody information for the Sample_Admin_Areas table.
//  
//  *****************  Version 11  *****************
//  User: Pauldavies   Date: 2/03/09    Time: 9:20
//  Updated in $/JNCC/Development/Build/Source
//  Incident 18232
//  CCN303
//  
//  Removed the string list entirely- now uses a temporary table. Also
//  removed a procedure that was never used.
//  
//  *****************  Version 10  *****************
//  User: Pauldavies   Date: 27/02/09   Time: 16:05
//  Updated in $/JNCC/Development/Build/Source
//  Incident 18232
//  CCN 303
//  
//  Changed the row by row custody transfer to use a temporary table
//  instead- hopefully faster + with no stack overflow.
//  
//  *****************  Version 9  *****************
//  User: Pauldavies   Date: 13/02/09   Time: 16:29
//  Updated in $/JNCC/Development/Build/Source
//  Incident 18012
//  CCN 292
//  
//  Added support for custodianship transfer of Survey_Event_Owner,
//  Survey_Event_Owner_Type, Survey_Tag and Concept tables.
//  
//  *****************  Version 8  *****************
//  User: Rickyshrestha Date: 3/01/08    Time: 11:31
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardocded strings to resourestring
//  
//  *****************  Version 7  *****************
//  User: Ericsalmon   Date: 12/02/03   Time: 10:05
//  Updated in $/JNCC/Source
//  IR 344 fixed.
//  
//  *****************  Version 6  *****************
//  User: Ericsalmon   Date: 10/01/03   Time: 10:36
//  Updated in $/JNCC/Source
//  IR 176 - v2.9.4.0
//  Fixed. Tables llisted in unit must match tables with Custodian field in
//  the database. Changes in the database must reflected here.
//  
//  *****************  Version 5  *****************
//  User: Michaelbailey Date: 20/12/02   Time: 16:35
//  Updated in $/JNCC/Source
//  Probably needs a few more comments, but I don't have a lot of time
//  left.
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================


unit Custody;

interface

uses Classes, DataClasses, ADODB, ExceptionForm;

type
  ECustodialError = class(TExceptionPath);

  TNotifyTransferProgressEvent = procedure(const TableName: string) of object;

  TCustodyRelinquisher = class
  private
    FTables: TStringList;
    FItemsAdded: boolean;
    FOnNextTable: TNotifyTransferProgressEvent;
    procedure SetOnNextTable(const Value: TNotifyTransferProgressEvent);
    function GetTableCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function DataAssigned: Boolean;
    procedure AssignKeyList(AKeyList: TKeyList);
    procedure RelinquishAllCustody(AQuery: TADOQuery; const NewCustodian: string);
    procedure RelinquishAssignedCustody(AQuery: TADOQuery; const NewCustodian: string);
    property OnNextTable: TNotifyTransferProgressEvent read FOnNextTable write SetOnNextTable;
    property TableCount: Integer read GetTableCount;
  end;

  // Why I've made this a class, I'm not sure.  Perhaps I yearn towards Java ideals today.
  // Please note that ATableName assumes UpperCase for all functions.
  TCustodyInfo = class
  private
    class function TableIndex(const ATableName: string): Integer;
    class function SimpleMasterSQL
      (const TableName, ItemKey, MasterTable, MasterKey: string; const AKey: TKeyString): string;
    class function RelationMasterSQL
      (const TableName, ItemKey, MasterTable, MasterKey: string; const AKey: TKeyString): string;
    class function SubMasterSQL(const TableName, ItemKey, InterTable, InterKey,
      MasterTable, MasterKey: string; const AKey: TKeyString): string;
    class function MasterDetailTables(const ATableName: string): TStringList;
    class function SubMasterDetailTables(const ATableName: string): TStringList;
    class procedure AddDetails(AQuery: TADOQuery; AKeyList: TEditableKeyList;
      const TableName, KeyField, MasterKeyField: string; const MasterKey: TKeyString);
    class procedure AddSubMasterDetails(AQuery: TADOQuery; AKeyList: TEditableKeyList;
      const TableName, KeyField: string; const Key: TKeyString);
    class procedure AddRelationDetails(AQuery: TADOQuery; AKeyList: TEditableKeyList;
      const TableName, KeyField: string; const Key: TKeyString);
  public
    class function CustodyTables: Integer;
    class function TableKey(const ATableName: string): string;
    class function MasterTable(const ATableName: string): string;
    class function MasterKey
      (AQuery: TADOQuery; const ATableName: string; const AKey: TKeyString): TKeyString;
    class function IsMaster(const ATableName: string): Boolean;
    class function IsSubMaster(const ATableName: string): Boolean;
    class function MasterCaption(const ATableName: string): string;
    class function SubMasterCaption(const ATableName: string): string;
    class function MasterDetails
      (AQuery: TADOQuery; const ATableName: string; const Key: TKeyString): TEditableKeyList;
    class function SubMasterDetails
      (AQuery: TADOQuery; const ATableName: string; const Key: TKeyString): TEditableKeyList;
  end;

  TCustodySetUp = class
  protected
    // These methods were used to set up the new Custodian fields.
    // They are left in case they are ever needed again.
    // There's no particular reason for them being protected.  Pure whim on my part.  MDB
    class procedure AddCustodianField(AQuery: TADOQuery);
    class procedure DropCustodianField(AQuery: TADOQuery);
    class procedure CreateCustodianTrigger(AQuery: TADOQuery);
    class procedure ResetCustodians(AQuery: TADOQuery);
    class procedure SetUpCustody(AQuery: TADOQuery);
  end;

implementation

uses SysUtils, ApplicationSettings, Math, Dialogs, DatabaseAccessADO;

const
  HIGH_CUSTODY = 102;

  CUSTODY_TABLES: array [0..HIGH_CUSTODY, 0..1] of string =
    (
      ('ADDRESS', 'ADDRESS_KEY'),
      ('ADMIN_AREA', 'ADMIN_AREA_KEY'),
      ('ADMIN_AREA_SOURCES', 'SOURCE_LINK_KEY'),
      ('ADMIN_BOUNDARY', 'ADMIN_BOUNDARY_KEY'),
      ('ADMIN_RELATION', 'ADMIN_RELATION_KEY'),

      ('ADMIN_TYPE', 'ADMIN_TYPE_KEY'),
      ('BIOTOPE', 'BIOTOPE_KEY'),
      ('BIOTOPE_CLASSIFICATION', 'BIOTOPE_CLASSIFICATION_KEY'),
      ('BIOTOPE_CLASSIFICATION_TYPE', 'BT_CL_TYPE_KEY'),
      ('BIOTOPE_CLASSIFICATION_VERSION', 'BT_CL_VERSION_KEY'),

      ('BIOTOPE_DESIGNATION', 'BIOTOPE_DESIGNATION_KEY'),
      ('BIOTOPE_DESIGNATION_TYPE', 'BIOTOPE_DESIGNATION_TYPE_KEY'),
      ('BIOTOPE_DETERMINATION', 'BIOTOPE_DETERMINATION_KEY'),
      ('BIOTOPE_FACT', 'BIOTOPE_FACT_KEY'),
      ('BIOTOPE_LIST_ITEM', 'BIOTOPE_LIST_ITEM_KEY'),

      ('BIOTOPE_OCCURRENCE', 'BIOTOPE_OCCURRENCE_KEY'),
      ('BIOTOPE_OCCURRENCE_DATA', 'BIOTOPE_OCCURRENCE_DATA_KEY'),
      ('BIOTOPE_OCCURRENCE_SOURCES', 'SOURCE_LINK_KEY'),
      ('BIOTOPE_RELATION', 'BIOTOPE_RELATION_KEY'),
      ('BIOTOPE_SOURCES', 'SOURCE_LINK_KEY'),

      ('COMMUNICATION', 'COMMUNICATION_KEY'),
      ('CONCEPT', 'CONCEPT_KEY'),
      ('CONTACT_NUMBER', 'CONTACT_NUMBER_KEY'),
      ('DAMAGE_OCCURRENCE', 'DAMAGE_OCCURRENCE_KEY'),
      ('DETERMINATION_TYPE', 'DETERMINATION_TYPE_KEY'),

      ('DETERMINER_ROLE', 'DETERMINER_ROLE_KEY'),
      ('GRID_SQUARE', 'GRID_SQUARE_KEY'),
      ('JOURNAL', 'JOURNAL_KEY'),
      ('LAND_PARCEL', 'LAND_PARCEL_KEY'),
      ('LOCATION', 'LOCATION_KEY'),

      ('LOCATION_ADMIN_AREAS', 'LOCATION_ADMIN_AREAS_KEY'),
      ('LOCATION_BOUNDARY', 'LOCATION_BOUNDARY_KEY'),
      ('LOCATION_DATA', 'LOCATION_DATA_KEY'),
      ('LOCATION_DESIGNATION', 'DESIGNATION_KEY'),
      ('LOCATION_FEATURE', 'LOCATION_FEATURE_KEY'),

      ('LOCATION_FEATURE_GRADING', 'FEATURE_GRADING_KEY'),
      ('LOCATION_FEATURE_SOURCES', 'SOURCE_LINK_KEY'),
      ('LOCATION_FEATURE_TYPE', 'LOCATION_FEATURE_TYPE_KEY'),
      ('LOCATION_NAME', 'LOCATION_NAME_KEY'),
      ('LOCATION_RELATION', 'LOCATION_RELATION_KEY'),

      ('LOCATION_SOURCES', 'SOURCE_LINK_KEY'),
      ('LOCATION_TYPE', 'LOCATION_TYPE_KEY'),
      ('LOCATION_USE', 'LOCATION_USE_KEY'),
      ('MANAGEMENT_AIM', 'MANAGEMENT_AIM_KEY'),
      ('MEASUREMENT_QUALIFIER', 'MEASUREMENT_QUALIFIER_KEY'),

      ('MEASUREMENT_TYPE', 'MEASUREMENT_TYPE_KEY'),
      ('MEASUREMENT_UNIT', 'MEASUREMENT_UNIT_KEY'),
      ('NAME', 'NAME_KEY'),
      ('NAME_RELATION', 'NAME_RELATION_KEY'),
      ('NAME_SOURCES', 'SOURCE_LINK_KEY'),

      ('ORGANISATION_TYPE', 'ORGANISATION_TYPE_KEY'),
      ('POTENTIAL_THREAT', 'POTENTIAL_THREAT_KEY'),
      ('RECORD_TYPE', 'RECORD_TYPE_KEY'),
      ('RECORDER_ROLE', 'RECORDER_ROLE_KEY'),
      ('REFERENCE_AUTHOR', 'AUTHOR_KEY'),

      ('REFERENCE_EDITOR', 'EDITOR_KEY'),
      ('REFERENCE_NUMBER', 'NUMBER_KEY'),
      ('RELATIONSHIP_TYPE', 'RELATIONSHIP_TYPE_KEY'),
      ('SAMPLE', 'SAMPLE_KEY'), 
      ('SAMPLE_ADMIN_AREAS', 'SAMPLE_ADMIN_AREAS_KEY'),

      ('SAMPLE_DATA', 'SAMPLE_DATA_KEY'),
      ('SAMPLE_RELATION', 'SAMPLE_RELATION_KEY'),
      ('SAMPLE_SOURCES', 'SOURCE_LINK_KEY'),
      ('SAMPLE_TYPE', 'SAMPLE_TYPE_KEY'),
      ('SITE_STATUS', 'SITE_STATUS_KEY'),

      ('SOURCE', 'SOURCE_KEY'),
      ('SPECIMEN', 'SPECIMEN_KEY'),
      ('SPECIMEN_TYPE', 'SPECIMEN_TYPE_KEY'),
      ('SUBSTRATE', 'SUBSTRATE_KEY'),
      ('SURVEY', 'SURVEY_KEY'),

      ('SURVEY_EVENT', 'SURVEY_EVENT_KEY'),
      ('SURVEY_EVENT_OWNER', 'SURVEY_EVENT_OWNER_KEY'),
      ('SURVEY_EVENT_OWNER_TYPE', 'SURVEY_EVENT_OWNER_TYPE_KEY'),
      ('SURVEY_EVENT_RECORDER', 'SE_RECORDER_KEY'),
      ('SURVEY_EVENT_SOURCES', 'SOURCE_LINK_KEY'),

      ('SURVEY_MEDIA', 'SURVEY_MEDIA_KEY'),
      ('SURVEY_SOURCES', 'SOURCE_LINK_KEY'),
      ('SURVEY_STATUS', 'SURVEY_STATUS_KEY'),
      ('SURVEY_TAG', 'SURVEY_TAG_KEY'),
      ('SURVEY_TYPE', 'SURVEY_TYPE_KEY'),

      ('TAXON', 'TAXON_KEY'),
      ('TAXON_BIOTOPE_ASSOCIATION', 'ASSOCIATION_KEY'),
      ('TAXON_DESIGNATION', 'TAXON_DESIGNATION_KEY'),
      ('TAXON_DESIGNATION_TYPE', 'TAXON_DESIGNATION_TYPE_KEY'),
      ('TAXON_DETERMINATION', 'TAXON_DETERMINATION_KEY'),

      ('TAXON_FACT', 'TAXON_FACT_KEY'),
      ('TAXON_LIST', 'TAXON_LIST_KEY'),
      ('TAXON_LIST_ITEM', 'TAXON_LIST_ITEM_KEY'),
      ('TAXON_LIST_TYPE', 'TAXON_LIST_TYPE_KEY'),
      ('TAXON_LIST_VERSION', 'TAXON_LIST_VERSION_KEY'),

      ('TAXON_NAME_TYPE', 'TAXON_NAME_TYPE_KEY'),
      ('TAXON_OCCURRENCE', 'TAXON_OCCURRENCE_KEY'),
      ('TAXON_OCCURRENCE_DATA', 'TAXON_OCCURRENCE_DATA_KEY'),
      ('TAXON_OCCURRENCE_RELATION', 'TAXON_OCCURRENCE_RELATION_KEY'),
      ('TAXON_OCCURRENCE_SOURCES', 'SOURCE_LINK_KEY'),

      ('TAXON_RANK', 'TAXON_RANK_KEY'),
      ('TAXON_SOURCES', 'SOURCE_LINK_KEY'),
      ('TAXON_TAXON_ASSOCIATION', 'ASSOCIATION_KEY'),
      ('TAXON_VERSION', 'TAXON_VERSION_KEY'),
      ('TAXON_VERSION_RELATION', 'TAXON_VERSION_RELATION_KEY'),

      ('TENURE', 'TENURE_KEY'),
      ('TENURE_TYPE', 'TENURE_TYPE_KEY'),
      ('THREAT_TYPE', 'THREAT_TYPE_KEY')
    );

  SQL_RELINQUISH_0 = 'UPDATE %s';
  SQL_RELINQUISH_1 = 'SET Custodian = ''%s'' WHERE Custodian = ''%s''';
  SQL_RELINQUISH_2 = 'AND %s IN (%s)';

  SQL_DROP_FIELD = 'ALTER TABLE %s DROP COLUMN CUSTODIAN;';

  SQL_ADD_FIELD = 'ALTER TABLE %s ADD CUSTODIAN CHAR (8);';

  SQL_POPULATE_FIELD = 'UPDATE %s SET CUSTODIAN = SUBSTRING(%s, 1, 8);';

  SQL_CREATE_TRIGGER = 'EXEC CreateCustodianTrigger ''%s'', ''%s'';';

  MASTERS: array [0..9] of string =
    (
      'BIOTOPE_OCCURRENCE=Biotope Occurrence',
      'LOCATION=Location',
      'NAME=Name',
      'SOURCE=Source',
      'SAMPLE=Sample',
      'SURVEY=Survey',
      'SURVEY_EVENT=Survey Event',
      'TAXON_OCCURRENCE=Taxon Occurrence',
      'CONCEPT=Concept',
      'SURVEY_EVENT_OWNER_TYPE=Survey Event Owner Type'
    );

  SIMPLE_MASTER_TABLE: array [0..31] of string =
    (
      'ADDRESS=NAME',
      'BIOTOPE_DETERMINATION=BIOTOPE_OCCURRENCE',
      'BIOTOPE_OCCURRENCE_DATA=BIOTOPE_OCCURRENCE',
      'BIOTOPE_OCCURRENCE_SOURCES=BIOTOPE_OCCURRENCE',
      'CONTACT_NUMBER=NAME',

      'GRID_SQUARE=LOCATION',
      'LAND_PARCEL=LOCATION',
      'LOCATION_ADMIN_AREAS=LOCATION',
      'LOCATION_BOUNDARY=LOCATION',
      'LOCATION_DATA=LOCATION',

      'LOCATION_DESIGNATION=LOCATION',
      'LOCATION_FEATURE=LOCATION',
      'LOCATION_NAME=LOCATION',
      'LOCATION_SOURCES=LOCATION',
      'LOCATION_USE=LOCATION',

      'NAME_SOURCES=NAME',
      'REFERENCE_AUTHOR=REFERENCE',
      'REFERENCE_EDITOR=REFERENCE',
      'REFERENCE_NUMBER=REFERENCE',
      'SAMPLE_DATA=SAMPLE',

      'SAMPLE_SOURCES=SAMPLE',
      'SPECIMEN=TAXON_OCCURRENCE',
      'SAMPLE_ADMIN_AREAS=SAMPLE',
      'SURVEY_EVENT_OWNER=SURVEY_EVENT',

      'SURVEY_EVENT_RECORDER=SURVEY_EVENT',
      'SURVEY_EVENT_SOURCES=SURVEY_EVENT',
      'SURVEY_SOURCES=SURVEY',
      'SURVEY_TAG=SURVEY',

      'TAXON_DETERMINATION=TAXON_OCCURRENCE',
      'TAXON_OCCURRENCE_DATA=TAXON_OCCURRENCE',
      'TAXON_OCCURRENCE_SOURCES=TAXON_OCCURRENCE',
      'TENURE=LOCATION'
    );

  RELATION_MASTER_TABLE: array [0..3] of string =
    (
      'LOCATION_RELATION=LOCATION',
      'NAME_RELATION=NAME',
      'SAMPLE_RELATION=SAMPLE',
      'TAXON_OCCURRENCE_RELATION=TAXON_OCCURRENCE'
    );

  SUB_MASTERS: array [0..0] of string =
    (
      'LOCATION_FEATURE=Location Feature'
    );

  SUB_MASTER_TABLE: array [0..3] of string =
    (
      'DAMAGE_OCCURRENCE=LOCATION_FEATURE',
      'LOCATION_FEATURE_SOURCES=LOCATION_FEATURE',
      'MANAGEMENT_AIM=LOCATION_FEATURE',
      'POTENTIAL_THREAT=LOCATION_FEATURE'
    );

resourcestring
  ResStr_NotMixedKeyList =  'KeyList supplied was not a MIXED_DATA KeyList.';
  ResStr_DropTableFailed =  'Drop %s Custodian failed.' + #13#10 + '%s raised.' + #13#10 + '%s';




var
  mMasters, mSimpleMasterList, mRelationMasters,
  mRelationMasterList, mSubMasters, mSubMasterList: TStringList;

procedure CreateMasterLists;
var k: Integer;
begin
  mMasters := TStringList.Create;
  mSimpleMasterList := TStringList.Create;
  mRelationMasters := TStringList.Create;
  mRelationMasterList := TStringList.Create;
  mSubMasters := TStringList.Create;
  mSubMasterList := TStringList.Create;

  for k := High(MASTERS) downto 0 do mMasters.Add(MASTERS[K]);
  for k := High(SIMPLE_MASTER_TABLE) downto 0 do mSimpleMasterList.Add(SIMPLE_MASTER_TABLE[K]);
  for k := High(RELATION_MASTER_TABLE) downto 0 do mRelationMasterList.Add(RELATION_MASTER_TABLE[K]);
  for k := High(RELATION_MASTER_TABLE) downto 0 do
    mRelationMasters.Add(mRelationMasterList.ValueFromIndex[k] + '=' + mRelationMasterList.Names[k]);
  for k := High(SUB_MASTERS) downto 0 do mSubMasters.Add(SUB_MASTERS[k]);
  for k := High(SUB_MASTER_TABLE) downto 0 do mSubMasterList.Add(SUB_MASTER_TABLE[K]);
end;

procedure FreeUpList(List: TStringList);
var k: Integer;
begin
  if Assigned(List) then begin
    for k := List.Count - 1 downto 0 do
      TStringList(List.Objects[k]).Free;
    List.Free;
  end;
end;

{ TCustodyRelinquisher }

procedure TCustodyRelinquisher.AssignKeyList(AKeyList: TKeyList);
// Adds the items in the KeyList to those this instance of TCustodyRelinquisher
// should fascilitate relinquishing the custody of.
var
  k: Integer;
  sql: string;
begin 
  if AKeyList.Header.TableName <> MIXED_DATA then
    raise ECustodialError.Create(ResStr_NotMixedKeyList);

  for k := AKeyList.Header.ItemCount - 1 downto 0 do begin
    sql := Format('INSERT INTO #Relinquish VALUES (''%s'', ''%s'')',
                  [AKeyList.Items[k].KeyField1, AKeyList.ItemTable[k]]);
    dmDatabase.ExecuteSQL(sql);
    FItemsAdded := True;
    if FTables.IndexOf(AKeyList.ItemTable[k]) = -1 then
      FTables.Add(AKeyList.ItemTable[k]);
  end;
end;

constructor TCustodyRelinquisher.Create;
var
  sql: string;
begin
  sql := 'CREATE TABLE #Relinquish ('
                    + 'Primary_Key CHAR(16) COLLATE database_default,'
                    + 'KeyTable    VARCHAR(30) COLLATE database_default)';
  dmDatabase.ExecuteSQL(sql);
  FItemsAdded := False;
  FTables := TStringList.Create;
end;

function TCustodyRelinquisher.DataAssigned: Boolean;
begin
  Result := FItemsAdded;
end;

destructor TCustodyRelinquisher.Destroy;
var
  sql: string;
begin
  FTables.Free;
  sql := 'DROP TABLE #Relinquish';
  dmDatabase.ExecuteSQL(sql);
  inherited;
end;

function TCustodyRelinquisher.GetTableCount: Integer;
begin
  if FItemsAdded then
    Result := FTables.Count
  else
    Result := TCustodyInfo.CustodyTables;
end;

procedure TCustodyRelinquisher.RelinquishAllCustody
  (AQuery: TADOQuery; const NewCustodian: string);
// Relinquish custody of all data with the current site ID.
var k: Integer;
begin
  AQuery.SQL.Clear;
  AQuery.SQL.Add('');
  AQuery.SQL.Add(Format(SQL_RELINQUISH_1, [NewCustodian, AppSettings.SiteID]));
  for k := 0 to HIGH_CUSTODY do begin
    if Assigned(FOnNextTable) then FOnNextTable(CUSTODY_TABLES[k, 0]);
    AQuery.SQL[0] := Format(SQL_RELINQUISH_0, [CUSTODY_TABLES[k, 0]]);
    AQuery.ExecSQL;
  end;
end;

procedure TCustodyRelinquisher.RelinquishAssignedCustody
  (AQuery: TADOQuery; const NewCustodian: string);
// Relinquish custody of all data that has been assigned to the TCustodyRelinquisher instance.
var i, j, k: Integer;
begin
  for k := 0 to FTables.Count - 1 do begin
    if Assigned(FOnNextTable) then FOnNextTable(FTables[k]);

    AQuery.SQL.Clear;

    j := 0;
    // Gets the index of the custody table with the same name as this table.
    for i := 0 to HIGH_CUSTODY do
      if CUSTODY_TABLES[i,0] = FTables[k] then
        j := i;

    AQuery.SQL.Add( Format('UPDATE     T '
                + 'SET        T.Custodian = ''%s'' '
                + 'FROM       %s          T '
                + 'INNER JOIN #Relinquish R '
                + '        ON T.%s        = R.Primary_Key '
                + '       AND R.KeyTable  = ''%s'' '
                + 'WHERE      T.Custodian = ''%s'' ',
                [NewCustodian,
                 FTables[k],
                 CUSTODY_TABLES[j,1],
                 FTables[k],
                 AppSettings.SiteID]));
    AQuery.ExecSQL;
  end;
end;

procedure TCustodyRelinquisher.SetOnNextTable(const Value: TNotifyTransferProgressEvent);
begin
  FOnNextTable := Value;
end;

{ TCustodyInfo }

class procedure TCustodyInfo.AddDetails(AQuery: TADOQuery; AKeyList: TEditableKeyList;
  const TableName, KeyField, MasterKeyField: string; const MasterKey: TKeyString);
const
  DETAIL_SQL = 'SELECT %s FROM %s WHERE %s = ''%s'';';
begin
  with AQuery do begin
    SQL.Text := Format(DETAIL_SQL, [KeyField, TableName, MasterKeyField, MasterKey]);
    Open;
    try
      while not EOF do begin
        AKeyList.AddItem(FieldByName(KeyField).AsString, TableName);
        Next;
      end;
    finally Close end;
  end;
end;

class procedure TCustodyInfo.AddRelationDetails(AQuery: TADOQuery;
  AKeyList: TEditableKeyList; const TableName, KeyField: string; const Key: TKeyString);
var RelationTable: string;
begin
  RelationTable := mRelationMasters.Values[TableName];
  if RelationTable <> '' then
    AddDetails(AQuery, AKeyList, RelationTable, TableKey(RelationTable), KeyField + '_1', Key);
end;

class procedure TCustodyInfo.AddSubMasterDetails(AQuery: TADOQuery;
  AKeyList: TEditableKeyList; const TableName, KeyField: string; const Key: TKeyString);
var lStringList: TStringList;
    k: Integer;
begin
  lStringList := SubMasterDetailTables(TableName);
  for k := lStringList.Count - 1 downto 0 do
    AddDetails(AQuery, AKeyList, lStringList.Names[k], lStringList.ValueFromIndex[k], KeyField, Key);
end;

class function TCustodyInfo.CustodyTables: Integer;
// The number of tables with the Custodian Field.
begin
  Result := HIGH_CUSTODY + 1;
end;

class function TCustodyInfo.IsMaster(const ATableName: string): Boolean;
begin
  Result := mMasters.Values[ATableName] <> '';
end;

class function TCustodyInfo.IsSubMaster(const ATableName: string): Boolean;
begin
  Result := mSubMasters.Values[ATableName] <> '';
end;

class function TCustodyInfo.MasterCaption(const ATableName: string): string;
begin
  Result := mMasters.Values[ATableName];
end;

class function TCustodyInfo.MasterDetails(AQuery: TADOQuery;
  const ATableName: string; const Key: TKeyString): TEditableKeyList;
var lStringList: TStringList;
    k, i: Integer;
    MasterKey, DetailTable, DetailKey: string;
begin
  lStringList := MasterDetailTables(ATableName);
  MasterKey := TableKey(ATableName);
  Result := TEditableKeyList.Create;
  try
    Result.SetTable(MIXED_DATA);
    for k := lStringList.Count - 1 downto 0 do begin
      DetailTable := lStringList.Names[k];
      DetailKey := lStringList.ValueFromIndex[k];
      AddDetails(AQuery, Result, DetailTable, DetailKey, MasterKey, Key);
      if IsSubMaster(DetailTable) then begin
        i := Result.Header.ItemCount - 1;
        while (i >= 0) and (Result.Items[i].KeyField2 = DetailTable) do begin
          AddSubMasterDetails(AQuery, Result, DetailTable, DetailKey, Result.Items[i].KeyField1);
          Dec(i);
        end;
      end;
    end;
    AddRelationDetails(AQuery, Result, ATableName, MasterKey, Key);
  except
    on E: Exception do begin
      Result.Free;
      raise E;
    end;
  end;
end;

class function TCustodyInfo.MasterDetailTables(const ATableName: string): TStringList;
var i, k: Integer;
begin
  i := mMasters.IndexOfName(ATableName);
  Result := TStringList(mMasters.Objects[i]);
  if Result = nil then begin
    Result := TStringList.Create;
    mMasters.Objects[i] := Result;
    for k := mSimpleMasterList.Count - 1 downto 0 do
      if mSimpleMasterList.ValueFromIndex[k] = ATableName then
        Result.Add(mSimpleMasterList.Names[k] + '=' + TableKey(mSimpleMasterList.Names[k]));
  end;
end;

class function TCustodyInfo.MasterKey
  (AQuery: TADOQuery; const ATableName: string; const AKey: TKeyString): TKeyString;
var masterName, interName: string;
begin
  masterName := mSimpleMasterList.Values[ATableName];
  if masterName = '' then begin
    masterName := mRelationMasterList.Values[ATableName];
    if masterName = '' then begin
      interName := mSubMasterList.Values[ATableName];
      if interName = '' then raise ECustodialError.Create('Bad Table.');
      masterName := mSimpleMasterList.Values[interName];
      AQuery.SQL.Text := SubMasterSQL(ATableName, TableKey(ATableName), interName,
        TableKey(interName), masterName, TableKey(masterName), AKey);
    end else
      AQuery.SQL.Text := RelationMasterSQL
        (ATableName, TableKey(ATableName), masterName, TableKey(masterName), AKey)
  end else
    AQuery.SQL.Text := SimpleMasterSQL
      (ATableName, TableKey(ATableName), masterName, TableKey(masterName), AKey);


  AQuery.Open;
  try
    Result := AQuery.Fields[0].AsString;
  finally AQuery.Close end;
end;

class function TCustodyInfo.MasterTable(const ATableName: string): string;
begin
  if not Assigned(mMasters) then CreateMasterLists;
  if mMasters.Values[ATableName] <> '' then Result := ATableName
  else Result := mSimpleMasterList.Values[ATableName];
  if Result = '' then Result := mRelationMasterList.Values[ATableName];
  if Result = '' then begin
    Result := mSubMasterList.Values[ATableName];
    if Result <> '' then Result := mSimpleMasterList.Values[Result];
  end;
end;

class function TCustodyInfo.RelationMasterSQL(const TableName, ItemKey,
  MasterTable, MasterKey: string; const AKey: TKeyString): string;
const
  SQL_RELATION_MASTER_KEY =
    'SELECT M.%s FROM %s AS M INNER JOIN %s AS D ON M.%s = D.%s_1 WHERE D.%s = ''%s'';';
begin
  Result := Format(SQL_RELATION_MASTER_KEY,
    [MasterKey, MasterTable, TableName, MasterKey, MasterKey, ItemKey, AKey]);
end;

class function TCustodyInfo.SimpleMasterSQL(const TableName, ItemKey,
  MasterTable, MasterKey: string; const AKey: TKeyString): string;
const
  SQL_SIMPLE_MASTER_KEY =
    'SELECT M.%s FROM %s AS M INNER JOIN %s AS D ON M.%s = D.%s WHERE D.%s = ''%s'';';
begin
  Result := Format(SQL_SIMPLE_MASTER_KEY,
    [MasterKey, MasterTable, TableName, MasterKey, MasterKey, ItemKey, AKey]);
end;

class function TCustodyInfo.SubMasterCaption(const ATableName: string): string;
begin
  Result := mSubMasters.Values[ATableName];
end;

class function TCustodyInfo.SubMasterDetails(AQuery: TADOQuery;
  const ATableName: string; const Key: TKeyString): TEditableKeyList;
begin
  Result := TEditableKeyList.Create;
  try
    Result.SetTable(MIXED_DATA);
    AddSubMasterDetails(AQuery, Result, ATableName, TableKey(ATableName), Key);
  except
    on E: Exception do begin
      Result.Free;
      raise E;
    end;
  end;
end;

class function TCustodyInfo.SubMasterDetailTables(const ATableName: string): TStringList;
var i, k: Integer;
begin
  i := mSubMasters.IndexOfName(ATableName);
  Result := TStringList(mSubMasters.Objects[i]);
  if Result = nil then begin
    Result := TStringList.Create;
    mSubMasters.Objects[i] := Result;
    for k := mSubMasterList.Count - 1 downto 0 do
      if mSubMasterList.ValueFromIndex[k] = ATableName then
        Result.Add(mSubMasterList.Names[k] + '=' + TableKey(mSubMasterList.Names[k]));
  end;
end;

class function TCustodyInfo.SubMasterSQL(const TableName, ItemKey, InterTable,
  InterKey, MasterTable, MasterKey: string; const AKey: TKeyString): string;
const
  SQL_SUB_MASTER_KEY =
    'SELECT M.%s FROM (%s AS M INNER JOIN %s AS I ON M.%s = I.%s) ' +
    'INNER JOIN %s AS D ON I.%s = D.%s WHERE D.%s = ''%s'';';
begin
  Result := Format(SQL_SUB_MASTER_KEY, [MasterKey, MasterTable, InterTable,
                   MasterKey, MasterKey, TableName, InterKey, InterKey, ItemKey, AKey]);
end;

class function TCustodyInfo.TableIndex(const ATableName: string): Integer;
var high, low, mid, c: Integer;
begin
  high := HIGH_CUSTODY;
  low := 0;
  mid := high div 2;
  c := CompareText(ATableName, CUSTODY_TABLES[mid, 0]);
  while (c <> 0) and (high <> low) do begin
    if c < 0 then high := mid
    else low := Max(mid, low + 1);
    mid := (high + low) div 2;
    c := CompareText(ATableName, CUSTODY_TABLES[mid, 0])
  end;
  if c = 0 then Result := mid
  else Result := -1;
end;

class function TCustodyInfo.TableKey(const ATableName: string): string;
begin
  Result := CUSTODY_TABLES[TableIndex(ATablename), 1];
end;

{ TCustodySetUp }

class procedure TCustodySetUp.AddCustodianField(AQuery: TADOQuery);
var k: Integer;
begin
  for k := 0 to HIGH_CUSTODY do begin
    AQuery.SQL.Text := Format(SQL_ADD_FIELD, [CUSTODY_TABLES[k, 0]]);
    AQuery.ExecSQL;
  end;
end;

class procedure TCustodySetUp.CreateCustodianTrigger(AQuery: TADOQuery);
var k: Integer;
begin
  for k := 0 to HIGH_CUSTODY do begin
    AQuery.SQL.Text := Format(SQL_CREATE_TRIGGER, [CUSTODY_TABLES[k, 0], CUSTODY_TABLES[k, 1]]);
    AQuery.ExecSQL;
  end;
end;

class procedure TCustodySetUp.DropCustodianField(AQuery: TADOQuery);
var k: Integer;
begin
  for k := 0 to HIGH_CUSTODY do
    try
      AQuery.SQL.Text := Format(SQL_DROP_FIELD, [CUSTODY_TABLES[k, 0]]);
      AQuery.ExecSQL;
    except
      on E: Exception do
        MessageDlg(Format(ResStr_DropTableFailed, [CUSTODY_TABLES[k, 0], E.ClassName, E.Message]), mtInformation, [mbOK], 0);
    end;
end;

class procedure TCustodySetUp.ResetCustodians(AQuery: TADOQuery);
var k: Integer;
begin
  for k := 0 to HIGH_CUSTODY do begin
    AQuery.SQL.Text := Format(SQL_POPULATE_FIELD, [CUSTODY_TABLES[k, 0], CUSTODY_TABLES[k, 1]]);
    AQuery.ExecSQL;
  end;
end;

class procedure TCustodySetUp.SetUpCustody(AQuery: TADOQuery);
begin
  AddCustodianField(AQuery);
  ResetCustodians(AQuery);
  CreateCustodianTrigger(AQuery);
end;

initialization
  mMasters := nil;
  mSimpleMasterList := nil;
  mRelationMasters := nil;
  mRelationMasterList := nil;
  mSubMasters := nil;
  mSubMasterList := nil;

finalization
  FreeUpList(mMasters);
  mSimpleMasterList.Free;
  mRelationMasters.Free;
  mRelationMasterList.Free;
  FreeUpList(mSubMasters);
  mSubMasterList.Free;

end.
