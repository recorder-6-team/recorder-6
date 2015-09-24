unit TransferQuery;

interface

uses
  Classes, Sysutils, DatabaseAccessADO, AdoDB, ADOInt, Variants, ExceptionForm,
  GeneralFunctions;

type
  TSourceDataSQLOption = (sdCount, sdRemainingRecordsOnly);
  TSourceDataSQLOptions = set of TSourceDataSQLOption;

  TTransferQuery = class
  private
    FSQLTableName : string;
    FAccessTableName : string;
    FSQLConnection: TADOConnection;
    FAccessConnection: TADOConnection;
    FAccessDBName : string;
    FIntoFieldList : string;
    FFromOpenRowsetFieldList : string;
    FAccessFieldList : string;
    FSystemSuppliedExists : boolean;
    FIncSysSupplied: boolean;
    function ConvertDateTimeField(const istFieldName: string): string;
    procedure GetFieldLists;
    function GetSourceDataLinkSQL : string;
    function GetSourceDataSQL(AOptions : TSourceDataSQLOptions) : string;
    function ModifyAccessField(const AFieldName: string): string;
    function ModifyOpenRowsetField(const AFieldName: string): string;
    function ShouldMigrateField(const AFieldName: string): boolean;
  public
    constructor Create(const AAccessTableName, ASqlTableName,
      AAccessDBName: String; ASQLConnection, AAccessConnection: TADOConnection;
      AIncSysSupplied: boolean=false); overload;
    property AccessTableName : string read FAccessTableName;
    property SqlTableName : string read FSqlTableName;
    function GetAccessSelectRemainingSQL(const APrimaryKey : TPrimaryKey) : string;
    function GetCountSourceSQL : string;
    function GetCreateViewSQL(const AViewName : string) : string;
    function GetIndividualRecordInsertSQL(APrimaryKey : TPrimaryKey) : string;
  end;

//==============================================================================
implementation

type
  ETransferQuery = class(TExceptionPath);

const
    // list of tables without custodian field - comma at beginning and end is deliberate
    NON_CUSTODIAN_TABLES = ',BIOTOPE_RELATION_JOIN,BIOTOPE_RELATION_ADMIN_AREA,'+
        'DATABASE_RELATIONSHIP,DTD_FRAGMENT,'+
        'EXPORT_FILTER,EXPORT_FILTER_SURVEY,EXPORT_FILTER_TAXON,EXPORT_FORMAT,INDEX_TAXON_NAME,'+
        'INDEX_TAXON_SYNONYM,INDEX_TAXON_GROUP,MAP_SHEET,INDIVIDUAL,ORGANISATION,'+
        'LAST_KEY,MEASUREMENT_TYPE_CONTEXT,PREFERRED_LINKS,'+
        'REFERENCE,SOURCE_FILE,REPORT_ATTRIBUTE,REPORT_JOIN,REPORT_WHERE,REPORT_FIELD,'+
        'SAMPLE_RECORDER,SETTING,SPECIAL_XML_ELEMENT,TAXON_COMMON_NAME,TAXON_USER_NAME,TERM_LIST,'+
        'USABLE_FIELD,USABLE_TABLE,USER,';

    SQL_INDIVIDUAL_INSERT =
      'INSERT into [%s] (%s) ' +
      'SELECT %s FROM #TempBadRecords AS T1 WHERE %s=''%s''';

    SQL_SOURCE_DATA_TEMPLATE =
      'SELECT %s FROM OPENROWSET(''Microsoft.Jet.OLEDB.4.0'', ''%s'';''admin'';'''','+
      ' ''%s'' ) AS T1';

    SQL_SOURCE_DATA_QUERY =
      'SELECT %s FROM %s %s ';

    SQL_CREATE_VIEW =
      'CREATE VIEW %s as %s';

//==============================================================================
// Description: Convert VAGUE_DATES to DOUBLE INTEGERS to allow pre 1753 dates on the
//              SQL server.
//
// Author: Ben Collier
// Created: 04/12/2002
//------------------------------------------------------------------------------
function TTransferQuery.ConvertDateTimeField(const istFieldName : string): string;
var
  lstFieldTypeName: string;
begin
  if Copy(istFieldName, Length(istFieldName) - Length('VAGUE_DATE_START')+1, 255) =
                      'VAGUE_DATE_START' then
  begin
    lstFieldTypeName := Copy(istFieldName, 1, Length(istFieldName) - Length('START') - 1) + '_TYPE';
    Result := 'IIF(ISNULL(ACCESS.' + istFieldName + '), NULL, ' +
	                    'IIF((CLng(ACCESS.' + istFieldName + ') = 0) AND ((ACCESS.'
                            + lstFieldTypeName + ' = "U") OR (ACCESS.' + lstFieldTypeName +
                            ' = "-Y")), NULL, CLng(ACCESS.'
                            + istFieldName + '))) AS [' + istFieldName + ']';
  end
  else if Copy(istFieldName, Length(istFieldName) - Length('VAGUE_DATE_END')+1, 255) =
                      'VAGUE_DATE_END' then
  begin
    lstFieldTypeName := Copy(istFieldName, 1, Length(istFieldName) - Length('END') - 1) + '_TYPE';
    Result := 'IIF(ISNULL(ACCESS.' + istFieldName + '), NULL, ' +
	                    'IIF((CLng(ACCESS.' + istFieldName + ') = 0) AND ((ACCESS.'
                            + lstFieldTypeName + ' = "U") OR (ACCESS.' + lstFieldTypeName
                            + ' = "Y-")), NULL, CLng(ACCESS.'
                            + istFieldName + '))) AS [' + istFieldName + ']';
    end
  else if istFieldName = 'OBS_DATE_START' then
  begin
    lstFieldTypeName := Copy(istFieldName, 1, Length(istFieldName) - Length('START') - 1) + '_TYPE';
    Result := 'IIF(ISNULL(ACCESS.' + istFieldName + '), NULL, CLng(ACCESS.' + istFieldName + ')) AS [' + istFieldName + ']';
  end
  else if istFieldName = 'OBS_DATE_END' then
  begin
    lstFieldTypeName := Copy(istFieldName, 1, Length(istFieldName) - Length('START') - 1) + '_TYPE';
    Result := 'IIF(ISNULL(ACCESS.' + istFieldName + '), NULL, CLng(ACCESS.' + istFieldName + ')) AS [' + istFieldName + ']';
  end
  else
    Result := 'ACCESS.[' + istFieldName + ']';
end;

{-------------------------------------------------------------------------------
}
constructor TTransferQuery.Create(const AAccessTableName, ASQLTableName,
  AAccessDBName: String; ASQLConnection, AAccessConnection: TADOConnection;
  AIncSysSupplied: boolean=false);
begin
  inherited Create;
  FSQLConnection := ASQLConnection;
  FAccessConnection := AAccessConnection;
  FAccessTableName := AAccessTableName;
  FAccessDBName := AAccessDBName;
  FSqlTableName := ASqlTableName;
  FIncSysSupplied := AIncSysSupplied;
  GetFieldLists;
end;

{-------------------------------------------------------------------------------
  Description : Returns an SQL statement that selects all the data in the Access
        table into a SQL Server temp table, ommitting records that are already
        on SQL Server
  Created : 04/03/2003 }
function TTransferQuery.GetAccessSelectRemainingSQL(const APrimaryKey: TPrimaryKey): string;
begin
  Result := GetSourceDataSQL([sdRemainingRecordsOnly]);
end;


{-------------------------------------------------------------------------------
  Description : Return SQL suitable for counting the records to transfer.  SQL
              must be run against the source Access db
  Created : 04/03/2003 }
function TTransferQuery.GetCountSourceSQL: string;
begin
  Result := GetSourceDataSQL([sdCount]);
end;


function TTransferQuery.GetCreateViewSQL(const AViewName: string): string;
begin
  result := Format(SQL_CREATE_VIEW, [AViewName, GetSourceDataLinkSQL]);
end;

{-------------------------------------------------------------------------------
  Description : Returns the field lists that are copied from Access to SQL
                server for splicing into SQL
  Created : 27/02/2003 }
procedure TTransferQuery.GetFieldLists;
var
  lFieldName: String;
  ladodsSchema: TADODataset;
  lRS: _Recordset;
  i: Integer;
  lAccessFields: TStringList;
begin
  FIntoFieldList := '';
  FFromOpenRowsetFieldList := '';
  FAccessFieldList := '';
  FSystemSuppliedExists := False;
  ladoDsSchema := TADODataset.Create(nil);
  lAccessFields := TStringList.Create;
  try
    FSQLConnection.OpenSchema(siColumns,
                           VarArrayOf([Null, Null, FSqlTableName, Null]),
                           EmptyParam,
                           ladoDsSchema);
    ladoDsSchema.First;
    // Gather fields from corresponding Access table, in case there are non-matching ones.
    // Get first record from access table, just to get all the fields.
    lRS := FAccessConnection.Execute('SELECT TOP 1 * FROM [' + FAccessTableName + ']');
    for i := 0 to lRS.Fields.Count - 1 do
      lAccessFields.Add(UpperCase(lRS.Fields.Item[i].Name));
    lRS.Close;
    // Want Custodian field to be allowed through, but it's not in Access, so pretend it is.
    lAccessFields.Add('CUSTODIAN');

    // Generate the list of fields to be SELECTed from the Access table and INSERTed into the
    // SQL server table
    while not ladoDsSchema.Eof do
      try
        lFieldName := Uppercase(ladoDsSchema.FieldByName('COLUMN_NAME').AsString);
        if ShouldMigrateField(lFieldName) and (lAccessFields.IndexOf(lFieldName) > -1) then
        begin
          if lFieldName = 'SYSTEM_SUPPLIED_DATA' then
            FSystemSuppliedExists := True;
          // add actual field we are selecting into
          AddToCommaSeparatedList(FIntoFieldList, '[' + lFieldName + ']');
          // and the access field we select from
          AddToCommaSeparatedList(FAccessFieldList, ModifyAccessField(lFieldName));
          // lastly, the field that is selected from the OpenRowset operation
          AddToCommaSeparatedList(FFromOpenRowsetFieldList, ModifyOpenRowsetField(lFieldName));
        end;
        ladoDSSchema.Next;
      except
        On E:Exception do begin
          raise ETransferQuery.Create('Error occurred during loop through field list - field ' +
              ladoDsSchema.FieldByName('COLUMN_NAME').AsString + ' in table ' + FSqlTableName, E);
        end; // on exception
      end; // try
  finally
    ladoDsSchema.Free;
    lAccessFields.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Description : Returns SQL suitable for copying data from a temp table into the
              main table copy, one record at a time.
              SQL must be formatted to insert the record key
  Created : 05/03/2003 }
function TTransferQuery.GetIndividualRecordInsertSQL(APrimaryKey : TPrimaryKey): string;
begin
  // retrieve additional custodian field if it is required
  Result := Format(SQL_INDIVIDUAL_INSERT, [
                  FSqlTableName,
                  FIntoFieldList,
                  FFromOpenRowsetFieldList,
                  APrimaryKey.Key1,
                  '%s'
                  ]);
  if APrimaryKey.Key2<>'' then
    Result := Result + ' AND ' + APrimaryKey.Key2 + '=''%s''';
end;


{-------------------------------------------------------------------------------
  Description : Returns the underlying data select that brings data across from
              SQL Server, including the OPENROWSET connection details
              if ASelectInto is selected, then a Select ... Into format is used
  Created : 04/03/2003 }
function TTransferQuery.GetSourceDataLinkSQL: string;
begin
  Result := Format(SQL_SOURCE_DATA_TEMPLATE, [FFromOpenRowsetFieldList,
                                              FAccessDBName,
                                              //FAccessDBPassword,
                                              GetSourceDataSQL([])]);
end;

{-------------------------------------------------------------------------------
  Description : Returns the underlying data select that brings data across from
              SQL Server - this is just the query as it would run on Access
              If ACount=True then the query is a count(*) query
  Created : 04/03/2003 }
function TTransferQuery.GetSourceDataSQL(AOptions : TSourceDataSQLOptions): string;
var
  lTempStrings : TStringList;
  lPrimaryKey : TPrimaryKey;
  lServerJoin : string;
  lTablesAndJoins : string;
  lWheres : string;
  lSysSuppliedAlias : string;

    // Add an item to a where clause, put in a WHERE or AND as required
    procedure AddToWhereClause(const AClause : string);
    begin
      if lWheres = '' then
        lWheres := 'WHERE ' + AClause
      else
        lWheres := lWheres + ' AND ' + AClause;
    end;

begin
  // if we want just the records that aren't already transferred, join to the
  // master db
  if (sdRemainingRecordsOnly in AOptions) then begin
    lPrimaryKey := dmDatabase.SplitPrimaryKey(FSQLTableName);
    lServerJoin := ' LEFT JOIN DBO_'+ FSQLTableName + ' AS SQLSRV'+
                ' ON SQLSRV.' + lPrimaryKey.Key1 +'=ACCESS.' + lPrimaryKey.Key1;
    AddToWhereClause('SQLSRV.' + lPrimaryKey.Key1 +' IS NULL');
    if lPrimaryKey.Key2 <> '' then
      AddToWhereClause('SQLSRV.' + lPrimaryKey.Key2 +'=ACCESS.' + lPrimaryKey.Key2);
  end else
    lServerJoin := '';
  lTempStrings := TStringList.Create;
  try
    lTablesAndJoins := '[' + FAccessTableName + '] AS ACCESS ';
    lSysSuppliedAlias := 'ACCESS'; // default
    // now get any special cases
    if Uppercase(FAccessTableName) = 'SOURCE' then begin
      lTablesAndJoins := lTablesAndJoins + 'LEFT JOIN [REFERENCE] [REF] ' +
                  'ON [ACCESS].[SOURCE_KEY] = [REF].[SOURCE_KEY]';
      lSysSuppliedAlias := 'REF';
    end else
    if Uppercase(FAccessTableName) = 'USER' then
      AddToWhereClause('[NAME_KEY] <> "TESTDATA00000001"')
    else
    if Uppercase(FAccessTableName) = 'TAXON_COMMON_NAME' then begin
      lTablesAndJoins := lTablesAndJoins + 'LEFT JOIN [TAXON_LIST_ITEM] [TLI] '
          + 'ON [ACCESS].[TAXON_LIST_ITEM_KEY] = [TLI].[TAXON_LIST_ITEM_KEY]';
      lSysSuppliedAlias := 'TLI';
    end else
    if (Uppercase(FAccessTableName) = 'INDEX_TAXON_SYNONYM') or
       (Uppercase(FAccessTableName) = 'INDEX_TAXON_NAME') or
       (Uppercase(FAccessTableName) = 'INDEX_TAXON_GROUP') then begin
      lTablesAndJoins := lTablesAndJoins + 'INNER JOIN [TAXON_LIST_ITEM] [TLI] '
                         + 'ON [ACCESS].[TAXON_LIST_ITEM_KEY] = [TLI].[TAXON_LIST_ITEM_KEY]';
      lSysSuppliedAlias := 'TLI';
    end else
    if (Uppercase(FAccessTableName) = 'REFERENCE_AUTHOR') or
       (Uppercase(FAccessTableName) = 'REFERENCE_NUMBER') then begin
      lTablesAndJoins := lTablesAndJoins + 'LEFT JOIN [REFERENCE] [R] ON '+
                                           '[ACCESS].[SOURCE_KEY] = [R].[SOURCE_KEY]';
      lSysSuppliedAlias := 'R';
    end;

    if (FSystemSuppliedExists or (lSysSuppliedAlias<>'ACCESS')) and
        (not FIncSysSupplied) then
      AddToWhereClause('['+lSysSuppliedAlias+'].SYSTEM_SUPPLIED_DATA = 0 ' +
          ' OR ['+lSysSuppliedAlias+'].SYSTEM_SUPPLIED_DATA IS NULL');
    // build up the SQL
    if (sdCount in AOptions) then
      lTempStrings.Text := Format(SQL_SOURCE_DATA_QUERY,
                                  ['count(*)', lTablesAndJoins + lServerJoin, lWheres])
    else
      lTempStrings.Text := Format(SQL_SOURCE_DATA_QUERY,
                                 [FAccessFieldList, lTablesAndJoins + lServerJoin, lWheres]);
    Result := lTempStrings.Text;
  finally
    lTempStrings.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Description : Return true if a field should be migrated
  Created : 31/3/2003 }
function TTransferQuery.ShouldMigrateField(const AFieldName: string): boolean;
begin
  Result := not ((FSqlTableName = 'USABLE_FIELD') and (AFieldName = 'CROSSTAB'));
end;

{-------------------------------------------------------------------------------
  Description : Returns a field to be selected from Access for migration, or
              a calculated field if required
  Created : 31/3/2003 }
function TTransferQuery.ModifyAccessField(const AFieldName : string) : string;
begin
  if AFieldName='CUSTODIAN' then
    Result := 'Left(ACCESS.[' + dmDatabase.GetPrimaryKey(FSqlTableName, False)+'],8) AS CUSTODIAN'
  else if ((FSqlTableName = 'LOCATION_BOUNDARY') and (AFieldName = 'MAP_FILE')) then
    Result := '" " AS MAP_FILE'
  else if ((FSqlTableName='REFERENCE_AUTHOR') or (FSqlTableName='REFERENCE_NUMBER')) and
           (AFieldName='SYSTEM_SUPPLIED_DATA') then
    Result := 'R.[SYSTEM_SUPPLIED_DATA]'
  else if (AFieldName='CHECKED_DATE') or (AFieldName='CHANGED_DATE') then
    // convert times in a date field to a null date
    Result := 'IIF(ACCESS.'+AFieldName+'<1, NULL, ACCESS.'+AFieldName+') AS ' + AFieldName
  else
    Result := ConvertDateTimeField(AFieldName);
end;


{-------------------------------------------------------------------------------
  Description : Returns a field to be selected from the OpenRowset for migration, or
              a calculated field if required
  Created : 14/02/2003 }
function TTransferQuery.ModifyOpenRowsetField(const AFieldName : string) : string;
begin
  if ((FSqlTableName = 'ADMIN_AREA') and (AFieldName = 'CHANGED_DATE')) then
    Result := 'CONVERT(smalldatetime, [CHANGED_DATE], 3) AS CHANGED_DATE'
  else
    Result := 'T1.[' + AFieldName + ']';
end;


end.
