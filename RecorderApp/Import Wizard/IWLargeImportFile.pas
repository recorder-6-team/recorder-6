{===============================================================================
  Unit:         IWLargeImportFile

  Defines:      TLargeImportFile
                TLargeFileTableRule

  Description:  Subclass of TImportFile with optimizations for dealing with
                large data files.

  Created:      February 2009

  Last revision information:
    $Revision: 48 $
    $Date: 08/04/13 16:46 $
    $Author: Michaelcaptain $

  Copyright © Dorset Software Services Ltd, 2009

===============================================================================}
unit IWLargeImportFile;

interface

uses
  Classes, DB, ADODB, Contnrs, DBGrids, IWColumnMappingClasses, IWTableRule,
  IWUserSuppliedData, IWSettings;

resourcestring
  ResStr_ParseAndValidate = 'Parsing and validating data...';

type
  TFastParseErrorEvent = procedure(Sender: TObject; const fieldName: String;
      errors: _Recordset) of object;

  { ----------------------------------------------------------------------------
    Helper class to keep track of column validation status.
  }
  TColumnValidationInfo = class
  private
    FMappedType: TColumnType;
    FErrorCount: Integer;
    FRequiresParsing: Boolean;
    procedure SetMappedType(const Value: TColumnType);
  public
    property MappedType: TColumnType read FMappedType write SetMappedType;
    property ErrorCount: Integer read FErrorCount write FErrorCount;
    property RequiresParsing: Boolean read FRequiresParsing write FRequiresParsing;
  end;

  { ----------------------------------------------------------------------------
    Import file with optimizations for dealing with large data files.
  }
  TLargeImportFile = class(TImportFile)
  private
    FValidationInfo: TStringList;
    FConfirmMappingChange: Boolean;
    FOnFastParseError: TFastParseErrorEvent;
  protected
    function CreateTableRule(const Key: String): TObject; override;
    procedure DropWorkingData(columnType: TColumnType); virtual;
    procedure PopulateChecksumTables; override;
    procedure PopulateChecksumTable(ColumnType: TColumnType); virtual;
    function UpdateMasterTableRecord(const fieldName: String;
        columnType: TColumnType; withCheck: Boolean): Boolean; virtual;
    function UpdateMultiRecordTableRecord(const fieldName: String;
        columnType: TColumnType; withCheck: Boolean): Boolean; virtual;
  public
    constructor Create(DataSource: TDataSet; LargeFields: TStringList;
        UserSuppliedData: TUserSuppliedData; UseOldImportWizard: Boolean); reintroduce;
    destructor Destroy; override;
    procedure InitialiseMatching; override;
    procedure ParseData; virtual;
    procedure PrepareData; override;
    procedure RemoveRow(rowID: Integer);
    procedure UpdateRowField(const fieldName: String);
    function ValidatedField(const fieldName: String): Boolean;
    property ConfirmMappingChange: Boolean read FConfirmMappingChange write FConfirmMappingChange;
    property ValidationInfo: TStringList read FValidationInfo;
    property OnFastParseError: TFastParseErrorEvent read FOnFastParseError write FOnFastParseError;
  end;

//==============================================================================
implementation

uses
  SysUtils, StrUtils, Forms, Controls, Maintbar, DatabaseAccessADO,
  IWConstants, IWParsers, IWOutputFieldGenerators, GeneralFunctions,
  ADOInt;

type
  { ----------------------------------------------------------------------------
    Table rule with optimizations for dealing with large data files.
  }
  TLargeFileTableRule = class(TTableRule)
  protected
    FGeneratedFields: TStringList;
    FColumnDefinitions: String;
    FSelectList: String;
    FTableSource: String;
    FGroupList: String;
    FAliasCount: Integer;
    FLastJoinTable: String;
    procedure Initialize();
    procedure AddRelatedTables;
    procedure AddGeneratingFields;
    procedure AddColumnTypeTable(const Table: String);
    procedure AddRelatedTable(const Table: String);
    procedure AddSimpleColumn(Column: TColumnType);
    procedure AddParsedColumn(TableName: String; Column: TColumnType;
        AddToGroupList: Boolean);
    procedure AddMultipleRecordColumn(Column: TColumnType);
    function UseGeneratedMappingTable: Boolean;
    procedure AddChecksumColumn;
    procedure AddUnmatchedField(const TableName: String; Column: TColumnType;
        FieldIndex: Integer; AddToGroupList: Boolean);
    procedure AddMatchedField(const TableName: String; Column: TColumnType;
        FieldIndex: Integer; Match: TMatchRule; AddToGroupList: Boolean);
    function CurrentAlias: String;
    procedure AddColumnDefinition(const Name, DataType: String);
  protected
    procedure EnsureGroups(const ColumnTypeKey: String); override;
    function OutputFieldByName(Name: String): TOutputField; virtual;
    procedure BuildGeneratingQuery; override;
    function IdentifyOutputRecordsSQL: String; virtual;
    function ReserveKeyValuesSQL: String; virtual;
    function GenerateOutputSQL: String; virtual;
    function MapOutputSQL: String; virtual;
    function GeneratedMappingTableSQL: String;
    function CopyOutputSQL: String; virtual;
  public
    constructor Create(const Key: String; ImportFile: TLargeImportFile);
    destructor Destroy; override;
    procedure Apply; override;
  end;

{ ------------------------------------------------------------------------------
  Adds a field name to the given comma separated list.
}
function AddField(const FieldList, FieldName: String): String;
begin
  if FieldList = '' then
    Result := FieldName
  else
    Result := FieldList + IfThen(FieldName <> '', ','#10#9 + FieldName, '');
end;

{===============================================================================
 TLargeImportFile
===============================================================================}
{ ------------------------------------------------------------------------------
}
constructor TLargeImportFile.Create(DataSource: TDataSet; LargeFields: TStringList;
    UserSuppliedData: TUserSuppliedData; UseOldImportWizard: Boolean);
begin
  inherited;

  FValidationInfo := TStringList.Create;
  FConfirmMappingChange := True;
end;

{ ------------------------------------------------------------------------------
}
destructor TLargeImportFile.Destroy;
var
  i: Integer;
begin
  if assigned(FValidationInfo) then begin
    for i := 0 to FValidationInfo.Count - 1 do
      FValidationInfo.Objects[i].Free;
    FValidationInfo.Free;
  end;
  inherited;
end;

{ ------------------------------------------------------------------------------
  Instantiates the table rule object for the given key.
}
function TLargeImportFile.CreateTableRule(const Key: String): TObject;
begin
  Result := TLargeFileTableRule.Create(Key, Self);  
end;

{ ------------------------------------------------------------------------------
  For each column type where there may be multiple values from a single field
  in the import data, calculates checksums over all of the values for each
  record in the source file.

  These checksums are used (for generating fields) in table rule processing,
  to determine when a new record in an output table is required.
}
procedure TLargeImportFile.PopulateChecksumTables;
var
  I: Integer;
  ColumnType: TColumnType;
  Keys: TStringList;
begin
  Keys := TStringList.Create;
  try
    Keys.Sorted := True;
    for I := 0 to FMultiRecordTableTypes.Count - 1 do
    begin
      ColumnType := TColumnType(FMultiRecordTableTypes[i]);
      if Keys.IndexOf(ColumnType.Key) = -1 then
      begin
        Keys.Add(ColumnType.Key);
        PopulateChecksumTable(ColumnType);
      end;
    end;
  finally
    Keys.Free;
    frmMain.SetProgress(0);
    frmMain.SetStatus('');
  end;
end;

{ ------------------------------------------------------------------------------
  Calculates checksums over all of the values in the specified column type
  for each record in the source file.

  For column types that support qualification (i.e. Taxon Occurrence Data)
  a separate checksum is generated for each sub-type that is mapped.
}
procedure TLargeImportFile.PopulateChecksumTable(ColumnType: TColumnType);
var
  Fields: String;
  Tables: String;
  I: Integer;
  Field: TParsedField;
  Aliases: Integer;
  Sql: String;

  function Alias: String;
  begin
    Result := 't' + IntToStr(Aliases);
  end;

begin
  Fields := '';
  Tables := '#CT_' + ColumnType.Key + ' AS ct';

  for I := 0 to ColumnType.Parser.FieldCount - 1 do
  begin
    Field := ColumnType.Parser.Fields[I];
    if I > 0 then Fields := Fields + ', ';
    if ColumnType.FieldMatchRuleKeys[I] = '' then
      Fields := Fields + Field.Name
    else
    begin
      Fields := Fields + Alias + '.Match_Key + ' + Alias + '.Match_Value';
      Tables := Tables + ' LEFT JOIN #'
          + MatchRuleByKey(ColumnType.FieldMatchRuleKeys[I]).Name
          + ' AS ' + Alias
          + ' ON ' + Alias + '.Import_Value = ct.' + Field.Name;
      Inc(Aliases);
    end;
  end;

  Sql := 'INSERT INTO #CS_' + ColumnType.Key + ' (Record_No, Checksum)'
      + ' SELECT ct.Record_No, CHECKSUM_AGG(CHECKSUM(' + Fields + '))'
      + ' FROM ' + Tables
      + ' GROUP BY ct.Record_No, ct.IW_Qualification';

  dmDatabase.ExecuteSql(Sql);
end;

{ ------------------------------------------------------------------------------
  Prepares the temp tables and column ready for the data. Alters existing
  structure as needed.
}
procedure TLargeImportFile.PrepareData;
var
  i, idx: Integer;
  mappedType: TColumnType;
  cvInfo: TColumnValidationInfo;
  sql, columns: String;
begin
  // Ensure the #Master table exists so we can add/remove columns.
  sql := 'IF Object_Id(''tempdb..#Master'') IS NULL'
      + #10#9'CREATE TABLE #Master (Record_No INT PRIMARY KEY)';
  dmDatabase.ExecuteSQL(sql);

  for i := 0 to FDataSource.FieldCount - 1 do
  begin
    idx        := FValidationInfo.IndexOf(FDataSource.Fields[i].FieldName);
    mappedType := ColumnMapping.MappedType(FDatasource.Fields[i].FieldName);
    // Column is mapped. New or changed.
    if Assigned(mappedType) then
    begin
      // Always parse the Determination Date column. Needs to be compared against
      // the Date column with every validation. Always parse the date and grid
      // reference columns because they need to be checked against the survey.
      // Always parse the spatial reference system because its validity is tied
      // to the validity of the grid reference column for historical reasons --
      // if TSpatialRefSystemParser is fixed to actually validate the spatial
      // reference system then this case can be removed.
      if (idx > -1) and
        ((mappedType.Key = CT_KEY_DETERMINATION_DATE) or
         (mappedType.Key = CT_KEY_DATE) or
         (mappedType.Key = CT_KEY_GRID_REFERENCE) or
         (mappedType.Key = CT_KEY_SPATIAL_SYSTEM)) then
      begin
        TColumnValidationInfo(FValidationInfo.Objects[idx]).RequiresParsing := True;
      end;
      // Newly mapped.
      if idx = -1 then
      begin
        cvInfo := TColumnValidationInfo.Create;
        cvInfo.MappedType := mappedType;
        FValidationInfo.AddObject(FDataSource.Fields[i].FieldName, cvInfo);
      end
      // Need to update existing. Also needs to be done in case column needs reparsing.
      else if (TColumnValidationInfo(FValidationInfo.Objects[idx]).MappedType <> mappedType)
         or TColumnValidationInfo(FValidationInfo.Objects[idx]).RequiresParsing then
      begin
        DropWorkingData(TColumnValidationInfo(FValidationInfo.Objects[idx]).MappedType);
        TColumnValidationInfo(FValidationInfo.Objects[idx]).MappedType := mappedType;
      end;
    end else
    // Column is no longer mapped, but clean up required.
    if idx > -1 then
    begin
      DropWorkingData(TColumnValidationInfo(FValidationInfo.Objects[idx]).MappedType);
      FValidationInfo.Objects[idx].Free;
      FValidationInfo.Delete(idx);
    end;
  end;
  // Add all necessary columns and tables.
  sql := '';
  for i := 0 to FValidationInfo.Count - 1 do
  begin
    cvInfo := TColumnValidationInfo(FValidationInfo.Objects[i]);
    if cvInfo.RequiresParsing then
    begin
      columns := GetMasterColumn(cvInfo.MappedType, True);
      // Get all the master columns together.
      if columns <> '' then
      begin
        if sql <> '' then sql := sql + ',';
        sql := sql + #10#9' ' + StringReplace(columns, ',', ','#10#9, [rfReplaceAll]);
      end else begin
        CreateImportTable(cvInfo.MappedType);
        FMultiRecordTableTypes.Add(cvInfo.MappedType);
      end;
    end;
  end;
  // Add all Master columns in one hit.
  if sql <> '' then
    dmDatabase.ExecuteSQL('ALTER TABLE #Master ADD' + sql);
end;  // TLargeImportFile.PrepareData

{-------------------------------------------------------------------------------
  Drop column from #master and/or temp table associated with the specified
  column type.
}
procedure TLargeImportFile.DropWorkingData(columnType: TColumnType);
var
  sql, columns: String;
  idx: Integer;
  dropTable: Boolean;
begin
  // If column(s) in #master, drop.
  columns := GetMasterColumn(columnType, False);
  if columns <> '' then begin
    sql := 'ALTER TABLE #Master DROP COLUMN'
        + #10#9' ' + StringReplace(columns, ',', ','#10#9, [rfReplaceAll]);
    dmDatabase.ExecuteSQL(sql);
  end else
  // Multi-records, separate temp tables.
  if Assigned(columnType.Parser) then
  begin
    dropTable := True;

    // Measurements all go in same table! So need to be a bit more clever here.
    if columnType is TTaxonOccurrenceDataColumnType then
    begin
      // Remove all the "dropped" data by removing the records.
      sql := 'IF Object_Id(''tempdb..#CT_' + columnType.Key + ''') IS NOT NULL'
          + #10#9'DELETE FROM #CT_' + columnType.Key
          + #10#9'WHERE  IW_Qualification = '''
              + TTaxonOccurrenceDataColumnType(columnType).Qualification + '''';
      dmDatabase.ExecuteSQL(sql);

      // Do we need top drop the table too? Yes if empty.
      sql := 'IF Object_Id(''tempdb..#CT_' + columnType.Key + ''') IS NOT NULL'
          + #10#9'IF EXISTS(SELECT TOP 1 * FROM #CT_' + columnType.Key + ')'
          + #10#9#9'SELECT 1 AS HasRecords'
          + #10#9'ELSE'
          + #10#9#9'SELECT 0 AS HasRecords'
          + #10'ELSE'
          + #10#9'SELECT 0 AS HasRecords';
      with dmDatabase.ExecuteSQL(sql, True) do
        dropTable := Fields['HasRecords'].Value = 0;
    end;

    if dropTable then begin
      // Remove reference to created table for column type.
      idx := FMultiRecordTableTypes.IndexOf(columnType);
      if idx <> -1 then
        FMultiRecordTableTypes.Delete(idx);

      // And drop associated tables from DB.
      DropTemporaryTable('CT_' + columnType.Key);
      DropTemporaryTable('CS_' + columnType.Key);
    end;
  end;
end;  // TLargeImportFile.DropWorkingData

{-------------------------------------------------------------------------------
  Ensure the MatchRules are cleared before initialising matching from mapped
  column types. This is required to avoid left-over data when some rows are
  removed after the matching process started.
}
procedure TLargeImportFile.InitialiseMatching;
begin
  ClearMatchRules;
  inherited;
end;  // TLargeImportFile.InitialiseMatching;

{-------------------------------------------------------------------------------
  Validates data, prepares error lists and populates temp tables.
}
procedure TLargeImportFile.ParseData;
var
  i: Integer;
  cvInfo: TColumnValidationInfo;
  sql, sqlDelete, tableName: String;
  columnsToValidate: TStringList;

  {-----------------------------------------------------------------------------
   Check for any inter-dependencies between columns. No update of #Master or other
   temp tables, just counting failures.
  }
  function ValidateDependencies(const fieldName: String): _Recordset;
  var
    i: Integer;
    relatedType: TRelatedType;
    relatedCol: TColumnType;
    sql: String;
  begin
    Result := nil;
    sql := '';
    for i := 0 to cvInfo.MappedType.RelatedTypeCount - 1 do
    begin
      relatedType := cvInfo.MappedType.RelatedTypes[i];
      relatedCol  := ColumnMapping.ColumnTypeByKey(relatedType.ColumnTypeKey);
      // Only proceed if the related column has been mapped for import.
      if Assigned(relatedCol) and ColumnMapping.IsMapped(relatedCol) then
      begin
        if (relatedType.RelationshipType in
            [rtRequires, rtFieldDependencyColumnRequired, rtFieldDependency]) then
        begin
          // A value is require in the other field, so check it exists
          sql := sql + IfThen(sql = '', '', #10'OR     ')
              + Format(
                  '(I."%s" <> '''' AND I."%s" = '''')',
                  [fieldName, ColumnMapping.MappedColumn(relatedCol)]);
        end else
        if relatedType.RelationshipType = rtOneRequired then
        begin
          // It's all a bit weird with the location stuff, but they asked for it...
          // GridRef and LocationName work kinda together.
          if cvInfo.MappedType is TLocationInfoColumnType then
          begin
            with TLocationInfoColumnType(cvInfo.MappedType) do
            begin
              if GridRefMapped and not LocationMapped and LocationNameMapped then
                 sql := sql + IfThen(sql = '', '', #10'OR     ')
                     + Format(
                     '(I."%s" = '''' AND I."%s" = '''')',
                     [ColumnMapping.MappedColumn(ColumnMapping.ColumnTypeByKey(CT_KEY_GRID_REFERENCE)),
                     ColumnMapping.MappedColumn(ColumnMapping.ColumnTypeByKey(CT_KEY_LOCATION_NAME))])
              else
              if  Not GridRefMapped and LocationMapped and LocationNameMapped then
                  sql := sql + IfThen(sql = '', '', #10'OR     ')
                     + Format(
                     '(I."%s" = '''' AND I."%s" = '''')',
                     [ColumnMapping.MappedColumn(ColumnMapping.ColumnTypeByKey(CT_KEY_LOCATION)),
                     ColumnMapping.MappedColumn(ColumnMapping.ColumnTypeByKey(CT_KEY_LOCATION_NAME))])
              else
              if  GridRefMapped and LocationMapped and not LocationNameMapped then
                  sql := sql + IfThen(sql = '', '', #10'OR     ')
                     + Format(
                     '(I."%s" = '''' AND I."%s" = '''')',
                     [ColumnMapping.MappedColumn(ColumnMapping.ColumnTypeByKey(CT_KEY_LOCATION)),
                     ColumnMapping.MappedColumn(ColumnMapping.ColumnTypeByKey(CT_KEY_GRID_REFERENCE))])
              else
              if  GridRefMapped and LocationMapped and LocationNameMapped then
                  sql := sql + IfThen(sql = '', '', #10'OR     ')
                      + Format(
                      '(I."%s" = '''' AND  I."%s" = '''' AND I."%s" = '''')',
                      [ColumnMapping.MappedColumn(ColumnMapping.ColumnTypeByKey(CT_KEY_LOCATION_NAME)),
                      ColumnMapping.MappedColumn(ColumnMapping.ColumnTypeByKey(CT_KEY_LOCATION)),
                      ColumnMapping.MappedColumn(ColumnMapping.ColumnTypeByKey(CT_KEY_GRID_REFERENCE))])
              end;
          end
          else
          // One or the other required.
            sql := sql + IfThen(sql = '', '', #10'OR     ')
                + Format(
                    '(I."%s" = '''' AND I."%s" = '''')',
                    [fieldName, ColumnMapping.MappedColumn(relatedCol)]);
        end;
      end;
    end;
    if sql <> '' then
    begin
      sql := Format('SELECT %s'
          + #10'FROM   %s I'
          + #10'WHERE  %s',
          [FLD_ROWID,
           TADOTable(FDataSource).TableName,
           sql]);
      Result := dmDatabase.ExecuteSQL(sql, True);
    end;
  end;  // ValidateDependencies

  {-----------------------------------------------------------------------------
   Generate a single UPDATE statement for all the mapped columns that have no
   associated parsers. No failures, everything goes in.
  }
  procedure UpdateForSingleFieldsNoParsers;
  var
    i: Integer;
  begin
    sql := '';
    for i := columnsToValidate.Count - 1 downto 0 do
    begin
      cvInfo := TColumnValidationInfo(columnsToValidate.Objects[i]);
      if (cvInfo.MappedType.FieldType <> '')
         and not Assigned(cvInfo.MappedType.Parser) then
      begin
        sql := sql + IfThen(sql <> '', ',', '')
            + #10'       ' + GetMasterColumn(cvInfo.MappedType, False)
                + ' = I."' + columnsToValidate[i] + '"';

        cvInfo.RequiresParsing := False;
        columnsToValidate.Delete(i);
      end;
    end;

    if sql <> '' then begin
      sql := 'UPDATE #Master '
          + #10'SET    ' + sql
          + #10'FROM   ' + TADOTable(FDataSource).TableName + ' I'
          + #10'JOIN   #Master   M ON M.Record_No = I."' + FLD_ROWID + '"';
      dmDatabase.ExecuteSQL(sql);
    end;
  end;  // UpdateForSingleFieldsNoParsers

  {-----------------------------------------------------------------------------
   Generate UPDATE statements for all the mapped columns that are associated with
   either TRequiredTextParser or TTextParser and resulting in only one parsed field.
   Run separate statements to get a count of invalid records.
  }
  function UpdateForTextParsers(const fieldName: String;
      parser: TTextParser): _Recordset;
  begin
    sql := '';
    // Length check.
    if TTextParser(parser).MaximumLength > 0 then
      sql := 'DATALENGTH(dbo.ufn_TrimWhiteSpaces(I."' + fieldName + '")) <= '
          + IntToStr(TTextParser(parser).MaximumLength);
    // Value required check.
    if parser is TRequiredTextParser then
    begin
      if sql <> '' then sql := sql + #10'AND    ';
      sql := sql
          + 'ISNULL(I."' + fieldName + '", '''') <> ''''';
    end;
    sql := Format('UPDATE #Master'
        + #10'SET    %s = dbo.ufn_TrimWhiteSpaces(I."%s")'
        + #10'FROM   %s I'
        + #10'JOIN   #Master   M ON M.Record_No = I."%s"'
        + #10'WHERE  %s',
        [GetMasterColumn(cvInfo.MappedType, False),
         fieldName,
         TADOTable(FDataSource).TableName,
         FLD_ROWID,
         sql]);
    dmDatabase.ExecuteSQL(sql);

    // Count how many records failed the validation.
    sql := '';
    // Length check.
    if TTextParser(parser).MaximumLength > 0 then
      sql := 'DATALENGTH(dbo.ufn_TrimWhiteSpaces(I."' + fieldName + '")) > '
          + IntToStr(TTextParser(parser).MaximumLength);
    // Value required check.
    if parser is TRequiredTextParser then
    begin
      if sql <> '' then sql := sql + #10'OR     ';
      sql := sql
          + 'ISNULL(I."' + fieldName + '", '''') = ''''';
    end;
    sql := Format('SELECT %s'
        + #10'FROM   %s I'
        + #10'WHERE  %s',
        [FLD_ROWID,
         TADOTable(FDataSource).TableName,
         sql]);
    Result := dmDatabase.ExecuteSQL(sql, True);
  end;  // UpdateForTextParsers

  {-----------------------------------------------------------------------------
   Generate UPDATE statements for all the mapped columns that are associated with
   a TBooleanParser and resulting in only one parsed field.
   Run separate statements to get a count of invalid records.
  }
  function UpdateForBooleanParsers(const fieldName: String;
      parser: TBooleanParser): _Recordset;
  var
    trueChoices, falseChoices: String;
  begin
    trueChoices  := Format('''1'', ''-1'', ''%s'', ''%s'', ''Y''', [ResStr_Yes, ResStr_True]);
    falseChoices := Format('''0'', ''%s'', ''%s'', ''N''', [ResStr_No, ResStr_False]);

    sql := Format('UPDATE #Master'
        + #10'SET    %s = CASE'
        + #10#9'WHEN dbo.ufn_TrimWhiteSpaces(I."%s") IN (%s) THEN 1'
        + #10#9'WHEN dbo.ufn_TrimWhiteSpaces(I."%s") IN (%s) THEN 0'
        + #10#9'END'
        + #10'FROM   %s I'
        + #10'JOIN   #Master   M ON M.Record_No = I."%s"'
        + #10'WHERE  dbo.ufn_TrimWhiteSpaces(I."%s") IN (%s, %s)',
        [GetMasterColumn(cvInfo.MappedType, False),
         fieldName, trueChoices,
         fieldName, falseChoices,
         TADOTable(FDataSource).TableName,
         FLD_ROWID,
         fieldName, trueChoices, falseChoices]);
    dmDatabase.ExecuteSQL(sql);

    // Count how many records failed the validation.
    sql := Format('SELECT %s'
        + #10'FROM   %s I'
        + #10'WHERE  "%s" NOT IN (%s, %s)',
        [FLD_ROWID,
         TADOTable(FDataSource).TableName,
         fieldName, trueChoices, falseChoices]);
    Result := dmDatabase.ExecuteSQL(sql, True);
  end;  // UpdateForBooleanParsers

  {-----------------------------------------------------------------------------
   Generate UPDATE statements for all the mapped columns that are associated with
   a TBRCSourceParser and resulting in only one parsed field.
   Run separate statements to get a count of invalid records.
  }
  function UpdateForBRCSourceParser(const fieldName: String;
      parser: TBRCSourceParser): _Recordset;
  begin
    sql := Format('UPDATE #Master'
        + #10'SET    %s = CASE dbo.ufn_TrimWhiteSpaces(I."%s") '
        + #10#9'WHEN ''FLD'' THEN ''NBNSYS0000000026'''
        + #10#9'WHEN ''MUS'' THEN ''NBNSYS0000000084'''
        + #10#9'WHEN ''LIT'' THEN ''NBNSYS0000000083'''
        + #10#9'END'
        + #10'FROM   %s I'
        + #10'JOIN   #Master   M ON M.Record_No = I."%s"'
        + #10'WHERE  dbo.ufn_TrimWhiteSpaces(I."%s") IN (''FLD'', ''MUS'', ''LIT'')',
        [GetMasterColumn(cvInfo.MappedType, False),
         fieldName,
         TADOTable(FDataSource).TableName,
         FLD_ROWID,
         fieldName]);
    dmDatabase.ExecuteSQL(sql);

    // Count how many records failed the validation.
    sql := Format('SELECT %s'
        + #10'FROM   %s I'
        + #10'WHERE  dbo.ufn_TrimWhiteSpaces(I."%s") NOT IN (''FLD'', ''MUS'', ''LIT'')',
        [FLD_ROWID,
         TADOTable(FDataSource).TableName,
         fieldName]);
    Result := dmDatabase.ExecuteSQL(sql, True);
  end;  // UpdateForBRCSourceParser

  {-----------------------------------------------------------------------------
   Generate UPDATE statements for all the mapped columns that are associated with
   a TViceCountyNumberParser and resulting in only one parsed field.
   Run separate statements to get a count of invalid records.
  }
  function UpdateForViceCountyNumberParser(const fieldName: String;
      parser: TViceCountyNumberParser): _Recordset;
  begin
    sql := Format('UPDATE #Master'
        + #10'SET    %s = L.Location_Key'
        + #10'FROM   %s I'
        + #10'JOIN   #Master   M ON  M.Record_No       = I."%s"'
        + #10'JOIN   Location  L ON  File_Code         = dbo.ufn_TrimWhiteSpaces(I."%s") COLLATE SQL_Latin1_General_CP1_CI_AS'
        + #10'                   AND Location_Type_Key = ''JNCCIMPW00000001''',
        [GetMasterColumn(cvInfo.MappedType, False),
         TADOTable(FDataSource).TableName,
         FLD_ROWID,
         fieldName]);
    dmDatabase.ExecuteSQL(sql);

    // Count how many records failed the validation.
    sql := Format('SELECT    %s'
        + #10'FROM      %s I'
        + #10'LEFT JOIN Location  L ON  File_Code         = dbo.ufn_TrimWhiteSpaces(I."%s") COLLATE SQL_Latin1_General_CP1_CI_AS'
        + #10'                      AND Location_Type_Key = ''JNCCIMPW00000001'''
        + #10'WHERE     L.File_Code IS NULL',
        [FLD_ROWID,
         TADOTable(FDataSource).TableName,
         fieldName]);
    Result := dmDatabase.ExecuteSQL(sql, True);
  end;  // UpdateForViceCountyNumberParser

  {-----------------------------------------------------------------------------
   Generate UPDATE statements for all the mapped columns that are associated with
   a TSiteIDParser and resulting in only one parsed field.
   Run separate statements to get a count of invalid records.
  }
  function UpdateForSiteIDParser(const fieldName: String;
      parser: TSiteIDParser): _Recordset;
  begin
    sql := Format('UPDATE #Master'
        + #10'SET    %s = dbo.ufn_TrimWhiteSpaces(I."%s")'
        + #10'FROM   %s I'
        + #10'JOIN   #Master   M ON M.Record_No = I."%s"'
        + #10'WHERE  DATALENGTH(ISNULL(dbo.ufn_TrimWhiteSpaces(I."%s"), '''')) = 8'
        + #10'AND    CHARINDEX('' '', dbo.ufn_TrimWhiteSpaces(I."%s")) = 0',
        [GetMasterColumn(cvInfo.MappedType, False),
         fieldName,
         TADOTable(FDataSource).TableName,
         FLD_ROWID,
         fieldName,
         fieldName]);
    dmDatabase.ExecuteSQL(sql);

    sql := Format('SELECT %s'
        + #10'FROM   %s I'
        + #10'WHERE  DATALENGTH(ISNULL(dbo.ufn_TrimWhiteSpaces(I."%s"), '''')) <> 8'
        + #10'OR     CHARINDEX('' '', dbo.ufn_TrimWhiteSpaces(I."%s")) <> 0',
        [FLD_ROWID,
         TADOTable(FDataSource).TableName,
         fieldName,
         fieldName]);
    Result := dmDatabase.ExecuteSQL(sql, True);
  end;  // UpdateForSiteIDParser

  {-----------------------------------------------------------------------------
   Generate UPDATE statements for all the mapped columns that are associated with
   a TRecordIDParser and resulting in only one parsed field.
   Run separate statements to get a count of invalid records.
  }
  function UpdateForRecordIDParser(const fieldName: String;
      parser: TRecordIDParser): _Recordset;
  begin
    sql := Format('UPDATE #Master'
        // Difference with TRecordIDParser, we need 8 chars now.
        + #10'SET    %s = Right(''00000000'' + dbo.ufn_TrimWhiteSpaces(I."%s"), 8)'
        + #10'FROM   %s I'
        + #10'JOIN   #Master   M ON M.Record_No = I."%s"'
        + #10'WHERE  DATALENGTH(ISNULL(dbo.ufn_TrimWhiteSpaces(I."%s"), '''')) BETWEEN 1 AND 6'
        + #10'AND    CHARINDEX('' '', dbo.ufn_TrimWhiteSpaces(I."%s")) = 0',
        [GetMasterColumn(cvInfo.MappedType, False),
         fieldName,
         TADOTable(FDataSource).TableName,
         FLD_ROWID,
         fieldName,
         fieldName]);
    dmDatabase.ExecuteSQL(sql);

    sql := Format('SELECT %s'
        + #10'FROM   %s I'
        + #10'WHERE  DATALENGTH(ISNULL(dbo.ufn_TrimWhiteSpaces(I."%s"), '''')) NOT BETWEEN 1 AND 6'
        + #10'OR     CHARINDEX('' '', dbo.ufn_TrimWhiteSpaces(I."%s")) <> 0',
        [FLD_ROWID,
         TADOTable(FDataSource).TableName,
         fieldName,
         fieldName]);
    Result := dmDatabase.ExecuteSQL(sql, True);
  end;  // UpdateForRecordIDParser

  {-----------------------------------------------------------------------------
   Validate columns that end up as single record/single field in one go.
  }
  procedure ParseSingleFieldColumns;
  var
    i: Integer;
    parser: TImportWizardParser;
    columnParsed: Boolean;
    errors, dependencyErrors: _Recordset;
  begin
    for i := columnsToValidate.Count - 1 downto 0 do
    begin
      cvInfo := TColumnValidationInfo(columnsToValidate.Objects[i]);
      parser := cvInfo.MappedType.Parser;

      if parser.SingleRecord
          and (parser.FieldCount = 1)
          and not (parser is TSpatialRefSystemParser) then
              // HACK: we treat a column with the spatial ref system parser as
              // a multi-field column (even though it is not) to ensure that
              // the actual parser object is invoked.
      begin
        // Assume column will be parsed.
        columnParsed := True;
        if parser is TTextParser then
          errors := UpdateForTextParsers(
              columnsToValidate[i],
              TTextParser(parser))
        else
        if parser is TBooleanParser then
          errors := UpdateForBooleanParsers(
              columnsToValidate[i],
              TBooleanParser(parser))
        else
        if parser is TBRCSourceParser then
          errors := UpdateForBRCSourceParser(
              columnsToValidate[i],
              TBRCSourceParser(parser))
        else
        if parser is TViceCountyNumberParser then
          errors := UpdateForViceCountyNumberParser(
              columnsToValidate[i],
              TViceCountyNumberParser(parser))
        else
        if parser is TSiteIDParser then
          errors := UpdateForSiteIDParser(
              columnsToValidate[i],
              TSiteIDParser(parser))
        else
        if parser is TRecordIDParser then
          errors := UpdateForRecordIDParser(
              columnsToValidate[i],
              TRecordIDParser(parser))
        else
          columnParsed := False;

        // Check for any inter-dependencies between columns.
        if columnParsed then
          dependencyErrors := ValidateDependencies(columnsToValidate[i]);

        // Column was parsed, and we have a recordset of errors back.
        if columnParsed then
        begin
          if Assigned(FOnFastParseError) then
            FOnFastParseError(parser, columnsToValidate[i], errors);
          cvInfo.ErrorCount := errors.RecordCount;

          if Assigned(FOnFastParseError) and Assigned(dependencyErrors) then
          begin
            FOnFastParseError(parser, columnsToValidate[i], dependencyErrors);
            cvInfo.ErrorCount := cvInfo.ErrorCount + dependencyErrors.RecordCount;
          end;

          cvInfo.RequiresParsing := False;
          columnsToValidate.Delete(i);
        end;
      end;
    end;
  end;  // ParseSingleFieldColumns

  {-----------------------------------------------------------------------------
   Validate Taxon Data columns.
  }
  procedure ParseTaxonDataColumns;
  var
    i: Integer;
    parser: TTaxonDataParser;
    errors: _Recordset;
  begin
    for i := columnsToValidate.Count - 1 downto 0 do
    begin
      cvInfo := TColumnValidationInfo(columnsToValidate.Objects[i]);
      if cvInfo.MappedType.Parser is TTaxonDataParser then
      begin
        parser := TTaxonDataParser(cvInfo.MappedType.Parser);

        sql := Format(
            'INSERT INTO #CT_%s (Record_No, Position, IW_Qualification, %s, %s, %s, %s)'
            + #10'SELECT IMP.%s,'                     // __ID__
            + #10'       %d,'                         // Position
            + #10'       ''%s'','                     // IW_Qualification
            + #10'       IMP.TrimmedData,'            // Data
            + #10'       ''%s'','                     // Measurement Qualifier Key
            + #10'       ''%s'','                     // Measurement Unit Key
            + #10'       CASE WHEN IsNumeric(IMP.OriginalData) = 0 THEN ''%s'' ELSE ''%s'' END'
            + #10'FROM   (SELECT %s, dbo.ufn_TrimWhiteSpaces("%s") AS OriginalData, '
                  // 3 cases to exactly replicate the parsing code: "~...?", "~..." and "...?"
                  // Want the same results at the end.
            + #10'        CASE'
            + #10'            WHEN LEFT(dbo.ufn_TrimWhiteSpaces("%s"), 1) = ''~''  AND'
            + #10'                 RIGHT(dbo.ufn_TrimWhiteSpaces("%s"), 1) = ''?'' THEN'
            + #10'                SUBSTRING(dbo.ufn_TrimWhiteSpaces("%s"), 2, DATALENGTH(dbo.ufn_TrimWhiteSpaces("%s")) - 2)'
            + #10'            WHEN LEFT(dbo.ufn_TrimWhiteSpaces("%s"), 1) = ''~''  THEN'
            + #10'                RIGHT(dbo.ufn_TrimWhiteSpaces("%s"), DATALENGTH(dbo.ufn_TrimWhiteSpaces("%s")) - 1)'
            + #10'            WHEN RIGHT(dbo.ufn_TrimWhiteSpaces("%s"), 1) = ''?'' THEN'
            + #10'                LEFT(dbo.ufn_TrimWhiteSpaces("%s"), DATALENGTH(dbo.ufn_TrimWhiteSpaces("%s")) - 1)'
            + #10'            ELSE dbo.ufn_TrimWhiteSpaces("%s")'
            + #10'        END AS TrimmedData'
            + #10'        FROM %s) IMP'
            + #10'%s'                                 // Extra check for restricted values
            + #10'WHERE  DATALENGTH(IMP.TrimmedData) BETWEEN 1 AND 20'  // Skip empty values too.
            + #10'%s',                                // Extra check for restricted values
            [cvInfo.MappedType.Key,
             FLD_DATA, FLD_DATAQUALIFIER, FLD_DATAUNIT, FLD_ACCURACY,
             FLD_ROWID,
             parser.Position,
             cvInfo.MappedType.Qualification,
             parser.QualifierKey,
             parser.UnitKey,
             ResStr_Estimate, ResStr_Exact,
             FLD_ROWID, columnsToValidate[i],
             columnsToValidate[i], columnsToValidate[i], columnsToValidate[i], columnsToValidate[i],
             columnsToValidate[i], columnsToValidate[i], columnsToValidate[i],
             columnsToValidate[i], columnsToValidate[i], columnsToValidate[i],
             columnsToValidate[i],
             TADOTable(FDataSource).TableName,
             IfThen(parser.RestrictedValues.Count = 0, '',
                 'JOIN   Measurement_Unit_Value	MUV ON MUV.Data = IMP.TrimmedData'),
             IfThen(parser.RestrictedValues.Count = 0, '',
                 'AND   MUV.Measurement_Unit_Key = ''' + parser.UnitKey + '''')]);
        dmDatabase.ExecuteSQL(sql);

        sql := Format(
            'SELECT   IMP.%s'                         // __ID__
            + #10'FROM   (SELECT %s,'
            + #10'        CASE'
            + #10'            WHEN LEFT(dbo.ufn_TrimWhiteSpaces("%s"), 1) = ''~''  AND'
            + #10'                 RIGHT(dbo.ufn_TrimWhiteSpaces("%s"), 1) = ''?'' THEN'
            + #10'                SUBSTRING(dbo.ufn_TrimWhiteSpaces("%s"), 2, DATALENGTH(dbo.ufn_TrimWhiteSpaces("%s")) - 2)'
            + #10'            WHEN LEFT(dbo.ufn_TrimWhiteSpaces("%s"), 1) = ''~''  THEN'
            + #10'                RIGHT(dbo.ufn_TrimWhiteSpaces("%s"), DATALENGTH(dbo.ufn_TrimWhiteSpaces("%s")) - 1)'
            + #10'            WHEN RIGHT(dbo.ufn_TrimWhiteSpaces("%s"), 1) = ''?'' THEN'
            + #10'                LEFT(dbo.ufn_TrimWhiteSpaces("%s"), DATALENGTH(dbo.ufn_TrimWhiteSpaces("%s")) - 1)'
            + #10'            ELSE dbo.ufn_TrimWhiteSpaces("%s")'
            + #10'        END AS TrimmedData'
            + #10'        FROM %s) IMP'
            + #10'%s'                                 // Extra check for restricted values
            + #10'WHERE  DATALENGTH(IMP.TrimmedData) > 20'   // Ignore empty values.
            + #10'%s',                                // Extra check for restricted values
            [FLD_ROWID,
             FLD_ROWID,
             columnsToValidate[i], columnsToValidate[i], columnsToValidate[i], columnsToValidate[i],
             columnsToValidate[i], columnsToValidate[i], columnsToValidate[i],
             columnsToValidate[i], columnsToValidate[i], columnsToValidate[i],
             columnsToValidate[i],
             TADOTable(FDataSource).TableName,
             IfThen(parser.RestrictedValues.Count = 0, '',
                 'LEFT JOIN Measurement_Unit_Value	MUV ON MUV.Data = IMP.TrimmedData AND MUV.Measurement_Unit_Key = ''' + parser.UnitKey + ''''),
             IfThen(parser.RestrictedValues.Count = 0, '',
                 'OR (IMP.TrimmedData<>'''' AND MUV.Measurement_Unit_Key IS NULL)')]);
        errors := dmDatabase.ExecuteSQL(sql, True);
        if Assigned(FOnFastParseError) then
          FOnFastParseError(parser, columnsToValidate[i], errors);
        cvInfo.ErrorCount      := errors.RecordCount;
        cvInfo.RequiresParsing := False;
        columnsToValidate.Delete(i);
      end;
    end;
  end;  // ParseTaxonDataColumns

  {-----------------------------------------------------------------------------
   Validate columns that end up as multiple fields in #Master table or multiple
   records in a #CT table.
  }
  procedure ParseMultiFieldColumns;
  var
    i: Integer;
  begin
    if not (FDataSource.EOF and FDataSource.BOF) then FDataSource.First;

    while not FDataSource.EOF do
    begin
      for i := 0 to columnsToValidate.Count - 1 do
      begin
        cvInfo := TColumnValidationInfo(columnsToValidate.Objects[i]);

        if Assigned(cvInfo.MappedType.Parser) then
          if (cvInfo.MappedType.Parser.SingleRecord and
              not UpdateMasterTableRecord(columnsToValidate[i], cvInfo.MappedType, False))
          or ((FMultiRecordTableTypes.IndexOf(cvInfo.MappedType) > -1) and
              not UpdateMultiRecordTableRecord(columnsToValidate[i], cvInfo.MappedType, False)) then
          begin
            DoParseErrorChanged(cvInfo.MappedType.Parser, columnsToValidate[i], True);
            cvInfo.ErrorCount := cvInfo.ErrorCount + 1;
          end;
          // Fail when Review Date is before Date.
          if cvInfo.MappedType.Key = CT_KEY_REVIEW_DATE then
            if ColumnMapping.ValidateFieldDependency(cvInfo.MappedType, FDataSource) =
              ResStr_ReviewDateBeforeSampleDate then
            begin
              DoParseErrorChanged(cvInfo.MappedType.Parser, columnsToValidate[i], True);
              cvInfo.ErrorCount := cvInfo.ErrorCount + 1;
            end;
          // Fail when Determination Date is before Date.
          if cvInfo.MappedType.Key = CT_KEY_DETERMINATION_DATE then
           if ColumnMapping.ValidateFieldDependency(cvInfo.MappedType, FDataSource) =
               ResStr_DeterminationDateBeforeSampleDate then
           begin
             DoParseErrorChanged(cvInfo.MappedType.Parser, columnsToValidate[i], True);
             cvInfo.ErrorCount := cvInfo.ErrorCount + 1;
           end;
      end;
      FDataSource.Next;
      frmMain.SetProgress(FDataSource.RecNo * 90 div FDataSource.RecordCount + 10);
    end;
  end;  // ParseMultiFieldColumns

begin
  frmMain.SetStatus(ResStr_ParseAndValidate);

  FDatasource.DisableControls;
  try
    // NOTE: Formatting included in strings to facilitate debugging.
    sqlDelete :=
          #10'DELETE    %s'
        + #10'FROM      %s TMP'
        + #10'LEFT JOIN ' + TADOTable(FDataSource).TableName
            + ' IMP ON TMP.Record_No = IMP."' + FLD_ROWID + '"'
        + #10'WHERE     IMP."' + FLD_ROWID + '" IS NULL'#10;

    // Cleanup first.
    sql := Format(sqlDelete, ['#Master', '#Master']);
    for i := 0 to ValidationInfo.Count - 1 do
    begin
      cvInfo := TColumnValidationInfo(ValidationInfo.Objects[i]);
      if Assigned(cvInfo.MappedType) then
      begin
        tableName := '#CT_' + cvInfo.MappedType.Key;
        sql := sql
            + Format(#10'IF Object_Id(''tempdb..%s'') IS NOT NULL', [tableName])
            + Format(sqlDelete, [tableName, tableName]);

        tableName := '#CS_' + cvInfo.MappedType.Key;
        sql := sql
            + Format(#10'IF Object_Id(''tempdb..%s'') IS NOT NULL', [tableName])
            + Format(sqlDelete, [tableName, tableName]);
      end;
    end;

    // Insert all new Record_No, or all if first time round, but only in #Master.
    // The other tables will remove existing records before inserting "clean" ones.
    sql := sql
        + #10'INSERT    #Master (Record_No)'
        + #10'SELECT    ' + FLD_ROWID
        + #10'FROM      ' + TADOTable(FDataSource).TableName
        + #10'LEFT JOIN #Master ON Record_No = ' + FLD_ROWID
        + #10'WHERE     Record_No IS NULL';

    dmDatabase.ExecuteSQL(sql);

    // Use a list to keep track of what columns still need validating.
    // Remove columns once validated, but DON'T FREE THE OBJECT.
    columnsToValidate := TStringList.Create;
    try
      for i := 0 to ValidationInfo.Count - 1 do
        if TColumnValidationInfo(ValidationInfo.Objects[i]).RequiresParsing then
          columnsToValidate.AddObject(ValidationInfo[i], ValidationInfo.Objects[i]);

      if columnsToValidate.Count > 0 then
      begin
        // Run one update statement for all single value fields with no parsers.
        UpdateForSingleFieldsNoParsers;
        frmMain.SetProgress(2);

        // Do single field/single record one column at a time through SQL, if possible.
        ParseSingleFieldColumns;
        frmMain.SetProgress(4);

        // Special treatment for the taxon data columns.
        ParseTaxonDataColumns;
        frmMain.SetProgress(6);

        // Parse as before for all remaining columns. Can't do much better here.
        ParseMultiFieldColumns;

        // Update validation flags of remaining columns.
        for i := 0 to columnsToValidate.Count - 1 do
          TColumnValidationInfo(columnsToValidate.Objects[i]).RequiresParsing := False;
      end;
    finally
      columnsToValidate.Free;
    end;
  finally
    FDatasource.EnableControls;
    frmMain.SetStatus('');
    frmMain.SetProgress(0);
  end;
end; // TLargeImportFile.ParseData;

{-------------------------------------------------------------------------------
  If row removed from import table, may need to also be removed from temp tables.
}
procedure TLargeImportFile.RemoveRow(rowID: Integer);
var
  sql: String;
  i: Integer;
  cvInfo: TColumnValidationInfo;
begin
  sql := 'IF Object_Id(''tempdb..#Master'') IS NOT NULL'
      + #10#9'DELETE FROM #Master WHERE Record_No = ' + IntToStr(rowID);

  for i := 0 to ValidationInfo.Count - 1 do
  begin
    cvInfo := TColumnValidationInfo(ValidationInfo.Objects[i]);
    if Assigned(cvInfo.MappedType) then
      sql := sql + Format(
            #10#10'IF Object_Id(''tempdb..#CT_%s'') IS NOT NULL'
          + #10#9'DELETE FROM #CT_%s WHERE Record_No = %d'
          + #10#10'IF Object_Id(''tempdb..#CS_%s'') IS NOT NULL'
          + #10#9'DELETE FROM #CS_%s WHERE Record_No = %d',
          [cvInfo.MappedType.Key,
           cvInfo.MappedType.Key, rowID,
           cvInfo.MappedType.Key,
           cvInfo.MappedType.Key, rowID]);
  end;
  dmDatabase.ExecuteSQL(sql);
end;  // TLargeImportFile.RemoveRow

{-------------------------------------------------------------------------------
  Update temp tables with content of field from DataSource. Assumes the current
  row in DataSource is the correct one.
}
procedure TLargeImportFile.UpdateRowField(const fieldName: String);
var
  mappedType: TColumnType;
  cvInfo: TColumnValidationInfo;
begin
  mappedType := ColumnMapping.MappedType(fieldName);
  if ValidatedField(fieldName) and Assigned(mappedType) then
  begin
    cvInfo := TColumnValidationInfo(ValidationInfo.Objects[ValidationInfo.IndexOf(fieldName)]);

    // Only proceed if there is somewhere to put the data in.
    // Also, no processing columns requiring parsing (re-mapped ones).
    if Assigned(mappedType)
       and Assigned(cvInfo)
       and (mappedType = cvInfo.MappedType)
       and not cvInfo.RequiresParsing then
    begin
      // Update remaining fields with appropriate values and update the cell error status.
      DoParseErrorChanged(
          mappedType.Parser,
          fieldName,
          not UpdateMasterTableRecord(fieldName, mappedType, True));

      // Update multi-record table, if needed and also update the cell error status.
      if FMultiRecordTableTypes.IndexOf(mappedType) > -1 then
        DoParseErrorChanged(
            mappedType.Parser,
            fieldName,
            not UpdateMultiRecordTableRecord(fieldName, mappedType, True));
    end;
  end;
end;  // TLargeImportFile.UpdateRowField

{ ------------------------------------------------------------------------------
  Update one record in #Master with the relevant data.
  Use "False" for withCheck when validating columns, since they start off empty.
  Use "True" when validation has already taken place once and the #Master table
  is already populated.
}
function TLargeImportFile.UpdateMasterTableRecord(const fieldName: String;
    columnType: TColumnType; withCheck: Boolean): Boolean;
var
  i: Integer;
  columns: TStringList;
  sql, updateStmt, rowId: String;
begin
  Result     := True;
  updateStmt := '';
  sql        := '';
  columns    := TStringList.Create;
  try
    // We can have several columns/values from the one field, hence the lists.
    columns.CommaText := GetMasterColumn(columnType, False);
    if columns.Count > 0 then
    begin
      // Relying on order of columns and values to match here.
      GetMasterValue(fieldName, columnType);
      Result := not Assigned(columnType.Parser) or not columnType.Parser.Failed;
      if Result then
      begin
        updateStmt := columns[0] + ' = ' + LastParsedValues[0];
        for i := 1 to LastParsedValues.Count - 1 do
          updateStmt := updateStmt + ','#10#9 + columns[i] + ' = ' + LastParsedValues[i];

        rowId := FDatasource.FieldByName(FLD_ROWID).AsString;

        // Reinsert row in #Master first, if it's missing.
        if withCheck then
          sql := 'IF NOT EXISTS (SELECT 1 FROM #Master WHERE Record_No = ' + rowId + ')'
              + #10#9'INSERT INTO #Master (Record_No) VALUES (' + rowId + ')'
              + #10;

        // Now update the record.
        sql := sql + Format(
              #10'UPDATE  #Master '
            + #10'SET     %s'
            + #10'FROM    %s I'
            + #10'JOIN    #Master   M ON M.Record_No = I.%s'
            + #10'WHERE   I.%s = %s',
            [updateStmt,
             TADOTable(FDataSource).TableName,
             FLD_ROWID,
             FLD_ROWID, rowId]);
        dmDatabase.ExecuteSQL(sql);
      end;
    end;
  finally
    columns.Free;
  end;
end;  // TLargeImportFile.UpdateMasterTableRecord

{ ------------------------------------------------------------------------------
  Update multi-record temp tables with the relevant data.
  Use "False" for withCheck when validating columns, since the #CT tables start off empty.
  Use "True" when validation has already taken place once and the #CT tables
  are already populated. In this case, it is safer to remove what was there initially.
}
function TLargeImportFile.UpdateMultiRecordTableRecord(const fieldName: String;
    columnType: TColumnType; withCheck: Boolean): Boolean;
var
  value: String;
begin
  value := LargeFields.Values[fieldName + ',' + FDatasource.FieldByName(FLD_ROWID).AsString];
  if value = '' then
    value := FDataSource.FieldByName(fieldName).AsString;

  // Have to clear up what's there first, or may end up with more records than expected.
  if withCheck then
    dmDatabase.ExecuteSQL(
        'DELETE FROM #CT_' + columnType.Key
        + #10'WHERE Record_No = ' + FDatasource.FieldByName(FLD_ROWID).AsString
        + #10'AND IW_Qualification = ''' + columnType.Qualification + '''');
        
  // Now add the new fresh stuff.
  columnType.AddParsedRecords(
      columnType.Parser,
      FDatasource.FieldByName(FLD_ROWID).AsInteger,
      value);

  Result := not columnType.Parser.Failed;
end;  // TLargeImportFile.UpdateMultiRecordTableRecord

{ ------------------------------------------------------------------------------
  Returns True if the column corresponding to the given fieldName has passed
  validation and if the column mapping hasn't changed.
}
function TLargeImportFile.ValidatedField(const fieldName: String): Boolean;
var
  idx: Integer;
  mappedType: TColumnType;
  cvInfo: TColumnValidationInfo;
begin
  Result     := False;
  idx        := ValidationInfo.IndexOf(fieldName);
  mappedType := ColumnMapping.MappedType(fieldName);

  // Validate value only if column already mapped and validated.
  // Otherwise, it's either newly mapped or unmapped and these don't matter.
  if (idx > -1) and Assigned(mappedType) then
  begin
    cvInfo := TColumnValidationInfo(ValidationInfo.Objects[idx]);

    // Validate if column validated fine first time and mapping hasn't changed.
    // Changed mapping will require full column validation.
    if (cvInfo.MappedType = mappedType) and not cvInfo.RequiresParsing then
      Result := ParseField(fieldName);
  end;
end;  // TLargeImportFile.ValidatedField

{===============================================================================
 TLargeFileTableRule
===============================================================================}
{ ------------------------------------------------------------------------------
  Instantiates the table rule object with the specified key for the given
  import file.
}
constructor TLargeFileTableRule.Create(const Key: String;
  ImportFile: TLargeImportFile);
begin
  inherited Create(Key, ImportFile);
  FGeneratedFields := TStringList.Create;
end;

{ ------------------------------------------------------------------------------
  Disposes of the table rule object.
}
destructor TLargeFileTableRule.Destroy;
begin
  FGeneratedFields.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Applies this table rule, creating and populating the corresponding temporary
  table.
}
procedure TLargeFileTableRule.Apply;
begin
  CreateTables;
  BuildGeneratingQuery;

  dmDatabase.ExecuteSQL(
      IdentifyOutputRecordsSQL + #10#10
      + ReserveKeyValuesSQL + #10#10
      + GenerateOutputSQL + #10#10
      + MapOutputSQL + #10#10
      + CopyOutputSQL);
end;

{ ------------------------------------------------------------------------------
  Finds the named output field, if it is present in this table rule.
}
function TLargeFileTableRule.OutputFieldByName(Name: String): TOutputField;
var
  I: Integer;
begin
  for I := 0 to OutputFieldCount - 1 do
  begin
    if SameText(OutputFields[I].Name, Name) then
    begin
      Result := OutputFields[I];
      Exit;
    end;
  end;
  Result := Nil;
end;

{ ------------------------------------------------------------------------------
}
procedure TLargeFileTableRule.BuildGeneratingQuery;
begin
  Initialize();
  AddRelatedTables();
  AddGeneratingFields();
  AddChecksumColumn();
end;

{ ------------------------------------------------------------------------------
  SQL statements that define and populate a table variable (@generated)
  containing one record for each unique combination of the generated fields.
}
function TLargeFileTableRule.IdentifyOutputRecordsSQL: String;
var
  I: Integer;
  lColumnDefinitions: String;
  lFields: String;
  lSelectList: String;

  procedure IncludeRecordIdentity(const SiteIdColumn, RecordIdColumn: String);
  begin
    lColumnDefinitions := lColumnDefinitions + ','#10#9
        + MakeColumnDefinition('Site_ID', 'CHAR(8)') + ','#10#9
        + MakeColumnDefinition('Record_ID', 'CHAR(8)');

    lFields := AddField(AddField(lFields, 'Site_ID'), 'Record_ID');
        
    if FGroupList = '' then
      lSelectList := AddField(AddField(
          lSelectList,
          '#master.' + SiteIdColumn + '_' + FLD_SITEID),
          '#master.' + RecordIdColumn + '_' + FLD_RECORDID)
    else
      lSelectList := AddField(AddField(
          lSelectList,
          'MIN(#master.' + SiteIdColumn + '_' + FLD_SITEID + ')'),
          'MIN(#master.' + RecordIdColumn + '_' + FLD_RECORDID + ')');
  end;
  
begin
  lColumnDefinitions := FColumnDefinitions;
  lSelectList := FSelectList;
  lFields := 'Record_No';
  for I := 0 to FGeneratedFields.Count - 1 do
  begin
    lFields := AddField(lFields, FGeneratedFields[I]);
  end;

  if ImportFile.ColumnMapping.KeyIsMapped(CT_KEY_MAPMATE_KEY) then
    IncludeRecordIdentity(CT_KEY_MAPMATE_KEY, CT_KEY_MAPMATE_KEY)
  else if ImportFile.ColumnMapping.KeyIsMapped(CT_KEY_RECORD_ID) then
    IncludeRecordIdentity(CT_KEY_SITE_ID, CT_KEY_RECORD_ID);

  Result := 'DECLARE @generated TABLE ('#10#9
      + 'Generated_Id INT IDENTITY PRIMARY KEY,'#10#9      
      + lColumnDefinitions + ')'#10#10

      + 'INSERT INTO @generated ('#10#9
      + lFields + ')'#10;

  if FGroupList = '' then
    Result := Result
        + 'SELECT DISTINCT ' + AddField('#master.Record_No', lSelectList)
  else
    Result := Result
        + 'SELECT ' + AddField('MIN(#master.Record_No)', lSelectList);

  Result := Result + #10'FROM ' + FTableSource;

  if FilterExpression <> '' then
    Result := Result + #10'WHERE ' + FilterExpression;

  if FGroupList <> '' then
    Result := Result + #10'GROUP BY ' + FGroupList;
end;

{-------------------------------------------------------------------------------
  If the table rule is associated with a key generator, returns SQL statements
  that reserve a key value in the destination table for each of the output
  records.

  Once these statements have been executed, the variable @base_key contains the
  key value immediately before the reserved range.
}
function TLargeFileTableRule.ReserveKeyValuesSQL: String;
var
  I: Integer;
begin
  if ImportFile.ColumnMapping.KeyIsMapped(CT_KEY_MAPMATE_KEY)
    or ImportFile.ColumnMapping.KeyIsMapped(CT_KEY_RECORD_ID) then
  begin
    // We do not need to reserve keys; they are generated according to the
    // scheme described in TSD section 1.18.2.11 (Advanced Column Types:
    // Record ID).
    Exit;
  end;

  // TODO: this does not reserve the enough key values if there are fields
  //       in the source data that parse to multiple values; in these cases
  //       there may currently be more than one output record for a single
  //       @generated record
  for I := 0 to OutputFieldCount - 1 do
  begin
    if OutputFields[I].Generator is TKeyFieldGenerator then
    begin
      Result := 'DECLARE @base_key CHAR(8), @number_of_keys INT' + #10
          + 'SELECT @number_of_keys = COUNT(*) FROM @generated' + #10
          + 'EXECUTE dbo.usp_ReserveKeys' + #10#9
                + '''' + TableName + ''',' + #10#9
                + '@number_of_keys,' + #10#9
                + '@base_key OUTPUT';
      Exit;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  SQL statements that define and populate a table variable (@output) containing
  the output of the table rule.  These are the data that will be copied to the
  table that is identified by the TableName property.

  The SQL returned by this method is dependent on the @generated table variable,
  which must have been defined and populated before these statements
  can be executed.
}
function TLargeFileTableRule.GenerateOutputSQL: String;
var
  I: Integer;
  Columns: String;
  Fields: String;
  Values: String;
  Tables: TTableSource;
  SearchCondition: String;

  // handle special cases where a record should not be generated
  function SpecialCaseFilter: String;
  var
    Key1: TOutputField;
    Key2: TOutputField;
  begin
    Result := '';
    // Ensure that taxon occurrences that were created for associated species
    // are not linked back to themselves.
    if SameText(TableName, 'taxon_occurrence_relation') then
    begin
      Key1 := OutputFieldByName('taxon_occurrence_key_1');
      Key2 := OutputFieldByName('taxon_occurrence_key_2');
      Assert(
          Assigned(Key1) and Assigned(Key2),
          'Taxon_Occurrence_Relation field list changed');
      Result := Format(
          '(%s <> %s)',
          [Key1.GeneratingSQL(Tables), Key2.GeneratingSQL(Tables)]);
    end;
  end;

begin
  Tables := TTableSource.Create;
  try
    Columns := 'Generated_Id INT NOT NULL';
    Fields := 'Generated_Id';
    Values := Tables.QualifiedField('@generated', 'Generated_Id');

    for I := 0 to OutputFieldCount - 1 do
    begin
      Columns := Columns + ','#10#9
          + MakeColumnDefinition(
              OutputFields[I].Name,
              OutputFields[I].DataType);
      Fields := Fields + ','#10#9 + OutputFields[I].Name;
      Values := Values + ','#10#9 + OutputFields[I].GeneratingSQL(Tables);
    end;
    
    Result := 'DECLARE @output TABLE ('#10#9
        + Columns + ','#10#9
        + 'PRIMARY KEY (' + PrimaryKey.Key1;

    if PrimaryKey.Key2 <> '' then
      Result := Result + ', ' + PrimaryKey.Key2;

    Result := Result + '))'#10#10
        + 'INSERT INTO @output ('#10#9
        + Fields + ')' + #10
        + 'SELECT ' + Values + #10
        + 'FROM ' + Tables.TableSourceSQL;

    SearchCondition := SpecialCaseFilter;
    if SearchCondition <> '' then
      Result := Result + #10
          + 'WHERE ' + SearchCondition;
  finally
    Tables.Free;
  end;
end;

{ ------------------------------------------------------------------------------
  SQL statements that add records to #RN_<TableName>, mapping records in the
  table rule's output to record numbers in the source data.

  The SQL returned by this method is dependent on the @output and @generated
  table variables (and possibly the #generated_mapping temporary table) which
  must have been defined and populated before these statements can be executed.
}
function TLargeFileTableRule.MapOutputSQL: String;
var
  I: Integer;
begin
  if UseGeneratedMappingTable then
  begin
    Result := GeneratedMappingTableSQL + #10#10;
  end;

  Result := Result
      + 'INSERT INTO #RN_' + TableName + '('#10#9
      + 'Record_No,'#10#9
      + PrimaryKey.Key1;

  if PrimaryKey.Key2 <> '' then
    Result := Result + ','#10#9 + PrimaryKey.Key2;

  Result := Result + ')'#10
      + 'SELECT DISTINCT ';

  if UseGeneratedMappingTable then
    Result := Result + 'm.Record_No'
  else
    Result := Result + 'g.Record_No';

  Result := Result + ','#10#9't.' + PrimaryKey.Key1;

  if PrimaryKey.Key2 <> '' then
    Result := Result + ','#10#9't.' + PrimaryKey.Key2;

  Result := Result + #10
      + 'FROM @output AS t'#10
      + 'INNER JOIN @generated AS g'#10
      + 'ON g.Generated_Id = t.Generated_Id';

  if UseGeneratedMappingTable then
  begin
    Result := Result + #10
      + 'INNER JOIN #generated_mapping AS m'#10
      + 'ON m.CS_Columns = g.CS_Columns';

    for I := 0 to FGeneratedFields.Count - 1 do
    begin
      Result := Result
          + Format(
              #10'AND (m.%0:s = g.%0:s OR (m.%0:s IS NULL AND g.%0:s IS NULL))',
              [FGeneratedFields[I]]);
    end;

    Result := Result + #10#10
        + 'DROP TABLE #generated_mapping';
  end;
end;

{ ------------------------------------------------------------------------------
  SQL statements that define and populate a temporary table (#generated_mapping)
  that maps the records of @generated to record numbers in the source data.
}
function TLargeFileTableRule.GeneratedMappingTableSQL: String;
var
  I: Integer;
begin
  Result := 'IF OBJECT_ID(''tempdb..#generated_mapping'') IS NOT NULL'#10#9
      + 'DROP TABLE #generated_mapping'#10#10
        
      + 'CREATE TABLE #generated_mapping ('#10#9
      + FColumnDefinitions + ')'#10#10

      + 'CREATE INDEX IX_Generated_Mapping'
      + ' ON #generated_mapping (CS_Columns)'#10#10

      + 'INSERT INTO #generated_mapping ('#10#9
      + 'Record_No';
  
  for I := 0 to FGeneratedFields.Count - 1 do
  begin
    Result := AddField(Result, FGeneratedFields[I]);
  end;
      
  Result := Result + ')'#10
      + 'SELECT #master.Record_No,'#10#9
      + FSelectList + #10
      + 'FROM ' + FTableSource;

  if FilterExpression <> '' then
  begin
    Result := Result + #10
        + 'WHERE ' + FilterExpression;
  end;
end;

{ ------------------------------------------------------------------------------
  An SQL statement that copies the output of the table rule from the @output
  table variable into the destination table, #<TableName>.
}
function TLargeFileTableRule.CopyOutputSQL: String;
var
  I: Integer;
  Fields: String;
begin
  Fields := OutputFields[0].Name;
  for I := 1 to OutputFieldCount - 1 do
  begin
    Fields := Fields + ','#10#9 + OutputFields[I].Name;
  end;

  Result := 'INSERT INTO #' + TableName + ' ('#10#9
      + Fields + ')'#10
      + 'SELECT ' + Fields + #10
      + 'FROM @output';
end;

{ ------------------------------------------------------------------------------
  Resets the instance member variables that are used to generate the SQL
  statements for the table rule batch.
}
procedure TLargeFileTableRule.Initialize();
begin
  FGeneratedFields.Clear;
  FColumnDefinitions := 'Record_No INT NOT NULL';
  FSelectList := '';
  FTableSource := '#master';
  FGroupList := '';
  FAliasCount := 0;
  FLastJoinTable := 'master';
end;

{ ------------------------------------------------------------------------------
  For each related table of this table rule, add the necessary terms to the
  SQL instance variables: FGeneratedFields, FColumnDefinitions, FSelectList,
  FTableSource and FGroupList.
}
procedure TLargeFileTableRule.AddRelatedTables;
var
  I: Integer;
  Table: String;
begin
  for I := 0 to FRelatedTables.Count - 1 do
  begin
    Table := FRelatedTables.Names[I];
    if LeftStr(Table, 3) = 'CT_' then
      AddColumnTypeTable(Table)
    else
      AddRelatedTable(Table);
  end;
end;

{ ------------------------------------------------------------------------------
  Adds the necessary terms to the SQL instance variables for a related
  column type ('#CT_...') table.  Note that nothing is added if the column
  type is not mapped in the import file.
}
procedure TLargeFileTableRule.AddColumnTypeTable(const Table: String);
var
  ColumnTypeKey: String;
  IsGenerating: Boolean;
begin
  ColumnTypeKey := MidStr(Table, 4, 16);
  if ImportFile.ColumnMapping.KeyIsMapped(ColumnTypeKey) then
  begin
    FTableSource := FTableSource + #10
        + 'INNER JOIN #' + Table + #10
        + 'ON #' + Table + '.Record_No = '
        + '#' + FLastJoinTable + '.Record_No';          
    FLastJoinTable := Table;
    IsGenerating := (FRelatedTables.Values[Table] = '1');
    AddParsedColumn(
        Table,
        ImportFile.ColumnMapping.ColumnTypeByKey(ColumnTypeKey),
        IsGenerating);      
  end;
end;

{ ------------------------------------------------------------------------------
  Adds the necessary terms to the SQL instance variables for a related table
  that is joined via its corresponding '#RN_...' table.  These tables will
  have been populated by earlier table rules.
}
procedure TLargeFileTableRule.AddRelatedTable(const Table: String);
var
  Key: TPrimaryKey;
  IsGenerating: Boolean;
begin
  Key := dmDatabase.SplitPrimaryKey(Table);
  IsGenerating := (FRelatedTables.Values[Table] = '1');
    
  AddColumnDefinition(Key.Key1, 'CHAR(16)');
  FSelectList := AddField(FSelectList, '#RN_' + Table + '.' + Key.Key1);
  if IsGenerating then
    FGroupList := AddField(FGroupList, '#RN_' + Table + '.' + Key.Key1);
      
  FTableSource := FTableSource + #10
      + 'INNER JOIN #RN_' + Table + #10
      + 'ON #RN_' + Table + '.Record_No = #'
      + FLastJoinTable + '.Record_No'#10
      + 'INNER JOIN #' + Table + #10
      + 'ON #' + Table + '.' + Key.Key1
      + ' = #RN_' + Table + '.' + Key.Key1;            
          
  if Key.Key2 <> '' then
  begin
    AddColumnDefinition(Key.Key2, 'CHAR(16)');
    FSelectList := AddField(FSelectList, '#RN_' + Table + '.' + Key.Key2);
    if IsGenerating then
      FGroupList := AddField(FGroupList, '#RN_' + Table + '.' + Key.Key2);
      
    FTableSource := FTableSource + #10
        + ' AND #' + Table + '.' + Key.Key2
        + ' = #RN_' + Table + '.' + Key.Key2;
  end;

  FLastJoinTable := 'RN_' + Table;
end;

{ ------------------------------------------------------------------------------
  Adds the necessary terms to the SQL instance variables for the generating
  fields of this table rule.  Terms are only added for fields that have been
  mapped in the import file.
}
procedure TLargeFileTableRule.AddGeneratingFields;
var
  I: Integer;
  Column: TColumnType;
begin
  for I := 0 to FGeneratingFields.Count - 1 do
  begin    
    if ImportFile.ColumnMapping.KeyIsMapped(GeneratingFields[I]) then
    begin
      Column := ImportFile.ColumnMapping.ColumnTypeByKey(GeneratingFields[I]);
      if not Assigned(Column.Parser) then
        AddSimpleColumn(Column)
      else if Column.Parser.SingleRecord then
        AddParsedColumn('master', Column, True)
      else
        AddMultipleRecordColumn(Column);
    end;
  end;
end;

{ ------------------------------------------------------------------------------
  Adds the necessary terms to the SQL instance variables for a mapped column
  type that has no parser.
}
procedure TLargeFileTableRule.AddSimpleColumn(Column: TColumnType);
begin
  FSelectList := AddField(FSelectList, '#master.' + Column.Key);
  FGroupList := AddField(FGroupList, '#master.' + Column.Key);
  AddColumnDefinition(Column.Key, Column.FieldType);
end;

{ ------------------------------------------------------------------------------
  Adds the necessary terms to the SQL instance variables for a mapped column
  type has a parser that produces a single record for each source value.
}
procedure TLargeFileTableRule.AddParsedColumn(TableName: String;
    Column: TColumnType; AddToGroupList: Boolean);
var
  I: Integer;
  MatchRule: TMatchRule;
begin
  for I := 0 to Column.Parser.FieldCount - 1 do
  begin
    MatchRule := ImportFile.MatchRuleByKey(Column.FieldMatchRuleKeys[I]);
    if not Assigned(MatchRule) then
      AddUnmatchedField(TableName, Column, I, AddToGroupList)
    else
      AddMatchedField(TableName, Column, I, MatchRule, AddToGroupList);
  end;
end;  

{ ------------------------------------------------------------------------------
  Adds the necessary terms to the SQL instance variables for a mapped column
  type has a parser that may produce multiple records for a single source value.
}
procedure TLargeFileTableRule.AddMultipleRecordColumn(Column: TColumnType);
begin
  Screen.Cursor := crHourglass;
  try
    EnsureGroups(Column.Key);
  finally
    Screen.Cursor := crDefault;
  end;

  FSelectList := AddField(FSelectList, CurrentAlias + '.Group_ID');

  FTableSource := FTableSource + #10
      + 'LEFT JOIN #IW_Group AS ' + CurrentAlias + #10
      + 'ON (' + CurrentAlias + '.Column_Type_Key = ''' + Column.Key + ''''
      + ' AND ' + CurrentAlias + '.Record_No = #'
      + FLastJoinTable + '.Record_No)';

  AddColumnDefinition(Column.Key, 'INT');

  FGroupList := AddField(FGroupList, CurrentAlias + '.Group_ID');
  FLastJoinTable := CurrentAlias;

  Inc(FAliasCount);
end;

{ ------------------------------------------------------------------------------
  Returns a value indicating whether the #generated_mapping table and
  CS_Columns checksum are required for this table rule.
}
function TLargeFileTableRule.UseGeneratedMappingTable: Boolean;
begin
  Result := (FSelectList <> '') and (FGroupList <> '');
end;

{ ------------------------------------------------------------------------------
  Adds the necessary terms to the SQL instance variables for the CS_Columns
  checksum column.
}
procedure TLargeFileTableRule.AddChecksumColumn;
begin
  if UseGeneratedMappingTable then
  begin
    AddColumnDefinition('CS_Columns', 'INT');
    if FGroupList <> '' then
      FGroupList := AddField(FGroupList, 'CHECKSUM(' + FSelectList + ')');
    FSelectList := AddField(FSelectList, 'CHECKSUM(' + FSelectList + ')');
  end;
end;

{ ------------------------------------------------------------------------------
  Adds the necessary terms to the SQL instance variables for an unmatched field.
}
procedure TLargeFileTableRule.AddUnmatchedField(
  const TableName: String;
  Column: TColumnType;
  FieldIndex: Integer;
  AddToGroupList: Boolean);
var
  Field: TParsedField;
  GeneratedName: String;
  QualifiedName: String;
begin
  Field := Column.Parser.Fields[FieldIndex];
  GeneratedName := Column.Key + '_' + Field.Name;
  if TableName = 'master' then
    QualifiedName := Format('#%s.%s', [TableName, GeneratedName])
  else
    QualifiedName := Format('#%s.%s', [TableName, Field.Name]);

  AddColumnDefinition(GeneratedName, Field.DataType);
  FSelectList := AddField(FSelectList, QualifiedName);
  if AddToGroupList then
    FGroupList := AddField(FGroupList, QualifiedName);
end;

{ ------------------------------------------------------------------------------
  Adds the necessary terms to the SQL instance variables for a matched field.
}
procedure TLargeFileTableRule.AddMatchedField(
  const TableName: String;
  Column: TColumnType;
  FieldIndex: Integer;
  Match: TMatchRule;
  AddToGroupList: Boolean);
var
  Field: TParsedField;
  GeneratedName: String;
  QualifiedName: String;
  MatchKey: String;
begin
  Field := Column.Parser.Fields[FieldIndex];
  GeneratedName := Column.Key + '_' + Field.Name;
  if TableName = 'master' then
    QualifiedName := Format('#%s.%s', [TableName, GeneratedName])
  else
    QualifiedName := Format('#%s.%s', [TableName, Field.Name]);
    
  if Column.Key <> CT_KEY_LOCATION then
  begin
    AddColumnDefinition(GeneratedName, 'CHAR(16)');
    MatchKey := CurrentAlias + '.Match_Key';
  end
  else begin
    // force location name as a generating field
    AddColumnDefinition(GeneratedName, 'VARCHAR(100)');
    MatchKey := 'ISNULL('
        + 'CAST(' + CurrentAlias + '.Match_Key AS VARCHAR(100)), '
        + CurrentAlias + '.Import_Value)';
  end;

  FSelectList := AddField(FSelectList, MatchKey);
  if AddToGroupList then
    FGroupList := AddField(FGroupList, MatchKey);

  FTableSource := FTableSource + #10
      + 'LEFT JOIN #' + Match.Name + ' AS ' + CurrentAlias + #10
      + 'ON ' + CurrentAlias + '.Import_Value = ' + QualifiedName;
            
  Inc(FAliasCount);
end;

{ ------------------------------------------------------------------------------
  The alias for the last joined table in FTableSource.
}
function TLargeFileTableRule.CurrentAlias: String;
begin
  Result := Format('t%d', [FAliasCount]);
end;

{ ------------------------------------------------------------------------------
  Adds a column definition to FColumnDefinitions.
}
procedure TLargeFileTableRule.AddColumnDefinition(
    const Name, DataType: String);
begin
  FColumnDefinitions :=
      AddField(FColumnDefinitions, MakeColumnDefinition(Name, DataType));
  FGeneratedFields.Add(Name);
end;

{-------------------------------------------------------------------------------
  Ensures that #IW_Group contains the data for the specified column type.
}
procedure TLargeFileTableRule.EnsureGroups(const ColumnTypeKey: String);
var
  lSql: String;
begin
  if not dmDatabase.GetStoredProcOutputParam(
      'usp_ImportWizardGroup_ColumnProcessed',
      ['@column_type_key', ColumnTypeKey],
       '@column_processed') then
  begin
    //Use distinct checksum values for columntypes to work out groupings
    lSql := 'DECLARE @Groupings TABLE ( GroupID INT IDENTITY(1,1) PRIMARY KEY,'
         + ' Checksum INT ) '
         + ' INSERT INTO @Groupings (Checksum) SELECT DISTINCT Checksum FROM'
         + ' #CS_' + ColumnTypeKey
         + ' INSERT INTO #IW_GROUP ('+
                'Column_Type_Key, '+
                'Qualification, '+
                'Record_No, '+
                'Group_Hash, '+
                'Group_ID) '+
           ' SELECT ''' + ColumnTypeKey + ''', '+
                ''''', '+
                'Record_No, '+
                'g.Checksum,'+
                'g.GroupID'+
                ' FROM #CS_' + ColumnTypeKey + ' cs' +
                ' INNER JOIN @Groupings g ON cs.Checksum = g.Checksum';
      dmDatabase.ExecuteSQL(lSql);
  end;
end;

{===============================================================================
 TColumnValidationInfo
===============================================================================}
{ ------------------------------------------------------------------------------
}
procedure TColumnValidationInfo.SetMappedType(const Value: TColumnType);
begin
  if Value <> FMappedType then begin
    FMappedType      := Value;
    FErrorCount      := 0;
    FRequiresParsing := True;
  end;
end;  // TColumnValidationInfo.SetMappedType

end.
