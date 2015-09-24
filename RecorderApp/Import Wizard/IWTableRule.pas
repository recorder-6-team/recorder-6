{===============================================================================
  Unit:         IWTableRule.pas

  Implements:   TOutputField
                TTableRule

  Description:  Rule applied to generate data for records in a single table
                that will be included in the final output.

  Model:        ImportWizardBackend.mpb

  Created:      March 2013

  Last Revision Details:
    $Revision: 58 $
    $Date: 21/03/13 15:56 $
    $Author: Michaelcaptain $

  Copyright © Environment and Heritage Service, 2004

===============================================================================}
unit IWTableRule;

interface

uses
  Classes, Contnrs, ADODB, ADOInt, ExceptionForm, DatabaseAccessADO, IWParsers,
  IWColumnMappingClasses, IWUserSuppliedData, IWConstants, ApplicationSettings;

resourcestring
  ResStr_UnknownOutputFieldGenerator = 'Unknown output field generator ''%s''';
  ResStr_BadOutputField = 'Output field ''%s'' is not fully specified';
  ResStr_InvalidFieldGeneratorIndex =
      'Invalid field index ''%d'' for generator ''%s''';
  ResStr_NoSuchParsedField = 'Column type ''%s'' does not support field ''%s''';
  ResStr_GeneratorFailed = 'Failure in output field generator ''%s'' (%s).';
  ResStr_FailedToInsertOutputRecord =
      'The import process failed to insert an output record in table rule'
      + ' ''%s''. The import job was aborted.'#10#10
      + 'The error that occurred was:'#10#10'%s'#10#10
      + 'This is probably due to an error in the definition of the import'
      + ' process.';
  ResStr_DuplicateRecord = 'Cannot enter a duplicate record into table %s';

type
  {--------------------------------------------------------------------------
  ---
    Error during generation of output tables.
  }
  ETableRuleException = class(TExceptionPath)
  end;

  ENoRecordInserted = class(TExceptionPath);
  
  TTableRule = class;
  TOutputField = class;
  TTableSource = class;

  {--------------------------------------------------------------------------
  ---
    Base class for output field generators.
  }
  TOutputFieldGenerator = class(TObject)
  protected
    FOutputField: TOutputField;
    function ColumnMapping: TColumnMapping; virtual;
    function FieldValue(Fields: Fields; const ColumnTypeKey: String; const
        FieldName: String): OLEVariant;
    function FieldValueSQL(Tables: TTableSource;
        const ColumnTypeKey, FieldName: String): String;
    procedure InvalidFieldIndex(FieldIndex: Integer); virtual;
    function TableRule: TTableRule; virtual;
    function UserSuppliedData: TUserSuppliedData; virtual;
    property OutputField: TOutputField read FOutputField;
  public
    constructor Create(Owner: TOutputField); virtual;
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; virtual;
        abstract;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        virtual; abstract;
  end;

  TOutputFieldGeneratorClass = class of TOutputFieldGenerator;

  {--------------------------------------------------------------------------
  ---
    A field in the output of a table rule.
  }
  TOutputField = class(TObject)
  protected
    FColumnType: TColumnType;
    FColumnTypeKey: String;
    FDataType: String;
    FGeneratingClassName: String;
    FGenerator: TOutputFieldGenerator;
    FGeneratorFieldIndex: Integer;
    FLiteralValue: String;
    FName: String;
    FSourceFieldName: String;
    FTableRule: TTableRule;
    constructor Create(Owner: TTableRule; Fields: Fields);
    function ColumnMapping: TColumnMapping; virtual;
    function FieldValue(Fields: Fields; ColumnType: TColumnType; const
        FieldName: String): OLEVariant;
    function FieldValueSQL(Tables: TTableSource; ColumnType: TColumnType;
        const FieldName: String): String; virtual;
    function GetColumnType: TColumnType; virtual;
    function GetGenerator: TOutputFieldGenerator; virtual;
    function UserSuppliedData: TUserSuppliedData; virtual;
    property ColumnType: TColumnType read GetColumnType;
    property ColumnTypeKey: String read FColumnTypeKey;
    property GeneratingClassName: String read FGeneratingClassName;
    property GeneratorFieldIndex: Integer read FGeneratorFieldIndex;
    property LiteralValue: String read FLiteralValue;
    property SourceFieldName: String read FSourceFieldName;
  public
    function Value(Fields: Fields): String; virtual;
    function GeneratingSQL(Tables: TTableSource): String; virtual;
    property DataType: String read FDataType;
    property Generator: TOutputFieldGenerator read GetGenerator;
    property Name: String read FName;
    property TableRule: TTableRule read FTableRule;
  end;
  
  {--------------------------------------------------------------------------
  ---
    Rule applied to generate data for records in 
    a single table that will be included in the 
    final output.
  }
  TTableRule = class(TObject)
  protected
    FFilterExpression: String;
    FRequiredFields: TStringList;
    FGeneratingFields: TStringList;
    FGeneratingQuery: String;
    FGeneratedRecordsQuery: String;
    FGeneratingTableExpression: String;
    FGeneratingTables: TStringList;
    FImportFile: TImportFile;
    FKey: String;
    FOnProgress: TProgressEvent;
    FOutputFields: TObjectList;
    FPrimaryKey: TPrimaryKey;
    FRelatedTables: TStringList;
    FSequence: Integer;
    FTableName: String;
    FIteration: Integer;
    procedure BuildGeneratingQuery; virtual;
    function MakeColumnDefinition(const FieldName, DataType: String): String;
    procedure CreateTables; virtual;
    procedure CreatePrimaryTable; virtual;
    procedure CreateRecordNumberTable; virtual;
    procedure CreateGroupTable; virtual;
    procedure EnsureGroups(const ColumnTypeKey: String); virtual;
    function ExistingGroupID(ColumnType: TColumnType; RecordNo: Integer;
             out Hash: string):
        Integer; virtual;
    function GetGeneratingFieldCount: Integer; virtual;
    function GetGeneratingFields(Index: Integer): String; virtual;
    function GetGeneratingTableCount: Integer; virtual;
    function GetGeneratingTables(Index: Integer): String; virtual;
    function GetOutputFieldCount: Integer; virtual;
    function GetOutputFields(Index: Integer): TOutputField; virtual;
    function GetRequiredFieldCount: Integer; virtual;
    function GetRequiredFields(Index: Integer): String; virtual;
    function GroupChecksum(ColumnType: TColumnType; RecordNo: Integer): Integer;
        virtual;
    function TryInsertOutputRecord(Fields: Fields): string; virtual;
    procedure Load; virtual;
    procedure LoadGeneratingFields; virtual;
    procedure LoadOutputFields; virtual;
    procedure LoadRelatedTables; virtual;
    procedure LoadRequiredFields; virtual;
    procedure UpdateProgress(Progress: Integer); virtual;
    property PrimaryKey: TPrimaryKey read FPrimaryKey;
  public
    constructor Create(Key: String; ImportFile: TImportFile);
    destructor Destroy; override;
    procedure Apply; virtual;
    function IsRelatedTable(const TableName: String): Boolean; virtual;
    function IsGeneratingField(const ColumnTypeKey: String): Boolean; virtual;
    property FilterExpression: String read FFilterExpression;
    property GeneratingFieldCount: Integer read GetGeneratingFieldCount;
    property GeneratingFields[Index: Integer]: String read
        GetGeneratingFields;
    property GeneratingTableCount: Integer read GetGeneratingTableCount;
    property GeneratingTables[Index: Integer]: String read GetGeneratingTables;
    property ImportFile: TImportFile read FImportFile;
    property Key: String read FKey;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OutputFieldCount: Integer read GetOutputFieldCount;
    property OutputFields[Index: Integer]: TOutputField read GetOutputFields;
    property RequiredFieldCount: Integer read GetRequiredFieldCount;
    property RequiredFields[Index: Integer]: String read GetRequiredFields;
    property Sequence: Integer read FSequence;
    property TableName: String read FTableName;
  end;

  { ----------------------------------------------------------------------------
    Accumulator of table names to produce an SQL table source expression.
  }
  TTableSource = class(TObject)
  protected
    FAliases: TStringList;
    function IsMatchName(const Name: String): Boolean; virtual;
    function MatchName(const MatchTable, DataTable, FieldName: String): String;
         virtual;
    procedure DecodeMatchName(const MatchName: String;
        out MatchTable, DataTable, FieldName: String); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure EnsureTable(const TableName: String); virtual;
    function QualifiedField(const TableName, FieldName: String): String;
        virtual;
    function MatchKey(const MatchTable, DataTable, FieldName: String): String;
         virtual;
    function TableSourceSQL: String; virtual;
  end;

procedure RegisterOutputFieldGenerator(AClass: TOutputFieldGeneratorClass);
procedure RegisterOutputFieldGenerators(Classes: array of
    TOutputFieldGeneratorClass);

implementation

uses
  ComObj, StrUtils, SysUtils, Variants, RTLConsts, Sql, Forms,
  Controls;

const
  SQL_SERECORDERS = 'SELECT %s FROM #%s WHERE Name_Key = ''%s''';

var
  mGenerators: TThreadList;
  // allow output fields to share the same SQL call
  mRecordsetNames: TStringList;
  mRecordsets: TInterfaceList;

{+------------------------------------------------------------------------------
  Register an output field generator class.
}
procedure RegisterOutputFieldGenerator(AClass: TOutputFieldGeneratorClass);
begin
  mGenerators.Add(AClass);
end;

{+------------------------------------------------------------------------------
}
procedure RegisterOutputFieldGenerators(Classes: array of
    TOutputFieldGeneratorClass);
var
  I: Integer;
begin
  for I := Low(Classes) to High(Classes) do
    RegisterOutputFieldGenerator(Classes[I]);
end;

{+------------------------------------------------------------------------------
  Registered output field generator class with the given name.
}
function GeneratorClass(const AClassName: String): TOutputFieldGeneratorClass;
var
  I: Integer;

begin
  Result := nil;

  with mGenerators.LockList do
    for I := 0 to Count - 1 do
      if TClass(Items[I]).ClassNameIs(AClassName) then
      begin
        Result := TOutputFieldGeneratorClass(Items[I]);
        Break;
      end;
end;


{-==============================================================================
    TOutputFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TOutputFieldGenerator.Create(Owner: TOutputField);
begin
  FOutputField := Owner;
end;  // TOutputFieldGenerator.Create


{-------------------------------------------------------------------------------
}
function TOutputFieldGenerator.ColumnMapping: TColumnMapping;
begin
  Result := TableRule.ImportFile.ColumnMapping;
end;  // TOutputFieldGenerator.ColumnMapping 

{-------------------------------------------------------------------------------
  Value from named parsed field of specified column type in input 
  data. 
}
function TOutputFieldGenerator.FieldValue(Fields: Fields; const ColumnTypeKey:
    String; const FieldName: String): OLEVariant;
begin
  Result := OutputField.FieldValue(
      Fields,
      ColumnMapping.ColumnTypeByKey(ColumnTypeKey),
      FieldName);
end;  // TOutputFieldGenerator.FieldValue 

{ ------------------------------------------------------------------------------
  Returns an SQL expression that computes the value of the specified field.
}
function TOutputFieldGenerator.FieldValueSQL(Tables: TTableSource;
    const ColumnTypeKey, FieldName: String): String;
begin
  Result := OutputField.FieldValueSQL(
      Tables,
      ColumnMapping.ColumnTypeByKey(ColumnTypeKey),
      FieldName);
end;

{-------------------------------------------------------------------------------
  Raise an ETableRuleException which states that the given output 
  field index is invalid for the field generator. 
}
procedure TOutputFieldGenerator.InvalidFieldIndex(FieldIndex: Integer);
begin
  raise ETableRuleException.CreateFmt(
      ResStr_InvalidFieldGeneratorIndex,
      [FieldIndex, ClassName]);
end;  // TOutputFieldGenerator.InvalidFieldIndex 

{-------------------------------------------------------------------------------
}
function TOutputFieldGenerator.TableRule: TTableRule;
begin
  Result := OutputField.TableRule;
end;  // TOutputFieldGenerator.TableRule 

{-------------------------------------------------------------------------------
}
function TOutputFieldGenerator.UserSuppliedData: TUserSuppliedData;
begin
  Result := TableRule.ImportFile.UserSuppliedData;
end;  // TOutputFieldGenerator.UserSuppliedData

{-==============================================================================
    TOutputField
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TOutputField.Create(Owner: TTableRule; Fields: Fields);
begin
  FTableRule := Owner;
  FName := Fields['Name'].Value;
  FDataType := Fields['Data_Type'].Value;
  FColumnTypeKey := VarToStr(Fields['IW_Column_Type_Key'].Value);
  FSourceFieldName := VarToStr(Fields['Source_Field_Name'].Value);
  FGeneratingClassName := VarToStr(Fields['Generating_Class_Name'].Value);
  if VarType(Fields['Generator_Field_Index'].Value) <> varNull then
    FGeneratorFieldIndex := Fields['Generator_Field_Index'].Value;
  FLiteralValue := VarToStr(Fields['Literal_Value'].Value);
end;  // TOutputField.Create

{-------------------------------------------------------------------------------
}
function TOutputField.ColumnMapping: TColumnMapping;
begin
  Result := TableRule.ImportFile.ColumnMapping;
end;  // TOutputField.ColumnMapping 

{-------------------------------------------------------------------------------
  Value from named parsed field of specified column type in input 
  data. 
}
function TOutputField.FieldValue(Fields: Fields; ColumnType: TColumnType; const
    FieldName: String): OLEVariant;
var
  I: Integer;
  lSql: String;
  lField: TParsedField;
  lMatch: TMatchRule;
  lSelectField: string;
  idx: integer;
  recordset: _Recordset;
begin
  lSql := '';
  lSelectField := '';

  if not Assigned(ColumnType.Parser) then begin
    lSelectField := ColumnType.Key;
    lSql := 'SELECT Record_No AS RowNum, * '
        + ' FROM #master'
        + ' ORDER BY Record_No';
  end
  else begin
    lField := nil;
    lMatch := nil;

    for I := 0 to ColumnType.Parser.FieldCount - 1 do
      if ColumnType.Parser.Fields[I].Name = FieldName then
      begin
        lField := ColumnType.Parser.Fields[I];
        lMatch :=
            TableRule.ImportFile.MatchRuleByKey(
                ColumnType.FieldMatchRuleKeys[I]);
        Break;
      end;

    if not Assigned(lField) then
      raise ETableRuleException.CreateFmt(
          ResStr_NoSuchParsedField,
          [ColumnType.Name, FieldName]);

    if ColumnType.Parser.SingleRecord then
    begin
      if not Assigned(lMatch) then begin
        lSelectField := ColumnType.Key + '_' + lField.Name;
        lSql := 'SELECT Record_No AS RowNum, *'
            + ' FROM #master'
            + ' ORDER BY Record_No';
      end
      else begin
        lSelectField := 'Match_Key';
        lSql := 'SELECT i.Record_No AS RowNum, m.*'
            + ' FROM #master AS i'
            + ' LEFT JOIN #' + lMatch.Name + ' AS m'
            + ' ON m.Import_Value = i.' + ColumnType.Key + '_' + lField.Name
            + ' ORDER BY i.Record_No';
      end;
    end
    else if TableRule.IsRelatedTable('CT_' + ColumnType.Key) then
      // multiple-record column, value available from Fields
      Result := Fields[ColumnType.Key + '_' + lField.Name].Value
    else
    begin
      if not Assigned(lMatch) then
        // multiple-record column with no matching
        lSql := 'SELECT i.Record_No AS RowNum, i.' + lField.Name
            + ' FROM #CT_' + ColumnType.Key + ' AS i'
      else
        // multiple-record column with matching
        lSql := 'SELECT i.Record_No AS RowNum, m.Match_Key'
            + ' FROM #CT_' + ColumnType.Key + ' AS i'
            + ' INNER JOIN #' + lMatch.Name + ' AS m'
            + ' ON m.Import_Value = i.' + lField.Name;

      lSql := lSql
          + ' ORDER BY i.Record_No, i.Position';
    end;
  end;
  // This procedure tends to get called a lot repetitively with the same query,
  // so use a simple cache to reduce unnecessary db hits.  Note that we
  // select * deliberately so that the query is likely to be useful elsewhere
  //
  //IntToStr(Fields['Record_No'].Value)

  if lSql <> '' then begin
    idx := mRecordsetNames.IndexOf(lSql);
    if idx>-1 then
      recordset := mRecordsets[idx] as _Recordset
    else begin
      recordset := dmDatabase.ExecuteSql(lSql, True);
      mRecordsetNames.Add(lSql);
      mRecordsets.Add(recordset);
    end;
    if recordset.Fields['RowNum'].Value > Fields['Record_No'].Value then
      // This is just a safety net, should never happen
      recordset.MoveFirst;
    while recordset.Fields['RowNum'].Value < Fields['Record_No'].Value do
      recordset.MoveNext;
    Assert(recordset.Fields['RowNum'].Value = IntToStr(Fields['Record_No'].Value));
    if lSelectField='' then
      Result := recordset.Fields[1].Value
    else
      Result := recordset.Fields[lSelectField].Value;
  end;
end;  // TOutputField.FieldValue

{ ------------------------------------------------------------------------------
  Returns an SQL expression that computes the value of the specified field.
}
function TOutputField.FieldValueSQL(Tables: TTableSource;
    ColumnType: TColumnType; const FieldName: String): String;
var
  lMatch: TMatchRule;
  lTableName: String;
  lFieldName: String;

  procedure FindMatchRule;
  var
    I: Integer;
    lField: TParsedField;
  begin
    lMatch := nil;
    lField := nil;
    
    for I := 0 to ColumnType.Parser.FieldCount - 1 do
    begin
      if ColumnType.Parser.Fields[I].Name = FieldName then
      begin
        lField := ColumnType.Parser.Fields[I];
        lMatch := TableRule.ImportFile.MatchRuleByKey(
            ColumnType.FieldMatchRuleKeys[I]);
        Break;
      end;
    end;

    if not Assigned(lField) then
      raise ETableRuleException.CreateFmt(
          ResStr_NoSuchParsedField,
          [ColumnType.Name, FieldName]);
  end;

begin
  if not Assigned(ColumnType.Parser) then
    Result := Tables.QualifiedField('#master', ColumnType.Key)
  else
  begin
    if ColumnType.Parser.SingleRecord then
    begin
      lTableName := '#master';
      lFieldName := ColumnType.Key + '_' + FieldName;
      FindMatchRule;
    end
    else if TableRule.IsRelatedTable('CT_' + ColumnType.Key)
        or TableRule.IsGeneratingField(ColumnType.Key) then
    begin
      lTableName := '@generated';
      lFieldName := ColumnType.Key + '_' + FieldName;
      lMatch := nil;
    end
    else begin
      lTableName := '#CT_' + ColumnType.Key;
      lFieldName := FieldName;
      FindMatchRule;
    end;
    if not Assigned(lMatch) then
      Result := Tables.QualifiedField(lTableName, lFieldName)
    else
      Result := Tables.MatchKey('#' + lMatch.Name, lTableName, lFieldName);
  end;
end;

{-------------------------------------------------------------------------------
}
function TOutputField.GetColumnType: TColumnType;
begin
  if not Assigned(FColumnType) and (ColumnTypeKey <> '') then
  begin
    FColumnType := ColumnMapping.ColumnTypeByKey(ColumnTypeKey);
  end;
  Result := FColumnType;
end;  // TOutputField.GetColumnType 

{-------------------------------------------------------------------------------
}
function TOutputField.GetGenerator: TOutputFieldGenerator;
var
  Cls: TOutputFieldGeneratorClass;
begin
  if (FGenerator = nil) and (FGeneratingClassName <> '') then
  begin
    Cls := GeneratorClass(FGeneratingClassName);
    if not Assigned(Cls) then
        raise ETableRuleException.CreateFmt(
            ResStr_UnknownOutputFieldGenerator,
            [FGeneratingClassName]);
    FGenerator := Cls.Create(Self);
  end;
  Result := FGenerator;
end;  // TOutputField.GetGenerator 

{-------------------------------------------------------------------------------
}
function TOutputField.UserSuppliedData: TUserSuppliedData;
begin
  Result := TableRule.ImportFile.UserSuppliedData;
end;  // TOutputField.UserSuppliedData 

{-------------------------------------------------------------------------------
  Value to be written to output for the specified imported record.
}
function TOutputField.Value(Fields: Fields): String;
begin
  if LiteralValue <> '' then
    Result := LiteralValue
  else if Assigned(Generator) then
    try
      Result := TransactSqlLiteral(
          Generator.Value(Fields, GeneratorFieldIndex));
    except
      on E: Exception do
        raise ETableRuleException.Create(
            Format(ResStr_GeneratorFailed, [Generator.ClassName, E.Message]),
            E);
    end
  else if ColumnTypeKey <> '' then
  begin
    if ColumnMapping.KeyIsMapped(ColumnTypeKey) then
      Result := TransactSqlLiteral(
          FieldValue(Fields, ColumnType, SourceFieldName))
    else if ColumnType.Key = CT_KEY_OBSERVER then
      Result := TransactSqlLiteral(UserSuppliedData.SingleObserverKey)
    else if UserSuppliedData.TermKey[ColumnType.TermListTable] <> '' then
      Result := TransactSqlLiteral(
          UserSuppliedData.TermKey[ColumnType.TermListTable])
    else
      Result := 'NULL';
  end
  else if SourceFieldName <> '' then
    // must be one of the generating fields
    Result := TransactSqlLiteral(Fields[SourceFieldName].Value)
  else
    raise ETableRuleException.CreateFmt(ResStr_BadOutputField, [Name]);
end;  // TOutputField.Value

{ ------------------------------------------------------------------------------
  Returns an SQL expression that computes the value of the output field.
}
function TOutputField.GeneratingSQL(Tables: TTableSource): String;
begin
  if LiteralValue <> '' then
    Result := LiteralValue
  else if Assigned(Generator) then
    Result := Generator.GeneratingSQL(GeneratorFieldIndex, Tables)
  else if ColumnTypeKey <> '' then
  begin
    if ColumnMapping.KeyIsMapped(ColumnTypeKey) then
      Result := FieldValueSQL(Tables, ColumnType, SourceFieldName)
    else if UserSuppliedData.TermKey[ColumnType.TermListTable] <> '' then
      Result := TransactSqlLiteral(
          UserSuppliedData.TermKey[ColumnType.TermListTable])
    else
      Result := 'NULL';
  end
  else if SourceFieldName <> '' then
  begin
    // TODO: the assumption that this must be one of the generating fields
    //       is not always correct; so far the only exception is when the
    //       value is taken from the first Taxon_Occurrence table rule's
    //       related table "Sample" (with Relationship = 2, i.e. not generating)
    //       I think we need to put the table name in the output field record.
    //      
    //       THIS IS A REVOLTING HACK.  FIX IT.
    if (TableRule.Key = 'SYSTEM0100000006') then
      Result := Tables.QualifiedField('#Sample', SourceFieldName)
    else
      Result := Tables.QualifiedField('@generated', SourceFieldName)
  end
  else
    raise ETableRuleException.CreateFmt(ResStr_BadOutputField, [Name]);
end;

{-==============================================================================
    TTableRule
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TTableRule.Create(Key: String; ImportFile: TImportFile);
begin
  FImportFile := ImportFile;
  FKey := Key;  
  FRequiredFields := TStringList.Create;
  FGeneratingFields := TStringList.Create;
  FGeneratingTables := TStringList.Create;
  FRelatedTables := TStringList.Create;
  FOutputFields := TObjectList.Create;
  Load;
  FPrimaryKey := dmDatabase.SplitPrimaryKey(TableName);
end;  // TTableRule.Create 

{-------------------------------------------------------------------------------
}
destructor TTableRule.Destroy;
begin
  FOutputFields.Free;
  FRelatedTables.Free;
  FGeneratingTables.Free;
  FGeneratingFields.Free;
  FRequiredFields.Free;
  inherited;
end;  // TTableRule.Destroy

{-------------------------------------------------------------------------------
  Apply this table rule, creating and populating the corresponding
  temporary table.
}
procedure TTableRule.Apply;
var
  lSql: String;
  lRecordsToGenerate, lFullRecordList: _Recordset;
  lLastInsertedKey: string;
  lInsertedKeys: TStringList;

  function HaveToGenerateRecord: boolean;
  begin
    result := FGeneratedRecordsQuery='';
    if FGeneratedRecordsQuery<>'' then begin
      // insert a record only for the specific list of import records (e.g. a
      // single sample can span many import records)
      if not lRecordsToGenerate.EOF then
        Result := lRecordsToGenerate.Fields['Record_No'].Value=lFullRecordList.Fields['Record_No'].Value;
    end;
  end;

  {-------------------------------------------------------------------------------
    Retrieve the match fields from the generating query - this is all the fields
    excluding the record number. Result is a string separated by # to give a
    means of looking this up.
  }
  function MatchFieldText: string;
  var
    i: integer;
  begin
    Result := '';
    for i := 1 to lFullRecordList.Fields.Count-1 do
      Result := Result + VarToStr(lFullRecordList.Fields[i].Value) + '#';
  end;

begin
  FIteration := 0;
  UpdateProgress(0);
  CreateTables;
  BuildGeneratingQuery;
  mRecordsets.Clear;
  mRecordsetNames.Clear;
  if FGeneratedRecordsQuery<>'' then
    lRecordsToGenerate := dmDatabase.ExecuteSQL(FGeneratedRecordsQuery, True);
  lFullRecordList := dmDatabase.ExecuteSql(FGeneratingQuery, True);
  lInsertedKeys := TStringList.Create;
  lInsertedKeys.Sorted := true;
  try
    with lFullRecordList do
      while not Eof do
      begin
        try
          if HaveToGenerateRecord then begin
            lLastInsertedKey := TryInsertOutputRecord(Fields);
            if Fields.Count>1 then
              lInsertedKeys.Add(MatchFieldText + '=' + lLastInsertedKey);
          end else if TableName = 'Sample_Recorder' then
            lLastInsertedKey := '''' + Fields[PrimaryKey.Key1].Value + ''', ''' +
                                       Fields[PrimaryKey.Key2].Value + ''''
          else if Fields.Count>1 then
            // if out generating query only has 1 field, the record number,
            // then all generating fields are missing so we are going to re-use
            // the same key for all rows. Otherwise, lookup the key from
            // last time these generating values were used.
            lLastInsertedKey := lInsertedKeys.Values[MatchFieldText];

          { Possibility of lLastInsertedKey being empty.
            Case found for this is for 1 same sample, 2 species to same associated species.
            So ignore.
          }
          if lLastInsertedKey <> '' then begin
            // record contributing input records
            lSql := 'INSERT INTO #RN_' + TableName + ' (' + PrimaryKey.Key1;
            if PrimaryKey.Key2 <> '' then
              lSql := lSql + ', ' + PrimaryKey.Key2;
            lSql := lSql + ', Record_No)';
            lSql := lSql + ' VALUES (' + lLastInsertedKey + ', '
                  + IntToStr(Fields['Record_No'].Value) + ')';
            dmDatabase.ExecuteSql(lSql);
          end;
        except
          on ENoRecordInserted do ;   // Allow insert of #RN join record to be skipped if
                                      // record insert was skipped
          on E:Exception do begin
            if dmDatabase.CheckError(E, dbePrimaryKeyViolation) then
              raise ETableRuleException.CreateNonCritical(Format(ResStr_DuplicateRecord, [TableName]), E)
            else
              raise;
          end;
        end;
        UpdateProgress(100 * Integer(AbsolutePosition) div RecordCount);
        if (FGeneratedRecordsQuery<>'') and HaveToGenerateRecord then
          lRecordsToGenerate.MoveNext;
        MoveNext;
      end;  // while not Eof
    mRecordsets.Clear;
    mRecordsetNames.Clear;
  finally
    lInsertedKeys.Free;
  end;
end;  // TTableRule.Apply

{-------------------------------------------------------------------------------
  Construct SQL to determine the records that will be added to the
  output table by this rule. Update FGeneratingQuery and
  FGeneratingTableExpression.
}
procedure TTableRule.BuildGeneratingQuery;
var
  I, J: Integer;
  lSelect: String;
  lFrom: String;
  lWhere: String;
  lOrder: String;
  lLastJoinTable: String;
  lAliases: Integer;
  lColumn: TColumnType;
  lMatch: TMatchRule;
  lKey: TPrimaryKey;
  lTable: String;
  lIsGenerating: Boolean;

  function NextAlias: String;
  begin
    Result := 't' + IntToStr(lAliases);
  end;

  function AddField(const FieldList: String; const FieldName: String): String;
  begin
    if FieldList = '' then
      Result := FieldName
    else
      Result := FieldList + ', ' + FieldName;
  end;

begin
  lSelect := '';
  lFrom := '#master';
  lOrder := '';
  lLastJoinTable := 'master';
  lAliases := 0;

  // add related tables to query
  for I := 0 to FRelatedTables.Count - 1 do
  begin
    lTable := FRelatedTables.Names[I];
    lIsGenerating := (FRelatedTables.ValueFromIndex[I] = '1');

    if LeftStr(lTable, 3) <> 'CT_' then
    begin
      lKey := dmDatabase.SplitPrimaryKey(lTable);
      lSelect := AddField(lSelect, '#' + lTable + '.' + lKey.Key1);
      if lIsGenerating then
        lOrder := AddField(lOrder, '#' + lTable + '.' + lKey.Key1);

      lFrom := lFrom
          + ' INNER JOIN #RN_' + lTable
          + ' ON #RN_' + lTable + '.Record_No = #'
              + lLastJoinTable + '.Record_No'
          + ' INNER JOIN #' + lTable
          + ' ON #' + lTable + '.' + lKey.Key1
              + ' = #RN_' + lTable + '.' + lKey.Key1;

      if lKey.Key2 <> '' then
      begin
        lSelect := AddField(lSelect, '#' + lTable + '.' + lKey.Key2);
        if lIsGenerating then
          lOrder := AddField(lOrder, '#' + lTable + '.' + lKey.Key2);
        lFrom := lFrom + ' AND #' + lTable + '.' + lKey.Key2
            + ' = #RN_' + lTable + '.' + lKey.Key2;
      end;

      lLastJoinTable := 'RN_' + lTable;
    end
    else if ImportFile.ColumnMapping.KeyIsMapped(MidStr(lTable, 4, 16)) then
    begin
      lColumn := ImportFile.ColumnMapping.ColumnTypeByKey(
          MidStr(lTable, 4, 16));

      lFrom := lFrom
          + ' INNER JOIN #' + lTable
          + ' ON #' + lTable + '.Record_No = #'
              + lLastJoinTable + '.Record_No';
      lLastJoinTable := lTable;

      for J := 0 to lColumn.Parser.FieldCount - 1 do
      begin
        lMatch := ImportFile.MatchRuleByKey(lColumn.FieldMatchRuleKeys[J]);
        if not Assigned(lMatch) then
        begin
          // non-matched field
          lSelect := AddField(
              lSelect,
              '#' + lTable + '.' + lColumn.Parser.Fields[J].Name + ' AS '
                  + lColumn.Key + '_' + lColumn.Parser.Fields[J].Name);
          if lIsGenerating then
            lOrder := AddField(
                lOrder,
                '#' + lTable + '.' + lColumn.Parser.Fields[J].Name);
        end
        else
        begin
          // matched field
          lSelect := AddField(
              lSelect,
              NextAlias + '.Match_Key AS '
                  + lColumn.Key + '_' + lColumn.Parser.Fields[J].Name);
          lFrom := lFrom
              + ' LEFT JOIN #' + lMatch.Name + ' AS ' + NextAlias
              + ' ON ' + NextAlias + '.Import_Value = #'
              + lLastJoinTable + '.' + lColumn.Parser.Fields[J].Name;
          if lIsGenerating then
            lOrder := AddField(lOrder, NextAlias + '.Match_Key');
          Inc(lAliases);
        end;
      end;
    end;  // if ImportFile.ColumnMapping.KeyIsMapped(...)
  end;  // for I := 0 ...

  // consider columns
  for I := 0 to GeneratingFieldCount - 1 do
  begin
    if not ImportFile.ColumnMapping.KeyIsMapped(GeneratingFields[I]) then
      Continue;

    lColumn := ImportFile.ColumnMapping.ColumnTypeByKey(GeneratingFields[I]);
    if lColumn.Parser = nil then
    begin
      // non-parsed column (=> no matching)
      lSelect := AddField(lSelect, '#master.' + lColumn.Key);
      lOrder := AddField(lOrder, '#master.' + lColumn.Key);
    end
    else if lColumn.Parser.SingleRecord then
    begin
      for J := 0 to lColumn.Parser.FieldCount - 1 do
      begin
        lMatch := ImportFile.MatchRuleByKey(lColumn.FieldMatchRuleKeys[J]);
        if not Assigned(lMatch) then
        begin
          // parsed field without matching
          lSelect := AddField(
              lSelect,
              '#master.' + lColumn.Key + '_' + lColumn.Parser.Fields[J].Name);
          lOrder := AddField(
              lOrder,
              '#master.' + lColumn.Key + '_' + lColumn.Parser.Fields[J].Name);
        end
        else
        begin
          // parsed field with matching
          lSelect := AddField(
                lSelect,
                NextAlias + '.Match_Key AS '
                    + lColumn.Key + '_' + lColumn.Parser.Fields[J].Name);
          lFrom := lFrom + ' LEFT JOIN #' + lMatch.Name + ' AS ' + NextAlias
              + ' ON ' + NextAlias + '.Import_Value = #master.'
              + lColumn.Key + '_' + lColumn.Parser.Fields[J].Name;
          // Force location name as a generating field, as its not handled like
          // other fields. Special case for Locations, which might use the
          // vague location name as a generator when the key is not specified. So
          // insert SQL to use the key if available, else use the location name
          if lColumn.Key = CT_KEY_LOCATION then
            lOrder := AddField(
                lOrder, 'ISNULL(CAST(' + NextAlias + '.Match_Key as VARCHAR(100)), '+
                NextAlias + '.Import_Value)')
          else
            lOrder := AddField(lOrder, NextAlias + '.Match_Key');
          Inc(lAliases);
        end;
      end;
    end
    else
    begin
      // multiple-record parsed column
      Screen.Cursor := crHourglass;
      try
        EnsureGroups(lColumn.Key);
      finally
        Screen.Cursor := crDefault;
      end;

      lSelect := AddField(lSelect, NextAlias + '.Group_ID AS ' + lColumn.Key);

      lFrom := lFrom + ' LEFT JOIN #IW_Group AS ' + NextAlias
          + ' ON (' + NextAlias + '.Column_Type_Key = ''' + lColumn.Key + ''''
          + ' AND ' + NextAlias + '.Record_No = #'
          + lLastJoinTable + '.Record_No)';

      lOrder := AddField(lOrder, NextAlias + '.Group_ID');

      lLastJoinTable := NextAlias;

      Inc(lAliases);
    end;
  end;

  if FilterExpression = '' then
    lWhere := ''
  else
    lWhere := ' WHERE ' + FilterExpression;

  FGeneratedRecordsQuery := '';
  if lSelect = '' then begin
    FGeneratedRecordsQuery := 'SELECT TOP 1 #master.Record_No'
        + ' FROM ' + lFrom + lWhere;
    // return all records to insert into the #RN_ table
    FGeneratingQuery := 'SELECT DISTINCT #master.Record_No'
        + ' FROM ' + lFrom + lWhere;
  end
  else if lOrder = '' then
    FGeneratingQuery := 'SELECT DISTINCT #master.Record_No, ' + lSelect
        + ' FROM ' + lFrom + lWhere
  else begin
    FGeneratingQuery := 'SELECT DISTINCT #master.Record_No AS Record_No, '
        + lSelect + ', ' + lOrder
        + ' FROM ' + lFrom + lWhere
        + ' ORDER BY #master.Record_No, ' + lOrder;
    // Also setup a query that returns just the distinct record numbers that
    // actually require an insert
    FGeneratedRecordsQuery := 'SELECT MIN(#master.Record_No) AS Record_No '
        + ' FROM ' + lFrom + lWhere
        + ' GROUP BY ' + lOrder
        + ' ORDER BY MIN(#master.Record_No), ' + lOrder;
  end;

  FGeneratingTableExpression := lFrom;
end;  // TTableRule.BuildGeneratingQuery

{ ------------------------------------------------------------------------------
  Constructs a Transact-SQL column definition.
}
function TTableRule.MakeColumnDefinition(const FieldName, DataType: String):
  String;
begin
  Result := FieldName + ' ' + DataType;
  if (UpperCase(LeftStr(DataType, 4)) = 'CHAR')
    or (UpperCase(LeftStr(DataType, 4)) = 'TEXT')
    or (UpperCase(LeftStr(DataType, 7)) = 'VARCHAR') then
  begin
    Result := Result + ' COLLATE SQL_Latin1_General_CP1_CI_AS';
  end;
end;
  
{-------------------------------------------------------------------------------
  Create the temporary tables used to hold the output from this
  rule.
}
procedure TTableRule.CreateTables;
begin
  CreatePrimaryTable;
  CreateRecordNumberTable;
  CreateGroupTable;
end;  // TTableRule.CreateTables

{ ------------------------------------------------------------------------------
  Create the temporary table used to hold the data that will be inserted into
  the database table corresponding to this rule.
}
procedure TTableRule.CreatePrimaryTable;
var
  I: Integer;
  lSql: String;
begin
  lSql := 'IF object_id(''tempdb..#' + TableName + ''') IS NULL'
      + ' CREATE TABLE #' + TableName + ' (';  
  for I := 0 to OutputFieldCount - 1 do
    lSql := lSql
        + MakeColumnDefinition(OutputFields[I].Name, OutputFields[I].DataType)
        + ', ';
  lSql := lSql + 'PRIMARY KEY (' + PrimaryKey.Key1;
  if PrimaryKey.Key2 <> '' then
    lSql := lSql + ', ' + PrimaryKey.Key2;
  lSql := lSql + '))';
  dmDatabase.ExecuteSql(lSql);
end;

{ ------------------------------------------------------------------------------
  Create the temporary table used to map record numbers in the source data onto
  primary key values for the table corresponding to this rule.
}
procedure TTableRule.CreateRecordNumberTable;
var
  lSql: String;
begin
  lSql := 'IF object_id(''tempdb..#RN_' + TableName + ''') IS NULL'
      + ' CREATE TABLE #RN_' + TableName + ' ('
      + PrimaryKey.Key1 + ' CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS, ';
  if PrimaryKey.Key2 <> '' then
    lSql := lSql + PrimaryKey.Key2 
        + ' CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS, ';
  lSql := lSql + 'Record_No INT' + ' PRIMARY KEY (' + PrimaryKey.Key1 + ', ';
  if PrimaryKey.Key2 <> '' then
    lSql := lSql + PrimaryKey.Key2 + ', ';
  lSql := lSql + 'Record_No))';
  dmDatabase.ExecuteSql(lSql);
end;

{ ------------------------------------------------------------------------------
  Create the temporary table used to group data from source fields that may be
  parsed to give multiple values.
}
procedure TTableRule.CreateGroupTable;
var
  lSql: String;
begin
  lSql := 'IF object_id(''tempdb..#IW_Group'') IS NULL'
      + ' CREATE TABLE #IW_Group ('
      + ' Column_Type_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,'
      + ' Qualification VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,'
      + ' Record_No INT,'
      + ' Group_Hash INT,'
      + ' Group_ID INT'
      + ' PRIMARY KEY (Record_No, Column_Type_Key, Qualification))';
  dmDatabase.ExecuteSql(lSql);
end;

{-------------------------------------------------------------------------------
  Ensure that #IW_Group contains the data for the given column
  type.
}
procedure TTableRule.EnsureGroups(const ColumnTypeKey: String);
var
  lGroupId: integer;
begin
  if not dmDatabase.GetStoredProcOutputParam(
      'usp_ImportWizardGroup_ColumnProcessed',
      ['@column_type_key', ColumnTypeKey],
       '@column_processed') then
  begin
    lGroupId := 1;
    // Get the list of distinct checksums, in first record order
    with dmDatabase.ExecuteSQL('SELECT DISTINCT Checksum, MIN(Record_No) ' +
          'FROM #CS_' + ColumnTypeKey +
          ' GROUP BY Checksum ' +
          ' ORDER BY MIN(Record_No) ASC', True) do
    begin
      while not EOF do begin
        dmDatabase.ExecuteSQL('INSERT INTO #IW_GROUP ('+
                  'Column_Type_Key, '+
                  'Qualification, '+
                  'Record_No, '+
                  'Group_Hash, '+
                  'Group_ID) '+
        'SELECT ''' + ColumnTypeKey + ''', '+
                  ''''', '+
                  'Record_No, '+
                  IntToStr(Fields[0].Value) + ','+
                  IntToStr(lGroupId)+
                  ' FROM #CS_' + ColumnTypeKey +
                  ' WHERE CheckSum=' + IntToStr(Fields[0].Value));
        Inc(lGroupId);
        MoveNext;
      end;
    end;
  end;
end; // TTableRule.EnsureGroups

{-------------------------------------------------------------------------------
  ID of an existing import wizard value group (or -1 if none
  exists) matching the values in the given column of the specified
  import record.
}
function TTableRule.ExistingGroupID(ColumnType: TColumnType; RecordNo:
    Integer; out Hash: string): Integer;
var
  lSql: String;
  lCount: Integer;
  lSqlGroupTest: string;
  lRs : _Recordset;
begin
  Result := -1;

  Hash := IntToStr(GroupChecksum(ColumnType, RecordNo));

  lSql := 'SELECT Record_No, Group_ID'
      + ' FROM #IW_Group'
      + ' WHERE Column_Type_Key = ''' + ColumnType.Key + ''''
      + ' AND Qualification = ''' + ColumnType.Qualification + ''''
      + ' AND Group_Hash = ' + Hash;

  with dmDatabase.ExecuteSql(lSql, True) do
    if not Eof then
    begin
      lSqlGroupTest := 'SELECT COUNT(*) AS Count '+
                    'FROM #CT_' + ColumnType.Key + ' AS c1 ' +
                    'INNER JOIN #CT_' + ColumnType.Key + ' AS c2 ' +
                    '	ON c2.Record_No='+ IntToStr(Fields['Record_No'].Value) +
                    '	AND c2.name=c1.name ' +
                    'WHERE c1.Record_No=' + IntToStr(RecordNo);
      lRs := dmDatabase.ExecuteSQL(lSqlGroupTest, True);
      lCount := dmDatabase.ExecuteSql(
            'SELECT COUNT(*) FROM #CT_' + ColumnType.Key +
            ' WHERE Record_No='+ IntToStr(RecordNo),
            True).Fields[0].Value;
      // check count of matched values is same as total in group (therefore complete match)
      if lCount = lRs.Fields['Count'].Value then
        Result := Fields['Group_ID'].Value;
    end;
end;  // TTableRule.ExistingGroupID

{-------------------------------------------------------------------------------
}
function TTableRule.GetGeneratingFieldCount: Integer;
begin
  Result := FGeneratingFields.Count;
end;  // TTableRule.GetGeneratingFieldCount 

{-------------------------------------------------------------------------------
  Generating field.
}
function TTableRule.GetGeneratingFields(Index: Integer): String;
begin
  Result := FGeneratingFields[Index];
end;  // TTableRule.GetGeneratingFields 

{-------------------------------------------------------------------------------
  Number of generating tables. 
}
function TTableRule.GetGeneratingTableCount: Integer;
begin
  Result := FGeneratingTables.Count;
end;  // TTableRule.GetGeneratingTableCount 

{-------------------------------------------------------------------------------
  Generating table. 
}
function TTableRule.GetGeneratingTables(Index: Integer): String;
begin
  Result := FGeneratingTables[Index];
end;  // TTableRule.GetGeneratingTables 

{-------------------------------------------------------------------------------
}
function TTableRule.GetOutputFieldCount: Integer;
begin
  Result := FOutputFields.Count;
end;  // TTableRule.GetOutputFieldCount 

{-------------------------------------------------------------------------------
}
function TTableRule.GetOutputFields(Index: Integer): TOutputField;
begin
  Result := FOutputFields[Index] as TOutputField;
end;  // TTableRule.GetOutputFields 

{-------------------------------------------------------------------------------
}
function TTableRule.GetRequiredFieldCount: Integer;
begin
  Result := FRequiredFields.Count;
end;  // TTableRule.GetRequiredFieldCount

{-------------------------------------------------------------------------------
}
function TTableRule.GetRequiredFields(Index: Integer): String;
begin
  Result := FRequiredFields[Index];
end;  // TTableRule.GetRequiredFields

{-------------------------------------------------------------------------------
  Hash of the individual values in the given column of the 
  specified import record. 
}
function TTableRule.GroupChecksum(ColumnType: TColumnType; RecordNo: Integer):
    Integer;  
begin
  Result := dmDatabase.ExecuteSQL('Select Checksum from #CS_' + ColumnType.Key
      + ' where Record_No = ' + IntToStr(RecordNo), True).Fields[0].Value;
end;  // TTableRule.GroupChecksum

{-------------------------------------------------------------------------------
  Insert a single record into the output temporary table for this 
  rule, based on the specified imported record. 
}
function TTableRule.TryInsertOutputRecord(Fields: Fields): string;
var
  I: Integer;
  lValue: String;
  lFieldList: String;
  lValueList: String;
  lSql: String;

  function ValueAvailable: Boolean;
  begin
    if (OutputFields[I].ColumnTypeKey = '')
        or (OutputFields[I].ColumnTypeKey = CT_KEY_OBSERVER)
    then
      Result := True
    else if
      ImportFile.ColumnMapping.KeyIsMapped(OutputFields[I].ColumnTypeKey)
    then
      Result := True
    else
      Result := (ImportFile.ColumnMapping.ColumnTypeByKey(
          OutputFields[I].ColumnTypeKey).TermListTable <> '');
  end;

  // handle special cases where a record should not be generated
  function SpecialCaseFilter: boolean;
  begin
    Result := False;
    // Handle case where we ensure a taxon occurrence created because it
    // is related to another is not joined to itself.
    if CompareText(TableName, 'taxon_occurrence_relation')=0 then begin
      Assert(OutputFields[2].Name='Taxon_Occurrence_Key_1', 'Taxon_Occurrence_Relation field list changed');
      Assert(OutputFields[3].Name='Taxon_Occurrence_Key_2', 'Taxon_Occurrence_Relation field list changed');
      Result := OutputFields[2].Value(Fields)=OutputFields[3].Value(Fields);
    end;
  end;

begin
  lFieldList := '';
  lValueList := '';
  Result := '';

  try
    for I := 0 to OutputFieldCount - 1 do
      if ValueAvailable then
      begin
        if lValueList<>'' then
        begin
          lValueList := lValueList + ', ';
          lFieldList := lFieldList + ', ';
        end;
        lValue := OutputFields[I].Value(Fields);
        lValueList := lValueList + VarToStr(lValue);

        lFieldList := lFieldList + OutputFields[I].Name;
        if CompareText(OutputFields[I].Name, PrimaryKey.Key1) = 0 then
          Result := lValue
        else if CompareText(OutputFields[I].Name, PrimaryKey.Key2) = 0 then
          Result := Result + ', ' + lValue;
      end;
    if SpecialCaseFilter then
      raise ENoRecordInserted.Create('Record insert aborted');

    lSql := 'INSERT INTO #' + TableName + ' (' + lFieldList + ')'
        + ' VALUES (' + lValueList + ')';

    dmDatabase.ExecuteSql(lSql);
  except
    on ENoRecordInserted do raise;
    on E: Exception do
      raise ETableRuleException.CreateNonCritical(
          Format(ResStr_FailedToInsertOutputRecord, [TableName, E.Message]));
  end;
end;  // TTableRule.InsertOutputRecord

{-------------------------------------------------------------------------------
  Is the named table one of the generating tables for this rule? 
}
function TTableRule.IsRelatedTable(const TableName: String): Boolean;
begin
  Result := FRelatedTables.IndexOfName(TableName) <> -1;
end;  // TTableRule.IsRelatedTable 

{ ------------------------------------------------------------------------------
  Is the specified column type one of the generating fields for this rule?
}
function TTableRule.IsGeneratingField(const ColumnTypeKey: String): Boolean;
begin
  Result := FGeneratingFields.IndexOf(ColumnTypeKey) <> -1;
end;

{-------------------------------------------------------------------------------
  Initialise the table rule from the corresponding database 
  record. 
}
procedure TTableRule.Load;
begin
  with dmDatabase.GetRecordset(
      'usp_IWTableRule_Get',
      ['@table_rule_key', Key]) do
  begin
    FTableName := Fields['Table_Name'].Value;
    FFilterExpression := VarToStr(Fields['Filter_Expression'].Value);
  end;
  LoadRequiredFields;
  LoadRelatedTables;
  LoadGeneratingFields;
  LoadOutputFields;
end;  // TTableRule.Load 

{-------------------------------------------------------------------------------
  Initialise GeneratingFields from the database. 
}
procedure TTableRule.LoadGeneratingFields;
begin
  with dmDatabase.GetRecordset(
      'usp_IWTableRule_SelectGeneratingFields',
      ['@table_rule_key', Key]) do
    while not Eof do
    begin
      FGeneratingFields.Add(Fields['IW_Column_Type_Key'].Value);
      MoveNext;
    end;
end;  // TTableRule.LoadGeneratingFields 

{-------------------------------------------------------------------------------
  Initialise OutputFields from the database.
}
procedure TTableRule.LoadOutputFields;
begin
  with dmDatabase.GetRecordset(
      'usp_IWTableRule_SelectOutputFields',
      ['@table_rule_key', Key]) do
    while not Eof do
    begin
      // Skip the Recorders output field for temporary imports
      if (not AppSettings.IWIsTempSurvey) and
          (Fields['IW_Output_Field_Key'].Value = OF_KEY_RECORDERS) then
        MoveNext
      else begin
        FOutputFields.Add(TOutputField.Create(Self, Fields));
        MoveNext;
      end;
    end;
end;  // TTableRule.LoadOutputFields

{-------------------------------------------------------------------------------
  Initialise FRelatedTables and FGeneratingTables from the database.
}
procedure TTableRule.LoadRelatedTables;
begin
  with dmDatabase.GetRecordset(
      'usp_IWTableRule_SelectRelatedTables',
      ['@table_rule_key', Key]) do
    while not Eof do
    begin
      if Fields['Relationship'].Value = 1 then
        FGeneratingTables.Add(Fields['Table_Name'].Value);
      FRelatedTables.Add(
          Fields['Table_Name'].Value 
          + '=' + VarToStr(Fields['Relationship'].Value));
      MoveNext;
    end;
end;  // TTableRule.LoadRelatedTables

{-------------------------------------------------------------------------------
}
procedure TTableRule.LoadRequiredFields;
begin
  with dmDatabase.GetRecordset(
      'usp_IWTableRule_SelectRequiredFields',
      ['@table_rule_key', Key]) do
    while not Eof do
    begin
      FRequiredFields.Add(Fields['IW_Column_Type_Key'].Value);
      MoveNext;
    end;
end;  // TTableRule.LoadRequiredFields

{-------------------------------------------------------------------------------
}
procedure TTableRule.UpdateProgress(Progress: Integer);
begin
  if Assigned(OnProgress) then OnProgress(Progress, True);
  if FIteration = 99 then Application.ProcessMessages;
  FIteration := (FIteration + 1) mod 100;
end;  // TTableRule.UpdateProgress


{ TTableSource }

{ ------------------------------------------------------------------------------
  Constructs and initializes a table source object.
}
constructor TTableSource.Create;
begin
  inherited;
  FAliases := TStringList.Create;
end;

{ ------------------------------------------------------------------------------
  Disposes of a table source object.
}
destructor TTableSource.Destroy;
begin
  FAliases.Free;
  inherited;
end;

{ ------------------------------------------------------------------------------
  Ensures that an alias has been generated for the specified table.
}
procedure TTableSource.EnsureTable(const TableName: String);
begin
  if FAliases.IndexOfName(TableName) = -1 then
  begin
    FAliases.Values[TableName] := 't' + IntToStr(FAliases.Count);
  end;  
end;

{ ------------------------------------------------------------------------------
  Returns the given field name, qualified with the alias for the specified
  table.
}
function TTableSource.QualifiedField(const TableName, FieldName: String):
  String;
begin
  EnsureTable(TableName);
  Result := FAliases.Values[TableName] + '.' + FieldName;
end;

{ ------------------------------------------------------------------------------
  Returns a value indicating whether the given name specifies a match.
}
function TTableSource.IsMatchName(const Name: String): Boolean;
begin
  Result := Pos(':', Name) > 0;
end;

{ ------------------------------------------------------------------------------
  Constructs a name that specifies a match.
}
function TTableSource.MatchName(const MatchTable, DataTable, FieldName: String):
    String;
begin
  Result := Format('%s:%s:%s', [MatchTable, DataTable, FieldName]);
end;

{ ------------------------------------------------------------------------------
  Decodes a name that specifies a match.
}
procedure TTableSource.DecodeMatchName(const MatchName: String;
  out MatchTable, DataTable, FieldName: String);
var
  I, J: Integer;
begin
  I := Pos(':', MatchName);
  J := PosEx(':', MatchName, I + 1);

  MatchTable := LeftStr(MatchName, I - 1);
  DataTable := MidStr(MatchName, I + 1, J - (I + 1));
  FieldName := RightStr(MatchName, Length(MatchName) - J);
end;

{ ------------------------------------------------------------------------------
  Returns the name of the match key field qualified with the alias for the
  specified match.
}
function TTableSource.MatchKey(const MatchTable, DataTable, FieldName: String):
  String;
var
  lMatchName: String;
begin
  lMatchName := MatchName(MatchTable, DataTable, FieldName);
  EnsureTable(DataTable);
  EnsureTable(lMatchName);
  Result := FAliases.Values[lMatchName] + '.Match_Key';
end;

{ ------------------------------------------------------------------------------
  Returns the SQL table source expression joining the tables whose aliases have
  been generated by this table source.  
}
function TTableSource.TableSourceSQL: String;
var
  I: Integer;
  Table: String;
  Alias: String;
  JoinTables: Integer;
  LastAlias: String;
  Key: TPrimaryKey;

  { ----------------------------------------------------------------------------
    Returns the alias for the current join table.
  }
  function JoinAlias: String;
  begin
    Result := 'j' + IntToStr(JoinTables);
  end;

  { ----------------------------------------------------------------------------
    Adds the match specified by the given match name to the table source
    expression.
  }
  procedure JoinMatchTable(const MatchName: String);
  var
    MatchTable: String;
    DataTable: String;
    FieldName: String;
  begin
    DecodeMatchName(MatchName, MatchTable, DataTable, FieldName);

    Result := Result + #10
        + 'LEFT JOIN ' + MatchTable + ' AS ' + Alias + #10
        + 'ON ' + Alias + '.Import_Value = '
        + QualifiedField(DataTable, FieldName);
  end;
  
begin
  Result := '';
  JoinTables := 0;
  
  for I := 0 to FAliases.Count - 1 do
  begin
    Table := FAliases.Names[I];
    Alias := FAliases.Values[Table];

    if I = 0 then
    begin
      Result := Table + ' AS ' + Alias;
      LastAlias := Alias;
    end
    else if (Table = '@generated')
        or (Table = '#master')
        or (LeftStr(Table, 4) = '#CT_') then
    begin
      Result := Result + #10
          + 'INNER JOIN ' + Table + ' AS ' + Alias + #10
          + 'ON ' + Alias + '.Record_No = ' + LastAlias + '.Record_No';
      LastAlias := Alias;
    end
    else if IsMatchName(Table) then
      JoinMatchTable(Table)
    else
    begin
      Result := Result + #10
          + 'INNER JOIN #RN_' + MidStr(Table, 2, Length(Table) - 1)
          + ' AS ' + JoinAlias + #10
          + 'ON ' + JoinAlias + '.Record_No = ' + LastAlias + '.Record_No';
      LastAlias := JoinAlias;

      Key := dmDatabase.SplitPrimaryKey(MidStr(Table, 2, Length(Table) - 1));
      
      Result := Result + #10
          + 'INNER JOIN ' + Table + ' AS ' + Alias + #10
          + 'ON ' + Alias + '.' + Key.Key1
          + ' = ' + LastAlias + '.' + Key.Key1;          

      if Key.Key2 <> '' then
        Result := Result + ' AND ' + Alias + '.' + Key.Key2
            + ' = ' + LastAlias + '.' + Key.Key2;

      LastAlias := JoinAlias;
      Inc(JoinTables);
    end;
    
  end;
end;


initialization
  mGenerators := TThreadList.Create;
  mRecordsetNames := TStringList.Create;
  mRecordsets := TInterfaceList.Create;

finalization
  mGenerators.Free;
  mRecordsetNames.Free;
  mRecordsets.Free;

end.
