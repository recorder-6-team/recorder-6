//==============================================================================
//  Unit:        DBMerger
//
//  Implements:  TDBMerger
//
//  Description: A utility class that takes a database and merges its contents
//               into the main database. Uses the Titan datasets to do the work.
//               Optional to override the SetProgress and SetStatus messages to
//               allow you to do what you want with progress information.
//
//  Author:      John van Breda
//  Created:     29 September 2000
//
//  Last Revision Details:
//    $Revision: 74 $
//    $Date: 26/05/10 14:45 $
//    $Author: Andrewkemp $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit DBMerger;

interface

uses
  Windows,Sysutils, Classes, Forms, ComCtrls, DataClasses, JnccDatasets, Constants,
  Db, ExceptionForm, TaskProgress, Relationships_ADO, TablePriorityList_ADO,
  DatabaseUtilities, ComObj, ADODB, ADOInt, DatabaseAccessADO, Variants,dialogs;

resourcestring
  ResStr_FailedToInsert = 'Failed to insert %s into %s';
  ResStr_DatabaseErrorMessage = 'Database Error Message';
  ResStr_ItemsRejected = 'Items rejected by %s in data imported at %s on %s';
  ResStr_NoItemsRejected = '%s did not reject any items in data imported at %s on %s.';
  ResStr_OccurrenceRejectedNoDetermination = 'The occurrence record '+
      'and all related data were not imported as the determination record is missing or invalid.';
  ResStr_RejectedBecauseOfParent =
      'Note: This record may have been rejected because of a parent record that was also rejected.';

type
  EDBMergerError = class(TExceptionPath);

  TDBMerger = class
  private
    FSourceDBPath : string;
    FConnection : TADOConnection;
    FRecordsProcessed : integer; // includes excluded records
    FRecordsCopied    : integer; // excludes excluded records
    FLogItemsCount    : integer; // counts failed imported items
    FSourceTable : TJnccTable;
    FLinkedTableName: String;
    FDestTable   : TJnccQuery;
    FDestTableCount: TJnccQuery;
    FInsertQuery : TJnccQuery;
    FExclusionList: TStringList; // supplied list of records to skip
    FDuplicateList: TStringList;
    FCreatedKeys : TStringList; //name-value pairs - last key values assigned to each table
    FKeysToBulkInsert : TStringList; // bulk insert of many recs into 1 table
    FTotalRecords : integer; // for progress tracking
    FRebuildTaxonIndex: Boolean;
    FSetProgress: TSetProgressEvent;
    FSetStatus: TSetStatusEvent;
    FLoggedErrors: Boolean;
    FSiteID: string;
    FDefaultOutputPath: String;
    procedure CheckDefaultSampleTypeImage(const ATableName: String);
    procedure CopyRecordToRecord(iSource, iDest: TDataset; const iKeyField1, iKeyField2: string);
    procedure DoSQLInsertRecords;
    procedure DoSQLInsertRecordsParent(const AParentKeyField: String);
    procedure ExamineWhyRecordsFailToInsert;
    procedure InsertUsingSQL(iPrimaryKey: TPrimaryKey);
    procedure LogRejection(const iTable, iKey, AError: string);
    procedure RememberKeyIfSiteIDOurs(const iTableName: String; const iKey: TKeyString);
    function RemoveUnwantedOccurrences: Integer;
    function SourceTableFieldList(const AAlias: String): String;
    function TotalRecordsInDest: Integer;
    function UpdateDuplicateRecord(iRecord: TSingleRecord; iPrimaryKey: TPrimaryKey): boolean;
    procedure UpdateLastKeys;
    procedure UpdateRebuildTaxonIndexFlag(const ATableName: String);
    procedure WriteToLog(const ALine: String; AClearFile: Boolean = false);
    function ErrorCanBeTrapped(Error: Error): Boolean;
    function IsZeroSmallDateField(AField: TField): Boolean;
    procedure SetInclusionMarks(const ATableName, APrimaryKey: string; AKeysIncluded: TStringList);
    function RoundToMinutes(ADateTime: TDateTime): TDateTime;
    procedure SetSiteID(const Value: string);
    function GetErrorFilePath : string;

  protected
    function CopySingleRecord(const iPrimaryKey: TPrimaryKey): boolean; virtual;
    procedure CopyTableIntoMainDB(const iTableName: string); virtual;
    function DuplicateRecord(iSingleRecord: TSingleRecord): boolean; virtual;
    function ExcludedRecord(iSingleRecord: TSingleRecord): boolean; virtual;
    procedure SetStatus(const iText: string); virtual; // supplies current status text
    procedure SetProgress(iRecCount: integer); virtual; // number of records processed
  public
    constructor Create(const iSourceDBPath: string; ASetProgress: TSetProgressEvent;
        ASetStatus: TSetStatusEvent); virtual;
    destructor Destroy; override;
    function DoMerge(iDuplicateList, iExclusionList: TStringList; iTablesToCopy: TStringList;
      iTotalRecords: integer): integer; overload; virtual;
    function DoMerge(iDuplicateList, iExclusionList: TStringList; iTablesToCopy: TStringList;
      iTotalRecords: integer; var AImportRejectsFile: TFileName): integer; overload; virtual;
    property RebuildTaxonIndex: Boolean read FRebuildTaxonIndex;
    property SiteID: string read FSiteID write SetSiteID;
    property DefaultOutputPath: String read FDefaultOutputPath write FDefaultOutputPath;
  end;

//==============================================================================
implementation

uses
  Genfuncs, GeneralFunctions, BaseADODataModule, DefaultPaths,registry;

const
  BLOCK_SIZE = 2000;

  JET_ERROR_ODBC_CALL_FAILED = 3146;
      // "ODBC--call failed"
  JET_ERROR_TRIED_TO_ASSIGN_NULL = 3162;
      // "You tried to assign the Null value to a variable that is not a
      // Variant data type"

resourcestring
  ResStr_InsertFailed =  'An Insert SQL statement failed in table ';
  ResStr_CleanupFailed = 'Cleanup of taxon and biotope occurrence records failed';
  ResStr_CopyTableError ='Error occurred transferring records for %s into the main database: %s';
  ResStr_CopyingRecords = 'Copying records into %s...';
  ResStr_InsertFail = 'Insert failed';
  ResStr_RemovingInvalidRec = 'Removing invalid records...';

//==============================================================================
{ TDBMerger }

constructor TDBMerger.Create(const iSourceDBPath: string; ASetProgress:
    TSetProgressEvent; ASetStatus: TSetStatusEvent);
begin
  inherited Create;
  FLogItemsCount := 0;
  FSourceDBPath := iSourceDBPath;
  FConnection := TADOConnection.Create(nil);
  FConnection.ConnectionString := FSourceDBPath;
  FConnection.LoginPrompt := false;
  FConnection.Open;

  FSourceTable := TJnccTable.Create(nil);
  FSourceTable.Connection := FConnection;

  // If using Access DB, destination table is in SourceDB, but is a linked table
  // to main DB
  FDestTable := TJnccQuery.Create(nil);
  FDestTable.Connection := dmDatabase.Connection;
  FDestTableCount := TJnccQuery.Create(nil);
  FDestTableCount.Connection := FConnection;

  FInsertQuery := TJnccQuery.Create(nil);
  FInsertQuery.Connection := FConnection;

  FCreatedKeys := TStringList.Create;
  FKeysToBulkInsert := TStringList.Create; // insert many records into main db in 1 sql statement

  FSetProgress := ASetProgress;
  FSetStatus   := ASetStatus;
  FRebuildTaxonIndex := false;
  // Assume main app is Recorder, but still allow other apps to change location of log file
  FDefaultOutputPath := GetProgramDataFolder(PATH_USER_FILES);
end;  // Create

//==============================================================================
destructor TDBMerger.Destroy;
begin
  FSourceTable.Free;
  FDestTable.Free;
  FDestTableCount.Free;
  FInsertQuery.Free;
  FCreatedKeys.Free;
  FKeysToBulkInsert.Free;
  if not FLoggedErrors then
    WriteToLog(
        Format(ResStr_NoItemsRejected, [Application.Title, TimeToStr(Now), DateToStr(Now)]), True);
  if FConnection.Connected then FConnection.Close;
  FConnection.Free;
  inherited;
end;  // Destroy

//==============================================================================
{ Must check all sample types have a default bitmap }
procedure TDBMerger.CheckDefaultSampleTypeImage(const ATableName: String); 
var lFileName: String;
    lqry: TJnccQuery;
begin
  lFileName := ExtractFilePath(Application.ExeName) + 'DEFAULT.BMP';
  if FileExists(lFileName) then begin
    lqry := TJnccQuery.Create(nil);
    with lqry do
      try
        lqry.Connection := FConnection;
        SQL.Text := 'SELECT * FROM ' + ATableName + ' WHERE "Image" IS NULL';
        Open;
        while not Eof do begin
          Edit;
          TBlobField(FieldByName('IMAGE')).LoadFromFile(lFileName);
          Post;
          Next;
        end;
      finally
        Close;
        Free;
      end;
  end;
end;  // CheckDefaultSampleTypeImage

//==============================================================================
{ Public procedure to actually perform the merge of 2 databases,  iTablesToCopy
     -  a stringlist of the tables in the source db to process.  If nil, then
     all tables processed. }
function TDBMerger.DoMerge(iDuplicateList, iExclusionList: TStringList;
  iTablesToCopy: TStringList; iTotalRecords: integer): integer;
var lDummyFile: TFileName;
begin
  Result := DoMerge(iDuplicateList, iExclusionList, iTablesToCopy,
                    iTotalRecords, lDummyFile);
end;

//==============================================================================
{ Public procedure to actually perform the merge of 2 databases,  iTablesToCopy
     -  a stringlist of the tables in the source db to process.  If nil, then
     all tables processed.  Also returns details of the file containing
     rejection errors.}
function TDBMerger.DoMerge(iDuplicateList, iExclusionList: TStringList;
  iTablesToCopy: TStringList; iTotalRecords: integer;
  var AImportRejectsFile: TFileName): integer;
var
  lPriorityList : TTablePriorityList;
  lTableIndex : integer;
  lFreeList: Boolean;
begin
  WriteToLog(Format(ResStr_ItemsRejected, [Application.Title, TimeToStr(Now), DateToStr(Now)]) + ':', true);
  AImportRejectsFile := GetErrorFilePath + IMPORT_REJECTS_FILE;
  dmDatabase.RunStoredProc('usp_ImportInitialise', []);
  FRecordsProcessed := 0;
  FRecordsCopied := 0;
  FTotalRecords := iTotalRecords;
  { Store the list of exclusions}
  FExclusionList := iExclusionList;
  FDuplicateList := iDuplicateList;

  lFreeList := false;
  // If no tables provided, get all of them from import DB.
  if iTablesToCopy = nil then begin
    lFreeList := true;
    iTablesToCopy := TStringList.Create;
    FConnection.GetTableNames(iTablesToCopy);
  end;
  { Build a list of the tables in the main db, sorted by order in which we
    must import them so we don't have referential integrity problems }
  lPriorityList := TTablePriorityList.Create(dmDatabase.Relationships);
  try
    // Create linked tables for data transfer to SQL Server database
    for lTableIndex := 0 to iTablesToCopy.Count - 1 do
      dmDatabase.CreateLinkedTable(FConnection, iTablesToCopy[lTableIndex],
                                   'SQLSERVER_' + iTablesToCopy[lTableIndex]);
    // Do the data transfer
    for lTableIndex := 0 to lPriorityList.Count-1 do
    begin
      { if we have a supplied table list, then skip non-found tables }
      if iTablesToCopy<>nil then
        if iTablesToCopy.IndexOf(lPriorityList.Item[lTableIndex])=-1 then
          Continue; // on to next table...
      SetStatus(Format(ResStr_CopyingRecords, [ReadableFormat(lPriorityList.Item[lTableIndex])]));
      FLinkedTableName := 'SQLSERVER_' + lPriorityList.Item[lTableIndex];
      CopyTableIntoMainDB(lPriorityList.Item[lTableIndex]);
    end;
    Result := FRecordsCopied - FLogItemsCount;
    UpdateLastKeys;
    Result := Result - RemoveUnwantedOccurrences;

    // Remove linked tables
    for lTableIndex := 0 to iTablesToCopy.Count - 1 do
      dmDatabase.RemoveLinkedTable(FConnection, 'SQLSERVER_' + iTablesToCopy[lTableIndex]);
  finally
    lPriorityList.Free;
    if lFreeList then iTablesToCopy.Free;
     dmDatabase.RunStoredProc('usp_ImportFinalise', []);
  end;
end;  // DoMerge

//==============================================================================
procedure TDBMerger.CopyTableIntoMainDB(const iTableName: string);
var
  lPrimaryKey : TPrimaryKey; //  fetch key details at this level for improved performance
  lCopyOk: boolean;
begin
  try
    FSourceTable.TableName := iTableName;
    try
      FSourceTable.Open;
    except on Exception do // if table doesn't exist or can't open, go to next table
      Exit;
    end;
    // Don't rebuild taxon indexes unless records in the table.
    if not FSourceTable.Eof then
      UpdateRebuildTaxonIndexFlag(FSourceTable.TableName);
    try
      FKeysToBulkInsert.Clear;
      FKeysToBulkInsert.Capacity := FSourceTable.RecordCount;
      lPrimaryKey := dmDatabase.SplitPrimaryKey(FSourceTable.TableName);
      lCopyOk := true;
      while not FSourceTable.Eof do
      begin
        lCopyOk := lCopyOk and CopySingleRecord(lPrimaryKey);
        FSourceTable.Next;
      end; // while

      { If we have a parent key then be careful about the order }
      // Some tables have a field called PARENT_KEY, others PARENT!!!!
      if Assigned(FSourceTable.FindField('Parent_Key')) then
        DoSQLInsertRecordsParent('Parent_Key')
      else
      if Assigned(FSourceTable.FindField('Parent')) then
        DoSQLInsertRecordsParent('Parent')
      else
        DoSQLInsertRecords;

      if not lCopyOk then
        ExamineWhyRecordsFailToInsert;
    finally  // ensure table is closed
      FSourceTable.Close;
    end; // try..finally
    { If Sample type table, check has default images }
    if CompareText(FLinkedTableName, 'SQLSERVER_Sample_Type')=0 then
      CheckDefaultSampleTypeImage('SQLSERVER_Sample_Type');
  except
    on E : Exception do
      raise EDBMergerError.Create(Format(ResStr_CopyTableError, [FLinkedTableName, E.Message]), E);
  end;
end;  // CopyTableIntoMainDB

{-------------------------------------------------------------------------------
  Copies a single record, or adds it to the list for later bulk insertion.
     Returns true if Ok, or false if attempt to copy record failed.
}
function TDBMerger.CopySingleRecord(const iPrimaryKey: TPrimaryKey): boolean;
var
  lSingleRecord : TSingleRecord;
begin
  Result := true;
  { First, locate the destination record to copy into }
  lSingleRecord.TableName := FSourceTable.TableName;
  lSingleRecord.Key1 := FSourceTable.FieldByName(iPrimaryKey.Key1).AsString;
  if iPrimaryKey.Key2 <> '' then
    lSingleRecord.Key2 := FSourceTable.FieldByName(iPrimaryKey.Key2).AsString
  else
    lSingleRecord.Key2 := '';
  if not ExcludedRecord(lSingleRecord) then
  begin
    { Remember record for local key checks }
    if iPrimaryKey.Key2='' then
      RememberKeyIfSiteIDOurs(FSourceTable.TableName,
                              FSourceTable.FieldByName(iPrimaryKey.Key1).AsString);
    // See if record is duplicate, and if it is, do the update,
    // otherwise it is a new record.
    Inc(FRecordsCopied); //  even if it failed, because the failure will get logged.
    if not UpdateDuplicateRecord(lSingleRecord, iPrimaryKey) then begin
      if iPrimaryKey.Key2='' then
        FKeysToBulkInsert.Add('''' + lSingleRecord.Key1 + '''')
      else
        try // must insert 1 record at a time, due to several fields in key
          InsertUsingSQL(iPrimaryKey);
        except
          on Exception do begin
            //Continue trying to insert records even if fail on one update
            Inc(FRecordsProcessed);  // increase it even if failed
            SetProgress(FRecordsProcessed);
            Result := false;
            Exit;
          end;
        end;
    end;
  end;
  Inc(FRecordsProcessed);  // increase it even if excluded
  SetProgress(FRecordsProcessed);
end;  // CopySingleRecord

//==============================================================================
// If iKeyField1 and iKeyField2 are empty, all fields will be copied.
procedure TDBMerger.CopyRecordToRecord(iSource, iDest: TDataset;
  const iKeyField1, iKeyField2: string);
var
  i : integer;
  lTransferField : String;
begin
  for i := 0 to iSource.FieldCount-1 do
  begin
    { Skip primary keys or fields that are not installed in the main db
        if appropriate }
    lTransferField := iSource.Fields[i].FieldName;
    if (lTransferField = iKeyField1) or (lTransferField = iKeyField2)
        or (iDest.FieldList.IndexOf(lTransferField)=-1) or (lTransferField = 'Timestamp') then
      Continue; // skip field in loop

    { Transfer data in best way for data type }
    if iSource.FieldByName(lTransferField).IsNull or
        IsZeroSmallDateField(iSource.FieldByName(lTransferField)) then
      iDest.FieldByName(lTransferField).Clear  // Value := Null
    else
    if (iSource.FieldByName(lTransferField).IsBlob) then
      iDest.FieldByName(lTransferField).Assign(iSource.Fields[i])
    else
    if iSource.FieldByName(lTransferField).DataType = ftDateTime then begin
      // Data type for VagueDates has changed to Integer
      if (Pos('VAGUE_DATE_START', lTransferField) > 0) or (Pos('VAGUE_DATE_END', lTransferField) > 0) then
        iDest.FieldByName(lTransferField).AsInteger := Trunc(iSource.Fields[i].AsDateTime)
      else if SameText(lTransferField, 'Entered_Date')
          or SameText(lTransferField, 'Changed_Date')
          or SameText(lTransferField, 'Checked_Date') then
        // small date times cannot store seconds
        iDest.FieldByName(lTransferField).AsDateTime := RoundToMinutes(iSource.Fields[i].AsDateTime)
      else
        iDest.FieldByName(lTransferField).AsDateTime := iSource.Fields[i].AsDateTime;
    end else
    if iSource.FieldByName(lTransferField).DataType = ftBoolean then
      iDest.FieldByName(lTransferField).AsBoolean := iSource.Fields[i].AsBoolean
    else
    if CompareText(iSource.FieldByName(lTransferField).FieldName, 'Timestamp')<>0 then
      iDest.FieldByName(lTransferField).Value := iSource.Fields[i].Value;
  end; // for
end;  // CopyRecordToRecord

//==============================================================================
function TDBMerger.SourceTableFieldList(const AAlias: String): String;
var
  i: Integer;
  lAlias: string;

    // For Changed Dates, zero values are converted to null to fix problems
    // introduced by third party imports which don't set this value to null.
    function GetSourceFieldName(AFieldName: string): string;
    const
      SQL_NOZERODATE = 'IIF(%s%s=0, NULL, %s%s) AS %s';
    begin
      if SameText(AFieldName, 'Changed_Date') or SameText(AFieldName, 'Checked_Date') then
        Result := Format(SQL_NOZERODATE, [lAlias, AFieldName, lAlias, AFieldName, AFieldName])
      else
        result := lAlias + AFieldName;
    end;

begin
  Result := '';
  if AAlias='' then
    lAlias := ''
  else
    lAlias := AAlias + '.';
  for i := 0 to FSourceTable.FieldCount - 1 do
    // Skip any timestamp fields.
    if CompareText(FSourceTable.Fields[i].FieldName, 'Timestamp') <> 0 then
      Result := Result + ', ' + GetSourceFieldName(FSourceTable.Fields[i].FieldName);
  // If any fields, then remove leading comma
  if Result <> '' then
    Result := Trim(Copy(Result, 2, Length(Result)));
end;  // SourceTableFieldList

{-------------------------------------------------------------------------------
  Rounds a date time to minutes so it can be stored in a smalldatetime
}
function TDBMerger.RoundToMinutes(ADateTime: TDateTime): TDateTime;
begin
  Result := trunc(ADateTime) + trunc((ADateTime - trunc(ADateTime)) * 24 * 60) / (24 * 60)
end;

//==============================================================================
{ Insert multiple records from source into destination db in one table.  Use
     single SQL statement and IN clause for better performance.  Primary key
     must be single field }
procedure TDBMerger.DoSQLInsertRecords;
var lSectionPos, lPos : integer;
    lInString         : String;
    lExpectedCount    : integer;
    lFailureDetected  : boolean;
begin
  lFailureDetected := False;
  if FKeysToBulkInsert.Count > 0 then
  begin
    with FInsertQuery do
    begin
      SQL.Clear;
      lPos := 0;
      { Prepare query to copy 2000 records at a time, so we don't break 64K limit }
      while lPos < FKeysToBulkInsert.Count do
      begin
        lInString := '';
        lSectionPos := lPos;
        lExpectedCount := TotalRecordsInDest;  // count how many we intend to insert
        while lSectionPos < Min(FKeysToBulkInsert.Count, lPos + BLOCK_SIZE) do
        begin
          lInString := lInString + ',' + FKeysToBulkInsert[lSectionPos];
          Inc(lSectionPos);
          Inc(lExpectedCount);
        end;
        lInString[1] := ' ';  // Remove first ','

        // just in case dest table not set yet
        SQL.Clear;
        SQL.Text := Format('INSERT INTO %s SELECT %s FROM %s AS Src WHERE %s IN ',
                           [FLinkedTableName, SourceTableFieldList('Src'), FSourceTable.TableName,
                            dmDatabase.GetPrimaryKey(FSourceTable.TableName, False)]);
        SQL.Add('(' + lInString + ')');
        try
          ExecSQL;
          WriteToLog('Created '+FLinkedTableName);
          if TotalRecordsInDest <> lExpectedCount then
            lFailureDetected := True;
        except
          on E:EOleException do begin
            WriteToLog('Exception on create for '+FLinkedTableName + ': ' + E.Message);
            if ErrorCanBeTrapped(FInsertQuery.Connection.Errors[0]) then
              lFailureDetected := true
            else
              raise EDBMergerError.Create(Format(ResStr_CopyTableError, [FLinkedTableName, E.Message]), E);
          end;
        end;
        lPos := lSectionPos;
      end;
    end;
    if lFailureDetected then
      ExamineWhyRecordsFailToInsert;
  end;  // if Count>0
end;  // DoSQLInsertRecords

//==============================================================================
{ Insert multiple records from source into destination db in one table.  Use
     single SQL statement and IN clause for better performance.  Primary key
     must be single field.  In this case, we repeat running the transfer SQL
     until no more records transfer as this allows records whose parent is
     also being imported to also import. }
procedure TDBMerger.DoSQLInsertRecordsParent(const AParentKeyField: String);
var
  lOldRecCount, lNewRecCount, lExpectedCount : integer;
  lFailureDetected : boolean;
  lPrimaryKey: string;
const
  SQL_TOP_LEVEL = 'INSERT INTO %s '+
      'SELECT %s '+
      'FROM %s Src '+
      'WHERE INCMark_ZZ=-1 AND SRC.%s IS NULL';
  SQL_CHILDREN = 'INSERT INTO %s ' +
      'SELECT %s ' +
      'FROM (%s A ' +
      'INNER JOIN %s S ON S.%s =A.%s) '+
      'LEFT JOIN %s S2 ON S2.%s = A.%s '+
      'WHERE A.INCMark_ZZ=-1 AND S2.%s IS NULL';
    procedure RunQuery;
    begin
      lOldRecCount := lNewRecCount;
      try
        FInsertQuery.ExecSQL;
      except
        on E:EOleException do
          if not ErrorCanBeTrapped(FInsertQuery.Connection.Errors[0]) then
            raise EDBMergerError.Create(Format(ResStr_CopyTableError, [FLinkedTableName, E.Message]), E);
      end;
      lNewRecCount := TotalRecordsInDest;
    end;
begin
  lFailureDetected := false;
  with FInsertQuery do begin
    SQL.Text := 'ALTER TABLE ' + FSourceTable.TableName + ' ADD COLUMN INCMark_ZZ YesNo';
    FInsertQuery.ExecSQL;
    try
      lPrimaryKey := dmDatabase.GetPrimaryKey(FSourceTable.TableName, False);
      lExpectedCount := TotalRecordsInDest + FKeysToBulkInsert.Count;
      SetInclusionMarks(FSourceTable.TableName, lPrimaryKey, FKeysToBulkInsert);
      SQL.Text := Format(SQL_TOP_LEVEL,
                         [FLinkedTableName, SourceTableFieldList('Src'), FSourceTable.TableName,
                          AParentKeyField]);
      RunQuery;
      if lNewRecCount<>lOldRecCount then begin
        // loop, adding each 'layer' of children at a time until there are no
        // more children to add
        SQL.Clear;
        SQL.Text := Format(SQL_CHILDREN,
                         [FLinkedTableName, SourceTableFieldList('A'), FSourceTable.TableName,
                         FLinkedTableName, lPrimaryKey, AParentKeyField,
                          FLinkedTableName, lPrimaryKey, lPrimaryKey,
                          lPrimaryKey]);
        repeat
          RunQuery;
        until lNewRecCount = lOldRecCount;
      end;
      if TotalRecordsInDest <> lExpectedCount then
        lFailureDetected := true;
    finally
      SQL.Text := 'ALTER TABLE ' + FSourceTable.TableName + ' DROP COLUMN INCMark_ZZ';;
      ExecSQL;
    end;
    if lFailureDetected then
      ExamineWhyRecordsFailToInsert;
  end;
end;  // DoSQLInsertRecordsParent

{-------------------------------------------------------------------------------
  For hierarchical data import performance, a column (INCMARK_ZZ) is set to
     true for records that must be included in the export.
}
procedure TDBMerger.SetInclusionMarks(const ATableName, APrimaryKey: string; AKeysIncluded: TStringList);
var
  lPos: integer;
  lKeys: string;
  i: integer;
const
  SET_SQL = 'UPDATE %s SET INCMark_ZZ=-1 WHERE %s IN (%s)';
  BATCH_SIZE=2000;
begin
  lPos := 0;
  while lPos<AKeysIncluded.Count do begin
    lKeys := '';
    for i := 0 to BATCH_SIZE-1 do begin
      AddToCommaSeparatedList(lKeys, AKeysIncluded[lPos]);
      Inc(lPos);
      if lPos >= AKeysIncluded.Count then break; // from for loop
    end;
    with FInsertQuery do begin
      SQL.Text := Format(SET_SQL, [ATableName, APrimaryKey, lKeys]);
      ExecSQL;
    end; // with
  end;
end;

//==============================================================================
{ If an insert statement results in not enough records being added to the
    table, then we need to locate the records which weren't added and report on
    them.  Parameters are the first and last item in FKeysToBulkInsert which
    we tried to insert, therefore defining the range of keys we tried }
procedure TDBMerger.ExamineWhyRecordsFailToInsert;
var
  lCurrentRecord : TSingleRecord;  // to facilitate scan for deliberately rejected records
  lPrimaryKey : TPrimaryKey;
  lErrors : String;
  i : Integer;
begin
  lPrimaryKey := dmDatabase.SplitPrimaryKey(FSourcetable.TableName);
  with FInsertQuery do begin
    SQL.Clear;
    // Find all records missing from main DB, heavily filtered query
    SQL.Text := Format('SELECT Src.* FROM %s Src ' +
                       'LEFT JOIN %s Dest ON Dest.%s = Src.%s',
                       [FSourceTable.TableName, FLinkedTableName, lPrimaryKey.Key1,
                        lPrimaryKey.Key1]);
    if lPrimaryKey.Key2 <> '' then
      SQL.Add(Format('AND Dest.%s = Src.%s', [lPrimaryKey.Key2, lPrimaryKey.Key2]));
    SQL.Add(Format('WHERE Dest.%s IS NULL', [lPrimaryKey.Key1]));
    Open;
  end;
  lCurrentRecord.TableName := FSourceTable.TableName;

  while not FInsertQuery.Eof do begin
    lCurrentRecord.Key1 := FInsertQuery.FieldByName(lPrimaryKey.Key1).AsString;
    if lPrimaryKey.Key2 <> '' then
      lCurrentRecord.Key2 := FInsertQuery.FieldByName(lPrimaryKey.Key2).AsString
    else
      lPrimaryKey.Key2 := '';
    { Check for records we genuinely tried to transfer }
    if not ExcludedRecord(lCurrentRecord) then
      with FDestTable do begin
        { Try to copy failed records from temp database to main database }
        try
          SQL.Text := 'SELECT * FROM ' + FSourceTable.TableName +   // the temp import db
                      ' WHERE ' + lPrimaryKey.Key1
                      + '=''' + lCurrentRecord.Key1 + ''''; // record currently selected
          if lPrimaryKey.Key2<>'' then
            SQL.Add(' AND ' + lPrimaryKey.Key2 + '=''' + lCurrentRecord.Key2 + '''');
          Open;
          Properties['Update Criteria'].Value := adCriteriaKey;
          try
            if Eof then Append
                   else Edit;
            CopyRecordToRecord(FInsertQuery, FDestTable, '', '');
            Post;
          finally
            // catch errors before the Close cleans them out
            lErrors := '';
            for i := 0 to FDestTable.Connection.Errors.Count - 1 do begin
              // Check for the "Row cannot be located for updating" so-called error.
              // It doesn't mean the record isn't there though!!!!!
              if FDestTable.Connection.Errors[i].Number = -2147217864 then begin
                // Check whether the record is REALLY NOT there before logging an error.
                with dmDatabase.ExecuteSQL(FDestTable.SQL.Text, True) do begin
                  if Eof then
                    lErrors := lErrors
                        + FDestTable.Connection.Errors[i].Description
                        + '(' + IntToStr(FDestTable.Connection.Errors[i].Number) + ') ';
                  Close;
                end;
              end else
                lErrors := lErrors
                    + FDestTable.Connection.Errors[i].Description
                    + '(' + IntToStr(FDestTable.Connection.Errors[i].Number) + ') ';
            end;
            Close;
          end; // try
        except
          on E: Exception do begin //Catch all Database Errors
            if lErrors <> '' then
              LogRejection(FSourceTable.TableName, lCurrentRecord.Key1, lErrors);
          end;
        end; // try...except
      end;
    FInsertQuery.next; // Move to next failed item
  end; // While not EOF do begin
end;  // ExamineWhyRecordsFailToInsert


{-------------------------------------------------------------------------------
  Description : Record a rejection exception into the log file.  Connection is
              used to read off the errors
  Created : 25/03/2003 }
procedure TDBMerger.LogRejection(const iTable, iKey, AError: string);
begin
  WriteToLog('');
  WriteToLog(Format(ResStr_FailedToInsert, [iKey, iTable]));
  WriteToLog(ResStr_DatabaseErrorMessage + ': ');
  WriteToLog(AError);
  if (FExclusionList <> nil) and (FExclusionList.Count > 0) then
    WriteToLog(ResStr_RejectedBecauseOfParent);
  WriteToLog('--------------------');
  Inc(FLogItemsCount);
end;

//==============================================================================
{ Returns true if a record should be excluded for validation purposes, or
     because data is rejected duplicate data }
function TDBMerger.DuplicateRecord(iSingleRecord : TSingleRecord): boolean;
begin
  Result := False; // Default
  if FDuplicateList <> nil then
    Result := FDuplicateList.IndexOf(UpperCase(iSingleRecord.TableName) + ';' +
                                     iSingleRecord.Key1 + ';' +
                                     iSingleRecord.Key2) <> -1;
end;  // DuplicateRecord

//==============================================================================
{ Returns true if a record should be excluded for validation purposes, or
     because data is rejected duplicate data }
function TDBMerger.ExcludedRecord(iSingleRecord : TSingleRecord): boolean;
begin
  Result := False; // Default
  if FExclusionList <> nil then
    Result := FExclusionList.IndexOf(UpperCase(iSingleRecord.TableName) + ';' +
                                     iSingleRecord.Key1 + ';' +
                                     iSingleRecord.Key2) <> -1;
end;  // ExcludedRecord

//==============================================================================
{ When appending to a table in the main DB, we use a query to copy the record
      across rather than appending to a dataset and copying data field by field.
      This avoids problems with the DBI_NO_CURRREC error.
      Copies a single record (for 2 field primary keys) rather than doing
      several in a block. }
procedure TDBMerger.InsertUsingSQL(iPrimaryKey: TPrimaryKey);
var
  lDelimiter: string;
begin
  { FInsertQuery is set to read data from the import database }
  with FInsertQuery do
  begin
    SQL.Clear;
    SQL.Add('INSERT INTO ' + FLinkedTableName);    // Table in main DB
    SQL.Add('SELECT * FROM ' + FSourceTable.TableName); // Table in import db
    SQL.Add('WHERE ' + iPrimaryKey.Key1 + '="' +
                       FSourceTable.FieldByName(iPrimaryKey.Key1).AsString + '"');
    // Special case as Lineage_ID is not a string key
    if iPrimaryKey.Key2 <> '' then begin
      if CompareText(iPrimaryKey.Key2, 'Lineage_ID')=0 then
        lDelimiter := ''
      else
        lDelimiter := '''';
     SQL.Add('AND ' + iPrimaryKey.Key2 + '=' + lDelimiter +
                      FSourceTable.FieldByName(iPrimaryKey.Key2).AsString + lDelimiter);
    end;
    ExecSQL;
    if RowsAffected = 0 then
      raise EDBMergerError.Create(ResStr_InsertFail);
  end; // with FInsertQuery
end;

//==============================================================================
{ Locates a record in the destination database to match the one we are going to
     copy from.  If it exists go into edit mode, otherwise return false.  The
     dest table is always set to the correct table. }
function TDBMerger.UpdateDuplicateRecord(iRecord: TSingleRecord; iPrimaryKey: TPrimaryKey): boolean;
var
  lErrors : string;
  i : integer;
  lKey2: string;
begin
  Result := false;
  if Assigned(FDuplicateList) then
    // if its definetely not a duplicate
    if not DuplicateRecord(iRecord) then
      Exit;
  if FDestTable.Active then FDestTable.Close;
  if CompareText(iRecord.TableName, 'Concept_Lineage')=0 then
    lKey2 := iRecord.Key2  // This is an integer field in the primary key, so no quotes
  else
    lKey2 := '''' + iRecord.Key2 + '''';
  FDestTable.SQL.Text := 'SELECT * FROM ' + FSourceTable.TableName +
                         ' WHERE ' + iPrimaryKey.Key1 + ' = ''' + iRecord.Key1 + ''' ';
  if iPrimaryKey.Key2 <> '' then
    FDestTable.SQL.Add('AND ' + iPrimaryKey.Key2 + ' = ' + lKey2);
  FDestTable.Open;
  // Set "Update Criteria" to adCriteriaKey for good behaviour during updates
  FDestTable.Properties['Update Criteria'].Value := adCriteriaKey;
  Result := not FDestTable.Eof;
  if Result then
    try
      FDestTable.Edit;
      CopyRecordToRecord(FSourceTable, FDestTable, iPrimaryKey.Key1, iPrimaryKey.Key2);
      FDestTable.Post;
    except
      on E:Exception do // add extra error info
      begin
        lErrors := '';
        if FDestTable.Connection.Errors.Count=0 then
          lErrors := E.Message
        else
          for i := 0 to FDestTable.Connection.Errors.Count-1 do
            lErrors := lErrors + FDestTable.Connection.Errors[i].Description;
        FDestTable.Cancel;
        LogRejection(FSourceTable.TableName,
                     FSourceTable.FieldByName(iPrimaryKey.Key1).AsString,
                     lErrors);
      end;
    end; // try..except
  FDestTable.Close;
end;  // UpdateDuplicateRecord

//==============================================================================
{ If site id is for our database, stores the greates key values in a list
    so we can update the last key table }
procedure TDBMerger.RememberKeyIfSiteIDOurs(const iTableName: String;
  const iKey: TKeyString);
var
  lKeyIndex: integer;
begin
  if Copy(iKey, 1, 8)=SiteID then
  begin
    lKeyIndex :=  FCreatedKeys.IndexOfName(iTableName);
    if lKeyIndex=-1 then
      FCreatedKeys.Add(iTableName + '=' + Copy(iKey, 9, 8))
    else
      if MaxKey(Copy(iKey, 9, 8), FCreatedKeys.Values[iTableName]) = cpFirst then
        { Update stored key if it is greater }
        FCreatedKeys.Values[iTableName] := Copy(iKey, 9, 8);
  end;
end;

//==============================================================================
{ Remove unwanted occurrences.  During an import, it is possible to receive
    valid occurrences with invalid determinations.  These are 'hanging' and
    cause problems when deleting records in the record }
function TDBMerger.RemoveUnwantedOccurrences: Integer;
var
  lRs: _Recordset;
begin
  SetStatus(ResStr_RemovingInvalidRec);
  // Remove Occurrence Data records first, or Occurrence records deletion will fail.
  lRs := dmDatabase.ExecuteSQL('EXEC usp_RemoveUnwantedOccurrences', true);
  with lRs do
    while not EOF do begin
      LogRejection(Fields['TableName'].Value, Fields['ItemKey'].Value,
          ResStr_OccurrenceRejectedNoDetermination);
      MoveNext;
    end;
  Result := lRS.RecordCount;
end;

//==============================================================================
{ Default implementation of SetProgress.  Not abstract so that the user does
    not have to implement it if they don't want to }
procedure TDBMerger.SetProgress(iRecCount: integer);
begin
  if FTotalRecords <> 0 then
    FSetProgress(iRecCount * 100 div FTotalRecords);
end;

//==============================================================================
{ Default implementation of SetStatus.  Not abstract so that the user does
    not have to implement it if they don't want to }
procedure TDBMerger.SetStatus(const iText: string);
begin
  FSetStatus(iText);
end;

//==============================================================================
{ UpdateLastKeys - because we 'grab' last key information from the key
     generator, then use this number as a starting point to generate new Keys
     inside the TempData unit.  This means the Last_Key table is not updated
     to reflect newly imported data.  This method writes the info back into the
     main DB's Last_key table }
procedure TDBMerger.UpdateLastKeys;
var
  ltblUpdateLastKey: TADOTable;
  i: integer;
  lKeyString: TKeyString;
begin
  SetStatus('Updating keys...');
  ltblUpdateLastKey := TADOTable.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([ltblUpdateLastKey]);
    with ltblUpdateLastKey do
    begin
      TableName := 'LAST_KEY';
      if FCreatedKeys.Count>0 then
        Open;
      for i := 0 to FCreatedKeys.Count-1 do
      begin
        lKeyString := FCreatedKeys.Values[FCreatedKeys.Names[i]];
        { Drop the location part from the key string }
        if Locate('TABLE_NAME', FCreatedKeys.Names[i], []) then // FindKey([FCreatedKeys.Names[i]]) then
        begin
          if MaxKey(lKeyString, FieldByName('LAST_KEY_TEXT').AsString)=cpFirst then
            Edit
          else
            Continue; // loop - last key counter already incremented far enough
        end
        else
        begin
          Append;
          FieldByName('TABLE_NAME').AsString := Uppercase(FCreatedKeys.Names[i]);
        end;
        FieldByName('LAST_KEY_TEXT').AsString := lKeyString;
        Post;
      end; // for
    end; // with query
  finally
    ltblUpdateLastKey.Free;
  end; // try..finally
end;

//==============================================================================
procedure TDBMerger.WriteToLog(const ALine: String; AClearFile: Boolean = false);
var lFileName: String;
    lLogFile : TextFile;
begin
  lFileName := GetErrorFilePath + IMPORT_REJECTS_FILE;
  AssignFile(lLogFile, lFileName);
  if AClearFile or not FileExists(lFileName) then Rewrite(lLogFile)
                                             else Append(lLogFile);
  WriteLn(lLogFile, ALine);
  CloseFile(lLogFile);
  // If writing something other than an error to the file, don't update flag
  if not ACLearFile then FLoggedErrors := true;
end;  // WriteToLog

//==============================================================================
function TDBMerger.TotalRecordsInDest: Integer;
var
  i: integer;
begin
  with FDestTableCount do begin
    SQL.Text := 'SELECT Count(*) AS Total FROM ' + FLinkedTableName;
    try
      try
        Open;
      except
        on Exception do begin
          for i:=0 to FDestTableCount.Connection.Errors.Count do begin
            WriteToLog(FDestTableCount.Connection.Errors[i].Source);
            WriteToLog(FDestTableCount.Connection.Errors[i].Description);            
          end;
        end;
      end;
      Result := FieldByName('Total').AsInteger;
    finally
      Close;
    end;
  end;
end;

//==============================================================================
procedure TDBMerger.UpdateRebuildTaxonIndexFlag(const ATableName: String);
begin
  FRebuildTaxonIndex := False;
  Exit;

  if (CompareText(ATableName, 'TAXON') = 0) or
     (CompareText(ATableName, 'TAXON_VERSION') = 0) or
     (CompareText(ATableName, 'TAXON_LIST_ITEM') = 0) or
     (CompareText(ATableName, 'TAXON_LIST_VERSION') = 0) or
     (CompareText(ATableName, 'TAXON_LIST') = 0) or
     (CompareText(ATableName, 'TAXON_RANK') = 0) or
     (CompareText(ATableName, 'TAXON_USER_NAME') = 0) then FRebuildTaxonIndex := true;
end;

//==============================================================================
function TDBMerger.ErrorCanBeTrapped(Error: Error): Boolean;
begin
  Result := (Error.SQLState = IntToStr(JET_ERROR_ODBC_CALL_FAILED)) or
            (Error.SQLState = IntToStr(JET_ERROR_TRIED_TO_ASSIGN_NULL));
end;

{-------------------------------------------------------------------------------
  Detects a zero Changed_By field which should actually be null.  This can
    occur through some external methods of getting data into Recorder 2002
    which is invalid for Recorder 6.
}
function TDBMerger.IsZeroSmallDateField(AField: TField): Boolean;
begin
  Result := false;
  if SameText(AField.DisplayName, 'Changed_Date') or
      SameText(AField.DisplayName, 'Checked_Date') then
    Result := AField.AsDateTime=0;
end;

//==============================================================================
procedure TDBMerger.SetSiteID(const Value: string);
begin
  FSiteID := Value;
end;

function TDBMerger.GetErrorFilePath: string;

begin
  Result := ''; // default
  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      if OpenKeyReadOnly(REG_KEY_SETTINGS) then
      begin
        if ValueExists('Error Path') then
          Result := ReadString('Error Path');
        CloseKey;
      end;
    finally
      Free;
    end;
end;
end.

