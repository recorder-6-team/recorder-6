//==============================================================================
//  Unit:        DatabaseAccessADO
//
//  Implements:  TdmDatabase
//
//  Description: Implements database specific functionality. Takes care of
//               establishing and terminating connections to ADO databases.
//
//  Author:      Eric Salmon
//  Created:     12 February 2002
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 63 $
//    $Date: 7/06/10 12:37 $
//    $Author: Andrewkemp $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit DatabaseAccessADO;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  GeneralFunctions, ADODB, ExceptionForm, JNCCRelationships, ComObj, JRO_TLB,
  DatabaseUtilities, JnccDatasets, Constants, ADOX_TLB, AdoInt, Variants, BaseADODataModule;

resourcestring
  ResStr_ExistingExportDB = 'The export process requires the creation of a database called %s '+
     ' in your export folder.  However, a file with the same name already exists '+
     'which cannot be deleted.  Please remove this file and try again, or try again with a different '+
     'file name.';


type
  EJNCCDatabaseError = class(TExceptionPath);
  EInvalidSQLID = class(TExceptionPath);
  EMultiFieldKey = class(EJNCCDatabaseError);

  TDatabaseType = (dbUnknown, dbAccess, dbSQLServer, dbOracle);

  TDatabaseErrorType = (dbeRecordLocked, dbeReferentialIntegrity, dbeNoSuchTable,
                        dbeInvalidPassword, dbeDatabaseLocked, dbePrimaryKeyViolation,
                        dbeOther);

  // Record to hold field names of a primary key
  TPrimaryKey = record
    Key1: String;
    Key2: String;
  end;

  TdmDatabase = class(TdmBaseADO)
  private
    FServerName  : String;
    FDatabaseName: String;
    FTrustedSecurity : boolean;
    FRelationships: TJNCCRelationshipList;
    FDatabaseType : TDatabaseType;
    FPrimaryKeys : TStringList;
    FUserAccessLevel: TUserAccessLevel;
    FDisplayCommonNames: boolean;
    FSessionID: string;
    FUserID: string;
    FUsername: string;
    FPassword: string;
    procedure GetAllPrimaryKeys;
    function GetRelationships: TJNCCRelationshipList;
    procedure SetUserAccessLevel(const Value: TUserAccessLevel);
    function GetdbLocal: TADOConnection;
    procedure SetVagueDateNulls(var AParams: array of Variant);
    procedure SetDisplayCommonNames(const Value: boolean);
    procedure SetSessionID(const Value: string);
    procedure SetUserID(const Value: string);
  protected
    procedure SetupCommandParameters(AParameters: TParameters;
              AParams: Array of Variant); override;
  public
    constructor Create(AOwner:TComponent; const AServerName, ADBName: String;
                ATrustedSecurity : boolean; ASetApplicationRole: boolean=true;
                const AUsername: string=''; APassword: string=''); reintroduce;
    destructor Destroy; override;

    function CheckError(AException: Exception; const AErrorType: TDatabaseErrorType;
        AConnection : TADOConnection = nil): Boolean;
    procedure CloseDatabase;
    procedure CompactAccessDatabase(const ADatabaseName: String; ASetStatus: TSetStatusEvent);
    function ConnectToAccessDatabase(const ADBName: String): TADOConnection;
    function CreateImportExportDatabase(const ADBName: String): TADOConnection;
    function CreateImportExportTable(const AConnection: TADOConnection;
      const ATableName: String) : boolean;
    procedure CreateLinkedTable(ALocalConnection: TADOConnection;
      const ATableName, ALinkedTableName: String);
    procedure DoBackup;
    procedure DoRestore;
    function ExecuteSQL(const ASQLText: String; AReturnsRecords: Boolean = False;
      const AErrMsg: String = ''): _Recordset;
    function ExecuteSQLGetRowsAffected(const ASQLText: String;
      const AErrMsg: String = ''): Integer;
    function GetErrorMessage(const EMsg: String; const AErrorType: TDatabaseErrorType): String;
    function GetNewConnectionString: string;
    function GetPrimaryKey(const ATableName: String; const AWantMultiField: Boolean): String;
    procedure GetTableList(const AStrings: TStrings);
    function IsFieldRequired(const ATableName: String; AField: TField): Boolean;
    procedure OpenDatabase(ASetApplicationRole: boolean);
    procedure RebuildAccessTablePrimaryKey(ALocalConnection: TADOConnection;
      const ATableName: String);
    procedure RemoveLinkedTable(ALocalConnection: TADOConnection;
      const ALinkedTableName: String);
    procedure SetConnectionToMainDB(AConnection : TADOConnection);
    procedure SetDatabaseLocal(ADODatasets: array of TCustomADODataset);
    function SplitPrimaryKey(const ATableName: String): TPrimaryKey;
    procedure SetApplicationRole(Connection: _Connection); overload;
    procedure SetApplicationRole(Connection: TADOConnection); overload;
    function BackupDeviceAvailable: boolean;

    property DatabaseType: TDatabaseType read FDatabaseType write FDatabaseType;
    property dbLocal: TADOConnection read GetdbLocal;
    property LocalDatabase: TADOConnection read GetdbLocal;
    property Relationships: TJNCCRelationshipList read GetRelationships;
    property UserAccessLevel : TUserAccessLevel read FUserAccessLevel write SetUserAccessLevel;
    property DisplayCommonNames: boolean read FDisplayCommonNames write SetDisplayCommonNames;
    property UserID: string read FUserID write SetUserID;
    property SessionID: string read FSessionID write SetSessionID;
  end;

var
  dmDatabase: TdmDatabase;

//==============================================================================
implementation

{$R *.DFM}

uses
  OleDB;

const
  // Connection string for main SQL Server DB
  CONNECTION_STRING = 'Provider=SQLOLEDB.1;%sPersist Security Info=False;' +
                      'Data Source=%s;Initial Catalog=%s;OLE DB Services=-2';
  //OLE DB Services=-2 turns off connection pooling which can cause confusing errors
  //when sp_SetAppRole is used.
  INTEGRATED_SECURITY = 'Integrated Security=SSPI;';
  SQL_SECURITY        = 'User ID=%s;password=%s;';
  DEF_LOGIN_USERNAME  = 'NBNUser';
  DEF_LOGIN_PASSWORD  = 'NBNPassword';

  SQL_CHECK_BACKUPDEVICE = 'Select name from [master].[dbo].[sysdevices] where name=''NBNData_Backup''';
  SQL_BACKUP = 'Backup Database "%s" to "NBNData_Backup" with init';
  SQL_RESTORE = 'Restore Database "%s" from "NBNData_Backup"';

  // Connection string used to create linked tables between Access and SQL Server
  LINK_PROVIDER_STRING = 'ODBC;DRIVER={SQL Server};Server=%s;Database=%s;%s';
  LINK_SECURITY = 'UID=%s;PWD=%s';

  // Jet 4.0, allow use of ADOX to create linked tables between SQL Server and Access
  JET_4_0  = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=';

  // Database error codes
  RECORD_LOCKED_ERROR         = 32;
  REFERENTIAL_INTEGRITY_ERROR = 547; //changed for SQL server
  INVALID_PASSWORD_ERROR      = 18456;
  DATABASE_LOCKED_ERROR       = -1;
  PRIMARY_KEY_VIOLATION       = 2627;

  // Parameters automatically populated when setting up an ADO command object.
  PARAM_USER_ID           = '@UserID';
  PARAM_SESSION_ID        = '@SessionID';
  PARAM_SHORT_DATE_FORMAT = '@ShortDateFormat';
  PARAM_SHOW_COMMON_NAMES = '@ShowCommonNames';

resourcestring
  ResStr_CatalogCreate =  'An error occurred whilst creating an ADOX catalog.  ADOX not installed correctly.';
  ResStr_IntegrityErrorForInsert = 'Unable to insert this item as it references an item which does not exist.';
  ResStr_IntegrityErrorForUpdate = 'Unable to update this item as it references an item which does not exist.';
  ResStr_IntegrityErrorForDelete = 'Unable to delete this item as it is referenced by other records in the database.';
  ResStr_ErBackupPermission = 'Failed to backup database because you do not have permission.';
  ResStr_ErRestorePermission = 'Failed to restore database because you do not have permission.';
  ResStr_ErBackupDiscSpace =  'Failed to backup database because there is not enough disc space available.';
  ResStr_ErBackupDeviceCantOpen = 'The backup operation cannot complete because the backup device location can''t be opened.';

  ResStr_RestoreDBError = 'Failed to restore database because it is in use.' + #13#10 +
                          'Please ensure that all users are logged out of Recorder.  '+
                          'If all users are logged out, then it is possible that an addin is '+
                          'holding a database connection which cannot be dropped, preventing the '+
                          'restore operation.  Try uninstalling all unnecessary addins, restarting '+
                          'Recorder and trying again.';

  ResStr_ErBackupNoValidBackup =  'Failed to restore database because a valid backup could not be found.';
  ResStr_CompactingDB = 'Compacting Database...';



//==============================================================================
constructor TdmDatabase.Create(AOwner:TComponent; const AServerName, ADBName: String;
            ATrustedSecurity : boolean; ASetApplicationRole: boolean=true;
            const AUsername: string=''; APassword: string='');
begin
  inherited Create(AOwner);
  FServerName   := AServerName;
  FDatabaseName := ADBName;
  FTrustedSecurity := ATrustedSecurity;
  FUsername := AUsername;
  FPassword := APassword;
  FUserAccessLevel := ualReadOnly;  // default until we know better
  SetConnectionToMainDB(dbLocal);

  FDatabaseType := dbSQLServer;
  OpenDatabase(ASetApplicationRole);

  FPrimaryKeys := TStringList.Create;
  GetAllPrimaryKeys;
end;  // Create

//==============================================================================
destructor TdmDatabase.Destroy;
begin
  FPrimaryKeys.Free;
  FPrimaryKeys := nil;
  FRelationships.Free;
  FRelationships := nil;
  CloseDatabase;
  inherited;
end;

//==============================================================================
// Description : Sets the connection string to point to the main nbndata
// database
procedure TdmDatabase.SetConnectionToMainDB(AConnection: TADOConnection);
begin
  AConnection.ConnectionString := GetNewConnectionString;
end;

{===============================================================================
 Description : Returns a connection string for the main database
 Created : 15/1/2003}
function TdmDatabase.GetNewConnectionString: string;
var lstSecurityInfo: string;
begin
  if FTrustedSecurity then
    lstSecurityInfo := INTEGRATED_SECURITY
  else begin
    if FUsername='' then
      // use default NBNUser login
      lstSecurityInfo := Format(SQL_SECURITY, [DEF_LOGIN_USERNAME, DEF_LOGIN_PASSWORD])
    else
      lstSecurityInfo := Format(SQL_SECURITY, [FUsername, FPassword]);
  end;
  Result := Format(CONNECTION_STRING, [lstSecurityInfo,
                                  FServerName, FDatabaseName]);
end;  // GetNewConnectionString

//==============================================================================
procedure TdmDatabase.OpenDatabase(ASetApplicationRole: boolean);
begin
  dbLocal.Open;
  if ASetApplicationRole then
    SetApplicationRole(dbLocal.ConnectionObject);
end;  // OpenDatabase

{===============================================================================
 Description : Sets the SQL Server application role for a connection, as
             determined by the USER record.
             Takes a _Connection so this code can be called for Addin
             connections as well.
 Created : 2/1/2003 }
procedure TdmDatabase.SetApplicationRole(Connection: _Connection);
var
  lstAppRole, lstAppRolePwd : string;
  VarRecsAffected: OleVariant; // ignored value
begin
  // find appropriate access level
  if UserAccessLevel = ualReadOnly then begin
    lstAppRole := 'R2k_ReadOnly';
    lstAppRolePwd := '1lqm4jozq';
  end
  else if UserAccessLevel = ualRecorder then begin
    lstAppRole := 'R2k_RecordCardsOnly';
    lstAppRolePwd := 'e095nf73d';
  end
  else if UserAccessLevel = ualAddOnly then begin
    lstAppRole := 'R2k_AddOnly';
    lstAppRolePwd := '9gjr74mc8';
  end
  else if UserAccessLevel = ualFullUser then begin
    lstAppRole := 'R2k_FullEdit';
    lstAppRolePwd := 'd93nmc741';
  end
  else if UserAccessLevel = ualAdmin then begin
    lstAppRole := 'R2k_Administrator';
    lstAppRolePwd := '4mzodgft7';
  end
  else
    //
    Exit;
  Connection.Execute('EXEC sp_setapprole ''' + lstAppRole + ''', ''' + lstAppRolePwd + '''',
              VarRecsAffected, adCmdText+adExecuteNoRecords);
end;  // SetApplicationRole

{===============================================================================
 Description : As SetApplicationRole(_Connection) but takes a TADOConnection
 Created : 15/1/2003 }
procedure TdmDatabase.SetApplicationRole(Connection: TADOConnection);
begin
  SetApplicationRole(Connection.ConnectionObject);
end;

//==============================================================================
procedure TdmDatabase.CloseDatabase;
begin
  if dbLocal.Connected then dbLocal.Close;
end;  // CloseDatabase

//==============================================================================
procedure TdmDatabase.GetTableList(const AStrings: TStrings);
begin
  dbLocal.GetTableNames(AStrings, false);
end;  // GetTableList

//==============================================================================
procedure TdmDatabase.SetDatabaseLocal(ADODatasets: array of TCustomADODataset);
var liIdx: Integer;
begin
  for liIdx := 0 to High(ADODatasets) do
    ADODatasets[liIdx].Connection := dbLocal;
end;

//==============================================================================
procedure TdmDatabase.GetAllPrimaryKeys;
var ladoDsSchema    : TADODataset;
    lstPrevTableName: String;
    lstFields       : String;
begin
  ladoDsSchema := TADODataset.Create(nil);
  try
    dbLocal.OpenSchema(siPrimaryKeys, EmptyParam, EmptyParam, ladoDsSchema);
    // Order by Table_Name AND Ordinal value.  Fields for multiple-fields keys are not
    // necessarily returned in order.
    ladoDsSchema.Sort := 'TABLE_NAME, ORDINAL';
    ladoDsSchema.First;
    lstPrevTableName := '';
    while not ladoDsSchema.Eof do begin
      if (ladoDsSchema.Fields[2].AsString <> lstPrevTableName) then begin
        // If still processing first table, don't do anything
        // Otherwise, save result in stringlist as TableName=PrimaryKeyFields for later retrieval
        if lstPrevTableName <> '' then FPrimaryKeys.Add(lstPrevTableName + '=' + lstFields);
        // And init the next one
        lstPrevTableName := ladoDsSchema.Fields[2].AsString;
        lstFields        := ladoDsSchema.Fields[3].AsString;
      end else
        // Multiple-fields key
        lstFields := lstFields + ';' + ladoDsSchema.Fields[3].AsString;
      ladoDsSchema.Next;
    end;
    // Need to save the last one
    FPrimaryKeys.Add(lstPrevTableName + '=' + lstFields);
  finally
    ladoDsSchema.Free;
  end;
end;  // GetAllPrimaryKeys

//==============================================================================
function TdmDatabase.GetPrimaryKey(const ATableName: String; const AWantMultiField: Boolean): String;
begin
  Result := FPrimaryKeys.Values[ATableName];
  { Raise an exception if the key is multi-field, depending on the option AWantMultiField }
  if (Pos(';', Result) <> 0) and not AWantMultiField then
    Raise EMultiFieldKey.Create(ResStr_MultipleFieldKey + ATableName);
end;  // GetPrimaryKey

//==============================================================================
function TdmDatabase.SplitPrimaryKey(const ATableName: String): TPrimaryKey;
begin
  // More likely to get a table with only one field for primary key
  Result.Key1 := FPrimaryKeys.Values[ATableName];
  Result.Key2 := '';
  // But if we have two...
  if (Pos(';', Result.Key1) > 0) then begin
    Result.Key2 := Copy(Result.Key1, Pos(';', Result.Key1) + 1, 255);
    Result.Key1 := Copy(Result.Key1, 1, Pos(';', Result.Key1) - 1);
  end;
end;  // SplitPrimaryKey

//==============================================================================
function TdmDatabase.GetRelationships: TJNCCRelationshipList;
begin
  // Create for first time use!
  if not assigned(FRelationships) then
    FRelationships := TJNCCRelationshipList.Create(dbLocal);
  Result := FRelationships;
end;  // GetRelationships

//==============================================================================
function TdmDatabase.IsFieldRequired(const ATableName: String; AField: TField): Boolean;
begin
  Result := AField.Required;
end;  // CheckFieldRequired

{-------------------------------------------------------------------------------
  Description : Checks if the current ADO errors are the one expected.
                Ignore AConnection param to use dbLocal's connection, otherwise
                explicitly pass the connection.
  Created : 09/05/2003 }
function TdmDatabase.CheckError(AException: Exception; const AErrorType:
    TDatabaseErrorType; AConnection : TADOConnection = nil): Boolean;
var lNativeErrorCode, lNumberErrorCode: Integer;
    lConnection : TADOConnection;
begin
  Result := false;
  if Assigned(AConnection) then
    lConnection := AConnection
  else
    lConnection := dbLocal;
  if (AException is EOleException) or (AException is EDatabaseError) and
     (lConnection.Errors.Count>0) then begin
    // for some errors, we match on the Jet number
    lNativeErrorCode := lConnection.Errors[0].NativeError;
    // for some errors, we match on the OLE number
    lNumberErrorCode := lConnection.Errors[0].Number;
    case AErrorType of
      dbeRecordLocked         : Result := lNativeErrorCode = RECORD_LOCKED_ERROR;
      dbeReferentialIntegrity : Result := lNativeErrorCode = REFERENTIAL_INTEGRITY_ERROR;
      dbeNoSuchTable          : Result := lNumberErrorCode = DB_E_NOTABLE;
      dbeInvalidPassword      : Result := lNativeErrorCode = INVALID_PASSWORD_ERROR;
      dbeDatabaseLocked       : Result := false;
      dbePrimaryKeyViolation  : Result := lNativeErrorCode = PRIMARY_KEY_VIOLATION;
      dbeOther                : Result := lNativeErrorCode <> 0;
    end;
  end;
end;  // CheckError

//==============================================================================
function TdmDatabase.GetErrorMessage(const EMsg: String;
  const AErrorType: TDatabaseErrorType): String;
begin
  case AErrorType of
    dbeRecordLocked :
        Result := '';
    dbeReferentialIntegrity :
        Result := '';
    dbeNoSuchTable :
        Result := '';
    dbeInvalidPassword :
        Result := '';
    dbeDatabaseLocked :
        Result := '';
    dbeOther :
        Result := '';
  end;
end;  // GetErrorMessage

//==============================================================================
// Create and connect to an Access database.
function TdmDatabase.CreateImportExportDatabase(const ADBName: String): TADOConnection;
var lCatalog: _Catalog;
begin
  try
    // If the database already exists, delete it. Otherwise, can't create new one
    if FileExists(ADBName) then
      if not DeleteFile(ADBName) then
        raise EJNCCDatabaseError.CreateNonCritical(Format(ResStr_ExistingExportDB,
            [ADBName]));
    // Create the database on disk.
    try
      lCatalog := CoCatalog.Create;
    except
      on E:Exception do
        raise EJNCCDatabaseError.Create(ResStr_CatalogCreate, E);
    end;
    // Use Jet 3.51 to create Access 97 compatible databases
    lCatalog.Create(JET_4_0 + ADBName);
    Result := ConnectToAccessDatabase(ADBName);
  except
    Raise;
  end;
end;  // CreateImportExportDatabase

//------------------------------------------------------------------------------
// Connect to an existing Access database.
function TdmDatabase.ConnectToAccessDatabase(const ADBName: String): TADOConnection;
begin
  try
    // Create and return ADO connection to target database
    // Jet 4.0 to be able to create linked tables
    Result := TADOConnection.Create(nil);
    Result.ConnectionString := JET_4_0 + ADBName;
    Result.LoginPrompt := false;
  except
    Raise;
  end;
end;  // ConnectToAccessDatabase


{-------------------------------------------------------------------------------
  Description : Creates an Access table on the connection using the table's
              SQL Server structure as a template.
              Ignores tables that already exist, and returns false.
}
function TdmDatabase.CreateImportExportTable(const AConnection: TADOConnection;
  const ATableName: String) : boolean;
var lDataset    : TADODataset;
    lQuery      : TADOQuery;
    lSQL        : String;
    lDefaultString: String;
    lPrimaryKey : TPrimaryKey;
    lCursor     : TCursor;
    lslTables   : TStringList;
    lTableExists: Boolean;
  //----------------------------------------------------------------------------
  function GetAccessDataType(const SQLServerDataType: string; APrecisionValue: Integer): String;
  begin
    Result := '';
    // Convert/match SQL Server data types to Access data types
    // char fields > 255 characters must be memos in access
    // int fields might have identity in the type name
    if (Pos('char', SQLServerDataType) > 0) and (APrecisionValue < 256) then
                                                        Result := ' Text (' + IntToStr(APrecisionValue) + ')'
    else if (SQLServerDataType = 'text') or (Pos('char', SQLServerDataType) > 0) then
                                                        Result := ' Memo'
    else if Pos('datetime', SQLServerDataType) > 0 then Result := ' Datetime'
    else if SQLServerDataType = 'bit' then              Result := ' YesNo'
    else if SQLServerDataType = 'float' then            Result := ' Double'
    else if SQLServerDataType = 'tinyint' then          Result := ' Byte'
    else if SQLServerDataType = 'smallint' then         Result := ' Short'
    else if SQLServerDataType = 'money' then            Result := ' Currency'
    else if Copy(SQLServerDataType, 1, 3) = 'int' then  Result := ' Long'
    else if SQLServerDataType = 'image' then            Result := ' OleObject'
    else if SQLServerDataType = 'timestamp' then        Result := ' Binary (8)';
  end;  // GetAccessDataType
  //----------------------------------------------------------------------------
begin
  // Try to locate the table in the target database
  lslTables := TStringList.Create;
  try
    AConnection.GetTableNames(lslTables);
    lTableExists := lslTables.IndexOf(ATableName) > -1;
  finally
    lslTables.Free;
  end;
  Result := False;
  // Table already exists, nothing else to do here
  if lTableExists then Exit;
  Result := True;
  // Table doesn't exist, create it
  lCursor := HourglassCursor;
  try
    lDataset := TADODataset.Create(nil);
    with lDataset do
      try
        // Get all the columns for the table from SQL Server
        Connection := Self.Connection;
        CommandType := cmdText;
        // Use system stored proc to get info required
        CommandText := 'exec sp_columns ''' + ATableName + '''';
        Open;

        lSQL := 'CREATE TABLE [' + ATableName + '] (';
        while not Eof do begin
          // Wrap column name in [..], due to reserved words, like COMMENT/TABLE/USER
          // that someone very clever decided to use for column names...
          lSQL := lSQL +
                  '[' + FieldByName('Column_Name').AsString + ']' +
                  GetAccessDataType(FieldByName('Type_Name').AsString,
                                    FieldByName('Precision').AsInteger);
                  
          // Default value comes back surrounded with (), and Access doesn't like that very much!
          if not FieldByName('Column_Def').IsNull then begin
            lDefaultString := Copy(FieldByName('Column_Def').AsString, 2, Length(FieldByName('Column_Def').AsString) - 2);
            // Change default date to something Access understands
            if lDefaultString = 'getdate()' then
              lSQL := lSQL + ' DEFAULT NOW'
            else begin
              // Negative number are surrounded with another set of (), get rid of that too
              if (lDefaultString[1] = '(') and (lDefaultString[Length(lDefaultString)] = ')') then
                lDefaultString := Copy(lDefaultString, 2, Length(lDefaultString) - 2);
              lSQL := lSQL + ' DEFAULT ' + lDefaultString;
            end;
          end;
          // Get next column description
          Next;
          if not Eof then lSQL := lSQL + ', ';
        end;
        Close;
      finally
        Free;
      end;

    // Add primary key constraint
    lPrimaryKey := SplitPrimaryKey(ATableName);
    if lPrimaryKey.Key1 <> '' then begin
      lSQL := lSQL + ', CONSTRAINT PrimaryKey PRIMARY KEY ([' + lPrimaryKey.Key1;
      // If multi-key, deal with that too.
      if lPrimaryKey.Key2 <> '' then lSQL := lSQL + '], [' + lPrimaryKey.Key2;
      lSQL := lSQL + '])';
    end;
    lSQL := lSQL + ')';

    // Create table in Access database
    lQuery := TADOQuery.Create(nil);
    try
      lQuery.Connection := AConnection;
      lQuery.SQL.Text := lSQL;

      if FileExists(IncludeTrailingPathDelimiter(GetWindowsTempDir) + 'CreateTable_' + ATableName + '.sql') then
        DeleteFile(IncludeTrailingPathDelimiter(GetWindowsTempDir) + 'CreateTable_' + ATableName + '.sql');
      lQuery.SQL.SaveToFile(IncludeTrailingPathDelimiter(GetWindowsTempDir) + 'CreateTable_' + ATableName + '.sql');

      lQuery.ExecSQL;
    finally
      lQuery.Free;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // CreateImportExportTable

//==============================================================================
// ALocalConnection is a connection to the database the linked tables will be
// create in.
procedure TdmDatabase.CreateLinkedTable(ALocalConnection: TADOConnection;
  const ATableName, ALinkedTableName: String);
var lCatalog  : _Catalog;
    lLinkTable: _Table;
    lstSecurity : string;
begin
  try
    lCatalog := CoCatalog.Create;
  except
    on E:Exception do
      raise EJNCCDatabaseError.Create(ResStr_CatalogCreate, E);
  end;
  lCatalog.Set_ActiveConnection(ALocalConnection.ConnectionObject);
  try
    lLinkTable := lCatalog.Tables[ALinkedTableName];
    // If no exceptions raised, next line will occur, otherwise, table not there
    // and needs to be created
    Exit;
  except
    on E:EOleException do
      if E.ErrorCode <> HRESULT($800A0000) + adErrItemNotFound then raise;
  end;

  lLinkTable := coTable.Create;
  with lLinkTable do begin
    ParentCatalog := lCatalog;
    Name := ALinkedTableName;
    if FTrustedSecurity then
      lstSecurity := 'Trusted_Connection=Yes'
    else
      lstSecurity := Format(LINK_SECURITY, [DEF_LOGIN_USERNAME, DEF_LOGIN_PASSWORD]);
    // Connection to remote database
    Properties['Jet OLEDB:Link Provider String'].Value := Format(LINK_PROVIDER_STRING,
                    [FServerName, FDatabaseName, lstSecurity]);
    // Table name on remote database
    Properties['Jet OLEDB:Remote Table Name'].Value := ATableName;
    Properties['Jet OLEDB:Create Link'].Value := True;
  end;// With
  // Add linked table to database
  lCatalog.Tables.Append(lLinkTable);
end;  // CreateLinkedTable

//==============================================================================
// ALocalConnection is a connection to the database the linked tables will be
// removed from.
procedure TdmDatabase.RemoveLinkedTable(ALocalConnection: TADOConnection;
  const ALinkedTableName: String);
var lCatalog: _Catalog;
begin
  try
    try
      lCatalog := CoCatalog.Create;
    except
      on E:Exception do
        raise EJNCCDatabaseError.Create(ResStr_CatalogCreate, E);
    end;
    lCatalog.Set_ActiveConnection(ALocalConnection.ConnectionObject);
    // Remove table from database
    lCatalog.Tables.Delete(ALinkedTableName);
  except
    on E:EOleException do
      if E.ErrorCode <> HRESULT($800A0000) + adErrItemNotFound then raise;
  end;
end;  // RemoveLinkedTable

//==============================================================================
procedure TdmDatabase.RebuildAccessTablePrimaryKey(ALocalConnection: TADOConnection;
  const ATableName: String);
var lCatalog   : _Catalog;
    lTable     : _Table;
    lKey       : _Key;
    lPrimaryKey: TPrimaryKey;
    i          : Integer;
begin
  try
    lCatalog := CoCatalog.Create;
  except
    on E:Exception do
      raise EJNCCDatabaseError.Create(ResStr_CatalogCreate, E);
  end;
  lCatalog.Set_ActiveConnection(ALocalConnection.ConnectionObject);
  lTable := lCatalog.Tables[ATableName];

  // Get fields for key
  lPrimaryKey := SplitPrimaryKey(ATableName);
  // Prepare new Key
  lKey := CoKey.Create;
  with lKey do begin
    Name := 'PrimaryKey';
    Columns.Append(lPrimaryKey.Key1, lTable.Columns[lPrimaryKey.Key1].Type_, 16);
    if lPrimaryKey.Key2 <> '' then
      Columns.Append(lPrimaryKey.Key2, lTable.Columns[lPrimaryKey.Key2].Type_, 16);
    RelatedTable := '';
  end;

  with lTable.Keys do begin
    // Remove any primary key first
    for i := 0 to Count - 1 do
      if Item[i].Type_ = adKeyPrimary then begin
        Delete(Item[i].Name);
        Break;
      end;
    // Add new key now
    Append(lKey, adKeyPrimary, EmptyParam, Unassigned, Unassigned)
  end;
end;  // RebuildAccessTablePrimaryKey

//==============================================================================
// Description : Accessor method
// Created : 02-01-2003
procedure TdmDatabase.SetUserAccessLevel(const Value: TUserAccessLevel);
begin
  FUserAccessLevel := Value;
  // reconnect using new app role
  CloseDatabase;
  SetConnectionToMainDB(dbLocal);
  OpenDatabase(true);
end;

{===============================================================================
 Description : Checks if the NBNData_Backup device is present.
 Created : 15/1/2003 }
function TdmDatabase.BackupDeviceAvailable: boolean;
var
  lQuery: TADOQuery;
begin
  lQuery := TADOQuery.Create(nil);
  try
    lQuery.Connection := dbLocal;
    lQuery.SQL.Text := SQL_CHECK_BACKUPDEVICE;
    lQuery.Open;
    Result := not lQuery.Eof;
  finally
    lQuery.Free;
  end;
end;  // BackupDeviceAvailable

{===============================================================================
 Description : Backup the database to the nbndata_backup device
 Created : 15/1/2003 }
procedure TdmDatabase.DoBackup;
var
  lCursor    : TCursor;
  lConnection: TADOConnection;
begin
  lCursor := HourglassCursor;
  // need a new connection to get backupoperator priveleges
  //(which are removed by the app role)
  lConnection := TADOConnection.Create(nil);
  try
    with lConnection do begin
      ConnectionTimeout := 0;
      CommandTimeout := 0;
      ConnectionString := GetNewConnectionString;
      LoginPrompt := False;
      Open;
      try
        Execute(Format(SQL_BACKUP, [FDatabaseName]));
      except
        on EOleException do
        begin
          Case Errors[0].NativeError of
            262: //Don't have permission
              raise EJNCCDatabaseError.CreateNonCritical(ResStr_ErBackupPermission);
            3201:
              raise EJNCCDatabaseError.CreateNonCritical(ResStr_ErBackupDeviceCantOpen);
            3203: //Hard disk out of space
              raise EJNCCDatabaseError.CreateNonCritical (ResStr_ErBackupDiscSpace);
            else begin
              // probably don't want to internationalise error message at this level.
              raise EJNCCDatabaseError.CreateNonCritical ('Unknown backup error: '+IntToStr(Errors[0].NativeError));
            end;
          end;//case
        end;//on EOleException do
      end;//try..except
    end;//with lConnection
  finally
    lConnection.Close;
    lConnection.Free;
    DefaultCursor(lCursor);
  end;
end;

{===============================================================================
 Description : restore the database from the nbndata_backup device
 Created : 15/1/2003 }
procedure TdmDatabase.DoRestore;
var
  lCursor    : TCursor;
  lConnection: TADOConnection;
begin
  lCursor := HourglassCursor;
  // need a new connection to get backupoperator priveleges
  //(which are removed by the app role)
  CloseDatabase;
  lConnection := TADOConnection.Create(nil);
  try
    with lConnection do begin
      // connect to master
      ConnectionTimeout := 0;
      ConnectionString := StringReplace(GetNewConnectionString, FDatabaseName,
                          'master', [rfReplaceAll]);
      LoginPrompt := False;
      CommandTimeout := 0;
      lConnection.Open;
      try
        Execute(Format(SQL_RESTORE, [FDatabaseName]));
      except
        on EOleException do
        begin
          Case lConnection.Errors[0].NativeError of
            3101: //Can't get Exclusive access
              raise EJNCCDatabaseError.CreateNonCritical(ResStr_RestoreDBError);
            3201: //Can't find the backup
              raise EJNCCDatabaseError.CreateNonCritical(ResStr_ErBackupNoValidBackup);
            3110: //Don't have permission
              raise EJNCCDatabaseError.CreateNonCritical(ResStr_ErRestorePermission);
            else
              raise;
          end;
        end;
      end;
    end;
  finally
    lConnection.Close;
    lConnection.Free;
    // restore security information (it is not persisted in the connection)
    SetConnectionToMainDB(dbLocal);
    OpenDatabase(true);
    DefaultCursor(lCursor);
  end;
end;

//==============================================================================
// Uses JRO (Jet and Replication Objects)
// JRO's CompactDatabase will also repair the database, according to MS.
procedure TdmDatabase.CompactAccessDatabase(const ADatabaseName: String;
  ASetStatus: TSetStatusEvent);
var JE         : JetEngine;  // Jet Engine
    lTempDBName: String;     // Temp database name
    lCursor    : TCursor;
begin
  lCursor := HourglassCursor;
  if Assigned(ASetStatus) then ASetStatus(ResStr_CompactingDB);
  try
    lTempDBName := ExtractFilePath(ADatabaseName) + 'TEMP_' + ExtractFileName(ADatabaseName);
    if FileExists(lTempDBName) then
      DeleteFile(lTempDBName);

    JE := CoJetEngine.Create;
    try
      try
        JE.CompactDatabase(JET_4_0 + ADatabaseName, JET_4_0 + lTempDBName);
        DeleteFile(ADatabaseName);
        RenameFile(lTempDBName, ADatabaseName);
      except
        on E:Exception do
          raise EJNCCDatabaseError.Create(E.Message, E);
      end;
    finally
      JE := nil;  // Free the object, i.e. lose the reference
    end;
  finally
    if Assigned(ASetStatus) then ASetStatus('');
    DefaultCursor(lCursor);
  end;
end;  // CompactAccessDatabase

{-------------------------------------------------------------------------------
}
function TdmDatabase.ExecuteSQL(const ASQLText: String; AReturnsRecords: Boolean = False;
  const AErrMsg: String = ''): _Recordset;
begin
  with TADOCommand.Create(nil) do begin
    try
      ParamCheck     := False;  // No param check, this method is for "straight" SQL.
      Connection     := LocalDatabase;
      CommandType    := cmdText;
      CommandTimeout := 0;
      CommandText    := ASQLText;
      if not AReturnsRecords then ExecuteOptions := [eoExecuteNoRecords];

      try
        Result := Execute;
      except
        on E:Exception do
          if CheckError(E, dbeReferentialIntegrity) then begin
            //Attempt to find out why this command is causing a referential integrity error
            if Pos('INSERT', UpperCase(ASQLText)) > 0 then
              raise EJnccDatabaseError.CreateNonCritical(ResStr_IntegrityErrorForInsert, E)
            else
            if Pos('UPDATE', UpperCase(ASQLText)) > 0 then
              raise EJnccDatabaseError.CreateNonCritical(ResStr_IntegrityErrorForUpdate, E)
            else
            if Pos('DELETE', UpperCase(ASQLText)) > 0 then
              raise EJnccDatabaseError.CreateNonCritical(ResStr_IntegrityErrorForDelete, E);
          end else
          if AErrMsg <> '' then
            raise EJnccDatabaseError.Create(AErrMsg, E)
          else
            raise;
      end;
    finally
      Free;
    end;
  end;
end;  // TdmGeneral.ExecuteSQL

{-------------------------------------------------------------------------------
  Runs a piece of SQL and returns the records affected.
}
function TdmDatabase.ExecuteSQLGetRowsAffected(const ASQLText,
  AErrMsg: String): Integer;
begin
  Result := 0;
  with TADOQuery.Create(nil) do begin
    try
      Connection     := LocalDatabase;
      CommandTimeout := 0;
      SQL.Text    := ASQLText;
      try
        ExecSQL;
        Result := RowsAffected;
      except
        on E:Exception do
          if CheckError(E, dbeReferentialIntegrity) then begin
            //Attempt to find out why this command is causing a referential integrity error
            if Pos('INSERT', UpperCase(ASQLText)) > 0 then
              raise EJnccDatabaseError.CreateNonCritical(ResStr_IntegrityErrorForInsert, E)
            else
            if Pos('UPDATE', UpperCase(ASQLText)) > 0 then
              raise EJnccDatabaseError.CreateNonCritical(ResStr_IntegrityErrorForUpdate, E)
            else
            if Pos('DELETE', UpperCase(ASQLText)) > 0 then
              raise EJnccDatabaseError.CreateNonCritical(ResStr_IntegrityErrorForDelete, E);
          end else
          if AErrMsg <> '' then
            raise EJnccDatabaseError.Create(AErrMsg, E)
          else
            raise;
      end;
    finally
      Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Accessor method since Recorder has historically referred to the conneciton as
      dbLocal
}
function TdmDatabase.GetdbLocal: TADOConnection;
begin
  Result := Connection;
end;

{-------------------------------------------------------------------------------
  Method to populate the parameters collection of a command object with the values given in
      the AParams array.
}
procedure TdmDatabase.SetupCommandParameters(AParameters: TParameters; AParams: Array of
    Variant);
var
  lParam: TParameter;
begin
  if AParameters.Count > 0 then begin
    // Deal with default parameters
    lParam := AParameters.FindParam(PARAM_SESSION_ID);
    if Assigned(lParam) then lParam.Value := FSessionID;
    lParam := AParameters.FindParam(PARAM_USER_ID);
    if Assigned(lParam) then lParam.Value := FUserID;
    lParam := AParameters.FindParam(PARAM_SHOW_COMMON_NAMES);
    if Assigned(lParam) then lParam.Value := DisplayCommonNames;
    lParam := AParameters.FindParam(PARAM_SHORT_DATE_FORMAT);
    if Assigned(lParam) then lParam.Value := ShortDateFormat;
    lParam := AParameters.FindParam(PARAM_RECORDS_AFFECTED);
    if Assigned(lParam) then lParam.Direction := pdOutput;
    SetVagueDateNulls(AParams);
    inherited;
  end;
end;  // TdmGeneral.SetupCommandParameters

{-------------------------------------------------------------------------------
  Scan parameters sent to stored procedures.  If vague date type parameters are found, then
      ensure that the fields that should be null are set correctly.  For example, for Y- type
      dates the End date field should be null.
}
procedure TdmDatabase.SetVagueDateNulls(var AParams: Array of Variant);
var
  lIdx: Integer;
  lParamPrefix: String;
  lType: String;

  // Search for a parameter, and null its associated value
  procedure NullParam(const AName: string);
  var
    lNullIdx: integer;
  begin
    lNullIdx := 0;
    while lNullIdx <= High(AParams) do begin
      if VarIsStr(AParams[lNullIdx]) then
        if CompareText(AName, AParams[lNullIdx])=0 then
          AParams[lNullIdx+1] := null;
      Inc(lNullIdx, 2);
    end;
  end;

begin
  lIdx := 0;
  while lIdx <= High(AParams) do begin
    if CompareText(Copy(AParams[lIdx], Length(AParams[lIdx])- 12, 13),
        'VagueDateType')=0 then begin
      lParamPrefix := Copy(AParams[lIdx], 1, Length(AParams[lIdx])-4);
      // Found a vague date parameter.  Ensure that fields that should be null are
      lType := VarToStr(AParams[lIdx+1]);
      if Length(lType) = 2 then begin
        if lType[1] = '-' then
          NullParam(lParamPrefix + 'Start')
        else if lType[2] = '-' then
          NullParam(lParamPrefix + 'End');
      end
      else if VarIsNull(lType) then begin
        NullParam(lParamPrefix + 'Start');
        NullParam(lParamPrefix + 'End');
      end
      else if (lType='U') or (lType='') then begin
        NullParam(lParamPrefix + 'Start');
        NullParam(lParamPrefix + 'End');
      end;
    end;
    Inc(lIdx, 2);
  end;
end;  // TdmDatabase.SetVagueDateNulls

{-------------------------------------------------------------------------------
  Accessor.  Dissociates this unit from AppSettings.
}
procedure TdmDatabase.SetDisplayCommonNames(const Value: boolean);
begin
  FDisplayCommonNames := Value;
end;

{-------------------------------------------------------------------------------
  Accessor.  Dissociates this unit from AppSettings.
}
procedure TdmDatabase.SetSessionID(const Value: string);
begin
  FSessionID := Value;
end;

{-------------------------------------------------------------------------------
  Accessor.  Dissociates this unit from AppSettings.
}
procedure TdmDatabase.SetUserID(const Value: string);
begin
  FUserID := Value;
end;

end.
