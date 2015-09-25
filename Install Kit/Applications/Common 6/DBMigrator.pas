//==============================================================================
//  Unit: DBMigrator
//
//  Implements: TDBMigrator
//
//  Description: Class that takes the existing NBN Access database and copies
//               the data to an equivalent database on the local SQL server.
//               After the data migration, linked tables are created in Access that
//               replace the original Access tables and link through to the corresponding
//               tables on the SQL server.
//               DateTime fields are converted to Integers to allow pre 1753 dates.
//
//  Requires: MSDTC to be running.
//
//  Author: Ben Collier
//  Created: 04/12/2002
//
//  Last Revision Details:
//    $Revision: 15 $
//    $Date: 6/03/09 11:50 $
//    $Author: Pauldavies $
//
//  $History: DBMigrator.pas $
//  
//  *****************  Version 15  *****************
//  User: Pauldavies   Date: 6/03/09    Time: 11:50
//  Updated in $/JNCC/Development/Install Kit/Applications/Common 6
//  Incident 18773
//  CCN 319
//  
//  Changed included database from MSDE to SQL Express.
//  
//  *****************  Version 14  *****************
//  User: Andrewkemp   Date: 23/02/09   Time: 14:44
//  Updated in $/JNCC/Development/Install Kit/Applications/Common 6
//  VI 18006 (CCN 262)
//  Enable ad-hoc distributed queries and xp_cmdshell for the duration of
//  the upgrade.
//  
//  *****************  Version 13  *****************
//  User: Johnvanbreda Date: 20/10/06   Time: 11:14
//  Updated in $/JNCC/Development/Install Kit/Applications/Common 6
//  Removed a debug setting
//  
//  *****************  Version 12  *****************
//  User: Johnvanbreda Date: 20/10/06   Time: 10:56
//  Updated in $/JNCC/Development/Install Kit/Applications/Common 6
//  IR12677
//  Map migration fix
//  
//  *****************  Version 11  *****************
//  User: Johnvanbreda Date: 26/09/05   Time: 10:32
//  Updated in $/JNCC/Development/Install Kit/Applications/Common 6
//  Handles Admin Areas for CD Master database creation
//  
//  *****************  Version 10  *****************
//  User: Johnvanbreda Date: 23/09/05   Time: 16:47
//  Updated in $/JNCC/Development/Install Kit/Applications/Common 6
//  Copes with system supplied data creation and new data model updates.
//  
//  *****************  Version 9  *****************
//  User: Johnvanbreda Date: 16/08/05   Time: 13:48
//  Updated in $/JNCC/Development/Install Kit/Applications/Common 6
//  AllowedErrors.txt can now be in System subfolder.
//  
//  *****************  Version 8  *****************
//  User: Johnvanbreda Date: 16/08/05   Time: 13:40
//  Updated in $/JNCC/Development/Install Kit/Applications/Common 6
//  Implemented AllowedErrors.txt file
//  
//  *****************  Version 7  *****************
//  User: Ericsalmon   Date: 3/02/05    Time: 13:39
//  Updated in $/JNCC/Development/Install Kit/Applications/Common 6
//  ID 8518. Added Index_Taxon_Name to the list of tables to ignore. This
//  one has no primary key.
//  
//  *****************  Version 6  *****************
//  User: Johnvanbreda Date: 20/12/04   Time: 17:51
//  Updated in $/JNCC/Development/Install Kit/Applications/Common 6
//  Fixed password removal on Access DB
//  
//  *****************  Version 5  *****************
//  User: Johnvanbreda Date: 27/08/04   Time: 12:48
//  Updated in $/JNCC/Development/Install Kit/Applications/Common 6
//  Works for system supplied install CD preparation.
//  
//  *****************  Version 4  *****************
//  User: Ericsalmon   Date: 19/05/04   Time: 16:33
//  Updated in $/JNCC/Development/Install Kit/Applications/Common 6
//  Fix.
//  
//  *****************  Version 3  *****************
//  User: Ericsalmon   Date: 17/05/04   Time: 16:19
//  Updated in $/JNCC/Development/Install Kit/Applications/Common 6
//  Fixed bug for Access password removal, to work with either DAO3.51 or
//  DAO3.60.
//  
//  *****************  Version 2  *****************
//  User: Ericsalmon   Date: 17/05/04   Time: 11:31
//  Updated in $/JNCC/Development/Install Kit/Applications/Common 6
//  Fixed connection problem on trusted login.
//  
//  *****************  Version 1  *****************
//  User: Ericsalmon   Date: 14/05/04   Time: 17:03
//  Created in $/JNCC/Development/Install Kit/Applications/Common 6
//  
//  *****************  Version 12  *****************
//  User: Johnvanbreda Date: 23/03/04   Time: 14:04
//  Updated in $/JNCC/Development/Install Kit/Applications/DB Plugin
//  Fixed so that new tables are not included in migration process
//  
//  *****************  Version 11  *****************
//  User: Johnvanbreda Date: 26/06/03   Time: 12:37
//  Updated in $/JNCC/Development/Install Kit/Applications/DB Plugin
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================

unit DBMigrator;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ADODB, Db, DBTables, OleServer, ADOX_TLB, Relationships_ADO,
  TablePriorityList_ADO, ComCtrls, Variants, DAOTools, ComObj,
  TransferQuery, ExceptionForm, DatabaseAccessADO, Logger;

const
  START_TABLE = 0;
  SINGLE_TABLE = '';
  UPGRADE_ERROR_FILE = 'UpgradeErrors.mdb';  

type
  TLabelType = (ltCurrentTable, ltRecordsCopied, ltRecordsOutOf, ltErrors);

  TSetLabelCallback = procedure (AType : TLabelType; const ACaption : string) of object;

  EDBMigrator = class(TExceptionPath);

  TDBMigrator = class
  private
    FIncSysSupplied: boolean; // do we want to migrate sys supplied data
    FTotalRecordsTransferred: integer;
    FTotalRecordsToTransfer : integer;
    FRecordCounts : array of integer;
    FLabelCallBack : TSetLabelCallback;
    FprgDataMigration: TProgressBar;
    FAccessDBName: string;
    FAccessCatalog: _Catalog;
    FconnAccessDB: TADOConnection;
    FstTableMappings: TStringList;
    FstMoveTableDataSQL: TStringList;
    FAccessTablesToMigrate : TStringList;
    FServerName : string; // sql server
    FTrustedConnection : boolean;
    FSaUsername : string;
    FSaPassword : string;
    FAccessErrorConnection : TADOConnection; // connection to UpgradeErrors if it exists
    FUpgradeErrorInsert : TADOQuery; // also only exists if there has been an error
    FErrorCount : integer;
    FqryAllPurpose : TADOQuery;
    FFirstLogEntry : boolean;
    FLastQuery : array[0..10] of string;
    FLastQueryTrack : integer;
    FSrvTempFolder : string; // folder created on SQL Server
    FDataFilesPath: String;
    FTablesNotMigrated: TStringList;
    FCancelled: Boolean;
    FAllowedErrorFile: TStringList;
    FIsSQLServer2005: Boolean;
    FAdvancedOptionsWereVisible: Boolean;
    FDistributedQueriesWereEnabled: Boolean;
    FCommandShellWasEnabled: Boolean;
    procedure FindServerTempFolder;
    procedure BulkTransferRecords(const ASQLTableName: string; const ASourceRecordCount: integer);
    procedure CreateLinkedTable(const istJetProviderString, istAccessTable, istSQLTableName: string);
    procedure GetAccessTablesToMigrate;
    procedure CreateUpgradeErrorDB(const ADBPath: string);
    function DoBulkInsert(ASQLTableName, ABcpFileName: string;
      ASourceRecordCount: integer): integer;
    procedure EnableTriggers(itfEnableTriggers: boolean);
    procedure ErrorInRecord(const AAccessTableName, APrimaryKey1,
      APrimaryKey2: string);
    function ExecSQL(const istSQL: string; itfUseSQLServer: boolean): integer;
    procedure GenerateMoveDataSQL(const ASQLTableName : string);
    function GetAccessTableName(const ASQLTableName: string): string;
    function GetRecordCount(const ASqlTableName: string; ADest: boolean): integer;
    procedure PreProcessTables(ATableList: TTablePriorityList);
    procedure GetCorrectConnection(ATrustedConnection: boolean; const ASaUsername, ASaPassword: 
        string);
    function GetSecurityString(JetConnection: boolean): string;
    function GetSqlServerTableName(const AAccessTableName: String): string;
    function GetTransferQuery(const ASQLTableName: string): TTransferQuery;
    procedure HandleError(const ATableIndex: integer; const ATableName: string;
      E : Exception);
    procedure IncrementErrorCount;
    procedure CheckSQLServerVersion;
    procedure ConfigureSQLServer;
    procedure ResetSQLServerConfiguration;
    function GetSQLServerOption(const OptionName: String): Boolean;
    procedure SetSQLServerOption(const OptionName: String; Value: Boolean);
    procedure MoveData;
    procedure PrepareTable(istTableName: string; itfBeforeExecute: boolean;
              var ARecordCount : integer);
    procedure SetTotalRecordsTransferred(const Value: integer);
    procedure TrackQuery(const ASQL: string);
    procedure TransferIndividualRecords(ASQLTableName: string);
    procedure TransferSiteID;

    property TotalRecordsTransferred: integer read FTotalRecordsTransferred write SetTotalRecordsTransferred;
    function GetBcpSecurity: string;
    function CreateCmdTableExists: TADOCommand;
    procedure HandleQueryErrors(const AAccessSelectSQL, AAccessDeleteSQL,
          AMainSQLBlock, ATableName : String; var ARecordCount : integer);
    procedure RunQueryAndTrackErrors(const AQuery: TADOQuery);
    procedure LogQueries;
    procedure LogConnectionErrors(const AConnection: TADOConnection);
    procedure DropParentConstraints;
    procedure CreateParentConstraints;
    procedure RunAndTrackQuery(const ASQL: string);
    function IsTruncationRequiredForSysSuppliedDb(const ATableName: string): boolean;
  public
    constructor Create(ALabelCallback : TSetLabelCallback; oprgDataMigration: TProgressBar;
      const AServerName, AAccessDBName, AAccessDBPassword: String; ATrustedConnection: Boolean;
      const ASaUsername, ASaPassword, ADataFilesPath: String;
      AIncSysSupplied: boolean=false); reintroduce;
    destructor Destroy; override;
    function Execute: boolean;
    procedure Cancel;
  end;

//==============================================================================
implementation

uses
  GeneralFunctions, VersionInfo;

const
  OPT_SHOW_ADVANCED_OPTS = 'show advanced options';
  OPT_DISTRIBUTED_QUERIES = 'Ad Hoc Distributed Queries';
  OPT_COMMAND_SHELL = 'xp_cmdshell';

  SQL_GET_PRODUCT_VERSION =
      'SELECT SERVERPROPERTY(''productversion'') AS Version';
  
  SQL_GET_SERVER_OPTION =
      'SELECT value FROM sys.configurations WHERE name = ''%s''';

  SQL_SET_SERVER_OPTION =
      'EXECUTE sp_configure ''%s'', %d';

  SQL_RECONFIGURE =
      'RECONFIGURE';
  
  SQL_ERROR_INSERT =
      'INSERT into [%s] SELECT * FROM [%s] in "%s" WHERE %s="%s"';

  SQL_BCP_OUT =
      'exec master..xp_cmdshell ''bcp NBNData..VW_IMPORT out "%s" %s -c -t*@*@ -r!@!@ -m50''';

  SQL_DELETE_BCP_FILE =
      'exec master..xp_cmdshell ''del "%s"''';

  SQL_BCP_IN =
      'BULK INSERT [%s] '+
              'FROM ''%s'' '+
              'WITH ( '+
              '  DATAFILETYPE=''char'', '+
              '  BATCHSIZE=%s, '+
              '  FIRSTROW=%s, '+
              '  MAXERRORS=0, '+
              '  FIELDTERMINATOR=''*@*@'', '+
              '  ROWTERMINATOR=''!@!@'', '+
              '  CHECK_CONSTRAINTS, '+
              '  TABLOCK '+
              ')';

  SQL_DROP_VIEW = 'IF EXISTS(SELECT * FROM sysobjects WHERE name=''VW_IMPORT'') '+
                                 'DROP VIEW VW_IMPORT';

  SQL_CREATE_BAD_TABLE='IF NOT EXISTS('+
                       '  SELECT * FROM tempdb.dbo.SysObjects'+
                       '  WHERE name LIKE ''#TempBadRecords%'''+
                       '    AND id =(SELECT OBJECT_ID(''tempdb.dbo.#TempBadRecords'')))'+
                       '    SELECT TOP 0 * INTO #TempBadRecords FROM VW_IMPORT';
  SQL_INDEX_BAD_TABLE='CREATE INDEX IX_TempLookup '+
                      'ON #TempBadRecords (%s)';

  SQL_DROP_BAD_TABLE='IF EXISTS('+
                       '          SELECT * FROM tempdb.dbo.SysObjects'+
                       '          WHERE name LIKE ''#TempBadRecords%'''+
                       '          AND id =(SELECT OBJECT_ID(''tempdb.dbo.#TempBadRecords'')))'+
                       '  DROP TABLE #TempBadRecords';

  SQL_DROP_PARENT_CONSTRAINTS=
               'if exists (select * from dbo.sysobjects where id = object_id(N'''+
               '[dbo].[%s]'') and OBJECTPROPERTY(id, N''IsForeignKey'') = 1) '+
               'ALTER TABLE [dbo].[%s] DROP CONSTRAINT %s';

  SQL_ADD_PARENT_CONSTRAINTS=
               'if not exists (select * from dbo.sysobjects where id = object_id(N'''+
               '[dbo].[%s]'') and OBJECTPROPERTY(id, N''IsForeignKey'') = 1) '+
               'ALTER TABLE %s'+
               '  ADD CONSTRAINT [%s] FOREIGN KEY'+
               '  ('+
               '    [%s]'+
               '  ) REFERENCES [%s] ('+
               '    [%s]'+
               '  )';

  BATCH_SIZE=500;

  TABLES_NOT_MIGRATED = 'EXPORT_FORMAT_LK,USABLE_TABLE,USABLE_FIELD,SPECIAL_XML_ELEMENT,'+
                        'ADMIN_AREA,ADMIN_AREA_SOURCES,ADMIN_RELATION,ADMIN_TYPE,'+
                        'DATABASE_RELATIONSHIP,PREFERRED_LINKS,REPORT_WHERE,REPORT_JOIN,'+
                        'REPORT_FIELD,REPORT_ATTRIBUTE,VW_REFERENCE_AUTHORS,VW_IMPORT,'+
                        'SETTING,Organisation_Department,Base_Map,Computer_Map,'+
                        'Survey_Event_Owner,Survey_Event_Owner_Type,INDEX_TAXON_NAME';

//==============================================================================
// Description: Create connections to both the Access and SQLExpress databases. Load up the
//              TableMappings ini file and sort it alphabetically
//
// Author: Ben Collier
// Created: 04/12/2002
//------------------------------------------------------------------------------
constructor TDBMigrator.Create(ALabelCallback : TSetLabelCallback;
  oprgDataMigration: TProgressBar; const AServerName, AAccessDBName, AAccessDBPassword: String;
  ATrustedConnection: Boolean; const ASaUsername, ASaPassword, ADataFilesPath: String;
  AIncSysSupplied: boolean=false);
var
  lDAOLink: TDAOLink;
begin
  inherited Create;
  FIncSysSupplied := AIncSysSupplied;
  FLabelCallback := ALabelCallback;
  FprgDataMigration := oprgDataMigration;
  FAccessDBName := AAccessDBName;
  FServerName := AServerName; // SQL Server name
  FTrustedConnection := ATrustedConnection;
  FSaUserName := ASaUsername;
  FSaPassword := ASaPassword;
  FDataFilesPath := ADataFilesPath;
  FFirstLogEntry := True;
  FAllowedErrorFile := TStringList.Create;
  if FileExists(ExtractFilePath(Application.Exename) + 'AllowedErrors.txt') then
    FAllowedErrorFile.LoadFromFile(ExtractFilePath(Application.Exename) + 'AllowedErrors.txt')
  else if FileExists(ExtractFilePath(Application.Exename) + 'System\AllowedErrors.txt') then
    FAllowedErrorFile.LoadFromFile(ExtractFilePath(Application.Exename) + 'System\AllowedErrors.txt');
  FAllowedErrorFile.Sorted := true;
  FTablesNotMigrated := TStringList.Create;
  with FTablesNotMigrated do begin
    Sorted := True;
    Duplicates := dupIgnore;
    // Start with some known tables.
    CommaText := TABLES_NOT_MIGRATED;
    // If doing the master copy of the DB for the install CD, include the admin areas
    if FIncSysSupplied then begin
      FTablesNotMigrated.Delete(FTablesNotMigrated.IndexOf('ADMIN_AREA'));
      FTablesNotMigrated.Delete(FTablesNotMigrated.IndexOf('ADMIN_AREA_SOURCES'));
      FTablesNotMigrated.Delete(FTablesNotMigrated.IndexOf('ADMIN_RELATION'));
      FTablesNotMigrated.Delete(FTablesNotMigrated.IndexOf('ADMIN_TYPE'));
    end;
  end;

  Log('Data migration started on ' + DateToStr(Now), False);
  Log('Dropping database password from Access database', False);

  lDAOLink := InitDAODatabase(AAccessDBName, AAccessDBPassword);
  // drop the password on the access database
  try
    lDAOLink.Database.NewPassword(AAccessDBPassword, #0);
  except
    on E:Exception do;
  end; // shouldn't matter
  Log('Checking Access tables are linked correctly', False);
  lDAOLink.Database.Close;
  lDAOLink.Database := nil;
  lDAOLink.Engine := nil;

  CheckTableLinks(AAccessDBName);
  dmDatabase := TdmDatabase.Create(nil, FServerName, 'NBNData', FTrustedConnection);
  GetCorrectConnection(FTrustedConnection, FSaUsername, FSaPassword);
  FqryAllPurpose := TADOQuery.Create(nil);
  FqryAllPurpose.Connection := dmDatabase.dbLocal;
  TADODataset(FqryAllPurpose).CommandTimeout := 0;

  Log('Checking Access tables are linked correctly', False);
  CheckTableLinks(AAccessDBName, '');

  //Connect to the Access Database
  FconnAccessDB := TADOConnection.Create(nil);
  with FconnAccessDB do begin
    ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=' + FAccessDBName +
                        ';Persist Security Info=False;' +
                        'Jet OLEDB:Database Password=' + AAccessDBPassword;
    LoginPrompt := False;
    ConnectionTimeout := 0;
    Open;
    KeepConnection := True;
  end;

  //Read the Catalog for the Access database and set its ActiveConnection
  FAccessCatalog := CoCatalog.Create;
  FAccessCatalog.Set_ActiveConnection(FconnAccessDB.ConnectionObject);

  //Load and sort the TableMappings
  FstTableMappings := TStringList.Create;
  FstTableMappings.LoadFromFile(FDataFilesPath + 'TableMappings.ini');
  FstTableMappings.Sort;

  //Create StringList to hold Table names and the SQL required to move the data for that table
  FstMoveTableDataSQL := TStringList.Create;
  FAccessTablesToMigrate := TStringList.Create;
  FErrorCount := 0;
  FLastQueryTrack := 0;
end;

{-------------------------------------------------------------------------------
  Closes the connection on the dmDatabase and reconnects either as trusted or
  using the Sa login. Closing for trusted connection is required to get rid of
  the application role that dmDatabase applies automatically. And in here, we DON'T
  want that.
}
procedure TDBMigrator.GetCorrectConnection(ATrustedConnection: Boolean; const ASaUsername,
    ASaPassword: String);
begin
  if ATrustedConnection then begin
      dmDatabase.dbLocal.Close;
      dmDatabase.dbLocal.Open;   // No roles! Bingo!
  end else
    with dmDatabase do begin
      dbLocal.Close;
      // Swap nbnuser login for Sa
      dbLocal.ConnectionString :=
            StringReplace(
                      StringReplace(GetNewConnectionString,
                                    'NBNUser', ASaUserName, [rfReplaceAll]),
                      'NBNPassword', ASaPassword, [rfReplaceAll]);
      dbLocal.Open;
    end;
end;

//==============================================================================
// Description: Drop database connections and free all used memory.
//
// Author: Ben Collier
// Created: 04/12/2002
//------------------------------------------------------------------------------
destructor TDBMigrator.Destroy;
begin
  FTablesNotMigrated.Free;

  if Assigned(FConnAccessDB) then
    if FconnAccessDB.Connected then FconnAccessDB.Close;
  FconnAccessDB.Free;
  if assigned(FAccessErrorConnection) then begin
    FAccessErrorConnection.Close;
    FAccessErrorConnection.Free;
  end;
  FUpgradeErrorInsert.Free;
  FAllowedErrorFile.Free;
  FstTableMappings.Free;
  FstMoveTableDataSQL.Free;
  FAccessTablesToMigrate.Free;
  dmDatabase.Free;
  Log('Migration object destroyed', False);
  inherited;
end;

{===============================================================================
 Description : Run the actual migration process.  Returns true if no errors
 Created : 17/1/2003 }
function TDBMigrator.Execute: boolean;
begin
  if SINGLE_TABLE<>'' then
    Showmessage('Debugging '+SINGLE_TABLE);
  if START_TABLE<>0 then
    ShowMessage('Debugging from table ' + IntToStr(START_TABLE));
  EnableTriggers(false);
  DropParentConstraints;
  try
    FindServerTempFolder;
    GetAccessTablesToMigrate;
    ConfigureSQLServer;
    try
      MoveData;
    finally
      ResetSQLServerConfiguration;
    end;
    TransferSiteID;
    // Should get TempDB back to something half decent.
    dmDatabase.ExecuteSQL('DBCC SHRINKDATABASE (TempDB)');
  finally
    FLabelCallback(ltCurrentTable, 'Finalising data migration');
    FLabelCallback(ltRecordsOutOf, '');
    Log('Finalising the migration process', False);
    CreateParentConstraints;
    EnableTriggers(true);
  end;
  Result := (FErrorCount=0);
  Log('Data migration completed', False);
end;  // TDBMigrator.Execute

//==============================================================================
// Description: For all Access tables that are of types 'TABLE' or 'LINK', create
//   a string list of them if they should be migrated ).
//
// Author: Ben Collier
// Created: 04/12/2002
//------------------------------------------------------------------------------
procedure TDBMigrator.GetAccessTablesToMigrate;
var
  liCounter: integer;
  lstJetProviderString: string;
  lstAccessTableName: string;
  lstSQLServerTableName: string;
  lcmdTableExists: TADOCommand;
  ltfTableExistsOnSQLServer: boolean;
  lstSecurity : string;
begin
  lcmdTableExists := CreateCmdTableExists;
  try
    lstSecurity := GetSecurityString(True);
    lstJetProviderString := 'ODBC;DRIVER={SQL Server};Server='+FServerName+';Database=NBNData;'
                         + lstSecurity + 'Mode= ReadWrite';
    //Loop through all Access tables
    for liCounter := 0 to (FAccessCatalog.Tables.Count -1) do
    begin
      if Copy(FAccessCatalog.Tables[liCounter].Name, 1, 4) <> 'DBO_' then
      begin
        {NB Not interested in tables of type:
        ACCESS TABLE: An Access system table
        PASS-THROUGH A linked table through an ODBC data source
        SYSTEM TABLE A Jet system table
        VIEW}

        //If the Access table is of type 'TABLE' or 'LINK' and no linked table exists then create
        //a linked table to SQLExpress
        if (FAccessCatalog.Tables[liCounter].Get_Type_ = 'TABLE')
        or (FAccessCatalog.Tables[liCounter].Get_Type_ = 'LINK') then
        begin
          lstAccessTableName := 'DBO_' + FAccessCatalog.Tables[liCounter].Name;
          lstSQLServerTableName := GetSqlServerTableName(FAccessCatalog.Tables[liCounter].Name);
          //Check that the table to be linked to exists on SQLExpress
          lcmdTableExists.Parameters.Refresh;
          lcmdTableExists.Parameters[1].Value := lstSQLServerTableName;
          lcmdTableExists.Parameters[2].Direction := pdReturnValue;
          lcmdTableExists.ExecuteOptions := [eoExecuteNoRecords];
          lcmdTableExists.Execute;

          ltfTableExistsOnSQLServer := lcmdTableExists.Parameters[2].Value;

          //Create the linked table if the table exists on SQLExpress
          if ltfTableExistsOnSQLServer then
            FAccessTablesToMigrate.Add(FAccessCatalog.Tables[liCounter].Name)
          else
            FTablesNotMigrated.Add(FAccessCatalog.Tables[liCounter].Name);
        end;
      end;
    end;
  finally
    lcmdTableExists.Free;
  end;
end;


{-------------------------------------------------------------------------------
  Description : Create a command object we can use to check if a table exists
              on the SQLExpress end.  This creates the object, caller must free it.
  Created : 02/04/2003 }
function TDBMigrator.CreateCmdTableExists : TADOCommand;
begin
  //Set up Command object to check if the table exists on the SQLExpress
  Result := TADOCommand.Create(nil);
  Result.Connection := dmDatabase.dbLocal;
  Result.CommandType := cmdStoredProc;
  Result.CommandText := 'spTableExists';
end;


{-------------------------------------------------------------------------------
  Description : Returns a SQL Server table name for a given Access table.
              Not all names are identical.
  Created : 27/02/2003 }
function TDBMigrator.GetSqlServerTableName(const AAccessTableName : String) : string;
begin
  //If the current table has a TableMapping then use it
  if FstTableMappings.IndexOfName(AAccessTableName) > -1 then
    Result := FstTableMappings.Values[AAccessTableName]
  else
    Result := AAccessTableName;
end;

{-------------------------------------------------------------------------------
  Description : Returns an Access table name for a given SQL Server table.
              Not all names are identical.
  Created : 27/02/2003 }
function TDBMigrator.GetAccessTableName(const ASQLTableName: string): string;
var
  i : integer;
begin
  Result := ASQLTableName; // default
  for i := 0 to FstTableMappings.Count - 1 do begin
    if CompareText(ASQLTableName,
                   Copy(FstTableMappings[i], Pos('=', FstTableMappings[i]) + 1, 255)) = 0 then
    begin
      Result := FstTableMappings.Names[i];
      Break; // from loop
    end;
  end;
end;

{===============================================================================
 Description : Returns a connection string snippet that defines security.
             If a Jet Connection then the string returned is different
 Created : 28/1/2003}
function TDBMigrator.GetSecurityString( JetConnection : boolean ) : string;
var
  lstPassword : string;
begin
  if FTrustedConnection and JetConnection then
    Result := 'Trusted_Connection=Yes;'
  else if FTrustedConnection and (not JetConnection) then
    Result := 'Integrated Security=SSPI;'
  else if JetConnection then
    Result := 'UID='+FSaUsername + ';PWD=' + FSaPassword + ';'
  else begin
    if FSaPassword = '' then
      lstPassword := ''
    else
      lstPassword := 'password=' + FSaPassword + ';';
    Result := 'User ID=' + FSaUsername + ';' + lstPassword;
  end;
end;

//==============================================================================
// Description: Create the linked table from the Access database to the SQL server
//
// Author: Ben Collier
// Created: 11/12/2002
//------------------------------------------------------------------------------
procedure TDBMigrator.CreateLinkedTable(const istJetProviderString, istAccessTable,
  istSQLTableName: string);
var
  lLinkTable: _Table;
begin
  lLinkTable := coTable.Create;
  lLinkTable.ParentCatalog := FAccessCatalog;

  with lLinkTable do begin
    Name := istAccessTable;
    //Properties['Jet OLEDB:Link Datasource'].value := istDirName ;
    Properties['Jet OLEDB:Link Provider String'].value := istJetProviderString;
    Properties['Jet OLEDB:Remote Table Name'].value := istSQLTableName;
    Properties['Jet OLEDB:Create Link'].value := True;
  end;// With
  FAccessCatalog.Tables.Append(lLinkTable);
end;

//==============================================================================
// Description: Moves the data from each Access table to the corresponding SQL server table
//              using the SQL server to 'pull' the data through.
//
// Author: Ben Collier
// Created: 11/12/2002
//------------------------------------------------------------------------------
procedure TDBMigrator.GenerateMoveDataSQL(const ASQLTableName : string);
var
  lstAccessTableName: string;
  lTransferQuery : TTransferQuery;
begin
  try
    //Retrieve current Access table name
    lstAccessTableName := GetAccessTableName(ASQLTableName);
    // Proceed if table should be migrated.
    if FTablesNotMigrated.IndexOf(ASQLTableName) = -1 then begin
      lTransferQuery := TTransferQuery.Create(ASQLTableName, lstAccessTableName, FAccessDBName,
                                              dmDatabase.dbLocal, FconnAccessDB,
                                              FIncSysSupplied);
      FstMoveTableDataSQL.AddObject(ASQLTableName, lTransferQuery);
    end;
  except
    on E:Exception do
      raise EDBMigrator.Create('Error occurred generating SQL for table table ' +
                               ASQLTableName, E);
  end; // try
end;

{ ------------------------------------------------------------------------------
  Determines whether the SQL Server is some version of SQL Server 2005, and
  sets FIsSQLServer2005;
}
procedure TDBMigrator.CheckSQLServerVersion;
var
  Version: String;
begin
  FqryAllPurpose.SQL.Text := SQL_GET_PRODUCT_VERSION;
  TrackQuery(SQL_GET_PRODUCT_VERSION);
  FqryAllPurpose.Open;
  try
    Version := FqryAllPurpose.FieldByName('Version').AsString;
    FIsSQLServer2005 := (Copy(Version, 1, 2) = '9.');
  finally
    FqryAllPurpose.Close;
  end;
end;

{ ------------------------------------------------------------------------------
  Sets any server options that we need to perform the migration.
}
procedure TDBMigrator.ConfigureSQLServer;
begin
  CheckSQLServerVersion;
  if FIsSQLServer2005 then
  begin
    FAdvancedOptionsWereVisible :=
        GetSQLServerOption(OPT_SHOW_ADVANCED_OPTS);
    FDistributedQueriesWereEnabled :=
        GetSQLServerOption(OPT_DISTRIBUTED_QUERIES);
    FCommandShellWasEnabled :=
        GetSQLServerOption(OPT_COMMAND_SHELL);
    
    SetSQLServerOption(OPT_SHOW_ADVANCED_OPTS, True);
    SetSQLServerOption(OPT_DISTRIBUTED_QUERIES, True);
    SetSQLServerOption(OPT_COMMAND_SHELL, True);
  end;  
end;

{ ------------------------------------------------------------------------------
  Resets any server options that were changed by the ConfigureSQLServer method.
}
procedure TDBMigrator.ResetSQLServerConfiguration;
begin
  if FIsSQLServer2005 then
  begin
    if not FCommandShellWasEnabled then
      SetSQLServerOption(OPT_COMMAND_SHELL, False);
    if not FDistributedQueriesWereEnabled then
      SetSQLServerOption(OPT_DISTRIBUTED_QUERIES, False);
    if not FAdvancedOptionsWereVisible then
      SetSQLServerOption(OPT_SHOW_ADVANCED_OPTS, False);
  end;
end;

{ ------------------------------------------------------------------------------
  Gets the value of the named server option.
}
function TDBMigrator.GetSQLServerOption(const OptionName: String): Boolean;
var
  Query: String;
begin
  Query := Format(SQL_GET_SERVER_OPTION, [OptionName]);
  FqryAllPurpose.SQL.Text := Query;
  TrackQuery(Query);
  FqryAllPurpose.Open;
  try
    Result := FqryAllPurpose.FieldByName('value').AsInteger = 1;
  finally
    FqryAllPurpose.Close;
  end;
end;

{ ------------------------------------------------------------------------------
  Sets the value of the named server option.
}
procedure TDBMigrator.SetSQLServerOption(const OptionName: String;
  Value: Boolean);
var
  IntValue: Integer;
begin
  if Value then IntValue := 1 else IntValue := 0;
  RunAndTrackQuery(Format(SQL_SET_SERVER_OPTION, [OptionName, IntValue]));
  RunAndTrackQuery(SQL_RECONFIGURE);
end;

procedure TDBMigrator.MoveData;
var
  i: integer;
  lPriorityList :TTablePriorityList;
begin
  //Create the list containing the priority order by which to populate tables on the SQL server
  lPriorityList := TTablePriorityList.Create(dmDatabase.Relationships);
  try
    TotalRecordsTransferred := 0;
    PreProcessTables(lPriorityList);
    // empty the tables we are going to transfer, in reverse order, if we are replacing
    // system supplied data
    if FIncSysSupplied then
      for i := lPriorityList.Count-1 downto START_TABLE do begin
        if (SINGLE_TABLE<>'') and (lPriorityList.Item[i]<>SINGLE_TABLE) then continue;
        if (FstMoveTableDataSQL.IndexOf(lPriorityList.Item[i]) > -1)
            or IsTruncationRequiredForSysSuppliedDb(lPriorityList.Item[i]) then
          try
            ExecSQL('truncate table [' + lPriorityList.Item[i]+ ']', true);
          except on Exception do
            ExecSQL('delete from [' + lPriorityList.Item[i]+ ']', true);
          end;
      end;
    //Move data for each Access table in order of priority so as not to violate any Foreign Keys
    for i := START_TABLE to lPriorityList.Count-1 do begin
      if (SINGLE_TABLE<>'') and (lPriorityList.Item[i]<>SINGLE_TABLE) then continue;

      if (FstMoveTableDataSQL.IndexOf(lPriorityList.Item[i]) > -1) then
      begin
        try
          //Allow pre-data-move operations on either the Access or SQL server tables
          FLabelCallback(ltCurrentTable,
                         'Current Table: ' + ReadableFormat(lPriorityList.Item[i]));
          FLabelCallback(ltRecordsOutOf, IntToStr(FRecordCounts[i]) + ' records to transfer');
          Log('Transferring ' + IntToStr(FRecordCounts[i]) +
              ' records in ' + lPriorityList.Item[i], False);
          if FTotalRecordsToTransfer>0 then
            FprgDataMigration.Position := (FTotalRecordsTransferred * 100) div FTotalRecordsToTransfer;
          Application.ProcessMessages;
          if FRecordCounts[i]>0 then begin
            PrepareTable(lPriorityList.Item[i], true, FRecordCounts[i]);
            //Move data
            BulkTransferRecords(lPriorityList.Item[i], FRecordCounts[i]);
            //Allow post-data-move operations on either the Access or SQL server tables
            PrepareTable(lPriorityList.Item[i], false, FRecordCounts[i]);
            TrimWorkingSet;
          end;
          Log(lPriorityList.Item[i] + ' completed', True);
        except
          on E:Exception do
            HandleError(i, lPriorityList.Item[i], E);
        end; // try..except
      end;
      Application.ProcessMessages;
      if FCancelled then Exit;
    end;
  finally
    lPriorityList.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Return true if a table is a new dictionary one
}
function TDBMigrator.IsTruncationRequiredForSysSuppliedDb(const ATableName: string): boolean;
begin
  Result := (CompareText(ATableName, 'Taxon_Group')=0) or
      (CompareText(ATableName, 'NameServer')=0) or
      (CompareText(ATableName, 'Index_Taxon_Name')=0);
end;



{-------------------------------------------------------------------------------
  Description : return record count - dest if ADest is true, else source view count
  Created : 05/03/2003 }
function TDBMigrator.GetRecordCount(const ASqlTableName : string; ADest : boolean) : integer;
begin
  if ADest then
    FqryAllPurpose.Sql.Text := 'Select Count(*) from ['+ASQLTableName+']'
  else
    FqryAllPurpose.Sql.Text := GetTransferQuery(ASqlTableName).GetCountSourceSQL;
  TrackQuery(FqryAllPurpose.Sql.Text);
  FqryAllPurpose.Open;
  try
    Result := FqryAllPurpose.Fields[0].AsInteger;
  finally
    FqryAllPurpose.Close;
  end;
end;

{-------------------------------------------------------------------------------
  Description : In the event of an error, try copying the records
              and storing the failures in a new Access datafile.
              Uses a binary search to get the records in, and drills further into
              each failed segment to single out the records that fail.
  Created : 27/02/2003 }
procedure TDBMigrator.BulkTransferRecords(const ASQLTableName : string;
          const ASourceRecordCount : integer);
var
  lAccessTableName : string;
  lRecordsTransferred : integer;
  lBcpFilename : string;
begin
  lAccessTableName := GetAccessTableName(ASQLTableName);
  // Create a view that sees the data in Access that needs to be transferred
  RunAndTrackQuery(SQL_DROP_VIEW);
  RunAndTrackQuery(GetTransferQuery(ASqlTableName).GetCreateViewSQL('VW_IMPORT'));
  try
    lBcpFileName := FSrvTempFolder + ASQLTableName + '.txt';
    TrackQuery(Format(SQL_BCP_OUT, [lBcpFileName, GetBcpSecurity]));
    dmDatabase.dbLocal.Execute(Format(SQL_BCP_OUT, [lBcpFileName, GetBcpSecurity]));
    lRecordsTransferred := DoBulkInsert(ASQLTableName, lBcpFileName, ASourceRecordCount);
    // Have all the records transferred?
    if lRecordsTransferred<ASourceRecordCount then
      TransferIndividualRecords(ASQLTableName);
  finally
    // cleanup view used for import
    RunAndTrackQuery(SQL_DROP_VIEW);
    RunAndTrackQuery(SQL_DROP_BAD_TABLE);
    // and the bulk import txt file
    dmDatabase.dbLocal.Execute(Format(SQL_DELETE_BCP_FILE, [lBcpFileName]));
  end; // try
end;

{-------------------------------------------------------------------------------
  Description : Return command line parameters for the security required
     by a bcp operation
  Created : 27/03/2003 }
function TDBMigrator.GetBcpSecurity : string;
begin
  if FTrustedConnection then
    Result := '-T'
  else
    Result := '-U' + FsaUserName + ' -P'+FsaPassword;
  Result := Result + ' -S'+FServername;
end;

{-------------------------------------------------------------------------------
  Description : Run the Bulk Insert operation.  This may involve multiple
              operations if errors occur
  Created : 05/03/2003 }
function TDBMigrator.DoBulkInsert(ASQLTableName, ABcpFileName : string;
         ASourceRecordCount : integer) : integer;
var
  lBatchStart, lFailedBatches : integer;
  lOrigRecordCount, lOrigTotal : integer;
begin
  lOrigRecordCount := GetRecordCount(ASqlTableName, True);
  lOrigTotal := TotalRecordsTransferred;
  lBatchStart := 1;
  lFailedBatches := 0;
  repeat
    try
      RunAndTrackQuery(Format(SQL_BCP_IN, [
            ASQLTableName, ABcpFileName,
            IntToStr(BATCH_SIZE), IntToStr(lBatchStart)]));
    except on EOleException do ;
    end;
    Result := GetRecordCount(ASqlTableName, True) - lOrigRecordCount;
    TotalRecordsTransferred := lOrigTotal + Result;
    if Result + lFailedBatches*BATCH_SIZE<ASourceRecordCount then begin
      // we have a failed batch, so reformat the query to move the
      // data into a temp table
      // First create an empty table to copy into
      RunAndTrackQuery(SQL_CREATE_BAD_TABLE);
      lBatchStart := Result+lFailedBatches*BATCH_SIZE+1;
      FqryAllPurpose.SQL.Text:= Format(SQL_BCP_IN, ['#TempBadRecords', ABcpFileName,
            IntToStr(BATCH_SIZE), IntToStr(lBatchStart) + ', LASTROW=' +
            IntToStr(lBatchStart+BATCH_SIZE-1)]);
      TrackQuery(FqryAllPurpose.Sql.Text);
      RunQueryAndTrackErrors(FqryAllPurpose);
      Log('Failed batch of records - #' + IntToStr(lBatchStart) +
          ' to #' + IntToStr(lBatchStart+BATCH_SIZE-1), True);
      Inc(lFailedBatches);
    end;
    lBatchStart := Result+lFailedBatches*BATCH_SIZE+1;
    Application.ProcessMessages;
    if FCancelled then Exit;
  until lBatchStart > ASourceRecordCount;
  if lFailedBatches > 0 then
    Log(IntToStr(lFailedBatches) + ' blocks of records failed during bulk transfer', True);
end;

{-------------------------------------------------------------------------------
  Logs the running of a query in FqryAllPurpose, then runs it
}
procedure TDBMigrator.RunAndTrackQuery(const ASQL: string);
begin
  FqryAllPurpose.Sql.Text := ASQL;
  TrackQuery(ASQL);
  FqryAllPurpose.ExecSQL;
end;

{-------------------------------------------------------------------------------
  Description : Execs a query.  If no records affected, or any sort of errors,
      puts detailed log info into UpgradeLog.txt
  Created : 08/04/2003 }
procedure TDBMigrator.RunQueryAndTrackErrors(const AQuery : TADOQuery);
begin
  try
    AQuery.ExecSQL;
    if AQuery.Connection.Errors.Count>0 then begin
      LogConnectionErrors(AQuery.Connection);
      LogQueries;
    end;
  except
    on E:EOleException do begin
      Log('Error occurred moving data into #TempBadRecords', True);
      Log(E.Classname, True);
      Log(E.Message, True);
      LogQueries;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Description : Dump any errors on a connection into the log
  Created : 08/04/2003 }
procedure TDBMigrator.LogConnectionErrors(const AConnection : TADOConnection);
var
  i : integer;
begin
  for i := 0 to AConnection.Errors.Count-1 do
    Log(AConnection.Errors[i].Description, True);
end;

{-------------------------------------------------------------------------------
  Description : Returns a TTransferQuery object for the given table name
  Created : 05/03/2003 }
function TDBMigrator.GetTransferQuery(const ASQLTableName : string): TTransferQuery;
begin
  if FstMoveTableDataSQL.IndexOf(ASQLTableName)>-1 then
    Result := TTransferQuery(FstMoveTableDataSQL.Objects[
           FstMoveTableDataSQL.IndexOf(ASQLTableName)])
  else
    raise EDBMigrator.Create('Cannot locate transfer query for ' + ASQLTableName);
end;

{-------------------------------------------------------------------------------
  Description : Transfers any records that could not be done by bulk insert
              one by one.  Records that still don't go in are moved to an
              UpgradeErrors.mdb file
  Created : 03/03/2003 }
procedure TDBMigrator.TransferIndividualRecords(ASQLTableName : string);
var
  lPrimaryKey : TPrimaryKey;
  lqryRecordKeys : TADOQuery;
  lAccessTableName : string;
  lRecordInsertSQL : string;
  lOrigErrorCount  : integer;
begin
  lOrigErrorCount := FErrorCount;
  FLabelCallback(ltRecordsOutOf, 'Scanning for errors in ' + ReadableFormat(ASQLTableName));
  Log('Scanning errors in ' + ASQLTableName, True);
  Application.ProcessMessages;
  if FCancelled then Exit;
  lqryRecordKeys := TADOQuery.Create(nil);
  try
    lPrimaryKey := dmDatabase.SplitPrimaryKey(ASQLTableName);
    lAccessTableName := GetAccessTableName(ASQLTableName);
    // create an index on the temp bad records table
    if lPrimaryKey.Key2='' then
      RunAndTrackQuery(Format(SQL_INDEX_BAD_TABLE, [lPrimaryKey.Key1]))
    else
      RunAndTrackQuery(Format(SQL_INDEX_BAD_TABLE, [lPrimaryKey.Key1 + ', ' + lPrimaryKey.Key2]));
    lqryRecordKeys.Connection := dmDatabase.dbLocal;
    lqryRecordKeys.CommandTimeout := 0;
    // Select into a temp table all the records so we can process SQL Server side
    lqryRecordKeys.Sql.Text := 'Select * from #TempBadRecords';
    TrackQuery(lqryRecordKeys.Sql.Text);
    lqryRecordKeys.Open;
    Log('Data selected into SQL Server: ' + IntToStr(lqryRecordKeys.RecordCount)+
              ' records', True);
    try
      // get a query template to copy in one record at a time
      lRecordInsertSQL := GetTransferQuery(ASQLTableName).
                       GetIndividualRecordInsertSQL(lPrimaryKey);
      while not lqryRecordKeys.EOF do begin
        try
          if lPrimaryKey.Key2 = '' then
            RunAndTrackQuery(Format(lRecordInsertSQL, [
                  lqryRecordKeys.FieldByName(lPrimaryKey.Key1).AsString]))
          else
            RunAndTrackQuery(Format(lRecordInsertSQL, [
                  lqryRecordKeys.FieldByName(lPrimaryKey.Key1).AsString,
                  lqryRecordKeys.FieldByName(lPrimaryKey.Key2).AsString]));
          TotalRecordsTransferred := TotalRecordsTransferred + FqryAllPurpose.RowsAffected;
        except
          on E:Exception do
              if lPrimaryKey.Key2='' then
                ErrorInRecord(lAccessTableName,
                              lqryRecordKeys.FieldByName(lPrimaryKey.Key1).AsString, '')
              else
                ErrorInRecord(lAccessTableName,
                              lqryRecordKeys.FieldByName(lPrimaryKey.Key1).AsString,
                              lqryRecordKeys.FieldByName(lPrimaryKey.Key2).AsString);
        end;
        lqryRecordKeys.Next;
        Application.ProcessMessages;
        if FCancelled then Exit;
      end;
    finally
      Log('Total errors found in ' + ASQLTableName + ': ' +
                 IntToStr(FErrorCount-lOrigErrorCount), True);
    end;
  finally
    lqryRecordKeys.Free;
  end; // try
end;

{-------------------------------------------------------------------------------
  Description : In the event of an error, copies the broken record into a new
      Access mdb file called 'UpgradeErrors'.  The rest of the records can then
      copy Ok.
  Created : 27/02/2003 }
procedure TDBMigrator.ErrorInRecord(const AAccessTableName, APrimaryKey1, APrimaryKey2 : string);
var
  lUpgradeErrorDB : string;
  lPrimaryKey : TPrimaryKey;
  lTableCreated : boolean;
begin
  // Don't record errors that the user does not need to know about
  if FAllowedErrorFile.IndexOf(APrimaryKey1 + #9 + APrimaryKey2 + #9 + AAccessTableName) > -1 then
    Exit;
  lUpgradeErrorDB := ExtractFilePath(FAccessDBName)+UPGRADE_ERROR_FILE;
  // ensure MDB file exists
  if FErrorCount=0 then
    CreateUpgradeErrorDB(lUpgradeErrorDB);
  if not assigned(FAccessErrorConnection) then begin
    FAccessErrorConnection := TADOConnection.Create(nil);
    FAccessErrorConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=' +
              lUpgradeErrorDB + ';Persist Security Info=False;';

    FUpgradeErrorInsert := TADOQuery.Create(nil);
    FUpgradeErrorInsert.Connection := FAccessErrorConnection;
  end;
  // Ensure table exists
  lTableCreated := dmDatabase.CreateImportExportTable(FAccessErrorConnection, AAccessTableName);
  lPrimaryKey := dmDatabase.SplitPrimaryKey(GetSQLServerTableName(AAccessTableName));
  FUpgradeErrorInsert.SQL.Text := Format(SQL_ERROR_INSERT, [
          AAccessTableName, AAccessTableName, FAccessDBName,
          lPrimaryKey.Key1, APrimaryKey1]);
  if lPrimaryKey.Key2<>'' then
    FUpgradeErrorInsert.SQL.Add('AND ' + lPrimaryKey.Key2 + '="' + APrimaryKey2 + '"');
  try
    IncrementErrorCount;
    FUpgradeErrorInsert.ExecSQL;
  except
    on E:Exception do
      if lTableCreated then // if first error in this table
        MessageDlg('Error occurred recording records in the ' + AAccessTableName +
            ' table in the '+UPGRADE_ERROR_FILE+' file.  Upgrade will continue but '+
            'not all data will transfer.', mtWarning, [mbOk], 0);
  end; // try
end;

{-------------------------------------------------------------------------------
  Description : First time it is required, create the UpgradeError.mdb file
              by copying the blank mdb file from the CD
  Created : 27/02/2003 }
procedure TDBMigrator.CreateUpgradeErrorDB(const ADBPath : string);
var
  lAttributes : Word;
begin
  // copy blank file over from CD, but rename it
  CopyFile(PChar(FDataFilesPath + 'Database\nbndata.mdb'),
           PChar(ADBPath), False);
  // remove readonly flag from copied file as it came from the CD
  lAttributes := FileGetAttr(ADBPath);
  lAttributes := lAttributes and not SysUtils.faReadOnly;
  FileSetAttr(ADBPath, lAttributes);
end;

//==============================================================================
// Description: Execute input SQL text on the Access or SQL server databases and return
//              the number of records affected.
//
// Author: Ben Collier
// Created: 04/12/2002
//------------------------------------------------------------------------------
function TDBMigrator.ExecSQL(const istSQL: string; itfUseSQLServer: boolean): integer;
var
  lcmdExecSQL: TADOCommand;
  liRowsAffected: integer;
begin
  lcmdExecSQL := TADOCommand.Create(nil);
  lcmdExecSQL.CommandTimeout := 0;
  if itfuseSQLServer then
  begin
    if not dmDatabase.dbLocal.Connected then dmDatabase.dbLocal.Connected := true;
    lcmdExecSQL.Connection := dmDatabase.dbLocal;
  end else begin
    if not FconnAccessDB.Connected then FconnAccessDB.Connected := true;
    lcmdExecSQL.Connection := FconnAccessDB;
  end;

  lcmdExecSQL.CommandType := cmdText;
  lcmdExecSQL.CommandText := istSQL;
  lcmdExecSQL.ExecuteOptions := [eoExecuteNoRecords];
  TrackQuery(istSQL);
  try
    lcmdExecSQL.Execute(liRowsAffected, EmptyParam);
  finally
    lcmdExecSQL.Free;
    Result := liRowsAffected;
  end;
end;

//==============================================================================
// Description: Facilitates operations on the Access/SQLExpress tables or data before
//              and after moving the data
//
// Author: Ben Collier
// Created: 04/12/2002
//------------------------------------------------------------------------------
procedure TDBMigrator.PrepareTable(istTableName: string; itfBeforeExecute: boolean;
          var ARecordCount : integer);
var
  lstSQL: string;
begin
  istTableName := UpperCase(istTableName);
  if itfBeforeExecute then begin
    if istTableName = 'ADMIN_RELATION' then begin
      //Fix referential integrity on ADMIN_AREA_KEY_1&2 in the ADMIN_RELATION access table
      HandleQueryErrors('SELECT [ADMIN_RELATION].ADMIN_RELATION_KEY ',
                'DELETE [ADMIN_RELATION].* FROM ([ADMIN_RELATION] ',
                'FROM ([ADMIN_RELATION] '
              + 'LEFT JOIN [ADMIN_AREA] AA1 ON [ADMIN_RELATION].ADMIN_AREA_KEY_1 = AA1.ADMIN_AREA_KEY) '
              + 'LEFT JOIN [ADMIN_AREA] AA2 ON [ADMIN_RELATION].ADMIN_AREA_KEY_2 = AA2.ADMIN_AREA_KEY '
              + 'WHERE AA1.ADMIN_AREA_KEY IS NULL OR AA2.ADMIN_AREA_KEY IS NULL',
                'ADMIN_RELATION', ARecordCount);
    end
    else if istTableName = 'TAXON_COMMON_NAME' then begin
      //Remove Access TAXON_COMMON_NAME records which do not have valid
      //TAXON_LIST_ITEM_KEYs or TAXON_VERSION_KEYs
      lstSQL := 'DELETE TCN.* FROM ([TAXON_COMMON_NAME] TCN LEFT JOIN [TAXON_LIST_ITEM] '
              + 'ON [TCN].[TAXON_LIST_ITEM_KEY] = [TAXON_LIST_ITEM].[TAXON_LIST_ITEM_KEY]) '
              + 'LEFT JOIN [TAXON_VERSION] '
              + 'ON [TCN].[TAXON_VERSION_KEY] = [TAXON_VERSION].[TAXON_VERSION_KEY] '
              + 'WHERE [TAXON_LIST_ITEM].[TAXON_LIST_ITEM_KEY] IS NULL '
              + 'OR [TAXON_VERSION].[TAXON_VERSION_KEY] IS NULL';
      ExecSQL(lstSQL, false);
    end
    else if istTableName = 'TAXON_DETERMINATION' then
      HandleQueryErrors('SELECT TAXON_DETERMINATION_KEY ',
            'DELETE ',
            'FROM TAXON_DETERMINATION WHERE TAXON_LIST_ITEM_KEY IS NULL',
            'TAXON_DETERMINATION', ARecordCount)
    else if istTableName = 'TAXON' then begin
      //Fix referential integrity on TAXON_NAME_TYPE_KEY in access table TAXON by setting it to
      //unknown for invalid cases
      lstSQL := 'UPDATE TAXON LEFT JOIN TAXON_NAME_TYPE ON TAXON.TAXON_NAME_TYPE_KEY = '
              + 'TAXON_NAME_TYPE.TAXON_NAME_TYPE_KEY SET TAXON.TAXON_NAME_TYPE_KEY = '
              + '''NBNSYS0000000000'' WHERE TAXON_NAME_TYPE.TAXON_NAME_TYPE_KEY Is Null';
      ExecSQL(lstSQL, false);
    end
    else if istTableName = 'TAXON_FACT' then begin
      //Fix referential integrity on TAXON_FACT.SOURCE_KEY in access by setting it to
      //NULL for invalid SOURCE.SOURCE_KEY values
      lstSQL := 'UPDATE TAXON_FACT LEFT JOIN SOURCE ON TAXON_FACT.SOURCE_KEY = '
              + 'SOURCE.SOURCE_KEY SET TAXON_FACT.SOURCE_KEY = NULL '
              + 'WHERE SOURCE.SOURCE_KEY IS NULL AND TAXON_FACT.SOURCE_KEY IS NOT NULL';
      ExecSQL(lstSQL, false);
    end
    else if istTableName = 'BIOTOPE_LIST_ITEM' then begin
      //DROP and RECREATE the PARENT foreign key on the SQL BIOTOPE table to full enable data insertion
      lstSQL := 'if exists (select * from dbo.sysobjects where id = object_id(N''[dbo].[FK_BIOTOPE_LIST_ITEM_BIOTOPE_LIST_ITEM]'') and OBJECTPROPERTY(id, N''IsForeignKey'') = 1) '
                + 'ALTER TABLE [dbo].[BIOTOPE_LIST_ITEM] DROP CONSTRAINT FK_BIOTOPE_LIST_ITEM_BIOTOPE_LIST_ITEM';
      ExecSQL(lstSQL, true);
    end
    else if istTableName = 'TAXON_LIST_ITEM' then
    begin
      //If no PREFERRED_NAME exists then set it equal to the TAXON_LIST_ITEM_KEY
      lstSQL := 'UPDATE TAXON_LIST_ITEM SET PREFERRED_NAME = TAXON_LIST_ITEM_KEY '
              + 'WHERE PREFERRED_NAME IS NULL';
      ExecSQL(lstSQL, false);
      //If no TAXON_VERSION_KEY or TAXON_LIST_VERSION_KEY exists then remove the
      //TAXON_LIST_ITEM record
      HandleQueryErrors('SELECT TAXON_LIST_ITEM_KEY ',
              'DELETE ',
              'FROM TAXON_LIST_ITEM WHERE TAXON_VERSION_KEY IS NULL OR TAXON_LIST_VERSION_KEY IS NULL',
              'TAXON_LIST_ITEM',
              ARecordCount);
      //DROP the PARENT foreign key on the SQL TAXON_LIST_ITEM table to full enable data insertion
      lstSQL := 'if exists (select * from dbo.sysobjects where id = object_id(N''[dbo].[FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM]'') and OBJECTPROPERTY(id, N''IsForeignKey'') = 1) '
              + 'ALTER TABLE [dbo].[TAXON_LIST_ITEM] DROP CONSTRAINT FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM';
      ExecSQL(lstSQL, true);
    end
    else if istTableName = 'INDEX_TAXON_GROUP' then begin
      //Remove Access INDEX_TAXON_GROUP records which do not have valid
      //TAXON_LIST_ITEM_KEYs
      lstSQL := 'DELETE ITG.* FROM ([INDEX_TAXON_GROUP] [ITG] LEFT JOIN [TAXON_LIST_ITEM] [TLI] '
              + 'ON [ITG].[TAXON_LIST_ITEM_KEY] = [TLI].[TAXON_LIST_ITEM_KEY]) '
              + 'WHERE [TLI].[TAXON_LIST_ITEM_KEY] IS NULL';
      ExecSQL(lstSQL, false);
    end
    else if istTableName = 'TAXON_OCCURRENCE_DATA' then begin
      //Remove Access TAXON_COMMON_NAME records which do not have valid
      //TAXON_LIST_ITEM_KEYs or TAXON_VERSION_KEYs
      HandleQueryErrors('SELECT TAXON_OCCURRENCE_DATA_KEY ',
                'DELETE TOD.* ',
                'FROM ([TAXON_OCCURRENCE_DATA] [TOD] LEFT JOIN [TAXON_OCCURRENCE] [TO] '+
                'ON [TOD].[TAXON_OCCURRENCE_KEY] = [TO].[TAXON_OCCURRENCE_KEY])'+
                'WHERE [TO].[TAXON_OCCURRENCE_KEY] IS NULL',
                'TAXON_OCCURRENCE_DATA',
                ARecordCount);
    end
    else if istTableName = 'LAST_KEY' then begin
      lstSQL := 'DELETE FROM [LAST_KEY]';
      ExecSQL(lstSQL, true);
    end;
  end
  else begin // after the transfer
    if istTableName = 'BIOTOPE_LIST_ITEM' then begin
      lstSQL := 'ALTER TABLE [dbo].[BIOTOPE_LIST_ITEM] ADD CONSTRAINT '
                + '[FK_BIOTOPE_LIST_ITEM_BIOTOPE_LIST_ITEM] FOREIGN KEY ([PARENT]) '
                + 'REFERENCES [dbo].[BIOTOPE_LIST_ITEM] ([BIOTOPE_LIST_ITEM_KEY])';
      ExecSQL(lstSQL, true);
    end
    else if istTableName = 'TAXON_LIST_ITEM' then begin
      //Fix referential integrity for the PARENT field in SQL table TAXON_LIST_ITEM
      lstSQL := 'UPDATE TLI SET TLI.PARENT = NULL FROM TAXON_LIST_ITEM TLI LEFT JOIN '
              + 'TAXON_LIST_ITEM TLI2 ON TLI.PARENT = TLI2.TAXON_LIST_ITEM_KEY '
              + 'WHERE TLI.PARENT IS NOT NULL AND TLI2.TAXON_LIST_ITEM_KEY IS NULL';
      ExecSQL(lstSQL, true);
      //ReCREATE the PARENT foreign key on the SQL TAXON_LIST_ITEM table after data insertion
      lstSQL := 'ALTER TABLE [dbo].[TAXON_LIST_ITEM] ADD CONSTRAINT '
              + '[FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM] FOREIGN KEY ([PARENT]) '
              + 'REFERENCES [dbo].[TAXON_LIST_ITEM] ([TAXON_LIST_ITEM_KEY])';
      ExecSQL(lstSQL, true);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Description : Handles errors identified in a table by a piece of SQL,
              by moving the data into UpgradeErrors.mdb
              The SQL must return a single primary key in the first field,
              for all the bad records.
              SQL is divided into 2 parts as the main block will be the same
              when you select and when you delete.
  Created : 02/04/2003 }
procedure TDBMigrator.HandleQueryErrors(const AAccessSelectSQL, AAccessDeleteSQL,
          AMainSQLBlock, ATableName : String; var ARecordCount : integer);
var
  lErrorsFound : boolean;
begin
  with FqryAllPurpose do begin
    Connection := FconnAccessDB;
    try
      SQL.Text := AAccessSelectSQL + AMainSQLBlock;
      Open;
      while not EOF do begin
        ErrorInRecord(ATableName, Fields[0].AsString, '');
        Next;
      end;
      // correct the record count to transfer
      ARecordCount := ARecordCount - RecordCount;
      lErrorsFound := RecordCount>0;
      if lErrorsFound then
        Log('Pre-checking found ' + IntToStr(RecordCount) + ' errors in ' + ATableName, True);
      Close;
      if lErrorsFound then
        RunAndTrackQuery(AAccessDeleteSQL + AMainSQLBlock);
    finally
      Connection := dmDatabase.dbLocal;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Description : Turn on or off all the triggers on the db
  Created : 14/02/2003 }
procedure TDBMigrator.EnableTriggers(itfEnableTriggers: boolean);
var
  lcmdExecSQL: TADOCommand;
const
  SQL_CUSTODIAN_TRIGGERS = 'EXEC spControlCustodianTriggers @Enable=%d';
begin
  lcmdExecSQL := TADOCommand.Create(nil);
  if not dmDatabase.dbLocal.Connected then dmDatabase.dbLocal.Connected := true;
  lcmdExecSQL.Connection := dmDatabase.dbLocal;

  lcmdExecSQL.CommandType := cmdText;
  if itfEnableTriggers then
    lcmdExecSQL.CommandText := Format(SQL_CUSTODIAN_TRIGGERS, [1])
  else
    lcmdExecSQL.CommandText := Format(SQL_CUSTODIAN_TRIGGERS, [0]);
  lcmdExecSQL.ExecuteOptions := [eoExecuteNoRecords];
  lcmdExecSQL.CommandTimeout := 0;
  try
    try
      lcmdExecSQL.Execute;
    finally
      lcmdExecSQL.Free;
    end;
  except
    on E:EOleException do begin
      raise EDBMigrator.Create('Unable to disable triggers on the database volume.  Ensure the '+
            'login used has sa privileges.', E);
    end;
  end; // try
end;

{-------------------------------------------------------------------------------
  Description : Increments the error count and updates the label
  Created : 28/02/2003 }
procedure TDBMigrator.IncrementErrorCount;
begin
  Inc(FErrorCount);
  FLabelCallback(ltErrors, 'Errors encountered: ' + IntToStr(FErrorCount));
  Application.ProcessMessages;
end;

{-------------------------------------------------------------------------------
  Description : Accessor method also updates the label
  Created : 28/02/2003 }
procedure TDBMigrator.SetTotalRecordsTransferred(const Value: integer);
begin
  FTotalRecordsTransferred := Value;
  FLabelCallback(ltRecordsCopied, 'Records Transferred: ' + IntToStr(Value));
  Application.ProcessMessages;
end;

{-------------------------------------------------------------------------------
  Description : Keep track of the last 5 pieces of SQL to run, so we can report
              them if there are errors.
  Created : 12/03/2003 }
procedure TDBMigrator.TrackQuery(const ASQL: string);
begin
  // implement a rolling stack so we don't move strings around too much
  FLastQuery[FLastQueryTrack] := ASQL;
  Inc(FLastQueryTrack);
  if FLastQueryTrack > High(FLastQuery) then FLastQueryTrack := 0;
end;

{-------------------------------------------------------------------------------
  Description : Record an error into the log and throw an exception.
  Created : 12/03/2003 }
procedure TDBMigrator.HandleError(const ATableIndex: integer;
  const ATableName: string; E : Exception);
begin
  Log('-----------------------------------------------------', False);
  Log('Error copying data from table ' + IntToStr(ATableIndex) + '. '
                 + ATableName, False);
  Log(E.Classname + ' - ' + E.Message, False);
  LogQueries;
  raise EDBMigrator.Create('Error copying data from table ' + IntToStr(ATableIndex) + '. '
                 + ATableName + #13#10 +
                 E.Classname + #13#10 +
                 E.Message);
end;

{-------------------------------------------------------------------------------
  Description : Puts the last known queries we ran into the log
  Created : 08/04/2003 }
procedure TDBMigrator.LogQueries;
var
  i : integer;
begin
  Log('Last SQL statements : ', False);
  // roll the last 5 errors of the rolling stack in correct order
  for i := FLastQueryTrack to High(FLastQuery) do
    if FLastQuery[i]<>'' then
      Log(FLastQuery[i]+#10#13, False);
  for i := 0 to FLastQueryTrack-1 do
    if FLastQuery[i]<>'' then
      Log(FLastQuery[i]+#10#13, False);
  Log('-----------------------------------------------------', False);
end;

{-------------------------------------------------------------------------------
  Description : Get a list of all table record counts, plus the grand total.
              This is just the records we are going to attempt to transfer,
              not system supplied stuff etc.
              This also creates the linked access tables (DBO_*) that we use
              to link Access and SQL Server
  Created : 14/02/2003 }
procedure TDBMigrator.PreProcessTables(ATableList : TTablePriorityList);
var
  i, lAccessTableIdx : integer;
  lstSecurity, lstJetProviderString: string;
  lstAccessTableName: string;
  ltfLinkTableFound : boolean;
begin
  lstSecurity := GetSecurityString(True);
  lstJetProviderString := 'ODBC;DRIVER={SQL Server};Server='+FServerName+';Database=NBNData;'
                       + lstSecurity + 'Mode= ReadWrite';
  FLabelCallback(ltCurrentTable, 'Counting records...');
  Log('Identifying records to transfer and preparing linked tables.', False);
  FTotalRecordsToTransfer := 0;
  SetLength(FRecordCounts, ATableList.Count);
  for i := START_TABLE to ATableList.Count - 1 do begin
    if (SINGLE_TABLE<>'') and (ATableList.Item[i]<>SINGLE_TABLE) then Continue; // Debugging only

    // First find out if table should actually be ignored, exist in SQLExpress but not in Access..
    lstAccessTableName := GetAccessTableName(ATableList.Item[i]);
    ltfLinkTableFound := False;
    for lAccessTableIdx := 0 to FAccessCatalog.Tables.Count - 1 do begin
      if CompareText(FAccessCatalog.Tables[lAccessTableIdx].Name, lstAccessTableName) = 0 then
      begin
        ltfLinkTableFound := True;
        Break; // from loop
      end;
    end;

    if not ltfLinkTableFound then
      // Table in SQLExpress not found in Access, add to list.
      FTablesNotMigrated.Add(lstAccessTableName)
    else begin
      // Table exists in both Access and SQLExpress.
      lstAccessTableName := 'DBO_' + GetAccessTableName(ATableList.Item[i]);
      //Check to see if a linked 'DBO_' table exists for the current Access table to link to
      ltfLinkTableFound := False;
      for lAccessTableIdx := 0 to FAccessCatalog.Tables.Count-1 do begin
        if CompareText(FAccessCatalog.Tables[lAccessTableIdx].Name, lstAccessTableName)=0 then begin
          ltfLinkTableFound := True;
          Break; // from loop
        end;
      end;
      if not ltfLinkTableFound then
        CreateLinkedTable(lstJetProviderString, lstAccessTableName, ATableList.Item[i]);
      // count records
      GenerateMoveDataSQL(ATableList.Item[i]);
      if (FstMoveTableDataSQL.IndexOf(ATableList.Item[i]) > -1) then begin
        // connect to Access db to do direct counting
        FqryAllPurpose.Connection := FconnAccessDB;
        try
          FRecordCounts[i] := GetRecordCount(ATableList.Item[i], False);
          FTotalRecordsToTransfer := FTotalRecordsToTransfer + FRecordCounts[i];
        finally
          FqryAllPurpose.Connection := dmDatabase.dbLocal;
        end;
      end;
    end;
    FprgDataMigration.Position := i * 100 div (ATableList.Count-1);
    Application.ProcessMessages;
    if FCancelled then Exit;
  end;
  Log('Total records to transfer : ' + IntToStr(FTotalRecordsToTransfer), False);
  FprgDataMigration.Position := 0;
end;


{===============================================================================
  Transfer the Site ID from existing Access DB to new SQL Server DB.
}
procedure TDBMigrator.TransferSiteID;
begin
  FconnAccessDB.Execute('UPDATE DBO_Setting INNER JOIN Setting ON DBO_Setting.Name = Setting.Name '+
                        'SET DBO_SETTING.Data = Setting.Data '+
                        'WHERE DBO_SETTING.Name = ''SiteID''', cmdText);
end;

{-------------------------------------------------------------------------------
  Description : Creates a migration folder in the data folder on the server,
              so we are guaranteed to be able to find it from T-SQL.
  Created : 31/3/2003 }
procedure TDBMigrator.FindServerTempFolder;
begin
  with FqryAllPurpose do begin
    SQL.Text := 'SELECT Filename FROM SysFiles WHERE Name=''nbndata_data''';
    Open;
    try
      FSrvTempFolder := ExtractFilePath(FieldByName('Filename').AsString);
    finally
      Close;
    end;
    Log('Server temporary migration folder set to ' + FSrvTempFolder, False);
  end;
end;

{-------------------------------------------------------------------------------
  Description : To ensure that hierarchical tables come in OK, we drop the
              constraints and re add them later.  This method drops them.
  Created : 10/04/2003 }
procedure TDBMigrator.DropParentConstraints;
begin
  with dmDatabase.dbLocal do begin
    Execute(Format(SQL_DROP_PARENT_CONSTRAINTS, ['FK_LOCATION_LOCATION',
                                                 'LOCATION',
                                                 'FK_LOCATION_LOCATION']));
    Execute(Format(SQL_DROP_PARENT_CONSTRAINTS, ['FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM',
                                                 'TAXON_LIST_ITEM',
                                                 'FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM']));
    Execute(Format(SQL_DROP_PARENT_CONSTRAINTS, ['FK_BIOTOPE_LIST_ITEM_BIOTOPE_LIST_ITEM',
                                                 'BIOTOPE_LIST_ITEM',
                                                 'FK_BIOTOPE_LIST_ITEM_BIOTOPE_LIST_ITEM']));
  end;
end;

{-------------------------------------------------------------------------------
  Description : Re-add the constraints on hierarchical tables
  Created : 10/04/2003 }
procedure TDBMigrator.CreateParentConstraints;
begin
  with dmDatabase.dbLocal do begin
    Execute(Format(SQL_ADD_PARENT_CONSTRAINTS, ['FK_LOCATION_LOCATION',
                                                 'LOCATION',
                                                 'FK_LOCATION_LOCATION',
                                                 'PARENT_KEY',
                                                 'LOCATION',
                                                 'LOCATION_KEY']));
    Execute(Format(SQL_ADD_PARENT_CONSTRAINTS, ['FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM',
                                                 'TAXON_LIST_ITEM',
                                                 'FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM',
                                                 'PARENT',
                                                 'TAXON_LIST_ITEM',
                                                 'TAXON_LIST_ITEM_KEY']));
    Execute(Format(SQL_ADD_PARENT_CONSTRAINTS, ['FK_BIOTOPE_LIST_ITEM_BIOTOPE_LIST_ITEM',
                                                 'BIOTOPE_LIST_ITEM',
                                                 'FK_BIOTOPE_LIST_ITEM_BIOTOPE_LIST_ITEM',
                                                 'PARENT',
                                                 'BIOTOPE_LIST_ITEM',
                                                 'BIOTOPE_LIST_ITEM_KEY']));
  end;
end;

procedure TDBMigrator.Cancel;
begin
  FCancelled := True;
end;

end.
