{-------------------------------------------------------------------------------
  Unit:         AttachDB.pas

  Defines:      TAttachDB

  Description:  Provides a class required to attach the nbn data file to the
                local SQLExpress Server instance

  Author:       John van Breda
  Created:      16/01/2003

  Last revision information:
    $Revision: 17 $
    $Date: 4/05/09 14:35 $
    $Author: Ericsalmon $

  Copyright © Dorset Software Services Ltd, 2003

-------------------------------------------------------------------------------}

unit AttachDB;

interface

uses
  Sysutils, classes, VCLUnZip, Forms, Dialogs, ADODb, DB, GeneralFunctions, Functions,
  ComObj, Windows, ComCtrls, Logger, Controls, SetupConstants, Registry, SHFolder;

resourcestring
  ResStr_FailedToConnectToLocalServer =
      'Failed to connect to the SQL Server.  '#13#10#13#10
      + 'The error was as follows:'#13#10
      + '''%s''';

  ResStr_NBNDataAlreadyExists =
      'There is already a database named NBNData on the server you are installing onto.'#13#10#13#10
      + 'Do you want to overwrite it?';

  ResStr_CouldNotDeleteDatabase =
      'Could not delete the existing database.'#13#10#13#10
      + 'The error was as follows:'#13#10
      + '''%s''';

  ResStr_AttachDatabaseAborted = 'Attaching the database aborted';
  ResStr_DatabaseAlreadyExists = 'Database already exists';

  ResStr_MissingRegistryEntries =
      'Recorder 6 registry entries missing.'#13#10
      + 'Please ensure the Recorder 6 install process is successful before you install the database.';

type
  EAttachAborted = class(Exception);
  EAttachFailed  = class(Exception);

  TSetProgress = procedure (AValue: Integer) of object;

  TAttachDB = class
  private
    FServerName: String;
    FRegistryInstanceName: String; // the name used to refer to this instance in parts of the registry
    FSetProgress: TSetProgress;
    FUserName : string;
    FPassword : string;
    FTrusted  : boolean;
    FErrorMessage: String;
    FConnection : TADOConnection;
    procedure UnzipFile(const ASourceZip, ADestFile: string);
    procedure ZipTotalPercentDone(Sender: TObject; Percent: LongInt);
  public
    constructor Create(const AServerName, ARegistryInstanceName: String; ASetProgress: TSetProgress;
        const AUserName, APassword : string; ATrusted : boolean);
    destructor Destroy; override;
    function ConnectAndCheckDb(const AConfirmOverwrite: Boolean = True): Boolean;
    procedure CopyMDF(const AZipFileName: String; const destination: String = '');
    procedure InitDB(standalone: boolean; const ASiteID: String = '');
    property ErrorMessage: String read FErrorMessage;
  end;

//==============================================================================
implementation

uses ADOInt;

const

  // Error code for invalid username/password
  INT_INVALID_USERNAME_OR_PASSWORD = -2147203048;

resourcestring
  ResStr_InvalidUsernameOrPassword = 'Invalid username or password.';

//==============================================================================
constructor TAttachDB.Create(const AServerName, ARegistryInstanceName: String; ASetProgress: TSetProgress;
    const AUserName, APassword : string; ATrusted : boolean);
begin
  FServerName   := AServerName;
  FRegistryInstanceName := ARegistryInstanceName;
  FSetProgress  := ASetProgress;
  FUserName     := AUserName;
  FPassword     := APassword;
  FTrusted      := ATrusted;
  FErrorMessage := '';
end;

{===============================================================================
 Connect to the DB and check if there is an existing database to overwrite.
}
function TAttachDB.ConnectAndCheckDb(const AConfirmOverwrite: Boolean = True): Boolean;
var
  lSecurity : string;
  lPassword : string;
  lRecordset: _Recordset;
  lSuccess  : boolean;
begin
  FErrorMessage := '';
  Result := true; // Assume everything is going to be alright...
  // get a security string
  if FTrusted  then
    lSecurity := 'Integrated Security=SSPI;'
  else begin
    if FPassword = '' then
      lPassword := ''
    else
      lPassword := 'password=' + FPassword + ';';
    lSecurity := 'User ID=' + FUsername + ';' + lPassword;
  end;
  FConnection := TADOConnection.Create(nil);
  FConnection.ConnectionString := Format(CONNECTION_STRING, [lSecurity, FServerName]);
  try
    FConnection.Open;
  except
    on E: EOleException do begin
      FErrorMessage := Format(ResStr_FailedToConnectToLocalServer, [E.Message]);
      result := false;
      exit;
    end;
  end;
  lRecordSet := FConnection.Execute('SELECT * FROM SYSDATABASES WHERE NAME=''NBNData''');
  if not lRecordSet.EOF then begin
    if not AConfirmOverwrite or
       (MessageDlg(ResStr_NBNDataAlreadyExists, mtWarning, [mbYes, mbAbort], 0) = mrYes) then
    begin
      lSuccess := False;
      repeat
        try
          FConnection.Execute('DROP DATABASE NBNData');
          lSuccess := True;
        except on E:EOleException do
          if MessageDlg(Format(ResStr_CouldNotDeleteDatabase, [E.Message]),
             mtWarning, [mbRetry, mbAbort], 0)=mrAbort then
            raise EAttachAborted.Create(ResStr_AttachDatabaseAborted);
        end;
      until lSuccess
    end else
      raise EAttachAborted.Create(ResStr_AttachDatabaseAborted);
  end;
  lRecordset := nil;
end;

{===============================================================================
 Description : Copy the MDF file to the server data location and attach it
 Created : 06/1/2003}
procedure TAttachDB.CopyMDF(const AZipFileName: String; const destination: String = '');
var
  ltfFoundDB : boolean;
  lstDestPath, script : string;
  rs: _Recordset;
  affected: integer;
begin
  // load info for master db to get the file path, plus NBNData to check if it exists
  rs := FConnection.Execute('SELECT name, filename FROM master.dbo.sysdatabases WHERE name in (''master'', '''+DATABASE_NAME+''')');
  ltfFoundDB := rs.RecordCount > 1; // do we have more than just the master db?

  // Copy MDF over if required
  if not ltfFoundDB then
  begin
    if destination = '' then
      lstDestPath := ExtractFilePath(rs.Fields[1].Value)  // this will be the master db location
    else
      lstDestPath := destination;

    UnzipFile(AZipFileName, lstDestPath);

    // Delete old log file if it exists
    if FileExists(lstDestPath + MDF_FILENAME + '.ldf') then
      SysUtils.DeleteFile(lstDestPath + MDF_FILENAME + '.ldf');

    Log('Attaching file ' + lstDestPath + MDF_FILENAME + '.mdf', False);
    // Not keeping the log file.
    if FileExists(lstDestPath + LDF_FILENAME) then
      DeleteFile(PChar(lstDestPath + LDF_FILENAME));
    script := 'CREATE DATABASE ['+DATABASE_NAME+'] ON '+
        '( FILENAME = N'''+lstDestPath + MDF_FILENAME + '.mdf'' ) '+
        'FOR ATTACH';
    // Set a long timeout as sometimes create database takes some time.
    FConnection.CommandTimeout := 200;
    FConnection.Execute(script, affected);
  end else begin
    MessageDlg(ResStr_DatabaseAlreadyExists, mtWarning, [mbOk], 0);
    Log('Existing database file found', False);
  end;
end;

{===============================================================================
 Description : Initialise the database default user and backup device.
 Created : 06/1/2003 }
procedure TAttachDB.InitDB(standalone: boolean; const ASiteID: String = '');
var step: Single;
    sqlVersion: String;

  function GetBackupFolder: String;
  begin
    if GetOSVersion >= wvVista then
      with TRegistry.Create do
        try
          RootKey := HKEY_LOCAL_MACHINE;
          // if standalone, then we will put the backup in the common appdata folder
          // in a subfolder with the same name as the instance data folder
          if standalone and OpenKeyReadOnly(REG_KEY_R6) then
            Result :=
                GetFolder(CSIDL_COMMON_APPDATA)
                + ExtractFileName(ExcludeTrailingPathDelimiter(ReadString('Installation Path')))
                + PathDelim
          // otherwise put it in the SQL Server directory
          else if OpenKeyReadOnly(Format(REG_KEY_SQLSERVER_NAMED,
                      [FRegistryInstanceName])) then
            Result := ReadString(REG_BACKUP_DIRECTORY) + PathDelim
          else begin
            // check for 64 bit instance names as well
            Access := KEY_READ OR KEY_WOW64_64KEY;
            if OpenKey(Format(REG_KEY_SQLSERVER_NAMED,
                      [FRegistryInstanceName]), false) then
              Result := ReadString(REG_BACKUP_DIRECTORY) + PathDelim
            else
              raise EAttachFailed.Create(ResStr_MissingRegistryEntries);
          end;

        finally
          Free;
        end
    else
      Result := GetWindowsTempDir;
  end;

begin
  FSetProgress(0);
  // Value for progress bar.
  step := 100/7;
  with FConnection do
  begin
    Execute(Format(
        'IF NOT EXISTS(SELECT * FROM SysDevices WHERE Name=''NBNData_Backup'')'
        + ' EXEC sp_addumpdevice ''disk'', ''NBNData_Backup'', ''%sNBNBackup.bak''',
        [GetBackupFolder]));
    FSetProgress(Trunc(step));
    sqlVersion := Execute('SELECT SERVERPROPERTY(''productversion'')').Fields[0].Value;
    if Copy(sqlVersion, 1, 1) = '8' then
      Execute(
          'IF NOT EXISTS(SELECT * FROM SysLogins WHERE Name=''NBNUser'') '
          + 'EXEC sp_addlogin ''NBNUser'', ''NBNPassword'', ''NBNData''')
    else
      Execute(
          'IF NOT EXISTS(SELECT * FROM Master.dbo.SysLogins WHERE Name=''NBNUser'') '
          + 'CREATE LOGIN NBNUser WITH PASSWORD=''NBNPassword'', CHECK_POLICY=OFF');
    FSetProgress(Trunc(2 * step));
    Execute('EXEC sp_addsrvrolemember ''NBNUser'', ''dbCreator''');
    FSetProgress(Trunc(3 * step));
    Execute('EXEC sp_addsrvrolemember ''NBNUser'', ''diskAdmin''');
    FSetProgress(Trunc(4 * step));
    Execute('USE NBNData');
    FSetProgress(Trunc(5 * step));
    // Fixed orphaned user.
    Execute('EXEC sp_change_users_login ''Update_One'', ''NBNUser'', ''NBNUser''');
    FSetProgress(Trunc(6 * step));
    // Ensure some tables are clean
    Execute('DELETE	MAP_SHEET');
    Execute('DELETE	Computer_Map');
    Execute('DELETE	Base_Map');
    Execute('UPDATE	"User" SET First_Login = 1');
    // If Site ID provided, update database with it.
    if ASiteID = '' then
      Execute('UPDATE	Setting SET	Data = NULL WHERE	Name = ''SiteID''')
    else
      Execute('UPDATE	Setting SET	Data = ''' + ASiteID + ''' WHERE Name = ''SiteID''');
    FSetProgress(Trunc(7 * step));
  end;
end;

{===============================================================================
 Description : Unzips the database file (system\database on the CD) to the
               SQL Server data folder
 Created : 16/1/2003 }
procedure TAttachDB.UnzipFile(const ASourceZip, ADestFile: string);
begin
  FSetProgress(0);
  Log('Unzipping file ' + ASourceZip + ' to ' + ADestFile, False);
  with TVCLUnZip.Create(nil) do
    try
      // Setup the progress bar feedback
      OnTotalPercentDone := ZipTotalPercentDone;
      ZipName            := ASourceZip;
      ReadZip;
      FilesList.Add(MDF_FILENAME + '.mdf');
      DestDir          := ADestFile;
      RecreateDirs     := False;
      RetainAttributes := True;
      OverwriteMode    := Always;
      UnZip;
    finally
      Free;
    end;
end;

//==============================================================================
procedure TAttachDB.ZipTotalPercentDone(Sender: TObject; Percent: Integer);
begin
  if Assigned(FSetProgress) then FSetProgress(Percent);
  // Asking unzip causes cursor to be turned back to default, from within component,
  // So turn it back to hourglass again. Only needs to be done once, but this is the
  // only appropriate place to do it, without adding extra rubbish to the code.
  Screen.Cursor := crHourglass;
end;

//==============================================================================
destructor TAttachDB.Destroy;
begin
  if Assigned(FConnection) then begin
    FConnection.Close;
    FConnection.Free;
  end;
  inherited;
end;

end.
