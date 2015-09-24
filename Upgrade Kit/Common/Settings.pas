{===============================================================================
  Unit:           Settings

  Defines:        TSettings

  Description:    Class to hold settings that are persistent across the
                  application

  Created:        29/5/2003

  Last revision information:
    $Revision: 15 $
    $Date: 23/02/09 14:24 $
    $Author: Andrewkemp $

===============================================================================}
unit Settings;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, ADODb,
  Registry, ExceptionForm, GeneralFunctions, SetupConstants;

resourcestring
  ResStr_RecorderUpgrade    = 'Recorder Application Upgrade';
  ResStr_DictionaryUpgrade  = 'Recorder Dictionary Upgrade';
  ResStr_WorkstationUpgrade = 'Recorder Workstation Application Upgrade';

  ResStr_BadInstall =
      'Recorder is not correctly installed on this machine.  Upgrade cannot proceed.';
  ResStr_BackdropFileMissing =
      'The backdrop image file is missing - upgrade kit incomplete and cannot proceed.';
  ResStr_AlreadyRunningOnThisMachine =
      'Recorder is already running on this machine.  Please close the application'#13
      + 'before running the upgrade.';

type
  ESettingsError = class (TExceptionPath)
  end;
  
  TSettings = class (TObject)
  private
    FAddinPath: String;
    FBaseMapPath: string;
    FDatabaseName: string;
    FInstallationPath: string;
    FMapFilePath: string;
    FPassword: string;
    FServerName: string;
    FUserName: string;
    FDatabasePath: string;
    FSequenceSettingName: string;
    FScriptsOnly: Boolean;
    FDatabaseDescription: string;
    FDBUpdated: Boolean;
    FTrustedSecurity: Boolean;
    FIntroMessage: string;
    FReportFilePath: string;
    procedure ReadRegistry;
    procedure SetPassword(const Value: string);
    procedure SetUserName(const Value: string);
    procedure SetSequenceSettingName(const Value: string);
    procedure SetScriptsOnly(Value: Boolean);
    procedure SetDatabaseDescription(const Value: string);
    procedure SetTrustedSecurity(Value: Boolean);
    procedure CorrectInstallationPath;
    procedure SetDBUpdated(const Value: Boolean);
    procedure SetIntroMessage(const Value: string);
  public
    constructor Create;
    function GetConnectionString: string;
    property AddinPath: String read FAddinPath;
    property BaseMapPath: string read FBaseMapPath;
    property DatabasePath: string read FDatabasePath;
    property InstallationPath: string read FInstallationPath;
    property MapFilePath: string read FMapFilePath;
    property Password: string read FPassword write SetPassword;
    property ServerName: string read FServerName;
    property UserName: string read FUserName write SetUserName;
    property SequenceSettingName: string read FSequenceSettingName write SetSequenceSettingName;
    property ScriptsOnly: Boolean read FScriptsOnly write SetScriptsOnly;
    property DatabaseDescription: string read FDatabaseDescription write SetDatabaseDescription;
    property TrustedSecurity: Boolean read FTrustedSecurity write SetTrustedSecurity;
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property DBUpdated: Boolean read FDBUpdated write SetDBUpdated;
    property IntroMessage: string read FIntroMessage write SetIntroMessage;
    property ReportFilePath: String read FReportFilePath;
   end;

//==============================================================================
implementation

const
  // Connection String for main SQL Server DB
  CONNECTION_STRING = 'Provider=SQLOLEDB.1;%sPersist Security Info=False;' +
                      'Data Source=%s;Initial Catalog=%s;OLE DB Services=-2';
  //OLE DB Services=-2 turns off connection pooling which can cause confusing errors
  //when sp_SetAppRole is used.
  INTEGRATED_SECURITY = 'Integrated Security=SSPI;';
  SQL_SECURITY = 'User ID=%s;password=%s;';
  OPT_DICTIONARY_LOCALE = 'Dictionary Locale';
  OPT_INSTALLATION_PATH = 'Installation Path';
  REG_PATH = 'SOFTWARE\Dorset Software\Recorder 6';
  REPORT_FILE_PATH = 'Report Path';

{-==============================================================================
    TSettings
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSettings.Create;
begin
  inherited Create;

  ReadRegistry;
  DBUpdated := False;
end;  // TSettings.Create

{-------------------------------------------------------------------------------
  Returns a connection string for access to the NBNData volume.
  Depends on the ATrustedConnection parameter (if true then integrated security used).
}
function TSettings.GetConnectionString: string;
var
  lstSecurityInfo: string;
begin
  if FTrustedSecurity then
    lstSecurityInfo := INTEGRATED_SECURITY
  else
    lstSecurityInfo := Format(SQL_SECURITY, [FUserName, FPassword]);
  Result := Format(CONNECTION_STRING, [lstSecurityInfo, FServerName, FDatabaseName]);
end;  // TSettings.GetConnectionString

{-------------------------------------------------------------------------------
  Reads the database name and server name from the registry.
}
procedure TSettings.ReadRegistry;
var
  lReg: TRegistry;
begin
  lReg := TRegistry.Create;
  try
    // Read off Local Machine key.
    lReg.Rootkey := HKEY_LOCAL_MACHINE;
    if not lReg.OpenKeyReadOnly(REG_PATH) then begin
      ShowInformation(ResStr_BadInstall);
      Application.Terminate;
    end;
    FAddinPath := lReg.ReadString('Addin Path');
    FServerName := lReg.ReadString('Server Name');
    FDatabaseName := lReg.ReadString('Database Name');
    FInstallationPath := lReg.ReadString(OPT_INSTALLATION_PATH);
    CorrectInstallationPath;
    // Read off Current User key.
    lReg.RootKey := HKEY_CURRENT_USER;
    if not lReg.OpenKeyReadOnly(REG_PATH + '\Settings') then begin
      ShowInformation(ResStr_BadInstall);
      Application.Terminate;
    end;
    FBaseMapPath := lReg.ReadString('Base Map Path');
    FMapFilePath := lReg.ReadString('Map File Path');
    FReportFilePath := lReg.ReadString(REPORT_FILE_PATH);
    // pick up the database from the registry.  This won't exist if Recorder
    // not run yet, so work it out.
    if lReg.KeyExists('Database Path') then
      FDatabasePath := lReg.ReadString('Database Path')
    else
      if FileExists(IncludeTrailingPathDelimiter(InstallationPath) + '\Database\nbndata.mdb') then
        FDatabasePath :=  IncludeTrailingPathDelimiter(InstallationPath) +
            '\Database\nbndata.mdb';
  finally
    lReg.Free;
  end;
end;  // TSettings.ReadRegistry

{-------------------------------------------------------------------------------
}
procedure TSettings.SetPassword(const Value: string);
begin
  FPassword := Value;
end;  // TSettings.SetPassword

{-------------------------------------------------------------------------------
  Name of the entry in Settings table for tracking
}
procedure TSettings.SetSequenceSettingName(const Value: string);
begin
  FSequenceSettingName := Value;
end;

{-------------------------------------------------------------------------------
}
{-------------------------------------------------------------------------------
}
procedure TSettings.SetUserName(const Value: string);
begin
  FUserName := Value;
end;  // TSettings.SetUserName 

{-------------------------------------------------------------------------------
  Upgrader configured for database upgrade only?  E.g a dict upgrade.
}
procedure TSettings.SetScriptsOnly(Value: Boolean);
begin
  FScriptsOnly := Value;
end;

{-------------------------------------------------------------------------------
  On screen label to describe the nature of the database update
}
procedure TSettings.SetDatabaseDescription(const Value: string);
begin
  FDatabaseDescription := Value;
end;

{-------------------------------------------------------------------------------
  Accessor.  Set by login frame.
}
procedure TSettings.SetTrustedSecurity(Value: Boolean);
begin
  FTrustedSecurity := Value;
end;

{-------------------------------------------------------------------------------
  Correct a bug in previous install kits, where this pointed to the
    workstation folder instead of the server folder
}
procedure TSettings.CorrectInstallationPath;
begin
  if not FileExists(FInstallationPath + STR_RECORDER_MAIN_OLD) then
  begin
    // use Addin path to correct it
    FInstallationPath := ExtractFilePath(Copy(FAddinPath, 1, Length(FAddinPath) - 2));
    if FileExists(FInstallationPath + STR_RECORDER_MAIN_OLD) then
      with TRegistry.Create do
        try
          Rootkey := HKEY_LOCAL_MACHINE;
          if OpenKey(REG_PATH, False) then
            WriteString('Installation Path', FInstallationPath);
        finally
          free;
        end; // try
  end;
end;

{-------------------------------------------------------------------------------
  Accessor. Where any db scripts run?
}
procedure TSettings.SetDBUpdated(const Value: Boolean);
begin
  FDBUpdated := Value;
end;

(**
 * Accessor for the message to appear on the first page when no requirement to login.
 *)
procedure TSettings.SetIntroMessage(const Value: string);
begin
  FIntroMessage := Value;
end;

end.
