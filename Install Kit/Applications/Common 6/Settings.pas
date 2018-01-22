{===============================================================================
  Unit:        Settings

  Defines:     TSettings

  Description:

  Model:

  Created:     March 2004

  Last revision information:
    $Revision: 23 $
    $Date: 6/07/09 9:06 $
    $Author: Ericsalmon $

===============================================================================}

unit Settings;

interface

uses
  Windows, Controls, Classes, Dialogs, SysUtils, SQLList, Registry, SetupConstants,
  TextMessages, GeneralFunctions, ShlObj, ActiveX, APIUtils, ShellAPI, Forms, ComCtrls,
  ExceptionForm, VCLUnzip;

type
  EInstallSettingsError = class(TExceptionPath)
  end;

  TInstallType = (itStandalone, itServer, itWorkstation, itUpgrade, itUninstall);

  // Install mode. The setup has various options and various routes through it.
  TMode = (moUnknown, moInstallDatabase, moInstallR6,
           moMigrate, moUninstallR2K2);

  // State of install for SQLExpress
  TInstallState = (isNotInstalled, isInstalling, isInstalled);

  TSettings = class(TObject)
  private
    FAddinsInstalled: TStringList;
    FAvailableSpatialRefSystems: TStringList;
    FCancelled: Boolean;
    FComplete: Boolean;
    FComputerName: String;
    FDatabasePath: String;
    FFilesInstalled: TStringList;
    FFoldersCreated: TStringList;
    FInstallFolder: String;
    FInstallType: TInstallType;
    FLocalInstanceNames: TStringList;
    FLocalInstanceNamesChecked: Boolean;
    FMode: TMode;
    FSQLExpressInstallState: TInstallState;
    FPassword: String;
    FRedoSQLExpressInstall: Boolean;
    FRegistryInstalled: Boolean;
    FRegistrySiteID: Boolean;
    FRootFolder: String;
    FServerName: String;
    FShortcutsInstalled: Boolean;
    FSiteID: String;
    FSpatialRefSystem: String;
    FSpecifyOwnDatabasePath: Boolean;
    FSpecifyOwnInstanceName: Boolean;
    FSpecifyOwnInstancePath: Boolean;
    FSQLInstanceNames: TStringList;
    FSQLInstanceNamesChecked: Boolean;
    FTrustedLogin: Boolean;
    FUserName: String;
    FVerificationKey: String;
    FOSVersion: TOSVersion;
    FResourcesLib: THandle;
    FInstalledInstanceName: String;
    FSkipToUninstall: Boolean;
    FRemoveSystemComponents: boolean;
    procedure CheckForExistingSiteID;
    function GetAvailableSpatialRefSystems: TStringList;
    function GetDatabaseSetupCommand: String;
    function GetDatabaseSetupCommandWithAuth: String;    
    function GetLocalInstanceNames: TStringList;
    function GetSQLInstanceNames: TStringList;
    procedure LoadAvailableSpatialRefSystems;
    procedure SetComputerName;
    procedure SetInstallFolder(const Value: String);
    procedure SetSiteID(const Value: String);
    procedure SetMode(const Value: TMode);
    procedure SetTrustedLogin(Value: Boolean);
    function GetRegistryInstanceName: string;
    procedure SetRemoveSystemComponents(const Value: boolean);
  public
    constructor Create(AInstallType: TInstallType); reintroduce; overload;
    destructor Destroy; override;
    procedure AddAddinName(const AAddinName: String);
    procedure AddFileName(const AFileName: String);
    procedure AddFolderName(const AFolderName: String);
    procedure DeleteLoggedFiles;
    procedure DeleteLoggedFolders;
    function FolderExists(const AFolderName: String; ForceCreate: Boolean = True; MustExists:
        Boolean = True; const AMessage: String = ''): Boolean;
    function ForceFolders(AFolder: String): Boolean;
    function GetProgramFilesDir: String;
    function GetConnectionString: string;
    function ProperServerName: String;
    function RequiredDiskSpace: Int64;
    function RequiredApplicationDiskSpace: Int64;
    function RequiredDatabaseDiskSpace: Int64;
    function SystemComponentsInstalled: Boolean;
    procedure SetAnimationFromResource(ctrl: TAnimate; const resName: String);
    procedure SaveLogToDisk;
    procedure SaveToIniFile;
    procedure SaveSiteID;
    property AddinsInstalled: TStringList read FAddinsInstalled;
    property AvailableSpatialRefSystems: TStringList read GetAvailableSpatialRefSystems;
    property Cancelled: Boolean read FCancelled write FCancelled;
    property Complete: Boolean read FComplete write FComplete;
    property ComputerName: String read FComputerName;
    property DatabasePath: String read FDatabasePath write FDatabasePath;
    property DatabaseSetupCommand: String read GetDatabaseSetupCommand;
    property DatabaseSetupCommandWithAuth: String read GetDatabaseSetupCommandWithAuth;
    property FilesInstalled: TStringList read FFilesInstalled;
    property FoldersCreated: TStringList read FFoldersCreated;
    property InstallFolder: String read FInstallFolder write SetInstallFolder;
    property InstallType: TInstallType read FInstallType;
    property InstalledInstanceName: String read FInstalledInstanceName write FInstalledInstanceName;
    property LocalInstanceNames: TStringList read GetLocalInstanceNames;
    property Mode: TMode read FMode write SetMode;
    property SQLExpressInstallState: TInstallState read FSQLExpressInstallState write FSQLExpressInstallState;
    property OSVersion: TOSVersion read FOSVersion;
    property Password: String read FPassword write FPassword;
    property RedoSQLExpressInstall: Boolean read FRedoSQLExpressInstall write FRedoSQLExpressInstall;
    property RegistryInstalled: Boolean read FRegistryInstalled write FRegistryInstalled;
    property RegistryInstanceName: string read GetRegistryInstanceName;
    property RegistrySiteID: Boolean read FRegistrySiteID write FRegistrySiteID;
    property ResourcesLib: THandle read FResourcesLib write FResourcesLib;
    property RootFolder: String read FRootFolder write FRootFolder;
    property ServerName: String read FServerName write FServerName;
    property ShortcutsInstalled: Boolean read FShortcutsInstalled write FShortcutsInstalled;
    property SiteID: String read FSiteID write SetSiteID;
    property SkipToUninstall: Boolean read FSkipToUninstall write FSkipToUninstall;
    property SpatialRefSystem: String read FSpatialRefSystem write FSpatialRefSystem;
    property SpecifyOwnDatabasePath: Boolean read FSpecifyOwnDatabasePath write FSpecifyOwnDatabasePath;
    property SpecifyOwnInstanceName: Boolean read FSpecifyOwnInstanceName write
        FSpecifyOwnInstanceName;
    property SpecifyOwnInstancePath: Boolean read FSpecifyOwnInstancePath write
        FSpecifyOwnInstancePath;
    property SQLInstanceNames: TStringList read GetSQLInstanceNames;
    property TrustedLogin: Boolean read FTrustedLogin write SetTrustedLogin;
    property UserName: String read FUserName write FUserName;
    property VerificationKey: String read FVerificationKey write FVerificationKey;
    property RemoveSystemComponents: boolean read FRemoveSystemComponents write SetRemoveSystemComponents;
  end;
  
//==============================================================================
implementation

uses
  Functions;

{-==============================================================================
    TSettings
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSettings.Create(AInstallType: TInstallType);
begin
  inherited Create;

  FInstallType := AInstallType;

  FFilesInstalled             := TStringList.Create;
  FFilesInstalled.Sorted      := True;
  FFilesInstalled.Duplicates  := dupIgnore;
  FFoldersCreated             := TStringList.Create;
  FFoldersCreated.Sorted      := True;
  FFoldersCreated.Duplicates  := dupIgnore;
  FAddinsInstalled            := TStringList.Create;
  FAddinsInstalled.Sorted     := True;
  FAddinsInstalled.Duplicates := dupIgnore;

  FMode := moUnknown;

  // Default settings
  FCancelled := False;
  FComplete  := False;

  { Default values for database setup. Allows automatic login on newly installed SQLExpress
    instance, or SQL Server if that login hasn't been removed. Trusted login should also
    work. }
  FUserName := 'sa';
  FPassword := '';

  FSpecifyOwnInstanceName := False;
  FSpecifyOwnInstancePath := False;

  FLocalInstanceNames        := TStringList.Create;
  FLocalInstanceNamesChecked := False;
  FSQLInstanceNames          := TStringList.Create;
  FSQLInstanceNamesChecked   := False;
  FRedoSQLExpressInstall     := False;
  
  FSQLExpressInstallState := isNotInstalled;
  FRegistryInstalled  := False;
  FShortcutsInstalled := False;
  
  FRootFolder := ExtractFilePath(Application.ExeName);
  case InstallType of
    itServer:      FInstallFolder := ExtractFileDrive(ProgramFilesFolder) + '\' + DEFAULT_SERVER_FOLDER;
    itWorkstation: FInstallFolder := ProgramFilesFolder + DEFAULT_WORKSTATION_FOLDER;
  else
    FInstallFolder := ProgramFilesFolder + DEFAULT_INSTALL_FOLDER;
  end;

  CheckForExistingSiteID;
  
  FAvailableSpatialRefSystems := TStringList.Create;

  FOSVersion := GetOSVersion;
  FResourcesLib := HInstance;
  FTrustedLogin := false;

  SetComputerName;
end;  // TSettings.Create

{-------------------------------------------------------------------------------
}
destructor TSettings.Destroy;
begin
  if FResourcesLib <> HInstance then FreeLibrary(FResourcesLib);

  FAvailableSpatialRefSystems.Free;
  FAddinsInstalled.Free;
  FFoldersCreated.Free;
  FFilesInstalled.Free;
  FLocalInstanceNames.Free;
  FSQLInstanceNames.Free;
  inherited;
end;  // TSettings.Destroy 

{-------------------------------------------------------------------------------
}
procedure TSettings.AddAddinName(const AAddinName: String);
begin
  FAddinsInstalled.Add(AAddinName);
end;  // TSettings.AddAddinName 

{-------------------------------------------------------------------------------
}
procedure TSettings.AddFileName(const AFileName: String);
begin
  FFilesInstalled.Add(AFileName);
end;  // TSettings.AddFileName 

{-------------------------------------------------------------------------------
}
procedure TSettings.AddFolderName(const AFolderName: String);
begin
  FFoldersCreated.Add(AFolderName);
end;  // TSettings.AddFolderName

{-------------------------------------------------------------------------------
}
procedure TSettings.DeleteLoggedFiles;
var i: Integer;
    lszFileName: PChar;
begin
  // Reset any Read-Only flags first, in case it interferes with DeleteFile.
  ResetReadOnlyFlag(FFilesInstalled);
  Application.ProcessMessages;

  for i := FFilesInstalled.Count - 1 downto 0 do begin
    if FileExists(FFilesInstalled[i]) then
      if not DeleteFile(FFilesInstalled[i]) and
        (Win32Platform = VER_PLATFORM_WIN32_NT) then
      begin
        GetMem(lszFileName, Length(FFilesInstalled[i]) + 1);
        StrPCopy(lszFileName, FFilesInstalled[i]);
        try
          MoveFileEx(lszFileName, nil, MOVEFILE_DELAY_UNTIL_REBOOT);
        except
          on Exception do;
        end;
        FreeMem(lszFileName, Length(FFilesInstalled[i]) + 1);
      end;
    Application.ProcessMessages;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TSettings.DeleteLoggedFolders;
var i: Integer;
begin
  for i := FFoldersCreated.Count - 1 downto 0 do begin
    RemoveFileOrFolder(FFoldersCreated[i]);
    Application.ProcessMessages;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TSettings.CheckForExistingSiteID;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      if OpenKeyReadOnly(REG_KEY_R2K2_SETTINGS) then begin
        // use the key specified for the upgrade if available
        if ValueExists('Upgrade Site ID') then
          SiteID := ReadString('Upgrade Site ID')
        else
          SiteID := ReadString('Site ID');
        CloseKey;
      end;
    finally
      Free;
    end;
  FRegistrySiteID := FSiteID <> '';
end;  // TSettings.CheckForExistingSiteID

{-------------------------------------------------------------------------------
}
function TSettings.FolderExists(const AFolderName: String; ForceCreate: Boolean = True;
    MustExists: Boolean = True; const AMessage: String = ''): Boolean;
var
  displayMessage: String;
begin
  if AMessage = '' then
    displayMessage := ResStr_CreateMissingFolder
  else
    displayMessage := AMessage;
  Result := False;
  if Cancelled then Exit;
  // If folder exists already, no need to do anything else
  if DirectoryExists(AFolderName) then
    Result := True
  else
  // Check it's a drive or not
  if (Length(AFolderName) < 3) and (AFolderName[2] <> ':') then
    MessageDlg(ResStr_InvalidFolderName, mtWarning, [mbOk], 0)
  else
  // If it looks like it's a valid path...
  if ForceCreate then begin
    if MessageDlg(displayMessage, mtInformation, [mbYes, mbNo], 0) = mrYes then
      try
        if ForceFolders(AFolderName) then
          Result := True
        else
          MessageDlg(ResStr_FolderCreateFailed, mtWarning, [mbOk], 0)
      except
        // If fodler name is complete nonsense, Windows will say so with an
        // exception, so trap it and deal with it.
        on EInOutError do
          MessageDlg(ResStr_InvalidFolderName, mtWarning, [mbOk], 0);
      end
    else
      MessageDlg(ResStr_FolderMustExist, mtWarning, [mbOk], 0);
  end else
  if MustExists then
    MessageDlg(ResStr_FolderMustExist, mtWarning, [mbOk], 0);
end;  // TSettings.FolderExists 

{-------------------------------------------------------------------------------
}
function TSettings.ForceFolders(AFolder: String): Boolean;
begin
  Result := False;
  if Cancelled then Exit;

  Result := True;

  // So that we can use the ExtractFilePath trick on folder name...
  AFolder := ExcludeTrailingPathDelimiter(AFolder);

  // Folder already there, nothing else to do. Don't log it, we didn't create that one.
  if DirectoryExists(AFolder) then Exit;

  // Log created folder, so it can be properly removed during uninstall.
  AddFolderName(AFolder);

  // Get here if folder doesn't already exist. Recurse back to top/root folder.
  Result := ForceFolders(ExtractFilePath(AFolder)) and CreateDir(AFolder);
end;  // TSettings.ForceFolders

{-------------------------------------------------------------------------------
  Gets the command line required to fire the database setup app, excluding the
  authentication details.
}
function TSettings.GetDatabaseSetupCommand: String;
begin
  Result := Format(
        '"%sSystem\DatabaseSetup.exe" /standalone "/installdir:%s"',
        [RootFolder, InstallFolder]);
  if SpecifyOwnDatabasePath then
    Result := Format ('%s "/databasedir:%s"', [Result, DatabasePath]);
end;

{-------------------------------------------------------------------------------
  Gets a full version of the database setup command line, including authentication
  details. This version should not be stored anywhere.
}
function TSettings.GetDatabaseSetupCommandWithAuth: String;
begin
  Result := GetDatabaseSetupCommand;
  if not FTrustedLogin then
    // not using a trusted login, so the database setup needs to know the login we
    // used to connect to the database
    Result := Result + Format(' /username:%s /password:%s', [FUsername, FPassword]);
  // no need to store the trusted login setting, as that comes in the installsettings.ini file.
end;  // TSettings.GetDatabaseSetupCommand

{-------------------------------------------------------------------------------
}
function TSettings.GetProgramFilesDir: String;
begin
  Result := 'C:\Program Files\';  // default as old Windows versions don't store it in the registry
  with TRegistry.Create do
    try
      Rootkey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion') then begin
        if ValueExists('ProgramFilesDir') then
          Result := ReadString('ProgramFilesDir') + '\';
      end;
    finally
      CloseKey;
      Free;
    end;
end;

{-------------------------------------------------------------------------------
}
function TSettings.GetLocalInstanceNames: TStringList;
var
  values, output: TStringList;
  i: integer;
  reg: TRegistry;

    procedure LoadInstances;
    var
      names: TStringList;
    begin
      names := TStringList.Create;
      try
        // because we set the access explicitly, call OpenKey rather than OpenKeyReadOnly
        // as this honours our setting
        if reg.OpenKey(REG_KEY_SQL2005_INSTANCES, false) then
          reg.GetValueNames(names);
        values.AddStrings(names);
      finally
        names.free;
      end;
    end;

begin
  values := TStringList.Create;
  try
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      reg.Access  := KEY_READ;
      if reg.KeyExists(REG_KEY_SQL2005_INSTANCES) then
        LoadInstances;
      if GetOSVersion > wvWin2K then begin
        // if higher than windows 2K, then we need to check 64 bit registry as well.
        reg.Access  := KEY_READ OR KEY_WOW64_64KEY;
        LoadInstances;
      end;
    finally
      reg.Free;
    end;
    output := TStringList.Create;
    output.Sorted := true;
    output.Duplicates := dupIgnore;
    for i := 0 to values.Count-1 do begin
      if values[i] = DEFAULT_LOCAL_INSTANCE then
        output.Add(ComputerName)
      else
        output.Add(ComputerName + '\' + values[i]);
    end;
  finally
    values.free;
  end;
  result := output;
  FLocalInstanceNamesChecked := True;
end;  // TSettings.GetLocalInstanceNames

{-------------------------------------------------------------------------------
}
function TSettings.GetSQLInstanceNames: TStringList;
var
  lCursor: TCursor;
begin
  if not FSQLInstanceNamesChecked then begin
    lCursor := HourglassCursor;
    try
      PopulateServerList(FSQLInstanceNames);
    finally
      DefaultCursor(lCursor);
    end;
  end;
  Result := FSQLInstanceNames;
  FSQLInstanceNamesChecked := True;
end;  // TSettings.GetSQLInstanceNames

{-------------------------------------------------------------------------------
}
function TSettings.GetAvailableSpatialRefSystems: TStringList;
begin
  if FAvailableSpatialRefSystems.Count = 0 then
    LoadAvailableSpatialRefSystems;
  Result := FAvailableSpatialRefSystems;
end;  // TSettings.GetAvailableSpatialRefSystems

{-------------------------------------------------------------------------------
}
procedure TSettings.LoadAvailableSpatialRefSystems;
var
  i: Integer;
  lFileName: String;
begin
  if InstallType = itWorkstation then
    lFileName := RootFolder + STR_SPATIAL_SYSTEMS_WKS
  else
    lFileName := RootFolder + STR_SPATIAL_SYSTEMS_STD;

  if not FileExists(lFileName) then
    raise EInstallSettingsError.Create(ResStr_SpatialRefSystemsError);
  
  FAvailableSpatialRefSystems.LoadFromFile(lFileName);
  // Remove all the items not of the form "Name=Value" before adding them to the
  // listbox
  i := 0;
  while i <> FAvailableSpatialRefSystems.Count do
    if Pos('=', FAvailableSpatialRefSystems[0]) = 0 then
      FAvailableSpatialRefSystems.Delete(i)
    else
      Inc(i);
end;  // TSettings.LoadAvailableSpatialRefSystems

{-------------------------------------------------------------------------------
}
function TSettings.ProperServerName: String;
begin
  // Server specified, use it (it can be a local instance, default or not)
  if ServerName <> '' then
    Result := ServerName
  else
    Result := '';
end;  // TSettings.ProperServerName

{-------------------------------------------------------------------------------
}
function TSettings.RequiredDiskSpace: Int64;
var
  lRecSize, lDBSize: Int64;
begin
  lRecSize := 0;
  lDBSize  := 0;
  if Mode = moInstallR6 then
    lRecSize := RequiredApplicationDiskSpace;

  // Also want database size for standalone install.
  if (Mode = moInstallDatabase) or (InstallType = itStandalone) then
    lDBSize := RequiredDatabaseDiskSpace;

  Result := lRecSize + lDBSize;
end;  // TSettings.RequiredDiskSpace

{-------------------------------------------------------------------------------
  Gets the disk space required for the installation of application components.
}
function TSettings.RequiredApplicationDiskSpace: Int64;
begin
  // Default for just Recorder
  if InstallType = itWorkstation then
    Result := 5242880    // 5 MB for Workstation (Base Maps mostly).
  else
    Result := 26214400;  // 25MB for Recorder itself.;
end;

{-------------------------------------------------------------------------------
  Gets the disk space required for the installation of database components.
}
function TSettings.RequiredDatabaseDiskSpace: Int64;
var
  unzip: TVCLUnzip;
  i: Integer;
  total: Int64;
begin
  // Get the amount of space for Database out of zip file.
  unzip := TVCLUnzip.Create(nil);
  with unzip do
    try
      if InstallType in [itServer, itStandalone] then
        ZipName := ExtractFilePath(Application.ExeName) + STR_NBNDATA_ZIP_STD
      else
        ZipName := ExtractFilePath(Application.ExeName) + STR_NBNDATA_ZIP_WKS;
      ReadZip;
      total := 0;
      for i := 0 to Count - 1 do
        total := total + UnCompressedSize[i];
      // Size needed for DB + 10% margin.
      Result := Round(total * 1.1);
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
}
procedure TSettings.SaveLogToDisk;
var
  idx: Integer;
begin
  // Write log file only if there is something to write there.
  if (AddinsInstalled.Count > 0) or (FilesInstalled.Count > 0) or
     (FoldersCreated.Count > 0) or (RegistryInstalled) or
     (SQLExpressInstallState <> isNotInstalled) or Complete then
  begin
    AddFileName(InstallFolder + STR_INSTALLLOG);
    with TStringList.Create do
      try
        Add(STR_INSTALL_MODE);
        case InstallType of
          itStandalone : Add('Standalone');
          itServer     : Add('Server');
          itWorkstation: Add('Workstation');
        end;

        // Section to contain Path + FileName of installed addins.
        Add('');
        Add(STR_ADDINS_SECTION);
        AddStrings(AddinsInstalled);

        // Flag for Shortcuts, if created.
        if ShortcutsInstalled then begin
          Add('');
          Add(STR_SHORTCUTS_SECTION);
        end;

        if DatabasePath <> '' then begin
          idx := FoldersCreated.IndexOf(ExcludeTrailingPathDelimiter(DatabasePath));
          if idx > -1 then FoldersCreated.Delete(idx);
        end;

        // Section to contain Names of folders created.
        Add('');
        Add(STR_FOLDERS_SECTION);
        AddStrings(FoldersCreated);

        // Section to contain Path + FileName of files installed.
        Add('');
        Add(STR_FILES_SECTION);
        AddStrings(FilesInstalled);
        Add(InstallFolder + STR_INSTALLSETTINGS);

        // Flag for registry settings, if app had time to create some
        if RegistryInstalled then begin
          Add('');
          Add(STR_REGISTRY_SECTION);
        end;
        ForceFolders(InstallFolder);
        SaveToFile(InstallFolder + STR_INSTALLLOG);
      finally
        Free;
      end;
  end;
end;  // TSettings.SaveLogToDisk 

{-------------------------------------------------------------------------------
}
procedure TSettings.SaveToIniFile;
begin
  with TStringList.Create do
    try
      Add('SiteID=' + SiteID);
      Add('Database Name=' + DATABASE_NAME);
      Add('Server Name=' + ProperServerName);
      Add('Trusted Security=' + IntToStr(Ord(TrustedLogin)));

      ForceFolders(InstallFolder);
      SaveToFile(InstallFolder + STR_INSTALLSETTINGS);
      AddFileName(InstallFolder + STR_INSTALLSETTINGS);
    finally
      Free;
    end;
end;  // TSettings.SaveToIniFile

{-------------------------------------------------------------------------------
  Allow just the Site ID to be saved back to the registry.
}
procedure TSettings.SaveSiteID;
begin
  with TRegistry.Create do
  try
    if OpenKey(REG_KEY_R2K2_SETTINGS, True) then begin
      WriteString('Upgrade Site ID', SiteID);
      CloseKey;
    end;
  finally
    Free;
  end;
end;

{-------------------------------------------------------------------------------
  Because of a bug in TAnimate, need to trap the exception so the other property
  can be set and the animation loaded properly.
}
procedure TSettings.SetAnimationFromResource(ctrl: TAnimate; const resName: String);
begin
  try
    ctrl.ResName := resName;
  except
    on Exception do
      ctrl.ResHandle := ResourcesLib;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TSettings.SetComputerName;
var
  lComputerName: Array[0..MAX_COMPUTERNAME_LENGTH + 1] of Char;
  lSize: Cardinal;
begin
  lSize := MAX_COMPUTERNAME_LENGTH + 1;
  GetComputerName(lComputerName, lSize);
  FComputerName := lComputerName;
end;  // TSettings.SetComputerName

{-------------------------------------------------------------------------------
}
procedure TSettings.SetInstallFolder(const Value: String);
begin
  // Make sure the last character is '\'
  FInstallFolder := IncludeTrailingPathDelimiter(Value);
end;  // TSettings.SetInstallFolder

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TSettings.SetSiteID(const Value: String);
begin
  FSiteID := Value;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TSettings.SetMode(const Value: TMode);
begin
  FMode := Value;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TSettings.SetTrustedLogin(Value: Boolean);
begin
  FTrustedLogin := Value;
end;

function TSettings.GetConnectionString: string;
var
  security, password: string;
begin
  // get a security string
  if FTrustedLogin  then
    security := 'Integrated Security=SSPI;'
  else begin
    if FPassword = '' then
      password := ''
    else
      password := 'password=' + FPassword + ';';
    security := 'User ID=' + FUsername + ';' + password;
  end;
  result := Format(CONNECTION_STRING, [security, FServerName]);
end;

{-------------------------------------------------------------------------------
  Retrieve the registry version of a user entered instance name.  In SQL 2005
    these are not the same!
}              
function TSettings.GetRegistryInstanceName: string;
var
  instanceName: string;
begin
  instanceName := Copy(FServerName, Length(ComputerName) + 2, 255);
  Result := '';
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_READ;
      if KeyExists(REG_KEY_SQL2005_INSTANCES) then begin
        OpenKey(REG_KEY_SQL2005_INSTANCES, FALSE);
        if ValueExists(instanceName) then begin
          Result := ReadString(Result);
        end;
      end;
      // in vista or higher, also check 64 bit registry
      if (Result='') and (GetOSVersion>=wvVista) then begin
        Access := KEY_READ OR KEY_WOW64_64KEY;
        if KeyExists(REG_KEY_SQL2005_INSTANCES) then begin
          OpenKey(REG_KEY_SQL2005_INSTANCES, FALSE);
          if ValueExists(instanceName) then begin
            Result := ReadString(instanceName);
          end;
        end;
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
}
function TSettings.SystemComponentsInstalled: Boolean;
var
  i: Integer;
  lKeys: TStringList;
begin
  Result := False;
  lKeys := nil;
  with TRegistry.Create() do
    try
      lKeys := TStringList.Create;
      RootKey := HKEY_CLASSES_ROOT;
      OpenKeyReadOnly('Installer\Products');
      GetKeyNames(lKeys);
      CloseKey;
      for i := 0 to lKeys.Count - 1 do begin
        OpenKeyReadOnly('Installer\Products\' + lKeys[i]);
        if ValueExists('ProductName') then
          if ReadString('ProductName') = STR_SYSCOMP_DISPLAY_NAME then begin
            Result := True;
            Break;
          end;
        CloseKey;
      end;
      CloseKey;
    finally
      Free;
      lKeys.Free;
    end;
end;  // TSettings.SystemComponentsInstalled

procedure TSettings.SetRemoveSystemComponents(const Value: boolean);
begin
  FRemoveSystemComponents := Value;
end;

end.
