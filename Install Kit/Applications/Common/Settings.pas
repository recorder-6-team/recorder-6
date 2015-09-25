{-------------------------------------------------------------------------------
  Unit:        Settings.pas

  Defines:     TSettings

  Description: Class of which only one instance is created and exists through
               the application. Holds data entered on each individual frames
               so that the user can navigate from one frame to the next without
               losing everything each time.

  Created:     February 2003

  Last revision information:
    $Revision: 15 $
    $Date: 1/02/05 14:05 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit Settings;

interface

uses
  Windows, Controls, Classes, Dialogs, SysUtils, SQLList, Registry, Constants,
  TextMessages, GeneralFunctions, ShlObj, ActiveX, APIUtils, ShellAPI, Forms;

type
  // Type of installation
  TInstallMode = (imServer, imWorkstation, imStandalone, imUpgrade);

  // State of install for MSDE
  TInstallState = (isNotInstalled, isInstalling, isInstalled);

  TSettings = class
  private
    FServerList : TStringList;
    FNewServer: Boolean;
    FServerName: String;
    FSpecifyOwnInstanceName: Boolean;
    FInstanceName: String;
    FSpecifyOwnDataDir: Boolean;
    FDataDir: String;
    FComplete: Boolean;
    FInstallMode: TInstallMode;
    FRootFolder: String;
    FInstallFolder: String;
    FSiteID: String;
    FVerificationKey: String;
    FSpatialRefSystem: String;
    FCutOffYear: integer;
    FSharePolygonFilters: Boolean;
    FShareRecordCards: Boolean;
    FShareReports: Boolean;
    FShareReportSnapshots: Boolean;
    FShareRucksacks: Boolean;
    FShareTemplates: Boolean;
    FLocalInstanceNames: TStringList;
    FLocalInstanceNamesChecked: Boolean;
    FAllowedToClose: Boolean;
    FRedoMSDEInstall: Boolean;
    FNTAuthentication: boolean;
    FMSDEInstallState: TInstallState;
    FCancelled: Boolean;
    FFilesInstalled: TStringList;
    FFoldersCreated: TStringList;
    FAddinsInstalled: TStringList;
    FSystemComponents: Boolean;
    FRegistryInstalled: Boolean;
    FShortcutsInstalled: Boolean;
    FSkipToUninstall: Boolean;

    procedure SetNewServer(const Value: Boolean);
    procedure SetServerName(const Value: String);
    procedure SetSpecifyOwnInstanceName(const Value: Boolean);
    procedure SetInstanceName(const Value: String);
    procedure SetDataDir(const Value: String);
    procedure SetSpecifyOwnDataDir(const Value: Boolean);
    procedure SetComplete(const Value: Boolean);
    procedure SetInstallMode(const Value: TInstallMode);
    procedure SetRootFolder(const Value: String);
    procedure SetInstallFolder(const Value: String);
    procedure SetSiteID(const Value: String);
    procedure SetVerificationKey(const Value: String);
    procedure SetSpatialRefSystem(const Value: String);
    procedure SetCutOffYear(const Value: integer);
    procedure SetSharePolygonFilters(const Value: Boolean);
    procedure SetShareRecordCards(const Value: Boolean);
    procedure SetShareReports(const Value: Boolean);
    procedure SetShareReportSnapshots(const Value: Boolean);
    procedure SetShareRucksacks(const Value: Boolean);
    procedure SetShareTemplates(const Value: Boolean);
    procedure SetAllowedToClose(const Value: Boolean);
    procedure SetRedoMSDEInstall(const Value: Boolean);
    procedure SetNTAuthentication(const Value: boolean);
    procedure SetMSDEInstallState(const Value: TInstallState);
    procedure SetCancelled(const Value: Boolean);
    procedure SetSystemComponents(const Value: Boolean);
    procedure SetRegistryInstalled(const Value: Boolean);
    procedure SetShortcutsInstalled(const Value: Boolean);
    procedure SetSkipToUninstall(const Value: Boolean);
    function SystemComponentsInstalled: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateOptionalServerFolders;
    function FolderExists(const AFolderName: String; ForceCreate: Boolean = True;
      MustExists: Boolean = True): Boolean;
    function ForceFolders(AFolder: String): Boolean;
    procedure GetLocalInstanceNames;
    function GetProgramFilesDir : string;
    function LocalInstancesPresent: Boolean;
    function ProperServerName: String;
    function SelectedServerIsLocal: Boolean;
    function ServerIsLocal(const AServerName: String): Boolean;

    procedure LoadFromIniFile;
    procedure SaveToIniFile;
    procedure SaveLogToDisk;

    procedure AddFileName(const AFileName: String);
    procedure AddFolderName(const AFolderName: String);
    procedure AddAddinName(const AAddinName: String);
    procedure DeleteLoggedFiles;
    procedure DeleteLoggedFolders;
    procedure ResetReadOnlyFlag(const APath: String); overload;
    procedure ResetReadOnlyFlag(AFileList: TStringList); overload;

    property FilesInstalled: TStringList read FFilesInstalled;
    property FoldersCreated: TStringList read FFoldersCreated;
    property AddinsInstalled: TStringList read FAddinsInstalled;
    property ShortcutsInstalled: Boolean read FShortcutsInstalled write SetShortcutsInstalled;
    property RegistryInstalled: Boolean read FRegistryInstalled write SetRegistryInstalled;
    property Cancelled: Boolean read FCancelled write SetCancelled;
    property AllowedToClose: Boolean read FAllowedToClose write SetAllowedToClose;
    property Complete: Boolean read FComplete write SetComplete;
    property ServerList: TStringList read FServerList;
    property NewServer: Boolean read FNewServer write SetNewServer;
    property ServerName: String read FServerName write SetServerName;
    property InstanceName: String read FInstanceName write SetInstanceName;
    property DataDir: String read FDataDir write SetDataDir;
    property SpecifyOwnInstanceName: Boolean read FSpecifyOwnInstanceName
                                             write SetSpecifyOwnInstanceName;
    property SpecifyOwnDataDir: Boolean read FSpecifyOwnDataDir
                                        write SetSpecifyOwnDataDir;
    property RedoMSDEInstall: Boolean read FRedoMSDEInstall write SetRedoMSDEInstall;
    property InstallMode: TInstallMode read FInstallMode write SetInstallMode;
    property RootFolder: String read FRootFolder write SetRootFolder;
    property InstallFolder: String read FInstallFolder write SetInstallFolder;
    property SiteID: String read FSiteID write SetSiteID;
    property VerificationKey: String read FVerificationKey write SetVerificationKey;
    property SpatialRefSystem: String read FSpatialRefSystem write SetSpatialRefSystem;
    property CutOffYear: integer read FCutOffYear write SetCutOffYear;
    property ShareReports: Boolean read FShareReports write SetShareReports;
    property ShareTemplates: Boolean read FShareTemplates write SetShareTemplates;
    property ShareReportSnapshots: Boolean read FShareReportSnapshots write SetShareReportSnapshots;
    property ShareRecordCards: Boolean read FShareRecordCards write SetShareRecordCards;
    property SharePolygonFilters: Boolean read FSharePolygonFilters write SetSharePolygonFilters;
    property ShareRucksacks: Boolean read FShareRucksacks write SetShareRucksacks;
    property NTAuthentication : boolean read FNTAuthentication write SetNTAuthentication;
    property MSDEInstallState: TInstallState read FMSDEInstallState write SetMSDEInstallState;
    // Used by uninstaller.
    property SystemComponents: Boolean read FSystemComponents write SetSystemComponents;
    property SkipToUninstall: Boolean read FSkipToUninstall write SetSkipToUninstall;
  end;

//==============================================================================
implementation

//==============================================================================
{ TSettings }
//------------------------------------------------------------------------------
constructor TSettings.Create;
begin
  FFilesInstalled := TStringList.Create;
  FFilesInstalled.Sorted := True;
  FFilesInstalled.Duplicates := dupIgnore;
  FFoldersCreated := TStringList.Create;
  FFoldersCreated.Sorted := True;
  FFoldersCreated.Duplicates := dupIgnore;
  FAddinsInstalled := TStringList.Create;
  FAddinsInstalled.Sorted := True;
  FAddinsInstalled.Duplicates := dupIgnore;

  FServerList := TStringList.Create;
  PopulateServerList(FServerList);

  // Default settings
  FNewServer := True;
  FCancelled := False;
  FAllowedToClose := True;
  FComplete := False;

  FSpecifyOwnInstanceName := False;
  FSpecifyOwnDataDir := False;
  FDataDir := GetProgramFilesDir + DEFAULT_MSDE_FOLDER;

  FLocalInstanceNames := TStringList.Create;
  FLocalInstanceNamesChecked := False;
  FRedoMSDEInstall := False;

  FMSDEInstallState := isNotInstalled;
  FSystemComponents := SystemComponentsInstalled;
  FRegistryInstalled := False;
  FShortcutsInstalled := False;
  FSkipToUninstall := False;

  FInstallFolder := GetProgramFilesDir + DEFAULT_INSTALL_FOLDER;

  FCutOffYear := 1980;
end;

//------------------------------------------------------------------------------
destructor TSettings.Destroy;
begin
  FAddinsInstalled.Free;
  FFoldersCreated.Free;
  FFilesInstalled.Free;
  FServerList.Free;
  FLocalInstanceNames.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  20/01/2003
  Save the settings to an INI file in the install folder on the server.
  Workstations can then pick this up automatically and use this server for their
  SQL Server.
}
function TSettings.ProperServerName: String;
var lComputerName: Array[0..MAX_COMPUTERNAME_LENGTH + 1] of Char;
    lSize        : Cardinal;
begin
  Result := '';
  lSize := MAX_COMPUTERNAME_LENGTH + 1;
  GetComputerName(lComputerName, lSize);

  if InstallMode <> imWorkstation then begin
    // Server specified, use it (it can be a local instance, default or not)
    if ServerName <> '' then
      Result := ServerName
    // New MSDE instance
    else if NewServer then
      if InstanceName = '' then begin
        // No name for new instance, therefore default instance
        if InstallMode = imServer then
          Result := lComputerName
        else
          Result := '(local)';
      end else
        // Named MSDE instance, so qualify it with computer name
        Result := lComputerName + '\' + InstanceName;
  end;
end;

//------------------------------------------------------------------------------
procedure TSettings.SaveToIniFile;
begin
  if InstallMode <> imWorkstation then begin
    with TStringList.Create do
      try
        Add('SiteID=' + SiteID);
        Add('Database Name=' + DATABASE_NAME);
        Add('Server Name=' + ProperServerName);
        Add('Trusted Security=' + IntToStr(Ord(NTAuthentication))); // always default to zero

        if InstallMode = imServer then begin
          Add('Shared Polygon Filters=' + IntToStr(Ord(SharePolygonFilters)));
          Add('Shared Record Cards=' + IntToStr(Ord(ShareRecordCards)));
          Add('Shared Reports=' + IntToStr(Ord(ShareReports)));
          Add('Shared Report Snapshots=' + IntToStr(Ord(ShareReportSnapshots)));
          Add('Shared Rucksacks=' + IntToStr(Ord(ShareRucksacks)));
          Add('Shared Templates=' + IntToStr(Ord(ShareTemplates)));
        end;
        
        ForceFolders(InstallFolder);
        SaveToFile(InstallFolder + STR_INSTALLSETTINGS);
        AddFileName(InstallFolder + STR_INSTALLSETTINGS);
      finally
        Free;
      end;
  end;
end;

{-------------------------------------------------------------------------------
  Read settings from INI file. Apply to workstation installs.
}
procedure TSettings.LoadFromIniFile;
begin
  if FileExists(RootFolder + STR_INSTALLSETTINGS) then
    with TStringList.Create do
      try
        LoadFromFile(RootFolder + STR_INSTALLSETTINGS);
        SiteID     := Values['SiteID'];
        ServerName := Values['Server Name'];
        NTAuthentication := Values['Trusted Security'] = '1';

        // Important for workstation install.
        SharePolygonFilters  := Values['Shared Polygon Filters'] = '1';
        ShareRecordCards     := Values['Shared Record Cards'] = '1';
        ShareReports         := Values['Shared Reports'] = '1';
        ShareReportSnapshots := Values['Shared Report Snapshots'] = '1';
        ShareRucksacks       := Values['Shared Rucksacks'] = '1';
        ShareTemplates       := Values['Shared Templates'] = '1';
      finally
        Free;
      end;
end;

{-------------------------------------------------------------------------------
  Create directories on the server for any file types selected to share across
  the network
}
procedure TSettings.CreateOptionalServerFolders;
var lDirsToWarn: String;

  { Creates a directory, also remembers the directory name so that a warning can
     be displayed later }
  procedure MakeDir(const ADir: String);
  begin
    if not DirectoryExists(ADir) then begin
      ForceFolders(ADir);
      lDirsToWarn := lDirsToWarn + #13#10 + ADir;
    end;
  end;

begin
  lDirsToWarn := '';
  if ShareRucksacks       then MakeDir(InstallFolder + 'User Files\Rucksacks');
  if ShareReports         then MakeDir(InstallFolder + 'User Files\Reports');
  if ShareTemplates       then MakeDir(InstallFolder + 'User Files\Templates');
  if ShareRecordCards     then MakeDir(InstallFolder + 'User Files\Recording Cards');
  if SharePolygonFilters  then MakeDir(InstallFolder + 'User Files\Polygon Filters');
  if ShareReportSnapshots then MakeDir(InstallFolder + 'User Files\Snapshots');
  if lDirsToWarn <> '' then
    MessageDlg(ST_SERVER_FOLDER_ACCESS + lDirsToWarn, mtInformation, [mbOk], 0);
end;

{-------------------------------------------------------------------------------
  04/03/2003
  Get the names of local instances already installed.
}
procedure TSettings.GetLocalInstanceNames;
var lValueLen       : DWORD;
    lzBuffer, lzName: PChar;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      // Locate key for local instances
      if OpenKey(REG_KEY_SQLSERVER, False) then
        // Value is multiline, so not just ReadString, or we'll miss the others!
        if ValueExists(REG_INSTANCES) then begin
          // Get size of buffer
          RegQueryValueEx(CurrentKey, PChar(REG_INSTANCES), nil,
                          nil, nil, @lValueLen);
          // Allocate buffer
          GetMem (lzBuffer, lValueLen);
          try
            // Get the instance names
            RegQueryValueEx(CurrentKey, PChar(REG_INSTANCES), nil, nil,
                            PBYTE(lzBuffer), @lValueLen);
            // Need to "walk" through the buffer to get each instance name
            lzName := lzBuffer;
            FLocalInstanceNames.Clear;
            while lzName^ <> #0 do begin
              FLocalInstanceNames.Add(lzName);
              Inc(lzName, StrLen(lzName) + 1);
            end;
          finally
            FreeMem(lzBuffer);
          end;
        CloseKey;
      end;
    finally
      Free;
    end;
  FLocalInstanceNamesChecked := True;
end;

{-------------------------------------------------------------------------------
  Function to check is given servername is local or not.
  Unnamed instance is listed as '(local)', but stored in registry as MSSQLSERVER!!!!
}
function TSettings.ServerIsLocal(const AServerName: String): Boolean;
begin
  // In case it hasn't been done yet, go look for them
  if not FLocalInstanceNamesChecked then GetLocalInstanceNames;
  Result := False;
  with FLocalInstanceNames do
    if Count > 0 then
      Result := ((CompareText(AServerName, '(local)') = 0) and (IndexOf('MSSQLSERVER') > -1)) or
                (IndexOf(UpperCase(Copy(AServerName, Pos('\', AServerName) + 1, 255))) > -1);
end;

{-------------------------------------------------------------------------------
  04/03/2003
  If one instance has been installed locally, the 'InstalledInstances' value
  will be present in the registry.
}
function TSettings.LocalInstancesPresent: Boolean;
begin
  // In case it hasn't been done yet, go look for them
  if not FLocalInstanceNamesChecked then GetLocalInstanceNames;
  Result := FLocalInstanceNames.Count > 0;
end;

{-------------------------------------------------------------------------------
  03/03/2003
  Find out if selected server is local or remote.  A default local instance is
  listed as '(local)', all other named instances have a key in the registry.
}
function TSettings.SelectedServerIsLocal: Boolean;
begin
  Result := ServerIsLocal(ProperServerName);
end;

//------------------------------------------------------------------------------
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
end;

{-------------------------------------------------------------------------------
  10/03/2003
  Check the install folder specified exists, and if not prompt the user to for
  its creation. If user declines, show another message saying the folder must
  exists in order to proceed, if ForceCreate is True.
  If MustExists is True, the selected folder must already be there.
  ForceCreate takes precedence on MustExists, obviously.
}
function TSettings.FolderExists(const AFolderName: String; ForceCreate: Boolean;
  MustExists: Boolean): Boolean;
begin
  Result := False;
  if Cancelled then Exit;
  // If folder exists already, no need to do anything else
  if DirectoryExists(AFolderName) then
    Result := True
  else
  // Check it's a drive or not
  if (Length(AFolderName) < 3) and (AFolderName[2] <> ':') then
    MessageDlg(EST_FAILED_CREATE_DIR, mtWarning, [mbOk], 0)
  else
  // If it looks like it's a valid path...
  if ForceCreate then begin
    if MessageDlg(ST_CREATE_MISSING_FOLDER, mtInformation, [mbYes, mbNo], 0) = mrYes then
      try
        if ForceFolders(AFolderName) then
          Result := True
        else
          MessageDlg(ST_FOLDER_CREATE_FAILED, mtWarning, [mbOk], 0)
      except
        // If fodler name is complete nonsense, Windows will say so with an
        // exception, so trap it and deal with it.
        on EInOutError do
          MessageDlg(EST_FAILED_CREATE_DIR, mtWarning, [mbOk], 0);
      end
    else
      MessageDlg(ST_FOLDER_MUST_EXIST, mtWarning, [mbOk], 0);
  end else
  if MustExists then
    MessageDlg(ST_FOLDER_MUST_EXIST, mtWarning, [mbOk], 0);
end;

//------------------------------------------------------------------------------
procedure TSettings.SetCancelled(const Value: Boolean);
begin
  FCancelled := Value;
end;

procedure TSettings.SetComplete(const Value: Boolean);
begin
  FComplete := Value;
end;

procedure TSettings.SetCutOffYear(const Value: integer);
begin
  FCutOffYear := Value;
end;

procedure TSettings.SetDataDir(const Value: String);
begin
  FDataDir := Value;
end;

procedure TSettings.SetInstallFolder(const Value: String);
begin
  // Make sure the last character is '\'
  FInstallFolder := IncludeTrailingPathDelimiter(Value);
end;

procedure TSettings.SetInstallMode(const Value: TInstallMode);
begin
  FInstallMode := Value;
end;

procedure TSettings.SetInstanceName(const Value: String);
begin
  FInstanceName := Value;
end;

procedure TSettings.SetNewServer(const Value: Boolean);
begin
  FNewServer := Value;
end;

procedure TSettings.SetServerName(const Value: String);
begin
  FServerName := Value;
end;

procedure TSettings.SetSiteID(const Value: String);
begin
  FSiteID := Value;
end;

procedure TSettings.SetRootFolder(const Value: String);
begin
  FRootFolder := Value;
end;

procedure TSettings.SetSpatialRefSystem(const Value: String);
begin
  FSpatialRefSystem := Value;
end;

procedure TSettings.SetSpecifyOwnDataDir(const Value: Boolean);
begin
  FSpecifyOwnDataDir := Value;
end;

procedure TSettings.SetSpecifyOwnInstanceName(const Value: Boolean);
begin
  FSpecifyOwnInstanceName := Value;
end;

procedure TSettings.SetVerificationKey(const Value: String);
begin
  FVerificationKey := Value;
end;

procedure TSettings.SetSharePolygonFilters(const Value: Boolean);
begin
  FSharePolygonFilters := Value;
end;

procedure TSettings.SetShareRecordCards(const Value: Boolean);
begin
  FShareRecordCards := Value;
end;

procedure TSettings.SetShareReports(const Value: Boolean);
begin
  FShareReports := Value;
end;

procedure TSettings.SetShareReportSnapshots(const Value: Boolean);
begin
  FShareReportSnapshots := Value;
end;

procedure TSettings.SetShareRucksacks(const Value: Boolean);
begin
  FShareRucksacks := Value;
end;

procedure TSettings.SetShareTemplates(const Value: Boolean);
begin
  FShareTemplates := Value;
end;

procedure TSettings.SetAllowedToClose(const Value: Boolean);
begin
  FAllowedToClose := Value;
end;

procedure TSettings.SetRedoMSDEInstall(const Value: Boolean);
begin
  FRedoMSDEInstall := Value;
end;

procedure TSettings.SetNTAuthentication(const Value: boolean);
begin
  FNTAuthentication := Value;
end;

procedure TSettings.SetMSDEInstallState(const Value: TInstallState);
begin
  FMSDEInstallState := Value;
end;

procedure TSettings.SetSystemComponents(const Value: Boolean);
begin
  FSystemComponents := Value;
end;

procedure TSettings.SetRegistryInstalled(const Value: Boolean);
begin
  FRegistryInstalled := Value;
end;

procedure TSettings.SetShortcutsInstalled(const Value: Boolean);
begin
  FShortcutsInstalled := Value;
end;

procedure TSettings.SetSkipToUninstall(const Value: Boolean);
begin
  FSkipToUninstall := Value;
end;

{-------------------------------------------------------------------------------
  Description : Return the program files directory path
  Created : 24/03/2003 }
function TSettings.GetProgramFilesDir: string;
begin
  Result := 'C:\Program Files\';  // default as old Windows versions don't store it in the registry
  with TRegistry.Create do try
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

//------------------------------------------------------------------------------
procedure TSettings.SaveLogToDisk;
begin
  // Write log file only if there is something to write there.
  if (AddinsInstalled.Count > 0) or (FilesInstalled.Count > 0) or
     (FoldersCreated.Count > 0) or (RegistryInstalled) or
     (MSDEInstallState <> isNotInstalled) or
     ((InstallMode in [imStandalone, imWorkstation, imUpgrade]) and Complete) then
  begin
    AddFileName(InstallFolder + STR_INSTALLLOG);
    with TStringList.Create do
      try
        Add(STR_INSTALL_MODE);
        case InstallMode of
          imStandalone : Add('Standalone');
          imServer     : Add('Server');
          imWorkstation: Add('Workstation');
          else
            Add('Upgrade');
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

        // Section to contain Names of folders created.
        Add('');
        Add(STR_FOLDERS_SECTION);
        AddStrings(FoldersCreated);

        // Section to contain Path + FileName of files installed.
        Add('');
        Add(STR_FILES_SECTION);
        AddStrings(FilesInstalled);

        // Flag for registry settings, if app had time to create some
        if RegistryInstalled then begin
          Add('');
          Add(STR_REGISTRY_SECTION);
        end;

        // If MSDE installed, save instance name.
        if MSDEInstallState <> isNotInstalled then begin
          Add('');
          Add(STR_MSDE_SECTION);
          // Save instance name to file
          Add(InstanceName);
        end;

        // Flag if system components are being installed. Complete means install not cancelled
        if (InstallMode in [imStandalone, imWorkstation, imUpgrade]) and Complete then begin
          Add('');
          Add(STR_SYSCOMP_SECTION);
        end;
        ForceFolders(InstallFolder);
        SaveToFile(InstallFolder + STR_INSTALLLOG);
      finally
        Free;
      end;
  end;
end;

//==============================================================================
procedure TSettings.AddFileName(const AFileName: String);
begin
  FFilesInstalled.Add(AFileName);
end;

//------------------------------------------------------------------------------
procedure TSettings.AddFolderName(const AFolderName: String);
begin
  FFoldersCreated.Add(AFolderName);
end;

//------------------------------------------------------------------------------
procedure TSettings.AddAddinName(const AAddinName: String);
begin
  FAddinsInstalled.Add(AAddinName);
end;

//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
procedure TSettings.DeleteLoggedFolders;
var i: Integer;
begin
  for i := FFoldersCreated.Count - 1 downto 0 do begin
    RemoveFileOrFolder(FFoldersCreated[i]);
    Application.ProcessMessages;
  end;
end;

//------------------------------------------------------------------------------
procedure TSettings.ResetReadOnlyFlag(AFileList: TStringList);
var i: Integer;
begin
  for i := 0 to AFileList.Count - 1 do
    if FileExists(AFileList[i]) then
      if (FileGetAttr(AFileList[i]) and faReadOnly) = faReadOnly then begin
        FileSetAttr(AFileList[i], FileGetAttr(AFileList[i]) - faReadOnly);
        Application.ProcessMessages;
      end;
end;

//------------------------------------------------------------------------------
procedure TSettings.ResetReadOnlyFlag(const APath: String);
var lSearchRec: TSearchRec;
begin
  if FindFirst(APath + '*.*', faReadOnly + faDirectory, lSearchRec) = 0 then
    repeat
      // Ignore DOS directory maps
      if (lSearchRec.Name <> '.') and (lSearchRec.Name <> '..') then begin
        // for directories, recurse into them
        if (lSearchRec.Attr and faDirectory) > 0 then
          ResetReadOnlyFlag(APath + lSearchRec.Name + '\')
        else
        // Remove ReadOnly flag if present
        if (lSearchRec.Attr and faReadOnly) = faReadOnly then
          FileSetAttr(APath + lSearchRec.Name, lSearchRec.Attr - faReadOnly);
      end;
      Application.ProcessMessages;
    until FindNext(lSearchRec) <> 0;
  // Clean up.
  FindClose(lSearchRec);
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

//------------------------------------------------------------------------------
end.
