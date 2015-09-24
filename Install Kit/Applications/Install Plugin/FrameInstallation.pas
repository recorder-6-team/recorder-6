{-------------------------------------------------------------------------------
  Unit:        FrameInstallation.pas

  Defines:     TfraInstallation

  Description: Install Recorder on the target machine, copies files, setup
               registry entries, register COM addins, and install MSDE if
               required.

  Created:     February 2003

  Last revision information:
    $Revision: 17 $
    $Date: 12/03/04 14:36 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameInstallation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, StdCtrls, TextMessages, ComCtrls, Registry, Constants,
  Settings, InstallMSDE, GeneralFunctions, ExtCtrls, WinSvc, ServiceHandler,
  CopyFiles, InstallAddins, DAOTools, ComObj;

type
  EInstallError = class(Exception);

  TfraInstallation = class(TPageFrame, IExecutePage)
    Animation: TAnimate;
    lblFile: TLabel;
    pbFiles: TProgressBar;
    pbOverall: TProgressBar;
    lblInstallStep: TLabel;
    lblOverall: TLabel;
    tmrMSDE: TTimer;
    lblCancel: TLabel;
    procedure tmrMSDETimer(Sender: TObject);
  private
    FNextPage : TPageFrameClass;
    FInstallMSDE: TInstallMSDE;
    FCopyFiles  : TCopyFiles;
    FInstallAddins: TInstallAddins;
    procedure StartLanManServer;
    procedure CreateRegistryEntries;
    procedure DeleteOldFiles;
    procedure DoCopyFiles;
    procedure DoCreateShortcuts(const APathToExe, APathToHelp: String);
    procedure DoInstallAddins;
    procedure DoInstallMSDE;
    procedure MSDEInstallFailed;
    procedure DropAccessPassword;
    function GetDaoDatabase(const ADbPath, APassword : string): TDAOLink;
  protected
    function GetNextFrame: TPageFrameClass; override;
    procedure SetSettings(const Value : TSettings); override;
    procedure SetNextButton(const Value: TButton); override;
    procedure Execute;
    procedure CancelFrame; override;
  public
    destructor Destroy; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  FrameComplete, FrameNewOrExisting, FrameServerSettings, Shortcuts,
  FixShortCuts, FrameRemoteConnectMode;

//==============================================================================
{ TfraInstallation }
//------------------------------------------------------------------------------
destructor TfraInstallation.Destroy;
begin
  if Assigned(FCopyFiles) then FCopyFiles.Free;
  if Assigned(FInstallAddins) then FInstallAddins.Free;
  if Assigned(FInstallMSDE) then FInstallMSDE.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  26/02/2003
  Implements ICompletionPage.Execute.  Runs the MSDE install, if needed, then
  moves to the next page.
}
procedure TfraInstallation.Execute;
var lCursor: TCursor;
begin
  lCursor := AppStartCursor;
  try
    try
      // Proceed with installing the various bits
      CreateRegistryEntries;

      case Settings.InstallMode of
        imStandalone:
            begin
              // If most of it already done, don't do it again.
              if not Settings.RedoMSDEInstall then begin
                DoCopyFiles;
                DoCreateShortcuts(Settings.InstallFolder, Settings.RootFolder);
                DoInstallAddins;
              end;
              if Settings.NewServer then DoInstallMSDE;
            end;

        imServer:
            begin
              // If most of it already done, don't do it again.
              if not Settings.RedoMSDEInstall then
                DoCopyFiles;
              if Settings.NewServer then DoInstallMSDE;
            end;

        imWorkstation:
            begin
              DoCopyFiles;
              DoCreateShortcuts(Settings.RootFolder, Settings.RootFolder);
              DoInstallAddins;
            end;

        imUpgrade:
            begin
              // If most of it already done, don't do it again.
              if not Settings.RedoMSDEInstall then begin
                lblFile.Visible    := False;
                pbFiles.Visible    := True;
                pbFiles.Position   := 0;
                lblOverall.Visible := False;
                pbOverall.Visible  := False;
                // Copy files
                DoCopyFiles;
                // Fix the shortcuts to point to the new files
                lblInstallStep.Caption := ST_FIX_SHORTCUTS;
                DoFixShortcuts(pbFiles, nil);
                // Remove a couple of files before copying the others
                DeleteOldFiles;
                DropAccessPassword;
              end;
              if Settings.NewServer then DoInstallMSDE;
            end;
      end;

      // If Cancelled, or no MSDE, still need to move on to next page
      if Cancelled or not Settings.NewServer then begin
        Application.ProcessMessages;
        NextButton.Enabled := True;
        NextButton.Click;
      end;
    except
      on Exception do;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;

{===============================================================================
 Description : Drop the password from the access database
 Created : 20/1/2003 }
procedure TfraInstallation.DropAccessPassword;
var
  lDAOLink : TDAOLink;
  lOldPassword : string;
  lDBPath : string;
begin
  if Cancelled then Exit;

  with TRegistry.Create do try
    if OpenKey(REG_KEY_SETTINGS, False) then begin
      lOldPassword := ReadString('Database Password');
      lDBPath := ReadString('Database Path');
    end
    else
      lDBPath := '';
  finally
    CloseKey;
    Free;
  end;
  if lDBPath <> '' then begin
    lDAOLink := GetDaoDatabase(lDBPath, lOldPassword);
    // drop the password on the access database
    try
      lDAOLink.Database.NewPassword(lOldPassword, '');
    except on Exception do ;
    end; // shouldn't matter
    lDAOLink.Database.Close;
    lDAOLink.Database := nil;
    lDAOLink.Engine := nil;
  end;
end;

{-------------------------------------------------------------------------------
  Description : Attemps to open a database object exclusively.  If the database
              is already open then gives the user the chance to retry.
  Created : 05/03/2003 }
function TfraInstallation.GetDaoDatabase(const ADbPath, APassword : string) : TDAOLink;
var
  lFinished : boolean;
begin
  lFinished := False;
  repeat
    try
      Result := InitDAODatabase(ADbPath, APassword);
      lFinished := True;
    except
      on E:EOleException do begin
        if Pos('You attempted to open ', E.Message)>0 then begin
          if MessageDlg('The Access database at ' + AdbPath + ' is currently in use. ' +
                  'Please ensure all users have closed Recorder and this database before '+
                  'proceeding.', mtWarning, [mbRetry, mbAbort], 0) = mrAbort then
            raise;
        end
        else
          raise;
      end;
    end; // try
  until lFinished;
end;

{-------------------------------------------------------------------------------
  Returns the Completion page as the next frame
  Created : 26/02/2003 }
function TfraInstallation.GetNextFrame: TPageFrameClass;
begin
  Screen.Cursor := crDefault;
  Result := FNextPage;
end;

//------------------------------------------------------------------------------
procedure TfraInstallation.SetSettings(const Value: TSettings);
begin
  inherited;
  // New server installed (MSDE), or Workstation install
  if Settings.NewServer or (Settings.InstallMode = imWorkstation) then
    FNextPage := TfraComplete          
  else
    FNextPage := TfraRemoteConnectMode;
  Animation.Active := True;
end;

{-------------------------------------------------------------------------------
  Next button is initially disabled until MSDE install completes
  Created : 26/02/2003 }
procedure TfraInstallation.SetNextButton(const Value: TButton);
begin
  inherited;
  Value.Enabled := False;
end;

//------------------------------------------------------------------------------
procedure TfraInstallation.CreateRegistryEntries;
var lReg: TRegistry;

    procedure WriteRegString(const AName, AValue: String);
    begin
      lblFile.Caption := AName;
      lReg.WriteString(AName, AValue);
      pbFiles.Position := pbFiles.Position + 1;
      Application.ProcessMessages;
    end;

    procedure WriteRegBool(const AName: String; AValue: Boolean);
    begin
      lblFile.Caption := AName;
      lReg.WriteBool(AName, AValue);
      pbFiles.Position := pbFiles.Position + 1;
      Application.ProcessMessages;
    end;

    { Create Key to start DB Plugin after reboot }
    procedure WriteDBPluginRebootValue(const AInstallTypeParam : string);
    var lRegEntry: String;
    begin
      if lReg.OpenKey(REG_KEY_RUNONCE, False) then begin
        if Settings.NewServer then
          lRegEntry := Format('"%sSystem\DBPlugin.exe" %s /ownsrv "/installdir:%s"',
                              [Settings.RootFolder, AInstallTypeParam, Settings.InstallFolder])
        else if Settings.SelectedServerIsLocal then
          lRegEntry := Format('"%sSystem\DBPlugin.exe" %s "/installdir:%s"',
                              [Settings.RootFolder, AInstallTypeParam, Settings.InstallFolder])
        else
          lRegEntry := Format('"%sSystem\DBPlugin.exe" %s /remotesrv "/installdir:%s"',
                              [Settings.RootFolder, AInstallTypeParam, Settings.InstallFolder]);
        // Save to batch file, in case registry fails.
        with TStringList.Create do
          try
            Add(lRegEntry);
            SaveToFile(Settings.InstallFolder + 'DBPlugin.bat');
            Settings.AddFileName(Settings.InstallFolder + 'DBPlugin.bat');
          finally
            Free;
          end;
        lReg.WriteString(REG_DBPLUGIN, lRegEntry);
        lReg.CloseKey;
        // These 2 will be added/created by DBPlugin, but will need to be removed
        // by the iuninstaller too.
        Settings.AddFolderName(Settings.InstallFolder + 'Database');
        Settings.AddFileName(Settings.InstallFolder + 'Database\nbndata.mdb');
      end else
        raise EInstallError.Create(EST_REGISTRY);
    end;

    procedure RegisterUninstaller(const ALocationFolder: String);
    begin
      with lReg do
        if OpenKey(REG_KEY_UNINSTALLER, True) then begin
          WriteString('DisplayName', 'Recorder (remove only)');
          WriteString('DisplayIcon', ALocationFolder + 'Recorder.exe');
          WriteString('DisplayVersion', '1.0.0.0');
          WriteString('InstallDate', DateToStr(Now));
          WriteString('HelpLink', 'http://www.jncc.gov.uk/');
          WriteString('Publisher', 'JNCC');
          WriteString('UninstallString', ALocationFolder + 'Uninstaller.exe');
          WriteBool('NoModify', True);
          WriteBool('NoRepair', True);
          CloseKey;
        end;
    end;

    procedure SetFolderAndRegistry(const ARegKeyName, AFolderName: String);
    begin
      Settings.ForceFolders(AFolderName);
      WriteRegString(ARegKeyName, AFolderName);
    end;

begin
  if Cancelled then Exit;

  lblInstallStep.Caption := ST_CREATE_REGISTRY;
  lblFile.Visible    := True;
  pbFiles.Visible    := True;
  pbFiles.Position   := 0;
  lblOverall.Visible := False;
  pbOverall.Visible  := False;

  Settings.RegistryInstalled := True;
  lReg := TRegistry.Create;
  try
    case Settings.InstallMode of
      imStandalone:
          begin
            pbFiles.Max := 27;
            // Current User settings
            if lReg.OpenKey(REG_KEY_SETTINGS, True) then begin
              WriteRegString('Base Map Path', Settings.InstallFolder + 'Base Maps\');
              WriteRegString('Dict Images Path', Settings.RootFolder + 'Dictionary Images\');
              WriteRegString('Current Checklist', 'NBNSYS0000000074');
              WriteRegBool('Display Common Names', True);
              WriteRegString('DTD Path', Settings.InstallFolder + 'DTD\');
              WriteRegBool('Graduated Menus', True);
              WriteRegString('Help Path', Settings.InstallFolder + 'Help\Rec20HLP.chm');
              WriteRegString('Local Images File Path', Settings.InstallFolder + 'User Files\User Dictionary Images');
              WriteRegBool('Plain Background', True);
              WriteRegBool('Tool Tips', True);
              WriteRegString('Spatial Ref System', Settings.SpatialRefSystem);
              WriteRegString('Cut Off Date', IntToStr(Settings.CutOffYear));

              SetFolderAndRegistry('Map File Path', Settings.InstallFolder + 'Map Files\');
              SetFolderAndRegistry('Object Sheet File Path', Settings.InstallFolder + 'Object Sheet\');
              SetFolderAndRegistry('Polygon Filter Path', Settings.InstallFolder + 'User Files\Polygon Filter\');
              SetFolderAndRegistry('Recording Card Path', Settings.InstallFolder + 'User Files\Recording Cards\');
              SetFolderAndRegistry('Report Path', Settings.InstallFolder + 'User Files\Reports\');
              SetFolderAndRegistry('Report Template Path', Settings.InstallFolder + 'User Files\Templates\');
              SetFolderAndRegistry('Rucksack Path', Settings.InstallFolder + 'User Files\Rucksacks\');
              SetFolderAndRegistry('Snapshot Path', Settings.InstallFolder + 'User Files\Snapshots\');

              // Backward compatibility, or addins requirements.
              WriteRegString('Database Password', '');
              lReg.CloseKey;
            end else
              raise EInstallError.Create(EST_REGISTRY);

            // Local Machine settings
            lReg.RootKey := HKEY_LOCAL_MACHINE;
            lReg.Access  := KEY_ALL_ACCESS;
            if lReg.OpenKey(REG_KEY_RECORDER, True) then begin
              WriteRegString('Addin Path', Settings.InstallFolder + 'Addins\');
              WriteRegString('Installation Path', Settings.InstallFolder);
              WriteRegString('Local Data Path', Settings.InstallFolder);
              WriteRegBool('Standalone', True);

              WriteRegString('Database Name', DATABASE_NAME);
              WriteRegString('Server Name', Settings.ProperServerName);
              WriteRegBool('Trusted Security', Settings.NTAuthentication);
              lReg.CloseKey;
            end else
              raise EInstallError.Create(EST_REGISTRY);
            WriteDBPluginRebootValue(PARAM_STANDALONE);
            // Uninstaller
            RegisterUninstaller(Settings.InstallFolder);
          end;

      imServer:
          with lReg do begin
            // Local Machine settings
            RootKey := HKEY_LOCAL_MACHINE;
            Access  := KEY_ALL_ACCESS;
            if OpenKey(REG_KEY_RECORDER, True) then begin
              WriteBool('Server', True);
              CloseKey;
            end else
              raise EInstallError.Create(EST_REGISTRY);
            WriteDBPluginRebootValue(PARAM_SERVER);
            // Uninstaller
            RegisterUninstaller(Settings.InstallFolder);
          end;

      imWorkstation:
          begin
            pbFiles.Max := 9;
            // Current User settings
            if lReg.OpenKey(REG_KEY_SETTINGS, True) then begin
              // Always local. Folder content copied from Server folder
              WriteRegString('Base Map Path', Settings.InstallFolder + 'Base Maps\');
              // Always local.
              SetFolderAndRegistry('Map File Path', Settings.InstallFolder + 'Map Files\');
              // Always remote.
              SetFolderAndRegistry('Object Sheet File Path', Settings.RootFolder + 'Object Sheet\');

              // Depending on Server settings, create local or point to remote.
              if Settings.SharePolygonFilters then
                WriteRegString('Polygon Filter Path', Settings.RootFolder + 'User Files\Polygon Filter\')
              else
                SetFolderAndRegistry('Polygon Filter Path', Settings.InstallFolder + 'User Files\Polygon Filter\');
              if Settings.ShareRecordCards then
                WriteRegString('Recording Card Path', Settings.RootFolder + 'User Files\Recording Cards\')
              else
                SetFolderAndRegistry('Recording Card Path', Settings.InstallFolder + 'User Files\Recording Cards\');
              if Settings.ShareReports then
                WriteRegString('Report Path', Settings.RootFolder + 'User Files\Reports\')
              else
                SetFolderAndRegistry('Report Path', Settings.InstallFolder + 'User Files\Reports\');
              if Settings.ShareTemplates then
                WriteRegString('Report Template Path', Settings.RootFolder + 'User Files\Templates\')
              else
                SetFolderAndRegistry('Report Template Path', Settings.InstallFolder + 'User Files\Templates\');
              if Settings.ShareRucksacks then
                WriteRegString('Rucksack Path', Settings.RootFolder + 'User Files\Rucksacks\')
              else
                SetFolderAndRegistry('Rucksack Path', Settings.InstallFolder + 'User Files\Rucksacks\');
              if Settings.ShareReportSnapshots then
                WriteRegString('Snapshot Path', Settings.RootFolder + 'User Files\Snapshots\')
              else
                SetFolderAndRegistry('Snapshot Path', Settings.InstallFolder + 'User Files\Snapshots\');

              WriteRegString('Spatial Ref System', Settings.SpatialRefSystem);
              WriteRegString('Cut Off Date', IntToStr(Settings.CutOffYear));

              // Backward compatibility, or addins requirements.
              WriteRegString('Database Password', '');
              lReg.CloseKey;
            end;
            // Local Machine settings
            lReg.RootKey := HKEY_LOCAL_MACHINE;
            lReg.Access  := KEY_ALL_ACCESS;
            if lReg.OpenKey(REG_KEY_RECORDER, True) then begin
              WriteRegString('Addin Path', Settings.RootFolder + 'Addins\');
              WriteRegString('Installation Path', Settings.RootFolder);
              WriteRegString('Local Data Path', Settings.InstallFolder);
              WriteRegBool('Workstation', True);

              WriteRegString('Database Name', DATABASE_NAME);
              WriteRegString('Server Name', Settings.ServerName);
              WriteRegBool('Trusted Security', Settings.NTAuthentication);
              lReg.CloseKey;
            end else
              raise EInstallError.Create(EST_REGISTRY);
            // Uninstaller, for Workstation, exe is on server
            RegisterUninstaller(Settings.RootFolder);
          end;

      imUpgrade:
          begin
            // Local Machine settings
            lReg.RootKey := HKEY_LOCAL_MACHINE;
            lReg.Access  := KEY_ALL_ACCESS;
            if lReg.OpenKey(REG_KEY_RECORDER, True) then begin
              WriteRegString('Database Name', DATABASE_NAME);
              WriteRegString('Server Name', Settings.ProperServerName);
              WriteRegBool('Trusted Security', False);
              lReg.CloseKey;
            end else
              raise EInstallError.Create(EST_REGISTRY);
            WriteDBPluginRebootValue(PARAM_UPGRADE);
            // Uninstaller
            RegisterUninstaller(Settings.InstallFolder);
          end;
    end;
  finally
    lReg.Free;
  end;
  lblFile.Caption  := '';
  pbFiles.Max      := 100;
  pbFiles.Position := 0;

  Application.ProcessMessages;
end;  // CreateRegistryEntries

//------------------------------------------------------------------------------
procedure TfraInstallation.DoCopyFiles;
begin
  if Cancelled then Exit;

  lblInstallStep.Caption := ST_INSTALL_FILES;
  lblFile.Visible    := True;
  pbFiles.Visible    := True;
  pbFiles.Position   := 0;
  lblOverall.Visible := True;
  pbOverall.Visible  := True;
  Refresh;
  FCopyFiles := TCopyFiles.Create(Settings, lblFile, pbFiles, pbOverall);
  try
    FCopyFiles.CopyFiles;
  finally
    FreeAndNil(FCopyFiles);
  end;
  lblFile.Caption  := '';
  pbFiles.Position := 0;
  Application.ProcessMessages;
end;

//------------------------------------------------------------------------------
procedure TfraInstallation.DoCreateShortcuts(const APathToExe, APathToHelp: String);
begin
  if Cancelled then Exit;

  lblInstallStep.Caption := ST_CREATE_SHORTCUTS;
  lblFile.Visible    := False;
  pbFiles.Visible    := False;
  lblOverall.Visible := False;
  pbOverall.Visible  := False;
  Refresh;
  Settings.ShortcutsInstalled := True;
  with TShortcuts.Create do
    try
      ShortcutToGettingStarted(APathToHelp);
      ShortcutToExe(APathToExe);
    finally
      Free;
    end;
  Application.ProcessMessages;
end;

//------------------------------------------------------------------------------
procedure TfraInstallation.DoInstallAddins;
begin
  if Cancelled then Exit;

  try
    lblInstallStep.Caption := ST_INSTALL_ADDINS;
    lblFile.Visible    := True;
    pbFiles.Visible    := True;
    pbFiles.Position   := 0;
    lblOverall.Visible := False;
    pbOverall.Visible  := False;
    Refresh;

    FInstallAddins := TInstallAddins.Create(Settings, lblFile, pbFiles);
    try
      FInstallAddins.InstallAddins;
      FInstallAddins.FixAddinButtons;
    finally
      FreeAndNil(FInstallAddins);
    end;
    Application.ProcessMessages;
  except
    on Exception do;
  end;
  Application.ProcessMessages;
end;

{-------------------------------------------------------------------------------
  Description : Checks that the LanManServer service is not disabled, and if
              so, sets it to automatic start up and starts it
  Created : 25/03/2003 }
procedure TfraInstallation.StartLanManServer;
begin
  if Cancelled then Exit;

  with TServiceHandler.Create('lanmanserver') do try
    EnableServiceIfDisabled(SERVICE_AUTO_START);
    Start;
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TfraInstallation.DoInstallMSDE;
var lDataDir, lInstanceName: String;
begin
  if Cancelled then Exit;

  lblInstallStep.Caption := ST_INSTALL_MSDE;
  lblFile.Visible    := True;
  lblFile.Caption    := ST_PLEASE_WAIT;
  pbFiles.Visible    := False;
  lblOverall.Visible := False;
  pbOverall.Visible  := False;
  Refresh;
  Application.ProcessMessages;

  // "Server" service not available on W95/98/ME, so don't try
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    StartLanManServer;

  if Settings.SpecifyOwnInstanceName then
    lInstanceName := Settings.InstanceName
  else
    lInstanceName := '';
  if Settings.SpecifyOwnDataDir then
    lDataDir := Settings.DataDir
  else
    lDataDir := '';

  // Create object
  FInstallMSDE := TInstallMSDE.Create(Settings);
  // Go for install
  if not FInstallMSDE.DoInstall(False, ST_COLLATION_SEQUENCE, lDataDir, lInstanceName) then
    MSDEInstallFailed
  else
    // Enable timer to check MSDE setup still going or finished.
    tmrMSDE.Enabled := True;
  Application.ProcessMessages;
end;

//------------------------------------------------------------------------------
procedure TfraInstallation.tmrMSDETimer(Sender: TObject);
var lErrorCode: Cardinal;
begin
  inherited;
  if Assigned(FInstallMSDE) then
    if FInstallMSDE.IsInstallFinished(lErrorCode) then begin
      tmrMSDE.Enabled := False;
      // Windows Installer, silent mode return code 1603 indicates an error occurred.
      if lErrorCode = I_SILENT_MODE_ERROR_CODE then begin
        MessageDlg(ST_MSDE_INSTALL_FAIL + FInstallMSDE.GetError, mtError, [mbOk], 0);
        MessageDlg(ST_RESET_MSDE_SETTINGS, mtInformation, [mbOk], 0);
        if Settings.ServerList.Count > 0 then
          FNextPage := TfraNewOrExisting
        else
          FNextPage := TfraServerSettings;
        Settings.RedoMSDEInstall := True;
        Settings.MSDEInstallState := isNotInstalled;
      end else begin
        Settings.MSDEInstallState := isInstalled;
        FreeAndNil(FInstallMSDE);
      end;
      // Fake click Next action.
      Application.ProcessMessages;
      NextButton.Enabled := True;
      NextButton.Click;
    end else
      AppStartCursor;  // Cursor appears to be reset to default every now and then!!!
end;

//------------------------------------------------------------------------------
procedure TfraInstallation.MSDEInstallFailed;
begin
  MessageDlg(ST_MSDE_INSTALL_FAIL + FInstallMSDE.GetError, mtError, [mbOk], 0);
  MessageDlg(ST_RESET_MSDE_SETTINGS, mtInformation, [mbOk], 0);
  FreeAndNil(FInstallMSDE);
  if Settings.ServerList.Count > 0 then
    FNextPage := TfraNewOrExisting
  else
    FNextPage := TfraServerSettings;
  Settings.RedoMSDEInstall := True;
end;

//------------------------------------------------------------------------------
procedure TfraInstallation.DeleteOldFiles;
begin
  if Cancelled then Exit;

  DeleteFile(Settings.InstallFolder + 'Logo.bmp');
  DeleteFile(Settings.InstallFolder + 'Rec20Hlp.chm');
  DeleteFile(Settings.InstallFolder + 'Recorder2000.exe');
  DeleteFile(Settings.InstallFolder + 'Recorder2000.ex_');
  DeleteFile(Settings.InstallFolder + 'Reports\Wizard SQL.txt');

  // Delete previous workstation setup files
  DeleteFile(Settings.InstallFolder + '_SETUP.1');
  DeleteFile(Settings.InstallFolder + '_ISDEL.EXE');
  DeleteFile(Settings.InstallFolder + 'SETUP.EXE');
  DeleteFile(Settings.InstallFolder + '_SETUP.DLL');
  DeleteFile(Settings.InstallFolder + 'SETUP.INI');
  DeleteFile(Settings.InstallFolder + '_INST32I.EX_');
  DeleteFile(Settings.InstallFolder + 'DISK1.ID');
  DeleteFile(Settings.InstallFolder + 'SETUP.INS');
  DeleteFile(Settings.InstallFolder + '_SETUP.LIB');
  DeleteFile(Settings.InstallFolder + 'SETUP.PKG');
  DeleteFile(Settings.InstallFolder + 'SETUP.ISS');
end;

//------------------------------------------------------------------------------
procedure TfraInstallation.CancelFrame;
begin
  lblCancel.Visible := True;
  Refresh;
  // Do not use inherited, we don't want to skip while stuff is going on
  // especially when TVCLUnzip is being used!!!!!
  
  // Stop copying files now
  if Assigned(FCopyFiles) then
    FCopyFiles.Cancel;

  // Stop doing stuff with Addins
  if Assigned(FInstallAddins) then
    FInstallAddins.Cancel;

  // If installing MSDE, wait for it to finish, then carry on.
  if Assigned(FInstallMSDE) then begin
    lblCancel.Caption := ST_WAITING_FOR_MSDE;
    // Only react when InstallState changes
    while Settings.MSDEInstallState = isInstalling do Application.ProcessMessages;
  end;
  inherited;
end;

//------------------------------------------------------------------------------
end.
