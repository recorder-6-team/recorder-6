{-------------------------------------------------------------------------------
  Unit:        FrameUninstall.pas

  Defines:     TfraUninstall

  Description: Main uninstall processes are run from here.

  Created:     April 2003

  Last revision information:
    $Revision: 9 $
    $Date: 6/07/09 9:05 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameUninstall;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, Settings, ComCtrls, TextMessages, StdCtrls, ExtCtrls;

resourcestring
  ResStr_UnableToRemoveSQLInstanceFolder =
      'The SQL Server instance folder could not be removed. This may be due'
      + #10'to insufficient permissions.';

type
  TfraUninstall = class(TPageFrame, IExecutePage)
    pnlExecute: TPanel;
    Animation: TAnimate;
    lblInformation: TLabel;
    lblStepShortcuts: TLabel;
    lblStepAddins: TLabel;
    lblAddins: TLabel;
    lblShortcuts: TLabel;
    lblFiles: TLabel;
    lblSQLInstance: TLabel;
    lblStepFiles: TLabel;
    lblStepSQLInstance: TLabel;
    pnlFinish: TPanel;
    lblUninstallComplete: TLabel;
    lblAllRemoved: TLabel;
    lblFinishToExit: TLabel;
    tmrAnim: TTimer;
    lblRegistry: TLabel;
    lblStepRegistry: TLabel;
    procedure tmrAnimTimer(Sender: TObject);
  private
    FDoWait: Boolean;
    procedure RemoveAddins;
    procedure RemoveShortcuts;
    procedure RemoveFiles;
    procedure RemoveRegistryEntries;
    procedure RemoveSQLInstance;
  protected
    procedure DoPreProcess; override;
    function GetIsFinal: Boolean; override;
    function GetNextButtonCaption: string; override;
    procedure SetSettings(const Value : TSettings); override;
    procedure Execute;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  InstallAddins, Shortcuts, GeneralFunctions, APIUtils, Registry, SetupConstants,
  ShellApi, ServiceHandler;

//==============================================================================
{ TfraUninstall }
//------------------------------------------------------------------------------
procedure TfraUninstall.DoPreProcess;
begin
  if Settings.OSVersion = wvVista then begin
    Settings.SetAnimationFromResource(Animation, ResAvi_VistaFileDelete);
    tmrAnim.Enabled  := False;
    Animation.Active := True;
  end;
end;

//------------------------------------------------------------------------------
procedure TfraUninstall.Execute;
var lCursor: TCursor;
begin
  Refresh;
  pnlExecute.Visible := True;
  pnlFinish.Visible  := False;
  NextButton.Enabled := False;
  lCursor := HourglassCursor;
  // Start animation, something to look at...
  Animation.Active := True;
  Application.ProcessMessages;
  try
    if lblAddins.Visible      then RemoveAddins;
    if lblShortcuts.Visible   then RemoveShortcuts;
    if lblFiles.Visible       then RemoveFiles;
    if lblRegistry.Visible    then RemoveRegistryEntries;
    if lblSQLInstance.Visible then RemoveSQLInstance;

    if FDoWait then WaitFor(1);
  finally
    DefaultCursor(lCursor);
    // Enough of that now
    Animation.Active := False;
  end;

  // Switch panels around
  pnlExecute.Visible := False;
  pnlFinish.Visible  := True;
  NextButton.Enabled := True;
  Application.ProcessMessages;
end;

//------------------------------------------------------------------------------
procedure TfraUninstall.SetSettings(const Value : TSettings);
var lTop: Integer;

  procedure SetVisibility(AStepLabel, ATextLabel: TLabel; AVisible: Boolean);
  begin
    AStepLabel.Visible := AVisible;  // Arrow/Tick label
    ATextLabel.Visible := AVisible;  // Text label
    if AVisible then begin
      AStepLabel.Top := lTop;
      ATextLabel.Top := lTop;
      Inc(lTop, 24);
      FDoWait := True;
    end;
  end;

begin
  inherited;
  FDoWait := False;
  lTop := lblStepAddins.Top;

  // Don't have registered addins for Server install, also hide if no addins installed.
  SetVisibility(lblStepAddins, lblAddins,
                (Settings.InstallType <> itServer) and (Settings.AddinsInstalled.Count > 0));

  // Don't have shortcuts for Server install
  SetVisibility(lblStepShortcuts, lblShortcuts,
                (Settings.InstallType <> itServer) and Settings.ShortcutsInstalled);

  // Don't have files installed on Workstation, they stay on Server.
  // And don't show anything if there is nothing to remove.
  SetVisibility(lblStepFiles, lblFiles,
                (Settings.InstallType <> itWorkstation) and
                ((Settings.FilesInstalled.Count > 0) or (Settings.FoldersCreated.Count > 0)));

  // Hide registry labels if entries not created
  SetVisibility(lblStepRegistry, lblRegistry, Settings.RegistryInstalled);

  // Don't bother uninstalling SQL Instance if is wasn't installed in the first place.
  SetVisibility(lblStepSQLInstance, lblSQLInstance,
                Settings.SQLExpressInstallState <> isNotInstalled);
end;

//------------------------------------------------------------------------------
function TfraUninstall.GetNextButtonCaption: string;
begin
  Result := ResStr_FinishCaption;
end;

//------------------------------------------------------------------------------
procedure TfraUninstall.RemoveAddins;
begin
  lblStepAddins.Caption := STR_ARROW;
  lblAddins.Font.Style := [fsBold];
  WaitFor(1);

  // List is empty if hasn't started installing addins.
  if Settings.AddinsInstalled.Count <> 0 then
    with TInstallAddins.Create(Settings, nil, nil) do
      try
        UninstallAddins(Settings.AddinsInstalled);
      finally
        Free;
      end;
  lblAddins.Font.Style := [];
  lblStepAddins.Caption := STR_TICK;
end;

//------------------------------------------------------------------------------
procedure TfraUninstall.RemoveShortcuts;
begin
  lblStepShortcuts.Caption := STR_ARROW;
  lblShortcuts.Font.Style := [fsBold];
  WaitFor(1);

  with TShortcuts.Create(R6_START_MENU) do
    try
      RemoveShortcuts([R6_PROGRAM_LINK, R6_GUIDE_LINK]);
    finally
      Free;
    end;

  lblShortcuts.Font.Style := [];
  lblStepShortcuts.Caption := STR_TICK;
end;

//------------------------------------------------------------------------------
procedure TfraUninstall.RemoveFiles;
begin
  lblStepFiles.Caption := STR_ARROW;
  lblFiles.Font.Style := [fsBold];
  WaitFor(1);

  // Remove files and folders.
  Settings.DeleteLoggedFiles;
  Settings.DeleteLoggedFolders;

  lblFiles.Font.Style := [];
  lblStepFiles.Caption := STR_TICK;
end;

//------------------------------------------------------------------------------
procedure TfraUninstall.RemoveRegistryEntries;
begin
  lblStepRegistry.Caption := STR_ARROW;
  lblRegistry.Font.Style := [fsBold];
  WaitFor(1);

  with TRegistry.Create do
    try
      // Delete user's settings.
      RootKey := HKEY_CURRENT_USER;
      Access  := KEY_ALL_ACCESS;
      if KeyExists(REG_KEY_R6) then DeleteKey(REG_KEY_R6);
      // Also remove the Dorset Software entry if there's nothing left below it.
      OpenKey(REG_KEY_DSS, False);
      if not HasSubKeys then DeleteKey(REG_KEY_DSS);
      // Delete the RunOnce item if there.
      OpenKey(REG_KEY_RUNONCE, False);
      if ValueExists(REG_DBPLUGIN) then DeleteValue(REG_DBPLUGIN);

      // Delete machine's settings.
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_ALL_ACCESS;
      if KeyExists(REG_KEY_R6) then DeleteKey(REG_KEY_R6);
      // Also remove the Dorset Software entry if there's nothing left below it.
      OpenKey(REG_KEY_DSS, False);
      if not HasSubKeys then DeleteKey(REG_KEY_DSS);
      // And a few other entries that shouldn't be left behind.
      if KeyExists(REG_KEY_UNINSTALLER) then DeleteKey(REG_KEY_UNINSTALLER);
      CloseKey;
    finally
      Free;
    end;

  lblRegistry.Font.Style := [];
  lblStepRegistry.Caption := STR_TICK;
end;

//------------------------------------------------------------------------------
procedure TfraUninstall.RemoveSQLInstance;
var
  instanceFolder: String;
  installedName: String;
  reg: TRegistry;
  services: TStringList;
  cur: TCursor;
begin
  lblStepSQLInstance.Caption := STR_ARROW;
  lblSQLInstance.Font.Style  := [fsBold];
  WaitFor(1);

  // 1. Stop SQL Express instance related service: MSSQL$<instance name>
  services := TStringList.Create;
  cur := HourglassCursor;
  try
    TServiceHandler.ServicesList(services);
    if services.IndexOf('MSSQL$' + Settings.InstalledInstanceName) > -1 then
      with TServicehandler.Create('MSSQL$' + Settings.InstalledInstanceName) do
        try
          Stop;
        finally
          Free;
        end;
  finally
    services.Free;
    DefaultCursor(cur);
  end;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.Access  := KEY_READ;

    // 2. Delete NBNData_Data.mdf and NBNData_Data_log.ldf
    if reg.OpenKeyReadOnly(REG_KEY_SQL2005_INSTANCES) then
    begin
      installedName := reg.ReadString(Settings.InstalledInstanceName);
      reg.CloseKey;
    end;

    if reg.OpenKeyReadOnly(Format(REG_KEY_SQLSERVER_NAMED_SETUP, [installedName])) then
    begin
      instanceFolder := IncludeTrailingPathDelimiter(reg.ReadString(REG_INSTANCE_DATA)) + 'Data\';
      reg.CloseKey;
    end;

    // Get rid of NBNData files. Anything else is not our responsibility.
    if FileExists(instanceFolder + MDF_FILENAME + '.mdf') then
      DeleteFile(instanceFolder + MDF_FILENAME + '.mdf');
    if FileExists(instanceFolder + MDF_FILENAME + '_log.ldf') then
      DeleteFile(instanceFolder + MDF_FILENAME + '_log.ldf');

    // 3. Trigger ARPWrapper and Microsoft take over.
    if reg.OpenKeyReadOnly(REG_KEY_UNINSTALL + '\Microsoft SQL Server 2005') then
    begin
      WinExec32(reg.ReadString('UninstallString'), GetWindowsTempDir, SW_SHOW);
    end;
  finally
    reg.Free;
  end;
  lblSQLInstance.Font.Style  := [];
  lblStepSQLInstance.Caption := STR_TICK;
  Application.ProcessMessages;
end;

//------------------------------------------------------------------------------
function TfraUninstall.GetIsFinal: Boolean;
begin
  Result := True;
end;

procedure TfraUninstall.tmrAnimTimer(Sender: TObject);
begin
  inherited;
  with Animation do begin
    StartFrame := (StartFrame + 1) mod FrameCount;
    Play(StartFrame, StartFrame, 1);
  end;
end;

end.

