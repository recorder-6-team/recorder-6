{===============================================================================
  Unit:        FrameUninstall.pas

  Defines:     TfraUninstall

  Description: Main uninstall processes are run from here.

  Model:       Workstation Uninstall 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 3 $
    $Date: 19/02/09 16:55 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameUninstall;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, Settings, ComCtrls, TextMessages, StdCtrls, ExtCtrls;

type
  TfraUninstall = class(TPageFrame, IExecutePage)
    Animation: TAnimate;
    lblInformation: TLabel;
    lblUninstallComplete: TLabel;
    lblUninstallInfo: TLabel;
    lblContinue: TLabel;
    lblAddins: TLabel;
    lblFiles: TLabel;
    lblRegistry: TLabel;
    lblShortcuts: TLabel;
    lblStepAddins: TLabel;
    lblStepFiles: TLabel;
    lblStepRegistry: TLabel;
    lblStepShortcuts: TLabel;
    lblStepSysComp: TLabel;
    lblSysComp: TLabel;
    pnlExecute: TPanel;
    pnlFinish: TPanel;
    tmrAnim: TTimer;
    procedure tmrAnimTimer(Sender: TObject);
  private
    FDoWait: Boolean;
    function GetUninstallString(const ADisplayName: String): String;
    procedure RemoveAddins;
    procedure RemoveFiles;
    procedure RemoveRegistryEntries;
    procedure RemoveShortcuts;
    procedure RemoveSystemComponents;
  protected
    procedure DoPreProcess; override;
    procedure Execute;
    function GetIsFinal: Boolean; override;
    function GetNextButtonCaption: String; override;
    procedure SetSettings(const Value : TSettings); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  InstallAddins, Shortcuts, GeneralFunctions, APIUtils, Registry, SetupConstants,
  ShellApi;

{-==============================================================================
    TfraUninstall
===============================================================================}
procedure TfraUninstall.DoPreProcess;
begin
  if Settings.OSVersion = wvVista then begin
    Settings.SetAnimationFromResource(Animation, ResAvi_VistaFileDelete);
    tmrAnim.Enabled  := False;
    Animation.Active := True;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraUninstall.Execute;
var
  lCursor: TCursor;
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
    if lblAddins.Visible    then RemoveAddins;
    if lblShortcuts.Visible then RemoveShortcuts;
    if lblFiles.Visible     then RemoveFiles;
    if lblRegistry.Visible  then RemoveRegistryEntries;
    if lblSysComp.Visible   then RemoveSystemComponents;
  
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
end;  // TfraUninstall.Execute 

{-------------------------------------------------------------------------------
}
function TfraUninstall.GetIsFinal: Boolean;
begin
  Result := True;
end;  // TfraUninstall.GetIsFinal 

{-------------------------------------------------------------------------------
}
function TfraUninstall.GetNextButtonCaption: String;
begin
  Result := ResStr_FinishCaption;
end;  // TfraUninstall.GetNextButtonCaption 

{-------------------------------------------------------------------------------
}
function TfraUninstall.GetUninstallString(const ADisplayName: String): String;
var
  lInstalledApps: TStringList;
  i: Integer;
begin
  Result := '';
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      Access := KEY_ALL_ACCESS;
      if OpenKey(REG_KEY_UNINSTALL, False) then begin
        lInstalledApps := TStringList.Create;
        try
          // Get the addin names
          GetKeyNames(lInstalledApps);
          CloseKey;
          // Addin filename is separated from interface name by a dot
          for i := 0 to lInstalledApps.Count - 1 do
            if OpenKey(REG_KEY_UNINSTALL + '\' + lInstalledApps[i], False) then begin
              if ValueExists('DisplayName') then begin
                if CompareText(ReadString('DisplayName'), ADisplayName) = 0 then begin
                  Result := ReadString('UninstallString');
                  CloseKey;
                  // Got what we were looking for
                  Exit;
                end;
              end;
              CloseKey;
            end;
        finally
          lInstalledApps.Free;
        end;
      end;
    finally
      Free;
    end;
end;  // TfraUninstall.GetUninstallString 

{-------------------------------------------------------------------------------
}
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
end;  // TfraUninstall.RemoveAddins 

{-------------------------------------------------------------------------------
}
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
end;  // TfraUninstall.RemoveFiles 

{-------------------------------------------------------------------------------
}
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
      // Delete machine's settings.
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_ALL_ACCESS;
      if KeyExists(REG_KEY_R6) then DeleteKey(REG_KEY_R6);
      // And a few other entries that shouldn't be left behind.
      if KeyExists(REG_KEY_UNINSTALLER) then DeleteKey(REG_KEY_UNINSTALLER);
      CloseKey;
    finally
      Free;
    end;
  
  lblRegistry.Font.Style := [];
  lblStepRegistry.Caption := STR_TICK;
end;  // TfraUninstall.RemoveRegistryEntries 

{-------------------------------------------------------------------------------
}
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
end;  // TfraUninstall.RemoveShortcuts 

{-------------------------------------------------------------------------------
}
procedure TfraUninstall.RemoveSystemComponents;
var
  lCommandLine: String;
begin
  lblStepSysComp.Caption := STR_ARROW;
  lblSysComp.Font.Style := [fsBold];
  WaitFor(2);
  
  // Display name we'll be looking for in the registry
  lCommandLine := GetUninstallString(STR_SYSCOMP_DISPLAY_NAME);
  // Run the uninstall string with additional 'silent mode' flag
  if lCommandLine<>'' then
    WinExecAndWait32(lCommandLine + ' /qn', GetWindowsTempDir, SW_SHOWNORMAL);
  
  lblSysComp.Font.Style := [];
  lblStepSysComp.Caption := STR_TICK;
end;  // TfraUninstall.RemoveSystemComponents 

{-------------------------------------------------------------------------------
}
procedure TfraUninstall.SetSettings(const Value : TSettings);
var
  lTop: Integer;
  
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
  
  // There are always some files installed for any install type.
  // But don't show anything if there is nothing to remove (unlikely though).
  SetVisibility(lblStepFiles, lblFiles,
                (Settings.FilesInstalled.Count > 0) or (Settings.FoldersCreated.Count > 0));
  
  // Hide registry labels if entries not created
  SetVisibility(lblStepRegistry, lblRegistry, Settings.RegistryInstalled);
  
  // If system components were installed (used by Uninstaller)
  SetVisibility(lblStepSysComp, lblSysComp, Settings.SystemComponentsInstalled);
end;  // TfraUninstall.SetSettings 

{-------------------------------------------------------------------------------
}
procedure TfraUninstall.tmrAnimTimer(Sender: TObject);
begin
  inherited;
  with Animation do begin
    StartFrame := (StartFrame + 1) mod FrameCount;
    Play(StartFrame, StartFrame, 1);
  end;
end;  // TfraUninstall.tmrAnimTimer 

end.

