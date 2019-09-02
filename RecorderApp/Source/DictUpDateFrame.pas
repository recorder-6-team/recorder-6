{===============================================================================
  Unit:        UpgradeFrame

  Defines:     TfraUpgrade

  Description: Progress splash screen for Recorder database upgrades.

  Created:

  Last revision information:
    $Revision: 12 $
    $Date: 23/02/09 16:58 $
    $Author: Andrewkemp $

===============================================================================}
unit DictUpDateFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Settings, BaseFrameUnit, ExtCtrls;

resourcestring
  ResStr_Unpacking = 'Please wait, unpacking application files...';
  ResStr_Upgrading = 'Please wait, upgrading %s...';

type
  TfraUpgrade = class (TBaseFrame)
    lblWait: TLabel;
    pbProgress: TProgressBar;
    lblZipFileMerge: TLabel;
    pbZipFileProgress: TProgressBar;
  private
    procedure ProcessDatabase(ASettings: TSettings);
    procedure ProcessFiles(ASettings: TSettings);
    procedure DisplayUpdateDescriptions(ADescriptions: TStrings);
  public
    procedure Execute(ASettings: TSettings); override;
  end;

//==============================================================================

implementation

uses
  Main, DBUpgrader, FileUpgrader, APIUtils, UpdateDescriptions, ComObj, Registry,
  SetupConstants, Functions, RecorderLock;

{$R *.dfm}

{-==============================================================================
    TfraUpgrade
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraUpgrade.Execute(ASettings: TSettings);
var
  valLibInstalled: String;
  oldValLibPresent: Boolean;
begin
  oldValLibPresent := False;
  Repaint;
  if not ASettings.ScriptsOnly then begin
    // Unpack from zip file
    lblWait.Caption := ResStr_Unpacking;
    Application.ProcessMessages;
    ProcessFiles(ASettings);

    ReleaseRecorderLock;
    RegisterRecorder(ASettings.InstallationPath);
    AcquireRecorderLock;

    // Discard outdated/deprecated registry entries
    with TRegistry.Create do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        // StdValLib name changed. Remove old one, but remember if it was "installed" first.
        if OpenKey(REG_KEY_R6_ADDINS + '\StdValLib.Recorder 2000 Standard Validation', False) then
        begin
          oldValLibPresent := True;
          valLibInstalled := ReadString(REG_INSTALLED);
          DeleteKey(REG_KEY_R6_ADDINS + '\StdValLib.Recorder 2000 Standard Validation');
        end;

        // Merge unpacked registry file
        Application.ProcessMessages;
        WinExec32('Regedit /S "' + ASettings.AddinPath + 'UpdateRec6Addins.reg"',
                  ASettings.AddinPath, SW_SHOWNORMAL);

        // Maintain "installed" state of validation library.
        if oldValLibPresent then begin
          if OpenKey(REG_KEY_R6_ADDINS + '\StdValLib.Recorder 6 Standard Validation', True) then
            WriteString(REG_INSTALLED, valLibInstalled);
        end else
        // Fix for bug in previous upgrades, the "Installed" value was left out.
        // If new reg found, check for the "Installed" value. Default to "1" if missing.
        if OpenKey(REG_KEY_R6_ADDINS + '\StdValLib.Recorder 6 Standard Validation', False) then
          if ReadString(REG_INSTALLED) = '' then
            WriteString(REG_INSTALLED, '1');
      finally
        Free;
      end;
  end;
  // Proceed with Database.
  lblWait.Caption := Format(ResStr_Upgrading, [ASettings.DatabaseDescription]);
  Application.ProcessMessages;
  ProcessDatabase(ASettings);
  frmMain.btnProceed.Click;
end;  // TfraUpgrade.Execute

{-------------------------------------------------------------------------------
  Performs the script by script upgrade of the database 
}
procedure TfraUpgrade.ProcessDatabase(ASettings: TSettings);
begin
  with TDBUpgrader.Create(ASettings) do
    try
      PrepareMapSheetTable(pbProgress);
      RunUpdates(pbProgress, pbZipFileProgress, lblZipFileMerge);
      FixMapDatasetFile;
      if UpdateDescriptions.Count<>0 then
        DisplayUpdateDescriptions(UpdateDescriptions);
    finally
      ASettings.DBUpdated := CurrentScriptSequence<>PreviousScriptSequence;
      Free;
    end;
end;  // TfraUpgrade.ProcessDatabase

{-------------------------------------------------------------------------------
}
procedure TfraUpgrade.ProcessFiles(ASettings: TSettings);
begin
  with TFileUpgrader.Create(ASettings) do
    try
      CopyFiles(pbProgress);
    finally
      Free;
    end;
end;  // TfraUpgrade.ProcessMapFiles

procedure TfraUpgrade.DisplayUpdateDescriptions(ADescriptions: TStrings);
begin
  with TfrmUpdateDescriptions.Create(nil) do
    try
      mmDescriptions.Lines.Assign(ADescriptions);
      ShowModal;
    finally
      Free;
    end; // try
end;

end.
