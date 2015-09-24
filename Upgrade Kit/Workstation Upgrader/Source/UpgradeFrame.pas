{===============================================================================
  Unit:        UpgradeFrame

  Defines:     TfraUpgrade

  Description: Progress splash screen for Recorder database upgrades.

  Created:

  Last revision information:
    $Revision: 2 $
    $Date: 23/02/09 17:08 $
    $Author: Andrewkemp $

===============================================================================}
unit UpgradeFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Settings, BaseFrameUnit, ExtCtrls, VCLUnzip;

resourcestring
  ResStr_FileUpgradeError =
      'The following file could not be upgraded, either because it is in use ' +
      'or you have insufficient priveleges: '#13#10'%s'#13#10 +
      'Please resolve the problem and restart the upgrade.';

  ResStr_UpgradeAborted =
      'The upgrade was aborted.  ' +
      'Please retry the upgrade after resolving the issue to complete the upgrade procedure.';

  ResStr_RemoveRecorderFile =
      'The %s file is no longer required by Recorder, but could not be removed ' +
      'automatically. Please delete this file manually.';

type
  TfraUpgrade = class (TBaseFrame)
    lblWait: TLabel;
    pbProgress: TProgressBar;
    lblZipFileMerge: TLabel;
    pbZipFileProgress: TProgressBar;
  private
    procedure ProcessFiles(ASettings: TSettings);
    procedure PreprocessZipFile(AUnzipper: TVCLUnzip; const ALocalPath: String);
    procedure SkippingFiles(Sender: TObject; Reason: TSkipReason;
      AName: string; FileIndex: Integer; var Retry: Boolean);
    procedure ZipTotalPercentDone(Sender: TObject; Percent: Integer);
  public
    procedure Execute(ASettings: TSettings); override;
  end;

//==============================================================================

implementation

uses
  Main, SetupConstants, Registry, GeneralFunctions, APIUtils, Functions,
  RecorderLock;

{$R *.dfm}

{-==============================================================================
    TfraUpgrade
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraUpgrade.Execute(ASettings: TSettings);
begin
  Repaint;
  if not ASettings.ScriptsOnly then begin
    // Unpack from zip file
    lblWait.Caption := 'Please wait, unpacking application files...';
    Application.ProcessMessages;
    ProcessFiles(ASettings);
    ReleaseRecorderLock;    
    RegisterRecorder(ASettings.InstallationPath);
    AcquireRecorderLock;

    // Merge unpacked registry file
    Application.ProcessMessages;
    WinExec32('Regedit /S "' + ASettings.AddinPath + 'UpdateRec6Addins.reg"',
              ASettings.AddinPath, SW_SHOWNORMAL);
  end;
  frmMain.btnProceed.Click;
end;  // TfraUpgrade.Execute

{-------------------------------------------------------------------------------
}
procedure TfraUpgrade.ProcessFiles(ASettings: TSettings);
var
  localPath: String;
  unzipper: TVCLUnzip;
begin
  with TRegistry.Create do
    try
      // Read off Lcoal Machine key.
      Rootkey := HKEY_LOCAL_MACHINE;
      if not OpenKeyReadOnly(REG_KEY_R6) then begin
        MessageDlg(ResStr_BadInstall, mtInformation, [mbOk], 0);
        Application.Terminate;
      end;
      localPath := ReadString('Local Data Path');
    finally
      Free;
    end;

    // Now copy new files across.
    if FileExists('WorkstationUpgradeFiles.zip') then begin
      unzipper := TVCLUnzip.Create(nil);
      with unzipper do begin
        OnTotalPercentDone := ZipTotalPercentDone;
        OnSkippingFile := SkippingFiles;
        DestDir := localPath;
        DoAll := True;
        RecreateDirs := True;
        RetainAttributes := False;
        OverwriteMode := Always;

        ZipName := 'WorkstationUpgradeFiles.zip';
        // See what's in there, and add the necessary entries to the InstallLog.
        PreprocessZipFile(unzipper, localPath);
        UnZip;
      end;
    end;
end;  // TfraUpgrade.ProcessMapFiles

{-------------------------------------------------------------------------------
}
procedure TfraUpgrade.PreprocessZipFile(AUnzipper: TVCLUnzip; const ALocalPath: String);
var
  i: Integer;
begin
  if FileExists(ALocalPath + STR_INSTALLLOG) then
    with TStringList.Create do
      try
        LoadFromFile(ALocalPath + STR_INSTALLLOG);

        AUnzipper.ReadZip;
        AppStartCursor;  // Unzip component has hardcoded cursor values!!!
        for i := 0 to AUnzipper.Count - 1 do begin
          // Note: Start of may change as new lines are added, so have to use IndexOf each time.
          if (IndexOf(IncludeTrailingPathDelimiter(ALocalPath + AUnzipper.PathName[i])) = -1) and
             (IndexOf(ExcludeTrailingPathDelimiter(ALocalPath + AUnzipper.PathName[i])) = -1) then
            Insert(IndexOf(STR_FOLDERS_SECTION) + 1, ALocalPath + AUnzipper.PathName[i]);

          if IndexOf(ALocalPath + AUnzipper.FullName[i]) = -1 then
            Insert(IndexOf(STR_FILES_SECTION) + 1, ALocalPath + AUnzipper.FullName[i]);
        end;
        Application.ProcessMessages;

        // Save changes back to log.
        SaveToFile(ALocalPath + STR_INSTALLLOG);
      finally
        Free;
      end;
end;

{-------------------------------------------------------------------------------
  When a file is skipped by the unzip, record it and inform the user
}
procedure TfraUpgrade.SkippingFiles(Sender: TObject; Reason: TSkipReason;
    AName: string; FileIndex: Integer; var Retry: Boolean);
begin
  MessageDlg(Format(ResStr_FileUpgradeError, [AName]), mtInformation, [mbOk], 0);
  raise EAbort.Create('');
end;

{-------------------------------------------------------------------------------
}
procedure TfraUpgrade.ZipTotalPercentDone(Sender: TObject; Percent: LongInt);
begin
  AppStartCursor;  // Unzip component has hardcoded cursor values!!!
  pbProgress.Position := Percent;
  Application.ProcessMessages;
end;  // TFileUpgrader.ZipTotalPercentDone

end.
