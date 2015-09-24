{-------------------------------------------------------------------------------
  Unit:        Main.pas

  Defines:     TfrmMain

  Description: Main form.

  Created:     February 2003

  Last revision information:
    $Revision: 14 $
    $Date: 8/03/04 10:24 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, SqlList, Settings, TextMessages, FrameBase,
  FrameWelcome, APIUtils, GeneralFunctions, XPMan, Registry, Constants;

type
  TfrmMain = class(TForm)
    imgBackdrop: TImage;
    btnNext: TButton;
    btnPrevious: TButton;
    btnCancel: TButton;
    XPManifest: TXPManifest;
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
    FCurrentFrame : TPageFrame;
    FSettings : TSettings;
    procedure CancelInstall;
    function GetExistingPath: String;
    procedure HandleException(Sender: TObject; E: Exception);
    procedure ReadParams;
    procedure SetCurrentFrame(const Value: TPageFrame);
    procedure RebootMachine;
    function SetPrivilegeForReboot(AEnable: Boolean): Boolean;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property CurrentFrame: TPageFrame read FCurrentFrame write SetCurrentFrame;
  end;

var
  frmMain: TfrmMain;

//==============================================================================
implementation

{$R *.dfm}

//==============================================================================
constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  // Global exception handler
  Application.OnException := HandleException;

  FSettings := TSettings.Create;
  ReadParams;
  // Get some settings in if installing a workstation
  if FSettings.InstallMode = imWorkstation then begin
    FSettings.LoadFromIniFile;
  end;
  CurrentFrame := TfraWelcome.Create(Self);
end;

//------------------------------------------------------------------------------
procedure TfrmMain.SetCurrentFrame(const Value: TPageFrame);
begin
  if Assigned(FCurrentFrame) then
    FreeAndNil(FCurrentFrame);

  FCurrentFrame := Value;
  if Assigned(FCurrentFrame) then
    with FCurrentFrame do begin
      Parent := Self;
      SetBounds(160,8,332,297);
      Settings := FSettings;
      NextButton := btnNext;
      PrevButton := btnPrevious;
      btnPrevious.Enabled := PrevFrame <> nil;
      { Next button enabled if there is a class type returned, or if the page
        is a completion page }
      btnNext.Enabled := btnNext.Enabled and
                         ((NextFrame <> nil) or CurrentFrame.IsFinal);
      Refresh;
    end;
end;

{-------------------------------------------------------------------------------
  26/02/2003
  Click the next button - ask the current page which class to load next.
  If the next page supports ICompletionPage then ask it what to set the Next
  button caption to.
  If the current page supports ICompletionPage then closes the form and run
  second part of install, InstallShield's setup.exe, to install system
  components required by Recorder.
}
procedure TfrmMain.btnNextClick(Sender: TObject);
var
  lExecutePage: IExecutePage;
  lNextFrame  : TPageFrameClass;
begin
  { If Install cancelled, call uninstaller to get rid of whatever has already
    been installed}
  if FSettings.Cancelled then begin
    // Install stopped in its tracks. Save log to Temp dir and run Uninstaller
    FSettings.InstallFolder := GetWindowsTempDir;
    FSettings.SaveLogToDisk;
    WinExec32('"' + FSettings.RootFolder + 'System\Uninstaller.exe" ' +
              PARAM_SKIP + ' "' + FSettings.InstallFolder + '"',
              FSettings.InstallFolder, SW_SHOWNORMAL);
    FSettings.Complete := True;
    Close;
  end else
  if CurrentFrame.IsFinal then begin
    FSettings.SaveLogToDisk;
    if FSettings.InstallMode in [imStandalone, imWorkstation, imUpgrade] then
      ShellFile(FSettings.RootFolder + 'System\System Components\Setup.exe')
    else if FSettings.InstallMode = imServer then
      if MessageDlg(MSG_ASK_REBOOT, mtInformation, [mbYes, mbNo], 0) = mrYes then
        RebootMachine;
    Close;
  end else begin
    CurrentFrame.PostProcess;

    // Allow more flexibility, if current frame not ready to be discarded
    // it can return nil and it will remain current frame.
    lNextFrame := CurrentFrame.NextFrame;
    if Assigned(lNextFrame) then begin
      SetCurrentFrame(lNextFrame.Create(Self));

      btnNext.Caption := CurrentFrame.NextButtonCaption;
      if CurrentFrame.IsFinal then FSettings.Complete := True;
      
      CurrentFrame.PreProcess;

      // Check for an executable page
      lExecutePage := nil;
      if Supports(CurrentFrame, IID_EXECUTEPAGE, lExecutePage) then
        lExecutePage.Execute;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  26/02/2003
  Click the previous button, asks the current page which class to load next.
}
procedure TfrmMain.btnPreviousClick(Sender: TObject);
var lPrevFrame: TPageFrameClass;
begin
  lPrevFrame := CurrentFrame.PrevFrame;
  if Assigned(lPrevFrame) then
    SetCurrentFrame(lPrevFrame.Create(Self));
end;

{-------------------------------------------------------------------------------
  25/02/2003
  If not completed, then prompt the user not to close the form as this could
  leave the install in an unpredictable state. If User really wants to quit,
  trigger the uninstall of what is already there, then quit.
}
procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not FSettings.Complete then begin
    CanClose := False;
    if DefMessageDlg(ST_CLOSE_QUERY, mtInformation, [mbYes, mbNo], mbNo, 0) = mrYes then
      CancelInstall;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  if DefMessageDlg(ST_CLOSE_QUERY, mtInformation, [mbYes, mbNo], mbNo, 0) = mrYes then
    CancelInstall;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.CancelInstall;
begin
  btnCancel.Enabled := False;   // Cancel once only!
  Application.ProcessMessages;
  FSettings.Complete := False;
  FSettings.Cancelled := True;
  CurrentFrame.Cancel;
end;

{-------------------------------------------------------------------------------
  Only 1 parameter expected, one of the following values:
    /server
    /workstation
    /standalone
    /upgrade
}
procedure TfrmMain.ReadParams;
var lPath: String;
begin
  if ParamCount <> 1 then
    // Missing parameter, or too many, can't continue
    raise Exception.Create('Missing parameter, unable to run application.')
  else
    with FSettings do begin
      if CompareText(ParamStr(1), PARAM_WORKSTATION) = 0 then begin
        InstallMode := imWorkstation;
        NewServer := false;  // Workstation uses an existing server.
      end else if CompareText(ParamStr(1), PARAM_SERVER) = 0 then
        InstallMode := imServer
      else if CompareText(ParamStr(1), PARAM_STANDALONE) = 0 then
        InstallMode := imStandalone
      else if CompareText(ParamStr(1), PARAM_UPGRADE) = 0 then begin
        InstallMode := imUpgrade;
        InstallFolder := GetExistingPath;
      end else
        // Unrecognised parameter, can't continue.
        raise Exception.Create('Invalid parameter, unable to run application.');

      // Extract Application's path and get to the root
      lPath := ExtractFilePath(Application.ExeName);
      // Fake a filename by removing last '\', easy way to get back up a level
      FSettings.RootFolder := ExtractFilePath(Copy(lPath, 1, Length(lPath) - 2));
    end;
end;

//------------------------------------------------------------------------------
function TfrmMain.GetExistingPath: String;
begin
  Result := DEFAULT_INSTALL_FOLDER;  // Just in case

  with TRegistry.Create do
    try
      if OpenKey('Software\JNCC\Recorder\Settings', False) then begin
        // Extract path to Help folder
        Result := ExtractFilePath(ReadString('Help Path'));
        CloseKey;
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
  If an exception occurs, then unlock the app so it can be closed, then re-
  raise the exception
}
procedure TfrmMain.HandleException(Sender: TObject; E: Exception);
begin
  if not (E is EAccessViolation) then begin
    ShowMessage(E.Message);
    Close;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.RebootMachine;
begin
  // Easy if not NT onward
  if Win32Platform <> VER_PLATFORM_WIN32_NT then begin
    ExitWindowsEx(EWX_LOGOFF + EWX_FORCE, 0);  // Force Explorer to logoff user
    ExitWindowsEx(EWX_REBOOT + EWX_FORCE, 0);  // Now we can ask for a reboot
  end else
  // Otherwise, need to do a bit more work
  if SetPrivilegeForReboot(True) then
  begin
    ExitWindowsEx(EWX_REBOOT + EWX_FORCE, 0);
    SetPrivilegeForReboot(False);
  end;
end;

//------------------------------------------------------------------------------
// Sets privileges under Windows NT ...
function TfrmMain.SetPrivilegeForReboot(AEnable: Boolean): Boolean;
var lTPPrev, lTP: TTokenPrivileges;
    lToken      : THandle;
    ldwRetLen   : DWord;
begin
  Result := False;
  if not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, lToken) then
    raise Exception.Create('Error: OpenProcessToken');

  lTP.PrivilegeCount := 1;
  if LookupPrivilegeValue(nil, 'SeShutdownPrivilege', lTP.Privileges[0].LUID) then
  begin
    if AEnable then lTP.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
               else lTP.Privileges[0].Attributes := 0;

    ldwRetLen := 0;
    if AdjustTokenPrivileges(lToken, False, lTP, SizeOf(lTPPrev), lTPPrev, ldwRetLen) then
      Result := True
    else
      raise Exception.Create('Error: AdjustTokenPrivileges');
  end;
  CloseHandle(lToken);
end;

//------------------------------------------------------------------------------
end.
