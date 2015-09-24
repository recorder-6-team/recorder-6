{-------------------------------------------------------------------------------
  Unit:        Main.pas

  Defines:     TfrmMain

  Description: Uninstaller main form.

  Created:     April 2003

  Last revision information:
    $Revision: 9 $
    $Date: 6/07/09 9:05 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, SqlList, Settings, FrameBase;

const
  WM_START = WM_APP + 1;

type
  TfrmMain = class(TForm)
    imgBackdrop: TImage;
    btnNext: TButton;
    btnPrevious: TButton;
    btnCancel: TButton;
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
    FCurrentFrame : TPageFrame;
    FSettings : TSettings;
    FPathToInstallLog: String;
    FSkipToUninstall: Boolean;
    procedure Cleanup;
    procedure HandleException(Sender: TObject; E: Exception);
    procedure PrepareSettings;
    function  ReadParams: Boolean;
    procedure SetCurrentFrame(const Value: TPageFrame);
    procedure WMStart(var Message : TMessage); message WM_START;
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

uses
  FrameWelcome, GeneralFunctions, Registry, SetupConstants, TextMessages, APIUtils;

//==============================================================================
constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  // Global exception handler
  Application.OnException := HandleException;
  if not ReadParams then
    Application.Terminate
  else begin
    PrepareSettings;
    PostMessage(Handle, WM_START, 0, 0);
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.WMStart(var Message : TMessage);
begin
  SetActiveWindow(Handle);
  CurrentFrame := TfraWelcome.Create(Self);
  // Skip to uninstall process if nothing else required.
  if FSkipToUninstall then begin
    btnPrevious.Visible := False;
    btnNextClick(nil);
  end;
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
  Click the next button - ask the current page which class to load next.
  If the next page supports ICompletionPage then ask it what to set the Next
  button caption to.
  If the current page supports ICompletionPage then closes the app as it is the
  end of the uninstall process.
}
procedure TfrmMain.btnNextClick(Sender: TObject);
var
  lExecutePage: IExecutePage;
  lNextFrame  : TPageFrameClass;
begin
  if CurrentFrame.IsFinal then begin
    // Cancelled during install, don't want to delete the app.
    if FSkipToUninstall then
      Close
    else
      Cleanup;
  end else begin
    CurrentFrame.PostProcess;
    // Allow more flexibility, if current frame not ready to be discarded
    // it can return nil and it will remain current frame.
    lNextFrame := CurrentFrame.NextFrame;
    if Assigned(lNextFrame) then begin
      SetCurrentFrame(lNextFrame.Create(Self));
      // Check for a completion page
      btnNext.Caption := CurrentFrame.NextButtonCaption;
      if CurrentFrame.IsFinal then FSettings.Complete := True;

      CurrentFrame.PreProcess;

      // Check for an executable page
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
  27/02/2003
  Triggers the FormCloseQuery event when Cancel button clicked or ESC pressed.
}
procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  Close;
end;

{-------------------------------------------------------------------------------
  Read info from log file created during install, and populate various properties
  of Settings object.
}
procedure TfrmMain.PrepareSettings;
var i: Integer;
begin
  // If install was cancelled before anything was done, log file won't exist.
  if FileExists(FPathToInstallLog + STR_INSTALLLOG) then begin
    with TStringList.Create do
      try
        LoadFromFile(FPathToInstallLog + STR_INSTALLLOG);
        // Get install mode
        i := IndexOf(STR_INSTALL_MODE);
        if i <> -1 then begin
          Inc(i);
          if Strings[i] = 'Standalone'  then FSettings := TSettings.Create(itStandalone) else
          if Strings[i] = 'Server'      then FSettings := TSettings.Create(itServer) else
          if Strings[i] = 'Workstation' then FSettings := TSettings.Create(itWorkstation) else
                                             FSettings := TSettings.Create(itUpgrade);
        end;

        // Get Addin names
        i := IndexOf(STR_ADDINS_SECTION);
        if i <> -1 then begin
          Inc(i);  // Skip section name
          while (i < Count) and (Strings[i] <> '') do begin
            FSettings.AddAddinName(Strings[i]);
            Inc(i);
          end;
        end;

        // Get File names
        i := IndexOf(STR_FILES_SECTION);
        if i <> -1 then begin
          Inc(i);  // Skip section name
          while (i < Count) and (Strings[i] <> '') do begin
            FSettings.AddFileName(Strings[i]);
            Inc(i);
          end;
        end;

        // Get Folder names
        i := IndexOf(STR_FOLDERS_SECTION);
        if i <> -1 then begin
          Inc(i);  // Skip section name
          while (i < Count) and (Strings[i] <> '') do begin
            FSettings.AddFolderName(Strings[i]);
            Inc(i);
          end;
        end;

        // Read Shortcuts flag (if present)
        FSettings.ShortcutsInstalled := IndexOf(STR_SHORTCUTS_SECTION) <> -1;

        // Read Registry Settings flag (if present)
        FSettings.RegistryInstalled := IndexOf(STR_REGISTRY_SECTION) <> -1;

        // Read Instance name (if present)
        i := IndexOf(STR_SQLEXPRESS_SECTION);
        if i <> -1 then begin
          Inc(i);
          FSettings.SQLExpressInstallState := isInstalled;
          if i < Count then
            FSettings.InstalledInstanceName := Strings[i]
          else
            FSettings.InstalledInstanceName := '';
        end;
      finally
        Free;
      end;

    // Check it's not been set in ReadParams first before setting it.
    if not FSkipToUninstall then
      // If only dealing with files and settings, skip MSDE/SysComps selection
      FSkipToUninstall := FSettings.SQLExpressInstallState = isNotInstalled;
  end else
    FSettings := TSettings.Create(itUninstall);
  FSettings.SkipToUninstall := FSkipToUninstall;
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
function TfrmMain.ReadParams: Boolean;
var
  lTemp: String;
begin
  Result := True;
  // Called from Setup, install process was cancelled
  if ParamCount = 2 then begin
    FSkipToUninstall  := CompareText(ParamStr(1), PARAM_SKIP) = 0;
    FPathToInstallLog := ParamStr(2);
  end else
  // Called after Exe copied to Temp folder, param is path to install folder
  if ParamCount = 1 then begin
    FSkipToUninstall  := False;
    FPathToInstallLog := ParamStr(1);
  end else begin
    { Called way after install. Exe and Log files should be together.
      Exe can't be deleted if in use, so make a copy in Temp dir and run that one.
      The copy will be deleted by batch file at the end.
    }
    lTemp := GetWindowsTempDir + ExtractFileName(Application.ExeName);
    // Copy exe and language files.
    gpcApiCheck(CopyFiles(
        ExtractFilePath(Application.ExeName)
            + StringReplace(
                ExtractFileName(Application.ExeName),
                ExtractFileExt(Application.ExeName),
                '',
                [rfReplaceAll])
            + '.*',
        GetWindowsTempDir));

    // Wait for the file to be there
    while not FileExists(lTemp) do;
    // Run it with path to log file as only param.
    WinExec32('"' + lTemp + '" "' + ExtractFilePath(Application.ExeName) + '"', GetWindowsTempDir, SW_SHOWNORMAL);
    // Stop to allow copy to carry on proper
    Result := False;
  end;
end;

//------------------------------------------------------------------------------
// Last to run, creates and runs a batch file to delete exe and remove last
// folders left lying around.
procedure TfrmMain.Cleanup;
var
  lBatchFileName: String;
  lFileNoExt: String;
  i: Integer;

  function GetTmpFileName: String;
  var lszName: PChar;
  begin
    lszName:= StrAlloc(MAX_PATH + 1);
    try
      GetTempFileName(PChar(GetWindowsTempDir), 'uis', 0, lszName);
      Result := String(lszName);
      Result := ChangeFileExt(Result, '.bat');
    finally
      StrDispose(lszName);
    end;
  end;

begin
  lBatchFileName := GetTmpFileName;
  lFileNoExt := StringReplace(
      Application.ExeName,
      ExtractFileExt(Application.ExeName),
      '',
      [rfReplaceAll]);

  with TStringList.Create do
    try
      // Need loop, as DEL will fail until Windows release locks.
      Add(':Label1');
      Add('DEL "' + lFileNoExt + '.*"'); //Application.ExeName + '"');
      Add('IF EXIST "' + Application.ExeName + '" GOTO Label1');
      // Now remove folders, only those still around
      for i := FSettings.FoldersCreated.Count - 1 downto 0 do
        Add(Format(
            'IF EXISTS "%s" RMDIR "%s"',
            [FSettings.FoldersCreated[i],FSettings.FoldersCreated[i]]));
      // And finally, add the line to delete the batch file itself
      Add('DEL "' + lBatchFileName + '"');
      SaveToFile(lBatchFileName);
    finally
      Free;
    end;
  // Now run it
  ChDir(GetWindowsTempDir);
  WinExec(PChar(lBatchFileName), SW_HIDE);
  // And exit right now. Nothing to wait for anyway
  Application.Terminate;
end;

//------------------------------------------------------------------------------
end.
