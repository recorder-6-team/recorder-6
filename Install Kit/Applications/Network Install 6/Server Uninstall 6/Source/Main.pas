{===============================================================================
  Unit:        Main

  Defines:     TfrmMain

  Description: Recorder 6 Server uninstaller main form.

  Model:       Server Uninstall 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 5 $
    $Date: 12/05/09 14:58 $
    $Author: Ericsalmon $

===============================================================================}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, SqlList, Settings, FrameBase;

const
  WM_START = WM_APP + 1;

type
  TfrmMain = class(TForm)
    btnCancel: TButton;
    btnNext: TButton;
    btnPrevious: TButton;
    imgBackdrop: TImage;
    pnlPages: TPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
  private
    FCurrentFrame: TPageFrame;
    FPathToInstallLog: String;
    FSettings: TSettings;
    FSkipToUninstall: Boolean;
    procedure Cleanup;
    procedure HandleException(Sender: TObject; E: Exception);
    procedure PrepareSettings;
    function ReadParams: Boolean;
    procedure SetCurrentFrame(const Value: TPageFrame);
    procedure WMStart(var Message : TMessage); message WM_START;
  public
    constructor Create(AOwner: TComponent); override;
    property CurrentFrame: TPageFrame read FCurrentFrame write SetCurrentFrame;
  end;

var
  frmMain: TfrmMain;

//==============================================================================
implementation

{$R *.dfm}

uses
  FrameUninstall, GeneralFunctions, Registry, SetupConstants, TextMessages, APIUtils;

{-==============================================================================
    TfrmMain
===============================================================================}
{-------------------------------------------------------------------------------
}
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
end;  // TfrmMain.Create 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  Close;
end;  // TfrmMain.btnCancelClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnNextClick(Sender: TObject);
var
  lNextFrame: TPageFrameClass;
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
    if Assigned(lNextFrame) then
      SetCurrentFrame(lNextFrame.Create(Self));
  end;
end;  // TfrmMain.btnNextClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnPreviousClick(Sender: TObject);
var
  lPrevFrame: TPageFrameClass;
begin
  lPrevFrame := CurrentFrame.PrevFrame;
  if Assigned(lPrevFrame) then
    SetCurrentFrame(lPrevFrame.Create(Self));
end;  // TfrmMain.btnPreviousClick 

{-------------------------------------------------------------------------------
  Last to run, creates and runs a batch file to delete exe and remove last folders left lying
      around.
}
procedure TfrmMain.Cleanup;
var
  lBatchFileName: String;
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
  
  with TStringList.Create do
    try
      // Need loop, as DEL will fail until Windows release locks.
      Add(':Label1');
      Add('del "' + Application.ExeName + '"');
      Add('if Exist "' + Application.ExeName + '" goto Label1');
      // Now remove folders, only those still around
      for i := FSettings.FoldersCreated.Count - 1 downto 0 do
        if DirectoryExists(FSettings.FoldersCreated[i]) then
          Add('rmdir "' + ExcludeTrailingPathDelimiter(FSettings.FoldersCreated[i]) + '"');
      // And finally, add the line to delete the batch file itself
      Add('del "' + lBatchFileName + '"');
      SaveToFile(lBatchFileName);
    finally
      Free;
    end;
  // Now run it
  ChDir(GetWindowsTempDir);
  WinExec(PChar(lBatchFileName), SW_HIDE);
  // And exit right now. Nothing to wait for anyway
  Application.Terminate;
end;  // TfrmMain.Cleanup 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.HandleException(Sender: TObject; E: Exception);
begin
  if not (E is EAccessViolation) then begin
    ShowMessage(E.Message);
    Close;
  end;
end;  // TfrmMain.HandleException 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.PrepareSettings;
var
  i: Integer;
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
          if SameText(Strings[i], 'Standalone') then
            FSettings := TSettings.Create(itStandalone)
          else
          if SameText(Strings[i], 'Server') then
            FSettings := TSettings.Create(itServer)
          else
          if SameText(Strings[i], 'Workstation') then
            FSettings := TSettings.Create(itWorkstation)
          else
            FSettings := TSettings.Create(itUpgrade);
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
      finally
        Free;
      end;
    FSkipToUninstall := True;
  end else
    FSettings := TSettings.Create(itUninstall);
  FSettings.SkipToUninstall := FSkipToUninstall;
end;  // TfrmMain.PrepareSettings 

{-------------------------------------------------------------------------------
}
function TfrmMain.ReadParams: Boolean;
var
  lTemp: String;
begin
  Result := True;
  // Called from InstallPlugin, install process was cancelled
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
    // Copy exe
    CopyFile(PChar(Application.ExeName), PChar(lTemp), False);
    // Wait for the file to be there
    while not FileExists(lTemp) do;
    // Run it with path to log file as only param.
    WinExec32('"' + lTemp + '" "' + ExtractFilePath(Application.ExeName) + '"',
        GetWindowsTempDir, SW_SHOWNORMAL);
    // Stop to allow copy to carry on proper
    Result := False;
  end;
end;  // TfrmMain.ReadParams 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.SetCurrentFrame(const Value: TPageFrame);
var
  lExecutePage: IExecutePage;
begin
  if Assigned(FCurrentFrame) then
    FreeAndNil(FCurrentFrame);
  
  FCurrentFrame := Value;
  if Assigned(FCurrentFrame) then
    with FCurrentFrame do begin
      Parent := pnlPages;
      Align := alClient;
      Settings := FSettings;
      NextButton := btnNext;
      PrevButton := btnPrevious;
      btnPrevious.Enabled := PrevFrame <> nil;
      { Next button enabled if there is a class type returned, or if the page
        is a completion page }
      btnNext.Enabled := btnNext.Enabled and
                         ((NextFrame <> nil) or CurrentFrame.IsFinal);
      Refresh;
  
  
      // Check for a completion page
      btnNext.Caption := CurrentFrame.NextButtonCaption;
      if CurrentFrame.IsFinal then FSettings.Complete := True;
  
      CurrentFrame.PreProcess;
  
      // Check for an executable page
      if Supports(CurrentFrame, IID_EXECUTEPAGE, lExecutePage) then
        lExecutePage.Execute;
    end;
end;  // TfrmMain.SetCurrentFrame 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.WMStart(var Message : TMessage);
begin
  SetActiveWindow(Handle);
  SetCurrentFrame(TfraUninstall.Create(Self));
end;  // TfrmMain.WMStart 

end.
