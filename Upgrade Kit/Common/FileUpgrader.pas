{===============================================================================
  Unit:           FileUpgrader

  Defines:

  Description:

  Created:

  Last revision information:
    $Revision: 8 $
    $Date: 19/03/07 11:49 $
    $Author: Ericsalmon $

===============================================================================}
unit FileUpgrader;

interface

uses
  SysUtils, Windows, Messages, Classes, Forms, ComCtrls, Settings, VCLUnzip, Dialogs;

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

  ResStr_FailedCopyingUserFiles =
      'Not all the users files could be upgraded. This could either be because a file was in use ' +
      'or you have insufficient priveleges: '#13#10'%s'#13#10 +
      'The upgrade will continue, but you should resolve the problem and run it again.';

  type
  TFileUpgrader = class (TObject)
  private
    FFolder: string;
    FProgress: TProgressBar;
    FUnzipper: TVCLUnzip;
    FUserFolder : string;
    procedure DeleteOldFile(const AFileName: String);
    procedure MoveUserFiles(const AUserSubFolder: String);
    function MoveFilesFromFolder(const startdir, destDir: String): boolean;
    procedure DeleteUserFiles(const AUserSubFolder: String);
    function DeleteFilesFromFolder(const Folder: String): boolean;
    procedure PreprocessZipFile;
    procedure SkippingFiles(Sender: TObject; Reason: TSkipReason; FName: string;
        FileIndex: Integer; var Retry: Boolean);
    procedure ZipTotalPercentDone(Sender: TObject; Percent: LongInt);
    function DeleteFilesInDeletedFolder(const AUserSubFolder: String): boolean;
  public
    constructor Create(ASettings: TSettings);
    destructor Destroy; override;
    procedure CopyFiles(AProgressBar: TProgressBar);
  end;

//==============================================================================
implementation

uses
  GeneralFunctions, SetupConstants;

{-==============================================================================
    TFileUpgrader
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TFileUpgrader.Create(ASettings: TSettings);
begin
  inherited Create;

  FFolder := IncludeTrailingPathDelimiter(ASettings.InstallationPath);
  FUserFolder := ExtractFilePath(Copy(ASettings.ReportFilePath, 1, Length(ASettings.ReportFilePath) - 8));
  // Prepare Unzip component and set a few properties.
  FUnzipper := TVCLUnzip.Create(nil);
  with FUnzipper do begin
    OnTotalPercentDone := ZipTotalPercentDone;
    OnSkippingFile := SkippingFiles;
    DestDir := FFolder;
    DoAll := True;
    RecreateDirs := True;
    RetainAttributes := False;
    OverwriteMode := Always;
  end;
end;  // TFileUpgrader.Create

{-------------------------------------------------------------------------------
}
destructor TFileUpgrader.Destroy;
begin
  try
    FUnzipper.Free;
  except
    on Exception do;
  end;
  inherited;
end;  // TFileUpgrader.Destroy

{-------------------------------------------------------------------------------
}
procedure TFileUpgrader.CopyFiles(AProgressBar: TProgressBar);
var
  i: Integer;
  lOldFileAttr: Integer;

  const Old_Files: Array[0..12] of String = (
    'MS4.dll', 'MS4Cnv.dll', 'MS4User.dll',
    'CnvBMP4.gll', 'CnvBNA4.gll', 'CnvDlg4.gll', 'CnvDXF4.gll', 'CnvMIF4.gll',
    'CnvNTF4.gll', 'CnvSHP4.gll', 'CnvTIF4.gll', 'CnvVpf4.gll', 'CnvWMF4.gll'
    );
   const User_Folders: Array[0..3] of String = (
    'Reports', 'Batch Updates', 'Rucksacks',
    'Recording Cards'
    );
begin

  lOldFileAttr := 0;
  FProgress := AProgressBar;
  // Ensure C4dll.dll is not read only, as we want it to be overwritten by the zip file,
  // but only if it's in the zip file
  if FileExists(FFolder + 'C4dll.dll') then begin
    lOldFileAttr := FileGetAttr(FFolder + 'C4dll.dll');
    FileSetAttr(FFolder + 'C4dll.dll', 0);
  end;
  try
    // Delete MapServer4 files first.
    FProgress.Position := 2;
    for i := 0 to High(Old_Files) do
      DeleteOldFile(Old_Files[i]);

    // If Recorder.ex_ file still exists, remove it, as replaced by RecorderApp.exe
    if FileExists(FFolder + STR_RECORDER_MAIN_OLD) then begin
      if not DeleteFile(PChar(FFolder + STR_RECORDER_MAIN_OLD)) then
        MessageDlg(Format(ResStr_RemoveRecorderFile, [FFolder + STR_RECORDER_MAIN_OLD]), mtInformation, [mbOk], 0);
    end;

    // Now copy new files across.
    if FileExists('UpgradeFiles.zip') then
      with FUnzipper do begin
        ZipName := 'UpgradeFiles.zip';
        // See what's in there, and add the necessary entries to the InstallLog.

        PreprocessZipFile;
        UnZip;
      end;
    // Delete the files which are listed in the delete folder from the actual folders
    for i := 0 to High(User_Folders) do
      DeleteFilesInDeletedFolder(User_Folders[i]);
    // Now copy user files to new location
    for i := 0 to High(User_Folders) do
      MoveUserFiles(User_Folders[i]);
    // Now delete the files from thw 'Deleted' folders (eg. 'Deleted Reports')
    for i := 0 to High(User_Folders) do
      DeleteUserFiles(User_Folders[i]);

  finally

    if FileExists(FFolder + 'C4dll.dll') then
      FileSetAttr(FFolder + 'C4dll.dll', lOldFileAttr);
  end; // try
end;  // TFileUpgrader.CopyFiles

{-------------------------------------------------------------------------------
}
procedure TFileUpgrader.DeleteOldFile(const AFileName: String);
begin
  if FileExists(FFolder + AFileName) then begin
    // Remove any readonly flag so file can be deleted.
    FileSetAttr(FFolder + AFileName, 0);
    // Now delete.
    DeleteFile(PChar(FFolder + AFileName));
  end;
  // There are 14 files for which this function gets called.
  // So, 7*14 = 98%, good enough for progress bar.
  FProgress.Position := FProgress.Position + 7;
end;  // TFileUpgrader.DeleteOldFile

{-------------------------------------------------------------------------------
}
// Mantis 470
procedure TFileUpgrader.MoveUserFiles(const AUserSubFolder: string);
begin
  If FUserFolder <> FFolder + 'User Files\' then begin
    if DirectoryExists(FuserFolder + AUserSubFolder) then
      if not MoveFilesFromFolder(FFolder + 'User Files\' + AUserSubFolder, FUserFolder + AUserSubFolder) then
       MessageDlg(ResStr_FailedCopyingUserFiles, mtInformation, [mbOk], 0);
  end;
FProgress.Position := FProgress.Position + 4;
end;  // TFileUpgrader.MoveUserFiles

{-------------------------------------------------------------------------------
  When a file is skipped by the unzip, record it and inform the user
}
procedure TFileUpgrader.SkippingFiles(Sender: TObject; Reason: TSkipReason;
    FName: string; FileIndex: Integer; var Retry: Boolean);
begin
  MessageDlg(Format(ResStr_FileUpgradeError, [FName]), mtInformation, [mbOk], 0);
  raise EAbort.Create('');
end;

{-------------------------------------------------------------------------------
}
procedure TFileUpgrader.ZipTotalPercentDone(Sender: TObject; Percent: LongInt);
begin
  AppStartCursor;  // Unzip component has hardcoded cursor values!!!
  if Assigned(FProgress) then
    FProgress.Position := Percent;
  Application.ProcessMessages;
end;  // TFileUpgrader.ZipTotalPercentDone

{-------------------------------------------------------------------------------
}
procedure TFileUpgrader.PreprocessZipFile;
var
  i: Integer;
begin
  if FileExists(FFolder + STR_INSTALLLOG) then
    with TStringList.Create do
      try
        LoadFromFile(FFolder + STR_INSTALLLOG);

        FUnzipper.ReadZip;
        AppStartCursor;  // Unzip component has hardcoded cursor values!!!
        for i := 0 to FUnzipper.Count - 1 do begin
          // Note: Start of may change as new lines are added, so have to use IndexOf each time.
          if (IndexOf(IncludeTrailingPathDelimiter(FFolder + FUnzipper.PathName[i])) = -1) and
             (IndexOf(ExcludeTrailingPathDelimiter(FFolder + FUnzipper.PathName[i])) = -1) then
            Insert(IndexOf(STR_FOLDERS_SECTION) + 1, FFolder + FUnzipper.PathName[i]);

          if IndexOf(FFolder + FUnzipper.FullName[i]) = -1 then
            Insert(IndexOf(STR_FILES_SECTION) + 1, FFolder + FUnzipper.FullName[i]);
        end;
        Application.ProcessMessages;

        // Save changes back to log.
        SaveToFile(FFolder + STR_INSTALLLOG);
      finally
        Free;
      end;
end;
//Mantis 470
// Move files from a folder
// -----------------------------------------------------------------------------
function TFileUpgrader.MoveFilesFromFolder(const startdir, destDir: String): boolean;
var
  folderSearch : TSearchRec;
begin
  Result:= true;
  try
    FindFirst(startdir +'\*.*', faAnyFile,folderSearch);
    if (foldersearch.name<>'.') and (foldersearch.name<>'..') then begin
      Copyfile(PChar(startdir+'\'+folderSearch.name),PChar(destdir +'\'+folderSearch.name),false);
      DeleteFile(PChar(startdir + '\' + folderSearch.name))
    end;
   While FindNext(folderSearch)= 0 do
    begin
      If (folderSearch.attr<>faDirectory) and
        (foldersearch.name<>'.') and
        (foldersearch.name<>'..') then begin
          Copyfile(PChar(startdir+'\'+folderSearch.name),PChar(destdir +'\'+folderSearch.name),false);
          DeleteFile(PChar(startdir + '\' + folderSearch.name))
      end;
    end;
  except
    Result := false;
  end; //try

end;  // MoveFilesFromFolder

{-------------------------------------------------------------------------------
Clear the folders holding the deleted files}

procedure TFileUpgrader.DeleteUserFiles(const AUserSubFolder: string);
begin
    if DirectoryExists(FuserFolder + 'Deleted ' + AUserSubFolder) then
        DeleteFilesFromFolder(FUserFolder + 'Deleted ' + AUserSubFolder);
    if DirectoryExists(FFolder + 'User Files\Deleted ' + AUserSubFolder) then
        DeleteFilesFromFolder(FFolder + 'User Files\Deleted ' + AUserSubFolder);
    FProgress.Position := FProgress.Position + 1;
end;  // TFileUpgrader.DeleteUserFiles

// Deletes files folder
// -----------------------------------------------------------------------------
function TFileUpgrader.DeleteFilesFromFolder(const Folder: String): boolean;
var
  folderSearch : TSearchRec;
begin
  Result:= true;
  try
    FindFirst(folder +'\*.*', faAnyFile,folderSearch);
    if (foldersearch.name<>'.') and (foldersearch.name<>'..') then begin
      DeleteFile(PChar(Folder +'\'+ folderSearch.name));
    end;
   While FindNext(folderSearch)= 0 do
    begin
      If (folderSearch.attr<>faDirectory) and
        (foldersearch.name<>'.') and
        (foldersearch.name<>'..') then begin
          DeleteFile(PChar(Folder + '\' + folderSearch.name))
      end;
    end;
  except
    Result := false;
  end; //try

end;  // DeleteFilesFromFolder


// Deletes files in deleted folder from the user file folder.
// -----------------------------------------------------------------------------
function TFileUpgrader.DeleteFilesInDeletedFolder(const AUserSubFolder: String): boolean;
var
  folderSearch : TSearchRec;
  deletedfilepath : string;
 
begin
  Result:= true;
  deletedfilepath:=  FFolder + 'User Files\Deleted ' + AUserSubFolder;
  try
    FindFirst(deletedfilepath  +'\*.*', faAnyFile,folderSearch);
    if (foldersearch.name<>'.') and (foldersearch.name<>'..') then begin
      DeleteFile(PChar(Fuserfolder + AUserSubFolder + '\' + folderSearch.name));
    end;
   While FindNext(folderSearch)= 0 do
    begin
      If (folderSearch.attr<>faDirectory) and
        (foldersearch.name<>'.') and
        (foldersearch.name<>'..') then begin
          DeleteFile(PChar(Fuserfolder + AUserSubFolder + '\' + folderSearch.name));
      end;
    end;
  except
    Result := false;
  end; //try

end;  // DeleteFilesInDeletedFolder
end.

