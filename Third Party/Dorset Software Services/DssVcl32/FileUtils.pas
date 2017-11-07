// -----------------------------------------------------------------------------
// Unit:          FileUtils
//
//                Copyright © Dorset Software Services Ltd, 1998
//
// Description:   File manipulation routines.
//
//                Note that this unit should ideally be 'used' after SysUtils
//                so that the exception throwing versions of DeleteFile,
//                CreateDir etc. 'hide' the Boolean return value functions with
//                the same names.
//
// Author:        AJWK
//
// Created:       26/11/1998
//
// Changes:				IPA 30/09/1999: Changed DestroyDir to use Delphi DeleteFile
//								IPA 29/09/1999: Added CheckPathExists
//								AJWK 08/12/1998: Added DeleteFileMatches.
//                AJWK 09/12/1998: Added callbacks for multi-file routines.
//                AJWK 15/12/1998: Added MoveFileMatches
//                AJWK 21/01/1999: Added ForEachFile
//                AJWK 24/02/1999: State variables for multiple file
//                    operations are now declared as 'threadvar': one instance
//                    per thread.
//
// To Do:
// -----------------------------------------------------------------------------

unit FileUtils;

interface

uses
  SysUtils, FileCtrl;

type
  EFileUtilsError = class(Exception);
  EFileUtilsObjectExists = class(Exception);
  EDirectoryLocked = class(Exception);

  TFileOpProgressEvent = procedure(percentageComplete: Integer);
  TFileOpNotifyEvent = procedure(const fileName: String; yourRef: Pointer);

function DirToPath(const dir: String): String;
procedure CreateDir(const dirPath: String);
procedure CopyDir(const fileName, destName: String; failIfExists: Boolean;
    onNotify: TFileOpProgressEvent);
procedure DestroyDir(dirPath: String; onNotify: TFileOpProgressEvent);
procedure MoveFile(const oldName, newName: String);
procedure CopyFile(const fileName, destName: String; failIfExists: Boolean);
procedure DeleteFile(const fileName: String);
procedure CopyFileMatches(const filePattern, destDir: String);
procedure MoveFileMatches(const filePattern, destDir: String);
procedure DeleteFileMatches(const filePattern: String);
procedure ForEachFile(const dirName: String; recursive: Boolean;
    onProgress: TFileOpProgressEvent; onNextFile: TFileOpNotifyEvent;
    yourRef: Pointer);

function CountObjects(const dirName: String; recursive: Boolean): Integer;
function HasAttr(const FileName: string; Attr: Word): Boolean;
procedure CheckPathExists(PathName: String);



implementation

uses
  Classes, Consts, Windows, APIUtils;

const
  NOTIFY_STEP = 1;  // notify caller every NOTIFY_STEP%

resourcestring
  ResStr_ObjectExists   = 'Failed to copy object: ''%s'' already exists.';
  ResStr_CreateDirFail  = 'Failed to create directory: ''%s''.';
  ResStr_DeleteFileFail = 'Failed to delete file: ''%s''.';

{$ifdef VER90}
// Delphi 2
var
{$else}
// later 32 bit versions of Delphi
threadvar
{$endif}
  // used to maintain progress information in multiple file operations
  mFileCount:           Integer;
  mFileIndex:           Integer;
  mFileUpdateEvery:     Integer;
  mOnFileOpProgressed:  TFileOpProgressEvent;


procedure FileProcessed;
begin
  Inc(mFileIndex);
  if (mFileIndex mod mFileUpdateEvery) = 0 then
    mOnFileOpProgressed(Trunc(100 * mFileIndex / mFileCount));
end;  // FileProcessed


// -----------------------------------------------------------------------------
// DirToPath
//
// -----------------------------------------------------------------------------
function DirToPath(const dir: String): String;
var
  path:  String;

begin
  path := dir;
  if Pos(path[Length(path)], '\:') = 0 then path := path + '\';
  Result := path;
end;  // DirToPath


// -----------------------------------------------------------------------------
// CreateDir
//
// Exception throwing wrapper for the Win32 API function CreateDirectory.
// -----------------------------------------------------------------------------
procedure CreateDir(const dirPath: String);
begin
  gpcApiCheck(
     Windows.CreateDirectory(PChar(dirPath), nil)
  );
end;  // CreateDir


// -----------------------------------------------------------------------------
// CopyDir
//
// Copies a directory (including sub directories).
// -----------------------------------------------------------------------------
procedure DoCopyDir(const fileName, destName: String; failIfExists: Boolean);
    forward;

procedure CopyDir(const fileName, destName: String; failIfExists: Boolean;
    onNotify: TFileOpProgressEvent);
begin
  mFileIndex := 0;
  mFileCount := CountObjects(fileName, True);
  mFileUpdateEvery := Round(mFileCount/ (100 * NOTIFY_STEP));
  if mFileUpdateEvery = 0 then mFileUpdateEvery := 1;
  mOnFileOpProgressed := onNotify;
  DoCopyDir(fileName, destName, failIfExists)
end;  // CopyDir


procedure DoCopyDir(const fileName, destName: String; failIfExists: Boolean);
var
  srcPath:   String;
  destPath:  String;
  found:     TSearchRec;
  rc:        Integer;

begin
  // create destination directory
  if HasAttr(destName, faDirectory) then begin
    if failIfExists then
      raise EFileUtilsObjectExists.CreateFmt(ResStr_ObjectExists, [destName]);

  end else begin
    CreateDir(destName);

  end;  // if DirectoryExists(destName) then .. else

  // convert directories to paths (if necessary)
  srcPath := DirToPath(fileName);
  destPath := DirToPath(destName);

  // copy contents
  rc := FindFirst(srcPath + '*.*', faAnyFile, found);
  try
    while rc = 0 do begin
      if Pos(found.Name, '..') = 0 then begin

        if HasAttr(srcPath + found.Name, faDirectory) then
          DoCopyDir(srcPath + found.Name, destPath + found.Name, failIfExists)
        else
          CopyFile(srcPath + found.Name, destPath + found.Name, failIfExists);

        if Assigned(mOnFileOpProgressed) then FileProcessed;

      end;  // if Pos(found.Name, '..') = 0

      rc := FindNext(found);
    end;  // while rc = 0

  finally
    SysUtils.FindClose(found);
  end;  // try .. finally

end;  // DoCopyDir


// -----------------------------------------------------------------------------
// DestroyDir
//
// More violent version of RemoveDir: removes all files and sub-directories
// within the directory before attempting to delete it.  Use with extreme
// care!
// -----------------------------------------------------------------------------
procedure DoDestroyDir(dirPath: String); forward;

procedure DestroyDir(dirPath: String; onNotify: TFileOpProgressEvent);
begin
  mFileIndex := 0;
  mFileCount := CountObjects(dirPath, True);
  mFileUpdateEvery := Round(mFileCount/(100 * NOTIFY_STEP));
  if mFileUpdateEvery = 0 then mFileUpdateEvery := 1;
  mOnFileOpProgressed := onNotify;
  DoDestroyDir(dirPath)
end;


procedure DoDestroyDir(dirPath: String);
var
  errCode:   Integer;
  fileName:  String;
  found:     TSearchRec;
  rc:        Integer;

begin
  // ensure that directory is either ':' or '\' terminated
  dirPath := DirToPath(dirPath);

  rc := FindFirst(dirPath + '*.*', faAnyFile, found);
  try
    while rc = 0 do begin
      if Pos(found.Name, '..') = 0 then begin

        fileName := dirPath + found.Name;
        if HasAttr(fileName, faDirectory) then
          DoDestroyDir(fileName)
        else
          if not SysUtils.DeleteFile(fileName) then
            Raise EFileUtilsError.CreateFmt(ResStr_DeleteFileFail, [fileName]);

        if Assigned(mOnFileOpProgressed) then FileProcessed;

      end;  // if Pos(found.Name, '..') = 0
      rc := FindNext(found);
    end;  // while rc = 0

  finally
    SysUtils.FindClose(found);
  end;  // try .. finally

  if not Windows.RemoveDirectory(PChar(dirPath)) then begin
    errCode := GetLastError;
    if errCode = ERROR_SHARING_VIOLATION then
      raise EDirectoryLocked.Create(SysErrorMessage(errCode))
    else
      gpcAPIResultCheck(errCode);
  end;  // if not Windows.RemoveDirectory(..)

end;  // DestroyDir


// -----------------------------------------------------------------------------
// MoveFile
//
// A wrapper for the Win32 API call MoveFile that (unlike the VCL's otherwise
// identical RenameFile) throws an exception in case of failure.
// -----------------------------------------------------------------------------
procedure MoveFile(const oldName, newName: String);
begin
  gpcApiCheck(
      Windows.MoveFile(PChar(oldName), PChar(newName))
  );
end;  // MoveFile


// -----------------------------------------------------------------------------
// CopyFile
//
// A wrapper for the Win32 API call CopyFile that throws an exception in case
// of failure.
// -----------------------------------------------------------------------------
procedure CopyFile(const fileName, destName: String; failIfExists: Boolean);
var
  finalDest:  String;

begin
  // if destination is a directory, then copy the file into that directory
  // with the leaf name unchanged
  finalDest := destName;

  if HasAttr(finalDest, faDirectory) then begin
    finalDest := DirToPath(finalDest) + ExtractFileName(fileName);
  end;  // if HasAttr(finalDest, faDirectory)

  gpcApiCheck(
      Windows.CopyFile(PChar(fileName), PChar(finalDest), failIfExists)
  );
end;  // CopyFile


// -----------------------------------------------------------------------------
// DeleteFile
//
// A wrapper for the Win32 API call DeleteFile that (unlike the VCL's otherwise
// identical DeleteFile) throws an exception in case of failure.
// -----------------------------------------------------------------------------
procedure DeleteFile(const fileName: String);
begin
  gpcApiCheck(
    Windows.DeleteFile(PChar(fileName))
  );
end;  // DeleteFile


// -----------------------------------------------------------------------------
// CopyFileMatches
//
// Copies files that match a given pattern using CopyFile.
// -----------------------------------------------------------------------------
procedure CopyFileMatches(const filePattern, destDir: String);
var
  destPath:  String;
  found:     TSearchRec;
  rc:        Integer;
  srcPath:   String;

begin
  srcPath := ExtractFilePath(filePattern);
  destPath := DirToPath(destDir);

  rc := FindFirst(filePattern, 0, found);
  try
    while rc = 0 do begin
      CopyFile(srcPath + found.Name, destPath, False);
      rc := FindNext(found);
    end;  // while rc = 0

  finally
    SysUtils.FindClose(found);
  end;  // try .. finally

end;  // CopyFileMatches


// -----------------------------------------------------------------------------
// MoveFileMatches
//
// Copies files that match a given pattern using MoveFile.
// -----------------------------------------------------------------------------
procedure MoveFileMatches(const filePattern, destDir: String);
var
  destPath:  String;
  found:     TSearchRec;
  rc:        Integer;
  srcPath:   String;

begin
  srcPath := ExtractFilePath(filePattern);
  destPath := DirToPath(destDir);

  rc := FindFirst(filePattern, 0, found);
  try
    while rc = 0 do begin
      MoveFile(srcPath + found.Name, destPath + found.Name);
      rc := FindNext(found);
    end;  // while rc = 0

  finally
    SysUtils.FindClose(found);
  end;  // try .. finally

end;  // MoveFileMatches


// -----------------------------------------------------------------------------
// DeleteFileMatches
//
// Deletes files that match a given pattern using DeleteFile.
// -----------------------------------------------------------------------------
procedure DeleteFileMatches(const filePattern: String);
var
  rc:     Integer;
  found:  TSearchRec;
  path:   String;

begin
  path := ExtractFilePath(filePattern);

  rc := FindFirst(filePattern, 0, found);
  try
    while rc = 0 do begin
      DeleteFile(path + found.Name);
      rc := FindNext(found);
    end;  // while rc = 0
  finally
    SysUtils.FindClose(found);
  end;  // try .. finally

end;  // DeleteFileMatches


// -----------------------------------------------------------------------------
// ForEachFile
//
//
// -----------------------------------------------------------------------------
procedure DoForEachFile(const dirName: String; recursive: Boolean;
    onNextFile: TFileOpNotifyEvent; yourRef: Pointer); forward;

procedure ForEachFile(const dirName: String; recursive: Boolean;
    onProgress: TFileOpProgressEvent; onNextFile: TFileOpNotifyEvent;
    yourRef: Pointer);
begin
  if Assigned(onNextFile) then begin
    mFileIndex := 0;
    mFileCount := CountObjects(dirName, recursive);
    mFileUpdateEvery := Round(mFileCount/ (100 * NOTIFY_STEP));
    if mFileUpdateEvery = 0 then mFileUpdateEvery := 1;
    mOnFileOpProgressed := onProgress;
    DoForEachFile(dirName, recursive, onNextFile, yourRef);
  end;  // if Assigned(OnNextFile)
end;

procedure DoForEachFile(const dirName: String; recursive: Boolean;
    onNextFile: TFileOpNotifyEvent; yourRef: Pointer);
var
  found:      TSearchRec;
  foundFile:  String;
  srcPath:    String;
  rc:         Integer;

begin
  srcPath := DirToPath(dirName);

  rc := FindFirst(srcPath + '*.*', faAnyFile, found);
  try
    while rc = 0 do begin
      if Pos(found.Name, '..') = 0 then begin

        foundFile := srcPath + found.Name;

        if recursive and HasAttr(foundFile, faDirectory) then
            DoForEachFile(foundFile, True, onNextFile, yourRef);

        onNextFile(foundFile, yourRef);
        if Assigned(mOnFileOpProgressed) then FileProcessed;

      end;  // if Pos(found.Name, '..') = 0

      rc := FindNext(found);
    end;  // while rc = 0
  finally
    SysUtils.FindClose(found);
  end;  // try .. finally

end;  // ForEachFile


// -----------------------------------------------------------------------------
// CountObjects
//
// Counts the number of files/directories in a given directory.  Optionally
// recursive.
// -----------------------------------------------------------------------------
function CountObjects(const dirName: String; recursive: Boolean): Integer;
var
  count:  Integer;
  found:  TSearchRec;
  path:   String;
  rc:     Integer;

begin
  // calculate search mask
  path := DirToPath(dirName);

  // intialise
  count := 0;

  // count objects
  rc := FindFirst(path + '*.*', faAnyFile, found);
  try
    while rc = 0 do begin
      if Pos(found.Name, '..') = 0 then begin

        if recursive and HasAttr(path +  found.Name, faDirectory) then begin
          // include objects in subdirectory
          count := count + CountObjects(path + found.Name, True);
        end;  // if recursive and HasAttr(..)

        count := count + 1;  // include object in count

      end;  // if Pos(..) = 0
      rc := FindNext(found);
    end;  // while rc = 0

  finally
    SysUtils.FindClose(found);
  end;  // try .. finally

  Result := count;

end;  // CountObjects


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
function HasAttr(const FileName: string; Attr: Word): Boolean;
var
  attributes:  Integer;

begin
  attributes := FileGetAttr(FileName);
  if attributes = -1 then
    // failed to get attributes: file does not exist
    Result := False
  else
    Result := (attributes and Attr) = Attr;
end;  // HasAttr


// -----------------------------------------------------------------------------
// CheckPathExists - IPA
//
// Checks the specified path exists and attemps to create it if not.
// Not to be confused with CreateDir which fails if the subdirectory does not exist
// -----------------------------------------------------------------------------
procedure CheckPathExists(PathName: String);
var
  sSubPath: String;
begin
  //If a drive is specified, we assume the path does exist
  if PathName[Length(PathName)] <> ':' then
  begin
	  //Remove trailing \ if required
	  if PathName[Length(PathName)] = '\' then
	    PathName:= Copy(PathName, 0, Length(PathName) - 1);

	  //If directory does not exist, attempt to create it
	  if not DirectoryExists(PathName) then
	  begin
	    //Use ExtractFileDir to get the subdirectory
	    sSubPath:= ExtractFileDir(PathName);

	    //Check subdirectory exists
	    CheckPathExists(sSubPath);

      //Create new directory
	    if not SysUtils.CreateDir(PathName) then
        Raise EFileUtilsError.CreateFmt(ResStr_CreateDirFail, [PathName]);
    end;
  end;
end;

end.
