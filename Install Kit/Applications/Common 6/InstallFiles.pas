{===============================================================================
  Unit:        CopyFiles.pas

  Defines:     TInstallFiles

  Description: Copy installation files to target machine, and unzip additional
               files from zipped archives. Display and update progress info
               during file copy.

  Model:

  Created:     March 2003

  Last revision information:
    $Revision: 10 $
    $Date: 22/04/09 9:51 $
    $Author: Ericsalmon $

===============================================================================}

unit InstallFiles;

interface

uses
  Windows, SysUtils, Classes, Forms, Dialogs, StdCtrls, ComCtrls, FileCtrl,
  Settings, VCLUnzip, SetupConstants, GeneralFunctions, SHFolder;

resourcestring
  ResStr_CopyFile  = 'Copying file';
  ResStr_Unpacking = 'Unpacking remaining application files...';

type
  ECopierError = class(Exception)
  end;
  
  TInstallFiles = class(TObject)
  private
    FCancelled: Boolean;
    FCurrentFile: TLabel;
    FFileList: TStringList;
    FFileProgress: TProgressBar;
    FOverallProgress: TProgressBar;
    FSettings: TSettings;
    FTotalBytes: Integer;
    FUnzipper: TVCLUnzip;
    FZipFiles: TStringList;
    procedure InstallFile(const ASource, ADest: String; ABytesCopied : Integer);
    procedure PreprocessZipFile(const ATargetPath: String);
    procedure SetFileProgress(APosition: Integer);
    procedure SetOverallProgress(APosition: Integer);
    procedure UnzipFile(const ATargetPath, AFileName: String);
    procedure ZipStartUnzipFile(Sender: TObject; FileIndex: Integer; var FName: string; var
        Skip: Boolean);
    procedure ZipTotalPercentDone(Sender: TObject; Percent: LongInt);
  public
    constructor Create(ASettings: TSettings; ALabel: TLabel; AFileProgress, AOverallProgress:
        TProgressBar);
    destructor Destroy; override;
    procedure Cancel;
    procedure CopyFiles;
    procedure GetFilesIn(const ASourcePath, ADestPath: String; const AFilter: String = '*.*');
    property ZipFiles: TStringList read FZipFiles;
  end;
  
//==============================================================================
implementation

const
  CHUNK_SIZE = 65536; // 64k chunck, arbitrary

  // Overall progress bar. Goes up to 90 for files, leaves 10 for the unzip.
  MAX_OVERALL_FOR_FILES = 90;

{-==============================================================================
    TInstallFiles
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TInstallFiles.Create(ASettings: TSettings; ALabel: TLabel; AFileProgress,
    AOverallProgress: TProgressBar);
begin
  FCancelled       := False;
  FSettings        := ASettings;
  FFileProgress    := AFileProgress;
  FOverallProgress := AOverallProgress;
  FCurrentFile     := ALabel;

  FFileList := TStringList.Create;
  // Count the bytes we are to copy, for progress info
  FTotalBytes := 0;

  // Prepare Unzip component and set a few properties.
  FUnzipper := TVCLUnzip.Create(nil);
  with FUnzipper do begin
    OnTotalPercentDone := ZipTotalPercentDone;
    OnStartUnZip       := ZipStartUnzipFile;
    DoAll              := True;
    RecreateDirs       := True;
    RetainAttributes   := False;
    OverwriteMode      := Always;
  end;

  FZipFiles := TStringList.Create;
end;  // TInstallFiles.Create

{-------------------------------------------------------------------------------
}
destructor TInstallFiles.Destroy;
begin
  try
    FUnzipper.Free;
  except
    on Exception do;
  end;
  FFileList.Free;
  FZipFiles.Free;
  inherited;
end;  // TInstallFiles.Destroy

{-------------------------------------------------------------------------------
}
procedure TInstallFiles.Cancel;
begin
  FUnzipper.CancelTheOperation;
  FCancelled := True;
end;  // TInstallFiles.Cancel

{-------------------------------------------------------------------------------
}
procedure TInstallFiles.CopyFiles;
var
  i, idx, bytesCopied: Integer;
  destRoot, userRoot: String;
begin
  bytesCopied := 0; // for progress bar
  // Lop "\" off end of dest root, if present, as it is already there in the list of files
  destRoot := ExcludeTrailingPathDelimiter(FSettings.InstallFolder);

  with FFileList do
    for i := 0 to Count - 1 do
      if not FCancelled then begin
        SetOverallProgress(Round((bytesCopied / FTotalBytes) * MAX_OVERALL_FOR_FILES));
        FCurrentFile.Caption := Format('%s: %s', [ResStr_CopyFile, ValueFromIndex[i]]);
        Application.ProcessMessages;
        // Ensure destination directories exist, and logged for uninstall...
        FSettings.ForceFolders(ExtractFilePath(destRoot + ValueFromIndex[i]));
        // And log destination filename too.
        FSettings.AddFileName(destRoot + ValueFromIndex[i]);

        InstallFile(Names[i], destRoot + ValueFromIndex[i], bytesCopied);
        Inc(bytesCopied, Integer(Objects[i]));
      end;

  if not FCancelled and (FZipFiles.Count > 0) then begin
    // Unzip the folder structure together with application files, saves having to do
    // each bit one at a time.
    FCurrentFile.Caption := ResStr_Unpacking;
    Application.ProcessMessages;
    for i := 0 to FZipFiles.Count - 1 do
      UnzipFile(IncludeTrailingPathDelimiter(FZipFiles.ValueFromIndex[i]), FZipFiles.Names[i]);

    // For Vista, need to move the following folders to "ProgramData\<Recorder 6>\"
    // - "Map Files"
    // - "Object Sheet"
    // and the following to "Public\Documents\<Recorder 6>"
    // - "User Files"
    if (FSettings.InstallType=itStandalone) and (FSettings.OSVersion >= wvVista) then begin
      destRoot :=
          GetFolder(CSIDL_COMMON_APPDATA)
          + ExtractFileName(ExcludeTrailingPathDelimiter(FSettings.InstallFolder));
      userRoot :=
          GetFolder(CSIDL_COMMON_DOCUMENTS)
          + ExtractFileName(ExcludeTrailingPathDelimiter(FSettings.InstallFolder));
      bytesCopied := 0;
      // Mantis 423/554 Map Files and object sheets go with user files 
      idx := FFileList.Count;
      GetFilesIn(FSettings.InstallFolder + 'Map Files', userRoot + '\Map Files');
      GetFilesIn(FSettings.InstallFolder + 'Object Sheet', userRoot + '\Object Sheet');
      GetFilesIn(FSettings.InstallFolder + 'User Files', userRoot + '\User Files');
      // If a folder is empty, it won't be picked up, so force the folder regardless.
      FSettings.ForceFolders(userRoot + '\Map Files');
      FSettings.ForceFolders(userRoot + '\Object Sheet');
      FSettings.ForceFolders(userRoot + '\User Files');
      // Proceed with individual files.
      for i := idx to FFileList.Count - 1 do begin
        FCurrentFile.Caption := Format('%s: %s', [ResStr_CopyFile, FFileList.ValueFromIndex[i]]);
        Application.ProcessMessages;
        FSettings.ForceFolders(ExtractFilePath(FFileList.ValueFromIndex[i]));
        FSettings.AddFileName(FFileList.ValueFromIndex[i]);
        InstallFile(FFileList.Names[i], FFileList.ValueFromIndex[i], bytesCopied);
      end;
      // Files moved, delete the originals.
      RemoveFolderAndContent(FSettings.InstallFolder + 'Map Files');
      RemoveFolderAndContent(FSettings.InstallFolder + 'Object Sheet');
      RemoveFolderAndContent(FSettings.InstallFolder + 'User Files');
    end;
  end;
  SetOverallProgress(100);
  Application.ProcessMessages;
end;  // TInstallFiles.CopyFiles 

{-------------------------------------------------------------------------------
 Gets the files from the Source folder and map them to the Destination folder as
 a list of key-value pairs (source=dest, source=dest...)
 Note, the provided paths are included in the key and value values.
}
procedure TInstallFiles.GetFilesIn(const ASourcePath, ADestPath: String;
    const AFilter: String = '*.*');
var
  lSearchRec: TSearchRec;
begin
  if FindFirst(ASourcePath + '\' + AFilter, faReadOnly + faDirectory, lSearchRec) = 0 then
    repeat
      // Ignore DOS directory maps
      if (lSearchRec.Name <> '.') and (lSearchRec.Name <> '..') then begin
        { for directories, recurse into them }
        if (lSearchRec.Attr and faDirectory) > 0 then
          GetFilesIn(ASourcePath + '\' + lSearchRec.Name, ADestPath + '\' + lSearchRec.Name)
        else begin
          { Record the file and size (string/object on stringlist) plus count total }
          FTotalBytes := FTotalBytes + lSearchRec.Size;
          FFileList.AddObject(ASourcePath + '\' + lSearchRec.Name + '=' +
                              ADestPath + '\' + lSearchRec.Name, Ptr(lSearchRec.Size));
        end;
      end;
    until FindNext(lSearchRec) <> 0;
  // Clean up.
  FindClose(lSearchRec);
end;  // TInstallFiles.GetFilesIn

{-------------------------------------------------------------------------------
}
procedure TInstallFiles.InstallFile(const ASource, ADest: String; ABytesCopied : Integer);
var
  lSource, lDest: TFileStream;
begin
  if FCancelled then Exit;
  
  SetFileProgress(0);
  lSource := TFileStream.Create(ASource, fmOpenRead);
  try
    if FileExists(ADest) then
      lDest := TFileStream.Create(ADest, fmOpenWrite + fmShareExclusive)
    else
      lDest := TFileStream.Create(ADest, fmCreate);
  except
    on E:EFOpenError do begin
      lSource.Free;
      raise ECopierError.Create('Error occurred copying file ' + ASource + #13#10 + E.Message);
    end;
  end;
  
  try
    while lSource.Size - lSource.Position > CHUNK_SIZE do
      if not FCancelled then begin
        lDest.Copyfrom(lSource, CHUNK_SIZE);
        SetFileProgress(Round((lSource.Position / lSource.Size) * 100));
        SetOverallProgress(Round(((ABytesCopied + lSource.Position) / FTotalBytes) *
                                 MAX_OVERALL_FOR_FILES));
      end; // while
    if not FCancelled then begin
      // Copy the final incomplete chunk
      lDest.Copyfrom(lSource, lSource.Size - lSource.Position);
      lDest.Size := lDest.Position; // in case the original file was larger
      SetFileProgress(100);
    end;
  finally
    lSource.Free;
    lDest.Free;
  end; // try
end;  // TInstallFiles.InstallFile 

{-------------------------------------------------------------------------------
}
procedure TInstallFiles.PreprocessZipFile(const ATargetPath: String);
var
  i: Integer;
begin
  FUnzipper.ReadZip;
  AppStartCursor;  // Unzip component has hardcoded cursor values!!!
  for i := 0 to FUnzipper.Count - 1 do begin
    if not DirectoryExists(ATargetPath + FUnzipper.PathName[i]) then
      FSettings.AddFolderName(ATargetPath + FUnzipper.PathName[i]);

    if not FileExists(ATargetPath + FUnzipper.FullName[i]) then
      FSettings.AddFileName(ATargetPath + FUnzipper.FullName[i]);
  end;
  Application.ProcessMessages;
end;  // TInstallFiles.PreprocessZipFile 

{-------------------------------------------------------------------------------
}
procedure TInstallFiles.SetFileProgress(APosition: Integer);
begin
  if Assigned(FFileProgress) then
    FFileProgress.Position := APosition;
  Application.ProcessMessages;
end;  // TInstallFiles.SetFileProgress 

{-------------------------------------------------------------------------------
}
procedure TInstallFiles.SetOverallProgress(APosition: Integer);
begin
  if Assigned(FOverallProgress) then
    FOverallProgress.Position := APosition;
  Application.ProcessMessages;
end;  // TInstallFiles.SetOverallProgress 

{-------------------------------------------------------------------------------
}
procedure TInstallFiles.UnzipFile(const ATargetPath, AFileName: String);
begin
  with FUnzipper do begin
    ZipName := AFileName;
    DestDir := ATargetPath;
    PreprocessZipFile(ATargetPath);
    try
      UnZip;
    except
      on EUserCanceled do;  // Seems only to react ages after we asked it to stop!!!!
    end;
  end;
  Application.ProcessMessages;
end;  // TInstallFiles.UnzipFile 

{-------------------------------------------------------------------------------
}
procedure TInstallFiles.ZipStartUnzipFile(Sender: TObject; FileIndex: Integer; var FName:
    string; var Skip: Boolean);
begin
  if FileExists(FName) then
    Skip := True
  else
    Skip := FCancelled;
end;  // TInstallFiles.ZipStartUnzipFile 

{-------------------------------------------------------------------------------
}
procedure TInstallFiles.ZipTotalPercentDone(Sender: TObject; Percent: LongInt);
begin
  AppStartCursor;  // Unzip component has hardcoded cursor values!!!
  SetFileProgress(Percent);
  SetOverallProgress(MAX_OVERALL_FOR_FILES + (Percent div (10 * FZipFiles.Count)));
  Application.ProcessMessages;
end;  // TInstallFiles.ZipTotalPercentDone 

end.
