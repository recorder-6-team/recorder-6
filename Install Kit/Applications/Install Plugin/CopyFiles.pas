{-------------------------------------------------------------------------------
  Unit:        CopyFiles.pas

  Defines:     TCopyFiles

  Description: Copy installation files to target machine, and unzip additional
               files from zipped archives. Display and update progress info
               during file copy.

  Created:     March 2003

  Last revision information:
    $Revision: 9 $
    $Date: 8/03/04 10:24 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit CopyFiles;

interface

uses
  Windows, SysUtils, Classes, Forms, Dialogs, StdCtrls, ComCtrls, FileCtrl,
  Settings, VCLUnzip, Constants, GeneralFunctions;

type
  ECopierError = class(Exception);

  TCopyFiles = class
  private
    FFileProgress   : TProgressBar;
    FOverallProgress: TProgressBar;
    FCurrentFile    : TLabel;
    FSettings       : TSettings;
    FFileList       : TStringList;
    FTotalBytes     : Integer;
    FUnzipper       : TVCLUnzip;
    FCancelled      : Boolean;
    procedure GetFilesIn(const APath, ADestPath: String; const AFilter: String = '*.*');
    procedure InstallFile(const ASource, ADest: String; ABytesCopied : Integer);
    procedure ZipTotalPercentDone(Sender: TObject; Percent: LongInt);
    procedure ZipStartUnzipFile(Sender: TObject; FileIndex: Integer;
      var FName: string; var Skip: Boolean);
    procedure PreprocessZipFile;
    procedure UnzipFile(const AFileName: String);
    procedure SetFileProgress(APosition: Integer);
    procedure SetOverallProgress(APosition: Integer);
  public
    constructor Create(ASettings: TSettings; ALabel: TLabel;
      AFileProgress, AOverallProgress: TProgressBar);
    destructor Destroy; override;
    procedure CopyFiles;
    procedure Cancel;
  end;

//==============================================================================
implementation

const
  CHUNK_SIZE = 65536; // 64k chunck, arbitrary

  // Overall progress bar. Goes up to 90 for files, leaves 10 for the unzip.
  MAX_OVERALL_FOR_FILES = 90;

//==============================================================================
{ TCopyFiles }
//------------------------------------------------------------------------------
constructor TCopyFiles.Create(ASettings: TSettings; ALabel: TLabel;
  AFileProgress, AOverallProgress: TProgressBar);
begin
  FCancelled       := False;
  FSettings        := ASettings;
  FFileProgress    := AFileProgress;
  FOverallProgress := AOverallProgress;
  FCurrentFile     := ALabel;

  FFileList := TStringList.Create;
  // Count the bytes we are to copy, for progress info
  FTotalBytes := 0;
  if FSettings.InstallMode <> imWorkstation then begin
    // Copy main files, no dest folder, all go to InstallFolder
    GetFilesIn(FSettings.RootFolder + 'Install Files', '');
    GetFilesIn(FSettings.RootFolder + 'System', '', STR_UNINSTALLER);
  end;

  case FSettings.InstallMode of
    imServer:
        begin
          // Copy "Getting Started" to "[InstallFolder]\Getting Started"
          GetFilesIn(FSettings.RootFolder + 'Getting Started', '\Getting Started');
          // Add the Workstation install files in the list of files to process
          FSettings.ForceFolders(FSettings.InstallFolder + 'System\');
          // CopyFile allows files in use to be copied, so use it!
          CopyFile(PChar(Application.ExeName), PChar(FSettings.InstallFolder + 'System\' + STR_INSTALLPLUGIN), False);
          FSettings.AddFileName(FSettings.InstallFolder + 'System\' + STR_INSTALLPLUGIN);
          GetFilesIn(FSettings.RootFolder + 'System\System Components', '\System\System Components');
          GetFilesIn(FSettings.RootFolder + 'System', '', STR_WORKSTATIONSETUP);
          GetFilesIn(FSettings.RootFolder + 'System', '\System', STR_SPATIAL_SYSTEMS);
        end;
        
    imWorkstation:
        // Copy "Base Maps" to "[InstallFolder]\Base Maps"
        GetFilesIn(FSettings.RootFolder + 'Base Maps', '\Base Maps');

    imUpgrade:
        FSettings.ResetReadOnlyFlag(FSettings.InstallFolder);
  end;

  // Prepare Unzip component and set a few properties.
  FUnzipper := TVCLUnzip.Create(nil);
  with FUnzipper do begin
    OnTotalPercentDone := ZipTotalPercentDone;
    OnStartUnZip := ZipStartUnzipFile;
    DestDir := FSettings.InstallFolder;
    DoAll := True;
    RecreateDirs := True;
    RetainAttributes := False;
    OverwriteMode := Always;
  end;
end;

//------------------------------------------------------------------------------
destructor TCopyFiles.Destroy;
begin
  try
    FUnzipper.Free;
  except
    on Exception do;
  end;
  FFileList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TCopyFiles.GetFilesIn(const APath, ADestPath: String; const AFilter: String = '*.*');
var lSearchRec: TSearchRec;
begin
  if FindFirst(APath + '\' + AFilter, faReadOnly + faDirectory, lSearchRec) = 0 then
    repeat
      // Ignore DOS directory maps
      if (lSearchRec.Name <> '.') and (lSearchRec.Name <> '..') then begin
        { for directories, recurse into them }
        if (lSearchRec.Attr and faDirectory) > 0 then
          GetFilesIn(APath + '\' + lSearchRec.Name, ADestPath + '\' + lSearchRec.Name)
        else begin
          { Record the file and size (string/object on stringlist) plus count total }
          FTotalBytes := FTotalBytes + lSearchRec.Size;
          FFileList.AddObject(APath + '\' + lSearchRec.Name + '=' + ADestPath + '\' + lSearchRec.Name,
                              Ptr(lSearchRec.Size));
        end;
      end;
    until FindNext(lSearchRec) <> 0;
  // Clean up.
  FindClose(lSearchRec);
end;

//------------------------------------------------------------------------------
procedure TCopyFiles.CopyFiles;
var i, lBytesCopied: Integer;
    lDestRoot      : String;
begin
  lBytesCopied := 0; // for progress bar
  // Lop "\" off end of dest root, if present, as it is already there in the list of files
  lDestRoot := ExcludeTrailingPathDelimiter(FSettings.InstallFolder);

  with FFileList do
    for i := 0 to Count - 1 do
      if not FCancelled then begin
        SetOverallProgress(Round((lBytesCopied / FTotalBytes) * MAX_OVERALL_FOR_FILES));
        FCurrentFile.Caption := 'Copying File: ' + Values[Names[i]];
        Application.ProcessMessages;
        // Ensure destination directories exist, and logged for uninstall...
        FSettings.ForceFolders(ExtractFilePath(lDestRoot + Values[Names[i]]));
        // And log destination filename too.
        FSettings.AddFileName(lDestRoot + Values[Names[i]]);

        InstallFile(Names[i], lDestRoot + Values[Names[i]], lBytesCopied);
        Inc(lBytesCopied, Integer(Objects[i]));
      end;

  if not FCancelled and
     (FSettings.InstallMode in [imStandalone, imServer, imUpgrade]) then
  begin
    // Unzip the folder structure together with application files, saves having to do
    // each bit one at a time.
    FCurrentFile.Caption := 'Unpacking remaining application files...';
    Application.ProcessMessages;
    UnzipFile(FSettings.RootFolder + STR_ZIPPED_FILES_1);
  end;
  SetOverallProgress(100);
  Application.ProcessMessages;
end;

//------------------------------------------------------------------------------
procedure TCopyFiles.UnzipFile(const AFileName: String);
begin
  with FUnzipper do begin
    ZipName := AFileName;
    PreprocessZipFile;
    try
      UnZip;
    except
      on EUserCanceled do;  // Seems only to react ages after we asked it to stop!!!!
    end;
  end;
  Application.ProcessMessages;
end;

//------------------------------------------------------------------------------
procedure TCopyFiles.InstallFile(const ASource, ADest: String; ABytesCopied: Integer);
var lSource, lDest: TFileStream;
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
      raise ECopierError.Create('Error occurred copying file ' + ASource + #13#10 +
                                E.Message);
    end;
  end;

  try
    while lSource.Size - lSource.Position > CHUNK_SIZE do
      if not FCancelled then begin
        lDest.Copyfrom(lSource, CHUNK_SIZE);
        SetFileProgress(Round((lSource.Position / lSource.Size) * 100));
        SetOverallProgress(Round(((ABytesCopied + lSource.Position) / FTotalBytes) * MAX_OVERALL_FOR_FILES));
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
end;

//------------------------------------------------------------------------------
procedure TCopyFiles.ZipTotalPercentDone(Sender: TObject; Percent: Integer);
begin
  AppStartCursor;  // Unzip component has hardcoded cursor values!!!
  SetFileProgress(Percent);
  SetOverallProgress(MAX_OVERALL_FOR_FILES + (Percent div 10));
  Application.ProcessMessages;
end;

//------------------------------------------------------------------------------
procedure TCopyFiles.ZipStartUnzipFile(Sender: TObject; FileIndex: Integer;
  var FName: string; var Skip: Boolean);
begin
  Skip := FCancelled;
end;

//------------------------------------------------------------------------------
procedure TCopyFiles.PreprocessZipFile;
var i: Integer;
begin
  FUnzipper.ReadZip;
  AppStartCursor;  // Unzip component has hardcoded cursor values!!!
  for i := 0 to FUnzipper.Count - 1 do begin
    if not DirectoryExists(FSettings.InstallFolder + FUnzipper.PathName[i]) then
      FSettings.AddFolderName(FSettings.InstallFolder + FUnzipper.PathName[i]);

    if not FileExists(FSettings.InstallFolder + FUnzipper.FullName[i]) then
      FSettings.AddFileName(FSettings.InstallFolder + FUnzipper.FullName[i]);
  end;
  Application.ProcessMessages;
end;

//------------------------------------------------------------------------------
procedure TCopyFiles.Cancel;
begin
  FUnzipper.CancelTheOperation;
  FCancelled := True;
end;

//------------------------------------------------------------------------------
procedure TCopyFiles.SetFileProgress(APosition: Integer);
begin
  if Assigned(FFileProgress) then
    FFileProgress.Position := APosition;
  Application.ProcessMessages;
end;

procedure TCopyFiles.SetOverallProgress(APosition: Integer);
begin
  if Assigned(FOverallProgress) then
    FOverallProgress.Position := APosition;
  Application.ProcessMessages;
end;

//------------------------------------------------------------------------------
end.
