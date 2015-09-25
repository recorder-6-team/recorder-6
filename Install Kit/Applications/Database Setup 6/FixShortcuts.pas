{-------------------------------------------------------------------------------
  Unit:         FixShortcuts.pas

  Defines:

  Description:

  Author:       John van Breda
  Created:      February 2003

  Last revision information:
    $Revision: 1 $
    $Date: 14/05/04 17:04 $
    $Author: Ericsalmon $

  Copyright © Dorset Software Services Ltd, 2003

-------------------------------------------------------------------------------}

unit FixShortcuts;

interface

uses
  Windows, ShlObj, SysUtils, Forms, ActiveX, Registry, ComObj, FileUtils,
  ApiUtils, ComCtrls, StdCtrls;

procedure DoFixShortcuts(AProgressBar : TProgressBar; AListBox : TListBox);

procedure GetPaths;
procedure UpdateProgress(PercentageComplete: Integer);
procedure Log(const AText: string);
procedure HandleFile(const FileName: String; YourRef: Pointer);
procedure ProcessLink(const AFileName: string);


//==============================================================================
implementation

var
  mDesktopPath : string;
  mStartMenuPath : string;
  mProgressBar : TProgressBar;
  mLogListBox : TListBox;

//==============================================================================
{ TFixShortcuts }
{-------------------------------------------------------------------------------
  Description : Look for any shortcut to Recorder 2000/2002, rename it and
              repoint it to the new exe
  Created : 14/02/2003 }
procedure DoFixShortcuts(AProgressBar : TProgressBar; AListBox : TListBox);
begin
  GetPaths;
  mProgressBar := AProgressBar;
  mLogListBox := AListBox;
  Log('Scanning directories under ' + mStartMenuPath);
  ForEachFile(mStartMenuPath, true, UpdateProgress, HandleFile, nil);
  Log('Scanning directories under ' + mDesktopPath);
  ForEachFile(mDesktopPath, true, UpdateProgress, HandleFile, nil);
end;

{-------------------------------------------------------------------------------
  Description : Retrieve the desktop and start menu paths from the desktop
  Created : 14/02/2003 }
procedure GetPaths;
var
  lReg : TRegistry;
begin
  lReg :=   TRegistry.Create;
  try
    lReg.RootKey := HKEY_LOCAL_MACHINE;
    lReg.OpenKeyReadOnly('Software\MicroSoft\Windows\CurrentVersion\Explorer\Shell Folders');
    mDesktopPath := lReg.ReadString('Common Desktop');
    mStartMenuPath := lReg.ReadString('Common Start Menu');
  finally
    lReg.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Description : For each file found, check if it is a shortcut that needs an
              update, if so then do the update 
  Created : 14/02/2003 }
procedure HandleFile(const FileName: String; YourRef: Pointer);
var
  lstNewFileName : string;
begin
  if (CompareText(ExtractFileExt(fileName), '.lnk')=0) then
    ProcessLink(fileName);
  if (CompareText(copy(ExtractFileName(fileName), 1, 12), 'Recorder 200')=0) then
    if (CompareText(ExtractFileExt(fileName), '.lnk')=0) or
           ((FileGetAttr(fileName) and faDirectory)>0) then begin
      try
        lstNewFileName := ExtractFilePath(fileName) + 'Recorder' +
                             copy(ExtractFileName(fileName), 14, 255);
        RenameFile(fileName, lstNewFileName);
        ProcessLink(lstNewFileName);
        Log('Renamed ' + lstNewFileName);
      except
        on Exception do ; // don't worry if this procedure fails
      end;
    end;
end;

{-------------------------------------------------------------------------------
  Description : Ensure a shortcut to Recorder2000.exe now points to Recorder.exe
  Created : 14/02/2003 }
procedure ProcessLink(const AFileName: string);
var
  slI: IShellLink;
  pfI: IPersistFile;  // needed to load/save the .lnk file
  pszLongFileName: PWideChar;
  LinkFile: String;
  a: array[0..MAX_PATH] of Char;
  fd: TWin32FindData;
  hRes : Integer;
  lstTargetPath : string;

begin
  Log('Found shortcut ' + AFileName);
  CoInitialize(nil);     // initializes COM-engine.
    { You need to call this at least once before you
      use the CoCreateInstance function, and for every
      call to CoInitialize you have to call CoUninitialize. }
  try
    hRes := CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
                               IID_IShellLinkA, slI);
      // creates IShellLink interface
    pfI := slI as IPersistFile;

    // ------- Load the .lnk file ------- //
    LinkFile := AFileName;
    try
      GetMem(pszLongFileName, Length(LinkFile) * 2 + 2);
      StringToWideChar(LinkFile, pszLongFileName, Length(LinkFile) * 2 + 2);
      // IPersistFile needs WideChar file name
      gpcAPIResultCheck(pfI.Load(pszLongFileName, 0));
    finally
      FreeMem(pszLongFileName);
    end;


    // ------ Now use IShellLink's methods to get the information
    hRes := slI.GetPath(@a, MAX_PATH, fd, 0);
     {                   ^     ^       ^  +--- flags. not needed
                         |     |       +------ Win32FindData record
                         |     +-------------- Buffer's Length
                         +------- Buffer to write the path to. }

    lstTargetPath := String(a);

    // If a shortcut to REcorder2000.exe, then change it
    if CompareText(ExtractFileName(lstTargetPath),'Recorder2000.exe')=0 then begin
      slI.SetPath(PChar(ExtractFilePath(lstTargetPath) + 'Recorder.exe'));
      pfI.Save(nil,false);
    end;
    slI := nil;
    pfI := nil;

  finally
    CoUninitialize;
  end;
end;

{-------------------------------------------------------------------------------
  Description : Update the progress bar as shortcuts are checked
  Created : 14/02/2003 }
procedure UpdateProgress(PercentageComplete: Integer);
begin
  if Assigned(mProgressBar) then
    mProgressBar.Position := PercentageComplete;
end;

{-------------------------------------------------------------------------------
  Description : If list box is assigned, add event details
  Created : 21/02/2003 }
procedure Log(const AText : string);
begin
  if Assigned(mLogListBox) then
    mLogListBox.Items.Add(AText);
end;

//==============================================================================
end.
