{-------------------------------------------------------------------------------
  Unit:        Shortcuts.pas

  Defines:     TShortcuts

  Description: Class to create Recorder shortcut on desktop and startmenu

  Created:     January 2003

  Last revision information:
    $Revision: 5 $
    $Date: 3/07/09 11:54 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit Shortcuts;

interface

uses
  Windows, ShlObj, SysUtils, Forms, ActiveX, Registry, ComObj, APIUtils;

type
  TShortcuts = class
  private
    FDesktopPath : String;
    FRecorderStartMenu: String;
    FStartMenuPath : String;
    procedure CreateLink(const ObjectPath, LinkPath, Description: String);
  public
    constructor Create(const ARecorderStartMenu: String);
    procedure ShortcutToGettingStarted(const APathToHelp, ALinkName: String);
    procedure ShortcutToExe(const APathToExe, ALinkName: String);
    procedure RemoveShortcuts(const ALinks: Array of String);
  end;

//==============================================================================
implementation

uses
  StrUtils, SetupConstants, TextMessages;

//==============================================================================
constructor TShortcuts.Create(const ARecorderStartMenu: String);
begin
  FRecorderStartMenu := ARecorderStartMenu;
  with TRegistry.Create do
    try
      if Win32Platform = VER_PLATFORM_WIN32_NT then begin
        RootKey := HKEY_LOCAL_MACHINE;
        Access  := KEY_READ;
        OpenKeyReadOnly('Software\MicroSoft\Windows\CurrentVersion\Explorer\Shell Folders');
        FDesktopPath   := ReadString('Common Desktop');
        FStartMenuPath := ReadString('Common Programs') + '\' + FRecorderStartMenu;
        CloseKey;
      end else begin
        // Win98 doesn't have the same registry entries for shell folders
        RootKey := HKEY_CURRENT_USER;
        Access  := KEY_READ;
        OpenKeyReadOnly('Software\MicroSoft\Windows\CurrentVersion\Explorer\Shell Folders');
        FDesktopPath   := ReadString('Desktop');
        FStartMenuPath := ReadString('Programs') + '\' + FRecorderStartMenu;
        CloseKey;
      end;
      // Ensure directory exists
      ForceDirectories(FStartMenuPath);
    finally
      Free;
    end;
  FDesktopPath   := IncludeTrailingPathDelimiter(FDesktopPath);
  FStartMenuPath := IncludeTrailingPathDelimiter(FStartMenuPath);
end;

//------------------------------------------------------------------------------
procedure TShortcuts.CreateLink(const ObjectPath, LinkPath, Description: string);
var MyObject : IUnknown;
    MySLink  : IShellLink;
    MyPFile  : IPersistFile;
    WFileName: WideString;
begin
  MyObject := CreateComObject(CLSID_ShellLink);
  MySLink := MyObject as IShellLink;
  MyPFile := MyObject as IPersistFile;
  with MySLink do begin
    SetPath(PChar(ObjectPath));
    SetWorkingDirectory(PChar(ExtractFilePath(ObjectPath)));
    SetDescription(Pchar(Description));
  end;
  WFileName := LinkPath;
  MyPFile.Save(PWChar(WFileName), False);
end;

//------------------------------------------------------------------------------
procedure TShortcuts.RemoveShortcuts(const ALinks: Array of String);
var
  i: Integer;
begin
  // Links on desktop
  for i := 0 to High(ALinks) do
    DeleteFile(FDesktopPath + ALinks[i]);

  { StartMenu items, delete the whole [RecorderStartMenu] folder tree, and all links stored there.
    FStartMenuPath = [Start Menu]\Programs\[RecorderStartMenu].}
  RemoveFileOrFolder(FStartMenuPath);
  // Shrink FStartMenuPath to [Start Menu]\Programs\[RecorderStartMenu] and
  // see if this can be removed too.
  FStartMenuPath := ExcludeTrailingPathDelimiter(ExtractFilePath(FStartMenuPath));
  // But check it is the right folder! (That's why the '\' is included).
  if CompareText(RightStr(FStartMenuPath, Length(FRecorderStartMenu) + 1),
                 '\' + FRecorderStartMenu) = 0 then
    RemoveFileOrFolder(FStartMenuPath);
end;

{-------------------------------------------------------------------------------
 Description : Create a shortcut to the exe on the desktop
 Created : 21/1/2003 }
procedure TShortcuts.ShortcutToExe(const APathToExe, ALinkName: String);
begin
  try
    // Desktop link
    CreateLink(APathToExe + STR_RECORDER_SPLASH_EXE, FDesktopPath + ALinkName, 'Recorder');
    // Start Menu link
    CreateLink(APathToExe + STR_RECORDER_SPLASH_EXE, FStartMenuPath + ALinkName, 'Recorder');
  except
    on Exception do ;  // don't bomb out for something as simple as a missing shortcut
  end;
end;

{-------------------------------------------------------------------------------
 Description : Create a shortcut to the Getting Started Guide on the desktop
 Created : 21/1/2003 }
procedure TShortcuts.ShortcutToGettingStarted(const APathToHelp, ALinkName: String);
begin
  try
    // Desktop link
    CreateLink(
        APathToHelp + STR_GETTING_STARTED,
        FDesktopPath + ALinkName,
        ResStr_GettingStartedGuide);
    // Start Menu link
    CreateLink(
        APathToHelp + STR_GETTING_STARTED,
        FStartMenuPath + ALinkName,
        ResStr_GettingStartedGuide);
  except
    on Exception do ;  // don't bomb out for something as simple as a missing shortcut
  end;
end;

//------------------------------------------------------------------------------
end.
