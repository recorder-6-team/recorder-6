{-------------------------------------------------------------------------------
  Unit:        Shortcuts.pas

  Defines:     TShortcuts

  Description: Class to create Recorder shortcut on desktop and startmenu

  Created:     January 2003

  Last revision information:
    $Revision: 8 $
    $Date: 24/04/03 12:21 $
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
    FStartMenuPath : String;
    procedure CreateLink(const ObjectPath, LinkPath, Description: String);
  public
    constructor Create;
    procedure ShortcutToGettingStarted(const APathToHelp: String);
    procedure ShortcutToExe(const APathToExe: String);
    procedure RemoveShortcuts;
  end;

//==============================================================================
implementation

uses StrUtils;

//==============================================================================
constructor TShortcuts.Create;
begin
  with TRegistry.Create do
    try
      if Win32Platform = VER_PLATFORM_WIN32_NT then begin
        RootKey := HKEY_LOCAL_MACHINE;
        OpenKeyReadOnly('Software\MicroSoft\Windows\CurrentVersion\Explorer\Shell Folders');
        FDesktopPath   := ReadString('Common Desktop');
        FStartMenuPath := ReadString('Common Start Menu') + '\Programs\JNCC\Recorder';
        CloseKey;
      end else begin
        // Win98 doesn't have the same registry entries for shell folders
        RootKey := HKEY_CURRENT_USER;
        OpenKeyReadOnly('Software\MicroSoft\Windows\CurrentVersion\Explorer\Shell Folders');
        FDesktopPath   := ReadString('Desktop');
        FStartMenuPath := ReadString('Programs') + '\JNCC\Recorder';
        CloseKey;
      end;
      // Ensure directory exists
      ForceDirectories(FStartMenuPath);
    finally
      Free;
    end;
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
procedure TShortcuts.RemoveShortcuts;
begin
  // Links on desktop
  DeleteFile(FDesktopPath + '\Recorder.lnk');
  DeleteFile(FDesktopPath + '\Recorder Getting Started Guide.lnk');

  { StartMenu items, delete the whole JNCC folder tree, and all links stored there.
    FStartMenuPath = [Start Menu]\Programs\JNCC\Recorder.}
  RemoveFileOrFolder(FStartMenuPath);
  // Shrink FStartMenuPath to [Start Menu]\Programs\JNCC and see if this can be removed too
  FStartMenuPath := ExcludeTrailingPathDelimiter(ExtractFilePath(FStartMenuPath));
  // But check it is the right folder!
  if CompareText(RightStr(FStartMenuPath, 5), '\Jncc') = 0 then
    RemoveFileOrFolder(FStartMenuPath);
end;

{-------------------------------------------------------------------------------
 Description : Create a shortcut to the exe on the desktop
 Created : 21/1/2003 }
procedure TShortcuts.ShortcutToExe(const APathToExe: String);
begin
  try
    // Desktop link
    CreateLink(APathToExe + 'Recorder.exe', FDesktopPath + '\Recorder.lnk', 'Recorder');
    // Start Menu link
    CreateLink(APathToExe + 'Recorder.exe', FStartMenuPath + '\Recorder.lnk', 'Recorder');
  except
    on Exception do ;  // don't bomb out for something as simple as a missing shortcut
  end;
end;

{-------------------------------------------------------------------------------
 Description : Create a shortcut to the Getting Started Guide on the desktop
 Created : 21/1/2003 }
procedure TShortcuts.ShortcutToGettingStarted(const APathToHelp: String);
begin
  try
    // Desktop link
    CreateLink(APathToHelp + 'Getting Started\Recorder Getting Started Guide.chm',
               FDesktopPath + '\Recorder Getting Started Guide.lnk', 'Recorder Getting Started Guide');
    // Start Menu link
    CreateLink(APathToHelp + 'Getting Started\Recorder Getting Started Guide.chm',
               FStartMenuPath + '\Recorder Getting Started Guide.lnk', 'Recorder Getting Started Guide');
  except
    on Exception do ;  // don't bomb out for something as simple as a missing shortcut
  end;
end;

//------------------------------------------------------------------------------
end.
