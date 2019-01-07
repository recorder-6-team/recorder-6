{===============================================================================
  Unit:         DefaultPaths.pas

  Implements:

  Description:  Default paths for various types of file in Recorder.

  Created:      February 2009  

  Last Revision Details:
    $Revision: 2 $
    $Date: 23/02/09 14:39 $
    $Author: Andrewkemp $

  Copyright © Dorset Software Services Ltd, 2009
===============================================================================}
unit DefaultPaths;

interface

const
  PATH_MAP_FILES        = 'Map Files\';
  PATH_OBJECT_SHEETS    = 'Object Sheet\';
  PATH_USER_FILES       = 'User Files\';
  PATH_BASE_MAPS        = 'Base Maps\';
  PATH_DTD              = 'DTD\';
  PATH_ERRORS           = 'Errors\';
  PATH_DICT_IMAGES      = 'Database\Images';
  PATH_DICT_UPGRADE     = 'User Files\';
  PATH_IMPORT_TEMPLATES = PATH_USER_FILES + 'Import Templates\';
  PATH_POLYGON_FILTERS  = PATH_USER_FILES + 'Polygon Filter\';
  PATH_RECORDING_CARDS  = PATH_USER_FILES + 'Recording Cards\';
  PATH_CUSTOM_SPECIES_CARDS = PATH_USER_FILES + 'Custom Species Cards\';
  PATH_REPORTS          = PATH_USER_FILES + 'Reports\';
  PATH_RUCKSACKS        = PATH_USER_FILES + 'Rucksacks\';
  PATH_SNAPSHOTS        = PATH_USER_FILES + 'Snapshots\';
  PATH_REPORT_TEMPLATES = PATH_USER_FILES + 'Templates\';
  PATH_EXPORT_TEMPLATES = PATH_USER_FILES + 'Export Templates\';  
  PATH_LOCAL_IMAGES     = PATH_USER_FILES + 'User Dictionary Images\';
  PATH_BATCH_UPDATES    = PATH_USER_FILES + 'Batch Updates\';
  PATH_EXTERNAL_FILES   = PATH_USER_FILES + 'External Files\';  


function GetProgramDataFolder(
    const RelativePath: String): String; overload;
function GetProgramDataFolder(
    const RecorderAppPath, RelativePath: String): String; overload;


implementation

uses
  SysUtils, Windows, Forms, GeneralFunctions, SHFolder;


{-------------------------------------------------------------------------------
  Returns a value indicating whether this installation of Recorder is running
  on a version of Windows where the 'User Files' folder is installed to the
  same directory as the executable.
}
function UseLegacyFolders: Boolean;
begin
  Result := (Win32Platform < VER_PLATFORM_WIN32_NT)
      or ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 6));
end;


{-------------------------------------------------------------------------------
  Takes a file path relative to Recorder's program data folder, and returns a
  fully qualified path.

  On Windows Vista or later, Recorder's program data folder is a directory
  beneath %ProgramData% with the same name as the directory that contains the
  executable.  On earlier versions of Windows, the program data folder is the
  same as the directory that contains the executable.
}
function GetProgramDataFolder(const RelativePath: String): String;
begin
  Result := GetProgramDataFolder(
      ExtractFilePath(Application.ExeName),
      RelativePath);
end;

function GetProgramDataFolder(
  const RecorderAppPath, RelativePath: String): String;
var
  OldPath: Boolean;
  ApplicationFolderName: String;
begin
  OldPath := UseLegacyFolders;
  if not OldPath then
  begin
    ApplicationFolderName :=
        ExtractFileName(ExtractFileDir(Application.ExeName));
    Result := GetFolder(CSIDL_COMMON_APPDATA)
        + ApplicationFolderName + '\'
        + RelativePath;
    OldPath := not DirectoryExists(Result);
  end;

  if OldPath then Result := RecorderAppPath + RelativePath;
end;


end.
