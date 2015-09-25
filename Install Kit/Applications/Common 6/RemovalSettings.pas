{===============================================================================
  Unit:        RemovalSettings

  Defines:     TRemovalSettings

  Description:

  Model:

  Created:     March 2004

  Last revision information:
    $Revision: 2 $
    $Date: 19/12/08 17:28 $
    $Author: Ericsalmon $

===============================================================================}

unit RemovalSettings;

interface

uses
  Windows, Controls, Classes, Dialogs, SysUtils, SQLList, Registry, SetupConstants,
  TextMessages, GeneralFunctions, ShlObj, ActiveX, APIUtils, ShellAPI, Forms,
  ExceptionForm, VCLUnzip, Settings;

type
  TRemovalSettings = class(TSettings)
  private
    FArchiveDatabase: Boolean;
    FArchiveZipName: String;
    FMigrationAccessDBPath: String;
    FR2K2InstallPath: String;
  public
    procedure LoadRemovalSettings;
    property ArchiveDatabase: Boolean read FArchiveDatabase write FArchiveDatabase;
    property ArchiveZipName: String read FArchiveZipName write FArchiveZipName;
    property MigrationAccessDBPath: String read FMigrationAccessDBPath write
        FMigrationAccessDBPath;
    property R2K2InstallPath: String read FR2K2InstallPath write FR2K2InstallPath;
  end;
  
//==============================================================================
implementation

uses
  Functions;

{-==============================================================================
    TRemovalSettings
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TRemovalSettings.LoadRemovalSettings;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_READ;
      // Recorder 6 install path, for database archiving.
      if OpenKeyReadOnly(REG_KEY_R6) then begin
        FArchiveZipName :=
            IncludeTrailingPathDelimiter(ReadString('Installation Path')) + 'Recorder 2002 Database.zip';
        CloseKey;
      end;
      // Recorder 2002 install path, for files and folders removal.
      if OpenKeyReadOnly(REG_KEY_R2K2) then begin
        FR2K2InstallPath :=
            ExpandLongPathName(IncludeTrailingPathDelimiter(ReadString('Local Install Dir')));
        CloseKey;
      end;
      // Recorder 2002 Database path, for archiving.
      RootKey := HKEY_CURRENT_USER;
      if OpenKeyReadOnly(REG_KEY_R2K2_SETTINGS) then begin
        FMigrationAccessDBPath := ExpandLongPathName(ExtractFilePath(ReadString('Database Path')));
        CloseKey;
      end;
    finally
      Free;
    end;
  FArchiveDatabase := True;
end;  // TRemovalSettings.LoadRemovalSettings 

end.
