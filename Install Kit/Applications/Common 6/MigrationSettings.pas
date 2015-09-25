{===============================================================================
  Unit:        Settings

  Defines:     TMigrationSettings

  Description:

  Model:

  Created:     March 2004

  Last revision information:
    $Revision: 6 $
    $Date: 19/12/08 17:28 $
    $Author: Ericsalmon $

===============================================================================}

unit MigrationSettings;

interface

uses
  Windows, Controls, Classes, Dialogs, SysUtils, SQLList, Registry, SetupConstants,
  TextMessages, GeneralFunctions, ShlObj, ActiveX, APIUtils, ShellAPI, Forms,
  ExceptionForm, VCLUnzip, Settings;

type
  TMigrationSettings = class(TSettings)
  private
    FMigrateCards: Boolean;
    FMigratedBioOccs: Integer;
    FMigratedLocations: Integer;
    FMigratedNames: Integer;
    FMigratedRecords: String;
    FMigratedReferences: Integer;
    FMigratedSurveys: Integer;
    FMigratedTaxOccs: Integer;
    FMigrateImages: Boolean;
    FMigrateMapFiles: Boolean;
    FMigrateObjectSheets: Boolean;
    FMigratePolygonFilters: Boolean;
    FMigrateRucksacks: Boolean;
    FMigrateTemplates: Boolean;
    FMigrationAccessDBPassword: String;
    FMigrationAccessDBPath: String;
    FMigrationCardsPath: String;
    FMigrationErrors: Boolean;
    FMigrationImagesPath: String;
    FMigrationMapFilesPath: String;
    FMigrationObjectSheetsPath: String;
    FMigrationPolygonFiltersPath: String;
    FMigrationRucksacksPath: String;
    FMigrationTemplatesPath: String;
    FRecorderBaseMapPath: String;
    FRecorderCardsPath: String;
    FRecorderImagesPath: String;
    FRecorderMapFilesPath: String;
    FRecorderObjectSheetsPath: String;
    FRecorderPolygonFiltersPath: String;
    FRecorderRucksacksPath: String;
    FRecorderTemplatesPath: String;
  public
    procedure LoadMigrationSettings;
    property MigrateCards: Boolean read FMigrateCards write FMigrateCards;
    property MigratedBioOccs: Integer read FMigratedBioOccs write FMigratedBioOccs;
    property MigratedLocations: Integer read FMigratedLocations write FMigratedLocations;
    property MigratedNames: Integer read FMigratedNames write FMigratedNames;
    property MigratedRecords: String read FMigratedRecords write FMigratedRecords;
    property MigratedReferences: Integer read FMigratedReferences write FMigratedReferences;
    property MigratedSurveys: Integer read FMigratedSurveys write FMigratedSurveys;
    property MigratedTaxOccs: Integer read FMigratedTaxOccs write FMigratedTaxOccs;
    property MigrateImages: Boolean read FMigrateImages write FMigrateImages;
    property MigrateMapFiles: Boolean read FMigrateMapFiles write FMigrateMapFiles;
    property MigrateObjectSheets: Boolean read FMigrateObjectSheets write FMigrateObjectSheets;
    property MigratePolygonFilters: Boolean read FMigratePolygonFilters write
        FMigratePolygonFilters;
    property MigrateRucksacks: Boolean read FMigrateRucksacks write FMigrateRucksacks;
    property MigrateTemplates: Boolean read FMigrateTemplates write FMigrateTemplates;
    property MigrationAccessDBPassword: String read FMigrationAccessDBPassword write
        FMigrationAccessDBPassword;
    property MigrationAccessDBPath: String read FMigrationAccessDBPath write
        FMigrationAccessDBPath;
    property MigrationCardsPath: String read FMigrationCardsPath write FMigrationCardsPath;
    property MigrationErrors: Boolean read FMigrationErrors write FMigrationErrors;
    property MigrationImagesPath: String read FMigrationImagesPath write FMigrationImagesPath;
    property MigrationMapFilesPath: String read FMigrationMapFilesPath write
        FMigrationMapFilesPath;
    property MigrationObjectSheetsPath: String read FMigrationObjectSheetsPath write
        FMigrationObjectSheetsPath;
    property MigrationPolygonFiltersPath: String read FMigrationPolygonFiltersPath write
        FMigrationPolygonFiltersPath;
    property MigrationRucksacksPath: String read FMigrationRucksacksPath write
        FMigrationRucksacksPath;
    property MigrationTemplatesPath: String read FMigrationTemplatesPath write
        FMigrationTemplatesPath;
    property RecorderBaseMapPath: String read FRecorderBaseMapPath;
    property RecorderCardsPath: String read FRecorderCardsPath write FRecorderCardsPath;
    property RecorderImagesPath: String read FRecorderImagesPath write FRecorderImagesPath;
    property RecorderMapFilesPath: String read FRecorderMapFilesPath write
        FRecorderMapFilesPath;
    property RecorderObjectSheetsPath: String read FRecorderObjectSheetsPath write
        FRecorderObjectSheetsPath;
    property RecorderPolygonFiltersPath: String read FRecorderPolygonFiltersPath write
        FRecorderPolygonFiltersPath;
    property RecorderRucksacksPath: String read FRecorderRucksacksPath write
        FRecorderRucksacksPath;
    property RecorderTemplatesPath: String read FRecorderTemplatesPath write
        FRecorderTemplatesPath;
  end;
  
//==============================================================================
implementation

uses
  Functions;

{-==============================================================================
    TMigrationSettings
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TMigrationSettings.LoadMigrationSettings;
  
  function CheckPath(var APath: String): Boolean;
  begin
    Result := DirectoryExists(APath);
    if Result then APath := ExcludeTrailingPathDelimiter(ExpandLongPathName(APath));
  end;
  
begin
  with TRegistry.Create do
    try
      // Recorder 6 info.
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_READ;
      if OpenKeyReadOnly(REG_KEY_R6) then begin
        // Get Recorder 6 server.
        InstallFolder := ReadString('Installation Path');
        if InstallFolder <> '' then
          InstallFolder := IncludeTrailingPathDelimiter(InstallFolder);
        ServerName := ReadString('Server Name');
        TrustedLogin := ReadBool('Trusted Security');
        CloseKey;
      end;
      RootKey := HKEY_CURRENT_USER;
      if OpenKeyReadOnly(REG_KEY_R6_SETTINGS) then begin
        FRecorderBaseMapPath := IncludeTrailingPathDelimiter(ReadString('Base Map Path'));
        if FRecorderBaseMapPath <> '' then
          FRecorderBaseMapPath := IncludeTrailingPathDelimiter(FRecorderBaseMapPath);
        FRecorderMapFilesPath :=
            ExcludeTrailingPathDelimiter(ReadString('Map File Path'));
        FRecorderObjectSheetsPath :=
            ExcludeTrailingPathDelimiter(ReadString('Object Sheet File Path'));
        FRecorderPolygonFiltersPath :=
            ExcludeTrailingPathDelimiter(ReadString('Polygon Filter Path'));
        FRecorderRucksacksPath :=
            ExcludeTrailingPathDelimiter(ReadString('Rucksack Path'));
        FRecorderCardsPath :=
            ExcludeTrailingPathDelimiter(ReadString('Recording Card Path'));
        FRecorderTemplatesPath :=
            ExcludeTrailingPathDelimiter(ReadString('Report Template Path'));
        FRecorderImagesPath :=
            ExcludeTrailingPathDelimiter(ReadString('Local Images File Path'));
        CloseKey;
      end;
  
      // Recorder 2002 settings.
      if OpenKeyReadOnly(REG_KEY_R2K2_SETTINGS) then begin
        // This one will always be done.
        FMigrationAccessDBPath :=
            ExcludeTrailingPathDelimiter(ExpandLongPathName(ExtractFilePath(ReadString('Database Path'))));
            
        if ValueExists('Database Password') then
          FMigrationAccessDBPassword := ReadString('Database Password')
        else
          FMigrationAccessDBPassword := 'password'; // default to try
        // These are optional.
        FMigrationMapFilesPath       := ReadString('Map File Path');
        FMigrationObjectSheetsPath   := ReadString('Object Sheet File Path');
        FMigrationPolygonFiltersPath := ReadString('Polygon Filter Path');
        FMigrationRucksacksPath      := ReadString('Rucksack Path');
        FMigrationCardsPath          := ReadString('Recording Card Path');
        FMigrationTemplatesPath      := ReadString('Report Template Path');
        FMigrationImagesPath         := ReadString('Local Images File Path');

        FMigrateMapFiles       := CheckPath(FMigrationMapFilesPath);
        FMigrateObjectSheets   := CheckPath(FMigrationObjectSheetsPath);
        FMigratePolygonFilters := CheckPath(FMigrationPolygonFiltersPath);
        FMigrateRucksacks      := CheckPath(FMigrationRucksacksPath);
        FMigrateCards          := CheckPath(FMigrationCardsPath);
        FMigrateTemplates      := CheckPath(FMigrationTemplatesPath);
        FMigrateImages         := CheckPath(FMigrationImagesPath);
        CloseKey;
      end
      else
        FMigrationAccessDBPassword := 'password'; // default to try
    finally
      Free;
    end;
  
  UserName := 'sa';
  Password := '';
end;  // TMigrationSettings.LoadMigrationSettings 

end.
