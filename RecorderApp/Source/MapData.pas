//==============================================================================
//  Unit:        MapData
//
//  Implements:  TdmMap
//
//  Description: Implements data access functionality for the Map screen.
//
//  Author:      Paul Thomas
//  Created:     23 June 1999
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 50 $
//    $Date: 9/01/08 16:22 $
//    $Author: Rickyshrestha $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit MapData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseData, JNCCDatasets, DataClasses, Db, ExceptionForm, Constants, ADODB;

type
  TSheetType = ( stUnknown, stVector, stRaster, stPolygon );

  EMapDataError = class (TExceptionPath)
  end;
  
  TdmMap = class (TBaseDataModule)
    qryDeleteBoundary: TJNCCQuery;
    qryLocationBoundary: TJNCCQuery;
    qryLocationName: TJNCCQuery;
    qryLocName: TJNCCQuery;
    qryMapSheets: TJNCCQuery;
    qryObjectDetails: TJNCCQuery;
    qrySampleType: TJNCCQuery;
    qryTaxonKeys: TJNCCQuery;
    spAdminAreaGetRootType: TADOStoredProc;
  public
    procedure DeleteBoundary(AStaticID: integer; const AMapSheetKey: TKeyString);
    procedure DeleteMapSheetFiles(const AFilePath, AFileName: String; ASheetType: TSheetType); 
        overload;
    procedure DeleteMapSheetFiles(const AFileName: String; ASheetType: TSheetType); overload;
    procedure DeleteSheet(const AMapSheetKey: TKeyString);
  end;
  
const
  { MapServer File Names etc }
  MAP_DS_NAME = 'R2K Map Dataset';
  MAP_DS_FILENAME = 'R2KMapDataset.gds';
  MAP_DS_DESCRIPTION = '%s Map Dataset';

  MAP_UK_BS_FILENAME = 'UKBaseMap.gsf';
  MAP_GB_BS_FILENAME = 'GBBaseMap.gsf';
  MAP_IRISH_BS_FILENAME = 'IrishBaseMap.gsf';

  MAPUSER_FILENAME = 'MapUserSheet.gsf';
  MAPUSER_SHEET_NAME = 'MapUser Sheet';
  MAPUSER_SHEET_DESCRIPTION = 'MapUser Sheet for custom drawing';

  { Set the height of the Map's status bar }
  MAP_STATUSBAR_HEIGHT = 41;
  COLOR_BACKGROUND = $00FFFFFF;//rgb(255,255,255);

  { MapServer sheets can have an internal database to hold details for objects
    on the sheets.
    For this app, we have two fields.
    - The first holds a Static ID which is an identifying number for each
      object.  The normal Object ID to be given to an object depends on the
      number of objects (i.e. delete an object and the IDs get re-ordered)
    - The second holds a Key String for an object
      If the object is a REGION, then the KeyValue holds the LOCATION_KEY
    - The third holds a Key String for an object
      For a region, this holds the Boundary_Key to which the object is linked }

  MS_FIELD_STATICID = 1;
  MS_FIELD_STATICID_NAME = 'STATIC_ID';
  { KeyValue field hold either
             the key value of the sample (for a sample)
             or the key value of the location boundary (for a region) }
  { 10 character field names are all MS will allow us! }
  MS_FIELD_LOCATION_KEY = 2;
  MS_FIELD_LOCATION_KEY_NAME = 'LOCATN_KEY';

  MS_FIELD_LOCATION_BOUNDARY_KEY = 3;
  MS_FIELD_LOCATION_BOUNDARY_KEY_NAME = 'LOC_BD_KEY';

resourcestring
  ResStr_DBWriteFail  = 'Unable to read from or write to the database';
  ResStr_ImportFail = 'Unable to import sheet';     { necessary }

  ResStr_ObjectSheet  = 'One or more essential map files cannot be accessed.'#13 +
                        'Do you want to reset the map dataset?';
  ResStr_BaseMapSheet = 'Base Map Sheet';

//==============================================================================
implementation

uses
  ApplicationSettings;

{$R *.DFM}

{-==============================================================================
    TdmMap
===============================================================================}
// DELETE MAP SHEET FILES

{ Deletes the *.gsf, *.mdx,  *.gix and *.gdb files which are created
  when a sheet is imported to a dataset

  NB : It is not necessary to pass in the full path name since all
       created files will be placed in the folder pointed to
       by the 'Map File Path' in the registry }
{-------------------------------------------------------------------------------
}
procedure TdmMap.DeleteBoundary(AStaticID: integer; const AMapSheetKey: TKeyString);
begin
  with qryDeleteBoundary do
  begin
    { Set table to delete from }
    SQL[1] := 'LOCATION_BOUNDARY';
    SQL[3] := 'and OBJECT_ID=:Object_ID';   // a specific id, not entire sheet
    { Search parameters }
    Parameters.ParamByName('Object_ID').Value := AStaticID;
    Parameters.ParamByName('MAp_Sheet_Key').Value := AMapSheetKey;
    { Delete records found }
    ExecSQL;
    { Set table to delete from for admin boundaries}
    SQL[1] := 'ADMIN_BOUNDARY';
    { Search parameters }
    Parameters.ParamByName('Object_ID').Value := AStaticID;
    Parameters.ParamByName('MAp_Sheet_Key').Value := AMapSheetKey;
    { Delete records found }
    ExecSQL;
  end;
end;  // TdmMap.DeleteBoundary 

{-------------------------------------------------------------------------------
}
procedure TdmMap.DeleteMapSheetFiles(const AFilePath, AFileName: String; ASheetType: 
    TSheetType);
var
  lExtension: String;
  lName: String;
  lNameRoot: String;
begin
  if (AFileName = '') or (AFilePath = AppSettings.BaseMapPath) then
    Exit; // safety check
  
  { VECTOR SHEET - Delete multiple files }
  if (ASheetType = stVector) or (ASheetType = stPolygon) then
  begin
    { Format the string so it is Only the name without
      either a file path or an extension }
    lNameRoot  := ExtractFileName(AFileName);
    lExtension := ExtractFileExt(lNameRoot);
  
    lName := AFilePath + Copy(lNameRoot, 1, Length(lNameRoot) - Length(lExtension));
  
    { *.gdb - vector sheet database }
    DeleteFile(lName + '.gsf');
    { *.gsf - sheet file containing map objects }
    DeleteFile(lName + '.gix');
    { *.mdx - sheet database index }
    DeleteFile(lName + '.mdx');
    { *.gix - sheet index file for querying and optimised display }
    DeleteFile(lName + '.gdb');
    { *.gbm - sheet index file for querying and optimised display }
    DeleteFile(lName + '.gbm');
  end // deleting vector sheets
  else
  if ASheetType = stRaster then
  { RASTER - Delete the file }
    DeleteFile(AFileName);
end;  // TdmMap.DeleteMapSheetFiles 

{-------------------------------------------------------------------------------
}
procedure TdmMap.DeleteMapSheetFiles(const AFileName: String; ASheetType: TSheetType);
var
  lExtension: String;
  lName: String;
  lPath: String;
begin
  if AFileName = '' then Exit; // safety check
  
  { VECTOR SHEET - Delete multiple files }
  if (ASheetType = stVector) or (ASheetType = stPolygon) then
  begin
    if ASheetType = stVector then lPath := AppSettings.MapFilePath
                             else lPath := AppSettings.ObjectSheetFilePath;
    // Format the string so it is Only the name without the nextension
    lExtension := ExtractFileExt(AFileName);
    lName      := lPath + Copy(AFileName, 1, Length(AFileName) - Length(lExtension));
  
    { *.gdb - vector sheet database }
    DeleteFile(lName + '.gsf');
    { *.gsf - sheet file containing map objects }
    DeleteFile(lName + '.gix');
    { *.mdx - sheet database index }
    DeleteFile(lName + '.mdx');
    { *.gix - sheet index file for querying and optimised display }
    DeleteFile(lName + '.gdb');
    { *.gbm - sheet index file for querying and optimised display }
    DeleteFile(lName + '.gbm');
  end else
  if ASheetType = stRaster then
    { RASTER - Delete the file }
    DeleteFile(AppSettings.MapFilePath + AFileName);
end;  // TdmMap.DeleteMapSheetFiles 

{-------------------------------------------------------------------------------
}
procedure TdmMap.DeleteSheet(const AMapSheetKey: TKeyString);
begin
  with qryDeleteBoundary do
  begin
    { Set table to delete from }
    SQL[1] := 'LOCATION_BOUNDARY';
    SQL[3] := '';   // delete entire sheet
    { Search parameters }
    Parameters.ParamByName('Map_Sheet_Key').Value := AMapSheetKey;
    { Delete records found }
    ExecSQL;
    { Set table to delete from for admin boundaries}
    SQL[1] := 'ADMIN_BOUNDARY';
    { Search parameters }
    Parameters.ParamByName('Map_Sheet_Key').Value := AMapSheetKey;
    { Delete records found }
    ExecSQL;
  end;
end;  // TdmMap.DeleteSheet 

//==============================================================================
end.

 
