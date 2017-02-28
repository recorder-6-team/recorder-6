{===============================================================================
  Unit:        MapServerLink

  Implements:  TMapServerLink

  Description:

  Model:       Map.mpb

  Created:     8 November 2001

  Last Revision Details:
    $Revision: 32 $
    $Date: 4/01/08 11:38 $
    $Author: Rickyshrestha $

===============================================================================}

unit MapServerLink;

interface

uses
  Windows, Sysutils, Classes, Controls, Forms, ExtCtrls, ExceptionForm, MapData,
  Db, DataClasses, JnccDatasets;

resourcestring
  ResStr_MapDatabaseWR = 'There has been an error reading or writing details ' +
                         'to the map''s internal database.  if you continue to get this message it may  ' +
                         'be necessary to reset your map.';

  ResStr_UnableToOpen = 'Unable to open the map dataset';
  ResStr_SelSheetNotVector = 'Selected sheet is not a vector sheet : %s';
  ResStr_CannotLocateMapSheet = 'Could not locate map sheet record for %s';
  ResStr_MapServNotReturnSheet = 'Map server cannot return the sheet name';
  ResStr_FileNotFound = 'File not found : ';
  ResStr_UnrecognisedFilePath = 'Unrecognised File Path';
  ResStr_CannotLocateMapFilePath = 'The map dataset file cannot be located in the Map File Path';
  ResStr_EssSheetCannotBeFound = 'An essential sheet cannot be found in the current map dataset.';
  ResStr_CannotImportFileType = 'This file type cannot be imported';
  ResStr_ImportCancelled = 'Import Cancelled';
  ResStr_ErrorWithObjectSheet = 'There is an error with your Object Sheet .gdb file';
  ResStr_ErrorCode = ' Error Code %s : %s';


type
  EMapServerLink = class (TExceptionPath)
  end;
  
  TMapServerLink = class (TObject)
  private
    FActiveDataset: String;
    FContainer: TWinControl;
    FInternalContainer: Boolean;
    FMapHandle: HWnd;
    function GetObjectTotal(SheetID: Integer ): Integer;
    function GetSheetTotal: Integer;
    procedure SetActiveDataset(const Value: String);
  public
    constructor Create(AContainer: TWinControl); reintroduce; overload;
    destructor Destroy; override;
    function ConvertToMSPattern(APattern: integer): ShortInt;
    function FileNameByMapSheetKey(const AMapSheetKey: String): String;
    procedure LinkObjectToBoundary(const AMapSheetKey: TKeyString; AStaticID: Integer; const
        ALocationKey, ABoundaryKey: TKeyString);
    procedure ReActivateDataset;
    procedure SaveChanges(ABaseMapKey: TKeyString);
    function SheetFileName(ASheetIndex: integer): String;
    function SheetIDByFileName(const AFileName: string): Integer;
    function SheetMapSheetKey(ASheetIndex: integer): String;
    function SheetName(ASheetIndex: Integer): String;
    function StaticIDforObject(ASheetIndex, AObjectID: integer): Integer;
    procedure WriteToMapDatabase(const AFieldNumber, ASheetID, AObjectID: Integer; const
        AData: String);
    property ActiveDataset: String read FActiveDataset write SetActiveDataset;
    property MapHandle: HWnd read FMapHandle;
    property ObjectTotal[SheetID: Integer ]: Integer read GetObjectTotal;
    property SheetTotal: Integer read GetSheetTotal;
  end;


function ConvertPatternToMSPattern(APattern: Integer): ShortInt;
function DatasetSheetFilename(ASheetType: Integer; const AFileName: String): String;
function GetErrorString(AReturnValue: Integer): String;
procedure MapCheck(ARetValue: Integer; const ADetails: String = '');
function MapFileName(ADataset: TDataset): String;

//==============================================================================
implementation

uses
  FormActions, Map, ApplicationSettings, MS5, MainTBar, GeneralData, DatabaseAccessADO,
  GeneralFunctions;

{-==============================================================================
    TMapServerLink
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TMapServerLink.Create(AContainer: TWinControl);
begin
  inherited Create;
  
  FMapHandle := 0;
  if Assigned(AContainer) then begin
    FInternalContainer := False;
    FContainer         := AContainer;
  end else begin
    FInternalContainer := True;
    FContainer         := TPanel.Create(nil);
    FContainer.Parent  := Application.MainForm;
    FContainer.Visible := False;
  end;
  
  // Link map server to container.
  with FContainer do
    FMapHandle := MapCreateNewWindow(Handle, 0, 0, Width, Height);
end;  // TMapServerLink.Create 

{-------------------------------------------------------------------------------
}
destructor TMapServerLink.Destroy;
begin
  if FMapHandle <> 0 then begin
    MapCloseData(FMapHandle);
    MapCheck(MapDestroyWindow(FMapHandle));
    FMapHandle := 0;
  end;
  
  if FInternalContainer then FreeAndNil(FContainer);
  
  inherited;
end;  // TMapServerLink.Destroy

{-------------------------------------------------------------------------------
  Takes an ordinal equivalent to TBrushStyle, and returns the equivalent
  MS Pattern index  
}
function TMapServerLink.ConvertToMSPattern(APattern: integer): ShortInt;
begin
  case APattern of
    0: Result := 0;
    1: Result := -1;
  else
    Result := APattern - 1;
  end; // case
end;  // TMapServerLink.ConvertToMSPattern 

{-------------------------------------------------------------------------------
  Retrieve the file name for a sheet where we know the map sheet key
}
function TMapServerLink.FileNameByMapSheetKey(const AMapSheetKey: String): String;
begin
  with dmDatabase.ExecuteSQL('SELECT Sheet_Type, Dataset_Sheet_FileName FROM Map_Sheet ' +
                             'WHERE Map_Sheet_Key = ''' + AMapSheetKey + '''', True) do
  begin
    Result := DatasetSheetFileName(Fields['Sheet_Type'].Value,
                                   Fields['Dataset_Sheet_FileName'].Value);
    Close;
  end;
end;  // TMapServerLink.FileNameByMapSheetKey 

{-------------------------------------------------------------------------------
  Returns the number of polygons on a sheet  
}
function TMapServerLink.GetObjectTotal(SheetID: Integer ): Integer;
var
  lObjects: Integer;
  lType: Integer;
  lName: String;
begin
  MapCheck(MapGetSheetType(MapHandle, SheetID, lType));
  if (lType = MS_VECTOR_SHEET_TYPE) then
    MapCheck(MapGetNumObjects(MapHandle, SheetID, lObjects))
  else begin
    lName := SheetFileName(SheetID);
    raise EMapBrowserError.Create(Format(ResStr_SelSheetNotVector, [lName]));
  end;
  Result := lObjects;
end;  // TMapServerLink.GetObjectTotal

{-------------------------------------------------------------------------------
  Read the total count of sheets from the map  
}
function TMapServerLink.GetSheetTotal: Integer;
begin
  if ActiveDataset = '' then
    Result := 0
  else
    MapGetNumSheets(MapHandle, Result);
end;  // TMapServerLink.GetSheetTotal 

{-------------------------------------------------------------------------------
  Method called by locations form once we successfully set up a linked boundary.
  Writes the boundary key into the map dataset for the object with the given
  static id  
}
procedure TMapServerLink.LinkObjectToBoundary(const AMapSheetKey: TKeyString; AStaticID: 
    Integer; const ALocationKey, ABoundaryKey: TKeyString);
var
  lObjectID: Integer;
  lFileName: String;
  lSheetID: Integer;
begin
  if AStaticID <> -1 then
  begin
    lFileName := FileNameByMapSheetKey(AMapSheetKey);
    lSheetId  := SheetIDbyFileName(lFileName);
    // If object is found, update, otherwise, ignore.
    if MapGetObjectFromStaticID( MapHandle, lSheetID, AStaticID, lObjectID) = MS_SUCCESS then
    begin;
      WriteToMapDatabase(MS_FIELD_LOCATION_KEY, lSheetID, lObjectID, ALocationKey);
      WriteToMapDatabase(MS_FIELD_LOCATION_BOUNDARY_KEY, lSheetID, lObjectID, ABoundaryKey);
    end;
  end;
end;  // TMapServerLink.LinkObjectToBoundary

{-------------------------------------------------------------------------------
}
procedure TMapServerLink.ReActivateDataset;
begin
  MapCloseData(FMapHandle);
  MapCheck(MapSelectDataset(FMapHandle, PChar(FActiveDataset)), ResStr_UnableToOpen);
end;  // TMapServerLink.ReActivateDataset

{-------------------------------------------------------------------------------
}
procedure TMapServerLink.SaveChanges(ABaseMapKey: TKeyString);
begin
  // Force the changes to be saved to the map dataset file, or other users won't see
  // them until the original is closed first.
  dmDatabase.ExecuteSQL(
      'UPDATE Map_Sheet SET Modified_Data = 1 WHERE Computer_ID <> Host_Name() ' +
      'AND Base_Map_Key = ''' + ABaseMapKey + '''');
  // And also make sure the file is updated.
  ReActivateDataset;
end;  // TMapServerLink.SaveChanges

{-------------------------------------------------------------------------------
}
procedure TMapServerLink.SetActiveDataset(const Value: String);
begin
  if Appsettings.MapFilePath + Value <> FActiveDataset then
    if FileExists(Appsettings.MapFilePath + Value) then begin
      FActiveDataset := AppSettings.MapFilePath + Value;
      // File exists, link MapHandle and file.
      MapCheck(MapSelectDataset(FMapHandle, PChar(FActiveDataset)), ResStr_UnableToOpen);
    end;
end;  // TMapServerLink.SetActiveDataset

{-------------------------------------------------------------------------------
  Get the filename for a sheet with the supplied index, from map server
}
function TMapServerLink.SheetFileName(ASheetIndex: integer): String;
var
  pFileName: array[0..255] of Char;
begin
  MapCheck(MapGetSheetFileName(MapHandle, ASheetIndex, pFileName, 255));
  Result := ExpandLongPathName(Trim(StrPas(pFileName)));
end;  // TMapServerLink.SheetFileName 

{-------------------------------------------------------------------------------
  Iterates through the sheets to find the ID of the one with the supplied
  filename  
}
function TMapServerLink.SheetIDByFileName(const AFileName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  if AFileName <> '' then
    for i := SheetTotal - 1 downto 0 do
      if CompareText(SheetFileName(i), ExpandLongPathName(AFileName)) = 0 then begin
        Result := i;
        Break;
      end;
end;  // TMapServerLink.SheetIDByFileName 

{-------------------------------------------------------------------------------
  Locate the map sheet key which corresponds to a sheet index  
}
function TMapServerLink.SheetMapSheetKey(ASheetIndex: integer): String;
var
  lFileName: String;
begin
  lFileName := SheetFileName(ASheetIndex);
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := 'SELECT Map_Sheet_Key ' +
                'FROM Map_Sheet ' +
                'WHERE (Dataset_Sheet_FileName =''' + lFileName + ''') ' +
                'OR (Dataset_Sheet_FileName = ''' + ExtractFileName(lFileName) + ''')';
    Open;
    try
      if RecordCount > 0 then
        Result := FieldByName('Map_Sheet_Key').AsString
      else
        raise EMapServerLink.Create(Format(ResStr_CannotLocateMapSheet, [lFileName]));
    finally
      Close;
    end;
  end; // with
end;  // TMapServerLink.SheetMapSheetKey 

{-------------------------------------------------------------------------------
  Get the map server name for a sheet according to its sheet index  
}
function TMapServerLink.SheetName(ASheetIndex: Integer): String;
var
  lszName: Array[0..255] of Char;
begin
  MapCheck(MapGetSheetName(MapHandle, ASheetIndex, lszName), ResStr_MapServNotReturnSheet);
  Result := Trim(StrPas(lszName));
end;  // TMapServerLink.SheetName 

{-------------------------------------------------------------------------------
  Object IDs are not static, they are always indexed in sequence from zero.
  Rach object has a static ID which is guaranteed to remain the same -
  this is useful for linking to the database  
}
function TMapServerLink.StaticIDforObject(ASheetIndex, AObjectID: integer): Integer;
begin
  MapCheck(MapGetObjectStaticID(MapHandle, ASheetIndex, AObjectID, Result));
end;  // TMapServerLink.StaticIDforObject 

{-------------------------------------------------------------------------------
  A wrapper for the MapServer.dll call and error handling so that a sheet's internal database 
      can be written to with ease.
  
  Notes : Takes in the Normal ID of the object, as would be returned by the map.  All IDs 
      stored in the Main database Location_Boundary table are Static ID numbers which can be 
      obtained by querying the MS_FIELD_STATICID field in the map database or by using the 
      ObjectIDToStaticID function.
}
procedure TMapServerLink.WriteToMapDatabase(const AFieldNumber, ASheetID, AObjectID: Integer; 
    const AData: String);
var
  lRet: Integer;
begin
  if (ObjectTotal[ASheetID] > 0) and (AObjectID > -1) then
  begin
    lRet := MapDataSetAttribute(MapHandle, ASheetID, AFieldNumber,
                                AObjectID, PChar(AData));
    if lRet <> MS_SUCCESS then
      Raise EMapBrowserError.CreateNonCritical(ResStr_MapDatabaseWR);
  end;
end;  // TMapServerLink.WriteToMapDatabase 

{-------------------------------------------------------------------------------
  Adds a path to the given filename depending on the type of sheet.
}
function DatasetSheetFilename(ASheetType: Integer; const AFileName: String): String;
begin
  case ASheetType of
    // Base maps.
    0   : Result := AppSettings.BaseMapPath + AFileName;
    // Non-drawing layers. They go in <MapFilePath>, same as Map Dataset, so no path needed.
    1, 2: Result := AFileName;
    // Polygon layers.
    3   : Result := AppSettings.ObjectSheetFilePath + AFileName;
   end;
end;

{-------------------------------------------------------------------------------
}
function MapFileName(ADataset: TDataset): String;
begin
  case ADataset.FieldByName('Sheet_Type').AsInteger of
    // Base maps.
    0: Result := AppSettings.BaseMapPath + ADataSet.FieldByName('File_Name').AsString;
    // Polygon layers.
    3: Result := AppSettings.ObjectSheetFilePath + ADataSet.FieldByName('File_Name').AsString;
  else
    // All others. Need to keep path to actual source file in this case.
    Result := ADataSet.FieldByName('File_Name').AsString;
  end;
end;

{-------------------------------------------------------------------------------
  Checks the result of a call to a MapServer function. 
}
procedure MapCheck(ARetValue: Integer; const ADetails: String = '');
var
  lErrorMsg: String;
begin
  if ARetValue <> MS_SUCCESS then begin
    lErrorMsg := GetErrorString(ARetValue);
    // If it's a 'No Error' message, don't display anything
    if Pos('NO ERROR', UpperCase(lErrorMsg)) = 0 then
      Raise EMapBrowserError.CreateNonCritical(lErrorMsg + ' - ' + ADetails);
  end;
end;  // MapCheck

{-------------------------------------------------------------------------------
  Returns the appropriate string message for the given value.
}
function GetErrorString(AReturnValue: Integer): String;
var
  pMessage: Array[0..255] of Char;
begin
  case AReturnValue of
    //  -609, -110, -90, -23, -20 : Exit;
    -1       : Exit;
    -2       : Result := ResStr_FileNotFound;
    -3       : Result := ResStr_UnrecognisedFilePath;
    -21      : Result := ResStr_CannotLocateMapFilePath;
    -22      : Result := ResStr_EssSheetCannotBeFound;
    -40,
    -63..-59,
    -56..-41 : Result := ResStr_ImportFail;
    -57      : Result := ResStr_CannotImportFileType;
    -58      : Result := ResStr_ImportCancelled;
    -82..-71 : Result := ResStr_ErrorWithObjectSheet;
    -601     : Abort;  // No Drag Rectangle error. Just abort what caused it.
  else // case
    begin
      MapGetErrorMsg(AReturnValue, pMessage, 255);
      Result := Format(ResStr_ErrorCode, [IntToStr(AReturnValue), Trim(StrPas(pMessage))]);
    end;
  end; // case
end;

{-------------------------------------------------------------------------------
  Convert a Windows pattern to a MapServer pattern.
}
function ConvertPatternToMSPattern(APattern: Integer): ShortInt;
begin
  case APattern of
    0: Result := 0;
    1: Result := -1;
  else
    Result := APattern - 1;
  end; // case
end;

end.
