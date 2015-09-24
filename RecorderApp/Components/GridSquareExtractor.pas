//==============================================================================
//  Unit:        GridSquareExtractor
//
//  Implements:  TGridSquareExtractor
//
//  Description: business class for the ability to extract grid squares from
//               a location boundary into the GRID_SQUARES table
//
//  Created:     July 2006
//
//  Last Revision Details:
//    $Revision: 9 $
//    $Date: 26/03/09 11:23 $
//    $Author: Pauldavies $
//==============================================================================
unit GridSquareExtractor;

interface

uses classes, sysutils, forms, MapServerLink, DataClasses, ExceptionForm,
     Windows, Messages, GridSquareItem;

resourcestring
  ResStr_GridSquareQualifier='original';
  ResStr_100mAreYouSure='You have selected to generate 100m grid squares.  This '+
    'may generate a large number of records and take a long time.  Are you sure '+
    'you want to continue?';
  ResStr_GridSquareExtractCancelAreYouSure='Are you sure you want to cancel the extraction of grid squares?';
  ResStr_GridSquareExtractionCancelled = 'The extraction of grid squares was cancelled.';
  ResStr_ExtractingGridSquares = 'Extracting grid squares...';
  ResStr_GridSquareExtractionComplete = 'Extracting the grid squares has been completed. '+
      '%d grid squares were extracted.';
  ResStr_NoLocationsInLayer = 'There are no polygons linked to the selected layer. No grid squares were extracted.';

type
  TSetProgressMethod = procedure(iProgress: Integer) of object;

  EGridSquareExtractorException = class(TExceptionPath);

  TGridSquareExtractor = class(TThread)
  private
    FBaseMapKey: string;
    FCurrentPrecision: Integer;
    FException: Exception;
    FRecordsInserted: Integer;
    FLinkedPolygons: TList;
    FMapServerLink: TMapServerLink;
    FMapSheetKey: string;
    FSheetID: Integer;
    FStaticID: Integer;
    FPrecisions: TList;
    FPrecisionsDone: Integer;
    FProgressGridSquaresRatio: Double;
    FProgressObject: Integer;
    FSetProgressMethod: TSetProgressMethod;
    FTotalPrecisions: Integer;
    FExceptionMessage: string;
    FGridListToAddTo: TGridDataList;
    FNewGridSquareItem: TGridSquareItem;
    FIncludePartialSquares: Boolean;
    procedure AddGridSquaresToDatabase(const ALocationKey: string; AGridSquares:
        TStringList; ASize: integer);
    procedure CheckIsLinked(AStaticID: integer);
    procedure FindLinkedPolygons;
    function IsTerminated: Boolean;
    procedure ProcessObject(AStaticID: integer);
    procedure SetBaseMapKey(const Value: string);
    procedure SetIncludePartialSquares(Value: Boolean);
    procedure SetMapSheetKey(const Value: string);
    procedure SetSetProgressMethod(const Value: TSetProgressMethod);
    procedure SetSheetID(const Value: Integer);
    procedure SetStaticID(const Value: Integer);
    procedure UpdateProgress;
    procedure HandleException;
    procedure DoHandleException;
    procedure SetGridListToAddTo(const Value: TGridDataList);
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure AddGridSquareToGrid;
    procedure AddPrecision(APrecision: integer);
    function FoundPolygonsInLayer: Boolean;
    function GetSizeLabel(ASize: integer): string;
    property BaseMapKey: string read FBaseMapKey write SetBaseMapKey;
    property SetProgressMethod: TSetProgressMethod read FSetProgressMethod write
        SetSetProgressMethod;
    property GridListToAddTo: TGridDataList read FGridListToAddTo write SetGridListToAddTo;
    property IncludePartialSquares: Boolean read FIncludePartialSquares write SetIncludePartialSquares;
    property MapSheetKey: string read FMapSheetKey write SetMapSheetKey;
    property RecordsInserted: Integer read FRecordsInserted;
    property SheetID: Integer read FSheetID write SetSheetID;
    property StaticID: Integer read FStaticID write SetStaticID;
  end;

implementation

uses
  MapPolygonScanner, DatabaseAccessADO, ApplicationSettings, SpatialRefFuncs, Variants;

resourcestring
  ResStr_IntErrMapKeyNotFound = 'Internal error - map sheet key not found for grid square extraction';

{-------------------------------------------------------------------------------
  Class construction
}
constructor TGridSquareExtractor.Create(CreateSuspended: Boolean);
begin
  inherited;
  Priority := tpNormal;
  FStaticID := -1;
  FPrecisions := TList.Create;
  FLinkedPolygons := TList.Create;
  // for progress tracking, this is the total number of metres in the grid
  // square precisions we are using
  FTotalPrecisions := 0;
  FRecordsInserted := 0;
end;

{-------------------------------------------------------------------------------
  Destroy thread objects
}
destructor TGridSquareExtractor.Destroy;
begin
  WaitFor;
  FPrecisions.Free;
  FLinkedPolygons.Free;
  FMapServerLink.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Populates the grid_squares table for a location and a list of grid squares
}
procedure TGridSquareExtractor.AddGridSquaresToDatabase(const ALocationKey:
    string; AGridSquares: TStringList; ASize: integer);
var
  i: integer;
  lLatLong: TLatLong;
begin
  for i := 0 to AGridSquares.Count-1 do begin
    FProgressGridSquaresRatio:=i/AGridSquares.Count;
    Synchronize(UpdateProgress);
    if Terminated then raise EAbort.Create('');
    if Assigned(FGridListToAddTo) then begin

      FNewGridSquareItem := TGridSquareItem.CreateNew(FGridListToAddTo);
      with FNewGridSquareItem do begin
        LocationKey := ALocationKey;
        SpatialRefSize := ASize;
        EnteredRef := AGridSquares[i];
        SpatialRef := AGridSquares[i];
      end;
      Synchronize(AddGridSquareToGrid);
    end else
    begin
      lLatLong := ConvertToLatLong(AGridSquares[i], AppSettings.SpatialRefSystem);

      // The stored procedure will return Null if no new grid square is added (e.g.
      // a duplicate).
      if dmDatabase.RunInsertStoredProc('Grid_Square', 'usp_GridSquare_Insert',
          ['@Spatial_Ref', AGridSquares[i],
           '@Location_Key', ALocationKey,
           '@Spatial_Ref_System', AppSettings.SpatialRefSystem,
           '@Spatial_Ref_Qualifier', ResStr_GridSquareQualifier,
           '@Size', ASize,
           '@Lat', lLatLong.Lat,
           '@Long', lLatLong.Long,
           '@Entered_By', AppSettings.UserID], '@Key') <> Null then  
        Inc(FRecordsInserted);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Synchronised method to add a grid square to the grid, since this affects the
    VCL.
}
procedure TGridSquareExtractor.AddGridSquareToGrid;
var
  lFound: boolean;
  i: integer;
begin
  lFound := false;
  for i := 0 to FGridListToAddTo.ItemCount-1 do
    if TGridSquareItem(FGridListToAddTo.GetDataItem(i)).
        EnteredRef = FNewGridSquareItem.EnteredRef then begin
      lFound := true;
      break; // from loop
    end;
  if not lFound then begin
    with FNewGridSquareItem do begin
      UserID := AppSettings.UserID;
      EnteredRefSystem := AppSettings.SpatialRefSystem;
      SpatialRefSystem := AppSettings.SpatialRefSystem;
    end;
    FGridListToAddTo.AddNew(FNewGridSquareItem);
  end;
end;

{-------------------------------------------------------------------------------
  Store the fact that grid square generation of a particular precision is
     requested.
}
procedure TGridSquareExtractor.AddPrecision(APrecision: integer);
begin
  FPrecisions.Add(Ptr(APrecision));
  // an indication of the time required to process this precision, for progress
  FTotalPrecisions := FTotalPrecisions + 10000 div APrecision;
end;

{-------------------------------------------------------------------------------
  If an object is linked to a location, then add it to the list to process
}
procedure TGridSquareExtractor.CheckIsLinked(AStaticID: integer);
begin
  with dmDatabase.GetRecordset('usp_Boundary_GetLinkedLocation',
      ['@Map_Sheet_Key', FMapServerLink.SheetMapSheetKey(FSheetID),
       '@Static_Object_ID', AStaticId]) do
    if not (EOF or BOF) then
      FLinkedPolygons.Add(Ptr(AStaticID));
end;

{-------------------------------------------------------------------------------
  Perform the extraction operation
}
procedure TGridSquareExtractor.Execute;
var
  i: integer;
begin
  FExceptionMessage := '';
  try
    FProgressObject := 0;
    FindLinkedPolygons;
    for i := 0 to FLinkedPolygons.Count-1 do begin
      FProgressObject := i;
      ProcessObject(Integer(FLinkedPolygons[i]));
    end;
  except
    on Exception do
      HandleException
  end;
end;

{-------------------------------------------------------------------------------
  Synchronized method to display the exception on the main thread
}
procedure TGridSquareExtractor.DoHandleException;
begin
  // Cancel the mouse capture
  if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  // Now actually show the exception
  if FException is Exception then
    Application.ShowException(FException)
  else
    SysUtils.ShowException(FException, nil);
  Terminate;
end;

{-------------------------------------------------------------------------------
  Thread exception handler, synchronizes thread display to main thread
}
procedure TGridSquareExtractor.HandleException;
begin
  // This function is virtual so you can override it
  // and add your own functionality.
  FException := Exception(ExceptObject);
  try
    // Don't show EAbort messages
    if not (FException is EAbort) then
      Synchronize(DoHandleException);
  finally
    FException := nil;
  end;
end;

{-------------------------------------------------------------------------------
  Build a list of only the linked polygons that require processing
}
procedure TGridSquareExtractor.FindLinkedPolygons;
var
  i: integer;
begin
  if FStaticID<>-1 then
    CheckIsLinked(FStaticID)
  else
    for i := 0 to FMapServerLink.ObjectTotal[FSheetID] - 1 do
      CheckIsLinked(FMapServerLink.StaticIDforObject(FSheetID, i));
end;

{-------------------------------------------------------------------------------
  Returns true if some linked polygons have been found to scan. Used to adjust
    the completion message appropriately.
}
function TGridSquareExtractor.FoundPolygonsInLayer: Boolean;
begin
  Result := FLinkedPolygons.Count>0;
end;

{-------------------------------------------------------------------------------
  converts a grid square size in metres to its display caption
}
function TGridSquareExtractor.GetSizeLabel(ASize: integer): string;
begin
  case ASize of
    10000, 2000, 1000:
      Result := IntToStr(ASize div 1000 ) + 'km';
    100: Result := '100m';
  else
    Result := IntToStr(ASize);
  end;
end;

{-------------------------------------------------------------------------------
  Callback allowing the called classed (such as the polygon scanner) to detect
       thread termination
}
function TGridSquareExtractor.IsTerminated: Boolean;
begin
  Result := Terminated;
end;

{-------------------------------------------------------------------------------
  Extraction for a single map boundary
}
procedure TGridSquareExtractor.ProcessObject(AStaticID: integer);
var
  lScanner: TPolygonScanner;
  lGridSqList: TStringList;
  i: integer;
begin
  lGridSqList := TStringList.Create;
  try
    lScanner := TPolygonScanner.Create(FMapServerLink.MapHandle, AppSettings.SpatialRefSystem);
    with dmDatabase.GetRecordset('usp_Boundary_GetLinkedLocation',
        ['@Map_Sheet_Key', FMapServerLink.SheetMapSheetKey(FSheetID),
         '@Static_Object_ID', AStaticId]) do
      // test for EOF confirms there is a linked location
      if not Eof then begin
        FPrecisionsDone := 0;
        for i := 0 to FPrecisions.Count-1 do begin
          FCurrentPrecision := 10000 div Integer(FPrecisions[i]);
          lScanner.GetGridSquaresForPolygon(FSheetID, AStaticID, AppSettings.SpatialRefSystem,
              Integer(FPrecisions[i]), lGridSqList, IsTerminated, IncludePartialSquares);
          FProgressGridSquaresRatio := 0;
          Synchronize(UpdateProgress);
          AddGridSquaresToDatabase(Fields['Location_Key'].Value, lGridSqList, Integer(FPrecisions[i]));
          Synchronize(UpdateProgress);
          Inc(FPrecisionsDone, 10000 div Integer(FPrecisions[i]));
        end;
      end;
  finally
    lGridSqList.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Accessor - activates the base map dataset
}
procedure TGridSquareExtractor.SetBaseMapKey(const Value: string);
begin
  FBaseMapKey := Value;
  FMapServerLink := TMapServerLink.Create(nil);
  FMapServerLink.ActiveDataset := FBaseMapKey + '.gds';
end;

{-------------------------------------------------------------------------------
}
procedure TGridSquareExtractor.SetIncludePartialSquares(Value: Boolean);
begin
  FIncludePartialSquares := Value;
end;

{-------------------------------------------------------------------------------
  Accessor. If this property is set, then rather than adding to the database,
    TGridDataItem entries are added to this list.
}
procedure TGridSquareExtractor.SetGridListToAddTo(const Value: TGridDataList);
begin
  FGridListToAddTo := Value;
end;

{-------------------------------------------------------------------------------
  Setting the map sheet key is an alternative to setting the sheet id and
     base map key.  These properties are automatically read from the db.
}
procedure TGridSquareExtractor.SetMapSheetKey(const Value: string);
begin
  FMapSheetKey := Value;
  with dmDatabase.ExecuteSQL(Format('SELECT Base_Map_Key, Dataset_Sheet_Filename '+
      'FROM Map_Sheet WHERE Map_Sheet_Key=''%s''',[Value]), true) do
    if not (EOF or BOF) then begin
      BaseMapKey := Fields['Base_Map_Key'].Value;
      FSheetID := FMapServerLink.SheetIDByFileName(
          AppSettings.ObjectSheetFilePath+Fields['Dataset_Sheet_Filename'].Value);
    end
    else
      raise EGridSquareExtractorException.Create(ResStr_IntErrMapKeyNotFound);
end;

{-------------------------------------------------------------------------------
  Accessor - callback to allow the main form progress bar to update
}
procedure TGridSquareExtractor.SetSetProgressMethod(const Value:
    TSetProgressMethod);
begin
  FSetProgressMethod := Value;
end;

{-------------------------------------------------------------------------------
  Sheet ID of the single object or entire layer to scan
}
procedure TGridSquareExtractor.SetSheetID(const Value: Integer);
begin
  FSheetID := Value;
end;

{-------------------------------------------------------------------------------
  Static Object ID of the object to scan (leave default -1 if scanning a layer)
}
procedure TGridSquareExtractor.SetStaticID(const Value: Integer);
begin
  FStaticID := Value;
end;

{-------------------------------------------------------------------------------
  Update the main form progress bar.  Calls to this method should be wrapped
    in Synchronize
}
procedure TGridSquareExtractor.UpdateProgress;
begin
  if Assigned(SetProgressMethod) then
    SetProgressMethod(Trunc(100 * (FProgressObject +
        (FPrecisionsDone + FProgressGridSquaresRatio*FCurrentPrecision) / FTotalPrecisions)
        / FLinkedPolygons.Count));
end;

end.
