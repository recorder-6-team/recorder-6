{===============================================================================
  Unit:        MapPolygonScanner

  Defines:     TPolygonScanner

  Description: Unit that provides methods to determine the list of samples that
               are associated with a polygon on the map

  Model:       <none>

  Created:

  Last revision information:
    $Revision: 24 $
    $Date: 19/05/08 12:06 $
    $Author: Johnvanbreda $

===============================================================================}

{ Nov 2002 - Michael Bailey

  This unit has been significantly altered to fascilitate the actSubtractBoundary
  action in the Map Unit.  See MapPolygonInteraction for more details.

  Importantly every use of the word vertice has been changed to edge.  The types
  TVertice and PVertice have been changed to TEdge and PEdge.  This is because the
  derived classes make heavy use of the concept of a corner of the polygon, a
  vertex, the plural of which is of course vertices.  The change in name is to
  avoid the otherwise inevitable confusion.
}
unit MapPolygonScanner;

interface

uses
  Classes, SysUtils, MS5, Windows, SpatialRefFuncs, DataClasses, MapServerLink,
  ADODb, forms, ExceptionForm;

resourcestring
  ResStr_PolygonMissing = 'A polygon is not longer available as it has been deleted.';

type
  EMapPolygonScanner = class (TExceptionPath);

  { Record to hold a single edge of a polygon }
  { MDB : Formerly called TVertice.  See above. }
  TEdge = record
    SPoint: MSCoord;
    NPoint: MSCoord;
  end;
  PEdge = ^TEdge;

  TPolygonScanner = class
  private
    FMapHandle: HWnd;
    FMapSystem: String;
    procedure GetPolygonBoundingBox(const iSheetID, iPolygonID, iSouthWestBuffer: Integer;
        out oSWLatLong, oNELatLong: TLatLong);
    function IsInPolygon(polygonRgn, spatialRgn: HRGN; wantPartialHit: Boolean): Boolean;
    function CreateRgnFromPolygon(ASheetID, APolygonID: Integer): HRGN;
    function CreateRgnFromSpatialRef(const spatialRef, system: String; lat, long: Double;
        precision: Integer): HRGN; overload;
    function CreateRgnFromSpatialRef(const spatialRef, system: String; lat, long: Double): HRGN; overload;
    procedure GetKeysForPolygon(ASheetID, AStaticID: Integer; AConnection: TADOConnection;
      AIncludePartialSquares: Boolean; const ASourceField, ASourceTable: string);
  public
    constructor Create(AMapHandle: HWnd; const AMapSystem: String);
    procedure GetSamplesForPolygon(ASheetID, AStaticID: Integer; AConnection: TADOConnection;
      AIncludePartialSquares: Boolean);
    procedure GetLocationsForPolygon(ASheetID, AStaticID: Integer; AConnection: TADOConnection;
      AIncludePartialSquares: Boolean);
    procedure GetGridSquaresForPolygon(ASheetID, AStaticID: Integer; const
        ASpatialRefSystem: string; APrecision: integer; AGridSquares: TStringList;
        AThreadTerminationCheck: TThreadTerminationCheck=nil;
        AIncludePartialSquares: Boolean = True);
  end;

  { MDB : New procedures have been added to TMapPolygon.  While they are not needed
    for TMapPolygon for its original purpose, they are needed for its new descendants.}
  TMapPolygon = class
  private
    FEdges: TList;
    FLatLongEdges: TList;
    FObjectID: Integer;
    FStaticID: Integer;
    FSheetID: Integer;
    FMapHandle: HWnd;
    FMapSystem: String;
    procedure ConstructEdges;
  protected
    procedure Construct; virtual;
    procedure ReConstruct; virtual;
    procedure AddEdge(iCoord1, iCoord2: MSCoord); virtual;
    procedure Clean; virtual;
  public
    constructor Create(iMapHandle: HWnd; iSheetID, iStaticID: Integer; iMapSystem: String);
    constructor CreateFromObjectID(iMapHandle: HWnd; iSheetID, iObjectID: Integer; iMapSystem: String);
    destructor Destroy; override;
    function IsHit(iTestCoord: MSCoord): boolean;
    property MapHandle: HWnd read FMapHandle;
    property MapSystem: String read FMapSystem write FMapSystem;
    property Edges: TList read FEdges;
    property LatLongEdges: TList read FLatLongEdges;
    property ObjectID: Integer read FObjectID;
    property StaticID: Integer read FStaticID;
    property SheetID: Integer read FSheetID;
  end;

//==============================================================================
implementation

uses
  GeneralData, DatabaseAccessADO, SQLConstants, ComObj, APIUtils,
  GeneralFunctions;

//==============================================================================
constructor TPolygonScanner.Create(AMapHandle: HWnd; const AMapSystem: String);
begin
  inherited Create;
  FMapHandle := AMapHandle;
  FMapSystem := AMapSystem;
end;

{-------------------------------------------------------------------------------
  Creates a Windows API region from a map polygon, to simplify hit testing
}
function TPolygonScanner.CreateRgnFromPolygon(ASheetID, APolygonID: Integer): HRGN;
var
  lPoints: array of TPoint;
  lCoord: MSCoord;
  lNumCoords: integer;
  i: integer;
begin
  { How many coords define the polygon? }
  MapCheck(MapEditGetNumCoords(FMapHandle, lNumCoords));
  SetLength(lPoints, lNumCoords);
  { Read them out, put into a point array }
  for i := 0 to lNumCoords - 1 do begin
    MapCheck(MapEditGetObjectCoord(FMapHandle, i, lCoord));
    lPoints[i] := Point(Trunc(lCoord.x), Trunc(lCoord.y))
  end;
  Result := CreatePolygonRgn(lPoints[0], Length(lPoints), ALTERNATE);
  gpcAPICheck(Result<>0);
end;

{-------------------------------------------------------------------------------
  Creates a Windows API region from a grid square, to simplify hit testing
}
function TPolygonScanner.CreateRgnFromSpatialRef(const spatialRef, system: String; lat, long: Double;
  precision: Integer): HRGN;
var
  eastNorth: TMapCoord;
begin
  try
    Result := GridSquareToRegion(SpatialRefToGridSquare(spatialRef, system, precision), FMapSystem);
  except
    // Grid squares not supported. Use LatLong directly to get Easting/Northing
    on ESpatialRefError do begin
      eastNorth := LatLongToSpecificEN(LatLong(lat, long), FMapSystem);
      Result := CreateRectRgn(
          Trunc(eastNorth.x),
          Trunc(eastNorth.y + precision),
          Trunc(eastNorth.x + precision),
          Trunc(eastNorth.y));
    end;
  end;
end;

function TPolygonScanner.CreateRgnFromSpatialRef(const spatialRef, system: String; lat, long: Double): HRGN;
begin
  Result := CreateRgnFromSpatialRef(spatialRef, system, lat, long, -1);
end;

{-------------------------------------------------------------------------------
  Works out if a given spatial region is in, out or partially in a given polygon.
}
function TPolygonScanner.IsInPolygon(polygonRgn, spatialRgn: HRGN; wantPartialHit: Boolean): Boolean;
var
  bufferRgn: HRGN;
  isIn, isOut: Boolean;
begin
  bufferRgn := CreateRectRgn(0, 0, 0, 0);
  try
    isIn :=
        CombineRgn(bufferRgn, spatialRgn, polygonRgn, RGN_AND) in [SIMPLEREGION, COMPLEXREGION];
    isOut :=
        CombineRgn(bufferRgn, spatialRgn, polygonRgn, RGN_DIFF) in [SIMPLEREGION, COMPLEXREGION];

    Result := isIn and (not isOut or wantPartialHit);
  finally
    DeleteObject(bufferRgn);
  end;
end;

//==============================================================================
{ Creates a temp table containing samples that are contained in the identified
    polygon.  This is achieved first by getting a list of samples in the
    polygon's bounding box, then checking each one in turn against the polygon.
    Note that this could be optimised by writing code to check each LatLong
    against the single polygon we are interested in, because Map Server returns
    any polygon not just the identified one. (i.e. it does multiple hit tests) }
procedure TPolygonScanner.GetSamplesForPolygon(ASheetID, AStaticID: Integer;
  AConnection: TADOConnection; AIncludePartialSquares: Boolean);
begin
  if ASheetID <> -1 then
    GetKeysForPolygon(ASheetID, AStaticID, AConnection, AIncludePartialSquares,
        'Sample_Key', 'Sample')
  else
    raise EMapPolygonScanner.Create(ResStr_PolygonMissing);
end;

//==============================================================================
{ Creates a temp table containing locations that are contained in the identified
    polygon.  This is achieved first by getting a list of locations in the
    polygon's bounding box, then checking each one in turn against the polygon.
    Note that this could be optimised by writing code to check each LatLong
    against the single polygon we are interested in, because Map Server returns
    any polygon not just the identified one. (i.e. it does multiple hit tests) }
procedure TPolygonScanner.GetLocationsForPolygon(ASheetID,
  AStaticID: Integer; AConnection: TADOConnection;
  AIncludePartialSquares: Boolean);
begin
  if ASheetID <> -1 then
    GetKeysForPolygon(ASheetID, AStaticID, AConnection, AIncludePartialSquares,
        'Location_Key', 'Location')
  else
    raise EMapPolygonScanner.Create(ResStr_PolygonMissing);
end;

//==============================================================================
{ Creates a temp table containing the keys from the specified table that are
    contained in the identified polygon.  This is achieved first by getting
    a list of locations in the polygon's bounding box, then checking each one
    in turn against the polygon.
    Note that this could be optimised by writing code to check each LatLong
    against the single polygon we are interested in, because Map Server returns
    any polygon not just the identified one. (i.e. it does multiple hit tests) }
procedure TPolygonScanner.GetKeysForPolygon(ASheetID, AStaticID: Integer;
  AConnection: TADOConnection; AIncludePartialSquares: Boolean;
  const ASourceField, ASourceTable: string);
var
  lSWLatLong, lNELatLong : TLatLong; // bounding box on which to search
  lPolygon : TMapPolygon;
  lFormatSettings: TFormatSettings;
  lPolygonRgn, lSpatialRgn: HRGN;
  lSouthWestBuffer: Integer;

    procedure InsertKey(const AKey: string);
    begin
      // rather than use an exception, we could use SQL_TEMPLIST_INSERT_DISTINCT
      // but this would incur a performance hit for something that only happens
      // when 2 polygons overlap.
      try
        AConnection.Execute(Format(SQL_TEMPLIST_INSERT, [AKey])); // got a hit
      except on E:EOleException do
        if E.Errorcode <> -2147217873 then
          raise; // throw error if not just a duplicate key
      end;
    end;

begin
  lPolygon := TMapPolygon.Create(FMapHandle, ASheetID, AStaticID, FMapSystem);
  // Create both versions now.
  lPolygonRgn := CreateRgnFromPolygon(ASheetID, lPolygon.ObjectID);

  // The temp table to hold sample keys must be populated on the main report connection
  AConnection.Execute(SQL_TEMPLIST_CREATE_IF_REQUIRED);
  try
    GetLocaleFormatSettings(LOCALE_USER_DEFAULT, lFormatSettings);
    lFormatSettings.DecimalSeparator := '.';

    try
      lSouthWestBuffer := SpatialSystemMaxCommonSamplingSquareSize(FMapSystem);
    except
      // System doesn't have a default size.
      on ESpatialRefError do
        lSouthWestBuffer := 0;
    end;
    GetPolygonBoundingBox(ASheetID, lPolygon.ObjectID, lSouthWestBuffer, lSWLatLong, lNELatLong);

    // Include 100km squares regardless, for OSGB and OSNI
    with dmDatabase.ExecuteSQL(Format(
        'SELECT %s, Spatial_Ref, Spatial_Ref_System, Lat, Long FROM %s '
        + 'WHERE (Spatial_Ref_System = ''OSGB'' AND Len(Spatial_Ref) = 2) '
        + ' OR (Spatial_Ref_System = ''OSNI'' AND Len(Spatial_Ref) = 1) '
        + ' OR (Lat>='     + FloatToStr(lSWLatLong.Lat, lFormatSettings)
        + '  AND Lat<='    + FloatToStr(lNELatLong.Lat, lFormatSettings)
        + '  AND [Long]>=' + FloatToStr(lSWLatLong.Long, lFormatSettings)
        + '  AND [Long]<=' + FloatToStr(lNELatLong.Long, lFormatSettings)
        + ' ) ORDER BY Lat, [Long]', [ASourceField, ASourceTable]), True) do
    begin
      while not Eof do begin
        // All SRs are converted to FMapSystem ENs
        lSpatialRgn := CreateRgnFromSpatialRef(
            Fields['Spatial_Ref'].Value,
            Fields['Spatial_Ref_System'].Value,
            Fields['Lat'].Value,
            Fields['Long'].Value);
        try
          if IsInPolygon(lPolygonRgn, lSpatialRgn, AIncludePartialSquares) then
            InsertKey(Fields[ASourceField].Value);
        finally
          DeleteObject(lSpatialRgn);
        end;
        MoveNext;
      end;
      Close;
    end;
  finally
    DeleteObject(lPolygonRgn);
    lPolygon.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Retrieve the list of grid squares that touch a polygon.  TheadTerminationCheck
     parameter allows the procedure to abort if the thread is terminated.
}
procedure TPolygonScanner.GetGridSquaresForPolygon(ASheetID, AStaticID:
    Integer; const ASpatialRefSystem: string; APrecision: integer;
    AGridSquares: TStringList;  AThreadTerminationCheck:
    TThreadTerminationCheck = nil; AIncludePartialSquares: Boolean = True);
var
  lPolygon : TMapPolygon;
  lSWLatLong, lNELatLong : TLatLong;
  lBBGridSquares: TStringList;
  lSWRef, lNERef: String;
  lPolygonRgn, lSquareRgn: HRGN;
  i, lSouthWestBuffer: Integer;
begin
  lPolygon := TMapPolygon.Create(FMapHandle, ASheetID, AStaticID, FMapSystem);
  lPolygonRgn := CreateRgnFromPolygon(ASheetID, lPolygon.ObjectID);
  lBBGridSquares := TStringList.Create;
  try
    try
      lSouthWestBuffer := SpatialSystemMaxCommonSamplingSquareSize(ASpatialRefSystem);
    except
      // System doesn't have a default size.
      on ESpatialRefError do
        lSouthWestBuffer := 0;
    end;
    GetPolygonBoundingBox(ASheetID, lPolygon.ObjectID, lSouthWestBuffer, lSWLatLong, lNELatLong);
    lSWRef := ConvertFromLatLong(lSWLatLong, ASpatialRefSystem);
    lNERef := ConvertFromLatLong(lNELatLong, ASpatialRefSystem);
    ReduceGridPrecision(lSWRef, APrecision, ASpatialRefSystem);
    ReduceGridPrecision(lNERef, APrecision, ASpatialRefSystem);

    // build a list of all the grid squares in a bounding box we need to test
    BuildGridSquareRange(
        lSWRef, lNERef, ASpatialRefSystem, lBBGridSquares, False, AThreadTerminationCheck);

    for i := 0 to lBBGridSquares.Count - 1 do begin
      if Assigned(AThreadTerminationCheck) then
        if AThreadTerminationCheck then raise EAbort.Create('');

      lSquareRgn := CreateRgnFromSpatialRef(
          lBBGridSquares[i],
          ASpatialRefSystem,
          lSWLatLong.Lat,
          lSWLatLong.Long,
          APrecision);
      try
        if SameText(ASpatialRefSystem, FMapSystem) and
           IsInPolygon(lPolygonRgn, lSquareRgn, AIncludePartialSquares) then
          AGridSquares.Add(lBBGridSquares[i]);
      finally
        DeleteObject(lSquareRgn);
      end;
    end;
  finally
    DeleteObject(lPolygonRgn);
    lPolygon.Free;
    lBBGridSquares.Free;
  end;
end;

{-------------------------------------------------------------------------------
  GetBoundingBox allows us to define a box around the polygon we are checking,
  and therefore we can use simple SQL to get a rough guess list of hits
}
procedure TPolygonScanner.GetPolygonBoundingBox(const iSheetID, iPolygonID, iSouthWestBuffer: Integer;
  out oSWLatLong, oNELatLong: TLatLong);
var
  sExtent : MSExtent;
  lSWEastNorth, lNEEastNorth : TMapCoord;
begin
  MapCheck(MapGetObjectExtents(FMapHandle, iSheetID, iPolygonID, sExtent));
  { Put the easting northing into TMapCoords for conversion }
  lSWEastNorth.x := sExtent.dWest - iSouthWestBuffer;
  lNEEastNorth.x := sExtent.dEast;
  lSWEastNorth.y := sExtent.dSouth - iSouthWestBuffer;
  lNEEastNorth.y := sExtent.dNorth;
  { Convert to lat long }
  oSWLatLong := SpecificENtoLatLong(lSWEastNorth, FMapSystem);
  oNELatLong := SpecificENtoLatLong(lNEEastNorth, FMapSystem);
end;

//==============================================================================
{ TMapPolygon }
//==============================================================================

{ Procedure to add an edge defined by 2 points to our list.  Edges are
    always stored south to north }

{ MDB : Altered so that horizontal edges are added east to west }
procedure TMapPolygon.AddEdge(iCoord1, iCoord2: MSCoord);
var
  lNewOriginalEdge, lNewLatLongEdge: PEdge;
  lLatLong: TLatLong;
begin
  New(lNewOriginalEdge);
  if iCoord1.Y < iCoord2.Y then begin// first point is southernmost
    lNewOriginalEdge^.SPoint := iCoord1;
    lNewOriginalEdge^.NPoint := iCoord2;
  end else
  if iCoord1.Y > iCoord2.Y then begin// second point is southernmost
    lNewOriginalEdge^.SPoint := iCoord2;
    lNewOriginalEdge^.NPoint := iCoord1;
  end else {horizontal line}
  if iCoord1.X > iCoord2.X then begin// first point is easternmost
    lNewOriginalEdge^.SPoint := iCoord1;
    lNewOriginalEdge^.NPoint := iCoord2;
  end else begin// second point is easternmost
    lNewOriginalEdge^.SPoint := iCoord2;
    lNewOriginalEdge^.NPoint := iCoord1;
  end;
  FEdges.Add(lNewOriginalEdge);

  // MSCoord, TMapCoord and TLatLong are all (x: double; y: double).
  New(lNewLatLongEdge);
  lLatLong := SpecificENToLatLong(TMapCoord(lNewOriginalEdge^.SPoint), FMapSystem);
  lNewLatLongEdge^.SPoint := MSCoord(lLatLong);
  lLatLong := SpecificENToLatLong(TMapCoord(lNewOriginalEdge^.NPoint), FMapSystem);
  lNewLatLongEdge^.NPoint := MSCoord(lLatLong);
  FLatLongEdges.Add(lNewLatLongEdge);
end;


{ MDB : Necessary both on destruction and before reconstruction.  (See Reconstruct) }
procedure TMapPolygon.Clean;
var i: Integer;
begin
  { Loop through freeing edges }
  for i := 0 to FEdges.Count - 1 do
    Dispose(PEdge(FEdges[i]));
  FEdges.Clear;

  for i := 0 to FLatLongEdges.Count - 1 do
    Dispose(PEdge(FLatLongEdges[i]));
  FLatLongEdges.Clear;
end;

{ MDB : Functionality removed from constructor as there are now multiple constructors. }
procedure TMapPolygon.Construct;
begin
  FEdges := TList.Create;
  FLatLongEdges  := TList.Create;
  MapEditClearObject(FMapHandle);  // a failure means we never had an edit object, so ignore
  MapCheck(MapEditSelectObject(FMapHandle, FSheetId, FObjectID));
  ConstructEdges;
end;

{ MDB : Pretty self explanatory. }
procedure TMapPolygon.ConstructEdges;
var
  lNumCoords : Integer;
  i : Integer;
  lCoord1, lCoord2 : MSCoord;
begin
  { How many coords define the polygon? }
  MapCheck(MapEditGetNumCoords(FMapHandle, lNumCoords));
  { Read them out, constructing edges }
  MapCheck(MapEditGetObjectCoord(FMapHandle, 0, lCoord1));
  for i := 1 to lNumCoords-1 do begin
    MapCheck(MapEditGetObjectCoord(FMapHandle, i, lCoord2));
    AddEdge(lCoord1, lCoord2);
    lCoord1 := lCoord2;
  end;
  { Need the edge from the last coord to the first coord to complete the loop }
  MapCheck(MapEditGetObjectCoord(FMapHandle, 0, lCoord2));
  AddEdge(lCoord1, lCoord2);
end;

{ MDB : The original constructor. }
constructor TMapPolygon.Create(iMapHandle: HWnd; iSheetID, iStaticID: Integer; iMapSystem: String);
var
  lReturnCode: integer;
begin
  FMapHandle := iMapHandle;
  FSheetID   := iSheetID;
  FStaticID  := iStaticID;
  FMapSystem := iMapSystem;
  lReturnCode := MapGetObjectFromStaticID(iMapHandle, iSheetID, iStaticID, FObjectID);
  if lReturnCode = -111 then
    // -111 means polygon static id invalid as polygon has been removed
    raise EMapPolygonScanner.Create(ResStr_PolygonMissing)
  else
    MapCheck(lReturnCode);
  Construct;
end;


{ MDB : New constructor needed. }
constructor TMapPolygon.CreateFromObjectID(iMapHandle: HWnd; iSheetID, iObjectID: Integer;
  iMapSystem: String);
begin
  FMapHandle := iMapHandle;
  FSheetID   := iSheetID;
  FObjectID  := iObjectID;
  FMapSystem := iMapSystem;
  MapCheck(MapGetObjectStaticID(iMapHandle, iSheetID, iObjectID, FStaticID));
  Construct;
end;

{ MDB : Note the introduction of the Clean method. }
destructor TMapPolygon.Destroy;
begin
  if FEdges <> nil then Clean;
  FEdges.Free;
  FLatLongEdges.Free;
  inherited;
end;


{ Function to check if a point lies inside or outside the polygon }
{ MDB : This function, while considerably longer now, only functions differently
  in a tiny number of cases.  It is to do with bugs that were present when a the
  line drawn westward from a point passes through a vertex of the containing
  polygon.  Previously both edges were counted, but this is not always desirable. }
function TMapPolygon.IsHit(iTestCoord: MSCoord): boolean;
var
  i, lEdgeCount : Integer;
  lEdge : PEdge;
  lSNRatio, lEWRatio : Double;
  lEdges: TList;

  function preI: Integer;
  begin
    if i = 0 then Result := lEdges.Count - 1
    else Result := i - 1;
  end;

  function postI: Integer;
  begin
    if i = lEdges.Count - 1 then Result := 0
    else Result := i + 1;
  end;

  procedure GetEdges(out preEdge, postEdge: TEdge);
  begin
    preEdge := PEdge(lEdges[preI])^;
    postEdge := PEdge(lEdges[postI])^;
  end;

  procedure IncCount;
  var preEdge, postEdge: TEdge;
      preSouth, postSouth: Boolean;
  begin
    if (lEdge^.SPoint.Y = iTestCoord.Y) then begin
      GetEdges(preEdge, postEdge);
      if (lEdge^.NPoint.Y = iTestCoord.Y) then {lEdge is horizontal} begin
        if iTestCoord.X <= lEdge^.SPoint.X then {point is in lEdge} Inc(lEdgeCount)
        else begin {Assuming pre and post edges are not horizontal}
          if preEdge.NPoint.Y = iTestCoord.Y
            then preSouth := preEdge.SPoint.Y < iTestCoord.Y
            else preSouth := preEdge.NPoint.Y < iTestCoord.Y;
          if postEdge.NPoint.Y = iTestCoord.Y
            then postSouth := postEdge.SPoint.Y < iTestCoord.Y
            else postSouth := postEdge.NPoint.Y < iTestCoord.Y;
          if preSouth = postSouth then // No Cut
          else Inc(lEdgeCount);
        end;
      end else begin
        if ((lEdge^.SPoint.X = preEdge.SPoint.X) and (lEdge^.SPoint.Y = preEdge.SPoint.Y)) or
           ((lEdge^.SPoint.X = preEdge.NPoint.X) and (lEdge^.SPoint.Y = preEdge.NPoint.Y)) then
          // Do nothing - dealt with by previous edge
        else
          if postEdge.SPoint.Y = postEdge.NPoint.Y then
            //Do nothing - postEdge is horizontal, dealt with by the horizontal edge
          else
            if (lEdge^.SPoint.X = postEdge.SPoint.X) and (lEdge^.SPoint.Y = postEdge.SPoint.Y) then
              //Do nothing - Touching a point
            else
              //Cutting the join  -increment the count
              Inc(lEdgeCount);
      end;
    end else if (lEdge^.NPoint.Y = iTestCoord.Y) then begin
      GetEdges(preEdge, postEdge);
      if ((lEdge^.NPoint.X = preEdge.SPoint.X) and (lEdge^.NPoint.Y = preEdge.SPoint.Y)) or
         ((lEdge^.NPoint.X = preEdge.NPoint.X) and (lEdge^.NPoint.Y = preEdge.NPoint.Y)) then
        // Do nothing - dealt with by previous edge
      else
        if postEdge.SPoint.Y = postEdge.NPoint.Y then
          //Do nothing - postEdge is horizontal, dealt with by the horizontal edge
        else
          if (lEdge^.NPoint.X = postEdge.NPoint.X) and (lEdge^.NPoint.Y = postEdge.NPoint.Y) then
            //Do nothing - Touching a point
          else
            //Cutting the join  -increment the count
            Inc(lEdgeCount);
    end else Inc(lEdgeCount);
  end;

begin
  // count of edges that intersect with a horizontal line from the point westwards to infinity
  lEdgeCount := 0;
  lEdges := FEdges;

  { Loop through our edges }
  for i := 0 to lEdges.Count-1 do begin
    lEdge := PEdge(lEdges[i]);
    { Ignore unless our point lies somewhere along the edge's range of latitudes }
    if (lEdge^.SPoint.Y <= iTestCoord.Y) and (lEdge^.NPoint.Y >= iTestCoord.Y) then begin
      if lEdge^.SPoint.Y = lEdge^.NPoint.Y then begin
         { Edge is horizontal line, NPoint is westernmost }
         if (lEdge^.NPoint.X <= iTestCoord.X) then
           IncCount;
      end else begin
        if lEdge.SPoint.X = lEdge.NPoint.X then begin
          { lEdge line - is it to the left? }
          if lEdge.SPoint.X <= iTestCoord.X then
            IncCount;
        end else begin
          { Count edges that lie to left of point }
          { If definitely to the left then include }
          if (iTestCoord.X > lEdge^.SPoint.X) and (iTestCoord.X > lEdge^.NPoint.X) then
            IncCount
          else if (iTestCoord.X > lEdge^.SPoint.X) or (iTestCoord.X > lEdge^.NPoint.X) then begin
            { this line crosses near our point at an angle, so need to calculate if left or right }
            lSNRatio := (iTestCoord.Y - lEdge^.SPoint.Y) / (lEdge^.NPoint.Y - lEdge^.SPoint.Y);
            lEWRatio := (iTestCoord.X - lEdge^.SPoint.X) / (lEdge^.NPoint.X - lEdge^.SPoint.X);
            { Count edge if point lies further along the east west direction than the north south }
            if ((lEWRatio > lSNRatio) and (lEdge^.NPoint.X > lEdge^.SPoint.X))
                or ((lEWRatio < lSNRatio) and (lEdge^.NPoint.X < lEdge^.SPoint.X)) then
              IncCount;
          end;
        end;
      end; // not a horizontal line
    end; // if in correct latitude
  end; // for
  Result := lEdgeCount mod 2 = 1; // if odd number of lines found, must be a hit
end;

{ MDB : This is needed by the new descendant TSuperMapPolygon. }
procedure TMapPolygon.Reconstruct;
begin
  Clean;
  FEdges.Clear;
  FLatLongEdges.Clear;
  ConstructEdges;
end;

end.

