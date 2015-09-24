//==============================================================================
//  Unit: MapPolygonInteraction
//
//  Description: This unit has been created to fascilitate the introduction of
//    holes in polygons on the map - called through the actSubtractBoundary
//    action.  It defines primarily two classes, TSuperMapPolygon and
//    TSubMapPolygon, respectively the class that the larger polygon and the
//    polygon describing the area to be subtracted.
//
//  Author: Michael Bailey
//  Created: Nov 2002
//
//  Last Revision Details:
//    $Revision: 7 $
//    $Date: 24/01/08 12:33 $
//    $Author: Ericsalmon $
//
//  $History: MapPolygonInteraction.pas $
//  
//  *****************  Version 7  *****************
//  User: Ericsalmon   Date: 24/01/08   Time: 12:33
//  Updated in $/JNCC/Components
//  VI 15060 - CCN245. Report Wizard polygon hit detection against grid
//  references.
//  
//  *****************  Version 6  *****************
//  User: Ericsalmon   Date: 26/08/04   Time: 12:26
//  Updated in $/JNCC/Components
//  Tidying destructors.
//  
//  *****************  Version 5  *****************
//  User: Ericsalmon   Date: 13/02/04   Time: 11:23
//  Updated in $/JNCC/Components
//  ID 3809. CCN99, upgrade to MapServer5.
//  
//  *****************  Version 4  *****************
//  User: Michaelbailey Date: 7/11/02    Time: 11:01
//  Updated in $/JNCC/Components
//  Of course, I then realise that the split could also cut the polygon
//  being added.
//  
//  *****************  Version 3  *****************
//  User: Michaelbailey Date: 6/11/02    Time: 17:28
//  Updated in $/JNCC/Components
//  Prevented splits being created which cut the polygon boundary.
//  
//  *****************  Version 2  *****************
//  User: Michaelbailey Date: 6/11/02    Time: 16:52
//  Updated in $/JNCC/Components
//  Minor Alteration.
//  
//  *****************  Version 1  *****************
//  User: Michaelbailey Date: 6/11/02    Time: 11:25
//  Created in $/JNCC/Components
//  CCN No: CEDaR3  -  Creating holes in Polygons.
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================


unit MapPolygonInteraction;

interface

uses MapPolygonScanner, Classes, MS5;

type
  PMSCoord = ^MSCoord;

  { This class is a base for TSubMapPolygon and TSuperMapPolygon.  While not
    abstract it is not intended that this should ever be instantiated. }
  TInteractiveMapPolygon = class(TMapPolygon)
  private
    FVertices: TList;
  protected
    procedure AddEdge(iCoord1, iCoord2: MSCoord); override;
    procedure Construct; override;
    procedure Reconstruct; override;
    procedure Clean; override;
    function ProposedSplitDontCut(point1, point2: PMSCoord): Boolean;
    class function EdgesParallel(const edge1, edge2: TEdge): Boolean;
    class function EdgeTouchesEdge(const toucher, touchee: TEdge): Boolean;
    class function EdgeCutsEdge(const edge1, edge2: TEdge): Boolean;
    class function EdgeCutsVerticalEdge(const edge, vEdge: TEdge): Boolean;
    class function CoincidentVertices(const edge1, edge2: TEdge): Boolean;
    class function m(const edge: TEdge): Double;
    class function c(const edge: TEdge): Double;
    class function XInEdge(const edge: TEdge; const x: Double): Boolean;
    class function YInEdge(const edge: TEdge; const y: Double): Boolean;
  public
    destructor Destroy; override;
  end;

  { This class represents a boundary to be subtracted from another boundary
    represented by an instance of TSuperMapPolygon. }
  TSubMapPolygon = class(TInteractiveMapPolygon)
  private
    procedure RenderInvalid;
  end;

  { Instances of TDummyMapPolygon are also intended to be subtracted from a
    super-boundary, but unlike its parent it is not intended that instances of
    TDummyMapPolygon should refer to an actual boundary on the map.  They are
    created temporarily when cuts (here called splits) have to be removed to
    fascilitate the subtraction of further boundaries. }
  TDummyMapPolygon = class(TSubMapPolygon)
  public
    constructor CreateInside(vertexList: TList; startPoint, endPoint: Integer; iMapSystem: String);
    constructor CreateOutside(vertexList: TList; endPoint, startPoint: Integer; iMapSystem: String);
  end;

  TDummyMapPolygonArray = array of TDummyMapPolygon;

  { Instances of TSuperMapPolygon represent polygons on the map from which
    sub-polygons are to be subtracted. }
  TSuperMapPolygon = class(TInteractiveMapPolygon)
  private
    FSplits: TList;
    procedure ConstructSplits;
    function CheckForEdgeCutting(subPolygon: TSubMapPolygon): Boolean;
    function CheckForHoleCircling(subPolygon: TSubMapPolygon): Boolean;
    function NicelyContains(subPolygon: TSubMapPolygon): Boolean;
    procedure Join(subPolygon: TSubMapPolygon);
    procedure JoinAt(subPolygon: TSubMapPolygon; superIndex, subIndex: Integer);
    procedure RemoveSplits(subPolygon: TSubMapPolygon; out freedPolygons: TDummyMapPolygonArray);
    procedure Remove(subPolygon: TSubMapPolygon);
    function CutsSplit(splitIndex: Integer; subPolygon: TSubMapPolygon): Boolean;
    function RemoveSplit(splitIndex: Integer): TDummyMapPolygon;
    function RemoveInnerSplit(splitIndex: Integer): TDummyMapPolygon;
    function RemoveOuterSplit(splitIndex: Integer): TDummyMapPolygon;
  protected
    procedure Construct; override;
    procedure Reconstruct; override;
    procedure Clean; override;
  public
    destructor Destroy; override;
    function FullyContains(subPolygon: TSubMapPolygon): Boolean;
    procedure Subtract(subPolygon: TSubMapPolygon);
  end;

//==============================================================================
implementation

uses
  MapServerLink, Math;

type
  //index1 and index2 reference the two edges that comprise the split.
  TSplit = record
    index1: Integer;
    index2: Integer;
  end;
  PSplit = ^TSplit;

{ TDummyMapPolygon }

constructor TDummyMapPolygon.CreateInside(vertexList: TList; startPoint, endPoint: Integer;
  iMapSystem: String);
// Creates a DummyMapPolygon using the vertices in vertexList between startPoint and endPoint.
var
  k: Integer;
  lCoord1, lCoord2: MSCoord;
begin
  MapSystem := iMapSystem;
  FVertices := TList.Create;

  lCoord1 := PMSCoord(vertexList[startPoint])^;
  for k := startPoint + 1 to endPoint do begin
    lCoord2 := PMSCoord(vertexList[k])^;
    AddEdge(lCoord1, lCoord2);
    lCoord1 := lCoord2;
  end;
  lCoord2 := PMSCoord(vertexList[startPoint])^;
  AddEdge(lCoord1, lCoord2);
end;

constructor TDummyMapPolygon.CreateOutside(vertexList: TList; endPoint, startPoint: Integer;
  iMapSystem: String);
// Creates a DummyMapPolygon using the vertices in vertexList prior and including endpoint,
// and post and including startPoint.
var
  k: Integer;
  lCoord1, lCoord2: MSCoord;
begin
  MapSystem := iMapSystem;
  FVertices := TList.Create;

  lCoord1 := PMSCoord(vertexList[0])^;
  for k := 1 to endPoint do begin
    lCoord2 := PMSCoord(vertexList[k])^;
    AddEdge(lCoord1, lCoord2);
    lCoord1 := lCoord2;
  end;
  for k := startPoint to vertexList.Count - 1 do begin
    lCoord2 := PMSCoord(vertexList[k])^;
    AddEdge(lCoord1, lCoord2);
    lCoord1 := lCoord2;
  end;
  lCoord2 := PMSCoord(vertexList[0])^;
  AddEdge(lCoord1, lCoord2);
end;

{ TInteractiveMapPolygon }

procedure TInteractiveMapPolygon.AddEdge(iCoord1, iCoord2: MSCoord);
// Overriden.  Stores the vertex indicated by iCoord1.
var lCoord: PMSCoord;
begin
  inherited;
  New(lCoord);
  lCoord^ := iCoord1;
  FVertices.Add(lCoord);
end;

class function TInteractiveMapPolygon.c(const edge: TEdge): Double;
//Returns the y coordinate of the intersection of the y-axis and the line of which the edge is a segment.
begin
  Result := (edge.SPoint.Y * edge.NPoint.X - edge.NPoint.Y * edge.SPoint.X)/(edge.NPoint.X - edge.SPoint.X);
end;

procedure TInteractiveMapPolygon.Clean;
// Overriden.
var k: Integer;
begin
  inherited;
  if FVertices <> nil then
    for k := FVertices.Count - 1 downto 0 do
      Dispose(PMSCoord(FVertices[k]));
end;

class function TInteractiveMapPolygon.CoincidentVertices(const edge1, edge2: TEdge): Boolean;
// Returns True if the edges have a coincident vertex.
begin
  Result := (((edge1.NPoint.X = edge2.NPoint.X) and (edge1.NPoint.Y = edge2.NPoint.Y)) or
             ((edge1.NPoint.X = edge2.SPoint.X) and (edge1.NPoint.Y = edge2.SPoint.Y)) or
             ((edge1.SPoint.X = edge2.NPoint.X) and (edge1.SPoint.Y = edge2.NPoint.Y)) or
             ((edge1.SPoint.X = edge2.SPoint.X) and (edge1.SPoint.Y = edge2.SPoint.Y)));
end;

procedure TInteractiveMapPolygon.Construct;
// Overriden.
begin
  FVertices := TList.Create;
  inherited;
end;

destructor TInteractiveMapPolygon.Destroy;
begin
  FVertices.Free;
  inherited;
end;

class function TInteractiveMapPolygon.EdgeCutsEdge(const edge1, edge2: TEdge): Boolean;
// Assumes that the edges are not parallel and don't touch.
// Returns True if the edges intersect.
var x: Double;
begin
  if edge2.NPoint.X = edge2.SPoint.X then {edge2 is vertical}
    Result := EdgeCutsVerticalEdge(edge1, edge2)
  else {edge2 is not vertical}
    if edge1.NPoint.X = edge1.SPoint.X then {edge1 is vertical}
      Result := EdgeCutsVerticalEdge(edge2, edge1)
    else {neither edge is vertical} begin
      // Calculate x coordinate of intersection of lines
      x := ( c(edge2) - c(edge1) )/( m(edge1) - m(edge2) );
      // If x coordinate is on both edges then they intersect.
      Result := XInEdge(edge1, x) and XInEdge(edge2, x);
    end;
end;

class function TInteractiveMapPolygon.EdgeCutsVerticalEdge(const edge, vEdge: TEdge): Boolean;
// Called only be EdgeCutsEdge.
// This function assumes vEdge is vertical and edge is not.
// Returns True if the edges intersect.
var y: Double;
begin
  if ((edge.NPoint.X < vEdge.SPoint.X) and (edge.SPoint.X < vEdge.SPoint.X)) or
     ((edge.NPoint.X > vEdge.SPoint.X) and (edge.SPoint.X > vEdge.SPoint.X)) then
    Result := False
  else begin
    //Calculate the y coordinate where edge2 crosses the line of edge1
    y := m(edge)*vEdge.SPoint.X + c(edge);
    Result := YInEdge(vEdge, y);
  end;
end;

class function TInteractiveMapPolygon.EdgesParallel(const edge1, edge2: TEdge): Boolean;
// Returns True if the edges are parallel.
begin
  if edge1.NPoint.X = edge1.SPoint.X then Result := edge2.NPoint.X = edge2.SPoint.X
  else if edge2.NPoint.X = edge2.SPoint.X then Result := False
  else Result := m(edge1) = m(edge2);
end;

class function TInteractiveMapPolygon.EdgeTouchesEdge(const toucher, touchee: TEdge): Boolean;
// Returns True if either vertex of toucher lies within touchee.
// Note that this assumes there are no coincident vertices.
begin
  Result := (XInEdge(touchee, toucher.NPoint.X) and (toucher.NPoint.Y = m(touchee)*toucher.NPoint.X + c(touchee))) or
            (XInEdge(touchee, toucher.SPoint.X) and (toucher.SPoint.Y = m(touchee)*toucher.SPoint.X + c(touchee)));
end;

class function TInteractiveMapPolygon.m(const edge: TEdge): Double;
//Returns the gradient of the line of which the edge is a segment.
begin
  Result := (edge.NPoint.Y - edge.SPoint.Y)/(edge.NPoint.X - edge.SPoint.X);
end;

function TInteractiveMapPolygon.ProposedSplitDontCut(point1, point2: PMSCoord): Boolean;
//Determines whether the proposed split will cut any of the existing edges of the polygon.
//If not the function returns True.
var newEdge: TEdge;
    k: Integer;
begin
  if point1^.Y > point2^.Y then begin
    newEdge.NPoint := point1^;
    newEdge.SPoint := point2^;
  end else if point1^.Y < point2^.Y then begin
    newEdge.SPoint := point1^;
    newEdge.NPoint := point2^;
  end else if point1^.X > point2^.X then begin
    newEdge.SPoint := point1^;
    newEdge.NPoint := point2^;
  end else begin
    newEdge.NPoint := point1^;
    newEdge.SPoint := point2^;
  end;

  Result := True;
  for k := Edges.Count - 1 downto 0 do
    if Result then
      if not (CoincidentVertices(newEdge, PEdge(Edges[k])^) or EdgesParallel(newEdge, PEdge(Edges[k])^)) then
        Result := not EdgeCutsEdge(newEdge, PEdge(Edges[k])^);
end;

procedure TInteractiveMapPolygon.Reconstruct;
// Overriden.
begin
  FVertices.Clear;
  inherited;
end;

class function TInteractiveMapPolygon.XInEdge(const edge: TEdge; const x: Double): Boolean;
// Returns True if x is within the edge's longitude.
begin
  Result := ((edge.NPoint.X > x) and (edge.SPoint.X < x)) or ((edge.NPoint.X < x) and (edge.SPoint.X > x));
end;

class function TInteractiveMapPolygon.YInEdge(const edge: TEdge; const y: Double): Boolean;
// Returns True if y is within the edge's latitude.
begin
  Result := ((edge.NPoint.Y > y) and (edge.SPoint.Y < y)) or ((edge.NPoint.Y < y) and (edge.SPoint.Y > y));
end;

{ TSubMapPolygon }

procedure TSubMapPolygon.RenderInvalid;
// Renders a polygon invalid, so that further use of the object will cause an
// error.  This is done after the polygon has been subtracted from a super-
// polygon.
begin
  Clean;
  FVertices.Free;
  FVertices := nil;
end;

{ TSuperMapPolygon }

function TSuperMapPolygon.CheckForEdgeCutting(subPolygon: TSubMapPolygon): Boolean;
// Returns True if subPolygon does not intersect with any edges excluding splits.
var i, j: Integer;
    splitArray: array of Boolean;
begin
  SetLength(splitArray, Edges.Count);
  for i := Edges.Count - 1 downto 0 do splitArray[i] := False;
  for i := FSplits.Count - 1 downto 0 do begin
    splitArray[PSplit(FSplits[i])^.index1] := True;
    splitArray[PSplit(FSplits[i])^.index2] := True;
  end;

  Result := True;
  for i := Edges.Count - 1 downto 0 do
    if Result and (not splitArray[i]) then
      for j := subPolygon.Edges.Count - 1 downto 0 do
        if Result then begin
          Result := not (CoincidentVertices(PEdge(Edges[i])^, PEdge(subPolygon.Edges[j])^) or
                         EdgeTouchesEdge(PEdge(Edges[i])^, PEdge(subPolygon.Edges[j])^) or
                         EdgeTouchesEdge(PEdge(subPolygon.Edges[j])^, PEdge(Edges[i])^));
          if Result and not EdgesParallel(PEdge(Edges[i])^, PEdge(subPolygon.Edges[j])^) then
            Result := not EdgeCutsEdge(PEdge(Edges[i])^, PEdge(subPolygon.Edges[j])^);
        end;
end;

function TSuperMapPolygon.CheckForHoleCircling(subPolygon: TSubMapPolygon): Boolean;
// Returns True if subPolygon does not itself contain any previously subtacted sub-polygons.
// It does this by checking the subPolygon does not circle any of the holes.  That can be determined by counting
// how many times subPolygon cuts any given split.  If the method is to return True then subPolygon must cut
// each split an even number of times.
var nSplit, nEdge, count: Integer;

  function IsCut(edge, splitEdge: TEdge): Boolean;
  begin
    if EdgeTouchesEdge(edge, splitEdge) then //scream
      Result := True
    else Result := not (EdgesParallel(edge, splitEdge) or not EdgeCutsEdge(edge, splitEdge));
  end;

begin
  Result := True;
  for nSplit := FSplits.Count - 1 downto 0 do
    if Result then begin
      count := 0;
      for nEdge := subPolygon.Edges.Count - 1 downto 0 do
        if IsCut(PEdge(subPolygon.Edges[nEdge])^, PEdge(Edges[PSplit(FSplits[nSplit])^.index1])^) then Inc(count);
      Result := (count mod 2) = 0;
    end;
end;

procedure TSuperMapPolygon.Clean;
// Overriden.
var k: Integer;
begin
  inherited;
  for k := FSplits.Count - 1 downto 0 do
    Dispose(PSplit(FSplits[k]));
end;

procedure TSuperMapPolygon.Construct;
// Overriden.
begin
  inherited;
  FSplits := TList.Create;
  ConstructSplits;
end;

procedure TSuperMapPolygon.ConstructSplits;
// Determines which edges are splits and stores them for later calculation.
var i, j: Integer;
    lSplit: PSplit;
begin
  for i := Edges.Count - 2 downto 0 do
    for j := Edges.Count - 1 downto i + 1 do
      if (PEdge(Edges[i])^.SPoint.X = PEdge(Edges[j])^.SPoint.X) and
         (PEdge(Edges[i])^.SPoint.Y = PEdge(Edges[j])^.SPoint.Y) and
         (PEdge(Edges[i])^.NPoint.X = PEdge(Edges[j])^.NPoint.X) and
         (PEdge(Edges[i])^.NPoint.Y = PEdge(Edges[j])^.NPoint.Y) then begin
        New(lSplit);
        lSplit^.index1 := i;
        lSplit^.index2 := j;
        FSplits.Add(lSplit);
      end;
end;

function TSuperMapPolygon.CutsSplit(splitIndex: Integer; subPolygon: TSubMapPolygon): Boolean;
// Returns True if subPolygon intersects in any way with the specified split.
// Note that this method should not be called if subPolygon intersects with the super-polygon proper,
// so that case is ignored.
var edge1, edge2: TEdge;
    k: Integer;
begin
  edge1 := PEdge(Edges[PSplit(FSplits[splitIndex])^.index1])^;
  Result := False;
  for k := subPolygon.Edges.Count - 1 downto 0 do
    if not Result then begin
      edge2 := PEdge(subPolygon.Edges[k])^;
      Result := CoincidentVertices(edge1, edge2) or EdgeTouchesEdge(edge1, edge2) or EdgeTouchesEdge(edge2, edge1);
      if (not Result) and (not EdgesParallel(edge1, edge2)) then Result := EdgeCutsEdge(edge1, edge2);
    end;
end;

destructor TSuperMapPolygon.Destroy;
begin
  FSplits.Free;
  inherited;
end;

function TSuperMapPolygon.FullyContains(subPolygon: TSubMapPolygon): Boolean;
// Public method.  Returns True if subPolygon is viable for subtraction.
// This method must be checked before Subtract is called.
var k: Integer;
begin
  Result := True;
  for k := subPolygon.FVertices.Count - 1 downto 0 do
    Result := Result and IsHit(PMSCoord(subPolygon.FVertices[k])^);

  Result := Result and CheckForEdgeCutting(subPolygon);
  Result := Result and CheckForHoleCircling(subPolygon);
end;

procedure TSuperMapPolygon.Join(subPolygon: TSubMapPolygon);
// Determines the closest vertices of the TSuperMapPolygon and subPolygon and calls JoinAt.
var i, j, superIndex, subIndex: Integer;
    dist, dy, dx: Double;
begin
  dist := 1.7E308;
  subIndex := -1;
  superIndex := -1;
  for i := FVertices.Count - 1 downto 0 do
    for j := subPolygon.FVertices.Count - 1 downto 0 do begin
      dx := PMSCoord(FVertices[i])^.X - PMSCoord(subPolygon.FVertices[j])^.X;
      dy := PMSCoord(FVertices[i])^.Y - PMSCoord(subPolygon.FVertices[j])^.Y;
      if (Sqrt(dx*dx + dy*dy) < dist) and
         ProposedSplitDontCut(FVertices[i], subPolygon.FVertices[j]) and
         subPolygon.ProposedSplitDontCut(FVertices[i], subPolygon.FVertices[j]) then
      begin
        dist := Sqrt(dx*dx + dy*dy);
        superIndex := i;
        subIndex := j;
      end;
    end;
  JoinAt(subPolygon, superIndex, subIndex);
end;

procedure TSuperMapPolygon.JoinAt(subPolygon: TSubMapPolygon; superIndex, subIndex: Integer);
// Adds coordinates to the object in MapServer to create the hole.  The instance of TSuperMapPolygon is then
// reconstructed, in case the user wishes to makes further subtractions.  subPolygon is rendered invalid.
var k: Integer;
begin
  MapCheck(MapEditInsertObjectCoord(MapHandle, superIndex, PMSCoord(subPolygon.FVertices[subIndex])^));
  for k := subIndex - 1 downto 0 do
    MapCheck(MapEditInsertObjectCoord(MapHandle, superIndex, PMSCoord(subPolygon.FVertices[k])^));
  for k := subPolygon.FVertices.Count - 1 downto subIndex do
    MapCheck(MapEditInsertObjectCoord(MapHandle, superIndex, PMSCoord(subPolygon.FVertices[k])^));
  MapCheck(MapEditInsertObjectCoord(MapHandle, superIndex, PMSCoord(FVertices[superIndex])^));
  Reconstruct;
  subPolygon.RenderInvalid;
end;

function TSuperMapPolygon.NicelyContains(subPolygon: TSubMapPolygon): Boolean;
var splitIndex: Integer;
begin
  splitIndex := 0;
  while (splitIndex < FSplits.Count) and (not CutsSplit(splitIndex, subPolygon)) do Inc(splitIndex);
  Result := splitIndex = FSplits.Count;
end;

procedure TSuperMapPolygon.Reconstruct;
// Overriden.
begin
  FSplits.Clear;
  inherited;
  ConstructSplits;
end;

procedure TSuperMapPolygon.Remove(subPolygon: TSubMapPolygon);
// This method is distinct from Subtract in that it can be called recursively.
// This fascilitates the rearrangement of splits when subPolygon intersects with one or more of these.
var freedPolygons: TDummyMapPolygonArray;
    k: Integer;
begin
  if NicelyContains(subPolygon) then Join(subPolygon)
  else begin
    RemoveSplits(subPolygon, freedPolygons);
    Join(subPolygon);
    for k := High(freedPolygons) downto 0 do begin
      Remove(freedPolygons[k]);
      freedPolygons[k].Free;
    end;
  end;
end;

function TSuperMapPolygon.RemoveInnerSplit(splitIndex: Integer): TDummyMapPolygon;
// This method remains untested as I'm not sure this instance will ever occur.
var k: Integer;
    lSplit: TSplit;
begin
  lSplit := PSplit(FSplits[splitIndex])^;
  Result := TDummyMapPolygon.CreateOutside(FVertices, lSplit.index1, lSplit.index2 + 2, MapSystem);
  for k := FVertices.Count - 1 downto lSplit.index2 do
    MapCheck(MapEditDeleteObjectCoord(MapHandle, lSplit.index2));
  for k := lSplit.index1 downto 0 do
    MapCheck(MapEditDeleteObjectCoord(MapHandle, 0));
end;

function TSuperMapPolygon.RemoveOuterSplit(splitIndex: Integer): TDummyMapPolygon;
// Called by RemoveSplit.
var k: Integer;
    lSplit: TSplit;
begin
  lSplit := PSplit(FSplits[splitIndex])^;
  Result := TDummyMapPolygon.CreateInside(FVertices, lSplit.index1 + 2, lSplit.index2, MapSystem);
  for k := lSplit.index2 - lSplit.index1 downto 0 do
    MapCheck(MapEditDeleteObjectCoord(MapHandle, lSplit.index1));
end;

function TSuperMapPolygon.RemoveSplit(splitIndex: Integer): TDummyMapPolygon;
// Deletes coordinates from the polygon to remove the split.  An instance of TDummyMapPolygon is returned
// representing the removed sub-polygon.  This will be resubtracted later.
var k: Integer;
    outWest, inWest: Double;
begin
  outWest := PMSCoord(FVertices[0])^.X;
  for k := PSplit(FSplits[splitIndex])^.index1 downto 1 do
    outWest := Min(outWest, PMSCoord(FVertices[k])^.X);
  for k := PSplit(FSplits[splitIndex])^.index2 + 1 to FVertices.Count - 1 do
    outWest := Min(outWest, PMSCoord(FVertices[k])^.X);
  inWest := PMSCoord(FVertices[PSplit(FSplits[splitIndex])^.index1 + 1])^.X;
  for k := PSplit(FSplits[splitIndex])^.index1 + 2 to PSplit(FSplits[splitIndex])^.index2 do
    inWest := Min(inWest, PMSCoord(FVertices[k])^.X);
  if inWest < outWest then Result := RemoveInnerSplit(splitIndex)
  else Result := RemoveOuterSplit(splitIndex);
  Reconstruct;
end;

procedure TSuperMapPolygon.RemoveSplits(subPolygon: TSubMapPolygon; out freedPolygons: TDummyMapPolygonArray);
// Removes all splits that subPolygon intersects with.  An array of the sub-Polygons removed is returned.
var splitIndex: Integer;
begin
  SetLength(freedPolygons, 0);
  splitIndex := 0;
  repeat
    while (splitIndex < FSplits.Count) and (not CutsSplit(splitIndex, subPolygon)) do Inc(splitIndex);
    if splitIndex < FSplits.Count then begin
      SetLength(freedPolygons, Length(freedPolygons) + 1);
      freedPolygons[High(freedPolygons)] := RemoveSplit(splitIndex);
      splitIndex := 0;
    end;
  until splitIndex = FSplits.Count;
end;

procedure TSuperMapPolygon.Subtract(subPolygon: TSubMapPolygon);
// Public method.  This subtracts subPolygon from the super-Polygon.  Users of this class should make
// absolutely sure that FullyContains returns True before this method is called as it is not checked.
begin
  MapEditClearObject(MapHandle);  // a failure means we never had an edit object, so ignore
  MapCheck(MapEditSelectObject(MapHandle, SheetID, ObjectID));
  Remove(subPolygon);
  MapCheck(MapEditCommitObject(MapHandle));
  MapCheck(MapDeleteObject(subPolygon.MapHandle, subPolygon.SheetID, subPolygon.ObjectID));
end;

end.
