//================================================================================
// Author:  Stuart Ball     stuart.ball@jncc.gov.uk
// Date:    Feb 2009
// Purpose: Objects to contain the reference data used to perform checksa and
//          to actually carry out the check, given an OSGB grid ref and a VC.
//--------------------------------------------------------------------------------
//================================================================================
unit VC_check;

interface

uses
  Classes, Windows, SysUtils, Math,
{$WARNINGS OFF}
  GridRefs, GPC;
{$WARNINGS ON}

Type
  EVcCheck = class(EXception);
  TCheckVCType = (chkOK, chk10kmNotInVC, chk1kmNotInVC, chkGridNotOverlapVC);
  TVCList = array of byte; // list of VC numbers

  // info about a grid sq (10km or 1km)
  TSqInfo = record
    sq: string;         // OSGB grid ref
    x,y: integer;       // coordinate in km
    nVCs: byte;         // number of VCs that overlap it (up to about 4)
    VCs: TVCList;       // list of VCs that overlap it
  end;
  TSqArray = array of TSqInfo;

  // info about grid/vc combinations that have been checked
  TCheckedInfo = record
    grid: string;
    VCs: TVCList;
    outcome: TCheckVCType;
  end;
  TChecksDone = array of TCheckedInfo;

  TInfoToCheck = record
    Table: string;
    Key: string;
    GridRef: string;
    VCs: TVCList;
  end;

  // info about a check that failed
  TErrorInfo = class(TObject)
  private
    FTable: string;
    FKey: string;
    FOriginalGrid: string;
    FGridChecked: string;
    FErrType: TCheckVCType;
    FVCs: TVCList;
  public
    Constructor Create(infoToCheck: TInfoToCheck; const sq: string; const checkType: TCheckVCType);
  end;

  // maintain a list of errors
  TErrors = class(TList)
  private
  public
    Destructor Destroy; override;
    Procedure Clear; override;
    Function AddError(infoToCheck: TInfoToCheck; const sq: string; const checkType: TCheckVCType): integer;
  end;

  // Structure to hold the polygon for a given VC
  TVCPolygon = class(TObject)
  private
    Fvcn: byte;  // vice-county number - 0 for the GB outline
    FName: string; // file name
    FBounds: TLongRect; // bounding box
    FPolygon: Pgpc_polygon;  // outline polygons
  public
    constructor Create(const sFileName: string);
    destructor Destroy; override;
    property vcn: byte read Fvcn write Fvcn;
    property Bounds: TLongRect read FBounds;
    property Polygon: Pgpc_polygon read FPolygon;
    property Name: string read FName write FName;
  End;

  // list of VC polygons
  TVCPolygons = class(TList)
  private
    FBounds: TLongRect;
    function Get_VC(Index: integer): TVCPolygon;
  public
    constructor Create;
    destructor Destroy; override;
    property VC[Index: integer]: TVCPolygon read Get_VC;
    procedure Clear; override;
    function AddVC(const sFileName: string): Integer;
    function PointInVC(const x,y: Double): integer;
    function IsRectInVC(const vcn: integer; bb: TLongRect): boolean;
    function VCsHere(const sGrid: string): string;
  end;

  // The object that puts it all together and manages the checks
  TVC_Checker = class(TObject)
  private
    FSqs: TSqArray;
    FDone: TChecksDone;
    FDoneCount: Longint;
    FGridInPoly: Integer;
    FErrors: TErrors;
    FVCs: TVCPolygons;
    FAddinPath: string;
    function Get_ErrorCount: integer;
    procedure LoadGridRefLookupTable;
    function Get_InfoFileName: string;
    function FindSq(const sq: string): Longint;
    function CheckSq(gridSq: string; VCs: TVCList; var nVCs: byte): boolean;
    function CheckAlreadyDone(infoToCheck: TInfoToCheck): Longint;
    function GridInVc(infoToCheck: TInfoToCheck): boolean;
    function ExtractSquares(const GridSq: string; var OneK, TenK: string): byte;
    procedure RecordDone(infoToCheck: TInfoToCheck; const res: TCheckVCType);
    procedure LoadVCpolygon(const vc: byte);
    function CompareVCLists(aList, bList: TVCList): boolean;
    function VCListInList(aList, bList: TVCList): boolean;
    function FormatVCList(VCs: TVCLIst): string;
  protected
    property InfoFileName: string read Get_InfoFileName;
  public
    Constructor Create(const AddinPath: string);
    Destructor Destroy; override;
    property ErrorCount: integer read Get_ErrorCount;
    property ChecksDone: Longint read FDoneCount;
    property GridInVcDone: Integer read FGridInPoly;
    Function Check(infoToCheck: TInfoToCheck): boolean;
    Procedure WriteErrors(const aFileName: string);
  end;

implementation

Const
  INFO_PATH = 'vc_check\';
  INFO_FILE = 'vc_info.txt';
  VC_OUTLINE = 'outlines\vc%0.3d_gpc.txt';

//==================================
{ TVC_Checker }
//==================================

//--------------------------------------------------------------------------------
// This is the core checking routine
// It checks whether a given grid ref overlaps the given vice-county
// If it doesn't, it adds an error for the given table and key to the FErrors
//--------------------------------------------------------------------------------
function TVC_Checker.Check(infoToCheck: TInfoToCheck): boolean;
var tenkm, onekm: string;
    figs: byte;
    v: byte;
    i: Longint;
begin
  Result := False;
  if (Length(infoToCheck.GridRef) >= 4) then
  begin
    Figs := ExtractSquares(infoToCheck.GridRef, onekm, tenkm);
    // have we already done this check?
    i := CheckAlreadyDone(infoToCheck);
    if  i >= 0 then
      // we have done this check - what was the outcome?
      case FDone[i].outcome of
      chkOK:           Result := True; // grid ref was in vc
      // otherwise record another error for this table/key combination
      chk10kmNotInVC:  FErrors.AddError(infoToCheck, tenkm, chk10kmNotInVC);
      chk1kmNotInVC:   FErrors.AddError(infoToCheck, onekm, chk1kmNotInVC);
      chkGridNotOverlapVC: FErrors.AddError(infoToCheck, infoToCheck.GridRef, chkGridNotOverlapVC);
      end {Case}
    else
      begin
      // now we can do the checks
      if Figs = 1 then
      begin
        // we only have a 10km square
        // check the vc is listed for that 10km
        if CheckSq(tenkm, infoToCheck.VCs, v) then
        begin
          Result := True;
          RecordDone(infoToCheck, chkOK);
        end
        else
        begin
          // that 10km is not in VC
          FErrors.AddError(infoToCheck, tenkm, chk10kmNotInVC);
          RecordDone(infoToCheck, chk10kmNotInVC);
        end
      end
      else
      begin
        // we have at least 1km square precision
        // check the vc is listed for that 1km
        if CheckSq(onekm, infoToCheck.VCs, v) then
        begin
          if (v>1) and (figs>2) then
          begin
            // we have more than one VC overlapping the 1km
            // and better than 1km precision grid ref
            // so we can do a rectangle in polygon check
            if gridInVc(infoToCheck) then
            begin
              Result := True;
              RecordDone(infoToCheck, chkOK);
            end
            else
            begin
              FErrors.AddError(infoToCheck, infoToCheck.GridRef, chkGridNotOverlapVC);
              RecordDone(infoToCheck, chkGridNotOverlapVC);
            end;
          end
          else
          begin
            // we only have one VC so it must be OK
            Result := True;
            RecordDone(infoToCheck, chkOK);
          end
        end
        else
        begin
          // that onekm is not listed for the vc
          FErrors.AddError(infoToCheck, onekm, chk1kmNotInVC);
          RecordDone(infoToCheck, chk1kmNotInVC);
        end
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------
// Extract 10km and 1km squares from the grid ref
// Returns the number of figures of Precision i.e. 10km = 1, 1km = 2, etc
//--------------------------------------------------------------------------------
function TVC_Checker.ExtractSquares(const GridSq: string; var OneK, TenK: string): byte;
var work: string;
begin
  OneK := '';
  Tenk := '';
  // do we have a tetrad?
  if Length(GridSq) = 5 then
    work := Copy(GridSq, 1, 4)
  else
    work := GridSq;
  // get number of figs 10km = 1, 1km = 2, 100m = 3, etc
  Result := (Length(GridSq) - 2) div 2;
  // extract 10km
  Tenk := Copy(GridSq, 1, 3) + Copy(GridSq, Result  + 3, 1);
  // extract 1km
  if Result >=2  then
    OneK := Copy(GridSq, 1, 4) + Copy(GridSq, Result  + 3, 2);
end;

//--------------------------------------------------------------------------------
// Record that we have checked this grid ref/vc combination and what the
// outcome was
//--------------------------------------------------------------------------------
procedure TVC_Checker.RecordDone(infoToCheck: TInfoToCheck; const res: TCheckVCType);
begin
  // check whether we need to extend FDone
  if FDoneCount > High(FDone) then
    SetLength(FDone, High(FDone) + 100);
  // store the details
  with FDone[FDoneCount] do
  begin
    grid := infoToCheck.GridRef;
    VCs := infoToCheck.VCs;
    outcome := res;
  end;
  // increment the count
  inc(FDoneCount);
end;

//--------------------------------------------------------------------------------
// Have we already checked this grid ref/vc combination?
// Returns the index of the check in the FDone array or -1 if it was not found
//--------------------------------------------------------------------------------
function TVC_Checker.CheckAlreadyDone(infoToCheck: TInfoToCheck): Longint;
var i: Longint;
begin
  Result := -1;
  if FDoneCount > 0 then
    for i := 0 to FDoneCount - 1 do
        if (FDone[i].grid = infoToCheck.GridRef) and
        CompareVCLists(InfoToCheck.VCs, FDone[i].VCs) then
        begin
          Result := i;
          Exit;
        end;
end;

//--------------------------------------------------------------------------------
// Compare two VCLists - returns true if they list the same VCs
// but not necessarily in the same order
//--------------------------------------------------------------------------------
function TVC_Checker.CompareVCLists(aList, bList: TVCList): boolean;
var i,j: byte;
    match: boolean;
begin
  Result := False;
  if High(aList) = High(bList) then
  begin
    match := False;
    for i := 0 to High(aList) do
    begin
      match := False;
      for j := 0 to High(bList) do
        if (aList[i] = bList[j]) then
        begin
          match := true;
          break;
        end;
      if not match then
        Exit;
    end;
    Result := match;
  end;
end;

//--------------------------------------------------------------------------------
// Are any of the VCs in aList also in bList?
//--------------------------------------------------------------------------------
function TVC_Checker.VCListInList(aList, bList: TVCList): boolean;
var i,j: byte;
begin
  Result := False;
  for i := 0 to High(aList) do
    for j := 0 to High(bList) do
      if (aList[i] = bList[j]) then
      begin
        Result := True;
        Exit;
      end;
end;

//--------------------------------------------------------------------------------
// Lookup GridSq in the table and check whether vc is one of those listed
//--------------------------------------------------------------------------------
function TVC_Checker.CheckSq(gridSq: string; VCs: TVCList; var nVCs: byte): boolean;
var i: Longint;
begin
  Result := False;
  nVCs := 0;
  i := FindSq(GridSq);
  if i >= 0 then
    if VCListInList(VCs, FSqs[i].VCs) then
    begin
      Result := True;
      nVCs := FSqs[i].nVCs;
      Exit;
    end;
end;

//--------------------------------------------------------------------------------
// Do a grid in VC check
//--------------------------------------------------------------------------------
function TVC_Checker.GridInVc(infoToCheck: TInfoToCheck): boolean;
var i: byte;
begin
  Result := False;
  for i := 0 to High(infoToCheck.VCs) do
  begin
    inc(FGridInPoly);
    LoadVCpolygon(infoToCheck.VCs[i]);
    if FVCs.IsRectInVC(infoToCheck.VCs[i], GridBounds(infoToCheck.GridRef)) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

//--------------------------------------------------------------------------------
// Check whether the polygon for VC is already loaded
// If not - load it. Raises an exception if it cannot find the file
//--------------------------------------------------------------------------------
procedure TVC_Checker.LoadVCpolygon(const vc: byte);
var aFile: string;
begin
  // load the VC polygon if necessary
  if FVCs.VC[vc] = nil then
  begin
    aFile := FAddinPath + INFO_PATH + Format(VC_OUTLINE, [vc]);
    FVCs.AddVC(aFile);
    if FVCs.VC[vc] = nil then
      raise EvcCheck.CreateFmt('Could not read the polygon file %s', [aFile]);
  end;
end;

//--------------------------------------------------------------------------------
// Initialise
//--------------------------------------------------------------------------------
constructor TVC_Checker.Create(const AddinPath: string);
begin
  inherited Create;
  FAddinPath := AddinPath;
  FErrors := TErrors.Create;
  FVCs := TVCPolygons.Create;
  FDoneCount := 0;
  FGRidInPoly := 0;
  // set the initial length of the array to store items we have checked
  SetLength(FDone, 100);
  // get the lookup table
  LoadGridRefLookupTable;
end;

//--------------------------------------------------------------------------------
// Make sure we free the memory we allocated
//--------------------------------------------------------------------------------
destructor TVC_Checker.Destroy;
begin
  FErrors.Free;
  FVCs.Free;
  if FSqs <> nil then
    FSqs := nil;
  if FDone <> nil then
    FDone := nil;
  inherited;
end;

//--------------------------------------------------------------------------------
// Return the number of errors
//--------------------------------------------------------------------------------
function TVC_Checker.Get_ErrorCount: integer;
begin
  Result := FErrors.Count;
end;

//--------------------------------------------------------------------------------
// Write the errors to the given file
//--------------------------------------------------------------------------------
procedure TVC_Checker.WriteErrors(const aFileName: string);
var oFile: TFileStream;
    aLine: string;
    i: Longint;
const Tab = chr(9);
      EoL = Char(13) + char(10);
begin
  if FErrors.Count > 0 then
  begin
    oFile := TFileStream.Create(aFileName, fmCreate or fmOpenWrite);
    try
      aLine := 'Description: OSGB Grid refs checked against Vice-counties on ' +
                DateToStr(now) + EoL;
      oFile.Write(aLine[1], Length(aLine));
      for i := 0 to FErrors.Count - 1 do
      begin
        aLine := TErrorInfo(FErrors[i]).FTable + tab +
                 TErrorInfo(FErrors[i]).FKey + tab;
        case TErrorInfo(FErrors[i]).FErrType of
        chk10kmNotInVC: aLine := aLine + Format('Ten km square %s is not in vice-county %s',
                                                [TErrorInfo(FErrors[i]).FGridChecked, FormatVCList(TErrorInfo(FErrors[i]).FVCs)]);
        chk1kmNotInVC:  aLine := aLine + Format('One km square %s is not in vice-county %s',
                                                [TErrorInfo(FErrors[i]).FGridChecked, FormatVCList(TErrorInfo(FErrors[i]).FVCs)]);
        chkGridNotOverlapVC: aLine := aLine + Format('Grid square %s is not in vice-county %s',
                                                [TErrorInfo(FErrors[i]).FOriginalGrid, FormatVCList(TErrorInfo(FErrors[i]).FVCs)]);
        end; {case}
        aLine := aLine + EoL;
        oFile.Write(aLine[1], Length(aLine));
      end;
    finally
      oFile.Free;
    end;
  end;
end;

//--------------------------------------------------------------------------------
// Get a VCList as a formatted string
//--------------------------------------------------------------------------------
function TVC_Checker.FormatVCList(VCs: TVCLIst): string;
var i, j, n: byte;
begin
  Result := '';
  n := 0;
  for i := 0 to High(VCs) do
    if VCs[i] > 0 then
      inc(n);
  if n>0 then
  begin
    j := 0;
    for i := 0 to High(VCs) do
      if VCs[i] > 0 then
      begin
        inc(j);
        if (j=1) then
          Result := IntToStr(VCs[i])
        else if (j=n) and (n>1) then
          Result := Result + ' or ' + IntToStr(VCs[i])
        else if (j > 1) then
          Result := Result +  ', ' + IntToStr(VCs[i]);
      end;
  end;
end;

//--------------------------------------------------------------------------------
// Return the path and name of the file containing the grid square
// reference information
//--------------------------------------------------------------------------------
function TVC_Checker.Get_InfoFileName: string;
begin
  Result := FAddinPath + INFO_PATH + INFO_FILE;;
end;

//--------------------------------------------------------------------------------
// Load the grid square lookup list
//--------------------------------------------------------------------------------
procedure TVC_Checker.LoadGridRefLookupTable;
var f: TextFile;
    i: Longint;
    sLine, gridsq, lastSq: string;
    Header: boolean;
    oLine: TStrings;
    v: integer;
begin
  if FileExists(InfoFileName) then
  begin
    AssignFile(f, InfoFileName);
    try
      // count the lines in the file
      Reset(f);
      i := 0;
      while not EoF(f) do
      begin
        Readln(f, sLine);
        inc(i);
      end;
      // now we can size the array
      SetLength(FSqs, i);
      // .. and read the data
      // NOTE: we assume that the file is sorted by grid square!
      Reset(f);
      i := 0;
      v := 0;
      lastSq := '';
      Header := true;
      oLine := TStringList.Create;
      try
        while not EoF(f) do
        begin
          // ignore the first line which contains field names
          if Header then
          begin
            readln(f, sLine);
            Header := false;
          end
          else
            begin
              Readln(f, sLine);
              // parse the line
              oLine.CommaText := sLine;
              gridsq   := oLine[0];
              // is it a new grid square
              if gridsq <> lastSq then
              begin
                if lastSq <> '' then
                  inc(i);
                with FSqs[i] do
                begin
                  sq   := oLine[0];
                  x    := StrToInt(oLine[2]);
                  y    := StrToInt(oLine[3]);
                  nVCs := StrToInt(oLine[4]);
                  SetLength(vcs, nVCs);
                  v := 0;
                  vcs[v] := StrToInt(oLine[1]);
                end;
                lastsq  := gridsq;
              end
              else
              // otherwise we have another vc for an existing square
              begin
                inc(v);
                FSqs[i].vcs[v] := StrToInt(oLine[1]);
              end;
            end;
        end;
        // we need to adjust the length of the array to the number
        // of grid squares we actually found
        SetLength(FSqs, i);
      finally
        oLine.Free;
      end;
    finally
      CloseFile(f);
    end;
  end
  else
    raise EVcCheck.CreateFmt('File %s not found!' + #13 + 'You need to reinstall the VCchecker addin.', [InfoFileName]);
end;

//--------------------------------------------------------------------------------
// Find a grid square in the refernece info.
// Binary search assuming the list is sorted alphabetically be grid square name
//--------------------------------------------------------------------------------
Function TVC_Checker.FindSq(const sq: string): Longint;
var s, e, d: Longint;
begin
  s := Low(FSqs);
  e := High(FSqs);
  repeat
    result:= (s + e) shr 1;
    // case insensitive comparison!
    d:= CompareText(sq, FSqs[Result].sq);
    if d = 0 then
      // we found the name
      EXIT
    else
      if d > 0 then
        s:= result + 1
      else
        e:= result - 1;
  until s > e;
  // if this happens, we didn't find the name
  Result:= -1;
end;

//==================================
{ TErrorInfo }
//==================================
constructor TErrorInfo.Create(infoToCheck: TInfoToCheck; const sq: string; const checkType: TCheckVCType);
begin
  inherited Create;
  FTable := infoTocheck.table;
  FKey := infoTocheck.key;
  FOriginalGrid :=  infoTocheck.gridref;
  FGridChecked := sq;
  FErrType := checkType;
  FVCs := infoTocheck.VCs;
end;

//==================================
{ TErrors }
//==================================

//--------------------------------------------------------------------------------
// Add an error to the list
//--------------------------------------------------------------------------------
function TErrors.AddError(infoToCheck: TInfoToCheck; const sq: string; const checkType: TCheckVCType): integer;
var oErr: TErrorInfo;
begin
  oErr := TErrorInfo.Create(infoToCheck, sq, checktype);
  Result := Add(oErr);
end;

//-------------------------------------------------------------------------------
// Make sure we free the objects in the list when it is cleared
//--------------------------------------------------------------------------------
procedure TErrors.Clear;
var i: integer;
begin
  if Count > 0 then
  for i := Count - 1 downto 0 do
    TErrorInfo(Items[i]).Free;
  inherited;
end;

//--------------------------------------------------------------------------------
// Calls clear before freeing to make sure memory used by the objects
// ihj the list is properly released
//--------------------------------------------------------------------------------
destructor TErrors.Destroy;
begin
  Clear;
  inherited;
end;

//==================================
{ TVCPolygon }
//==================================

//------------------------------------------------------------------------------
// Read a VC from a file into a  Pgpc_polygon structure
// We also determine its bounding box
//------------------------------------------------------------------------------
constructor TVCPolygon.Create(const sFileName: string);
var f: TextFile;
    iv: Longint;
    ic: integer;
begin
  inherited Create;
  // Create the Polygon structure
  FPolygon := new(Pgpc_polygon);
  // Read the polygon from a file
  if FileExists(sFileName) then
  begin
    AssignFile(f, sFilename);
    Reset(f);
    gpc_read_polygon(f, FPolygon);
    CloseFile(f);
  end;
  // Get the bounding box
  if FPolygon.num_contours > 0 then
  begin
    FBounds.Xmin := Maxint;
    FBounds.Xmax := -Maxint;
    FBounds.Ymin := Maxint;
    FBounds.Ymax := -Maxint;
    for ic := 0 to FPolygon.num_contours - 1 do
      for iv := 0 to FPolygon.Contour[ic].num_vertices - 1 do
      begin
        if FPolygon.Contour[ic].Vertex[iv].x < FBounds.Xmin then
          FBounds.Xmin := Floor(FPolygon.Contour[ic].Vertex[iv].x);
        if FPolygon.Contour[ic].Vertex[iv].x > FBounds.Xmax then
          FBounds.Xmax := Ceil(FPolygon.Contour[ic].Vertex[iv].x);
        if FPolygon.Contour[ic].Vertex[iv].y < FBounds.Ymin then
          FBounds.Ymin := Floor(FPolygon.Contour[ic].Vertex[iv].y);
        if FPolygon.Contour[ic].Vertex[iv].y > FBounds.Ymax then
          FBounds.Ymax := Ceil(FPolygon.Contour[ic].Vertex[iv].y);
      end;
  end
  else
  begin
    FBounds.Xmin := 0;
    FBounds.Xmax := 0;
    FBounds.Ymin := 0;
    FBounds.Ymax := 0;
  end;
  FName := ExtractFileName(sFileName);
  Fvcn := StrToInt(Copy(FName, 3, 3));
end;

//------------------------------------------------------------------------------
// We need to free the Pgpc_polygon when the object is dropped
//------------------------------------------------------------------------------
destructor TVCPolygon.Destroy;
begin
  if FPolygon.num_contours > 0 then
    gpc_free_polygon(FPolygon);
  Dispose(Fpolygon);
  inherited;
end;

//==================================
{ TVCPolygons }
//==================================

//------------------------------------------------------------------------------
// Load the polygon for a given VC
// Update the overall bounding box to ensure that it includes this VC's
// bounding box
//------------------------------------------------------------------------------
function TVCPolygons.AddVC(const sFileName: string): Integer;
var oVC: TVCPolygon;
begin
  Result := -1;
  oVC := TVCPolygon.Create(sFileName);
  if oVC.Polygon.num_contours > 0 then
  begin
    Result := Add(oVC);
    // update the overall bounding box
    if oVC.Bounds.Xmin < FBounds.Xmin  then
      FBounds.Xmin := oVC.Bounds.Xmin;
    if oVC.Bounds.Xmax > FBounds.Xmax  then
      FBounds.Xmax := oVC.Bounds.Xmax;
    if oVC.Bounds.Ymin < FBounds.Ymin  then
      FBounds.Ymin := oVC.Bounds.Ymin;
    if oVC.Bounds.Ymax > FBounds.Ymax  then
      FBounds.Ymax := oVC.Bounds.Ymax;
  end;
end;

//------------------------------------------------------------------------------
// Drop the VC objects
//------------------------------------------------------------------------------
procedure TVCPolygons.Clear;
var i: integer;
begin
  if Count > 0 then
    For i := Count-1 downto 0 do
      TVCPolygon(Items[i]).Free;
  inherited;
end;

//------------------------------------------------------------------------------
// Create List and initailise the overall bounding box
//------------------------------------------------------------------------------
constructor TVCPolygons.Create;
begin
  inherited;
  FBounds.Xmin := Maxint;
  FBounds.Xmax := -Maxint;
  FBounds.Ymin := Maxint;
  FBounds.Ymax := -Maxint;
end;

//------------------------------------------------------------------------------
// Drop the VC objects by calling Clear before we drop the list
//------------------------------------------------------------------------------
destructor TVCPolygons.Destroy;
begin
  Clear;
  inherited;
end;

//------------------------------------------------------------------------------
// Return a VC's object from the vice-county number
//------------------------------------------------------------------------------
function TVCPolygons.Get_VC(Index: integer): TVCPolygon;
var i: integer;
begin
  Result := nil;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if TVCPolygon(Items[i]).vcn = Index then
      begin
        Result := Items[i];
        Break;
      end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
function TVCPolygons.IsRectInVC(const vcn: integer; bb: TLongRect): boolean;
var oVC: TVCPolygon;
begin
  Result := False;
  oVC := Get_VC(vcn);
  if oVC <> nil then
    Result := not(RectangleInPolygon(bb, oVC.Polygon) = rtOutside);
end;

//------------------------------------------------------------------------------
// Find the VC in which the given point falls. Since VCs are non-overlapping
// there should only be one. Returns the vice-county number.
//------------------------------------------------------------------------------
function TVCPolygons.PointInVC(const x,y: Double): integer;
var i: integer;
begin
  Result := 0;
  if Count > 0 then
    for i := 1 to Count - 1 do
      with TVCPolygon(Items[i]) do
        // first check whether the point is in the bounding box
        if (x >= Bounds.Xmin) and (x <= Bounds.Xmax) and
           (y >= Bounds.Ymin) and (y <= Bounds.Ymax) then
          // then do the point in polygon check
          if PointInPolygon(x, y, Polygon) then
          begin
            Result := TVCPolygon(Items[i]).vcn;
            Exit;
          end;
end;

//------------------------------------------------------------------------------
// Find the VCs that overlap a given grid reference box
//------------------------------------------------------------------------------
function TVCPolygons.VCsHere(const sGrid: string): string;
var bb: TLongRect;
    i: integer;
    res: TRectInPolygonOutcome;
    VCs: TStrings;
begin
  Result := '';
  if Count > 0 then
  begin
    bb := GridBounds(sGrid);
    if bb.Xmax > bb.Xmin then
    begin
      VCs := TStringList.Create;
      try
        for i := 1 to Count - 1 do
          with TVCPolygon(Items[i]) do
          begin
            res := RectangleInPolygon(bb, Polygon);
            if res <> rtOutside then
            begin
              VCs.Add(IntToStr(vcn));
              if res = rtContained then
                Break;
            end;
          end;
        if VCs.Count > 0 then
        begin
          Result := 'VC';
          if VCs.Count > 1 then
            Result := Result + 's here are '
          else
            Result := Result + ' here is ';
          for i := 0 to VCs.Count - 1 do
          begin
            if (VCs.Count - i) = 2 then
              Result := Result + VCs[i] + ' & '
            else if (VCs.Count - i) > 2 then
              Result := Result + VCs[i] + ', '
            else Result := Result + VCs[i];
          end;
        end;
      finally
        VCs.Free;
      end;
    end;
  end;
end;

end.
