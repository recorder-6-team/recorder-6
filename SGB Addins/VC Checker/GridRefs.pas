unit GridRefs;
//==============================================================================
//  Author:          Stuart G. Ball     (stuart.ball@jncc.gov.uk)
//  Date started:    24/09/1999
//  Version:         0.1
//
//------------------------------------------------------------------------------
//  Functions to convert to and from grid refs
//==============================================================================

interface
uses SysUtils;

Type
  {Signals original precision of grid - pNoGrid is used as a flag}
  TPrecisionType = (prNoGrid, pr10km, pr5km, pr2km, pr1km, pr100m, pr10m, pr1m);
  TCoordinateSystemType = (OSGrid, IrishGrid, UTMGrid, LatLong);

  TLongRect = Record
    Xmin, Xmax, Ymin, Ymax: Longint;
  end;


Function IsNumber(const InStr: string): Boolean;
Function StrToGrid(const sGrid: string;
                   var x,y: LongInt;
                   var Precision: TPrecisionType;
                   var CoordSystem: TCoordinateSystemType;
                   var ErrorCode: integer): Boolean;
Function GridToStr(const x,y: LongInt;
                   const Precision: TPrecisionType;
                   const CoordSystem: TCoordinateSystemType): string;
Function GridSquareSize(const p: TPrecisionType): integer;
Function GridSquareType(const p: TPrecisionType): string; overload;
Function GridSquareType(const sGrid: string): string; overload;
Function GridBounds(const sGrid: string): TLongRect;

implementation

//-------------------------------------------------------------------------------
// Test for numeric characters
//-------------------------------------------------------------------------------
Function IsNumber(const InStr: string): Boolean;
{Is InStr a valid number?}
var code: integer;
    n: real;
begin
     try
        Val(InStr,n,code);
        Result:=(code = 0);
     except
        Result:=False;
     end;
end;

//-------------------------------------------------------------------------------
// Convert grid letter into numeric co-ordinates.
// Note that the answer is returned in km at this stage
//-------------------------------------------------------------------------------
procedure GridLetter(const Letter: Char;
                     var x,y: integer);
var l: integer;
begin
    l:=pos(Letter,'ABCDEFGHIJKLMNOPQRSTUVWXYZ')-1;
    {I is not used. NB A=0, B=1 .. I=8}
    If l>7 then dec(l);
    x:=l mod 5;
    y:=4-l div 5;
end;

//-------------------------------------------------------------------------------
// Convert grid reference in string sGrid to coordinates in m.
// Precision returns the type of grid ref supplied.
// Returns an integer Error code
//         0: OK
//         1: Not numeric
//         2: Numeric format incorrect
//         3: Not an even number of digits
//         4: Unknown syntax}
//-------------------------------------------------------------------------------
Function StrToGrid(const sGrid: string;
                   var x,y: LongInt;
                   var Precision: TPrecisionType;
                   var CoordSystem: TCoordinateSystemType;
                   var ErrorCode: integer): Boolean;
var GridLen, i, lx, ly, Len100k: Integer;
    WorkGrid: string;
    pwr: longint;
begin
    {Initialise parameters}
    ErrorCode   := 0;
    x           := -1;
    y           := -1;
    Precision   := prNoGrid;
    CoordSystem := OSGrid;
    Result      := False;

    {Sort out string - Upper case and trim leading or trailing spaces}
    WorkGrid    := Trim(UpperCase(sGrid));
    GridLen     := Length(WorkGrid);

    {---------------------------------------------
    Sort out what sort of grid we are dealing with
    and get the Square letter or number coordinates
    ----------------------------------------------}
    If GridLen > 0 then
    begin
       If WorkGrid[1] in  ['A'..'Z'] then
       begin
          {Alphanumeric grid - NZ1265}
          If WorkGrid[2] in ['A'..'Z'] then
             begin
             {GB grid ref}
             Len100k:=2;
             {500km cordinate from first letter}
             GridLetter(WorkGrid[1],lx,ly);
             x:=(lx-2)*500;
             y:=(ly-1)*500;
             GridLetter(WorkGrid[2],lx,ly);
             {100km coordinate}
             x:=x+lx*100;
             y:=y+ly*100;
             end else
                 begin
                 {Irish Grid ref}
                 CoordSystem := IrishGrid;
                 Len100k:=1;
                 GridLetter(WorkGrid[1],lx,ly);
                 {100km coordinate}
                 x:=lx*100;
                 y:=ly*100;
                 end
       {Note that x,y are in km at this point}
       end else begin
          {Numeric grid - 45/1265}
          If WorkGrid[3] <> '/' then
             begin
             {format incorrect}
             ErrorCode:=2;
             Exit;
             end else
                 begin
                      {Get km coordinates from first 2 digits}
                      x:=StrToInt(WorkGrid[1])*100;
                      y:=StrToInt(WorkGrid[2])*100;
                      Len100k:=3;
                 end;
          end;

        {----------------------------------
        Global part - do this for all grids
        -----------------------------------}
        {put into m}
        x := x*1000;
        y := y*1000;

        {Check format}
        If not IsNumber(Copy(WorkGrid,Len100k+1,GridLen-Len100k)) then
           begin
           {We do not have a numeric string}
           ErrorCode:=1;
           Exit;
        end;
        If odd(GridLen-Len100k) then
           begin
           {we have an odd number of digits}
           ErrorCode:=3;
           Exit
        end;
        GridLen:=(GridLen-Len100k) DIV 2;
        {get multiplier to put in m - depends on number of figures in grid}
        pwr:=1;
        If GridLen <= 4 then
           for i:=GridLen to 4 do
                pwr:=pwr*10;

        {convert figures for numeric part and add to square letter}
        x:=x + StrToInt(Copy(WorkGrid, Len100k+1, GridLen))*pwr;
        y:=y + StrToInt(Copy(WorkGrid, Len100k+GridLen+1, GridLen))*pwr;
   end;

   {set precision}
   case GridLen of
        1:      Precision:=pr10km;
        2:      Precision:=pr1km;
        3:      Precision:=pr100m;
        4:      Precision:=pr10m;
        5:      Precision:=pr1m;
   else         begin  {Cannot handle more digits}
                     ErrorCode := 4;
                     Exit;
                end;
   end;

   {Signal that it worked OK}
   Result := True;
end;

//-------------------------------------------------------------------------------
// Construct grid ref in Alphanumeric format from x,y cordinates
//-------------------------------------------------------------------------------
Function GridToStr(const x,y: LongInt;
                   const Precision: TPrecisionType;
                   const CoordSystem: TCoordinateSystemType): string;
const
   k500 = 500000;
   k100 = 100000;
var
   lx,ly,l,d: integer;
begin
     Result := '';
     If CoordSystem = OSGrid then
     begin
         {First letter for GB grid refs.
         Note the OS grid starts in 100km square SV.
         S is at 2,1 in the 5X5 grid of letters}
         ly:= (y div k500) + 1;
         lx:= (x div k500) + 2;
         l:=(4-ly)*5+lx;
         {I is not used}
         if l>7 then inc(l);
         Result := chr(l+65);
     end;
     If (CoordSystem = OSGrid) or (CoordSystem = IrishGrid) then
     begin
          {Second letter for GB, only letter for Ireland}
          ly:=(y mod k500) div k100;
          lx:=(x mod k500) div k100;
          l:=(4-ly) * 5 + lx;
          {I is not used}
          if l>7 then inc(l);
          Result := Result+chr(l+65);

          {Set up to extract numbers}
          case Precision of
          pr10km:       begin
                        l:=10000; {no m per digit}
                        d:=1;     {number of digits}
                        end;
          pr1km:        begin
                        l:=1000;
                        d:=2;
                        end;
          pr100m:       begin
                        l:=100;
                        d:=3;
                        end;
          pr10m:        begin
                        l:=10;
                        d:=4;
                        end;
          pr1m:         begin
                        l:=1;
                        d:=5;
                        end;
          else          begin
                        Result := '';
                        Exit;
                        end;
          end;{Case}

          {Get the coordinates within the 100km square}
          lx:=((x mod k500) mod k100) div l;
          ly:=((y mod k500) mod k100) div l;

          Result := Result + format('%*.*d',[d,d,lx]) + format('%*.*d',[d,d,ly]);
     end else Result := '';
end;

Function GridSquareSize(const p: TPrecisionType): integer;
begin
  case p of
  pr10km:   Result :=  10000;
  pr5km:    Result :=  5000;
  pr2km:    Result :=  2000;
  pr1km:    Result :=  1000;
  pr100m:   Result :=  100;
  pr10m:    Result :=  10;
  pr1m :    Result :=  1;
  else      Result :=  1;
  end;
end;

Function GridSquareType(const p: TPrecisionType): string;
begin
  case p of
  pr10km:   Result :=  '10km';
  pr5km:    Result :=  '5km';
  pr2km:    Result :=  'Tetrad';
  pr1km:    Result :=  '1km';
  pr100m:   Result :=  '100m';
  pr10m:    Result :=  '10m';
  pr1m :    Result :=  '1m';
  else      Result :=  'Point';
  end;
end;

Function GridSquareType(const sGrid: string): string;
var x,y: LongInt;
    p: TPrecisionType;
    sys: TCoordinateSystemType;
    err: integer;
begin
  Result := 'unknown';
  if StrToGrid(sGrid, x, y, p, sys, err) then
    Result := GridSquareType(p);
end;

Function GridBounds(const sGrid: string): TLongRect;
var x,y,m: LongInt;
    p: TPrecisionType;
    sys: TCoordinateSystemType;
    err: integer;
begin
  Result.Xmin := 0;
  Result.Xmax := 0;
  Result.Ymin := 0;
  Result.Ymax := 0;

  if StrToGrid(sGrid, x, y, p, sys, err) then
    if (sys = OSGrid) then
    begin
      m := GridSquareSize(p);
      Result.Xmin := x;
      Result.Xmax := x + m;
      Result.Ymin := y;
      Result.Ymax := y + m;
    end;
end;

end.
