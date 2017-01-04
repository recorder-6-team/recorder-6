{************************************************************************}
{                        DELPHI 2/3/4  interface                         }
{     for some functions of the 32 bit Dynamic Link Library GeoDLL32     }
{    The interface must be extended for other functions if necessary.    }
{  The interface can be built in into the DELPHI source code directly.   }
{                                                                        }
{               Authors C. Killet Softwareentwicklung GbR                }

{                            and IVU GmbH, Berlin                        }
{                                                                        }
{      In the calling directory of GeoDLL32 the files geodll32.dll,      }
{             geobin32.bin and vo27run.dll must be present.              }
{                                                                        }
{   A lot of changes were accomplished. Unfortunately we did not have    }
{  the possibility to compile the interface. If there are contained any  }

{     syntax errors in the source code, the author asks for report!      }
{************************************************************************}


unit geodll32;

interface

uses Windows, Dialogs;

{ Coordinate systems }
const
GEO = 1;  { geographic coordinates }
GK3 = 2;  { Gauss-Krueger coordinates (3 degree) }
UTM = 3;  { UTM coordinates }
GK6 = 4;  { Gauss-Krueger coordinates (6 degree) }
BMN = 5;  { Transversal Mercator (England) }

{ Reference systems }

DEFAULT     =    0;  { with geographic coordinates: WGS84 }
                     { with Gauss-Krueger coordinates (3 degree): WGS84 }
                     { with UTM coordinates: WGS84 }
                     { with Gauss-Krueger coordinates (6 degree): WGS84 }
                     { with Transversal Mercator (England): OSGB36 }
PD          =    1;  { PD, Rauenberg, Bessel (1841) }
ED50        =    2;  { ED50, Potsdam, Hayford (1909) / Internat. (1924) }
SYSTEM4283  =    3;  { SYSTEM42/83, Pulkowo, Krassowskij (1940) }

ETRS89      =    4;  { ETRS89, earth center d, GRS80 (1980) }
MGI         =    5;  { MGI, Hermannskogel, Bessel (1841) }
NTF         =    6;  { NTF, Pantheon (Paris), Clarke IGN (1880) }
RDNAP       =    7;  { RD/NAP, Amersfoort, Bessel (1841) }
CH1903      =    8;  { CH1903, Old Observatory Bern, Bessel (1841) }
RD83        =    9;  { RD83, Bessel (1841) }
WGS84       =   10;  { WGS84 (GPS), earth center d, WGS84 (1984) }
WGS72       =   11;  { WGS72, earth center d, WGS72 (1972) }

USERDEF     = 1000;  { User defined geodetic datum shift (setuserrefsys) }
KEINS       = 1100;  { No geodetic datum shift }

{ Notation (way of writing) of the geographic coordinates }
GGGNNN = 0;  { ddd.nnn (d = degree, n = minutes and seconds as }
             { decimals of a degree }
GGMMSS = 1;  { dddmmss.nnn (d = degree, m = minutes, s = seconds, }
             { n = decimals of the seconds }

{ Set the language for error messages }
type tGeo_Setlanguage = function

(
 wLanguage: dword {1=German, 2=English}
) : dword; stdcall;
var  setlanguage : tGeo_Setlanguage;

{ Unlock the GeoDLL function groups }
type tGeo_Setunlockcode = function

(
 FCode : pchar;          { Unlock key }
 LNehm : pchar           { Name of the licensee }
) : dword; stdcall;
var  setunlockcode : tGeo_Setunlockcode;

type tGeo_Geterrorcode = function
(
 var pszError: pchar
) : dword; stdcall;
var geterrorcode: tGeo_Geterrorcode;

{ Coordinate transformations }
type tGeo_Coordtrans3 = function
(
 dKoordXQ     : double;  { X coordinate of the source coordinate system }
 dKoordYQ     : double;  { Y coordinate of the source coordinate system }
 sKoordSysQ   : word;    { Flag for the source coordinate system }
 sBezSysQ     : word;    { Flag for the source reference system }

 sNotationQ   : word;    { Flag for the notation of the source coordinates }
 var dKoordXZ : double;  { X coordinate of the target coordinate system }
 var dKoordYZ : double;  { Y coordinate of the target coordinate system }
 sKoordSysZ   : word;    { Flag for the target coordinate system }
 sBezSysZ     : word;    { Flag for the target reference system }
 sNotationZ   : word;    { Flag for the notation of the target coordinates }
 sStreifenZ   : word     { Flag for the target meridian strip }

 ) : dword;  stdcall;
 var coordtrans3 : tGeo_Coordtrans3;


 type tGeo_Coordtrans = function
(
 dKoordXQ     : double;  { X coordinate of the source coordinate system }
 dKoordYQ     : double;  { Y coordinate of the source coordinate system }
 pszCoordQ    : pchar;
 sKoordSysQ   : word;    { Flag for the source coordinate system }
 sBezSysQ     : word;    { Flag for the source reference system }
 var dKoordXZ : double;  { X coordinate of the target coordinate system }
 var dKoordYZ : double;  { Y coordinate of the target coordinate system }
 var pszCoordZ: pchar;
 sKoordSysZ   : word;    { Flag for the target coordinate system }
 sBezSysZ     : word;    { Flag for the target reference system }
 sStreifenZ   : word     { Flag for the target meridian strip }
 ) : dword;  stdcall;
 var coordtrans : tGeo_Coordtrans;

 { Set user defined coord system }
 type tGeo_Setusercoordsys1 = function
 (
 nProjNum : word;
 nPar1    : double;
 nPar2    : double;
 nPar3    : double;
 nPar4    : double;
 nPar5    : double;
 nPar6    : double;
 nPar7    : double;
 nPar8    : double
 ) : dword;  stdcall;
 var setusercoordsys1 : tGeo_Setusercoordsys1;

{ Transformation parameters for a user defined reference system }
{ Function must be used only, if an own reference system is to be defined }
type tGeo_Setuserrefsys = function
(
 sRefSysTyp  : word;    { Flag to set the reference system }
 dTranslX    : double;  { Translation on the X axis }
 dTranslY    : double;  { Translation on the Y axis }
 dTranslZ    : double;  { Translation on the Z axis }
 dRotX       : double;  { Rotation of the X axis }

 dRotY       : double;  { Rotation of the Y axis }
 dRotZ       : double;  { Rotation of the Z axis }
 dMasstab    : double   { Measure factor in ppm }
 ) : dword;  stdcall;
 var setuserrefsys : tGeo_Setuserrefsys;

{ Ellipsoid half axis for a user defined source ellipsoid }
{ Function must be used only, if an own reference system is to be defined }
type tGeo_Setuserellsource = function
(
 sEllSource  : word;    { Flag for the source ellipsoid } 
 dHalbRadGrQ : double;  { Large half axis of the source ellipsoid }

 dHalbRadKlQ : double   { Small half axis of the source ellipsoid }
 ) : dword;  stdcall;
 var setuserellsource : tGeo_Setuserellsource;

{ Ellipsoid half axis for a user defined target ellipsoid }
{ Function must be used only, if an own reference system is to be defined }
type tGeo_Setuserelltarget = function
(
 sEllTarget  : word;    { Flag for the target ellipsoid } 
 dHalbRadGrZ : double;  { Large half axis of the target ellipsoid }
 dHalbRadKlZ : double   { Small half axis of the target ellipsoid }

 ) : dword;  stdcall;
 var setuserelltarget : tGeo_Setuserelltarget;

{ Distance between two coordinate pairs }
type tGeo_Distancegeo = function
(
 dGeoLaenge1 : double;  { Longitude of the first geographic coordinate }
 dGeoBreite1 : double;  { Latitude of the first geographic coordinate }
 dGeoLaenge2 : double;  { Longitude of the second geographic coordinate }
 dGeoBreite2 : double;  { Latitude of the second geographic coordinate }
 var dStrecke: double;  { Calculated distance }

 sEllipsoid  : word     { Flag for the geodetic ellipsoid }
) : dword;  stdcall;
var distancegeo : tGeo_Distancegeo;

function KoordSysCode (S:String):Integer;
function BezSysCode   (S:String):Integer;
procedure LoadLib;

implementation

function KoordSysCode (S:String):Integer;
begin
 KoordSysCode := 1;
 if S = 'GEO' then KoordSysCode := 1;
 if S = 'GK3' then KoordSysCode := 2;
 if S = 'UTM' then KoordSysCode := 3;
 if S = 'GK6' then KoordSysCode := 4;
 if S = 'TME' then KoordSysCode := 5;

end;

function BezSysCode (S:String):Integer;
begin
 BezSysCode := 0;
 if S = 'PD'          then BezSysCode := 1;
 if S = 'ED50'        then BezSysCode := 2;
 if S = 'SYSTEM4283'  then BezSysCode := 3;
 if S = 'ETRS89'      then BezSysCode := 4;
 if S = 'MGI'         then BezSysCode := 5;
 if S = 'NTF'         then BezSysCode := 6;
 if S = 'RDNAP'       then BezSysCode := 7;
 if S = 'CH1903'      then BezSysCode := 8;
 if S = 'RD83'        then BezSysCode := 9;

 if S = 'WGS84'       then BezSysCode := 10;
 if S = 'WGS72'       then BezSysCode := 11;
 if S = 'KEINS'       then BezSysCode := 1100;
end;

function T_Error (
 dKoordXQ     : double;
 dKoordYQ     : double;
 sKoordSysQ   : word;
 sBezSysQ     : word;
 sNotationQ   : word;
 var dKoordXZ : double;
 var dKoordYZ : double;
 sKoordSysZ   : word;
 sBezSysZ     : word;
 sNotationZ   : word;
 sStreifenZ   : word
 ) : dword; stdcall;
begin
 ShowMessage ('GeoDLL32 : Function coordtrans3() not bound ');

 dKoordXZ := 0;
 dKoordYZ := 0;
 T_ERROR  := 0;
end;

function L_Error
(
 FCode : pchar;
 LNehm : pchar
) : dword; stdcall;
begin
 ShowMessage ('GeoDLL32 : Function setlanguage() not bound ');
 L_ERROR  := 0;
end;

function F_Error
(
 FCode : pchar;
 LNehm : pchar
) : dword; stdcall;
begin
 ShowMessage ('GeoDLL32 : Function setunlockcode() not bound ');
 F_ERROR  := 0;
end;

function D_Error
(
 dGeoLaenge1 : double;
 dGeoBreite1 : double;
 dGeoLaenge2 : double;
 dGeoBreite2 : double;
 var dStrecke: double;
 sEllipsoid  : word
) : dword;  stdcall;
begin
 ShowMessage ('GeoDLL32 : Function distancegeogeo() not bound ');

 dStrecke := 0;
 D_Error := 0;
end;

function TP_Error
(
 sRefSysTyp  : word;
 dTranslX    : double;
 dTranslY    : double;
 dTranslZ    : double;
 dRotX       : double;
 dRotY       : double;
 dRotZ       : double;
 dMasstab    : double
) : dword;  stdcall;
begin
 ShowMessage ('GeoDLL32 : Function setuserrefsys() not bound ');
 TP_Error := 0;
end;

function ES_Error
(
 sEllSource  : word;
 dHalbRadGrQ : double;
 dHalbRadKlQ : double
) : dword;  stdcall;

begin
 ShowMessage ('GeoDLL32 : Function getuserellsource() not bound ');
 ES_Error := 0;
end;

function ET_Error
(
 sEllTarget  : word;
 dHalbRadGrZ : double;
 dHalbRadKlZ : double
) : dword;  stdcall;
begin
 ShowMessage ('GeoDLL32 : Function getuserelltarget() not bound ');
 ET_Error := 0;
end;

var LHandle : tHandle;

procedure LoadLib;
begin
{ Load the DLL GeoDLL32 and set pointers to the DLL functions }
   LHandle := LoadLibrary('GeoDLL32.DLL');

   if LHandle <> 0
   then begin
         Setlanguage      := GetProcAddress(LHandle, 'setlanguage');
         Setunlockcode    := GetProcAddress(LHandle, 'setunlockcode');
         Coordtrans3      := GetProcAddress(LHandle, 'coordtrans3');
         Coordtrans       := GetProcAddress(LHandle, 'coordtrans');
         Distancegeo      := GetProcAddress(LHandle, 'distancegeo');
         Setuserrefsys    := GetProcAddress(LHandle, 'setuserrefsys');
         Setuserellsource := GetProcAddress(LHandle, 'setuserellsource');
         Setuserelltarget := GetProcAddress(LHandle, 'setuserelltarget');
         Setusercoordsys1 := GetProcAddress(LHandle, 'setusercoordsys1');
         Geterrorcode     := GetProcAddress(LHandle, 'geterrorcode');
        end

   else begin
         Setlanguage      := @L_ERROR;
         Setunlockcode    := @F_ERROR;
         Coordtrans3      := @T_ERROR;
         Coordtrans       := @T_ERROR;
         Distancegeo      := @D_ERROR;
         Setuserrefsys    := @TP_Error;
         Setuserellsource := @ES_Error;
         Setuserelltarget := @ET_Error;
         Setusercoordsys1 := @ET_Error;
         ShowMessage ('Unable to load GeoDLL32.DLL');
        end;
end;

begin


end.
