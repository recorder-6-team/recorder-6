{===============================================================================
  Unit:        SpatialRefFuncs

  Defines:     TGridSquare
               TLatLong
               TMapCoord
               TPrefix
               TSpatialRefSubFuncs
               TStringCoord
               TutmCoord
               TValidBoundingBox
               TValidSpatialRef

  Description: Global methods to handle spatial references and systems, and
               conversion to and from lat long.

  Last revision information:
    $Revision: 134 $
    $Date: 12/03/09 12:14 $
    $Author: Ericsalmon $

===============================================================================}

unit SpatialRefFuncs;

interface

uses
  Windows, SysUtils, Classes, Math, Dialogs, Recorder2000_TLB,
  StdCtrls, GenFuncs, StrUtils;

const
  { Standard reference systems }
  LAT_LONG = 'LTLN';
  UTM      = 'UTM';
  OS_GB    = 'OSGB';
  OS_NI    = 'OSNI';
  NULL_LATLONG = -1000; // dummy value to represent null lat or long

  { Validation Constants }
  MIN_LAT = -90;

  MAX_LAT  =   90;
  MIN_LONG = -180;
  MAX_LONG =  180;

  MIN_OSGB_EASTING  = 0;
  MIN_OSGB_NORTHING = 0;
  MAX_OSGB_EASTING  = 699999;
  MAX_OSGB_NORTHING = 1299999;

  MIN_OSIG_EASTING  = 0;
  MIN_OSIG_NORTHING = 0;
  MAX_OSIG_EASTING  = 500000;
  MAX_OSIG_NORTHING = 500000;
  MIN_OSIG_PREFIX   = 0;
  MAX_OSIG_PREFIX   = 44;

  MIN_UTM_ZONE     = 0;
  MAX_UTM_ZONE     = 61;
  MIN_UTM_EASTING  = 0;
  MAX_UTM_EASTING  = 999999;
  MIN_UTM_NORTHING = 0;
  MAX_UTM_NORTHING = 9999999;

  LAT_LONG_SIGFIGS          =  5;
  LAT_LONG_SIGFIGS_ACCURATE = 10;

resourcestring
  ResStr_InvalidGridPrecision = 'Invalid grid cell precision %d.';
  ResStr_SystemNotAGridSystem = 'The spatial reference system is not a grid system.';
  ResStr_BeyondSystemLimits   = 'Beyond System Limits.';
  ResStr_LatLongTitle         = 'Latitude and Longitude';
  ResStr_GBTitle              = 'Ordnance Survey - GB Grid';
  ResStr_NITitle              = 'Ordnance Survey - Irish Grid';
  ResStr_SystemUnknown        = 'Grid System Unknown';
  ResStr_UTMTitle             = 'UTM';

  { Error Messages }
  ResStr_InvalidSpRefFormat   = 'The spatial reference is not recognised.';
  ResStr_ConvertStringError   = 'The string passed to this function cannot be converted to an integer';
  ResStr_OutsideSystem        = 'The spatial reference lies beyond the limits of the current spatial reference system';
  ResStr_NoComInterfaces      = 'COM inteface list not initialised.';
  ResStr_NoComSystems         = 'COM systems list not initialised.';
  ResStr_UnrecognisedSystem   = 'The spatial reference system cannot be recognised.';
  ResStr_BoundingBoxInvalid   = 'One or both bounding box corners is an invalid spatial reference : ';
  ResStr_SameSpatialRef       = 'The spatial reference of the sample must be identical to that of the survey event.';
  ResStr_UTMBaseMap           = 'UTM cannot be used as a base map system, please reset your map.';

  ResStr_NoBasemapSystemAddin = 'The base map uses a coordinate system %s ' +
                                ' for which there is no conversion addin available.  Please either '+
                                ' install the appropriate addin, or reset your base map.';

  ResStr_CannotConvertEN      = 'Conversion from Easting and Northing is not possible for this spatial reference system.';
  
  ResStr_SouthWest = 'The SouthWest ';
  ResStr_NorthEast = 'The NorthEast ';
  ResStr_SRSystemLimit = '%s spatial reference is beyond the limits of the current system.';
  ResStr_SameSRSystem = 'Please enter both spatial references in the same spatial reference system.';
  ResStr_SW    = 'SouthWest';
  ResStr_AndNE = ' and NorthEast';
  ResStr_NE    = 'NorthEast';

  ResStr_DirCornerPositions = 'The SouthWest corner is not to the southwest of the NorthEast corner.'#13 +
            		              'Please enter a valid bounding box.';

  ResStr_AddinReduceNotImp = 'Addin Reduce Grid Precision support not implemented yet';

var
  { General Variables }
  LatTrueOrigin : extended;
  LongTrueOrigin : extended;
  nZero : extended;
  eZero : extended;
  eSquared : extended;
  a : extended;
  b : extended;
  n : extended;
  tfUTM : boolean;
  gComMapFormat:IUnknown;

  tfAcceptLargerBoundingBox : boolean;

type
  ESpatialRefError = class(Exception);
  ESpatialRefSystemConversionError = class(ESpatialRefError);
  EOutOfRangeError = class(Exception);

  TGridType = (gtUK_Grid, gtIrish_Grid, gtUTM_Grid);

  TMapCoord = record
    x : double;
    y : double;
  end;

  TPrefix = record
    Numbers : string;
    Letters : string;
  end;

  TLatLong = record
    Lat  : double;
    Long : double;
  end;

  TStringCoord = record
    x : string;
    y : string;
  end;

  TValidSpatialRef = record
    EnteredFormattedSR : string;
    FormattedSR        : string;
    Valid              : boolean;
    Error              : String;
  end;

  TValidBoundingBox = record
    FormattedNorthEastSR : string;
    FormattedSouthWestSR : string;
    Valid                : boolean;
    Error                : String;
  end;

  TutmCoord = record
    northing : integer;
    easting  : integer;
    zone     : integer;
  end;

  TGridSquare = packed record
    BottomLeft: TLatLong;
    BottomRight: TLatLong;
    TopLeft: TLatLong;
    TopRight: TLatLong;
    Precision: Integer;
    System: String;
  end;

  TThreadTerminationCheck = function(): boolean of object;

  { PRIMARY FUNCTIONS

    These are usually called directly by the Source Code and in turn
    call various secondary functions}

function ConvertFromLatLong(iLatLong: TLatLong; const iSystem: string): string;
function ConvertToLatLong(const iSpatialRef, iSystem : string): TLatLong;
function SpatialRefToSpecificEastNorth(
         const ASpatialRef, ASystem, AENSystem: string): TMapCoord;

function EastingNorthingToSpatialRef(AEastingNorthing : TMapCoord; AENSystem,
    ASystem : String): String;

function GetSpatialRefIntf(const iSystem : string): IUnknown;
function IsComSpatialSystem(const iSystem : string): boolean;

function ConvertDummyEastNorthToLatLong(iEastNorth : TMapCoord): TLatLong;
function ConvertLatLongToDummyEastNorth(iLongitude, iLatitude : double): TMapCoord;

function ValidSpatialRef(iSpatialRef : string;
    const iCurrentSystem: string; const iInputSystem : string=''):TValidSpatialRef;
function ValidSpecificSpatialRef(const iSpatialRef, iSpecificSystem : string): TValidSpatialRef;
function TestSpatialRefAgainstSystem(const spatialRef: String; var system: String): Boolean;
function DetermineSpatialRefSystem(iSpatialRef : String): string;
function IsValidOSGBRef(const iSpatialRef : string): boolean;
function IsValidOSNIRef(const iSpatialRef : string): boolean;
function IsValidLTLNRef(const iSpatialRef : string): boolean;
function IsValidUTMRef(const iSpatialRef : string): boolean;
function LatLongToSpecificEN(iLatLong : TLatLong; iSystem : string) : TMapCoord;
function SpecificENToLatLong(iEastNorth : TMapCoord; iSystem : string) : TLatLong;
function ConvertSystems(iSpatialRef, iSystemIn, iSystemOut : string) : string;
function CurrentSpatialRefInterface(const iSystem: string): ISpatialReference;
procedure BuildGridSquareRange(const ASWRef, ANERef, ASystem: string; ARange:
    TStringList; AWantQuotes: boolean; AThreadTerminationCheck: TThreadTerminationCheck=nil);
procedure ReduceGridPrecision(var ARef: string; APrecision: integer;
    const ASystem: string);
procedure CheckIsGridSystem(const ASystem: string);
function GetSpatialRefPrecision(const ASpatialRef, ASystem: String): Integer;
function SpatialSystemMaxCommonSamplingSquareSize(const ASystem: String): Integer;
function SpatialRefToGridSquare(const ASpatialRef, ASystem: String; APrecision: Integer): TGridSquare;
function GridSquareToRegion(square: TGridSquare; const system: String): HRgn;

{ SUB_FUNCTIONS - "ConvertFromLatLong"
  to convert TLatLong strings to Grid References strings }
function LTLNToOSGB(iLatLong: TLatLong): 	string;
function LTLNToOSIG(iLatLong: TLatLong): 	string;
function LTLNToUTM(iLatLong: TLatLong): 	string;
function LTLNtoFormattedLTLN(iLatLong: TLatLong; iSigFigs : integer = -1) : string;
function LTLNtoAccurateFormattedLTLN(iLatLong: TLatLong) : string;

function LatToString(iLat : double; iSigPlaces : integer = 0) : string;
function LongToString(iLong : double; iSigPlaces : integer = 0) : string;
function StringToLat(istLat : string) : double;
function StringToLong(istLong : string) : double;


{ SUB_FUNCTIONS - "ConvertToLatLong"
  to convert Spatial Reference strings to TLatLongs }
function OSGBtoLTLN(const iUKSpatialReference : string): TLatLong;
function OSIGToLTLN(const iIrishSpatialReference : string): TLatLong;
function UTMToLTLN(iUTMSpatialReference : string): TLatLong;
function FormattedLTLNtoLTLN(iLTLNSpatialReference : string) : TLatLong;

{ SUB_FUNCTIONS - "SpatialRefToEastNorth"
 Convert Grid References strings to Easting-Northing }
function OSGBSpatialRefToEN(iSpatialRef : string): TMapCoord;
function OSIGSpatialRefToEN(iSpatialRef : string): TMapCoord;
function UTMSpatialRefToEN(const iutmGridRef : string): TutmCoord;
function TetradToEN(const iTetrad : string): TMapCoord;
function DINTYtoMetres(iDintyChar : char) : TPoint;

{ SUB_FUNCTIONS - "EastingNorthingToSpatialRef"
  Convert Easting-Northing to Spatial ref string }
function EastingNorthingToOSGB(iEastingNorthing : TStringCoord): string;
function EastingNorthingToOSIG(iEastingNorthing : TStringCoord): string;
function EastingNorthingToUTM(iEastingNorthing : TStringCoord): string;

{ SUB_FUNCTIONS - "ValidSpatialRef"
  Validtion of Individual Spatial Ref Strings }
function ValidUKSpatialRef(iSpatialRef :string) : TValidSpatialRef;
function ValidIrishSpatialRef(iSpatialRef :string) : TValidSpatialRef;
function ValidUTMSpatialRef(iSpatialRef :string) : TValidSpatialRef;
function ValidLatLongSpatialRef(iSpatialRef : String) : TValidSpatialRef;
function ValidLatLongRecord(iLatLong : TLatLong) : Boolean;


{ MAIN PROJECTION CONVERSION FUNCTIONS - Not to be called directly }
function EastNorthToLatLong(iEasting, iNorthing : double; iZone:integer): TLatLong;
function LatLongToEastNorth(iLongitude, iLatitude : extended): TMapCoord;
function CalculateMeridian(iLatInRad : extended): extended;
procedure SetGridConstants(const iSystem : TGridType);
function DecodePrefixFromLetter(const iPrefix: string; iSystem : TGridType): string;
function EncodePrefixToLetters(iNumericPrefix: string; iSystem : TGridType): string;

{ Secondary Dataset system CONVERSION FUNCTIONS }
function OSGBMapCoordToLatLong(const iEastingNorthing : TMapCoord) :TLatLong;
function OSIGMapCoordToLatLong(const iEastingNorthing : TMapCoord) : TLatLong;
function LatLongToOSIGEastNorth(const iLatLong: TLatLong): TMapCoord;
function LatLongToOSGBEastNorth(const iLatLong: TLatLong): TMapCoord;

{ Helper functions / procedures }
function PadWithZeros(iString: string; iLength: integer; iTrailing: boolean): string;
function RemoveCommas(iScale: String) : double;
function RemoveSubstrings(iString : string; iSubString : char) : string;
function ValidInteger(iTestString : string) : boolean;
function ValidLetter(iTestString : string) : boolean;
function LatLongFromStr(const iLat, iLong : string) : TLatLong;
function LatLong(const iLat, iLong : double) : TLatLong;
function FormatLLStringAsTLatLong(iLLString : String) : TLatLong;
function FormatTLatLongAsString(const iLatLong : TLatLong) : String;
function Tetrad(const iSpatialRef : string): boolean;

{validation functions}

function CheckBoundingBox(const iSWSpatialRef, iNESpatialRef, iCurrentSystem: String): TValidBoundingBox;
function CheckSpatialRefInBB(const RBL, RTR: TLatLong;
                              const iSpatialRef, iSystem: string): boolean;
function NextGridSquare(const iOldRef: string;
                         const iSize: integer; const iSystem: string): TMapCoord;
function CheckSRefInSRef(const iParentSR, iChildSR,
                          iParentSystem, iChildSystem: string) : boolean;
function CompareEastingNorthings(iCoord1, iCoord2 : Double) : boolean;
procedure ValidateSpatialRefEntry(var iControl : TEdit; const iSystem : string; var iEnteredRef : string);

{ Functions to separate this unit from the project, so it DOES NOT use
  ApplicationSettings directly! }
procedure SetCurrentSpatialRefSystem(const NewValue:string);
function GetCurrentSpatialRefSystem: string;
procedure SetCommAddInLink(iSystems:TStringList; iInterfaces:TInterfaceList);

{ Functions to be used by SpatialRef component to get and set to the database }
function GetDisplaySpatialRef(const iDisplaySystem, iSpatialRef,
         iSpatialRefSystem, iLat, iLong, iFieldString : string) : string;
function DifferentEastingAndNorthing(const iSWSpatialRef, iNESpatialRef : string) : boolean;
function ConvertOutOfRangeToSystemLimit(const iLatLong : TLatLong;
                                         const iSystem : string) : string;

{ Converts the given spatial ref from english (as read from DB) to current locale for display. }
function LocaliseSpatialRef(const ASpatialRef: String): String;
{ Converts the given spatial ref from current locale to english format, ready to be stored in DB. }
function DelocaliseSpatialRef(const ASpatialRef: String): String;

function MapCoord(X, Y: Double): TMapCoord;

//==============================================================================
implementation

uses
  GeneralFunctions;

type
  // class used to make some functions private
  TSpatialRefSubFuncs = class
    class function SpatialRefToEastNorth(const AGridRef, ASystem : string):
                                      TMapCoord;
  end;

var
  { The following are only pointers to the actual objects.
    Don't create or free anything! }
  CurrentSpatialRefSystem : string = '';
  CurrentSpatialSystems : TStringList = nil;
  CurrentSpatialInterfaces : TInterfaceList = nil;


{==============================================================================
    Function  Name: RemoveNonLLSubstrings
            Called: Various
           Purpose: Private Takes a string parameter and a number of flags. The function
                    returns the string, having removed all instances non-integer
                    characters except for "N", "E", "S", "W", "+", "-" and ".".
                    The flags (iRemovePeriod, iRemoveMinus, iRemovePlus) are
                    set to true if these are also to be removed
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function RemoveNonLLSubstrings(istString : string) : string;
var
  liCounter : integer;
  lstWorkingString : string;
begin
  lstWorkingString := ''; // init to empty
  for liCounter := 1 to Length(istString) do begin
    case istString[liCounter] of
      { Don't remove N, E, S, W or 0-9, +, - or . }
      'N', 'E', 'S', 'W', '0'..'9', '+', '-' :
        lstWorkingString := lstWorkingString + istString[liCounter];
    end; // case
    // can't put DecimalSeparator in case as not a constant
    if istString[liCounter] = DecimalSeparator then
      lstWorkingString := lstWorkingString + DecimalSeparator;
  end; // for
  Result := lstWorkingString;
end;


//==============================================================================
procedure SetCommAddInLink(iSystems:TStringList; iInterfaces:TInterfaceList);
begin
  CurrentSpatialSystems    := iSystems;
  CurrentSpatialInterfaces := iInterfaces;
end;  // SetCommAddInLink


//==============================================================================
procedure SetCurrentSpatialRefSystem(const NewValue: string);
begin
  CurrentSpatialRefSystem := NewValue;
end;  // SetCurrentSpatialRefSystem

function GetCurrentSpatialRefSystem : string;
begin
  Result := CurrentSpatialRefSystem;
end;

{==============================================================================
    Function  Name: CurrentSpatialRefInterface
            Called: Many
           Purpose: To return the current Spatial Reference interface
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function CurrentSpatialRefInterface(const iSystem: string): ISpatialReference;
var
  lIndex : integer;
begin
  if CurrentSpatialSystems <> nil then
  begin
    // CaseSensitive set to False on stringlist, so IndexOf will find even if case is different.
    lIndex := CurrentSpatialSystems.IndexOf(iSystem);
    if lIndex <>-1 then // found one
    begin
      if CurrentSpatialInterfaces <> nil then
        Result := CurrentSpatialInterfaces[lIndex] as ISpatialReference;
    end else
      raise ESpatialRefError.Create(ResStr_UnrecognisedSystem);
  end;
end;

{==============================================================================
    Function  Name: ConvertFromLatLong
            Called: Many
           Purpose: Converts a Latitude and Longitude TLatLong (a record of two
                    strings) to a Spatial Reference string for any system
      Dependencies:
      Side Effects: Calls sub-functions LTLNtoUTM, LTLNtoOSGB, LTLNtoOSNI
     Special Notes:
------------------------------------------------------------------------------}
function ConvertFromLatLong(iLatLong: TLatLong; const iSystem: string): string;
var
  lComSystemIntf: ISpatialReference;
begin
  { LAT-LONG }
  If not ValidLatLongRecord(iLatLong) then
    Result := ResStr_OutsideSystem;

  if SameText(iSystem, LAT_LONG) then
    Result := LTLNtoFormattedLTLN(iLatLong)
  else
  { UTM Grid }
  if SameText(iSystem, UTM) then
  begin
    SetGridConstants(gtUTM_Grid);
    Result := LTLNtoUTM(iLatLong);
  end
  else
  { UK Grid }
  if SameText(iSystem, OS_GB) then
  begin
    SetGridConstants(gtUK_Grid);
    Result := LTLNtoOSGB(iLatLong);
  end
  else
  { Irish Grid }
  if SameText(iSystem, OS_NI) then
  begin
    SetGridConstants(gtIrish_Grid);
    Result := LTLNtoOSIG(iLatLong);
  end
  else
  begin { check for a COM system }
    lComSystemIntf := CurrentSpatialRefInterface(iSystem) as iSpatialReference;
    Result := lComSystemIntf.ConvertFromLatLong(FloatToStr(iLatLong.Lat), FloatToStr(iLatLong.Long));
    if Result = '' then
      raise ESpatialRefError.Create(lComSystemIntf.GetLastError);
  end; // checking for com
end;  // ConvertFromLatLong

{==============================================================================
    Function  Name: EastingNorthingToSpatialRef
            Called:
           Purpose: Conversion of an Easting and Northing (TMapCoord) to a Spatial
                    Reference string for any system
      Dependencies:
      Side Effects: Calls sub-functions EastingNorthingToUTM, EastingNorthingToOSGB,
                    EastingNorthingToOSIG
     Special Notes:
------------------------------------------------------------------------------}
function EastingNorthingToSpatialRef(AEastingNorthing : TMapCoord; AENSystem,
    ASystem : String): String;
var
  lLatlong : TLatLong;
begin
  lLatLong:=SpecificENToLatLong(AEastingNorthing, AENSystem);
  { convert our latlong to a display string }
  Result := ConvertFromLatLong(lLatLong, ASystem);
end;

{==============================================================================
    Function  Name: ConvertToLatLong
            Called:
           Purpose: Converts an Spatial Reference string to Latitude and
                    Longitude a TLatLong record
      Dependencies:
      Side Effects: Calls sub-functions UTMtoLTLN, OSIGtoLTLN, OSGBtoLTLN
     Special Notes:
------------------------------------------------------------------------------}
function ConvertToLatLong(const iSpatialRef, iSystem : string): TLatLong;
var
  lComSystemIntf : ISpatialReference;
  lstSpatial : string;
begin
  if SameText(iSystem, LAT_LONG) then
    Result := FormattedLTLNtoLTLN(iSpatialRef)
  else
  { UTM }
  if SameText(iSystem, UTM) then
  begin
    SetGridConstants(gtUTM_Grid);
    Result := UTMToLTLN(iSpatialref);
  end else
  { UK Grid }
  if SameText(iSystem, OS_GB) then
  begin
    SetGridConstants(gtUK_Grid);
    Result := OSGBtoLTLN(iSpatialRef);
  end else
  { Irish Grid }
  if SameText(iSystem, OS_NI) then
  begin
    SetGridConstants(gtIrish_Grid);
    Result := OSIGtoLTLN(iSpatialref);
  end else begin
    lComSystemIntf := CurrentSpatialRefInterface(iSystem) as iSpatialReference;
    lstSpatial     := lComSystemIntf.ConvertToLat(iSpatialRef);
    if lstSpatial = '' then // raise the error message specified by the COM object
      raise ESpatialRefError.Create(lComSystemIntf.GetLastError);
    Result.Lat := StringToLat(lstSpatial);
    lstSpatial := lComSystemIntf.ConvertToLong(iSpatialRef);
    if lstSpatial = '' then // raise the error message specified by the COM object
      raise ESpatialRefError.Create(lComSystemIntf.GetLastError);
    Result.Long := StringToLong(lstSpatial);
  end
end;  // ConvertToLatLong

{-------------------------------------------------------------------------------
  Converts an input spatial reference in the supplied system into an east
  north in a different system
}
function SpatialRefToSpecificEastNorth(const ASpatialRef, ASystem, AENSystem:
    string): TMapCoord;
var
  lLatlong : TLatLong;
begin
  // first convert to a lat long
  lLatlong := ConvertToLatLong(ASpatialRef, ASystem);
  Result := LatLongToSpecificEN(lLatLong, AENSystem);
end;


{ Function returns true if supplied spatial system string represents a COM
    system }
function IsComSpatialSystem(const iSystem : string): boolean;
begin
  Result := (iSystem <> LAT_LONG) and (iSystem <> UTM) and
            (iSystem <> OS_GB) and (iSystem <> OS_NI);
end;


{ Function finds a spatial reference interface of required name.  Note it
    returns IUnknown so you should cast it to the required interface }
function GetSpatialRefIntf(const iSystem : string): IUnknown;
var
  lIndex : integer;
begin
  if CurrentSpatialSystems <> nil then
  begin
    lIndex := CurrentSpatialSystems.IndexOf(iSystem);
    if lIndex <>-1 then // found one
    begin
      if CurrentSpatialInterfaces <> nil then
      begin
        Result := CurrentSpatialInterfaces[lIndex] as IUnknown;
      end
      else
        raise ESpatialRefError.Create(ResStr_NoComInterfaces);
    end
    else
      Raise ESpatialRefError.Create(ResStr_UnrecognisedSystem + ' : ' + iSystem);
  end
  else
    raise ESpatialRefError.Create(ResStr_NoComSystems);
end;



{ This function takes a lat long, and converts it to a dummy easting/northing
    based on the UTM projection.  This allows us to get a measurement in metres
    based on a latlong map }
function ConvertLatLongToDummyEastNorth(iLongitude, iLatitude : double): TMapCoord;
var
  lOldUtm : boolean;
begin
  SetGridConstants(gtUTM_Grid);
  lOldUTM := tfUTM;
  tfUTM := false;
  try
    Result := LatLongToEastNorth(iLongitude, iLatitude);
  finally
    tfUTM := lOldUTM;
  end; // try
end;


{ This function takes a dummy easting/northing, and converts it to a latlong
    based on the UTM projection. }
function ConvertDummyEastNorthToLatLong(iEastNorth : TMapCoord): TLatLong;
var
  lOldUtm : boolean;
begin
  SetGridConstants(gtUTM_Grid);
  lOldUTM := tfUTM;
  tfUTM := false;
  try
    Result := EastNorthToLatLong(iEastNorth.x, iEastNorth.y, 0);
  finally
    tfUTM := lOldUTM;
  end; // try
end;


{-------------------------------------------------------------------------------
  Private method.  Retrieves the COM base map system handling interface.
      If the interface provided does not support base maps, but provides a
      reference to a suitable one that does, then use that instead.
}
function GetComMapFormat(ASystem: string): IBaseMapFormat;
begin
  try
    Result := CurrentSpatialRefInterface(ASystem) as IBaseMapFormat;
  except
    on EIntfCastError do begin
      try
        Result := CurrentSpatialRefInterface(
            (CurrentSpatialRefInterface(ASystem) as ISpatialReference6).
            EquivalentBaseMapSystem) as IBaseMapFormat;
      except
        on EIntfCastError do
          Raise ESpatialRefError.Create
              (Format(ResStr_NoBasemapSystemAddin, [ASystem]));
      end;
    end;
  end;
end;


{==============================================================================
    Function  Name: LatLongToSpecificEN
            Called:
           Purpose: Primary Function which is called connvert from a lat long
                    to a Easting Northing of a the current base map's spatial
                    reference system
                    Change : If LatLong is required system then just returns the
                    latlong without changing it
      Dependencies:
      Side Effects: Calls sub-functions LatLongToOSGBEastNorth,
                                        LatLongToOSIGEastNorth,
     Special Notes: Used because of datset setup being spatial reference system
                    specific.
------------------------------------------------------------------------------}
function LatLongToSpecificEN(iLatLong : TLatLong; iSystem : string) : TMapCoord;
var
  lComMapFormat:IBaseMapFormat;
begin
{ Depending on the base map system, the easting northing will represent a
  different type of spatial reference.  Convert to LatLong, then to a display
  string }
  if SameText(iSystem, LAT_LONG) then begin // just return it as it is
    Result.x := iLatLong.Long;
    Result.y := iLatLong.Lat;
  end else
  if SameText(iSystem, UTM) then
    ESpatialRefError.Create(ResStr_CannotConvertEN)
  else
  if SameText(iSystem, OS_GB) then
    Result := LatLongToOSGBEastNorth(iLatLong)
  else
  if SameText(iSystem, OS_NI) then
    Result := LatLongToOSIGEastNorth(iLatLong)
  else begin {COM System}
    lComMapFormat := GetComMapFormat(iSystem);
    Result.X:=lComMapFormat.LatLongToMapCoordX(iLatLong.Lat,iLatLong.Long);
    Result.Y:=lComMapFormat.LatLongToMapCoordY(iLatLong.Lat,iLatLong.Long);
  end;//if
end;

{==============================================================================
    Function  Name: SpecificENToLatLong
            Called:
           Purpose: Primary Function which is called to convert an
                    Easting/Northing as read by a dataset to a Lat/Long
      Dependencies:
      Side Effects: Calls OSGBMapCoordToLatLong,  OSIGMapCoordToLatLong
     Special Notes: Used because of datset setup being spatial reference system
                    specific.
------------------------------------------------------------------------------}
function SpecificENToLatLong(iEastNorth : TMapCoord; iSystem : string) : TLatLong;
var
  lComMapFormat:IBaseMapFormat;
begin
  if SameText(iSystem, LAT_LONG) then
  begin
    Result.Lat := iEastNorth.Y;
    Result.Long:= iEastNorth.X;
  end // if LatLong
  else
  { UTM }
  if SameText(iSystem, UTM) then
    raise ESpatialRefError.Create(ResStr_CannotConvertEN)
  else
  { UK Grid }
  if SameText(iSystem, OS_GB) then
    Result := OSGBMapCoordToLatLong(iEastNorth)
  else
  { Irish Grid }
  if SameText(iSystem, OS_NI) then
    Result := OSIGMapCoordToLatLong(iEastNorth)
  else
  begin {COM System}
    lComMapFormat := GetComMapFormat(iSystem);
    Result.lat    := StringToLong(lComMapFormat.MapCoordToLat(iEastNorth.x, iEastNorth.y));
    Result.Long   := StringToLong(lComMapFormat.MapCoordToLong(iEastNorth.x, iEastNorth.y));
  end; // COM system
end; //function

{==============================================================================
    Function  Name: ValidSpatialRef
            Called:
           Purpose: - Determines the input system
                    - Converts it to the current system
                    - Checks whether the converted value is a valid spatial ref

                    Also Returns 3 parts
                    (a) a formatted Spatial Reference
                    (b) a validity boolean
                        false if - input system is unknown
                        true if  - the converted spatial reference lies outside
                                   the current display system

                    (c) an error message
                    (d) an enterred spatial reference but in a formatted form

      Dependencies:
      Side Effects: Calls sub-functions ValidLatLongSpatialRef, ValidUTMSpatialRef,
                          and ValidIrishSpatialRef
     Special Notes:
------------------------------------------------------------------------------}
function ValidSpatialRef(iSpatialRef : string;
    const iCurrentSystem: string; const iInputSystem : string=''):TValidSpatialRef;
var
  lInputSys : String;
  lConvertedSpatialRef : string;
  lValidCurrentSR, lValidConvertedSR : TValidSpatialRef;
  lSpatialRefIntf : ISpatialReference;

  //-------------Inline Procedure----------------//
  procedure InvalidateResult;
  begin
    Result.Valid := False;
    Result.FormattedSR := ResStr_BeyondSystemLimits;
    Result.Error := ResStr_UnrecognisedSystem;
  end;
  //-------------Inline Procedure----------------//

begin
  Result.Valid := false;
  { (1) Check for no value... this returns a "True" value. }
  if iSpatialRef = '' then
  begin
    Result.valid       := true;
    Result.FormattedSR := '';
    Result.Error       := '';
    Exit;
  end else
  { (2) Check that the Spatial Ref isn't beyond the limits of the system }
  if SameText(iSpatialRef, ResStr_BeyondSystemLimits) then
  begin
    InvalidateResult;
    Exit;
  end;

  { (3) Can the Input system be recognised as one of the 4 default systems?  If
     a known system supplied, then use that rather than auto determine. }
  if (iInputSystem = '') or (iInputSystem = ResStr_SystemUnknown) then
    lInputSys := DetermineSpatialRefSystem( iSpatialRef )
  else
    lInputSys := iInputSystem;

  if lInputSys <> ResStr_SystemUnknown then begin
    { Get the valid Spatial Reference in the entered system }
    lValidCurrentSR := ValidSpecificSpatialRef(iSpatialRef, lInputSys);
    Result.EnteredFormattedSR := lValidCurrentSR.FormattedSR;
    if not lValidCurrentSR.Valid then begin
      InvalidateResult;
      Exit;   // stop processing if known to be invalid
    end;
    { If not entered in current system, then Convert to current system }
    if (lInputSys <> iCurrentSystem) then begin
      try
        { Convert spatial reference into the current system }
        lConvertedSpatialRef := ConvertSystems(iSpatialRef, lInputSys, iCurrentSystem);
        { If it lies outside the current system (i.e. long lat from outside the
          UK converted into a UK spatial reference) }
        if lConvertedSpatialRef <> ResStr_BeyondSystemLimits then
          lValidConvertedSR := ValidSpecificSpatialRef(lConvertedSpatialRef, iCurrentSystem)
        else
        begin
          { If it lies outside the current system (i.e. long lat from outside the
            UK converted into a UK spatial reference) }
          Result.valid       := False;
          Result.FormattedSR := ResStr_BeyondSystemLimits;
          Result.Error       := ResStr_OutsideSystem;
          Exit;
        end;
      except
        on E:ESpatialRefSystemConversionError do begin
          Result.valid       := False;
          Result.FormattedSR := E.Message;
          Result.Error       := ResStr_OutsideSystem;
          Exit;
        end;
      end;
    end
    else
      lValidConvertedSR := lValidCurrentSR;
  end // if input system is one of the recognised syetems
  else
  begin
    { Check whether the current system is a COM system }
    if iCurrentSystem <> ResStr_SystemUnknown then
      try
        try
          lSpatialRefIntf := CurrentSpatialRefInterface(iCurrentSystem);
        except
          on ESpatialRefError do begin
            InvalidateResult;
            Exit;
          end;
        end; // try

        if not lSpatialRefIntf.ValidSpatialRef(iSpatialRef) then
        begin
          InvalidateResult;
          Exit;
        end
        else
        begin
          Result.Valid              := true;
          Result.EnteredFormattedSR := iSpatialRef;
          Result.FormattedSR        := iSpatialRef;
          exit;
        end;
      except
        on ESpatialRefError do // catch exceptions and return invalid
        begin
          InvalidateResult;
          Exit;
        end;
      end; // try..except
  end; // if Input System has not been recognised

  if lValidConvertedSR.FormattedSR = ResStr_BeyondSystemLimits then
  begin
    InvalidateResult;
    Exit;
  end;
  Result.EnteredFormattedSR := lValidCurrentSR.FormattedSR;
  Result.FormattedSR        := lValidConvertedSR.FormattedSR;
  Result.Valid              := true;
  Result.Error              := lValidConvertedSR.error;
end;  // ValidSpatialRef

{==============================================================================
    Function  Name: ValidSpecificSpatialRef
            Called: ValidSpatialRef
           Purpose: This validity check requires the extra, specific system
                    parameter and validates against only one system.
      Dependencies:
      Side Effects: Calls sub-functions ValidLatLongSpatialRef, ValidUTMSpatialRef,
                    ValidUKSpatialRef and ValidIrishSpatialRef
     Special Notes:
------------------------------------------------------------------------------}
function ValidSpecificSpatialRef(const iSpatialRef, iSpecificSystem : string): TValidSpatialRef;
var
  lSpatialRefIntf : ISpatialReference;
begin
  if SameText(iSpecificSystem, LAT_LONG) then
    Result := ValidLatLongSpatialRef(iSpatialRef)
  else
  { UTM }
  if SameText(iSpecificSystem, UTM) then
    Result := ValidUTMSpatialRef(iSpatialRef)
  else
  { UK Grid }
  if SameText(iSpecificSystem, OS_GB) then
    Result := ValidUKSpatialRef(iSpatialRef)
  else
  { Irish Grid }
  if SameText(iSpecificSystem, OS_NI) then
    Result := ValidIrishSpatialRef(iSpatialRef)
  else
  { COM System }
  begin
    lSpatialRefIntf := CurrentSpatialRefInterface(iSpecificSystem);
    if lSpatialRefIntf.ValidSpatialRef(iSpatialRef) then
    begin
      Result.EnteredFormattedSR := iSpatialRef;
      Result.FormattedSR        := iSpatialRef;
      Result.Valid              := True;
    end else
      Result.EnteredFormattedSR := iSpatialRef;
  end;
end;


// Description : Returns true if the supplied spatial ref is OK under the supplied
//     system
// Created : 24/09/2002
function TestSpatialRefAgainstSystem(const spatialRef: String; var system: string): Boolean;
var
  i: integer;
  tempIntf: ISpatialReference;
begin
  Result := False; // default not found
  { First check against our default reference system }
  if (SameText(system, OS_GB) and IsValidOSGBRef(spatialRef))
     or (SameText(system, OS_NI) and IsValidOSNIRef(spatialRef))
     or (SameText(system, UTM) and IsValidUTMRef(spatialRef))
     or (SameText(system, LAT_LONG) and IsValidLTLNRef(spatialRef)) then
  begin
    system := UpperCase(system);
    Result := True;
  end else
  if not (SameText(system, OS_GB)
          or SameText(system, OS_NI)
          or SameText(system, UTM)
          or SameText(system, LAT_LONG))
     and Assigned(CurrentSpatialInterfaces) then
  begin
    { COM system, so find our interface and test if it will accept the reference }
    for i := 0 to CurrentSpatialInterfaces.Count - 1 do
    begin
      tempIntf := CurrentSpatialInterfaces.items[i] as iSpatialReference;
      if SameText(tempIntf.SpatialRefSystem, system) then
      begin
        if tempIntf.ValidSpatialRef(spatialRef) then
        begin
          system := tempIntf.SpatialRefSystem;
          Result := True;
          Break;  // from loop - no need to check further
        end;
      end;
    end;
  end;
end;

{==============================================================================
    Function  Name: DetermineSpatialRefSystem
            Called: Various
           Purpose: The function queries spatial reference stings in order
                    to assertain which system the string is a spatial reference
                    for.
                    Modification - searches the current spatial system first
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function DetermineSpatialRefSystem(iSpatialRef: String): string;
var
  lSpatialRef : string;
  lTempIntf : ISpatialReference;
  lCount : integer;
begin
  Result := ResStr_SystemUnknown;
  { Check for initial letters, but don't remove spaces within. Additional systems may need them! }
  lSpatialRef := Trim(UpperCase(iSpatialRef));
  if TestSpatialRefAgainstSystem(lSpatialRef, CurrentSpatialRefSystem) then
    Result := CurrentSpatialRefSystem
  else // doesn't match our default system, so try any other
  begin
    if IsValidOSGBRef(lSpatialRef) then
      Result := OS_GB
    else
    if IsValidOSNIRef(lSpatialRef) then
      Result := OS_NI
    else
    if IsValidUTMRef(lSpatialRef) then
      Result := UTM
    else
    if IsValidLTLNRef(lSpatialRef) then
      Result := LAT_LONG
    else
    { Not a standard system, so check for any COM system that accepts it }
    if Assigned(CurrentSpatialInterfaces) then
      for lCount := 0 to CurrentSpatialInterfaces.Count - 1 do
      begin
        lTempIntf := CurrentSpatialInterfaces.Items[lCount] as iSpatialReference;
        if lTempIntf.ValidSpatialRef(iSpatialRef) then
        begin
          Result := lTempIntf.SpatialRefSystem;
          Break;  // from loop - no need to check further
        end;
      end; // iterating through COM iSpatialReference interfaces
  end;
end; // DetermineSpatialRefSystem



{ Return true if the supplied reference can be interpreted as an OSGB reference,
  Supplied reference has had its spaces stripped out }
function IsValidOSGBRef(const iSpatialRef : string): boolean;
begin
  Result := (ValidLetter(Copy(iSpatialRef, 1, 2)) and
             ValidInteger(Copy(iSpatialRef, 3, Length(iSpatialRef))) and
             (Odd(Length(Copy(iSpatialRef, 3, Length(iSpatialRef)))) = false) and
             (Length(iSpatialRef) > 1))
            or
            ((Length(iSpatialRef) = 5) and
             ValidLetter(Copy(iSpatialRef, 1, 2)) and
             ValidInteger(Copy(iSpatialRef, 3, 2)) and
             ValidLetter(Copy(iSpatialRef, 5, 1)));
end;


{ Return true if the supplied reference can be interpreted as an OSNI reference.
  Supplied reference has had its spaces stripped out }
function IsValidOSNIRef(const iSpatialRef : string): boolean;
begin
  Result := (ValidLetter(Copy(iSpatialRef, 1, 1)) and
             ValidInteger(Copy(iSpatialRef, 2, Length(iSpatialRef))) and
             (Odd(Length(Copy(iSpatialRef, 2, Length(iSpatialRef)))) = false) and
             (Length(iSpatialRef) > 0))
            or
            ((Length(iSpatialRef) = 4) and
             ValidLetter(Copy(iSpatialRef, 1, 1)) and
             ValidInteger(Copy(iSpatialRef, 2, 2)) and
             ValidLetter(Copy(iSpatialRef, 4, 1)));
end;


{ Return true if the supplied reference can be interpreted as an UTM reference.
  Supplied reference has had its spaces stripped out }
function IsValidUTMRef(const iSpatialRef : string): boolean;
begin
  Result := ValidInteger(iSpatialRef) and (Length(iSpatialRef) in [13..15]);
end;


{ Return true if the supplied reference can be interpreted as a LTLN reference.
  Supplied reference has had its spaces stripped out }
function IsValidLTLNRef(const iSpatialRef : string): boolean;
var lEWPos, lNSPos    : Integer;
    lValidEW, lValidNS: Boolean;
begin
  // Assume invalid spatial ref.
  lValidEW := False;
  lValidNS := False;

  // Is there a E?
  lEWPos := Pos('E', iSpatialRef);
  // If not, is there a W?
  if lEWPos = 0 then lEWPos := Pos('W', iSpatialRef);
  // If so, is there a digit before the char?
  if lEWPos > 1 then
    lValidEW := iSpatialRef[lEWPos - 1] in ['0'..'9'];

  // Is there a N?
  lNSPos := Pos('N', iSpatialRef);
  // If not, is there a S?
  if lNSPos = 0 then lNSPos := Pos('S', iSpatialRef);
  // If so, is there a digit before the char?
  if lNSPos > 1 then
    lValidNS := iSpatialRef[lNSPos - 1] in ['0'..'9'];

  Result := lValidEW and lValidNS;
end;

{==============================================================================
    Function  Name: LTLNToOSGB
            Called: ConvertFromLatLong
           Purpose: Conversion of a TLatLong string (ie. '52.543661', '2.343253')
                    To a UK Spatial Ref (ie. 'TM 434 665')
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function LTLNToOSGB(iLatLong : TLatLong): string;
var
  lEastNorth : TMapCoord;
  lEastNorthString : TStringCoord;
begin
  { Convert from 'E'/'W' and 'N'/'S' to '+' and '-' }
  lEastNorth := LatLongToEastNorth(iLatLong.Long, iLatLong.Lat);
  lEastNorthString.x := FloatToStr(lEastNorth.x);
  lEastNorthString.y := FloatToStr(lEastNorth.y);
  Result := EastingNorthingToOSGB(lEastNorthString);
end;

{==============================================================================
    Function  Name: LTLNToOSIG
            Called:  ConvertFromLatLong
           Purpose: Conversion of a TLatLong string (ie. '52.543661', '2.343253')
                    To a UK Spatial Ref (ie. 'TM 434 665')
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function LTLNToOSIG(iLatLong: TLatLong): 	string;
var
  lEastNorth : TMapCoord;
  lEastNorthString : TStringCoord;
begin
  { Convert from 'E'/'W' and 'N'/'S' to '+' and '-' }
  { Call the procedure }
  lEastNorth := LatLongToEastNorth(iLatLong.Long, iLatLong.Lat);
  { Convert TMapCoord to a String }
  lEastNorthString.x := FloatToStr(lEastNorth.x);
  lEastNorthString.y := FloatToStr(lEastNorth.y);
  Result := EastingNorthingToOSIG(lEastNorthString);
end;

{==============================================================================
    Function  Name: LTLNToUTM
            Called: ConvertFromLatLong
           Purpose: Conversion of a TLatLong string (ie. '52.543661', '2.343253')
                     To a UTM Spatial Ref (ie. '31 123456 7654321')
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function LTLNToUTM(iLatLong : TLatLong): string;
var
  lEastNorth : TMapCoord;
  lZone : integer;
  lLatLongFloat : TMapCoord;
begin
  { Interpret strings as floats }
  lLatLongFloat.x := iLatLong.Long;
  lLatLongFloat.y := iLatLong.Lat;

  { UTM is a special case in the conversion algorithms and for southern
    latitudes and western longitudes needs a different false origin }
  if (lLatLongFloat.y < 0) then
    nZero := -10000000
  else
    nZero := 0;
  LongTrueOrigin := (trunc(abs(lLatLongFloat.x)/6)*6 +3);
  if lLatLongFloat.x < 0 then
    LongTrueOrigin := -LongTrueOrigin;

  lZone := trunc((lLatLongFloat.x/6)+31);
  lEastNorth := LatLongToEastNorth(lLatLongFloat.x, lLatLongFloat.y);

  { Format as a single string }
  Result := IntToStr(lZone) + ' ' + FloatToStr(lEastNorth.x) + ' '
                                  + FloatToStr(lEastNorth.y);
end;

{==============================================================================
    Function  Name: LTLNtoFormattedLTLN
            Called: ConvertToLatLong
           Purpose: Conversion of a TLatLong (ie. '52.543661', '2.343253')
                    To a LatLong Spatial Ref (ie. '31.11E, 21.22W')
      Dependencies:
      Side Effects:
     Special Notes: iSigFigs defaults to -1, in which case LAT_LONG_SIGFIGS is used
------------------------------------------------------------------------------}
function LTLNtoFormattedLTLN(iLatLong: TLatLong; iSigFigs : integer = -1) : string;
var
  lSigFigs : integer;
begin
  if iSigFigs = -1 then
    lSigFigs := LAT_LONG_SIGFIGS
  else
    lSigFigs := iSigFigs;
  Result := LatToString(iLatLong.Lat, lSigFigs) + ListSeparator + ' ' +
            LongToString(iLatLong.Long, lSigFigs);
end;


//This function is identical to LTLNToFormattedLTLN in SpatialRefFuncs except
//that the spatial references are more accurate.
function LTLNtoAccurateFormattedLTLN(iLatLong: TLatLong) : string;
begin
  Result := LTLNtoFormattedLTLN(iLatLong, LAT_LONG_SIGFIGS_ACCURATE);
end;

{==============================================================================
    Function  Name: OSGBtoLTLN
            Called: ConvertToLatLong
           Purpose: Conversion of a OSGB Spatial Reference string
                    (ie. SV 1234 5678)
                    to a TLatLong record (ie. '52.543661, 2.343253')
      Dependencies:
      Side Effects:
     Special Notes: Primarily a sub-function called by ConvertToLatLong
                    also called by Wizard and GeneralData
------------------------------------------------------------------------------}
function OSGBtoLTLN(const iUKSpatialReference: string): TLatLong;
var
  lFormatedGR : TMapCoord;
begin
  { Convert string to a TMapCoord }
  lFormatedGR := OSGBSpatialRefToEN(iUKSpatialReference);
  { Convert TMapCoord to  Lat Long }
  Result := EastNorthToLatLong(lFormatedGR.x, lFormatedGR.y, 0);
end;

{==============================================================================
    Function  Name: OSIGToLTLN
            Called: ConvertToLatLong
           Purpose: Conversion of an Irish Spatial Reference string
                    (ie. H 1234 5678)
                    to a TLatLong record (ie. '52.543661', '2.343253')
      Dependencies:
      Side Effects:
     Special Notes: Primarily a sub-function called by ConvertToLatLong
                    also called by Wizard and GeneralData
------------------------------------------------------------------------------}
function OSIGToLTLN(const iIrishSpatialReference: string): TLatLong;
var
  lFormatedGR : TMapCoord;
begin
  { Convert string to a TMapCoord }
  lFormatedGR := OSIGSpatialReftoEN(iIrishSpatialReference);
  { Convert TMapCoord to  Lat Long }
  Result := EastNorthToLatLong(lFormatedGR.x, lFormatedGR.y, 0);
end;

{==============================================================================
    Function  Name: UTMToLTLN
            Called: FormattedLTLNtoLTLN
           Purpose: Conversion of a UTM Spatial Reference string
                    (ie. 31 12345 675678)
                    to a TLatLong record (ie. '52.543661', '2.343253')
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function UTMToLTLN(iUTMSpatialReference: string): TLatLong;
var
  lFormatedGR : TutmCoord;
begin
  { Convert string to a TMapCoord }
  lFormatedGR := UTMSpatialRefToEN(iUTMSpatialReference);
  { Convert TMapCoord to  Lat Long }
  Result := EastNorthToLatLong(lFormatedGR.easting, lFormatedGR.northing,
                                   lFormatedGR.zone);
end;

{==============================================================================
    Function  Name: ConvertToLatLong
            Called: FormattedLTLNtoLTLN
           Purpose: Conversion of a LatLong Spatial Reference string
                    to a TLatLong record (ie. '52.543661', '2.343253')
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function FormattedLTLNtoLTLN(iLTLNSpatialReference : string) : TLatLong;
var
  lSpatialRef : string;
  lChar : char;
  lFirstHalf, lSecondHalf : string;
  lCount : integer;

begin
  { Remove unwanted characters }
  lSpatialRef := RemoveNonLLSubstrings(iLTLNSpatialReference);
  { Separate into N/S and E/W }
  For lCount := 1 to Length(lSpatialRef) do
  begin
    lChar := lSpatialRef[lCount];
    If (lChar in ['A'..'Z']) then
    begin
      lFirstHalf := copy(lSpatialRef, 1, lCount);
      lSecondHalf := Copy (lSpatialRef, lCount+1, Length(lSpatialRef));
      break;
    end;
  end;

  { Manipulate so that always North and East
                       always without letters.. using +/- instead }
  Result.Lat  := StringToLat(lFirstHalf);
  Result.Long := StringToLong(lSecondHalf);
end;

{==============================================================================
    Function  Name:
            Called:
           Purpose:
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
//==============================================================================
// OSIG Sub Function for "ConvertSpatialReferenceToEastNorth"

{ Conversion of a Irish Spatial Reference string (ie. H 1234 5678)
  to an Easting and Northing TMapCoord record (ie. 312343, 436543) }

function OSIGSpatialRefToEN(iSpatialRef: string): TMapCoord;
var
  lFirstLetter : string;
  lPrefixAsNumber : string;
  lHalfLength : integer;
  lTempString : TStringCoord;
  lSuffix : String;
  lSpatialRef : String;
begin
  lSpatialRef := StringReplace(iSpatialRef, ' ', '', [rfReplaceAll]);

  { Get the first letter.. if there }
  lFirstLetter := copy(lSpatialRef, 1, 1);
  If not ValidInteger(lFirstLetter) then
  begin
    { Convert Prefix }
    lPrefixAsNumber := DecodePrefixFromLetter(lFirstLetter, gtIrish_Grid);
    { Seperate the suffix }
    lSuffix := Copy(lSpatialRef, 2, (length(lSpatialRef)));
  end;

  If not Tetrad(lSpatialRef) then
  begin
    { Find out how much precision there is in the Grid Ref }
    lHalfLength := (Length(lSuffix)) div 2;

    lTempString.x := copy(lSuffix, 1 , lHalfLength);
    lTempString.y := copy(lSuffix, lHalfLength + 1, lHalfLength + 2);

    { Add the extra zeros to the suffix make a 6 figure E / N }
    lTempString.x := PadWithZeros(lTempString.x, 5, True);
    lTempString.y := PadWithZeros(lTempString.y, 5, True);

    { Split up and add the individual easting and northing }
    lTempString.x := copy(lPrefixAsNumber, 1, 1) + lTempString.x;
    lTempString.y := copy(lPrefixAsNumber, 2, Length(lPrefixAsNumber))
                     + lTempString.y;

    { Add some zeros }
    lTempString.x := PadWithZeros(lTempString.x, 6, True);
    lTempString.y := PadWithZeros(lTempString.y, 6, True);

    Result.x := StrToInt(lTempString.x);
    Result.y := StrToInt(lTempString.y);
  end
  else
    Result := TetradToEN(lSpatialRef);
end;

{==============================================================================
    Function  Name:
            Called:
           Purpose:
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
//==============================================================================
// OSGB Sub Function for "ConvertSpatialReferenceToEastNorth"

{ Conversion of a UK Spatial Reference string (ie. SV 1234 5678)
  to an Easting and Northing TMapCoord record (ie. 312343, 436543) }

function OSGBSpatialRefToEN(iSpatialRef: string): TMapCoord;
var
  lSuffix : string;
  lFirstLetters : string;
  lPrefixAsNumber : string;
  lHalfLength : integer;
  lTempString : TStringCoord;
  lSpatialRef : String;
begin
  { Remove any spaces }
  lSpatialRef := StringReplace(iSpatialRef , ' ', '', [rfReplaceAll]);

  { Decode the prefix }
  lFirstLetters := copy(lSpatialRef, 1, 2);
  lPrefixAsNumber := DecodePrefixfromLetter(lFirstLetters, gtUK_Grid);
  lSuffix := Copy(lSpatialRef, 3, (length(lSpatialRef)) -2);

  If not Tetrad(lSpatialRef) then
  begin
    { Find out how much precision there is in the Grid Ref }
    lHalfLength := (Length ( lSuffix)) div 2;

    lTempString.x := copy(lSuffix, 1 , lHalfLength);
    lTempString.y := copy(lSuffix, lHalfLength + 1, lHalfLength + 2);

    { Add the extra zeros to the suffix to make a 6 figure suffix }
    lTempString.x := PadWithZeros(lTempString.x, 5, true);
    lTempString.y := PadWithZeros(lTempString.y, 5, true);

    { Split up and add the individual easting and northing }
    lTempString.x := copy(lPrefixAsNumber, 1, 1) + lTempString.x;
    lTempString.y := copy(lPrefixAsNumber, 2, Length(lPrefixAsNumber))
                           + lTempString.y;

    { Add some zeros }
    lTempString.x := PadWithZeros(lTempString.x, 6, True);
    lTempString.y := PadWithZeros(lTempString.y, 6, True);

    Result.x := StrToInt(lTempString.x);
    Result.y := StrToInt(lTempString.y);
  end // if not a Tetrad
  else
    Result := TetradToEN(lSpatialRef);
end;

{==============================================================================
    Function  Name:
            Called:
           Purpose:
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
//==============================================================================
// UTM Sub Function for "ConvertSpatialReferenceToEastNorth"

{ Conversion of a Irish Spatial Reference string (ie. 31 12343 335678)
  to an Easting and Northing TMapCoord record (ie. 312343, 436543) }

function UTMSpatialRefToEN(const iutmGridRef : string): TutmCoord;
var
  lZoneStr : string;
  lSpace : integer;
  lLength : integer;
  lEastingStr : string;
  lNorthingstr : string;
  lutmSpatialRef : String;
begin
  { Split the Zone Off}
  lutmSpatialRef := iutmGridRef;
  lSpace         := Pos(' ', lutmSpatialRef);
  lZoneStr       := Copy(lutmSpatialRef ,1,lSpace-1);
  lutmSpatialRef := Copy(lutmSpatialRef, lSpace + 1, Length(lutmSpatialRef));

  { Split off Easting }
  lSpace       := Pos(' ', lutmSpatialRef);
  lLength      := Length(lutmSpatialRef);
  lEastingStr  := Copy(lutmSpatialRef, 1,lSpace - 1);
  lNorthingStr := Copy(lutmSpatialRef, lSpace + 1,lLength);

  Result.easting  := StrToInt(lEastingStr);
  Result.northing := StrToInt(lNorthingStr);
  Result.zone     := StrToInt(lZoneStr);
end;

{==============================================================================
    Procedure Name: TetradSuffixToEN
            Called:
           Purpose: When a Tetrad is Passed in, this function will convert it
                    to sn easting and northing.
      Dependencies:
     Special Notes:
------------------------------------------------------------------------------}
function TetradToEN(const iTetrad : string): TMapCoord;
var
  lPrefixAsNumber : string;
  lTetrad : string;
  lSyst : string;
  lDecodedDINTY : TPoint;
  lStringResult : TStringCoord;
begin
  { Remove any spaces }
  lTetrad := StringReplace(iTetrad , ' ', '', [rfReplaceAll]);
  { Determine the 100km characters for the easting and northing }
  lSyst := DetermineSpatialRefSystem(lTetrad);
  if lSyst = OS_GB then
    lPrefixAsNumber := DecodePrefixfromLetter(Copy(lTetrad, 1, 2), gtUK_Grid)
  else
  if lSyst = OS_NI then
    lPrefixAsNumber := DecodePrefixfromLetter(Copy(lTetrad, 1, 2), gtIrish_Grid);

  { The x-coordinate for 100s of km is only ever one character however the
    y-coordinate can be 2 characters (because of the shape of the UK - only
    800km wide but 1300km north-south).  The following lines thus split the
    decoded prefix appropriately }
  lStringResult.x := copy(lPrefixAsNumber, 1, 1);
  lStringResult.y := copy(lPrefixAsNumber, 2, length(lPrefixAsNumber) -1);

  { Add the numerical part of the Tetrad }
  lStringResult.x := lStringResult.x + copy(lTetrad, Length(lTetrad)-2, 1);
  lStringResult.y := lStringResult.y + copy(lTetrad, Length(lTetrad)-1, 1);

  { Multiply each part by 2000 to give the northing / easting of the bottom
    left corner of the 2km box in metres }
  lDecodedDINTY := DINTYToMetres(lTetrad[length(lTetrad)]);
  lStringResult.x := lStringResult.x +
                     PadWithZeros(IntToStr(lDecodedDINTY.X), 4, true) ;
  lStringResult.y := lStringResult.y +
                     PadWithZeros(IntToStr(lDecodedDINTY.Y), 4, true);

  Result.x := StrToInt(lStringResult.x);
  Result.y := StrToInt(lStringResult.y);
end;



//==============================================================================
{ Function to decode a tetrad letter into the number of metres this represents
    from the origin of the aquare.  DINTY grid used as follows:
        8 | E J P U Z
        6 | D I N T Y
        4 | C H M S X
        2 | B G L R W
        0 | A F K Q V
           ----------
           0 2 4 6 8
 }
function DINTYtoMetres(iDintyChar : char) : TPoint;
var
  lCharIndex : integer; // a = 0, z=25
  lUpperChr : char;
begin
  lUpperChr := Uppercase(iDintyChar)[1];   // 1 char only
  { character O is ignored so shift subsequent chars back one }
  if lUpperChr >= 'O' then
    lCharIndex := Ord(lUpperChr) - Ord('A') - 1
  else
    lCharIndex := Ord(lUpperChr) - Ord('A');
  // tetrad squares are 2km
  Result.x := (lCharIndex div 5) * 2000;
  Result.Y := (lCharIndex mod 5) * 2000;
end;



{==============================================================================
    Function  Name: EastingNorthingToOSGB
            Called: EastingNorthingToSpatialRef
           Purpose: Conversion of a UK Grid EAsting and Northing record
                    (ie. 312343, 436543) to a Spatial Reference string
                    (ie. SV 1234 5678)
      Dependencies:
      Side Effects: Calls EncodePrefixToLetters,  PadWithZeros
     Special Notes:
------------------------------------------------------------------------------}
function EastingNorthingToOSGB(iEastingNorthing : TStringCoord): string;
var
  ltfRangeError : boolean;
  lPrefixStr : String;
  lLength : TPoint;
  lSuffix : TStringCoord;
  lEastNorth : TStringCoord;
  lNumFigs : integer;
begin
  ltfRangeError := false;
  { Check for valid eastings and northings - convert to limits if exceeded}
  if (StrToFloat(iEastingNorthing.x) < MIN_OSGB_EASTING) then
  begin
    iEastingNorthing.x := IntToStr(MIN_OSGB_EASTING);
    ltfRangeError := true;
  end;
  if (StrToFloat(iEastingNorthing.x) > MAX_OSGB_EASTING) then
  begin
    iEastingNorthing.x := IntToStr(MAX_OSGB_EASTING);
    ltfRangeError := true;
  end;
  if (StrToFloat(iEastingNorthing.y) < MIN_OSGB_NORTHING) then
  begin
    iEastingNorthing.y := IntToStr(MIN_OSGB_NORTHING);
    ltfRangeError := true;
  end;
  if (StrToFloat(iEastingNorthing.y) > MAX_OSGB_NORTHING) then
  begin
    iEastingNorthing.y := IntToStr(MAX_OSGB_NORTHING);
    ltfRangeError := true;
  end;
  lLength.x := length(iEastingNorthing.x);
  lLength.y := length(iEastingNorthing.y);

  If lLength.x > lLength.y then
    lNumFigs := lLength.x
  else
    lNumFigs := lLength.y;

  { In case of strings smaller than 6 characters }
  if lNumFigs < 6 then
    lNumFigs := 6;

  { Pad with zeros to make both parts lNumFigs characters long }
  lEastNorth.x := PadWithZeros(iEastingNorthing.x, lNumFigs, False);
  lEastNorth.y := PadWithZeros(iEastingNorthing.y, lNumFigs, False);

  { Seperate the prefix by removing the last 5 numbers of each half
    which correspond to the non-prefix component }
  lPrefixStr := copy(lEastNorth.x, 1, lNumFigs - 5) +
                copy(lEastNorth.y, 1, lNumFigs - 5);

  { Find the appropriate prefix letters }
  lPrefixStr := EncodePrefixToLetters(lPrefixStr, gtUK_Grid);

  { Format the rest of the string }
  lSuffix.x := copy(lEastNorth.x, lNumFigs - 4, lNumFigs);
  lSuffix.y := copy(lEastNorth.y, lNumFigs - 4, lNumFigs);

  If (not ltfRangeError) or (tfAcceptLargerBoundingBox) then
    Result := lPrefixStr + lSuffix.x + lSuffix.y

  Else If ltfRangeError then
    Result := ResStr_BeyondSystemLimits;
end;

{==============================================================================
    Function  Name: EastingNorthingToOSIG
            Called: EastingNorthingToSpatialRef
           Purpose: Conversion of a Irish Grid EAsting and Northing record
                    (ie. 312343, 436543) to a Spatial Reference string
                    (ie. H 1234 5678)
      Dependencies:
      Side Effects: Calls EncodePrefixToLetters,  PadWithZeros
     Special Notes:
------------------------------------------------------------------------------}
function EastingNorthingToOSIG(iEastingNorthing : TStringCoord): string;
var
  lPrefixStr : String;
  lLength : TPoint;
  lEastNorth : TStringCoord;
  lSuffix : TStringCoord;
  lNumFigs : integer;
begin
  { Check for valid eastings and northings }
  if (StrToFloat(iEastingNorthing.x) < MIN_OSIG_EASTING) or
     (StrToFloat(iEastingNorthing.x) > MAX_OSIG_EASTING) or
     (StrToFloat(iEastingNorthing.y) < MIN_OSIG_NORTHING) or
     (StrToFloat(iEastingNorthing.y) > MAX_OSIG_NORTHING) then
  begin
    Result := ResStr_BeyondSystemLimits;
    exit;
  end;
  lLength.x := length(iEastingNorthing.x);
  lLength.y := length(iEastingNorthing.y);

  If lLength.x > lLength.y then
    lNumFigs := lLength.x
  else
    lNumFigs := lLength.y;

  { In case of strings smaller than 6 characters }
  if lNumFigs < 6 then
    lNumFigs := 6;

  { Pad with zeros to make both parts lNumFigs characters long }
  lEastNorth.x := PadWithZeros(iEastingNorthing.x, lNumFigs, False);
  lEastNorth.y := PadWithZeros(iEastingNorthing.y, lNumFigs, False);

  { Seperate the prefix by removing the last 5 numbers of each half
    which correspond to the non-prefix component }
  lPrefixStr := copy(lEastNorth.x, 1, lNumFigs - 5) +
                copy(lEastNorth.y, 1, lNumFigs - 5);

  { Find the appropriate prefix letters }
  lPrefixStr := EncodePrefixToLetters(lPrefixStr, gtIrish_Grid);

  { Format the rest of the string }
  lSuffix.x := copy(lEastNorth.x, lNumFigs - 4, lNumFigs);
  lSuffix.y := copy(lEastNorth.y, lNumFigs - 4, lNumFigs);

  Result := lPrefixStr + lSuffix.x + lSuffix.y;
end;

{==============================================================================
    Function  Name: EastingNorthingToUTM
            Called: EastingNorthingToSpatialRef
           Purpose: Conversion of a UTM Grid Easting and Northing record
                    (ie. 312343, 436543) to a Spatial Reference string
                    (ie. 31 451234 545678)
      Dependencies:
      Side Effects: Calls EncodePrefixToLetters,  PadWithZeros
     Special Notes:
------------------------------------------------------------------------------}
function EastingNorthingToUTM(iEastingNorthing : TStringCoord): string;
var
  lPrefixStr : String;
  lLength : TPoint;
  lSuffix : TStringCoord;
begin
  { Check for valid eastings and northings }
  if (StrToInt(iEastingNorthing.x) < MIN_UTM_EASTING) or
     (StrToInt(iEastingNorthing.x) > MAX_UTM_EASTING) or
     (StrToInt(iEastingNorthing.y) < MIN_UTM_NORTHING) or
     (StrToInt(iEastingNorthing.y) > MAX_UTM_NORTHING) then
  begin
    Result := ResStr_BeyondSystemLimits;
    exit;
  end;

  lLength.x := length(iEastingNorthing.x);
  lLength.y := length(iEastingNorthing.y);

  lPrefixStr := copy(iEastingNorthing.x, 1, 1) +
                copy(iEastingNorthing.y, 1, 1);

  lSuffix.x := copy(iEastingNorthing.x, 2, lLength.x);
  lSuffix.y := copy(iEastingNorthing.y, 2, lLength.y);

  { Add zeros to the end of the string if necessary }
  lSuffix.x := PadwithZeros(lSuffix.x, 6, true);
  lSuffix.x := PadwithZeros(lSuffix.x, 7, true);

  Result := lPrefixStr + ' ' + lSuffix.x + ' ' + lSuffix.y;
end;

{==============================================================================
    Function  Name: ValidUKSpatialRef
            Called: ValidSpecificSpatialRef.
           Purpose: A spatial reference string is passed in and the spacess are
                    removed using the RemoveSubstrings function.  The Odd
                    function checks that the string an even number of characters
                    and this is split into the prefix, an easting and a northing.

                    The validity of the prefix of the easting and northing are
                    checked. Finally, the decoded prefix is combined with the
                    easting and northing to find the full easting and northings,
                    which must lie within limits set by constants.

                    If all is valid, the string is reconstructed with spaces
                    added for formatting and returned as the FormattedSR part
                    of the TValidSpatialRef. Otherwise, the error part is
                    populated with exception messages produced during the
                    validation.
      Dependencies:
      Side Effects: Calls RemoveSubstrings, DecodeFromLetters, ValidInteger,
                    OSGBSpatialRefToEN
     Special Notes:
------------------------------------------------------------------------------}
function ValidUKSpatialRef(iSpatialRef :string) : TValidSpatialRef;
var
  lPrefixStr : string;
  lPrefixNum : string;
  lEastNorth : TMapCoord;
  lSpatialRef : String;
  lSuffixStr : string;
begin
  lSpatialRef := StringReplace(iSpatialRef, ' ', '', [rfReplaceAll]);

  { UK - always an even number of characters }
  if Odd(Length(lSpatialRef)) then
  { Check for a tetrad }
  begin
    if ((Length(lSpatialRef) = 5) and
        ValidLetter(Copy(lSpatialRef, 1, 2)) and
        ValidInteger(Copy(lSpatialRef, 3, 2)) and
        ValidLetter(Copy(lSpatialRef, 5, 1))) then
    begin
      try
        lPrefixStr := Copy(lSpatialRef, 1, 2);
        lPrefixNum := DecodePrefixFromLetter(lPrefixStr, gtUK_Grid);
      except
        on ESpatialRefError do
        begin
          Result.Valid := False;
          Result.FormattedSR := ResStr_BeyondSystemLimits;
          Result.Error := ResStr_OutsideSystem;
          Exit;
        end;
      end; // try..except
      { TETRAD }
      Result.FormattedSR := UpperCase(lSpatialRef);
      Result.Valid := True;
      Result.Error :='';
      Exit;
    end;
  end
  else
  begin
    { Get the Prefix }
    { Is it preceeded by a letter? }
    lPrefixStr := Copy(lSpatialRef, 1, 2);
    lSuffixStr := Copy(lSpatialRef, 3, Length(lSpatialRef));

    If ValidInteger(lPrefixStr) then
    begin
      Result.Valid := False;
      Result.FormattedSR := ResStr_BeyondSystemLimits;
      Result.Error := ResStr_InvalidSpRefFormat;
      Exit;
    end
    else

    begin
      try
        lPrefixNum := DecodePrefixFromLetter(lPrefixStr, gtUK_Grid);
      except
        on ESpatialRefError do
        begin
          Result.Valid := False;
          Result.FormattedSR := ResStr_BeyondSystemLimits;
          Result.Error := ResStr_OutsideSystem;
          Exit;
        end;
      end; // try..except
    end;

    { Convert to Easting and Northing }
    lEastNorth := OSGBSpatialRefToEN(lSpatialRef);
    If (lEastNorth.x < MIN_OSGB_EASTING) or
       (lEastNorth.x > MAX_OSGB_EASTING) or
       (lEastNorth.y < MIN_OSGB_NORTHING) or
       (lEastNorth.x > MAX_OSGB_NORTHING) then
    begin
      Result.Valid := False;
      Result.FormattedSR := ResStr_BeyondSystemLimits;
      Result.Error := ResStr_BeyondSystemLimits;
      Exit;
    end;
    Result.Valid := True;
    if Length(lSuffixStr) >1 then
      Result.FormattedSR := UpperCase(lPrefixStr) +
             copy(lSuffixStr, 1, (Length(lSuffixStr)) div 2) +
             copy(lSuffixStr, ((Length(lSuffixStr) div 2) + 1), Length(lSuffixStr))
    else
      Result.FormattedSR := UpperCase(lPrefixStr);
    Result.Error := '';
  end;
end;

{==============================================================================
    Procedure Name: ValidIrishSpatialRef
            Called: ValidSpatialRef, VAlidSpecificSpatialRef
           Purpose: Checks that a spatial reference is valid for the irish
                    system by converting to an easting and northing by calling
                    the OSIGSpatialRefToEN function and checking the limits.
                    Checks also, if a tetrad, that this is ok
      Dependencies:
      Side Effects: Calls RemoveSubstrings, DecodeFromLetters, ValidInteger,
                    OSIGSpatialRefToEN
------------------------------------------------------------------------------}
function ValidIrishSpatialRef(iSpatialRef :string) : TValidSpatialRef;
var
  lPrefixStr : string;
  lSuffixStr : string;
  lPrefixNum : string;
  lEastNorth : TMapCoord;
  lSpatialRef : String;
begin
  lSpatialRef := StringReplace(iSpatialRef, ' ', '', [rfReplaceAll]);
  if not Odd(Length(lSpatialRef)) then
  { Check to see if it is a Tetrad }
  begin
    If ((Length(lSpatialRef) = 4) and
           ValidLetter(Copy(lSpatialRef, 1, 1)) and
           ValidInteger(Copy(lSpatialRef, 2, 2)) and
           ValidLetter(Copy(lSpatialRef, 4, 1))) then
    begin
      { If it looks like a Tetrad, check the initial grid square letter }
      try
        lPrefixStr := Copy(lSpatialRef, 1, 2);
        lPrefixNum := DecodePrefixFromLetter(lPrefixStr, gtIrish_Grid);
      except
        on ESpatialRefError do
        begin
          Result.Valid := False;
          Result.FormattedSR := ResStr_BeyondSystemLimits;
          Result.Error := ResStr_OutsideSystem;
          Exit;
        end;
      end; // try..except
      Result.FormattedSR := lSpatialRef;
      Result.Valid := True;
      Result.Error :='';
      Exit;
    end // if it looks like a tetrad
    else
    begin // if it is odd
      Result.Valid := False;
      Result.FormattedSR := ResStr_BeyondSystemLimits;
      Result.Error := ResStr_InvalidSpRefFormat;
      Exit;
    end;
  end; // if it is not an odd number of characters
  { Is it preceeded by a letter? }
  lPrefixStr := Copy(lSpatialRef, 1, 1);
  lSuffixStr := Copy(lSpatialRef, 2, Length(lSpatialRef));

  If ValidInteger(lPrefixStr) then
  begin
    Result.Valid := False;
    Result.FormattedSR := ResStr_BeyondSystemLimits;
    Result.Error := ResStr_InvalidSpRefFormat;
    Exit;
  end
  else
  begin
    try
      lPrefixNum := DecodePrefixFromLetter(lPrefixStr, gtIrish_Grid);
    except
      on ESpatialRefError do
      begin
        Result.Valid := False;
        Result.FormattedSR := ResStr_BeyondSystemLimits;
        Result.Error := ResStr_OutsideSystem;
        Exit;
      end;
    end; // try..except
  end;

  { Convert to Easting and Northing to see if it lays within bounds }
  lEastNorth := OSIGSpatialRefToEN(iSPatialRef);

  { Check the prefix lies within the range }
  if (StrToInt(lPrefixNum) < MIN_OSIG_PREFIX) or
     (StrToInt(lPrefixNum) > MAX_OSIG_PREFIX) or
     (lEastNorth.x < MIN_OSIG_EASTING) or
     (lEastNorth.x > MAX_OSIG_EASTING) or
     (lEastNorth.y < MIN_OSIG_NORTHING) or
     (lEastNorth.x > MAX_OSIG_NORTHING) then
  begin
    Result.Valid := False;
    Result.FormattedSR := ResStr_BeyondSystemLimits;
    Result.Error := ResStr_OutsideSystem;
    Exit;
  end;
  Result.Valid := True;
  if Length(lSuffixStr) >1 then
    Result.FormattedSR := UpperCase(lPrefixStr) +
           copy(lSuffixStr, 1, (Length(lSuffixStr) div 2)) +
           copy(lSuffixStr, ((Length(lSuffixStr) div 2) + 1), Length(lSuffixStr))
  else
    Result.FormattedSR := Uppercase(lPrefixStr);
  Result.Error := '';
end;

{==============================================================================
    Function  Name: ValidUTMSpatialRef
            Called: ValidSpatialRef, ValidSpecificSpatialRef
           Purpose: The passed in spatial reference parameter has spaces removed
                     and the Result is checked to ensure that it is composed
                     only of integers.
                     The spatial reference string is then broken into its
                     constituent zone, easting and northing parts, whose
                     magnitudes are checked against the limits.
                     If all is valid, the string is reconstructed with spaces
                     added for formatting and returned as the FormattedSR field
                     of the TValidSpatialRef.
                     Otherwise, the error field is populated with exception
                     messages produced during the validation.
      Dependencies:
      Side Effects: Calls RemoveSubstrings, DecodeFromLetters, ValidInteger,
                    UTMSpatialRefToEN
     Special Notes:
------------------------------------------------------------------------------}
function ValidUTMSpatialRef(iSpatialRef :string) : TValidSpatialRef;
var
  lZone, lFirstHalf, lSecondHalf : integer;
  lZoneStr, lFirstHalfStr, lSecondHalfStr : string;
  lCount : integer;
  lSpatialRef : string;
begin
  { Check that it is all numbers }
  lSpatialRef := StringReplace(iSpatialRef, ' ', '', [rfReplaceAll]);

  { UTM should be all numbers }
  If not ValidInteger (lSpatialRef) then
  begin
    Result.FormattedSR := ResStr_BeyondSystemLimits;
    Result.Valid := False;
    Result.Error := ResStr_InvalidSpRefFormat;
    Exit;
  end;

  { Split into the Zone, the north/south Component and the east/west Component
    ie. 31, (0 to 500000), (0 to 10000000) }
  lCount := Pos(' ', iSpatialRef);
  If lCount = 0 then
  begin
    Result.Valid := False;
    Result.FormattedSR := ResStr_BeyondSystemLimits;
    Result.Error := ResStr_OutsideSystem;
    Exit;
  end;
  lZoneStr := copy(iSpatialRef, 1, lCount-1);
  lSecondHalfStr := Copy(iSpatialRef, lCount + 1, Length(iSpatialRef));
  lCount := pos(' ', lSecondHalfStr);
  If lCount = 0 then
  begin
    Result.Valid := False;
    Result.FormattedSR := ResStr_BeyondSystemLimits;
    Result.Error := ResStr_OutsideSystem;
    Exit;
  end;
  lFirstHalfStr := copy(lSecondHalfStr, 1, lCount-1);
  lSecondHalfStr := copy(lSecondHalfStr, lCount + 1, length(lSecondHalfStr));

  lZone := StrToInt(lZoneStr);
  lFirstHalf := StrToInt(lFirstHalfStr);
  lSecondHalf := StrToInt(lSecondHalfStr);

  If (lZone < MIN_UTM_ZONE) or
     (lZone > MAX_UTM_ZONE) or
     (lFirstHalf < MIN_UTM_EASTING) or
     (lFirstHalf > MAX_UTM_EASTING) or
     (lSecondHalf < MIN_UTM_NORTHING) or
     (lSecondHalf > MAX_UTM_NORTHING) then
  begin
    Result.Valid := False;
    Result.FormattedSR := ResStr_BeyondSystemLimits;
    Result.Error := ResStr_OutsideSystem;
    Exit;
  end;
  Result.Valid := True;
  Result.FormattedSR := iSpatialRef;
  Result.Error := '';
end;

{==============================================================================
    Function  Name: ValidLatLongSpatialRef
            Called: ValidSpatialRef, ValidSpecificSpatialRef
           Purpose: Takes in any string and returns a TValidSpatialRef to show
                    whether the string is a valid LongLat Spatial Reference
                    string or not
      Dependencies:
      Side Effects: Calls RemoveSubstrings, DecodeFromLetters, ValidInteger,
                    UTMSpatialRefToEN
     Special Notes:
------------------------------------------------------------------------------}
function ValidLatLongSpatialRef(iSpatialRef : String) : TValidSpatialRef;
var
  lLatLong : TLatLong;
  lSpatialRef : String;
begin
  { Make a continuous string of only numbers, NESW, periods and +/- }
  lSpatialRef := RemoveNonLLSubstrings(iSpatialRef);

  { Split into Lat and Long }
  try
    lLatLong := FormatLLStringAsTLatLong(lSpatialRef);
  except
    on Exception do
    begin
      Result.FormattedSR := '';
      Result.Error := ResStr_InvalidSpRefFormat;
      Result.Valid := False;
      Exit;
    end;
  end; // try..except

  If not ValidLatLongRecord(lLatLong) then
  begin
    Result.Valid := False;
    Result.FormattedSR := ResStr_BeyondSystemLimits;
    Result.Error := ResStr_InvalidSpRefFormat;
    Exit;
  end;
  Result.Valid := True;
  Result.FormattedSR := FormatTLatLongAsString(lLatLong);
  Result.Error := '';
end;


{==============================================================================
    Function  Name: ValidLatLongRecord
            Called: ValidLatLongSpatialRef, Map
           Purpose: Returns a boolean to show whether a LongLat record
                    is valid or not
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function ValidLatLongRecord(iLatLong : TLatLong) : Boolean;
begin
  Result := true; // default
  try
    { This picks up invalid values, and also our NULL_LATLONG constant }
    if (iLatLong.Lat < MIN_LAT) or (iLatLong.Lat > MAX_LAT) or
       (iLatLong.Long < MIN_LONG) or (iLatLong.Long > MAX_LONG) then
      Result := false;
  except
    on Exception do
      Result := false;
  end; // try..finally
end;




{==============================================================================
    Function  Name: EastNorthToLatLong
            Called:
           Purpose: Gerneric Code to convert Easting and Northing from
                    any map projection system to Latitude and Longitude.

                    DO NOT call this function directly as constants need
                    initialising differently depending on which spatial
                    reference projection system is being used
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function EastNorthToLatLong(iEasting, iNorthing : double ;
                             iZone : integer): TLatLong;
var
  k, {phi} lMeridian : extended;
  sinK, cosK, tanK : extended;
  v : extended; // radius of cuvature at lat 'theta', perpendicular to meridian
  rho : extended; // radius of curvature of a meridian at latitude 'phi'
  EtaSquared : extended;
  eTrue : extended;
  JThree, JFour, JFive, JSix, JSeven, JEight, JNine : extended;
  LatTrueOriginRad, LongTrueOriginRad : extended;
  Latitude, Longitude : extended;
begin
  { Calculate Long of True origin for the UTM case }
  If tfUTM then
    LongTrueOrigin := (((iZone - 31)*6)+3);

  LatTrueOriginRad := DegToRad(LatTrueOrigin);
  LongTrueOriginRad := DegToRad(LongTrueOrigin);

  { 8.3i k is (phi2 - phi1) }
  k :=((iNorthing-nZero)/a) + LatTrueOriginRad;

  { Go to Equation 8.3ii to evaluate (M02-MO1) }
  lMeridian := CalculateMeridian(k);
  { 8.3v }
  While abs(iNorthing - nZero - lMeridian) > 0.0001 do { should be greater than or equal to }
  begin
    { 8.3iii -  }
    K := k + ((iNorthing - nZero - lMeridian)/a);
    lMeridian := CalculateMeridian(k);
  end;

  sinK := sin(K);
  tanK := tan(K);
  cosK := cos(K);

  { v - radius of cuvature at lat 'phi', perpendicular to meridian }
  { Section 3, page 6, OS Geodetic Info paper}
  v := a / sqrt(1-(eSquared*(sqr(sinK))));

  { rho = radius of curvature of a meridian at latitude theta(K) }
  rho := v*(1-eSquared)/(1-eSquared*(sqr(sinK)));

  EtaSquared:= v/rho-1;
  eTrue := iEasting - eZero;

  { Section 8.3(vi) }
  JThree := tanK / (2*rho*v);
  JFour := (tanK / (24*rho*(power(v,3)))) * (5 + 3*sqr(tanK) + EtaSquared
                                          - 9*(sqr(tanK))*EtaSquared);
  JFive := (tanK / (720*rho*(power(v,5))) * (61 + 90*(sqr(tanK))
                                          + 45*(power(tanK,4))));

  { Latitude in degrees }
  Latitude := RadToDeg((k - JThree*(sqr(eTrue)) + (power(eTrue,4))*JFour
                                     - (power(eTrue,6))*JFive));
  JSix := 1/(cosK*v);
  JSeven := (1/(cosK*6*(power(v,3)))) * ((v/rho) + 2*(sqr(tanK)));
  JEight := (1/(cosK*120*(power(v,5))))*(5 + 28*(sqr(tanK))
                                           + 24*(power(tanK,4)));
  JNine := (1/(cosK*5040*(power(v,7)))) * (61 + 662*(sqr(tanK))
                                        + 1320*(power(tanK,4))
                                        + 720*(power(tanK,6)));
  { Longitude (lambda) in degrees }
  Longitude := RadToDeg(LongTrueOriginRad + (eTrue*JSix) - ((power(eTrue,3))*JSeven)
               + ((power(eTrue,5))*JEight) - (power(eTrue,7)*JNine));

  Result.Long := Longitude;
  Result.Lat := Latitude;
end;

{==============================================================================
    Function  Name: LatLongToEastNorth
            Called:
           Purpose: Gerneric Code to convert Latitude and Longitude to Easting
                    and Northing for any map projection system

                    DO NOT call this function directly as constants need
                    initialising differently depending on which spatial
                    reference projection system is being used
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function LatLongToEastNorth(iLongitude, iLatitude: extended): TMapCoord;
var
  LatInRad, LongInRad : extended;
  SinLat, CosLat, TanLat : extended;
  v : extended;
  rho : extended;
  etaSquared : extended;
  p, pOne : extended;
  lMeridianArc : extended;
  bOne, bTwo, bThree, bThreeA, bFour, bFive, bSix : extended;
  LongTrueOriginRad : extended;
begin
  try
    { Special case - If a UTM projection and in southern hemisphere }
    if (tfUTM = true) and (iLatitude < 0) then
      nZero:=10000000;

    LatInRad := DegToRad(iLatitude);
    LongInRad := DegToRad(iLongitude);

    { Special Case - UTM Grid needs Longitude to set up true Origin }
    if tfUTM = true then
    begin
      LongTrueOrigin := (Trunc(Abs(iLongitude) / 6) * 6 + 3);
      if iLongitude < 0 then
        LongTrueOrigin:= - LongTrueOrigin;
    end;

    LongTrueOriginRad := DegToRad(LongTrueOrigin);

    { Conversion to degrees }
    SinLat := sin(LatInRad);
    CosLat := cos(LatInRad);
    TanLat := tan(LatInRad);

    v   := a / Power((1 - eSquared * Sqr(SinLat)), 0.5);
    rho := a * (1 - eSquared) / Power((1 - eSquared * Sqr(SinLat)), 1.5);
    etaSquared := (v / rho) - 1;

    if iLongitude < 0 then
      LongTrueOrigin := -LongTrueOrigin;

    p := LongInRad - LongTrueOriginRad;
    pOne := abs(p);

    lMeridianArc := CalculateMeridian(LatInRad);

    { Northing Equations }
    bOne    := lMeridianArc + nZero;
    bTwo    := (v / 2)   * SinLat * CosLat;
    bThree  := (v / 24)  * SinLat * Power(CosLat, 3) * (5 - Sqr(TanLat) + 9 * etaSquared);
    bThreeA := (v / 720) * SinLat * Power(CosLat, 5) * (61 - 58 * Sqr(TanLat) + Power(TanLat, 4));

    Result.y := Round(bOne
        + Sqr(pOne) * bTwo
        + Power(pOne, 4) * bThree
        + Power(pOne, 6) * bThreeA);

    { Easting Equations }
    bFour := v * CosLat;
    bFive := (v / 6)   * Power(CosLat, 3) * ((v / rho) - Sqr(TanLat));
    bSix  := (v / 120) * Power(CosLat, 5) *
        (5
        - 18 * Sqr(TanLat)
        + Power(TanLat, 4)
        + 14 * etaSquared
        - 58 * etaSquared * Sqr(TanLat));

    Result.x := Round(eZero
        + p * bFour
        + p * Power(pOne, 2) * bFive
        + p * power(pOne, 4) * bSix);
  except
    on E:Exception do
      raise EOutOfRangeError.Create(ResStr_BeyondSystemLimits);
  end;
end;

{==============================================================================
    Function  Name: CalculateMeridian
            Called: LatLongToEastNorth, EastNorthToLatLong
           Purpose: An intermediate function called by the generic functions
                    EastNorthToLatLong and LatLongToEastNorth

                    Source : OS Geodetic information paper No 1,
                    equation  8.1
                    Pass in 'k' to return 'M'
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function CalculateMeridian(iLatInRad : extended): extended;
var
  phiMinus, phiPlus : extended;
  LatTrueOriginRad : extended;
  lTemp : extended;
begin
  LatTrueOriginRad := DegToRad(latTrueOrigin);
  PhiMinus := iLatInRad - LatTrueOriginRad; // phi2 - phi1
  phiPlus := iLatInRad + LatTrueOriginRad;  // phi2 + phi1

  lTemp := ((1 + n + ((5/4)*(sqr(n))) + (5/4)*(power(n,3))) * PhiMinus)

         -((3*n + 3*(sqr(n)) + (21/8)*(power(n,3)))*sin(PhiMinus)*cos(phiPlus))

         +(((15/8)*(sqr(n)) + (15/8)*(power(n,3)))*sin(2*PhiMinus)*cos(2*phiPlus))

         -(((35/24)*(power(n,3)))*sin(3*PhiMinus)*cos(3*phiPlus));

  Result := b*lTemp;
end;

{==============================================================================
    Function  Name: SetGridConstants
            Called: Various
           Purpose: Because the Easting-Northing to Latitude-Longitude
                    conversion algorithms are generic mathematical models but
                    based on different projections, the UTM, UK grid and Irish
                    grid all use different constants.
                    This method sets up the constants based on the Spatial
                    Reference system passed in
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
procedure SetGridConstants(const iSystem : TGridType);
begin
  case iSystem of

    gtUK_Grid :
    begin
      tfUTM := false;
      a := 6375020.481; { actually A*Fo}
      b := 6353722.490; { actually B*Fo}
      eZero := 400000; { 400,000m }
      nZero := -100000; { -100,000m}
      { True origin }
      LatTrueOrigin := 49;
      LongTrueOrigin := -2;
    end;

    gtIrish_Grid :
    begin
      tfUTM := false;
      a := 6377563.395906615; { actually A*Fo}
      b := 6356256.9612075; { actually B*Fo}
      EZero := 200000; { 2000,000m West of true}
      nZero := 250000; { 250,000m South of true }
      {   True Origin }
      LatTrueOrigin := 53.5;
      LongTrueOrigin := -8;
      { ScaleFactor = 1.000 035 }
    end;

    gtUTM_Grid:
    begin
      tfUTM := true;
      { Factored.. }
      a := 6375836.6445; { actually A*Fo}
      b := 6354368.2356;
      { GridCoordinates of True Origin }
      EZero := 500000;
      nZero := 0; { in northern hemisphere... -10000000 in southern }
      LatTrueOrigin := 0; { Always zero }
      LongTrueOrigin := 0; { only used when calculating dummy lat longs into east northing }
      { LongTrueOrigin set by zone }
    end;
  end;
  eSquared := (sqr(a)-sqr(b))/sqr(a);
  n := (a-b)/(a+b);
end;

{==============================================================================
    Procedure Name: DecodePrefixFromLetter
            Called: Various
           Purpose: Pass in either a one or two letter prefix and this returns
                    a string of two or four numbers which represent the letters

                    For example, pass in "R" as an Irish Prefix, will return
                    "11" because of the following grid ...
                    4 . A B C D E
                    3 . F G H J K
                    2 . L M N O P
                    1 . Q R S T U
                    0 . V W X Y Z
                        - - - - -
                        0 1 2 3 4

                    Also, for a UK grid, "HX", which in 100 km squares is in
                    column 3, row 11, is returned as "210"
      Dependencies:
      Side Effects:
------------------------------------------------------------------------------}
function DecodePrefixFromLetter(const iPrefix: string; iSystem : TGridType): string;
var
  lFirstCoords : TPoint;
  lSecondCoords : TPoint;
  lResult : TPoint;
  lPrefix : String;
  //-------------------------Inline Procedure------------------------------------
  function LetterToBoxCoords(iLetter: string): TPoint;
  var
    lChar : char;
    lAsciInteger : integer;
    lLetter : string;
  begin
    lLetter := UpperCase(iLetter);
    lChar := lLetter[1];
    lAsciInteger := ord(lChar);
    lAsciInteger := lAsciInteger - 65;
    If lAsciInteger >7 then
      lAsciInteger := lAsciInteger -1;

    If lAsciInteger > 25 then
      lAsciInteger := lAsciInteger - 25;

    { lAsciInteger is now a number which represents a box number }

    { Convert to UK false Origin }
    Result.x := lAsciInteger mod 5;
    Result.y := 4 - (lAsciInteger div 5);
  end;
  //-------------------------Inline Procedure-----------------------------------
begin
  lPrefix := UpperCase(iPrefix);

  { Convert box coords from origin at top left to origin
    at bottom left and take account of false origin }
  lFirstCoords := LetterToBoxCoords(Copy(lPrefix, 1,1));

  Case iSystem of
    gtUK_Grid :
    begin
      lFirstCoords.x := (lFirstCoords.x -2) *5;
      lFirstCoords.y := (lFirstCoords.y -1) *5;
      lSecondCoords := LetterToBoxCoords(Copy(iPrefix, 2,2));
    end;

    gtIrish_Grid :
    begin
      { Irish Grid has no First Coordinate }
      lSecondCoords.x := lFirstCoords.x;
      lSecondCoords.y := lFirstCoords.y;
      lFirstCoords.x := 0;
      lFirstCoords.y := 0;
    end;
  end; // case

  lResult.x := lFirstCoords.x + lSecondCoords.x;
  lResult.y := lFirstCoords.y + lSecondCoords.y;

  If (lResult.x < 0) or (lResult.y < 0) then
    raise ESpatialRefError.Create(ResStr_OutsideSystem);

  Result := InttoStr(lResult.x) + InttoStr(lResult.y);
end;

{==============================================================================
    Function  Name: EncodePrefixToLetters
            Called: Various
           Purpose: Takes in a string, which is formed from an easting and
                    northing, after having removed the last 5 digits of both
                    (i.e. for (7654321 654321) it would be 766 and for
                    (123456, 987654) it would be 19) and a TGridType
                    parameter for the conversion being done.

                    The returned string is the encoded letter prefix.
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function EncodePrefixToLetters(iNumericPrefix: string; iSystem : TGridType): string;
var
  lFirsTMapCoord : TPoint;
  lSecondPoint : TPoint;
  FirstLetter : String;
  SecondLetter : String;
  lTempPrefix : String;
  //------------------------------------------------------------------------------
  function BoxCoordsToLetter(iBoxCoord: TPoint): String;
  var
    iBoxIndex : Integer;
  begin
    { The box index is the number of the box which the coords would
      fall in if we consider the 5x5 box of prefixes
      This can then be converted to a letter }
    iBoxIndex := (4 - iBoxCoord.y) *5 + iBoxCoord.x;
    If IBoxIndex < 0 then
      IBoxIndex := IBoxIndex + 25;
    If IBoxIndex > 25 then
      IBoxIndex := IBoxIndex - 25;
    iBoxIndex :=  iBoxIndex + 65;
    If iBoxIndex > 72 then
      iBoxIndex := iBoxIndex + 1;
    Result := Char(iBoxIndex);
    { Result is the letter denominated by the box }
  end;
//------------------------------------------------------------------------------

begin
  { Format as a 4 figure string of numbers }
  If Length(iNumericPrefix) = 2 then
    lTempPrefix := '0' + Copy(iNumericPrefix, 1 ,1)
                 + '0' + Copy(iNumericPrefix, 2 ,2)
  else
    if Length(iNumericPrefix) = 3 then
      lTempPrefix := '0' + Copy(iNumericPrefix,1,1)
                   + Copy(iNumericPrefix, 2, 3)
    else
      lTempPrefix := iNumericPrefix;

  { Take the 4 fig string and split into the first pair
    and second pair of coods }
  lFirsTMapCoord.x := StrToInt (Copy(lTempPrefix, 1,1));
  lFirsTMapCoord.y := StrToInt (Copy(lTempPrefix, 3,1));

  lSecondPoint.x := StrToInt (Copy(lTempPrefix, 2,1));
  lSecondPoint.y := StrToInt (Copy(lTempPrefix, 4,1));

  case iSystem of
    gtUK_Grid :
    begin
      { Adjust to make false origin }
      lFirsTMapCoord.x := ((lFirsTMapCoord.x *2)  + 2) ;
      lFirsTMapCoord.y := ((lFirsTMapCoord.y *2)   + 1);
      { If second pair are > 5 then the first pair's grrid letter is different }
      if lSecondPoint.x > 4 then
      begin
        lSecondPoint.x := lSecondPoint.x - 5;
        lFirsTMapCoord.x := lFirsTMapCoord.x + 1;
      end;

      if lSecondPoint.y > 4 then
      begin
        lSecondPoint.y := lSecondPoint.y - 5;
        lFirsTMapCoord.y := lFirsTMapCoord.y + 1;
      end;
      FirstLetter := BoxCoordsToLetter(lFirsTMapCoord);
      SecondLetter := BoxCoordsToLetter(lSecondPoint);
    end;

    gtIrish_Grid :
    begin
      { Adjust to make false origin }
      FirstLetter := BoxCoordsToLetter(lSecondPoint);
      SecondLetter := '';
    end;

  end; // case

  { Pass in the second pair }
  Result := FirstLetter + SecondLetter;
end;

{==============================================================================
    Function  Name: OSGBMapCoordToLatLong
            Called: Various
           Purpose: Conversion of a UK Easting-Northing as a TMapCoord (a
                    record of two doubles) to return a TLatLong record (two strings)
      Dependencies:
      Side Effects:
     Special Notes: Called directly by the Map to convert MapServer coordinates
                    to LatLongs.
                    The base map was constructed though converting Latitudes and
                    Longitudes to UK Eastings and Northings and so to transfer
                    data, points are reconverted to Latitudes and Longitudes for
                    global recognition by the database
------------------------------------------------------------------------------}
function OSGBMapCoordToLatLong(const iEastingNorthing : TMapCoord) :TLatLong;
var
  lFormattedEN : TMapCoord;
begin
  SetGridConstants(gtUK_Grid);
  lFormattedEN.x := round(iEastingNorthing.x);
  lFormattedEN.y := round(iEastingNorthing.y);
  Result := EastNorthToLatLong(lFormattedEN.x, lFormattedEN.y, 0);
end;

{==============================================================================
    Function  Name: OSIGMapCoordToLatLong
            Called: Various
           Purpose: Conversion of a Irish Easting-Northing as a TMapCoord (a
                    record of two doubles) to return a TLatLong record (two strings)
      Dependencies:
      Side Effects:
     Special Notes: Called directly by the Map to convert MapServer coordinates
                    to LatLongs.
                    The base map was constructed though converting Latitudes and
                    Longitudes to UK Eastings and Northings and so to transfer
                    data, points are reconverted to Latitudes and Longitudes for
                    global recognition by the database
------------------------------------------------------------------------------}
function OSIGMapCoordToLatLong(const iEastingNorthing : TMapCoord) :TLatLong;
var
  lFormattedEN : TMapCoord;
begin
  SetGridConstants(gtIrish_Grid);

  lFormattedEN.x := round(iEastingNorthing.x);
  lFormattedEN.y := round(iEastingNorthing.y);

  Result := EastNorthToLatLong(lFormattedEN.x, lFormattedEN.y, 0);
end;

{==============================================================================
    Function  Name: LatLongToOSGBEastNorth
            Called: Various
           Purpose: Converts into a UK Easting and Northing TMapCoord (which
                    is two doubles) Conversion of a a TLatLong record
                    (two strings) to a UK Easting-Northing TMapCoord (a record
                    of two doubles)
      Dependencies:
      Side Effects:
     Special Notes: Called directly by the Map to convert MapServer coordinates
                    to LatLongs.
                    The base map was constructed though converting Latitudes
                    and Longitudes to UK Eastings and Northings and so to
                    transfer data, points are reconverted to Latitudes and
                    Longitudes for global recognition by the database
------------------------------------------------------------------------------}
function LatLongToOSGBEastNorth(const iLatLong: TLatLong): TMapCoord;
var
  lMapCoord : TMapCoord;
  lPoint : TMapCoord;
begin
  SetGridConstants(gtUK_Grid);
  lMapCoord.x := iLatLong.Long ;
  lMapCoord.y := iLatLong.Lat ;
  lPoint := LatLongToEastNorth(lMapCoord.x, lMapCoord.y);
  Result.x := lPoint.x;
  Result.y := lPoint.y;
end;

{==============================================================================
    Function  Name: LatLongToOSIGEastNorth
            Called: Various
           Purpose: Converts into a Irish Easting and Northing TMapCoord (which
                    is two doubles) Conversion of a a TLatLong record
                    (two strings) to a UK Easting-Northing TMapCoord (a record
                    of two doubles)
      Dependencies:
      Side Effects:
     Special Notes: Called directly by the Map to convert MapServer coordinates
                    to LatLongs.
                    The base map was constructed though converting Latitudes
                    and Longitudes to UK Eastings and Northings and so to
                    transfer data, points are reconverted to Latitudes and
                    Longitudes for global recognition by the database
------------------------------------------------------------------------------}
function LatLongToOSIGEastNorth(const iLatLong: TLatLong): TMapCoord;
var
  lMapCoord : TMapCoord;
  lPoint : TMapCoord;
begin
  SetGridConstants(gtIrish_Grid);
  lMapCoord.x := iLatLong.Long ;
  lMapCoord.y := iLatLong.Lat;
  lPoint := LatLongToEastNorth(lMapCoord.x, lMapCoord.y);
  Result.x := lPoint.x;
  Result.y := lPoint.y;
end;

{==============================================================================
    Function  Name: RemoveSubstrings
            Called: Various
           Purpose: Takes in a string and returns the same string but without
                    any substring..
                    i.e. ' ' removes spaces so 'NS 332 242' converts to 'NS332242'
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function RemoveSubstrings(iString : string; iSubString : char) : string;
var
  lSpace : integer;
  lLength : integer;
  lFirstHalf : string;
  lSecondHalf : string;
begin
  lSpace := pos(iSubString, iString);
  while lSpace > 0 do
  begin
    lLength := Length(iString);
    lFirstHalf := copy(iString,1, (lSpace-1));
    lSecondHalf := copy(iString, lSpace+1, (lLength-lSpace));
    iString := lFirstHalf + lSecondHalf;
    lSpace := pos(' ', iString);
  end; // while lCount > 0
  Result := iString;
end;

{==============================================================================
    Function  Name: RemoveCommas
            Called: Various
           Purpose: Removes Commas from the Scale and formats it as a
                    MapServer scale i.e. "1:10,000" to 10000
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function RemoveCommas(iScale: String) : double;
var
  lColon : integer;
  lFirsthalf, lSecondHalf : string;
begin
  lColon      := Pos(':', iScale);
  lFirstHalf  := Copy(iScale, 0, lColon - 1);
  lSecondHalf := StringReplace(
      Copy(iScale, lColon + 1, Length(iScale)),
      ThousandSeparator,
      '',
      [rfReplaceAll]);
  Result      := StrToFloat(lSecondHalf) / StrToFloat(lFirstHalf);
end;

{==============================================================================
    Function  Name: RemoveCommas
            Called: Various
           Purpose: Adds zeros onto either the beginning of a stirng
                    or the end of a string in order to keep it to a
                    consistent length
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function PadWithZeros(iString: string; iLength: integer; iTrailing: boolean): string;
var
  lAsInt : integer;
  lNeg : boolean;
  lResult : string;
begin
  If iString = '' then
    iString := '0';
  lNeg := false;
  try
    lAsInt := StrToInt(iString);
    If (lAsInt < 0) then
    begin
      lNeg := true;
      lResult := IntToStr(abs(lAsInt));
    end
    else
      lResult := iString;
    while length(lResult) < iLength do
      if iTrailing then
        lResult := lResult + '0'
      else
        lResult := '0' + lResult;
    if lNeg then
      lResult := '-'+lResult;
    Result := lResult;
  except
    on EConvertError do
    //   MessageDlg(ResStr_ConvertStringError, mtInformation, [mbOk], 0);
  end;
end;  // PadWithZeros

{==============================================================================
    Function  Name: CheckBoundingBox
            Called:
           Purpose: To check both the corners of any bounding box, given two
                    spatial reference strings and checks whether they are to the
                    northeast and southwest.
      Dependencies:
      Side Effects:  Calls DetermineSpatialRefSystem and ValidSpatialRef,
                     ConvertToLatLong
     Special Notes:
------------------------------------------------------------------------------}
function CheckBoundingBox(const iSWSpatialRef, iNESpatialRef, iCurrentSystem: String): TValidBoundingBox;
var
  lTempMessage : string;
  lneLL, lswLL : TLatLong;
  lneValid, lswValid : TValidSpatialRef;
begin
  Result.Valid := False;
  { Valid Spatial Ref...
    - Determines the input system
    - Converts it to the current system
    - Checks whether the converted value is valid }
  If SameText(iSWSpatialRef, ResStr_BeyondSystemLimits) then
    lTempMessage := ResStr_SouthWest
  else
  if SameText(iSWSpatialRef, ResStr_BeyondSystemLimits) then
    lTempMessage := ResStr_NorthEast
  else
    lTempMessage := '';

  if Length(lTempMessage) > 1 then
  begin
    Result.Valid := false;
    Result.Error := Format(ResStr_SRSystemLimit, [lTempMessage]);
    Exit;
  end;

  if DetermineSpatialRefSystem(iSWSpatialRef) <> DetermineSpatialRefSystem(iNESpatialRef) then
  begin
    Result.Valid := False;
    Result.Error := ResStr_SameSRSystem;
    Exit;
  end;

  lneValid := ValidSpatialRef(iNESpatialRef, iCurrentSystem);
  If lneValid.Valid then
    Result.FormattedNorthEastSR := lneValid.FormattedSR
  else
    Result.Error := lneValid.Error;

  lswValid := ValidSpatialRef(iSWSpatialRef, iCurrentSystem);
  If lswValid.Valid then
    Result.FormattedSouthWestSR := lswValid.FormattedSR
  else
    Result.Error := lswValid.Error;

  if not lswValid.Valid then
    Result.Error := ResStr_SW;

  if not lneValid.Valid then
  begin
    If Result.Error <> '' then
      Result.Error := Result.Error + ResStr_AndNE
    else
      Result.Error := ResStr_NE;
  end;

  { CheckSW is SW of NE only if both are valid }
  if Result.Error = '' then
  begin
    lneLL := ConvertToLatLong(lneValid.FormattedSR, iCurrentSystem);
    lswLL := ConvertToLatLong(lswValid.FormattedSR, iCurrentSystem);

    Result.Valid := (lneLL.Lat > lswLL.Lat) and (lneLL.Long > lswLL.Long);
    if not Result.Valid then
      Result.Error := ResStr_DirCornerPositions;
  end else begin
    Result.Valid := False;
    Result.Error := ResStr_BoundingBoxInvalid  + Result.Error;
  end;
end;  // CheckBoundingBox


{==============================================================================
    Function  Name: ValidInteger
            Called:
           Purpose: Checks whether a sting passed in contains an integer Value
                    or not.
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function ValidInteger(iTestString : string) : boolean;
var
  lCOunt : integer;
  lChar : char;
begin
  If iTestString = '' then
  begin
    Result := true;
    exit;
  end;
  for lCount := 1 to Length(iTestString) do
  begin
    lChar := iTestString[lCount];
    If not (lChar in ['0'..'9']) then
    begin
      Result := False;
      exit;
    end;
  end;
  Result := True;
end;

{==============================================================================
    Function  Name: ValidLetter
            Called:
           Purpose: Checks whether a string passed in contains an letter A-Z
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function ValidLetter(iTestString : string) : boolean;
var
  lCount : integer;
  lString : String;
  lChar : Char;
begin
  If iTestString = '' then
  begin
    Result := false;
    exit;
  end;
  lString := UpperCase(iTestString);
  for lCount := 1 to Length(lString) do
  begin
    lChar := lString[lCount];
    If not (lChar in ['A'..'Z']) then
    begin
      Result := False;
      exit;
    end;
  end;
  Result := true;
end;

{==============================================================================
    Function  Name: LatLongFromStr
           Purpose: Builds a TLatLong from two strings
------------------------------------------------------------------------------}
function LatLongFromStr(const iLat, iLong : string) : TLatLong;
begin
  Result.Lat := StringToLat(iLat);
  Result.Long := StringToLong(iLong);
end;


{==============================================================================
    Function  Name: LatLong
           Purpose: Builds a TLatLong from two doubles
------------------------------------------------------------------------------}
function LatLong(const iLat, iLong : double) : TLatLong;
begin
  Result.Lat := iLat;
  Result.Long := iLong;
end;

{==============================================================================
    Function  Name: ConvertSystems
            Called:
           Purpose: Calls functions to Convert a spatial ref from any system to
                    any other system.
      Dependencies:
      Side Effects: Calls ConvertToLatLong and ConvertFromLatLong
     Special Notes:
------------------------------------------------------------------------------}
function ConvertSystems(iSpatialRef, iSystemIn, iSystemOut : string) : string;
var
  lLatLong : TLatLong;
begin
  try
    lLatLong := ConvertToLatLong(iSpatialRef, iSystemIn);
    Result   := ConvertFromLatLong(lLatLong, iSystemOut);
  except
    on E:ESpatialRefError do
      raise ESpatialRefSystemConversionError.Create(E.Message);
  end; // try..except
end;

{==============================================================================
    Function  Name: FormatLLStringAsTLatLong
            Called:
           Purpose: Takes a lat long string as inputted and returns it as a
                    formatted TLatLong... always simply numbers
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function FormatLLStringAsTLatLong(iLLString : String) : TLatLong;
var
  lSpatialRef : string;
  lChar : char;
  lFirstHalf, lSecondHalf : string;
  lCount : integer;
begin
  { Remove unwanted characters }
  lSpatialRef := RemoveNonLLSubstrings(illString);

  { Seperate into N / S / E / W parts }
  For lCount := 1 to Length(lSpatialRef) do
  begin
    lChar := lSpatialRef[lCount];
    If (lChar in ['A'..'Z']) then
    begin
      lFirstHalf := copy(lSpatialRef, 1, lCount);
      lSecondHalf := Copy (lSpatialRef, lCount+1, Length(lSpatialRef));
      break;
    end;
  end;

  Result.Lat  := StringToLat(lFirstHalf);
  Result.Long := StringToLong(lSecondHalf);
end;

{==============================================================================
    Function  Name: FormatTLatLongAsString
            Called: Various - Map, self.
           Purpose: Takes a TLatLong and formats it as a spatial reference string.
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function FormatTLatLongAsString(const iLatLong : TLatLong) : String;
begin
  Result := LatToString(iLatLong.Lat, LAT_LONG_SIGFIGS) + ListSeparator +
      ' ' + LongToString(iLatLong.Long, LAT_LONG_SIGFIGS);
end;

{==============================================================================
     Function Name: Tetrad
            Called:
           Purpose: Identifies whether a passed spatial reference is a tetrad
                    or not.  To be a tetrad it has to be ...
                    2 letters, 2 numbers, 1 letter (UK)
                    1 letter, 2 numbers, 1 letter (Irish)
                    O is excluded because this is not in the tetrad system.
      Dependencies:
     Special Notes:
------------------------------------------------------------------------------}
function Tetrad(const iSpatialRef : string): boolean;
var
  lSystem : string;
begin
  Result := false;
  lSystem := DetermineSpatialRefSystem(iSpatialRef);
  if (((lSystem = OS_GB) and (length(iSpatialRef) = 5)) or
       ((lSystem = OS_NI) and (length(iSpatialRef) = 4))) then
    If ValidLetter(Copy(iSpatialRef, Length(iSpatialRef), 1)) and
       (UpperCase(Copy(iSpatialRef, Length(iSpatialRef), 1)) <> 'O') then
      Result := True;
end;

{==============================================================================
    Function  Name:
            Called:
           Purpose: Takes in a spatial reference and a system, plus an integer
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function NextGridSquare(const iOldRef: string;
                         const iSize: integer; const iSystem: string): TMapCoord;
var
  lOldRefEN : TMapCoord;
begin
  Result.x := 0;
  Result.y := 0;
  // convert old gridref to N / E
  if iSystem = OS_GB then
    lOldRefEN := OSGBSpatialRefToEN(iOldRef)
  else
  if iSystem = OS_NI then
    lOldRefEN := OSIGSpatialRefToEN(iOldRef)
  else
  if iSystem = UTM then begin
    lOldRefEN.x := UTMSpatialRefToEN(iOldRef).easting;
    lOldRefEN.y := UTMSpatialRefToEN(iOldRef).northing;
  end;
  Result.x := lOldRefEN.x + iSize;
  Result.y := lOldRefEN.y + iSize;
end;

{==============================================================================
    Function  Name: CheckSpatialRefInBB
            Called:
           Purpose: Pass in a BottomLeft and a TopRight TLatLong, a spatial ref
                    and a system and the function converts the spatial reference
                    to a TLatLong record.
                    The Converted spatial refernce string is then checked to
                    be within the bounding box lat and long
      Dependencies:
      Side Effects: Calls ConvertToLatLong,
     Special Notes:
------------------------------------------------------------------------------}
function CheckSpatialRefInBB(const RBL, RTR: TLatLong;
                              const iSpatialRef, iSystem: string): boolean;
var
  lRefAsLatLong : TLatLong;
begin
  lRefAsLatLong := ConvertToLatLong(iSpatialRef, iSystem);

  if (lRefAsLatLong.Lat > RTR.Lat)
     or (lRefAsLatLong.Long > RTR.Long)
     or (lRefAsLatLong.Lat < RBL.Lat)
     or (lRefAsLatLong.Long < RBL.Long) then
    Result := false
  else
    Result := true;
end;  // CheckSpatialRefInBB

{==============================================================================
    Function  Name: CheckSRefInSRef
            Called:
           Purpose: Takes two spatial references and their systems and tests to
                    see if one is contained within the other
      Dependencies:
      Side Effects: Calls SpatialRefToEastNorth and CompareEastingNorthings
     Special Notes:
------------------------------------------------------------------------------}
function CheckSRefInSRef(const iParentSR, iChildSR,
                          iParentSystem, iChildSystem: string): boolean;
var
  lPoint1, lPoint2 : TMapCoord;
  lChildSR: string;
begin
  Result := False;
  { Check if the two references and systems are equal }
  if iParentSystem = iChildSystem then begin
    if iParentSR = iChildSR then begin
      Result := True;
      Exit;
    end; { If exactly identical }
    lChildSR := iChildSR
  end
  else
    lChildSR := ConvertSystems(iChildSR, iChildSystem, iParentSystem);

  if lChildSR=ResStr_BeyondSystemLimits then begin
    Result := false;
    exit;
  end;

  { If systems are the same and LatLong then the Spatial refs must be equal }
  if (iParentSystem = LAT_LONG) or (iParentSystem = UTM) then
    Raise ESpatialRefError.Create(ResStr_SameSpatialRef);

  lPoint1 := TSpatialRefSubFuncs.SpatialRefToEastNorth(iParentSR, iParentSystem);
  { Catch exceptions that may be raised if the spatial reference for the
    child lies outside the bounds of the spatial reference system.
    Return negative Result where this is the case. }
  try
    lPoint2 := TSpatialRefSubFuncs.SpatialRefToEastNorth(lChildSR, iParentSystem);
  except
    on E:ESpatialRefError do begin
      Result := False;
      Exit;
    end;
  end; // try..except
  //  Round to nearest cm in case of rounding error}
  lPoint1.x := RoundTo(lPoint1.x, -2);
  lPoint1.y := RoundTo(lPoint1.y, -2);
  lPoint2.x := RoundTo(lPoint2.x, -2);
  lPoint2.y := RoundTo(lPoint2.y, -2);

  if not Tetrad(iParentSR) then begin
    { Check that lPoint2 starts with the same digits as lPoint1 and is
      therefore inside it. }
    // Check the easting
    if not CompareEastingNorthings(lPoint1.x, lPoint2.x) then exit;
    // Do the same for the Northing
    if not CompareEastingNorthings(lPoint1.y, lPoint2.y) then exit;
  end
  else begin
    // tetrads work in 2000m blocks
    if (Trunc(lPoint1.X/2000) <> Trunc(lPoint2.X/2000)) or
       (Trunc(lPoint1.Y/2000) <> Trunc(lPoint2.Y/2000)) then
      Exit;
  end;
  Result := True;
end;  // CheckSRefInSRef

{==============================================================================
    Function  Name: CompareEastingNorthings
            Called:
           Purpose: This function checks that a Easting or a Northing is the
                    same as another and where they differ is only by 0s
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function CompareEastingNorthings(iCoord1, iCoord2: double): boolean;
var
  ltfZeros : Boolean;
  i : integer;
begin
  Result   := False;
  ltfZeros := False;
  for i := 1 to length(FloatToStr(iCoord1)) do
  begin
    if not SameText(Copy(FloatToStr(iCoord1), i, 1), Copy(FloatToStr(iCoord2), i, 1)) then
    begin
      if ltfZeros then
      begin
        if Copy(FloatToStr(iCoord1), i, 1) <> '0' then
          Exit
      end
      else
      begin
        if Copy(FloatToStr(iCoord1), i, 1) = '0' then
          ltfZeros := True
        else
          Exit;
      end
    end
    else
      if ltfZeros then
        if Copy(FloatToStr(iCoord1), i, 1) <> '0' then exit;
  end;
  Result := True;
end;

{==============================================================================
    Function  Name: GetDisplaySpatialRef
            Called: SpatialRef component.
           Purpose: Function to get and set to the database.
                    Pass in
                    - a System which the Spatial Refernce is to be displayed in
                    - a Spatial Reference string and System
                    - a Latitude and a Longitude
                    - a FieldString
                    The function determines whether the system to display the
                    spatial ref in is the same as the system which the string
                    currently is in.  If they are different then it converts the
                    passed Latitude and Longitude to the Current Display System
      Dependencies:
      Side Effects: Calls ConvertFromLatLong
     Special Notes:
------------------------------------------------------------------------------}
function GetDisplaySpatialRef(const iDisplaySystem, iSpatialRef,
                               iSpatialRefSystem, iLat, iLong, iFieldString : string) : string;
begin
  { If we are in the correct system already, don't reconvert from latlong
     as we don't want rounding errors }
  if SameText(iDisplaySystem, iSpatialRefSystem) then
  begin
    if (iDisplaySystem = OS_GB) or (iDisplaySystem = OS_NI) then
      { strip spaces, because old versions of rec2k inserted unecessary spaces }
      Result := StringReplace(iSpatialRef, ' ', '', [rfReplaceAll])
    else
      Result := iSpatialRef
  end else // need to convert
    try
      // check before converting
      if GenFuncs.IsNumber(iLat) and GenFuncs.IsNumber(iLong) then begin
        if iDisplaySystem <> LAT_LONG then
          Result := ConvertFromLatLong(LatLongFromStr(iLat, iLong), iDisplaySystem)
        else
          Result := LatToString(StrToFloat(iLat), LAT_LONG_SIGFIGS) +
                    ListSeparator + ' ' +
                    LongToString(StrToFloat(iLong), LAT_LONG_SIGFIGS);
      end else
        Result := iFieldString;
    except
      on E:ESpatialRefError do
        Result := E.Message;
    end;
end;

{==============================================================================
    Function  Name: ValidateSpatialRefEntry
            Called: On exiting edit boxes and the like.
           Purpose: Allows edit boxes to validate spatial references in the
                    same way as the spatial refererence component might.
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
procedure ValidateSpatialRefEntry(var iControl : TEdit; const iSystem : string;
                                      var iEnteredRef : string);
var
  lValidSr : TValidSpatialRef;
  lValidated : boolean;
begin
  lValidated := False;
  { Case 1.  If there is no text...  }
  if iControl.Text = '' then
  begin
    if iControl.Modified then
      iEnteredRef := ''
    else
    begin
      if iEnteredRef <> '' then
      begin
        lValidSR := ValidSpatialRef(iEnteredRef, iSystem);
        lValidated := True;
      end;
    end;
  end // if no text
  else
  begin
    if iEnteredRef = '' then
    begin
      { Case 2.  If the text entered is the first to be entered (not modified) }
      lValidSR := ValidSpatialRef(iControl.Text, iSystem);
      lValidated := True;
      if lValidSR.Valid then
        iEnteredRef := lValidSr.EnteredFormattedSR;
    end // if iEnteredRef = ''
    else
    begin
      { Case 3.  If the text entered is NOT the first to be entered (ie. the text
                 in the control is being modified) }
      if iControl.Modified then
      begin
        lValidSR := ValidSpatialRef(iControl.Text, iSystem);
        if lValidSR.Valid then
          iEnteredRef := lValidSr.EnteredFormattedSR;
      end
      else
        lValidSR := ValidSpatialRef(iEnteredRef, iSystem);
      lValidated := True;
    end;
  end;

  if lValidated then
  begin
    if not lValidSR.Valid then
      If iControl.Modified then
        raise ESpatialRefError.Create(lValidSr.Error);
    iControl.Text := lValidSR.FormattedSR;
    //    iControl.Text := UpperCase(iControl.Text);
  end;
end;

{==============================================================================
    Function  Name: DifferentEastingAndNorthing
            Called: RegisterMap dialog
           Purpose: Takes two spatial references and checks that they do not
                    share either the same easting or northing so that an
                    EZeroDIvide error is not produced when a sheet is displayed.
      Dependencies: Assumes the spatial references are valid.
      Side Effects: Calls DetermineSpatialRefSystem,
     Special Notes:
------------------------------------------------------------------------------}
function DifferentEastingAndNorthing(const iSWSpatialRef, 
                                           iNESpatialRef : string) : boolean;
var
  lSWSystem, lNESystem : string;
  lSWEastNorth, lNEEastNorth : TMapCoord;
begin
  Result    := false;
  lSWSystem := DetermineSpatialRefSystem(iSWSpatialRef);
  lNESystem := DetermineSpatialRefSystem(iNESpatialRef);
  if not SameText(lSWSystem, lNESystem) then
  begin
    Result := false;
    exit;
  end;
  { Convert to easting and northings. }
  lSWEastNorth := TSpatialRefSubFuncs.SpatialRefToEastNorth(iSWSpatialRef, lSWSystem);
  lNEEastNorth := TSpatialRefSubFuncs.SpatialRefToEastNorth(iNESpatialRef, lNESystem);

  { Check different }
  If (lSWEastNorth.x <> lNEEastNorth.x) and
     (lSWEastNorth.y <> lNEEastNorth.y) then
    Result := True;
end;

{==============================================================================
    Function  Name: ConvertOutOfRangeToSystemLimit
            Called: Various
           Purpose: If a spatialReference is out of range then this returns the
                    valid version of that spatial reference/
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function ConvertOutOfRangeToSystemLimit(const iLatLong : TLatLong;
                                         const iSystem : string) : string;
var
  lEastNorth : TMapCoord;
begin
  lEastNorth := LatLongToSpecificEN(iLatLong, iSystem);
  if SameText(iSystem, OS_GB) then
  begin
    if (lEastNorth.x < MIN_OSGB_EASTING)  then lEastNorth.x := MIN_OSGB_EASTING;
    if (lEastNorth.x > MAX_OSGB_EASTING)  then lEastNorth.x := MAX_OSGB_EASTING;
    if (lEastNorth.y < MIN_OSGB_NORTHING) then lEastNorth.y := MIN_OSGB_NORTHING;
    if (lEastNorth.y > MAX_OSGB_NORTHING) then lEastNorth.y := MAX_OSGB_NORTHING;
  end
  else
  { Irish Grid }
  if SameText(iSystem, OS_NI) then
  begin
    if (lEastNorth.x < MIN_OSIG_EASTING)  then lEastNorth.x := MIN_OSIG_EASTING;
    if (lEastNorth.x > MAX_OSIG_EASTING)  then lEastNorth.x := MAX_OSIG_EASTING;
    if (lEastNorth.y < MIN_OSIG_NORTHING) then lEastNorth.y := MIN_OSIG_NORTHING;
    if (lEastNorth.y > MAX_OSIG_NORTHING) then lEastNorth.y := MAX_OSIG_NORTHING;
  end;
  Result := EastingNorthingToSpatialRef(lEastNorth, iSystem, iSystem);
end;


//==============================================================================
{ Private LatOrLongToString
  iSigChars is the number of characters after the decimal place returned.
     Default of zero means all characters returned }
function LatOrLongToString(idblValue : double;
         istPosChar, istNegChar : string; iSigPlaces : integer = 0) : string;
begin
  // Handle rounding if necessary
  if iSigPlaces <> 0 then
    Result := FloatToStr(Abs(RoundTo(idblValue, 0-iSigPlaces)))
  else
    Result := FloatToStr(Abs(idblValue));
  // append apropriate character for direction from equator
  if idblValue>0 then
    Result := Result + istPosChar
  else
    Result := Result + istNegChar;
end;


//==============================================================================
// convert a lat float into a string representation
function LatToString(iLat : double; iSigPlaces : integer = 0) : string;
begin
  Result := LatOrLongToString(iLat, 'N', 'S', iSigPlaces)
end;


//==============================================================================
// convert a long float into a string representation
function LongToString(iLong : double; iSigPlaces : integer = 0) : string;
begin
  Result := LatOrLongToString(iLong, 'E', 'W', iSigPlaces)
end;


//==============================================================================
{ Private StringToLatOrLong
  iSigChars is the number of characters after the decimal place returned.
     Default of zero means all characters returned }
function StringToLatOrLong(istStr, istPosChar, istNegChar : string) : double;
begin
  if Copy(istStr, Length(istStr), 1) = istPosChar then
    Result := StrToFloat(Copy(istStr, 1, Length(istStr)-1))
  else if Copy(istStr, Length(istStr), 1) = istNegChar then
    Result := 0 - StrToFloat(Copy(istStr, 1, Length(istStr)-1))
  else
    Result := StrToFloat(istStr);
end;


//==============================================================================
{ Convert a string into a long floating point }
function StringToLong(istLong : string) : double;
begin
  if SameText(istLong, ResStr_BeyondSystemLimits) then
    raise EOutOfRangeError.Create(ResStr_BeyondSystemLimits);
  Result := StringToLatOrLong(istLong, 'E', 'W')
end;


//==============================================================================
{ Convert a string into a lat floating point }
function StringToLat(istLat : string) : double;
begin
  if SameText(istLat, ResStr_BeyondSystemLimits) then
    raise EOutOfRangeError(ResStr_BeyondSystemLimits);
  Result := StringToLatOrLong(istLat, 'N', 'S')
end;

{-------------------------------------------------------------------------------
  Convert an input SW and NE grid square to the range of grid squares that
     covers the entire rectangle.  ARange parameter is for output.  Option to
     wrap each ref in single quotes (easier to inject into SQL using CommaText)
}
procedure BuildGridSquareRange(const ASWRef, ANERef, ASystem: string; ARange:
    TStringList; AWantQuotes: boolean;
    AThreadTerminationCheck: TThreadTerminationCheck=nil);
var
  lSWXY, lNEXY: TPoint;
  x, y: integer;
  lGridType: TGridType;
  lExpectedFakePointLength: integer;
  lQuotes: string;

    function IsTetrad: boolean;
    begin
      Result := (not IsInt(RightStr(ASWRef, 1))) and (Length(ASWRef)>2);
    end;

    function CreateTetradXY(AXY: TPoint; ADintyChar: char): TPoint;
    var
      lDintyMetres: TPoint;
    begin
      lDintyMetres := DintyToMetres(ADintyChar);
      Result.X := AXY.X*5 + lDintyMetres.X div 2000;
      Result.Y := AXY.Y*5 + lDintyMetres.Y div 2000;
    end;

    // create a fake XY value for a grid ref
    function ConvertToXY(const ARef: string; APrefixLen: integer): TPoint;
    var
      lPrefix, lSuffix, lPrefixNum: string;
    begin
      lPrefix := LeftStr(ARef, APrefixLen);
      lSuffix := Copy(ARef, APrefixLen + 1, 255);
      lPrefixNum := DecodePrefixFromLetter(lPrefix, lGridType);
      Result.X := StrToInt(LeftStr(lPrefixNum, Length(lPrefixNum) div 2) +
                  LeftStr(lSuffix, Length(lSuffix) div 2));
      // don't take y value from right in case its a tetrad
      Result.Y := StrToInt(RightStr(lPrefixNum, Length(lPrefixNum) div 2) +
                  Copy(lSuffix, Length(lSuffix) div 2 + 1, Length(lSuffix) div 2));
      if IsTetrad then
        Result := CreateTetradXY(Result, RightStr(ARef, 1)[1]);
    end;

    // ensure that any zeros lost when converting to an integer are restored
    function PadZeros(const AValue: string): string;
    begin
      Result := LeftStr('00000000', lExpectedFakePointLength - Length(AValue))
             + AValue;
    end;

    // create a fake XY value for a grid ref
    function ConvertFromXY(const AFakePoint: TPoint): string;
    var
      lPrefix, lPrefixNum: string;
      lFakePointStrX, lFakePointStrY: string;
      lFakePointNotTetrad: TPoint;
    begin
      if IsTetrad then begin
        lFakePointNotTetrad.X := AFakePoint.X div 5;
        lFakePointNotTetrad.Y := AFakePoint.Y div 5;
      end else
        lFakePointNotTetrad := AFakePoint;
      lFakePointStrX := IntToStr(lFakePointNotTetrad.X);
      lFakePointStrX := PadZeros(lFakePointStrX);
      lFakePointStrY := IntToStr(lFakePointNotTetrad.Y);
      lFakePointStrY := PadZeros(lFakePointStrY);
      lPrefixNum := LeftStr(lFakePointStrX, 1) + LeftStr(lFakePointStrY, 1);
      lPrefix := EncodePrefixToLetters(lPrefixNum, lGridType);
      Result := lPrefix + Copy(lFakePointStrX, 2, 255) + Copy(lFakePointStrY, 2, 255);
      if IsTetrad then
        Result := Result + Chr(Ord('A') + AFakePoint.X mod 5 * 5 + AFakePoint.Y mod 5);
    end;

    // ensures that the grid square range is built from sw to ne, no matter what
    // the user initially requested
    procedure FixDirectionSwToNE;
    var
      lTemp: integer;
    begin
      if lSWXY.X>lNEXY.X then begin
        lTemp := lSWXY.X;
        lSWXY.X := lNEXY.X;
        lNEXY.X := lTemp;
      end;
      if lSWXY.Y>lNEXY.Y then begin
        lTemp := lSWXY.Y;
        lSWXY.Y := lNEXY.Y;
        lNEXY.Y := lTemp;
      end;
    end;

begin
  // first convert to a dummy X,y for sw and ne corners, so we can iterate the
  // rectangle and convert back to a grid square
  if (ASystem = OS_GB) then begin
    lGridType := gtUK_Grid;
    lSWXY := ConvertToXY(ASWRef, 2);
    lNEXY := ConvertToXY(ANERef, 2);
    // in case a 'fake' point has zero at the beginning which is lost when
    // converted to an int, remember the expected length so we can pad it
    lExpectedFakePointLength := (Length(ASWRef)-2) div 2 + 1;
  end
  else if (ASystem = OS_NI) then begin
    lGridType := gtIrish_Grid;
    lSWXY := ConvertToXY(ASWRef, 1);
    lNEXY := ConvertToXY(ANERef, 1);
    // see above
    lExpectedFakePointLength := (Length(ASWRef)-1) div 2 + 1;
  end
  else begin
    raise Exception.Create(ResStr_AddinReduceNotImp);
  end;
  if AWantQuotes then
    lQuotes := ''''
  else
    lQuotes := '';
  FixDirectionSwToNE;
  for y := lSWXY.y to lNEXY.y do
    for x := lSWXY.x to lNEXY.x do begin
      ARange.Add(lQuotes + ConvertFromXY(Point(x, y)) + lQuotes);
      if Assigned(AThreadTerminationCheck) then
        if AThreadTerminationCheck then raise EAbort.Create('');
    end;
end;

{-------------------------------------------------------------------------------
  Takes a grid reference and reduces the precision to 100m, 1k, 2k or 10k
}
procedure ReduceGridPrecision(var ARef: string; APrecision: integer;
    const ASystem: string);
var
  lPrefix, lXNums, lYNums: string;
  lNumDigits: integer;
  lEN: TMapCoord;
  lDintyCoord: TPoint;
  lDintyCode : char;

    // break a grid into the prefix and separate x, y parts
    procedure DecodeGrid(APrefixLen: integer);
    var
      lSuffix: string;
    begin
      lPrefix := LeftStr(ARef, APrefixLen);
      lSuffix := Copy(ARef, APrefixLen + 1, 255);
      lXNums := LeftStr(lSuffix, Length(lSuffix) div 2);
      lYNums := RightStr(lSuffix, Length(lSuffix) div 2);
    end;
begin
  if ASystem = OS_GB then
    DecodeGrid(2)
  else if ASystem = OS_NI then
    DecodeGrid(1)
  else
    raise Exception.Create(ResStr_AddinReduceNotImp);
  // work out how many numbers we want in our ref
  case APrecision of
    100: lNumDigits := 6;
    1000: lNumDigits := 4;
    2000, 10000: lNumDigits := 2;
  else
    raise ESpatialRefError.Create(Format(ResStr_InvalidGridPrecision, [APrecision]));
  end;
  if APrecision=2000 then begin
    // Work out the DINTY code
    lEN := SpatialRefToSpecificEastNorth(ARef, ASystem, ASystem);
    lDintyCoord.X := (trunc(lEN.X) mod 10000) div 2000;
    lDintyCoord.Y := (trunc(lEN.Y) mod 10000) div 2000;
    lDintyCode := Chr((lDintyCoord.X*5 + lDintyCoord.Y) + Ord('A'));
  end else
    lDintyCode := Chr(0);
  lXNums := Leftstr(lXNums, lNumDigits div 2);
  lYNums := Leftstr(lYNums, lNumDigits div 2);
  ARef := lPrefix + lXNums + lYNums;
  if APrecision=2000 then
    ARef := ARef + lDintyCode;
end;

{-------------------------------------------------------------------------------
  Raises an exception if a system does not support grid functions
}
procedure CheckIsGridSystem(const ASystem: string);
var
  lIsGrid: boolean;
  lIntf: ISpatialReference;
begin
  if ASystem=ResStr_SystemUnknown then
    raise ESpatialRefError.Create(ResStr_SystemUnknown);
  lIsGrid := SameText(ASystem, OS_GB) or SameText(ASystem, OS_NI);
  if not lIsGrid then
    if not SameText(ASystem, UTM) or SameText(ASystem, LAT_LONG) then begin
      // check addins
      lIntf := CurrentSpatialRefInterface(ASystem);
      lIsGrid := Supports(lIntf, IID_IGridSystem);
    end;
  if not lIsGrid then
    raise ESpatialRefError.Create(ResStr_SystemNotAGridSystem);
end;

{==============================================================================
    Function  Name: SpatialRefToEastNorth
            Called:
           Purpose: Converts a Spatial Reference when passed in as a string to
                    an Easting-Northing TMapCoord of the same Spatial Reference
                    system type.
      Dependencies:
      Side Effects: Calls sub-functions UTMSpatialRefToEN, OSGBSpatialRefToEN,
                      OSIGSpatialRefToEN
     Special Notes:
------------------------------------------------------------------------------}
class function TSpatialRefSubFuncs.SpatialRefToEastNorth(const AGridRef, ASystem: string): TMapCoord;
begin
  Result := SpatialRefToSpecificEastNorth(AGridRef, ASystem, ASystem);
end;  // SpatialRefToEastNorth

{-------------------------------------------------------------------------------
  Converts the given spatial ref from english (as read from DB) to current
  locale for display.
}
function LocaliseSpatialRef(const ASpatialRef: String): String;
var
  convertedRef: String;
begin
  // Can't assume there's be no conflict while replacing separators, so use #1 as temp
  convertedRef := StringReplace(ASpatialRef, '.', #1, [rfReplaceAll]);
  // If '.' was replaced, carry on with replacing the list separator.
  // Otherwise, it can be assumed it's already localised, and continuing replacing characters
  // is likely to result in an incorrectly formatted spatial ref.
  if convertedRef <> ASpatialRef then begin
    // Deal with ListSeparator, ',' in english.
    convertedRef := StringReplace(convertedRef, ',', ListSeparator, [rfReplaceAll]);
    // And now deal with DecimalSeparator, '.' replaced by #1 earlier.
    convertedRef := StringReplace(convertedRef, #1, DecimalSeparator, [rfReplaceAll]);
  end;
  Result := convertedRef;
end;

{-------------------------------------------------------------------------------
  Converts the given spatial ref from current locale to english format,
  ready to be stored in DB.
}
function DelocaliseSpatialRef(const ASpatialRef: String): String;
var
  convertedRef: String;
begin
  // Can't assume there's be no conflict while replacing separators, so use #1 as temp
  convertedRef := StringReplace(ASpatialRef, DecimalSeparator, #1, [rfReplaceAll]);
  // If DecimalSeparator was replaced, carry on with replacing the list separator.
  // Otherwise, it can be assumed it's already localised, and continuing replacing characters
  // is likely to result in an incorrectly formatted spatial ref.
  if convertedRef <> ASpatialRef then begin
    // Deal with ListSeparator, ',' in english.
    convertedRef := StringReplace(convertedRef, ListSeparator, ',', [rfReplaceAll]);
    // And now deal with DecimalSeparator, '.' replaced by #1 earlier.
    convertedRef := StringReplace(convertedRef, #1, '.', [rfReplaceAll]);
  end;
  Result := convertedRef;
end;

{-------------------------------------------------------------------------------
  Works out the size of the grid square represented by the spatial reference and
  returns a populate TGridSquare record with the relevant information.
  If grid square precision is not -1, use it as specified. Otherwise, try figuring it out.
}
function SpatialRefToGridSquare(const ASpatialRef, ASystem: String; APrecision: Integer): TGridSquare;
var
  precision: Integer;
  eastNorth: TMapCoord;
  systemUsed: String;
begin
  systemUsed := ASystem;
  if APrecision = -1 then
    try
      precision := GetSpatialRefPrecision(ASpatialRef, systemUsed);
    except
      // Get here if SR system is not same as ASystem.
      on ESpatialRefError do begin
        systemUsed := DetermineSpatialRefSystem(ASpatialRef);
        // If it fails again, there is a bigger problem with that SpatialRef.
        precision := GetSpatialRefPrecision(ASpatialRef, systemUsed);
      end;
    end
  else
    precision := APrecision;

  eastNorth := SpatialRefToSpecificEastNorth(ASpatialRef, systemUsed, ASystem);
  Result.BottomLeft  := SpecificENToLatLong(eastNorth, ASystem);
  Result.BottomRight := SpecificENToLatLong(MapCoord(eastNorth.x + precision, eastNorth.y), ASystem);
  Result.TopLeft     := SpecificENToLatLong(MapCoord(eastNorth.x            , eastNorth.y + precision), ASystem);
  Result.TopRight    := SpecificENToLatLong(MapCoord(eastNorth.x + precision, eastNorth.y + precision), ASystem);
  Result.Precision   := precision;
  Result.System      := systemUsed;
end;

{-------------------------------------------------------------------------------
  Create a polygon region out of easting/northing from the given grid square
  on to the target system.
  Have to use polygon region as the grid square may not be really square.
}
function GridSquareToRegion(square: TGridSquare; const system: String): HRgn;
var
  polyPoints: Array[0..3] of TPoint;

  function GetPoint(latLong: TLatLong): TPoint;
  var
    eastNorth: TMapCoord;
  begin
    eastNorth := LatLongToSpecificEN(latLong, system);
    Result := Point(Round(eastNorth.x), Round(eastNorth.y));
  end;

begin
  polyPoints[0] := GetPoint(square.BottomLeft);
  polyPoints[1] := GetPoint(square.TopLeft);
  polyPoints[2] := GetPoint(square.TopRight);
  polyPoints[3] := GetPoint(square.BottomRight);
  Result := CreatePolygonRgn(polyPoints[0], 4, ALTERNATE);
end;

{-------------------------------------------------------------------------------
  Work out, or try to, the size of the grid square represented by the given
  spatial reference. Returned size in metres, I think.
}
function GetSpatialRefPrecision(const ASpatialRef, ASystem: String): Integer;
begin
  if Tetrad(ASpatialRef) then
    Result := 2000
  else
  // Even number of chars in OSGB spatial refs, odd in OSNI.
  if (SameText(ASystem, OS_GB) and (Length(ASpatialRef) mod 2 = 0)) or
     (SameText(ASystem, OS_NI) and (Length(ASpatialRef) mod 2 = 1)) then
  begin
    case Length(ASpatialRef) of
       1,  2: Result := 100000;
       3,  4: Result := 10000;
       5,  6: Result := 1000;
       7,  8: Result := 100;
       9, 10: Result := 10;
      11, 12: Result := 1;
    else
      raise ESpatialRefError.Create(ResStr_InvalidSpRefFormat);
    end;
  end else
    // TODO? Implement function in ISpatialReference for addins to contribute?
    raise ESpatialRefError.Create(ResStr_UnrecognisedSystem);
end;

{-------------------------------------------------------------------------------
}
function SpatialSystemMaxCommonSamplingSquareSize(const ASystem: String): Integer;
begin
  if SameText(ASystem, OS_GB) or SameText(ASystem, OS_NI) then
    Result := 10000
  else
    // TODO? Implement function in ISpatialReference for addins to contribute?
    raise ESpatialRefError.Create(ResStr_UnrecognisedSystem);
end;

{-------------------------------------------------------------------------------
}
function MapCoord(X, Y: Double): TMapCoord;
begin
  Result.X := X;
  Result.Y := Y;
end;

//==============================================================================
initialization
  // set flag so that bouding boxes are reported as beyond limits, if too big.
  tfAcceptLargerBoundingBox := false;

end.
