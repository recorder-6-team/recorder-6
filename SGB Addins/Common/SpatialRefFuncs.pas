{ Unit containing global methods for spatial reference conversion to and from
lat long. }

unit SpatialRefFuncs;

interface

uses
  Windows, SysUtils, Classes, Math, Dialogs, Recorder2000_TLB, 
  StdCtrls;

const
  { Standard reference systems }
  LAT_LONG = 'LTLN';
  LAT_LONG_TITLE = 'Latitude and Longitude';
  UTM = 'UTM';
  UTM_TITLE = 'UTM';
  OS_GB = 'OSGB';
  OS_GB_TITLE = 'Ordnance Survey - GB Grid';
  OS_NI = 'OSNI';
  OS_NI_TITLE = 'Ordnance Survey - Irish Grid';
  SYSTEM_UNKNOWN = 'Grid System Unknown';

  { Validation Constants }
  MIN_LAT = -90;
  MAX_LAT = 90;
  MIN_LONG = -180;
  MAX_LONG = 180;

  MIN_OSGB_EASTING = 0;
  MIN_OSGB_NORTHING = 0;
  MAX_OSGB_EASTING = 699999;
  MAX_OSGB_NORTHING = 1299999;

  MIN_OSIG_EASTING = 0;
  MIN_OSIG_NORTHING = 0;
  MAX_OSIG_EASTING = 500000;
  MAX_OSIG_NORTHING = 500000;
  MIN_OSIG_PREFIX = 0;
  MAX_OSIG_PREFIX = 44;

  MIN_UTM_ZONE = 0;
  MAX_UTM_ZONE = 61;
  MIN_UTM_EASTING = 0;
  MAX_UTM_EASTING = 999999;
  MIN_UTM_NORTHING = 0;
  MAX_UTM_NORTHING = 9999999;

  LAT_LONG_SIGFIGS = 5;

  { Error Messages }
  EST_INVALID_SPATIAL_REF_FORMAT = 'The spatial reference is not recognised.';
  EST_CONVERT_STRING_ERROR = 'The string passed to this function cannot be converted to an integer';
  EST_OUTSIDE_SYSTEM = 'The spatial reference lies beyond the limits of the current spatial reference system';
  EST_NO_COM_INTERFACES = 'COM inteface list not initialised.';
  EST_NO_COM_SYSTEMS = 'COM systems list not initialised.';
  EST_UNRECOGNISED_SYSTEM = 'The spatial reference system cannot be recognised.';
  EST_BOUNDINGBOX_INVALID = 'One or both bounding box corners is an invalid spatial reference : ';
  EST_SAME_SPATIAL_REF = 'The spatial reference of the sample must be identical to that of the survey event.';
  MSG_OUTSIDE_LIMITS = 'Beyond System Limits.';

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

type
  ESpatialRefError = class(Exception);
  EOutOfRangeError = class(Exception);

  TGridType = ( gtUK_Grid, gtIrish_Grid, gtUTM_Grid );

  TMapCoord = record
    x : double;
    y : double;
  end;

  TPrefix = record
    Numbers : string;
    Letters : string;
  end;

  TLatLong = record
    Lat  : string;
    Long : string;
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

  { PRIMARY FUNCTIONS

    These are usually called directly by the Source Code and in turn
    call various secondary functions}

function ConvertFromLatLong( iLatLong : TLatLong;
                             const iSystem : string ): string;
function ConvertToLatLong( const iSpatialRef, iSystem : string ): TLatLong;
function ConvertSpatialRefToEastNorth( const iGridRef : string; iSystem : string ):
                                      TPoint;
function EastingNorthingToSpatialRef( iEastingNorthing : TPoint;
                                      iSystem : String ): String;
function ValidSpatialRef( iSpatialRef : string;
                          const iCurrentSystem : string ): TValidSpatialRef;

function ValidSpecificSpatialRef( const iSpatialRef, iSpecificSystem : string ):
                                        TValidSpatialRef;
function DetermineSpatialRefSystem( iSpatialRef : String ): string;
function LatLongToSpecificEN( iLatLong : TLatLong; iSystem : string ) : TMapCoord;
function SpecificENToLatLong( iEastNorth : TMapCoord; iSystem : string ) : TLatLong;
function ConvertSystems( iSpatialRef, iSystemIn, iSystemOut : string ) : string;
function CurrentSpatialRefInterface( iSystem : string ) : ISpatialReference;

{ SUB_FUNCTIONS - "ConvertFromLatLong"
  to convert TLatLong strings to Grid References strings }
function LTLNToOSGB( iLatLong: TLatLong): 	string;
function LTLNToOSIG( iLatLong: TLatLong): 	string;
function LTLNToUTM( iLatLong: TLatLong): 	string;
function LTLNtoFormattedLTLN( iLatLong: TLatLong ) : string;

{ SUB_FUNCTIONS - "ConvertToLatLong"
  to convert Spatial Reference strings to TLatLongs }
function OSGBtoLTLN( iUKSpatialReference : string): TLatLong;
function OSIGToLTLN( iIrishSpatialReference : string): TLatLong;
function UTMToLTLN( iUTMSpatialReference : string): TLatLong;
function FormattedLTLNtoLTLN( iLTLNSpatialReference : string ) : TLatLong;

{ SUB_FUNCTIONS - "ConvertSpatialRefToEastNorth"
 Convert Grid References strings to Easting-Northing }
function OSGBSpatialRefToEN( iSpatialRef : string): TPoint;
function OSIGSpatialRefToEN( iSpatialRef : string): TPoint;
function UTMSpatialRefToEN( const iutmGridRef : string): TutmCoord;
function TetradToEN( const iTetrad : string ): TPoint;

{ SUB_FUNCTIONS - "EastingNorthingToSpatialRef"
  Convert Easting-Northing to Spatial ref string }
function EastingNorthingToOSGB( iEastingNorthing : TStringCoord): string;
function EastingNorthingToOSIG( iEastingNorthing : TStringCoord): string;
function EastingNorthingToUTM( iEastingNorthing : TStringCoord ): string;

{ SUB_FUNCTIONS - "ValidSpatialRef"
  Validtion of Individual Spatial Ref Strings }
function ValidUKSpatialRef( iSpatialRef :string ) : TValidSpatialRef;
function ValidIrishSpatialRef( iSpatialRef :string ) : TValidSpatialRef;
function ValidUTMSpatialRef( iSpatialRef :string ) : TValidSpatialRef;
function ValidLatLongSpatialRef( iSpatialRef : String ) : TValidSpatialRef;
function ValidLatLongRecord( iLatLong : TLatLong ) : Boolean;

{ MAIN PROJECTION CONVERSION FUNCTIONS - Not to be called directly }
function EastNorthToLatLong( iEasting, iNorthing : integer; iZone:integer): TMapCoord;
function LatLongToEastNorth( iLongitude, iLatitude : extended): TPoint;
function CalculateMeridian( iLatInRad : extended): extended;
procedure SetGridConstants( const iSystem : TGridType);
function DecodePrefixFromLetter( const iPrefix: string; iSystem : TGridType ): string;
function EncodePrefixToLetters( iNumericPrefix: string; iSystem : TGridType ): string;

{ Secondary Dataset system CONVERSION FUNCTIONS }
function OSGBMapCoordToLatLong( iEastingNorthing : TMapCoord ) :TLatLong;
function OSIGMapCoordToLatLong( iEastingNorthing : TMapCoord) : TLatLong;
function LatLongToOSIGEastNorth( iLatLong: TLatLong): TMapCoord;
function LatLongToOSGBEastNorth( iLatLong: TLatLong): TMapCoord;

{ Helper functions / procedures }
function PadWithZeros( iString: string; iLength: integer; iTrailing: boolean): string;
function RemoveCommas( iScale: String) : double;
function RemoveSubstrings( iString : string; iSubString : char ) : string;
function RemoveNonLLSubstrings( iString : string; iRemovePeriod, iRemoveMinus,
                                iRemovePlus : boolean ) : string;
function ValidInteger( iTestString : string ) : boolean;
function ValidLetter( iTestString : string ) : boolean;
function LatLong( const iLat, iLong : string ) : TLatLong;
function FormatLLStringAsTLatLong( iLLString : String ) : TLatLong;
function FormatTLatLongAsString( var iLatLong : TLatLong ) : String;
function Tetrad( const iSpatialRef : string ): boolean;

{validation functions}

function CheckBoundingBox( iSWSpatialRef, iNESpatialRef : string;
                           iCurrentSystem : string ) : TValidBoundingBox;
function CheckSpatialRefInBB( const RBL, RTR: TLatLong;
                              const iSpatialRef, iSystem: string): boolean;
function NextGridSquare( const iOldRef: string;
                         const iSize: integer; const iSystem: string): TPoint;
function CheckSRefInSRef( const iParentSR, iChildSR,
                          iParentSystem, iChildSystem: string ) : boolean;
function CompareEastingNorthings(iCoord1, iCoord2 : LongInt) : boolean;
procedure ValidateSpatialRefEntry(var iControl : TEdit; const iSystem : string; var iEnteredRef : string);

{ Functions to separate this unit from the project, so it DOES NOT use
  ApplicationSettings directly! }
procedure SetCurrentSpatialRefSystem( const NewValue:string );
procedure SetCommAddInLink( iSystems:TStringList; iInterfaces:TInterfaceList );

{ Functions to be used by SpatialRef component to get and set to the database }
function GetDisplaySpatialRef(const iDisplaySystem, iSpatialRef,
         iSpatialRefSystem, iLat, iLong, iFieldString : string) : string;
function DifferentEastingAndNorthing( const iSWSpatialRef, iNESpatialRef : string ) : boolean;
function ConvertOutOfRangeToSystemLimit( const iLatLong : TLatLong;
                                         const iSystem : string ) : string;
//==============================================================================
implementation

var
  { The following are only pointers to the actual objects.
    Don't create or free anything! }

  CurrentSpatialRefSystem : string = '';
  CurrentSpatialSystems : TStringList = nil;
  CurrentSpatialInterfaces : TInterfaceList = nil;


  //==============================================================================
procedure SetCommAddInLink(iSystems:TStringList; iInterfaces:TInterfaceList);
begin
  CurrentSpatialSystems   :=iSystems;
  CurrentSpatialInterfaces := iInterfaces;
end;  // SetCommAddInLink

//==============================================================================
procedure SetCurrentSpatialRefSystem(const NewValue:string);
begin
  CurrentSpatialRefSystem := NewValue;
end;  // SetCurrentSpatialRefSystem

{==============================================================================
    Function  Name: CurrentSpatialRefInterface
            Called: Many
           Purpose: To return the current Spatial Reference interface
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function CurrentSpatialRefInterface( iSystem : string ) : ISpatialReference;
var
  lIndex : integer;
begin
  if CurrentSpatialSystems <> nil then
  begin
    lIndex := CurrentSpatialSystems.IndexOf(iSystem);
    if lIndex <>-1 then // found one
    begin
      if CurrentSpatialInterfaces <> nil then
        Result := CurrentSpatialInterfaces[lIndex] as ISpatialReference;
    end
    else
      raise ESpatialRefError.Create( EST_UNRECOGNISED_SYSTEM );
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
function ConvertFromLatLong( iLatLong:TLatLong; const iSystem:string):string;
{var
  lComSystemIntf : ISpatialReference; }
begin
  { LAT-LONG }
  If not ValidLatLongRecord ( iLatLong ) then
    Result := EST_OUTSIDE_SYSTEM;

  if CompareText( iSystem, LAT_LONG ) = 0 then
    Result := LTLNtoFormattedLTLN( iLatLong )
  else
  { UTM Grid }
  if CompareText( iSystem, UTM ) = 0 then
  begin
    SetGridConstants( gtUTM_Grid );
    Result := LTLNtoUTM( iLatLong );
  end
  else
  { UK Grid }
  if CompareText( iSystem, OS_GB ) = 0 then
  begin
    SetGridConstants( gtUK_Grid );
    Result := LTLNtoOSGB( iLatLong );
  end
  else
  { Irish Grid }
  if CompareText( iSystem, OS_NI ) = 0 then
  begin
    SetGridConstants( gtIrish_Grid );
    Result := LTLNtoOSIG( iLatLong );
  end
  else  raise ESpatialRefError.Create(EST_INVALID_SPATIAL_REF_FORMAT );
  { check for a COM system }
  {begin
    lComSystemIntf := CurrentSpatialRefInterface( iSystem ) as iSpatialReference;
    Result := lComSystemIntf.ConvertFromLatLong( iLatLong.Lat, iLatLong.Long);
    if Result = '' then
      raise ESpatialRefError.Create( lComSystemIntf.GetLastError );
  end; // checking for com }
end;  // ConvertFromLatLong

{==============================================================================
    Function  Name: EastingNorthingToSpatialRef
            Called:
           Purpose: Conversion of an Easting and Northing (TPoint) to a Spatial
                    Reference string for any system
      Dependencies:
      Side Effects: Calls sub-functions EastingNorthingToUTM, EastingNorthingToOSGB,
                    EastingNorthingToOSIG
     Special Notes:
------------------------------------------------------------------------------}
function EastingNorthingToSpatialRef( iEastingNorthing : TPoint;
                                      iSystem : String ): String;
var
  {lIndex : integer;
  lComSystemIntf : ISpatialReference;}
  lStringCoord : TStringCoord;
begin
  lStringCoord.x := IntToStr( iEastingNorthing.x );
  lStringCoord.y := IntToStr( iEastingNorthing.y );

  { LONG-LAT }
  if CompareText( iSystem, LAT_LONG ) = 0 then
  begin
    // do nothing?
  end
  else

  { UTM }
    if CompareText( iSystem, UTM ) = 0 then
    begin
      SetGridConstants( gtUTM_Grid );
      Result := EastingNorthingToUTM( lStringCoord );
    end
    else

    { UK Grid }
      if CompareText( iSystem, OS_GB ) = 0 then
      begin
        SetGridConstants( gtUK_Grid );
        Result := EastingNorthingToOSGB( lStringCoord );
      end
      else

      { Irish Grid }
        if CompareText( iSystem, OS_NI ) = 0 then
        begin
          SetGridConstants( gtIrish_Grid );
          Result := EastingNorthingToOSIG( lStringCoord );
        end
        else
            raise ESpatialRefError.Create(EST_INVALID_SPATIAL_REF_FORMAT );

        { check for a COM system }
        {begin
          if CurrentSpatialSystems <> nil then
          begin
            lIndex := CurrentSpatialSystems.IndexOf(iSystem);
            if lIndex <>-1 then // found one
            begin
              if CurrentSpatialInterfaces <> nil then
              begin
                // Set a pointer to the current spatial reference system
                lComSystemIntf := CurrentSpatialInterfaces[lIndex] as ISpatialReference;
                //Result := lComSystemIntf. EastingNorthingToSpatialRef( iEastingNorthing);
                if Result = '' then
                  raise ESpatialRefError.Create( lComSystemIntf.GetLastError );
              end
              else
                raise ESpatialRefError.Create( EST_NO_COM_INTERFACES );
            end
            else
              Raise ESpatialRefError.Create( EST_UNRECOGNISED_SYSTEM + ' : ' + iSystem );
          end
          else
            raise ESpatialRefError.Create( EST_NO_COM_SYSTEMS );
        end; // of checking for a com system}
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
function ConvertToLatLong( const iSpatialRef, iSystem : string ): TLatLong;
{var
  lComSystemIntf : ISpatialReference;  }
begin
  if CompareText( iSystem, LAT_LONG ) = 0 then
    Result := FormattedLTLNtoLTLN( iSpatialRef )
  else
  { UTM }
  if CompareText(iSystem, UTM) = 0 then
  begin
    SetGridConstants( gtUTM_Grid );
    Result := UTMToLTLN( iSpatialref );
  end else
  { UK Grid }
  if CompareText( iSystem, OS_GB ) = 0 then
  begin
    SetGridConstants( gtUK_Grid );
    Result := OSGBtoLTLN( iSpatialRef );
  end else
  { Irish Grid }
  if CompareText( iSystem, OS_NI ) = 0 then
  begin
    SetGridConstants( gtIrish_Grid );
    Result := OSIGtoLTLN( iSpatialref );
  end else
    raise ESpatialRefError.Create(EST_INVALID_SPATIAL_REF_FORMAT );
  {begin
    lComSystemIntf := CurrentSpatialRefInterface( iSystem ) as iSpatialReference;
    Result.Lat := lComSystemIntf.ConvertToLat( iSpatialRef );
    if Result.Lat = '' then // raise the error message specified by the COM object
      raise ESpatialRefError.Create(lComSystemIntf.GetLastError);
    Result.Long := lComSystemIntf.ConvertToLong(iSpatialRef);
    if Result.Long = '' then // raise the error message specified by the COM object
      raise ESpatialRefError.Create(lComSystemIntf.GetLastError);
  end }
end;  // ConvertToLatLong

{==============================================================================
    Function  Name: ConvertSpatialRefToEastNorth
            Called:
           Purpose: Converts a Spatial Reference when passed in as a string to
                    an Easting-Northing TPoint of the same Spatial Reference
                    system type.
      Dependencies:
      Side Effects: Calls sub-functions UTMSpatialRefToEN, OSGBSpatialRefToEN,
                      OSIGSpatialRefToEN
     Special Notes:
------------------------------------------------------------------------------}
function ConvertSpatialRefToEastNorth( const iGridRef : string;
                                       iSystem : string ): TPoint;
var
  lUTMCoord : TUTMCoord;
begin
  if CompareText( iSystem, LAT_LONG ) = 0 then
  begin
    ESpatialRefError.Create( 'Conversion from Easting and Northing is not possible '
                                              +'for this spatial reference system.');
  end // if LatLong
  else
  { UTM }
    if CompareText(iSystem, UTM) = 0 then
    begin
      SetGridConstants( gtUTM_Grid );
      lUTMCoord := UTMSpatialRefToEN( iGridRef );
      Result.x := lUTMCoord.easting;
      Result.y := lUTMCoord.northing;
    end
    else
    { UK Grid }
      if CompareText(iSystem, OS_GB) = 0 then
      begin
        SetGridConstants( gtUK_Grid );
        Result := OSGBSpatialRefToEN( iGridRef );
      end
      else
      { Irish Grid }
        if CompareText(iSystem, OS_NI) = 0 then
        begin
          SetGridConstants( gtIrish_Grid );
          Result := OSIGSpatialReftoEN( iGridRef );
        end
        else
        begin // check for a COM system
          Raise ESpatialRefError.Create( 'Conversion function not possible for a COM system' );
        end;
end;  // ConvertSpatialRefToEastNorth

{==============================================================================
    Function  Name: LatLongToSpecificEN
            Called:
           Purpose: Primary Function which is called connvert from a lat long
                    to a Easting Northing of a specific spatial reference system
      Dependencies:
      Side Effects: Calls sub-functions LatLongToOSGBEastNorth,
                                        LatLongToOSIGEastNorth,
     Special Notes: Used because of datset setup being spatial reference system
                    specific.
------------------------------------------------------------------------------}
function LatLongToSpecificEN( iLatLong : TLatLong; iSystem : string ) : TMapCoord;
begin
  if CompareText( iSystem, LAT_LONG ) = 0 then
    ESpatialRefError.Create( 'Conversion from Easting and Northing is not possible '
                                    +'for this spatial reference system.')
  else
  { UTM }
    if CompareText(iSystem, UTM) = 0 then
      ESpatialRefError.Create( 'Conversion from Easting and Northing is not possible '
                                    +'for this spatial reference system.')
    else
    { UK Grid }
      if CompareText(iSystem, OS_GB) = 0 then
      begin
        Result := LatLongToOSGBEastNorth( iLatLong );
        exit;
      end
      else
      { Irish Grid }
        if CompareText(iSystem, OS_NI) = 0 then
        begin
          Result := LatLongToOSIGEastNorth( iLatLong );
          exit;
        end
        else
          Raise ESpatialRefError.Create( 'Conversion function not possible for a COM system' );

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
function SpecificENToLatLong( iEastNorth : TMapCoord; iSystem : string ) : TLatLong;
begin
  if CompareText( iSystem, LAT_LONG ) = 0 then
  begin
    raise ESpatialRefError.Create( 'Conversion from Easting and Northing is not possible '
                                              +'for this spatial reference system.');
  end // if LatLong
  else
  { UTM }
    if CompareText(iSystem, UTM) = 0 then
    begin
      raise ESpatialRefError.Create( 'Conversion from Easting and Northing is not possible '
                                              +'for this spatial reference system.');
    end // if LatLong
    else
    { UK Grid }
      if CompareText(iSystem, OS_GB) = 0 then
      begin
        Result := OSGBMapCoordToLatLong( iEastNorth );
        Exit;
      end
      else
      { Irish Grid }
        if CompareText(iSystem, OS_NI) = 0 then
        begin
          Result := OSIGMapCoordToLatLong( iEastNorth );
          Exit;
        end
        else
        begin // check for a COM system
          Raise ESpatialRefError.Create( 'Conversion function not possible for a COM system' );
        end;
end;

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
                          ValidUKSpatialRef and ValidIrishSpatialRef
     Special Notes:
------------------------------------------------------------------------------}
function ValidSpatialRef( iSpatialRef : string; const iCurrentSystem : string ):TValidSpatialRef;
var
  lInputSys : String;
  lConvertedSpatialRef : string;
  lValidCurrentSR, lValidConvertedSR : TValidSpatialRef;
  {lSpatialRefIntf : ISpatialReference;}
begin
  Result.Valid := false;
  { (1) Check for no value... this returns a "True" value. }
  if iSpatialRef = '' then
  begin
    Result.valid := true;
    Result.FormattedSR := iSpatialRef;
    Result.Error := '';
    exit;
  end
  else
  { (2) Check that the Spatial Ref isn't beyond the limits of the system }
    if CompareText( iSpatialRef, MSG_OUTSIDE_LIMITS ) = 0 then
    begin
      Result.valid := false;
      Result.Error := EST_OUTSIDE_SYSTEM;
      exit;
    end;

  { (3) Can the Input system be recognised as one of the 4 default systems? }
  lInputSys := DetermineSpatialRefSystem( iSpatialRef );
  If lInputSys <> SYSTEM_UNKNOWN then
  begin
    { Get the valid Spatial Reference in the entered system }
    lValidCurrentSR := ValidSpecificSpatialRef( iSpatialRef, lInputSys );
    Result.EnteredFormattedSR := lValidCurrentSR.FormattedSR;

    { If not entered in current system, then Convert to current system }
    If ( lInputSys <> iCurrentSystem ) then
    begin
      { Convert spatial reference into the current system }
      lConvertedSpatialRef := ConvertSystems( iSpatialRef, lInputSys, iCurrentSystem );
      { If it lies outside the current system (i.e. long lat from outside the
        UK converted into a UK spatial reference ) }
      if lConvertedSpatialRef <> MSG_OUTSIDE_LIMITS then
        lValidConvertedSR := ValidSpecificSpatialRef( lConvertedSpatialRef, iCurrentSystem )
      else
      begin
        { If it lies outside the current system (i.e. long lat from outside the
          UK converted into a UK spatial reference ) }
        Result.valid := True;
        Result.FormattedSR := MSG_OUTSIDE_LIMITS;
        Result.Error := EST_OUTSIDE_SYSTEM;
        Exit;
      end;
    end
    else
      lValidConvertedSR := lValidCurrentSR;
  {end // if input system is one of the recognised syetems
  else
  begin
    // Check whether the current system is a COM system
    If iCurrentSystem <> SYSTEM_UNKNOWN then
      try
        lSpatialRefIntf := CurrentSpatialRefInterface( iCurrentSystem );
        If not lSpatialRefIntf.ValidSpatialRef( iSpatialRef ) then
        begin
          Result.Valid := False;
          Result.FormattedSR := MSG_OUTSIDE_LIMITS;
          Result.Error := EST_UNRECOGNISED_SYSTEM;
          Exit;
        end
        else
        begin
          Result.Valid := true;
          Result.EnteredFormattedSR := iSpatialRef;
          Result.FormattedSR := iSpatialRef;
          exit;
        end;
      except
        on ESpatialRefError do // catch exceptions and return invalid
        begin
          Result.Valid := False;
          Result.FormattedSR := MSG_OUTSIDE_LIMITS;
          Result.Error := EST_UNRECOGNISED_SYSTEM;
          Exit;
        end;
      end; // try..except  }
  end; // if Input System has not been recognised

  If lValidConvertedSR.FormattedSR = MSG_OUTSIDE_LIMITS then
  begin
    Result.Valid := False;
    Result.FormattedSR := MSG_OUTSIDE_LIMITS;
    Result.Error := EST_OUTSIDE_SYSTEM;
    Exit;
  end;
  Result.EnteredFormattedSR := lValidCurrentSR.FormattedSR;
  Result.FormattedSR := lValidConvertedSR.FormattedSR;
  Result.Valid := true;
  Result.Error := lValidConvertedSR.error;
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
function ValidSpecificSpatialRef( const iSpatialRef, iSpecificSystem : string ):
                                                      TValidSpatialRef;
{var
  lSpatialRefIntf : ISpatialReference;}
begin
  if CompareText( iSpecificSystem, LAT_LONG ) = 0 then
  begin
    Result := ValidLatLongSpatialRef( iSpatialRef );
    exit;
  end

  else
  { UTM }
  if CompareText( iSpecificSystem, UTM) = 0 then
  begin
    Result := ValidUTMSpatialRef( iSpatialRef );
    exit;
  end
  else
  { UK Grid }
  if CompareText( iSpecificSystem, OS_GB ) = 0 then
  begin
    Result := ValidUKSpatialRef( iSpatialRef );
    exit;
  end
  else
  { Irish Grid }
  if CompareText( iSpecificSystem, OS_NI ) = 0 then
  begin
    Result := ValidIrishSpatialRef( iSpatialRef );
    exit;
 { end
  else
  // COM System
  begin
    lSpatialRefIntf := CurrentSpatialRefInterface( iSpecificSystem );
    If lSpatialRefIntf.ValidSpatialRef( iSpatialRef ) then
    begin
      Result.EnteredFormattedSR := iSpatialRef;
      Result.FormattedSR := iSpatialRef;
      Result.Valid := True;
    end
    else
      Result.EnteredFormattedSR := iSpatialRef;  }
  end;
end;

{==============================================================================
    Function  Name: DetermineSpatialRefSystem
            Called: Various
           Purpose: The function queries spatial reference stings in order
                    to assertain which system the string is a spatial reference
                    for.
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function DetermineSpatialRefSystem( iSpatialRef : String ): string;
var
  lSpatialRef : string;
  {lTempIntf : ISpatialReference;
  lCount : integer; }
begin
  { Check for initial letters }
  lSpatialRef := UpperCase(RemoveSubstrings( iSpatialRef, ' '));

  { Is it a UK - ie. two letters at the beginning? }
  If ( ValidLetter( Copy( lSpatialRef, 1, 2 ) ) and
       ValidInteger( Copy ( lSpatialRef, 3, Length(lSpatialRef) ) ) and
       (Odd(Length(Copy ( lSpatialRef, 3, Length(lSpatialRef) ))) = false) and
       (Length(lSpatialRef) > 1) )
       or
     ( (Length(lSpatialRef) = 5) and
       ValidLetter(Copy(lSpatialRef, 1, 2 )) and
       ValidInteger( Copy( lSpatialRef, 3, 2 )) and
       ValidLetter( Copy( lSpatialRef, 5, 1 )) ) then
  begin
    Result := OS_GB;
    exit;
  end;
  If ( ValidLetter( Copy( lSpatialRef, 1, 1 ) ) and
     ValidInteger( Copy ( lSpatialRef, 2, Length(lSpatialRef) ) ) and
     ( Odd(Length(Copy ( lSpatialRef, 2, Length(lSpatialRef) ))) = false) and
     (Length(lSpatialRef) > 0) )
       or
     ( (Length(lSpatialRef) = 4) and
       ValidLetter(Copy(lSpatialRef, 1, 1 )) and
       ValidInteger( Copy( lSpatialRef, 2, 2 )) and
       ValidLetter( Copy( lSpatialRef, 4, 1 )) ) then
  begin
    Result := OS_NI;
    exit;
  end else
  { If it is all numbers, with two spaces, it could be a UTM }
    If ValidInteger( lSpatialRef ) and
       ( Length( lSpatialRef ) in [13..15] ) then
    begin
      Result := UTM;
      exit;
    end

    else
    { If it has an 'E' and a 'N' then it could be a LongLat }
      If (( Pos('E', iSpatialRef ) >0 ) or ( Pos('W', iSpatialRef ) >0 )) and
        (( Pos('N', iSpatialRef ) >0 ) or ( Pos('S', iSpatialRef ) > 0 )) then
      begin
        Result := LAT_LONG;
        exit;
     { end
      else
      // Itterate through the com systems, validating until a 'true' is returmed
      begin
        For lCount := 0 to CurrentSpatialInterfaces.Count - 1 do
        begin
          lTempIntf := CurrentSpatialInterfaces.items[lCount] as iSpatialReference;
          if lTempIntf.ValidSpatialRef( iSpatialRef ) then
          begin
            Result := lTempIntf.SpatialRefSystem;
            exit;
          end;
        end; }
      end; // iterating through COM iSpatialReference interfaces


    Result := SYSTEM_UNKNOWN;
end; // DetermineSpatialRefSystem

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
  lEastNorth : TPoint;
  lEastNorthString : TStringCoord;
begin
  { Convert from 'E'/'W' and 'N'/'S' to '+' and '-' }
  lEastNorth := LatLongToEastNorth( StrToFloat( iLatLong.Long),
                                    StrToFloat( iLatLong.Lat));

  lEastNorthString.x := InttoStr( lEastNorth.x);
  lEastNorthString.y := InttoStr( lEastNorth.y);
  Result := EastingNorthingToOSGB( lEastNorthString );
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
function LTLNToOSIG( iLatLong: TLatLong ): 	string;
var
  lEastNorth : TPoint;
  lEastNorthString : TStringCoord;
  lLatLongFloat : TMapCoord;

begin
  { Convert from 'E'/'W' and 'N'/'S' to '+' and '-' }

  { Call the procedure }
  lLatLongFloat.x := StrToFloat(iLatLong.Long);
  lLatLongFloat.y := StrToFloat(iLatLong.Lat);
  lEastNorth := LatLongToEastNorth(lLatLongFloat.x, lLatLongFloat.y);

  { Convert Tpoint to a String }
  lEastNorthString.x := InttoStr(lEastNorth.x);
  lEastNorthString.y := InttoStr(lEastNorth.y);
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
function LTLNToUTM( iLatLong : TLatLong ): string;
var
  lEastNorth : TPoint;
  lZone : integer;
  lLatLongFloat : TMapCoord;
begin
  { Interpret strings as floats }
  lLatLongFloat.x := StrToFloat( iLatLong.Long );
  lLatLongFloat.y := StrToFloat( iLatLong.Lat );

  { UTM is a special case in the conversion algorithms and for southern
    latitudes and western longitudes needs a different false origin }
  if ( lLatLongFloat.y < 0 ) then
    nZero := -10000000
  else
    nZero := 0;
  LongTrueOrigin := (trunc(abs(lLatLongFloat.x)/6)*6 +3);
  if lLatLongFloat.x < 0 then
    LongTrueOrigin := -LongTrueOrigin;

  lZone := trunc((lLatLongFloat.x/6)+31);
  lEastNorth := LatLongToEastNorth( lLatLongFloat.x, lLatLongFloat.y );

  { Format as a single string }
  Result := IntToStr(lZone) + ' ' + InttoStr( lEastNorth.x ) + ' '
                                  + InttoStr( lEastNorth.y );
end;

{==============================================================================
    Function  Name: LTLNtoFormattedLTLN
            Called: ConvertToLatLong
           Purpose: Conversion of a TLatLong string (ie. '52.543661', '2.343253')
                    To a LatLong Spatial Ref (ie. '31.11E, 21.22W')
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function LTLNtoFormattedLTLN( iLatLong: TLatLong ) : string;
var
  lLLDouble : TMapCoord;
  lLLString : TLatLong;
begin
  lLLDouble.x := abs( StrToFloat( iLatLong.Long ));
  lLLDouble.y := abs( StrToFloat( iLatLong.Lat ));

  lLLString.Long := Copy( FloatToStr( lLLDouble.x ), 1, LAT_LONG_SIGFIGS );
  lLLString.Lat := Copy( FloatToStr( lLLDouble.y ), 1, LAT_LONG_SIGFIGS );

  If StrToFloat( iLatLong.Lat ) < 0 then
    lLLString.Lat := lLLString.Lat + 'S'
  else
    lLLString.Lat := lLLString.Lat + 'N';

  If StrToFloat( iLatLong.Long ) < 0 then
    lLLString.Long := lLLString.Long + 'W'
  else
    lLLString.Long := lLLString.Long + 'E';

  Result := lLLString.lat + ', ' + lLLString.Long;
end;

{==============================================================================
    Function  Name: OSGBtoLTLN
            Called: ConvertToLatLong
           Purpose: Conversion of a OSGB Spatial Reference string
                    (ie. SV 1234 5678)
                    to a TLatLong record (ie. '52.543661', '2.343253')
      Dependencies:
      Side Effects:
     Special Notes: Primarily a sub-function called by ConvertToLatLong
                    also called by Wizard and GeneralData
------------------------------------------------------------------------------}
function OSGBtoLTLN( iUKSpatialReference: string ): TLatLong;
var
  lLatLong : TMapCoord;
  lFormatedGR : TPoint;
begin
  { Convert string to a TPoint }
  lFormatedGR := OSGBSpatialRefToEN( iUKSpatialReference );
  { Convert TPoint to  Lat Long }
  lLatLong := EastNorthToLatLong( lFormatedGR.x, lFormatedGR.y, 0 );
  Result.Lat := FloatToStr( lLatLong.y );
  Result.Long := FloatToStr( lLatLong.x );
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
function OSIGToLTLN( iIrishSpatialReference: string ): TLatLong;
var
  lLatLong : TMapCoord;
  lFormatedGR : TPoint;
begin
  { Convert string to a TPoint }
  lFormatedGR := OSIGSpatialReftoEN( iIrishSpatialReference );
  { Convert TPoint to  Lat Long }
  lLatLong := EastNorthToLatLong( lFormatedGR.x, lFormatedGR.y, 0);
  Result.Lat := FloatToStr( lLatLong.y );
  Result.Long := FloatToStr( lLatLong.x );
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
function UTMToLTLN( iUTMSpatialReference: string): TLatLong;
var
  lLatLong : TMapCoord;
  lFormatedGR : TutmCoord;
begin
  { Convert string to a TPoint }
  lFormatedGR := UTMSpatialRefToEN( iUTMSpatialReference );
  { Convert TPoint to  Lat Long }
  lLatLong := EastNorthToLatLong( lFormatedGR.easting, lFormatedGR.northing,
                                   lFormatedGR.zone );
  Result.long := FloatToStr( lLatLong.x );
  Result.lat := FloatToStr( lLatLong.y );
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
function FormattedLTLNtoLTLN( iLTLNSpatialReference : string ) : TLatLong;
var
  lSpatialRef : string;
  lChar : char;
  lFirstHalf, lSecondHalf : string;
  lFirstSuffix, lSecondSuffix : string;
  lCount : integer;

begin
  { Remove unwanted characters }
  lSpatialRef := RemoveNonLLSubstrings( iLTLNSpatialReference, false, false, false );
  { Separate into N/S and E/W }
  For lCount := 1 to Length( lSpatialRef ) do
  begin
    lChar := lSpatialRef[lCount];
    If ( lChar in ['A'..'Z'] ) then
    begin
      lFirstHalf := copy( lSpatialRef, 1, lCount );
      lSecondHalf := Copy (lSpatialRef, lCount+1, Length( lSpatialRef ));
      break;
    end;
  end;

  { Manipulate so that always North and East
                       always without letters.. using +/- instead }
  lFirstSuffix := char( lFirstHalf[Length(lFirstHalf)] );
  lSecondSuffix := char( lSecondHalf[Length(lSecondHalf)]);
  lFirstHalf := copy( lFirstHalf, 1, Length(lFirstHalf) -1 );
  lSecondHalf := copy( lSecondHalf, 1, Length(lSecondHalf) -1 );

  If (lFirstSuffix = 'N') then
    Result.Lat :=  lFirstHalf;
  If (lFirstSuffix = 'S') then
    Result.Lat :=  FloatToStr(-1 * StrToFloat(lFirstHalf));
  If (lFirstSuffix = 'E') then
    Result.Long :=  lFirstHalf;
  If (lFirstSuffix = 'W') then
    Result.Long :=  FloatToStr(-1 * StrToFloat(lFirstHalf));
  If (lSecondSuffix = 'N') then
    Result.Lat :=  lSecondHalf;
  If (lSecondSuffix = 'S') then
    Result.Lat :=  FloatToStr(-1 * StrToFloat(lSecondHalf));
  If (lSecondSuffix = 'E') then
    Result.Long :=  lSecondHalf;
  If (lSecondSuffix = 'W') then
    Result.Long :=  FloatToStr(-1 * StrToFloat(lSecondHalf));
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
  to an Easting and Northing TPoint record (ie. 312343, 436543) }

function OSIGSpatialRefToEN( iSpatialRef: string): TPoint;
var
  lFirstLetter : string;
  lPrefixAsNumber : string;
  lHalfLength : integer;
  lTempString : TStringCoord;
  lSuffix : String;
  lSpatialRef : String;
begin
  lSpatialRef := RemoveSubstrings( iSpatialRef, ' ' );

  { Get the first letter.. if there }
  lFirstLetter := copy( lSpatialRef, 1, 1 );
  If not ValidInteger( lFirstLetter ) then
  begin
    { Convert Prefix }
    lPrefixAsNumber := DecodePrefixFromLetter( lFirstLetter, gtIrish_Grid );
    { Seperate the suffix }
    lSuffix := Copy( lSpatialRef, 2, ( length( lSpatialRef )));
  end;

  If not Tetrad( lSpatialRef ) then
  begin
    { Find out how much precision there is in the Grid Ref }
    lHalfLength := ( Length( lSuffix )) div 2;

    lTempString.x := copy( lSuffix, 1 , lHalfLength );
    lTempString.y := copy( lSuffix, lHalfLength + 1, lHalfLength + 2 );

    { Add the extra zeros to the suffix make a 6 figure E / N }
    lTempString.x := PadWithZeros( lTempString.x, 5, True );
    lTempString.y := PadWithZeros( lTempString.y, 5, True );

    { Split up and add the individual easting and northing }
    lTempString.x := copy( lPrefixAsNumber, 1, 1) + lTempString.x;
    lTempString.y := copy( lPrefixAsNumber, 2, Length( lPrefixAsNumber ))
                     + lTempString.y;

    { Add some zeros }
    lTempString.x := PadWithZeros( lTempString.x, 6, True );
    lTempString.y := PadWithZeros( lTempString.y, 6, True );

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
  to an Easting and Northing TPoint record (ie. 312343, 436543) }

function OSGBSpatialRefToEN( iSpatialRef: string): TPoint;
var
  lSuffix : string;
  lFirstLetters : string;
  lPrefixAsNumber : string;
  lHalfLength : integer;
  lTempString : TStringCoord;
  lSpatialRef : String;
begin
  { Remove any spaces }
  lSpatialRef := RemoveSubstrings( iSpatialRef , ' ');

  { Decode the prefix }
  lFirstLetters := copy( lSpatialRef, 1, 2 );
  lPrefixAsNumber := DecodePrefixfromLetter( lFirstLetters, gtUK_Grid );
  lSuffix := Copy( lSpatialRef, 3, ( length( lSpatialRef )) -2 );

  If not Tetrad( lSpatialRef ) then
  begin
    { Find out how much precision there is in the Grid Ref }
    lHalfLength := ( Length (  lSuffix )) div 2;

    lTempString.x := copy( lSuffix, 1 , lHalfLength );
    lTempString.y := copy( lSuffix, lHalfLength + 1, lHalfLength + 2 );

    { Add the extra zeros to the suffix to make a 6 figure suffix }
    lTempString.x := PadWithZeros( lTempString.x, 5, true );
    lTempString.y := PadWithZeros( lTempString.y, 5, true );

    { Split up and add the individual easting and northing }
    lTempString.x := copy( lPrefixAsNumber, 1, 1) + lTempString.x;
    lTempString.y := copy( lPrefixAsNumber, 2, Length( lPrefixAsNumber ))
                           + lTempString.y;

    { Add some zeros }
    lTempString.x := PadWithZeros( lTempString.x, 6, True );
    lTempString.y := PadWithZeros( lTempString.y, 6, True );

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
  to an Easting and Northing TPoint record (ie. 312343, 436543) }

function UTMSpatialRefToEN( const iutmGridRef : string): TutmCoord;
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
  lSpace := pos( ' ', lutmSpatialRef );
  lZoneStr := copy( lutmSpatialRef ,1,lSpace-1);
  lutmSpatialRef := copy( lutmSpatialRef, lSpace + 1, Length( lutmSpatialRef ));

  { Split off Easting }
  lSpace := pos( ' ', lutmSpatialRef );
  lLength := length( lutmSpatialRef );
  lEastingStr := copy( lutmSpatialRef, 1,lSpace - 1 );
  lNorthingStr := copy( lutmSpatialRef, lSpace + 1,lLength );

  Result.easting := StrToInt( lEastingStr );
  Result.northing := StrToInt( lNorthingStr );
  Result.zone := StrToInt( lZoneStr );
end;

{==============================================================================
    Procedure Name: TetradSuffixToEN
            Called:
           Purpose: When a Tetrad is Passed in, this function will convert it
                    to sn easting and northing.
      Dependencies:
     Special Notes:
------------------------------------------------------------------------------}
function TetradToEN( const iTetrad : string ): TPoint;
var
  lPrefixAsNumber : string;
  lTetrad : string;
  lSyst : string;
  lDecodedSuffix : string;
  lStringResult : TStringCoord;
begin
  { Remove any spaces }
  lTetrad := RemoveSubstrings( iTetrad , ' ');
  { Determine the 100km characters for the easting and northing }
  lSyst := DetermineSpatialRefSystem( lTetrad );
  If lSyst = OS_GB then
    lPrefixAsNumber := DecodePrefixfromLetter( copy( lTetrad, 1, 2 ),
                                               gtUK_Grid )
  else
    If lSyst = OS_NI then
      lPrefixAsNumber := DecodePrefixfromLetter( copy( lTetrad, 1, 2 ),
                                                 gtIrish_Grid );

  { The x-coordinate for 100s of km is only ever one character however the
    y-coordinate can be 2 characters (because of the shape of the UK - only
    800km wide but 1300km north-south).  The following lines thus split the
    decoded prefix appropriately }
  lStringResult.x := copy( lPrefixAsNumber, 1, 1);
  lStringResult.y := copy( lPrefixAsNumber, 2, length( lPrefixAsNumber) -1 );

  { Add the numerical part of the Tetrad }
  lStringResult.x := lStringResult.x + copy( lTetrad, Length(lTetrad)-2, 1);
  lStringResult.y := lStringResult.y + copy( lTetrad, Length(lTetrad)-1, 1);

  { Decode according to the Irish grid because we are only passing in one letter
    and only looking on a 5 x 5 grid for the index coordinates of that letter }
  lDecodedSuffix := DecodePrefixFromLetter( Copy( lTetrad, length(lTetrad), 1),
                                            gtIrish_Grid );
  { Multiply each part by 2000 to give the northing / easting of the bottom
    left corner of the 2km box in metres }
  lStringResult.x := lStringResult.x +
                     PadWithZeros( IntToStr( StrToInt( copy(lDecodedSuffix, 1, 1)) * 2000 ), 4, true ) ;
  lStringResult.y := lStringResult.y +
                     PadWithZeros( IntToStr( StrToInt( copy(lDecodedSuffix, 2, 1)) * 2000 ), 4, true );

  Result.x := StrToInt( lStringResult.x);
  Result.y := StrToInt( lStringResult.y);
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
function EastingNorthingToOSGB( iEastingNorthing : TStringCoord ): string;
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
  if ( StrToInt( iEastingNorthing.x) < MIN_OSGB_EASTING ) then
  begin
    iEastingNorthing.x := IntToStr(MIN_OSGB_EASTING);
    ltfRangeError := true;
  end;
  if ( StrToInt( iEastingNorthing.x) > MAX_OSGB_EASTING ) then
  begin
    iEastingNorthing.x := IntToStr(MAX_OSGB_EASTING);
    ltfRangeError := true;
  end;
  if ( StrToInt( iEastingNorthing.y) < MIN_OSGB_NORTHING ) then
  begin
    iEastingNorthing.y := IntToStr(MIN_OSGB_NORTHING);
    ltfRangeError := true;
  end;
  if ( StrToInt( iEastingNorthing.y) > MAX_OSGB_NORTHING ) then
  begin
    iEastingNorthing.y := IntToStr(MAX_OSGB_NORTHING);
    ltfRangeError := true;
  end;
  lLength.x := length( iEastingNorthing.x );
  lLength.y := length( iEastingNorthing.y );

  If lLength.x > lLength.y then
    lNumFigs := lLength.x
  else
    lNumFigs := lLength.y;

  { In case of strings smaller than 6 characters }
  if lNumFigs < 6 then
    lNumFigs := 6;

  { Pad with zeros to make both parts lNumFigs characters long }
  lEastNorth.x := PadWithZeros( iEastingNorthing.x, lNumFigs, False );
  lEastNorth.y := PadWithZeros( iEastingNorthing.y, lNumFigs, False );

  { Seperate the prefix by removing the last 5 numbers of each half
    which correspond to the non-prefix component }
  lPrefixStr := copy( lEastNorth.x, 1, lNumFigs - 5 ) +
                copy( lEastNorth.y, 1, lNumFigs - 5 );

  { Find the appropriate prefix letters }
  lPrefixStr := EncodePrefixToLetters( lPrefixStr, gtUK_Grid );

  { Format the rest of the string }
  lSuffix.x := copy( lEastNorth.x, lNumFigs - 4, lNumFigs );
  lSuffix.y := copy( lEastNorth.y, lNumFigs - 4, lNumFigs );

  Result := lPrefixStr + ' ' + lSuffix.x + ' ' + lSuffix.y;
  If ltfrangeError then
    raise EOutOfRangeError.Create(EST_OUTSIDE_SYSTEM);
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
function EastingNorthingToOSIG( iEastingNorthing : TStringCoord ): string;
var
  lPrefixStr : String;
  lLength : TPoint;
  lEastNorth : TStringCoord;
  lSuffix : TStringCoord;
  lNumFigs : integer;
begin
  { Check for valid eastings and northings }
  if ( StrToInt( iEastingNorthing.x ) < MIN_OSIG_EASTING ) or
     ( StrToInt( iEastingNorthing.x ) > MAX_OSIG_EASTING ) or
     ( StrToInt( iEastingNorthing.y ) < MIN_OSIG_NORTHING ) or
     ( StrToInt( iEastingNorthing.y ) > MAX_OSIG_NORTHING ) then
  begin
    Result := MSG_OUTSIDE_LIMITS;
    exit;
  end;
  lLength.x := length( iEastingNorthing.x );
  lLength.y := length( iEastingNorthing.y );

  If lLength.x > lLength.y then
    lNumFigs := lLength.x
  else
    lNumFigs := lLength.y;

  { In case of strings smaller than 6 characters }
  if lNumFigs < 6 then
    lNumFigs := 6;

  { Pad with zeros to make both parts lNumFigs characters long }
  lEastNorth.x := PadWithZeros( iEastingNorthing.x, lNumFigs, False );
  lEastNorth.y := PadWithZeros( iEastingNorthing.y, lNumFigs, False );

  { Seperate the prefix by removing the last 5 numbers of each half
    which correspond to the non-prefix component }
  lPrefixStr := copy( lEastNorth.x, 1, lNumFigs - 5 ) +
                copy( lEastNorth.y, 1, lNumFigs - 5 );

  { Find the appropriate prefix letters }
  lPrefixStr := EncodePrefixToLetters( lPrefixStr, gtIrish_Grid );

  { Format the rest of the string }
  lSuffix.x := copy( lEastNorth.x, lNumFigs - 4, lNumFigs );
  lSuffix.y := copy( lEastNorth.y, lNumFigs - 4, lNumFigs );

  Result := lPrefixStr + ' ' + lSuffix.x + ' ' + lSuffix.y;
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
function EastingNorthingToUTM( iEastingNorthing : TStringCoord ): string;
var
  lPrefixStr : String;
  lLength : TPoint;
  lSuffix : TStringCoord;
begin
  { Check for valid eastings and northings }
  if ( StrToInt( iEastingNorthing.x ) < MIN_UTM_EASTING ) or
     ( StrToInt( iEastingNorthing.x ) > MAX_UTM_EASTING ) or
     ( StrToInt( iEastingNorthing.y ) < MIN_UTM_NORTHING ) or
     ( StrToInt( iEastingNorthing.y ) > MAX_UTM_NORTHING ) then
  begin
    Result := MSG_OUTSIDE_LIMITS;
    exit;
  end;

  lLength.x := length( iEastingNorthing.x );
  lLength.y := length( iEastingNorthing.y );

  lPrefixStr := copy( iEastingNorthing.x, 1, 1 ) +
                copy( iEastingNorthing.y, 1, 1 );

  lSuffix.x := copy( iEastingNorthing.x, 2, lLength.x );
  lSuffix.y := copy( iEastingNorthing.y, 2, lLength.y );

  { Add zeros to the end of the string if necessary }
  lSuffix.x := PadwithZeros( lSuffix.x, 6, true );
  lSuffix.x := PadwithZeros( lSuffix.x, 7, true );

  Result := lPrefixStr + ' ' + lSuffix.x + ' ' + lSuffix.y;
end;

{==============================================================================
    Function  Name: ValidUKSpatialRef
            Called: ValidSpatialRef, ValidSpecificSpatialRef.
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
function ValidUKSpatialRef( iSpatialRef :string ) : TValidSpatialRef;
var
  lPrefixStr : string;
  lPrefixNum : string;
  lEastNorth : TPoint;
  lSpatialRef : String;
  lSuffixStr : string;
begin
  lSpatialRef := RemoveSubstrings( iSpatialRef, ' ' );

  { UK - always an even number of characters }
  if Odd( Length( lSpatialRef )) then
  { Check for a tetrad }
  begin
    If ( ( Length( lSpatialRef ) = 5) and
           ValidLetter( Copy( lSpatialRef, 1, 2 )) and
           ValidInteger( Copy( lSpatialRef, 3, 2 )) and
           ValidLetter( Copy( lSpatialRef, 5, 1 )) ) then
    begin
      try
        lPrefixStr := Copy( lSpatialRef, 1, 2 );
        lPrefixNum := DecodePrefixFromLetter( lPrefixStr, gtUK_Grid );
      except
        on ESpatialRefError do
        begin
          Result.Valid := False;
          Result.FormattedSR := MSG_OUTSIDE_LIMITS;
          Result.Error := EST_OUTSIDE_SYSTEM;
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
    lPrefixStr := Copy( lSpatialRef, 1, 2 );
    lSuffixStr := Copy( lSpatialRef, 3, Length( lSpatialRef ));

    If ValidInteger( lPrefixStr) then
    begin
      Result.Valid := False;
      Result.FormattedSR := MSG_OUTSIDE_LIMITS;
      Result.Error := EST_INVALID_SPATIAL_REF_FORMAT;
      Exit;
    end
    else

    begin
      try
        lPrefixNum := DecodePrefixFromLetter( lPrefixStr, gtUK_Grid );
      except
        on ESpatialRefError do
        begin
          Result.Valid := False;
          Result.FormattedSR := MSG_OUTSIDE_LIMITS;
          Result.Error := EST_OUTSIDE_SYSTEM;
          Exit;
        end;
      end; // try..except
    end;

    { Convert to Easting and Northing }
    lEastNorth := OSGBSpatialRefToEN( lSpatialRef );
    If ( lEastNorth.x < MIN_OSGB_EASTING ) or
       ( lEastNorth.x > MAX_OSGB_EASTING ) or
       ( lEastNorth.y < MIN_OSGB_NORTHING ) or
       ( lEastNorth.x > MAX_OSGB_NORTHING ) then
    begin
      Result.Valid := False;
      Result.FormattedSR := MSG_OUTSIDE_LIMITS;
      Result.Error := MSG_OUTSIDE_LIMITS;
      Exit;
    end;
    Result.Valid := True;
    if Length( lSuffixStr ) >1 then
      Result.FormattedSR := UpperCase( lPrefixStr ) + ' ' +
             copy( lSuffixStr, 1, ( Length(lSuffixStr)) div 2 )  + ' ' +
             copy( lSuffixStr, ( (Length(lSuffixStr) div 2) + 1 ), Length( lSuffixStr) )
    else
      Result.FormattedSR := UpperCase( lPrefixStr );
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
function ValidIrishSpatialRef( iSpatialRef :string ) : TValidSpatialRef;
var
  lPrefixStr : string;
  lSuffixStr : string;
  lPrefixNum : string;
  lEastNorth : TPoint;
  lSpatialRef : String;
begin
  lSpatialRef := RemoveSubstrings( iSpatialRef, ' ');
  if not Odd( Length( lSpatialRef )) then
  { Check to see if it is a Tetrad }
  begin
    If ( ( Length( lSpatialRef ) = 4) and
           ValidLetter( Copy( lSpatialRef, 1, 1 )) and
           ValidInteger( Copy( lSpatialRef, 2, 2 )) and
           ValidLetter( Copy( lSpatialRef, 4, 1 )) ) then
    begin
      { If it looks like a Tetrad, check the initial grid square letter }
      try
        lPrefixStr := Copy( lSpatialRef, 1, 2 );
        lPrefixNum := DecodePrefixFromLetter( lPrefixStr, gtIrish_Grid );
      except
        on ESpatialRefError do
        begin
          Result.Valid := False;
          Result.FormattedSR := MSG_OUTSIDE_LIMITS;
          Result.Error := EST_OUTSIDE_SYSTEM;
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
      Result.FormattedSR := MSG_OUTSIDE_LIMITS;
      Result.Error := EST_INVALID_SPATIAL_REF_FORMAT;
      Exit;
    end;
  end; // if it is not an odd number of characters
  { Is it preceeded by a letter? }
  lPrefixStr := Copy( lSpatialRef, 1, 1 );
  lSuffixStr := Copy( lSpatialRef, 2, Length( lSpatialRef ));

  If ValidInteger( lPrefixStr) then
  begin
    Result.Valid := False;
    Result.FormattedSR := MSG_OUTSIDE_LIMITS;
    Result.Error := EST_INVALID_SPATIAL_REF_FORMAT;
    Exit;
  end
  else
  begin
    try
      lPrefixNum := DecodePrefixFromLetter( lPrefixStr, gtIrish_Grid );
    except
      on ESpatialRefError do
      begin
        Result.Valid := False;
        Result.FormattedSR := MSG_OUTSIDE_LIMITS;
        Result.Error := EST_OUTSIDE_SYSTEM;
        Exit;
      end;
    end; // try..except
  end;

  { Convert to Easting and Northing to see if it lays within bounds }
  lEastNorth := OSIGSpatialRefToEN( iSPatialRef );

  { Check the prefix lies within the range }
  if ( StrToInt( lPrefixNum ) < MIN_OSIG_PREFIX ) or
     ( StrToInt( lPrefixNum) > MAX_OSIG_PREFIX ) or
     ( lEastNorth.x < MIN_OSIG_EASTING ) or
     ( lEastNorth.x > MAX_OSIG_EASTING ) or
     ( lEastNorth.y < MIN_OSIG_NORTHING ) or
     ( lEastNorth.x > MAX_OSIG_NORTHING ) then
  begin
    Result.Valid := False;
    Result.FormattedSR := MSG_OUTSIDE_LIMITS;
    Result.Error := EST_OUTSIDE_SYSTEM;
    Exit;
  end;
  Result.Valid := True;
  if Length( lSuffixStr ) >1 then
    Result.FormattedSR := UpperCase( lPrefixStr ) + ' ' +
           copy( lSuffixStr, 1, ( Length(lSuffixStr) div 2) ) + ' ' +
           copy( lSuffixStr, ( (Length(lSuffixStr) div 2) + 1 ), Length( lSuffixStr) )
  else
    Result.FormattedSR := Uppercase( lPrefixStr );
  Result.Error := '';
end;

{==============================================================================
    Function  Name: ValidUTMSpatialRef
            Called: ValidSpatialRef, ValidSpecificSpatialRef
           Purpose: The passed in spatial reference parameter has spaces removed
                     and the result is checked to ensure that it is composed
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
function ValidUTMSpatialRef( iSpatialRef :string ) : TValidSpatialRef;
var
  lZone, lFirstHalf, lSecondHalf : integer;
  lZoneStr, lFirstHalfStr, lSecondHalfStr : string;
  lCount : integer;
  lSpatialRef : string;
begin
  { Check that it is all numbers }
  lSpatialRef := RemoveSubstrings( iSpatialRef, ' ' );

  { UTM should be all numbers }
  If not ValidInteger ( lSpatialRef ) then
  begin
    Result.FormattedSR := MSG_OUTSIDE_LIMITS;
    Result.Valid := False;
    Result.Error := EST_INVALID_SPATIAL_REF_FORMAT;
    Exit;
  end;

  { Split into the Zone, the north/south Component and the east/west Component
    ie. 31, (0 to 500000), (0 to 10000000) }
  lCount := Pos( ' ', iSpatialRef );
  If lCount = 0 then
  begin
    Result.Valid := False;
    Result.FormattedSR := MSG_OUTSIDE_LIMITS;
    Result.Error := EST_OUTSIDE_SYSTEM;
    Exit;
  end;
  lZoneStr := copy( iSpatialRef, 1, lCount-1 );
  lSecondHalfStr := Copy( iSpatialRef, lCount + 1, Length( iSpatialRef ));
  lCount := pos( ' ', lSecondHalfStr );
  If lCount = 0 then
  begin
    Result.Valid := False;
    Result.FormattedSR := MSG_OUTSIDE_LIMITS;
    Result.Error := EST_OUTSIDE_SYSTEM;
    Exit;
  end;
  lFirstHalfStr := copy( lSecondHalfStr, 1, lCount-1 );
  lSecondHalfStr := copy( lSecondHalfStr, lCount + 1, length( lSecondHalfStr ));

  lZone := StrToInt( lZoneStr );
  lFirstHalf := StrToInt( lFirstHalfStr );
  lSecondHalf := StrToInt( lSecondHalfStr );

  If ( lZone < MIN_UTM_ZONE ) or
     ( lZone > MAX_UTM_ZONE ) or
     ( lFirstHalf < MIN_UTM_EASTING ) or
     ( lFirstHalf > MAX_UTM_EASTING ) or
     ( lSecondHalf < MIN_UTM_NORTHING ) or
     ( lSecondHalf > MAX_UTM_NORTHING ) then
  begin
    Result.Valid := False;
    Result.FormattedSR := MSG_OUTSIDE_LIMITS;
    Result.Error := EST_OUTSIDE_SYSTEM;
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
function ValidLatLongSpatialRef( iSpatialRef : String ) : TValidSpatialRef;
var
  lLatLong : TLatLong;
  lSpatialRef : String;
begin
  { Make a continuous string of only numbers, NESW, periods and +/- }
  lSpatialRef := RemoveNonLLSubstrings( iSpatialRef, false, false, false );

  { Split into Lat and Long }
  try
    lLatLong := FormatLLStringAsTLatLong( lSpatialRef );
  except
    on Exception do
    begin
      Result.FormattedSR := '';
      Result.Error := EST_INVALID_SPATIAL_REF_FORMAT;
      Result.Valid := False;
      Exit;
    end;
  end; // try..except

  If not ValidLatLongRecord( lLatLong ) then
  begin
    Result.Valid := False;
    Result.FormattedSR := MSG_OUTSIDE_LIMITS;
    Result.Error := EST_INVALID_SPATIAL_REF_FORMAT;
    Exit;
  end;
  Result.Valid := True;
  Result.FormattedSR := FormatTLatLongAsString( lLatLong );
  Result.Error := '';
end;


{==============================================================================
    Function  Name: ValidLatLongRecord
            Called: ValidLatLongSpatialRef, Map
           Purpose: Returns a boolean to show whether a LongLat record (two strings)
                    is valid or not
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function ValidLatLongRecord( iLatLong : TLatLong ) : Boolean;
begin
  try
    if ( iLatLong.Long = '') or ( iLatLong.Lat = '') then
    begin
      result := false;
      exit;
    end
    else
      if ( strtofloat( iLatLong.Lat ) < MIN_LAT ) or
         ( strtofloat( iLatLong.Lat ) > MAX_LAT ) then
      begin
        Result := false;
        exit;
      end
      else
        if ( strtofloat( iLatLong.Long ) < MIN_LONG ) or
           ( strtofloat( iLatLong.Long ) > MAX_LONG ) then
        begin
          Result := false;
          Exit;
        end
  except on Exception do
    begin
      result := false;
      exit;
    end;
  end; // try..finally
  result := true;
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
function EastNorthToLatLong( iEasting, iNorthing : integer ;
                             iZone : integer): TMapCoord;
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

  LatTrueOriginRad := DegToRad( LatTrueOrigin );
  LongTrueOriginRad := DegToRad( LongTrueOrigin );

  { 8.3i k is (phi2 - phi1) }
  k :=(( iNorthing-nZero)/a) + LatTrueOriginRad;

  { Go to Equation 8.3ii to evaluate (M02-MO1) }
  lMeridian := CalculateMeridian(k);
  { 8.3v }
  While abs( iNorthing - nZero - lMeridian) > 0.0001 do { should be greater than or equal to }
  begin
    { 8.3iii -  }
    K := k + (( iNorthing - nZero - lMeridian)/a);
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

  { Latitude }
  Latitude := (k - JThree*(sqr(eTrue)) + (power(eTrue,4))*JFour
                                     - (power(eTrue,6))*JFive);
  { Convert latitude to degrees }
  Latitude := RadToDeg(Latitude);

  JSix := 1/(cosK*v);
  JSeven := (1/(cosK*6*(power(v,3)))) * ((v/rho) + 2*(sqr(tanK)));
  JEight := (1/(cosK*120*(power(v,5))))*(5 + 28*(sqr(tanK))
                                           + 24*(power(tanK,4)));
  JNine := (1/(cosK*5040*(power(v,7)))) * (61 + 662*(sqr(tanK))
                                        + 1320*(power(tanK,4))
                                        + 720*(power(tanK,6)));
  { Longitude (lambda) }
  Longitude := LongTrueOriginRad + (eTrue*JSix) - ((power(eTrue,3))*JSeven)
               + ((power(eTrue,5))*JEight) - (power(eTrue,7)*JNine);

  { Convert longitude to degrees }
  Longitude := RadToDeg( Longitude );

  Result.x := Longitude;
  Result.y := Latitude;
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
function LatLongToEastNorth( iLongitude, iLatitude: extended): TPoint;
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
  Northing : integer;

begin
  { Special case - If a UTM projection and in southern hemisphere }
  if ( tfUTM = true ) and ( iLatitude < 0 ) then
    nZero:=10000000;

  LatInRad := DegToRad( iLatitude );
  LongInRad := DegToRad( iLongitude );

  { Special Case - UTM Grid needs Longitude to set up true Origin }
  if tfUTM = true then
  begin
    LongTrueOrigin := ( trunc( abs( iLongitude )/6 )*6 +3 );
    if iLongitude < 0 then
      LongTrueOrigin:= - LongTrueOrigin;
  end;

  LongTrueOriginRad := DegToRad( LongTrueOrigin );

  { Conversion to degrees }
  SinLat := sin( LatInRad );
  CosLat := cos( LatInRad );
  TanLat := tan( LatInRad );

  v := a / (power((1-eSquared*(sqr(SinLat))),0.5));
  rho := a*(1-eSquared)/power((1 - eSquared*(sqr(SinLat))),1.5);
  etaSquared := (v/rho)-1;

  If iLongitude < 0 then
    LongTrueOrigin:=-LongTrueOrigin;

  p := LongInRad - LongTrueOriginRad;
  pOne := abs(p);

  lMeridianArc := CalculateMeridian(LatInRad);

  { Northing Equations }
  bOne := lMeridianArc + nZero;
  bTwo := (v/2)*SinLat*CosLat;
  bThree := (v/24) * SinLat * (power(CosLat,3)) * (5 - sqr(tanLat) + 9*etaSquared);
  bThreeA := (v/720) * SinLat * (power(CosLat,5))
                     * (61 - 58*(sqr(TanLat)) + (power(TanLat,4)));

  Northing := round(bOne + ((sqr(pOne))*bTwo) + ((power(pOne,4))*bThree)
                    + (power(pOne,6)*bThreeA));

  Result.y := Northing;

  { Easting Equations }
  bFour := v*CosLat;
  bFive := (v/6) * (Power(CosLat,3)) * ((v/rho)-(sqr(TanLat)));
  bSix := (v/120) * (power(CosLat,5)) *
          (5 - (18*sqr(TanLat)) + (power(TanLat,4)) + 14*etaSquared
                                - 58*etaSquared*(sqr(TanLat)));
  Result.x := round(eZero + p*bFour + p*(power(pOne,2))*bFive
                          + p*(power(pOne,4))*bSix);
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
function CalculateMeridian( iLatInRad : extended ): extended;
var
  phiMinus, phiPlus : extended;
  LatTrueOriginRad : extended;
  lTemp : extended;
begin
  LatTrueOriginRad := DegToRad( latTrueOrigin );
  PhiMinus := iLatInRad - LatTrueOriginRad; // phi2 - phi1
  phiPlus := iLatInRad + LatTrueOriginRad;  // phi2 + phi1

  lTemp := ((1 + n + ((5/4)*(sqr(n))) + (5/4)*(power(n,3))) * PhiMinus)

         -( (3*n + 3*(sqr(n)) + (21/8)*(power(n,3)))*sin(PhiMinus)*cos(phiPlus) )

         +( ((15/8)*(sqr(n)) + (15/8)*(power(n,3)))*sin(2*PhiMinus)*cos(2*phiPlus) )

         -( ((35/24)*(power(n,3)))*sin(3*PhiMinus)*cos(3*phiPlus) );

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
function DecodePrefixFromLetter( const iPrefix: string; iSystem : TGridType ): string;
var
  lFirstCoords : TPoint;
  lSecondCoords : TPoint;
  lResult : TPoint;
  lPrefix : String;
  //-----------------------------------------------------------------------------
  function LetterToBoxCoords(iLetter: string): TPoint;
  var
    lChar : char;
    lAsciInteger : integer;
    lLetter : string;
  begin
    lLetter := UpperCase( iLetter );
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
    Result.y := 4 - ( lAsciInteger div 5 );
  end;
//-----------------------------------------------------------------------------
begin
  lPrefix := UpperCase( iPrefix );

  { Convert box coords from origin at top left to origin
    at bottom left and take account of false origin }
  lFirstCoords := LetterToBoxCoords( Copy( lPrefix, 1,1 ));

  Case iSystem of
    gtUK_Grid :
    begin
      lFirstCoords.x := ( lFirstCoords.x -2 ) *5;
      lFirstCoords.y := ( lFirstCoords.y -1 ) *5;
      lSecondCoords := LetterToBoxCoords( Copy(iPrefix, 2,2 ));
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

  If ( lResult.x < 0 ) or ( lResult.y < 0 ) then
    raise ESpatialRefError.Create( EST_OUTSIDE_SYSTEM );

  Result := InttoStr( lResult.x ) + InttoStr( lResult.y );
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
function EncodePrefixToLetters( iNumericPrefix: string; iSystem : TGridType ): string;
var
  lFirstPoint : TPoint;
  lSecondPoint : TPoint;
  FirstLetter : String;
  SecondLetter : String;
  lTempPrefix : String;
  //------------------------------------------------------------------------------
  function BoxCoordsToLetter(iBoxCoord: TPoint ): String;
  var
    iBoxIndex : Integer;
  begin
    { The box index is the number of the box which the coords would
      fall in if we consider the 5x5 box of prefixes
      This can then be converted to a letter }
    iBoxIndex := ( 4 - iBoxCoord.y ) *5 + iBoxCoord.x;
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
  If Length( iNumericPrefix ) = 2 then
    lTempPrefix := '0' + Copy( iNumericPrefix, 1 ,1 )
                 + '0' + Copy( iNumericPrefix, 2 ,2 )
  else
    if Length( iNumericPrefix ) = 3 then
      lTempPrefix := '0' + Copy( iNumericPrefix,1,1 )
                   + Copy( iNumericPrefix, 2, 3 )
    else
      lTempPrefix := iNumericPrefix;

  { Take the 4 fig string and split into the first pair
    and second pair of coods }
  lFirstPoint.x := StrToInt (Copy( lTempPrefix, 1,1 ));
  lFirstPoint.y := StrToInt (Copy( lTempPrefix, 3,1 ));

  lSecondPoint.x := StrToInt (Copy( lTempPrefix, 2,1 ));
  lSecondPoint.y := StrToInt (Copy( lTempPrefix, 4,1 ));

  case iSystem of
    gtUK_Grid :
    begin
      { Adjust to make false origin }
      lFirstPoint.x := (( lFirstPoint.x *2 )  + 2) ;
      lFirstPoint.y := (( lFirstPoint.y *2 )   + 1);
      { If second pair are > 5 then the first pair's grrid letter is different }
      if lSecondPoint.x > 4 then
      begin
        lSecondPoint.x := lSecondPoint.x - 5;
        lFirstPoint.x := lFirstPoint.x + 1;
      end;

      if lSecondPoint.y > 4 then
      begin
        lSecondPoint.y := lSecondPoint.y - 5;
        lFirstPoint.y := lFirstPoint.y + 1;
      end;
      FirstLetter := BoxCoordsToLetter( lFirstPoint );
      SecondLetter := BoxCoordsToLetter( lSecondPoint );
    end;

    gtIrish_Grid :
    begin
      { Adjust to make false origin }
      FirstLetter := BoxCoordsToLetter( lSecondPoint );
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
function OSGBMapCoordToLatLong( iEastingNorthing : TMapCoord ) :TLatLong;
var
  lLatLong : TMapCoord;
  lString : TLatLong;
  lFormattedEN : TPoint;
begin
  SetGridConstants( gtUK_Grid );
  lFormattedEN.x := round( iEastingNorthing.x );
  lFormattedEN.y := round( iEastingNorthing.y );

  lLatLong := EastNorthToLatLong( lFormattedEN.x, lFormattedEN.y, 0 );
  lString.Lat := FloatToStr( lLatLong.y );
  lString.Long := FloatToStr( lLatLong.x );
  Result := lString;
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
function OSIGMapCoordToLatLong( iEastingNorthing : TMapCoord) :TLatLong;
var
  lLatLong : TMapCoord;
  lString : TLatLong;
  lFormattedEN : TPoint;
begin
  SetGridConstants( gtIrish_Grid );

  lFormattedEN.x := round( iEastingNorthing.x );
  lFormattedEN.y := round( iEastingNorthing.y );

  lLatLong := EastNorthToLatLong( lFormattedEN.x, lFormattedEN.y, 0 );
  lString.Lat := FloatToStr( lLatLong.y );
  lString.Long := FloatToStr( lLatLong.x );
  Result := lString;
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
function LatLongToOSGBEastNorth( iLatLong: TLatLong ): TMapCoord;
var
  lMapCoord : TMapCoord;
  lPoint : TPoint;
begin
  SetGridConstants( gtUK_Grid );
  lMapCoord.x := StrToFloat( iLatLong.Long );
  lMapCoord.y := StrToFloat( iLatLong.Lat );
  lPoint := LatLongToEastNorth( lMapCoord.x, lMapCoord.y );
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
function LatLongToOSIGEastNorth( iLatLong: TLatLong): TMapCoord;
var
  lMapCoord : TMapCoord;
  lPoint : TPoint;
begin
  SetGridConstants( gtIrish_Grid );
  lMapCoord.x := StrToFloat( iLatLong.Long );
  lMapCoord.y := StrToFloat( iLatLong.Lat );
  lPoint := LatLongToEastNorth( lMapCoord.x, lMapCoord.y );
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
function RemoveSubstrings( iString : string; iSubString : char ) : string;
var
  lSpace : integer;
  lLength : integer;
  lFirstHalf : string;
  lSecondHalf : string;
begin
  lSpace := pos( iSubString, iString );
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
    Function  Name: RemoveNonLLSubstrings
            Called: Various
           Purpose: Takes a string parameter and a number of flags. The function
                    returns the string, having removed all instances non-integer
                    characters except for "N", "E", "S", "W", "+", "-" and ".".
                    The flags (iRemovePeriod, iRemoveMinus, iRemovePlus) are
                    set to true if these are also to be removed
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function RemoveNonLLSubstrings( iString : string; iRemovePeriod, iRemoveMinus,
                                iRemovePlus : boolean ) : string;
var
  lSpace : integer;
  lLength : integer;
  lFirstHalf : string;
  lSecondHalf : string;
  iAsciCount : integer;
  lCharacter : String;
  lWorkingString : string;
begin
  lWorkingString := iString;
  iAsciCount := 0;
  while iAsciCount < 255 do
  begin
    inc( iAsciCount );
    lCharacter := Char( iAsciCount );
    lSpace := pos( lCharacter, lWorkingString );
    while lSpace > 0 do
    begin
      case iAsciCount of
        { Don't remove A-Z or 0-9 }
        ord('A')..ord('Z') : break;
        ord('0')..ord('9') : break;
        ord('.') : if not iRemovePeriod then break;
        ord('+') : if not iRemovePlus then break;
        ord('-') : if not iRemoveMinus then break
      end; // case
      lLength := Length( lWorkingString );
      lFirstHalf := copy( lWorkingString, 1, (lSpace-1));
      lSecondHalf := copy( lWorkingString, lSpace+1, (lLength-lSpace));
      lWorkingString := lFirstHalf + lSecondHalf;
      lSpace := pos( lCharacter, lWorkingString );
    end; // while lCount > 0
  end;
  Result := lWorkingString;
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
  lComma, lLength : integer;
  lFirsthalf, lSecondHalf : string;
begin
  { Needs to be more robust }
  lColon := pos(':', iScale);
  lLength := Length( iScale );
  lFirstHalf := copy( iScale, 0, lColon-1 );
  lSecondHalf := copy( iScale, lColon+1, lLength ) ;
  lComma := pos(',' , lSecondHalf);
  while lComma > 0 do
  begin
    lSecondHalf := copy( lSecondHalf, 0, lComma-1)
                + copy( lSecondHalf, lComma+1, length( lSecondHalf ));
    lComma := pos(',', lSecondHalf);
  end;
  result := strToFloat( lSecondHalf ) / StrToFloat( lFirstHalf );
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
    //   MessageDlg(EST_CONVERT_STRING_ERROR, mtInformation, [mbOk], 0);
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
function CheckBoundingBox( iSWSpatialRef, iNESpatialRef : string;
                           iCurrentSystem : string ) : TValidBoundingBox;
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
  If CompareText ( iSWSpatialRef, MSG_OUTSIDE_LIMITS ) = 0 then
    lTempMessage := 'The SouthWest '
  else
    If CompareText ( iSWSpatialRef, MSG_OUTSIDE_LIMITS ) = 0 then
      lTempMessage := 'The NorthEast '
    else
      lTempMessage := '';
  If Length(lTempMessage) > 1 then
  begin
    Result.Valid := false;
    Result.Error := lTempMessage + 'spatial refenernce is beyond the limits of the current system.';
    exit;
  end;
  If DetermineSpatialRefSystem( iSWSpatialRef ) <>
     DetermineSpatialRefSystem( iNESpatialRef ) then
  begin
    Result.Valid := false;
    Result.Error := 'Please enter both spatial references in the same spatial reference system.';
    exit;
  end;
  lneValid := ValidSpatialRef( iNESpatialRef, iCurrentSystem );
  If lneValid.Valid then
    Result.FormattedNorthEastSR := lneValid.FormattedSR
  else
    Result.Error := lneValid.Error;

  lswValid := ValidSpatialRef( iSWSpatialRef, iCurrentSystem );
  If lswValid.Valid then
    Result.FormattedSouthWestSR := lswValid.FormattedSR
  else
    Result.Error := lswValid.Error;

  if not lswValid.Valid then
    Result.Error := 'SouthWest';
  If not lneValid.Valid then
  begin
    If Result.Error <> '' then
      Result.Error := Result.Error + ' and NorthEast'
    else
      Result.Error := 'NorthEast';
  end;

  { CheckSW is SW of NE only if both are valid }
  If Result.Error = '' then
  begin
    lneLL := ConvertToLatLong( lneValid.FormattedSR, iCurrentSystem );
    lswLL := ConvertToLatLong( lswValid.FormattedSR, iCurrentSystem );

    Result.Valid := ( StrToFloat( lneLL.Lat) > StrToFloat( lswLL.Lat )) and
                    ( StrToFloat( lneLL.Long ) > StrToFloat( lswLL.Long ));
    if not Result.Valid then
      Result.Error := 'The SouthWest corner is not to the southwest of the NorthEast corner.'#13
                      + 'Please enter a valid bounding box.';
  end
  else
  begin
    Result.Valid := False;
    Result.Error := EST_BOUNDINGBOX_INVALID + Result.Error;
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
function ValidInteger( iTestString : string ) : boolean;
var
  lCOunt : integer;
  lChar : char;
begin
  If iTestString = '' then
  begin
    Result := true;
    exit;
  end;
  for lCount := 1 to Length( iTestString ) do
  begin
    lChar := iTestString[lCount];
    If not ( lChar in ['0'..'9'] ) then
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
           Purpose: Checks whether a sting passed in contains an letter A-Z
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function ValidLetter( iTestString : string ) : boolean;
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
  lString := UpperCase( iTestString );
  for lCount := 1 to Length( lString ) do
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
    Function  Name: LatLong
            Called:
           Purpose: Builds a TLatLong from two strings
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function LatLong( const iLat, iLong : string ) : TLatLong;
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
function ConvertSystems( iSpatialRef, iSystemIn, iSystemOut : string ) : string;
var
  lLatLong : TLatLong;
begin
  lLatLong := ConvertToLatLong( iSpatialRef, iSystemIn );
  Result := ConvertFromLatLong( lLatLong, iSystemOut );
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
function FormatLLStringAsTLatLong( iLLString : String ) : TLatLong;
var
  lSpatialRef : string;
  lChar : char;
  lFirstHalf, lSecondHalf : string;
  lFirstSuffix, lSecondSuffix : string;
  lCount : integer;
begin
  { Remove unwanted characters }
  lSpatialRef := RemoveNonLLSubstrings( illString, false, false, false );

  { Seperate into N / S / E / W parts }
  For lCount := 1 to Length( lSpatialRef ) do
  begin
    lChar := lSpatialRef[lCount];
    If ( lChar in ['A'..'Z'] ) then
    begin
      lFirstHalf := copy( lSpatialRef, 1, lCount );
      lSecondHalf := Copy (lSpatialRef, lCount+1, Length( lSpatialRef ));
      break;
    end;
  end;

  lFirstSuffix := copy( lFirstHalf, Length(lFirstHalf), Length(lFirstHalf));
  lSecondSuffix := copy( lSecondHalf, Length(lSecondHalf), Length(lSecondHalf));

  lFirstHalf := copy( lFirstHalf, 1, Length(lFirstHalf) -1 );
  lSecondHalf := copy( lSecondHalf, 1, Length(lSecondHalf) -1 );

  If (lFirstSuffix = 'N') then
    Result.Lat :=  lFirstHalf;
  If (lFirstSuffix = 'S') then
    Result.Lat := FloatToStr( -1 * StrToFloat(lFirstHalf));
  If (lFirstSuffix = 'E') then
    Result.Long :=  lFirstHalf;
  If (lFirstSuffix = 'W') then
    Result.Long :=  FloatToStr(-1 * StrToFloat(lFirstHalf));
  If (lSecondSuffix = 'N') then
    Result.Lat :=  lSecondHalf;
  If (lSecondSuffix = 'S') then
    Result.Lat :=  FloatToStr(-1 * StrToFloat(lSecondHalf));
  If (lSecondSuffix = 'E') then
    Result.Long :=  lSecondHalf;
  If (lSecondSuffix = 'W') then
    Result.Long :=  FloatToStr(-1 * StrToFloat(lSecondHalf));
end;

{==============================================================================
    Function  Name: FormatTLatLongAsString
            Called: Various - Map, self.
           Purpose: Takes a TLatLong and formats it as a spatial reference string.
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function FormatTLatLongAsString( var iLatLong : TLatLong ) : String;
var
  lNumLL : TMapCoord;
  lLatLong : TLatLong;
begin
  try
    lNumLL.x := StrToFloat( iLatLong.Long );
    lNumLL.y := StrToFloat( iLatLong.Lat );
  except
    on EXception do
    raise ESpatialRefError.Create( EST_INVALID_SPATIAL_REF_FORMAT );
  end;

  { Taking a TLatLong as purely numerical but in a string form, then if
    less than zero then S or W, else N or E }

  If StrToFloat( iLatLong.Lat ) < 0 then
  begin
    lLatLong.Lat := Copy( iLatLong.Lat, 2, Length(iLatLong.Lat) );
    lLatLong.Lat := lLatLong.Lat + 'S';
  end
  else
    lLatLong.Lat := iLatLong.Lat + 'N';

  If StrToFloat( iLatLong.Long) < 0 then
  begin
    lLatLong.Long := Copy( iLatLong.Long, 2, Length(iLatLong.Long) );
    lLatLong.Long := lLatLong.Long + 'W';
  end
  else
    lLatLong.Long := iLatLong.Long + 'E';
  Result := lLatLong.lat + ', ' + lLatLong.long;

end;

{==============================================================================
     Function Name: Tetrad
            Called:
           Purpose: Identifies whether a passed spatial reference is a tetrad
                    or not.  To be a tetrad it has to be ...
                    2 letters, 2 numbers, 1 letter (UK)
                    1 letter, 2 numbers, 1 letter (Irish)
      Dependencies:
     Special Notes:
------------------------------------------------------------------------------}
function Tetrad( const iSpatialRef : string ): boolean;
var
  lSystem : string;
begin
  Result := false;
  lSystem := DetermineSpatialRefSystem( iSpatialRef );
  if ( (( lSystem = OS_GB ) and (length(iSpatialRef) = 5 )) or
       (( lSystem = OS_NI ) and (length(iSpatialRef) = 4 )) ) then
    If ValidLetter( Copy( iSpatialRef, Length(iSpatialRef), 1) ) and
       (UpperCase( Copy(iSpatialRef, Length(iSpatialRef), 1)) <> 'I') then
      result := True;
end;

{==============================================================================
    Function  Name:
            Called:
           Purpose: Takes in a spatial reference and a system, plus an integer
      Dependencies:
      Side Effects:
     Special Notes:
------------------------------------------------------------------------------}
function NextGridSquare( const iOldRef: string;
                         const iSize: integer; const iSystem: string): TPoint;
var
  lOldRefEN : TPoint;
begin
  Result.x := 0;
  Result.y := 0;
  // convert old gridref to N / E
  if iSystem = OS_GB then begin
    lOldRefEN := OSGBSpatialRefToEN( iOldRef )
  end else if iSystem = OS_NI then begin
      lOldRefEN := OSIGSpatialRefToEN( iOldRef )
    end else if iSystem = UTM then begin
        lOldRefEN.x := UTMSpatialRefToEN( iOldRef ).easting;
        lOldRefEN.y := UTMSpatialRefToEN( iOldRef ).northing;
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
function CheckSpatialRefInBB( const RBL, RTR: TLatLong;
                              const iSpatialRef, iSystem: string): boolean;
var
  lRefAsLatLong : TLatLong;
begin
  If CompareText( DetermineSpatialRefSystem( iSpatialRef ), iSystem ) <> 0 then
    lRefAsLatLong := ConvertToLatLong( iSpatialRef,
                                       DetermineSpatialRefSystem( iSpatialRef ) )
  else
    lRefAsLatLong := ConvertToLatLong( iSpatialRef, iSystem );

  if ( StrToFloat( lRefAsLatLong.Lat ) > StrToFloat( RTR.Lat ) )
    or ( StrToFloat( lRefAsLatLong.Long ) > StrToFloat( RTR.Long ) )
    or ( StrToFloat( lRefAsLatLong.Lat )  < StrToFloat( RBL.Lat ) )
    or ( StrToFloat( lRefAsLatLong.Long ) < StrToFloat( RBL.Long ) ) then
    result := false
  else
    Result := true;
end;  // CheckSpatialRefInBB

{==============================================================================
    Function  Name: CheckSRefInSRef
            Called:
           Purpose: Takes two spatial references and their systems and tests to
                    see if one is contained within the other
      Dependencies:
      Side Effects: Calls ConvertSpatialRefToEastNorth and CompareEastingNorthings
     Special Notes:
------------------------------------------------------------------------------}
function CheckSRefInSRef( const iParentSR, iChildSR,
                          iParentSystem, iChildSystem: string): boolean;
var
  lPoint1, lPoint2 : TPoint;
begin
  Result := False;
  { Check if the two references and systems are equal }
  if iParentSystem = iChildSystem then
  begin
    if iParentSR = iChildSR then
    begin
      Result := True;
      Exit;
    end { If exactly identical }
    else
    { If systems are the same and LatLong then the Spatial refs must be equal }
      if ( iParentSystem = LAT_LONG ) or
         ( iParentSystem = UTM ) then
      begin
        Raise ESpatialRefError.Create(EST_SAME_SPATIAL_REF);
        Exit;
      end
      else
      begin
        lPoint1 := ConvertSpatialRefToEastNorth( iParentSR, iParentSystem );
        lPoint2 := ConvertSpatialRefToEastNorth( iChildSR, iChildSystem );
        { check that lPoint2 starts with the same digits as lPoint1 and is
          therefore inside it. }

        // Check the easting
        if not CompareEastingNorthings(lPoint1.x, lPoint2.x) then exit;
        // Do the same for the Northing
        if not CompareEastingNorthings(lPoint1.y, lPoint2.y) then exit;
        Result := True;
      end;
  end // if systems are the same
  else
    result := true
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
function CompareEastingNorthings(iCoord1, iCoord2: Integer): boolean;
var
  ltfZeros : Boolean;
  i : integer;
begin
  Result := False;
  ltfZeros := False;
  for i:=1 to length(inttostr(iCoord1)) do
  begin
    if CompareText(Copy(inttostr(iCoord1), i, 1), Copy(inttostr(iCoord2), i, 1)) <> 0 then
    begin
      if ltfZeros then
      begin
        if Copy(inttostr(iCoord1), i, 1) <> '0' then
          exit
      end
      else
      begin
        if Copy(inttostr(iCoord1), i, 1) = '0' then
          ltfZeros := True
        else
          exit;
      end
    end
    else
      if ltfZeros then
        if Copy(inttostr(iCoord1), i, 1) <> '0' then exit;
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
function GetDisplaySpatialRef( const iDisplaySystem, iSpatialRef,
                               iSpatialRefSystem, iLat, iLong,
                               iFieldString : string) : string;
begin
  { If we are in the correct system already, don't reconvert from latlong
     as we don't want rounding errors }
  if CompareText( iDisplaySystem, iSpatialRefSystem ) = 0 then
    Result := iSpatialRef
  else // need to convert
    try
      StrToFloat( iLat );
      StrToFloat( iLong );
      Result := ConvertFromLatLong( LatLong(iLat, iLong), iDisplaySystem);
    except
      { test long/lat can be converted OK }
      Result := iFieldString;
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
        lValidSR := ValidSpatialRef( iEnteredRef, iSystem );
        lValidated := True;
      end;
    end;
  end // if no text
  else
  begin
    if iEnteredRef = '' then
    begin
      { Case 2.  If the text entered is the first to be entered (not modified) }
      lValidSR := ValidSpatialRef( iControl.Text, iSystem );
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
        lValidSR := ValidSpatialRef( iControl.Text, iSystem );
        if lValidSR.Valid then
          iEnteredRef := lValidSr.EnteredFormattedSR;
      end
      else
        lValidSR := ValidSpatialRef( iEnteredRef, iSystem );
      lValidated := True;
    end;
  end;

  if lValidated then
  begin
    if not lValidSR.Valid then
      If iControl.Modified then
        raise ESpatialRefError.Create( lValidSr.Error);
    iControl.Text := lValidSR.FormattedSR;
    //    iControl.Text := UpperCase( iControl.Text );
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
function DifferentEastingAndNorthing( const iSWSpatialRef,
                                            iNESpatialRef : string ) : boolean;
var
  lSWSystem, lNESystem : string;
  lSWEastNorth, lNEEastNorth : TPoint;
begin
  Result := false;
  lSWSystem := DetermineSpatialRefSystem( iSWSpatialRef );
  lNESystem := DetermineSpatialRefSystem( iNESpatialRef );
  If CompareText( lSWSystem, lNESystem ) <> 0 then
  begin
    result := false;
    exit;
  end;
  { Convert to easting and northings. }
  lSWEastNorth := ConvertSpatialRefToEastNorth( iSWSpatialRef, lSWSystem );
  lNEEastNorth := ConvertSpatialRefToEastNorth( iNESpatialRef, lNESystem );

  { Check different }
  If ( lSWEastNorth.x <> lNEEastNorth.x ) and
     ( lSWEastNorth.y <> lNEEastNorth.y ) then
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
function ConvertOutOfRangeToSystemLimit( const iLatLong : TLatLong;
                                         const iSystem : string ) : string;
var
  lEastNorth : TMapCoord;
  lPoint : TPoint;
begin
  lEastNorth := LatLongToSpecificEN( iLatLong, iSystem );
  if CompareText(iSystem, OS_GB) = 0 then
  begin
    if ( lEastNorth.x < MIN_OSGB_EASTING ) then
      lEastNorth.x := MIN_OSGB_EASTING;
    if ( lEastNorth.x > MAX_OSGB_EASTING ) then
      lEastNorth.x := MAX_OSGB_EASTING;
    if ( lEastNorth.y < MIN_OSGB_NORTHING ) then
      lEastNorth.y := MIN_OSGB_NORTHING;
    if ( lEastNorth.y > MAX_OSGB_NORTHING ) then
      lEastNorth.y := MAX_OSGB_NORTHING;
  end
  else
  { Irish Grid }
  if CompareText(iSystem, OS_NI) = 0 then
  begin
    if ( lEastNorth.x < MIN_OSIG_EASTING ) then
      lEastNorth.x := MIN_OSIG_EASTING;
    if ( lEastNorth.x > MAX_OSIG_EASTING ) then
      lEastNorth.x := MAX_OSIG_EASTING;
    if ( lEastNorth.y < MIN_OSIG_NORTHING ) then
      lEastNorth.y := MIN_OSIG_NORTHING;
    if ( lEastNorth.y > MAX_OSIG_NORTHING ) then
      lEastNorth.y := MAX_OSIG_NORTHING;
  end;
  lPoint.x := round( lEastNorth.x );
  lPoint.y := round( lEastNorth.y );
  Result := EastingNorthingToSpatialRef( lPoint, iSystem );
end;

//==============================================================================
end.
