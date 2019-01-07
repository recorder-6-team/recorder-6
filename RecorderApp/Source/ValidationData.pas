//==============================================================================
//  Unit:        ValidationData                          
//
//  Implements:  TdmValidation
//
//  Description: Contains various functions to validate data entered in R2K.
//
//  Author:      Paul Thomas
//  Created:     6 October 1999
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 83 $
//    $Date: 8/04/10 17:22 $
//    $Author: Andrewkemp $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit ValidationData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, JNCCDatasets, DataClasses, VagueDate, SpatialRefFuncs, ExceptionForm,
  ADODB, DatabaseAccessADO;

resourcestring
  ResStr_FindingLocations = 'Finding locations in bounding box';

type
  EValidationDataError = class(TExceptionPath);

  TProgressEvent = procedure(const iProgress: Integer; iPrcMsgs: Boolean =
      True) of object;

  //For validation of Survey Recording and Operation Dates.
  TValidationResult = record
    Success : boolean;
    Message : string;
    FailParamIndx : integer;
  end;

  TValidOccDates = record
    Valid : Boolean;
    Message : String;
  end;

  TDateError = record
    DateFrom : string;
    DateTo : string;
    DateCompare : string;
  end;

  TdmValidation = class(TDataModule)
    tblEntity: TJNCCTable;
    qrySurvey: TJNCCQuery;
    qryEvent: TJNCCQuery; // not JNCC query - need direct access to spatial ref stuff
    qryLocation: TJNCCQuery;
    qrySample: TJNCCQuery;
    qryDetermination: TJNCCQuery;
    qryGridSquares: TJNCCQuery;
  private
    FLastCheckedSample : TKeyString;  // Key of sample last checked by CheckDeterminationDateAgainstSample
    FSampleDateToTest: TVagueDate;
    FSurveyIsTemp  : boolean;
    FLastCheckedSurvey : TKeyString; // Identified survey whose details are cached
    FLastCheckedSurveySW : TLatLong;
    FLastCheckedSurveyNE : TLatLong;
    FLastCheckedSurveyDates : TActualDateRange;
    FRecorderConnection: TADOConnection;
    procedure ReadSurveyDetails(const iSurveyKey: TKeyString);
    function RecorderConnection: TADOConnection;
    function CheckLocationCompleteHit(const RBL, RTR: TLatLong;
      const iLocationKey: String): boolean;
    procedure OpenQueryOnAccessOrSqlServer(AQuery: TJNCCQuery);
  public
    constructor Create(AOwner : TComponent); override;
    procedure SetDatabase(AConnection: TADOConnection);
    function CheckEventDateAgainstSurvey(iSurveyKey: TKeyString;iEventDate: TVagueDate;
                                              iCacheData : boolean = false): Boolean;
    function CheckSampleDateAgainstEvent(iEventKey: TKeyString;iSampleDate: TVagueDate): Boolean;
    function CheckEventInSurvey(const iSurveyKey : TKeyString;
      const iSpatialRef, iSpatialRefSystem, iLocationKey: string;
      iCacheData : boolean = false) : TValidationResult;
    function CheckEventLocationInfo(const iSpatialRef, iLocationKey,
             iLocationName : string): boolean;
    function CreateLocationHitList( const RBL, RTR: TLatLong;
      ioLocationKeys:TStringList;
      const iExcludeOverlaps : boolean;
      ProgressEvent: TProgressEvent): boolean;
    function CheckSampleInEvent(const iEventKey : TKeyString; const
        iSampleSpatialRef : string; const iSampleLocationKey : TKeyString; const
        iSampleSystem : string; AEventLocationKey: string=''; ASurveyKey:
        string=''): TValidationResult;
    function CheckSRefInLocation(const iLocationKey : TKeyString; const iSpatialRef,
      iSystem : string) : boolean;
    procedure CheckSRefInGridSquares(const iLocationKey: TKeyString;
      const iSpatialRef, iSystem: string; var ioResult: boolean);
    function CheckSampleLocInEventLoc(const iEventLocationKey,
      iSampleLocationKey : TKeyString) : boolean;
    procedure CheckAllOccurrenceDates(const iOccurrenceType: string; oErrors : TstringList);
    function CheckDeterminationDateAgainstSample(const iSampleKey : TKeyString;
        const iDeterminationDate : TVagueDate; iUseCachedSampleDate:
        boolean=false): boolean;
    function CheckDeterminationDateAgainstSampleDate(
        const sampleDate, determinationDate : TVagueDate): boolean;
    function CheckDeterminationDate(const sampleKey, determinationKey: TKeyString;
        sampleDate, determinationDate: TVagueDate): TValidOccDates;
    function CheckIsTemporary(const iSurveyKey : TKeyString) : TValidationResult;
  end;

resourcestring
  ResStr_SampleDateAgainstEvent = 'The sample date must be the same as, or fall within the '+
      'date range given for, its Survey event.';
  ResStr_OtherSampleDateAgainstEvent = 'One or more of the other samples in this Survey event will no longer fall within its new date range.';
  ResStr_EventDateAgainstSurvey = 'Please enter a date which lies within the survey date range.';
  ResStr_SampleNotInSurvey      = 'The sample is not in the bounding box of its survey.';

  ResStr_DateTypeConflict = 'The %s cannot contain the %s.';
  ResStr_DateTypeOrderConflict =  'The %s cannot come before the %s.';
  ResStr_DateTypePresenceConflict = 'An %s must be present when an %s is present.';
  ResStr_DateTypeRequired = 'A %s is required for every Survey.';
  ResStr_SurveyNotFound = 'Survey %s not found during validation';
  ResStr_DeterminationKey = ' Determination Key : ';

  //Error Messages
  ResStr_InvalidVagueDate = 'must be a valid vague date and it cannot be after today''s date of %s.';
  ResStr_InvalidVagueDateBlank = 'must be a valid vague date and it cannot be after today''s date of %s.' +
                                 ' Enter a valid vague date or leave blank.';

  ResStr_InvalidDate = 'must be a valid standard date and cannot be after today ''s date of %s.';
  ResStr_InvalidDateBlank = 'must be a valid standard date and it cannot be after today''s date of %s.' +
                            ' Enter a valid standard date or leave blank.';
  ResStr_InvalidStandardDate = ' must be a valid standard date and cannot be blank.';
  ResStr_InvalidStandardDateBlank = ' must be a valid standard date. Enter a valid standard date or leave blank.';

  ResStr_FromDate = 'The Start Date '+ 'must be a valid vague date and it cannot be after today''s date of %s.';
  ResStr_ToDate = 'The End Date ' + 'must be a valid vague date and it cannot be after today''s date of %s.';
  ResStr_CompareDate  = 'The End Date cannot come before the Start Date.';
  ResStr_SurveyBoundingBox = 'You must enter both the South-West and North-East ' +
                            'corners for a bounding box if you enter either of them.';
  ResStr_InvalidVagueDateValue  = 'The date must be a valid vague date and it cannot be after today''s date of %s.';
  ResStr_EventInSurvey  = 'The event is not in the bounding box of its survey.';
  ResStr_SystemUnknown  = 'The Spatial Reference system is not recognised.';
  ResStr_InvalidSpatialRef = 'The spatial reference is invalid or cannot be recognised.';
  ResStr_SampleInEvent  = 'The Spatial Reference or location entered is not '+
                          'within that of the survey event.';
  ResStr_LocNotInList = 'The specified location was not found in the list of Locations. ' +
                        'Please make sure that the location for the sample is valid';
  ResStr_RefNotInLocGridSQ =  'The Spatial Reference does not fall in the specified location''s grid squares.';
  ResStr_SampleRefNotInEventLocGridSQ = 'The Sample''s Spatial Reference does not fall in the Survey Event location''s grid squares.';
  ResStr_DetDateAgainstSample = 'The Determination Date cannot come before the Sample Date.';
  ResStr_DetDate  = 'The Determination Date ' + 'must be a valid vague date and it cannot be after today''s date of %s.';
  ResStr_AccessionDate  = 'The Accession Date ' + 'must be a valid standard date and it cannot be after today''s date of %s.' +
                          ' Enter a valid standard date or leave blank.';
  ResStr_SpatialRefSystemMissing = 'The spatial reference has no spatial reference system so is invalid.';
  ResStr_LocationInfoMissing  = 'At least one of the spatial reference, ' +
                                'location, or location name must be supplied for a %s.';
  ResStr_SRefNotInLocation = 'The Spatial Reference does not fall in the specified location.';
  ResStr_LocationMissing = 'The import data refers to a location that cannot be found in either the Recorder database or the import dataset.';
  ResStr_RecordsAllowedFromDate='"Records Allowed From" Date ';
  ResStr_RecordsAllowedToDate='The "Records Allowed To" Date ';
  ResStr_OperatingFromDate='The "Operating From" Date ';
  ResStr_OperatingToDate='The "Operating To" Date ';
const
  DateTypeString: array [1..4] of string = (ResStr_RecordsAllowedFromDate,
     ResStr_RecordsAllowedToDate, ResStr_OperatingFromDate, ResStr_OperatingToDate);

var
  dmValidation: TdmValidation;

function InvalidDate(const AQualifier:string; const IsVague, IsBlank:boolean):string;
function InvalidStandardDate(const AQualifier:string; IsBlank:boolean):string;

function ValidateFromToVagueDates(const iFromDateStart, iToDateStart : string;
         var ioError : string; const iDateError : TDateError) : boolean;
function ValidateFromToRealDates(const iFromDateStart, iToDateStart : string;
         var ioError : string; const iDateError : TDateError) : boolean;
function IsDate(const DateString:string):boolean;
function ValidateSurveyDates(const RecFrom, RecTo, OpFrom, OpTo: string) : TValidationResult;

//==============================================================================
implementation

{$R *.DFM}

uses ComObj, GridSquareItem, Recorder2000_TLB, OleDB;

//==============================================================================
function InvalidDate(const AQualifier:string; const IsVague, IsBlank:boolean):string;
var DateString:string;
begin
  DateString:=DateToStr(Date);
  if IsVague then
    if IsBlank then
      Result:=AQualifier+Format(ResStr_InvalidVagueDateBlank,[DateString])
    else
      Result:=AQualifier+Format(ResStr_InvalidVagueDate,[DateString])
  else
    if IsBlank then
      Result:=AQualifier+Format(ResStr_InvalidDateBlank,[DateString])
    else
      Result:=AQualifier+Format(ResStr_InvalidDate,[DateString]);
end;  // InvalidDate

//==============================================================================
function InvalidStandardDate(const AQualifier:string; IsBlank:boolean):string;

begin
 if IsBlank then
   Result:=AQualifier + ResStr_InvalidStandardDateBlank
 else
   Result:=AQualifier + ResStr_InvalidStandardDate;
end;  // InvalidStandardDate

//==============================================================================
function IsDate(const DateString:string):boolean;
begin
  Result:=true;
  try
    StrToDate(DateString);
  except
    on EConvertError do Result:=false;
  end;
end;  // IsDate

//==============================================================================
function ValidateFromToRealDates(const iFromDateStart, iToDateStart: string;
  var ioError : string; const iDateError : TDateError) : boolean;
var
  lPassedError : string;
  lValidFromDate: Boolean;
begin
  // See comment below regarding constants
  lPassedError := ioError;
  lValidFromDate := False;
  if iFromDateStart <> '' then begin
    if not ((IsDate(iFromDateStart)) and (StrToDate(iFromDateStart)<=Date)) then begin
      if ioError = '' then
        ioError := iDateError.DateFrom
      else
        ioError := ioError + iDateError.DateFrom;
    end else
      lValidFromDate := True;
  end;

  if iToDateStart <> '' then begin
    if not ((IsDate(iToDateStart)) and (StrToDate(iToDateStart)<=Date)) then begin
      if ioError = '' then
        ioError := iDateError.DateTo
      else
        ioError := ioError + #13#10#13#10 + iDateError.DateTo;
    end;
    if lValidFromDate and (iFromDateStart <> '') then begin
      if StrToDate(iFromDateStart)>StrToDate(iToDateStart) then begin
        if ioError = '' then
          ioError := iDateError.DateCompare
        else
          ioError := ioError + #13#10#13#10 + iDateError.DateCompare;
      end;
    end;
  end;
  Result := (lPassedError=ioError);
end;  // ValidateFromToRealDates

//==============================================================================
function ValidateFromToVagueDates(const iFromDateStart, iToDateStart : string;
  var ioError : string; const iDateError : TDateError) : boolean;
var
  lPassedError : string;
begin
//Can pass in just the from date for validation
//ioError is populated if an error occurs, iDateError is the TDateError that ioError is
//populated with.
  lPassedError := ioError;
  if not CheckVagueDate(iFromDateStart) then begin
    if ioError = '' then
      ioError := iDateError.DateFrom
    else
      ioError := ioError + #13#10#13#10 + iDateError.DateFrom;
  end;

  if iToDateStart <> '' then begin
    if not CheckVagueDate(iToDateStart) then begin
      if ioError = '' then
        ioError := iDateError.DateTo
      else
        ioError := ioError + #13#10#13#10 + iDateError.DateTo;
    end;
    if (iFromDateStart <> '') then begin
      if CompareVagueDateToVagueDate(StringToVagueDate(iToDateStart), StringToVagueDate(iFromDateStart)) < 0 then begin
        if ioError = '' then
          ioError := iDateError.DateCompare
        else
          ioError := ioError + #13#10#13#10 + iDateError.DateCompare;
      end;
    end;
  end;
  Result := (lPassedError=ioError);
end;  // ValidateFromToVagueDates

{Method checks that DF DT (From and To dates) are valid.  DateType is 1 for Recording
Dates and 3 for Operating Dates}
function CheckFromTo(DateType: integer;
  const DF, DT: TVagueDate): TValidationResult;
begin
  Result.Success := False;
  Result.FailParamIndx := DateType + 1;
  if (DF.DateTypeString='U') or (DT.DateTypeString='U') or // if a date is unknown, cannot fail validation
     (DF.DateTypeString='') or (DT.DateTypeString='') then
    Result.Success := True
  else if IsVagueDateInVagueDate(DF, DT) then
    Result.Message := Format(ResStr_DateTypeConflict, [DateTypeString[DateType + 1], DateTypeString[DateType]])
  else if IsVagueDateInVagueDate(DT, DF) then
    Result.Message := Format(ResStr_DateTypeConflict, [DateTypeString[DateType], DateTypeString[DateType + 1]])
  else if CompareVagueDateToVagueDate(DT, DF) < 0 then
    Result.Message := Format(ResStr_DateTypeOrderConflict, [DateTypeString[DateType + 1], DateTypeString[DateType]])
  else Result.Success := True;
end;

function CheckRecOp(const Rec, Op: TVagueDate): TValidationResult;
begin
  if IsVagueDateInVagueDate(Op, Rec) then begin
    Result.Success := False;
    Result.Message := Format(ResStr_DateTypeConflict, [DateTypeString[2], DateTypeString[4]]);
    Result.FailParamIndx := 4;
  end else
    if (not IsVagueDateInVagueDate(Rec, Op)) and
       (CompareVagueDateToVagueDate(Op, Rec) < 0) then begin
      Result.Success := False;
      Result.Message := Format(ResStr_DateTypeOrderConflict, [DateTypeString[4], DateTypeString[2]]);
      Result.FailParamIndx := 4;
    end else Result.Success := True;
end;

function CheckValid(DateType: integer; const DateString: string;
  out DateDate: TVagueDate): TValidationResult;
begin
  if IsVagueDate(DateString) then
    if CheckVagueDate(DateString) then begin
      DateDate := StringToVagueDate(DateString);
      Result.Success := True;
    end else begin
      Result.Success := False;
      Result.Message := InvalidDate(DateTypeString[DateType], True, False);
      Result.FailParamIndx := DateType;
    end
  else begin
    Result.Success := False;
    Result.Message := Format(ResStr_InvalidVagueDate, [FormatDateTime('dd/mm/yyyy', Date)]);
    Result.FailParamIndx := DateType;
  end;
end;

function RecFTOK(RecTo: boolean; const RcF, RcT, OpF, OpT: TVagueDate;
  const OpFrom, OpTo: string): TValidationResult;
begin
  if OpTo = '' then Result.Success := True
  else if RecTo then begin
    Result := CheckRecOp(RcT, OpT);
    if Result.Success then
      if OpFrom = '' then begin
        Result.Success := False;
        Result.Message := Format(ResStr_DateTypePresenceConflict, [DateTypeString[3],DateTypeString[4]]);
        Result.FailParamIndx := 3;
      end else Result := CheckFromTo(3, OpF, OpT);
  end else begin
    Result.Success := False;
    Result.Message := Format(ResStr_DateTypePresenceConflict, [DateTypeString[2], DateTypeString[4]]);
    Result.FailParamIndx := 2;
  end;
end;

function GetAllDates(const RecFrom, RecTo, OpFrom, OpTo: string;
  out RcF, RcT, OpF, OpT: TVagueDate): TValidationResult;
begin
  Result := CheckValid(1, RecFrom, RcF);
  if Result.Success then Result := CheckValid(2, RecTo, RcT);
  if Result.Success then Result := CheckValid(3, OpFrom, OpF);
  if Result.Success then Result := CheckValid(4, OpTo, OpT);
end;

function ValidateSurveyDates(const RecFrom, RecTo, OpFrom,
  OpTo: string): TValidationResult;
var RcF, RcT, OpF, OpT : TVagueDate;
begin
  Result := GetAllDates(RecFrom, RecTo, OpFrom, OpTo, RcF, RcT, OpF, OpT);
  if Result.Success then
    if RecFrom = '' then begin
      Result.Success := False;
      Result.Message := Format(ResStr_DateTypeRequired, [DateTypeString[1]]);
      Result.FailParamIndx := 1;
    end else begin
      if (RecTo = '') then
        Result := RecFTOK(False, RcF, RcF, OpF, OpT, OpFrom, OpTo)
      else begin
        Result := CheckFromTo(1, RcF, RcT);
        if Result.Success then Result := RecFTOK(True, RcF, RcT, OpF, OpT, OpFrom, OpTo);
      end;
      if Result.Success then begin
        Result.Message := '';
        Result.FailParamIndx := 1;
      end;
    end;
end;

//==============================================================================
//==============================================================================
{ TdmValidation }
//==============================================================================
function TdmValidation.CheckDeterminationDate(const sampleKey,
    determinationKey: TKeyString; sampleDate, determinationDate: TVagueDate):
    TValidOccDates;
begin
  Result.Valid := False;
  Result.Message := '';
  if not CheckVagueDate(VagueDateToString(determinationDate)) then
    Result.Message := Format(ResStr_DetDate, [DateToStr(Date)]) + ResStr_DeterminationKey + determinationKey
  else begin
    FSampleDateToTest := sampleDate;
    if not CheckDeterminationDateAgainstSample(sampleKey, determinationDate, true) then
      Result.Message := ResStr_DetDateAgainstSample + ResStr_DeterminationKey + determinationKey;
  end;
  Result.Valid := Result.Message = '';
end;  // CheckDeterminationDate

//=============================================================================
// Checks that a determination date is valid for a given sample.
//
// Loads the sample from the database (with caching option) then checks the
// determination date is on or after the sample date, returning true if it is.
//
function TdmValidation.CheckDeterminationDateAgainstSample(const iSampleKey :
    TKeyString; const iDeterminationDate : TVagueDate; iUseCachedSampleDate:
    boolean=false): boolean;
begin
  if not iUseCachedSampleDate then    // used cached sample date only if checking data en masse
    with qrySample do begin
      Parameters.ParamByName('KeyParameter').Value := iSampleKey;
      try
        Open;
        if not Eof then begin
          FSampleDateToTest := StringToVagueDate(FieldByName('VAGUE_DATE_START').Text);
        end;
      finally
        Close;
      end;
    end;

  Result := CheckDeterminationDateAgainstSampleDate(FSampleDateToTest, iDeterminationDate);
end;  // CheckDeterminationDateAgainstSample

//=============================================================================
// Checks that a determination date is valid for a given sample date.
//
// Checks the determination date is on or after the given sample date,
// returning true if it is.
//
function TdmValidation.CheckDeterminationDateAgainstSampleDate(
        const sampleDate, determinationDate : TVagueDate): boolean;
begin
  if IsVagueDateInVagueDate(sampleDate, determinationDate) then
    Result := false
  else if AreVagueDatesEqual(sampleDate, determinationDate) then
    Result := true
  else
    Result := CompareVagueDateToVagueDate(determinationDate, sampleDate) >= 0;
end;

//==============================================================================
function TdmValidation.CheckEventDateAgainstSurvey(iSurveyKey: TKeyString;
  iEventDate: TVagueDate; iCacheData : boolean = false): Boolean;
var
  lActualEventDate : TActualDateRange;
begin
  if (iEventDate.DateTypeString = 'U') or (iEventDate.DateTypeString = '') then
    Result := True // unknown date so must pass validation check
  else begin
    if (not iCacheData) or (FLastCheckedSurvey <> iSurveyKey) then
      ReadSurveyDetails(iSurveyKey);
    lActualEventDate := TActualDateRange.Create( iEventDate );
    case FLastCheckedSurveyDates.CompareTo( lActualEventDate ) of
      dcBefore, dcAfter :
        Result := False; // only sure that event date not in survey if clear before or after
    else
      Result := True;
    end;
  end;
end;  // CheckEventDateAgainstSurvey

//==============================================================================
function TdmValidation.CheckIsTemporary(const iSurveyKey : TKeyString) :
         TValidationResult;
begin
  ReadSurveyDetails(iSurveyKey);
  if FSurveyIsTemp then
    Result.Success := True
  else
    Result.Success := False;
end;  // CheckIsTemporary


//==============================================================================
function TdmValidation.CheckEventInSurvey(const iSurveyKey : TKeyString;
      const iSpatialRef, iSpatialRefSystem, iLocationKey: string;
      iCacheData : boolean = false) : TValidationResult;
begin
  if iSpatialRef <> '' then begin
    if (not iCacheData) or (FLastCheckedSurvey <> iSurveyKey) then
      ReadSurveyDetails(iSurveyKey);
    // Check bounding box is correctly specified
    if (FLastCheckedSurveySW.Lat <> NULL_LATLONG) and
       (FLastCheckedSurveySW.Long <> NULL_LATLONG) and
       (FLastCheckedSurveyNE.Lat <> NULL_LATLONG) and
       (FLastCheckedSurveyNE.Long <> NULL_LATLONG) then
        { Always check using spatial reference now }
      Result.Success := CheckSpatialRefInBB(FLastCheckedSurveySW, FLastCheckedSurveyNE,
                                            iSpatialRef, iSpatialRefSystem)
    else
      Result.Success := True;  // Nothing to validate against

    if not Result.Success then
      Result.Message := ResStr_EventInSurvey;

    // if everything Ok, check the sample grid square falls inside its own location
    if Result.Success and (iLocationKey <> '') then begin
      CheckSRefInGridSquares(iLocationKey, iSpatialRef,
                             iSpatialRefSystem, Result.Success);
      if not Result.Success then
        Result.Message := ResStr_RefNotInLocGridSQ;
    end;
  end else
    Result.Success := True; // no spatial ref to validate
end;  // CheckEventInSurvey

{-------------------------------------------------------------------------------
 Connection to the main Recorder database.
}
function TdmValidation.RecorderConnection: TADOConnection;
var
  lRecorder: IRecorder2000;
  lCon: TADOConnection;
begin
  if not Assigned(FRecorderConnection) then
    if Assigned(dmDatabase) then
      FRecorderConnection := dmDatabase.Connection
    else
    begin
      lRecorder := 
          CreateComObject(CLASS_AutoApplicationSettings) as IRecorder2000;
      lCon := TADOConnection.Create(Self);
      try
        lCon.LoginPrompt := False;
        lCon.ConnectionString := lRecorder.ConnectionString;
        lCon.Open;
        lRecorder.SetApplicationSecurity(lCon.ConnectionObject);
      except
        lCon.Free;
        raise;
      end;
      FRecorderConnection := lCon;
    end;
    
  Result := FRecorderConnection;
end;

//==============================================================================
{ Read the bounding box, date and temporary survey information for a survey, into the class
     data for this purpose }
procedure TdmValidation.ReadSurveyDetails( const iSurveyKey : TKeyString );
var
  lFailed: Boolean;
  lOldCon: TADOConnection;

    procedure ReadLatLong( const iFieldName : string; var iRef : double );
    begin
      if not qrySurvey.FieldByName(iFieldName).IsNull then
          iRef := qrySurvey.FieldByName(iFieldName).AsFloat
        else
          iRef := NULL_LATLONG;
    end;

begin
  with qrySurvey do
  begin
    lOldCon := Connection;
    try
      Parameters.ParamByName('KeyParameter').Value := iSurveyKey;

      try
        Open;
        lFailed := Eof;
      except
        on E: EOleException do
          if E.ErrorCode = DB_E_NOTABLE then
            lFailed := True
          else
            raise;
      end;

      if lFailed then
      begin
        Close;
        Connection := RecorderConnection;
        Open;
      end;

      if Eof then
        raise EValidationDataError.Create(Format(ResStr_SurveyNotFound, [iSurveyKey]));

      First;
      // bounding box
      ReadLatLong('SW_LAT', FLastCheckedSurveySW.Lat);
      ReadLatLong('SW_LONG', FLastCheckedSurveySW.Long);
      ReadLatLong('NE_LAT', FLastCheckedSurveyNE.Lat);
      ReadLatLong('NE_LONG', FLastCheckedSurveyNE.Long);
      // date
      FLastCheckedSurveyDates := TActualDateRange.Create(
                         StringToVagueDate(FieldByName('FROM_VAGUE_DATE_START').Text),
                         StringToVagueDate(FieldByName('TO_VAGUE_DATE_START').Text) );
      FLastCheckedSurvey := iSurveyKey;
      FSurveyIsTemp := FieldByName('TEMPORARY_SURVEY').AsBoolean;
    finally
      Close;
      Connection := lOldCon;
    end;
  end; // with
end;

//==============================================================================
{ Function checks that at least one of the spatial referenece, location key,
     or location name (vague location) are present for a event }
function TdmValidation.CheckEventLocationInfo(const iSpatialRef, iLocationKey,
             iLocationName : string): boolean;
begin
  Result := (iSpatialRef<>'') or (iLocationKey<>'') or (iLocationName<>'');
end;

//==============================================================================
{
  CheckLocationCompleteHit

  Parameters:
    RBL , RTR are the Bottom Left and Top Right of the bounding box
      as LatLong
    ioLocationKey is the location key string for comparison

  Returns:
    True if ALL of the Location's grid squares overlap
}
Function TdmValidation.CheckLocationCompleteHit(const RBL : TLatLong; const RTR : TLatLong;
  const iLocationKey: string) : boolean;
var
  GBL, GTR : TLatLong;     // Bottom Left and Top Right of the database square
  lGridSquare : TGridSquareItem;    // Grid Square object for later use.
  lSpatialRefSize:integer;
  lGridSquareCheck : boolean;
  ltfOverlap : Boolean;
begin
  Result := false;
  //  First initialise the query
  with qryGridSquares do
  begin
    Parameters.ParamByName('KeyParameter').Value := iLocationKey;
    try
      try
        Open;
        lGridSquareCheck := RecordCount>0;
      except
        on E:Exception do
          if dmDatabase.CheckError(E, dbeNoSuchTable, qryGridSquares.Connection) then
            lGridSquareCheck := false
          else
            Raise;
      end;
      if lGridSquareCheck then begin//check that squares are defined for location
        First;
        //  Get each row from the table
        while not Eof do
        begin
          lGridSquare := TGridSquareItem.Create;
          try
            try
              // prepare a grid square object
              lGridSquare.InitFromRecord( qryGridSquares );
              lSpatialRefSize:=FieldByName('Size').AsInteger;  // Get the int value instead of interpreted string
              if lGridSquare.SpatialRefSystem = OS_GB then
              begin
                // we have a gb grid ref
                SetGridConstants(gtUK_Grid);
                GBL := OSGBtoLTLN( lGridSquare.EnteredRef );
                GTR := SpecificENToLatLong( NextGridSquare( lGridSquare.EnteredRef, lSpatialRefSize, OS_GB ), OS_GB);
              end else
              if lGridSquare.SpatialRefSystem = OS_NI then
              begin
                // we have an irish grid ref
                SetGridConstants(gtIrish_Grid);  //
                GBL := OSIGtoLTLN( lGridSquare.EnteredRef );
                GTR := OSIGtoLTLN( EastingNorthingToSpatialRef(NextGridSquare( lGridSquare.EnteredRef, lSpatialRefSize, OS_NI ), OS_NI, OS_NI) );
              end else
              if lGridSquare.SpatialRefSystem = UTM then
              begin
                // we have a utm grid ref
                SetGridConstants(gtUTM_Grid);  //
                GBL := UTMtoLTLN( lGridSquare.EnteredRef );
                GTR := UTMtoLTLN( EastingNorthingToSpatialRef(NextGridSquare( lGridSquare.EnteredRef, lSpatialRefSize, UTM ), UTM, UTM) );
              end else
              if lGridSquare.SpatialRefSystem = LAT_LONG then
              begin
                // we have a lat long grid ref
                GBL := FormattedLTLNtoLTLN(lGridSquare.EnteredRef);
                GTR := FormattedLTLNtoLTLN(EastingNorthingToSpatialRef(NextGridSquare(
                       lGridSquare.EnteredRef, lSpatialRefSize, LAT_LONG ), LAT_LONG, LAT_LONG));
              end; // if
              ltfOverlap :=  (GTR.Lat  > RBL.Lat) and
                             (GBL.Lat  < RTR.Lat) and
                             (GTR.Long > RBL.Long) and
                             (GBL.Long < RTR.Long);
              if not ltfOverlap then begin // found a non-overlapping grid square, so exclude
                Result := false;
                Exit; // no need to check other grid squares
              end;
            except
              on ESpatialRefError do { ignore grid square as off the limits of the current map };
            end; // try .. except
          finally
            lGridSquare.Free;
          end;
          Next;
        end; // while loop
      end
      else
        // no grid squares, we already no centroid in square so return True
        Result := True;
    finally
      Close;
    end;
  end; // with
end;  // CheckLocationCompleteHit

//==============================================================================
{ Function as above, except it quickly checks all occurrence (determination)
    dates in the entire determinations dataset.  Returns list of errors
    (key=message format) in oErrors, which should be pre-initialised }
procedure TdmValidation.CheckAllOccurrenceDates(const iOccurrenceType: string;
             oErrors : TstringList);
var
  lSQLSelect, lMessage : string;
  lDeterminationDate : TVagueDate;
  lDetKey, lSampleKey : TKeyString;
begin
  oErrors.Sorted := true;
  oErrors.Duplicates := dupIgnore; // 1 error string per occurrence
  oErrors.Clear;
  lMessage := '';
  FLastCheckedSample := '';  // initialise the check

  lSQLSelect := 'SELECT ' + iOccurrenceType + '_DETERMINATION_KEY, ' +
             iOccurrenceType + '_OCCURRENCE.' + iOccurrenceType + '_OCCURRENCE_KEY, ' +
             'VAGUE_DATE_START, VAGUE_DATE_END, VAGUE_DATE_TYPE, ' +
             'SAMPLE_KEY FROM ' + iOccurrenceType + '_OCCURRENCE ' +
             ' INNER JOIN ' + iOccurrenceType + '_DETERMINATION ON ' +
             iOccurrenceType + '_OCCURRENCE.' + iOccurrenceType + '_OCCURRENCE_KEY ' +
             '= ' + iOccurrenceType + '_DETERMINATION.' + iOccurrenceType + '_OCCURRENCE_KEY';
  with qryDetermination do begin
    SQL.Clear;
    SQL.Add(lSQLSelect);
    try
      Open;
      First;
      while not eof do begin
        lMessage := '';
        lDeterminationDate := StringToVagueDate(FieldByName('VAGUE_DATE_START').Text);
        lDetKey := TKeyString(FieldByName(iOccurrenceType + '_DETERMINATION_KEY').AsString);
        if not CheckVagueDate(VagueDateToString(lDeterminationDate)) then
          lMessage := Format(ResStr_DetDate, [DateToStr(Date)]) + ResStr_DeterminationKey + lDetKey
        else begin
          lSampleKey := TKeyString(FieldByName('SAMPLE_KEY').AsString);
          if not CheckDeterminationDateAgainstSample(lSampleKey, lDeterminationDate, true) then
            lMessage := ResStr_DetDateAgainstSample + ResStr_DeterminationKey + lDetKey;
        end;
        if lMessage <> '' then // have an error
          oErrors.Add(FieldByName(iOccurrenceType + '_OCCURRENCE_KEY').AsString + '=' + lMessage);
        Next;
      end;
    finally
      Close;
    end;
  end;
end;  // CheckAllOccurrenceDates

//==============================================================================
function TdmValidation.CheckSampleDateAgainstEvent(iEventKey: TKeyString;
  iSampleDate: TVagueDate): Boolean;
var
  lEventDate : TVagueDate;
begin
  Result := False;
  if (iSampleDate.DateTypeString = 'U') or (iSampleDate.DateTypeString = '') then
    Result := True // unknown date so must pass validation check
  else
    with qryEvent do
    begin
      Parameters.ParamByName('KeyParameter').Value := iEventKey;
      try
        Open;
        if not EOF then begin
          lEventDate := StringToVagueDate(FieldByName('VAGUE_DATE_START').Text);
          if IsVagueDateInVagueDate(lEventDate, iSampleDate) then
            Result := False
          else if AreVagueDatesEqual(lEventDate, iSampleDate) then
            Result := True
          else
            Result := IsVagueDateInVagueDate(iSampleDate, lEventDate);
        end;
      finally
        Close;
      end;
    end;
end;  // CheckSampleDateAgainstEvent

//==============================================================================
function TdmValidation.CheckSampleInEvent(const iEventKey : TKeyString; const
    iSampleSpatialRef : string; const iSampleLocationKey : TKeyString; const
    iSampleSystem : string; AEventLocationKey: string=''; ASurveyKey:
    string=''): TValidationResult;
var
  lEventLocationKey : TKeyString;
  lSurveyKey: TKeyString;
begin
  // if optional survey key parameter is not passed, we need to query for it
  if ASurveyKey='' then
    with qryEvent do begin
      Parameters.ParamByName('KeyParameter').Value := iEventKey;
      Open;
      First;
      lEventLocationKey := FieldByName('LOCATION_KEY').AsString;
      lSurveyKey := FieldByName('SURVEY_KEY').AsString;
      Close;
    end
  else begin
    lSurveyKey := ASurveyKey;
    lEventLocationKey := AEventLocationKey;
  end;

  if iSampleSpatialRef <> '' then begin
    if FLastCheckedSurvey <> lSurveyKey then ReadSurveyDetails(lSurveyKey);

    // Check bounding box is correctly specified
    if (FLastCheckedSurveySW.Lat <> NULL_LATLONG) and
       (FLastCheckedSurveySW.Long <> NULL_LATLONG) and
       (FLastCheckedSurveyNE.Lat <> NULL_LATLONG) and
       (FLastCheckedSurveyNE.Long <> NULL_LATLONG) then
        { Always check using spatial reference now }
      Result.Success := CheckSpatialRefInBB(FLastCheckedSurveySW, FLastCheckedSurveyNE,
                                            iSampleSpatialRef, iSampleSystem)
    else
      Result.Success := True;  // Nothing to validate against

    if not Result.Success then
      Result.Message := ResStr_SampleNotInSurvey;

    // if everything Ok, check the sample grid square falls inside its own location
    if Result.Success and (iSampleLocationKey <> '') then begin
      CheckSRefInGridSquares(iSampleLocationKey, iSampleSpatialRef,
                             iSampleSystem, Result.Success);
      if not Result.Success then
        Result.Message := ResStr_RefNotInLocGridSQ
      else
      // Also checks it's within its event
      if (lEventLocationKey <> '') and (lEventLocationKey<>iSampleLocationKey) then begin
        CheckSRefInGridSquares(lEventLocationKey, iSampleSpatialRef,
                               iSampleSystem, Result.Success);
        if not Result.Success then
          Result.Message := ResStr_SampleRefNotInEventLocGridSQ;
      end;
    end;
  end else
    Result.Success := True; // no spatial ref to validate
end;  // CheckSampleInEvent

//==============================================================================
function TdmValidation.CheckSampleLocInEventLoc(const iEventLocationKey,
  iSampleLocationKey: TKeyString): boolean;
var
  lTestKey : TKeyString;
begin
  Result := false;

  if iEventLocationKey = iSampleLocationKey then // they are the same location
  begin
    Result := True;
    Exit;
  end;

  with qryLocation do begin
    lTestKey := iSampleLocationKey;
    while lTestKey <> '' do
    begin
      Parameters.ParamByName('KeyParameter').Value := lTestKey;
      try
        Open;
        try
          if RecordCount=0 then begin
            // event location missing - might be system supplied so must pass validation
            Result := True;
            Exit;
          end;
          First;
          lTestKey := FieldByName('PARENT_KEY').AsString;
          if iEventLocationKey = lTestKey then // they are the same location
          begin
            Result := True;
            Exit;
          end;
        finally
          Close;
        end;
      except
        on EDatabaseError do begin
          Result := True; // Location table not available so passes validation as far as we can tell
          Exit;
        end;
      end;
    end;
  end;
end;  // CheckSampleLocInEventLoc

//==============================================================================
function TdmValidation.CheckSRefInLocation(const iLocationKey: TKeyString;
  const iSpatialRef, iSystem: string): boolean;
// Checks that given spatial reference is contained within given location
var
  lLocationSRef, lLocationSystem : String;
  lOrigConnection: TADOConnection;
begin
  with qryLocation do begin
    Parameters.ParamByName('KeyParameter').Value := iLocationKey;
    // Location table may not exist in Access database (import)
    lOrigConnection := qryLocation.Connection;
    try
      OpenQueryOnAccessOrSqlServer(qryLocation);
      First;
      try
        if (EOF and BOF) then raise
          EValidationDataError.Create(ResStr_LocationMissing);
        lLocationSRef   := FieldByName('SPATIAL_REF').AsString;
        lLocationSystem := FieldByName('SPATIAL_REF_SYSTEM').AsString;
      finally
        Close;
      end;
    finally
      qryLocation.Connection := lOrigConnection;
    end;
  end;
  Result := CheckSRefInSRef(lLocationSRef, iSpatialRef, lLocationSystem, iSystem);
  if not Result then begin
    Result := true; // default to accept if no grid squares
    CheckSRefInGridSquares(iLocationKey, iSpatialRef, iSystem, Result);
  end;
end;  // CheckSRefInLocation

//==============================================================================
{ Checks if a spatial reference is in one of the grid squares attached to a location.
     NOTE - the Var Result is untouched if no grid squares exist }
procedure TdmValidation.CheckSRefInGridSquares(const iLocationKey: TKeyString;
  const iSpatialRef, iSystem: string; var ioResult : boolean);
var
  ltfTempResult : boolean;
  lOrigConnection: TADOConnection;
begin
  with qryGridSquares do begin
    // not in centroid grid ref, so check grid squares
    Parameters.ParamByName('KeyParameter').Value := iLocationKey;
    lOrigConnection := qryGridSquares.Connection;
    try
      try
        // Grid_Squares table may not exist in Access database (import)
        OpenQueryOnAccessOrSqlServer(qryGridSquares);
        try
          if RecordCount>0 then begin
            ltfTempResult := False;
            First;
            While (not EOF) and (not ltfTempResult) do begin
              ltfTempResult := CheckSRefInSRef(
                  FieldByName('SPATIAL_REF').AsString,
                  iSpatialRef,
                  FieldByName('SPATIAL_REF_SYSTEM').AsString ,
                  iSystem);
              Next;
            end; // while
            ioResult := ltfTempResult;
          end; //if
        finally
          Close;
        end; // try
      finally
        qryGridSquares.Connection := lOrigConnection;
      end;
    except
      on E:Exception do
        if not dmDatabase.CheckError(E, dbeNoSuchTable, qryGridSquares.Connection) then
          Raise;
    end; // try..except
  end;
end;  // CheckSRefInGridSquares

//==============================================================================
function TdmValidation.CreateLocationHitList(const RBL:TLatLong; const RTR:TLatLong;
  ioLocationKeys: TStringList; const iExcludeOverlaps : boolean;
  ProgressEvent: TProgressEvent): boolean;
var
  lLocationKey : TKeyString;
begin
  with dmDatabase.GetRecordset('usp_Locations_Select_InBoundingBox',
      ['@swlat', RBL.Lat,
       '@swlong', RBL.Long,
       '@nelat', RTR.Lat,
       '@nelong', RTR.Long]) do begin
    while not Eof do begin
      lLocationKey := Fields['Location_Key'].Value;
      // Stored proc output includes overlaps, so may need additional test
      if iExcludeOverlaps then begin
        if CheckLocationCompleteHit(RBL, RTR, lLocationKey) then
          ioLocationKeys.Add(lLocationKey);
      end else
        ioLocationKeys.Add(lLocationKey);
      MoveNext;
      if Assigned(ProgressEvent) then
        ProgressEvent(Integer(AbsolutePosition) * 100 div RecordCount);
    end;
  end;
  Result := ioLocationKeys.Count>0;
end; //CreateLocationHitList;

//==============================================================================
constructor TdmValidation.Create(AOwner: TComponent);
begin
  inherited;
  FLastCheckedSurvey := ''; // nothing cached to start with
end;

//==============================================================================
procedure TdmValidation.SetDatabase(AConnection: TADOConnection);
var i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TCustomADODataSet then
      TCustomADODataset(Components[i]).Connection := AConnection;
end;  // SetDatabase

//==============================================================================
  // Opens the supplied query on it's default connection, but if it is using an
  //  Access database (e.g. Import Wizard) and the table either doesn't exist,
  //  or it contains no records, the query switches to the SQL Server database.
//------------------------------------------------------------------------------
procedure TdmValidation.OpenQueryOnAccessOrSqlServer(AQuery: TJNCCQuery);
begin
  with AQuery do begin
    if (Pos('Provider=Microsoft.Jet.OLEDB', Connection.ConnectionString) > 0) then
      try
        Open;
        if AQuery.Eof then begin
          Close;
          // record not available in import db, so see if we can find it in main db
          Connection := RecorderConnection;
          Open;
        end;
      except
        on E:EOleError do
          if dmDatabase.CheckError(E, dbeNoSuchTable, Connection) then begin
            Connection := RecorderConnection;
            Open;
          end;
      end
    else
      Open;
  end;
end;

end.
