unit VagueDate;

{---------------------------------------------------------------------
Author:    Dr Stuart G. Ball
Version:   1
Date:      26 Jan 1999
Purpose:   This implements Recorder's vague date handling in Delphi.
           Dates stored in Recorder format are correctly interpreted
           and data entry by users follows the same syntax and range
           of options.
Copyright: Joint Nature Conservation Committee, 1999
Modified:
date       initials  changes made
-----------------------------------------------------------------
-----------------------------------------------------------------
 Three consecutive fields are used for storing vague dates:

   n     StartDate   Date field
   n+1   EndDate     Date field
   n+2   DateType    Two character text field

 NOTE: It doesn't actually matter what they are called, but they must be
       consecutive, in this order, and of the types given above.

 The range of possibilities allowed, and the values stored in the three
 fields, are illustrated by the following examples:

   Date string                DateType   StartDate    EndDate
   --------------------------------------------------------------
   23 MAR 1987                    D      23 MAR 1987  23 MAR 1987
   23 MAR 1987 - 30 MAR 1987      DD     23 MAR 1987  30 MAR 1987
   MAR 1987                       O      01 MAR 1987  31 MAR 1987
   MAR 1987 - MAY 1987            OO     01 MAR 1987  31 MAY 1987
   SUMMER 1987                    P      01 JUN 1987  31 AUG 1987
   1987                           Y      01 JAN 1987  31 DEC 1987
   1981-1983                      YY     01 JAN 1981  31 DEC 1983
   1980-                          Y-     01 JAN 1980  <empty>
   -1979                          -Y     <empty>      31 DEC 1979
   July                           M      01 JUL 9999  31 JUL 9999
   Autumn                         S      01 SEP 9999  30 OCT 9999
   Winter                         S      01 DEC 9999  28 FEB 9999
   Unknown                        U      <empty>      <empty>

   19c                            C      01 JAN 1801  31 DEC 1900
   19c - 20c                      CC     01 JAN 1801  31 DEC 2000
   19c-                           C-     01 JAN 1801  <empty>
   -19c                           -C     <empty>      31 DEC 1900

 Seasons are defined as follows:

 WINter   Dec, Jan, Feb
 SPRing   Mar, Apr, May
 SUMmer   Jun, Jul, Aug
 AUTumn   Sep, Oct, Nov

 Note that there is a perculiarity in the way "Winter <year>" is stored.
 Winter 1989 is stored as 1/12/89 - 28/2/89 - the StartDate should really
 be in 1988. This is the way dates are stored in Recorder so it is retained
 here.
---------------------------------------------------------------------------}
interface

uses
  SysUtils, Classes, DB, GenFuncs, DateUtils;

resourcestring
  ResStr_Unknown = 'Unknown';

type
  TVagueDateTypes = (vdUnknown, vdDate, vdDateRange, vdYear, vdYearRange,
                            vdToYear, vdFromYear, vdMonth, vdMonthRange,
                            vdMonthOnly, vdSeasonOnly, vdSeason, vdCentury,
                            vdCenturyRange, vdFromCentury, vdToCentury);

  TDateTypes      = (dtUnknown, dtYear, dtMonth, dtSeason, dtMonthYear,
                            dtSeasonYear, dtFullDate, dtCentury);

  TDateRangeType  = (drNone, drBefore, drAfter, drFromTo);

  TDateComparison = (dcEqual, dcBefore, dcBeforeOverlapped, dcAfter,
                     dcAfterOverlapped, dcWithin, dcContains);

  TVagueDate = record
    StartDate      : TDateTime;
    EndDate        : TDateTime;
    {DateType       : TVagueDateTypes; }
    DateTypeString : string[2];
  end;

  // Basic implementation to store start and end dates in a real, unvague
  // fashion.  TActualDateRange derives from this so class can compare to itself
  TBaseActualDateRange = class
  private
    FVagueDate : TVagueDate;
    function GetEndDate: TDateTime;
    function GetStartDate: TDateTime;
  public
    constructor Create( iVagueDate : TVagueDate );  overload;
    constructor Create( iStartVagueDate, iEndVagueDate : TVagueDate );  overload;
    property StartDate : TDateTime read GetStartDate;
    property EndDate : TDateTime read GetEndDate;
  end;

  // Derived class allows comparison of base class
  TActualDateRange = class(TBaseActualDateRange)
  public
    function CompareTo( iTestDate : TBaseActualDateRange ) : TDateComparison;
  end;

  EVagueDateError = class(Exception);

const
  MAX_DATE: TDateTime = 2958465; // 31/12/9999
  MIN_DATE: TDateTime = -693593; // 01/01/0001

  MIN_VAGUE_DATE_DAY: TVagueDate = (StartDate: -693593; EndDate: -693593; DateTypeString: 'D');
  MAX_VAGUE_DATE_DAY: TVagueDate = (StartDate: 2958465; EndDate: 2958465; DateTypeString: 'D');

{ The global functions made available...}
function VagueDateToString(iVagueDate: TVagueDate): string;
function StringToVagueDate(iDateString: string): TVagueDate;
function DateToVagueDate(ADate: TDateTime): TVagueDate;
function IsVagueDateField(iFieldName: string): boolean;
function GetVagueDateName( iFieldName: string): string;
function IsVagueDate(const iDateString:string): boolean;
function CheckVagueDate(const iDateString:string):boolean;

function CheckDateInVagueDateRange(const iVTestDate:TDateTime; const iFrom, iTo: TVagueDate) : Boolean;
function CompareDateToVagueDate(const iTestDate: TDateTime; const iVDate: TVagueDate) : Integer;
function CompareVagueDateToVagueDate(const iVTestDate: TVagueDate; const iVDate: TVagueDate) : Integer;
function CheckVagueDateInVagueDateRange(const iVTestDate,iFrom, iTo: TVagueDate) : Boolean;
function IsVagueDateInVagueDate(const iVTestDate, iVDate:TVagueDate):boolean;
function AreVagueDatesEqual(const iVDate1, iVDate2:TVagueDate):boolean;

function SetVagueDateType(const DateType: TVagueDateTypes): string;

// New date comparison functions - JVB 22/9/00
function CompareVagueDates(const iDate1: TVagueDate; const iDate2: TVagueDate) : TDateComparison;

{ These 2 function variables allow any application to provide its own values
  for months and/or seasons, in any language, if the defaults are not appropriate.
  The strings returned are expected to be comma-delimited strings.
}
var
  GetApplicationSuppliedMonthNames: function: String;
  GetApplicationSuppliedSeasonNames: function: String;

//==============================================================================
implementation

resourcestring
  ResStr_Includes             = 'includes';
  ResStr_StartDatesSameType   = 'Dates at beginning and end of range'#13'must be of the same type';
  ResStr_NotSupported         = 'Not supported';
  ResStr_SeasonNames          = 'Winter,Spring,Summer,Autumn';
  ResStr_InvalidField         = 'The field name supplied is not a vague date field :';
  ResStr_InvalidDay           = '"%s" is not a valid day number.';
  ResStr_InvalidMonth         = '%s is not a valid month.';
  ResStr_InvalidMonthOrSeason = '"%s" is not a valid month or season name.';
  ResStr_InvalidCentury       = '"%s" is not a valid century.';
  ResStr_InvalidYear          = '"%s" is not a valid year.';
  ResStr_BeginningMustPrecedeEnd = 'Beginning of a date range'#13'must be before the end.';

const
  // vague date types that can'T be validated against each other
  UNCHECKED_TYPES = ['U', 'M', 'S'];

var
  mMonthNames: TStringList;
  {These could be used as delimeters in dates.
   Note: must not include space, ? or -.
   If date separator is - then Date Range separator changes to > }
  SpecialChars: string;
  DateRangeSeparator: string;

//------------------------------------------------------------------------------
// Convert letter codes to types
//------------------------------------------------------------------------------
function SetVagueDateType(const DateType: TVagueDateTypes): string;
begin
  case DateType of
    vdUnknown:      Result := 'U';
    vdDate:         Result := 'D';
    vdDateRange:    Result := 'DD';
    vdYear:         Result := 'Y';
    vdYearRange:    Result := 'YY';
    vdToYear:       Result := DateRangeSeparator + 'Y';
    vdFromYear:     Result := 'Y' + DateRangeSeparator;
    vdMonth:        Result := 'O';
    vdMonthRange:   Result := 'OO';
    vdMonthOnly:    Result := 'M';
    vdSeasonOnly:   Result := 'S';
    vdSeason:       Result := 'P';
    vdCentury:      Result := 'C';
    vdCenturyRange: Result := 'CC';
    vdFromCentury:  Result := 'C' + DateRangeSeparator;
    vdToCentury:    Result := DateRangeSeparator + 'C';
  else
    Result := 'U';
  end; {Case}
end;  // SetVagueDateType

//------------------------------------------------------------------------------
// Get the name of a season from the month
//------------------------------------------------------------------------------
function GetSeason(aDate: TDateTime): string;
var
  d, m, y : Word;
begin
  if not Assigned(mMonthNames) then mMonthNames := TStringList.Create;

  if Assigned(GetApplicationSuppliedSeasonNames) then
    mMonthNames.CommaText := GetApplicationSuppliedSeasonNames
  else
    mMonthNames.CommaText := ResStr_SeasonNames;

  Result := '';
  DecodeDate(aDate, y, m, d);
  case m of
    1, 2,12 : Result := mMonthNames[0]; //'Winter';
    3, 4, 5 : Result := mMonthNames[1]; //'Spring';
    6, 7, 8 : Result := mMonthNames[2]; //'Summer';
    9,10,11 : Result := mMonthNames[3]; //'Autumn'
  else
    Result := ''
  end; {Case}
end;  // GetSeason

//------------------------------------------------------------------------------
function GetCenturyFormat(AVagueDate: TVagueDate): String;
begin
  with AVagueDate do
    if DateTypeString = 'C' then
      Result := IntToStr(YearOf(StartDate) div 100 + 1) + 'c'
    else
    if DateTypeString = 'CC' then
      Result := IntToStr(YearOf(StartDate) div 100 + 1) + 'c ' + DateRangeSeparator + ' ' +
                IntToStr(YearOf(EndDate) div 100) + 'c'
    else
    if DateTypeString = 'C' + DateRangeSeparator then
      Result := IntToStr(YearOf(StartDate) div 100 + 1) + 'c ' + DateRangeSeparator
    else
    if DateTypeString = DateRangeSeparator + 'C' then
      Result := DateRangeSeparator + ' ' + IntToStr(YearOf(EndDate) div 100) + 'c'
end;

//------------------------------------------------------------------------------
// This gets the date in string form from the three fields
//------------------------------------------------------------------------------
function VagueDateToString(iVagueDate : TVagueDate): string;
begin
  Result := ''; // default
  try
    if iVagueDate.DateTypeString='' then
      Result := ''
    else begin
      case iVagueDate.DateTypeString[1] of
        'U' : Result := ResStr_Unknown;

        'D' : if iVagueDate.DateTypeString = 'D' then
                Result:=FormatDateTime(ShortDateFormat, iVagueDate.StartDate)
              else if iVagueDate.DateTypeString = 'DD' then
                Result:=FormatDateTime(shortDateFormat,iVagueDate.StartDate)+
                        ' ' + DateRangeSeparator + ' '+
                        FormatDateTime(ShortDateFormat,iVagueDate.EndDate);

        'Y' : if iVagueDate.DateTypeString = 'Y' then
                Result:=FormatDateTime('yyyy', iVagueDate.StartDate)
              else if iVagueDate.DateTypeString = 'YY' then
                Result:=FormatDateTime('yyyy', iVagueDate.StartDate)+
                        ' ' + DateRangeSeparator + ' ' +
                        FormatDateTime('yyyy', iVagueDate.EndDate)
              else if iVagueDate.DateTypeString = 'Y' + DateRangeSeparator then
                Result:=FormatDateTime('yyyy', iVagueDate.StartDate)+ ' ' + DateRangeSeparator;
        'O' : begin
                // For some reason, english month names are capitalised, but not french ones...
                // So UpCase the first character regardless of chosen language.
                if iVagueDate.DateTypeString = 'O' then
                  Result := FormatDateTime('mmmm yyyy', iVagueDate.StartDate)
                else
                if iVagueDate.DateTypeString = 'OO' then begin
                  // End date first, so month can be upcased easily.
                  Result := FormatDateTime('mmmm yyyy', iVagueDate.EndDate);
                  Result[1] := UpCase(Result[1]);
                  Result := FormatDateTime('mmmm yyyy', iVagueDate.StartDate) +
                            ' ' + DateRangeSeparator + ' ' + Result;
                end;
                Result[1] := UpCase(Result[1]);
              end;

        'M' : begin
                Result := FormatDateTime('mmmm', iVagueDate.EndDate);
                Result[1] := UpCase(Result[1]);
              end;

        'S' : Result := GetSeason(iVagueDate.EndDate);

        'P' : Result := GetSeason(iVagueDate.StartDate) + Space +
                                  FormatDateTime('yyyy', iVagueDate.EndDate);

        'C' : Result := GetCenturyFormat(iVagueDate);
      else
        if iVagueDate.DateTypeString = DateRangeSeparator + 'Y' then
          Result:=DateRangeSeparator + ' ' + FormatDateTime('yyyy', iVagueDate.EndDate)
        else if iVagueDate.DateTypeString = DateRangeSeparator + 'C' then
          Result := GetCenturyFormat(iVagueDate);;
      end; // case
    end; // if
  except
    {Conversion error}
    Result := ''
  end;
end;  // VagueDateToString

//------------------------------------------------------------------------------
//
// This is the main routine to parse a string, recover date information
// and store it in the three fields.
//
// Exception handling: ErrorMsg is used both to indicate an error
//                     has occurred and to convey the error to user.
//------------------------------------------------------------------------------
function StringToVagueDate(iDateString : string) : TVagueDate;
var
  lVagueDate                       : TVagueDate;
  RangeType                        : TDateRangeType;
  sdType, edType                   : TDateTypes;
  WorkDate, sDate, eDate, ErrorMsg : string;
  p, d, m                          : Word;

  {-----------------------------------------------------------------------------
   Set date to last day of month, must take into account different numbers
   of days in months and leap years.
  -----------------------------------------------------------------------------}
  function EndOfMonth(aDate: TDateTime): TDateTime;
  var
    d, m, y : Word;
    Work    : string;
    lBuffer : TDateTime;
  begin
    DecodeDate(aDate, y, m, d);
    case m of
      2 : begin
            d   := 29; {Try leap year}
            Work:= IntToStr(d) + DateSeparator +
                   IntToStr(m) + DateSeparator +
                   IntToStr(y);
            if not TryStrToDate(Work, lBuffer) then d := 28;
          end;
      {30 days hath September and all that ...}
      4,6,9,11:   d := 30;
    else
      d := 31;
    end; {Case}
    Work  := IntToStr(d) + DateSeparator +
             IntToStr(m) + DateSeparator +
             IntToStr(y);
    Result:= StrToDate(Work);
  end; {EndOfMonth}

  {-----------------------------------------------------------------------------
   Set date to last day of the last month in season
  -----------------------------------------------------------------------------}
  function EndOfSeason(aDate: TDateTime): TDateTime;
  var
    d, m, y : Word;
    Work    : string;
  begin
    DecodeDate(aDate, y, m, d);
    if (m + 2 > 12) and (y<9999) then
      y := y + 1; //increment the year unless just a season
    Work  := '14' + DateSeparator +
            IntToStr((m+2) mod 12) + DateSeparator +
            IntToStr(y);
    Result:= EndOfMonth(StrToDate(Work));
  end; {EndOfSeason}

  {-----------------------------------------------------------------------------
   Set the date to last day of the year
  -----------------------------------------------------------------------------}
  function EndOfYear(aDate: TDateTime): TDateTime;
  var
    d, m, y : Word;
    Work    : string;
  begin
    DecodeDate(aDate, y, m, d);
    Work := '31' + DateSeparator + '12' + DateSeparator + IntToStr(y);
    Result:=StrToDate(Work);
  end; {EndOfYear}

  {-----------------------------------------------------------------------------
   Set the date to last day of the century
  -----------------------------------------------------------------------------}
  function EndOfCentury(aDate: TDateTime): TDateTime;
  var
    d, m, y : Word;
    Work    : string;
  begin
    DecodeDate(aDate, y, m, d);
    Work := '31' + DateSeparator + '12' + DateSeparator + IntToStr(y + 99);
    Result:=StrToDate(Work);
  end; {EndOfCentury}

  {-----------------------------------------------------------------------------
   Parse the day
   Don't worry about month numbers outside range - date conversion exceptions
   will deal with this.
  -----------------------------------------------------------------------------}
  function ParseDay(aDate: string; var dType: TDateTypes): Word;
  begin
    Result := 0;
    try
      Result := StrToInt(aDate);
      dType := dtFullDate;
    except
      on EConvertError do
      ErrorMsg := Format(ResStr_InvalidDay,[aDate]);
    end;
  end; {ParseDay}

  {-----------------------------------------------------------------------------
   Get month number from string. Months may be passed as names, numbers or
   roman numerals.
  -----------------------------------------------------------------------------}
  function GetMonth(AMonth: string): Word;
  var
    m : Word;
  begin
    Result := 0;
    if not Assigned(mMonthNames) then mMonthNames := TStringList.Create;
    mMonthNames.Clear;
    // Add month names, from application if provided, or Windows if not.
    if Assigned(GetApplicationSuppliedMonthNames) then
      mMonthNames.CommaText := GetApplicationSuppliedMonthNames
    else
      // Get month names according to local language settings. Can't rely it being english!!!
      for m := 1 to 12 do
        mMonthNames.Add(FormatDateTime('mmmm', EncodeDate(1, m, 1)));

    // Add season names, from application if provided, or default if not.
    if Assigned(GetApplicationSuppliedSeasonNames) then
      mMonthNames.CommaText := mMonthNames.CommaText + ',' + GetApplicationSuppliedSeasonNames
    else
      mMonthNames.CommaText := mMonthNames.CommaText + ',' + ResStr_SeasonNames;

    // Add months in Roman numerals at the end.
    mMonthNames.CommaText := mMonthNames.CommaText + ',I,II,III,IV,V,VI,VII,VIII,IX,X,XI,XII';

    // Look for name whose first characters match the given value. Allow for names starting
    // with the same first letters to be differentiated at some point.
    // Like Juin and Juillet in French, first 3 letters are the same!
    for m := 0 to mMonthNames.Count - 1 do
      if CompareText(Copy(mMonthNames[m], 1, Length(AMonth)), AMonth) = 0 then begin
        Result := m + 1; // StringList indexes starts at 0!!!!
        Break;
      end;
    if Result > 16 then Result := Result - 16; {Roman numeral}
  end; {GetMonth}

  {-----------------------------------------------------------------------------
   Parse the month - return the month number and possible date
   type (Month or Season)
   Don't worry about month numbers outside 1-12 - date conversion exceptions
   will deal with this.
  -----------------------------------------------------------------------------}
  function ParseMonth(aDate: string; var dType: TDateTypes): Word;
  begin
    Result:=0;
    if IsNumber(aDate) then
      try
        Result := StrToInt(aDate);
      except
        on EConvertError do
        ErrorMsg := Format(ResStr_InvalidMonth,[aDate]);
      end
    else
      Result := GetMonth(aDate);
    if Result > 12 then dType := dtSeason
                   else dType := dtMonth;
    if Result = 0 then
      ErrorMsg := Format(ResStr_InvalidMonthOrSeason,[aDate]);
  end;  {ParseMonth}

  {-----------------------------------------------------------------------------
   Parse the year
   Date conversion will handle two digit year numbers - they are assumed to
   be in the current century.
  -----------------------------------------------------------------------------}
  function ParseYear(aDate: string; var dType: TDateTypes): Word;
  begin
    Result := 0;
    try
      Result := StrToInt(aDate);
      dType := dtYear;
    except
      on EConvertError do
      ErrorMsg := Format(ResStr_InvalidYear,[aDate]);
    end;
  end;  {ParseYear}

  {-----------------------------------------------------------------------------
   Extract the date from a string - dates may consiste of one (year, month or
   season), two (month year or season year) or three items (full date).
  -----------------------------------------------------------------------------}
  function ParseDate(aDate: string; var dType: TDateTypes): TDateTime;
  var
    n, d, m, y : Word;
    Work       : string;
  begin
    d     := 0;
    m     := 0;
    y     := 0;
    dType := dtUnknown;

    // Remove all spaces if dealing with a century.
    if (Pos('C', aDate) > 1) and (aDate[Pos('C', aDate) - 1] in [' ', '0'..'9']) then
      aDate := StringReplace(aDate, ' ', '', [rfReplaceAll]);

    {How many elements do we have in the date}
    n := FieldCount(aDate, Space);

    case n of
      1 : if IsNumber(aDate) then
          begin
            {Must be a year}
            y:= ParseYear(aDate, dType);
            d:= 1;
            m:= 1;
          end else
          // Check a 'C' is found and is not preceded by a letter.
          // if preceded by a letter, it could be part of a month name.
          if (Pos('C', aDate) > 1) and (aDate[Pos('C', aDate) - 1] in ['0'..'9']) then
          begin
            // Last character in string must be "C".
            if (Pos('C', aDate) <> Length(aDate)) then begin
              Result := 0;
              ErrorMsg := Format(ResStr_InvalidCentury,[aDate]);
              Exit;
            end;

            // Century
            dType := dtCentury;

            // Special case. Delphi doesn't like year 0.
            if aDate = '1C' then begin
              Result := StrToDate('1' + DateSeparator + '1' + DateSeparator + '0001');
              Exit;
            end;

            Work := RemoveStr(aDate, 'C');  // Just return the number
            // To be consistent, all century dates will start on their year 1.
            y := (StrToInt(Work) - 1) * 100 + 1; // Work out a proper YYYY year value
            d := 1;
            m := 1;
          end else begin
            {Must be a season or month name}
            m := ParseMonth(aDate, dType);
            if m > 12 then
            begin
              m := (m - 13) * 3;
              if m = 0 then m := 12;
            end;
            d := 1;
            y := 9999;
          end;

      2 : begin
            {Must be a month or season and year}
            Work := RemoveStr(aDate, Space);
            y    := ParseMonth(aDate, dType);
            m    := ParseMonth(Work, dType);
            {Update date type}
            case dType of
              dtMonth : dType := dtMonthYear;
              dtSeason: begin
                          dType := dtSeasonYear;
                          if m > 12 then
                          begin
                            m := (m - 13) * 3;
                            if m = 0 then begin //we have winter so decrement the year by one
                              m := 12;
                              y := y - 1;
                            end;
                          end
                        end
            else
              dType := dtUnknown;
            end; {Case}
            d := 1;
          end;

      3 : begin
            {Must be a full date}
            Work := RemoveStr(aDate, Space);
            d    := ParseDay(Work, dType);
            Work := RemoveStr(aDate, Space);
            m    := ParseMonth(Work, dType);
            y    := ParseYear(aDate, dType);
            dType:= dtFullDate;
          end;
    else
      dType := dtUnknown;
    end; {Case}

    {If we have a date - convert it}
    if (dType <> dtUnknown) and (ErrorMsg = '') then
      Result := StrToDate(IntToStr(d) + DateSeparator +
                          IntToStr(m) + DateSeparator +
                          IntToStr(y))
    else
      Result := 0;
  end; {ParseDate}

  {-----------------------------------------------------------------------------
   This does some error checking (first date of a range before last and start
   date not after todays date), but it mainly does the correct setting of end
   dates and the generation of the date type string
  -----------------------------------------------------------------------------}
  procedure CheckDates;
  begin
    {Have we already got an error?}
    lVagueDate.DateTypeString:='U';  // Default value until proper type set
    if ErrorMsg <> '' then Exit;

    if (lVagueDate.StartDate > lVagueDate.EndDate) And (RangeType <> drAfter) And
       (RangeType <> drBefore) then
    begin
      ErrorMsg := ResStr_BeginningMustPrecedeEnd;
      Exit;
    end;

    {if lVagueDate.StartDate > Date then
    begin
      if not ((sdType = dtMonth) or (sdType = dtSeason)) then
        ErrorMsg := 'Dates in the future are not allowed';
    end;}

    {This is the detailed stuff}
    if sdType = edType then
      case sdType of
        dtFullDate:
            begin
              case RangeType of
                drNone   : lVagueDate.DateTypeString := 'D';
                drFromTo : lVagueDate.DateTypeString := 'DD';
                drBefore : ErrorMsg := ResStr_NotSupported;
                drAfter : ErrorMsg := ResStr_NotSupported;
              end; {case}
            end;
        dtYear:
            begin
              lVagueDate.EndDate := EndOfYear(lVagueDate.EndDate);
              case RangeType of
                drNone  : lVagueDate.DateTypeString := 'Y';
                drBefore: begin
                            lVagueDate.DateTypeString := DateRangeSeparator + 'Y';
                            lVagueDate.StartDate:= 0;
                          end;
                drAfter : begin
                            lVagueDate.DateTypeString:= 'Y' + DateRangeSeparator;
                            lVagueDate.EndDate := 0;
                          end;
                drFromTo: lVagueDate.DateTypeString := 'YY';
              end; {Case}
            end;
        dtMonthYear:
            begin
              lVagueDate.EndDate := EndOfMonth(lVagueDate.EndDate);
              case RangeType of
                drNone  : lVagueDate.DateTypeString := 'O';
                drBefore: ErrorMsg := ResStr_NotSupported;
                drAfter : ErrorMsg := ResStr_NotSupported;
                drFromTo: lVagueDate.DateTypeString := 'OO';
              end; {Case}
            end;
        dtMonth:
            begin
              lVagueDate.EndDate := EndOfMonth(lVagueDate.EndDate);
              lVagueDate.DateTypeString := 'M';
            end;
        dtSeasonYear, dtSeason:
            if RangeType <> drNone then
              ErrorMsg := ResStr_NotSupported
            else begin
              {lVagueDate.DateType:= vdSeason;}
              if sdType=dtSeason then lVagueDate.DateTypeString:= 'S'
                                 else lVagueDate.DateTypeString:= 'P';
              lVagueDate.EndDate := EndOfSeason(lVagueDate.EndDate);
            end;
        dtCentury:
            begin
              lVagueDate.EndDate := EndOfCentury(lVagueDate.EndDate);
              case RangeType of
                drNone  : lVagueDate.DateTypeString := 'C';
                drBefore: begin
                            lVagueDate.DateTypeString := DateRangeSeparator + 'C';
                            lVagueDate.StartDate:= 0;
                          end;
                drAfter : begin
                            lVagueDate.DateTypeString:= 'C' + DateRangeSeparator;
                            lVagueDate.EndDate := 0;
                          end;
                drFromTo: lVagueDate.DateTypeString := 'CC';
              end; {Case}
            end;
      end {Case}
    else
      ErrorMsg := ResStr_StartDatesSameType;
  end; {CheckDate}
  {----------------------------------------------------------------------------}
begin
  {SetVagueDate}
  {Initialise - default Unknown date}
  lVagueDate.DateTypeString := '';
  lVagueDate.StartDate:= 0;
  lVagueDate.EndDate  := 0;
  RangeType           := drNone;
  ErrorMsg            := '';

  {Clean the string up for parsing}
  WorkDate := UpperCase(Trim(iDateString));
  {Change any delimeters to space}
  WorkDate := ConvertStr(WorkDate, SpecialChars, StringOfChar(Space, Length(SpecialChars)));
  {Get rid of any multiple spaces}
  WorkDate := TrimInsideStr(WorkDate);

  {If date is blank - these initialisation settings will return an Unknown date}
  if WorkDate <> '' then
  begin
    {Unknown date can be dealt with very simply}
    if (WorkDate[1] = 'U') or (WorkDate[1] = '?') then
    begin
      lVagueDate.DateTypeString := 'U';  //set vague date type correctly.
      Result := lVagueDate;
      Exit;
    end;

    {Check if we have a date range}
    p := Pos(DateRangeSeparator, WorkDate);
    if p > 0 then
      if p = 1 then
      begin
        RangeType:= drBefore;
        WorkDate := Trim(Copy(WorkDate, p+1, Length(WorkDate)-p))
      end
      else if p = Length(WorkDate) then
      begin
        RangeType:= drAfter;
        WorkDate := Trim(Copy(WorkDate, 1, Length(WorkDate)-1))
      end
      else
        RangeType := drFromTo;

    if RangeType = drFromTo then
    begin
      {Handle the second date - if necessary copy the month and/or
       year into the first date to deal with 13 - 16/3/98 or
       3 May - 6 June 98}
      sDate:= Trim(Copy(WorkDate, 1, p-1));
      eDate:= Trim(Copy(WorkDate, p+1, Length(WorkDate)-p));
      d    := FieldCount(sDate, Space);
      m    := FieldCount(eDate, Space);
      if d<m then
        sDate := sDate + Space + GetFieldStr(eDate, Space, d+1, m-d);
    end
    else if RangeType = drBefore then begin
      sDate := '';
      eDate := Trim(WorkDate);
    end
    else if RangeType = drAfter then begin
      sDate := Trim(WorkDate);
      eDate := '';
    end else
    begin
      {Its not a range - make sDate and eDate the same}
      sDate:= WorkDate;
      eDate:= sDate;
    end;
    {Extract the dates}
    lVagueDate.StartDate:= ParseDate(sDate, sdType);
    lVagueDate.EndDate  := ParseDate(eDate, edType);
    {if one of the start date or end date is unknown, but the other is known then
     make the same type}
    if ((sdType = dtUnknown) and (edType <> dtUnknown)) then
       sdType := edType;
    if ((sdType <> dtUnknown) and (edType = dtUnknown)) then
      edType := sdType;
    {Check them and get end date and type string into correct formats}
    CheckDates;
  end;

  {Have we got an error?}
  if ErrorMsg = '' then
  {if not - update the data source}
    Result := lVagueDate
  else
  {if so, raise exception - passing ErrorMsg back}
    raise EVagueDateError.Create(ErrorMsg);
end;{SetVagueDate}

//==============================================================================
{ Simple function to make a vague date out of a normal date }
function DateToVagueDate(ADate: TDateTime): TVagueDate;
begin
  Result.StartDate := ADate;
  Result.EndDate   := ADate;
  Result.DateTypeString := SetVagueDateType(vdDate);
end;  // DateToVagueDate

//==============================================================================
{ Simple function returns true if a fieldname is a vague date start field }
function IsVagueDateField(iFieldName : string): boolean;
var stUName:string;
begin
  stUName:=UpperCase(iFieldName);
  Result := (Copy(stUName, Length(stUName) - 15, 16) = 'VAGUE_DATE_START');
end;  // IsVagueDateField

//==============================================================================
{ Simple function to find the unique bit of a vague date field's name }
function GetVagueDateName( iFieldName : string): string;
var stUName:string;
begin
  stUName:=UpperCase(iFieldName);
  if not IsVagueDateField( stUName ) then
    raise EVagueDateError.Create(ResStr_InvalidField + iFieldName);
  Result := Copy(stUName, 1, Length(stUName)-16);
end;  // GetVagueDateName

//==============================================================================
function IsVagueDate(const iDateString:string):boolean;
begin
  Result:=true;
  if iDateString<>'' then
    try
      StringToVagueDate(iDateString);
    except
      on Exception do
        Result:=false;
    end;
end;  // IsVagueDate

//==============================================================================
function CheckVagueDate(const iDateString:string):boolean;
var lVagueDate:TVagueDate;
begin
  Result:=true;
  if IsVagueDate(iDateString) then begin
    lVagueDate:=StringToVagueDate(iDateString);
    // M for just Month, and S for just Season, year is 9999 in these cases
    if (lVagueDate.DateTypeString<>'M') and (lVagueDate.DateTypeString<>'S') then
      if lVagueDate.StartDate>Date then
        Result:=false;
  end else
    Result:=false;
end;  // CheckVagueDate

//==============================================================================
function CompareDateToVagueDate(const iTestDate: TDateTime; const iVDate: TVagueDate) : Integer;
var
  lTestDate: TVagueDate;
begin
  with lTestDate do
  begin
    StartDate := iTestDate;
    EndDate := iTestDate;
    DateTypeString := 'DD';
  end;
  Result := CompareVagueDateToVagueDate(lTestDate, iVDate);
end;  // CompareDateToVagueDate

//==============================================================================
function CheckDateInVagueDateRange(const iVTestDate:TDateTime; const iFrom, iTo: TVagueDate) : Boolean;
begin
  if (not (iFrom.DateTypeString[1] in UNCHECKED_TYPES)) and
     (not (iTo.DateTypeString[1] in UNCHECKED_TYPES)) then
    Result := (CompareDateToVagueDate(iVTestDate,iTo) <= 0) and (CompareDateToVagueDate(iVTestDate,iFrom) >= 0)
  else if not (iFrom.DateTypeString[1] in UNCHECKED_TYPES) then
    Result := (CompareDateToVagueDate(iVTestDate,iFrom) >= 0)
  else if not (iTo.DateTypeString[1] in UNCHECKED_TYPES) then
    Result := (CompareDateToVagueDate(iVTestDate,iTo) <= 0)
  else
    Result := False;
    // Date lies within vague date range
end;  // CheckDateInVagueDateRange

//==============================================================================
function CheckVagueDateInVagueDateRange(const iVTestDate,iFrom, iTo: TVagueDate) : Boolean;
var
  lResult : boolean;
begin
  lResult := True;      // default pass - if we can't test
  { check that the test dates are defined before calling the comparevaguedateto vaguedate function}
  if (iVTestDate.DateTypeString <> '') and (not (iVTestDate.DateTypeString[1] in UNCHECKED_TYPES)) then
  begin
    if (iFrom.DateTypeString <> '') and (not (iFrom.DateTypeString[1] in UNCHECKED_TYPES)) then
      lResult := (CompareVagueDateToVagueDate(iVTestDate,iFrom) >= 0);
    if lResult and (iTo.DateTypeString <> '') and (not (iTo.DateTypeString[1] in UNCHECKED_TYPES)) then
      lResult := (CompareVagueDateToVagueDate(iVTestDate, iTo) <= 0);
  end;
  Result := lResult;
end;  // CheckVagueDateInVagueDateRange

//==============================================================================
function CompareVagueDateToVagueDate(const iVTestDate: TVagueDate; const iVDate: TVagueDate) : Integer;
begin
  //If either date is 'Unknown', or month/season only then return success
  // (no comparison can be done)
  if (iVTestDate.DateTypeString[1] in UNCHECKED_TYPES)
      or (iVDate.DateTypeString[1] in UNCHECKED_TYPES)
  then
    Result := 1 //Standard value for success
  else case CompareVagueDates(iVTestDate, iVDate) of
    dcEqual, dcWithin:
      Result := 0;
    dcBefore, dcBeforeOverlapped:
      Result := -1;
    dcAfter, dcAfterOverlapped:
      Result := 1;
  else
    // iVTestDate includes iVDate and so is invalid
    raise EVagueDateError.Create('"'+VagueDateToString(iVTestDate)+'" ' + ResStr_Includes +
                                     ' "'+VagueDateToString(iVDate)+'".');
  end;
end;  // CompareVagueDateToVagueDate

//==============================================================================
function IsVagueDateInVagueDate(const iVTestDate, iVDate:TVagueDate):boolean;
begin
  // Mantis 62 - this test does not really make sense for Unknown dates.
  if (iVTestDate.DateTypeString = 'U') or (iVDate.DateTypeString = 'U') then
    Result := false
  else
    Result := CompareVagueDates(iVTestDate, iVDate) = dcWithin;
end;  // IsVagueDateInVagueDate

//==============================================================================
function AreVagueDatesEqual(const iVDate1, iVDate2:TVagueDate):boolean;
begin
  Result := CompareVagueDates(iVDate1, iVDate2) = dcEqual;
end;  // AreVagueDatesEqual


{ Vague date comparison routine that returns a enumerated value clearly
     defining how the 2 dates are related }
function CompareVagueDates(const iDate1: TVagueDate; const iDate2: TVagueDate) : TDateComparison;
var
  lActualDateRange1, lActualDateRange2 : TActualDateRange;
begin
  // Convert dates to something that can be compared Ok
  lActualDateRange1 := TActualDateRange.Create(iDate1);
  lActualDateRange2 := TActualDateRange.Create(iDate2);
  Result := lActualDateRange1.CompareTo(lActualDateRange2);
end;


//==============================================================================
{ TActualDateRange + TBaseActualDateRange }
//==============================================================================
{ Class that takes a vague date, and turns it into a guaranteed usable date
    range.  Vague dates themselves have the problem of 'undefined' dates
    making comparison difficult.  Eg for date type -Y you cannot reliably use
    the start date, but it is awkward to code tests for the date type every
    time you use it.  This replaces it with the minimum possible date so
    comparisons work }

{ Constructor just stores the vague date }
constructor TBaseActualDateRange.Create(iVagueDate: TVagueDate);
begin
  inherited Create;
  FVagueDate := iVagueDate;
end;


{ Overloaded version of constructor builds the class using 2 vague dates,
    finding the maximum possible date range.  Bear in mind that a vague date
    range can actually be represented as a single vague date }
constructor TBaseActualDateRange.Create(iStartVagueDate, iEndVagueDate: TVagueDate);
begin
  inherited Create;
  FVagueDate.DateTypeString := 'DD'; // always use this for exact info

  if (iStartVagueDate.DateTypeString = DateRangeSeparator + 'Y')
      or (iStartVagueDate.DateTypeString = DateRangeSeparator + 'C')
      or (iStartVagueDate.DateTypeString[1] in UNCHECKED_TYPES)
      or (iStartVagueDate.DateTypeString = '')
  then
    FVagueDate.StartDate := MIN_DATE
  else
    FVagueDate.StartDate := iStartVagueDate.StartDate;

  if (iEndVagueDate.DateTypeString = 'Y' + DateRangeSeparator)
      or (iEndVagueDate.DateTypeString = 'C' + DateRangeSeparator)
      or (iEndVagueDate.DateTypeString[1] in UNCHECKED_TYPES)
      or (iEndVagueDate.DateTypeString = '')
  then
    FVagueDate.EndDate := MAX_DATE
  else
    FVagueDate.EndDate := iEndVagueDate.EndDate;
end;


{ Accessor method for end date.  Traps date types that mean the end date is
   undefined, and returns the max possible date instead }
function TBaseActualDateRange.GetEndDate: TDateTime;
begin
  if (FVagueDate.DateTypeString = 'Y' + DateRangeSeparator)
      or (FVagueDate.DateTypeString = 'C' + DateRangeSeparator)
      or (FVagueDate.DateTypeString[1] in UNCHECKED_TYPES)
      or (FVagueDate.DateTypeString = '')
  then
    Result := MAX_DATE
  else
    Result := FVagueDate.EndDate;
end;


{ As above for start date }
function TBaseActualDateRange.GetStartDate: TDateTime;
begin
  if (FVagueDate.DateTypeString = DateRangeSeparator + 'Y')
      or (FVagueDate.DateTypeString = DateRangeSeparator + 'C')
      or (FVagueDate.DateTypeString[1] in UNCHECKED_TYPES)
      or (FVagueDate.DateTypeString = '')
  then
    Result := MIN_DATE
  else
    Result := FVagueDate.StartDate;
end;


//==============================================================================
{ TActualDateRange }
//==============================================================================
{ Produces a date comparison result for this date range against another date
  range.

    [----]                     dcEqual
    [----]

    [--]      or      [--]     dcWithin
    [----]          [----]

        [--]                   dcAfter
    [--]

    [----]    or       [----]  dcAfterOverlapped
    [--]            [----]

    [--]                       dcBefore
        [--]

    [----]    or    [----]     dcBeforeOverlapped
      [--]             [----]

    [----]                     dcContains
     [--]
}
function TActualDateRange.CompareTo(
  iTestDate: TBaseActualDateRange): TDateComparison;
begin
  if StartDate = iTestDate.StartDate then begin
    if EndDate = iTestDate.EndDate then
      Result := dcEqual
    else if EndDate < iTestDate.EndDate then
      Result := dcWithin
    else // if EndDate > iTestDate.EndDate then
      Result := dcAfterOverlapped;
  end else if StartDate < iTestDate.StartDate then begin
    if EndDate = iTestDate.EndDate then
      Result := dcBeforeOverlapped
    else if EndDate < iTestDate.EndDate then begin
      if EndDate < iTestDate.StartDate then
        Result := dcBefore
      else
        Result := dcBeforeOverlapped
    end else // if EndDate > iTestDate.EndDate then
      Result := dcContains;
  end else begin // if StartDate > iTestDate.StartDate
    if EndDate <= iTestDate.EndDate then
      Result := dcWithin
    else // if EndDate > iTestDate.EndDate then begin
      if StartDate > iTestDate.EndDate then
        Result := dcAfter
      else
        Result := dcAfterOverlapped;
  end;
end;

//==============================================================================
initialization
  mMonthNames := nil;
  GetApplicationSuppliedMonthNames := nil;
  GetApplicationSuppliedSeasonNames := nil;
  SpecialChars := '/.=_,#@:;#!$%^&*<@{}[]()+\|';
  // if user has a '-' date separator, then date ranges must use > to separate
  // dates, e.g. 10-02-2005>14-03-2005
  if DateSeparator='-' then begin
    DateRangeSeparator := '>';
    SpecialChars := SpecialChars + '-';
  end
  else begin
    DateRangeSeparator := '-';
    SpecialChars := SpecialChars + '>';
  end;


finalization
  // If it's been used, don't forget to get rid of it.
  if Assigned(mMonthNames) then mMonthNames.Free;

end.
