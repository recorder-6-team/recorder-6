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
   Unknown                        U      <empty>      <empty>

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
  SysUtils, db, GenFuncs;

const
  {These could be used as delimeters in dates.
   Note: must not include space, ? or - }
  SpecialChars = '/.=_,#@:;#!$%^&*<>@{}[]()+\|';

type
  TVagueDateTypes = (vdUnknown, vdDate, vdDateRange, vdYear, vdYearRange,
                            vdToYear, vdFromYear, vdMonth, vdMonthRange,
                            vdMonthOnly, vdSeasonOnly, vdSeason);

  TDateTypes      = (dtUnknown, dtYear, dtMonth, dtSeason, dtMonthYear,
                            dtSeasonYear, dtFullDate);

  TDateRangeType  = (drNone, drBefore, drAfter, drFromTo);

  TVagueDate = record
    StartDate      : TDateTime;
    EndDate        : TDateTime;
    {DateType       : TVagueDateTypes; }
    DateTypeString : string[2];
  end;

  EVagueDateError = class(Exception);

{ The global functions made available...}
function VagueDateToString(iVagueDate : TVagueDate): string;
function StringToVagueDate(iDateString : string) : TVagueDate;
function IsVagueDateField(iFieldName : string): boolean;
function GetVagueDateName( iFieldName : string): string;
function IsVagueDate(const iDateString:string):boolean;
function CheckVagueDate(const iDateString:string):boolean;
//
function CheckDateInVagueDateRange(const iVTestDate:TDateTime; const iFrom, iTo: TVagueDate) : Boolean;
function CompareDateToVagueDate(const iTestDate: TDateTime; const iVDate: TVagueDate) : Integer;
function CompareVagueDateToVagueDate(const iVTestDate: TVagueDate; const iVDate: TVagueDate) : Integer;
function CheckVagueDateInVagueDateRange(const iVTestDate,iFrom, iTo: TVagueDate) : Boolean;
//==============================================================================
implementation

const
  EST_INVALID_FIELD = 'The field name supplied is not a vague date field :';

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
    vdToYear:       Result := '-Y';
    vdFromYear:     Result := 'Y-';
    vdMonth:        Result := 'O';
    vdMonthRange:   Result := 'OO';
    vdMonthOnly:    Result := 'M';
    vdSeasonOnly:   Result := 'S';
    vdSeason:       Result := 'P'
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
  Result := '';
  DecodeDate(aDate, y, m, d);
  case m of
    1, 2,12 : Result := 'Winter';
    3, 4, 5 : Result := 'Spring';
    6, 7, 8 : Result := 'Summer';
    9,10,11 : Result := 'Autumn'
  else
    Result := ''
  end; {Case}
end;  // GetSeason

//------------------------------------------------------------------------------
// This gets the date in string form from the three fields
//------------------------------------------------------------------------------
function VagueDateToString(iVagueDate : TVagueDate): string;
Var IFormatSettings : TFormatSettings;
begin
  Result := ''; // default
  try
    case iVagueDate.DateTypeString[1] of
      'U' : Result := 'Unknown';

      'D' : if iVagueDate.DateTypeString = 'D' then
              Result:=FormatDateTime(IFormatSettings.ShortDateFormat, iVagueDate.StartDate)
            else if iVagueDate.DateTypeString = 'DD' then
              Result:=FormatDateTime(IFormatSettings.ShortDateFormat,iVagueDate.StartDate)+' - '+
                      FormatDateTime(IFormatSettings.ShortDateFormat,iVagueDate.EndDate);

      'Y' : if iVagueDate.DateTypeString = 'Y' then
              Result:=FormatDateTime('yyyy', iVagueDate.StartDate)
            else if iVagueDate.DateTypeString = 'YY' then
              Result:=FormatDateTime('yyyy', iVagueDate.StartDate)+ ' - ' +
                      FormatDateTime('yyyy', iVagueDate.EndDate)
            else if iVagueDate.DateTypeString = 'Y-' then
              Result:=FormatDateTime('yyyy', iVagueDate.StartDate)+ ' -';

      '-' : if iVagueDate.DateTypeString = '-Y' then
              Result:='- ' + FormatDateTime('yyyy', iVagueDate.StartDate);

      'O' : if iVagueDate.DateTypeString = 'O' then
              Result:=FormatDateTime('mmmm yyyy', iVagueDate.StartDate)
            else if iVagueDate.DateTypeString = 'OO' then
              Result:=FormatDateTime('mmmm yyyy', iVagueDate.StartDate)+' - '+
                      FormatDateTime('mmmm yyyy', iVagueDate.EndDate);

      'M' : Result:=FormatDateTime('mmmm', iVagueDate.EndDate);

      'S' : Result:=GetSeason(iVagueDate.EndDate);

      'P' : Result:=GetSeason(iVagueDate.StartDate) + Space +
                    FormatDateTime('yyyy', iVagueDate.StartDate);
    end; // case
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
    IFormatSettings : TFormatSettings;
  begin
    DecodeDate(aDate, y, m, d);
    case m of
      2 : begin
            d   := 29; {Try leap year}
            Work:= IntToStr(d) + IFormatSettings.DateSeparator +
                   IntToStr(m) + IFormatSettings.DateSeparator +
                   IntToStr(y);
            try
              StrToDate(Work);
            except
              {Conversion error, so its not a leap year}
              on EConvertError do d:=28;
            end;
          end;
      {30 days hath September and all that ...}
      4,6,9,11:   d := 30;
    else
      d := 31;
    end; {Case}
    Work  := IntToStr(d) + IFormatSettings.DateSeparator +
             IntToStr(m) + IFormatSettings.DateSeparator +
             IntToStr(y);
    Result:= StrToDate(Work);
  end; {EndOfMonth}

  {-----------------------------------------------------------------------------
   Set the date to last day of the year
  -----------------------------------------------------------------------------}
  function EndOfYear(aDate: TDateTime): TDateTime;
  var
    d, m, y : Word;
    Work    : string;
    IFormatSettings : TFormatSettings;
   begin
    DecodeDate(aDate, y, m, d);
    Work := '31' + IFormatSettings.DateSeparator + '12' + IFormatSettings.DateSeparator + IntToStr(y);
    Result:=StrToDate(Work);
  end; {EndOfYear}

  {-----------------------------------------------------------------------------
   Set date to last day of the last month in season
  -----------------------------------------------------------------------------}
  function EndOfSeason(aDate: TDateTime): TDateTime;
  var
    d, m, y : Word;
    Work    : string;
    IFormatSettings : TFormatSettings;
  begin
    DecodeDate(aDate, y, m, d);
    Work  := '14' + IFormatSettings.DateSeparator +
            IntToStr((m+2) mod 12) + IFormatSettings.DateSeparator +
            IntToStr(y);
    Result:= EndOfMonth(StrToDate(Work));
  end; {EndOfSeason}

  {-----------------------------------------------------------------------------
   Get month number from string. Months may be passed as names, numbers or
   roman numerals.
  -----------------------------------------------------------------------------}
  function GetMonth(aMonth: string): Word;
  const Months = 'JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC,' +
                 'WIN,SPR,SUM,AUT,' +
                 'I,II,III,IV,V,VI,VII,VIII,IX,X,XI,XII';
  var
    m : Word;
  begin
    m     := LocateStr(Months, Copy(aMonth, 1, 3), Comma, False);
    if m > 16 then m := m-16; {Roman numeral}
    Result := m;
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
        ErrorMsg := Format('%s is not a valid month.',[aDate]);
      end
    else
      Result := GetMonth(aDate);
    if Result > 12 then dType := dtSeason
                   else dType := dtMonth;
    if Result = 0 then
      ErrorMsg := Format('"%s" is not a valid month or season name.',[aDate]);
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
      ErrorMsg := Format('"%s" is not a valid year.',[aDate]);
    end;
  end;  {ParseYear}

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
      ErrorMsg := Format('"%s" is not a valid day number.',[aDate]);
    end;
  end; {ParseDay}

  {-----------------------------------------------------------------------------
   Extract the date from a string - dates may consiste of one (year, month or
   season), two (month year or season year) or three items (full date).
  -----------------------------------------------------------------------------}
  function ParseDate(aDate: string; var dType: TDateTypes): TDateTime;
  var
    n, d, m, y : Word;
    Work       : string;
    IFormatSettings :TFormatSettings;
    begin
    d    :=0;
    m    :=0;
    y    :=0;
    dType:= dtUnknown;

    {How many elements do we have in the date}
    n    := FieldCount(aDate, Space);

    case n of
      1 : if IsNumber(aDate) then
          begin
            {Must be a year}
            y:= ParseYear(aDate, dType);
            d:= 1;
            m:= 1;
          end else begin
            {Must be a season or month name}
            m := ParseMonth(aDate, dType);
            if m>12 then
            begin
              m := (m-13)*3;
              if m=0 then m:=12;
            end;
            d:= 1;
            y:= 9999;
          end;

      2 : begin
            {Must be a month or season and year}
            Work:= RemoveStr(aDate, Space);
            y   := ParseMonth(aDate, dType);
            m   := ParseMonth(Work, dType);
            {Update date type}
            case dType of
              dtMonth : dType := dtMonthYear;
              dtSeason: begin
                          dType := dtSeasonYear;
                          if m>12 then
                          begin
                            m := (m-13)*3;
                            if m=0 then m:=12;
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
      try
         Result := StrToDate(IntToStr(d) + IFormatSettings.DateSeparator + IntToStr(m) +
                   IFormatSettings.DateSeparator + IntToStr(y))
      except
            begin
                 ErrorMsg := 'Date format is not valid!';
                 Result := 0;
                 Exit;
            end;
      end
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

    if lVagueDate.StartDate > lVagueDate.EndDate then
    begin
      ErrorMsg := 'Beginning of a date range' + #13 + 'must be before the end!';
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
            if lVagueDate.StartDate = lVagueDate.EndDate then
              lVagueDate.DateTypeString := 'D'
            else
              lVagueDate.DateTypeString := 'DD';
        dtYear:
            begin
              lVagueDate.EndDate := EndOfYear(lVagueDate.EndDate);
              case RangeType of
                drNone  : lVagueDate.DateTypeString := 'Y';
                drBefore: begin
                            lVagueDate.DateTypeString := '-Y';
                            lVagueDate.StartDate:= 0;
                          end;
                drAfter : begin
                            lVagueDate.DateTypeString:= 'Y-';
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
                drBefore: ErrorMsg := 'Not supported';
                drAfter : ErrorMsg := 'Not supported';
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
              ErrorMsg := 'Not supported'
            else begin
              {lVagueDate.DateType:= vdSeason;}
              if sdType=dtSeason then lVagueDate.DateTypeString:= 'S'
                                 else lVagueDate.DateTypeString:= 'P';
              lVagueDate.EndDate := EndOfSeason(lVagueDate.EndDate);
            end;
      end {Case}
    else
      ErrorMsg := 'Dates at beginning and end of range' + #13 +
                  'must be of the same type';
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
    p := Pos('-', WorkDate);
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
    else
    begin
      {Its not a range - make sDate and eDate the same}
      sDate:= WorkDate;
      eDate:= sDate;
    end;

    {Extract the dates}
    lVagueDate.StartDate:= ParseDate(sDate, sdType);
    if ErrorMsg = '' then
       lVagueDate.EndDate  := ParseDate(eDate, edType);

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
    raise EVagueDateError.Create(EST_INVALID_FIELD + iFieldName);
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
begin
  if (iVDate.StartDate <> 0) and (iTestDate < iVDate.StartDate) then
    Result := -1
  else if (iVDate.EndDate <> 0) and (iTestDate > iVDate.EndDate) then
    Result := 1
  else
    Result := 0;
end;

//==============================================================================

function CheckDateInVagueDateRange(const iVTestDate:TDateTime; const iFrom, iTo: TVagueDate) : Boolean;
begin
  if (iFrom.DateTypeString <> 'U') and (iTo.DateTypeString <> 'U') then
    Result := (CompareDateToVagueDate(iVTestDate,iTo) <= 0) and (CompareDateToVagueDate(iVTestDate,iFrom) >= 0)
  else if iFrom.DateTypeString <> 'U' then
    Result := (CompareDateToVagueDate(iVTestDate,iFrom) >= 0)
  else if iTo.DateTypeString <> 'U' then
    Result := (CompareDateToVagueDate(iVTestDate,iTo) <= 0)
  else
    Result := False;
    // Date lies within vague date range
end;

//==============================================================================

function CheckVagueDateInVagueDateRange(const iVTestDate,iFrom, iTo: TVagueDate) : Boolean;
begin
  Result := (CompareVagueDateToVagueDate(iVTestDate,iFrom)>= 0) and (CompareVagueDateToVagueDate(iVTestDate,iTo) <= 0);
end;

//==============================================================================

function CompareVagueDateToVagueDate(const iVTestDate: TVagueDate; const iVDate: TVagueDate) : Integer;
var
  lStartResult,lEndResult : Integer;
begin

  lStartResult := CompareDateToVagueDate(iVTestDate.StartDate,iVDate);
  lEndResult := CompareDateToVagueDate(iVTestDate.EndDate,iVDate);

  if (iVTestDate.EndDate <> 0) and (iVTestDate.StartDate <> 0) and (lStartResult = 0) and (lEndResult = 0) then
    Result := 0     // iVTestDate is within iVDate
  else if (iVDate.EndDate <> 0) and (lEndResult <= 0) then
    Result := -1 // iVTestDate is before iVDate
  else if (iVDate.StartDate <> 0) and (lStartResult >= 0) then
    Result := 1 // iVTestDate is after iVDate
  else
    // iVTestDate includes iVDate and so is invalid
    raise EVagueDateError.Create('The date includes the date to be compared against.');
end;



end.
