unit GenFuncs;
{-----------------------------------------------------------------
Author:    Dr Stuart G. Ball
Version:   4
Date:      24 Jan 1999
Purpose:   General rouines for string handling which provide Arev like
           string handling functionality
Copyright: Joint Nature Conservation Committee, 1999
Modified:
date       initials  changes made

Additional functions added by Dorset Software
-----------------------------------------------------------------
------------------------------------------------------------------}
interface

uses SysUtils, Classes, Forms;

const Space = Chr(32);
      Comma = Chr(44);
type
  TComparison = (cpFirst, cpSecond, cpEqual);

{General string handling routines}
Function LeftStr(const InStr: String; const n: integer): String;
Function RightStr(const InStr: String; const n: integer): String;
Function TrimInsideStr(const InStr: String): String;
Function RemoveStr(var InStr: String; const Delim: Char): String;
Function FieldCount(const InStr: String; const Delim: Char): Integer;
Function GetFieldStr(const InStr: String; const Delim: Char; const nStart, nFields: integer): string;
Function LocateStr(const InStr, FindStr: String; const Delim: Char; const CaseSensitive: Boolean): integer;
Function ConvertStr(const InStr, FromStr, ToStr: String): String;
Function SwapStr(const InStr, FromStr, ToStr: string): string;
Function IsNumber(const InStr: string): Boolean;
Function IsAlpha(const InStr: string): Boolean;
function StripAmpersands( iText : string ): string;
procedure ParseSemiColonString(iList: string; ioStrings : TStrings);
procedure ParseCommaSepString(iList: string; ioStrings : TStrings);
function Min( iVal1, iVal2 : integer ): integer;
function Max( iVal1, iVal2 : integer ): integer;
function ReadableFormat( const iText : string ) : string;
function StringListToCommaSepList(AStringList : TStringList): string;
function GetPlural( iNumber : integer; const iDescriptor : string ): string;
function MaxKey(const iKey1, iKey2: string): TComparison;
function FloorDP(iNumber: double; iLog: integer): string;

implementation

uses math;

var mDBNames: TStringList;

Function LeftStr(const InStr: String; const n: integer): String;
{Return a string containing the n leftmost characters
of InStr. If n<=0, return an empty string. If n > length of
InStr then return the whole of InStr.}
begin
     If n <= 0 then
        Result:=''
     else
         If n > Length(InStr) then
            Result:=InStr
         else
            Result:=Copy(InStr,1,n);
end;

Function RightStr(const InStr: String; const n: integer): String;
{Return a string containing the n rightmost characters
of InStr. If n<=0, return an empty string. If n > length of
InStr then return the whole of InStr. }
begin
     If n <= 0 then
        Result:=''
     else
         If n > Length(InStr) then
            Result:=InStr
         else
            Result:=Copy(InStr,Length(InStr)-n+1,n)
end;

Function TrimInsideStr(const InStr: String): String;
{Does the same as Trim, but in addition, it reduces
any embedded multiple spaces to only one space character
(equivalent to Arev TRIM function).}
var Work: string;
    p: integer;
    Last: Char;
const
  Whitespace = [#13,#10, ' ', ^I];
begin
     Work:=Trim(InStr);
     Result := '';
     Last := #0;
     If Length(Work) > 0 then
        For p:=1 to Length(Work) do
        begin
            if Work[p] in Whitespace then
            begin
               if not (Last in WhiteSpace) then
                  Result:=Concat(Result, ' ');
            end
            else
              Result:=Concat(Result,Work[p]);
            Last:=Work[p];
        end;
end;

Function RemoveStr(var InStr: String; const Delim: Char): String;
{InStr consists of a series of substrings separated by a
delinator (e.g. 23,45,67) passed in Delim (e.g. (Comma).
Return the first substring (23) and remove it from InStr
(e.g. InStr is returned as 45,67). If InStr does not
contain Delim the whole of InStr will be returned as the
result and InStr will be set to empty.
A bit like Arev REMOVE x FROM y AT Col USING delim SETTING More}
var p: integer;
    Work: String;
begin
    Work:=Trim(InStr);
    If Work[Length(Work)] <> Delim then
          Work:=Concat(Work,Delim);
     p:=Pos(Delim,Work);
     If p > 1 then
          Result:=Trim(LeftStr(Work, p-1))
     else Result:='';
     p:=Pos(Delim, InStr);
     If (p > 0) and (p < Length(InStr)) then
          InStr:=RightStr(InStr,Length(InStr)-p)
     else InStr:='';
end;

Function FieldCount(const InStr: String; const Delim: Char): Integer;
{InStr consists of a series of substrings separated by a
delinator (e.g. 23,45,67) passed in Delim (e.g. (Comma).
This function returns the number of Substrings in InStr.
If InStr is empty it returns 0. If it is not empty,
but doesn't contain Delim, it returns 1.
Equivalent to Arev FIELDCOUNT function (and not like
COUNT which just returned the number of times Delim
is in InStr - which is one less than the number of substrings).}
var i,n: integer;
    Work: String;
begin
     n:=0;
     Work:=Trim(InStr);
     If Length(Work)>0 then
     begin
          If Work[Length(Work)] <> Delim then
             Work:=Concat(Work,Delim);
          For i:=1 to Length(Work) do
              If Work[i] = Delim then
                 inc(n);
     end;
     Result:=n;
end;

Function GetFieldStr(const InStr: String; const Delim: Char; const nStart, nFields: integer): string;
{InStr consists of a series of substrings separated by a
delinator (e.g. 23,45,67) passed in Delim (e.g. (Comma).
This function returns nFields substrings starting at
nStart}
var i: integer;
    Work, Temp: string;
begin
     Result := '';
     Work := InStr;
     i := 0;
     While (Length(Work) > 0) AND (i < nStart+nFields-1) do
     begin
          Temp := RemoveStr(Work, Delim);
          inc(i);
          If (i >= nStart) AND (i <= nStart+nFields-1) then
             Result := Result + Temp + Delim;
     end;
     {Trim trailing Delim}
     If Result <> '' then
        Result := LeftStr(Result, Length(Result)-1);
end;

Function LocateStr(const InStr, FindStr: String; const Delim: Char; const CaseSensitive: Boolean): integer;
{InStr consists of a series of substrings separated by a
 delinator (e.g. 23,45,67) passed in Delim (e.g. (Comma).
 This function looks for FindStr and returns its position.
 CaseSensitive determines whether of not case is taken into
 account when looking for a match. Returns 0 if FindStr
 is not found.}
var i, f: integer;
    Work, Temp: string;
begin
     Result := 0;
     If (InStr='') OR (FindStr='') then Exit;
     Work := InStr;
     i:= 0;
     Repeat
          Temp := RemoveStr(Work, Delim);
          inc(i);
          If CaseSensitive then
             f := CompareStr(Temp, FindStr)
          else
             f := CompareText(Temp, FindStr);
          If f=0 then Result := i;
     Until (Length(Work) <= 0) OR (Result > 0);
end;

Function ConvertStr(const InStr, FromStr, ToStr: String): String;
{Returns a string in which characters from FromStr in InStr
are replaced by the corresponding character in ToStr.
e.g. ConvertStr('Stuart Ball','rat','123') will return
'S3u213 B2ll' - r is replaced by 1, a by 2, etc.
If ToStr is longer than FromStr then the additional characters
are never used. Thus if ToStr was '12345' in the example, '4'
and '5' would be ignored.
If ToStr is shorter than FormStr then the extra characters in
FromStr are deleted. Thus if ToStr was '12' in the example,
this would have the effect of deleting 't' from InStr and
the result would be 'Su21 B2ll'.
Equivalent to Arev CONVERT x TO y IN str.}
var i,p: integer;
    Work: string;
    r: string[1];
begin
     Work:=Instr;
     If (Length(Work)>0) and (Length(FromStr)>0) then
        For i:=1 to Length(FromStr) do
        begin
             if i<=Length(ToStr) then
                r:=ToStr[i]
             else r:='';
             repeat
                   p:=Pos(FromStr[i],Work);
                   if p>0 then
                      Work:=Concat(LeftStr(Work,p-1),r,
                            RightStr(Work,Length(Work)-p));
             until p=0;
        end;
     Result:=Work;
end;

Function SwapStr(const InStr, FromStr, ToStr: string): string;
{Returns a string with FromStr replaced with ToStr in InStr.
Equivalent to Arev SWAP x WITH y IN str.}
var Work: String;
    p: integer;
begin
     Work:=InStr;
     If (Length(Work) > 0) and (Length(FromStr) > 0) then
        If FromStr <> ToStr then
        Repeat
              p:=Pos(FromStr, Work);
              if p > 0 then
                 Work:=Concat(LeftStr(Work,p-1),ToStr,
                       RightStr(Work,Length(Work)-p-Length(FromStr)+1));
        until p=0;
     Result:=Work;
end;

// Don't want hint on the unused 'n'. We know that!
{$HINTS OFF}
Function IsNumber(const InStr: string): Boolean;
{Is InStr a valid number?}
var lDummy: Extended;
begin
  Result := TextToFloat(PChar(InStr), lDummy, fvExtended);
end;
{$HINTS ON}

Function IsAlpha(const InStr: string): Boolean;
{Does InStr consist only of alphabetic characters
and spaces?}
var Work: String;
    p: integer;
begin
    Work:=UpperCase(Instr);
    Result:=True;
    If Length(Work) > 0 then
       For p:=1 to Length(Work) do
           if not (Work[p] in ['A'..'Z',Space]) then
           begin
              Result:=False;
              Exit;
           end;
end;



function StripAmpersands(iText: string): string;
{ Function to strip ampersands from a string.  Allows comparison of strings
     without being affected by shortcut specifications.  Also uppercases the
     result }
var
  i : integer;
begin
  Result := '';
  for i := 1 to Length(iText) do
    if iText[i] <> '&' then
      Result:=Result+Uppercase(iText[i]);
end;  // StripAmpersands


{ Takes a string separated by semi colons, returns a string list in the
     ioStrings parameter }
procedure ParseSemiColonString(iList: string; ioStrings : TStrings);
var
  lParseString : string;
begin
  ioStrings.Clear;
  { Parse the iList string into an array (if it has semi-colon separators) }
  lParseString := iList;
  while Pos(';', lParseString) <> 0 do
  begin
    { Read the next item from the parse string }
    ioStrings.Add(Copy(lParseString, 1, Pos(';', lParseString) - 1));
    { Take remaining string and continue parsing }
    lParseString := Copy(lParseString, Pos(';', lParseString) + 1, High(integer));
  end;
  { Read the final item from the parse string }
  ioStrings.Add(lParseString);
end;

{ Takes a string separated by commas, returns a string list in the
     ioStrings parameter }
procedure ParseCommaSepString(iList: string; ioStrings : TStrings);
var
  lParseString : string;
begin
  ioStrings.Clear;
  { Parse the iList string into an array (if it has semi-colon separators) }
  lParseString := iList;
  while Pos(',', lParseString) <> 0 do
  begin
    { Read the next item from the parse string }
    ioStrings.Add(Copy(lParseString, 1, Pos(',', lParseString) - 1));
    { Take remaining string and continue parsing }
    lParseString := Copy(lParseString, Pos(',', lParseString) + 1, High(integer));
  end;
  { Read the final item from the parse string }
  ioStrings.Add(lParseString);
end;


{ Returns the minimum value of the 2 supplied }
function Min( iVal1, iVal2 : integer ): integer;
begin
  if iVal1 < iVal2 then
    Result := iVal1
  else
    Result := iVal2;
end;


{ Returns the maximum value of the 2 supplied }
function Max( iVal1, iVal2 : integer ): integer;
begin
  if iVal1 > iVal2 then
    Result := iVal1
  else
    Result := iVal2;
end;


{ Makes a readable representation of a text string used for a table or field
    title.  For example, SURVEY_EVENT is returned as Survey Event. The readable
    name is either calculated, or read from a file called TableNames.txt in the
    application directory. }
function ReadableFormat( const iText : string ) : string;
var
  lstWorkingName : string;
  liIdx : integer;
begin
  if mDBNames.Values[lowercase(iText)]<>'' then
    result := mDBNames.Values[iText]
  else begin
    lstWorkingName := iText;
    Result := '';
    repeat
      lstWorkingName := Uppercase(copy(lstWorkingName,0,1)) +
                      LowerCase(copy(lstWorkingName,2,length(lstWorkingName)));
      liIdx := Pos('_',lstWorkingName);
      if liIdx = 0 then
        Result := Result + lstWorkingName
      else
        Result := Result + copy(lstWorkingName, 0, liIdx - 1) + ' ';
      lstWorkingName := copy(lstWorkingName,liIdx + 1,length(iText));
    until liIdx = 0;
  end;
end;


{ Makes a comma separated string out of a string list }
function StringListToCommaSepList(AStringList : TStringList): string;
var lIdx:integer;
begin
  Result:='';
  with AStringList do begin
    for lIdx:= 0 to Count - 2 do
      Result := Result + Strings[lIdx] + ', ';
    if AStringList.Count>0 then
      Result := Result + Strings[Count -1]
  end;
end;  // StringListToCommaSepList



{ General purpose function to construct a plural where necessary.  For example,
    pass in 1, 'item' and the result is '1 item'.  Pass in 2, 'item' and the
    result is '2 items'. }
function GetPlural( iNumber : integer; const iDescriptor : string ): string;
begin
  Result := IntToStr(iNumber) + ' ' + iDescriptor;
  if iNumber<>1 then // tag s on if necessary
    Result := Result + 's';
end;


{ MaxKey.  Returns the greatest of the two keys in a TComparison type }
function MaxKey(const iKey1, iKey2: string): TComparison;
begin
  if iKey1 > iKey2 then
    Result := cpFirst
  else if iKey1 = iKey2 then
    Result := cpEqual
  else 
    Result := cpSecond;
end;



//This function floors a number to the number of decimal
//places specified in iLog. E.g.
//   FloorDP(1563.4,2)='1500'
//   FloorDP(1563.4,-2)='1563.40'
//Created by Polly Shaw

function FloorDP(iNumber: double; iLog: integer): String;
var
  lproduct:Double;
  lFloatingResult:Double;
  lFormatOption:String;
  i:integer;
begin
  {This is not the most efficient way of doing this but
  is easiest to code}
  lproduct:=iNumber*Power(10,-iLog);
  lFloatingResult:=Floor(lProduct)*Power(10,iLog);
  if iLog<0 then
  begin
    lFormatOption:='0.';
    for i:=1 to -iLog do
      lFormatOption:=lFormatOption+'0';
    Result:=FormatFloat(lFormatOption,lFloatingResult);
  end
  else
    Result:=floatToStr(lFloatingResult);
end;

initialization
  mDBNames := TStringList.Create;
  if FileExists(ExtractFilePath(Application.Exename)+'DBNames.txt') then
    mDBNames.LoadFromFile(ExtractFilePath(Application.Exename)+'DBNames.txt');

finalization
  FreeAndNil(mDBNames);



end.
