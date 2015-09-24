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
-----------------------------------------------------------------
------------------------------------------------------------------}
interface

uses SysUtils;

const Space = Chr(32);
      Comma = Chr(44);

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

implementation

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
begin
     Work:=Trim(InStr);
     Result:='';
     Last := Char(0);
     If Length(Work) > 0 then
        For p:=1 to Length(Work) do
        begin
            if Work[p] = Space then
            begin
               if Last <> Space then
                  Result:=Concat(Result,Work[p]);
            end else Result:=Concat(Result,Work[p]);
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
             else r:=string('');
             repeat
                   p:=Pos(FromStr[i],Work);
                   if p>0 then
                      Work:=Concat(LeftStr(Work,p-1), string(r),
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

{$HINTS OFF}
// Suppress the hint about var n never being used
Function IsNumber(const InStr: string): Boolean;
{Is InStr a valid number?}
var code: integer;
    n: real;
begin
     try
        Val(InStr, n, code);
        Result:=(code = 0);
     except
        Result:=False;
     end;
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

end.
