//------------------------------------------------------------------------------------------------
// Code to get the plain text from an RTF memo
// This was converted from Access VBA
//------------------------------------------------------------------------------------------------
unit RTF;

interface

uses
  SysUtils, StrUtils;

  Function StripRTF(const RTF: string): string;

implementation

//------------------------------------------------------------------------------------------------
// Code words consists of \word[nnn] and are terminated by a character that is not a digit
// letter or -, but usually by a space (which is part of the code word)
// \\, \{ and \} are special cases allowing the characters \{} to be included in the text
//------------------------------------------------------------------------------------------------
Function RemoveCodeWord(var sTemp: string): boolean;
var e,p: integer;
    NextChar, work: string;
begin
     Work := sTemp;
     p := Pos('\', Work);
     If p > 0 then
     begin
          NextChar := Copy(Work, p + 1, 1);
          e := Pos(NextChar, '\{}');
          If e > 0 then
          begin
             sTemp := Copy(Work, 1, p - 1) + RightStr(Work, (Length(Work) - p));
             RemoveCodeWord := True;
          end
          else
          begin
             e := PosEx(' ', Work, p+1);
             If e < 1 then e := Length(Work);
             sTemp := Copy(Work, 1, p - 1) + RightStr(Work, Length(Work) - e);
             RemoveCodeWord := True;
          end;
     end
     else RemoveCodeWord := False;
end;

Function StripCodes(const sTemp: string): string;
var work: string;
begin
     work := sTemp;
     while RemoveCodeWord(work) do; //nothing
     Result := work;
end;

Function GetPara(const sTemp: string; const IsFirst: boolean): string;
const Para = '\par';
      FirstPara = '\pard';
var e,p: integer;
    work: string;
begin
     Work := sTemp;
     If IsFirst then
        p := Pos( FirstPara, Work)
     else
        p := Pos(Para, Work);

     If p > 0 then
     begin
       p := PosEx(' ', Work, p);
       If p > 0 then
       begin
          e := PosEx(Para, Work, p);
          If e > 0 then
             Work := Copy(Work, p + 1, e - p)
          else
             Work := ''
       end else Work := ''
     end;
     Work := StripCodes(Work);

     Result := Work;
end;

//------------------------------------------------------------------------------------------------
// This is the publically accessible function
//------------------------------------------------------------------------------------------------
Function StripRTF(const RTF: string): string;
var sTemp: string;
begin
     sTemp := Trim(RTF);
     if sTemp <> '' then
     begin
          sTemp := StringReplace(sTemp, Chr(13) + Chr(10), ' ', [rfReplaceAll, rfIgnoreCase]);
          sTemp := GetPara(sTemp, True)
     end;
     Result := Trim(sTemp);
end;

end.
