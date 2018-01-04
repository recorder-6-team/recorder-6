{Version 9.03}           
{*********************************************************}
{*                    STYLEPARS.PAS                      *}
{*               Copyright (c) 2001-2002 by              *}
{*                   L. David Baldwin                    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$i htmlcons.inc}

unit StylePars;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Controls,
  Dialogs, StdCtrls, StyleUn;

type
  CharFunction = function: Char;

procedure DoStyle(Styles: TStyleList; var C: char; GC: CharFunction; const APath: string);
procedure ParsePropertyStr(const PropertyStr: string; var Propty: TProperties);

implementation

uses Graphics, ReadHTML, UrlSubs, htmlun2;

const
  NeedPound = True;
  EofChar = ^Z;

type
  TProcessProc = procedure (Obj: TObject; Selectors: TStringList; Prop, Value: string);

var
  LCh, Back: char;
  Get: CharFunction;
  LinkPath: string;

function GetC: char;
begin
if Back <> #0 then
  begin
  Result := Back;
  Back := #0;
  end
else Result := Get;
if Result = ^M then
  Result := ' ';
end;

procedure GetCh;
var
  Comment: boolean;
  NextCh: char;
begin
repeat    {in case a comment immediately follows another comment}
  Comment := False;
  LCh := GetC;
  if LCh = '/' then
    begin
    NextCh := GetC;
    if NextCh = '*' then
      repeat
        Comment := True;
        LCh := GetC;
        while (LCh <> '*') and (LCh <> EofChar) and (LCh <> '<') do
          LCh := GetC;
        if LCh <> '<' then
          LCh := GetC;
      until (LCh = '/') or (LCh = EofChar) or (LCh = '<')
    else Back := NextCh; {put character back}
    end;
until not Comment;
end;

{-------------SkipWhiteSpace}
procedure SkipWhiteSpace;
begin
while (LCh in [' ']) do
  GetCh;
end;

{----------------RemoveQuotes}
function RemoveQuotes(const S: string): string;
{if string is a quoted string, remove the quotes (either ' or ")}
begin
if (Length(S) >= 2) and (S[1] in ['''', '"']) and (S[Length(S)] = S[1]) then
  Result := Copy(S, 2, Length(S)-2)
else Result := S;
end;

{----------------AddPath}
function AddPath(S: string): string;
{for <link> styles, the path is relative to that of the stylesheet directory
 and must be added now}
begin
S := ReadUrl(S);   {extract the info from url(....) }
if (Pos('://', LinkPath) > 0) then  {it's TFrameBrowser and URL}
  if not IsFullUrl(S) then
    Result := Combine(LinkPath, S)
  else ReSult := S
else
  begin
  S := HTMLToDos(S);
  if (Pos(':', S) <> 2) and (Pos('\\', Result) <> 1) then
     Result := LinkPath + S
  else Result := S;
  end;
Result := 'url(' + Result + ')';
end;

{----------------ProcessProperty}
procedure ProcessProperty(Styles: TObject; Selectors: TStringList; Prop, Value: string);
var
  I: integer;
begin
for I := 0 to Selectors.Count-1 do
  (Styles as TStyleList).AddModifyProp(Selectors[I], Prop, Value);
end;

{---------  Detect Shorthand syntax }
type
  ShortIndex = (MarginX, PaddingX, BorderWidthX, BorderX, 
                BorderTX, BorderRX, BorderBX, BorderLX,
                FontX, BackgroundX, ListStyleX);
var
  ShortHands: array[Low(ShortIndex)..High(ShortIndex)] of string =
     ('margin', 'padding', 'border-width', 'border',
      'border-top', 'border-right', 'border-bottom', 'border-left',
      'font', 'background', 'list-style');

function FindShortHand(S: string; var Index: ShortIndex): boolean;
var
  I: ShortIndex;
begin
for I := Low(ShortIndex) to High(ShortIndex) do
  if S = ShortHands[I] then
    begin
    Result := True;
    Index := I;
    Exit;
    end;
Result := False;
end;

procedure SplitString(Src: string; var Dest: array of string; var Count: integer);
{Split a Src string into pieces returned in the Dest string array.  Splitting
 is on spaces with spaces within quotes being ignored.  String containing a '/'
 are also split to allow for the "size/line-height" Font construct. }
var
  I, Q, Q1, N: integer;
  Z: string;
  Done: boolean;
  Match: char;
begin
Src := Trim(Src);
I := Pos('  ', Src);
while I > 0 do   {simplify operation by removing extra white space}
  begin
  Delete(Src, I+1, 1);
  I := Pos('  ', Src);
  end;
I := Pos(', ', Src);
while I > 0 do   {simplify operation by removing spaces after commas}
  begin
  Delete(Src, I+1, 1);
  I := Pos(', ', Src);
  end;

N := 0;
while (N <= High(Dest)) and (Src <> '') do
  begin
  Z := '';
  repeat
    Done := True;
    I := Pos(' ', Src);
    Q := Pos('"', Src);
    Q1 := Pos('''', Src);
    if (Q1 > 0) and ((Q > 0) and (Q1 < Q) or (Q = 0)) then
      begin
      Q := Q1;
      Match := '''';   {the matching quote char}
      end
    else Match := '"';
    if I = 0 then
      begin
      Z := Z + Src;
      Src := '';
      end
    else if (Q=0) or (I<Q) then
      begin
      Z := Z + Copy(Src, 1, I-1);
      Delete(Src, 1, I);
      end
    else    {Q<I}  {quoted string found}
      begin
      Z := Z + Copy(Src, 1, Q);  {copy to quote}
      Delete(Src, 1, Q);
      Q := Pos(Match, Src);   {find next quote}
      if Q > 0 then
        begin
        Z := Z+Copy(Src, 1, Q); {copy to second quote}
        Delete(Src, 1, Q);
        Done := False;  {go back and find the space}
        end
      else        {oops, missing second quote, copy remaining}
        begin
        Z := Z + Src;
        Src := '';
        end;
      end;
  until Done;
  I := Pos('/', Z);    {look for splitter for Line-height}
  if I >= 2 then
    begin         {this part is font size}
    Dest[N] := Copy(Z, 1, I-1);
    Delete(Z, 1, I-1);
    Inc(N);
    end;
  if N <= High(Dest) then
    Dest[N] := Z;
  Inc(N);
  end;
Count := N;
end;

procedure ExtractParn(var Src: string; var Dest: array of string; var Count: integer);
{Look for strings in parenthesis like "url(....)" or rgb(...)".  Return these in
 Dest Array.  Return Src without the extracted string}
var
  I, J: integer;

begin
Count := 0;
while (Count <= High(Dest)) and (Src <> '') do
  begin
  I := Pos('url(', Src);
  if I = 0 then
    I := Pos('rgb(', Src);
  if I = 0 then
    Exit;
  J := Pos(')', Src);
  if (J = 0) or (J < I) then
    Exit;
  Dest[Count] := Copy(Src, I, J-I+1);
  Delete(Src, I, J-I+1);
  Inc(Count);
  end;
end;

{$ifndef ver120_plus}
{Delphi 3 doesn't like this to be inside DoFont}
type
  FontEnum =
    (italic, oblique, normal, bolder, lighter, bold, smallcaps,
     larger, smaller, xxsmall, xsmall, small, medium, large,
     xlarge, xxlarge);
const
  FontWords: array[italic..xxlarge] of string =
    ('italic', 'oblique', 'normal', 'bolder', 'lighter', 'bold', 'small-caps',
     'larger', 'smaller', 'xx-small', 'x-small', 'small', 'medium', 'large',
     'x-large', 'xx-large');
{$endif}

procedure DoFont(Styles: TObject; Selectors: TStringList; Prop, Value: string;
             Process: TProcessProc);
{ do the Font shorthand property specifier }
{$ifdef ver120_plus}
type
  FontEnum =
    (italic, oblique, normal, bolder, lighter, bold, smallcaps,
     larger, smaller, xxsmall, xsmall, small, medium, large,
     xlarge, xxlarge);
const
  FontWords: array[italic..xxlarge] of string =
    ('italic', 'oblique', 'normal', 'bolder', 'lighter', 'bold', 'small-caps',
     'larger', 'smaller', 'xx-small', 'x-small', 'small', 'medium', 'large',
     'x-large', 'xx-large');
{$endif}
var
  S: array[0..6] of string;
  Count, I: integer;
  Index: FontEnum;

  function FindWord(const S: string; var Index: FontEnum):boolean;
  var
    I: FontEnum;
  begin
  Result := False;
  for I := Low(FontEnum) to High(FontEnum) do
    if FontWords[I] = S then
      begin
      Result := True;
      Index := I;
      Exit;
      end;
  end;

begin
SplitString(Value, S, Count);
for I := 0 to Count-1 do
  begin
  if S[I,1] = '/' then
    begin
    Process(Styles, Selectors, 'line-height', Copy(S[I], 2, Length(S[I])-1));
    Continue;
    end;
  if FindWord(S[I], Index) then
    begin
    case Index of
      italic, oblique:
        Process(Styles, Selectors, 'font-style', S[I]);
      normal..bold:
        Process(Styles, Selectors, 'font-weight', S[I]);
      smallcaps:
        Process(Styles, Selectors, 'font-variant', S[I]);
      larger..xxlarge:
        Process(Styles, Selectors, 'font-size', S[I]);
      end;
    continue;
    end;
  if S[I,1] in ['0'..'9'] then
    Process(Styles, Selectors, 'font-size', S[I])
  else
    Process(Styles, Selectors, 'font-family', S[I])
  end;
end;

procedure DoBackground(Styles: TObject; Selectors: TStringList; Prop, Value: string;
             Process: TProcessProc);
{ do the Background shorthand property specifier }
var
  S: array[0..6] of string;
  S1: string;
  Count, I, N: integer;
  Dummy: TColor;

begin
ExtractParn(Value, S, Count);
for I := 0 to Count-1 do
  begin
  if Pos('rgb(', S[I]) > 0 then
    Process(Styles, Selectors, 'background-color', S[I])
  else if (Pos('url(', S[I]) > 0) then
    begin
    if LinkPath <> '' then  {path added now only for <link...>}
      S[I] := AddPath(S[I]);
    Process(Styles, Selectors, 'background-image', S[I]);
    end;
  end;
SplitString(Value, S, Count);
for I := 0 to Count-1 do
  if ColorFromString(S[I], NeedPound, Dummy) then
    begin
    Process(Styles, Selectors, 'background-color', S[I]);
    S[I] := '';
    end
  else if S[I] = 'none' then
    begin
    Process(Styles, Selectors, 'background-image', S[I]);
    S[I] := '';
    end;
for I := 0 to Count-1 do
  if Pos('repeat', S[I]) > 0 then
    begin
    Process(Styles, Selectors, 'background-repeat', S[I]);
    S[I] := '';
    end;
for I := 0 to Count-1 do     
  if (S[I] = 'fixed') or (S[I] = 'scroll') then
    begin
    Process(Styles, Selectors, 'background-attachment', S[I]);
    S[I] := '';
    end;
N := 0;  S1 := '';  {any remaining are assumed to be position info}
for I := Count-1 downto 0 do
  if S[I] <> '' then
    begin
    S1 := S[I]+' '+S1;
    Inc(N);
    if N >= 2 then Break;  {take only last two}
    end;
if S1 <> '' then
  Process(Styles, Selectors, 'background-position', S1);
end;

procedure DoBorder(Styles: TObject; Selectors: TStringList; Prop, Value: string;
             Process: TProcessProc);
{ do the Border, Border-Top/Right/Bottom/Left shorthand properties.  However, there
  currently is only one style and color supported for all border sides }
var
  S: array[0..6] of string;
  Count, I: integer;
  Dummy: TColor;

  function FindStyle(const S: string): boolean;
  const
    Ar: array[1..9] of string = ('none', 'solid', 'dashed', 'dotted', 'double', 'groove',
        'inset', 'outset', 'ridge');
  var
    I: integer;
  begin
  for I := 1 to 9 do
    if S = Ar[I] then
      begin
      Result := True;
      Exit;
      end;
  Result := False;
  end;

begin
ExtractParn(Value, S, Count);
for I := 0 to Count-1 do
  if ColorFromString(S[I], NeedPound, Dummy) then
    Process(Styles, Selectors, 'border-color', S[I]);

SplitString(Value, S, Count);
for I := 0 to Count-1 do
  begin
  if ColorFromString(S[I], NeedPound, Dummy) then
    Process(Styles, Selectors, 'border-color', S[I])
  else if FindStyle(S[I]) then
    Process(Styles, Selectors, Prop+'-style', S[I])  {Border-Style will change all four sides}
  else if Prop = 'border' then
    begin
    Process(Styles, Selectors, 'border-top-width', S[I]);
    Process(Styles, Selectors, 'border-right-width', S[I]);
    Process(Styles, Selectors, 'border-bottom-width', S[I]);
    Process(Styles, Selectors, 'border-left-width', S[I]);
    end
  else
    Process(Styles, Selectors, Prop+'-width', S[I]);
  end;
end;

procedure DoListStyle(Styles: TObject; Selectors: TStringList; Prop, Value: string;
             Process: TProcessProc);
{ do the List-Style shorthand property specifier }
var
  S: array[0..6] of string;
  Count, I: integer;

begin
SplitString(Value, S, Count);
for I := 0 to Count-1 do
  begin
  if Pos('url(', S[I]) > 0 then
    begin
    if LinkPath <> '' then  {path added now only for <link...>}
      S[I] := AddPath(S[I]);
    Process(Styles, Selectors, 'list-style-image', S[I])
    end
  else Process(Styles, Selectors, 'list-style-type', S[I]);
  {should also do List-Style-Position }
  end;
end;

{----------------DoMarginItems}
procedure DoMarginItems(X: ShortIndex; Styles: TObject; Selectors: TStringList;
          Prop, Value: string; Process: TProcessProc);
{ Do the Margin, Border, Padding shorthand property specifiers}
var
  S: array[0..3] of string;
  I, Count : integer;
  Index: array[0..3] of PropIndices;

  procedure DoIndex(ix: PropIndices; const AValue: string);
  begin
  Process(Styles, Selectors, PropWords[ix], AValue);
  end;

begin
if Value = '' then Exit;

SplitString(Value, S, Count);  {split Value into parts}

case X of
  MarginX: Index[0] := MarginTop;
  PaddingX: Index[0] := PaddingTop;
  BorderWidthX: Index[0] := BorderTopWidth;
  end;

for I := 1 to 3 do
  Index[I] := Succ(Index[I-1]);

DoIndex(Index[0], S[0]);
case Count of
  1: for I := 1 to 3 do
     DoIndex(Index[I], S[0]);
  2: begin
     DoIndex(Index[2], S[0]);
     DoIndex(Index[1], S[1]);
     DoIndex(Index[3], S[1]);
     end;
  3: begin
     DoIndex(Index[2], S[2]);
     DoIndex(Index[1], S[1]);
     DoIndex(Index[3], S[1]);
     end;
  4: begin
     DoIndex(Index[1], S[1]);
     DoIndex(Index[2], S[2]);
     DoIndex(Index[3], S[3]);
     end;
  end;
end;

{----------------GetSelectors}
procedure GetSelectors(Styles: TStyleList; Selectors: TStringList);
{Get a series of selectors seperated by ',', like:  H1, H2, .foo }
var
  S: string;

  function FormatContextualSelector(S: string): string;
  {Takes a contextual selector and reverses the order.  Ex: 'div p em' will
   change to 'em Np div'.   N is a number added.  The first digit of N is
   the number of extra selector items.  The remainder of the number is a sequnce
   number which serves to sort entries by time parsed.}
  var
    I, Cnt: integer;
    Tmp: string;
  begin
  Result := '';
  Cnt := 0;
  I := Pos(' ', S);
  if (I > 0) and (Cnt <= 8) then
    begin
    while I > 0 do
      begin
      Inc(Cnt);
      Insert(Copy(S, 1, I-1)+' ', Result, 1);
      S := Trim(Copy(S, I+1, Length(S)));
      I := Pos(' ', S);
      end;
    if S <> '' then
      Result := S+' '+Result;
    I := Pos(' ', Result);
    Str(Cnt, Tmp);
    Insert(Tmp+Styles.GetSeqNo, Result, I+1);
    end
  else Result := S;
  end;

begin
repeat
  if LCh = ',' then
    GetCh;
  SkipWhiteSpace;
  S := '';
  while LCh in ['A'..'Z', 'a'..'z', '0'..'9', ' ', '.', ':', '#', '-', '_'] do
    begin
    S := S+LCh;
    GetCh;
    end;
  S := Trim(Lowercase(S));
  S := FormatContextualSelector(S);
  Selectors.Add(S);
until LCh <> ',';
while not (LCh in ['{', '<', EofChar]) do
  GetCh;
end;

{----------------GetCollection}
procedure GetCollection(Styles: TStyleList; Selectors: TStringList);
//Read a series of property, value pairs such as "Text-Align: Center;" between
//  '{', '}'  brackets. Add these to the Styles list for the specified selectors
var
  Prop, Value, Value1: string;
  Index: ShortIndex;
begin
if LCh <> '{' then Exit;
GetCh;
repeat
  Prop := '';
  SkipWhiteSpace;
  while LCh in ['A'..'Z', 'a'..'z', '0'..'9', '-'] do
    begin
    Prop := Prop+LCh;
    GetCh;
    end;
  Prop := Trim(LowerCase(Prop));
  SkipWhiteSpace;
  if LCh in [':', '='] then
    begin
    GetCh;
    Value := '';
    while not (LCh in [';', '}', '<', EofChar]) do
      begin
      Value := Value+LCh;
      GetCh;
      end;
    Value1 := Trim(Lowercase(Value));  {leave quotes on for font:}
    Value := RemoveQuotes(Value1);
    if FindShortHand(Prop, Index) then
      case Index of
        MarginX, BorderWidthX, PaddingX:
          DoMarginItems(Index, Styles, Selectors, Prop, Value, ProcessProperty);
        FontX:
          DoFont(Styles, Selectors, Prop, Value1, ProcessProperty);
        BackgroundX:
          DoBackground(Styles, Selectors, Prop, Value, ProcessProperty);
        ListStyleX:
          DoListStyle(Styles, Selectors, Prop, Value, ProcessProperty);
        BorderX..BorderLX:
          DoBorder(Styles, Selectors, Prop, Value, ProcessProperty);
        end
    else
      begin
      if (LinkPath <> '') and (Pos('url(', Value) > 0) then
        Value := AddPath(Value);
      ProcessProperty(Styles, Selectors, Prop, Value);
      end;
    end;
  SkipWhiteSpace;
  if LCh = ';' then
    GetCh;
  while not (LCh in ['A'..'Z', 'a'..'z', '0'..'9', '-', '}', '<', EofChar]) do    
    GetCh;
until LCh in ['}', '<', EofChar];
if LCh = '}' then
  GetCh;
end;

{----------------DoStyle}
procedure DoStyle(Styles: TStyleList; var C: char; GC: CharFunction; const APath: string);
var
  Selectors: TStringList;

  procedure ReadAt; {read thru @import or some other @}    

    procedure Brackets;  // read thru nested '{...}' pairs
    begin
    repeat
      GetCh;
      if LCh = '{' then
        Brackets;
    until LCh in ['}', '<', EOFChar];
    if LCh = '}' then
      GetCh;
    end;

  begin
  repeat
    GetCh;
  until LCh in ['{', ';', '<', EOFChar];
  if LCh = '{' then     
    Brackets
  else if LCh = ';' then
    GetCh;
  end;

begin
Get := GC;
LinkPath := APath;
{enter with the first character in C}
if C = ^M then
  C := ' ';

LCh := ' ';  {This trick is needed if the first char is part of comment, '/*'}
Back := C;

Selectors := TStringList.Create;

try
  while LCh in [' ', '<', '>', '!', '-'] do {'<' will probably be present from <style>}
    GetCh;
  repeat
    if LCh = '@' then
      ReadAt
    else
      begin
      Selectors.Clear;
      GetSelectors(Styles, Selectors);
      GetCollection(Styles, Selectors);
      end;
    while LCh in [' ', '-', '>'] do
      GetCh;
  until LCh in ['<', EOFChar];
  C := UpCase(LCh);
finally
  Selectors.Free;
  end;
end;

// The following is to process the Style=  attribute strings

{----------------MyProcess}
procedure MyProcess(Propty: TObject; Selectors: TStringList; Prop, Value: string);
begin
(Propty as TProperties).AddPropertyByName(Prop, Value);
end;

{----------------ParsePropertyStr}
procedure ParsePropertyStr(const PropertyStr: string; var Propty: TProperties);
var
  Prop, Value, Value1, S: string;
  LCh: char;
  I: integer;
  Index: ShortIndex;

  procedure GetCh;
  begin
  if I <= Length(S) then
    begin
    LCh := S[I];
    Inc(I);
    if LCh = ^M then
      LCh := ' ';
    end
  else LCh := EofChar;
  end;

  Procedure SkipWhiteSpace;
  begin
  while (LCh in [' ']) do
    GetCh;
  end;

begin
LinkPath := '';
S := Lowercase(PropertyStr);
I := 1;
GetCh;
repeat
  Prop := '';
  SkipWhiteSpace;
  while LCh in ['A'..'Z', 'a'..'z', '0'..'9', '-'] do
    begin
    Prop := Prop+LCh;
    GetCh;
    end;
  Prop := Trim(Prop);
  SkipWhiteSpace;
  if LCh in [':', '='] then
    begin
    GetCh;
    Value := '';
    while not (LCh in [';', EofChar]) do
      begin
      Value := Value+LCh;
      GetCh;
      end;
    Value1 := Trim(Value);   {leave quotes on for font}
    Value := RemoveQuotes(Value1);

    if FindShortHand(Prop, Index) then
      case Index of
        MarginX, BorderWidthX, PaddingX:
          DoMarginItems(Index, Propty, Nil, Prop, Value, MyProcess);
        FontX:
          DoFont(Propty, Nil, Prop, Value1, MyProcess);
        BackgroundX:
          DoBackground(Propty, Nil, Prop, Value, MyProcess);
        BorderX..BorderLX:
          DoBorder(Propty, Nil, Prop, Value, MyProcess);
        ListStyleX:
          DoListStyle(Propty, Nil, Prop, Value, MyProcess);
        end
    else
      Propty.AddPropertyByName(Prop, Value);
    end;
  SkipWhiteSpace;
  if LCh = ';' then
    GetCh;
  while not (LCh in ['A'..'Z', 'a'..'z', '0'..'9', '-', EofChar]) do
    GetCh;
until LCh in [EofChar];
end;

end.
