//==============================================================================
//  Unit:        SQLConverter
//
//  Implements:  TSQLConverter
//
//  Description: Implements a SQL converter to adapt queries using Access SQL
//               syntax for SQL Server,
//               Oracle and possibly any other SQL dialect.
//
//  Author:      Eric Salmon
//  Created:
//
//  Changes:     Exception handling in ConvertSQL function. Returns ERROR and
//               error message on first and second lines of the result string.
//
//  Last Revision Details:
//    $Revision: 14 $
//    $Date: 18/01/08 8:34 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================

unit SQLConverter;

interface

uses
  SysUtils, Classes, GeneralFunctions;

type
  TConversionType = (ctSQLServer, ctOracle);

function ConvertSQL(const InText: String; const ConversionType: TConversionType): String;

//==============================================================================
implementation

resourcestring
  ResStr_SQLConvertError =  'ERROR'#13#10 +'%s' + #13#10 + '%s';

type
  TSQLConverter = class
  private
    FOriginalSQL : String;
    FParsedText  : String;
    FSelectFields: TStringList;
    FFromTables  : TStringList;
    FWhereFilters: TStringList;
    FSelectStart : String;
    FKeywords    : TStringList;
    function ConvertSQL(const ConversionType: TConversionType): String;
    procedure DissectSelectFields(var NextClausePos: Integer);
    procedure DissectFromTables(var NextClausePos: Integer);
    procedure DissectWhereFilters(var NextClausePos: Integer);
    procedure ConvertSelectFields;
    procedure ConvertFromTables;
    procedure ConvertWhereFilters;
    function ReAssembleSelectFields: String;
    function ReAssembleFromTables: String;
    function ReAssembleWhereFilters: String;

    function SkipTo(const APosition: Integer; const ADelimiter: Char; const ANextClause: String;
      var AClauseFound: Boolean): Integer;
    function SkipSqlString(APosition: integer): integer;
    procedure SkipBlanks(var APosition: Integer; const AString: String = '');
    procedure SkipToBlank(var APosition: Integer; const AString: String = '');
    procedure SkipAlphaNum(var APosition: Integer; const AString: String = '');
    procedure SkipToAlphaNum(var APosition: Integer; const AString: String = '');
    function GetWord(AStartPos: Integer; const AString: String; const AWantUpperCase: Boolean = false): String;

    function ConvertSelectQuery: String;
    function ConvertInsertUpdateQuery: String;

    function ConvertTrueFalse(const InText: String): String;
    function ProtectNames(const InText: String): String;
    function ConvertCDATE(const InText: String): String;
    function ConvertIIF(const InText: String): String;
    function ConvertISNULL(const InText: String): String;
    function ConvertNOW(const InText: String): String;
  public
    constructor Create(const InText: String);
    destructor Destroy; override;
    property OriginalSQL: String read FOriginalSQL;
  end;

//==============================================================================
//==============================================================================
function ConvertSQL(const InText: String; const ConversionType: TConversionType): String;
begin
  // Just in case something goes wrong in here, return the text untouched
  Result := InText;
  with TSQLConverter.Create(Trim(InText)) do
    try
      try
        Result := ConvertSQL(ConversionType);
      except
        on E:Exception do
          Result := Format(ResStr_SQLConvertError, [E.Message, InText]);
      end;
    finally
      Free;
    end;
end;  // ConvertSQL

//==============================================================================
{ TSQLConverter }

constructor TSQLConverter.Create(const InText: String);
begin
  FOriginalSQL  := InText;
  FSelectFields := TStringList.Create;
  FFromTables   := TStringList.Create;
  FWhereFilters := TStringList.Create;
  FKeywords     := TStringList.Create;
  FKeywords.CommaText := 'COUNT,AS,IS,NOT,NULL,AND,OR,IN,CASE,WHEN,THEN,ELSE,END,' +
                         'BETWEEN,LIKE,INNER,LEFT,RIGHT,JOIN,ON,SELECT,FROM,WHERE,YEAR,' +
                         'CONVERT,DATETIME,INSERT,INTO,UPDATE,DELETE,VALUES,SET,GETDATE,' +
                         'ISNULL,LEN,CASE,WHEN,THEN,ELSE,END';
end;  // Create

//==============================================================================
destructor TSQLConverter.Destroy;
begin
  FSelectFields.Free;
  FFromTables.Free;
  FWhereFilters.Free;
  FKeywords.Free;
  inherited;
end;  // Destroy

//==============================================================================
// Find out the type of query being processed.
function TSQLConverter.ConvertSQL(const ConversionType: TConversionType): String;
var liQueryTypePos: Integer;
    lstWord       : String;
begin
  liQueryTypePos := 1;
  lstWord := GetWord(liQueryTypePos, OriginalSQL, true);

  if lstWord = 'SELECT' then       // SELECT query
    Result := ConvertSelectQuery
  else if (lstWord = 'INSERT') or (lstWord = 'UPDATE') then  // INSERT/UPDATE query
    Result := ConvertInsertUpdateQuery
  else if lstWord = 'DELETE' then  // DELETE query
    Result := ProtectNames(ConvertTrueFalse(OriginalSQL))
  else
    Result := OriginalSQL;
end;  // ConvertSQL

//==============================================================================
// Adapt the Access syntax of a SELECT query to SQL Server or Oracle.  The loop is to take care
// of possible UNIONs, so that all SELECTs are dealt with.
function TSQLConverter.ConvertSelectQuery: String;
var liNextClausePos, liEndPos  : Integer;
    ltfFinished, ltfClauseFound: Boolean;
    lstWord                    : String;
begin
  Result := '';
  liNextClausePos := 1;

  ltfFinished := false;
  while not ltfFinished do begin
    liNextClausePos := SkipTo(liNextClausePos, #0, '"SELECT"', ltfClauseFound);
    DissectSelectFields(liNextClausePos);
    ConvertSelectFields;
    Result := Result + ReAssembleSelectFields;

    // Process FROM, can't be anything else
    DissectFromTables(liNextClausePos);
    ConvertFromTables;
    Result := Result + ' ' + ReAssembleFromTables;

    // There might not be a WHERE
    if GetWord(liNextClausePos, OriginalSQL, true) = 'WHERE' then begin
      DissectWhereFilters(liNextClausePos);
      ConvertWhereFilters;
      Result := Result + ' ' + ReAssembleWhereFilters;
    end;

    FParsedText := OriginalSQL;
    // Skip to the UNION keyword, if there is one, otherwise, skip to end of SQL
    lstWord := GetWord(liNextClausePos, OriginalSQL, true);
    if lstWord <> 'UNION' then begin
      liEndPos := SkipTo(liNextClausePos, #0, '"UNION"', ltfClauseFound);
      lstWord := Copy(OriginalSQL, liNextClausePos, liEndPos - liNextClausePos);
      lstWord := ConvertIIF(lstWord);
      Result := Result + lstWord;
    end;
    
    // If UNION keyword found, go round again to process next SELECT in line.
    if GetWord(liNextClausePos, OriginalSQL, true) = 'UNION' then begin
      Result := Result + ' UNION ';
      liNextClausePos := SkipTo(liNextClausePos, #0, '"SELECT"', ltfClauseFound);
      ltfFinished := false;
    end else
      ltfFinished := true;
  end;
end;  // ConvertSelectQuery

//==============================================================================
// Change Access's TRUE/YES and FALSE/NO to SQL Server/Oracle 1/0 in the given text.  The keywords
// are converted only if not inside a string of any kind.
function TSQLConverter.ConvertTrueFalse(const InText: String): String;
var liStartPos: Integer;
    lstWord   : String;
    ltfNothing: Boolean;
begin
  FParsedText := InText;
  // Look for TRUE/FALSE and convert to 1/0 accordingly, but only if at least one True/False exists
  if (Pos('TRUE', UpperCase(FParsedText)) > 0) or (Pos('FALSE', UpperCase(FParsedText)) > 0) or
     (Pos('YES', UpperCase(FParsedText)) > 0) or (Pos('NO', UpperCase(FParsedText)) > 0) then begin
    // Go through the string, in case some TRUE/FALSE are part of actual string values!
    // And we don't want to change those.
    liStartPos := 1;
    while liStartPos < Length(FParsedText) do begin
      // Get next word and check for TRUE/FALSE
      lstWord := GetWord(liStartPos, FParsedText, true);
      if (lstWord = 'TRUE') or (lstWord = 'YES') then
        FParsedText := Copy(FParsedText, 1, liStartPos - 1) + '1' +
                       Copy(FParsedText, liStartPos + Length(lstWord), Length(FParsedText))
      else
      if (lstWord = 'FALSE') or (lstWord = 'NO') then
        FParsedText := Copy(FParsedText, 1, liStartPos - 1) + '0' +
                       Copy(FParsedText, liStartPos + Length(lstWord), Length(FParsedText))
      else
      if (FParsedText[liStartPos] in [#34, #39]) then
        // Don't forget to skip the end [']/["] delimiter liStartPos points to when returning from SkipTo
        // or the rest of the string could be skipped...
        liStartPos := SkipTo(liStartPos + 1, FParsedText[liStartPos], '', ltfNothing) + 1
      else begin
        Inc(liStartPos);
        SkipBlanks(liStartPos, FParsedText);
      end;
    end;
  end;
  Result := FParsedText;
end;  // ConvertTrueFalse

//==============================================================================
function TSQLConverter.ConvertNOW(const InText: String): String;
var liStartPos: Integer;
    lstWord   : String;
    ltfNothing: Boolean;
begin
  FParsedText := InText;
  // Look for NOW function and replace it with GETDATE()
  if (Pos('NOW', UpperCase(FParsedText)) > 0) then begin
    // Go through the string, in case some NOWs are part of actual string values!
    // And we don't want to change those.
    liStartPos := 1;
    while liStartPos < Length(FParsedText) do begin
      // Get next word and check for NOW
      lstWord := GetWord(liStartPos, FParsedText, true);
      if (lstWord = 'NOW') then
        FParsedText := Copy(FParsedText, 1, liStartPos - 1) + 'GETDATE()' +
                       Copy(FParsedText, liStartPos + Length(lstWord), Length(FParsedText))
      else
      if (FParsedText[liStartPos] in [#34, #39]) then
        // Don't forget to skip the end [']/["] delimiter liStartPos points to when returning from SkipTo
        // or the rest of the string could be skipped...
        liStartPos := SkipTo(liStartPos + 1, FParsedText[liStartPos], '', ltfNothing) + 1
      else begin
        Inc(liStartPos);
        SkipBlanks(liStartPos, FParsedText);
      end;
    end;
  end;
  Result := FParsedText;
end;  // ConvertNOW

//==============================================================================
// Skip characters considered as blanks (Space, Tab, CR, LF) up to a non-blank character.
procedure TSQLConverter.SkipBlanks(var APosition: Integer; const AString: String = '');
var lstLocal: String;
begin
  lstLocal := AString;
  if AString = '' then lstLocal := OriginalSQL;

  while (APosition <= Length(lstLocal)) and
        (lstLocal[APosition] in [' ', #9, #10, #13]) do
    Inc(APosition)
end;  // SkipBlanks

//--------------------------------------------------------------------------------------------------
// Skip everything up to a blank character (Space, Tab, CR, LF).
procedure TSQLConverter.SkipToBlank(var APosition: Integer; const AString: String = '');
var lstLocal: String;
begin
  lstLocal := AString;
  if AString = '' then lstLocal := OriginalSQL;

  while (APosition <= Length(lstLocal)) and
        not (lstLocal[APosition] in [' ', #9, #10, #13]) do
    Inc(APosition)
end;  // SkipToBlank

//--------------------------------------------------------------------------------------------------
// Skip alphanum characters up to a non-alphanum character.
procedure TSQLConverter.SkipAlphaNum(var APosition: Integer; const AString: String = '');
var lstLocal: String;
begin
  lstLocal := AString;
  if AString = '' then lstLocal := OriginalSQL;
  if APosition=0 then
    Inc(APosition); // there is no character 0 in a string
  {$BOOLEVAL OFF}
  while (APosition <= Length(lstLocal)) and
      (lstLocal[APosition] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.']) do
    Inc(APosition);
end;  // SkipAlphaNum

//--------------------------------------------------------------------------------------------------
// Skip everything up to an alphanum character.
procedure TSQLConverter.SkipToAlphaNum(var APosition: Integer; const AString: String = '');
var lstLocal: String;
begin
  lstLocal := AString;
  if AString = '' then lstLocal := OriginalSQL;

  while (APosition <= Length(lstLocal)) and
        not (lstLocal[APosition] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.']) do
    Inc(APosition)
end;  // SkipToAlphaNum

//--------------------------------------------------------------------------------------------------
// Return a complete word found from the given position in the given string.  Optional parameter
// to ask for the word to be returned uppercased.
function TSQLConverter.GetWord(AStartPos: Integer; const AString: String;
  const AWantUpperCase: Boolean = false): String;
var liEndPos: Integer;
begin
  SkipBlanks(AStartPos, AString);
  liEndPos := AStartPos;
  SkipAlphaNum(liEndPos, AString);
  Result := Trim(Copy(AString, AStartPos, liEndPos - AStartPos));
  if AWantUpperCase then
    Result := UpperCase(Result);
end;  // GetWord

//==============================================================================
// Recursive function to skip blocks of characters, or until the given delimiter is found.
// Skip nested groups, i.e. surrounded by (...), [...], '...', "..." by calling itself with the
// appropriate delimiter.
// This function relies on FParsedText to contain the correct value to be processed.
function TSQLConverter.SkipTo(const APosition: Integer; const ADelimiter: Char;
  const ANextClause: String; var AClauseFound: Boolean): Integer;
var lstWord: String;
begin
  Result := APosition;
  while (Result <= Length(FParsedText)) and (FParsedText[Result] <> ADelimiter) do begin
    // Skip SQL strings. Don't use normal SkipTo function as these don't contain normal SQL.
    if (FParsedText[Result] = '''') then
      Result := SkipSqlString(Result);
    // Skip whatever is inside a (...) block, so we don't get the wrong matching delimiter, if looking for ')'
    if FParsedText[Result] = '(' then
      Result := SkipTo(Result + 1, ')', ANextClause, AClauseFound);
    // Skip whatever is inside a [...] block, so we don't get the wrong matching delimiter, if looking for ']'
    if FParsedText[Result] = '[' then
      Result := SkipTo(Result + 1, ']', ANextClause, AClauseFound);
    // Skip whatever is inside a "..."(#34) block.
    if FParsedText[Result] = #34 then
      Result := SkipTo(Result + 1, #34, ANextClause, AClauseFound);
    // Make sure it's not the end of the SELECT, i.e. found FROM outside ' or " delimiter
    if not (ADelimiter in [#34, #39]) then begin
      lstWord := GetWord(Result, FParsedText, true);
      // If word might be part of a composite keyword, get the next word and check both
      if (lstWord = 'GROUP') or (lstWord = 'ORDER') then
        lstWord := lstWord + ' ' + GetWord(Result + 5, FParsedText, true);
      // Check the Word found is not a keyword for next SQL clause
      if Pos('"' + lstWord + '"', ANextClause) > 0  then begin
        AClauseFound := true;
        Break;
      end;
    end;
    // Move on to next character
    Inc(Result);
    SkipAlphaNum(Result, FParsedText);
    SkipBlanks(Result, FParsedText);
  end;
end;  // SkipTo

//=============================================================================
// Winds on from the start of a delimited string in SQL to the end.
//
function TSQLConverter.SkipSqlString(APosition: integer): Integer;
begin
  Result := APosition;
  if (FParsedText[Result] = '''') and (Result < Length(FParsedText)-1) then begin
    repeat
      Inc(Result);
    until (Result >= Length(FParsedText))
      or ((FParsedText[Result] = '''') and (Copy(FParsedText, Result, 2) <> ''''''));
  end;
end;

//==============================================================================
// Break down the SELECT part of the query, until the start of the FROM clause is found.
procedure TSQLConverter.DissectSelectFields(var NextClausePos: Integer);
var liStartPos, liEndPos: Integer;
    ltfFromFound        : Boolean;
    lstWord             : String;
begin
  FSelectStart := 'SELECT';
  // Initialise at end of "SELECT "
  liStartPos := NextClausePos;
  SkipToBlank(liStartPos);
  SkipBlanks(liStartPos);
  lstWord := GetWord(liStartPos, OriginalSQL, true);
  // Skip [ ALL | DISTINCT ] if found
  if (lstWord = 'ALL') or (lstWord = 'DISTINCT') then begin
    FSelectStart := FSelectStart + ' ' + lstWord;
    SkipToBlank(liStartPos);
    SkipBlanks(liStartPos);
  end;
  // Skip a [ TOP n ] clause if found
  lstWord := GetWord(liStartPos, OriginalSQL, true);
  if lstWord = 'TOP' then begin
    FSelectStart := FSelectStart + ' ' + lstWord;
    SkipToBlank(liStartPos);
    SkipBlanks(liStartPos);
    // Skip the number that comes after TOP
    lstWord := GetWord(liStartPos, OriginalSQL);
    FSelectStart := FSelectStart + ' ' + lstWord;
    SkipToBlank(liStartPos);
    SkipBlanks(liStartPos);
  end;

  // Now dissect each field in the clause
  ltfFromFound := false;
  FParsedText := OriginalSQL;
  FSelectFields.Clear;
  repeat
    liEndPos := SkipTo(liStartPos, ',', '"FROM"', ltfFromFound);
    FSelectFields.Add(Trim(Copy(OriginalSQL, liStartPos, liEndPos - liStartPos)));
    liStartPos := liEndPos + 1;
  until ltfFromFound;
  NextClausePos := liStartPos - 1;
end;  // DissectSelectFields

//------------------------------------------------------------------------------------------------
// Convert Access's ISNULL(---) to (--- IS NULL)
function TSQLConverter.ConvertISNULL(const InText: String): String;
var liStartPos, liEndPos: Integer;
    ltfNothing: Boolean;
begin
  Result := InText;
  if Pos('ISNULL', UpperCase(InText)) > 0 then begin
    FParsedText := UpperCase(InText);
    liStartPos := Pos('ISNULL', FParsedText);
    // Get whatever comes before the ISNULL function
    Result := Copy(InText, 1, liStartPos - 1);
    // Skip the ISNULL itself
    SkipAlphaNum(liStartPos, FParsedText);
    SkipBlanks(liStartPos, FParsedText);
    // Skip the '(' of the ISNULL function to get to the corresponding ')'
    liEndPos := SkipTo(liStartPos + 1, ')', '', ltfNothing);
    // Now get the rest of the string
    Result := Result + Copy(InText, liStartPos, liEndPos - liStartPos) + ' IS NULL' + Copy(InText, liEndPos, Length(InText));
  end;
end;  // ConvertISNULL

//------------------------------------------------------------------------------------------------
// Convert Access " IIF(-1-,-2-,-3-) " to
// SQL Server     " CASE WHEN -1- THEN -2- ELSE -3- END "
function TSQLConverter.ConvertIIF(const InText: String): String;
var lstCondition, lstThen, lstElse, lstRest   : String;
    liStartPos, liEndPos, liThenPos, liElsePos: Integer;
    ltfNothing                                : Boolean;
begin
  Result := InText;
  if Pos('IIF(', UpperCase(InText)) > 0 then begin
    FParsedText := UpperCase(InText);
    liStartPos := Pos('IIF(', FParsedText);
    Result := Copy(InText, 1, liStartPos - 1);
    // Skip to IIF's '('
    SkipAlphaNum(liStartPos, FParsedText);
    // Skip IIF's '('
    Inc(liStartPos);
    // Find where the condition stops and the THEN part starts
    liThenPos := SkipTo(liStartPos, ',', '', ltfNothing) + 1;
    lstCondition := Trim(Copy(InText, liStartPos, liThenPos - liStartPos - 1));
    // Find where the THEN part stops and the ELSE part starts
    liElsePos := SkipTo(liThenPos, ',', '', ltfNothing) + 1;
    lstThen := Trim(Copy(InText, liThenPos, liElsePos - liThenPos - 1));
    // Find the end of the IIF
    liEndPos := SkipTo(liElsePos, ')', '', ltfNothing) + 1;
    lstElse := Trim(Copy(InText, liElsePos, liEndPos - liElsePos - 1));
    // If there is anything behind, like AS ColumnName, get it too
    lstRest := Trim(Copy(InText, liEndPos, Length(InText)));

    // Convert extra nested bits
    lstCondition := ConvertISNULL(lstCondition);
    lstThen := ConvertIIF(lstThen);
    lstElse := ConvertIIF(lstElse);
    lstRest := ConvertIIF(lstRest);

    // Put it back together with new syntax
    Result := Result + ' CASE WHEN ' + lstCondition + ' THEN ' + lstThen + ' ELSE ' + lstElse + ' END ' + lstRest;
  end;
end;  // ConvertIIF

//------------------------------------------------------------------------------------------------
function TSQLConverter.ConvertCDATE(const InText: String): String;
var liPos: Integer;
begin
  // Convert the CDATE function to a valid SQL Server CONVERT(DateTime, '')
  Result := InText;
  liPos := Pos('CDATE(', UpperCase(Result));
  while liPos > 0 do begin
    Result := Copy(Result, 1, liPos - 1) +
              'CONVERT(DateTime,' +
              Copy(Result, liPos + 6, Length(Result));
    liPos := Pos('CDATE(', UpperCase(Result));
  end;
end;  // ConvertCDATE

//------------------------------------------------------------------------------------------------
// Process each fields in the SELECT clause and convert any Access functions such as ISNULL() and
// IIF() to corresponding functions and syntax in SQL Server/Oracle
procedure TSQLConverter.ConvertSelectFields;
var liIdx, liPos: Integer;
    lstField    : String;
begin
  for liIdx := 0 to FSelectFields.Count - 1 do begin
    lstField := FSelectFields[liIdx];
    // If column name is enclosed in [...], change them to "...",
    // or neither SQL Server nor Oracle will agree
    if (Pos('[', lstField) > 0) and (lstField[Length(lstField)] = ']') then begin
      liPos := Length(lstField);
      while lstField[liPos] <> '[' do Dec(liPos);
      FSelectFields[liIdx] := Copy(lstField, 1, liPos - 1) + #34 +
                              Copy(lstField, liPos + 1, Length(lstField) - liPos - 1) + #34;
    end;

    // Convert the IIF function, if found
    // Will also convert ISNULL([condition]) to ([condition] IS NULL)
    FSelectFields[liIdx] := ConvertIIF(FSelectFields[liIdx]);

    // Surround table and field names in "..." so that using reserved keywords like USER
    // or TABLE won't break anything
    FSelectFields[liIdx] := ProtectNames(FSelectFields[liIdx]);
  end;
end;  // ConvertSelectFields

//--------------------------------------------------------------------------------------------------
// Put the list of fields back together
function TSQLConverter.ReAssembleSelectFields: String;
var liIdx: Integer;
begin
  Result := FSelectStart + ' ' + FSelectFields[0];
  for liIdx := 1 to FSelectFields.Count - 1 do
    Result := Result + ', ' + FSelectFields[liIdx];
end;  // ReAssembleSelectFields

//==============================================================================
// Get the tables from the FROM part of the query
procedure TSQLConverter.DissectFromTables(var NextClausePos: Integer);
var liStartPos, liEndPos: Integer;
    ltfClauseFound      : Boolean;
begin
  liStartPos := NextClausePos;  // Skip 'FROM ' keyword
  SkipToBlank(liStartPos);
  SkipBlanks(liStartPos);

  FParsedText := UpperCase(OriginalSQL);
  liEndPos   := SkipTo(liStartPos, #0, '"WHERE","GROUP BY","HAVING","ORDER BY","UNION"', ltfClauseFound);
  NextClausePos := liEndPos;

  FFromTables.Clear;
  FFromTables.Text := Trim(Copy(OriginalSQL, liStartPos, liEndPos - liStartPos));
end;  // DissectFromTables

//--------------------------------------------------------------------------------------------------
// Convert to SQL Server/Oracle syntax
procedure TSQLConverter.ConvertFromTables;
begin
  // Keep as is for SQL Server
  FFromTables.Text := ConvertTrueFalse(FFromTables.Text);
end;  // ConvertFromTables

//--------------------------------------------------------------------------------------------------
// Put the list of tables back together
function TSQLConverter.ReAssembleFromTables: String;
begin
  Result := 'FROM ' + ProtectNames(FFromTables.Text);
end;  // ReAssembleFromTables

//==============================================================================
// Get the bits from the WHERE clause
procedure TSQLConverter.DissectWhereFilters(var NextClausePos: Integer);
var liStartPos, liEndPos: Integer;
    ltfClauseFound      : Boolean;
begin
  liStartPos := NextClausePos;  // Skip 'WHERE' keyword
  SkipToBlank(liStartPos);
  SkipBlanks(liStartPos);

  FWhereFilters.Clear;
  FParsedText := UpperCase(OriginalSQL);
  // Skip the whole WHERE clause. Use #0 as delimiter, unlikely to find that in the string before the end
  liEndPos := SkipTo(liStartPos, #0, '"GROUP BY","HAVNG","ORDER BY","UNION"', ltfClauseFound);
  FWhereFilters.Text := Copy(OriginalSQL, liStartPos, liEndPos - liStartPos);
  NextClausePos := liEndPos;
end;  // DissectWhereFilters

//--------------------------------------------------------------------------------------------------
// Convert to SQL Server/Oracle syntax
procedure TSQLConverter.ConvertWhereFilters;
begin
  // Change all TRUE/FALSE to correponding 1/0, and any IIF/CDATE
  FWhereFilters.Text := ConvertCDATE(ConvertIIF(ConvertTrueFalse(FWhereFilters.Text)));
end;  // ConvertWhereFilters

//--------------------------------------------------------------------------------------------------
// Put the clause back together
function TSQLConverter.ReAssembleWhereFilters: String;
begin
  Result := 'WHERE ' + ProtectNames(FWhereFilters.Text);
end;  // ReAssembleWhereFilters

//==============================================================================
// Surround every table name and field name with "...", so that reserved keywords, such as USER
// and TABLE, can be used as valid name for tables or fields
function TSQLConverter.ProtectNames(const InText: String): String;
var liStartPos, liEndPos, liResync: Integer;
    lstWord                       : String;
    ltfFinished, ltfNothing       : Boolean;
begin
  // If nothing to do, exit now.
  if Trim(InText) = '' then Exit;

  liStartPos := 1;
  ltfFinished := false;
  FParsedText := InText;
  while not ltfFinished do begin
    // Make sure we're always on a character
    SkipBlanks(liStartPos, FParsedText);
    while (liStartPos <= Length(FParsedText)) and
          (FParsedText[liStartPos] in ['=', '<', '>', '(', ',', ')', ':']) do
    begin
      // This means a possible parameter, so don't touch it at all
      if FParsedText[liStartPos] = ':' then
        SkipToBlank(liStartPos, FParsedText)
      else
        Inc(liStartPos);  // Otherwise just skip one
      // Skip blanks to next 'valid' character
      SkipBlanks(liStartPos, FParsedText);
    end;
    liEndPos := liStartPos;
    if liStartPos <= Length(FParsedText) then
      // Skip bits surrounded by '/"
      if FParsedText[liStartPos] = #34 then begin
        liEndPos := SkipTo(liStartPos + 1, FParsedText[liStartPos], '', ltfNothing);
        Inc(liEndPos);  // Skip found '/" for next round in loop
      end
      else if FParsedText[liStartPos] = #39 then begin
        liEndPos := SkipSqlString(liStartPos);
        Inc(liEndPos);
      end
      else begin
        SkipToAlphaNum(liStartPos, FParsedText);
        liEndPos := liStartPos;
        // Make sure we're not going over
        if liEndPos <= Length(FParsedText) then begin
          lstWord := GetWord(liStartPos, FParsedText, true);
          if FKeywords.IndexOf(lstWord) <> - 1 then begin
            SkipToBlank(liStartPos, FParsedText);  // Skip keyword found
            liEndPos := liStartPos;
          end else if ((lstWord = 'GROUP') or (lstWord = 'ORDER')) and
                      (GetWord(liStartPos + 5, FParsedText, true) = 'BY') then
          begin
            SkipToBlank(liStartPos, FParsedText);  // Skip 'GROUP' or 'ORDER'
            SkipBlanks(liStartPos, FParsedText);
            SkipToBlank(liStartPos, FParsedText);  // Skip 'BY'
            liEndPos := liStartPos
          end else begin
            SkipAlphaNum(liEndPos, FParsedText);
            lstWord := Copy(FParsedText, liStartPos, liEndPos - liStartPos);
            if not (IsInt(lstWord) or IsFloat(lstWord) or IsDate(lstWord)) then begin
              liResync := 2;
              // Work out what to do
              if Pos('.', lstWord) = 0 then
                // Just a standard '---'
                lstWord := '"' + lstWord + '"'
              else begin
                // Got '---.---'
                lstWord := '"' + Copy(lstWord, 1, Pos('.', lstWord) - 1) + '"' +
                           Copy(lstWord, Pos('.', lstWord), Length(lstWord));
                // Work out if it's '---.*', in which case, don't put " around *
                if (liEndPos <= Length(FParsedText)) and (FParsedText[liEndPos] <> '*') then
                begin
                  lstWord := Copy(lstWord, 1, Pos('.', lstWord)) +
                             '"' + Copy(lstWord, Pos('.', lstWord) + 1, Length(lstWord)) + '"';
                  Inc(liResync, 2);
                end;
              end;
              // Replace string
              FParsedText := Copy(FParsedText, 1, liStartPos - 1) + lstWord +
                             Copy(FParsedText, liEndPos, Length(FParsedText));
              // Re-sync
              Inc(liEndPos, liResync);
            end;
          end;
        end;  // liEndPos <= ...
      end;
    liStartPos := liEndPos;
    ltfFinished := (liStartPos >= Length(FParsedText));
  end;
  Result := FParsedText;
end;  // ProtectNames

//==============================================================================
function TSQLConverter.ConvertInsertUpdateQuery: String;
var SelectPos: Integer;
begin
  Result := OriginalSQL;
  Result := ConvertNOW(Result);
  // Checking if we have something like INSERT INTO ... SELECT * FROM ...
  SelectPos := Pos('SELECT ', UpperCase(OriginalSQL));
  if SelectPos > 0 then begin
    // Set the environment for the "Select" conversion stuff
    FOriginalSQL := Copy(Result, SelectPos, Length(Result));
    // Use ConvertSelectQuery function to do the rest
    Result := ProtectNames(ConvertCDATE(ConvertTrueFalse(Copy(Result, 1, SelectPos - 1))));
    // Need to reset FParsedText
    FParsedText := '';
    Result := Result + ConvertSelectQuery;
  end else
    Result := ProtectNames(ConvertCDATE(ConvertTrueFalse(Result)));
end;  // ConvertInsertQuery

//==============================================================================
end.
