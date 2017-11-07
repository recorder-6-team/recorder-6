{===============================================================================
  Unit:         Sql.pas

  Implements:   -

  Description:  Generation and manipulation of SQL statements.

  Model:        -

  Created:      June 2004

  Last Revision Details:
    $Revision: 4 $
    $Date: 9/06/09 11:23 $
    $Author: Ericsalmon $

  Copyright © Dorset Software Services Ltd, 2004

===============================================================================}
unit Sql;

interface

function TransactSqlLiteral(const Value: OleVariant): String;
function AccessSqlLiteral(const Value: OleVariant): String;

implementation

uses
  SysUtils, Variants;


{+------------------------------------------------------------------------------
  Converts some characters that are beyond the handled range with their
  "standard" equivalents. Handles any other characters as required.
  This mainly is because of unicode and Delphi's lack of support for it.
}
function HandleSpecialCharacters(const value: String): String;
begin
  Result := value;

  // Handles the doubling of single quote/apostrophe.
  Result := StringReplace(Result, '''', '''''', [rfReplaceAll]);

  // Replace the long dash with standard hyphen.
  Result := StringReplace(Result, #150, '-', [rfReplaceAll]);

  // Replace variants of the " with the standard ".
  Result := StringReplace(Result, #147, '"', [rfReplaceAll]);  // Left double quotation mark.
  Result := StringReplace(Result, #148, '"', [rfReplaceAll]);  // Right double quotation mark.
  Result := StringReplace(Result, #132, '"', [rfReplaceAll]);  // Double low-9 quotation.
end;

{+------------------------------------------------------------------------------
  Unambiguous T-SQL literal form of a value, quoted if appropriate for the data
  type.
}
function TransactSqlLiteral(const Value: OleVariant): string;
var
  lDecSeparator: char;
begin
  // ensure floats convert with decimal point, as they are being sent to SQL Server
  lDecSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    case VarType(Value) of
      varNull:
        Result := 'NULL';
      varOleStr, varStrArg, varString:
        Result := ''''
            + HandleSpecialCharacters(VarToStr(Value))
            + '''';
      varDate:
        Result := '''' + FormatDateTime('yyyymmdd', VarToDateTime(Value)) + '''';
      varBoolean:
        if Value then Result := '1' else Result := '0';
    else
      Result := VarToStr(Value);
    end;
  finally
    DecimalSeparator := lDecSeparator;
  end; // try
end;

{+------------------------------------------------------------------------------
  Unambiguous Access SQL literal form of a value, quoted if appropriate for the 
  data type.
}
function AccessSqlLiteral(const Value: OleVariant): string;
var
  lDecSeparator: char;
begin
  // ensure floats convert with decimal point, as they are being sent to SQL Server
  lDecSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    case VarType(Value) of
      varNull:
        Result := 'NULL';
      varOleStr, varStrArg, varString:
        Result := ''''
            + HandleSpecialCharacters(VarToStr(Value))
            + '''';
      varDate:
        Result := '#' + FormatDateTime('mm/dd/yyyy', VarToDateTime(Value)) + '#';
      varBoolean:
        if Value then Result := 'TRUE' else Result := 'FALSE';
    else
      Result := VarToStr(Value);
    end;
  finally
    DecimalSeparator := lDecSeparator;
  end; // try
end;

end.
