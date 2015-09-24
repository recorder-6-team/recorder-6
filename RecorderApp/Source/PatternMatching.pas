{===============================================================================

  Unit:         PatternMatching

  Defines:      Like

  Description:  Pattern matching on strings.

  Created:      December 2009

  Last revision information:
    $Revision: 3 $
    $Date: 7/12/09 16:44 $
    $Author: Andrewkemp $

===============================================================================}
unit PatternMatching;

interface

function Like(const Value: String; Pattern: String): Boolean;


implementation

function CharLowerA(C: Cardinal): Cardinal; stdcall; external 'user32.dll';

{-------------------------------------------------------------------------------
  Converts a single character to lower case, using the current locale.
}
function CharLower(C: Char): Char;
begin
  Result := Chr(CharLowerA(Ord(C)));
end;

{-------------------------------------------------------------------------------
  Performs a subset of the pattern matching offered by the T-SQL 'LIKE'
  operator.

  Only the '_' and '%' wildcards are supported.
}
function Like(const Value: String; Pattern: String): Boolean;

  {-----------------------------------------------------------------------------
    Removes redundant wildcards (repeated '%' characters) from the given
    pattern.
  }
  function RemoveRedundantWildcards(const APattern: String): String;
  var
    I: Integer;
    lChar: Char;
    lWasMany: Boolean;
  begin
    Result := '';
    lWasMany := False;
    for I := 1 to Length(APattern) do
    begin
      lChar := APattern[I];
      if not (lWasMany and (lChar = '%')) then
        Result := Result + lChar;
      lWasMany := lChar = '%';
    end;
  end;

  {-----------------------------------------------------------------------------
    Attempts to match the substring of Pattern that starts at PatternOffset with
    the substring of Value that starts at ValueOffset.
  }
  function Match(PatternOffset, ValueOffset: Integer): Boolean;
  var
    lPatternChar: Char;

    {---------------------------------------------------------------------------
      The minimum length for values that match the remaining pattern.
    }
    function MinimumMatchLength: Integer;
    var
      I: Integer;
    begin
      Result := 0;
      for I := PatternOffset to Length(Pattern) do
        if Pattern[I] <> '%' then Inc(Result);
    end;

    {---------------------------------------------------------------------------
      Attempts to match the remaining pattern by working backwards from the
      end of the value to ValueOffset.
    }
    function MatchFromEnd: Boolean;
    var
      lMatchOffset: Integer;
    begin
      Result := False;
      lMatchOffset := Length(Value) - MinimumMatchLength + 1;

      for lMatchOffset := lMatchOffset downto ValueOffset do
      begin
        Result := Match(PatternOffset, lMatchOffset);
        if Result then Break;
      end;
    end;

  begin
    Result := True;
    while Result and (PatternOffset <= Length(Pattern)) do
    begin
      lPatternChar := CharLower(Pattern[PatternOffset]);
      Inc(PatternOffset);

      if lPatternChar = '%' then
      begin
        Result := MatchFromEnd;
        Break;
      end
      else if ValueOffset > Length(Value) then
        Result := False
      else if not (lPatternChar in ['_', CharLower(Value[ValueOffset])]) then
        Result := False
      else
        Inc(ValueOffset);
    end;
  end;  

begin
  Pattern := RemoveRedundantWildcards(Pattern);
  Result := Match(1, 1);
end;

end.
