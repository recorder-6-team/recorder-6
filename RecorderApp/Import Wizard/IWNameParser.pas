unit IWNameParser;

interface
uses
SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
StrUtils, Contnrs,DateUtils,FastStrings, FastStringFuncs,
IWConstants;

resourcestring

ResStr_Titles = 'mr, mrs, ms, miss, dr, sir, lord, rev, col';
ResStr_And = 'and';

procedure ParseNameField(const Value: string;Parsednames:Tstringlist);

implementation

procedure ParseNameField(const Value: string;Parsednames:Tstringlist);
const
  SeparatorCharacters = [',', ';', '/', '\', '&', '+'];

var
  lPos: Integer;
  lCurrentToken: String;
  lSeparatorPosition: Integer;
  lSeparator, lTerm1, lTerm2: String;
  lWorking : String;
  {-----------------------------------------------------------------------------
    Finds the next set of alpha characters in the given string from the given
    position.
  }
  function FindNextAlphaString(ATerm: String; APos: integer): string;
  var
    lAlphaPos: integer;
  begin
    Result := '';
    lAlphaPos := APos;
    while ATerm[lAlphaPos] in ['a'..'z', 'A'..'Z'] do begin
      if not (ATerm[lAlphaPos] in [' ', '.']) then
        Result := Result + ATerm[lAlphaPos];
      Inc(lAlphaPos);
      if lAlphaPos > Length(ATerm) then
        Break; // from while
    end;
  end;

  {-----------------------------------------------------------------------------
    Sets lSeparator as the current string from the given position,
    and updates lSeparatorPosition
  }
  procedure GetNextSeparator;
  var
    lNextSeparatorPosition: Integer;
  begin
    lNextSeparatorPosition:= lPos;
    while not (lWorking[lNextSeparatorPosition] in SeparatorCharacters) do
    begin
    lNextSeparatorPosition:= lNextSeparatorPosition + 1;
      if lNextSeparatorPosition > Length(lWorking) then
        Break; // from while
    end;
    lSeparatorPosition:= lNextSeparatorPosition;
    if lSeparatorPosition <= Length(lWorking) then
      lSeparator:= MidStr(lWorking, lSeparatorPosition, 1)
    else
      lSeparator:= '';
  end;

  {-----------------------------------------------------------------------------
    Returns the next term in the current string from the current position,
    and changes lPos to the position after this term.
  }
  function ReadTerm: String;
  begin
    GetNextSeparator;
    Result:= Trim(MidStr(lWorking, lPos, lSeparatorPosition - lPos));
    lPos:= lSeparatorPosition + 1;
  end;

  {-----------------------------------------------------------------------------
    Indicates if the given term is a title, ignoring any differences in case
    and trailing '.'
  }
  function IsTitle(ATerm: String): Boolean;
  var
    lTitleList : TStringList;
  begin
    if Length(ATerm) = 0 then Result := false
    else
    begin
      lTitleList := TStringList.Create;
      try
        lTitleList.DelimitedText := LowerCase(Trim(ResStr_Titles));
        ATerm:= LowerCase(Trim(ATerm));
        if ATerm[Length(ATerm)] = '.' then ATerm:= LeftStr(ATerm, Length(ATerm) - 1);
        Result := lTitleList.IndexOf(ATerm) <> -1;
      finally
        lTitleList.Free;
      end;
    end;
  end;

  {-----------------------------------------------------------------------------
    Indicates if the given term consists only of alphabetic characters, hyphens,
    and apostrophes which could therefore be a name.
  }
  function IsName(ATerm: String): Boolean;
  var
    i : integer;
  begin
    ATerm:= Trim(ATerm);
    Result := true;
    for i := 1 to Length(ATerm) do
    begin
      if not (ATerm[i] in ['a'..'z', 'A'..'Z','''','-']) then
      begin
        Result := false;
        break;
      end;
    end;
  end;

  {-----------------------------------------------------------------------------
    Indicates if the given term is a single letter.
  }
  function IsLetter(ATerm: String): Boolean;
  begin
    Result := (Length(ATerm) = 1) and (ATerm[1] in ['a'..'z', 'A'..'Z']);
  end;

  {---------------------------------------------------------------------------
    Splits the given term into a StringList, delimited by ' ' and '.'.
  }
  function SplitTerm(ATerm: String): TStringList;
  const
    Delimeters = [' ', '.'];
  var
    i: integer;
    lCurrentWord: String;
  begin
    Trim(ATerm);
    lCurrentWord := '';
    Result := TStringList.Create;
    i := 1;
    while i <= Length(ATerm) do
    begin
      if not (ATerm[i] in Delimeters) then
        lCurrentWord := lCurrentWord + ATerm[i]
      else
      begin
        Result.Add(lCurrentWord);
        lCurrentWord := '';
        if (ATerm[i] = '.') and (i < Length(ATerm)) and (ATerm[i + 1] = ' ') then
          i := i + 1
      end;
      i := i + 1;
    end;
    if lCurrentWord <> '' then Result.Add(lCurrentWord);
  end;

  {-----------------------------------------------------------------------------
    Indicates if the given term is not a complete name
  }
  function IsIncomplete(ATerm: String): Boolean;
  var
    lSubTerms: TStringList;
    i: integer;

  begin
    Result:= false;
    lSubTerms := SplitTerm(ATerm);
    try
      if lSubTerms.Count = 0 then Result := false
      // Is the term a single word of more than one letter?
      else if (lSubTerms.Count = 1) and (IsName(lSubTerms[0])) then Result := true
      else if lSubTerms.Count >= 2 then
      begin
        if IsTitle(lSubTerms[0]) then
        begin
          // Is the term a title followed by a single word of more than one letter?
          if (lSubTerms.Count = 2) and IsName(lSubTerms[1]) then Result := true
          // Is the term a title followed by one or more initials?
          else
          begin
            Result := true;
            for i:= 1 to lSubTerms.Count - 1 do
              if not IsLetter(lSubTerms[i]) then Result := false;
          end;
        end
      end
      // Is the term a set of one or more initials?
      else
      begin
        Result := true;
        for i:= 0 to lSubTerms.Count - 1 do
          if not IsLetter(lSubTerms[i]) then Result := false;
      end;
    finally
      lSubTerms.Free;
    end; // try
  end; // IsIncomplete

  {-----------------------------------------------------------------------------
    Returns a string of the given name with the title removed.
  }
  function RemoveTitle(ATerm: String): String;
  var
    lFirstAlphaString: String;
  begin
    Result := ATerm;
    if Length(ATerm) > 0 then
      begin
      lFirstAlphaString := FindNextAlphaString(ATerm, 1);
      if IsTitle(lFirstAlphaString) then
      begin
        Delete(Result, 1, Length(lFirstAlphaString));
        if Result[1] = '.' then Delete(Result, 1, 1);
      end;
      Result := Trim(Result);
    end;
  end;

  {-----------------------------------------------------------------------------
    Returns the last word in the given term.
  }
  function GetLastWord(ATerm: String): String;
  var
    lWords: TStringList;
  begin
    Result := '';
    lWords := SplitTerm(ATerm);
    try
      if lWords.Count > 0 then Result := lWords[lWords.Count - 1]
    finally
      lWords.Free;
    end;
  end;

begin
  lWorking := Value;
  if lWorking <> '' then begin
    // Convert 'and' or '+' to a single char (&) as it's easier to work with
    lWorking := FastAnsiReplace(lWorking, ' ' + ResStr_And + ' ', ' & ', [rfReplaceAll]);
    lWorking := FastAnsiReplace(lWorking, ' + ', ' & ', [rfReplaceAll]);

    lPos := 1;
    lCurrentToken := '';
    while lPos <= Length(lWorking) do
    begin
      lTerm1 := ReadTerm;
      if IsTitle(lTerm1) and (lSeparator = '&') then
      begin
        lTerm2 := ReadTerm;
        lCurrentToken := lTerm1 + ' ' + RemoveTitle(lTerm2);
        if Trim(lCurrentToken) <> '' then ParsedNames.Add(lCurrentToken);
        lCurrentToken := lTerm2;
        if Trim(lCurrentToken) <> '' then ParsedNames.Add(lCurrentToken);
      end
      else if IsIncomplete(lTerm1) and (lSeparator = '&') then
      begin
        lTerm2 := ReadTerm;
        lCurrentToken := lTerm1 + ' ' + GetLastWord(lTerm2);
        if Trim(lCurrentToken) <> '' then ParsedNames.Add(lCurrentToken);
        lCurrentToken := lTerm2;
        if Trim(lCurrentToken) <> '' then ParsedNames.Add(lCurrentToken);
      end
      else if IsName(lTerm1) and (lSeparator = ',') then
      begin
        lTerm2 := ReadTerm;
        lCurrentToken := lTerm2 + ' ' + lTerm1;
        if Trim(lCurrentToken) <> '' then ParsedNames.Add(lCurrentToken);
      end
      else
      begin
        lCurrentToken := lTerm1;
        if Trim(lCurrentToken) <> '' then ParsedNames.Add(lCurrentToken);
      end;
    end;
  end;
end;


end.
