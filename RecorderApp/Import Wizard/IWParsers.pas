{===============================================================================
  Unit:        IWParsers

  Defines:     TParsedField
               TImportWizardParser
                 TErrorParser
                   TAbundanceDataParser
                   TAltitudeParser
                   TBooleanParser
                   TBRCSourceParser
                   TMapMateKeyParser
                   TNonRequiredDateParser
                     TDateParser
                   TDeterminationDateParser
                   TObserverParser
                   TRecordIDParser
                   TSiteIDParser
                   TSpatialRefParser
                     TRequiredSpatialRefParser
                   TTaxonDataParser
                   TTextParser
                     TRequiresTextParser
                     TSpatialRefSystemParser
                   TViceCountyNumberParser
                   TDeterminerParser
                   TSampleVCParser
  Description: Parsers used to handle data coming in through the Import Wizard.

  Model:

  Last revision information:
    $Revision: 50 $
    $Date: 21/03/13 15:46 $
    $Author: Michaelcaptain $

===============================================================================}
unit IWParsers;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StrUtils, Contnrs, ExceptionForm, FastStrings, FastStringFuncs, DateUtils,
  DataClasses, VagueDate, IWConstants, ApplicationSettings, ValidationData;

resourcestring
  ResStr_None = 'None';
  ResStr_AltitudeInvalid =
      'Character ''%s'' is invalid in the altitude data.';
  ResStr_And = 'and';
  ResStr_BooleanInvalid   = 'Value supplied is not recognised as Yes or No.';
  ResStr_BRCSourceInvalid =
      'Value supplied is not recognised as a BRC Source.  Valid values are ''FLD'', ''MUS'', ''LIT''.';
  ResStr_DateInFuture = 'A date cannot be in the future.';
  ResStr_DateRequired = 'A date must be specified.';
  ResStr_DateNotValid = 'Value supplied is not recognised as a valid date or vague date.';
  ResStr_DateNotInSurvey = 'Date must lie in the survey date range.';
  ResStr_Estimate = 'Estimate';
  ResStr_Exact    = 'Exact';
  ResStr_False    = 'FALSE';
  ResStr_ImportedQualifier = 'Imported';
  ResStr_InvalidFieldType  = 'Invalid field type index requested from parser';
  ResStr_MapMateKeyInvalid =
      'Value supplied is not recognised as a MapMate key, which should be '+
      'between 4 and 10 characters in length and cannot contain spaces.';
  ResStr_No = 'NO';
  ResStr_RecordIDInvalid =
      'Record ID values must be 6 characters or less and cannot contain spaces.';
  ResStr_ObserverRequired = 'At least one observer must be specified.';
  ResStr_DeterminerRequired = 'A determiner is required for every field.';
  ResStr_SiteIDInvalid =
      'Site ID values must be 8 characters in length and cannot contain spaces.';
  ResStr_SpatialRefInvalid = 'Value supplied is not recognised as a valid grid reference.';
  ResStr_SpatialRefInvalidForSystem =
      'Value supplied is not recognised as a valid grid reference for the specified system.';
  ResStr_SpatialRefRequired = 'A grid reference must be specified.';
  ResStr_SpatialRefNotInBoundingBox =
      'Grid reference must lie in the bounding box of its survey.';
  ResStr_Titles = 'mr, mrs, ms, miss, dr, sir, lord, rev, col';
  ResStr_True = 'TRUE';
  ResStr_ValueRequired = 'A value must be specified.';
  ResStr_ValueToLong =
      'The value supplied is too long (%d).  A maximum of %d characters is allowed.';
  ResStr_ViceCountyInvalid = 'Value supplied is not recognised as a vice-county number.';
  ResStr_Yes = 'YES';
  ResStr_ValueNotAllowed =
      'The value supplied is not allowed. It must be one of the following:%s';
  ResStr_ValueTooLongForItem =
      'The value ''%s'' is too long (%d). A maximum of %d characters is allowed.';

const
  FLD_ACCURACY      = 'accuracy';
  FLD_BOOLEAN       = 'boolean';
  FLD_DATA          = 'data';
  FLD_DATAQUALIFIER = 'measurement_qualifier_key';
  FLD_DATAUNIT      = 'measurement_unit_key';
  FLD_DATESTART     = 'date_start';
  FLD_DATEEND       = 'date_end';
  FLD_DATETYPE      = 'date_type';
  FLD_KEY           = 'key';
  FLD_LAT           = 'lat';
  FLD_LONG          = 'long';
  FLD_NAME          = 'name';
  FLD_QUALIFIER     = 'qualifier';
  FLD_RECORDID      = 'record_id';
  FLD_SITEID        = 'site_id';
  FLD_SPATIALREF    = 'spatial_ref';
  FLD_SYSTEM        = 'system';

type
  EFieldParser = class(TExceptionPath)
  end;

  TFieldParserEvent = procedure (Sender: TObject; Position: Integer; const Field: string;
      const Value: Variant) of object;

  TParsedField = class(TObject)
  protected
    FDataType: String;
    FName: String;
  public
    constructor Create(Name: String; DataType: String);
    property DataType: String read FDataType;
    property Name: String read FName;
  end;

  TImportWizardParser = class(TPersistent)
  protected
    FFailed: Boolean;
    FLastErrorMessage: String;
    FSingleRecord: Boolean;
    FWorking: String;
    FFields: TObjectList;
    FUseOldImportWizard: Boolean;
    function GetFieldCount: Integer; virtual;
    function GetFields(Index: Integer): TParsedField; virtual;
    procedure TrimWhitespace; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); virtual; abstract;
    property Failed: Boolean read FFailed;
    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TParsedField read GetFields;
    property LastErrorMessage: String read FLastErrorMessage;
    property SingleRecord: Boolean read FSingleRecord;
    property UseOldImportWizard: Boolean read FUseOldImportWizard write FUseOldImportWizard;
  end;

  TErrorParser = class(TImportWizardParser)
  protected
    procedure Fail(const AMessage: string); virtual;
  end;

  TNameParser = class(TErrorParser)
  protected
    procedure ParseNameField(const Value: string;Parsednames:Tstringlist);
  end;

  TAbundanceDataParser = class(TErrorParser)
  private
    FCallback: TFieldParserEvent;
    FItems: TStringList;
    procedure GetSeparateDataItems;
    procedure ParseDataItem(const AText: string; AIndex: integer);
    procedure ParseIndividualItems;
  public
    constructor Create; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;

  TTextParser = class(TErrorParser)
  private
    FMaximumLength: Integer;
  public
    constructor Create; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent);
        override;
    property MaximumLength: Integer read FMaximumLength write FMaximumLength;
  end;
  
  TObserverParser = class(TNameParser)
  public
    constructor Create; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;

  TDeterminerParser = class(TNameParser)
  public
    constructor Create; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;

  TAltitudeParser = class(TErrorParser)
  public
    constructor Create; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;

  TMapMateKeyParser = class(TErrorParser)
  public
    constructor Create; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;

  TBooleanParser = class(TErrorParser)
  public
    constructor Create; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;

  TRecordIDParser = class(TErrorParser)
  public
    constructor Create; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;
  
  TSiteIDParser = class(TErrorParser)
  public
    constructor Create; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;
  
  TBRCSourceParser = class(TErrorParser)
  public
    constructor Create; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;
  
  TViceCountyNumberParser = class(TErrorParser)
  private
    FCachedKeys: TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;

  TSampleVCParser = class(TErrorParser)
  private
    FCachedKeys: TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;


  TNonRequiredDateParser = class(TErrorParser)
  private
    FShortDateFormat: String;
    FDateSeparator: char;
    FSurveyKey : TKeyString;
    FValidationData : TdmValidation;
    procedure SetShortDateFormat(const Value: String);
    procedure SetDateSeparator(const Value: char);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
    property ShortDateFormat: String read FShortDateFormat write SetShortDateFormat;
    property DateSeparator: char read FDateSeparator write SetDateSeparator;
  end;

  TDateParser = class(TNonRequiredDateParser)
  public
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;

  TDeterminationDateParser = class(TErrorParser)
  private
    FShortDateFormat: String;
    FDateSeparator: char;
    procedure SetShortDateFormat(const Value: String);
    procedure SetDateSeparator(const Value: char);
  public
    constructor Create; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
    property ShortDateFormat: String read FShortDateFormat write SetShortDateFormat;
    property DateSeparator: char read FDateSeparator write SetDateSeparator;
  end;

  TSpatialRefParser = class(TErrorParser)
  private
    FSurveyKey : TKeyString;
    FValidationData : TdmValidation;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;

  TRequiredSpatialRefParser = class(TSpatialRefParser)
  public
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;

  {-----------------------------------------------------------------------------
    Parser for spatial reference system fields.
  }
  TSpatialRefSystemParser = class(TTextParser)
  protected
    procedure ParseSpatialRef(const Value: string); virtual;
  public
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;

  TRequiredTextParser = class(TTextParser)
  public
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
  end;

  {-----------------------------------------------------------------------------
    Parser for taxon occurrence data fields.
  }
  TTaxonDataParser = class(TErrorParser)
  private
    FQualifierKey: String;
    FUnitKey: String;
    FPosition: Integer;
    FRestrictedValues: TStringList;
    FFormattedRestrictedValues: String;
    procedure SetUnitKey(const AValue: String);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ParseField(const Value: string; CallBack: TFieldParserEvent); override;
    procedure RefreshRestrictedValues;
    property RestrictedValues: TStringList read FRestrictedValues;
    property Position: Integer read FPosition;
    property QualifierKey: String read FQualifierKey write FQualifierKey;
    property UnitKey: String read FUnitKey write SetUnitKey;
  end;

  TImportWizardParserClass = class of TImportWizardParser;

{==============================================================================}
implementation

uses
  GeneralFunctions, Variants, DatabaseAccessADO, SpatialRefFuncs,
  Measurements, IWSettings, IWUserSuppliedData, IWBasePage, MainTBar, IWTableRule;

var
  mTaxonDataParserPosition: Integer;

{-==============================================================================
    TParsedField
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TParsedField.Create(Name: String; DataType: String);
begin
  inherited Create;
  FName := Name;
  FDataType := DataType;
end;  // TParsedField.Create

{-==============================================================================
    TImportWizardParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TImportWizardParser.Create;
begin
  inherited;
  FLastErrorMessage := '';
  FFailed           := False;
  FSingleRecord     := True;
  FFields           := TObjectList.Create;
end;  // TImportWizardParser.Create

{-------------------------------------------------------------------------------
}
destructor TImportWizardParser.Destroy;
begin
  FFields.Free;
  inherited;
end;  // TImportWizardParser.Destroy

{-------------------------------------------------------------------------------
}
function TImportWizardParser.GetFieldCount: Integer;
begin
  Result := FFields.Count;
end;  // TImportWizardParser.GetFieldCount

{-------------------------------------------------------------------------------
}
function TImportWizardParser.GetFields(Index: Integer): TParsedField;
begin
  if (Index < 0) or (Index > FieldCount - 1) then
    raise EFieldParser.Create(ResStr_InvalidFieldType);
  Result := FFields[Index] as TParsedField;
end;  // TImportWizardParser.GetFields

{-------------------------------------------------------------------------------
}
procedure TImportWizardParser.TrimWhitespace;
var
  lWorkingLength: Integer;
begin
  FWorking := Trim(FWorking);
  // Convert tab, CR, LF to space
  FWorking := FastAnsiReplace(FWorking, #9, ' ', [rfReplaceAll]);
  FWorking := FastAnsiReplace(FWorking, #13, ' ', [rfReplaceAll]);
  FWorking := FastAnsiReplace(FWorking, #10, ' ', [rfReplaceAll]);
  // Reduce any double spacing to single spaces
  repeat
    lWorkingLength := Length(FWorking);
    FWorking := FastAnsiReplace(FWorking, '  ', ' ', [rfReplaceAll]);
  until lWorkingLength = Length(FWorking);
end;  // TImportWizardParser.TrimWhitespace

{-==============================================================================
    TAbundanceDataParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TAbundanceDataParser.Create;
begin
  inherited Create;
  FSingleRecord := False;
  FFields.Add(TParsedField.Create(FLD_DATA, 'varchar(20)'));
  FFields.Add(TParsedField.Create(FLD_QUALIFIER, 'varchar(100)'));
  FFields.Add(TParsedField.Create(FLD_ACCURACY, 'varchar(20)'));
end;  // TAbundanceDataParser.Create

{-------------------------------------------------------------------------------
}
procedure TAbundanceDataParser.GetSeparateDataItems;
var
  lPos: Integer;
  lCurrentToken: String;
begin
  lPos := 1;
  lCurrentToken := '';
  while lPos <= Length(FWorking) do begin
    // always accept a semi-colon as a list separator, as this may often be
    // used in the UK where the locale list separator is comma. Likewise, accept a comma
    // for countries such as Germany where the list separator is semi-colon.
    if (FWorking[lPos]=ListSeparator) or (FWorking[lPos]=';') or (FWorking[lPos]=',') then begin
      FItems.Add(Trim(lCurrentToken));
      lCurrentToken := '';
    end
    else begin
      lCurrentToken := lCurrentToken + FWorking[lPos];
    end;
    Inc(lPos);
  end;
  if lCurrentToken <> '' then
    FItems.Add(Trim(lCurrentToken))
end;  // TAbundanceDataParser.GetSeparateDataItems 

{-------------------------------------------------------------------------------
}
procedure TAbundanceDataParser.ParseDataItem(const AText: string; AIndex: integer);
var
  lPos: Integer;
  lQualifier: String;
  lData: String;
  lNumber: Extended;
  lIsEstimate: Boolean;
begin
  // Retrieve alpha characters from end of text - the qualifier
  lPos := Length(AText);
  while (lPos > 0) and (AText[lPos] in ['a'..'z', 'A'..'Z', 'À'..'ÿ', '(', ')']) do
    Dec(lPos);
  // Read the qualifier, assuming none if not specified
  lQualifier := Copy(AText, lPos + 1, Length(AText));
  lQualifier := AnsiReplaceStr(AnsiReplaceStr(lQualifier, '(', ''), ')', '');
  if lQualifier = '' then lQualifier := ResStr_None;
  // Read the data, assuming 1 if not specified
  lData := Trim(Copy(AText, 1, lPos));
  if lData = '' then lData := '1';

  lIsEstimate := not TryStrToFloat(lData, lNumber);
  if lData[1] = '~' then lData := MidStr(lData, 2, Length(lData) - 1);
  if lData[Length(lData)] = '?' then
    lData := FastStringFuncs.LeftStr(lData, Length(lData) - 1);

  if Length(lData) > MAX_LENGTH_DATA then
    Fail(Format(ResStr_ValueTooLongForItem, [lData, Length(lData), MAX_LENGTH_DATA]))
  else
  if Length(lQualifier) > MAX_LENGTH_QUALIFIER then
    Fail(Format(
        ResStr_ValueTooLongForItem,
        [lQualifier,
        Length(lQualifier),
        MAX_LENGTH_QUALIFIER]))
  else
  if Assigned(FCallback) then begin
    FCallback(Self, AIndex, FLD_DATA, lData);
    FCallback(Self, AIndex, FLD_QUALIFIER, lQualifier);
    if lIsEstimate then
      FCallback(Self, AIndex, FLD_ACCURACY, ResStr_Estimate)
    else
      FCallback(Self, AIndex, FLD_ACCURACY, ResStr_Exact);
  end;
end;  // TAbundanceDataParser.ParseDataItem

{-------------------------------------------------------------------------------
}
procedure TAbundanceDataParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
begin
  FWorking := Value;
  TrimWhitespace;
  FFailed   := False;
  FItems    := TStringList.Create;
  FCallback := Callback;
  try
    GetSeparateDataItems;
    ParseIndividualItems;
  finally
    FreeAndNil(FItems);
  end;
end;  // TAbundanceDataParser.ParseField

{-------------------------------------------------------------------------------
}
procedure TAbundanceDataParser.ParseIndividualItems;
var
  i: Integer;
begin
  for i := 0 to FItems.Count-1 do
    ParseDataItem(FItems[i], i);
end;  // TAbundanceDataParser.ParseIndividualItems

{-==============================================================================
    TTextParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TTextParser.Create;
begin
  inherited;
  FFields.Add(TParsedField.Create(FLD_DATA, 'varchar(500)'));
end;  // TTextParser.Create

{-------------------------------------------------------------------------------
}
procedure TTextParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
begin
  FWorking := Value;
  TrimWhitespace;
  FFailed := False;
  if (MaximumLength > 0) and (Length(FWorking) > MaximumLength) then
    Fail(Format(ResStr_ValueToLong, [Length(FWorking), MaximumLength]))
  else if Assigned(Callback) then
    CallBack(Self, 0, FLD_DATA, FWorking);
end;  // TTextParser.ParseField

{-==============================================================================
    TErrorParser
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TErrorParser.Fail(const AMessage: string);
begin
  FFailed := True;
  FLastErrorMessage := AMessage;
end;  // TErrorParser.Fail

{-==============================================================================
    TNameParser
===============================================================================}
{-------------------------------------------------------------------------------
  Special handling for parsing of names, e.g. determiner or observer names
}
procedure TNameParser.ParseNameField(const Value: string;Parsednames:Tstringlist);
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

{-==============================================================================
    TObserverParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TObserverParser.Create;
begin
  inherited Create;
  FSingleRecord := False;
  FFields.Add(TParsedField.Create(FLD_NAME, 'varchar(100)'));
end;  // TObserverParser.Create


{-------------------------------------------------------------------------------
}
procedure TObserverParser.ParseField(const Value: string; CallBack: TFieldParserEvent);

var
  i : integer;
  lCurrentToken: String;
  lTokenId: Integer;
  ParsedNames : TStringList;
  procedure DoCallback;

  begin
    // Return the token to the caller, and reset the token
    if Assigned(CallBack) then
      CallBack(Self, lTokenId, FLD_NAME, Trim(lCurrentToken));
    Inc(lTokenId);
    lCurrentToken := '';
  end;

begin
  FWorking := Value;
  TrimWhitespace;
  FFailed := False;
  if FWorking='' then
    Fail(ResStr_ObserverRequired)
  else begin
    ParsedNames := TStringList.Create;
    ParseNameField (FWorking,ParsedNames);
    lTokenId := 0;
    for i := 0 to ParsedNames.Count-1 do
    begin
      lCurrentToken:= (ParsedNames[i]);
      DoCallBack;
    end;
  end;
end;  // TObserverParser.ParseField


{-==============================================================================
    TDeterminerParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TDeterminerParser.Create;
begin
  inherited Create;
  FSingleRecord := True;
  FFields.Add(TParsedField.Create(FLD_DATA, 'varchar(100)'));
end;  // TDeterminerParser.Create

procedure TDeterminerParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
var
  lCurrentToken: String;
  lTokenId: Integer;
  ParsedNames : TStringList;
  procedure DoCallback;

  begin
    // Return the token to the caller, and reset the token
    if Assigned(CallBack) then
      CallBack(Self, lTokenId, FLD_DATA, Trim(lCurrentToken));
    Inc(lTokenId);
    lCurrentToken := '';
  end;

begin
  FWorking := Value;
  TrimWhitespace;
  FFailed := False;
  lTokenId := 0;
  if FWorking  <> ''  then begin
    ParsedNames := TStringList.Create;
    ParseNameField (FWorking,ParsedNames);
    lCurrentToken:= (ParsedNames[0]);
  end else
    lCurrentToken:= (FWorking);

  DoCallBack;
end;

{-==============================================================================
    TAltitudeParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TAltitudeParser.Create;
begin
  inherited;
  FFields.Add(TParsedField.Create(FLD_DATA, 'varchar(20)'));
  FFields.Add(TParsedField.Create(FLD_ACCURACY, 'varchar(20)'));
end;  // TAltitudeParser.Create

{-------------------------------------------------------------------------------
}
procedure TAltitudeParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
var
  lIsEstimate: Boolean;
  i: Integer;
begin
  FWorking := Value;
  TrimWhitespace;
  FFailed := False;
  // Strip m from end if format is 54m
  if Length(FWorking) > 0 then
    if CompareText(FWorking[Length(FWorking)], 'm') = 0 then
      FWorking := FastStringFuncs.LeftStr(FWorking, Length(FWorking) - 1);
  lIsEstimate := (SmartPos('~', FWorking) > 0) or (SmartPos('?', FWorking) > 0);
  FWorking := FastAnsiReplace(FWorking, '~', '', [rfReplaceAll]);
  FWorking := FastAnsiReplace(FWorking, '?', '', [rfReplaceAll]);
  lIsEstimate := lIsEstimate or (not IsFloat(FWorking));

  if Length(FWorking) > MAX_LENGTH_DATA then
    Fail(Format(ResStr_ValueToLong, [Length(FWorking), MAX_LENGTH_DATA]))
  else
    // Check no other alpha characters in text
    for i := 1 to Length(FWorking) do
      if FWorking[i] in ['a'..'z', 'A'..'Z'] then begin
        Fail(Format(ResStr_AltitudeInvalid, [FWorking[i]]));
        Break; // from loop
      end;

  if (not FFailed) and Assigned(CallBack) then
  begin
    CallBack(Self, 0, FLD_DATA, FWorking);
    if lIsEstimate then
      CallBack(Self, 0, FLD_ACCURACY, ResStr_Estimate)
    else
      CallBack(Self, 0, FLD_ACCURACY, ResStr_Exact);
  end;
end;  // TAltitudeParser.ParseField

{-==============================================================================
    TMapMateKeyParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TMapMateKeyParser.Create;
begin
  inherited;
  FFields.Add(TParsedField.Create(FLD_SITEID, 'char(8)'));
  FFields.Add(TParsedField.Create(FLD_RECORDID, 'char(8)'));
end;  // TMapMateKeyParser.Create

{-------------------------------------------------------------------------------
}
procedure TMapMateKeyParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
var
  lSiteID: String;
  lRecordID: String;

const
  PADDING = 'EEEEEEE';

begin
  FWorking := Value;
  TrimWhitespace;
  FFailed := False;
  if FWorking='' then
    Fail(ResStr_ValueRequired)
  else if (Length(FWorking) < 4) or (Length(FWorking) > 10) or (SmartPos(' ', FWorking) > 0) then
    Fail(ResStr_MapMateKeyInvalid)
  else begin
    lSiteID   := 'MMEEE' + Uppercase(RightStr(FWorking, 3));
    lRecordID := Uppercase(FastStringFuncs.LeftStr(FWorking, Length(FWorking) - 3));
    lRecordID := FastStringFuncs.RightStr(PADDING + lRecordID, 8);
    if Assigned(Callback) then begin
      CallBack(Self, 0, FLD_SITEID, lSiteID);
      CallBack(Self, 0, FLD_RECORDID, lRecordID);
    end;
  end;
end;  // TMapMateKeyParser.ParseField

{-==============================================================================
    TBooleanParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBooleanParser.Create;
begin
  inherited;
  FFields.Add(TParsedField.Create(FLD_BOOLEAN, 'bit'));
end;  // TBooleanParser.Create

{-------------------------------------------------------------------------------
}
procedure TBooleanParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
begin
  FWorking := Value;
  TrimWhitespace;
  FFailed := False;
  FWorking := Uppercase(FWorking);
  if (FWorking='1') or (FWorking='-1') or (FWorking=ResStr_Yes)
      or (FWorking=ResStr_True) or (FWorking='Y') then begin
    if Assigned(Callback) then
      CallBack(Self, 0, FLD_BOOLEAN, 1)
  end
  else if (FWorking='0') or (FWorking=ResStr_No)
      or (FWorking=ResStr_False) or (FWorking='N') then begin
    if Assigned(Callback) then
      CallBack(Self, 0, FLD_BOOLEAN, 0)
  end
  else
    Fail(ResStr_BooleanInvalid);
end;  // TBooleanParser.ParseField

{-==============================================================================
    TRecordIDParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TRecordIDParser.Create;
begin
  inherited;
  FFields.Add(TParsedField.Create(FLD_RECORDID, 'char(8)'));
end;  // TRecordIDParser.Create

{-------------------------------------------------------------------------------
}
procedure TRecordIDParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
var
  lRecordID: String;

const
  ZEROS = '00000000';

begin
  FWorking := Value;
  TrimWhitespace;
  FFailed := False;
  if FWorking = '' then
    Fail(ResStr_ValueRequired)
  else if (Length(FWorking) > 6) or (SmartPos(' ', FWorking) > 0) then
    Fail(ResStr_RecordIDInvalid)
  else begin
    if UseOldImportWizard then
      lRecordID := FastStringFuncs.RightStr(ZEROS + FWorking, 6)
    else
      lRecordID := FastStringFuncs.RightStr(ZEROS + FWorking, 8);
    if Assigned(Callback) then
      CallBack(Self, 0, FLD_RECORDID, lRecordID);
  end;
end;  // TRecordIDParser.ParseField

{-==============================================================================
    TSiteIDParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSiteIDParser.Create;
begin
  inherited;
  FFields.Add(TParsedField.Create(FLD_SITEID, 'char(8)'));
end;  // TSiteIDParser.Create

{-------------------------------------------------------------------------------
}
procedure TSiteIDParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
begin
  FWorking := Value;
  TrimWhitespace;
  FFailed := False;
  if FWorking = '' then
    Fail(ResStr_ValueRequired)
  else if (Length(FWorking) <> 8) or (SmartPos(' ', FWorking) > 0) then
    Fail(ResStr_SiteIDInvalid)
  else if Assigned(Callback) then
    CallBack(Self, 0, FLD_SITEID, FWorking);
end;  // TSiteIDParser.ParseField

{-==============================================================================
    TBRCSourceParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBRCSourceParser.Create;
begin
  inherited;
  FFields.Add(TParsedField.Create(FLD_KEY, 'char(16)'));
end;  // TBRCSourceParser.Create

{-------------------------------------------------------------------------------
}
procedure TBRCSourceParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
var
  lKey: String;
begin
  FWorking := Value;
  TrimWhitespace;
  FFailed := False;
  FWorking := Uppercase(FWorking);
  lKey:='';
  if FWorking='FLD' then
    lKey := 'NBNSYS0000000026'
  else if FWorking='MUS' then
    lKey :='NBNSYS0000000084'
  else if FWorking='LIT' then
    lKey := 'NBNSYS0000000083'
  else if FWorking='' then
    Fail(ResStr_ValueRequired)
  else
    Fail(ResStr_BRCSourceInvalid);
  if Assigned(Callback) and (lKey<>'') then
      CallBack(Self, 0, FLD_KEY, lKey)
end;  // TBRCSourceParser.ParseField 

{-==============================================================================
    TViceCountyNumberParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TViceCountyNumberParser.Create;
begin
  inherited;
  FFields.Add(TParsedField.Create(FLD_KEY, 'char(16)'));
end;  // TViceCountyNumberParser.Create

{-------------------------------------------------------------------------------
}
destructor TViceCountyNumberParser.Destroy;
begin
  inherited;
  FreeAndNil(FCachedKeys);
end;  // TViceCountyNumberParser.Destroy

{-------------------------------------------------------------------------------
}
procedure TViceCountyNumberParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
var
  lLocationKey: String;
begin
  FWorking := Value;
  TrimWhitespace;
  FFailed := False;
  if FWorking = '' then
    Fail(ResStr_ValueRequired)
  else
  if UseOldImportWizard then
  begin
    // Check if number is a valid vice-county number
    lLocationKey := VarToStr(dmDatabase.GetStoredProcOutputParam(
        'usp_LocationKeyFromVCNumber_Get', ['@Value', FWorking], '@Location_Key'));
    if lLocationKey = '' then
      Fail(ResStr_ViceCountyInvalid)
    else
    if Assigned(CallBack) then
      CallBack(Self, 0, FLD_KEY, lLocationKey);
  end else begin
    // Using cached version of parser, to speed up lookups.
    if not Assigned(FCachedKeys) then begin
      FCachedKeys := TStringList.Create;
      with dmDatabase.GetRecordset('usp_Locations_Select_AllViceCounties', []) do
      begin
        while not Eof do begin
          // Data returned is formatted as key/value pairs.
          FCachedKeys.Add(Fields['Data'].Value);
          MoveNext;
        end;
        Close;
      end;
    end;

    if FCachedKeys.Values[FWorking] = '' then
      Fail(ResStr_ViceCountyinvalid)
    else
    if Assigned(CallBack) then
      Callback(Self, 0, FLD_KEY, FCachedKeys.Values[FWorking]);
  end;
end;  // TViceCountyNumberParser.ParseField

{-==============================================================================
    TViceCountyNumberParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSampleVCParser.Create;
begin
  inherited;
  FFields.Add(TParsedField.Create(FLD_KEY, 'char(16)'));
end;  // TSampleVCParser.Create

{-------------------------------------------------------------------------------
}
destructor TSampleVCParser.Destroy;
begin
  inherited;
  FreeAndNil(FCachedKeys);
end;  // TSampleVCParser.Destroy

{-------------------------------------------------------------------------------
}
procedure TSampleVCParser.ParseField(const Value: string; CallBack: TFieldParserEvent);

begin
  FWorking := Value;
  TrimWhitespace;
  FFailed := False;
  // Using cached version of parser, to speed up lookups.
  if not Assigned(FCachedKeys) then begin
    FCachedKeys := TStringList.Create;
    with dmDatabase.GetRecordset('usp_Admin_Areas_Select_AllViceCounties', []) do
    begin
      while not Eof do begin
        // Data returned is formatted as key/value pairs.
        FCachedKeys.Add(Fields['Data'].Value);
        MoveNext;
      end;
      Close;
    end;
    end;
  if (FCachedKeys.Values[FWorking] = '') AND (FWorking <> '') then
      Fail(ResStr_ViceCountyinvalid)
  else
  if Assigned(CallBack) then
      Callback(Self, 0, FLD_KEY, FCachedKeys.Values[FWorking]);

  end;  // TSampleVCParser.ParseField

{-==============================================================================
    TDateParser
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TDateParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
begin
  if Trim(Value) = '' then
    Fail(ResStr_DateRequired)
  else
    inherited;
end;  // TDateParser.ParseField

{-==============================================================================
    TNonRequiredDateParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TNonRequiredDateParser.Create;
begin
  inherited;
  FFields.Add(TParsedField.Create(FLD_DATESTART, 'int'));
  FFields.Add(TParsedField.Create(FLD_DATEEND, 'int'));
  FFields.Add(TParsedField.Create(FLD_DATETYPE, 'varchar(2)'));
  // Create validation object to validate date against the chosen survey
  FSurveyKey := AppSettings.IWSurveyKey;
  if FSurveyKey <> '' then begin
    FValidationData := TdmValidation.Create(nil);
    FValidationData.SetDatabase(dmDatabase.Connection);
  end;
end;  // TNonRequiredDateParser.Create

{-------------------------------------------------------------------------------
}
destructor TNonRequiredDateParser.Destroy;
begin
  inherited;
  FreeAndNil(FValidationData);
end;  // TNonRequiredDateParser.Destroy

{-------------------------------------------------------------------------------
}
procedure TNonRequiredDateParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
var
  lSystemShortDateFormat: String;
  lSystemDateSeparator: char;
  lDate: TDateTime;
  lVagueDate: TVagueDate;

  {-----------------------------------------------------------------------------
    If the given vague date is of type 'unknown' and the given string
    is not equal to 'unknown', then returns false.
    Otherwise, returns true.
  }
  function IsValidUnknownDate(const AWorking: string; const AVagueDate: TVagueDate): boolean;
  begin
    Result:= True;
    If (AVagueDate.DateTypeString = 'U') and
        (AnsiCompareText(Trim(AWorking), ResStr_Unknown) <> 0) then Result:= false;
  end;

begin
  FWorking := Value;
  TrimWhitespace;
  FFailed := False;
  // Use system default date format if not specified, otherwise use specified one
  if FShortDateFormat='' then
    FShortDateFormat := SysUtils.ShortDateFormat;
  if FDateSeparator='' then
    FDateSeparator := SysUtils.DateSeparator;
  lSystemShortDateFormat   := SysUtils.ShortDateFormat;
  lSystemDateSeparator     := SysUtils.DateSeparator;
  SysUtils.ShortDateFormat := FShortDateFormat;
  SysUtils.DateSeparator   := FDateSeparator;
  try
    if TryStrToDateTime(FWorking, lDate) then begin
      lDate:= DateOf(lDate);
      if CompareDate(Now, lDate) >= 0 then begin
        if (FSurveyKey <> '') then begin
          // Validates a system date against the survey in the new import wizard
          if (not FValidationData.CheckEventDateAgainstSurvey(
              FSurveyKey,
              DateToVagueDate(lDate))) then
                Fail(ResStr_DateNotInSurvey);
        end;
        if Assigned(Callback) then begin
          Callback(Self, 0, FLD_DATESTART, Trunc(lDate));
          Callback(Self, 0, FLD_DATEEND, Trunc(lDate));
          // if the string does not contain a date part (which has therefore
          // defaulted to Delphi's zero date, 1899-12-30), treat it as 'Unknown'
          Callback(Self, 0, FLD_DATETYPE,
              IfThen((lDate = 0) and (Pos('1899', FWorking) = 0), 'U', 'D'));
        end;
      end
      else Fail(ResStr_DateInFuture);
    end
    else begin
      // Not a system date, so test using vague dates
      if (ansipos(lSystemDateSeparator +  lSystemDateSeparator, FWorking) = 0 )
      and (FWorking <>  lSystemDateSeparator)
      and (IsVagueDate(FWorking)) then begin
        lVagueDate := StringToVagueDate(FWorking);
        if IsValidUnknownDate(FWorking, lVagueDate) then
        begin
          if (CompareDateToVagueDate(Now, lVagueDate) >= 0) then
          begin
            if (FSurveyKey <> '') then begin
              // Validates a vague date against the survey in the new import wizard
              if (not FValidationData.CheckEventDateAgainstSurvey(
                  FSurveyKey,
                  lVagueDate)) then
                Fail(ResStr_DateNotInSurvey);
            end;
              if Assigned(Callback) then
              begin
                Callback(Self, 0, FLD_DATESTART, Trunc(lVagueDate.StartDate));
                Callback(Self, 0, FLD_DATEEND, Trunc(lVagueDate.EndDate));
                Callback(Self, 0, FLD_DATETYPE, lVagueDate.DateTypeString);
              end;
          end
          else Fail(ResStr_DateInFuture);
        end
        else Fail(ResStr_DateNotValid);
      end
      else
        Fail(ResStr_DateNotValid);
    end;
  finally
    SysUtils.ShortDateFormat := lSystemShortDateFormat;
    SysUtils.DateSeparator   := lSystemDateSeparator;
  end; // try
end;  // TNonRequiredDateParser.ParseField

{-------------------------------------------------------------------------------
}
procedure TNonRequiredDateParser.SetDateSeparator(const Value: char);
begin
  FDateSeparator := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TNonRequiredDateParser.SetShortDateFormat(const Value: String);
begin
  FShortDateFormat := Value;
end;  // TNonRequiredDateParser.SetShortDateFormat

{-==============================================================================
    TDeterminationDateParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TDeterminationDateParser.Create;
begin
  inherited;
  FFields.Add(TParsedField.Create(FLD_DATESTART, 'int'));
  FFields.Add(TParsedField.Create(FLD_DATEEND, 'int'));
  FFields.Add(TParsedField.Create(FLD_DATETYPE, 'varchar(2)'));
end;

{-------------------------------------------------------------------------------
}
procedure TDeterminationDateParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
var
  lSystemShortDateFormat: String;
  lSystemDateSeparator: char;
  lDate: TDateTime;
  lVagueDate: TVagueDate;

  {-----------------------------------------------------------------------------
    If the given vague date is of type 'unknown' and the given string
    is not equal to 'unknown', then returns false.
    Otherwise, returns true.
  }
  function IsValidUnknownDate(const AWorking: string; const AVagueDate: TVagueDate): boolean;
  begin
    Result:= True;
    If (AVagueDate.DateTypeString = 'U') and
        (AnsiCompareText(Trim(AWorking), ResStr_Unknown) <> 0) then Result:= false;
  end;
begin
  FFailed := False;
  if Trim(Value) = '' then begin
    if Assigned(Callback) then begin
      // Dummy values so that later the class in IWOutputFieldGenerators
      // can copy values from the Date column instead.
      Callback(Self, 0, FLD_DATESTART, -1);
      Callback(Self, 0, FLD_DATEEND, -1);
      Callback(Self, 0, FLD_DATETYPE, 'Z');
    end;
  end
  else
  begin
    FWorking := Value;
    TrimWhitespace;
    // Use system default date format if not specified, otherwise use specified one
    if FShortDateFormat='' then
      FShortDateFormat := SysUtils.ShortDateFormat;
    if FDateSeparator='' then
      FDateSeparator := SysUtils.DateSeparator;
    lSystemShortDateFormat   := SysUtils.ShortDateFormat;
    lSystemDateSeparator     := SysUtils.DateSeparator;
    SysUtils.ShortDateFormat := FShortDateFormat;
    SysUtils.DateSeparator   := FDateSeparator;
    try
      if TryStrToDateTime(FWorking, lDate) then begin
        lDate:= DateOf(lDate);
        if CompareDate(Now, lDate) >= 0 then begin
          if Assigned(Callback) then begin
            Callback(Self, 0, FLD_DATESTART, Trunc(lDate));
            Callback(Self, 0, FLD_DATEEND, Trunc(lDate));
            // if the string does not contain a date part (which has therefore
            // defaulted to Delphi's zero date, 1899-12-30), treat it as 'Unknown'
            Callback(Self, 0, FLD_DATETYPE,
                IfThen((lDate = 0) and (Pos('1899', FWorking) = 0), 'U', 'D'));
          end;
        end
        else Fail(ResStr_DateInFuture);
      end
      else begin
        // Not a system date, so test using vague dates
        if IsVagueDate(FWorking) then begin
          lVagueDate := StringToVagueDate(FWorking);
          if IsValidUnknownDate(FWorking, lVagueDate) then
          begin
            if (CompareDateToVagueDate(Now, lVagueDate) >= 0) then
            begin
              if Assigned(Callback) then
              begin
                Callback(Self, 0, FLD_DATESTART, Trunc(lVagueDate.StartDate));
                Callback(Self, 0, FLD_DATEEND, Trunc(lVagueDate.EndDate));
                Callback(Self, 0, FLD_DATETYPE, lVagueDate.DateTypeString);
              end;
            end
            else Fail(ResStr_DateInFuture);
          end
          else Fail(ResStr_DateNotValid);
        end
        else
          Fail(ResStr_DateNotValid);
      end;
    finally
      SysUtils.ShortDateFormat := lSystemShortDateFormat;
      SysUtils.DateSeparator   := lSystemDateSeparator;
    end; // try
  end;
end;  // TDeterminationDateParser.ParseField

{-------------------------------------------------------------------------------
}
procedure TDeterminationDateParser.SetDateSeparator(const Value: char);
begin
  FDateSeparator := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TDeterminationDateParser.SetShortDateFormat(const Value: String);
begin
  FShortDateFormat := Value;
end;  // TDeterminationDateParser.SetShortDateFormat

{-==============================================================================
    TSpatialRefParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSpatialRefParser.Create;
begin
  inherited;
  FFields.Add(TParsedField.Create(FLD_SPATIALREF, 'varchar(40)'));
  FFields.Add(TParsedField.Create(FLD_SYSTEM, 'varchar(4)'));
  FFields.Add(TParsedField.Create(FLD_LAT, 'float'));
  FFields.Add(TParsedField.Create(FLD_LONG, 'float'));
  FFields.Add(TParsedField.Create(FLD_QUALIFIER, 'varchar(20)'));
  // Creates the validation object to validate the spatial ref against the
  // chosen survey's bounding box
  FSurveyKey := AppSettings.IWSurveyKey;
  if FSurveyKey <> '' then begin
    FValidationData := TdmValidation.Create(nil);
    FValidationData.SetDatabase(dmDatabase.Connection);
  end;
end;  // TSpatialRefParser.Create

{-------------------------------------------------------------------------------
}
destructor TSpatialRefParser.Destroy;
begin
  inherited;
  FreeAndNil(FValidationData);
end;  // TSpatialRefParser.Destroy

{-------------------------------------------------------------------------------
}
procedure TSpatialRefParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
var
  system: String;
  SpatialRef: String;
  latLong: TLatLong;
  idx: Integer;
begin
  FFailed  := False;
  FWorking := Value;
  TrimWhitespace;
  if FWorking <> '' then begin
    try
      // Split Spatial System if '|' delimiter found (assumes it's a Spatial System...)
      idx := Pos('|', FWorking);
      if idx > 0 then begin
        system  := Trim(Copy(FWorking, idx + 1, Length(FWorking)));
        FWorking := Trim(Copy(FWorking, 1, idx - 1));
        SpatialRef := DelocaliseSpatialRef(FWorking);
        if system = '' then
          system := DetermineSpatialRefSystem(SpatialRef)
        else
        if not TestSpatialRefAgainstSystem(SpatialRef, system) then begin
          Fail(ResStr_SpatialRefInvalidForSystem);
          Exit;
        end;
      end else
      begin
        SpatialRef := DelocaliseSpatialRef(FWorking);
        system := DetermineSpatialRefSystem(SpatialRef);
      end;
      if system <> ResStr_SystemUnknown then
      begin
        // Validates the grid reference to make sure it is in the survey bounding box
        if (FSurveyKey <> '') then begin
          if not (FValidationData.CheckEventInSurvey(
              FSurveyKey,
              SpatialRef,
              system,
              '').Success = True) then
            Fail(ResStr_SpatialRefNotInBoundingBox);
        end;
        latLong := ConvertToLatLong(SpatialRef, system);
        if Assigned(Callback) then begin
          Callback(Self, 0, FLD_SPATIALREF, SpatialRef);
          Callback(Self, 0, FLD_SYSTEM, system);
          Callback(Self, 0, FLD_LAT, latLong.Lat);
          Callback(Self, 0, FLD_LONG, latLong.Long);
          Callback(Self, 0, FLD_QUALIFIER, ResStr_ImportedQualifier);
        end;
      end else
        Fail(ResStr_SpatialRefInvalid);
    except
      on E:Exception do
        Fail(ResStr_SpatialRefInvalid + #13#10 + E.Message);
    end;
  end else
  if Assigned(Callback) then
  begin
    Callback(Self, 0, FLD_SPATIALREF, '');
    Callback(Self, 0, FLD_SYSTEM, '');
    Callback(Self, 0, FLD_LAT, 0);
    Callback(Self, 0, FLD_LONG, 0);
    Callback(Self, 0, FLD_QUALIFIER, '');
  end;
end;  // TSpatialRefParser.ParseField

{-==============================================================================
    TRequiredSpatialRefParser
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TRequiredSpatialRefParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
begin
  if Trim(Value) = '' then
    Fail(ResStr_SpatialRefRequired)
  else
    inherited;
end;  // TRequiredSpatialRefParser.ParseField

{-==============================================================================
    TSpatialRefSystemParser
===============================================================================}
{-------------------------------------------------------------------------------
  Parses the given value as a combined spatial reference and spatial reference
  system (similar to the way TSpatialRefParser behaves).
}
procedure TSpatialRefSystemParser.ParseField(const Value: string;
  CallBack: TFieldParserEvent);
var
  lPosition: Integer;
  lSystem: string;
begin
  lPosition := Pos('|', Value);
  lSystem := MidStr(Value, lPosition + 1, Length(Value) - lPosition);
  inherited ParseField(lSystem, nil);
  if not Failed and (lPosition > 0) then
  begin
    ParseSpatialRef(Value);
  end;
  if not Failed and Assigned(CallBack) then
    CallBack(Self, 0, FLD_DATA, FWorking)
end;

{-------------------------------------------------------------------------------
  Uses a TSpatialRefParser to parse the given value, which combines a spatial
  reference value with a spatual
}
procedure TSpatialRefSystemParser.ParseSpatialRef(const Value: string);
var
  lParser: TSpatialRefParser;
begin
  lParser := TSpatialRefParser.Create;
  try
    lParser.ParseField(Value, nil);
    if lParser.Failed then Fail(lParser.LastErrorMessage);
  finally
    lParser.Free;
  end;
end;

{-==============================================================================
    TRequiredTextParser
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TRequiredTextParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
begin
  if Trim(Value) = '' then
    Fail(ResStr_ValueRequired)
  else
   inherited;
end;  // TRequiredTextParser.ParseField

{-==============================================================================
    TTaxonDataParser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TTaxonDataParser.Create;
begin
  inherited Create;
  FPosition := mTaxonDataParserPosition;
  FRestrictedValues := TStringList.Create;
  Inc(mTaxonDataParserPosition);

  // Fake the multiple record for taxon measurements. This is to force the
  // use of a #CT_ table (normally used for "real" multi-record fields).
  FSingleRecord := False;

  FFields.Add(TParsedField.Create(FLD_DATA, 'varchar(20)'));
  FFields.Add(TParsedField.Create(FLD_DATAQUALIFIER, 'char(16)'));
  FFields.Add(TParsedField.Create(FLD_DATAUNIT, 'char(16)'));
  FFields.Add(TParsedField.Create(FLD_ACCURACY, 'varchar(20)'));
end;  // TTaxonDataParser.Create

{-------------------------------------------------------------------------------
}
destructor TTaxonDataParser.Destroy;
begin
  FRestrictedValues.Free;
  inherited Destroy;
end;  // TTaxonDataParset.Destroy

{-------------------------------------------------------------------------------
}
procedure TTaxonDataParser.ParseField(const Value: string; CallBack: TFieldParserEvent);
var
  lNumber: Extended;
begin
  FWorking := Value;
  TrimWhitespace;
  FFailed := False;

  if FWorking = '' then Exit;

  if FWorking[1] = '~' then
    FWorking := MidStr(FWorking, 2, Length(FWorking) - 1);
  if FWorking[Length(FWorking)] = '?' then
    FWorking := FastStringFuncs.LeftStr(FWorking, Length(FWorking) - 1);

  if Length(FWorking) > MAX_LENGTH_DATA then
    Fail(Format(ResStr_ValueToLong, [Length(FWorking), MAX_LENGTH_DATA]))
  else
  if (FRestrictedValues.Count > 0) and (FRestrictedValues.IndexOf(FWorking) = -1) then
    Fail(Format(ResStr_ValueNotAllowed, [FFormattedRestrictedValues]))
  else
  if Assigned(Callback) then begin
    Callback(Self, Position, FLD_DATA, FWorking);
    Callback(Self, Position, FLD_DATAQUALIFIER, FQualifierKey);
    Callback(Self, Position, FLD_DATAUNIT, FUnitKey);
    // Check Value, not FWorking to decide "estimate" or "exact".
    if TryStrToFloat(Value, lNumber) then
      Callback(Self, Position, FLD_ACCURACY, ResStr_Exact)
    else
      Callback(Self, Position, FLD_ACCURACY, ResStr_Estimate);
  end;
end;  // TTaxonDataParser .ParseField

{-------------------------------------------------------------------------------
}
procedure TTaxonDataParser.SetUnitKey(const AValue: String);
begin
  FUnitKey := AValue;
  RefreshRestrictedValues;
end;  // TTaxonDataParset.SetUnitKey

{-------------------------------------------------------------------------------
}
procedure TTaxonDataParser.RefreshRestrictedValues;
begin
  FRestrictedValues.Clear;
  FFormattedRestrictedValues := '';
  with dmDatabase.GetRecordset(
      'usp_MeasurementUnitValue_Select_ForMeasurementUnit',
      ['@Key', FUnitKey]) do
  begin
    while not Eof do begin
      FRestrictedValues.Add(Fields['Data'].Value);
      FFormattedRestrictedValues := FFormattedRestrictedValues
          + #10'    '
          + Fields['Data'].Value;
      MoveNext;
    end;
    Close;
  end;
end;  // TTaxonDataParser.RefreshRestrictedValues

initialization
  RegisterClasses([TAbundanceDataParser,
                   TTextParser,
                   TObserverParser,
                   TDeterminerParser,
                   TAltitudeParser,
                   TMapMateKeyParser,
                   TBooleanParser,
                   TRecordIDParser,
                   TSiteIDParser,
                   TBRCSourceParser,
                   TViceCountyNumberParser,
                   TSampleVCParser,
                   TDateParser,
                   TNonRequiredDateParser,
                   TDeterminationDateParser,
                   TSpatialRefParser,
                   TRequiredTextParser,
                   TTaxonDataParser,
                   TRequiredSpatialRefParser,
                   TSpatialRefSystemParser]);
end.
