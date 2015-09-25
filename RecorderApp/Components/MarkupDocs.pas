unit MarkupDocs;

interface

uses
  Classes, SysUtils, ExceptionForm, FileOutput, Db, TaskProgress, Dataclasses,
  Forms, Relationships_ADO;

resourcestring
  { Some generic error messages }
  ResStr_CreateProblem = 'The structure could not be read for an ';
//  SPACES = '                                                                           ';

type
  EImportCantProceed = class(TExceptionPath);
  EInvalidZipFile = class(EImportCantProceed);

{ New exception class which allows an error to record the row the problem
      occurred on }
  TMarkupException = class(EImportCantProceed)
  private
    FRow : integer;
    FBlockPos : int64;
  public
    constructor Create(const Msg : string; const iRow : integer; const iBlockPos : int64); overload;
    constructor Create(const Msg : string; iException : Exception;
                                const iRow : integer; const iBlockPos : int64); overload;
    constructor CreateNonCritical(const Msg : string;
                                const iRow : integer; const iBlockPos : int64); overload;
    constructor CreateNonCritical(const Msg : string; iException : Exception;
                                const iRow : integer; const iBlockPos : int64); overload;
    { Now without the Row parameter so we can default it to -1 }
    constructor Create(const Msg : string); overload;
    constructor Create(const Msg : string; iException : Exception); overload;
    constructor CreateNonCritical(const Msg : string); overload;
    constructor CreateNonCritical(const Msg : string;
                                        iException : Exception); overload;
    property Row : integer read FRow;
    property BlockPos : Int64 read FBlockPos;
  end;

  { Forward declaration }
  TMarkupDocument = class;

  { Abstract base class for classes which can read/write a 'chunk' from the
       XML file into their own properties }

  EDocItemError = class(TMarkupException);

  TAbstractDocItem = class(TObject)
  private
    FDocItems : TList;
    FParent : TAbstractDocItem;
    function GetDocItem(const iIndex : integer): TAbstractDocItem;
    function GetCloseString : string;
  protected
    FName : string;
    FDocument : TMarkupDocument;
    FCloseChar : char;
  public
    constructor CreateFromStrings( iParent : TAbstractDocItem;
                iDocument : TMarkupDocument );
    procedure AddDocItem( iDocItem : TAbstractDocItem );
    property Document : TMarkupDocument read FDocument;
    property DocItems[ const iIndex : integer ] : TAbstractDocItem read GetDocItem;
    property ExpectedCloseString : String read GetCloseString;
    property Parent : TAbstractDocItem read FParent;
    property Name : string read FName;
  end;


  { Parser directives class - in XML they start <! }

  EDirectiveError = class(EDocItemError);

  TDirective = class(TAbstractDocItem)
  protected
    procedure ConfirmType( const iName : string; var ioRow, ioPos : integer );
  public
    { Class function - a sort of pseudo constructor }
    class function CreateDirective( iParent : TAbstractDocItem;
          iDocument : TMarkupDocument; var ioRow, ioPos : integer ): TDirective;
  end;



  { Things which start <? }
  EProcessInstructionError = class(TMarkupException);

  TProcessInstruction = class(TAbstractDocItem)
  public
    constructor CreateFromStrings( iParent : TAbstractDocItem;
                iDocument : TMarkupDocument; var ioRow, ioPos : integer );
  end;


  { TDocParser
    Class to assist the parsing of a document into constituent tokens.
    The TDocParser class provides a generic utility for parsing tokens from
    string lists.  It is used throughout the XML parsing class hierarchy to
    assist in location of XML tokens within the file.  When created it is passed
    a string list.  Each parse operation takes a current position and row from
    which to perform the operation. }

  EDocParserError = class(TMarkupException);
  EEOFError = class(EDocParserError);

  { Supported search types when parsing documents }
  TSearchType = (stNonSpace, stEndWord, stFindWords);

  { Generic search method type so we can implement lots of different ways of
       searching }
  TSearchMethod = function ( const iText : string ): integer of object;

  TDocParser = class(TObject)
  private
    FSearchWords : array of string;
    FDocStrings: TStrings;
    { Search methods }
    function FindNonSpaceInLine( const iText : string ): integer;
    function FindWordInLine(const iText: string): integer;
    function FindEndWordInLine(const iText : string): integer;
    procedure DoSearch( iSearchMethod : TSearchMethod;
                                 var ioRow, ioPos : integer);
    procedure SetDocStrings(const Value: TStrings);
  public
    constructor Create(iDocStrings : TStrings);
    procedure SearchFor( iSearchType : TSearchType; var ioRow, ioPos: integer); overload;
    procedure SearchFor( iSearchType : TSearchType; const iWords : array of string;
                          var ioRow, ioPos: integer); overload;
    function GetNextWord( const iRow, iPos : integer ) : string; virtual;
    function GetChar( const iRow, iPos : integer) : char; virtual;
    function GetString( const iRow, iPos, iLength : integer) : string;
    function GetAllUpTo(const iText : string; var ioRow, ioPos : integer): string;
    function StripSpacesFromEnd( const iText : string ): string;
    function IsPartOfWord( const iChar : char ) : boolean;
    property DocStrings : TStrings read FDocStrings write SetDocStrings;
  end;


  { TMarkupStack
    Class to hold a stack of nested markups as we progress through a document.
    For example, much of a document is held inside the HEAD markup, which is
    stored on the stack. }

  EStackError = class(TExceptionPath);

  TMarkupStack = class
  private
    FItems : TList;
    function PeekItem(const iIndex: integer): TAbstractDocItem;
    function GetSize: integer;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push( iItem : TAbstractDocItem );
    function Pop: TAbstractDocItem;
    function PeekTop: TAbstractDocItem;
    procedure Clear;
    property Items[ const iIndex : integer ] : TAbstractDocItem read PeekItem;
    property Size: integer read GetSize;
  end;


  { TDocument
    The TDocument class provides a base class for managing entire text based
    document files, in this instance either DTD or XML files.  It manages the
    interaction between the document itself, the document parser and the
    document outputted (for data export). }

  EDocumentError = class(TMarkupException);

  TDocument = class(TObject)
  private
    FFileName : string;
    FOutputter : TFileOutput;
    {Store lines processed for progress bar tracking}
    FProcessedLines: integer;
    FTotalLines: integer;
    FProgressBar : TTaskProgressBar;
    function GetFileStrings: TStringList;
    procedure SetProcessedLines(const Value: integer);
    function GetEmbeddedFileCount: integer;
  protected
    FDocParser: TDocParser;
    FFileList : TList; // list of documents.  Normally one, but embedded docs go here as well
    FEmbeddedStrings : TStringList;
    procedure CreateDocParser; virtual;
    procedure LoadFile(iFileStrings : TStrings;
                                    const FFileName : string); virtual;
  public
    constructor Create( iFileName : string; iOutputter : TFileOutput;
                iProgressBar : TTaskProgressBar );
    destructor Destroy; override;
    procedure EmbedDocument( iDocument : TStrings ); virtual;
    procedure FinishEmbeddedDoc; virtual;
    property DocParser : TDocParser read FDocParser;
    property Outputter : TFileOutput read FOutputter;
    property FileStrings : TStringList read GetFileStrings;
    property FileName : string read FFileName;
    property EmbeddedFileCount : integer read GetEmbeddedFileCount;
    property ProcessedLines : integer read FProcessedLines write SetProcessedLines;
    property TotalLines : integer read FTotalLines;
  end;


  { TTag - for input documents, stores information regarding a single <x></x>
     pair.  Not directives or processing instructions though (ie things which
     don't start <!). }

  ETagError = class(EDocumentError);

  TTagInfo = set of (tiLocalKey, tiHasSpecifier);

  TTag = class(TAbstractDocItem)
  private
    FText : string;
    FStream : TMemoryStream;
    FAttributeData : TStringList;
    FNestedTags : TList;
    FTagInfo : TTagInfo;
    FDocBlockPos : Int64; // so we can report errors correctly - bytes from start of file stream
    FDocRow : integer; // rows from start of doc block
    FSpecifier : string; // if a specifier present
    FHasSpecifier : boolean;
    FIsContentTag : boolean;
    FInvalidationText : string;
    FIsInvalid : boolean;
  protected
    procedure ReadAttributeData( var ioRow, ioPos : integer );
    procedure CheckTagContent;
    procedure CheckAttributeContent;
    procedure HandleNoElement( var ioRow, ioPos : integer );
    procedure CheckSpecialAttributes(const iAttributeString: string);
    procedure CheckIsContentTag(iParent: TAbstractDocItem);
  public
    constructor CreateFromStrings( iParent : TAbstractDocItem;
                iDocument : TMarkupDocument; var ioRow, ioPos : integer );
    constructor CreateDummySurveyKey( iParent : TAbstractDocItem;
                iDocument : TMarkupDocument; iKey : TKeyString );
    destructor Destroy; override;
    procedure ReadText( var ioRow, ioPos : integer );
    procedure Close;
    procedure AddChild( iTag : TTag );
    function CountRepetitions( const iTagName : string;
                               const iStartPos : integer ): integer;
    procedure InvalidateTag(const iMessage : string);
    procedure LoadStream( var ioRow, ioPos: integer );
    procedure DropChildTag( iTagToDrop : TTag );
    property Text : string read FText;
    property Stream : TMemoryStream read FStream;
    property AttributeData : TStringList read FAttributeData;
    property NestedTags : TList read FNestedTags;
    property TagInfo : TTagInfo read FTagInfo;
    property DocBlockPos : int64 read FDocBlockPos;
    property DocRow : integer read FDocRow;
    property Specifier : string read FSpecifier;
    property HasSpecifier : boolean read FHasSpecifier;
    property IsContentTag : boolean read FIsContentTag;
    property IsInvalid : boolean read FIsInvalid;
    property InvalidationText : string read FInvalidationText;
  end;


  { TMarkupDocument
    Derivative of TDocument which builds a document hierarchy of markups.  Can
    be used for any markup language parsing. }
  TMarkupDocument = class(TDocument)
  private
    { Whilst reading a Document }
    FMarkupStack : TMarkupStack;
    { Whilst outputting a document }
    FElementStack : TStringList;
    FFirstText : boolean;
    function GetDocItemCount: integer;
  protected
    FLastTagProcessed : TTag;
    FDocItems : TStringList;
    FTagCount : integer;
    FContentTagCount : integer;
    FRow, FPos : integer; // position in doc as we read it
    FParentDocRow, FParentDocPos : integer; // parent document pos when embedding docs
    function CreateDocItem( var ioRow, ioPos : integer ):
                                     TAbstractDocItem; virtual;
    procedure SpecialProcessing( var ioLine, ioPos ); virtual;
    function GetDocItem(const iIndex : integer): TAbstractDocItem;
    procedure ProcessStartLTBracket( var ioRow, ioPos : integer );
    procedure ProcessEndSQBracket( var ioRow, ioPos : integer );
    procedure ProcessEndGTBracket( var ioRow, ioPos : integer );
    procedure ProcessEndTag( var ioRow, ioPos : integer );
  public
    constructor Create( iFileName : string; iOutputter : TFileOutput;
                iProgressBar : TTaskProgressBar );
    function ProcessDocument( iMustBeXML : boolean ) : TTag; virtual;
    procedure CheckStack;
    destructor Destroy; override;
    procedure EmbedDocument( iDocument : TStrings ); override;
    procedure FinishEmbeddedDoc; override;
    procedure FreeTagTree( iTag : TTag );
    property DocItems[ const iIndex : integer ] : TAbstractDocItem read GetDocItem;
    property DocItemCount : integer read GetDocItemCount;
    property MarkupStack : TMarkupStack read FMarkupStack;
    property ElementStack : TStringList read FElementStack; // should be a proper stack
    property TagCount : integer read FTagCount;
    property ContentTagCount : integer read FContentTagCount;
  end;



implementation

uses
  JNCCXMLDoc, XMLData, XMLTypes, DataImport;

const
  NOT_FOUND = -1;
  VALID_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', ':', '.'];

resourcestring
  { Error messages }
  { End of file hit by Doc Parser }
  ResStr_EndOfFile            = 'The end of the file has been encountered.';
  ResStr_EOFInCompare         = 'EOF occurred whilst comparing word ';
  ResStr_NeedWord             = 'A word is required before word searching can be performed.';
  ResStr_DirTypeUnknown       = 'The directive type is unkown : ';
  ResStr_StackProblem         = 'The stack is not correctly cleared.  Items remaining : ';
  ResStr_BadSQBracket         = 'A ] character has been encountered incorrectly.';
  ResStr_BadGTBracket         = 'A > character has been encountered incorrectly.';
  ResStr_StackEmpty           = 'Trying to examine the markup stack which is empty';
  ResStr_NoClose              = 'Could not find the section closing text : ';
  ResStr_WhitespaceInDirective= 'A directive has whitepace between the <! and directive name.';
  ResStr_NoDocument           = 'No documents are available in the document processor';
  ResStr_FileEmbedProblem     = 'A problem with embedded files has been encountered';
  ResStr_TagParent            = 'A markup has been located nested inside a directive';
  ResStr_BadEndTag            = 'An end markup has been located which is not nested correctly with the open markup';
  ResStr_AttDefWrong          = 'The definition of an attribute is wrong inside the markup';
  ResStr_AttRequired          = 'An attribute is required for the markup ';
  ResStr_AttValueWrong        = 'The attribute value is not allowed for ';
  ResStr_TagProblem           = 'Problem occurred whilst reading information for a markup';
  ResStr_EmptyContent         = 'The content model for the tag is empty but data was found :';
  ResStr_MarkupClose          = 'A problem occurred whilst closing the markup';
  ResStr_IncorrectlyTerminated= 'An item is not correctly terminated. Expected : ';
  ResStr_NotXML               = 'The file is not an XML file and cannot be imported.';

  ResStr_NotDefined           = 'A markup has been encountered which is not defined in the NBN Docment Type Definition : ';
  ResStr_SearchNotImplemented = 'Search method not implemented.';
  ResStr_OperationCancelled   = 'Operation cancelled.';
  ResStr_DocParNotRecognised  = '%s not recognised, probable nesting failure.';


  //==============================================================================
  { TAbstractDocItem }
//==============================================================================


{ Constructor - doesn't really do anything except set up the FDocument + FParent
     items. }
constructor TAbstractDocItem.CreateFromStrings(iParent : TAbstractDocItem;
            iDocument : TMarkupDocument );
begin
  FDocument := iDocument;
  FParent := iParent;
  inherited Create;
end;


{ GetDocItem.  Accessor method for the FDocItems list - returns a typecasted
     object from the TList }
function TAbstractDocItem.GetDocItem(const iIndex : integer): TAbstractDocItem;
begin
  Result := TAbstractDocItem(FDocItems[iIndex]);
end;


{GetCloseString - reads the expected close string for the tag }
function TAbstractDocItem.GetCloseString : string;
begin
  if FCloseChar = '>' then
    Result := '>'
  else if FCloseChar = ']' then
    Result := ']'
  else
    Result := '</' + FName;
end;


{ Add a document item to the internal list }
procedure TAbstractDocItem.AddDocItem(iDocItem: TAbstractDocItem);
begin
  FDocItems.Add( iDocitem );
end;



//==============================================================================
{ TDocParser }
//==============================================================================

{ Constructor - just store a string list reference for searching later }
constructor TDocParser.Create(iDocStrings : TStrings);
begin
  inherited Create;
  FDocStrings := iDocStrings;
end;


{ Overloaded search method - this version does not search for words, only
     according to fixed criteria }
procedure TDocParser.SearchFor(iSearchType: TSearchType; var ioRow,
  ioPos: integer);
begin
  case iSearchType of
    stNonSpace :
      DoSearch( FindNonSpaceInLine, ioRow, ioPos );
    stEndWord :
      DoSearch( FindEndWordInLine, ioRow, ioPos );
    stFindWords :
      raise EDocParserError.Create(ResStr_NeedWord, ioRow, -1);
  else
    raise EDocParserError.Create(ResStr_SearchNotImplemented);
  end; // case
end;


{ Overloaded search method - this version only searches for words, not
     according to fixed criteria }
procedure TDocParser.SearchFor(iSearchType: TSearchType;
  const iWords: array of string; var ioRow, ioPos: integer);
var
  i : integer;
begin
  { If wrong search type, transfer responsibility }
  if iSearchType = stFindWords then
  begin
    { Store the words we are looking for }
    SetLength(FSearchWords, high(iWords) + 1);
    for i := 0 to High(iWords) do
      FSearchWords[i] := iWords[i];
    { Then do the search }
    DoSearch( FindWordInline, ioRow, ioPos );
  end
  else
    SearchFor( iSearchType, ioRow, ioPos );
end;



{ Locate the first non-white space character in the strings }
function TDocParser.FindNonSpaceInLine(const iText: string): integer;
var
  lPos: integer;
begin
  Result := NOT_FOUND; // default is not found
  if Length(iText) > 0 then
  begin
    lPos := 1;
    { May need to check for other whitespace - currently ' ' or tab }
    while (iText[lPos]=' ') or (iText[lpos]=#9) do
    begin
      Inc(lPos);
      if lPos>Length(iText) then
        break; // from while loop to avoid range error
    end;
    { If i shoots off end of string, then space not found.  In which case, the
         null at the end of the string has finished our while loop }
    if lPos<=Length(iText) then
      { Note minus 1 - no need to move if we are already at a non-space }
      Result := lPos-1;
  end; // if length
end;


{ Locate the first instance of any of the current FSearchWords in the text }
function TDocParser.FindWordInLine(const iText: string): integer;
var
  i : integer;
  lCurrentPos : integer;
  lFirstFound : integer; // position of first word match
begin
  lFirstFound := NOT_FOUND;
  for i := 0 to High(FSearchWords) do
  begin
    lCurrentPos := Pos( FSearchWords[i], iText );
    if lCurrentPos <> 0 then
      if    (lCurrentPos < lFirstFound) or
            (lFirstFound = NOT_FOUND) then
        lFirstFound := lCurrentPos-1;
  end;
  Result := lFirstFound;
end;


{ Search method to locate the end of a word.  Cannot return NOT_FOUND because
     the end is always there! }
function TDocParser.FindEndWordInLine(const iText: string): integer;
var
  lPos : integer;
  ltfFinished : boolean;
begin
  lPos := 0;
  ltfFinished := False;
  { Loop along text until we go off the end of the word }
  repeat
    Inc(lPos);
    if lPos <= Length(iText) then // still inside total string
    begin
      if not IsPartOfWord(iText[lPos]) then // but off end of word
      begin
        ltfFinished := True;
        Dec(lPos);
      end;
    end else
    begin // off end of string
      ltfFinished := True;
      lPos := Length(iText);
    end;
  until ltfFinished;
  Result := lPos;
end;


{ Returns true if a character is alphanumeric or underscore.  Uses all allowed
     element name characters for the xml definition.  Used both internally to
     determine words, and available publically to all users of the DocParser. }
function TDocParser.IsPartOfWord(const iChar: char): boolean;
begin
  if iChar in VALID_CHARS then
    Result := True
  else
    Result := False;
end;


{ DoSearch - method to do generic searching using a supplied method type.
     Search through the string list using the supplied search method.  If
     non are found, an exception is raised.  If found, the iRow and iCharPos
     parameters are moved accordingly }
procedure TDocParser.DoSearch( iSearchMethod : TSearchMethod;
                                 var ioRow, ioPos : integer);
var
  lCharsToMoveBy : integer;
begin
  lCharsToMoveBy := NOT_FOUND; // default
  while (lCharsToMoveBy = NOT_FOUND) and (ioRow < FDocStrings.Count) do
  begin
    lCharsToMoveBy := iSearchMethod( Copy(FDocStrings[ioRow], ioPos,
                      High(Integer)) );
    if lCharsToMoveBy = NOT_FOUND then
    begin
      { Move to start of next line }
      Inc(ioRow);
      ioPos := 1;
    end;
  end;
  if ioRow >= FDocStrings.Count then
    { Not found - everything left in the string list is a character }
    raise EEOFError.Create(ResStr_EndOfFile, ioRow, -1);
  { Advance to the found character }
  ioPos := ioPos + lCharsToMoveBy;
end;


{ locate the next word, and advance the pointers to the character afterwards }
function TDocParser.GetNextWord(const iRow, iPos: integer): string;
var
  lTempPos, lNameStartPos,  lTempRow : integer;
begin
  lTempPos := iPos;
  lTempRow := iRow;
  SearchFor( stNonSpace, lTempRow, lTempPos ); // the start of the word
  lNameStartPos := lTempPos;
  SearchFor( stEndWord, lTempRow, lTempPos ); // the end of the word
  if lTempPos > 0 then // not back at start of next line
    Result := Copy(FDocStrings[lTempRow], lNameStartPos,
                                                   (lTempPos - lNameStartPos))
  else
    { take the rest of the line }
    Result := Copy(FDocStrings[lTempRow], iPos + 2, High(Integer));
end;


{ Return a single character from the strings.  Great for case statements.  If
     off the end of the string, then return value is #0 }
function TDocParser.GetChar(const iRow, iPos: integer): char;
var
  lCurrentLine : string;
begin
  lCurrentLine := FDocStrings[iRow];
  if iPos<=Length(lCurrentLine) then
    Result := lCurrentLine[iPos]
  else
    Result := #0;
end;


{ Read a set number of characters from the text file.  Has to do it 1 character
    at a time to ensure that any special processing occurs in the virtual
    GetChar method ( eg entities).  If the string goes off the end of a line,
    it is truncated }
function TDocParser.GetString(const iRow, iPos, iLength: integer): string;
var
  i : integer;
  lPos : integer;
begin
  Result := '';
  lPos := iPos; // need a variable
  for i := 1 to iLength do
  begin
    if lPos > Length( FDocStrings[iRow] ) then
      Break; // from loop - end of line
    Result := Result + GetChar( iRow, lPos );
    Inc(lPos);
  end; // for
end;



{ Removes all whitespace from the end of a string }
function TDocParser.StripSpacesFromEnd(const iText: string): string;
var
  i : integer;
begin
  i := Length(iText);
  while i > 0 do
  begin
    if ((iText[i] <> ' ') and (iText[i] <> #9)) then
      break; // from while loop
    Dec(i);
  end;
  { lop any whitespace characters off }
  Result := Copy(iText, 1, i);
end;



{ Locate the text provided, and return all characters before that text from the
     starting point.  Concatenates all the required stuff from the string list
     into a single string.  Useful for locating end -markers, eg closing quotes
     or > in HTML.  Moves the position to the character after the located text }
function TDocParser.GetAllUpTo(const iText: string; var ioRow,
  ioPos: integer): string;
var
  lStartPos, lStartRow : integer;
  i : integer;
  lRowPosStart, lRowPosEnd : integer;
begin
  { Record where we are now }
  lStartRow := ioRow;
  lStartPos := ioPos;
  try
    { Locate the word }
    SearchFor( stFindWords, [iText], ioRow, ioPos );
  except
    on E:EEOFError do
      raise EDocParserError.Create(ResStr_NoClose + iText, E);
  end;
  { Loop through each line and build the appropriate results string }
  Result := '';
  for i := lStartRow to ioRow do
  begin
    { First and last row - need to account for partial lines }
    if i = lStartRow then
      lRowPosStart := lStartPos // from starting point only
    else
      lRowPosStart := 1;  // from start of line
    if i = ioRow then
      lRowPosEnd := ioPos // to finishing point only
    else
      lRowPosEnd := High(integer); // all of rest of line
    Result := Result + Copy(FDocStrings[i], lRowPosStart,
                       lRowPosEnd - lRowPosStart);
  end;
  { Advance past the search string }
  Inc( ioPos, Length(iText) );
end;


{ Accessor method allows the DocParser to attach to a new document, for example
     when embedding documents }
procedure TDocParser.SetDocStrings(const Value: TStrings);
begin
  FDocStrings := Value;
end;


//==============================================================================
{ TDocument }
//==============================================================================


{ TDocument constructor - simply links the instance to the file which
     specifies it }
constructor TDocument.Create( iFileName : string; iOutputter : TFileOutput;
                iProgressBar : TTaskProgressBar );
var
  lFileStrings : TStringList;
begin
  inherited Create;
  { Record the progress bar }
  FProgressBar := iProgressBar;
  FFileName := iFileName;
  { Create a list to hold the main file and eny embedded ones }
  FFileList := TList.Create;
  { Read the file into a string list }
  lFileStrings := TStringlist.Create;
  LoadFile(lFileStrings, FFileName);
  FTotalLines := lFileStrings.Count;
  { Keep this stringlist on a list }
  FFileList.Add(lFileStrings);
  { Prepare a document parser instance to help us work our way through the file }
  CreateDocParser;
  { And something to output the file }
  FOutputter := iOutputter;
end;



{ Destructor - just cleanup }
destructor TDocument.Destroy;
var
  i : integer;
begin
  { Ensure the main file, and any embedded ones, are free }
  if FFileList <> nil then
    for i := 0 to FFileList.Count-1 do
      TStringList(FFileList[i]).Free;
  { Free the file list }
  FFileList.Free;
  { and the parser }
  FDocParser.Free;
  inherited Destroy;
end;



{ This method is provided just to allow descendant classes to override and
     create their own docparsers.  Simply instantiates the DocParser }
procedure TDocument.CreateDocParser;
begin
  FDocParser := TDocParser.Create( Self.FileStrings );
end;



{ Embed a document.  When an external reference is located, it is embedded which
     means that the document processor will start to process the nested document
     - this continues until the FinishEmbeddedDocument is called }
procedure TDocument.EmbedDocument(iDocument: TStrings);
var
  lStringList : TStringList;
begin
  { Create a string list to assign the strings to }
  lStringList := TStringList.Create;
  lStringList.Assign(iDocument);
  { And pop it onto the list }
  FFileList.Add(lStringList);
  { Set the docparser to the new document }
  FDocParser.DocStrings := iDocument;
  { Record the extra work to do so the progress bar can remain correct }
  FTotalLines := FTotallines + iDocument.Count;
end;


{ Remove the most recently embedded document from the list of embedded documents.
     This means we are reverting to continue processing the file which contains
     the current embedded document }
procedure TDocument.FinishEmbeddedDoc;
begin
  if FFileList.Count < 1 then
    raise EDocumentError.Create(ResStr_FileEmbedProblem);
  { Don't forget all the hard work we have put in! (for the progress bar) }
  ProcessedLines := ProcessedLines + TStringList(FFileList[FFileList.Count-1]).Count;
  { Free the top item }
  TStringList(FFileList[FFileList.Count-1]).Free;
  { And remove it from the list }
  FFileList.Delete(FFileList.Count-1);
  { Reset the docparser to use the previous file }
  FDocParser.DocStrings := FFileList[FFileList.Count-1];
end;


{ Accessor method allows external users to check the number of embedded docs }
function TDocument.GetEmbeddedFileCount: integer;
begin
  Result := FFileList.Count;
end;


{ Accessor method for GetFileStrings.  Returns the current file in use - this
     could be embedded if necessary }
function TDocument.GetFileStrings: TStringList;
begin
  if FFileList.Count > 0 then
    Result := TStringList(FFileList[FFileList.Count-1])
  else
    raise EDocumentError.Create(ResStr_NoDocument);
end;



{ Provide a virtual means of loading the file string list so derived classes
     can change this }
procedure TDocument.LoadFile(iFileStrings: TStrings;
  const FFileName: string);
begin
  iFileStrings.LoadFromFile( FFileName );
end;


{ Accessor method, allows the recording of lines processed which are not in the
    current embedded document so we can set the progress bar correctly }
procedure TDocument.SetProcessedLines(const Value: integer);
begin
  FProcessedLines := Value;
end;



//==============================================================================
{ TMarkupDocument }
//==============================================================================



{ Constructor - parses the loaded file into constituent tag hierarchy }
constructor TMarkupDocument.Create( iFileName : string; iOutputter : TFileOutput;
                iProgressBar : TTaskProgressBar );
begin
  inherited Create(iFileName, iOutputter, iProgressBar);
  { Create an object to hold markups which we are nested inside as we process
       the document }
  FMarkupStack := TMarkupStack.Create;
  { Hold the elements which we have already opened whilst outputting }
  FElementStack := TStringList.Create;
  FDocItems := TStringList.Create;
  FDocItems.Sorted := True;
  { This is really important - we must allow more than one of each tag! }
  FDocItems.Duplicates := dupAccept;
  FTagCount := 0; // keep running total of Tags in the document
  FFirstText := true; // so we know first tag must be xml declaration
  FRow := 0; // position in document as it is read
  FPos := 1;
end;



{ Returns a newly instantiated document item.  Moves the position markers to the
     character after the end of the item. }
function TMarkupDocument.CreateDocItem(var ioRow,
  ioPos: integer): TAbstractDocItem;
begin
  Result := TAbstractDocItem.Create;
end;


{ Destructor - cleanup lists }
destructor TMarkupDocument.Destroy;
var
  i : integer;
begin
  FMarkupStack.Free;
  if FDocItems <> nil then
    for i := 0 to FDocItems.Count-1 do
      TAbstractDocItem(FDocItems.Objects[i]).Free;
  FDocItems.Free;
  FElementStack.Free;
  inherited Destroy;
end;


{ Override the EmbedDocument so we can record our previous document position }
procedure TMarkupDocument.EmbedDocument(iDocument: TStrings);
begin
  inherited;
  FParentDocRow := FRow;
  FParentDocPos := FPos;
  FRow := 0;
  FPos := 1;
end;

{ Override this to reset the previous document position }
procedure TMarkupDocument.FinishEmbeddedDoc;
begin
  inherited;
  FRow := FParentDocRow;
  FPos := FParentDocPos;
  FParentDocRow := 0;
  FParentDocPos := 0;
end;


{ Procedure to free a tree structure of tag objects.  Used to ensure we cleanup
     blocks of data (at the survey_event level) as we process them, otherwise
     we end up with memory problems when importing large datasets.  Recursive! }
procedure TMarkupDocument.FreeTagTree(iTag: TTag);
var
  lIndex : integer;
begin
  while iTag.NestedTags.Count > 0 do
    FreeTagTree(TTag(iTag.NestedTags[0]));
  if iTag.Parent <> nil then
    if iTag.Parent is TTag then
      TTag(iTag.Parent).DropChildTag(iTag);
  lIndex := FDocItems.IndexOfObject(iTag);
  if lIndex <> -1 then
    FDocItems.Delete(lIndex);
  { Don't free tags that have been labelled as invalid - we need to show these later }
  if iTag.InvalidationText='' then
    iTag.Free;
end;


{ Actually perform the processing of the document into constituent tags/elements
     etc.  Can be called to process an embedded document. }
function TMarkupDocument.ProcessDocument( iMustBeXML : boolean ) : TTag;
var
  ltfFinished : boolean;
  lTextFragment : string;
begin
  ltfFinished := False;
  FLastTagProcessed := nil;
  repeat
    try
      { Set the progress bar }
      if FProgressBar <> nil then
        FProgressBar.TaskPosition := ( (100 * (FRow + ProcessedLines)) div TotalLines );
      { Allow escape key to be registered }
      Application.ProcessMessages;
      { Allow derived classes to do anything unusual }
      SpecialProcessing( FRow, FPos );
      DocParser.SearchFor( stNonSpace, FRow, FPos );
        { For first text, must be xml statement }
      if FFirstText and iMustBeXML then
      begin
        lTextFragment := DocParser.GetString(FRow, FPos, 5);
        if lTextFragment <> '<?xml' then
          raise TMarkupException.CreateNonCritical(ResStr_NotXML);
        FFirstText := False;
      end;
      if (DocParser.GetString(FRow, FPos, 9) = '<![CDATA[') and
                                    (FMarkupStack.PeekTop is TTag) then
        TTag(FMarkupStack.PeekTop).LoadStream( FRow, FPos )
      else
      begin
        case DocParser.GetChar(FRow, FPos) of
          '<' : ProcessStartLTBracket( FRow, FPos );
          ']' : ProcessEndSqBracket( FRow, FPos );
          '>' : ProcessEndGTBracket( FRow, FPos );
        else if MarkupStack.PeekTop <> nil then
          if MarkupStack.PeekTop is TTag then
            TTag(MarkupStack.PeekTop).ReadText(FRow, FPos)
          else
            Raise EDocumentError.Create(Format(ResStr_DocParNotRecognised, [DocParser.GetString(FRow, FPos, 9)]), FRow, -1);
        end; // case
      end;
    except
      on EEOFError do
        ltfFinished := True;
    end; // try
    Inc(FPos);
    if FLastTagProcessed <> nil then
      if FLastTagProcessed.Name='survey_event' then
      begin
        ltfFinished := true;
        LoadFile(DocParser.DocStrings, FileName);
        FTotalLines := FTotalLines + DocParser.DocStrings.Count;
      end;
  until ltfFinished;
  { Return the tag we most recently closed }
  Result := FLastTagProcessed;
end;


{ When finished processing need to confirm stack is finished.  Otherwise, raise
   an exception as the document can't be well formed }
procedure TMarkupDocument.CheckStack;
begin
  if FMarkupStack.Size <> 0 then
    raise EDocumentError.Create(ResStr_StackProblem + IntToStr(FMarkupStack.Size),
                                                  FileStrings.Count-1, -1);
end;


{ Accessor method for doc items }
function TMarkupDocument.GetDocItem(
  const iIndex: integer): TAbstractDocItem;
begin
  Result := TAbstractDocItem(FDocItems.Objects[iIndex]);
end;




{ Accessor method to get a count of doc items }
function TMarkupDocument.GetDocItemCount: integer;
begin
  if FDocItems = nil then // must be a problem even before we started!
    Result := 0
  else
    Result := FDocitems.Count;
end;



{ Handle stuff when a > symbol is encountered }
procedure TMarkupDocument.ProcessEndGTBracket(var ioRow, ioPos: integer);
var
  lClosingDocItem : TAbstractDocItem;
begin
  lClosingDocItem := MarkupStack.Peektop;
  if lClosingDocItem.ExpectedCloseString <> '>' then
    raise EDocumentError.Create(ResStr_BadGTBracket, ioRow, -1);
  if lClosingDocItem is TDocType then
    TDocType(lClosingDocItem).ProcessExternalDoc;
  Markupstack.Pop;
end;


{ Handle stuff when a '<' symbol is encountered }
procedure TMarkupDocument.ProcessStartLTBracket(var ioRow, ioPos: integer);
var
  lCurrentText : string;
  lDocItem : TAbstractDocItem;
begin
  lCurrentText := DocParser.GetString(ioRow, ioPos, 2); // maximum text we will need
  if lCurrentText = '</' then // end tag
    ProcessEndTag( ioRow, ioPos )
  else
  begin // need a new DocItem
    lDocItem := nil;
    if lCurrentText = '<!' then
      lDocItem := TDirective.CreateDirective
                                (FMarkupStack.PeekTop, Self, ioRow, ioPos)
    else if lCurrentText = '<?' then  // processor instruction
      lDocItem := TProcessInstruction.CreateFromStrings
                     (FMarkupStack.PeekTop, Self, ioRow, ioPos)
    else if lCurrentText[1] = '<' then // start tag
    begin
      lDocItem := TTag.CreateFromStrings
                         (FMarkupStack.PeekTop, Self, ioRow, ioPos);
      { Update our running totals }
      Inc(FTagCount);
      if TTag(lDocItem).IsContentTag then
        Inc(FContentTagCount);
    end;
    { Add the object to a string list with a unique reference so we can locate
         it }
    FDocItems.AddObject( lDocItem.ClassName + '=' +
                                   lDocItem.Name, lDocItem );
  end;
end;


{ Handle stuff when a ] symbol is encountered }
procedure TMarkupDocument.ProcessEndSQBracket(var ioRow, ioPos: integer);
var
  lClosingDocItem : TAbstractDocItem;
begin
  lClosingDocItem := MarkupStack.PeekTop;
  if lClosingDocItem.ExpectedCloseString <> ']' then
    raise EDocumentError.Create(ResStr_BadSQBracket, ioRow, -1);
  if lClosingDocItem is TDocType then
  begin
    DocParser.SearchFor(stFindWords, ['>'], ioRow, ioPos);
    TDocType(lClosingDocItem).ProcessExternalDoc;
  end;
  MarkupStack.Pop;
end;



procedure TMarkupDocument.ProcessEndTag(var ioRow, ioPos: integer);
var
  lClosingDocItem : TAbstractDocItem;
begin
  lClosingDocItem := MarkupStack.Pop;
  if lClosingDocItem.ExpectedCloseString + '>' <> Copy(FileStrings[ioRow], ioPos,
                     Length(lClosingDocItem.ExpectedCloseString) + 1) then
    raise EDocumentError.Create(ResStr_BadEndTag, ioRow, -1);
  { Skip to end of end tag </end> }
  if lClosingDocItem is TTag then
  begin
    DocParser.SearchFor(stFindWords, ['>'], ioRow, ioPos);
    try
      TTag(lClosingDocItem).Close;
    except
      on E:Exception do
        raise EDocumentError.Create(ResStr_MarkupClose, E, ioRow, -1);
    end; // try..except
  end;
  FLastTagProcessed :=  TTag(lClosingDocItem);
end;



{ Virtual SpecialProcessing method.  Allows things like comments to be catered
     for - in this class the method is empty but not abstract as it is perfectly
     valid not to do any special processing }
procedure TMarkupDocument.SpecialProcessing(var ioLine; var ioPos);
begin
  { Really empty }
end;



//==============================================================================
{ TDirective }
//==============================================================================

{ Class function CreateDirective.  Acts like a constructor, but creates
     instances of the derived classes according to the document contents }
class function TDirective.CreateDirective(iParent : TAbstractDocItem;
               iDocument: TMarkupDocument; var ioRow, ioPos: integer) : TDirective;
var
  lTempPos, lTempRow : integer;
  lstIdentifier : string;
begin
  lTempPos := ioPos + 2;  // move after the <!
  lTempRow := ioRow;
  lstIdentifier := iDocument.DocParser.GetNextWord(lTempRow, lTempPos);
  { Check for [CDATA or '--' - not picked up by GetNextWord }
  if (lstIdentifier = '') then
  begin
    iDocument.DocParser.SearchFor(stNonSpace, lTempRow, lTempPos);
    lstIdentifier := Copy(iDocument.FileStrings[lTempRow], lTempPos, 7);
  end;
  { Check the identifier string, and use this to create the appropriate class }
  if Copy(lstIdentifier, 1, 2) = '--' then
    Result := TComment.CreateFromStrings( iParent, iDocument, ioRow, ioPos )
  else
    if lstIdentifier = 'ELEMENT' then
      Result := TElement.CreateFromStrings( iParent, iDocument, ioRow, ioPos )
    else
      if lstIdentifier = 'ATTLIST' then
        Result := TAttList.CreateFromStrings( iParent, iDocument, ioRow, ioPos )
      else
        if lstIdentifier = 'DOCTYPE' then
          Result := TDocType.CreateFromStrings( iParent, iDocument, ioRow, ioPos )
        else
          if lstIdentifier = 'ENTITY' then
            Result := TEntity.CreateFromStrings( iParent, iDocument, ioRow, ioPos )
          else
            Raise EDocItemError.Create(ResStr_DirTypeUnknown + lstIdentifier, ioRow, -1)
end;


{ Check the document is pointing to a correct directive declaration, and advance
     the position past it so we can start reading the declaration definition.  If
     anything's wrong, an exception is raised }
procedure TDirective.ConfirmType(const iName: string; var ioRow,
  ioPos: integer);
var
  lstIdentifier : string;
begin
  if Copy(FDocument.FileStrings[ioRow], ioPos, 2) <> '<!' then
    raise EDirectiveError.Create(ResStr_CreateProblem + iName, ioRow, -1);
  Inc(ioPos, 2);
  if   (FDocument.DocParser.GetChar( ioRow, ioPos ) = ' ') or
       (FDocument.DocParser.GetChar( ioRow, ioPos ) = #9) then
    raise EDirectiveError.Create(ResStr_WhitespaceInDirective, ioRow, -1);
  lstIdentifier := FDocument.DocParser.GetNextWord( ioRow, ioPos );
  if Uppercase(lstIdentifier) <> iName then
    raise EDirectiveError.Create(ResStr_CreateProblem + iName, ioRow, -1);
  Inc(ioPos, Length(iName));
  FDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
end;



//==============================================================================
{ TProcessInstruction }
//==============================================================================


constructor TProcessInstruction.CreateFromStrings(iParent : TAbstractDocItem;
            iDocument: TMarkupDocument; var ioRow, ioPos: integer);
begin
  inherited CreateFromStrings( iParent, iDocument );
  { Find the word following <? }
  inc( ioPos, 2 );
  FName := iDocument.DocParser.GetNextWord( ioRow, ioPos );

  { navigate to the end of the directive }
  iDocument.DocParser.SearchFor( stFindWords, ['?>', '<', '>'], ioRow, ioPos );
  if iDocument.DocParser.GetString( ioRow, ioPos, 2 ) <> '?>' then
    raise EProcessInstructionError.Create( ResStr_IncorrectlyTerminated + '?>', ioRow, -1 );
  { because the above SearchFor call stops on the char before the
    one we want - we must do an Inc }
  Inc(ioPos);
end;





//==============================================================================
{ TMarkupStack }
//==============================================================================

{ Constructor - prepare the list }
constructor TMarkupStack.Create;
begin
  Inherited Create;
  FItems := TList.Create;
end;


{ Destructor - clean up the stack list.  No need to walk the list as the items
     themselves are owned by someone else }
destructor TMarkupStack.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;


{ CheckTop - examine the most recent item on the stack, without removing it from
     the stack.  }
function TMarkupStack.PeekTop: TAbstractDocItem;
begin
  Result := nil; // default value
  if FItems.Count > 0 then
    Result := TAbstractDocItem(FItems[FItems.Count-1]);
end;


{ Clear - resets the contents of the stack }
procedure TMarkupStack.Clear;
begin
  FItems.Clear;
end;


{ PeekItem - accessor method for the Items property.  Called PeekItem rather
     than get so we are clear that the item itself is not removed }
function TMarkupStack.PeekItem(const iIndex: integer): TAbstractDocItem;
begin
  Result := TAbstractDocItem(FItems[iIndex]);
end;


{ Pop - Remove the most recent item from the stack and return it }
function TMarkupStack.Pop: TAbstractDocItem;
begin
  if FItems.Count = 0 then
    raise EStackError.Create(ResStr_StackEmpty);
  Result := TAbstractDocItem(FItems[FItems.Count-1]);
  FItems.Delete(FItems.Count-1);
end;


{ Push an item onto the stack }
procedure TMarkupStack.Push(iItem: TAbstractDocItem);
begin
  FItems.Add( iItem );
end;


{ Accessor method to return the number of items on the stack }
function TMarkupStack.GetSize: integer;
begin
  Result := FItems.Count;
end;



//==============================================================================
{ TMarkupException }
//==============================================================================


{ Replacement constructors for our new exception class - allow the row to be
    recorded but otherwise just inherited behaviour.  If the row is not
    specified, set it to -1 so we know not to display the XML error report
    form. }
constructor TMarkupException.Create(const Msg: string;
  const iRow: integer; const iBlockPos : int64);
begin
  FRow := iRow;
  FBlockPos := iBlockPos;
  inherited Create(Msg);
end;

constructor TMarkupException.Create(const Msg: string;
  iException: Exception; const iRow: integer; const iBlockPos : int64);
begin
  FRow := iRow;
  FBlockPos := iBlockPos;
  inherited Create(Msg, iException);
end;

constructor TMarkupException.CreateNonCritical(const Msg: string;
  const iRow: integer; const iBlockPos : int64);
begin
  FRow := iRow;
  FBlockPos := iBlockPos;
  inherited CreateNonCritical(Msg);
end;

constructor TMarkupException.CreateNonCritical(const Msg: string;
  iException: Exception; const iRow: integer; const iBlockPos : int64);
begin
  FRow := iRow;
  FBlockPos := iBlockPos;
  inherited CreateNonCritical(Msg, iException);
end;

constructor TMarkupException.Create(const Msg: string);
begin
  FRow := -1;
  inherited Create(Msg);
end;


constructor TMarkupException.Create(const Msg: string;
  iException: Exception);
begin
  FRow := -1;
  inherited Create(Msg, iException);
end;

constructor TMarkupException.CreateNonCritical(const Msg: string);
begin
  FRow := -1;
  inherited CreateNonCritical(Msg);
end;

constructor TMarkupException.CreateNonCritical(const Msg: string;
  iException: Exception);
begin
  FRow := -1;
  inherited CreateNonCritical(Msg, iException);
end;




//==============================================================================
{ TTag }
//==============================================================================


{ Adds a tag to the list of nested tags within }
procedure TTag.AddChild(iTag: TTag);
begin
  FNestedTags.Add(iTag);
end;


{ Confirms that the attribute content matches the requirements for the attlist
     of the same name.  Otherwise, raises an exception. }
procedure TTag.CheckAttributeContent;
var
  lAttlist : TAttList;
  lAttribute : TAttribute;
  i : integer;
begin
  try
    lAttlist := TXMLDoc(FDocument).GetAttlistByName(FName);
  except
    on EDTDNotComplete do
      exit; // no further checking - no attributes specified so all are ignored
  end; // try..except
  { First check all required attributes are present, and fill in defaults }
  for i := 0 to lAttlist.AttributeCount-1 do
  begin
    if (FAttributeData.IndexOfName(lAttList.Attributes[i].Name)=-1) then // not present
    begin
      if lAttList.Attributes[i].DefaultValue <> '' then
        FAttributeData.Add(lAttList.Attributes[i].Name + '=' +
                                            lAttList.Attributes[i].DefaultValue)
      else
        { If attribute required but not found, and no default available }
        if (lAttList.Attributes[i].Usage = auRequired) then
          InvalidateTag(ResStr_AttRequired + FName + ' : '
                                      + lAttList.Attributes[i].Name);
    end;
  end; // for
  { Now check all attributes present are of correct data type }
  for i := 0 to FAttributeData.Count-1 do
  begin
    lAttribute := lAttlist.FindAttribute(FAttributeData.Names[i]);
    if lAttribute <> nil then // no need to check it if we know nothing about it
    begin
      if lAttribute.DataType = atEnumerated then // only need to check enumerated types
        if lAttribute.Enumerations.IndexOf(FAttributeData.Values
                                        [FAttributeData.Names[i]]) = -1 then
          { attribute not in list of possible values }
          InvalidateTag(ResStr_AttValueWrong + lAttribute.Name + ' : '
                     + FAttributeData.Values[FAttributeData.Names[i]]);
    end;
  end;
end;



{ Checks a newly identified attribute string and if it indicates a special
     attribute, then store the information in the TagInfo }
procedure TTag.CheckSpecialAttributes(const iAttributeString: string);
begin
  if Copy(iAttributeString, 1, Length(FIELD_NAME + '=')) = FIELD_NAME + '=' then
    FTagInfo := FTagInfo + [tiHasSpecifier]
  else if iAttributeString = KEY_TYPE + '=local' then
    FTagInfo := FTagInfo + [tiLocalKey];
end;



{ Called when the tag parsing is finished (ie at the end tag).  Checks that the
     element's content model requirements have been met. }
procedure TTag.CheckTagContent;
var
  lElement : TElement;
begin
  lElement := TXMLDoc(FDocument).GetElementByName(FName);
  if lElement.ContentModel = cmEmpty then
  begin
    { check element is empty }
    if FNestedTags.Count <> 0 then
      InvalidateTag(ResStr_EmptyContent + FName);
    if FText <> '' then
      InvalidateTag(ResStr_EmptyContent + FName);
  end
  else if lElement.ContentModel = cmMixed then
    lElement.MixedContent.ValidateTag(Self, 0);
end;



{ Called by the parser when the closing markup of this item is located.  Causes
     the markup to validate its content against its content model }
procedure TTag.Close;
begin
  try
    CheckTagContent;
  except on E:Exception do
    InvalidateTag(E.Message);
  end;
end;



{ Works out how many sequential repetitions of the same markup are found within
     the current markup, starting at a defined position }
function TTag.CountRepetitions( const iTagName: string;
                                const iStartPos : integer ): integer;
var
  i : integer;
begin
  i := iStartPos;
  while (i<FNestedTags.Count) and (TTag(FNestedTags[i]).Name = iTagName) do
    Inc(i);
  Result := i - iStartPos;
end;



constructor TTag.CreateFromStrings(iParent: TAbstractDocItem;
  iDocument: TMarkupDocument; var ioRow, ioPos: integer);
//var
//  lElement : TElement;
begin
  FIsInvalid := False;
  if iParent <> nil then
    if iParent is TDirective then
      InvalidateTag(ResStr_TagParent);
  inherited CreateFromStrings(iParent, iDocument);
  try
    { Default FStream = nil - indicates text data }
    FStream := nil;
    { If in an embedded document, don't record the row as when the error form
        is displayed we will be on the wrong document }
    if iDocument.EmbeddedFileCount = 1 then
    begin
      FDocRow := ioRow;
      if iDocument is TXMLDoc then // locate beginning of file block containing tag
        FDocBlockPos := TXMLDoc(iDocument).CurrentBlockStart;
    end
    else
      FDocRow := -1;
    FAttributeData := TStringList.Create;
    FNestedTags := TList.Create;
    if iParent is TTag then
      TTag(iParent).AddChild(Self);
    { Skip past < }
    Inc(ioPos);
    FDocument.DocParser.SearchFor(stNonSpace, ioRow, ioPos);
    FName := FDocument.DocParser.GetNextWord(ioRow, ioPos);
    { Record if this tag is a content tag, or contained within the content tag }
    CheckIsContentTag(iParent);
    Inc(ioPos, Length(FName));
    try
       {Just test the element exists, then forget about it}
      TXMLDoc(FDocument).GetElementByName(FName);
    except
      On EDTDNotComplete do    // must ignore elements we don't understand
      begin
        HandleNoElement( ioRow, ioPos );
        dmNBNXML.Log(ResStr_NotDefined + FName);
        Exit; // nothing more to do for this tag
      end;
    end; // try..except
    ReadAttributeData(ioRow, ioPos);
    CheckAttributeContent;
    if Copy(FDocument.FileStrings[ioRow], ioPos, 2) = '/>' then
    begin
      Inc(ioPos, 1); // empty tag so just move on and Close the tag
      Close;
    end else
    begin
      { Remeber we are an open tag for the future }
      TXMLDoc(FDocument).MarkupStack.Push(Self);
    end;
  except
    on E:Exception do  /// invalidate the tag, but continue
      InvalidateTag(E.Message);
  end; // try except
end;


{ Special constructor used only when a survey key is missing, and we need to
    link up a survey_event.  This is because events are handled as separate
    blocks of XML data to ensure no memory problems occur.  This constructor
    allows us to create a fake link to the outside world }
constructor TTag.CreateDummySurveyKey( iParent : TAbstractDocItem;
                iDocument : TMarkupDocument; iKey : TKeyString );
begin
  inherited CreateFromStrings( iParent, iDocument );
  FName := 'survey_key';
  FText := iKey;
  FAttributeData := TStringList.Create;
  FNestedTags := TList.Create;
  FAttributeData.Add('type=nbn');
end;


{ Destructor - cleanup lists of contained stuff }
destructor TTag.Destroy;
begin
  FAttributeData.Free;
  FNestedTags.Free;
  inherited Destroy;
end;


{ checks if the current tag is inside the content tag (ie a real data tag).
    Uses a property of the parent tag to check this }
procedure TTag.CheckIsContentTag( iParent : TAbstractDocItem );
begin
  FIsContentTag := False; // default
  { Set to true if either this tag, or an 'ancestor', is the content item }
  if CompareText( FName, CONTENT_TAG )=0 then
    FIsContentTag := True;
  if iParent is TTag then
    if TTag(iParent).IsContentTag then
      FIsContentTag := True;
end;



{ When a tag is invalidated during construction, rather than raising exceptions
    we store the invalidation information so the user can later browse the
    invalid items }
procedure TTag.InvalidateTag(const iMessage: string);
begin
  if not FIsInvalid then
  begin
    FIsInvalid := True;
    FInvalidationText := iMessage;
  end;
end;




{ Handles the case where an tag is specified which is not recognised.
     Basically, just skips to the end of the tag, either looking for the />
     end, or </tagname> }
procedure TTag.HandleNoElement(var ioRow, ioPos: integer);
var
  lStartpos, lStartRow : integer;
begin
  { Remember where we started looking }
  lStartPos := ioPos;
  lStartRow := ioRow;
  { Find out if this tag is closed before the next one is opened (<tagname/>)}
  FDocument.DocParser.SearchFor(stFindWords, ['/>','<'], ioRow, ioPos);
  if Copy(FDocument.FileStrings[ioRow], ioPos, 2) = '/>' then
    { An empty tag found, eg <hello/> }
    Inc(ioPos, 2)
  else
  begin
    FDocument.DocParser.SearchFor(stFindWords, ['</' + FName + '>'], lStartRow, lStartPos);
    { Skip past the end tag }
    ioPos := lStartPos + Length(FName) + 4; // 4 = 3 characters for </ + >, and skip past last one
    ioRow := lStartRow;
  end;
end;



{ Loads the information for a tag into the memory stream, when <![CDATA[]>
    is encountered }
procedure TTag.LoadStream(var ioRow, ioPos : integer);
var
  lIndex : integer;
begin
  Inc(ioPos, Length('<!CDATA[')+1);
  lIndex := StrToInt(FDocument.DocParser.GetAllupTo(']]', ioRow, ioPos));
  { Move to end of markup }
  FDocument.DocParser.SearchFor(stFindWords, ['>'], ioRow, ioPos);
  { Store the stream pointer identified by the CDATA content }
  FStream := TXMLDoc(FDocument).GetCData( lIndex );
end;



{ Public method to allow a tag to tell its parent that it must be dropped from
    the list.  When cleaning up blocks of tag memory, this prevents us being
    left with stray pointers }
procedure TTag.DropChildTag(iTagToDrop: TTag);
var
  lIndex : integer;
begin
  lIndex := FNestedTags.IndexOf( iTagToDrop );
  if lIndex <> -1 then
    FNestedTags.Delete( lIndex );
end;



{ Reads the attribute data from a tag, one item at a time.  Stores the data as
     name, value pairs in a stringlist. }
procedure TTag.ReadAttributeData(var ioRow, ioPos: integer);
var
  lAttributeString : string;   // name value pair for each attribute read
  lDelimiter : char;
  lData : string;
  lReadSpecifier : boolean;
begin
  while (FDocument.DocParser.GetChar(ioRow, ioPos) <> '>') and
        (Copy(FDocument.FileStrings[ioRow], ioPos, 2) <> '/>') do
  begin
    lReadSpecifier := False; // default - don't read a specifier
    { we must have an attribute to read so read the name }
    FDocument.DocParser.SearchFor(stNonSpace, ioRow, ioPos);
    lAttributeString := FDocument.DocParser.GetNextWord( ioRow, ioPos );
    if lAttributeString = '' then
      InvalidateTag(ResStr_AttDefWrong);
    if lAttributeString = FIELD_NAME then
    begin
      FHasSpecifier := True;
      lReadSpecifier := True;
    end;
    Inc(ioPos, Length(lAttributeString));
    FDocument.DocParser.SearchFor(stNonSpace, ioRow, ioPos);
    if FDocument.DocParser.GetChar(ioRow, ioPos) <> '=' then
      InvalidateTag(ResStr_AttDefWrong);
    lAttributeString := lAttributeString + '=';
    Inc(ioPos);
    FDocument.DocParser.SearchFor(stNonSpace, ioRow, ioPos);
    { Allow " or ' delimiter for data }
    lDelimiter := FDocument.DocParser.GetChar(ioRow, ioPos);
    if (lDelimiter <> '"') and (lDelimiter <> '''' {single quote}) then
      InvalidateTag(ResStr_AttDefWrong);
    Inc(ioPos);
    lData := FDocument.DocParser.GetAllUpTo(lDelimiter, ioRow, ioPos );
    if lReadSpecifier then // store the specifier
      FSpecifier:=lData;
    lAttributeString := lAttributeString + lData;
    FAttributeData.Add(lAttributeString);
    CheckSpecialAttributes(lAttributeString);
  end;
end;



{ When text is encountered within a tag, we read all the text up to the next
     '<' symbol }
procedure TTag.ReadText(var ioRow, ioPos: integer);
begin
  FText := FDocument.DocParser.GetAllUpTo('<', ioRow, ioPos);
  { We knock 2 of the pos, because the GetAllUpTo command moves us to the
      character after the <, but the parser assumes we finish on the character
      before. }
  Dec(ioPos, 2);
end;













end.

