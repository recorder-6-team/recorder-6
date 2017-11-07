{ ------------------------------------------------------------------------------
// Unit:          html2rtf
//
//                Copyright © Dorset Software Services Ltd, 1999
//
// Description:   Simple HTML <-> RTF conversion implemented as a subclass of
//                TConversion.
//
//                Supported markup elements:
//
//                  -- bold, italic, underline
//                  -- emphasis, strong emphasis
//                  -- named and numeric character entities (ISO 8859-1)
//                  -- comments
//
//                Other tags are ignored. No RTF header information is output.
//                See also dssvcl32\streamhtml2rtf.pas.
//
// Author:        AJWK
//
// Created:       1999-09-14
//
// Changes:       19/07/2000 - Ian Addis - Implemented ConvertWriteStream to
//                enable RTF->HTML
//
// To Do:         More complete implementation of HTML.
//                More correct implementation of RTF.
// ----------------------------------------------------------------------------}
unit html2rtf;

interface

uses
  sysutils, comctrls, classes, splist;

type
  EHTMLRTFConversion = class(Exception);

  THTMLState = (
      htmlNormal, htmlWhitespace, htmlInTag, htmlInComment, htmlInEntity
    );

  TRTFFormat = record
    Bold: Boolean;
    Italic: Boolean;
    Underlined: Boolean;
    Bulleted: Boolean;
    Alignment: TAlignment;
    FontSize: Integer;
  end;

  THTMLRTFConversion = class(TConversion)
  private
    FOutputBuffer: PChar;
    FOutputBufferSize: Longint;
    FOutputBufferPosition: Longint;
    FCurrentHTMLTag: String;
    FTagNameComplete: Boolean;
    FPreviousHTMLState: THTMLState;
    FCurrentHTMLState: THTMLState;
    FPendingOutput: String;
    FHTMLNamedEntities: TStringAndPointerList;
    FConvertStartOffset: Longint;
    FConvertEndOffset: Longint;

    FHTMLStream: TStream;
    FRTFBuffer: PChar;
    FRTFBufferSize: Integer;
    FRTFBufferPos: Integer;
    FRTFFormat: TRTFFormat;
    FRTFBraceGroup: Integer;

    function GetHTMLNamedEntity(const Name: String): Byte;

  protected
    procedure SetConvertStartOffset(newValue: Longint);
    procedure SetConvertEndOffset(newValue: Longint);

    property HTMLNamedEntity[const Name: String]: Byte read GetHTMLNamedEntity;

    function OutputSpaceLeft: Longint;

    procedure InitialiseHTMLNamedEntities;
    procedure InitialiseRTFOutput;
    procedure OpenHTMLTag;
    procedure OpenHTMLEntity;

    function ProcessHTML(inputbuffer: PChar; inputbuffersize: Longint): Longint;
    function ProcessHTMLNormalChar(ch: Char): String;
    function ProcessHTMLWhitespaceChar(ch: Char): String;
    function ProcessHTMLTagChar(ch: Char): String;
    function ProcessHTMLCommentChar(ch: Char): String;
    function ProcessHTMLEntityChar(ch: Char): String;
    function ProcessHTMLTag(const tag: String): String;
    function ProcessHTMLEntity(const entity: String): String;
    function ProcessAnyChar(ch: char): string;

    function GetRTFChar(iBufferPos: Integer): Char;
    procedure ProcessRTFCode;
    procedure IgnoreRTFCode;
    procedure ProcessRTFKeywordCode;
    procedure AppendHTMLStream(Buffer: String);

    procedure WriteOutput(const outputtext: String);

  public
    destructor Destroy; override;
    function ConvertReadString( const iHTML : String ) : string;
    function ConvertReadStream(Stream: TStream; Buffer: PChar;
        BufSize: Integer): Integer; override;

    function ConvertWriteStream(Stream: TStream; Buffer: PChar;
        BufSize: Integer): Integer; override;

    { ------------------------------------------------------------------------
    // properties defining the section of the input stream to be considered
    //
    // -- If ConvertEndOffset is zero then it is taken to mean the end of
    //    the stream
    //
    // -- Note that it is the caller's responsibility to ensure that the
    //    current position of the input stream is equal to CurrentStartOffset
    //    on the first call in a conversion to ConvertReadStream.
    // ----------------------------------------------------------------------}
    property ConvertStartOffset: Longint
        read FConvertStartOffset write SetConvertStartOffset;
    property ConvertEndOffset: Longint
        read FConvertEndOffset write SetConvertEndOffset;

  end;  {class THTMLRTFConversion}




implementation

{$RANGECHECKS ON}

type
  THTMLNamedCharacterEntity = record
    Name: String;
    Code: Byte;
  end;  {record THTMLNamedCharacterEntity}

const
  READ_BUFFER_SIZE = 1000;

  HTML_CHAR_OPEN_TAG = '<';
  HTML_CHAR_CLOSE_TAG = '>';
  HTML_CHAR_SPACE = ' ';
  HTML_CHAR_OPEN_ENTITY = '&';
  HTML_CHAR_CLOSE_ENTITY = ';';
  HTML_CHAR_NUMERIC_ENTITY = '#';

  HTML_TAG_OPEN_COMMENT = '!--';
  HTML_TAG_CLOSE_COMMENT = '--';

  HTML_ENTITY_NEWLINE = 'br';
  HTML_ENTITY_PARA = 'p';
  HTML_ENTITY_BOLD = 'b';
  HTML_ENTITY_BOLDOFF = '/b';
  HTML_ENTITY_STRONG = 'strong';
  HTML_ENTITY_STRONGOFF = '/strong';
  HTML_ENTITY_EMPHASIS = 'em';
  HTML_ENTITY_EMPHASISOFF = '/em';
  HTML_ENTITY_ITALIC = 'i';
  HTML_ENTITY_ITALICOFF = '/i';
  HTML_ENTITY_UNDERLINE = 'u';
  HTML_ENTITY_UNDERLINEOFF = '/u';

  HTML_WHITESPACE = [' ', #9, #10, #13];

  HTML_NUMERIC_ENTITIES = [9, 10, 13, 32..126, 130..140, 145..156, 159..255];

  HTML_NAMED_ENTITIES: array[1..100] of THTMLNamedCharacterEntity = (
    (Name: 'quot';    Code: 34),
    (Name: 'amp';     Code: 38),
    (Name: 'lt';      Code: 60),
    (Name: 'gt';      Code: 62),
    (Name: 'nbsp';    Code: 160),
    (Name: 'iexcl';   Code: 161),
    (Name: 'cent';    Code: 162),
    (Name: 'pound';   Code: 163),
    (Name: 'curren';  Code: 164),
    (Name: 'yen';     Code: 165),
    (Name: 'brvbar';  Code: 166),
    (Name: 'sect';    Code: 167),
    (Name: 'uml';     Code: 168),
    (Name: 'copy';    Code: 169),
    (Name: 'ordf';    Code: 170),
    (Name: 'laquo';   Code: 171),
    (Name: 'not';     Code: 172),
    (Name: 'shy';     Code: 173),
    (Name: 'reg';     Code: 174),
    (Name: 'macr';    Code: 175),
    (Name: 'deg';     Code: 176),
    (Name: 'plusmn';  Code: 177),
    (Name: 'sup2';    Code: 178),
    (Name: 'sup3';    Code: 179),
    (Name: 'acute';   Code: 180),
    (Name: 'micro';   Code: 181),
    (Name: 'para';    Code: 182),
    (Name: 'middot';  Code: 183),
    (Name: 'cedil';   Code: 184),
    (Name: 'sup1';    Code: 185),
    (Name: 'ordm';    Code: 186),
    (Name: 'raquo';   Code: 187),
    (Name: 'frac14';  Code: 188),
    (Name: 'frac12';  Code: 189),
    (Name: 'frac34';  Code: 190),
    (Name: 'iquest';  Code: 191),
    (Name: 'Agrave';  Code: 192),
    (Name: 'Aacute';  Code: 193),
    (Name: 'Acirc';   Code: 194),
    (Name: 'Atilde';  Code: 195),
    (Name: 'Auml';    Code: 196),
    (Name: 'Aring';   Code: 197),
    (Name: 'AElig';   Code: 198),
    (Name: 'Ccedil';  Code: 199),
    (Name: 'Egrave';  Code: 200),
    (Name: 'Eacute';  Code: 201),
    (Name: 'Ecirc';   Code: 202),
    (Name: 'Euml';    Code: 203),
    (Name: 'Igrave';  Code: 204),
    (Name: 'Iacute';  Code: 205),
    (Name: 'Icirc';   Code: 206),
    (Name: 'Iuml';    Code: 207),
    (Name: 'ETH';     Code: 208),
    (Name: 'Ntilde';  Code: 209),
    (Name: 'Ograve';  Code: 210),
    (Name: 'Oacute';  Code: 211),
    (Name: 'Ocirc';   Code: 212),
    (Name: 'Otilde';  Code: 213),
    (Name: 'Ouml';    Code: 214),
    (Name: 'times';   Code: 215),
    (Name: 'Oslash';  Code: 216),
    (Name: 'Ugrave';  Code: 217),
    (Name: 'Uacute';  Code: 218),
    (Name: 'Ucirc';   Code: 219),
    (Name: 'Uuml';    Code: 220),
    (Name: 'Yacute';  Code: 221),
    (Name: 'THORN';   Code: 222),
    (Name: 'szlig';   Code: 223),
    (Name: 'agrave';  Code: 224),
    (Name: 'aacute';  Code: 225),
    (Name: 'acirc';   Code: 226),
    (Name: 'atilde';  Code: 227),
    (Name: 'auml';    Code: 228),
    (Name: 'aring';   Code: 229),
    (Name: 'aelig';   Code: 230),
    (Name: 'ccedil';  Code: 231),
    (Name: 'egrave';  Code: 232),
    (Name: 'eacute';  Code: 233),
    (Name: 'ecirc';   Code: 234),
    (Name: 'euml';    Code: 235),
    (Name: 'igrave';  Code: 236),
    (Name: 'iacute';  Code: 237),
    (Name: 'icirc';   Code: 238),
    (Name: 'iuml';    Code: 239),
    (Name: 'eth';     Code: 240),
    (Name: 'ntilde';  Code: 241),
    (Name: 'ograve';  Code: 242),
    (Name: 'oacute';  Code: 243),
    (Name: 'ocirc';   Code: 244),
    (Name: 'otilde';  Code: 245),
    (Name: 'ouml';    Code: 246),
    (Name: 'divide';  Code: 247),
    (Name: 'oslash';  Code: 248),
    (Name: 'ugrave';  Code: 249),
    (Name: 'uacute';  Code: 250),
    (Name: 'ucirc';   Code: 251),
    (Name: 'uuml';    Code: 252),
    (Name: 'yacute';  Code: 253),
    (Name: 'thorn';   Code: 254),
    (Name: 'yuml';    Code: 255)
  );

  RTF_START = '{'; // \rtf1 \ansi ';
  RTF_END = '}';
  RTF_ESCAPE = '\';
  RTF_ESCAPED_CHARS = ['\', '{', '}'];

  RTF_NEWLINE = '\line ';
  RTF_PARA = '\par ';
  RTF_BOLD = '\b ';
  RTF_BOLDOFF = '\b0 ';
  RTF_ITALIC = '\i ';
  RTF_ITALICOFF = '\i0 ';
  RTF_UNDERLINE = '\ul ';
  RTF_UNDERLINEOFF = '\ul0 ';

resourcestring
    ResStr_BadEntity = 'Unknown named character entity ''%s''';

{ ------------------------------------------------------------------------------
// THTMLRTFConversion.Destroy
// ----------------------------------------------------------------------------}
destructor THTMLRTFConversion.Destroy;
begin
  FHTMLNamedEntities.Free;
  inherited Destroy;
end;  {THTMLRTFConversion.Destroy}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.InitialiseHTMLNamedEntities
//
// Populates the lookup list of HTML named entities.
// ----------------------------------------------------------------------------}
procedure THTMLRTFConversion.InitialiseHTMLNamedEntities;
var
  i:        Integer;
  pEntity:  ^THTMLNamedCharacterEntity;

begin
  if not Assigned(FHTMLNamedEntities) then
      FHTMLNamedEntities := TStringAndPointerList.Create;

  FHTMLNamedEntities.Clear;
  for i := Low(HTML_NAMED_ENTITIES) to High(HTML_NAMED_ENTITIES) do begin
    pEntity := @(HTML_NAMED_ENTITIES[i]);
    FHTMLNamedEntities.Add(pEntity^.Name, pEntity);
  end;
end;  {THTMLRTFConversion.InitialiseHTMLNamedEntities}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.GetHTMLNamedEntity
//
// Converts an HTML entity name to the corresponding ISO 8859-1 character
// number.
//
// -- Raises EListError if the entity name is not recognised.
// ----------------------------------------------------------------------------}
function THTMLRTFConversion.GetHTMLNamedEntity(const Name: String): Byte;
var
  pEntity:  ^THTMLNamedCharacterEntity;

begin
  if not Assigned(FHTMLNamedEntities) then InitialiseHTMLNamedEntities;

  if not FHTMLNamedEntities.FindKey(Name) then
    raise EListError.Create(Format(ResStr_BadEntity, [Name]))

  else begin
    pEntity := FHTMLNamedEntities.FoundData;
    Result := pEntity^.Code;
  end;  {if FHTMLNamedEntities.FindKey(..)}
end;  {THTMLRTFConversion.GetHTMLNamedEntity}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.SetConvertStartOffset
// ----------------------------------------------------------------------------}
procedure THTMLRTFConversion.SetConvertStartOffset(newValue: Longint);
begin
  FConvertStartOffset := newValue;
end;  {THTMLRTFConversion.SetConvertStartOffset}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.SetConvertEndOffset
// ----------------------------------------------------------------------------}
procedure THTMLRTFConversion.SetConvertEndOffset(newValue: Longint);
begin
  FConvertEndOffset := newValue;
end;  {THTMLRTFConversion.SetConvertEndOffset}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.ConvertWriteStream
//
// Converts from RTF to HTML (corresponding to an attempt to save a .html file
// from a TRichEdit).
//
// -- Reads from the buffer argument and outputs to the stream.
//
// -- If the current position of the stream is not ConvertStartOffset then this
//    is assumed to be a continuation of a previous conversion and the state is
//    not reset.
// ----------------------------------------------------------------------------}
function THTMLRTFConversion.ConvertWriteStream(Stream: TStream; Buffer: PChar;
    BufSize: Integer): Integer;
var
  iInitialStreamPos: Integer;
  chCurrent: Char;
begin

  //Set object variables
  FHTMLStream:= Stream;
  FRTFBuffer:= Buffer;
  FRTFBufferSize:= BufSize;
  FRTFBufferPos:= 0;
  iInitialStreamPos:= Stream.Position;

  //Process each character in the buffer
  chCurrent:= GetRTFChar(FRTFBufferPos);
  repeat

    //Determine current character
    case chCurrent of
      #0..#12: ; //ASCII control character - ignore
      #13: AppendHTMLStream(Chr(13) + Chr(10)); //Carridge return
      #14..#31: ; //ASCII control character - ignore
      '{' : Inc(FRTFBraceGroup);
      '}' : Dec(FRTFBraceGroup);
      '\' : ProcessRTFCode; //RTF control code
    else
      //Append standard character to the HTML stream
      AppendHTMLStream(chCurrent);
    end;

    //Process next character
    Inc(FRTFBufferPos);
    chCurrent:= GetRTFChar(FRTFBufferPos);
  until chCurrent = #0;

  //Return number of characters appended to the stream
  Result:= FHTMLStream.Position - iInitialStreamPos;
end;  {THTMLRTFConversion.ConvertWriteStream}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.ConvertReadStream
//
// Converts from HTML to RTF (corresponding to an attempt to load from a .html
// file into a TRichEdit).
//
// -- Reads from the Stream argument and outputs to the buffer.
//
// -- If the current position of the stream is not ConvertStartOffset then this
//    is assumed to be a continuation of a previous conversion and the state is
//    not reset.
// ----------------------------------------------------------------------------}
function THTMLRTFConversion.ConvertReadStream(Stream: TStream; Buffer: PChar;
    BufSize: Integer): Integer;
var
  readbuffer:      array[0..READ_BUFFER_SIZE - 1] of Char;
  readbytes:       Longint;
  processedbytes:  Longint;
  endoffset:       Longint;

begin
  FOutputBuffer := Buffer;
  FOutputBufferSize := BufSize - Length(RTF_END);
  FOutputBufferPosition := 0;

  if Stream.Position > ConvertStartOffset then
    {write any pending output from previous calls}
    WriteOutput(FPendingOutput)
  else
    {this is a new conversion -- reset state}
    InitialiseRTFOutput;

  {work out where the end of the input is}
  endoffset := ConvertEndOffset;
  if endoffset = 0 then endoffset := Stream.Size;

  {check that we haven't finished already}
  if Stream.Position = endoffset then begin
    Result := 0;
    Exit;
  end;  {if Stream.Position = endoffset}

  {process the input stream in chunks of READ_BUFFER_SIZE}
  while (OutputSpaceLeft > 0) and (Stream.Position < endoffset) do begin
    readbytes := Stream.Read(readbuffer, READ_BUFFER_SIZE);
    processedbytes := ProcessHTML(readbuffer, readbytes);
    if processedbytes < readbytes then begin
      {move the stream back to the start of the input that
       has not been processed}
      Stream.Seek(processedbytes - readbytes, soFromCurrent);
    end;
  end;  {(while OutputSpaceLeft > 0) and ..}

  if Stream.Position = endoffset then begin
    {terminate the outer rtf group}
    if FPendingOutput <> '' then
      FPendingOutput := FPendingOutput + RTF_END
    else begin
      FOutputBuffer[FOutputBufferPosition] := RTF_END;
      Inc(FOutputBufferPosition, Length(RTF_END));
    end;  {if FPendingOutput = ''}
  end;  {if Stream.Position = endoffset}

  Result := FOutputBufferPosition;
end;  {THTMLRTFConversion.ConvertReadStream}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.OutputSpaceLeft
//
// The number of bytes of free space remaining in the output buffer.
// ----------------------------------------------------------------------------}
function THTMLRTFConversion.OutputSpaceLeft: Longint;
begin
  Result := FOutputBufferSize - FOutputBufferPosition;
end;


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.InitialiseRTFOutput
// ----------------------------------------------------------------------------}
procedure THTMLRTFConversion.InitialiseRTFOutput;
begin
  FCurrentHTMLState := htmlNormal;
  FPendingOutput := '';
  WriteOutput(RTF_START);
end;  {THTMLRTFConversion.InitialiseRTFOutput}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.OpenHTMLTag
// ----------------------------------------------------------------------------}
procedure THTMLRTFConversion.OpenHTMLTag;
begin
  FPreviousHTMLState := FCurrentHTMLState;
  FCurrentHTMLState := htmlInTag;
  FCurrentHTMLTag := '';
  FTagNameComplete := False;
end;  {THTMLRTFConversion.OpenHTMLTag}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.OpenHTMLEntity
// ----------------------------------------------------------------------------}
procedure THTMLRTFConversion.OpenHTMLEntity;
begin
  FCurrentHTMLState := htmlInEntity;
  FCurrentHTMLTag := '';
  FTagNameComplete := False;
end;  {THTMLRTFConversion.OpenHTMLEntity}


{ ------------------------------------------------------------------------------
// HTMLRTFConversion.ProcessHTML
//
// Processes the HTML source text from the input buffer, starting in the
// current state.
//
// -- Terminates before processing all the input if the output buffer becomes
//    exhausted.
//
// -- Returns the number of bytes actually processed from the input buffer.
// ----------------------------------------------------------------------------}
function THTMLRTFConversion.ProcessHTML(
    inputbuffer: PChar; inputbuffersize: Longint): Longint;
var
  processed:   Longint;
  ch:          Char;
  outputtext:  String;

begin
  processed := 0;
  while processed < inputbuffersize do begin

    ch := inputbuffer[processed];
    outputText := ProcessAnyChar(ch);

    WriteOutput(outputtext);
    Inc(processed);
    if OutputSpaceLeft = 0 then Break;

  end;  {while processed < inputbuffersize}

  Result := processed;
end;  {THTMLRTFConversion.ProcessHTML}


{ ------------------------------------------------------------------------------
// HTMLRTFConversion.ConvertReadString
//
// String version of ConvertReadsStream
// Returns the RTF version of the input HTML text
// ----------------------------------------------------------------------------}
function THTMLRTFConversion.ConvertReadString(const iHTML: String): string;
var
  i : integer;
  lRTF : string;
begin
  lRTF := RTF_START;
  for i := 1 to Length(iHTML) do
    lRtf := lRtf + ProcessAnyChar(iHTML[i]);
  Result := lRTF + RTF_END;
end;


{ ------------------------------------------------------------------------------
// HTMLRTFConversion.ConvertReadString
//
// Converts any single char into the RTF equivalent
// ----------------------------------------------------------------------------}
function THTMLRTFConversion.ProcessAnyChar(ch : char): string;
begin
  if ch in HTML_WHITESPACE then ch := HTML_CHAR_SPACE;

  case FCurrentHTMLState of
    htmlNormal:
      Result := ProcessHTMLNormalChar(ch);
    htmlWhitespace:
      Result := ProcessHTMLWhitespaceChar(ch);
    htmlInTag:
      Result := ProcessHTMLTagChar(ch);
    htmlInComment:
      Result := ProcessHTMLCommentChar(ch);
    htmlInEntity:
      Result := ProcessHTMLEntityChar(ch);
  end;  {case FCurrentHTMLState}
end;



{ ------------------------------------------------------------------------------
// THTMLRTFConversion.ProcessHTMLNormalChar
//
// Processes the next byte of the HTML text.
//
// -- If the character considered is whitespace, an open markup delimiter
//    or the first byte of an HTML character entity then sets the state
//    appropriately and returns an empty string.
//
// -- Otherwise returns the input character (with RTF escaping if required)
//    and sets the state to htmlNormal.
// ----------------------------------------------------------------------------}
function THTMLRTFConversion.ProcessHTMLNormalChar(ch: Char): String;
begin
  Result := ch;  {default return value}

  case ch of

    HTML_CHAR_SPACE:
      FCurrentHTMLState := htmlWhitespace;

    HTML_CHAR_OPEN_TAG: begin
      Result := '';
      OpenHTMLTag;
    end;  {HTML_CHAR_OPEN_TAG}

    HTML_CHAR_OPEN_ENTITY: begin
      Result := '';
      OpenHTMLEntity;
    end;  {HTML_CHAR_OPEN_ENTITY}

  else
    FCurrentHTMLState := htmlNormal;

    {make sure we don't output RTF control characters without escaping them...}
    if ch in RTF_ESCAPED_CHARS then Result := RTF_ESCAPE + ch;

  end;  {case ch}

end;  {THTMLRTFConversion.ProcessHTMLNormalChar}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.ProcessHTMLWhitespaceChar
//
// Processes the next byte of the current run of HTML whitespace.
//
// -- If the character considered is not whitespace then delegates to
//    ProcessHTMLNormalChar
//
// -- Otherwise returns an empty string.
// ----------------------------------------------------------------------------}
function THTMLRTFConversion.ProcessHTMLWhitespaceChar(ch: Char): String;
begin
  if ch = HTML_CHAR_SPACE then
    Result := ''
  else
    Result := ProcessHTMLNormalChar(ch);
end;  {THTMLRTFConversion.ProcessHTMLWhitespaceChar}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.ProcessHTMLTagChar
//
// Processes the next byte of the current HTML tag.
//
// -- If the markup delimiter has been reached then sets the state to its
//    value before the tag.
//
// -- If the tag name has just been fully determined then returns the
//    equivalent (if supported); otherwise returns an empty string.
//
// -- Sets the state to htmlInComment on encountering an open comment tag.
// ----------------------------------------------------------------------------}
function THTMLRTFConversion.ProcessHTMLTagChar(ch: Char): String;
begin
  Result := '';  {default return value}

  case ch of

    HTML_CHAR_CLOSE_TAG: begin
      if not FTagNameComplete then Result := ProcessHTMLTag(FCurrentHTMLTag);
      FCurrentHTMLState := FPreviousHTMLState;
    end;  {HTML_CHAR_CLOSE_TAG}

    HTML_CHAR_SPACE: if not FTagNameComplete then begin
      FTagNameComplete := True;
      Result := ProcessHTMLTag(FCurrentHTMLTag);
    end;  {HTML_CHAR_SPACE}

  else
    if not FTagNameComplete then begin
      FCurrentHTMLTag := FCurrentHTMLTag + LowerCase(ch);

      {spot comment tags early -- we won't insist on the space after '<!--'}
      if FCurrentHTMLTag = HTML_TAG_OPEN_COMMENT then begin
        FCurrentHTMLState := htmlInComment;
        FCurrentHTMLTag := '';
      end;  {if FCurrentHTMLTag = HTML_TAG_OPEN_COMMENT}
    end;  {if not FTagNameComplete}

  end;  {case ch}

end;  {THTMLRTFConversion.ProcessHTMLTagChar}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.ProcessHTMLCommentChar
//
// Processes the next byte of the current HTML comment.
//
// -- If the comment has been closed then sets the state to its value before
//    the comment.
//
// -- Always returns an empty string.
// ----------------------------------------------------------------------------}
function THTMLRTFConversion.ProcessHTMLCommentChar(ch: Char): String;
var
  handled:  Boolean;

begin
  Result := '';  {default return value}
  handled := False;

  if FCurrentHTMLTag = HTML_TAG_CLOSE_COMMENT then begin
    {we may be about to close the comment, but whitespace
     is allowed between the '--' and the markup delimiter}
    handled := (ch in [HTML_CHAR_SPACE, HTML_CHAR_CLOSE_TAG]);

    if ch = HTML_CHAR_CLOSE_TAG then
      {the comment has been closed}
      FCurrentHTMLState := FPreviousHTMLState;

  end;  {if FCurrentHTMLTag = HTML_TAG_CLOSE_COMMENT}

  if not handled then begin
    {the comment text considered so far does not contain a close element
     -- add the next character to the end of the current tag and remove
     excess characters from the left}
    FCurrentHTMLTag := FCurrentHTMLTag + LowerCase(ch);
    if Length(FCurrentHTMLTag) > Length(HTML_TAG_CLOSE_COMMENT) then
      FCurrentHTMLTag := Copy(FCurrentHTMLTag, 2, Length(FCurrentHTMLTag) - 1);
  end;

end;  {THTMLRTFConversion.ProcessHTMLCommentChar}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.ProcessHTMLEntityChar
//
// Processes the next byte of the current HTML character entity.
//
// -- If the whole entity has been read then sets the state to htmlNormal
//    and returns the RTF equivalent.
//
// -- Otherwise returns an empty string.
// ----------------------------------------------------------------------------}
function THTMLRTFConversion.ProcessHTMLEntityChar(ch: Char): String;
begin
  case ch of

    HTML_CHAR_CLOSE_ENTITY, HTML_CHAR_SPACE: begin
      {note: although not strictly correct we will allow HTML character
       entities to be terminated by whitespace as well as ';'}
      FCurrentHTMLState := htmlNormal;
      Result := ProcessHTMLEntity(FCurrentHTMLTag);
      if ch = HTML_CHAR_SPACE then
        Result := Result + ProcessHTMLNormalChar(ch);
    end;  {HTML_CHAR_CLOSE_ENTITY, HTML_CHAR_SPACE}

  else
    FCurrentHTMLTag := FCurrentHTMLTag + ch;
    Result := '';

  end;  {case ch}
end;  {THTMLRTFConversion.ProcessHTMLEntityChar}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.ProcessHTMLTag
//
// Converts a selected subset of the standard HTML entities into RTF code
// words.
// ----------------------------------------------------------------------------}
function THTMLRTFConversion.ProcessHTMLTag(const tag: String): String;
begin
  Result := '';  {default return value}

  if tag = HTML_ENTITY_NEWLINE then begin
    FPreviousHTMLState := htmlWhitespace;
    Result := RTF_NEWLINE;

  end else if tag = HTML_ENTITY_PARA then begin
    FPreviousHTMLState := htmlWhitespace;
    {nasty hack to allow us to pretend we handle inferred </p> tags}
    if FOutputBufferPosition > 1 then Result := RTF_PARA;

  end else if (tag = HTML_ENTITY_BOLD) or (tag = HTML_ENTITY_STRONG) then
    Result := RTF_BOLD

  else if (tag = HTML_ENTITY_BOLDOFF) or (tag = HTML_ENTITY_STRONGOFF) then
    Result := RTF_BOLDOFF

  else if (tag = HTML_ENTITY_ITALIC) or (tag = HTML_ENTITY_EMPHASIS) then
    Result := RTF_ITALIC

  else if (tag = HTML_ENTITY_ITALICOFF) or (tag = HTML_ENTITY_EMPHASISOFF) then
    Result := RTF_ITALICOFF

  else if (tag = HTML_ENTITY_UNDERLINE) then
    Result := RTF_UNDERLINE

  else if (tag = HTML_ENTITY_UNDERLINEOFF) then
    Result := RTF_UNDERLINEOFF;

end;  {THTMLRTFConversion.ProcessHTMLTag}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.ProcessHTMLEntity
//
// Converts an HTML character entity (e.g. '#169' or 'copy') into a
// corresponding RTF code word (based on the character number).
//
// -- Converts only those characters from ISO 8859-1 that are supported by
//    HTML 4.0.
//
// -- Invalid or unrecognised character entities are converted to '?'.
// ----------------------------------------------------------------------------}
function THTMLRTFConversion.ProcessHTMLEntity(const entity: String): String;
var
  code:  Byte;

begin
  Result := '?';  {default return value}
  if entity = '' then Exit;  {give up now!}

  if entity[1] = HTML_CHAR_NUMERIC_ENTITY then begin
    {numeric entity of form #nnn}
    if Length(entity) = 4 then begin
      try
        code := StrToInt(Copy(entity, 2, Length(entity) - 1));
        if code in HTML_NUMERIC_ENTITIES then begin
          Result := Format('\''%.2x', [code]);
        end;
      except
        on EConvertError do {nothing};
        on ERangeError do {nothing};
      end;  {try .. except}
    end;  {if Length(entity) = 4}

  end else begin
    {named entity}
    try
      code := HTMLNamedEntity[entity];
      Result := Format('\''%.2x', [code]);
    except
      on EListError do {nothing};
    end;  {try .. except}

  end;  {if entity[1] <> HTML_CHAR_NUMERIC_ENTITY}

end;  {THTMLRTFConversion.ProcessHTMLEntity}


{ ------------------------------------------------------------------------------
// THTMLRTFConversion.WriteOutput
//
// Writes the given text into the output buffer.
//
// -- If there is only sufficient space for part of the string then the
//    remaining text is cached in FPendingOutput.
//
// -- Otherwise FPendingOutput is cleared.
// ----------------------------------------------------------------------------}
procedure THTMLRTFConversion.WriteOutput(const outputtext: String);
var
  outputlength:  Longint;
  spaceleft:     Longint;

begin
  outputlength := Length(outputtext);
  spaceleft := OutputSpaceLeft;

  if outputlength <= spaceleft then
    FPendingOutput := ''

  else begin
    {save what we can't write now}
    FPendingOutput := Copy(
        outputtext, spaceleft + 1, outputlength - spaceleft
      );
    outputlength := spaceleft;

  end;

  StrPLCopy(FOutputBuffer + FOutputBufferPosition, outputtext, outputlength);
  Inc(FOutputBufferPosition, outputlength);
end;  {THTMLRTFConversion.WriteOutput}

function THTMLRTFConversion.GetRTFChar(iBufferPos: Integer): Char;
begin

  //Check we have not reached the end of the buffer
  if iBufferPos >= FRTFBufferSize then
    Result:= #0
  else
    Result:= FRTFBuffer[iBufferPos];
end;

procedure THTMLRTFConversion.ProcessRTFCode;
var
  chCodeType: Char;
begin

  //Determine code type
  Inc(FRTFBufferPos);
  chCodeType:= GetRTFChar(FRTFBufferPos);
  case chCodeType of
    '*', '''': IgnoreRTFCode; //Special code - ignore
    '\','{','}': AppendHTMLStream(chCodeType); //Append character to the HTML stream (special meaning has been "escaped" by by putting \ in front)
  else
    ProcessRTFKeywordCode; //RTF keyword code
  end;
end;

procedure THTMLRTFConversion.IgnoreRTFCode;
begin

  //Ignore characters until new code tag is found
  while not (GetRTFChar(FRTFBufferPos + 1) In ['\','{','}',' ',';',#0]) do
    Inc(FRTFBufferPos);
end;

procedure THTMLRTFConversion.ProcessRTFKeywordCode;
var
  chCurrent: Char;
  sKeyword: string;
  sValue: string;
  iFontSize: integer;
begin

  //Get keyword
  chCurrent:= GetRTFChar(FRTFBufferPos);
  repeat
    sKeyword:= sKeyword + chCurrent;
    Inc(FRTFBufferPos);
    chCurrent:= GetRTFChar(FRTFBufferPos);
  until chCurrent in ['\','{','}',' ',';','-','0'..'9',#0];
  Dec(FRTFBufferPos); //Reset buffer position to be last character of keyword

  //Process keyword
  if sKeyword = 'par' then //New paragraph
    AppendHTMLStream('<br>')
  else if sKeyword='tab' then //Tab
    AppendHTMLStream('        ') //Append 8 spaces as tab characters do not work in HTML
  else if sKeyword='plain' then
  begin
    if FRTFFormat.Bold then
    begin
      AppendHTMLStream('</b>');
      FRTFFormat.Bold:= False;
    end;
    if FRTFFormat.Italic then
    begin
      AppendHTMLStream('</i>');
      FRTFFormat.Italic:= False;
    end;
    if FRTFFormat.Underlined then
    begin
      AppendHTMLStream('</u>');
      FRTFFormat.Underlined:= False;
    end;
  end else if sKeyword='b' then //Bold
  begin
    if FRTFFormat.Bold then
      AppendHTMLStream('</b>')
    else
      AppendHTMLStream('<b>');
    FRTFFormat.Bold:= not FRTFFormat.Bold;
  end else if sKeyword='i' then //Italic
  begin
    if FRTFFormat.Italic then
      AppendHTMLStream('</i>')
    else
      AppendHTMLStream('<i>');
    FRTFFormat.Italic:= not FRTFFormat.Italic;
  end else if sKeyword='ul' then //Underline
  begin
    if FRTFFormat.Underlined then
      AppendHTMLStream('</u>')
    else
      AppendHTMLStream('<u>');
    FRTFFormat.Underlined:= not FRTFFormat.Underlined;
  end else if sKeyword = 'qc' then //Centred
  begin
    AppendHTMLStream('<center>');
    FRTFFormat.Alignment:= taCenter;
  end else if sKeyword = 'fs' then
  begin

    //Get size
    Inc(FRTFBufferPos);
    chCurrent:= GetRTFChar(FRTFBufferPos);
    repeat
      sValue:= sValue + chCurrent;
      Inc(FRTFBufferPos);
      chCurrent:= GetRTFChar(FRTFBufferPos);
    until not (chCurrent in ['0'..'9']);
    Dec(FRTFBufferPos); //Reset buffer position to be last character of keyword

    //Check size not already set
    iFontSize:= StrToInt(sValue);
    if iFontSize <> FRTFFormat.FontSize then
    begin
      FRTFFormat.FontSize:= iFontSize;

      //Convert size
      case (FRTFFormat.FontSize div 2) of
        1.. 5: iFontSize:= 1;
        6.. 9: iFontSize:= 2;
        10..11: iFontSize:= 3;
        12..13: iFontSize:= 4;
        14..15: iFontSize:= 5;
      else
        iFontSize:= 6;
      end;

      //Append font size
      AppendHTMLStream('<FONT size=' + IntToStr(iFontSize) + '>');
    end;
  end else if sKeyword = 'fi' then //Bullet list
  begin
    AppendHTMLStream('<ul>');
    FRTFFormat.Bulleted:= True;
    IgnoreRTFCode; //Ignore rest of code
  end else if sKeyword = 'pntext' then //Bullet item
    AppendHTMLStream('<li>')
  else if sKeyword = 'pard' then //End of Bullet list or centered text!
  begin
    if FRTFFormat.Bulleted then
    begin
      AppendHTMLStream('</ul>');
      FRTFFormat.Bulleted:= False;
    end;
    if FRTFFormat.Alignment = taCenter then
    begin
      AppendHTMLStream('</center>');
      FRTFFormat.Alignment:= taLeftJustify;
    end;
  end else
    IgnoreRTFCode; //Ignore code

  //Ignore trailing space
  if GetRTFChar(FRTFBufferPos + 1) = ' ' then
    Inc(FRTFBufferPos);
end;

procedure THTMLRTFConversion.AppendHTMLStream(Buffer: String);
begin

  //Append buffer to stream
  FHTMLStream.WriteBuffer(Pointer(Buffer)^, Length(Buffer));
end;

end.
