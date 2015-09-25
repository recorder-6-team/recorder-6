{ Fileoutput
     contains TFileOutput class, for outputting text and binary information into
     a file }
unit FileOutput;

interface

uses
  Sysutils, Classes, ExceptionForm, db;

const
  CRLF = #13#10; // carriage return and line feed
  TAB = #9;

type

  EFileOutputError = class(TExceptionPath);

  { Class to output lines of information into a file.  Basically a file handling
       class which uses a stream to manage the data.  This allows binary
       info to be embedded into the text. }
  TFileOutput = class(TObject)
  private
    FOutputStream : TMemoryStream;
    FFileName : string;
    FCancelled: boolean;
    procedure SetFileSize(const Value: Longint);
    procedure SetCancelled(const Value: boolean);
  protected
    function GetFileSize : Longint;
  public
    constructor Create( const iFileName : string );
    destructor Destroy; override;
    procedure OutputText(const iLine: string);  // contains list of records currently being output
    procedure OutputBlob( iBlobField :TField );
    procedure NewLine( const iIndent : integer );
    function Safetext( const iText : string ): string;
    { The filesize property allows rollback to a certain point in the output
         file - effectively an undo facility }
    property FileSize : Longint read GetFileSize write SetFileSize;
    property Cancelled : boolean read FCancelled write SetCancelled;
  end;


implementation

uses GeneralFunctions;

resourcestring
  ResStr_SizeIncrease = 'File output streams cannot be increased in size directly.';
  ResStr_MaxFieldSizeExc = 'Maximum field size exceeded';

{ Constructor - just set up the output file stream }
constructor TFileOutput.Create( const iFileName : string );
begin
  inherited Create;
  FFileName := iFileName;
  FOutputStream := TMemoryStream.Create;
  FOutputStream.Clear;
  FOutputStream.Size := $FFFFF;     // Init to 1MB
  FCancelled := False;
end;


{ Destructor - free the string list }
destructor TFileOutput.Destroy;
begin
  FOutputStream.SetSize(FOutputStream.Position);
  FOutputStream.SaveToFile(FFileName);
  FOutputStream.Free;
  inherited Destroy;
end;




{ Output a single line of text into the file. }
procedure TFileOutput.OutputText(const iLine: string);
begin
  // Write the string to the stream. We want to write from SourceString's
  // data, starting at a pointer to SourceString (this returns a pointer to
  // the first character), dereferenced (this gives the actual character).
  FOutputStream.WriteBuffer(Pointer(iLine)^, Length(iLine));
  // if using all of the memory stream, then increase it by 1MB block.
  if FOutputStream.Position >= FOutputStream.Size then begin
    FOutputStream.Size := FOutputStream.Size + $FFFFF;
    TrimWorkingSet;
  end;
end;




{ Output a BLOB into the stream.  Takes a string prefix and suffix to delimit
     the binary data within the file.  The Blob is preceded by a hex
     size specifier.  If the field is not a blob field, then outputs it as a
     string, but the size prefix is still included }
procedure TFileOutput.OutputBlob( iBlobField: TField);
var
  lSize : Longint; // yes - longint, not integer to be consistent
begin
  { Read size for blob/string }
  if iBlobField.IsBlob then
    lSize := TBlobField(iBlobField).BlobSize
  else
    lSize := Length(iBlobField.AsString);
  { first check we are not over the max blob size }
  if lSize > $FFFFFF then
    OutputText(ResStr_MaxFieldSizeExc)
  else
  begin // size ok so output correct blob data
    OutputText( '<![CDATA[' );
    { Write out the size specifier }
    OutputText(IntToHex(lSize,6));
    if iBlobField.IsBlob then
      TBlobField(iBlobField).SaveToStream( FOutputStream )
    else
      OutputText( iBlobField.AsString );
    outputText( ']]>' );
  end;
end;



{ Accessor method to allow checking of the current data file size }
function TFileOutput.GetFileSize: Longint;
begin
  Result := FOutputStream.Position;
end;


{ Accessor method allowing output stream position to be rolled back - an effective
     undo }
procedure TFileOutput.SetFileSize(const Value: Longint);
begin
  { Don't allow the size to increase in this fashion or we will have
      uninitialised stream data }
  if FOutputStream.Position >= Value then
    FOutputStream.Position := Value
  else
    raise EFileOutputError.Create(ResStr_SizeIncrease);
end;


{ Start a new line and indent to the correct level }
procedure TFileOutput.NewLine(const iIndent: integer);
var
  i : integer;
begin
  OutputText( CRLF );
  for i := 1 to iIndent do
    OutputText( TAB );
end;


{ Checks if any invalid characters exist in a piece of text.  If so, then result
    is wrapped in a CDATA construxt }
function TFileOutput.Safetext(const iText: string): string;
begin
  if (Pos('<',iText)=0) and (Pos('>',iText)=0) then
    Result := iText
  else
    Result := '<![CDATA[' + IntToHex(Length(iText), 6) + iText + ']]>';
end;

procedure TFileOutput.SetCancelled(const Value: boolean);
begin
  FCancelled := Value;
end;

end.
