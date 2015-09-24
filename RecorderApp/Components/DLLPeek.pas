unit DLLPeek;

interface

uses
  WinTypes, WinProcs, SysUtils, Classes;

procedure GetDLLList (const FileName: String; List: TStringList);

//==============================================================================
implementation

const
    { Magic numbers }
    DOS_Magic         = $5A4D;    { Magic word for old-style DOS EXE's    }
    W16_Magic         = $454E;    { Magic word for new-style 16-bit EXE's }
    W32_Magic         = $4550;    { Magic word for new-style 32-bit EXE's }

resourcestring
    ResStr_FileNotFound = 'File not found';
    ResStr_FileNotExe = 'File is not a Windows 16-bit (NE) or 32-bit (PE) executable';

type
  Directory = record
    VirtualAddress: LongInt;
    Size: LongInt;
  end;

  PSection = ^Section;
  Section = record
    Name: array [0..7] of Char;  { name of this section }
    VirtualSize: LongInt;        { size of code or data before rounding }
    VirtualAddress: LongInt;     { RVA to where loader should map section }
    SizeOfRawData: LongInt;      { size of code or data after rounding }
    PointerToRawData: LongInt;   { file-based offset to raw data }
    PointerToRelocs: LongInt;    { file-based offset to relocation info }
    PointerToLineNums: LongInt;  { file-based offset to line number table }
    NumRelocs: Word;             { number of relocations in table }
    NumLineNums: Word;           { number of line numbers in table }
    Flags: LongInt;              { flags for this section }
  end;

  ImageImportDescriptor = record
    Hints: LongInt;              { offset of hint import information }
    TimeDataStamp: LongInt;      { time/date stamp of imported file  }
    ForwarderChain: LongInt;     { for DLL call forwarding           }
    Name: LongInt;               { RVA to name of this imported DLL  }
    FirstThunk: LongInt;         { RVA to image_thunk_data info      }
  end;

  PEHeader = record
    Sig: LongInt;                             { $00 }
    Machine: Word;                            { $04 }
    NumSections: Word;                        { $06 }
    TimeDataStamp: LongInt;                   { $08 }
    PointerToSymbolTable: LongInt;            { $0C }
    NumberOfSymbols: LongInt;                 { $10 }
    SizeOfOptionalHeader: Word;               { $14 }
    Characteristics: Word;                    { $16 }
    Magic: Word;                              { $18 }
    MajorLinkerVersion: Byte;                 { $1A }
    MinorLinkerVersion: Byte;                 { $1B }
    SizeOfCode: LongInt;                      { $1C }
    SizeOfInitializedData: LongInt;           { $20 }
    SizeOfUninitializedData: LongInt;         { $24 }
    AddressOfEntryPoint: LongInt;             { $28 }
    BaseOfCode: LongInt;                      { $2C }
    BaseOfData: LongInt;                      { $30 }
    ImageBase: LongInt;                       { $34 }
    SectionAlignment: LongInt;                { $38 }
    FileAlignment: LongInt;                   { $3C }
    MajorOperatingSystemVersion: Word;        { $40 }
    MinorOperatingSystemVersion: Word;        { $42 }
    MajorImageversion: Word;                  { $44 }
    MinorImageversion: Word;                  { $46 }
    MajorSubsystemVersion: Word;              { $48 }
    MinorSubsystemVersion: Word;              { $4A }
    Reserved1: LongInt;                       { $4C }
    SizeOfImage: LongInt;                     { $50 }
    SizeOfHeaders: LongInt;                   { $54 }
    CheckSum: LongInt;                        { $58 }
    Subsystem: Word;                          { $5C }
    DLLCharacteristics: Word;                 { $5E }
    SizeOfStackReserve: LongInt;              { $60 }
    SizeOfStackCommit: LongInt;               { $64 }
    SizeOfHeapReserve: LongInt;               { $68 }
    SizeOfHeapCommit: LongInt;                { $6C }
    LoaderFlags: LongInt;                     { $70 }
    NumberOfRvaAndSizes: LongInt;             { $74 }
    ImageDirs: array [0..15] of Directory;    { $78 }
  end;

//==============================================================================
procedure GetDLLList (const FileName: String; List: TStringList);
var fs: TFileStream;
    HeaderPos: LongInt;
  //----------------------------------------------------------------------------
  function ReadByte: Byte;
  begin
    fs.Read (Result, sizeof (Result));
  end;

  //----------------------------------------------------------------------------
  function ReadWord: Word;
  begin
    fs.Read (Result, sizeof (Result));
  end;

  //----------------------------------------------------------------------------
  function ReadLong: LongInt;
  begin
    fs.Read (Result, sizeof (Result));
  end;

  //----------------------------------------------------------------------------
  function ReadString: String;
  var Idx: Word;
  begin
    SetLength(Result,ReadByte);
    for Idx := 1 to Length(Result) do Result [Idx] := Char (ReadByte);
{      Result [0] := Char (ReadByte);
    for Idx := 1 to Ord (Result [0]) do Result [Idx] := Char (ReadByte);}
  end;

  //----------------------------------------------------------------------------
  function ReadSZString (NewPos: LongInt): String;
  var Ch: Char;
      OldPos: LongInt;
  begin
    Result := '';
    OldPos := fs.Position;
    fs.Position := NewPos;
    repeat
      Ch := Char (ReadByte);
      if Ch <> #0 then Result := Result + Ch;
    until Ch = #0;
    fs.Position := OldPos;
  end;

  //----------------------------------------------------------------------------
  procedure GetDLLList16;
  var Idx, Count: Integer;
      OffsetListPos, ImpTabOffset: LongInt;
  begin
    with fs do begin
      { Point to modtabentries count }
      Position := HeaderPos + $1E;
      Count := ReadWord;
      { If zero, then this file needs no DLL's ! }
      if Count <> 0 then begin
        Position := HeaderPos + $28;
        OffsetListPos := HeaderPos + ReadWord;
        { Now get location of import table }
        ImpTabOffset := HeaderPos + ReadWord;
        { Finally, loop for each item }
        for Idx := 0 to Count - 1 do
        begin
          Position := OffsetListPos + (Idx * sizeof (Word));
          Position := ImpTabOffset + ReadWord;
          List.Add (ReadString);
        end;
      end;
    end;
  end;

  //----------------------------------------------------------------------------
  procedure GetDLLList32;
  var Idx: Integer;
      Hdr: PEHeader;
      ImportAddr: LongInt;
      CurSection, SectionList: PSection;
      ImportDescriptor: ImageImportDescriptor;
  begin
    with fs do begin
      Position := HeaderPos;
      fs.Read (Hdr, sizeof (Hdr));
      ImportAddr := Hdr.ImageDirs [1].VirtualAddress;
      { Allocate memory for section table }
      GetMem (SectionList, sizeof (Section) * Hdr.NumSections);
      fs.Read (SectionList^, sizeof (Section) * Hdr.NumSections);
      try
        CurSection := SectionList;
        { Find the import table section }
        for Idx := 0 to Hdr.NumSections - 1 do
        begin
          if CurSection^.VirtualAddress = ImportAddr then begin
            fs.Position := CurSection^.PointerToRawData;
            { Read and process the Image Import Descriptors }
            repeat
              fs.Read (ImportDescriptor, sizeof (ImportDescriptor));
              if ImportDescriptor.Name = 0 then Break;
              List.Add (ReadSZString (CurSection^.PointerToRawData +
                        ImportDescriptor.Name - CurSection^.VirtualAddress));
            until False;
          end;
          Inc (CurSection);
        end;
      finally
        FreeMem (SectionList, sizeof (Section) * Hdr.NumSections);
      end;
    end;
  end;
  //----------------------------------------------------------------------------
begin
  if not FileExists (FileName) then raise Exception.Create (ResStr_FileNotFound);
  fs := TFileStream.Create (FileName, fmOpenRead or fmShareDenyNone);
  with fs do try
    if ReadWord <> DOS_Magic then raise Exception.Create (ResStr_FileNotExe);
    Position := $3C; Position := ReadLong; HeaderPos := Position;
    case ReadWord of
      W16_Magic: GetDLLList16;
      W32_Magic: GetDLLList32;
      else       raise Exception.Create (ResStr_FileNotExe);
    end;
  finally
    fs.Free;
  end;
end;

//==============================================================================
end.



