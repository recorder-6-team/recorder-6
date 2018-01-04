{Version 9.03}
{*********************************************************}
{*                     HTMLUN2.PAS                       *}
{*              Copyright (c) 1995-2002 by               *}
{*                   L. David Baldwin                    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$i htmlcons.inc}

unit HTMLUn2;

interface
uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Clipbrd, StyleUn;

const
  VersionNo = '9.03';
  HandCursor = 1;
  ThickIBeamCursor = 2;
  UpDownCursor = 3;
  UpOnlyCursor = 4;
  DownOnlyCursor = 5;
  Tokenleng = 300;  
  TopLim = -200;   {drawing limits}
  BotLim = 5000;   
  FmCtl = WideChar(#2);
  ImgPan = WideChar(#4);
  nbSp = WideChar(#5);
  BrkCh = WideChar(#8);

var
  IsWin95: Boolean;
  IsWin32Platform: boolean; {win95, 98, ME}

type
  TScriptEvent = procedure(Sender: TObject; const Name, Language,    
       Script: string) of Object;

  TFreeList = class(TList)
  {like a TList but frees it's items.  Use only descendents of TObject}
    destructor Destroy; override;
    {$Warnings Off}
    procedure Clear;       {do not override}
    end;
    {$Warnings On}

  Transparency = (NotTransp, LLCorner, TGif, TPng);
  JustifyType = (NoJustify, Left, Centered, Right, FullJustify);  

  Symb = (
    HtmlSy, TitleSy, BodySy, HeadSy, PSy, PEndSy, BSy, BEndSy, ISy, IEndSy,
    HtmlEndSy, TitleEndSy, BodyEndSy, HeadEndSy, BRSy, HeadingSy, HeadingEndSy,
    EmSy, EmEndSy, StrongSy, StrongEndSy, USy, UEndSy, HRSy,
    CiteSy, VarSy, CiteEndSy, VarEndSy, BaseSy,
       {Keep order}
    TTSy, CodeSy, KbdSy, SampSy,  TTEndSy, CodeEndSy, KbdEndSy, SampEndSy,
       {end order}
    OLSy, OLEndSy, LISy, ULSy, ULEndSy, DirSy, DirEndSy, MenuSy, MenuEndSy,
    DLSy, DLEndSy, DDSy, DTSy, AddressSy, AddressEndSy, BlockQuoteSy, BlockQuoteEndSy,
    PreSy, PreEndSy, ImageSy, Centersy, CenterEndSy,
    OtherAttribute, ASy, AEndSy, HrefSy, NameSy, SrcSy, AltSy, AlignSy,
    OtherChar, OtherSy, CommandSy, TextSy, EofSy, LinkSy, BGColorSy,
    BackgroundSy, TableSy, TableEndSy, TDSy, TDEndSy, TRSy, TREndSy, THSy, THEndSy,
    ColSpanSy, RowSpanSy, BorderSy, CellPaddingSy, CellSpacingSy, VAlignSy,
    WidthSy, CaptionSy, CaptionEndSy, StartSy, ButtonSy, InputSy, ValueSy,
    TypeSy, CheckBoxSy, RadioSy, FormSy, FormEndSy, MethodSy, ActionSy,
    CheckedSy, SizeSy, MaxLengthSy, TextAreaSy, TextAreaEndSy, ColsSy,
    RowsSy, SelectSy, SelectEndSy, OptionSy, OptionEndSy, SelectedSy,
    MultipleSy, FontSy, FontEndSy, ColorSy, FaceSy, BaseFontSy,
    TranspSy, SubSy, SubEndSy, SupSy, SupEndSy, ClearSy, IsMapSy,
    BigSy, BigEndSy, SmallSy, SmallEndSy, BorderColorSy, MapSy, MapEndSy,
    AreaSy, ShapeSy, CoordsSy, NoHrefSy, UseMapSy, HeightSy, PlainSy,
    FrameSetSy, FrameSetEndSy, FrameSy, TargetSy, NoFramesSy, NoFramesEndSy,
    NoResizeSy, ScrollingSy, PageSy, HSpaceSy, VSpaceSy, ScriptSy, ScriptEndSy,  
    LanguageSy, DivSy, DivEndSy, SSy, SEndSy, StrikeSy, StrikeEndSy,
    FrameBorderSy, MarginWidthSy, MarginHeightSy, BgSoundSy, LoopSy,
    OnClickSy, WrapSy, NoShadeSy, MetaSy, HttpEqSy, ContentSy, EncTypeSy,
    VLinkSy, OLinkSy, ActiveSy, PanelSy, NoBrSy, NoBrEndSy, WbrSy,
    ClassSy, IDSy, StyleSy, StyleEndSy, SpanSy, SpanEndSy, liAloneSy,
    RelSy, RevSy, NoWrapSy, BorderColorLightSy, BorderColorDarkSy,
    CharSetSy, RatioSy, OnFocusSy, OnBlurSy, OnChangeSy, ColSy, ColGroupSy,
    ColGroupEndSy, TabIndexSy, BGPropertiesSy, EolSy);

  TAttribute = class(TObject)  {holds a tag attribute}
  public
    Which: Symb;     {symbol of attribute such as HrefSy}
    Value: integer;  {numeric value if appropriate}
    Percent: boolean;{if value is in percent}
    Name: String;   {String (mixed case), value after '=' sign}
    constructor Create(ASym: Symb; AValue: integer; Const AString: String);
    destructor Destroy; override;
  end;

  TAttributeList = class(TFreeList)  {a list of tag attributes,(TAttributes)}
    private
      Prop: TProperties;
      SaveID: string;
      function GetClass: string;    
      function GetID: string;
      function GetTitle: string;  
      function GetStyle: TProperties;
    public
      destructor Destroy; override;
      procedure Clear;
      function Find(Sy: Symb; var T: TAttribute): boolean;
      property TheClass: string read GetClass;    
      property TheID: string read GetID;
      property TheTitle: string read GetTitle;    
      property TheStyle: TProperties read GetStyle;
    end;

  TBitmapItem = class(TObject)
  public
    AccessCount: integer;
    UsageCount: integer;     {how many in use}
    Transp: Transparency;    {identifies what the mask is for}
    MImage: TPersistent;     {main image, bitmap or animated GIF}
    Mask: TBitmap;  {its mask}
    constructor Create(AImage: TPersistent; AMask: TBitmap; Tr: Transparency);
    destructor Destroy; override;
  end;

  TStringBitmapList = class(TStringList)
      {a list of bitmap filenames and TBitmapItems}
  public
    MaxCache: integer;
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure DecUsage(const S: string);
    procedure IncUsage(const S: string);
    procedure BumpAndCheck;
    procedure PurgeCache;
    function GetImage(I: integer): TPersistent;
    procedure SetCacheCount(N: integer);
  end;

  SelTextCount = class(TObject)
  private
    Buffer: PWideChar;
    BufferLeng: integer;
    Leng: integer;
  public
    procedure AddText(P: PWideChar; Size: integer); virtual;
    procedure AddTextCR(P: PWideChar; Size: integer);
    function Terminate: integer; virtual;
    end;

  SelTextBuf = class(SelTextCount)
  public
    constructor Create(ABuffer: PWideChar; Size: integer);
    procedure AddText(P: PWideChar; Size: integer); override;
    function Terminate: integer; override;
    end;

  ClipBuffer = class(SelTextBuf)
  public
    constructor Create(Leng: integer);  
    destructor Destroy; override;
    function Terminate: integer; override;
    end;

  TUrlTarget = Class(TObject)
    URL,
    Target: string;
    ID: integer;
    TabIndex: integer;    
    constructor Create;
    procedure Copy(UT: TUrlTarget);
    destructor Destroy; override;
    procedure Assign(AnUrl, ATarget: String);  
    procedure Clear;
    end;

  TMapItem = class(TObject)   {holds a client map info}
    MapName: String;
    Areas: TStringList;       {holds the URL and region handle}
    AreaTargets: TStringList; {holds the target window}
    constructor Create;
    destructor Destroy; override;
    function GetURL(X, Y: integer; var URLTarg: TURLTarget): boolean;
    procedure AddArea(Attrib: TAttributeList);
    end;

  TDib = class(TObject)
    private
      Info  : PBitmapInfoHeader;
      InfoSize: integer;
      Image: Pointer;
      ImageSize : integer;
      FHandle: THandle;
      procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP);
      procedure GetDIBX(DC: HDC; Bitmap: HBITMAP; Palette: HPALETTE);
      procedure Allocate(Size: integer);
      procedure DeAllocate;
    public
      constructor CreateDIB(DC: HDC; Bitmap: TBitmap);
      destructor Destroy; override;
      function CreateDIBmp: hBitmap;
      procedure DrawDIB(DC: HDC; X: Integer; Y: integer; W, H: integer;
                ROP: DWord);
    end;

  IndentRec = Class(TObject)
    X: integer;       {indent for this record}
    YT, YB: integer;  {top and bottom Y values for this record}
    ID: TObject;      {level inicator for this record, 0 for not applicable}
    Float: boolean;   {set if Floating block boundary}
    end;

  IndentManagerBasic = class(TObject)
    Width, ClipWidth: Integer;
    L, R: TFreeList;  {holds info (IndentRec's) on left and right indents}
    CurrentID: TObject;       {the current level (a TBlock pointer)}
    LfEdge, RtEdge: integer;     {current extreme edges}

    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Reset(Lf, Rt: integer);
    procedure UpdateTable(Y: integer; IW: integer; IH: integer; Justify: JustifyType);
    function LeftIndent(Y: integer): integer;
    function RightSide(Y: integer): integer;
    function ImageBottom: integer;
    procedure GetClearY(var CL, CR: integer);
    function GetNextWiderY(Y: integer): integer;
    function SetLeftIndent(XLeft, Y: integer): integer;
    function SetRightIndent(XRight, Y: integer): integer;
    procedure FreeLeftIndentRec(I: integer);   
    procedure FreeRightIndentRec(I: integer);   
    end;

  AllocRec = Class(TObject)
    Ptr: Pointer;
    ASize: integer;
    AHandle: THandle;
    end;

  IndexArray = array[1..TokenLeng] of integer;
  PIndexArray = ^IndexArray;
  ChrArray = array[1..TokenLeng] of WideChar;     

  {Simplified variant of TokenObj, to temporarily keep a string of ANSI
   characters along with their original indices.}
  TCharCollection = class
  private
    FChars: string;
    FIndices: PIndexArray;
    FCurrentIndex: Integer;
    function GetSize: Integer;
    function GetAsString: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(C: Char; Index: Integer);
    procedure Clear;
    procedure Concat(T: TCharCollection);

    property AsString: string read GetAsString;
    property Chars: string read FChars;
    property Indices: PIndexArray read FIndices;
    property Size: Integer read GetSize;
  end;

  TokenObj= class
  private
    St: WideString;
    StringOK: boolean;
    function GetString: WideString;
  public
    C: ^ChrArray;
    I: ^IndexArray;
    MaxIndex, Leng: integer;
    constructor Create;
    destructor Destroy; override;
    procedure AddUnicodeChar(Ch: WideChar; Ind: integer);
    procedure AddString(S: TCharCollection; CodePage: Integer);
    procedure Concat(T: TokenObj);
    procedure Clear;
    procedure Remove(N: integer);
    procedure Replace(N: integer; Ch: WideChar);

    property S: WideString read GetString;
    end;

  TIDObject = class(TObject)
  protected
    function GetYPosition: integer; virtual; abstract;
  public
    property YPosition: integer read GetYPosition;
    destructor Destroy; override;
    end;

  TChPosObj = class (TIDObject)
  private
    ChPos: integer;
    List: TList;
  protected
    function GetYPosition: integer; override;
    end;

  TIDNameList = class(TStringList)
  private
    OwnerList: TList;
  public
    constructor Create(List: TList);
    destructor Destroy; override;
    procedure Clear; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure AddChPosObject(const S: string; Pos: integer);
    end;

  ImageType = (NoImage, Bmp, Gif, Gif89, Png, Jpg);
  SetOfChar = Set of Char;

var
  ColorBits: Byte;
  ThePalette: HPalette;       {the rainbow palette for 256 colors}
  DefBitMap, ErrorBitMap, ErrorBitmapMask: TBitMap;
  ABitmapList: TStringBitmapList; {the image cache}
  WaitStream: TMemoryStream;

function InSet(W: WideChar; S: SetOfChar): boolean;

function StrLenW(Str: PWideChar): Cardinal;
function StrPosW(Str, SubStr: PWideChar): PWideChar;
function StrScanW(const Str: PWideChar; Chr: WideChar): PWideChar;
function StrRScanW(const Str: PWideChar; Chr: WideChar): PWideChar;
function FitText(DC: HDC; S: PWideChar; Max, Width: Integer; var Extent: integer): Integer;
function WidePos(SubStr, S: WideString): Integer;
function WideTrim(const S : WideString) : WideString;
function WideUpperCase1(const S: WideString): WideString;
function WideLowerCase1(const S: WideString): WideString;
function WideSameText1(const S1, S2: WideString): boolean;
function WideSameStr1(const S1, S2: WideString): boolean;

function IntMin(A, B: Integer): Integer;
function IntMax(A, B: Integer): Integer;

function GetImageFromFile(const Filename: string): TBitmap;
function GetImageAndMaskFromFile(const Filename: string; var Transparent: Transparency;
                            var Mask: TBitmap): TBitmap;
function HTMLToDos(FName: string): string;
  {convert an HTML style filename to one for Dos}
function HTMLServerToDos(FName, Root: string): string;

procedure WrapText(Canvas: TCanvas; X1, Y1, X2, Y2: integer; S: string);

procedure FinishTransparentBitmap (ahdc: HDC;
            InImage, Mask: TBitmap; xStart, yStart, W, H: integer);  
function GetImageMask(Image: TBitmap; ColorValid: boolean; AColor: TColor): TBitmap;
function TransparentGIF(const FName: string; var Color: TColor): boolean;
function Allocate(Size: integer): AllocRec;
procedure DeAllocate(AR: AllocRec);
function CopyPalette(Source: hPalette): hPalette;
procedure SetGlobalPalette(Value: HPalette);
function GetImageAndMaskFromStream(Stream: TMemoryStream;
        var Transparent: Transparency; var AMask: TBitmap): TBitmap;
function KindOfImage(Start: Pointer): ImageType;
procedure FormControlRect(Canvas: TCanvas; X1: integer;
           Y1: integer; X2: integer; Y2: integer; Raised, PrintMonoBlack: boolean);  
function GetXExtent(DC: HDC; P: PWideChar; N: integer): integer;
procedure RaisedRect(SectionList: TFreeList; Canvas: TCanvas; X1: integer;
           Y1: integer; X2: integer; Y2: integer; Raised: boolean);
procedure RaisedRectColor(SectionList: TFreeList; Canvas: TCanvas; X1: integer;
           Y1: integer; X2: integer; Y2: integer; Light, Dark: TColor; Raised: boolean);
procedure PrintBitmap(Canvas: TCanvas; X, Y, W, H: integer;
             BMHandle: HBitmap);
procedure PrintTransparentBitmap1(Canvas: TCanvas; X, Y, NewW, NewH: integer;
             Bitmap, Mask: TBitmap; YI, HI: integer);
procedure PrintTransparentBitmap3(Canvas: TCanvas; X, Y, NewW, NewH: integer;
             Bitmap, Mask: TBitmap; YI, HI: integer);

implementation

uses
   jpeg, DitherUnit,
  {$ifndef NoPNG}
     PngImage1,   
  {$endif}
     htmlview, htmlsubs, HtmlGif2, StylePars;

type
  TJpegMod = class(TJpegImage)
  public
    property Bitmap;
  end;

var
  DC: HDC;

{----------------StrLenW}
function StrLenW(Str: PWideChar): Cardinal;
{returns number of characters in a string excluding the null terminator}

asm
       MOV     EDX, EDI
       MOV     EDI, EAX
       MOV     ECX, 0FFFFFFFFH
       XOR     AX, AX
       REPNE   SCASW
       MOV     EAX, 0FFFFFFFEH
       SUB     EAX, ECX
       MOV     EDI, EDX

end;

{----------------StrPosW}
function StrPosW(Str, SubStr: PWideChar): PWideChar;
// returns a pointer to the first occurance of SubStr in Str
asm
       PUSH    EDI
       PUSH    ESI
       PUSH    EBX
       OR      EAX, EAX
       JZ      @@2
       OR      EDX, EDX
       JZ      @@2
       MOV     EBX, EAX
       MOV     EDI, EDX
       XOR     AX, AX
       MOV     ECX, 0FFFFFFFFH
       REPNE   SCASW
       NOT     ECX
       DEC     ECX
       JZ      @@2
       MOV     ESI, ECX
       MOV     EDI, EBX
       MOV     ECX, 0FFFFFFFFH
       REPNE   SCASW
       NOT     ECX
       SUB     ECX, ESI
       JBE     @@2
       MOV     EDI, EBX
       LEA     EBX, [ESI - 1]
@@1:
       MOV     ESI, EDX
       LODSW
       REPNE   SCASW
       JNE     @@2
       MOV     EAX, ECX
       PUSH    EDI
       MOV     ECX, EBX
       REPE    CMPSW
       POP     EDI
       MOV     ECX, EAX
       JNE     @@1
       LEA     EAX, [EDI - 2]
       JMP     @@3

@@2:
       XOR     EAX, EAX
@@3:
       POP     EBX
       POP     ESI
       POP     EDI
end;

{----------------StrRScanW}
function StrRScanW(const Str: PWideChar; Chr: WideChar): PWideChar; assembler;
asm
        PUSH    EDI
        MOV     EDI,Str
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        NOT     ECX
        STD
        DEC     EDI
        DEC     EDI
        MOV     AX,Chr
        REPNE   SCASW
        MOV     EAX,0
        JNE     @@1
        MOV     EAX,EDI
        INC     EAX
        INC     EAX
@@1:    CLD
        POP     EDI
end;

{----------------StrScanW}
function StrScanW(const Str: PWideChar; Chr: WideChar): PWideChar; assembler;
asm
        PUSH    EDI
        PUSH    EAX
        MOV     EDI,Str
        MOV     ECX,$FFFFFFFF
        XOR     AX,AX
        REPNE   SCASW
        NOT     ECX
        POP     EDI
        MOV     AX,Chr
        REPNE   SCASW
        MOV     EAX,0
        JNE     @@1
        MOV     EAX,EDI
        DEC     EAX
        DEC     EAX
@@1:    POP     EDI
end;

{----------------FitText}
function FitText(DC: HDC; S: PWideChar; Max, Width: Integer; var Extent: integer): Integer;
  {return count <= Max which fits in Width.  Return X, the extent of chars that fit}

type
  Integers = array[1..1] of integer;
var
  ExtS: TSize;
  Ints: ^Integers;
  L, H, I: integer;

begin
if not IsWin32Platform then   
  begin
  Extent := 0;
  Result := 0;
  if (Width <= 0) or (Max = 0) then
    Exit;
  GetMem(Ints, Sizeof(Integer)* Max);
  try
    {$ifdef ver120_plus}
    if GetTextExtentExPointW(DC, S, Max, Width, @Result, @Ints^, ExtS) then
    {$else}
    if GetTextExtentExPointW(DC, S, Max, Width, Result, Integer(Ints^), ExtS) then
    {$endif}
      Extent := Ints^[Result];
  finally
    FreeMem(Ints);
    end;
  end
else       {GetTextExtentExPointW not available in win98, 95}
  begin          {optimize this by looking for Max to fit first -- it usually does}
  L := 0;
  H := Max;
  I := H;
  while L <= H do
    begin
    GetTextExtentPoint32W(DC, S, I, ExtS);
    if ExtS.cx < Width then
      L := I+1
    else H := I-1;
    if ExtS.cx = Width then
      Break;
    I := (L+H) shr 1;
    end;
  Result := I;
  Extent := ExtS.cx;
  end;
end;

{----------------WidePos}
function WidePos(SubStr, S: WideString): Integer;
// Unicode equivalent for Pos() function.
var
  P: PWideChar;
begin
  P := StrPosW(PWideChar(S), PWideChar(SubStr));
  if P = nil then
    Result := 0
  else
    Result := P - PWideChar(S) + 1;
end;

{----------------WideUpperCase1}
function WideUpperCase1(const S: WideString): WideString;
var
  Len, NewLen: Integer;
  Tmp: string;
begin
Len := Length(S);
if not IsWin32Platform then
  begin
  SetString(Result, PWideChar(S), Len);
  if Len > 0 then CharUpperBuffW(Pointer(Result), Len);
  end
else
  begin   {win95,98,ME}
  SetLength(Tmp, 2*Len);
  NewLen := WideCharToMultiByte(CP_ACP, 0, PWideChar(S), Len, PChar(Tmp), 2*Len, Nil, Nil);
  SetLength(Tmp, NewLen);
  Tmp := AnsiUppercase(Tmp);
  SetLength(Result, Len);
  MultibyteToWideChar(CP_ACP, 0, PChar(Tmp), NewLen, PWideChar(Result), Len);
  end;
end;

function WideLowerCase1(const S: WideString): WideString;    
var
  Len, NewLen: Integer;
  Tmp: string;
begin
Len := Length(S);
if not IsWin32Platform then
  begin
  SetString(Result, PWideChar(S), Len);
  if Len > 0 then CharLowerBuffW(Pointer(Result), Len);
  end
else
  begin   {win95,98,ME}
  SetLength(Tmp, 2*Len);
  NewLen := WideCharToMultiByte(CP_ACP, 0, PWideChar(S), Len, PChar(Tmp), 2*Len, Nil, Nil);
  SetLength(Tmp, NewLen);
  Tmp := AnsiLowercase(Tmp);
  SetLength(Result, Len);
  MultibyteToWideChar(CP_ACP, 0, PChar(Tmp), NewLen, PWideChar(Result), Len);
  end;
end;

function WideSameText1(const S1, S2: WideString): boolean;
begin
Result := WideUpperCase1(S1) = WideUpperCase1(S2);
end;

function WideSameStr1(const S1, S2: WideString): boolean;
begin
Result := S1 = S2;
end;

function IntMin(A, B: Integer): Integer;
asm
  cmp edx, eax
  jnle @1
  mov eax, edx
@1:
end;

Function IntMax(A, B : Integer) : Integer;
asm
  cmp edx, eax
  jl @1
  mov eax, edx
@1:
end;

function HTMLServerToDos(FName, Root: string): string;  
{Add Prefix Root only if first character is '\' but not '\\'}
begin
Result := Trim(HTMLToDos(FName));
if (Result <> '') and (Root <> '') then
  begin
  if Pos('\\', Result) = 1 then
    Exit;
  if Pos(':', Result) = 2 then
    Exit;
  if Result[1] = '\' then
    Result := Root+Result;
  end;
end;

function HTMLToDos(FName: string): string;
{convert an HTML style filename to one for Dos}
var
  I: integer;

  procedure Replace(Old, New: char);
  var
    I: integer;
  begin
  I := Pos(Old, FName);
  while I > 0 do
    begin
    FName[I] := New;
    I := Pos(Old, FName);
    end;
  end;

  procedure ReplaceEscapeChars;  
  var
    S: string[3];
    I: integer;
  begin
  I := Pos('%', FName);
  while (I > 1) and (I <= Length(FName)-2) do
    begin
    S := '$'+FName[I+1]+FName[I+2];
    try
      FName[I] := chr(StrToInt(S));
      Delete(FName, I+1, 2);
    except   {ignore exception}
      Exit;
      end;
    I := Pos('%', FName);
    end;
  end;

begin
ReplaceEscapeChars; 
I := pos('/', FName);
if I <> 0 then
  begin
  I := Pos('file:///', Lowercase(FName));
  if I > 0 then
    System.Delete(FName, I, 8)
  else
    begin
    I := Pos('file://', Lowercase(FName));
    if I > 0 then System.Delete(FName, I, 7);
    end;
  Replace('|', ':');
  Replace('/', '\');
  end;
Result := FName;
end;

function WideTrim(const S: WideString): WideString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

procedure WrapText(Canvas: TCanvas; X1, Y1, X2, Y2: integer; S: string);
{Wraps text in a clipping rectangle. Font must be set on entry}
var
  S1, S2: string;
  I, Y, Width, Step: integer;
  ARect: TRect;
  SaveStyle: TBrushStyle;
begin
with Canvas do
  begin
  SaveStyle := Brush.Style;  
  Brush.Style := bsClear;   
  ARect := Rect(X1, Y1, X2, Y2);
  Width := X2 - X1;                                
  Y := Y1;
  Step := TextHeight('A');
  SetTextAlign(Canvas.Handle, TA_Top);
  S1 := '';
  while (Length(S) > 0) and (Y+Step <= Y2) do
    begin
    S := Trim(S);   {in case of extra spaces}
    I := Pos(' ', S);
    if I > 0 then
      begin
      S2 := Copy(S, 1, I-1);
      Delete(S, 1, I);
      end
    else
      Begin
      S2 := S;
      S := '';
      end;
    if TextWidth(S1+S2) <= Width then
      begin
      S1 := S1+S2+' ';
      end
    else
      begin
      if S1 <> '' then
        begin
        TextRect(ARect, X1, Y, S1);
        Inc(Y, Step);
        end;
      S1 := S2+' ';
      end;
    end;
  if (S1 <> '') and (Y+Step <= Y2) then
    TextRect(ARect, X1, Y, S1);
  Brush.Style := SaveStyle;
  end;
end;

function Allocate(Size: integer): AllocRec;
begin
Result := AllocRec.Create;
with Result do
  begin
  ASize := Size;
  if Size < $FF00 then
    GetMem(Ptr, Size)
  else
     begin
     AHandle := GlobalAlloc(HeapAllocFlags, Size);
     if AHandle = 0 then
         ABort;
     Ptr := GlobalLock(AHandle);
     end;
  end;
end;

procedure DeAllocate(AR: AllocRec);
begin
with AR do
  if ASize < $FF00 then
    Freemem(Ptr, ASize)
  else
    begin
    GlobalUnlock(AHandle);
    GlobalFree(AHandle);
    end;
AR.Free;
end;

function GetXExtent(DC: HDC; P: PWideChar; N: integer): integer;
var
  ExtS: TSize;
  Dummy: integer;  
 {$ifndef ver120_plus}
   NilP: integer absolute 0;
 {$endif}

begin
if not IsWin32Platform then   
  begin
  {$ifdef ver120_plus}
  GetTextExtentExPointW(DC, P, N, 0, @Dummy, Nil, ExtS);
  {$else} {do Nil the hard way for Delphi 3}
  GetTextExtentExPointW(DC, P, N, 0, Dummy, NilP, ExtS);
  {$endif}
  end
else
  begin   {win95, 98 ME}
  {$ifdef ver120_plus}
  GetTextExtentPoint32W(DC, P, N, ExtS);
  {$else} {do Nil the hard way for Delphi 3}
  GetTextExtentPoint32W(DC, P, N, ExtS);
  {$endif}
  end;
Result := ExtS.cx;
end;

procedure FormControlRect(Canvas: TCanvas; X1: integer;
           Y1: integer; X2: integer; Y2: integer; Raised, PrintMonoBlack: boolean);  
{Draws lowered rectangles for form control printing}
var
  OldStyle: TPenStyle;
  OldWid: integer;
  OldBrushStyle: TBrushStyle;
  OldBrushColor: TColor;
  MonoBlack: boolean;     
begin
with Canvas do
  begin
  MonoBlack := PrintMonoBlack and (GetDeviceCaps(Handle, BITSPIXEL) = 1) and     
          (GetDeviceCaps(Handle, PLANES) = 1);
  Dec(X2);  Dec(Y2);
  OldWid := Pen.Width;
  OldStyle := Pen.Style;
  OldBrushStyle := Brush.Style;   {save style first}
  OldBrushColor := Brush.Color;
  Brush.Color := clWhite;
  Brush.Style := bsSolid;
  FillRect(Rect(X1, Y1, X2, Y2));
  Brush.Color := OldBrushColor;
  Brush.Style := OldBrushStyle;    {style after color as color changes style}

  Pen.Style := psInsideFrame;
  if MonoBlack then
    begin
    Pen.Width := 1;
    Pen.Color := clBlack;
    end
  else
    begin
    Pen.Width := 2;
    if Raised then Pen.Color := clSilver
      else Pen.Color := clBtnShadow;
    end;
  MoveTo(X1, Y2);
  LineTo(X1, Y1);
  LineTo(X2, Y1);
  if not MonoBlack then
    if Raised then Pen.Color := clBtnShadow
      else Pen.Color := clSilver;
  LineTo(X2, Y2);
  LineTo(X1, Y2);
  Pen.Style := OldStyle;
  Pen.Width := OldWid;
  end;
end;

var
  ASectionList: TSectionList;
procedure RaisedRect(SectionList: TFreeList; Canvas: TCanvas; X1: integer;
           Y1: integer; X2: integer; Y2: integer; Raised: boolean);
{Draws raised or lowered rectangles for table borders}
var
  White, BlackBorder: boolean;
  OldStyle: TBrushStyle;
begin
Y1 := IntMax(Y1, TopLim);
Y2 := IntMin(Y2, BotLim);
with Canvas do
  begin
  OldStyle := Brush.Style;    
  Brush.Style := bsSolid;     {seems to be required by at least one printer, Canon BJ-200e}
  with SectionList as TSectionList do
    begin   ASectionList := TSectionlist(SectionList);
    White := Printing or ((Background and $FFFFFF = clWhite) or
        ((Background = clWindow) and (GetSysColor(Color_Window) = $FFFFFF)));  
    BlackBorder := Printing and PrintMonoBlack and (GetDeviceCaps(Handle, BITSPIXEL) = 1) and
        (GetDeviceCaps(Handle, PLANES) = 1);    
    end;
  if BlackBorder then
    begin
    Pen.Color := clBlack;
    end
  else if Raised then
    if White then
      Pen.Color := clSilver
    else Pen.Color := clBtnHighLight
  else Pen.Color := clBtnShadow;

  MoveTo(X1, Y2);
  LineTo(X1, Y1);
  LineTo(X2, Y1);
  if BlackBorder then        
    Pen.Color := clBlack
  else if not Raised then
    if White then
      Pen.Color := clSilver
    else Pen.Color := clBtnHighLight
  else Pen.Color := clBtnShadow;
  LineTo(X2, Y2);
  LineTo(X1, Y2);
  Brush.Style := OldStyle;   
  end;
end;

procedure RaisedRectColor(SectionList: TFreeList; Canvas: TCanvas; X1: integer;
           Y1: integer; X2: integer; Y2: integer; Light, Dark: TColor; Raised: boolean);
{Draws colored raised or lowered rectangles for table borders}
begin
Y1 := IntMax(Y1, TopLim);
Y2 := IntMin(Y2, BotLim);
with Canvas do
  begin
  if Raised then
    Pen.Color := Light
  else Pen.Color := Dark;

  MoveTo(X1, Y2);
  LineTo(X1, Y1);
  LineTo(X2, Y1);
  if not Raised then
    Pen.Color := Light
  else Pen.Color := Dark;
  LineTo(X2, Y2);
  LineTo(X1, Y2); 
  end;
end;

{$ifdef Ver90}
procedure Assert(B: boolean; const S: string);
begin   {dummy Assert for Delphi 2}
end;
{$endif}

destructor TFreeList.Destroy;
var
  I: integer;
begin
for I := 0 to Count-1 do
  TObject(Items[I]).Free;
inherited Destroy;
end;

procedure TFreeList.Clear;
var
  I: integer;
begin
for I := 0 to Count-1 do
  TObject(Items[I]).Free;
inherited Clear;
end;

constructor TBitmapItem.Create(AImage:TPersistent; AMask: TBitmap; Tr: Transparency);
begin
inherited Create;
MImage := AImage;
Mask := AMask;
AccessCount := 0;
Transp := Tr;
end;

destructor TBitmapItem.Destroy;
begin
Assert(UsageCount = 0, 'Freeing Image still in use'); 
MImage.Free;
Mask.Free;
inherited Destroy;
end;

constructor TStringBitmapList.Create;
begin
inherited Create;
MaxCache := 4;
end;

destructor TStringBitmapList.Destroy;
var
  I: integer;
begin
for I := 0 to Count-1 do
  (Objects[I] as TBitmapItem).Free;
inherited Destroy;
end;

function TStringBitmapList.AddObject(const S: string; AObject: TObject): Integer;
begin
Result := inherited AddObject(S, AObject);
if AObject is TBitmapItem then
  Inc(TBitmapItem(AObject).UsageCount);
end;

procedure TStringBitmapList.DecUsage(const S: string);
var
  I: integer;
begin
I := IndexOf(S);
if I >= 0 then
  with Objects[I] as TBitmapItem do
    begin
    Dec(UsageCount);
    Assert(UsageCount >= 0, 'Cache image usage count < 0');  
    end;
end;

procedure TStringBitmapList.IncUsage(const S: string);
var
  I: integer;
begin
I := IndexOf(S);
if I >= 0 then
  with Objects[I] as TBitmapItem do
    Inc(UsageCount);
end;

procedure TStringBitmapList.SetCacheCount(N: integer);
var
  I: integer;
begin
for I := Count-1 downto 0 do
  with (Objects[I] as TBitmapItem)do
    begin
    if (AccessCount > N) and (UsageCount <= 0) then     
      begin
      Delete(I);
      Free;
      end;
    end;
MaxCache := N;
end;

function TStringBitmapList.GetImage(I: integer): TPersistent;
begin
with Objects[I] as TBitmapItem do
  begin
  Result := MImage;
  AccessCount := 0;
  Inc(UsageCount);
  end;
end;

procedure TStringBitmapList.BumpAndCheck;
var
  I: integer;
  Tmp: TBitmapItem;
begin
  for I := Count-1 downto 0 do
    begin
    Tmp := (Objects[I] as TBitmapItem);
    with Tmp do
      begin
      Inc(AccessCount);
      if (AccessCount > MaxCache) and (UsageCount <= 0) then
        begin
        Delete(I);
        Free;          {the TBitmapItem}
        end;
      end;
    end;
end;

procedure TStringBitmapList.PurgeCache;
var
  I: integer;
  Tmp: TBitmapItem;
begin
for I := Count-1 downto 0 do
  begin
  Tmp := (Objects[I] as TBitmapItem);
  with Tmp do
    begin
    if (UsageCount <= 0) then
      begin
      Delete(I);
      Free;          {the TBitmapItem}
      end;
    end;
  end;
end;

procedure TStringBitmapList.Clear;
var
  I: integer;
begin
for I := 0 to Count-1 do
  (Objects[I] as TBitmapItem).Free;
inherited Clear;
end;

constructor TAttribute.Create(ASym: Symb; AValue: integer; Const AString: String);
begin
inherited Create;
Which := ASym;
Value := AValue;
Name := AString;
end;

destructor TAttribute.Destroy;
begin
inherited Destroy;
end;

{----------------TAttributeList}
destructor TAttributeList.Destroy;
begin
Prop.Free;
inherited;
end;

procedure TAttributeList.Clear;
begin
SaveID := '';
inherited Clear;
end;

function TAttributeList.Find(Sy: Symb; var T: TAttribute): boolean;
var
  I: integer;
begin
for I := 0 to Count-1 do
  if TAttribute(Items[I]).which = Sy then
    begin
    Result := True;
    T := Items[I];
    Exit;
    end;
Result := False;
end;

function TAttributeList.GetClass: string;
var
  T: TAttribute;
begin
if Find(ClassSy, T) then
  Result := Lowercase(T.Name)
else Result := '';
end;

function TAttributeList.GetID: string;
var
  T: TAttribute;
begin
Result := SaveID;
if (Result = '') and Find(IDSy, T) then
  begin
  Result := Lowercase(T.Name);
  SaveID := Result;
  end;
end;

function TAttributeList.GetTitle: string;    
var
  T: TAttribute;
begin
if Find(TitleSy, T) then
  Result := T.Name
else Result := '';
end;

function TAttributeList.GetStyle: TProperties;
var
  T: TAttribute;
begin
if Find(StyleSy, T) then
  begin
  Prop.Free;
  Prop := TProperties.Create;
  Result := Prop;
  ParsePropertyStr(T.Name, Result);
  end
else Result := Nil;
end;

{----------------TUrlTarget.Create}
constructor TUrlTarget.Create;
begin
inherited Create;
end;

destructor TUrlTarget.Destroy;
begin
inherited Destroy;
end;

var
  Sequence: integer = 10;

procedure TUrlTarget.Assign(AnUrl, ATarget: String);
begin
Url := AnUrl;
Target := ATarget;
ID := Sequence;
Inc(Sequence);
end;

procedure TUrlTarget.Copy(UT: TUrlTarget);
begin
Url := UT.Url;
Target := UT.Target;
ID := UT.ID;
TabIndex := UT.TabIndex;
end;

procedure TUrlTarget.Clear;
begin
Url := '';
Target := '';
ID := 0;
TabIndex := 0;
end;

{----------------SelTextCount}
procedure SelTextCount.AddText(P: PWideChar; Size: integer);
var
  I: integer;
begin
for I := 0 to Size-1 do
  if not (P[I] in [FmCtl, ImgPan]) then {ImgPan and FmCtl used to mark images, form controls}
    Inc(Leng);
end;

procedure SelTextCount.AddTextCR(P: PWideChar; Size: integer);
begin
AddText(P, Size);
AddText(^M^J, 2);
end;

function SelTextCount.Terminate: integer;
begin
Result := Leng;  
end;

{----------------SelTextBuf.Create}
constructor SelTextBuf.Create(ABuffer: PWideChar; Size: integer);
begin
inherited Create;
Buffer := ABuffer;
BufferLeng := Size;
end;

procedure SelTextBuf.AddText(P: PWideChar; Size: integer);
var
  SizeM1 : integer;
  I: integer;
begin
SizeM1 := BufferLeng-1;
for I := 0 to Size-1 do
  if not (P[I] in [FmCtl, ImgPan, BrkCh]) then {ImgPan and FmCtl used to mark images, form controls}
    if Leng < SizeM1 then
      begin
      Buffer[Leng] := P[I];
      Inc(Leng);
      end;
end;

function SelTextBuf.Terminate: integer;
begin
Buffer[Leng] := #0;
Result := Leng+1;
end;

{----------------ClipBuffer.Create}
constructor ClipBuffer.Create(Leng: integer);   
begin
inherited Create(Nil, 0);
BufferLeng := Leng;
Getmem(Buffer, BufferLeng*2);
end;

destructor ClipBuffer.Destroy;
begin
if Assigned(Buffer) then FreeMem(Buffer);  
inherited Destroy;
end;

function ClipBuffer.Terminate: integer;
begin
Buffer[Leng] := #0;
Result := Leng+1;
Clipboard.AsText := Buffer;
end;

{----------------TMapItem.Create}
constructor TMapItem.Create;
begin
inherited Create;
Areas := TStringList.Create;
AreaTargets := TStringList.Create;
end;

destructor TMapItem.Destroy;
var
  I: integer;
begin
for I := 0 to Areas.Count-1 do
  DeleteObject(THandle(Areas.Objects[I]));
Areas.Free;
AreaTargets.Free;
inherited Destroy;
end;

function TMapItem.GetURL(X, Y: integer; var URLTarg: TUrlTarget): boolean;
var
  I: integer;
begin
Result := False;
with Areas do
  for I := 0 to Count-1 do
    if PtInRegion(THandle(Objects[I]), X, Y) then
      begin
      if Strings[I] <> '' then  {could be NoHRef}    
        begin
        URLTarg := TUrlTarget.Create;
        URLTarg.URL := Strings[I];
        URLTarg.Target := AreaTargets[I];
        Result := True;
        end;
      Exit;
      end;
end;

procedure TMapItem.AddArea(Attrib: TAttributeList);
Const
  MAXCNT = 300;  
var
  I, Cnt, Rad: integer;
  HRef, S, Target: string;
  S1, Nm: string[20];
  Coords: array[0..MAXCNT] of integer;
  Rect: TRect absolute Coords;
  Handle: THandle;
  Shape: (Rec, Circle, Poly);

  procedure GetSubStr;    
  var
    J,K: integer;
  begin
  J := Pos(',', S);
  K := Pos(' ', S);   {for non comma situations (bad syntax)}
  if (J > 0) and ((K = 0) or (K > J)) then
    begin
    S1 := copy(S, 1, J-1);
    Delete(S, 1, J);
    end
  else if K > 0 then
    begin
    S1 := copy(S, 1, K-1);
    Delete(S, 1, K);
    end
  else
    begin
    S1 := Trim(S);
    S := '';
    end;
  while (Length(S) > 0) and ((S[1]=',') or (S[1]=' ')) do
    Delete(S, 1, 1);
  end;

begin
HRef := '';
Target := '';
Shape := Rec;
Cnt := 0;
Handle := 0;
for I := 0 to Attrib.Count-1 do
  with TAttribute(Attrib[I]) do
    case Which of
      HRefSy:  HRef := Name;
      TargetSy:  Target := Name;
      NoHrefSy: HRef := '';
      CoordsSy:
        begin
        S := Trim(Name);
        Cnt := 0;
        GetSubStr;
        while (S1 <> '') and (Cnt <= MAXCNT) do
          begin
          Coords[Cnt] := StrToIntDef(S1, 0);
          GetSubStr;
          Inc(Cnt);
          end;
        end;
      ShapeSy:
        begin
        Nm := copy(Lowercase(Name),1, 4);
        if Nm = 'circ' then Shape := Circle
        else if (Nm = 'poly') then Shape := Poly;
        end;
      end;
case Shape of
  Rec:
    begin
    if Cnt < 4 then Exit;
    Inc(Coords[2]);
    Inc(Coords[3]);
    Handle := CreateRectRgnIndirect(Rect);
    end;
  Circle:
    begin
    if Cnt < 3 then Exit;
    Rad := Coords[2];
    Dec(Coords[0],Rad);
    Dec(Coords[1],Rad);
    Coords[2] := Coords[0] + 2*Rad +1;
    Coords[3] := Coords[1] + 2*Rad +1;
    Handle := CreateEllipticRgnIndirect(Rect);
    end;
  Poly:
    begin
    if Cnt < 6 then Exit;
    Handle := CreatePolygonRgn(Coords, Cnt div 2, Winding);
    end;
  end;
if Handle <> 0 then
  begin
  Areas.AddObject(HRef, TObject(Handle));
  AreaTargets.Add(Target);
  end;
end;

function KindOfImage(Start: Pointer): ImageType;
type
  ByteArray = array[0..10] of byte;
var
  PB: ^ByteArray absolute Start;
  PW: ^Word absolute Start;
  PL: ^DWord absolute Start;
begin
if PL^ = $38464947 then
  begin
  if PB^[4] = Ord('9') then Result := Gif89
  else Result := Gif;
  end
else if PW^ = $4D42 then Result := Bmp
else if PL^ = $474E5089 then Result := Png
else if PW^ = $D8FF then Result := Jpg
else Result := NoImage;
end;

{$A-} {record field alignment off for this routine}

function IsTransparent(Stream: TStream; var Color: TColor): boolean;
{Makes some simplifying assumptions that seem to be generally true for single
 images.}
Type
  RGB = record
    Red, Green, Blue: byte;
    end;

  GifHeader = record
    GIF: array[0..2] of char;
    Version: array[0..2] of char;
    ScreenWidth, ScreenHeight: Word;
    Field: Byte;
    BackGroundColorIndex: byte;
    AspectRatio: byte;
    end;
  ColorArray = array[0..255] of RGB;

var
  Header: ^GifHeader;
  X: integer;
  Colors: ^ColorArray;
  Buff: array[0..Sizeof(GifHeader)+Sizeof(ColorArray)+8] of byte;
  P: PChar;
  OldPosition: integer;

begin
Result := False;
Fillchar(Buff, Sizeof(Buff), 0);  {in case read comes short}
OldPosition := Stream.Position;
Stream.Position := 0;
Stream.Read(Buff, Sizeof(Buff));
Stream.Position := OldPosition;

Header := @Buff;
if KindOfImage(Header) <> Gif89 then Exit; 
Colors := @Buff[Sizeof(GifHeader)];
with Header^ do
  begin
  X := 1 shl ((Field and 7) +1) - 1;  {X is last item in color table}
  if X = 0 then Exit;   {no main color table}
  end;
P := PChar(Colors)+(X+1)*Sizeof(RGB);
if (P^ <> #$21) or ((P+1)^ <> #$F9) then Exit;  {extension block not found}
if (ord(P[3]) and 1 <> 1) then Exit;     {no transparent color specified}

with Colors^[Ord(P[6])] do
  Color := integer(Blue) shl 16 or integer(Green) shl 8 or integer(Red);
Result := True;
end;

{$A+}


{$A-} {record field alignment off for this routine}


function IsTransparentPng(Stream: TStream; var Color: TColor): boolean;
Type
  RGB = record
    Red, Green, Blue: byte;
    end;

  PngHeader = record
    width       : integer;
    height      : integer;
    bitDepth    : byte;
    colorType   : byte;
    compression : byte;
    filter      : byte;
    interlace   : byte;
    end;
var
  Header: PngHeader;
  CRC: integer;
  OldPosition: integer;
  pngPalette: array[0..255] of RGB;
  dataSize : integer;
  chunkType: array[0..4] of Char;
  chunkTypeStr: string;
  done : Boolean;
  Ar: Array[0..10] of byte;

  function IntSwap(data: integer): integer;
  var
     byte0 : integer;
     byte1 : integer;
     byte2 : integer;
     byte3 : integer;
  begin
     byte0 := data and $FF;
     byte1 := (data shr 8) and $FF;
     byte2 := (data shr 16) and $FF;
     byte3 := (data shr 24) and $FF;

     result := (byte0 shl 24) or (byte1 shl 16) or (byte2 shl 8) or byte3;
  end;

begin
result := false;
OldPosition := Stream.Position;

try
   Stream.Position := 0;
   Stream.Read(Ar, 8);

   if KindOfImage(@Ar) <> Png then
     begin
     Stream.Position := OldPosition;
     Exit;
     end;

   Stream.Position := 8; {past the PNG Signature}
   done := False;

{Read Chunks}
   repeat
      Stream.Read(dataSize, 4);
      dataSize := IntSwap(dataSize);
      Stream.Read(chunkType, 4);
      chunkType[4] := #0; {make sure string is NULL terminated}
      chunkTypeStr := StrPas(chunkType);
      if chunkTypeStr = 'IHDR' then
      begin
         Stream.Read(Header, DataSize);
         Header.width := IntSwap(Header.width);
         Header.height := IntSwap(Header.height);
         Stream.Read(CRC, 4); {read it in case we need to read more}
         if (Header.colorType < 2) or (Header.colorType > 3) then
            done := True; {only type 2 and 3 use tRNS}
      end
      else if chunkTypeStr = 'PLTE' then
      begin
         Stream.Read(pngPalette, DataSize);
         Stream.Read(CRC, 4); {read it in case we need to read more}
      end
      else if chunkTypeStr =  'tRNS' then
      begin
         if Header.colorType = 3 then
         begin
            with pngPalette[dataSize - 1] do
               Color := integer(Blue) shl 16 or integer(Green) shl 8 or integer(Red);
         end
         else  {has to have been 2}
         begin
            {for now I am ignoring this since I can't make one}
         end;
         result := true;
         done := true; {got everything we need at this point}
      end
      else if chunkTypeStr = 'IDAT' then
         done := True {if this chunk is hit there is no tRNS}
      else
         Stream.Position := Stream.Position + dataSize + 4; {additional 4 for the CRC}
   until done = True;
except
   end;

Stream.Position := OldPosition;
end;

{$A+}

function TransparentGIF(const FName: string; var Color: TColor): boolean;
{Looks at a GIF image file to see if it's a transparent GIF.}
{Needed for OnBitmapRequest Event handler}
var
  Stream: TFileStream;
begin
Result := False;
try
  Stream := TFileStream.Create(FName, fmShareDenyWrite or FmOpenRead);
  try
    Result := IsTransparent(Stream, Color);
  finally
    Stream.Free;
  end;
except
  end;
end;

function ConvertImage(Bitmap: TBitmap): TBitmap;
{convert bitmap into a form for BitBlt later}
  function DIBConvert: TBitmap;
  var
    DC: HDC;
    DIB: TDib;
    OldBmp: HBitmap;
    OldPal: HPalette;
    Hnd: HBitmap;
  begin
  DC := CreateCompatibleDC(0);
  OldBmp := SelectObject(DC, Bitmap.Handle);
  OldPal := SelectPalette(DC, ThePalette, False);
  RealizePalette(DC);
  DIB := TDib.CreateDIB(DC, Bitmap);
  Hnd := DIB.CreateDIBmp;
  DIB.Free;
  SelectPalette(DC, OldPal, False);
  SelectObject(DC, OldBmp);
  DeleteDC(DC);
  Bitmap.Free;
  Result := TBitmap.Create;
  Result.Handle := Hnd;
  if (ColorBits = 8) and (Result.Palette = 0) then  
    Result.Palette := CopyPalette(ThePalette);
  end;

begin
if not Assigned(Bitmap) then
  begin
  Result := Nil;
  Exit;
  end;

if ColorBits > 8 then
  begin
  if Bitmap.PixelFormat <= pf8bit then
    Result := DIBConvert
  else
    Result := Bitmap;
  Exit;
  end;

if Bitmap.HandleType = bmDIB then
  begin
  Result := GetBitmap(Bitmap);
  Bitmap.Free;
  Exit;
  end;
Result := DIBConvert;
end;

function GetImageFromFile(const Filename: String): TBitmap;
{used only externally in OnBitmapRequest handler}
var
  IT: ImageType;
  Mask: TBitmap;
  Transparent: Transparency;
  Stream: TMemoryStream;

  function GetGif: TBitmap;
  var
    TmpGif: TGifImage;
    NonAnimated: boolean;
  begin
  Result := Nil;
  TmpGif := CreateAGifFromStream(NonAnimated, Stream);
  if Assigned(TmpGif) then
    begin
    Result := TBitmap.Create;
    try
      Result.Assign(TmpGif.Bitmap);
    except
      Result.Free;
      Result := Nil;
      end;
    TmpGif.Free;
    end
  end;

begin
try
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(Filename);
    IT := KindOfImage(Stream.Memory);
    if IT in [Gif, Gif89] then
      Result := GetGif
    else
      begin
      Result := GetImageAndMaskFromStream(Stream, Transparent, Mask);
      Mask.Free;
      end;
  finally
    Stream.Free;
    end;
except
  Result := Nil;
  end;
end;

{----------------GetImageAndMaskFromFile}
function GetImageAndMaskFromFile(const Filename: String; var Transparent: Transparency;
                            var Mask: TBitmap): TBitmap;
var
  Stream: TMemoryStream;
begin
Result := Nil;
Mask := Nil;
if not FileExists(Filename) then Exit;
Stream := TMemoryStream.Create;
Stream.LoadFromFile(Filename);
try
  Result := GetImageAndMaskFromStream(Stream, Transparent, Mask);
finally
  Stream.Free;
  end;
end;

{----------------GetImageAndMaskFromStream}
function GetImageAndMaskFromStream(Stream: TMemoryStream;
        var Transparent: Transparency; var AMask: TBitmap): TBitmap;
var
  IT: ImageType;
  Color: TColor;
  Pal: TPaletteEntry;
  jpImage: TJpegMod;
  Tmp: TBitmap;
  {$ifndef NoPng}
  PI: TPngImage;
  {$endif}
begin
Result := Nil;
AMask := Nil;
if not Assigned(Stream) or (Stream.Memory = Nil) or (Stream.Size < 20) then
  Exit;
Stream.Position := 0;
IT := KindOfImage(Stream.Memory);

if not (IT in [Bmp, Jpg, Png]) then
  Exit;

Result := TBitmap.Create;
try
  if IT = Jpg then
    begin
    Transparent := NotTransp;
    jpImage := TJpegMod.Create;
    try
      jpImage.LoadFromStream(Stream);
      if ColorBits <= 8 then
        begin
        jpImage.PixelFormat := jf8bit;
        if not jpImage.GrayScale and (ColorBits = 8) then
          jpImage.Palette := CopyPalette(ThePalette);
        end
      else jpImage.PixelFormat := jf24bit;
      Result.Assign(jpImage.Bitmap);
    finally
      jpImage.Free;
      end;
    end
  {$ifndef NoPng}
  else if IT = Png then
    begin
    if IsTransparentPNG(Stream, Color) then  {check for transparent PNG}
      Transparent := TPng;   
    PI := TPngImage.Create;
    try
     PI.LoadFromStream(Stream);
     Result.Assign(PI);
     if Result.Handle <> 0 then;      {force proper initiation win98/95}
    finally
     PI.Free;
     end;
    end
  {$else}
  else if IT = Png then
    Result := Nil
  {$endif}
  else
    begin
    Result.LoadFromStream(Stream);   {Bitmap}
    end;
  if Transparent = LLCorner then
    AMask := GetImageMask(Result, False, 0)
  else if Transparent = TPng then
    begin
    GetPaletteEntries(Result.Palette, 0, 1, Pal);
    Color := integer(Pal.peBlue) shl 16 or integer(Pal.peGreen) shl 8 or integer(Pal.peRed);
    AMask := GetImageMask(Result, True, Color);
    {Replace the background color with black.  This is needed if the Png is a
     background image.}
    Tmp := Result;
    Result := TBitmap.Create;
    Result.Width := Tmp.Width;
    Result.Height := Tmp.Height;
    Result.Palette := CopyPalette(ThePalette);
    with Result do
      begin
      Canvas.Brush.Color := Color;
        PatBlt(Canvas.Handle, 0, 0, Width, Height, PatCopy);
      BitBlt(Canvas.Handle, 0, 0, Width, Height, AMask.Canvas.Handle, 0, 0, SrcAnd);
      BitBlt(Canvas.Handle, 0, 0, Width, Height, Tmp.Canvas.Handle, 0, 0, SrcInvert);
      end;
    Tmp.Free;
    end;
  Result := ConvertImage(Result);
except
  Result.Free;
  Result := Nil;
  end;
end;

function GetImageMask(Image: TBitmap; ColorValid: boolean; AColor: TColor): TBitmap;
begin
try
  if ColorValid then
    Image.TransparentColor := AColor;  {color has already been selected}
  {else the transparent color is the lower left pixel of the bitmap}

  Image.Transparent := True;

  Result := TBitmap.Create;
  try
    Result.Handle := Image.ReleaseMaskHandle;
    Image.Transparent := False;
  except
    FreeAndNil(Result);
    end;
except
  Result := Nil;
  end;
end;

{----------------FinishTransparentBitmap }
procedure FinishTransparentBitmap (ahdc: HDC;
              InImage, Mask: TBitmap; xStart, yStart, W, H: integer);
var
  bmAndBack,
  bmSave,
  bmBackOld,
  bmObjectOld : HBitmap;
  hdcInvMask,
  hdcMask,
  hdcImage: HDC;
  DestSize, SrcSize : TPoint;
  OldBack, OldFore: TColor;
  BM: Windows.TBitmap;
  Image: TBitmap;

begin
Image := TBitmap.Create;  {protect original image} 
try
  Image.Assign(InImage);

  hdcImage := CreateCompatibleDC (ahdc);
  SelectObject (hdcImage, Image.Handle); { select the bitmap }

  { convert bitmap dimensions from device to logical points}
  SrcSize.x := Image.Width;
  SrcSize.y := Image.Height;
  DPtoLP(hdcImage, SrcSize, 1);

  DestSize.x := W;
  DestSize.y := H;
  DPtoLP (hdcImage, DestSize, 1);

  { create a bitmap for each DC}
  { monochrome DC}
  bmAndBack := CreateBitmap (SrcSize.x, SrcSize.y, 1, 1, nil);

  bmSave := CreateCompatibleBitmap (ahdc, DestSize.x, DestSize.y);
  GetObject(bmSave, SizeOf(BM), @BM);
  if (BM.bmBitsPixel > 1) or (BM.bmPlanes > 1) then
    begin
    { create some DCs to hold temporary data}
    hdcInvMask   := CreateCompatibleDC(ahdc);
    hdcMask := CreateCompatibleDC(ahdc);

    { each DC must select a bitmap object to store pixel data}
    bmBackOld   := SelectObject (hdcInvMask, bmAndBack);

    { set proper mapping mode}
    SetMapMode (hdcImage, GetMapMode (ahdc));

    bmObjectOld := SelectObject(hdcMask, Mask.Handle);

    { create the inverse of the object mask}
    BitBlt (hdcInvMask, 0, 0, SrcSize.x, SrcSize.y, hdcMask, 0, 0, NOTSRCCOPY);

    {set the background color of the source DC to the color contained in the
     parts of the bitmap that should be transparent, the foreground to the parts that
     will show}
    OldBack := SetBkColor(ahDC, clWhite);
    OldFore := SetTextColor(ahDC, clBlack);

    { Punch out a black hole in the background where the image will go}
    SetStretchBltMode(ahDC, WhiteOnBlack);
    StretchBlt (ahDC, XStart, YStart, DestSize.x, DestSize.y, hdcMask, 0, 0, SrcSize.x, SrcSize.y, SRCAND);

    { mask out the transparent colored pixels on the bitmap}
    BitBlt (hdcImage, 0, 0, SrcSize.x, SrcSize.y, hdcInvMask, 0, 0, SRCAND);

    { XOR the bitmap with the background on the destination DC}
    SetStretchBltMode(ahDC, ColorOnColor);
    StretchBlt(ahDC, XStart, YStart, W, H, hdcImage, 0, 0, Image.Width, Image.Height, SRCPAINT);

    SetBkColor(ahDC, OldBack);
    SetTextColor(ahDC, OldFore);

    { delete the memory bitmaps}
    DeleteObject (SelectObject (hdcInvMask, bmBackOld));
    SelectObject (hdcMask, bmObjectOld);

    { delete the memory DCs}
    DeleteDC (hdcInvMask);
    DeleteDC (hdcMask);
    end
  else
    begin
    DeleteObject(bmAndBack);
    end;
  DeleteObject(bmSave);     
  DeleteDC (hdcImage);
finally
  Image.Free;
  end;
end;

{----------------TDib.CreateDIB}
constructor TDib.CreateDIB(DC: HDC; Bitmap: TBitmap);
{given a TBitmap, construct a device independent bitmap}
var
  ImgSize: DWord;
begin
InitializeBitmapInfoHeader(Bitmap.Handle);
ImgSize := Info^.biSizeImage;
Allocate(ImgSize);
try
  GetDIBX(DC, Bitmap.Handle, Bitmap.Palette);
except
  DeAllocate;
  Raise;
  end;
end;

destructor TDib.Destroy;
begin
DeAllocate;
inherited Destroy;
end;

procedure TDib.Allocate(Size: integer);
begin
ImageSize := Size;
if Size < $FF00 then
  GetMem(Image, Size)
else
   begin
   FHandle := GlobalAlloc(HeapAllocFlags, Size);
   if FHandle = 0 then
       ABort;
   Image := GlobalLock(FHandle);
   end;
end;

procedure TDib.DeAllocate;
begin
if ImageSize > 0 then
  begin
  if ImageSize < $FF00 then
    Freemem(Image, ImageSize)
  else
    begin
    GlobalUnlock(FHandle);
    GlobalFree(FHandle);
    end;
  ImageSize := 0;
  end;
if InfoSize > 0 then
  begin
  FreeMem(Info, InfoSize);
  InfoSize := 0;
  end;
end;

procedure TDib.InitializeBitmapInfoHeader(Bitmap: HBITMAP);
var
  BM: Windows.TBitmap;
  BitCount: integer;

   function WidthBytes(I: integer): integer;
   begin
     Result := ((I + 31) div 32) * 4;
   end;

begin
GetObject(Bitmap, SizeOf(BM), @BM);
BitCount := BM.bmBitsPixel * BM.bmPlanes;
if BitCount > 8 then
  InfoSize := SizeOf(TBitmapInfoHeader)
else
  InfoSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * (1 shl BitCount);  
GetMem(Info, InfoSize);

with Info^ do
  begin
  biSize := SizeOf(TBitmapInfoHeader);
  biWidth := BM.bmWidth;
  biHeight := BM.bmHeight;
  biBitCount := BM.bmBitsPixel * BM.bmPlanes;
  biPlanes := 1;
  biXPelsPerMeter := 0;
  biYPelsPerMeter := 0;
  biClrUsed := 0;
  biClrImportant := 0;
  biCompression := BI_RGB;
  if biBitCount in [16, 32] then
    biBitCount := 24;
  biSizeImage := WidthBytes(biWidth * biBitCount) * biHeight;
  end;
end;

procedure TDib.GetDIBX(DC: HDC; Bitmap: HBITMAP; Palette: HPALETTE);
var
  OldPal: HPALETTE;
  Rslt: integer;
  bmInfo: PBitmapInfo;
begin
  OldPal := 0;
  if Palette <> 0 then
    begin
    OldPal := SelectPalette(DC, Palette, False);
    RealizePalette(DC);
    end;
  bmInfo := PBitmapInfo(Info);
  Rslt := GetDIBits(DC, Bitmap, 0, Info^.biHeight, Image, bmInfo^, DIB_RGB_COLORS);
  if OldPal <> 0 then
    SelectPalette(DC, OldPal, False);
  if Rslt = 0 then
    begin
    OutofMemoryError;
    end;
end;

procedure TDib.DrawDIB(DC: HDC; X: Integer; Y: integer; W, H: integer;
          ROP: DWord);
var
  bmInfo: PBitmapInfo;
begin
bmInfo := PBitmapInfo(Info);
with Info^ do
  StretchDIBits(DC, X, Y, W, H, 0, 0, biWidth, biHeight, Image,
      bmInfo^, DIB_RGB_COLORS, ROP);
end;

function TDib.CreateDIBmp: hBitmap;
var
  bmInfo: PBitmapInfo;
  DC: HDC;
  OldPal: HPalette;
begin
bmInfo := PBitmapInfo(Info);
DC := GetDC(0);
OldPal := SelectPalette(DC, ThePalette, False);
RealizePalette(DC);
try
  Result := CreateDIBitmap(DC, bmInfo^.bmiHeader, CBM_INIT, Image,
            bmInfo^, DIB_RGB_COLORS);
finally
  SelectPalette(DC, OldPal, False);
  ReleaseDC(0, DC);
  end;
end;

{----------------IndentManagerBasic.Create}
constructor IndentManagerBasic.Create;
begin
inherited Create;
R := TFreeList.Create;
L := TFreeList.Create;
end;

destructor IndentManagerBasic.Destroy;
begin
R.Free;
L.Free;
inherited Destroy;
end;

procedure IndentManagerBasic.Clear;
begin
R.Clear;
L.Clear;
CurrentID := Nil;
end;

{----------------IndentManagerBasic.Reset}
procedure IndentManagerBasic.Reset(Lf, Rt: integer);
begin
LfEdge := Lf;
RtEdge := Rt;
CurrentID := Nil;
end;

procedure IndentManagerBasic.UpdateTable(Y: integer; IW: integer; IH: integer;
                           Justify: JustifyType);
{Given a floating table, update the edge information. }
var
  IR: IndentRec;
begin
  IR := IndentRec.Create;
  if (Justify = Left) then
    begin
    with IR do
      begin
      X := -LfEdge + IW;
      YT := Y;
      YB := Y + IH;
      L.Add(IR);
      end;
    end
  else if (Justify = Right) then
    begin
    with IR do
      begin
      X := RightSide(Y) - IW;
      YT := Y;
      YB := Y + IH;
      R.Add(IR);
      end;
    end;
end;

const
  BigY = 9999999;

function IndentManagerBasic.LeftIndent(Y: integer): integer;
var
  I: integer;
begin
Result := -99999;
for I := 0 to L.Count-1 do
  with IndentRec(L.Items[I]) do
    begin
    if (Y >= YT) and (Y < YB) and (Result < X) then
      if not Assigned(ID) or (ID = CurrentID) then
        Result := X;
    end;
if Result = -99999 then  
  Result := 0;
Inc(Result, LfEdge);
end;

function IndentManagerBasic.RightSide(Y: integer): integer;
{returns the current right side dimension as measured from the left, a positive
 number}
var
  I: integer;
  IR: IndentRec;
begin
Result := 99999;
for I := 0 to R.Count-1 do
  begin
  IR := IndentRec(R.Items[I]);
  with IR do
    if (Y >= YT) and (Y < YB) and (Result > X) then
      if not Assigned(ID) or (ID = CurrentID) then
        Result := X;
  end;
if Result = 99999 then
  Result := RtEdge     
else Inc(Result, LfEdge);
end;

function IndentManagerBasic.ImageBottom: integer;
{finds the bottom of the last floating image}
var
  I: integer;
begin
Result := 0;
for I := 0 to L.Count-1 do
  with IndentRec(L.Items[I]) do
    if not Assigned(ID) and (YB > Result) then  
      Result := YB;
for I := 0 to R.Count-1 do
  with IndentRec(R.Items[I]) do
    if not Assigned(ID) and (YB > Result) then
      Result := YB;
end;

procedure IndentManagerBasic.GetClearY(var CL, CR: integer);
{returns the left and right Y values which will clear image margins}
var
  I: integer;
begin
CL := -1;
for I := 0 to L.Count-1 do
  with IndentRec(L.Items[I]) do
    if not Assigned(ID)and (YB > CL) then
      CL := YB;
CR := -1;
for I := 0 to R.Count-1 do
  with IndentRec(R.Items[I]) do
    if not Assigned(ID)and (YB > CR) then
      CR := YB;
Inc(CL);
Inc(CR);
end;

function IndentManagerBasic.GetNextWiderY(Y: integer): integer;
{returns the next Y value which offers a wider space or Y if none}
var
  I, CL, CR: integer;
begin
CL := Y;
for I := 0 to L.Count-1 do
  with IndentRec(L.Items[I]) do
    if not Assigned(ID)and (YB > Y) and ((YB < CL) or (CL = Y)) then
      CL := YB;
CR := Y;
for I := 0 to R.Count-1 do
  with IndentRec(R.Items[I]) do
    if not Assigned(ID)and (YB > Y) and ((YB < CR) or (CR = Y)) then
      CR := YB;
if CL = Y then
  Result := CR
else if CR = Y then
  Result := CL
else Result := IntMin(CL, CR);
end;

function IndentManagerBasic.SetLeftIndent(XLeft, Y: integer): integer;
var
  IR: IndentRec;
begin
IR := IndentRec.Create;
with IR do
  begin
  YT := Y;
  YB := BigY;
  X := XLeft;
  ID := CurrentID;
  end;
Result := L.Add(IR);
end;

function IndentManagerBasic.SetRightIndent(XRight, Y: integer): integer;
var
  IR: IndentRec;
begin
IR := IndentRec.Create;
with IR do
  begin
  YT := Y;
  YB := BigY;  
  X := XRight;
  ID := CurrentID;
  end;
Result := R.Add(IR);
end;

procedure IndentManagerBasic.FreeLeftIndentRec(I: integer);  
begin
IndentRec(L.Items[I]).Free;
L.Delete(I);
end;

procedure IndentManagerBasic.FreeRightIndentRec(I: integer);
begin
IndentRec(R.Items[I]).Free;
R.Delete(I);
end;

procedure SetGlobalPalette(Value: HPalette);
begin
end;

function CopyPalette(Source: hPalette): hPalette;
var
  LP: ^TLogPalette;
  NumEntries: integer;
begin
Result := 0;
if ColorBits > 8 then    
  Exit;
GetMem(LP, Sizeof(TLogPalette) + 256*Sizeof(TPaletteEntry));
try
  with LP^ do
    begin
    palVersion := $300;
    palNumEntries := 256;
    NumEntries := GetPaletteEntries(Source, 0, 256, palPalEntry);
    if NumEntries > 0 then
      begin
      palNumEntries := NumEntries;
      Result := CreatePalette(LP^);
      end;
    end;
finally
  FreeMem(LP, Sizeof(TLogPalette) + 256*Sizeof(TPaletteEntry));
  end;
end;

procedure CalcPalette(DC: HDC);
{calculate a rainbow palette, one with equally spaced colors}
const
  Values: array[0..5] of integer = (55, 115, 165, 205, 235, 255);
var
  LP: ^TLogPalette;
  I, J, K, Sub: integer;
begin
GetMem(LP, Sizeof(TLogPalette) + 256*Sizeof(TPaletteEntry));
try
  with LP^ do
    begin
    palVersion := $300;
    palNumEntries := 256;
    GetSystemPaletteEntries(DC, 0, 256, palPalEntry);
    Sub := 10;  {start at entry 10}
    for I := 0 to 5 do
      for J := 0 to 5 do
        for K := 0 to 5 do
          if not ((I=5) and (J=5) and (K=5)) then  {skip the white}
            with palPalEntry[Sub] do
              begin
              peBlue := Values[I];
              peGreen := Values[J];
              peRed := Values[K];
              peFlags := 0;
              Inc(Sub);
              end;
    for I := 1 to 24 do
       if not (I in [7, 15, 21]) then   {these would be duplicates}  
          with palPalEntry[Sub] do
            begin
            peBlue := 130 + 5*I;
            peGreen := 130 + 5*I;
            peRed := 130 + 5*I;
            peFlags := 0;
            Inc(Sub);
            end;
    ThePalette := CreatePalette(LP^);
    end;
finally
  FreeMem(LP, Sizeof(TLogPalette) + 256*Sizeof(TPaletteEntry));
  end;
end;

const
  DefaultBitmap = 1002;
  ErrBitmap = 1001;
  ErrBitmapMask = 1005;
  Hand_Cursor = 1003;
  ThickIBeam_Cursor = 1006;

procedure ThisExit; far;
begin
if ThePalette <> 0 then
  DeleteObject(ThePalette);
DefBitMap.Free;
ErrorBitMap.Free;
ErrorBitMapMask.Free;
WaitStream.Free;
end;

{----------------TIDNameList}
constructor TIDNameList.Create(List: TList);
begin
inherited Create;
Sorted := True;
OwnerList := List;
end;

destructor TIDNameList.Destroy;
begin
Clear;
inherited
end;

procedure TIDNameList.Clear;
var
  I: integer;
begin
for I := 0 to Count-1 do
  try       
    if Objects[I] is TChPosObj then
      Objects[I].Free;
  except
    end; 
inherited Clear;
end;

function TIDNameList.AddObject(const S: string; AObject: TObject): Integer;
var
  I: integer;
begin
if Find(S, I) then
  begin
  try
    if Objects[I] is TChPosObj then
      Objects[I].Free;
  except
    end;
  Delete(I);
  end;
Result := inherited AddObject(S, AObject);
end;

procedure TIDNameList.AddChPosObject(const S: string; Pos: integer);
var
  ChPosObj: TChPosObj;
begin
ChPosObj := TChPosObj.Create;
ChPosObj.List := OwnerList;
ChPosObj.ChPos := Pos;
AddObject(S, ChPosObj);
end;

destructor TIDObject.Destroy;
begin
inherited;
end;

{----------------TChPosObj.GetYPosition:}
function TChPosObj.GetYPosition: integer;
var
  Pos, X, Y: integer;
begin
with List as TSectionList do
  begin
  Pos := FindDocPos(ChPos, False);
  if CursorToXY(Nil, Pos, X, Y) then
      Result := Y
  else Result := 0;
  end;
end;

function InSet(W: WideChar; S: SetOfChar): boolean;
begin
if Ord(W) > 255 then
  Result := False
else
  Result := Char(W) in S;
end;

{----------------TCharCollection.GetAsString:}
function TCharCollection.GetAsString: string;
begin
  Result := Copy(FChars, 1, FCurrentIndex);
end;

function TCharCollection.GetSize: Integer;

begin
  Result := FCurrentIndex;
end;

constructor TCharCollection.Create;
begin
inherited;
SetLength(FChars, TokenLeng);
GetMem(FIndices, TokenLeng*Sizeof(integer));
FCurrentIndex := 0;
end;

destructor TCharCollection.Destroy;
begin
FreeMem(FIndices);
inherited;
end;

procedure TCharCollection.Add(C: Char; Index: Integer);
begin
  if FCurrentIndex = Length(FChars) then
  begin
    SetLength(FChars, FCurrentIndex + 50);
    ReallocMem(FIndices, (FCurrentIndex+50)*Sizeof(integer));
  end;
  Inc(FCurrentIndex);
  FIndices^[FCurrentIndex] := Index;
  FChars[FCurrentIndex] := C;
end;

procedure TCharCollection.Clear;
begin
  FCurrentIndex := 0;
  FChars := '';
end;

procedure TCharCollection.Concat(T: TCharCollection);
var
  K: integer;
begin
  K := FCurrentIndex + T.FCurrentIndex;
  if K >= Length(FChars) then
  begin
    SetLength(FChars, K + 50);
    ReallocMem(FIndices, (K+50)*Sizeof(integer));
  end;
  Move(PChar(T.FChars)^, FChars[FCurrentIndex + 1], T.FCurrentIndex);
  Move(T.FIndices^[1], FIndices^[FCurrentIndex + 1], T.FCurrentIndex * Sizeof(Integer));
  FCurrentIndex := K;
end;
{----------------TokenObj.Create}
constructor TokenObj.Create;    
begin
inherited;
GetMem(C, TokenLeng * Sizeof(WideChar));
GetMem(I, TokenLeng*Sizeof(integer));
MaxIndex := TokenLeng;
Leng := 0;
St := '';
StringOK := True;
end;

destructor TokenObj.Destroy;
begin
FreeMem(I);
FreeMem(C);
inherited;
end;

procedure TokenObj.AddUnicodeChar(Ch: WideChar; Ind: integer);
{Ch must be Unicode in this method}
begin
if Leng >= MaxIndex then
  begin
  ReallocMem(C, (MaxIndex + 50) * Sizeof(WideChar));
  ReallocMem(I, (MaxIndex+50)*Sizeof(integer));
  Inc(MaxIndex, 50);
  end;
Inc(Leng);
C^[Leng] := Ch;
I^[Leng] := Ind;
StringOK := False;
end;

procedure TokenObj.Clear;
begin
Leng := 0;
St := '';
StringOK := True;
end;

procedure TokenObj.AddString(S: TCharCollection; CodePage: Integer);
// Takes the given string S and converts it to Unicode using the given code page.
// If we are on Windows 95 then CP_UTF8 (and CP_UTF7) are not supported.
// We compensate for this by using a Delphi function.
// Note: There are more code pages (including CP_UTF7), which are not supported
// on all platforms. These are rather esoteric and therefore not considered here.

var
  WS: WideString;
  I, J,
  Len, NewLen: Integer;
  P: PChar;

begin
  Len := S.FCurrentIndex;
{$ifdef Delphi6_Plus}
  if IsWin95 and (CodePage = CP_UTF8) then
  begin
    {Provide initial space. The resulting string will never be longer than the
     UTF-8 encoded string.}
    SetLength(WS, Len+1);  {add 1 for #0 terminator}    
    NewLen := UTF8ToUnicode(PWideChar(WS), Len+1, PChar(S.FChars), Len) - 1;  {subtr 1 as don't want to count null terminator}
  end
  else
{$endif}
  begin
    {Provide initial space. The resulting string will never be longer than the
     UTF-8 or multibyte encoded string.}
    SetLength(WS, 2 * Len);
    NewLen := MultiByteToWideChar(CodePage, 0, PChar(S.FChars), Len, PWideChar(WS), Len);
    if NewLen = 0 then
      { Invalid code page. Try default.}
      NewLen := MultiByteToWideChar(CP_ACP, 0, PChar(S.FChars), Len, PWideChar(WS), Len);
  end;

  {Store the wide string and character indices.}
  if Len = NewLen then    {single byte character set or at least no multibyte conversion}
    for I := 1 to NewLen do
      AddUnicodeChar(WS[I], S.FIndices[I])
  else
    begin     {multibyte character set}
    J := 1;
    for I := 1 to NewLen do
      begin
      AddUnicodeChar(WS[I], S.FIndices[J]);
      {find index for start of next character}
      P := CharNextEx(CodePage, @S.FChars[J], 0);
      if Assigned(P) then
        J := P - PChar(S.FChars) + 1
      else
        Break;
      end;
    end;
end;

procedure TokenObj.Concat(T: TokenObj);
var
  K: integer;
begin
K := Leng + T.Leng;
if K > MaxIndex then
  begin
  ReallocMem(C, (K + 50) * Sizeof(WideChar));
  ReallocMem(I, (K+50)*Sizeof(integer));
  MaxIndex := K+50;
  end;
Move(T.C^, C^[Leng + 1], T.Leng * Sizeof(WideChar));
Move(T.I^, I^[Leng+1], T.Leng*Sizeof(integer));
Leng := K;
StringOK := False;
end;

procedure TokenObj.Remove(N: integer);
begin    {remove a single character}
if N <= Leng then
  begin
  Move(C^[N + 1], C^[N], (Leng - N) * Sizeof(WideChar));
  Move(I^[N+1], I^[N], (Leng-N)*Sizeof(integer));
  if StringOK then
    Delete(St, N, 1);
  Dec(Leng);
  end;
end;

procedure TokenObj.Replace(N: integer; Ch: WideChar);
begin    {replace a single character}
if N <= Leng then
  begin
  C^[N] := Ch;
  if StringOK then
    St[N] := Ch;
  end;
end;

function TokenObj.GetString: WideString;
begin
if not StringOK then
  begin
  SetLength(St, Leng);
  Move(C^, St[1], SizeOf(WideChar) * Leng);
  StringOK := True;
  end;
Result := St;
end;

{----------------BitmapToRegion}
function BitmapToRegion(ABmp: TBitmap; XForm: PXForm; TransparentColor: TColor): HRGN;
{Find a Region corresponding to the non-transparent area of a bitmap.

 Thanks to Felipe Machado.  See http://www.delphi3000.com/
 Minor modifications made.}
const
  AllocUnit = 100;
type
  PRectArray = ^TRectArray;
  TRectArray = Array[0..(MaxInt div SizeOf(TRect))-1] of TRect;
var
  pr: PRectArray;    // used to access the rects array of RgnData by index
  h: HRGN;           // Handles to regions
  RgnData: PRgnData; // Pointer to structure RGNDATA used to create regions
  lr, lg, lb: Byte; // values for lowest and hightest trans. colors
  x,y, x0: Integer;  // coordinates of current rect of visible pixels
  b: PByteArray;     // used to easy the task of testing the byte pixels (R,G,B)
  ScanLinePtr: Pointer; // Pointer to current ScanLine being scanned
  ScanLineInc: Integer; // Offset to next bitmap scanline (can be negative)
  maxRects: Cardinal;   // Number of rects to realloc memory by chunks of AllocUnit
  bmp: TBitmap;
begin
  Result := 0;
  lr := GetRValue(TransparentColor);
  lg := GetGValue(TransparentColor);
  lb := GetBValue(TransparentColor);
  { ensures that the pixel format is 32-bits per pixel }
  bmp := TBitmap.Create;
  try
    bmp.Assign(ABmp);
    bmp.PixelFormat := pf32bit;
    { alloc initial region data }
    maxRects := AllocUnit;
    GetMem(RgnData,SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects));
    FillChar(RgnData^, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects), 0); 
    try
      with RgnData^.rdh do
      begin
        dwSize := SizeOf(TRgnDataHeader);
        iType := RDH_RECTANGLES;
        nCount := 0;
        nRgnSize := 0;
        SetRect(rcBound, MAXLONG, MAXLONG, 0, 0);
      end;
      { scan each bitmap row - the orientation doesn't matter (Bottom-up or not) }
      ScanLinePtr := bmp.ScanLine[0];
      if bmp.Height > 1 then   
        ScanLineInc := Integer(bmp.ScanLine[1]) - Integer(ScanLinePtr)
      else ScanLineInc := 0;   
      for y := 0 to bmp.Height - 1 do
      begin
        x := 0;
        while x < bmp.Width do
        begin
          x0 := x;
          while x < bmp.Width do
          begin
            b := @PByteArray(ScanLinePtr)[x*SizeOf(TRGBQuad)];
            // BGR-RGB: Windows 32bpp BMPs are made of BGRa quads (not RGBa)
            if (b[2] = lr) and
               (b[1] = lg) and
               (b[0] = lb) then
              Break; // pixel is transparent
            Inc(x);
          end;
          { test to see if we have a non-transparent area in the image }
          if x > x0 then
          begin
            { increase RgnData by AllocUnit rects if we exceeds maxRects }
            if RgnData^.rdh.nCount >= maxRects then
            begin
              Inc(maxRects,AllocUnit);
              ReallocMem(RgnData,SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects));
              pr := @RgnData^.Buffer;    
              FillChar(pr^[maxRects-AllocUnit], AllocUnit*SizeOf(TRect), 0);   
            end;
            { Add the rect (x0, y)-(x, y+1) as a new visible area in the region }
            pr := @RgnData^.Buffer; // Buffer is an array of rects
            with RgnData^.rdh do
            begin
              SetRect(pr[nCount], x0, y, x, y+1);
              { adjust the bound rectangle of the region if we are "out-of-bounds" }
              if x0 < rcBound.Left then rcBound.Left := x0;
              if y < rcBound.Top then rcBound.Top := y;
              if x > rcBound.Right then rcBound.Right := x;
              if y+1 > rcBound.Bottom then rcBound.Bottom := y+1;
              Inc(nCount);
            end;
          end; // if x > x0
          { Need to create the region by muliple calls to ExtCreateRegion, 'cause }
          { it will fail on Windows 98 if the number of rectangles is too large   }
          if RgnData^.rdh.nCount = 2000 then
          begin
            h := ExtCreateRegion(XForm, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects), RgnData^);
            if Result > 0 then
            begin // Expand the current region
              CombineRgn(Result, Result, h, RGN_OR);
              DeleteObject(h);
            end
            else  // First region, assign it to Result
              Result := h;
            RgnData^.rdh.nCount := 0;
            SetRect(RgnData^.rdh.rcBound, MAXLONG, MAXLONG, 0, 0);
          end;
          Inc(x);
        end; // scan every sample byte of the image
        Inc(Integer(ScanLinePtr), ScanLineInc);
      end;
      { need to call ExCreateRegion one more time because we could have left    }
      { a RgnData with less than 2000 rects, so it wasn't yet created/combined  }
      if RgnData^.rdh.nCount > 0 then  {LDB  0 Count causes exception and abort in Win98}
        h := ExtCreateRegion(XForm, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects), RgnData^)
      else h := 0;
      if Result > 0 then
      begin
        CombineRgn(Result, Result, h, RGN_OR);
        DeleteObject(h);
      end
      else
        Result := h;
    finally
      FreeMem(RgnData,SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects));
    end;
  finally
    bmp.Free;
    end;
end;

{----------------PrintBitmap}
procedure PrintBitmap(Canvas: TCanvas; X, Y, W, H: integer;
             BMHandle: HBitmap);
{Y relative to top of display here}
var
  OldPal: HPalette;
  DC: HDC;
  Info: PBitmapInfo;
  Image: AllocRec;
  ImageSize: DWord;
  InfoSize: DWord;
begin
if BMHandle = 0 then
  Exit;
DC := Canvas.Handle;
try
  GetDIBSizes(BMHandle, InfoSize, ImageSize);
  GetMem(Info, InfoSize);
  try
    Image := Allocate(ImageSize);
    OldPal := SelectPalette(DC, ThePalette, False);
    try
      GetDIB(BMHandle, ThePalette, Info^, Image.Ptr^);
      RealizePalette(DC);
      with Info^.bmiHeader do
        StretchDIBits(DC, X, Y, W, H,
           0, 0, biWidth, biHeight, Image.Ptr, Info^, DIB_RGB_COLORS, SRCCOPY);
    finally
      DeAllocate(Image);
      SelectPalette(DC, OldPal, False);
     end;
  finally
   FreeMem(Info, InfoSize);
   end;
except
  end;
end;

{----------------PrintTransparentBitmap1}
procedure PrintTransparentBitmap1(Canvas: TCanvas; X, Y, NewW, NewH: integer;
             Bitmap, Mask: TBitmap; YI, HI: integer);
{Y relative to top of display here}
{This routine prints transparently but only on a white background}
{X, Y are point where upper left corner will be printed.
 NewW, NewH are the Width and Height of the output (possibly stretched)
 Vertically only a portion of the Bitmap, Mask may be printed starting at
   Y=YI in the bitmap and a height of HI
}
var
  OldPal: HPalette;
  DC: HDC;
  Info: PBitmapInfo;
  Image: AllocRec;
  ImageSize: DWord;
  InfoSize: DWord;
  Abitmap: TBitmap;

begin
ABitmap := TBitmap.Create;
try
  Abitmap.Assign(Bitmap);
  ABitmap.Height := HI;
  BitBlt(ABitmap.Canvas.Handle, 0, 0, Bitmap.Width, HI, Bitmap.Canvas.Handle, 0, YI, SrcCopy);
  BitBlt(ABitmap.Canvas.Handle, 0, 0, Bitmap.Width, HI, Mask.Canvas.Handle, 0, YI, SRCPAINT);
  DC := Canvas.Handle;
  GetDIBSizes(ABitmap.Handle, InfoSize, ImageSize);
  GetMem(Info, InfoSize);
  try
    Image := Allocate(ImageSize);
    OldPal := SelectPalette(DC, ThePalette, False);
    try
      GetDIB(ABitmap.Handle, ThePalette, Info^, Image.Ptr^);
      RealizePalette(DC);
      with Info^.bmiHeader do
        StretchDIBits(DC, X, Y, NewW, NewH,
           0, 0, biWidth, biHeight, Image.Ptr, Info^, DIB_RGB_COLORS, SRCCOPY);
    finally
      DeAllocate(Image);
      SelectPalette(DC, OldPal, False);
     end;
  finally
   FreeMem(Info, InfoSize);
   end;
finally
  ABitmap.Free;
  end;
end;

{----------------PrintTransparentBitmap3}
procedure PrintTransparentBitmap3(Canvas: TCanvas; X, Y, NewW, NewH: integer;
             Bitmap, Mask: TBitmap; YI, HI: integer);
{Y relative to top of display here}
{This routine prints transparently on complex background by printing through a clip region}
{X, Y are point where upper left corner will be printed.
 NewW, NewH are the Width and Height of the output (possibly stretched)
 Vertically only a portion of the Bitmap, Mask may be printed starting at
   Y=YI in the bitmap and a height of HI
}
var
  OldPal: HPalette;
  DC: HDC;
  Info: PBitmapInfo;
  Image: AllocRec;
  ImageSize: DWord;
  InfoSize: DWord;
  hRgn, OldRgn: THandle;
  Rslt: integer;
  XForm: TXForm;
  SizeV, SizeW: TSize;
  HF, VF: double;
  ABitmap, AMask: TBitmap;
  BitmapCopy: boolean;

begin
{the following converts the black masked area in the image to white.  This may look
 better in WPTools which currently doesn't handle the masking}
if (Bitmap.Handle = 0) or (HI <= 0) or (Bitmap.Width <= 0) then
  Exit;
BitmapCopy := Bitmap.Height <> HI;
try
  if BitmapCopy then
    begin
    ABitmap := TBitmap.Create;
    AMask := TBitmap.Create;
    end
  else
    begin
    ABitmap := Bitmap;
    AMask := Mask;
    end;
  try
    if BitmapCopy then
      begin
      Abitmap.Assign(Bitmap);
      ABitmap.Height := HI;
      BitBlt(ABitmap.Canvas.Handle, 0, 0, Bitmap.Width, HI, Bitmap.Canvas.Handle, 0, YI, SrcCopy);
      AMask.Assign(Mask);
      AMask.Height := HI;
      BitBlt(AMask.Canvas.Handle, 0, 0, AMask.Width, HI, Mask.Canvas.Handle, 0, YI, SrcCopy);
      end;

    BitBlt(ABitmap.Canvas.Handle, 0, 0, Bitmap.Width, HI, AMask.Canvas.Handle, 0, 0, SRCPAINT);

    DC := Canvas.Handle;
    {calculate a transform for the clip region as it may be a different size than
     the mask and needs to be positioned on the canvas.}
    GetViewportExtEx(DC, SizeV);
    GetWindowExtEx(DC, SizeW);
    HF := (SizeV.cx/SizeW.cx);  {Horizontal adjustment factor}
    VF := (SizeV.cy/SizeW.cy);  {Vertical adjustment factor}

    XForm.eM11 := HF * (NewW/Bitmap.Width);
    XForm.eM12 := 0;
    XForm.eM21 := 0;
    XForm.eM22 := VF * (NewH/HI);
    XForm.edx := HF*X;
    XForm.edy := VF*Y;          

    {Find the region for the white area of the Mask}
    hRgn := BitmapToRegion(AMask, @XForm, $FFFFFF);
    if hRgn <> 0 then  {else nothing to output--this would be unusual}
      begin
      OldRgn := CreateRectRgn(0,0,1,1);  {a valid region is needed for the next call}
      Rslt := GetClipRgn(DC, OldRgn);    {save the Old clip region}
      try
        if Rslt = 1 then
          CombineRgn(hRgn, hRgn, OldRgn, RGN_AND);
        SelectClipRgn(DC, hRgn);
        GetDIBSizes(ABitmap.Handle, InfoSize, ImageSize);   
        GetMem(Info, InfoSize);
        try
          Image := Allocate(ImageSize);
          OldPal := SelectPalette(DC, ThePalette, True);
          try
            GetDIB(ABitmap.Handle, ThePalette, Info^, Image.Ptr^);  
            RealizePalette(DC);
            with Info^.bmiHeader do
              StretchDIBits(DC, X, Y, NewW, NewH,
                 0, 0, biWidth, biHeight, Image.Ptr, Info^, DIB_RGB_COLORS, SRCCOPY);
          finally
            DeAllocate(Image);
            SelectPalette(DC, OldPal, False);
           end;
        finally
         FreeMem(Info, InfoSize);
         end;
      finally
        if Rslt = 1 then
          SelectClipRgn(DC, OldRgn)
        else SelectClipRgn(DC, 0);
        DeleteObject(hRgn);
        DeleteObject(OldRgn);
        end;
      end;
  finally
    if BitmapCopy then
      begin
      ABitmap.Free;
      AMask.Free;
      end;
    end;
except
  end;
end;

initialization
DC := GetDC(0);
try
  ColorBits := GetDeviceCaps(DC, BitsPixel)*GetDeviceCaps(DC, Planes);

  if ColorBits <= 4 then
    ColorBits := 4
  else if ColorBits <= 8 then
    ColorBits := 8
  else  ColorBits := 24;

  ThePalette := 0;
  if ColorBits = 8 then
    CalcPalette(DC);
finally
  ReleaseDC(0, DC);
  end;

IsWin95 := (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and (Win32MinorVersion in [0..9]);
IsWin32Platform := Win32Platform = VER_PLATFORM_WIN32_WINDOWS;

DefBitMap := TBitmap.Create;
DefBitMap.Handle := LoadBitmap(HInstance, MakeIntResource(DefaultBitmap));
ErrorBitMap := TBitmap.Create;
ErrorBitMap.Handle := LoadBitmap(HInstance, MakeIntResource(ErrBitmap));
ErrorBitMapMask := TBitmap.Create;
ErrorBitMapMask.Handle := LoadBitmap(HInstance, MakeIntResource(ErrBitmapMask));
Screen.Cursors[HandCursor] := LoadCursor(HInstance, MakeIntResource(Hand_Cursor));
Screen.Cursors[ThickIBeamCursor] := LoadCursor(HInstance, MakeIntResource(ThickIBeam_Cursor));
Screen.Cursors[UpDownCursor] := LoadCursor(HInstance, 'UPDOWNCURSOR');
Screen.Cursors[UpOnlyCursor] := LoadCursor(HInstance, 'UPONLYCURSOR');
Screen.Cursors[DownOnlyCursor] := LoadCursor(HInstance, 'DOWNONLYCURSOR');

WaitStream := TMemoryStream.Create;

Finalization
ThisExit;
end.



