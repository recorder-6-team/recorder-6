//==============================================================================
//  Unit:        GeneralFunctions
//
//  Implements:  -
//
//  Description: General routines (functions/procedures).
//               This unit should contain only non-project specific routines
//               that could be useful anywhere.
//
//  Author:      Eric Salmon
//  Created:     1 May 2000
//
//  Last Revision Details:
//    $Revision: 60 $
//    $Date: 8/04/10 17:30 $
//    $Author: Andrewkemp $
//
//  Copyright © Dorset Software Services Ltd, 2000
//
//==============================================================================

{$Include DelphiVersions.inc}

unit GeneralFunctions;

interface

uses
  Windows, SysUtils, Messages, Controls, Forms, Graphics, ExtCtrls, Dialogs,
  Classes, ComCtrls, ListDlls, VersionInfo, ShellAPI, ShlObj, ActiveX, Grids,
  StdCtrls, Math, Registry, SysConst, CommCtrl{$IFDEF DELPHI7UP}, StrUtils{$ENDIF};

const
  InvalidFileNameCharSet = ['\', '/', ':', '?', '"', '<', '>', '|'];

type
  EGeneralFunctionsError = class(Exception);

  { Types for handling postcodes }
  TCharacterType = (ctAlpha, ctNumeric, ctSpace, ctUnknown);

  TTextStructureInfo = record
    CharacterType: TCharacterType;
    RepeatCount: integer;
  end;

  TPostcodeStructure = array[0..5] of TTextStructureInfo;

  TOSVersion = (
      wvUnknown, wvWin95, wvWin95OSR2, wvWin98, wvWin98SE,
      wvWinME, wvWinNT, wvWin2k, wvWinXP, wvWin2k3, wvVista, wvSeven, wvFuture);


// Cursor functions
function HourglassCursor:TCursor;
function AppStartCursor :TCursor;
procedure DefaultCursor(const ACursor:TCursor);

// Message boxes
procedure ShowInformation(const Msg:string);
function ConfirmYesNo(const Msg:string):word;
function ConfirmYesNoCancel(const Msg:string):word;
function ConfirmYesNoDefaultNo(Msg: string):word;
function ConfirmDeletionYesNo(const Msg:string):word;
function ConfirmDeletionYesNoCancel(const Msg:string):word;

// Graphical functions
procedure DrawChoppedText(const AString:string; ACanvas:TCanvas;
  const ARect:TRect; const AGap:byte=0);
function GetTextWithinLimit(const ACanvas:TCanvas; const AString:string;
  const AMaxWidth:integer):string;
procedure DrawCheckBox(const ACanvas: TCanvas; const xPos, yPos: Integer;
  const DrawTick: Boolean; const AEnabled: Boolean = True);
procedure DrawHorzSplitter(const ACanvas:TCanvas; const ASplitter:TSplitter);
procedure DrawVertSplitter(const ACanvas:TCanvas; const ASplitter:TSplitter);
procedure SetTextAttributes(ioAttributes: TTextAttributes;
  const iFontName: string; const iFontSize: integer; iFontStyle: TFontStyles;
  const iColor: TColor);

function GetContrastColour(iColor: TColor): TColor;
function MergeColours(iColour1, iColour2: TColor; iRatioPercent: byte): Longint;
{$IFDEF DELPHI7UP}
procedure SetListBoxHintIfTooWide(Sender: TObject; X, Y: Integer);
{$ENDIF}
procedure ResetListBoxExtent(AListBox: TCustomListBox);

// Maths
function Min(const iVal1, iVal2: integer): integer;
function Max(const iVal1, iVal2: integer): integer;
function FloatToFixedStr(iFloat: double; iCharacters: integer): string;

// Other
procedure AddLineInGrid(const AGrid:TStringGrid; const ATestColumn: Byte = 0;
  const AText: string = '');
procedure DelLineInGrid(const AGrid: TStringGrid);
// Sets the titles of the grid's columns. Ideally to use with resource strings.
procedure SetGridColumnTitles(grid: TStringGrid; const titles: Array of String);

function GetWindowsTempDir: String;
function UniqueFileName(const Path: String; const Prefix: String): String;
function GetUniqueFileName(const AFile: string): string;
function GetWinDir: string;
function GetCDDrive: char;

function IsDate(const ADate:string):boolean;
function IsFloat(const aFloat: string): boolean; overload;
function IsFloat(const aFloat: string;
    const FormatSettings: TFormatSettings): Boolean; overload;
function IsInt(const aInt: string): boolean;
function StringGridHint(AStringGrid:TStringGrid; const ARow:integer;
  const LinearHint:boolean):string;
procedure ParseStringIntoList(const AList, ASeparator: string; ioStrings: TStrings);
procedure ParseSemiColonString(const AList: string; ioStrings: TStrings);
procedure CommaSepListToStringList(const AList:String; ioStrings:TStrings);
function StringListToCommaSepList(AStringList: TStringList): string;
function StringListToCSVString(iStringList: TStringList): string;
function ReadableFormat(const iText: string): string;
function GetPlural(iNumber: integer; const iDescriptor: string): string; overload;
function GetPlural(value: Integer; const singular, plural: String): String; overload;
function LopColon(const AText: string): string;
function ProblemText(const iAction, iObject: string): string;
procedure AddToCommaSeparatedList(var ioList: string; const iAddition: string);
procedure AddToSQLWhereClause(var ioList: string; const iAddition: string);
function FormatWithQuotes(const iText: string): String;
function DuplicateCharacters(const iText: string; iChar: char): String;
function SafeQuotes(const iText: string): string;
function RemoveDuplicateCharacters(const iText: string; iChar: char): String;
function BoolToStr(const iValue: Boolean; const iTrue: string = 'True';
  const iFalse: string = 'False'): string;
{$IFDEF DELPHI7UP}
function ExtractWithoutExt(const iFileName: string): string;
{$ENDIF}
function ExpandLongPathName(ShortName: string): string;
function FormatWithSquareBrackets(AstText: String): String;
function RemoveAmpersands(const AString: String): String;
function RemoveSubStrings(const AString: String; ASubStrings: Array of String): String;

// Post code validation functions
function ValidPostCodeText(const iText: String): boolean;
// and other string checking
function IsNumber(const iCharacter: char): boolean;
function IsAtoZ(const iCharacter: char): boolean;
function IsSpace(const iCharater :char): boolean;
function IsSingleQuote(const iCharacter: char): boolean;

{ Current operating environment information }
function GetOSInfo: string;
function GetOSName: string;
function GetOSVersion: TOSVersion;
function GetMemoryInfo: string;
procedure GetDllVersions(iList: TStrings);
function GetMyDocuments: String;
function GetFolder(csidl: Integer; forceFolder: Boolean = False): String;
function GetIEMajorVersion: integer;
function GetFileDate(const AFileName: string=''): TDateTime;
function GetFileNameIcon(const AFileName: String; AFlags: Cardinal = 0): HICON;

procedure LockTaskKeysForWin95or98orME(iLock: boolean);

// Determine if the current user has administrator rights
function IsAdmin: Boolean;

{ and memory optimisation }
procedure TrimWorkingSet;

{ Date time functions }
{$IFDEF DELPHI7UP}
function ConvertStringToDate(iDateString: string; out DateValue: TDate): Boolean;
{$ELSE}
function ConvertStringToDate(iDateString: string): TDate;
{$ENDIF}

function DateToSQLSvrDate(iDate: TDateTime): String; { DO NOT USE: see below}
function ShowOnlyDate(iDateAndTime: TDateTime): String;

{ Utility functions which are probably only useful inside this unit }
function GetCharacterType(iChar: char): TCharacterType;

// Rich Text formatting functions
procedure SetRTFStyle(AControl: TCustomRichEdit; const AStyle: TFontStyle);
procedure SetRTFBold(AControl: TCustomRichEdit);
procedure SetRTFItalic(AControl: TCustomRichEdit);
procedure SetRTFUnderline(AControl: TCustomRichEdit);
function GetRTFText(AControl: TRichedit): String;

// SelItemText functions return the selected items text for a variety of components
// saves referring to the item index all the time
function SelItemText(iComponent: TRadioGroup): string; overload;
function SelItemText(iComponent: TComboBox): string; overload;
function SelItemText(iComponent: TListBox): string; overload;

procedure ClearListBoxObjects(iComponent: TListBox);

function DefMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; HelpCtx: Longint): integer;

function DiskInDriveA: boolean;

function RemoveFolderAndContent(const path: String): Boolean;
function DeleteFiles(const Location, FileName: string): Boolean;
procedure CopyFilesLike(iFileName,iPath:String);

procedure ShellFile(const AFileName: String);
procedure TryQuickView(const AFileName: String);
procedure CheckShellExecuteRetCode(const ARetCode: Integer);
// Check a character isn't an invalid one fot a file name
function ValidateCharForFilename(const Key: char): char;
// Removes any invalid characters from given filename
function ValidateFileName(const AFileName: String): String;

function IIF(const AExpression: boolean; iSuccess, iFailure: Variant): Variant;
procedure WaitFor(const Seconds:integer);
function AllParentsAreVisible(AControl: TControl): boolean;

function QuoteEscapeStr(Value: string): String;
//==============================================================================
implementation

uses
  ApiUtils, FileCtrl, SHFolder;

// Messages used by the message dialog boxes.
resourcestring
  ResStr_Deletion = 'Are you sure you want to delete %s?';
  ResStr_TooManyDigits = 'The number supplied is too big to round into the specified number of characters';
  ResStr_ErrorOccured = 'An error occurred whilst %s the %s.';
  ResStr_PersonalDocsFolderNotFound = 'Cannot find personal documents folder';
  ResStr_OSOutOfMemory      = 'The operating system is out of memory or resources.';
  ResStr_FileNotFound       = 'The specified file was not found.';
  ResStr_PathNotFound       = 'The specified path was not found.';
  ResStr_BadFormat          = 'The .EXE file is invalid (non-Win32 .EXE or error in .EXE image).';
  ResStr_AccessDenied       = 'The operating system denied access to the specified file.';
  ResStr_AssocIncomplete    = 'The filename association is incomplete or invalid.';
  ResStr_DDEBusy            = 'The DDE transaction could not be completed because other DDE transactions were being processed.';
  ResStr_DDEFail            = 'The DDE transaction failed.';
  ResStr_DDETimeout         = 'The DDE transaction could not be completed because the request timed out.';
  ResStr_DLLNotFound        = 'The specified dynamic-link library was not found.';
  ResStr_ErrOOM             = 'There was not enough memory to complete the operation.';
  ResStr_ErrShare           = 'A sharing violation occurred.';

//==============================================================================
// Returns the state of the cursor prior to change to Hourglass cursor
function HourglassCursor:TCursor;
begin
  Result:=Screen.Cursor;
  Screen.Cursor:=crHourglass;
end;  // HourglassCursor

//==============================================================================
// Returns the state of the cursor prior to change to AppStart cursor
function AppStartCursor:TCursor;
begin
  Result:=Screen.Cursor;
  Screen.Cursor:=crAppStart;
end;  // HourglassCursor

//==============================================================================
// Restore the cursor to its previous state. To be used in conjonction with HourglassCursor
procedure DefaultCursor(const ACursor:TCursor);
begin
  Screen.Cursor:=ACursor;
end;  // DefaultCursor

//==============================================================================
// General purpose function to draw text onto a provided canvas.  If the text
// is too big for the rectangle, then the text is shortened and '...' is added
// to the end.
procedure DrawChoppedText(const AString:string; ACanvas:TCanvas;
  const ARect:TRect; const AGap:byte=0);
var lText   :string;
    lVertGap:integer;
begin
  // Get the right amount of text
  lText:=GetTextWithinLimit(ACanvas,AString,ARect.Right-ARect.Left-(2*AGap));
  { Stick the text on the canvas }
  with ACanvas do
  begin
    lVertGap:=((ARect.Bottom-ARect.Top)-TextHeight(lText)) div 2;
    TextOut(ARect.Left+AGap,ARect.Top+lVertGap,lText);
  end;
end;  // DrawChoppedText

//==============================================================================
// Returns the part of the input text fitting in the MaxWidth specified. If the
// text doesn't fit, it is shortened and '...' is added at the end to denote
// it's not the whole text. If the size is too small to fit any text, an empty
// string is returned.
function GetTextWithinLimit(const ACanvas:TCanvas; const AString:string;
  const AMaxWidth:integer):string;
var stText:string;
    lDispWidth, lCount:integer;
begin
  Result:='';
  lDispWidth:=AMaxWidth-ACanvas.TextWidth('...');
  if (AMaxWidth>0) and (lDispWidth>0) then begin
    Result:=AString;
    // Chop some text off only if too big
    if ACanvas.TextWidth(AString)>AMaxWidth then begin
      stText:='';
      lCount:=1;
      while ACanvas.TextWidth(Copy(AString,1,lCount))<lDispWidth do
        Inc(lCount);
      Result:=Copy(AString,1,lCount-1)+'...';
    end;
  end;
end;  // GetTextWithinLimit

//==============================================================================
{ Draws a 3D Checkbox on ACanvas at the xPos,yPos position.
  DrawTick indicates if the box is checked or not. Box can be drawn enabled or disabled.
}
procedure DrawCheckBox(const ACanvas: TCanvas; const xPos, yPos: Integer;
  const DrawTick: Boolean; const AEnabled: Boolean = True);
var lBColor, lPColor: TColor;
begin
  with ACanvas do begin
    // Save Brush and Pen colours
    lBColor := Brush.Color;
    lPColor := Pen.Color;

    // Use Windows colours
    if AEnabled then Brush.Color := clWindow
                else Brush.Color := clBtnFace;

    Pen.Style := psSolid;
    // Draw box
    Pen.Color := clBtnHighlight;  Rectangle(xPos, yPos, xPos + 13, yPos + 13);
    Pen.Color := clBtnFace;       Rectangle(xPos, yPos, xPos + 12, yPos + 12);
    Pen.Color := clBtnShadow;     MoveTo(xPos, yPos + 11);
                                  LineTo(xPos, yPos);
                                  LineTo(xPos + 12, yPos);
    Pen.Color := clBlack;         MoveTo(xPos +  1, yPos + 10);
                                  LineTo(xPos +  1, yPos +  1);
                                  LineTo(xPos + 11, yPos +  1);
    // Draw tick in box is required
    if DrawTick then begin
      // Use Windows colour still
      if AEnabled then Pen.Color := clWindowText
                  else Pen.Color := clBtnShadow;
      MoveTo(xPos + 3, yPos + 5);  LineTo(xPos + 5, yPos + 7);  LineTo(xPos + 10, yPos + 2);
      MoveTo(xPos + 3, yPos + 6);  LineTo(xPos + 5, yPos + 8);  LineTo(xPos + 10, yPos + 3);
      MoveTo(xPos + 3, yPos + 7);  LineTo(xPos + 5, yPos + 9);  LineTo(xPos + 10, yPos + 4);
    end;
    // Restore Brush and Pen colours
    Brush.Color := lBColor;
    Pen.color   := lPColor;
  end;
end;  // DrawCheckBox

//==============================================================================
// Draw horizontal splitter with 3D effect.
// Use height of 5 for only one horizontal bar, and use 10 for two bars.
procedure DrawHorzSplitter(const ACanvas:TCanvas; const ASplitter:TSplitter);
var lPenColor  :TColor;
    lBrushStyle:TBrushStyle;
begin
  with ASplitter, ACanvas do begin
    // Save brush style and pen colour
    lBrushStyle:=Brush.Style;
    lPenColor  :=Pen.Color;

    Brush.Style:=bsClear;          // Use Windows colours
    Pen.Color  :=clBtnShadow;
    Rectangle(Left,Top+2,Left+Width,Top+5);
    Rectangle(Left,Top+5,Left+Width,Top+8);

    Pen.Color:=clBtnHighLight;
    MoveTo (Left+Width,Top+2);  LineTo (Left,Top+2);  LineTo (Left,Top+4);
    MoveTo (Left+Width,Top+5);  LineTo (Left,Top+5);  LineTo (Left,Top+7);

    // Restore brush style and pen colour
    Brush.Style:=lBrushStyle;
    Pen.Color  :=lPenColor;
  end;
end;  // DrawHorzSplitter

//==============================================================================
// Draw vertical splitter with 3D effect.
// Use a width of 5 for only one vertical bar, and use 10 for two bars.
procedure DrawVertSplitter(const ACanvas:TCanvas; const ASplitter:TSplitter);
var lPenColor  :TColor;
    lBrushStyle:TBrushStyle;
begin
  with ASplitter, ACanvas do begin
    // Save brush style and pen colour
    lBrushStyle:=Brush.Style;
    lPenColor  :=Pen.Color;

    Brush.Style:=bsClear;          // Use Windows colours
    Pen.Color  :=clBtnShadow;
    Rectangle(Left+2,Top,Left+5,Top+Height);
    Rectangle(Left+5,Top,Left+8,Top+Height);

    Pen.Color:=clBtnHighLight;
    MoveTo (Left+2,Top+Height);  LineTo (Left+2,Top);  LineTo (Left+4,Top);
    MoveTo (Left+5,Top+Height);  LineTo (Left+5,Top);  LineTo (Left+7,Top);

    // Restore brush style and pen colour
    Brush.Style:=lBrushStyle;
    Pen.Color  :=lPenColor;
  end;
end;  // DrawVertSplitter

//==============================================================================
{ Procedure which sets the properties of a TTextAttributes object in one go.
    Useful for handling SelAttribtes in RichEdit controls for example }
procedure SetTextAttributes(ioAttributes: TTextAttributes;
  const iFontName: string; const iFontSize: integer; iFontStyle: TFontStyles;
  const iColor: TColor);
begin
  with ioAttributes do
  begin
    Name := iFontName;
    Size := iFontSize;
    Style := iFontStyle;
    Color := iColor;
  end;
end;  // SetTextAttributes

//==============================================================================
{ Returns black or white, whichever gives the most contrast against the
    supplied colour.  Good for selecting font colours on variable colour
    background }
function GetContrastColour(iColor: TColor): TColor;
var
  lLuminance: integer; // value out of 765 (3*255)
begin
  { Break down each component (RGB) and add up the total to see if color
      is nearer black or white }
  lLuminance := ((iColor and $FF0000) shr 16) + ((iColor and $FF00) shr 8) + (iColor and $FF);
  if lLuminance >= 255 * 3 div 2 then // nearer white, so return black
    Result := clBlack
  else // vice versa
    Result := clWhite;
end;  // GetContrastColour


//==============================================================================
{ Merge 2 colours together, using the ratio as a percentage of the first colour
    used }
function MergeColours(iColour1, iColour2: TColor; iRatioPercent: byte): Longint;
var
  lRed, lGreen, lBlue: Byte;
  lColour1RGB, lColour2RGB: LongInt;

  function GetColourComponent(iAndMask: Longint; iShift: integer): byte;
  begin
    Result := Round((((lColour1RGB and iAndMask) shr iShift) / (100 / iRatioPercent)) +
                    (((lColour2RGB and iAndMask) shr iShift) / (100 / (100-iRatioPercent))))
  end;

begin
  lColour1RGB := ColorToRGB(iColour1);
  lColour2RGB := ColorToRGB(iColour2);
  lRed := GetColourComponent($FF, 0);
  lGreen := GetColourComponent($FF00, 8);
  lBlue := GetColourComponent($FF0000, 16);
  Result := RGB(lRed, lGreen, lBlue);
end;

//==============================================================================
// Show a message box with the Information icon and the OK button
procedure ShowInformation(const Msg:string);
begin
  MessageDlg(Msg, mtInformation, [mbOk], 0);
end;  // ShowInformation

//==============================================================================
// Shows a message box with Confirmation icon and Yes/No buttons
function ConfirmYesNo(const Msg:string):word;
begin
  Result:=MessageDlg(Msg, mtWarning, [mbYes, mbNo], 0);
end;  // ConfirmYesNo

//==============================================================================
// Shows a message box with Confirmation icon and Yes/No/Cancel buttons
function ConfirmYesNoCancel(const Msg:string):word;
begin
  Result:=MessageDlg(Msg, mtWarning, [mbYes, mbNo, mbCancel], 0);
end;  // ConfirmYesNoCancel

//==============================================================================
// Shows a message box with Confirmation icon and Yes/No buttons,
//  with the No button set to be the default.
function ConfirmYesNoDefaultNo(Msg: string):word;
var
  i: integer;
begin
  with CreateMessageDialog(Msg, mtWarning, [mbYes, mbNo]) do
    try
      for i := 0 to Pred(ControlCount) do
        if (Controls[i] is TButton) then
          with (Controls[i] as TButton) do
            if (Name = 'No') then TabOrder := 0;
      Result := ShowModal;
    finally
      Free;
    end;
end;    // ConfirmYesNoDefaultNo

//==============================================================================
// Shows a message box with Confirmation icon and Yes/No buttons and insert the
// user message Msg in the default string ResStr_Deletion.
function ConfirmDeletionYesNo(const Msg: string): word;
begin
  Result := MessageDlg(Format(ResStr_Deletion, [Msg]), mtWarning, [mbYes, mbNo], 0);
end;  // ConfirmDeletion

//==============================================================================
// Shows a message box with Confirmation icon and Yes/No/Cancel buttons and
// insert the user message Msg in the default string MST_DELETION.
function ConfirmDeletionYesNoCancel(const Msg: string): word;
begin
  Result := MessageDlg(Format(ResStr_Deletion, [Msg]), mtWarning, [mbYes, mbNo, mbCancel], 0);
end;  // ConfirmDeletion

//==============================================================================
// Returns TRUE if ADate is actually a valid date and FALSE if not
{$IFDEF DELPHI7UP}
function IsDate(const ADate:string):boolean;
var lDate: TDateTime;
begin
  // TryStrToDate doesn't cause exception.
  Result := TryStrToDate(ADate, lDate);
end;  // IsDate
{$ELSE}
function IsDate(const ADate:string):boolean;
begin
  Result:=true;
  try
    ConvertStringTodate(ADate);
  except
    on EConvertError do Result:=false;
  end;
end;  // IsDate
{$ENDIF}

{-------------------------------------------------------------------------------
  Tests if a string passed in is a float, without generating an exception.
}
function IsFloat(const aFloat: string): boolean;
var
  E: Extended; // throw away result
begin
  Result := TextToFloat(PChar(aFloat), E, fvExtended);
end;  // IsFloat

{-------------------------------------------------------------------------------
  Tests if a string passed in is a float, without generating an exception.
}
function IsFloat(const aFloat: string;
    const FormatSettings: TFormatSettings): Boolean;
var
  E: Extended; // throw away result
begin
  Result := TextToFloat(PChar(aFloat), E, fvExtended, FormatSettings);
end;

{-------------------------------------------------------------------------------
  Tests if a string passed in is an integer, without generating an exception.
}
{$HINTS OFF}
function IsInt(const aInt: string): boolean;
var
  E: Integer;
  lInt: integer;
begin
  // val converts to int and returns an error code
  Val(aInt, lInt, E);
  // 0=Ok
  Result := (E=0);
end;  // IsInt
{$HINTS ON}

//==============================================================================
// Generates a string by concatenating the content of all the cells on ARow,
// and according to LinearHint, separates each cell content with ' - ' of a
// carriage return so they appear each on a separate line.
function StringGridHint(AStringGrid:TStringGrid; const ARow:integer;
                        const LinearHint:boolean):string;
var lIdx:integer;
begin
  with AStringGrid do begin
    Result:=Cells[0,ARow];
    if LinearHint then
      for lIdx:=1 to ColCount-1 do
        Result:=Result+' - '+Cells[lIdx,ARow]
    else
      for lIdx:=1 to ColCount-1 do
        Result:=Result+#13+Cells[lIdx,ARow];
  end;
end;  // StringGridHint

{-------------------------------------------------------------------------------
 Takes a string separated by a specified separator, returns a string list in the
     ioStrings parameter }
procedure ParseStringIntoList(const AList, ASeparator: string; ioStrings: TStrings);
var
  lParseString: string;
begin
  ioStrings.Clear;
  { Parse the iList string into an array (if it has semi-colon separators) }
  lParseString := AList;
  while Pos(ASeparator, lParseString) <> 0 do
  begin
    { Read the next item from the parse string }
    ioStrings.Add(Copy(lParseString, 1, Pos(ASeparator, lParseString) - 1));
    { Take remaining string and continue parsing }
    lParseString := Copy(lParseString, Pos(ASeparator, lParseString) + Length(ASeparator),
        High(integer));
  end;
  { Read the final item from the parse string }
  ioStrings.Add(lParseString);
end;

//==============================================================================
{ Takes a string separated by semi colons, returns a string list in the
     ioStrings parameter }
procedure ParseSemiColonString(const AList: string; ioStrings: TStrings);
begin
  ParseStringIntoList(AList, ';', ioStrings);
end;  // ParseSemiColonString

//==============================================================================
{ Takes a string separated by commas, returns a string list in the
     ioStrings parameter }
procedure CommaSepListToStringList(const AList: string; ioStrings: TStrings);
begin
  ParseStringIntoList(AList, ',', ioStrings);
end;  // ParseSemiColonString

//==============================================================================
{ Makes a comma separated string out of a string list }
function StringListToCommaSepList(AStringList: TStringList): string;
var lIdx:integer;
begin
  Result:='';
  with AStringList do
    if Count > 0 then begin
      for lIdx:= 0 to Count - 2 do
        Result := Result + Strings[lIdx] + ', ';
      Result := Result + Strings[Count -1]
    end;
end;  // StringListToCommaSepList

//==============================================================================
{ Makes a CSV style comma separated string out of a string list }
function StringListToCSVString(iStringList: TStringList): string;
var
  liIndex: integer;
begin
  Result:='';
  with iStringList do
    if Count > 0 then
    begin
      for liIndex := 0 to Count - 2 do
        Result := Result + '"' + Strings[liIndex] + '",';
      Result := Result + '"' + Strings[Count -1] + '"'
    end;
end;  // StringListToCommaSepList

//==============================================================================
{ Makes a readable representation of a text string used for a table or field
    title.  For example, SURVEY_EVENT is returned as Survey Event }
function ReadableFormat(const iText: string): string;
var
  lstWorkingName: string;
  liIdx: integer;
begin
  lstWorkingName := iText;
  Result := '';
  repeat
    lstWorkingName := Uppercase(copy(lstWorkingName,0,1)) +
                    LowerCase(copy(lstWorkingName,2,length(lstWorkingName)));
    liIdx := Pos('_',lstWorkingName);
    if liIdx = 0 then
      Result := Result + lstWorkingName
    else
      Result := Result + copy(lstWorkingName, 0, liIdx - 1) + ' ';
    lstWorkingName := copy(lstWorkingName,liIdx + 1,length(iText));
  until liIdx = 0;
end;  // ReadableFormat

//==============================================================================
{ General purpose function to construct a plural where necessary.  For example,
    pass in 1, 'item' and the result is '1 item'.  Pass in 2, 'item' and the
    result is '2 items'. }
function GetPlural(iNumber: integer; const iDescriptor: string): string;
begin
  Result := IntToStr(iNumber) + ' ' + iDescriptor;
  if iNumber<>1 then // tag s on if necessary
    Result := Result + 's';
end;

{-------------------------------------------------------------------------------
  Internationalisation-friendly version.
}
function GetPlural(value: Integer; const singular, plural: String): String;
begin
  if value = 1 then
    Result := IntToStr(value) + ' ' + singular
  else
    Result := IntToStr(value) + ' ' + plural;
end;

//==============================================================================
{ Returns a string the same as the first one, but trimmed and without any colons
      on the end.  Useful for converting label captions to usable text }
function LopColon(const AText: string): string;
begin
  // trim whitespace
  Result := Trim(AText);
  // and lop the colon
  if AText[Length(AText)]=':' then
    Result := Copy(Result, 1, Length(Result)-1);
end;

//==============================================================================
{ Returns a string useful for creating error messages.  The string is in the
     format 'An error occurred whilst <iAction> the <iObject>.'
     For example: 'An error occurred whilst deleting the Business Centre.' }
function ProblemText(const iAction, iObject: string): string;
begin
  Result := Format(ResStr_ErrorOccured, [iAction, iObject]);
end;  // ProblemText

//==============================================================================
{ Simple procedure to facilitate building of comma separated lists.  Checks if
    list is empty, if not then ensures a comma is inserted to separate items }
procedure AddToCommaSeparatedList(var ioList: string; const iAddition: string);
begin
  if (ioList <> '') and (iAddition<>'') then
    ioList := ioList + ', ';
  ioList := ioList + iAddition;
end;  // AddToCommaSeparatedList

//==============================================================================
{ Simple procedure to facilitate building of SQL where clauses.  If list is
     empty then inserts 'WHERE', else inserts 'AND' }
procedure AddToSQLWhereClause(var ioList: string; const iAddition: string);
begin
  if ioList = '' then
    ioList := 'WHERE '
  else
    ioList := ioList + ' AND ';
  ioList := ioList + iAddition;
end;  // AddToSQLWhereClause

//==============================================================================
{ Returns a similar string as is passed in, although any single quotes are
  replaced with double quotes }
function FormatWithQuotes(const iText: string): String;
var
  lstResult: string;
  liCount: integer;
begin
  For liCount := 1 to length(iText)  do
  begin
    lstResult := lstResult + itext[liCount];
    if IssingleQuote(iText[liCount]) then
      lstResult := lstResult + '''';
  end; // for liCount
  Result := lstResult;
end;  // FormatWithQuotes

//==============================================================================
{ Returns the text passed in, but any single ampersands are returned as doubles
     so they are not lost when displayed in controls that use ampersand for
     underscore characters }
function DuplicateCharacters(const iText: string; iChar: char): String;
var
  lResult: string;
  i: integer;
begin
  lResult := '';
  for i := 1 to length(iText) do
  begin
    if iText[i]=iChar then
      lResult := lResult + iChar + iChar
    else
      lResult := lResult + iText[i];
  end;
  Result := lResult;
end;  // DuplicateCharacters


{ Returns a string safe for use in SQL, by duplicating single chars
  Created: 03/12/2002 }
function SafeQuotes(const iText: string): string;
begin
  Result := StringReplace(iText, '''', '''''', [rfReplaceAll]);
end;

//==============================================================================
{ Use to function button/menu captions as normal strings.
  Returns the text as passed in.  Any single instances of the character passed in
    are removed, and doubles are reduced to single.  So, '&Click && Hope'
    displays in a button caption as 'Click & Hope' with an underscore on the 'C'.
    This function returns 'Click & Hope' }
function RemoveDuplicateCharacters(const iText: string; iChar: char): String;
var
  lResult: string;
  i: integer;
begin
  lResult := '';
  i := 1;
  while i <= length(iText) do
  begin
    if iText[i]=iChar then
      Inc(i); // skip the character - but only 1 instance of it
    if i <= length(iText) then
      lResult := lResult + iText[i];
    Inc(i);
  end;
  Result := lResult;
end;  // RemoveDuplicateCharacters


//==============================================================================
{ Return True or False string for a boolean value.  Optional values allow
   this to be configured to return yes or no for example }
function BoolToStr(const iValue: Boolean;
         const iTrue: string = 'True'; const iFalse: string = 'False' ): string;
begin
  if iValue then
    Result := iTrue
  else
    Result := iFalse;
end;

{$IFDEF DELPHI7UP}
//==============================================================================
{ Return the name of a file, with the path AND the extension stripped off }
function ExtractWithoutExt(const iFileName: string): string;
var
  lNoPath: string;
begin
  // lop off path
  lNoPath := ExtractFileName(iFileName);
  Result := LeftStr(lNoPath, Length(lNoPath)-Length(ExtractFileExt(lNoPath)));
end;
{$ENDIF}

//==============================================================================
{ Returns long file name, if possible. ShortName can be a long name as
 well. }
function ExpandLongPathName(ShortName: string): string;
var
  PIDL: PItemIDList;
  Desktop: IShellFolder;
  WideName: WideString;
  AnsiName: AnsiString;
begin
  Result := ShortName;
  if Succeeded(SHGetDesktopFolder(Desktop)) then
  begin
    WideName := ShortName;
    if Succeeded(Desktop.ParseDisplayName(0, nil, PWideChar(WideName),
         ULONG(nil^), PIDL, ULONG(nil^))) then
    try
      SetLength(AnsiName, MAX_PATH);
      SHGetPathFromIDList(PIDL, PChar(AnsiName));
      Result := PChar(AnsiName);
    finally
      CoTaskMemFree(PIDL);
    end;
  end;

  if (ShortName <> '') and (ShortName[Length(ShortName)] = '\') and
     (Result <> '') and (Result[Length(Result)] <> '\') then
    Result := Result + '\';
end;


//==============================================================================
{ Returns the minimum value of the 2 supplied }
function Min(const iVal1, iVal2: integer): integer;
begin
  if iVal1 < iVal2 then
    Result := iVal1
  else
    Result := iVal2;
end;  // Min

//------------------------------------------------------------------------------
{ Returns the maximum value of the 2 supplied }
function Max(const iVal1, iVal2: integer): integer;
begin
  if iVal1 > iVal2 then
    Result := iVal1
  else
    Result := iVal2;
end;  // Max

//------------------------------------------------------------------------------
{ Returns a float rounded to provide a string with up to n characters.  The number
     of characters is the total number in the string, including the '-', decimal
     point, and all numbers before and after the point }
function FloatToFixedStr(iFloat: double; iCharacters: integer): string;
var
  lWorkingValue: double;
  lDigits: integer;
begin
  if Length(IntToStr(Round(iFloat)))>iCharacters then
    raise EGeneralFunctionsError.Create(ResStr_TooManyDigits);
  lWorkingValue := Abs(iFloat);
  { Find out how many digits to move the decimal point by for rounding reasons }
  if lWorkingValue < 1 then
    lDigits := (iCharacters-2)
  else
    lDigits := (iCharacters-2) - Trunc(Log10(lWorkingValue));
  if (iFloat < 0) and (lDigits > 0) then
    Dec(lDigits);  // allow one extra char for '-' symbol
  { Move decimal point }
  lWorkingValue := lWorkingValue * Power(10, lDigits);
  { Because the power function can leave a 0.5 fraction that is stored internally
     as something very slightly less, we can get 0.5 rounding down, unless
     we rebuilt the number as in the following line.  Horrible! }
  lWorkingValue := StrToFloat(FloatToStr(lWorkingValue));
  lWorkingValue := Trunc(lWorkingValue + 0.5);
  { And move it back again }
  lWorkingValue := lWorkingValue / Power(10, lDigits);
  if iFloat>=0 then
    Result := FloatToStr(lWorkingValue)
  else
    Result := '-' + FloatToStr(lWorkingValue);
end;  // FloatToFixedStr

//==============================================================================
{ Return a string defining the Operating System }
function GetOSInfo: string;
var lstPlatform: String;
    liBuildNumber: Integer;
begin
  lstPlatform := GetOSName;
  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS:
      liBuildNumber := Win32BuildNumber and $0000FFFF;
    VER_PLATFORM_WIN32_NT:
      liBuildNumber := Win32BuildNumber;
    else
      liBuildNumber := 0;
  end; // case
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) or
     (Win32Platform = VER_PLATFORM_WIN32_NT) then begin
    if Win32CSDVersion = '' then
      Result := Format('%s  %d.%.2d.%d', [lstPlatform, Win32MajorVersion,
                           Win32MinorVersion, liBuildNumber])
    else
      Result := Format('%s  %d.%.2d.%d  %s', [lstPlatform, Win32MajorVersion,
                           Win32MinorVersion, liBuildNumber, Win32CSDVersion]);
  end else
    Result := Format('%s  %d.%.2d', [lstPlatform, Win32MajorVersion,
                         Win32MinorVersion])
end;  // GetOSInfo

{===============================================================================
 Description: Returns a string describing the OS
 Created: 14/1/2003 }
function GetOSName: string;
begin
  case GetOSVersion of
    wvWin95:     Result := 'Windows 95';
    wvWin95OSR2: Result := 'Windows 95 OSR2';
    wvWin98:     Result := 'Windows 98';
    wvWin98SE:   Result := 'Windows 98 SE';
    wvWinME:     Result := 'Windows ME';
    wvWinNT:     Result := 'Windows NT';
    wvWin2k:     Result := 'Windows 2000';
    wvWinXP:     Result := 'Windows XP';
    wvWin2k3:    Result := 'Windows Server 2003';
    wvVista:     Result := 'Windows Vista';
    wvSeven:     Result := 'Windows 7';
  else
    Result :=  'Windows 32s';
  end;
end;

function GetOSVersion: TOSVersion;
begin
  Result := wvUnknown;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    case Win32MajorVersion of
      4: Result := wvWinNT;
      5: case Win32MinorVersion of
           0: Result := wvWin2k;   // 2000
           1: Result := wvWinXP;   // XP
           2: Result := wvWin2k3;  // Server 2003
         end;
      6: case Win32MinorVersion of
           0: Result := wvVista;   // Vista
           1: Result := wvSeven;   // Windows 7
      7: Result := wvFuture;   // Future version
    end
  else
    // No need to check Major on 9x, it is always 4
    case Win32MinorVersion of
      00: if (Length(Win32CSDVersion) > 0) and (Win32CSDVersion[1] in ['B', 'C']) then
            Result := wvWin95OSR2  // Win95 OEM Service Release 2
          else
            Result := wvWin95;     // Win 95
      10: if (Length(Win32CSDVersion) > 0) and (Win32CSDVersion[1] = 'A') then
            Result := wvWin98SE    // Win 98 Second Edition
          else
            Result := wvWin98;     // Win 98
      90: Result := wvWinME;       // Millenium Edition
    end;
  end;
end;

//==============================================================================
{ Return a readable string containing the total memory on the system }
function GetMemoryInfo: string;
var
  MS: TMemoryStatus;
begin
  GetOSInfo;
  MS.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(MS);
  Result := FormatFloat('#,###" KB"', MS.dwTotalPhys div 1024);
end;  // GetMemoryInfo

//==============================================================================
{ Return a list of dlls and their versions loaded currently into the
    application.  Doesn't clear the list - just adds stuff on the end }
procedure GetDllVersions(iList: TStrings);
var lDlls:TStringList;
    lIdx :integer;
begin
  lDlls := TStringlist.Create;
  try
    lDlls.Sorted:=true;  // Will prevent duplicates
    { Read the dll list }
    GetDllList(Application.ExeName,lDlls);
    { Add them into the supplied strings with version info }
    for lIdx:=0 to lDlls.Count-1 do
      iList.Add(Format('  %s (%s)',[lDlls[lIdx],GetFileVersion(lDlls[lIdx])]));
  finally
    lDlls.Free;
  end;
end;  // GetDllVersions

//==============================================================================
{ Returns the current folder set as MyDocuments }
function GetMyDocuments: String;
var
  PIDL: PItemIDList;
  MyDocsC: array[0..MAX_PATH] of Char;
  Malloc: IMalloc;
begin
  SHGetMalloc(Malloc);
  if (SHGetSpecialFolderLocation(Application.Handle, CSIDL_PERSONAL, PIDL) = NOERROR)
     and SHGetPathFromIDList(PIDL, MyDocsC) then
  begin
    Result := MyDocsC;
    Malloc.Free(PIDL)
  end
  else
    raise EInvalidOp.Create(ResStr_PersonalDocsFolderNotFound)
end;  // GetMyDocuments

{-------------------------------------------------------------------------------
  Returns the value of a special folder as specified in csidl.
}
function GetFolder(csidl: Integer; forceFolder: Boolean = False): String;
var
  i: Integer;
begin
  SetLength(Result, MAX_PATH);
  if forceFolder then
    SHGetFolderPath(0, csidl or CSIDL_FLAG_CREATE, 0, 0, PChar(Result))
  else
    SHGetFolderPath(0, csidl, 0, 0, PChar(Result));
  i := Pos(#0, Result);
  if i > 0 then begin
    SetLength(Result, Pred(i));
    Result := IncludeTrailingPathDelimiter(Result);
  end;
end;  // GetFolder

{===============================================================================
 Description: Return the internet explorer major version number installed on this machine
     (e.g 3, 4, 5, 6).  Returns -1 if not installed.
 Created: 16/12/2002 }
function GetIEMajorVersion: integer;
var
  lReg: TRegistry;
  lstVersion: string;
begin
  lReg := TRegistry.Create;
  try
    lReg.RootKey := HKEY_LOCAL_MACHINE;
    if lReg.OpenKeyReadOnly('Software\Microsoft\Internet Explorer') then begin
      if lReg.ValueExists('Version') then begin
        lstVersion := lReg.ReadString('Version');
        // read first part of version string up to .
        Result := StrToInt(Copy(lstVersion, 1, Pos('.', lstVersion)-1));
      end else
        // IE 3 does not have a version value
        Result := 3;
    end else
      Result := -1;  // not installed
  finally
    lReg.Free;
  end; // try
end;


function GetFileDate(const AFileName: string=''): TDateTime;
var
  lFileName: string;
begin
  if AFileName = '' then
    lFileName := Application.Exename
  else
    lFileName := AFileName;
  try
     result := FileDateToDateTime(FileAge(lFileName));
  except
     result := 0;
  end;
end;  // GetFileDate


//==============================================================================
{ This procedure prevents the task keys from operating in Windows 95, 98 or ME.
     It has no effect on WinNT/2000
     The keys disabled include alt-tab, ctrl-alt-del, ctrl F4 }
procedure LockTaskKeysForWin95or98orME(iLock: boolean);
begin
  case iLock of
    True :
      { Tell windows a screen saver is running }
      SystemParametersInfo(SPI_SCREENSAVERRUNNING, 1, nil, 0);
    False :
      { Tell windows a screen saver is not running }
      SystemParametersInfo(SPI_SCREENSAVERRUNNING, 0, nil, 0);
  end;
end;  // LockTaskKeysForWin95or98orME

//==============================================================================
{ This procedure minimises the amount of memory used by the application.
     The API call only works in NT or Windows 2000 }
procedure TrimWorkingSet;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    SetProcessWorkingSetSize(GetCurrentProcess, $FFFFFFFF, $FFFFFFFF);
end;  // TrimWorkingSet

//==============================================================================
{ Validate that the supplied text is a postcode.  Doesn't need to be complete }
function ValidPostCodeText(const iText: String): boolean;
var
  i, lStructureIndex: integer;
  lPreviousType, lCurrentType: TCharacterType;
  lCurrentTypeCount: integer;
  lStructure: TPostcodeStructure;
begin
  Result := True; // default
  if Length(iText)=0 then
    Exit;
  lStructureIndex := 0;
  lPreviousType := GetCharacterType(iText[1]);
  lCurrentTypeCount := 1;
  for i := 2 to Length(iText) do begin
    lCurrentType := GetCharacterType(iText[i]);
    if lCurrentType = lPreviousType then
      Inc(lCurrentTypeCount)
    else begin
      lStructure[lStructureIndex].CharacterType := lPreviousType;
      lStructure[lStructureIndex].RepeatCount := lCurrentTypeCount;
      lPreviousType := lCurrentType;
      lCurrentTypeCount := 1;
      Inc(lStructureIndex);
      if lStructureIndex > High(lStructure) then
      begin // too much for a postcode
        Result := False;
        Exit;
      end;
    end;
  end;
  lStructure[lStructureIndex].CharacterType := lPreviousType;
  lStructure[lStructureIndex].RepeatCount := lCurrentTypeCount;
  { Validate postcode structure }
  if (lStructure[0].CharacterType <> ctAlpha) or (lStructure[0].RepeatCount > 2) then
    Result := False;
  if lStructureIndex >= 1 then
    if (lStructure[1].CharacterType <> ctNumeric) or (lStructure[1].RepeatCount > 2) then
      Result := False;
  if lStructureIndex >= 2 then
    if lStructure[2].CharacterType = ctSpace then
    begin
      if lStructure[2].RepeatCount > 1 then Result := False;
      if lStructureIndex >= 3 then
        if (lStructure[3].CharacterType <> ctNumeric) or (lStructure[3].RepeatCount > 1) then
          Result := False;
      if lStructureIndex >= 4 then
        if (lStructure[4].CharacterType <> ctAlpha) or (lStructure[4].RepeatCount > 2) then
          Result := False;
      if lStructureIndex >= 5 then
        Result := False;
    end else if lStructure[2].CharacterType = ctAlpha then
    begin
      if lStructure[2].RepeatCount > 1 then Result := False;
      if lStructureIndex >= 3 then
        if (lStructure[3].CharacterType <> ctSpace) or (lStructure[3].RepeatCount > 1) then
          Result := False;
      if lStructureIndex >= 4 then
        if (lStructure[4].CharacterType <> ctNumeric) or (lStructure[4].RepeatCount > 1) then
          Result := False;
      if lStructureIndex >= 5 then
        if (lStructure[5].CharacterType <> ctAlpha) or (lStructure[5].RepeatCount > 2) then
          Result := False;
    end else Result := False;
end;  // ValidPostCodeText

//==============================================================================
{ Function to translate a character into a TCharacterType enumeration }
function GetCharacterType(iChar: char): TCharacterType;
begin
  if IsAtoZ(iChar) then
    Result := ctAlpha
  else if IsNumber(iChar) then
    Result := ctNumeric
  else if iChar=' ' then
    Result := ctSpace
  else
    Result := ctUnknown;
end;  // GetCharacterType

//==============================================================================
function IsAtoZ(const iCharacter: char): boolean;
{ Returns true if a valid uppercase letter is passed in }
const
  ACCEPTED_LETTERS = ['A'..'Z'];
begin
  Result:=iCharacter in ACCEPTED_LETTERS;
end;  // IsAtoZ
//==============================================================================
function IsNumber(const iCharacter: char): boolean;
{ Returns true if a valid number is passed in }
const
  ACCEPTED_NUMBERS = ['0'..'9'];
begin
  Result:=iCharacter in ACCEPTED_NUMBERS;
end;  // IsNumber

//==============================================================================
function IsSingleQuote(const iCharacter: char): boolean;
{ Returns true if a valid number is passed in }
const
  SINGLE_QUOTE = '''';
begin
  Result:=iCharacter = SINGLE_QUOTE;
end;  // IsSingleQuote

//==============================================================================
function IsSpace(const iCharater: char): boolean;
{ Returns true if a valid space is passed in }
const
  ACCEPTED_SPACE = ' ';
begin
  Result:=iCharater = ACCEPTED_SPACE;
end;  // IsSpace

//==============================================================================
function ShowOnlyDate(iDateAndTime: TDateTime): String;
{ If a DateTime is passed in the result is a string comprising only of the
  date portion }
begin
  Result := DateToStr(trunc(iDateAndTime));
end;  // ShowOnlyDate

//==============================================================================
function DateToSQLSvrDate(iDate: TDateTime): String;
{ When a dateTime is passed in, this returns a US formated string (MM/DD/YY)
  which is usefull for sql-server etc }

{ NOTE: This is, of course, rubbish; literal values in this form are ambiguous 
        on SQL Server because they are interpreted according to the DATEFORMAT 
        setting.  The only unambiguous numeric format is YYYYMMDD (8 digits, 
        no separator) which is interpreted independently of DATEFORMAT.

        DO NOT USE this function to form date literals for use with SQL Server.
        It is actually more useful for Access SQL.
}
var
  lDateSep: char;
begin
  lDateSep := DateSeparator;
  try
    DateSeparator := '/';
    Result := FormatDateTime('mm/dd/yy', iDate);
  finally
    DateSeparator := lDateSep;
  end; // try..finally
end;  // DateToSQLSvrDate

//==============================================================================
// Traps the exception
{$IFDEF DELPHI7UP}
function ConvertStringToDate(iDateString: string; out DateValue: TDate): Boolean;
{$ELSE}
function ConvertStringToDate(iDateString: string): TDate;
{$ENDIF}
{ Like the normal StrToDate function, although this will (unlike the
  normal one), convert dates when the date separator is a space... or
  in fact anything other than a character }
var
  liCharPos: integer;
  lChar: char;
  lstConvDate: String;
  lDateSep: char;
  liSepCount: integer;
{$IFDEF DELPHI7UP}
  lDateTime: TDateTime;
{$ENDIF}
begin
  // Store the date separator in a local variable
  liSepCount := 0;
  liCharPos := 0;
  lDateSep := DateSeparator;
  try
    // Check the string can be converted into 3 parts
    DateSeparator := '/';
    // only format for the first 3 parts
    while liSepCount < 3 do
    begin
      Inc(liCharPos);
      if liCharPos <= length(iDateString) then
      begin
        lChar := iDateString[liCharPos];
        if lChar in ['0'..'9'] then
          lstConvDate := lstConvDate + lChar
        else begin
          if liSepCount < 2 then
            lstConvDate := lstConvDate + DateSeparator;
          Inc(liSepCount);
        end; // else
      end // if
      else
        Break;
    end; // while
    {$IFDEF DELPHI7UP}
    Result := TryStrToDate(lstConvDate, lDateTime);
    if Result then DateValue := lDateTime;
    {$ELSE}
    Result := StrToDate(lstConvDate);
    {$ENDIF}
  finally
    DateSeparator := lDateSep
  end; // try.. finally
end;  // ConvertStringToDate

//==============================================================================
function CurrentRichText(const AControl: TCustomRichEdit): TTextAttributes;
begin
  Result := TCustomRichEdit(AControl).SelAttributes
end;  // CurrentRichText

//==============================================================================
procedure SetRTFStyle(AControl: TCustomRichEdit; const AStyle: TFontStyle);
var
  lStyles: TFontStyles;
begin
  lStyles := CurrentRichText(AControl).Style;
  if AStyle in lStyles then Exclude(lStyles, AStyle)
                       else Include(lStyles, AStyle);
  CurrentRichText(AControl).Style := lStyles;
end;  // SetRTFStyle

//==============================================================================
procedure SetRTFBold(AControl: TCustomRichEdit);
begin
  SetRTFStyle(AControl, fsBold);
end;  // SetRTFBold

//==============================================================================
procedure SetRTFItalic(AControl: TCustomRichEdit);
begin
  SetRTFStyle(AControl, fsItalic);
end;  // SetRTFItalic

//==============================================================================
procedure SetRTFUnderline(AControl: TCustomRichEdit);
begin
  SetRTFStyle(AControl, fsUnderline);
end;  // SetRTFUnderline


{-------------------------------------------------------------------------------
  Extract the RTF from the control and returns it as a string. This takes
  TRichEdit NOT TCustomRichEdit, because if TCustomRichEdit.Lines.SaveToStream
   is called you only get plaintext - must be an inheritance bug somewhere.
}
function GetRTFText(AControl: TRichEdit): String;
var
  stream: TStringStream;
begin
  stream := TStringStream.Create('');
  try
    AControl.Lines.SaveToStream(stream);

    // Quotes in the RTF for SQL need escaping by making them double.
    // Skip the null termination character of the stream (size - 1).
    stream.Position := 0;
    result := StringReplace(
        StringReplace(
            stream.ReadString(stream.Size - 1),
            #13#10, '', [rfReplaceAll]),
        #39, #39#39, [rfReplaceAll]);
  finally
    stream.Free;
  end;
end;  // GetRTFText

//==============================================================================
function GetWindowsTempDir: String;
var
  BufLen: Integer;
  TempPath: AnsiString;
begin
  BufLen := GetTempPath(0, PChar(TempPath));

  if BufLen > 0 then
  begin
    SetLength(TempPath, BufLen);
    BufLen := GetTempPath(BufLen, PChar(TempPath));
  end;

  if BufLen = 0 then
    {$IFDEF DELPHI6UP}
    RaiseLastOSError;
    {$ELSE}
    RaiseLastWin32Error;
    {$ENDIF}

  Result := Copy(TempPath, 1, BufLen);  // removes zero terminator
end;  // GetWindowsTempDir

{-------------------------------------------------------------------------------
 A unique file name within the specified directory, beginning with the given
 prefix.  Path must be no longer than MAX_PATH - 14 characters.  If Prefix is
 longer than three characters then the remainder is ignored.
}
function UniqueFileName(const Path: String; const Prefix: String): String;
var
  Buffer: PChar;
begin
  Buffer := StrAlloc(MAX_PATH);
  try
    if GetTempFileName(PChar(Path), PChar(Prefix), 0, Buffer) = 0 then
      {$IFDEF DELPHI6UP}
      RaiseLastOSError;
      {$ELSE}
      RaiseLastWin32Error;
      {$ENDIF}

    Result := Buffer;
  finally
    StrDispose(Buffer);
  end;
end;  // UniqueFileName

{-------------------------------------------------------------------------------
  Description: Returns a filename guaranteed to be unique, by appending digits
  to the supplied filename when necessary.
  Created: 03/07/2008 }
function GetUniqueFileName(const AFile: string): string;
var
  lFileNameWithoutExtension, lExtension: string;
  i: integer;
begin
  if FileExists(AFile) then begin
    lExtension := ExtractFileExt(AFile);
    lFileNameWithoutExtension := ChangeFileExt(AFile, '');
    i := 1;
    while FileExists(lFileNameWithoutExtension + '-' + IntToStr(i) + lExtension) do
      Inc(i);
    Result := lFileNameWithoutExtension + '-' + IntToStr(i) + lExtension;
  end else
    Result := AFile;
end;

{-------------------------------------------------------------------------------
  Description: Returns the path to the windows directory
  Created: 05/03/2003 }
function GetWinDir: String;
var
  szBuffer: PChar;
begin
  //Allocate buffer
  szBuffer:= StrAlloc(255);

  //Get Windows temp directory
  GetWindowsDirectory(szBuffer, 255);
  Result:= szBuffer;

  //Dispose buffer
  StrDispose(szBuffer);
end;  // GetWinDir


//==============================================================================
{ Returns a character representing the CD ROM drive, eg d.  Returns #0 if no
    CD ROM found }
function GetCDDrive: Char;
var
  lDrive: Char;
begin
  for lDrive := 'a' to 'z' do
    if GetDriveType(PChar(lDrive + ':\')) = DRIVE_CDROM then
    begin
      Result := lDrive; // drive is CD Rom
      Break;
    end else
      Result := #0;
end;  // GetCDDrive

//==============================================================================
{ Returns equivalent of iComponent.items[iComponent.itemindex] }
function SelItemText(iComponent: TRadioGroup): string;
begin
  Result := iComponent.Items[iComponent.ItemIndex];
end;  // SelItemText

//------------------------------------------------------------------------------
{ Returns equivalent of iComponent.items[iComponent.itemindex] }
function SelItemText(iComponent: TComboBox): string;
begin
  Result := iComponent.Items[iComponent.ItemIndex];
end;  // SelItemText

//------------------------------------------------------------------------------
{ Returns equivalent of iComponent.items[iComponent.itemindex] }
function SelItemText(iComponent: TListBox): string;
begin
  Result := iComponent.Items[iComponent.ItemIndex];
end;  // SelItemText

//==============================================================================
{As MessageDlg but allows the user to specify the default button.}
function DefMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; HelpCtx: Longint): integer;
const ModalResults: array[TMsgDlgBtn] of Integer =
       (mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll, mrYesToAll, 0);
var k: Integer;
begin
  with CreateMessageDialog(Msg, DlgType, Buttons) do
    try
      HelpContext := HelpCtx;
      HelpFile := '';
      Position := poScreenCenter;
      if DefButton in Buttons then
        for k := ControlCount - 1 downto 0 do
          if Controls[k] is TButton then
            with TButton(Controls[k]) do begin
              Default := ModalResult = ModalResults[DefButton];
              if Default then TabOrder := 0;
            end;
      Result := ShowModal;
    finally
      Free;
    end;
end;  // DefMessageDlg

//==============================================================================
{Anyone needing an explanation for this function should not be allowed to program a computer.}
function DiskInDriveA: boolean;
var OldMode: Cardinal;
begin
  Result := False;
  OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try if DirectoryExists('A:\') then Result := True;
  finally SetErrorMode(OldMode) end;
end;  // DiskInDriveA

{-------------------------------------------------------------------------------
  Remove a folder and all it's content, recursively since a folder can't be removed
  unless it's empty.
}
function RemoveFolderAndContent(const path: String): Boolean;
var
  sr: TSearchRec;
begin
  // Assume it's all gonna work out just fine.
  Result := True;
  if FindFirst(IncludeTrailingPathDelimiter(path) + '*.*', faAnyFile, sr) = 0 then
    repeat
      if (sr.Name <> '.') and (sr.Name <> '..') then
        if sr.Attr and faDirectory > 0 then
          Result := Result
              and RemoveFolderAndContent(IncludeTrailingPathDelimiter(path) + sr.Name)
        else begin
          Result := Result
              and DeleteFile(IncludeTrailingPathDelimiter(path) + sr.Name);
        end;
    until (FindNext(sr) <> 0) or (not Result);
  FindClose(sr);
  // Folder empty, it can now be deleted.
  Result := Result and RemoveDir(path);
end;

//==============================================================================
{Deletes all files from the given Location with the given FileName.
  Example: Location = "C:\Database\", FileName := "DBName.*".
 Returns True unless it finds a file it cannot delete.}
function DeleteFiles(const Location, FileName: string): Boolean;
var FileRecord: TWIN32FindData;
    AHandle: THandle;
begin
  AHandle := FindFirstFile(PChar(Location + FileName), FileRecord);
  Result := True;
  if AHandle <> INVALID_HANDLE_VALUE then
    try
      repeat Result := DeleteFile(PChar(Location + FileRecord.cFileName)) and Result;
      until not FindNextFile(AHandle, FileRecord);
    finally
      Windows.FindClose(AHandle);
    end;
end;  // DeleteFiles

//==============================================================================
{ Function to copy abc.* to a given path }
procedure CopyFilesLike(iFileName,iPath:String);
  var
  lSearchRecName:TSearchRec;
begin
  if FindFirst(ExtractFilePath(iFileName) +Copy(ExtractFileName(iFileName), 1,
             ansiPos('.',ExtractFileName(iFileName))-1)+'.*',faAnyFile, lSearchRecName)=0 then
    try
      repeat
        if CompareText(iFileName,lSearchRecName.Name)<>0 then
          gpcApiCheck(CopyFile(PChar(lSearchRecName.Name),
                     PChar(iPath+ExtractFileName(lSearchRecName.Name)), false));
      until FindNext(lSearchRecName)<>0;
    finally
      FindClose(lSearchRecName);
    end;
end;  // CopyFilesLike

//==============================================================================
{ Attempt to shell the given filename }
procedure ShellFile(const AFileName: String);
var lszCmdString: Array[0..255] of Char;
    lszFileString: Array[0..255] of Char;
    liRetCode  : Longint;
begin
  if AFileName <> '' then begin
    { start with default setting to use windows extension matching }
    StrPCopy(lszCmdString, AFileName);
    StrPCopy(lszFileString, '');
    liRetCode := ShellExecute(0, 'open', lszCmdString, lszFileString, '', SW_SHOW);
    if liRetCode = SE_ERR_NOASSOC then
      TryQuickView(AFileName)
    else
      CheckShellExecuteRetCode(liRetCode);
  end; // if
end;  // ShellFile

//==============================================================================
{ Assuming a normal ShellExecute has failed, this procedure will try to shell
     an external reference using quick view }
procedure TryQuickView(const AFileName: String);
var loReg          : TRegistry;
    lsQuikViewPath : String;
    lszWindowsDirPath: Array[0..255] of Char;
    lszCmdString   : Array[0..255] of Char;
    lszFileString  : Array[0..255] of Char;
    liRetCode      : Integer;
begin
  // find out if this machine has quikview.exe
  loReg := TRegistry.Create;
  try
    loReg.RootKey := HKEY_CLASSES_ROOT;
    if loReg.KeyExists('QUICKVIEW') then begin
      {False because we do not want to create it if it doesn't exists}
      if loReg.OpenKey('QuickView\shell\open\command', false) then begin
        StrPCopy(lszFileString, AFileName);
        lsQuikViewPath := loReg.ReadString('');
        { Check for SystemRoot use and convert to real path }
        if Pos('%SystemRoot%', lsQuikViewPath) <> 0 then begin
          GetWindowsDirectory(lszWindowsDirPath, 255);
          lsQuikViewPath := lszWindowsDirPath +
                            Copy(lsQuikViewPath,
                            Pos('%SystemRoot%', lsQuikViewPath) + 12, 255);
        end;
        StrPCopy(lszCmdString, lsQuikViewPath);
      end;
      liRetCode := ShellExecute(0, 'open', lszCmdString, lszFileString, '', SW_SHOW);
      CheckShellExecuteRetCode(liRetCode);
    end;
  finally
    loReg.CloseKey;
    loReg.Free;
  end; // try
end;  // TryQuickView

//==============================================================================
{ Check the return code from a call to ShellExecute.  Display a message dialog
     if the call failed }
procedure CheckShellExecuteRetCode(const ARetCode: Integer);
var lstMessage: String;
begin
  case ARetCode of
    0                     : lstMessage := ResStr_OSOutOfMemory;
    ERROR_FILE_NOT_FOUND  : lstMessage := ResStr_FileNotFound;
    ERROR_PATH_NOT_FOUND  : lstMessage := ResStr_PathNotFound;
    ERROR_BAD_FORMAT      : lstMessage := ResStr_BadFormat;
    SE_ERR_ACCESSDENIED   : lstMessage := ResStr_AccessDenied;
    SE_ERR_ASSOCINCOMPLETE: lstMessage := ResStr_AssocIncomplete;
    SE_ERR_DDEBUSY        : lstMessage := ResStr_DDEBusy;
    SE_ERR_DDEFAIL        : lstMessage := ResStr_DDEFail;
    SE_ERR_DDETIMEOUT     : lstMessage := ResStr_DDETimeout;
    SE_ERR_DLLNOTFOUND    : lstMessage := ResStr_DLLNotFound;
    SE_ERR_OOM            : lstMessage := ResStr_ErrOOM;
    SE_ERR_SHARE          : lstMessage := ResStr_ErrShare;
  end; // case
  if lstMessage <> '' then MessageDlg(lstMessage, mtWarning, [mbOk], 0);
end;  // CheckRetCode

{Clear out the contents of a list box, freeing any objects referred to}
procedure ClearListBoxObjects(iComponent: TListBox);
var
  i: integer;
begin
  for i := 0 to Pred(iComponent.Items.Count) do
    iComponent.Items.Objects[i].Free;
  iComponent.Clear;
end;

//==============================================================================
// Adds a row at the bottom of sgGrid, and adds stText to the cell at location
// [iTestColumn,RowCount-1]. By default, it will add nothing in [0,Rowcount-1].
// Also checks if there is any need to actually add an empty row.
procedure AddLineInGrid(const AGrid:TStringGrid; const ATestColumn: Byte = 0;
  const AText:string = '');
begin
  with AGrid do begin
    if (RowCount = 0) or (Cells[ATestColumn, RowCount - 1] <> '') then  // Add line only if last not empty
      RowCount := RowCount + 1;
    Row := RowCount - 1;               // Set current row to last, added or not
    Cells[ATestColumn, RowCount-1] := AText;  // Put text in cell
    Refresh;
  end;
end;  // AddLineInGrid

//==============================================================================
// Removes the current Row from sgGrid if not the first row after FixedRows
// and, if needed, moves the rest of the grid up
procedure DelLineInGrid(const AGrid:TStringGrid);
var iCount:integer;
begin
  with AGrid do begin  // Do not rely on having 1 fixed row
    if (RowCount > 0) and (RowCount <= FixedRows + 1) and (Row = FixedRows) then  // On first row after fixed row, if any
      Rows[Row].Clear  // Just clear row content
    else begin                                // Further down the grid
      for iCount := Row + 1 to RowCount - 1 do      // Copying while overwrite current Row
        Rows[iCount - 1] := Rows[iCount];
      Rowcount := RowCount - 1;                   // Only need to descrease RowCount now
    end;
    Refresh;
  end;
end;  // DelLineInGrid

//==============================================================================
function IIF(const AExpression: boolean; iSuccess, iFailure: Variant): Variant;
begin
  if AExpression then
    Result := iSuccess
  else
    Result := iFailure;
end; //IIF

//==============================================================================
procedure WaitFor(const Seconds:integer);
var
  lBefore: TDateTime;
  H,M,S,mS: Word;
begin
  lBefore := Now;
  S := 0;
  while (S + mS / 1000) < Seconds do begin
    DecodeTime(Now - lBefore,H,M,S,mS);
    Application.ProcessMessages;
  end;
end;  // WaitFor

//==============================================================================
// Determine if the current user has Admin rights
function IsAdmin: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));

const
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS     = $00000220;

var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  i: Integer;
  bSuccess: BOOL;
begin
  {$RANGECHECKS OFF} // required because of the way PTokenGroups works
  // No security to speak of on Win95/98/ME
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    Result := true
  else begin
    // However, on NT/2000/XP
    Result := False;
    bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True,
                                hAccessToken);
    if not bSuccess then begin
      if GetLastError = ERROR_NO_TOKEN then
      bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY,
                                   hAccessToken);
    end;

    if bSuccess then begin
      GetMem(ptgGroups, 1024);
      bSuccess := GetTokenInformation(hAccessToken, TokenGroups,
                                      ptgGroups, 1024, dwInfoBufferSize);
      CloseHandle(hAccessToken);
      if bSuccess then begin
        AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
                                 SECURITY_BUILTIN_DOMAIN_RID,
                                 DOMAIN_ALIAS_RID_ADMINS,
                                 0, 0, 0, 0, 0, 0, psidAdministrators);
        for i := 0 to ptgGroups.GroupCount - 1 do
          if EqualSid(psidAdministrators, ptgGroups.Groups[i].Sid) then begin
            Result := True;
            Break;
          end;
        FreeSid(psidAdministrators);
      end;
      FreeMem(ptgGroups);
    end;
  end;
end;  // IsAdmin

//==============================================================================
// Check a character isn't an invalid one fot a file name
function ValidateCharForFilename(const Key: char): char;
begin
  if Key in InvalidFileNameCharSet then Result := #0
                                   else Result := Key;
end;

//==============================================================================
// Removes any invalid characters from given filename
function ValidateFileName(const AFileName: String): String;
var i: Integer;
begin
  Result := AFileName;
  i := 1;
  while i <= Length(Result) do begin
    if Result[i] in InvalidFileNameCharSet then
      Result := Copy(Result, 1, i - 1) + Copy(Result, i + 1, Length(Result))
    else
      Inc(i);
  end;
end;

//==============================================================================
///This formats a table name for use in SQL server.
function FormatWithSquareBrackets(AstText: String): String;
begin
  Result := '[' + DuplicateCharacters(AstText, ']') + ']';
end;

{$IFDEF DELPHI7UP}
{===============================================================================
 Description: Sets the list box hint to detail the item if the list box
   is not wide enough.   This should be called from the mouse move event:
   SetListBoxHintIfTooWide(Sender, X, Y);
 Created: 04/02/3004}
procedure SetListBoxHintIfTooWide(Sender: TObject; X, Y: Integer);
var
  liItemIndex: integer;
  lstNewHint: string;
begin
  // find the current item's text
  liItemIndex := TListBox(Sender).ItemAtPos(Point(x, y), True);
  if liItemIndex=-1 then
    lstNewHint := ''
  else
    lstNewHint := TListBox(Sender).Items[liItemIndex];
  // check if we need a hint because of the width of the list box
  if TListBox(Sender).Canvas.TextWidth(lstNewHint)<=TListBox(Sender).ClientWidth then
    lstNewHint := '';
  if TListBox(Sender).Hint<>lstNewHint then begin
    TListBox(Sender).Hint:=lstNewHint;
    Application.CancelHint;
    Application.ActivateHint(Point(x, y));
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------
function RemoveAmpersands(const AString: String): String;
begin
  Result := StringReplace(AString, '&', '', [rfReplaceAll]);
end;

//==============================================================================
procedure ResetListBoxExtent(AListBox: TCustomListBox);
var i, lScrollWidth: Integer;
begin
  lScrollWidth := 0;
  // Find the maximum width.
  for i := 0 to AListbox.Items.Count - 1 do
    if lScrollWidth < AListBox.Canvas.TextWidth(AListbox.Items[i]) then
      lScrollWidth := AListBox.Canvas.TextWidth(AListbox.Items[i]);
  // If scroll bar needed, show it, with correct width.
  SendMessage(AListbox.Handle, WM_HSCROLL,MAKELONG(0, SB_TOP), AListbox.Handle);
  if lScrollWidth > AListbox.Width then
    SendMessage(AListbox.Handle, LB_SETHORIZONTALEXTENT, lScrollWidth + 4, 0)
  else
    // Otherwise, remove the scrollbar
    SendMessage(AListbox.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
end;

//==============================================================================
function GetFileNameIcon(const AFileName: String; AFlags: Cardinal = 0): HICON;
var
  lFileInfo: TSHFileInfo;
  lImageList: HIMAGELIST;
begin
  FillChar(lFileInfo, SizeOf(lFileInfo), #0);
  if AFlags = 0 then
    AFlags := SHGFI_SHELLICONSIZE;
  lImageList := SHGetFileInfo(PChar(AFileName), 0, lFileInfo, SizeOf(lFileInfo),
                             AFlags or SHGFI_SYSICONINDEX);
  if lImageList <> 0 then
    Result := ImageList_ExtractIcon(0, lImageList, lFileInfo.iIcon)
  else
    Result := 0;
end;

{-------------------------------------------------------------------------------
  Removes all specified substrings from the given string.
}
function RemoveSubStrings(const AString: String; ASubStrings: Array of String): String;
var
  i: Integer;
begin
  Result := AString;
  for i := 0 to High(ASubStrings) do
    Result := StringReplace(Result, ASubStrings[i], '', [rfReplaceAll]);
end;

{-------------------------------------------------------------------------------
  Function that returns true if ALL parents of a control are visible.
}
function AllParentsAreVisible(AControl: TControl): boolean;
var
  lParent: TControl;
begin
  Result := true;
  lParent := AControl.Parent;
  while Assigned(lParent) do begin
    if not lParent.Visible then begin
      Result := false;
      break; // from while loop
    end; // if
    lParent := lParent.Parent;
  end;
end;

{-------------------------------------------------------------------------------
  Sets the titles of the grid's columns. Ideally to use with resource strings.
}
procedure SetGridColumnTitles(grid: TStringGrid; const titles: Array of String);
var
  i: Integer;
begin
  for i := 0 to High(titles) do
    if i < grid.ColCount then
      grid.Cells[i, 0] := titles[i];
end;

{-------------------------------------------------------------------------------
  function replace the doublequote '"' to backslash'\'  which accept by ADO component
}
function QuoteEscapeStr(Value: String): String;
var i:Integer;
begin
  for i:=length (Value) downto 1 do
    if Value[i] in ['\', '''', '"', #0] then
      begin
        if Value[i]=#0 then
          Value[i]:='0';
        Insert ('\', Value, i);
      end;
      Value:=QuotedStr(StringReplace (Value, '"', '', [rfReplaceAll]));
  Result := Value;
end;

end.
