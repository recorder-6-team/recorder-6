unit HTMLDisplayFuncs;

interface

uses
  Windows, Classes, SysUtils;

const
  ST_SOURCE_DENOT_CHAR   = '*';
  CHECKLIST_DENOTE_CHAR  = '%';
  HEADING_DENOTE_STRING  = '#HEADING';

resourcestring
  ResStr_GoBackToChkList =  'Go Back to current checklist details';
  ResStr_NoFurtherInfoPresent = 'No further information is present in this checklist';

{STYLES}
function AddTagText(const iTag, iText,iOptions : string) : string;

function Anchor(const iName, ihRef, iText : string) : string;

function AnchorCheckList(const iCheckList, iListKey, iPreferredNameKey : string) : string;

function AnchorHeading(const iHeading : string) : string;

function Font(const iName:string; const iSize:integer; const iColour:string;
  const iText:string):string;

function FontStyle(const iBold, iItalic, iUnderlined:Boolean; const iText:string):string;

function Header(const iLevel:Integer; const iText:string):string;

function LineBreak : string;

function Para(const iText:string):string;

procedure ParaSL(var ioslSection:TStringList);

function SubHeaderStyle(const iHeader:string):string;

function MiniHeaderStyle(const iHeader:string):string;

function MainHeaderStyle(const iHeader:string):string;

function AddCheckListRow(const iImagePath, iText:string):string;

function LinkBackToMain: string;

function NoNewInfo: string;

function LineSpace: string;

function Bracket( ABracketMe: string ): string;

function Style(): string;
//==============================================================================
implementation

function AddTagText(const iTag, iText,iOptions : string) : string;
var
  lStartTag : string;
  lEndTag : string;
begin
  lStartTag:= '<' + iTag + ' ' + iOptions + '>';
  lEndTag  := '</'+ iTag + '>';

  Result := lStartTag + iText + lEndTag;
end;  // AddTagText

//==============================================================================
{Paragraphing for string}
function Para(const iText:string):string;
begin
  Result := AddTagText('P',iText,'');
end;  // Para

//==============================================================================
{Paragraphing for TStringList}
procedure ParaSL(var ioslSection:TStringList);
begin
  ioslSection.Insert(0,'<P>');
  ioslSection.Add('</P><BR>');
end;  // ParaSL

//==============================================================================
function Header(const iLevel:Integer; const iText:string):string;
begin
  Result := AddTagText('H' + IntToStr(iLevel),iText,'');
end;  // Header

//==============================================================================
function LineBreak:string;
begin
  Result := '<BR>';
end;  // LineBreak

//==============================================================================
function FontStyle(const iBold, iItalic, iUnderlined:Boolean;
  const iText:string):string;
var
  lstText : string;
begin
  lstText := iText;
  if iBold then
    lstText := AddTagText('B',lstText,'');
  if iItalic then
    lstText := AddTagText('I',lstText,'');
  if iUnderlined then
    lstText := AddTagText('U',lstText,'');

  Result := lstText;
end;  // FontStyle

//==============================================================================
function Font(const iName:string; const iSize:integer; const iColour:string;
  const iText:string):string;
var
  lstOptions : string;
begin
  lstOptions := '';
  if iName<>'' then
    lstOptions := lstOptions + ' NAME=' + iName;
  if iSize > 0 then
    lstOptions := lstOptions + ' SIZE=' + IntToStr(iSize);
  if iColour<>'' then
    lstOptions := lstOptions + ' COLOR=' + iColour;

  Result := AddTagText('FONT',iText, lstOptions);
end;  // Font

//==============================================================================
function MainHeaderStyle(const iHeader:string):string;
begin
  {Set Style of header}
  Result := FontStyle(True, False, False, Font('Arial', 5, '000080', iHeader));
end;  // MainHeaderStyle

//==============================================================================
function SubHeaderStyle(const iHeader:string):string;
begin
  Result := Font('Arial', 4, '000080', iHeader) + LineBreak;    // Header size
end;  // SubHeaderStyle

//==============================================================================
function MiniHeaderStyle(const iHeader: string): string;
begin
  Result := FontStyle(True, False, False, Font('Arial', 4, '000080', iHeader)) + LineBreak;    // Header size
end;

//==============================================================================
function Anchor(const iName, ihRef, iText : string) : string;
begin
  Result := '<A';
  if ihRef<>'' then
    Result := Result + ' HREF="' + ST_SOURCE_DENOT_CHAR + ihRef + '"';
  if iName<>'' then
    Result := Result + ' NAME="' + iName + '"';
  if iText<>'' then
    Result := Result + '>' + iText + '</A>'
  else
    Result := Result + '></A>'
end;  // Anchor

//==============================================================================
{ Flags a Checklist as a hotspot}
function AnchorCheckList(const iCheckList, iListKey, iPreferredNameKey : string) : string;
begin
  Result := '<A HREF="' +
            CHECKLIST_DENOTE_CHAR  + iListKey + iPreferredNameKey +
            '">' +
            iCheckList + '</A>';
end;

//==============================================================================
function AnchorHeading(const iHeading : string) : string;
begin
  Result := '<A NAME="' + HEADING_DENOTE_STRING + '">'
             + MainHeaderStyle(iHeading) +
             '</A><HR>';
end;

//==============================================================================
function AddCheckListRow(const iImagePath, iText:string) : string;
begin
  Result := '<TR>' +
              '<TD>&nbsp;&nbsp;&nbsp;' + iText + '</TD>' +
            '</TR>';
end;

//==============================================================================
function LinkBackToMain: string;
begin
  Result := '<P>' +
            '<A HREF="' + HEADING_DENOTE_STRING + '">' +
            ResStr_GoBackToChkList +
            '</A></P>';
end;

//==============================================================================
function NoNewInfo: string;
begin
  Result := '<LI><I>' +
            ResStr_NoFurtherInfoPresent +
            '</I></LI>';
end;

//==============================================================================
function LineSpace: string;
begin
  Result:='<div style="height:20px"> </div>'
end; // LineSpace

//==============================================================================
function Bracket( ABracketMe: string ): string;
begin
  if ABracketMe[1] = '(' then Result := ABracketMe
                         else Result := '('+ABracketMe+')';
end; // Bracket

//==============================================================================

function Style(): string;
begin
  result := '<Style type = "text/css">A:link{color:blue;}A:visited{color:purple;}A:active{color:blue;}A:hover{color:red;}' +
         'body{font-family:arial;}'+
         'H1{color:green;padding-top:10px;padding-bottom:10px;margin:0px}' +
         'H2,H3,H4,H5,H6{color:midnightblue;padding-top:5px;padding-bottom:5px;margin:0px}' +
         'td{background-color:white;}'+
         'th{background-color:lightgoldenrodyellow;}'+
         'P{padding-top:5px;padding-bottom:5px;margin:0px;}'+
         '</style>';

end;


//==============================================================================


end.
