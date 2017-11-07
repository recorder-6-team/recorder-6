//==============================================================================
{
   DisplayFuncs unit
   Provides general purpose display functions for Delphi.
   Date 31/3/99
}
//==============================================================================
unit DisplayFuncs;

interface

uses
  Windows, Graphics, Messages, SysUtils, Classes, ComCtrls, Grids;

function DrawChoppedText( const iText : string; iCanvas : TCanvas;
  const iRect : TRect; const iGap:byte=0 ): string;

procedure DrawCheckBox(const ACanvas:TCanvas; const xPos,yPos:integer;
                       const DrawTick:boolean);

procedure DrawVertSplitter(Const ACanvas:TCanvas; const Left,Top,Height:integer);

procedure AddLineInGrid(const sgGrid:TStringGrid; const iTestColumn:byte = 0;
                        const stText:string = '');

procedure DelLineInGrid(const sgGrid:TStringGrid);

procedure AddRichText(const ARichEdit:TRichEdit; const NewText:string;
                      const TextFontSize:integer; const TextFontStyle:TFontStyles;
                      const TextFontColor:TColor; const StartOnNewLine:boolean);

function GetTextWithinLimit(const ACanvas:TCanvas; const AString:string;
  const ALimit:integer):string;

//==============================================================================
implementation

//==============================================================================
{ General purpose function to draw text onto a provided canvas.  If the text
    is too big for the rectangle, then the text is shortened and '...' is added
    to the end }
function DrawChoppedText( const iText : string; iCanvas : TCanvas;
  const iRect : TRect; const iGap:byte=0 ): string;
var
  lOutputText   : string;
  lCharCount    : integer;
  lDisplayWidth : integer;
  lTextLeader   : integer;
begin
  lOutputText := iText;
  with iCanvas do
  begin
    lDisplayWidth := iRect.Right - iRect.Left-(2*iGap);
    { Do we chop off extra characters and add an ellipses? }
    if lDisplayWidth < iCanvas.TextWidth(lOutputText) then
    begin
      { As we are concatenating the characters, put in space for ellipses }
      lDisplayWidth := lDisplayWidth - iCanvas.TextWidth('...');
      { Find out the minimum number of chars we can definitely display }
      lCharCount := lDisplayWidth div iCanvas.TextWidth('W');
      while iCanvas.TextWidth(Copy(lOutputText, 1, lCharCount)) < lDisplayWidth do
        Inc(lCharCount);
      { Note - we overshoot the mark so come back 1 character }
      lOutputText := Copy(lOutputText, 1, lCharCount-1) + '...';
    end;
    { Stick the text on the canvas }
    lTextLeader := ((iRect.Bottom - iRect.Top) -
                                  iCanvas.TextHeight(lOutputText)) div 2;
    TextOut(iRect.Left+iGap, iRect.Top+lTextLeader,lOutputText);
  end;
end;

//==============================================================================
// Draws a 3D Checkbox on ACanvas at the xPos,yPos position.
// DrawTick indicates if the box is checked or not
procedure DrawCheckBox(const ACanvas:TCanvas; const xPos,yPos:integer;
                       const DrawTick:boolean);
var lBColor,lPColor:TColor;
begin
  with ACanvas do begin
    // Save Brush and Pen colours
    lBColor:=Brush.Color;
    lPColor:=Pen.Color;

    Brush.Color:=clWindow;  // Use Windows colours
    Pen.Style  :=psSolid;
    Pen.Color  :=clBtnHighlight;  Rectangle(xPos,yPos,xPos+13,yPos+13);
    Pen.Color  :=clBtnFace;       Rectangle(xPos,yPos,xPos+12,yPos+12);
    Pen.Color  :=clBtnShadow;     MoveTo(xPos,yPos+11);    LineTo(xPos,yPos);
                                                           LineTo(xPos+12,yPos);
    Pen.Color  :=clBlack;         MoveTo(xPos+1,yPos+10);  LineTo(xPos+ 1,yPos+ 1);
                                                           LineTo(xPos+11,yPos+1);

    if DrawTick then begin  // Use black colour to draw tick
      Pen.Color:=clBlack;
      MoveTo(xPos+3,yPos+5);  LineTo(xPos+5,yPos+7);  LineTo(xPos+10,yPos+2);
      MoveTo(xPos+3,yPos+6);  LineTo(xPos+5,yPos+8);  LineTo(xPos+10,yPos+3);
      MoveTo(xPos+3,yPos+7);  LineTo(xPos+5,yPos+9);  LineTo(xPos+10,yPos+4);
    end;
    // Restore Brush and Pen colours
    Brush.Color:=lBColor;
    Pen.color  :=lPColor;
  end;
end;  // DrawCheckBox

//==============================================================================
// Draw a 3D vertical splitter on ACanvas.
procedure DrawVertSplitter(Const ACanvas:TCanvas; const Left,Top,Height:integer);
begin
  with ACanvas do begin
    Brush.Style:=bsClear;          // Use Windows colours
    Pen.Color  :=clBtnShadow;
    Rectangle(Left+2,Top,Left+5,Top+Height);
    Rectangle(Left+5,Top,Left+8,Top+Height);

    Pen.Color:=clBtnHighLight;
    MoveTo (Left+2,Top+Height);  LineTo (Left+2,Top);  LineTo (Left+4,Top);
    MoveTo (Left+5,Top+Height);  LineTo (Left+5,Top);  LineTo (Left+7,Top);
  end;
end;  // DrawVertSplitter

//==============================================================================
// Adds a row at the bottom of sgGrid, and adds stText to the cell at location
// [iTestColumn,RowCount-1]. By default, it will add nothing in [0,Rowcount-1].
// Also checks if there is any need to actually add an empty row.
procedure AddLineInGrid(const sgGrid:TStringGrid; const iTestColumn:byte = 0;
                        const stText:string = '');
begin
  with sgGrid do begin
    if (RowCount=0) or (Cells[iTestColumn,RowCount-1]<>'') then  // Add line only if last not empty
      RowCount:=RowCount+1;
    Row:=RowCount-1;                 // Set current row to last, added or not                     
    Cells[iTestColumn,Row]:=stText;  // Put text in cell
    Refresh;
  end;
end;  // AddLineInGrid

//==============================================================================
// Removes the current Row from sgGrid if not the first row after FixedRows
// and, if needed, moves the rest of the grid up
procedure DelLineInGrid(const sgGrid:TStringGrid);
var iCount:integer;
begin
  with sgGrid do begin  // Do not rely on having 1 fixed row
    if (RowCount>0) and (RowCount<=FixedRows+1) and (Row=FixedRows) then  // On first row after fixed row, if any
      Rows[Row].Clear  // Just clear row content
    else begin                                // Further down the grid
      for iCount:=Row+1 to RowCount-1 do      // Copying while overwrite current Row
        Rows[iCount-1]:=Rows[iCount];
      Rowcount:=RowCount-1;                   // Only need to descrease RowCount now
    end;
    Refresh;
  end;
end;  // DelLineInGrid

//==============================================================================
// Adds some text at the end of a RichEdit component.
// Sets the style of the added text according to the parameters 
procedure AddRichText(const ARichEdit:TRichEdit; const NewText:string;
                      const TextFontSize:integer; const TextFontStyle:TFontStyles;
                      const TextFontColor:TColor; const StartOnNewLine:boolean);
var stText:string;
begin
  with ARichEdit do begin
    stText:='';
    if StartOnNewLine then Lines.Add('')                  // Add a line if requested
                      else stText:=Lines[Lines.Count-1];  // else grab last bit of text
    stText:=stText+NewText;  // Add NewText at the end
    Lines[Lines.Count-1]:=stText;  // Set the last line to new text

    SelStart :=SelStart-Length(NewText);  // Locate add text and select it
    SelLength:=Length(NewText);
    SelAttributes.Size :=TextFontSize;    // Change the attributes of the selection
    SelAttributes.Style:=TextFontStyle;   // according to parameters
    SelAttributes.Color:=TextFontColor;
    SelLength:=0;                         // Deselect text
  end;
end;  // AddRichText

//==============================================================================
// Gets the text limited to a specified size. If the text doesn't fit, it is
// shortened and the '...' characters are added at the end. If the size is too
// small to fit any text, an empty string is returned
function GetTextWithinLimit(const ACanvas:TCanvas; const AString:string;
  const ALimit:integer):string;
var stText:string;
    iDisplayWidth, iCount:integer;

begin
  Result:='';
  iDisplayWidth:=ALimit-ACanvas.TextWidth('...');
  if iDisplayWidth>0 then begin
    Result:=AString;
    if ACanvas.TextWidth(AString)>iDisplayWidth then begin
      stText:='';
      iCount:=1;
      while ACanvas.TextWidth(stText)<iDisplayWidth do begin
        stText:=stText+AString[iCount];
        Inc(iCount);
      end;
      Result:=stText+'...';
    end;
  end;
end;  // GetTextWithinLimit

//==============================================================================
end.
