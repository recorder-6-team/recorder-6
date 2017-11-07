{===============================================================================
  Unit:        AddinInterfaceDataModule

  Defines:     TdmInterface

  Description: Data module for interface stuff such as image lists that apply
               to all Recorder addins

  Created:     April 2004

  Model:       <none>

  Last revision information:
    $Revision: 1 $
    $Date: 2/09/04 15:22 $
    $Author: Andrewkemp $

===============================================================================}

unit AddinInterfaceDataModule;

interface

uses
  SysUtils, Classes, ImgList, Controls, Graphics, Windows, GeneralFunctions,
  Menus, RapTree, ExGrid, StrUtils;

type
  {-----------------------------------------------------------------------------
    Interface data module class
  }
  TdmAddinInterface = class(TDataModule)
    ilButtons: TImageList;
  private
    function PaintWord(ACanvas: TCanvas; AX, AY: integer; const ATerm: string;
             ACalcRect: boolean): integer;
    procedure SetItalic(ACanvas: TCanvas; const ADefaultFontName: String; Active: Boolean);
    procedure SplitTerm(const ATerm: String; AStrings: TStrings; var StartItalic: Boolean);
  public
    procedure DrawTerm(ACanvas: TCanvas; ARect: TRect; const ATerm: String;
        ASelected: Boolean);
    procedure DrawWrappedTerm(ACanvas: TCanvas; var ARect: TRect;
      const ATerm: String; ASelected, ACalcRect: Boolean);
    function GetAlignedTextRect(ACanvas: TCanvas; ARect: TRect;
        const AText: string; AAlignment: TAlignment;ABorderWidth: integer): TRect;
    procedure TermMenuDrawItem(Sender: TObject; ACanvas: TCanvas;
        ARect: TRect; Selected: Boolean);
    procedure RepaintNode(ANode: TFlyNode; ATree: TRapidTree);
    function TermTextWidth(ACanvas: TCanvas; const ATerm: String): Integer;
  end;

//==============================================================================
implementation

{$R *.dfm}

type
  TRapidTreeAccessor = class(TRapidTree);

{-==============================================================================
    TdmInterface
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TdmAddinInterface.SetItalic(ACanvas: TCanvas; const ADefaultFontName: String;
    Active: Boolean);
begin
  with ACanvas.Font do
    if Active then begin
      if CompareText(Name, 'MS Sans Serif') = 0 then Name := 'Arial';
      Style := Style + [fsItalic];
    end else begin
      // Change back to normal font, if changed.
      Name := ADefaultFontName;
      Style := Style - [fsItalic];
    end;
end;  // TdmAddinInterface.SetItalic

{-------------------------------------------------------------------------------
}
procedure TdmAddinInterface.SplitTerm(const ATerm: String; AStrings: TStrings;
    var StartItalic: Boolean);
var
  lText, lTag: String;
  lTagPos: Integer;
begin
  lText := ATerm;
  lTag := '<i>';
  lTagPos := Pos(lTag, lText);
  StartItalic := lTagPos = 1;
  while lTagPos > 0 do begin
    // Don't add empty items.
    if lTagPos > 1 then AStrings.Add(Copy(lText, 1, lTagPos - 1));
    lText := Copy(lText, lTagPos + Length(lTag), 255);
    if lTag = '<i>' then lTag := '</i>' else lTag := '<i>';
    lTagPos := Pos(lTag, lText);
  end;
  // Add last one, if anything.
  if lText <> '' then AStrings.Add(lText);
end;  // TdmAddinInterface.SplitTerm

{-------------------------------------------------------------------------------
}
function TdmAddinInterface.TermTextWidth(ACanvas: TCanvas; const ATerm: String): Integer;
var
  i: Integer;
  lList: TStringList;
  lItalicState: Boolean;
  lFontName: String;
begin
  lList := TStringList.Create;
  try
    lFontName := ACanvas.Font.Name;
    SplitTerm(ATerm, lList, lItalicState);
    SetItalic(ACanvas, lFontName, lItalicState);
    Result := 0;
    for i := 0 to lList.Count - 1 do begin
      Result := Result + ACanvas.TextWidth(lList[i]);
      lItalicState := not lItalicState;
      SetItalic(ACanvas, lFontName, lItalicState);
    end;
  finally
    SetItalic(ACanvas, lFontName, False);
    lList.Free;
  end;
end;  // TdmAddinInterface.TermTextWidth

{-------------------------------------------------------------------------------
  Draws a term onto the canvas, allowing for the <i> formatting
}
procedure TdmAddinInterface.DrawTerm(ACanvas: TCanvas; ARect: TRect; const ATerm: String;
    ASelected: Boolean);
var
  lOutText, lFontName: String;
  i, lWidth, lXPos: Integer;
  lList: TStringList;
  lItalicState: Boolean;
begin
  lList := TStringList.Create;
  try
    //lText := ATerm;
    lFontName := ACanvas.Font.Name;

    SplitTerm(ATerm, lList, lItalicState);

    SetItalic(ACanvas, lFontName, lItalicState);
    ACanvas.FillRect(ARect);
    lXPos := ARect.Left + 1;
    i := 0;
    while (i < lList.Count) do begin
      lOutText := GetTextWithinLimit(ACanvas, lList[i], ARect.Right - lXPos);
      lWidth :=  ACanvas.TextWidth(lList[i]);
      // Can't fit more text in.
      if (lXPos + lWidth > ARect.Right) or (RightStr(lOutText, 3) = '...') then begin
        ACanvas.TextOut(lXPos, ARect.Top, lOutText);
        Exit
      end else begin
        // Check next part can be at least partially displayed.
        if i < lList.Count - 1 then begin
          SetItalic(ACanvas, lFontName, not lItalicState);
          // Can't display next part, have to 'rework' current to show '...'
          if GetTextWithinLimit(ACanvas, lList[i + 1], ARect.Right - lXPos - lWidth) = '' then
            lOutText := '';
          SetItalic(ACanvas, lFontName, lItalicState);
        end;
        if lOutText = '' then
          lList[i] := lList[i] + '...'
        else begin
          lOutText := GetTextWithinLimit(ACanvas, lList[i], ARect.Right - lXPos);
          ACanvas.TextOut(lXPos, ARect.Top, lOutText);
          Inc(lXPos, lWidth);
          lItalicState := not lItalicState;
          SetItalic(ACanvas, lFontName, lItalicState);
          // Move on to next part.
          Inc(i);
        end;
      end;
    end;
  finally
    lList.Free;
    SetItalic(ACanvas, lFontName, False);
  end;
end;

{-------------------------------------------------------------------------------
  Draws a term onto the canvas, allowing for the <i> formatting, and word-
    wrapping.  If ACalcRect is true, then the text is not drawn, it is
    used to calculate the rectangle, in which case if the rectangle is not wide
    enough for the widest word, it is widened.  If the text contains explicit
    line feeds, then text is only wrapped when these are encountered, not
    automatically
}
procedure TdmAddinInterface.DrawWrappedTerm(ACanvas: TCanvas; var ARect: TRect;
  const ATerm: string; ASelected: boolean; ACalcRect: boolean);
var
  lWordWidth: integer;
  lRemainingTerm: string;
  lWord: string;
  lX, lY: integer;
  lPossibleLineSeparator: string;

    procedure FindRectangleWidth;
    begin
      // find the width of each word.
      lRemainingTerm := Trim(ATerm);
      while lRemainingTerm<>'' do begin
        if Pos(lPossibleLineSeparator, lRemainingTerm)>0 then
          lWord := Copy(lRemainingTerm, 1, Pos(lPossibleLineSeparator, lRemainingTerm))
        else
          lWord := lRemainingTerm;
        lWordWidth := PaintWord(ACanvas, 0, 0, lWord, True);
        if lWordWidth>ARect.Right - ARect.Left then
          ARect.Right := ARect.Left + lWordWidth;
        lRemainingTerm := Copy(lRemainingTerm, Length(lWord)+1, High(Integer));
      end;
    end;

    procedure DrawOutput;
    var
      lbmpTemp : Graphics.TBitmap;
      lFontStyle : TFontStyles;
    begin
      // Create a bitmap to draw text on, so we can then centre it once a line is complete
      lbmpTemp := Graphics.TBitmap.Create;
      lbmpTemp.Width := ARect.Right - ARect.Left;
      lbmpTemp.Height := ACanvas.TextHeight('A')+2;
      lbmpTemp.Canvas.Brush.Assign(ACanvas.Brush);
      lbmpTemp.Canvas.Font.Assign(ACanvas.Font);
      lbmpTemp.Canvas.FillRect(Rect(0, 0, lbmpTemp.Width, lbmpTemp.Height));
      try
        // Draw the output
        lX := 0;
        lY := ARect.Top;
        lRemainingTerm := Trim(ATerm);
        while lRemainingTerm<>'' do begin
          if Pos(lPossibleLineSeparator, lRemainingTerm)>0 then
            lWord := Copy(lRemainingTerm, 1,
                Pos(lPossibleLineSeparator, lRemainingTerm)-1) + ' '
          else
            lWord := lRemainingTerm;
          lFontStyle := lbmpTemp.Canvas.Font.Style;
          lWordWidth := PaintWord(lbmpTemp.Canvas, lX, 0, lWord, True);
          // Preserve the font style for the start of the actual draw operation
          if not ACalcRect then
            lbmpTemp.Canvas.Font.Style := lFontStyle;
          if lX + lWordWidth > ARect.Right-ARect.Left then begin
            // Draw the bitmap, and centre it
            lbmpTemp.Width := lX;
            if not ACalcRect then
              ACanvas.Draw(ARect.Left + (ARect.Right - ARect.Left) div 2 - lX div 2,
                  lY, lbmpTemp);
            // Clear bitmap for next line
            lbmpTemp.Width := ARect.Right - ARect.Left;
            if not ACalcRect then
              lbmpTemp.Canvas.FillRect(Rect(0, 0, lbmpTemp.Width, lbmpTemp.Height));
            // New Line
            lX := 0;
            lY := lY + lbmpTemp.Canvas.TextHeight('A')+2;
          end;
          if not ACalcRect then
            // actually paint the word
            PaintWord(lbmpTemp.Canvas, lX, 0, lWord, False);
          lX := lX + lWordWidth;
          lRemainingTerm := Copy(lRemainingTerm, Length(lWord)+
              Length(lPossibleLineSeparator), High(Integer));
        end;
        // Draw the final lines bitmap, and centre it
        lbmpTemp.Width := lX;
        if not ACalcRect then
          ACanvas.Draw(ARect.Left + (ARect.Right - ARect.Left) div 2 - lX div 2,
              lY, lbmpTemp);
      finally
        lbmpTemp.Free;
      end;
    end;

begin
  // use spaces or line feeds to wrap text?
  if Pos(#13#10, ATerm)>0 then
    lPossibleLineSeparator:=#13#10
  else
    lPossibleLineSeparator:=' ';
  // If the word has own formatting, then font cannot start italic
  if Pos('<i>', ATerm)>0 then
    ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic];
  if ACalcRect then
    FindRectangleWidth;
  DrawOutput;
  // Find the very bottom of the text, allowing for the last line
  if ACalcRect then
    ARect.Bottom := lY + ACanvas.TextHeight('A');
end;


{-------------------------------------------------------------------------------
  Returns a rectangle aligned within the supplied rectangle, suitable for
      drawing the text into.  Pixels are added to the rectangle in each
      dimension to allow for a border.
}
function TdmAddinInterface.GetAlignedTextRect(ACanvas: TCanvas; ARect: TRect;
  const AText: string; AAlignment: TAlignment; ABorderWidth: integer): TRect;
var
  lRectTop, lRectBottom: integer;
begin
  lRectTop := (ARect.Top+ARect.Bottom) div 2 - ACanvas.TextHeight(AText) div 2 - ABorderWidth;
  lRectBottom := (ARect.Top+ARect.Bottom) div 2 + ACanvas.TextHeight(AText) div 2 + ABorderWidth;
  case AAlignment of
    taLeftJustify :
      Result := Classes.Rect(ARect.Left,
          lRectTop,
          ARect.Left+ACanvas.TextWidth(AText)+ABorderWidth*2,
          lRectBottom);
    taRightJustify :
      Result := Classes.Rect(ARect.Left+ACanvas.TextWidth(AText)+ABorderWidth*2,
          lRectTop,
          ARect.Right,
          lRectBottom);
    taCenter :
      Result := Classes.Rect(
          (ARect.Left+ARect.Right) div 2 - ACanvas.TextWidth(AText) div 2 - ABorderWidth,
          lRectTop,
          (ARect.Left+ARect.Right) div 2 + ACanvas.TextWidth(AText) div 2 + ABorderWidth,
          lRectBottom);
  end; // case
end;

{-------------------------------------------------------------------------------
  Draw code that can be attached to TMenuItem.OnDrawItem if term formatting
      is required.
}
procedure TdmAddinInterface.TermMenuDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; Selected: Boolean);
begin
  ARect.Top := ARect.Top + 2;
  
  //Draw image if it exists
  if TMenuItem(Sender).ImageIndex > -1 then begin
    TMenuItem(Sender).GetImageList.Draw(ACanvas, ARect.Left, ARect.Top,
        TMenuItem(Sender).ImageIndex);
    ARect.Left := ARect.Left + TMenuItem(Sender).GetImageList.Width + 4;
  end;

  DrawTerm(ACanvas, ARect, TMenuItem(Sender).Caption, Selected);
end;

{-------------------------------------------------------------------------------
  Paints a single node onto the hierarchy canvas.  Used when refreshing a
      common name.
}
procedure TdmAddinInterface.RepaintNode(ANode: TFlyNode; ATree: TRapidTree);
var
  lDummy: boolean;
begin
  // If node is not off the top of the tree view
  if ANode.GetRow >= ATree.TopItem.GetRow then
    TRapidTreeAccessor(ATree).DrawCell(ATree.Canvas, 0, ANode.GetRow,
        ATree.DisplayRect(ANode, False), [], lDummy);
end;  // TdmInterface.RepaintNode


{-------------------------------------------------------------------------------
  Paints a word, and finds the width of a word that will be painted onto the
      canvas.
}
function TdmAddinInterface.PaintWord(ACanvas: TCanvas; AX, AY: integer; const ATerm: string;
             ACalcRect: boolean): integer;
var
  lRemainingTerm: string;
  lStartItalicPos, lEndItalicPos: integer;
  lCurrentWord: string;
begin
  Result := 0;
  lRemainingTerm := ATerm;
  while lRemainingTerm <> '' do begin
    lStartItalicPos := Pos('<i>', lRemainingTerm);
    lEndItalicPos := Pos('</i>', lRemainingTerm);
    // Find the first formatting tag
    if lStartItalicPos=0 then lStartItalicPos := High(Integer);
    if lEndItalicPos=0 then lEndItalicPos := High(Integer);
    lCurrentWord := Copy(lRemainingTerm, 1, Min(lStartItalicPos, lEndItalicPos)-1);
    if not ACalcRect then
      ACanvas.TextOut(AX + Result, AY, lCurrentWord);
    Result := Result + ACanvas.TextWidth(lCurrentWord);
    if lStartItalicPos <lEndItalicPos then begin
      lRemainingTerm := Copy(lRemainingTerm, Length(lCurrentWord)+4, High(Integer));
      ACanvas.Font.Style := ACanvas.Font.Style + [fsItalic];
    end
    else if lEndItalicPos < lStartItalicPos then begin
      lRemainingTerm := Copy(lRemainingTerm, Length(lCurrentWord)+5, High(Integer));
      ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic];
    end
    else
      lRemainingTerm := '';
  end; // while
end;

end.
