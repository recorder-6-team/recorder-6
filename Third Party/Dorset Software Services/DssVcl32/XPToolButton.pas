(*==============================================================================
  Unit: XPToolButton

  Implements: TXPToolbar, TXPToolButton

  Description: Provides a TToolbutton replacement that behaves like an XP
               toolbutton under any OS.
               Usage - use the GExperts Replace Components tool, or search
               and replace pas and dfm files to replace TToolbutton with
               TXPToolButton and TToolbar with TXPToolbar.  The 2 components
               are mutually dependent.


  Author:      John van Breda
  Created:     June 2002

  Changes:

  Last Revision Details:
    $Revision: 12 $
    $Date: 23/06/09 10:29 $
    $Author: Ericsalmon $

  $History: XPToolButton.pas $
//  
//  *****************  Version 12  *****************
//  User: Ericsalmon   Date: 23/06/09   Time: 10:29
//  Updated in $/SRCLIB/DELPHI/dssvcl32
//  Improvements on drawing toolbuttons with regards to background.
//  
//  *****************  Version 11  *****************
//  User: Ericsalmon   Date: 12/06/09   Time: 15:54
//  Updated in $/SRCLIB/DELPHI/dssvcl32
//  Cleanup and fixes for XP theming to work properly. The whole thing
//  actually works as intented too, using Paint overridden.
//
//*****************  Version 10  *****************
//User: Johnvanbreda Date: 11/06/02   Time: 3:03p
//Updated in $/SRCLIB/DELPHI/dssvcl32
//Uses CNNotify to get proper info about how buttons should draw
//
//*****************  Version 9  *****************
//User: Johnvanbreda Date: 11/06/02   Time: 11:59a
//Updated in $/SRCLIB/DELPHI/dssvcl32
//Fixed drag over, by adding TXPToolbar which traps CN_NOTIFY and decodes
//the button state properly
 * 
 * *****************  Version 8  *****************
 * User: Johnvanbreda Date: 10/06/02   Time: 2:34p
 * Updated in $/SRCLIB/DELPHI/dssvcl32
 * Fixed header comment
 *
 * *****************  Version 7  *****************
 * User: Johnvanbreda Date: 10/06/02   Time: 2:09p
 * Updated in $/SRCLIB/DELPHI/dssvcl32
 * Display of drop down buttons fixed.

  Copyright © Dorset Software Services Ltd, 2002

==============================================================================*)

unit XPToolButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Commctrl, ApiUtils, ImgList;

type
  TXPToolbar = class(TToolbar)
  protected
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  end;

  TXPToolbutton = class(TToolbutton)
  private
    FImages: TCustomImageList;
    FPaintState: TCustomDrawState;
    procedure DimBitmap(ABitmap: TBitmap; Value: Integer);
    procedure DrawDimmedImage(ALeft: Integer);
    procedure DrawDisabledImage(ALeft: Integer);
    procedure DrawDropDownTriangle;
    procedure DrawHighlightBox(isBright: Boolean);
    procedure DrawImageShadow(ALeft: Integer);
    procedure DrawButtonText;
    procedure GrayBitmap(ABitmap: TBitmap);
    function GrayColour(AColour: TColor): TColor;
    procedure SetImages(const Value: TCustomImageList);
  protected
    procedure Paint; override;
  published
    property Images: TCustomImageList read FImages write SetImages; {#ToDo1 Document Images property}
  end;

{-------------------------------------------------------------------------------
}
implementation

uses
  GeneralFunctions;

var
  mbmpMask  : TBitmap;
  mbmpShadow: TBitmap;
  mbmpText:   TBitmap;

{===============================================================================
  TXPToolbar
}
{-------------------------------------------------------------------------------
}
procedure TXPToolbar.CNNotify(var Message: TWMNotify);
var
  lButton: TXPToolbutton;
begin
  with Message do
    if NMHdr^.code = NM_CUSTOMDRAW then
      with PNMTBCustomDraw(NMHdr)^ do
      begin
        if (nmcd.dwDrawStage and CDDS_ITEM) = 0 then
        begin
          Result := CDRF_DODEFAULT;
          if nmcd.dwDrawStage = CDDS_PREPAINT then
            Result := CDRF_NOTIFYITEMDRAW;
        end else begin
          if nmcd.dwDrawStage = CDDS_ITEMPREPAINT then
          begin
            lButton := TXPToolbutton(Buttons[nmcd.dwItemSpec]);
            if lButton <> nil then
              lButton.FPaintState :=  TCustomDrawState(Word(nmcd.uItemState));
            Result := Result or CDRF_SKIPDEFAULT;
          end;
        end;
      end
    else
      inherited;  // Required to process other messages as normal.
end;  // TXPToolbar.CNNotify

{===============================================================================
  TXPToolbutton
}
{-------------------------------------------------------------------------------
  Tones down the supplied bitmap, by an amount dictated by Value.
}
procedure TXPToolButton.DimBitmap(ABitmap: TBitmap; Value: Integer);
var
  x, y: Integer;
  lastColour1, lastColour2, colour: TColor;
begin
  if Value > 100 then Value := 100;
  lastColour1 := -1;
  lastColour2 := -1;

  for y := 0 to ABitmap.Height - 1 do
    for x := 0 to ABitmap.Width - 1 do
    begin
      colour := ABitmap.Canvas.Pixels[x, y];
      if colour = lastColour1 then
        ABitmap.Canvas.Pixels[x, y] := lastColour2
      else begin
        lastColour2 := MergeColours(clBtnFace, colour, Value);
        ABitmap.Canvas.Pixels[x, y] := lastColour2;
        lastColour1 := colour;
      end;
    end;
end;  // TXPToolButton.DimBitmap

{-------------------------------------------------------------------------------
  Automatically dim the image and draw it to the button, when it is not hot.
}
procedure TXPToolButton.DrawDimmedImage(ALeft: Integer);
begin
  mbmpShadow.Canvas.Brush.Color := clBtnFace;
  mbmpShadow.Canvas.FillRect(Rect(0, 0, Width,Height - 1));
  FImages.Draw(mbmpShadow.Canvas, 0, 0, ImageIndex);
  DimBitmap(mbmpShadow, 30);
  mbmpShadow.Transparent      := True;
  mbmpShadow.TransparentColor := clBtnFace;
  Canvas.Draw(ALeft, (Height - FImages.Height) div 2, mbmpShadow);
end;  // TXPToolButton.DrawDimmedImage

{-------------------------------------------------------------------------------
  Draws an image on the toolbutton in a disabled appearance.
}
procedure TXPToolbutton.DrawDisabledImage(ALeft: Integer);
begin
  mbmpShadow.Canvas.Brush.Color := clFuchsia;
  mbmpShadow.Canvas.FillRect(Rect(0, 0, Width, Height - 1));
  FImages.Draw(mbmpShadow.Canvas, 0, 0, ImageIndex);
  GrayBitmap(mbmpShadow);
  DimBitmap(mbmpShadow, 50);
  mbmpShadow.Transparent      := True;
  mbmpShadow.TransparentColor := MergeColours(clBtnFace, GrayColour(clFuchsia), 50);
  Canvas.Draw(ALeft, (Height - FImages.Height) div 2, mbmpShadow);
end;  // TXPToolbutton.DrawDisabledImage

{-------------------------------------------------------------------------------
  Draw triangular drop down indicator for combo buttons.
}
procedure TXPToolButton.DrawDropDownTriangle;
begin
  Canvas.Brush.Style := bsSolid;
  if Enabled then
    Canvas.Pen.Color := clWindowText
  else
    Canvas.Pen.Color := clGrayText;
  Canvas.Brush.Color := Canvas.Pen.Color;
  Canvas.Polygon([Point(TToolbar(Parent).ButtonWidth + 4, 10),
                  Point(TToolbar(Parent).ButtonWidth + 8, 10),
                  Point(TToolbar(Parent).ButtonWidth + 6, 12)]);
end;  // TXPToolButton.DrawDropDownTriangle

{-------------------------------------------------------------------------------
  Draw text as transparent so background shows through.
}
procedure TXPToolButton.DrawButtonText;
var
  r: TRect;
begin
  // Dim the bitmap to easily accomodate the whole text.
  mbmpText.Height := Canvas.TextHeight('A') * 2;
  mbmpText.Width  := Canvas.TextWidth(Caption) * 2;
  mbmpText.Canvas.Brush.Color := clBtnFace;
  mbmpText.Canvas.FillRect(Rect(0, 0, Width,Height - 1));
  r := Rect(0, 0, ClientWidth, ClientHeight);
  DrawText(
      mbmpText.Canvas.Handle,
      PChar(Caption),
      Length(Caption),
      r,
      DT_CENTER);
  mbmpText.Transparent      := True;
  mbmpText.TransparentColor := clBtnFace;
  Canvas.Draw(0, (Height - Canvas.TextHeight('A')) div 2, mbmpText);
end;  // TXPToolButton.DrawButtonText

{-------------------------------------------------------------------------------
  Draw the shaded highlight box that XP uses when the mouse is over a
  toolbutton.  isBright = true -> a bright hiughlight box, when the mouse
  is over the button.  False -> dimmer version, when the button is down.
}
procedure TXPToolButton.DrawHighlightBox(isBright: Boolean);
begin
  with Canvas do
  begin
    Pen.Color   := clHighlight;
    Brush.Style := bsSolid;
    if isBright then
      Brush.Color := MergeColours(MergeColours(clBtnFace, clHighlight, 45), clWindow, 60)
    else
      Brush.Color := MergeColours(MergeColours(clBtnFace, clHighlight, 75), clWindow, 60);
    Rectangle(0, 0, ClientWidth, ClientHeight);
    if Style = tbsDropDown then
      Rectangle(0, 0, FToolbar.ButtonWidth + 1, ClientHeight); // extra box from drop down
  end;
end;  // TXPToolButton.DrawHighlightBox

{-------------------------------------------------------------------------------
  Create a shadow bitmap and draw it onto the button. Used when the image pops
  up - when the mouse hovers over the button.
}
procedure TXPToolButton.DrawImageShadow(ALeft: Integer);
var
  lShadowColour: TColor;
begin
  // First get a Black on White mask
  mbmpMask.Canvas.FillRect(Rect(0, 0, 16, 16));
  gpcApiCheck(ImageList_DrawEx(
      FImages.Handle,
      ImageIndex,
      mbmpMask.Canvas.Handle,
      0,
      0,
      FImages.Width,
      FImages.Height,
      clFuchsia,
      CLR_NONE,
      ILD_MASK));
  // Create a square of the required shadow colour
  lShadowColour := MergeColours(MergeColours(clBtnFace, clHighlight, 55), clGrayText, 75);
  mbmpShadow.Canvas.Brush.Color := lShadowColour;
  mbmpShadow.Canvas.FillRect(Rect(0, 0, Width, Height));
  // Combine the mask and shadow colour - the background colour becomes the inverted shadow colour
  mbmpShadow.Canvas.CopyMode := cmSrcInvert;
  mbmpShadow.Canvas.Draw(0, 0, mbmpMask);
  // and draw the result as a drop shadow
  Canvas.CopyMode := cmSrcCopy;
  mbmpShadow.Transparent := True;
  // use inverted shadow colour as transparent
  mbmpShadow.TransparentColor := lShadowColour xor $FFFFFF;
  Canvas.Draw(ALeft + 1, (Height - FImages.Height) div 2 + 1, mbmpShadow);
end;  // TXPToolButton.DrawImageShadow

{-------------------------------------------------------------------------------
  Convert a bitmap to a grayscale version.  White colours are dimmed back to
  light gray.
}
procedure TXPToolButton.GrayBitmap(ABitmap: TBitmap);
var
  x, y: Integer;
begin
  for y := 0 to ABitmap.Height - 1 do
    for x := 0 to ABitmap.Width - 1 do
      ABitmap.Canvas.Pixels[x,y] := GrayColour(ABitmap.Canvas.Pixels[x, y]);
end;  // TXPToolButton.GrayBitmap

{-------------------------------------------------------------------------------
  Returns a grayscale translation of a colour.  In order that we don't return
  white, which shouldn't appear on disabled images, the max colour returned is $CC.
}
function TXPToolButton.GrayColour(AColour: TColor): TColor;
var
  lShade: Integer;
begin
  lShade := Min((AColour and $FF + (AColour and $FF00) shr 8 + (AColour and $FF0000) shr 16) div 3,
                $CC);
  Result := RGB(lShade, lShade, lShade);
end;  // TXPToolButton.GrayColour

{-------------------------------------------------------------------------------
  Override the standard toolbutton paint so that XP appearance is obtained.
}
procedure TXPToolbutton.Paint;
var
  R: TRect;
  lLeft: Integer;
begin
  { Set basic background colour }
  if (cdsHot in FPaintState) or (cdsSelected in FPaintState) then
    DrawHighlightBox(True) // darker highlight box
  else
  if cdsChecked in FPaintState then
    DrawHighlightBox(False); // paler highlight box

  if FToolBar = nil then Exit;

  if not Assigned(FImages) then
    FImages := FToolbar.Images;

  case Style of
    tbsDivider :
      with Canvas do begin
        R := Rect(Width div 2 - 1, 0, Width, Height);
        DrawEdge(Handle, R, EDGE_ETCHED, BF_LEFT)
      end;
    tbsSeparator :
      with Canvas do begin
        Pen.Color := clGrayText;
        MoveTo(Width div 2 - 1, 2);
        LineTo(Width div 2 - 1, Height - 4);
      end;
  else
      if (ImageIndex <> -1) and Assigned(FImages) then
      begin
        if Style <> tbsDropDown then
          lLeft := (Width - FImages.Width) div 2 // centred image
        else
          lLeft := 3;  // image towards left, allow for drop down triangle

        { Check if mouse is over current button as this requires a highlight box }
        if cdsHot in FPaintState then
        begin
          if cdsSelected in FPaintState then // draw flat
            FImages.Draw(Canvas, lLeft, (Height - FImages.Height) div 2, ImageIndex)
          else begin         // draw raised bitmap over shadow
            DrawImageShadow(lLeft);
            FImages.Draw(Canvas, lLeft - 1, (Height - FImages.Height) div 2-1, ImageIndex);
          end;
        end else
        if not ((cdsGrayed in FPaintState) or (cdsDisabled in FPaintState)) then
          { Draw a non-hot toolbutton }
          DrawDimmedImage(lLeft)
        else
          DrawDisabledImage(lLeft);

        if Style = tbsDropDown then
          DrawDropDownTriangle;
      end else begin
        // Draw text - no images.
        { Check if mouse is over current button as this requires a highlight box }
        if (cdsGrayed in FPaintState) or (cdsDisabled in FPaintState) then
          Canvas.Font.Color := clGrayText
        else
          Canvas.Font.Color := clWindowText;
        DrawButtonText;
      end;
  end;
end;  // TXPToolbutton.Paint

{-------------------------------------------------------------------------------
  Accessor Method.
}
procedure TXPToolbutton.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  Invalidate;
end;  // TXPToolbutton.SetImages

{-------------------------------------------------------------------------------
}
initialization
  mbmpMask          := TBitmap.Create;
  mbmpMask.Width    := 16;
  mbmpMask.Height   := 16;
  mbmpShadow        := TBitmap.Create;
  mbmpShadow.Width  := 16;
  mbmpShadow.Height := 16;
  mbmpText          := TBitmap.Create;

finalization
  mbmpMask.Free;
  mbmpShadow.Free;
  mbmpText.Free;

end.
