{===============================================================================
  Unit:        HintPointer

  Defines:     THintPointer

  Description: Copyright Dorset Software Services Ltd 2001
               Replacement THintWindow class that draws a little arrow in the top left
               of the hint

  Model:       <none>

  Created:

  Last revision information:
    $Revision: 2 $
    $Date: 19/01/04 16:10 $
    $Author: Ericsalmon $

===============================================================================}

unit HintPointer;

interface

uses
  Classes, Controls, Graphics, Messages, Windows, Forms;

type
  THintPointer = class(THintWindow)
  protected
    procedure Paint; override;
  public
    function CalcHintRect(MaxWidth: Integer; const AHint: String;
      AData: Pointer): TRect; override;
  end;

//==============================================================================
implementation

{-------------------------------------------------------------------------------
  Paint draws the text and the arrow
}
procedure THintPointer.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  Inc(R.Left, 11); // allow space for arrow
  Inc(R.Top, 2);
  Canvas.Font.Color := clInfoText;
  DrawText(Canvas.Handle, PChar(Caption), -1, R,
           DT_LEFT or DT_NOPREFIX or DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
  // Draw an additional arrow.
  Canvas.Brush.Color := clInfoText;
  Canvas.FillRect(Rect(2, 2, 8, 8));
  Canvas.Brush.Color := clInfoBk;

  // Now remove 2 bits to show an arrow.
  Canvas.Pen.Color := clInfoBk;
  Canvas.Pen.Width := 3;
  Canvas.MoveTo(7, 4);
  Canvas.LineTo(8, 4);
  Canvas.MoveTo(4, 7);
  Canvas.LineTo(4, 8);
  Canvas.Pen.Color := clInfoText;
end;

{-------------------------------------------------------------------------------
  Allow extra space for the hint's arrow
}
function THintPointer.CalcHintRect(MaxWidth: Integer; const AHint: String; AData: Pointer): TRect;
begin
  Result := Rect(0, 0, MaxWidth, 0);
  DrawText(Canvas.Handle, PChar(AHint), -1, Result, DT_CALCRECT or 
           DT_LEFT or DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);
  Inc(Result.Right, 20);
  Inc(Result.Bottom, 2);
end;

end.
