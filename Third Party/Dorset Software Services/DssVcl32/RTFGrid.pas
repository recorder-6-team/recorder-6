{ DSS In-House component
Copyright Dorset Software Services Ltd 1999.
TDBRTFGrid component
  Provides a DB Grid descendant which displays rich text in a memo field rather
  than just [MEMO]
  By John van Breda 01/04/99

  Notes:
    To achieve the effect, it is necessary to ask a hidden rich text control to
    render its contents onto a canvas.  Contents are rendered onto an interim
    bitmap which allows the colours to be inverted as the bitmap is drawn in the
    case of selected cells.
    Override the DrawColumnCell method to change or add to the drawing
    capabilities
}

unit RTFGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, RichEdit, ComCtrls, Db;

type
  TDBRTFGrid = class(TDBGrid)
  private
    FreCellDraw  : TRichEdit;
    FbmpInterim  : TBitmap;
    FtfFirstDraw : boolean; // Flag to indicate the first draw of a cell
    FHandle      : HWnd; // store the rich edit handle
    FLogX        : Integer; // scaling factor from TWIPS to pixels
    FLogY        : integer;
    FLastHintedCell: TGridCoord;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure GetCellHint(ACol, ARow: integer);
  protected
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
              Column: TColumn; State: TGridDrawState); override;
    procedure DrawRichMemo(const Rect: TRect;
              Column: TColumn; State: TGridDrawState); virtual;
    procedure InitialiseRichEdit;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PrepareDummyComponents( const iRect : TRect;
            const iColumn : TColumn; const iState : TGridDrawState );
    procedure PrepareFirstDraw;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
  end;

//==============================================================================
implementation

uses
  GeneralFunctions;

//==============================================================================
constructor TDBRTFGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitialiseRichEdit;
  FbmpInterim := TBitmap.Create;
  FtfFirstDraw := True;
  // Set FHandle initialisation value so we can detect if it is changed
  FHandle := 0;
  FLastHintedCell.X := -1;
  FLastHintedCell.Y := -1;
end;   

//==============================================================================
destructor TDBRTFGrid.Destroy;
begin
  { Flush the buffers again as we've finished.  Note we can't use the normal
    handle as it is lost once the parent form starts destroying }
  if FHandle = 0 then
    SendMessage(FHandle, EM_FORMATRANGE, 0, 0);
  FreCellDraw.Free;
  FbmpInterim.Free;
  inherited Destroy;
end;

//==============================================================================
{ Overriden DrawColumnCell which traps memo fields }
procedure TDBRTFGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
begin
  with Canvas do begin
    if gdFocused in State then
    begin
      Brush.color := clHighlight;
      Font.Color  := clHighlightText;
      DrawFocusRect(Rect);
    end;
    FillRect(Rect);
    // check field available, otherwise if dataset failed to load this causes exceptions
    // which prevents the original exception from appearing.
    if assigned(Column.Field) then begin
      if (Column.Field.DataType = ftMemo) then
        DrawRichMemo(Rect, Column, State)
      else
        DrawChoppedText(Column.Field.Text, Canvas, Rect, 2);
    end;
  end; // with Canvas
  if assigned(Column.Field) then
    inherited DrawColumnCell( Rect, DataCol, Column, State );
end;

//==============================================================================
{ Actually render our rich text into the grid }
procedure TDBRTFGrid.DrawRichMemo(const Rect: TRect; Column: TColumn;
  State: TGridDrawState);
var
  lRange : TFormatRange;
begin
  if FtfFirstDraw then PrepareFirstDraw;

  PrepareDummyComponents(Rect, Column, State);
  with lRange do
  begin
    hdc := FbmpInterim.Canvas.Handle;
    hdcTarget := hdc;
    FLogX := GetDeviceCaps(hdc, LOGPIXELSX);
    FLogY := GetDeviceCaps(hdc, LOGPIXELSY);
    rc := Classes.Rect(0, 0,
                       (Rect.Right + 100) * 1440 div FLogX,
                       Rect.Bottom * 1440 div FLogY);
    rcPage := Classes.Rect(0, 0,
                           (FbmpInterim.Width) * 1440 div FLogX,
                           (FbmpInterim.Height) * 1440 div FLogY);
    chrg.cpMax := -1;
    chrg.cpMin := 0;
  end;
  SendMessage(FreCellDraw.Handle, EM_FORMATRANGE, 1, Longint(@lRange));
  // draw 2 pixels down - this aligns the text better with existing text
  Canvas.CopyRect(Classes.Rect(Rect.Left + 2, Rect.Top + 2, Rect.Right, Rect.Bottom),
                  FbmpInterim.Canvas,
                  Classes.Rect(0, 0, FbmpInterim.Width, FbmpInterim.Height));
  FreCellDraw.DefAttributes.Color := clWindowText;
end;

//==============================================================================
{ Setup the dummy rich edit control during the constructor }
procedure TDBRTFGrid.InitialiseRichEdit;
begin
  FreCellDraw := TRichEdit.Create(Self);
  FreCellDraw.Visible := False;
  FreCellDraw.Parent := Self; // it must have a parent for internal vcl reasons
  // Make it invisible at design time
  FreCellDraw.Width := 0;
  FreCellDraw.Height := 0;
end;

//==============================================================================
{ Setup the bitmap/rich edit dimensions and clear it before we start drawing
     on the canvas.  Called whilst drawing memo cells.  Also sets the copy mode
     of the grid to handle selected cell inversion }
procedure TDBRTFGrid.PrepareDummyComponents( const iRect : TRect;
            const iColumn : TColumn; const iState : TGridDrawState );
begin
  FbmpInterim.Width := iRect.Right - iRect.Left - 2;
  FbmpInterim.Height := iRect.Bottom - iRect.Top - 2;  // 2 pixel margin left at top
  FreCellDraw.Lines.Assign(iColumn.Field);
  if gdSelected in iState then begin
    FbmpInterim.Canvas.Brush.color := clHighlight;
    FreCellDraw.Color := clHighlight;
    FreCellDraw.DefAttributes.Color := clHighlightText;
  end else begin
    FbmpInterim.Canvas.Brush.color := clWindow;
    FreCellDraw.Color := clWindow;
  end;
  FbmpInterim.Canvas.FillRect(Classes.Rect(0, 0, FbmpInterim.Width, FbmpInterim.Height));
  FreCellDraw.Width := (iRect.Right - iRect.Left);
end;

//==============================================================================
{ Do anything which needs to be done before drawing a cell, but can't be done
     on the constructor }
procedure TDBRTFGrid.PrepareFirstDraw;
begin
  // Flush the buffers
  SendMessage(FreCellDraw.Handle, EM_FORMATRANGE, 0, 0);
  FtfFirstDraw := False;
  FHandle := FreCellDraw.Handle;
end;

{-------------------------------------------------------------------------------
}
procedure TDBRTFGrid.CMHintShow(var Message: TCMHintShow);
var
  lGridCoord: TGridCoord;
  lMousePos: TPoint;
begin
  lMousePos := ScreenToClient(Mouse.CursorPos);
  lGridCoord := MouseCoord(lMousePos.X, lMousePos.Y);
  GetCellHint(lGridCoord.X, lGridCoord.Y);
  if Hint = '' then
    Application.HideHint
  else
    Message.HintInfo^.ReshowTimeout := Application.HintPause;
  FLastHintedCell := lGridCoord;
  inherited;
end;

{-------------------------------------------------------------------------------
  Retrieve the appropriate hint (either an error, the full cell, or nothing) when hovering
      over a cell.
}
procedure TDBRTFGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lGridCoord: TGridCoord;
begin
  inherited;
  // Don't want anything happening at design-time.
  if not (csDesigning in ComponentState) then begin
    lGridCoord := MouseCoord(X, Y);
    // Find out if mouse has moved to another cell, and ignore fixed cells.
    if (lGridCoord.X <> FLastHintedCell.X) or (lGridCoord.Y <> FLastHintedCell.Y) then
      GetCellHint(lGridCoord.X, lGridCoord.Y);
    FLastHintedCell := lGridCoord;
  end; // if
end;  // TDBRTFGrid.MouseMove

{-------------------------------------------------------------------------------
  Updates the Hint property for the contents of a cell. 
}
procedure TDBRTFGrid.GetCellHint(ACol, ARow: integer);
var
  lNewHint: String;
  lOldRow, lCol: Integer;
begin
  lCol := ACol;
  // allow for indicator col
  if dgIndicator in Options then Dec(lCol);

  Hint := '';
  if (lCol < 1) or (lCol >= Columns.Count) or (ARow < 0) or (ARow > Datalink.RecordCount) then
    Exit
  else
  if ARow = 0 then begin
    if Canvas.TextWidth(Columns[lCol].Field.FieldName) > Columns[lCol].Width - 4 then
      Hint := Columns[lCol].Field.FieldName
  end else begin
    // record the current row and switch to the one hovered over
    lOldRow := Datalink.ActiveRecord;
    Datalink.ActiveRecord := ARow - 1;
    try
      lNewHint := '';
      if Canvas.TextWidth(Columns[lCol].Field.DisplayText) > Columns[lCol].Width - 4 then
        // Text too wide for cell, so set as hint
        Hint := Columns[lCol].Field.DisplayText;
    finally
      Datalink.ActiveRecord := lOldRow;
    end; // try
  end;
end;  // TDBRTFGrid.GetCellHint

end.
