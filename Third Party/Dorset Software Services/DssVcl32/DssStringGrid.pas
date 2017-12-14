{===============================================================================
  Unit:        DssStringGrid

  Defines:     TDSSStringGrid

  Description: StringGrid with extended functionality.

  Packages:    InHouse7, Delphi 7 package for generic components.

  Created:     September 2003

  Last revision information:
    $Revision: 42 $
    $Date: 20/12/07 16:14 $
    $Author: Johnvanbreda $

  Copyright © Dorset Software Services Ltd, 2003

===============================================================================}

unit DssStringGrid;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Grids, Contnrs, Graphics, ComCtrls,
  RichEdit, Dialogs, Messages;

type
  TDSSStringGrid = class;
  EDSSStringGridError = class (Exception)
  end;
  
  TCellDefaultEvent = procedure (Sender: TObject; ACol, ARow: Integer; var Options: 
      TGridOptions) of object;
  //----------------------------------------------------------------------------
  TCellCustomEvent = procedure (Sender: TObject; ACol, ARow: Integer; WinControl: 
      TWinControl) of object;
  //----------------------------------------------------------------------------
  TCellCustomKeyDown = procedure (Sender: TObject; var Key: Word; Shift: TShiftState; 
      AWinControl: TWinControl) of object;
  //----------------------------------------------------------------------------
  TColumnType = (ctDefault, ctCheckbox, ctCustom);

  TColumnInfo = class (TObject)
  private
    FColumnType: TColumnType;
    FGrid: TDSSStringGrid;
    FReadOnly: Boolean;
    FRichTextContent: Boolean;
    FWinControl: TWinControl;
    FWinControlKeyDown: TKeyEvent;
    procedure SetColumnType(Value: TColumnType);
    procedure SetRichTextContent(Value: Boolean);
    procedure SetWinControl(Value: TWinControl);
  public
    constructor Create(AGrid: TDSSStringGrid); reintroduce; overload;
    destructor Destroy; override;
    property Grid: TDSSStringGrid read FGrid;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property WinControlKeyDown: TKeyEvent read FWinControlKeyDown write FWinControlKeyDown;
  published
    property ColumnType: TColumnType read FColumnType write SetColumnType;
    property RichTextContent: Boolean read FRichTextContent write SetRichTextContent;
    property WinControl: TWinControl read FWinControl write SetWinControl;
  end;

  {-----------------------------------------------------------------------------
    Grid that allow TCustomEdit and TCustomComboBox descendant components to be used
    instead of the usual inplace editor.
  }
  TDSSStringGrid = class (TStringGrid)
  private
    FLastHintedCell: TGridCoord;
    FOnCellCustomKeyDown: TCellCustomKeyDown;
    FOnCellLeaveCustom: TCellCustomEvent;
    FOnCellLeaveDefault: TCellDefaultEvent;
    FOnCellSelectedCustom: TCellCustomEvent;
    FOnCellSelectedDefault: TCellDefaultEvent;
    FOnKeyDown: TKeyEvent;
    FReadOnly: Boolean;
    FRichTextBmp: TBitmap;
    FRichTextCtrl: TRichEdit;
    FRichTextFirstRun: Boolean;
    FColumnInfoObjects: TObjectList;
    FDrawRichText: Boolean;
    FHideSelection: Boolean;
    procedure CreateColumnInfoObjects;
    procedure DrawRichTextContent(const ARichText: String; const ARect: TRect; AState: TGridDrawState);
    function GetColumnInfo(Index: Integer): TColumnInfo;
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MoveGridCell(const AMoveRight: Boolean);
    procedure SetColCount(const Value: Integer);
    procedure SetDrawRichText(const Value: Boolean);
    procedure SetFixedCols(const Value: Integer);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetRowCount(const Value: Integer);
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure SetHideSelection(const Value: Boolean);
  protected
    procedure ColWidthsChanged; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure TopLeftChanged; override;
    procedure WinControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RemoveRow(ARow: Integer);
    property ColumnsInfo[Index: Integer]: TColumnInfo read GetColumnInfo;
  published
    property ColCount write SetColCount;
    property DefaultRowHeight default 19;
    property DrawRichText: Boolean read FDrawRichText write SetDrawRichText default True;
    property FixedCols write SetFixedCols;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property OnCellCustomKeyDown: TCellCustomKeyDown read FOnCellCustomKeyDown write
        FOnCellCustomKeyDown;
    property OnCellLeaveCustom: TCellCustomEvent read FOnCellLeaveCustom write
        FOnCellLeaveCustom;
    property OnCellLeaveDefault: TCellDefaultEvent read FOnCellLeaveDefault write
        FOnCellLeaveDefault;
    property OnCellSelectedCustom: TCellCustomEvent read FOnCellSelectedCustom write
        FOnCellSelectedCustom;
    property OnCellSelectedDefault: TCellDefaultEvent read FOnCellSelectedDefault write
        FOnCellSelectedDefault;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property Options default [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
        goColSizing];
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property RowCount write SetRowCount;
  end;
  
//==============================================================================
implementation

uses
  StdCtrls, GeneralFunctions;

const
  NO_MOVE    =  0;
  MOVE_RIGHT =  1;
  MOVE_LEFT  = -1;

type
  {-----------------------------------------------------------------------------
    Accessor for Style property.
  }
  TCustomComboAccessor = class (TCustomComboBox)
  end;
  
  {-----------------------------------------------------------------------------
    Allow access to Text property and OnKeyDown event regardless of the WinControl passed 
    to the ColumnInfo object.
  }
  TWinControlAccessor = class (TWinControl)
  end;
  
{-==============================================================================
    TDSSStringGrid
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TDSSStringGrid.Create(AOwner: TComponent);
begin
  inherited;
  FLastHintedCell.X := -1;
  FLastHintedCell.Y := -1;
  // Don't show highlight on cell when grid isn't focused.
  HideSelection := True;
  // Set to 19 to match a standard height of Edit box when placed over a cell.
  DefaultRowHeight := 19;
  // As goRangeSelect is left often requested than goColSizing, update the default.
  Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
                                                          goColSizing];
  inherited OnKeyDown := GridKeyDown;

  FColumnInfoObjects := TObjectList.Create;

  // RichText handling
  FDrawRichText := True;
  FRichTextBmp := TBitmap.Create;
  FRichTextCtrl := TRichEdit.Create(Self);
  with FRichTextCtrl do begin
    // Don't want anybody to know it's there.
    Visible := False;
    // It must have a parent for internal vcl reasons.
    Parent := Self;
    // Make it invisible at design time, completely.
    Width := 0;
    Height := 0;
  end;
  FRichTextFirstRun := True;
end;  // TDSSStringGrid.Create

{-------------------------------------------------------------------------------
}
destructor TDSSStringGrid.Destroy;
begin
  FRichTextCtrl.Free;
  FRichTextBmp.Free;

  FColumnInfoObjects.Free;

  inherited Destroy;
end;  // TDSSStringGrid.Destroy

{-------------------------------------------------------------------------------
  Handle hints, and let customized hints through too.
}
procedure TDSSStringGrid.CMHintShow(var Message: TCMHintShow);
var
  lPos: TPoint;
  lRect: Trect;
  lCol, lRow: Integer;
  lCellText: String;
  lHint: String;
begin
  lHint := Message.HintInfo^.HintStr;

  lPos:= ScreenToClient(Mouse.CursorPos);
  MouseToCell(lPos.X, lPos.Y, lCol, lRow);
  lRect := CellRect(lCol, lRow);
  if PtInRect(lRect, lPos) and (lHint = '') then begin
    lCellText := GetTextWithinLimit(Canvas, Cells[lCol, lRow],
                                    lRect.Right - lRect.Left - 4);
    if Cells[lCol, lRow] <> lCellText then
      lHint := Cells[lCol, lRow]
    else
      lHint := '';
  end;

  if lHint = '' then begin
    Hint := '';
    Application.HideHint;
  end else begin
    Hint := lHint;
    Message.HintInfo^.ReshowTimeout := Application.HintPause;
  end;

  inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.ColWidthsChanged;
begin
  inherited;
  // Call SelectCell to let user refresh the display of selected cell in resizes column.
  SelectCell(Col, Row);
end;  // TDSSStringGrid.ColWidthsChanged

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.CreateColumnInfoObjects;
begin
  while FColumnInfoObjects.Count < ColCount - FixedCols do
    FColumnInfoObjects.Add(TColumnInfo.Create(Self));
end;  // TDSSStringGrid.CreateColumnInfoObjects 

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  lDrawDone: Boolean;
begin
  inherited;

  // If user decided to take over drawing of cell, let it be...
  if Assigned(OnDrawCell) then Exit;

  // Otherwise, we do it.
  lDrawDone := False;
  if ACol >= FixedCols then
    if ColumnsInfo[ACol].RichTextContent and (ARow >= FixedRows) then begin
      if AState * [gdSelected, gdFocused] = [gdSelected] then
        Canvas.Brush.Color := clHighlight
      else
        Canvas.Brush.Color := clWindow;
      Canvas.FillRect(ARect);
      // Delegate render to another method.
      DrawRichTextContent(Cells[ACol, ARow], ARect, AState);
      lDrawDone := True;
    end;
    
  if not lDrawDone then begin
    // If grid not focused, get rid of default highlight in last selected cell, if required.
    if ([gdSelected, gdFocused] * AState = [gdSelected]) and HideSelection then begin
      Canvas.Brush.Color := Self.Color;
      Canvas.Font.Color  := Self.Font.Color;
    end;
    // Clear cell of default-drawn text.
    Canvas.FillRect(ARect);
    // Draw text with '...' if needed.
    DrawChoppedText(Cells[ACol, ARow], Canvas, ARect, 2);
  end;
end;  // TDSSStringGrid.DrawCell

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.DrawRichTextContent(const ARichText: String; const ARect: TRect;
  AState: TGridDrawState);
var
  lRange : TFormatRange;
  lLogX, lLogY: Integer;
  lText: String;

  { Setup the bitmap/rich edit dimensions and clear it before we start drawing
    on the canvas.  Called whilst drawing memo cells.  Also sets the copy mode
    of the grid to handle selected cell inversion }
  procedure PrepareComponents;
  var
    lMemStream: TMemoryStream;
  begin
    lMemStream := TMemoryStream.Create;
    try
      if Length(ARichText)>0 then
        lMemStream.Write(ARichText[1], Length(ARichText));
      lMemStream.Position := 0;
      FRichTextBmp.Width  := ARect.Right - ARect.Left - 2;
      FRichTextBmp.Height := ARect.Bottom - ARect.Top - 2;  // 2 pixel margin left at top
      FRichTextCtrl.Lines.LoadFromStream(lMemStream);

      if AState * [gdSelected, gdFocused] = [gdSelected] then begin
        FRichTextBmp.Canvas.Brush.Color := clHighlight;
        FRichTextCtrl.Color := clHighlight;
        FRichTextCtrl.DefAttributes.Color := clHighlightText;
      end else begin
        FRichTextBmp.Canvas.Brush.color := clWindow;
        FRichTextCtrl.Color := clWindow;
      end;

      FRichTextBmp.Canvas.FillRect(Rect(0, 0, FRichTextBmp.Width, FRichTextBmp.Height));
      FRichTextCtrl.Width := ARect.Right - ARect.Left;
    finally
      lMemStream.Free;
    end;
  end;

begin
  if FRichTextFirstRun then begin
    SendMessage(FRichTextCtrl.Handle, EM_FORMATRANGE, 0, 0);
    FRichTextFirstRun := False;
  end;

  PrepareComponents;
  if DrawRichText then begin
    with lRange do begin
      hdc := FRichTextBmp.Canvas.Handle;
      hdcTarget := hdc;
      lLogX  := GetDeviceCaps(hdc, LOGPIXELSX);
      lLogY  := GetDeviceCaps(hdc, LOGPIXELSY);
      rc     := Rect(0, 0,
                     (ARect.Right + 100) * 1440 div lLogX, ARect.Bottom * 1440 div lLogY);
      rcPage := Rect(0, 0,
                     FRichTextBmp.Width * 1440 div lLogX, FRichTextBmp.Height * 1440 div lLogY);
      chrg.cpMax := -1;
      chrg.cpMin := 0;
    end;

    SendMessage(FRichTextCtrl.Handle, EM_FORMATRANGE, 1, Longint(@lRange));
    // draw 2 pixels down - this aligns the text better with existing text
    Canvas.CopyRect(Rect(ARect.Left + 2, ARect.Top + 2, ARect.Right, ARect.Bottom),
                    FRichTextBmp.Canvas,
                    Rect(0, 0, FRichTextBmp.Width, FRichTextBmp.Height));
  end else begin
    // Remove all CR/LF from the text before it gets displayed.
    lText := StringReplace(FRichTextCtrl.Lines.Text, #13#10, '', [rfReplaceAll]);
    DrawChoppedText(lText, Canvas, ARect, 2);
  end;

  FRichTextCtrl.DefAttributes.Color := clWindowText;
end;  // TDSSStringGrid.DrawRichTextContent

{-------------------------------------------------------------------------------
}
function TDSSStringGrid.GetColumnInfo(Index: Integer): TColumnInfo;
begin
  // Ensure there are objects to return.
  CreateColumnInfoObjects;
  Result := TColumnInfo(FColumnInfoObjects[Index - FixedCols]);
end;  // TDSSStringGrid.GetColumnInfo

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lCellMove: Integer;
begin
  if not Focused and CanFocus and not EditorMode then SetFocus;

  if Assigned(FOnKeyDown) then FOnKeyDown(Sender, Key, Shift);

  lCellMove := NO_MOVE; // no movement by default

  // return key always moves, irrespective of edit text
  if Key = VK_RETURN then begin
    if not (ssCtrl in Shift) then lCellMove := MOVE_RIGHT
                             else lCellMove := MOVE_LEFT;
  end;
  
  if not(goRowSelect in Options) and Assigned(InplaceEditor) then
    // right arrow
    if Key = VK_RIGHT then begin
      // cannot edit, so always navigate within grid
      if not EditorMode then
        lCellMove := MOVE_RIGHT
      else
      // editable, but caret at right limit of text, or all text selected
      if (InplaceEditor.SelStart = Length(InplaceEditor.Text)) or
         (InplaceEditor.SelLength = Length(InplaceEditor.Text)) then
        lCellMove := MOVE_RIGHT;
    end else
    // left arrow
    if Key = VK_LEFT then begin
      // cannot edit, so always navigate within grid
      if not EditorMode then
        lCellMove := MOVE_LEFT
      else
      // editable, but caret at left limit of text, or all text selected
      if (InplaceEditor.SelStart = 0) or
         (InplaceEditor.SelLength = Length(InplaceEditor.Text)) then
        lCellMove := MOVE_LEFT;
    end;

  if lCellMove <> NO_MOVE then begin
    MoveGridCell(lCellMove = MOVE_RIGHT);
    Key := 0; // no further action on this keypress
  end; // if
end;  // TDSSStringGrid.GridKeyDown

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lGridCoord: TGridCoord;
  lCol, lRow: Integer;
  lRect: TRect;
  lCellText: String;
begin
  inherited;
  // Don't want anything happening at design-time.
  if not (csDesigning in ComponentState) then begin
    lGridCoord := MouseCoord(X, Y);
    lCol       := lGridCoord.X;
    lRow       := lGridCoord.Y;
    // Find out if mouse has moved to another cell, and ignore fixed cells.
    if ((lCol <> FLastHintedCell.X) or (lRow <> FLastHintedCell.Y)) and
       (lCol > FixedCols - 1) and (lRow > FixedRows - 1) then
    begin
      // Find out if the text shown is shorter than the text stored.
      lRect := CellRect(lCol, lRow);
      lCellText := GetTextWithinLimit(Canvas, Cells[lCol, lRow],
                                      lRect.Right - lRect.Left - 4);
      if Cells[lCol, lRow] <> lCellText then
      begin
        FLastHintedCell.X := lCol;
        FLastHintedCell.Y := lRow;
        Hint := Cells[lCol, lRow];
      end else
        Hint := '';  // Text fully displayed, no need for hint.
    end;
  end;
end;  // TDSSStringGrid.MouseMove

{-------------------------------------------------------------------------------
  MoveGridCell - navigates the current cell to the right or left in the grid. If you hit
      the end, goes to the next/previous row where one is available
}
procedure TDSSStringGrid.MoveGridCell(const AMoveRight: Boolean);
begin
  if AMoveRight then begin
    if not(goRowSelect in Options) and (Col < ColCount - 1) then
      Col := Col + 1
    else
    if (Row < RowCount - 1) then begin
      Row := Row + 1;
      Col := FixedCols;
    end;
  end else begin  // Shift-Return
    if not (goRowSelect in Options) and (Col > FixedCols) then
      Col := Col - 1
    else
    if (Row > 1) then begin // don't move into title row
      Row := Row - 1;
      Col := ColCount - 1;
    end;
  end;
end;  // TDSSStringGrid.MoveGridCell 

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.RemoveRow(ARow: Integer);
var
  i: Integer;
  lColInfo: TColumnInfo;
begin
  if ARow > FixedRows - 1 then begin
    if not (goRowSelect in Options) then begin
      // If custom control on column, hide before deleting row.
      // It affects SelectCell, so it must be hidden!
      lColInfo := ColumnsInfo[Col];
      if (lColInfo.ColumnType = ctCustom) and lColInfo.WinControl.Visible then
        lColInfo.WinControl.Visible := False;
    end;
  
    // On first row after fixed row, if any
    if (RowCount > 0) and (RowCount <= FixedRows + 1) and (ARow = FixedRows) then
      Rows[Row].Clear  // Just clear row content
    else begin                              // Further down the grid
      for i := Row + 1 to RowCount - 1 do   // Copying while overwrite current Row
        Rows[i - 1] := Rows[i];
      Rowcount := RowCount - 1;             // Only need to descrease RowCount now
    end;
  
    // Now reselect the cell to get any custom control redisplayed, if any.
    SelectCell(Col, Row);
    Refresh;
  end;
end;  // TDSSStringGrid.RemoveRow 

{-------------------------------------------------------------------------------
}
function TDSSStringGrid.SelectCell(ACol, ARow: Longint): Boolean;
var
  lColInfo: TColumnInfo;
  lOptions: TGridOptions;
  lRect: TRect;
begin
  // Ensure there are objects for each column.
  CreateColumnInfoObjects;

  Result := inherited SelectCell(ACol, ARow);

  // If RowSelect option is on, can't select individual cells.
  if not (csLoading in ComponentState) and not (goRowSelect in Options) then begin
    // Use current Col/Row in this part.
    lColInfo := ColumnsInfo[Col];
    lOptions := Options;
    // Always do the stuff to de-select the cell.
    if lColInfo.ColumnType = ctDefault then begin
      // Allow for tailored behaviour.
      if not ReadOnly and Assigned(FOnCellLeaveDefault) then
        FOnCellLeaveDefault(Self, Col, Row, lOptions);
      Options := lOptions - [goEditing, goAlwaysShowEditor];
    end else
    if (lColInfo.ColumnType = ctCustom) and lColInfo.WinControl.Visible then begin
      // Allow for tailored behaviour.
      if not ReadOnly and Assigned(FOnCellLeaveCustom) then
        FOnCellLeaveCustom(Self, Col, Row, lColInfo.WinControl);
      lColInfo.WinControl.Visible := False;
    end;

    // If OnSelectCell returned False, don't do anything else.
    if Result and not ReadOnly then begin
      // Use target ACol/ARow in this part.
      lColInfo := ColumnsInfo[ACol];
      if lColInfo.ColumnType = ctDefault then begin
        if lColInfo.ReadOnly then
          lOptions := Options - [goEditing, goAlwaysShowEditor]
        else begin
          EditorMode := True;
          lOptions := Options + [goEditing, goAlwaysShowEditor];
        end;
        // Allow for tailored behaviour.
        if Assigned(FOnCellSelectedDefault) then
          FOnCellSelectedDefault(Self, ACol, ARow, lOptions);
        Options := lOptions;
      end else
      if lColInfo.ColumnType = ctCustom then begin
        lRect := CellRect(ACol, ARow);
        if lColInfo.WinControl = nil then
          raise EDSSStringGridError.Create(Format('Control not assigned for column (%d).',
                                                  [ACol]));
        lColInfo.WinControl.SetBounds(lRect.Left - 1,
                                      lRect.Top - 1,
                                      lRect.Right - lRect.Left + 2,
                                      lRect.Bottom - lRect.Top);
        // Allow for tailored behaviour.
        if Assigned(FOnCellSelectedCustom) then
          FOnCellSelectedCustom(Self, ACol, ARow, lColInfo.WinControl);
        lColInfo.WinControl.Visible := True;
        // Set focus only if possible, or bad things happen...
        if ColumnsInfo[ACol].WinControl.CanFocus and
                      AllParentsArevisible(ColumnsInfo[ACol].WinControl) then
          ColumnsInfo[ACol].WinControl.SetFocus;
      end;
    end;
  end;
end;  // TDSSStringGrid.SelectCell

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.SetColCount(const Value: Integer);
var
  lColCount: Integer;
begin
  // Get existing count
  lColCount := inherited ColCount;
  if lColCount <> Value then begin
    // If there are more columns than requested, get rid of superfluous objects first,
    // before we lose the reference.
    while FColumnInfoObjects.Count > Value - FixedCols do
      FColumnInfoObjects.Delete(FColumnInfoObjects.Count - 1);

    // Resize grid
    inherited ColCount := Value;
  
    // Now, in case there were more columns added, ensure they all have an object.
    CreateColumnInfoObjects;
  end;
end;  // TDSSStringGrid.SetColCount 

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.SetDrawRichText(const Value: Boolean);
begin
  FDrawRichText := Value;
  Refresh;
end;  // TDSSStringGrid.SetDrawRichText

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.SetFixedCols(const Value: Integer);
begin
  if FixedCols <> Value then
  begin
    // in case FixedCols has increased, dispose of excess column info objects
    while FColumnInfoObjects.Count > ColCount - Value do
      FColumnInfoObjects.Delete(FColumnInfoObjects.Count - 1);

    inherited FixedCols := Value;

    // in case FixedCols has decreased, create additional column info objects
    CreateColumnInfoObjects;
  end;
end;  // TDSSStringGrid.SetFixedCols 

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.SetHideSelection(const Value: Boolean);
begin
  FHideSelection := Value;
  Invalidate;
end;  // TDSSStringGrid.SetHideSelection

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.SetReadOnly(const Value: Boolean);
begin
  SelectCell(Col, Row);  // Needed so that the current cell's data is saved.
  FReadOnly := Value;
  SelectCell(Col, Row);  // Necesarry in case read only changed.
end;  // TDSSStringGrid.SetReadOnly 

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.SetRowCount(const Value: Integer);
begin
  inherited RowCount := Value;
end;  // TDSSStringGrid.SetRowCount

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.TopLeftChanged;
var
  lRect: TRect;
begin
  inherited;
  if Row < TopRow then  begin
    // Current row was scrolled up out of view. So set to TopRow (first visible row).
    SelectCell(Col, TopRow);
    Row := TopRow;
  end
  else if Row > (TopRow + VisibleRowCount - 1) then begin
    // Current row was scrolled down out of view. So set to last visible row.
    SelectCell(Col, Row);
    Row := TopRow + VisibleRowCount - 1;
  end;
  if Assigned(ColumnsInfo[Col]) and (Row >= TopRow) then
    if Assigned(ColumnsInfo[Col].WinControl) then begin
      lRect := CellRect(Col, Row);
      ColumnsInfo[Col].WinControl.SetBounds(lRect.Left - 1,
                              lRect.Top - 1,
                              lRect.Right - lRect.Left + 2,
                              lRect.Bottom - lRect.Top);
    end;
end;  // TDSSStringGrid.TopLeftChanged

{-------------------------------------------------------------------------------
}
procedure TDSSStringGrid.WinControlKeyDown(Sender: TObject; var Key: Word; Shift: 
    TShiftState);
var
  lCellMove: Integer;
  lText: String;
  lMoveLeftOk: Boolean;
  lMoveRightOk: Boolean;
  
  procedure SendKeyDownToGrid;
  begin
    KeyDown(Key, Shift);
    // Key handled, clear it.
    Key := 0;
  end;
  
begin
  lText := '';
  
  // Get some value to determine which way we should move in the grid.
  if (Sender is TCustomEdit) or (Sender is TCustomCombo) then begin
    // Text is coming all the way from TControl. So the property exists.
    lText := TWinControlAccessor(Sender).Text;
    if Sender is TCustomEdit then
      with TCustomEdit(Sender) do begin
        // editable, caret at left limit of text, or all text selected
        lMoveLeftOk := (SelStart = 0) or (SelLength = Length(lText));
        // editable, caret at right limit of text, or all text selected
        lMoveRightOk := (SelStart = Length(lText)) or (SelLength = Length(lText));
      end
    else
      with TCustomComboAccessor(Sender) do begin
        // editable, caret at left limit of text, or all text selected
        lMoveLeftOk := (Style = csDropDownList) or
                       (SelStart = 0) or (SelLength = Length(lText));
        // editable, caret at right limit of text, or all text selected
        lMoveRightOk := (Style = csDropDownList) or
                        (SelStart = Length(lText)) or (SelLength = Length(lText));
      end
  end
  else begin
    lMoveLeftOk := True;
    lMoveRightOk := True;
  end;
  
  // Let user do stuff for custom control. Key should be set to zero if nothing
  // should happen afterward.
  if ColumnsInfo[Col].ColumnType = ctCustom then begin
    if Assigned(ColumnsInfo[Col].WinControlKeyDown) then
      ColumnsInfo[Col].WinControlKeyDown(Sender, Key, Shift);
    if Assigned(FOnCellCustomKeyDown) then
      FOnCellCustomKeyDown(Sender, Key, Shift, ColumnsInfo[Col].WinControl);
  end;
  
  // Now find out if we need to move rigth or left.
  lCellMove := NO_MOVE; // no movement by default
  case Key of
    VK_LEFT:
        if lMoveLeftOk then
          lCellMove := MOVE_LEFT;
    VK_RIGHT:
        if lMoveRightOk then
          lCellMove := MOVE_RIGHT;
    VK_RETURN:
        begin
          // Close combo before moving on, if it's a combo.
          if (Sender is TCustomComboBox) then
            TCustomComboAccessor(Sender).DroppedDown := False;

          if not (ssCtrl in Shift) then lCellMove := MOVE_RIGHT
                                   else lCellMove := MOVE_LEFT;
        end;
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT:
        if (Sender is TCustomComboBox) then begin
          if not TCustomComboAccessor(Sender).DroppedDown and not (ssAlt in Shift) then
            SendKeyDownToGrid;
        end else
          SendKeyDownToGrid;
    VK_HOME:
        if (Sender is TCustomComboBox) then begin
          if not TCustomComboAccessor(Sender).DroppedDown and lMoveLeftOk then
            SendKeyDownToGrid;
        end else
        if lMoveLeftOk then
          SendKeyDownToGrid;
    VK_END:
        if (Sender is TCustomComboBox) then begin
          if not TCustomComboAccessor(Sender).DroppedDown and lMoveRightOk then
            SendKeyDownToGrid;
        end else
        if lMoveRightOk then
          SendKeyDownToGrid;
    VK_F2:
        if (Sender is TCustomEdit) then begin
          TCustomEdit(Sender).SelLength := 0;
          Key := 0;
        end else if (Sender is TCustomComboBox) then begin
          TCustomComboAccessor(Sender).SelLength := 0;
          Key := 0;
        end;
    VK_ESCAPE:
        SendKeyDownToGrid;
  end;
  
  if lCellMove <> NO_MOVE then begin
    // Give focus back to grid for next steps.
    SetFocus;
    MoveGridCell(lCellMove = MOVE_RIGHT);
    Key := 0; // no further action on this keypress
  end;
end;  // TDSSStringGrid.WinControlKeyDown 

{-------------------------------------------------------------------------------
  Relay any command messages to parented combo boxes
}
procedure TDSSStringGrid.WMCommand(var Message: TWMCommand);
var
  lHandled: Boolean;
begin
  lHandled := False;
  // Test if InplaceEditor showing first.
  if Assigned(InplaceEditor) then
    if EditorMode and (Message.Ctl = InplaceEditor.Handle) then begin
      lHandled := True;
      inherited;
    end;
  // If not InplaceEditor, send on to whatever control wants it.
  if not lHandled and (Message.Ctl <> 0) then
    Message.Result := SendMessage(Message.Ctl, CN_COMMAND,
                                  TMessage(Message).WParam, TMessage(Message).LParam);
end;

{-==============================================================================
    TColumnInfo
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TColumnInfo.Create(AGrid: TDSSStringGrid);
begin
  inherited Create;

  FGrid       := AGrid;
  FColumnType := ctDefault;
  FRichTextContent := False;
  FWinControl := nil;
  FWinControlKeyDown := nil;
end;  // TColumnInfo.Create

{-------------------------------------------------------------------------------
}
destructor TColumnInfo.Destroy;
begin
  WinControl := nil;

  inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TColumnInfo.SetColumnType(Value: TColumnType);
begin
  // If resetting to default type, clear the reference in WinControl.
  if Value = ctDefault then begin
    // Restore direct link to KeyDown event, if there was one.
    if Assigned(FWinControl) then
      TWinControlAccessor(FWinControl).OnKeyDown := FWinControlKeyDown;
    // Now lose the links to control itself.
    FWinControl := nil;
    FWinControlKeyDown := nil;
  end;
end;  // TColumnInfo.SetColumnType

{-------------------------------------------------------------------------------
}
procedure TColumnInfo.SetRichTextContent(Value: Boolean);
begin
  FRichTextContent := Value;
end;  // TColumnInfo.SetRichTextContent

{-------------------------------------------------------------------------------
}
procedure TColumnInfo.SetWinControl(Value: TWinControl);
begin
  // Reset ColumnType to default if Value is nil. Otherwise, set it to custom.
  if Value = nil then FColumnType := ctDefault
                 else FColumnType := ctCustom;

  if Assigned(FWinControl) then
    TWinControlAccessor(FWinControl).OnKeyDown := FWinControlKeyDown;

  FWinControl := Value;
  FWinControlKeyDown := nil;

  if Assigned(FWinControl) then begin
    FWinControl.Parent := Grid;
    // If control has a KeyDown event linked already, keep it.
    FWinControlKeyDown := TWinControlAccessor(FWinControl).OnKeyDown;
    TWinControlAccessor(FWinControl).OnKeyDown := Grid.WinControlKeyDown;
  end;
end;  // TColumnInfo.SetWinControl

end.
