{===============================================================================
  Unit:        ImportWizardDBGrid

  Defines:     TImportWizardDBGrid

  Description: Component for grid functionality in the import wizard.

  Model:       IWGrid.mpb

  Created:     June 2004

  Last revision information:
    $Revision: 24 $
    $Date: 5/05/09 9:14 $
    $Author: Ericsalmon $

===============================================================================}
unit ImportWizardDBGrid;

interface

uses
  Sysutils, Classes, Types, Grids, DBGrids, ExceptionForm, DB, Graphics,
  Controls, Windows, DBClient, Messages, Contnrs, StdCtrls, Forms, ADODB;

resourcestring
  ResStr_NoKeyField =
      'Selected rows and error rows not supported without specifying the primary key field.';
  ResStr_RangeSelectionUnavailable =
      'Selection by range is only supported with ADO datasets.';

type
  TComboEvent = procedure (Sender: TObject; ACombo: TComboBox) of object;
  TGetCellErrorEvent = procedure (Sender: TObject; ACol: integer; var AError: string) of object;
  TGetCellValueEvent = procedure (Sender: TObject; ACol: Integer; var AValue: String) of object;

  TColumnSort = (csAscending, csDescending, csNone);

  EIWDBGrid = class(TExceptionPath)
  end;

  {-----------------------------------------------------------------------------
    Clicking column titles resorts using the column, or toggles the sort order if already
    sorted.
  }
  TImportWizardDBGrid = class(TDBGrid)
  private
    FComboDropDownCount: Integer;
    FCombos: TStringList;
    FErrorCount: Integer;
    FErrors: TStringList;
    FKeyField: TField;
    FLastCol: Integer;
    FLastHintedCell: TGridCoord;
    FOnColWidthsChanged: TNotifyEvent;
    FOnComboChange: TNotifyEvent;
    FOnComboDrawItem: TDrawItemEvent;
    FOnComboSetInitialValue: TComboEvent;
    FOnGetCellError: TGetCellErrorEvent;
    FOnGetCellHint: TGetCellValueEvent;
    FOnGetSelectedCellHighlight: TDBGridClickEvent;
    FOnPopulateCombo: TComboEvent;
    FOnSelectedRowsChanged: TNotifyEvent;
    FOnTopLeftChanged: TNotifyEvent;
    FPersistRowHeights: TList;
    FSelected: TStringList;
    FSelecting: Boolean;
    FSelectionEndRow: Integer;
    FSelectionStartRow: Integer;
    FShowCombos: Boolean;
    FShowErrors: Boolean;
    FShowSelected: Boolean;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure FilterRecord(DataSet: TDataSet; var Accept: Boolean);
    function GetActiveCellRect: TRect;
    function GetCellHasError(AColumn: TColumn): Boolean;
    procedure GetCellHint(ACol, ARow: integer);
    function GetColumnSortInfo(AColumn: TColumn): TColumnSort;
    function GetCurrentCellHasError: Boolean;
    function GetCurrentRowHasError: Boolean;
    function GetCurrentRowSelected: Boolean;
    function GetSelectedCount: Integer;
    function HasADODataset: Boolean;
    function OnGridData(AGridCoord: TGridCoord): Boolean;
    procedure PositionCombos;
    procedure RemoveCombos;
    procedure SelectRange(AFromRow, AToRow: integer);
    procedure SetCellHasError(AColumn: TColumn; const Value: Boolean);
    procedure SetComboDropDownCount(const Value: Integer);
    procedure SetCurrentCellHasError(Value: Boolean);
    procedure SetCurrentRowSelected(const Value: Boolean);
    procedure SetKeyField(const Value: TField);
    procedure SetOnComboChange(const Value: TNotifyEvent);
    procedure SetOnComboDrawItem(const Value: TDrawItemEvent);
    procedure SetShowCombos(Value: Boolean);
    procedure SetShowErrors(const Value: Boolean);
    procedure SetShowSelected(Value: Boolean);
    procedure WMCommand(var msg: TWMCommand); message WM_COMMAND;
    function GetPersistRowHeights(Index: Integer): Integer;
    procedure SetPersistRowHeights(Index: Integer; const Value: Integer);
  protected
    procedure ColEnter; override;
    procedure ColWidthsChanged; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RowHeightsChanged; override;
    procedure Scroll(Distance: Integer); override;
    procedure TitleClick(Column: TColumn); override;
    procedure TopLeftChanged; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function Active: Boolean;
    function GetColumnForCombo(ACombo: TComboBox): TColumn;
    function GetComboForColumn(Index: integer): TComboBox; overload;
    function GetComboForColumn(FieldName: String): TComboBox; overload;
    property ActiveCellRect: TRect read GetActiveCellRect;
    property CellHasError[AColumn: TColumn]: Boolean read GetCellHasError write
        SetCellHasError;
    property CurrentCellHasError: Boolean read GetCurrentCellHasError write
        SetCurrentCellHasError;
    property CurrentRowHasError: Boolean read GetCurrentRowHasError;
    property CurrentRowSelected: Boolean read GetCurrentRowSelected write
        SetCurrentRowSelected;
    property ErrorCount: Integer read FErrorCount;
    property KeyField: TField read FKeyField write SetKeyField;
    property PersistRowHeights[Index: Integer]: Integer read GetPersistRowHeights write
        SetPersistRowHeights;
    procedure ResetRowHeights;        
    property SelectedCount: Integer read GetSelectedCount;
    property ShowCombos: Boolean read FShowCombos write SetShowCombos;
    property ShowErrors: Boolean read FShowErrors write SetShowErrors;
    property ShowSelected: Boolean read FShowSelected write SetShowSelected;
  published
    property ComboDropDownCount: Integer read FComboDropDownCount write SetComboDropDownCount;
    property OnColWidthsChanged: TNotifyEvent read FOnColWidthsChanged write FOnColWidthsChanged;
    property OnComboChange: TNotifyEvent read FOnComboChange write SetOnComboChange;
    property OnComboDrawItem: TDrawItemEvent read FOnComboDrawItem write SetOnComboDrawItem;
    property OnComboSetInitialValue: TComboEvent read FOnComboSetInitialValue write
        FOnComboSetInitialValue;
    property OnGetCellError: TGetCellErrorEvent read FOnGetCellError write FOnGetCellError;
    property OnGetCellHint: TGetCellValueEvent read FOnGetCellHint write FOnGetCellHint;
    property OnGetSelectedCellHighlight: TDBGridClickEvent read FOnGetSelectedCellHighlight write
        FOnGetSelectedCellHighlight;
    property OnPopulateCombo: TComboEvent read FOnPopulateCombo write FOnPopulateCombo;
    property OnSelectedRowsChanged: TNotifyEvent read FOnSelectedRowsChanged write
        FOnSelectedRowsChanged;
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;
  end;

//==============================================================================
implementation

uses
  GeneralFunctions, ADOInt, Variants;

const
  LINE_BREAK=#13#10;

{-==============================================================================
    TImportWizardDBGrid
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TImportWizardDBGrid.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  Options := [dgEditing, dgAlwaysShowEditor, dgTitles,
              dgColumnResize, dgColLines, dgRowLines,
              dgTabs, dgConfirmDelete, dgCancelOnExit];

  FErrors              := TStringlist.Create;
  FErrors.Sorted       := True;
  FErrors.Duplicates   := dupIgnore;
  FSelected            := TStringlist.Create;
  FSelected.Sorted     := True;
  FSelected.Duplicates := dupIgnore;
  FSelecting           := False;
  FComboDropDownCount  :=  8;
  FLastHintedCell.X    := -1;
  FLastHintedCell.Y    := -1;
  FPersistRowHeights   := TList.Create;
end;  // TImportWizardDBGrid.Create

{-------------------------------------------------------------------------------
}
destructor TImportWizardDBGrid.Destroy;
begin
  FPersistRowHeights.Free;
  RemoveCombos;
  FreeAndNil(FErrors);
  FreeAndNil(FSelected);
  
  inherited Destroy;
end;  // TImportWizardDBGrid.Destroy 

{-------------------------------------------------------------------------------
  Returns true if the grid is properly loaded and populated. 
}
function TImportWizardDBGrid.Active: Boolean;
begin
  Result := True;
  if (csLoading in ComponentState) or (not Assigned(Datasource)) then
    Result := False
  else  if not Assigned(Datasource.Dataset) then
    Result := False
  else if not Datasource.Dataset.Active then
    Result := False;
end;  // TImportWizardDBGrid.Active 

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.CMHintShow(var Message: TCMHintShow);
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
end;  // TImportWizardDBGrid.CMHintShow 

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.ColEnter;
begin
  inherited;
  if Columns[Col].Field = KeyField then
    Col := FLastCol;
  FLastCol := Col;
end;  // TImportWizardDBGrid.ColEnter 

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.ColWidthsChanged;
begin
  inherited ColWidthsChanged;

  PositionCombos;
  if Assigned(FOnColWidthsChanged) then FOnColWidthsChanged(Self);
end;  // TImportWizardDBGrid.ColWidthsChanged

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState:
    TGridDrawState);
var
  lOldRow: Integer;
  lDrawCol: TColumn;

  // Draw the title row, including combos if relevant
  procedure DrawTitleCell;
  var
    lTitleRect: TRect;
  begin
    with Canvas do
    begin
      Font.Color := clBtnText;
      if FShowCombos then
      begin
        if not Assigned(GetComboForColumn(ACol)) then
        begin
          // For columns with no combo, title goes alongside combos.
          Brush.Color := clBtnFace;
          FillRect(ARect);
          lTitleRect := ARect;
          lTitleRect.Bottom := lTitleRect.Top + 22;
          DrawChoppedText(lDrawCol.Title.Caption, Canvas, lTitleRect, 2);
        end;
        if FCombos.Count > 0 then
          ARect.Top := ARect.Top + 22;
      end;
      Pen.Color   := clWindowText;
      Brush.Color := clBtnFace;
      InflateRect(ARect, 1, 0);
      ARect.Bottom := ARect.Bottom+1;
      Rectangle(ARect);
      Font.Color := GetContrastColour(MergeColours(Brush.Color, Brush.Color, 50));
      if (not FShowCombos) or Assigned(GetComboForColumn(ACol)) then
        DrawChoppedText(lDrawCol.Title.Caption, Canvas, ARect, 2);
      // Draw the sort order arrow if relevant
      Brush.Color := clWindowText;
      case GetColumnSortInfo(lDrawCol) of
        csAscending:
          Polygon([Point(ARect.Right - 12, ARect.Top + 11),
                   Point(ARect.Right -  4, ARect.Top + 11),
                   Point(ARect.Right -  8, ARect.Top +  7)]);
        csDescending:
          Polygon([Point(ARect.Right - 12, ARect.Top +  7),
                   Point(ARect.Right -  4, ARect.Top +  7),
                   Point(ARect.Right -  8, ARect.Top + 11)]);
      end; // case
    end;
  end;

  // Draw code when the grid is not active
  procedure DrawInactive;
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ARect);
  end;

  // Checks that the current row is the correct height for multiline data
  function CheckRowHeight: Boolean;
  var
    lRows: Integer;
    lTextToCheck: string;
  begin
    Result := True;
    lRows := 1;
    lTextToCheck := lDrawCol.Field.DisplayText;
    while Pos(LINE_BREAK, lTextToCheck) > 0 do begin
      lTextToCheck := Copy(lTextToCheck,
                           Pos(LINE_BREAK, lTextToCheck) + Length(LINE_BREAK),
                           Length(lTextToCheck));
      Inc(lRows);
    end;
    if RowHeights[ARow] < DefaultRowHeight * lRows then begin
      RowHeights[ARow] := DefaultRowHeight * lRows;
      Result := False;
    end;
    // Get updated value so it can be used in other places, like ActiveCellRect.
    PersistRowHeights[ARow] := RowHeights[ARow];
  end;

  procedure DrawCellText;
  var
    lRect: TRect;
    lLines: TStringList;
    i: integer;
  begin
    lLines := TStringList.Create;
    try
      ParseStringIntoList(lDrawCol.Field.DisplayText, LINE_BREAK, lLines);
      lRect := ARect;
      for i := 0 to lLines.Count - 1 do begin
        lRect.Bottom := lRect.Top + DefaultRowHeight;
        DrawChoppedText(lLines[i], Canvas, lRect, 2);
        lRect.Top := lRect.Top + DefaultRowHeight;
      end; // for
    finally
      lLines.Free;
    end;
  end;

begin
  if not Active then
  begin
    DrawInactive;
    Exit;
  end;

  lDrawCol := Columns[ACol];
  with Canvas do begin
    if ARow=0 then
      DrawTitleCell
    else begin
      lOldRow := Datalink.ActiveRecord;
      Datalink.ActiveRecord := ARow-1;
      try
        if CheckRowHeight then
        begin
          // Work out cell shading.
          if CurrentRowHasError and (lDrawCol.Field <> FKeyField) then
          begin
            if CellHasError[lDrawCol] then
              Brush.Color := clRed
            else
            if CurrentRowSelected then begin
              if ShowSelected then
                Brush.Color := MergeColours(clWindow, clHighlight, 90)
              else
                Brush.Color := clHighlight;
            end else
              Brush.Color := MergeColours(clWindow, clRed, 90);
          end else
          if CurrentRowSelected then begin
            Brush.Color := clHighlight;
            if Assigned(FOnGetSelectedCellHighlight) then
              FOnGetSelectedCellHighlight(lDrawCol);
          end else
          if lDrawCol.Readonly then begin
            Brush.Color := MergeColours(clWindow, clBtnFace, 50);
            if AState * [gdSelected, gdFocused] <> [] then
              Brush.Color := MergeColours(Brush.Color, clHighlight, 50);
          end else
            Brush.Color := clWindow;

          if Datasource.Dataset.RecNo mod 5 = 0 then
            Brush.Color := MergeColours(Brush.Color, clHighlight, 95);

          Font.Color := GetContrastColour(MergeColours(Brush.Color, Brush.Color, 50));
          FillRect(ARect);
          DrawCellText;
          DrawColumnCell(ARect, ACol, lDrawCol, AState);
        end;
      finally
        Datalink.ActiveRecord := lOldRow;
      end;
      if  (gdSelected in AState) and
          ((dgAlwaysShowSelection in Options) or Focused)
          and not (csDesigning in ComponentState)
          and not (dgRowSelect in Options)
          and (UpdateLock = 0)
          and (ValidParentForm(Self).ActiveControl = Self) then
        Windows.DrawFocusRect(Handle, ARect);
    end;
  end;
end;  // TImportWizardDBGrid.DrawCell

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.FilterRecord(DataSet: TDataSet; var Accept: Boolean);
begin
  Accept := True;

  if Assigned(KeyField) and Assigned(FErrors) and Assigned(FSelected) then
  begin
    if FShowErrors then
      Accept := FErrors.IndexOf(KeyField.AsString) > -1;
    if FShowSelected then
      Accept := Accept and (FSelected.IndexOf(KeyField.AsString) > -1);
  end;
end;  // TImportWizardDBGrid.FilterRecord

{-------------------------------------------------------------------------------
}
function TImportWizardDBGrid.GetActiveCellRect: TRect;
var
  i: Integer;
begin
  Result := CellRect(Col, Row);
  Result.Top := 0;
  for i := 0 to Row - 1 do
    Inc(Result.Top, PersistRowHeights[i] + 1); // +1 for the grid line.
  Result.Bottom := Result.Top + PersistRowHeights[Row];
end;  // TImportWizardDBGrid.GetActiveCellRect

{-------------------------------------------------------------------------------
}
function TImportWizardDBGrid.GetCellHasError(AColumn: TColumn): Boolean;
var
  lIndex: Integer;
begin
  Result := false;
  if Assigned(FKeyField) then begin
    try
      lIndex := FErrors.IndexOf(FKeyField.AsString);
    except on Exception do
      Exit;
    end;
    if lIndex>-1 then
      Result := TObjectList(FErrors.Objects[lIndex]).IndexOf(AColumn)>-1;
  end;
end;  // TImportWizardDBGrid.GetCellHasError 

{-------------------------------------------------------------------------------
  Updates the Hint property for the contents of a cell. 
}
procedure TImportWizardDBGrid.GetCellHint(ACol, ARow: integer);
var
  lNewHint: String;
  lOldRow: Integer;
begin
  Hint := '';
  if (ACol<0) or (ACol>=Columns.Count) or (ARow<1) or (ARow>Datalink.RecordCount) then
    Exit;
  // record the current row and switch to the one hovered over
  lOldRow := Datalink.ActiveRecord;
  Datalink.ActiveRecord := ARow-1;
  try
    if CellHasError[Columns[ACol]] then
    begin
      if Assigned(FOnGetCellError) then begin
        lNewHint := '';
        FOnGetCellError(Self, ACol, lNewHint);
        Hint := lNewHint;
      end;
    end else begin
      lNewHint := '';
      if Assigned(FOnGetCellHint) then
        FOnGetCellHint(Self, ACol, lNewHint);

      if lNewHint <> '' then
        Hint := lNewHint
      else
      if Canvas.TextWidth(Columns[ACol].Field.DisplayText) > Columns[ACol].Width - 4 then
        // Text too wide for cell, so set as hint
        Hint := Columns[ACol].Field.DisplayText;
    end;
  finally
    Datalink.ActiveRecord := lOldRow;
  end; // try
end;  // TImportWizardDBGrid.GetCellHint 

{-------------------------------------------------------------------------------
  Retrieve the column that a combo box is linked to, or nil if none found. 
}
function TImportWizardDBGrid.GetColumnForCombo(ACombo: TComboBox): TColumn;
var
  lIndex: Integer;
  i: Integer;
begin
  lIndex := FCombos.IndexOfObject(ACombo);
  Result := nil;
  if lIndex>-1 then
    for i := 0 to Columns.Count do
      if Columns[i].Field.FieldName = FCombos[lIndex] then begin
        Result := Columns[i];
        Break; // from loop
      end;
end;  // TImportWizardDBGrid.GetColumnForCombo 

{-------------------------------------------------------------------------------
  Retrieve the sort information currently available for a column from the IndexDefs of the
      dataset.
}
function TImportWizardDBGrid.GetColumnSortInfo(AColumn: TColumn): TColumnSort;
begin
  Result := csNone;
  if Datasource.Dataset is TClientDataset then begin
    with TClientDataset(Datasource.Dataset) do
      if IndexDefs.Count > 0 then
        if IndexDefs.Items[0].Fields = AColumn.Field.FieldName then
          if ixDescending in IndexDefs.Items[0].Options then
            Result := csDescending
          else
            Result := csAscending;
  end else
  if Datasource.Dataset is TCustomADODataSet then
    with TCustomADODataSet(Datasource.Dataset) do
      if Pos(AColumn.Field.FieldName, Sort) > 0 then
        if Pos(' DESC', Sort) > 0 then
          Result := csDescending
        else
          Result := csAscending;
end;  // TImportWizardDBGrid.GetColumnSortInfo

{-------------------------------------------------------------------------------
  Retrieve the combo box bound to a column.  Returns nil if none found.
}
function TImportWizardDBGrid.GetComboForColumn(Index: integer): TComboBox;
begin
  Result := GetComboForColumn(Columns[Index].Field.FieldName);
end;  // TImportWizardDBGrid.GetComboForColumn

{-------------------------------------------------------------------------------
  Retrieve the combo box bound to a column.  Returns nil if none found.
}
function TImportWizardDBGrid.GetComboForColumn(FieldName: String): TComboBox;
var
  lIndex: Integer;
begin
  lIndex := FCombos.IndexOf(FieldName);
  if lIndex>-1 then
    Result := TComboBox(FCombos.Objects[lIndex])
  else
    Result := nil;
end;  // TImportWizardDBGrid.GetComboForColumn

{-------------------------------------------------------------------------------
}
function TImportWizardDBGrid.GetCurrentCellHasError: Boolean;
begin
  Result := CellHasError[Columns[Col]];
end;  // TImportWizardDBGrid.GetCurrentCellHasError 

{-------------------------------------------------------------------------------
}
function TImportWizardDBGrid.GetCurrentRowHasError: Boolean;
begin
  if Assigned(FKeyField) then
    Result := FErrors.IndexOf(FKeyField.AsString)>-1
  else
    Result := False;
end;  // TImportWizardDBGrid.GetCurrentRowHasError 

{-------------------------------------------------------------------------------
}
function TImportWizardDBGrid.GetCurrentRowSelected: Boolean;
begin
  if Assigned(FKeyField) then
    Result := FSelected.IndexOf(FKeyField.AsString)>-1
  else
    Result := False;
end;  // TImportWizardDBGrid.GetCurrentRowSelected 

{-------------------------------------------------------------------------------
}
function TImportWizardDBGrid.GetSelectedCount: Integer;
begin
  Result := FSelected.Count;
end;  // TImportWizardDBGrid.GetSelectedCount 

{-------------------------------------------------------------------------------
}
function TImportWizardDBGrid.HasADODataset: Boolean;
begin
  Result := DataSource.DataSet is TCustomADODataset;
end;  // TImportWizardDBGrid.HasADODataset 

{-------------------------------------------------------------------------------
  MouseDown handler to implement multi-select when user clicks in the key column. 
}
procedure TImportWizardDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
    Integer);
var
  lCell: TGridCoord;
  lSelectionChanged: Boolean;
begin
  inherited;
  if not Active then Exit;
  FSelecting        := False;
  lSelectionChanged := False;
  lCell             := MouseCoord(X, Y);

  // If click not in title, clear selection, unless user is using extended select keys
  if (not ((ssShift in Shift) or (ssCtrl in Shift))) and (lCell.Y <> 0) then
  begin
    FSelected.Clear;
    lSelectionChanged := true;
  end;

  if not OnGridData(lCell) then
    Exit;

  if Columns[lCell.X].Field = KeyField then
  begin
    EditorMode := False;
    if (Button = mbLeft) and (dgMultiSelect in Options) and Datalink.Active then
    begin
      if (ssShift in Shift) and HasADODataset and (FSelectionStartRow <> 0) then
        SelectRange(FSelectionStartRow, DataSource.DataSet.RecNo)
      else begin
        FSelecting := True;
        FSelectionStartRow := DataSource.DataSet.RecNo;
        FSelectionEndRow   := DataSource.DataSet.RecNo;
        if ssCtrl in Shift then
          CurrentRowSelected := not CurrentRowSelected
        else begin
          FSelected.Clear;
          CurrentRowSelected := True;
        end;
        lSelectionChanged := True;
      end;
    end;
  end;

  if lSelectionChanged and Assigned(FOnSelectedRowsChanged) then
    FOnSelectedRowsChanged(Self);
end;  // TImportWizardDBGrid.MouseDown 

{-------------------------------------------------------------------------------
  Retrieve the appropriate hint (either an error, the full cell, or nothing) when hovering
      over a cell.
}
procedure TImportWizardDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lGridCoord: TGridCoord;
begin
  inherited;
  // Don't want anything happening at design-time.
  if not (csDesigning in ComponentState) then
  begin
    lGridCoord := MouseCoord(X, Y);
    if not OnGridData(lGridCoord) then
      Exit;

    // Find out if mouse has moved to another cell, and ignore fixed cells.
    if (lGridCoord.X <> FLastHintedCell.X) or (lGridCoord.Y <> FLastHintedCell.Y) then
      GetCellHint(lGridCoord.X, lGridCoord.Y);
    FLastHintedCell := lGridCoord;

    if FSelecting and HasADODataset then
      SelectRange(
          FSelectionStartRow, 
          DataSource.Dataset.RecNo + lGridCoord.Y - Row);
  end; // if
end;  // TImportWizardDBGrid.MouseMove

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lMouseUpCell: TGridCoord;
begin
  inherited;
  if not (Active and FSelecting and HasADODataset) then Exit;
  
  lMouseUpCell := MouseCoord(X, Y);
  if OnGridData(lMouseUpCell) then
    SelectRange(
        FSelectionStartRow, 
        DataSource.Dataset.RecNo + lMouseUpCell.Y - Row);
        
  FSelecting := False;
end;  // TImportWizardDBGrid.MouseUp 

{-------------------------------------------------------------------------------
  Returns true if a grid coordinate is on the data, i.e. not in the title row, or off the
      bottom or sides.
}
function TImportWizardDBGrid.OnGridData(AGridCoord: TGridCoord): Boolean;
begin
  Result := (AGridCoord.X>=0) and (AGridCoord.X<Columns.Count);
  if dgTitles in Options then
    Result := Result and (AGridCoord.Y>=1) and (AGridCoord.Y<=Datalink.RecordCount)
  else
    Result := Result and (AGridCoord.Y>=0) and (AGridCoord.Y<Datalink.RecordCount);
end;  // TImportWizardDBGrid.OnGridData 

{-------------------------------------------------------------------------------
  Hide all combos that should not be visible, and realign those that are visible to their
      respective columns.
}
procedure TImportWizardDBGrid.PositionCombos;
var
  i: Integer;
  lLeft: Integer;
  lCombo: TComboBox;
begin
  // Hide combos to left of first col
  if Assigned(FCombos) then begin
    for i := 0 to LeftCol-1 do begin
      lCombo := GetComboForColumn(i);
      if Assigned(lCombo) then
        lCombo.Visible := False;
    end;
    // Position and show the others
    lLeft := 0;
    for i := LeftCol to Columns.Count-1 do begin
      lCombo := GetComboForColumn(i);
      if Assigned(lCombo) then begin
        lCombo.Left := lLeft;
        lCombo.Width := Columns[i].Width;
        lCombo.Visible := True;
      end;
      lLeft := lLeft + Columns[i].Width+1;
    end;
  end;
end;  // TImportWizardDBGrid.PositionCombos 

{-------------------------------------------------------------------------------
  Clear the list of combos and free them, then nil the string list. 
}
procedure TImportWizardDBGrid.RemoveCombos;
var
  i: Integer;
begin
  if Assigned(FCombos) then
    for i := 0 to FCombos.Count-1 do
      TComboBox(FCombos.Objects[i]).Free;
  FreeAndNil(FCombos);
end;  // TImportWizardDBGrid.RemoveCombos 

{-------------------------------------------------------------------------------
  Increase the height of first row to accommodate combos if relevant. 
}
procedure TImportWizardDBGrid.RowHeightsChanged;
begin
  inherited RowHeightsChanged;
  
  if FShowCombos then
    if FCombos.Count > 0 then
      RowHeights[0] := DefaultRowHeight + TComboBox(FCombos.Objects[0]).Height+2;
end;  // TImportWizardDBGrid.RowHeightsChanged 

{-------------------------------------------------------------------------------
  Select a range of rows. This removes the previous selected range of rows if relevant.
}
procedure TImportWizardDBGrid.SelectRange(AFromRow, AToRow: integer);
var
  lBookmark: Variant;
  lRs: _Recordset;

  procedure SelectRow(const Key: String; State: Boolean);
  var
    I: Integer;
  begin
    if State then
      FSelected.Add(Key)
    else
    begin
      I := FSelected.IndexOf(Key);
      if I > -1 then FSelected.Delete(I);
    end;
    Invalidate;
  end;
  
  procedure SelectRows(AStart, AEnd: integer; AState: boolean);
  begin
    lRs.AbsolutePosition := AStart;
    while Integer(lRs.AbsolutePosition) <> AEnd do
    begin
      SelectRow(VarToStr(lRs.Fields[FKeyField.FieldName].Value), AState);
      if AEnd < AStart then
        lRs.MovePrevious
      else
        lRs.MoveNext;
    end;
    SelectRow(VarToStr(lRs.Fields[FKeyField.FieldName].Value), AState);
  end;

begin
  if not HasADODataSet then
    raise EIWDBGrid.Create(ResStr_RangeSelectionUnavailable);
  if not Assigned(FKeyField) then
    raise EIWDBGrid.Create(ResStr_NoKeyField);
  if AFromRow = AToRow then Exit;

  lRs := TCustomADODataset(DataSource.Dataset).Recordset;
  lBookmark := lRs.Bookmark;
  try
    if FSelectionStartRow <> FSelectionEndRow then
    begin
      // clear existing selection
      SelectRows(FSelectionStartRow, FSelectionEndRow, False);
    end;
    SelectRows(AFromRow, AToRow, True);
  finally
    lRs.Bookmark := lBookmark;
  end;
  FSelectionEndRow := AToRow;
  if Assigned(FOnSelectedRowsChanged) then
    FOnSelectedRowsChanged(Self);
end;  // TImportWizardDBGrid.SelectRange 

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.SetCellHasError(AColumn: TColumn; const Value: Boolean);
var
  lErrorIndex: Integer;
  lColIndex: Integer;
begin
  if not Assigned(FKeyField) then
    raise EIWDBGrid.Create(ResStr_NoKeyField);

  if Value then begin
    lErrorIndex := FErrors.IndexOf(FKeyField.DisplayText);
    if lErrorIndex=-1 then
      // No error in row, so make row level error
      lErrorIndex := FErrors.AddObject(FKeyField.DisplayText,
          TObjectList.Create(False));
    lColIndex := TObjectList(FErrors.Objects[lErrorIndex]).IndexOf(AColumn);
    if lColIndex=-1 then begin
      // mark cell with error
      TObjectList(FErrors.Objects[lErrorIndex]).Add(AColumn);
      Inc(FErrorCount);
    end;
  end
  else begin
    lErrorIndex := FErrors.IndexOf(FKeyField.DisplayText);
    if lErrorIndex > -1 then begin
      // Row has error, check if Col has error
      lColIndex := TObjectList(FErrors.Objects[lErrorIndex]).IndexOf(AColumn);
      if lColIndex >-1 then begin
        TObjectList(FErrors.Objects[lErrorIndex]).Delete(lColIndex);
        Dec(FErrorCount);
      end;
      if TObjectList(FErrors.Objects[lErrorIndex]).Count=0 then begin
        // no errors left in row, so clear row level error
        TObjectList(FErrors.Objects[lErrorIndex]).Free;
        FErrors.Delete(lErrorIndex);
      end;
    end;
  end;
  Invalidate;
end;  // TImportWizardDBGrid.SetCellHasError 

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.SetComboDropDownCount(const Value: Integer);
var
  i: Integer;
begin
  FComboDropDownCount := Value;
  if Assigned(FCombos) then
    for i := 0 to FCombos.Count-1 do
      TComboBox(FCombos.Objects[i]).DropDownCount := Value;
end;  // TImportWizardDBGrid.SetComboDropDownCount 

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.SetCurrentCellHasError(Value: Boolean);
begin
  CellHasError[Columns[Col]] := Value;
end;  // TImportWizardDBGrid.SetCurrentCellHasError 

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.SetCurrentRowSelected(const Value: Boolean);
var
  lIndex: Integer;
begin
  if Assigned(FKeyField) then
    if Value then
      FSelected.Add(FKeyField.DisplayText)
    else begin
      // remove selection
      lIndex := FSelected.IndexOf(FKeyField.DisplayText);
      if lIndex>-1 then
        FSelected.Delete(lIndex);
    end
  else
    raise EIWDBGrid.Create(ResStr_NoKeyField);
  Invalidate;
end;  // TImportWizardDBGrid.SetCurrentRowSelected 

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.SetKeyField(const Value: TField);
begin
  FKeyField := Value;
  
  if Col=Value.Index then
    Col := Col + 1;
end;  // TImportWizardDBGrid.SetKeyField 

{-------------------------------------------------------------------------------
  Accessor method (for all combos). 
}
procedure TImportWizardDBGrid.SetOnComboChange(const Value: TNotifyEvent);
var
  i: Integer;
begin
  FOnComboChange:= Value;
  if Assigned(FCombos) then
    for i := 0 to FCombos.Count-1 do
      TComboBox(FCombos.Objects[i]).OnChange := Value;
end;  // TImportWizardDBGrid.SetOnComboChange

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TImportWizardDBGrid.SetOnComboDrawItem(const Value: TDrawItemEvent);
var
  i: Integer;
begin
  FOnComboDrawItem:= Value;
  if Assigned(FCombos) then
    for i := 0 to FCombos.Count-1 do
      TComboBox(FCombos.Objects[i]).OnDrawItem := Value;
end;  // TImportWizardDBGrid.SetOnComboDrawItem

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.SetShowCombos(Value: Boolean);
var
  i: Integer;
  lCombo: TComboBox;
begin
  if FShowCombos <> Value then
  begin
    FShowCombos := Value;

    if FShowCombos then
    begin
      FCombos := TStringList.Create;
      for i := 0 to Columns.Count - 1 do
        if Columns[i].Field <> KeyField then
        begin
          lCombo := TComboBox.Create(nil);
          with lCombo do begin
            BevelOuter    := bvNone;
            BevelKind     := bkFlat;
            Ctl3D         := False;
            Top           := 0;
            Parent        := Self;
            Visible       := False;
            OnChange      := FOnComboChange;
            OnDrawItem    := FOnComboDrawItem;
            DropDownCount := FComboDropDownCount;
            if Assigned(FOnComboDrawItem) then
              Style := csOwnerDrawFixed;
            if Assigned(FOnPopulateCombo) then
              FOnPopulateCombo(Self, lCombo);
          end;

          FCombos.AddObject(Columns[i].Field.FieldName, lCombo);
        end;
      PositionCombos;
    end else
      RemoveCombos;
    Invalidate;
    RowHeightsChanged;
  end;
end;  // TImportWizardDBGrid.SetShowCombos 

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.SetShowErrors(const Value: Boolean);
begin
  if FShowErrors <> Value then
  begin
    FShowErrors := Value;
    Datasource.Dataset.OnFilterRecord := FilterRecord;
    Datasource.Dataset.Filtered       := False;
    Datasource.Dataset.Filtered       := FShowErrors or FShowSelected;
  end;
end;  // TImportWizardDBGrid.SetShowErrors

{-------------------------------------------------------------------------------
  Accessor method for displaying selected rows only.
}
procedure TImportWizardDBGrid.SetShowSelected(Value: Boolean);
begin
  if FShowSelected <> Value then
  begin
    FShowSelected := Value;
    Datasource.Dataset.OnFilterRecord := FilterRecord;
    Datasource.Dataset.Filtered       := False;
    Datasource.Dataset.Filtered       := FShowErrors or FShowSelected;
  end;
end;  // TImportWizardDBGrid.SetShowSelected 

{-------------------------------------------------------------------------------
  Click on the title to sort the data by the column. 
}
procedure TImportWizardDBGrid.TitleClick(Column: TColumn);
var
  lSort: TColumnSort;
  lCursor: TCursor;
begin
  inherited;
  if not Active then Exit;
  lCursor := HourglassCursor;
  try
    lSort := GetColumnSortInfo(Column);
    if Datasource.Dataset is TClientDataset then begin
      TClientDataset(Datasource.Dataset).IndexDefs.Clear;
      with TClientDataset(Datasource.Dataset).IndexDefs.AddIndexDef do begin
        Fields := Column.Field.FieldName;
        if lSort = csAscending then
          Options := [ixDescending];
        TClientDataset(Datasource.Dataset).IndexName := Name;
      end;
    end else
    if Datasource.Dataset is TCustomADODataset then begin
      with TCustomADODataset(Datasource.Dataset) do
        if lSort in [csNone, csDescending] then
          Sort := '[' + Column.Field.FieldName + ']'
        else
          Sort := '[' + Column.Field.FieldName + '] DESC';
      Scroll(0);
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // TImportWizardDBGrid.TitleClick

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.TopLeftChanged;
begin
  inherited TopLeftChanged;
  PositionCombos;
  if Assigned(FOnTopLeftChanged) then FOnTopLeftChanged(Self);
end;  // TImportWizardDBGrid.TopLeftChanged

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.WMCommand(var msg: TWMCommand);
begin
  if EditorMode and (msg.Ctl = InplaceEditor.Handle) Then
    inherited
  else
    If msg.Ctl <> 0 Then
      msg.result :=
        SendMessage(msg.ctl, CN_COMMAND,
                     TMessage(msg).wparam,
                     TMessage(msg).lparam);
end;  // TImportWizardDBGrid.WMCommand

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.Scroll(Distance: Integer);
begin
  inherited;
  ResetRowHeights;
end;

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.ResetRowHeights;
var
  i: integer;
begin
  for i := 0 to RowCount - 1 do
    RowHeights[i] := DefaultRowHeight;
end;

{-------------------------------------------------------------------------------
}
function TImportWizardDBGrid.GetPersistRowHeights(Index: Integer): Integer;
begin
  if not Assigned(FPersistRowHeights) then
    Result := DefaultRowHeight
  else begin
    while FPersistRowHeights.Count < RowCount do
      FPersistRowHeights.Add(TObject(DefaultRowHeight));
    Result := Integer(FPersistRowHeights[Index]);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TImportWizardDBGrid.SetPersistRowHeights(Index: Integer;
  const Value: Integer);
begin
  if not Assigned(FPersistRowHeights) then Exit;

  while FPersistRowHeights.Count < RowCount do
    FPersistRowHeights.Add(TObject(DefaultRowHeight));
  FPersistRowHeights[Index] := TObject(Value);
end;

end.
