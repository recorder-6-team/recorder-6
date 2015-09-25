{===============================================================================
  Unit:        BaseLegend

  Defines:     TBaseLegend
               TBaseLegendItem
               TGridAccessor

  Description: Base classes for the legend boxes on the side of the map browser.

  Model:       Map.mpb

  Created:     December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 15/04/09 16:51 $
    $Author: Ericsalmon $

===============================================================================}
unit BaseLegend;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  Grids, Contnrs, DataClasses;

const
  MAX_TITLE_LENGTH = 20;  // Max character length for titles.

type
  TBaseLegendItem = class;

  TGridAccessor = class (TStringGrid)
  public
    property InplaceEditor;
    property OnMouseWheel;
  end;
  
  {-----------------------------------------------------------------------------
    The AOwner parameter is for XPMenu to know about the popup menu, so it can be drawm 
    properly.
  }
  TBaseLegend = class (TObject)
  private
    FGrid: TStringGrid;
    FGridSelectCell: TSelectCellEvent;
    FItems: TObjectList;
    FLegendDelete: TMenuItem;
    FLegendProperties: TMenuItem;
    FLegendRename: TMenuItem;
    FNavigatingGrid: Boolean;
    FOnChange: TNotifyEvent;
    FOnDeleteItem: TNotifyEvent;
    FOnUpdatePopupItemState: TNotifyEvent;
    FPopupMenu: TPopupMenu;
    function GetCount: Integer;
    function GetLegendItems(Index: Integer): TBaseLegendItem;
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: 
        TGridDrawState);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridKeyPress(Sender: TObject; var Key: Char);
    procedure GridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; 
        MousePos: TPoint; var Handled: Boolean);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure GridStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure PopupDeleteClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure PopupPropertiesClick(Sender: TObject);
    procedure PopupRenameClick(Sender: TObject);
    procedure ProcessItem(ACol, ARow: Integer; AItem: TBaseLegendItem);
  protected
    procedure Change; virtual;
    procedure GridClick(Sender: TObject); virtual;
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
    procedure InitialisePopupMenu; virtual;
    procedure RefreshData; virtual;
    procedure RefreshItem(AItem: TBaseLegendItem); virtual;
    procedure SetGrid(Value: TStringGrid); virtual;
    procedure SynchroniseGrid;
    procedure UpdateMenuState(AItem: TBaseLegendItem); virtual;
    procedure UpdateMenuItemState(AMenuItem: TMenuItem); virtual;
    property Items: TObjectList read FItems;
  public
    constructor Create(AOwner: TComponent; AGrid: TStringGrid); reintroduce; virtual;
    destructor Destroy; override;
    function AddItem(AItem: TBaseLegendItem): Boolean; virtual;
    function AddPopupMenuItem(const ACaption: String; AHandler: TNotifyEvent = nil; APosition: 
        Integer = -1): TMenuItem;
    procedure CheckColumnWidths;
    procedure Clear;
    procedure DeleteItem(const Index: Integer); virtual;
    procedure EditItemProperties(Index: Integer);
    function IndexOf(AItem: TBaseLegendItem): Integer;
    procedure Refresh;
    procedure RenameItem(Index: Integer);
    property Count: Integer read GetCount;
    property Grid: TStringGrid read FGrid write SetGrid;
    property LegendItems[Index: Integer]: TBaseLegendItem read GetLegendItems; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDeleteItem: TNotifyEvent read FOnDeleteItem write FOnDeleteItem;
    property OnUpdatePopupItemState: TNotifyEvent read FOnUpdatePopupItemState write FOnUpdatePopupItemState;
  end;
  
  TBaseLegendItem = class (TObject)
  private
    FEditingProperties: Boolean;
    FInitializing: Boolean;
    FLegend: TBaseLegend;
    FMapSheetKey: TKeyString;
    FOnNeedRefresh: TNotifyEvent;
    FTitle: String;
    FTitleChanged: Boolean;
    FVisibilityChanged: Boolean;
    FVisible: Boolean;
    procedure SetTitle(const Value: String);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure DoAssign(ASource: TBaseLegendItem); virtual;
    function DoEditProperties: Boolean; virtual; abstract;
    procedure DoSetTitle(const Value: String); virtual;
    procedure DoSetVisible(const Value: Boolean); virtual;
    function GetAlwaysVisible: Boolean; virtual;
    function GetCanDelete: Boolean; virtual;
    function GetCanEdit: Boolean; virtual;
    function GetCanRename: Boolean; virtual;
    procedure ResetFlags; virtual;
    property EditingProperties: Boolean read FEditingProperties;
    property Initializing: Boolean read FInitializing write FInitializing;
  public
    constructor Create; virtual;
    procedure Assign(ASource: TBaseLegendItem);
    procedure DrawSymbol(ACanvas: TCanvas; ARect: TRect); virtual; abstract;
    procedure EditProperties;
    property AlwaysVisible: Boolean read GetAlwaysVisible;
    property CanDelete: Boolean read GetCanDelete;
    property CanEdit: Boolean read GetCanEdit;
    property CanRename: Boolean read GetCanRename;
    property Legend: TBaseLegend read FLegend;
    property MapSheetKey: TKeyString read FMapSheetKey write FMapSheetKey;
    property OnNeedRefresh: TNotifyEvent read FOnNeedRefresh write FOnNeedRefresh;
    property Title: String read FTitle write SetTitle;
    property TitleChanged: Boolean read FTitleChanged;
    property VisibilityChanged: Boolean read FVisibilityChanged;
    property Visible: Boolean read FVisible write SetVisible;
  end;
  
//==============================================================================
implementation

uses
  GeneralFunctions, ApplicationSettings, Constants;

const
  COL_OBJECT = 0;  // Column which holds the reference to the LegendItem object.
  COL_CHECK  = 0;  // Column for the visibility switch checkboxes
  COL_SYMBOL = 1;  // Column for the symbol for the legend items
  COL_TITLE  = 2;  // Column for the items title

resourcestring
  ResStr_PopupDelete = 'Delete';
  ResStr_PopupRename = 'Rename';
  ResStr_PopupProperties = 'Properties...';

{-==============================================================================
    TBaseLegend
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBaseLegend.Create(AOwner: TComponent; AGrid: TStringGrid);
begin
  inherited Create;
  
  FPopupMenu := TPopupMenu.Create(AOwner);
  FPopupMenu.OnPopup := PopupMenuPopup;
  InitialisePopupMenu;
  FItems := TObjectList.Create;
  Grid := AGrid;
end;  // TBaseLegend.Create 

{-------------------------------------------------------------------------------
}
destructor TBaseLegend.Destroy;
begin
  Grid := nil;
  FItems.Free;
  FPopupMenu.Items.Clear;
  FreeAndNil(FPopupMenu);
  
  inherited;
end;  // TBaseLegend.Destroy 

{-------------------------------------------------------------------------------
  Returns true if item added, false if something went wrong. 
}
function TBaseLegend.AddItem(AItem: TBaseLegendItem): Boolean;
begin
  FItems.Add(AItem);
  AItem.FLegend := Self;
  if Assigned(Grid) then
    with Grid do begin
      // If first row has item, add more rows. If first row does not have item, add it there.
      if Assigned(Objects[COL_OBJECT, 0]) then
        RowCount := RowCount + 1;
      Objects[COL_OBJECT, RowCount - 1] := AItem;
      Cells[COL_TITLE, RowCount - 1]    := AItem.Title;
      Invalidate;
      CheckColumnWidths;
    end;
  Result := True;
  Change;
end;  // TBaseLegend.AddItem 

{-------------------------------------------------------------------------------
}
function TBaseLegend.AddPopupMenuItem(const ACaption: String; AHandler: TNotifyEvent = nil; 
    APosition: Integer = -1): TMenuItem;
begin
  Result := TMenuItem.Create(FPopupMenu);
  Result.Caption := ACaption;
  Result.OnClick := AHandler;
  if APosition = -1 then FPopupMenu.Items.Add(Result)
                    else FPopupMenu.Items.Insert(APosition, Result);
end;  // TBaseLegend.AddPopupMenuItem 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;  // TBaseLegend.Change 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.CheckColumnWidths;
var
  lMin, lMax: Integer;
begin
  if Assigned(FGrid) then
    with FGrid do begin
      ColWidths[COL_CHECK]  := 16;
      ColWidths[COL_SYMBOL] := 30;
      ColWidths[COL_TITLE]  := Width - 46 - 4;  // 4 for borders
  
      // Check the horizontal scrollbar is visible, or not.
      // That would be because column width is too big.
      GetScrollRange(Handle, SB_HORZ, lMin, lMax);
      // Adjust width of Title column accordingly so that it disappears.
      if lMin <> lMax then
        ColWidths[COL_TITLE] := ColWidths[COL_TITLE] - GetSystemMetrics(SM_CYHSCROLL);
    end;
end;  // TBaseLegend.CheckColumnWidths 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.Clear;
begin
  FItems.Clear;  // Calls Free on each contained object.
  if Assigned(Grid) then begin
    Grid.RowCount := 1;
    Grid.Rows[0].Clear;
    Grid.Invalidate;
  end;
end;  // TBaseLegend.Clear 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.DeleteItem(const Index: Integer);
var
  lItem: TBaseLegendItem;
  i: Integer;
  lDummyCanSelect: Boolean;
begin
  if Assigned(Grid) then
    with Grid do begin
      lItem := LegendItems[Index];
      if Assigned(lItem) then begin
        // Hide the item, but don't trigger an refresh event.
        lItem.FVisible := False;
        // If something needs to be done before the objet disappears for good...
        if Assigned(FOnDeleteItem) then FOnDeleteItem(lItem);
        // Extract from list without freeing, as we send it through in event
        lItem := TBaseLegendItem(FItems.Extract(lItem));
        // Now free the object.
        FreeAndNil(lItem);
  
        // Now shrink the grid.
        if (RowCount > 0) and (RowCount <= FixedRows + 1) and (Index = FixedRows) then
          Rows[Index].Clear  // Just clear row content
        else begin                              // Further down the grid
          for i := Index + 1 to RowCount - 1 do  // Copying while overwrite current row i
            Rows[i - 1] := Rows[i];
          RowCount := RowCount - 1;             // Only need to decrease RowCount now
        end;
        // Forces a reselect of the cell, for user-defined SelectCell event to fire.
        GridSelectCell(Grid, Grid.Col, Grid.Row - 1, lDummyCanSelect);  
            // Need dummy var param.
      end;
      Options := Options - [goEditing];
      Invalidate;
      CheckColumnWidths;
    end;
end;  // TBaseLegend.DeleteItem 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.EditItemProperties(Index: Integer);
begin
  if Assigned(Grid) then
    if Assigned(LegendItems[Index]) then
      LegendItems[Index].EditProperties;
end;  // TBaseLegend.EditItemProperties 

{-------------------------------------------------------------------------------
}
function TBaseLegend.GetCount: Integer;
begin
  Result := FItems.Count;
end;  // TBaseLegend.GetCount 

{-------------------------------------------------------------------------------
}
function TBaseLegend.GetLegendItems(Index: Integer): TBaseLegendItem;
begin
  if (Index > -1) and (Index < Count) then
    Result := TBaseLegendItem(FItems[Index])
  else
    Result := nil;
end;  // TBaseLegend.GetLegendItems 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.GridClick(Sender: TObject);
var
  lItem: TBaseLegendItem;
begin
  with Grid do begin
    lItem := LegendItems[Row];
    if Assigned(lItem) then begin
      // Turn off editing, regardless
      Options := Options - [goEditing];
  
      // Now figure out what should be done.
      if (Col = COL_TITLE) and lItem.CanRename then
        Options := Options + [goEditing]
      else
      if not FNavigatingGrid then
        ProcessItem(Col, Row, lItem);
    end;
  end;
  // Reset, don't want it to stay on.
  FNavigatingGrid := False;
  Change;
end;  // TBaseLegend.GridClick 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.GridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: 
    TGridDrawState);
var
  lItem: TBaseLegendItem;
  lLeft, lTop: Integer;
begin
  inherited;
  with Grid do begin
    lItem := LegendItems[ARow];
    if Assigned(lItem) then begin
      if (ACol = Col) and (ARow = Row) then begin
        Canvas.Font.Color := clHighlightText;
        Canvas.Brush.Color := clHighlight;
      end else begin
        Canvas.Font.Color := clWindowText;
        Canvas.Brush.Color := clWindow;
      end;
      Canvas.FillRect(Rect);
  
      case ACol of
        COL_CHECK :
            begin
              lLeft := Rect.Left + (Rect.Right - Rect.Left - 13) div 2;
              lTop  := Rect.Top  + (Rect.Bottom - Rect.Top - 13) div 2;
              DrawCheckBox(Canvas, lLeft, lTop,
                           lItem.Visible or lItem.AlwaysVisible, not lItem.AlwaysVisible);
            end;
        COL_SYMBOL: lItem.DrawSymbol(Canvas, Rect);
        COL_TITLE : DrawChoppedText(lItem.Title, Canvas, Rect, 2);
      end;
    end;
  end;
end;  // TBaseLegend.GridDrawCell 

{-------------------------------------------------------------------------------
  As VK_ESCAPE is not picked up in KeyDown, it is moved to its own handler. Also, VK_RETURN 
      doesn't seem to be picked up by the KeyUp event either. Don't really know what is going 
      on here, but this way works. Investigate further when time allows. 
}
procedure TBaseLegend.GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lItem: TBaseLegendItem;
begin
  inherited;
  FNavigatingGrid := False;
  with Grid do begin
    lItem := LegendItems[Row];
    if Assigned(lItem) then
      case Key of
        VK_RETURN:
            ProcessItem(Col, Row, lItem);
        VK_PRIOR, VK_NEXT, VK_END, VK_HOME, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN:
            FNavigatingGrid := True;
      end;
  end;
end;  // TBaseLegend.GridKeyDown 

{-------------------------------------------------------------------------------
  Ensure the inplace editor, when showing, allows only the allowed MAX_TITLE_LENGTH characters 
      to be entered. 
}
procedure TBaseLegend.GridKeyPress(Sender: TObject; var Key: Char);
var
  lItem: TBaseLegendItem;
  lEditor: TInplaceEdit;
begin
  with Grid do begin
    lItem := LegendItems[Row];
    if Assigned(lItem) then begin
      lEditor := TGridAccessor(Grid).InplaceEditor;
      if (Col = COL_TITLE) and Assigned(lEditor) then begin
        if SendMessage(lEditor.Handle, EM_GETLIMITTEXT, 0, 0) <> MAX_TITLE_LENGTH then
          SendMessage(lEditor.Handle, EM_LIMITTEXT, MAX_TITLE_LENGTH, 0);
      end;
    end;
  end;
end;  // TBaseLegend.GridKeyPress 

{-------------------------------------------------------------------------------
  Seems VK_ESCAPE is not picked up in KeyDown event, although it really should, as it does in 
      other places.... 
}
procedure TBaseLegend.GridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lItem: TBaseLegendItem;
begin
  with Grid do begin
    lItem := LegendItems[Row];
    if Assigned(lItem) then
      case Key of
        VK_F2:
            if Col = COL_TITLE then EditorMode := lItem.CanRename;
        VK_DELETE:
            if (Col <> COL_TITLE) or not EditorMode then
              DeleteItem(Row);
        VK_ESCAPE:
            // Although this would most likely happen only on the COL_TITLE column
            if Col = COL_TITLE then
              if Cells[COL_TITLE, Row] <> lItem.Title then
                Cells[COL_TITLE, Row] := lItem.Title
              else
              if EditorMode then EditorMode := False;
      end;
  end;
end;  // TBaseLegend.GridKeyUp 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  lCol, lRow: Integer;
begin
  with Grid do begin
    MouseToCell(X, Y, lCol, lRow);
    // lRow = -1 if mouse is not on a valid row.
    if (lRow >= 0) and Assigned(LegendItems[lRow]) and (lCol = COL_SYMBOL) then
      Cursor := crHandPoint
    else
      Cursor := crDefault;
  end;
end;  // TBaseLegend.GridMouseMove 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.GridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; 
    MousePos: TPoint; var Handled: Boolean);
begin
  FNavigatingGrid := True;
end;  // TBaseLegend.GridMouseWheel 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: 
    Boolean);
begin
  with Grid do
    if (Col = COL_TITLE) and EditorMode and ((Col <> ACol) or (Row <> ARow)) then
      if Trim(Cells[COL_TITLE, Row]) <> '' then  // Don't allow empty cells.
        LegendItems[Row].Title := Cells[COL_TITLE, Row];
  // If there was a previous handler, don't forget to call it.
  if Assigned(FGridSelectCell) then FGridSelectCell(Grid, ACol, ARow, CanSelect);
end;  // TBaseLegend.GridSelectCell 

{-------------------------------------------------------------------------------
  To prevent the click that occurs when you drag forcing the cell into edit mode, we reset the 'last clicked row' so that this click doesn't count as the second click on a row 
}
procedure TBaseLegend.GridStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  Grid.Options := Grid.Options - [goEditing];
  Grid.EditorMode := False;
end;  // TBaseLegend.GridStartDrag 

{-------------------------------------------------------------------------------
}
function TBaseLegend.IndexOf(AItem: TBaseLegendItem): Integer;
begin
  Result := Items.IndexOf(AItem);
end;  // TBaseLegend.IndexOf 

{-------------------------------------------------------------------------------
  Populate popup menu with default options, and reset the XPMenu to get the popup to properly 
      display. 
}
procedure TBaseLegend.InitialisePopupMenu;
begin
  FLegendDelete := AddPopupMenuItem(ResStr_PopupDelete, PopupDeleteClick);
  FLegendRename := AddPopupMenuItem(ResStr_PopupRename, PopupRenameClick);
  AddPopupMenuItem('-');
  FLegendProperties := AddPopupMenuItem(ResStr_PopupProperties, PopupPropertiesClick);
end;  // TBaseLegend.InitialisePopupMenu

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.PopupDeleteClick(Sender: TObject);
begin
  DeleteItem(Grid.Row);
end;  // TBaseLegend.PopupDeleteClick 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.PopupMenuPopup(Sender: TObject);
begin
  UpdateMenuState(LegendItems[Grid.Row]);
end;  // TBaseLegend.PopupMenuPopup 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.PopupPropertiesClick(Sender: TObject);
begin
  EditItemProperties(Grid.Row);
end;  // TBaseLegend.PopupPropertiesClick 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.PopupRenameClick(Sender: TObject);
begin
  RenameItem(Grid.Row);
end;  // TBaseLegend.PopupRenameClick 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.ProcessItem(ACol, ARow: Integer; AItem: TBaseLegendItem);
begin
  case ACol of
    COL_CHECK : if not AItem.AlwaysVisible then AItem.Visible := not AItem.Visible;
    COL_SYMBOL: AItem.EditProperties;
    COL_TITLE : AItem.Title := Grid.Cells[ACol, ARow];
  end;
end;  // TBaseLegend.ProcessItem 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.Refresh;
begin
  Clear;
  RefreshData;
end;  // TBaseLegend.Refresh 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.RefreshData;
begin
  // Nothing in Base class. Override when needed.
end;  // TBaseLegend.RefreshData 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.RefreshItem(AItem: TBaseLegendItem);
var
  i: Integer;
begin
  if Assigned(Grid) then
    with Grid do begin
      // Loop through to refresh the Title column. The rest is only drawn.
      for i := 0 to RowCount - 1 do
        if LegendItems[i] = AItem then
          Cells[COL_TITLE, i] := AItem.Title;
      Invalidate;
    end;
end;  // TBaseLegend.RefreshItem 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.RenameItem(Index: Integer);
begin
  if Assigned(Grid) then
    if Assigned(LegendItems[Index]) then
      if LegendItems[Index].CanRename then begin
        Grid.Col := COL_TITLE;
        Grid.EditorMode := True;
      end;
end;  // TBaseLegend.RenameItem 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.SetGrid(Value: TStringGrid);
begin
  if Assigned(FGrid) and (Value = nil) then begin
    // Disengage all events and clear the grid.
    with FGrid do begin
      RowCount := 1;
      Rows[0].Clear;
      // Restore previous handlers.
      OnClick      := nil;
      OnDrawCell   := nil;
      OnKeyDown    := nil;
      OnKeyPress   := nil;
      OnKeyUp      := nil;
      OnMouseMove  := nil;
      OnSelectCell := FGridSelectCell;
      OnStartDrag  := nil;
      PopupMenu    := nil;
    end;
    TGridAccessor(FGrid).OnMouseWheel := nil;
    // And remove the reference.
    FGrid := nil;
  end;
  
  // Setup grid properties, if necessary.
  if Assigned(Value) then begin
    FGrid := Value;
    with FGrid do begin
      // Save previous handlers, is any
      FGridSelectCell := OnSelectCell;
      // Setup own handlers
      OnClick      := GridClick;
      OnDrawCell   := GridDrawCell;
      OnKeyDown    := GridKeyDown;
      OnKeyPress   := GridKeyPress;
      OnKeyUp      := GridKeyUp;
      OnMouseMove  := GridMouseMove;
      OnSelectCell := GridSelectCell;
      OnStartDrag  := GridStartDrag;
      PopupMenu    := FPopupMenu;
    end;
    TGridAccessor(FGrid).OnMouseWheel := GridMouseWheel;
    // Populate with existing content, if any.
    SynchroniseGrid;
    CheckColumnWidths;
  end;
end;  // TBaseLegend.SetGrid 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.SynchroniseGrid;
var
  i: Integer;
begin
  if Assigned(Grid) then
    with Grid do begin
      RowCount := Count;
      for i := 0 to FItems.Count - 1 do begin
        Objects[COL_OBJECT, i] := FItems[i];
        Cells[COL_TITLE, i] := TBaseLegendItem(FItems[i]).Title;
      end;
      Invalidate;
    end;
end;  // TBaseLegend.SynchroniseGrid 

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.UpdateMenuState(AItem: TBaseLegendItem);
var
  i: Integer;
  item: TMenuItem;
begin
  for i := 0 to FPopupMenu.Items.Count - 1 do begin
    item := FPopupMenu.Items[i];
    item.Enabled := Assigned(AItem);
    if Assigned(AItem) then begin
      if item = FLegendDelete then FLegendDelete.Enabled := AItem.CanDelete else
      if item = FLegendRename then FLegendRename.Enabled := AItem.CanRename else
      if item = FLegendProperties then FLegendProperties.Enabled := AItem.CanEdit
      else
        UpdateMenuItemState(item);
    end;
  end;
end;  // TBaseLegend.UpdateMenuState

{-------------------------------------------------------------------------------
}
procedure TBaseLegend.UpdateMenuItemState(AMenuItem: TMenuItem);
begin
  if Assigned(FOnUpdatePopupItemState) then
    FOnUpdatePopupItemState(AMenuItem);
end; // TBaseLegend.UpdateMenuItemState

{-==============================================================================
    TBaseLegendItem
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBaseLegendItem.Create;
begin
  inherited Create;
  FVisible := True;
end;  // TBaseLegendItem.Create 

{-------------------------------------------------------------------------------
}
procedure TBaseLegendItem.Assign(ASource: TBaseLegendItem);
begin
  FEditingProperties := True;
  DoAssign(ASource);
  if Assigned(Legend) then Legend.RefreshItem(Self);
  if Assigned(OnNeedRefresh) and not Initializing then OnNeedRefresh(Self);
  ResetFlags;
  FEditingProperties := False;
end;  // TBaseLegendItem.Assign 

{-------------------------------------------------------------------------------
}
procedure TBaseLegendItem.DoAssign(ASource: TBaseLegendItem);
begin
  Visible := ASource.Visible;
  Title   := ASource.Title;
end;  // TBaseLegendItem.DoAssign 

{-------------------------------------------------------------------------------
}
procedure TBaseLegendItem.DoSetTitle(const Value: String);
begin
  FTitle := Copy(Value, 0, MAX_TITLE_LENGTH);
end;  // TBaseLegendItem.DoSetTitle 

{-------------------------------------------------------------------------------
}
procedure TBaseLegendItem.DoSetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;  // TBaseLegendItem.DoSetVisible 

{-------------------------------------------------------------------------------
}
procedure TBaseLegendItem.EditProperties;
begin
  FEditingProperties := True;
  if DoEditProperties then begin
    if Assigned(Legend) then Legend.RefreshItem(Self);
    if Assigned(OnNeedRefresh) and not Initializing then OnNeedRefresh(Self);
  end;
  ResetFlags;
  FEditingProperties := False;
end;  // TBaseLegendItem.EditProperties 

{-------------------------------------------------------------------------------
}
function TBaseLegendItem.GetAlwaysVisible: Boolean;
begin
  Result := False;
end;  // TBaseLegendItem.GetAlwaysVisible 

{-------------------------------------------------------------------------------
}
function TBaseLegendItem.GetCanDelete: Boolean;
begin
  Result := True;
end;  // TBaseLegendItem.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TBaseLegendItem.GetCanEdit: Boolean;
begin
  Result := True;
end;  // TBaseLegendItem.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TBaseLegendItem.GetCanRename: Boolean;
begin
  Result := not (AppSettings.UserAccessLevel in [ualReadOnly, ualRecorder, ualAddOnly]);
end;  // TBaseLegendItem.GetCanRename 

{-------------------------------------------------------------------------------
}
procedure TBaseLegendItem.ResetFlags;
begin
  FTitleChanged := False;
  FVisibilityChanged := False;
end;  // TBaseLegendItem.ResetFlags 

{-------------------------------------------------------------------------------
}
procedure TBaseLegendItem.SetTitle(const Value: String);
begin
  if FTitle <> Value then begin
    DoSetTitle(Value);
    FTitleChanged := True;
    if not EditingProperties then begin
      if Assigned(Legend) then Legend.RefreshItem(Self);
      if Assigned(OnNeedRefresh) and not Initializing then OnNeedRefresh(Self);
    end;
  end;
  if not EditingProperties then ResetFlags;
end;  // TBaseLegendItem.SetTitle 

{-------------------------------------------------------------------------------
}
procedure TBaseLegendItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then begin
    DoSetVisible(Value);
    FVisibilityChanged := True;
    if not EditingProperties then begin
      if Assigned(Legend) then Legend.RefreshItem(Self);
      if Assigned(OnNeedRefresh) and not Initializing  then OnNeedRefresh(Self);
    end;
  end;
  if not EditingProperties then ResetFlags;
end;  // TBaseLegendItem.SetVisible 

end.
