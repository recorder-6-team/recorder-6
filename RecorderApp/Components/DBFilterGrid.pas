{===============================================================================

  Unit:         DBFilterGrid

  Defines:      TDBFilterGrid

  Description:  Data-bound grid control that allows data to be filtered by
                column.

  Created:      December 2009

  Last revision information:
    $Revision: 13 $
    $Date: 3/03/10 11:48 $
    $Author: Andrewkemp $
    
===============================================================================}
unit DBFilterGrid;

interface

uses
  Types, Classes, Graphics, Controls, StdCtrls, ExtCtrls, Grids, Menus, DB,
  JNCCGrid;

const
  DEFAULT_FILTER_COLOR = $00BCF9D3; // sickly green

type
  {-----------------------------------------------------------------------------
    The type of event handlers that filter operators from the complete list
    for a particular column.
  }
  TFilterOperatorEvent = procedure(Sender: TObject; Col: Integer;
      const Operator: String; var Accept: Boolean) of object;
  
  {-----------------------------------------------------------------------------
    Data-bound grid control that allows data to be filtered by column.
  }
  TDBFilterGrid = class(TDBJNCCGrid)
  private
    FAllowFiltering: Boolean;
    FFilterImage: TBitmap;
    FFilterColor: TColor;
    FOperatorMenu: TPopupMenu;
    FFilterOperators: TStrings;
    FUnaryOperators: TStrings;
    FFilterControls: TList;
    FFilterChanged: Boolean;
    FApplyingFilter: Boolean;
    FOnFilterChange: TNotifyEvent;
    FOnFilterOperator: TFilterOperatorEvent;
    procedure OperatorButtonMouseDown(Sender: TObject;
        Button: TMouseButton;
        Shift: TShiftState;
        X, Y: Integer);
    procedure OperatorMenuItemClick(Sender: TObject);
    procedure FilterEditKeyDown(Sender: TObject;
        var Key: Word;
        Shift: TShiftState);
    procedure FilterEditChange(Sender: TObject);
    procedure FilterControlsExit(Sender: TObject);
  protected
    procedure LinkActive(Value: Boolean); override;
    procedure SetColumnAttributes; override;
    procedure LayoutChanged; override;
    procedure ColWidthsChanged; override;
    procedure RowHeightsChanged; override;
    procedure TopLeftChanged; override;
    procedure DrawCell(ACol, ARow: Longint;
        ARect: TRect; AState:
        TGridDrawState); override;
    function FilterControlsCreated: Boolean; virtual;
    procedure CreateFilterControls; virtual;
    procedure CreateFilterControlsForColumn; virtual;
    function GetFilterImage: TBitmap; virtual;
    function GetFilterControls(Index: Integer): TPanel; virtual;
    function GetOperatorButton(Index: Integer): TButton; virtual;
    function GetColumnFromOperatorButton(Button: TButton): Integer; virtual;
    function GetEditControl(Index: Integer): TEdit; virtual;
    function GetFilterOperator(Index: Integer): String; virtual;
    function GetFilterText(Index: Integer): String; virtual;
    procedure SetFilterText(Index: Integer; const Value: String); virtual;
    function GetColumnFiltered(Index: Integer): Boolean; virtual;
    procedure SetAllowFiltering(Value: Boolean); virtual;
    procedure SetFilterColor(Value: TColor); virtual;
    procedure SetFilterOperators(Value: TStrings); virtual;
    procedure SetUnaryOperators(Value: TStrings); virtual;
    function IsOperatorVisible(Column, Index: Integer): Boolean; virtual;
    procedure UpdateOperatorMenu(Column: Integer); virtual;
    function IsUnaryOperator(const Operator: String): Boolean; virtual;
    procedure SelectOperator(Column, Index: Integer); virtual;
    procedure UpdateHeaderRow; virtual;
    procedure PositionFilterControls; virtual;
    procedure HideFilterControls; virtual;
    procedure DisplayFilterControlsInColumn(Index, ALeft, AWidth: Integer);
        virtual;
    procedure ApplyFilter; virtual;
    procedure DoFilterChange; virtual;
    property FilterImage: TBitmap read GetFilterImage;
    property FilterControls[Index: Integer]: TPanel read GetFilterControls;
    property OperatorButton[Index: Integer]: TButton read GetOperatorButton;
    property EditControl[Index: Integer]: TEdit read GetEditControl;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FilterOperator[Index: Integer]: String read GetFilterOperator;
    property FilterText[Index: Integer]: String
        read GetFilterText write SetFilterText;
    property ColumnFiltered[Index: Integer]: Boolean read GetColumnFiltered;
  published
    property AllowFiltering: Boolean
        read FAllowFiltering write SetAllowFiltering default True;
    property FilterColor: TColor
        read FFilterColor write SetFilterColor default DEFAULT_FILTER_COLOR;
    property FilterOperators: TStrings
        read FFilterOperators write SetFilterOperators;
    property UnaryOperators: TStrings
        read FUnaryOperators write SetUnaryOperators;
    property OnFilterChanged: TNotifyEvent
        read FOnFilterChange write FOnFilterChange;
    property OnFilterOperator: TFilterOperatorEvent
        read FOnFilterOperator write FOnFilterOperator;
  end;

  // a hack to gain access to protected properties
  THackDataset = class(TDataset);

implementation

{$R *.res}

uses
  SysUtils, StrUtils, DBGrids, Windows, GeneralFunctions;

const
  INDICATOR_WIDTH = 16;
  FILTER_IMAGE_RESOURCE = 'BMP_FILTER';
  BUTTON_WIDTH = 24;
  BUTTON_FONT = 'Microsoft Sans Serif';

type
  {-----------------------------------------------------------------------------
    List of name-value pairs defining the search operators that appear in the
    pop-up menu.
  }
  TDBFilterGridOperatorStrings = class(TStrings)
  private
    FMenu: TPopupMenu;
    FStrings: TStrings;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
  public
    constructor Create(Menu: TPopupMenu);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;


{-------------------------------------------------------------------------------
  Creates and initializes an instance of TDBFilterGrid.
}
constructor TDBFilterGrid.Create(AOwner: TComponent);
begin
  inherited;
  FAllowFiltering := True;
  FFilterColor := DEFAULT_FILTER_COLOR;
  FOperatorMenu := TPopupMenu.Create(Self);
  FFilterOperators := TDBFilterGridOperatorStrings.Create(FOperatorMenu);
  FUnaryOperators := TStringList.Create;
  TStringList(FUnaryOperators).Sorted := True;
  FFilterControls := TList.Create;
end;

{-------------------------------------------------------------------------------
  Destroys an instance of TDBFilterGrid.
}
destructor TDBFilterGrid.Destroy;
begin
  FreeAndNil(FFilterImage);
  FreeAndNil(FFilterControls);
  FreeAndNil(FUnaryOperators);
  FreeAndNil(FFilterOperators);
  inherited;
end;

{-------------------------------------------------------------------------------
  If an existing dataset is being deactivated, disposes of the corresponding
  filter controls.
}
procedure TDBFilterGrid.LinkActive(Value: Boolean);
var
  I: Integer;
begin
  inherited;
  if not Value and FilterControlsCreated then
  begin
    for I := 0 to FFilterControls.Count - 1 do
    begin
      TControl(FFilterControls[I]).Free;
    end;
    FFilterControls.Clear;
  end;
end;

{-------------------------------------------------------------------------------
  Sets the column widths.
}
procedure TDBFilterGrid.SetColumnAttributes;
begin
  inherited;
  if (dgIndicator in Options) then
    ColWidths[0] := INDICATOR_WIDTH;
end;

{-------------------------------------------------------------------------------
  Updates the grid to reflect changes in the Columns property or the dataset.
}
procedure TDBFilterGrid.LayoutChanged;
begin
  inherited;
  if Assigned(DataSource) and not FilterControlsCreated then
  begin
    if DataSource.State <> dsInactive then CreateFilterControls;
  end;
end;

{-------------------------------------------------------------------------------
  Updates the positions of the filter controls when the widths of the columns
  in the grid change.
}
procedure TDBFilterGrid.ColWidthsChanged;
begin
  inherited ColWidthsChanged;
  if AllowFiltering and FilterControlsCreated then PositionFilterControls;
end;

{-------------------------------------------------------------------------------
  Responds when the row heights changed, maintaining the space allotted in the
  first row of the grid for the filter controls.
}
procedure TDBFilterGrid.RowHeightsChanged;
begin
  inherited;
  if AllowFiltering and FilterControlsCreated then UpdateHeaderRow;
end;

{-------------------------------------------------------------------------------
  Updates the positions of the filter controls when the grid scrolls.
}
procedure TDBFilterGrid.TopLeftChanged;
begin
  inherited;
  if AllowFiltering and FilterControlsCreated then PositionFilterControls;
end;

{-------------------------------------------------------------------------------
  Draws the cell specified by the ACol and ARow parameters.
}
procedure TDBFilterGrid.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);

  {-----------------------------------------------------------------------------
    Draws the filter image in the first column of the header row.
  }
  procedure DrawFilterImage;
  var
    ImageRect: TRect;
  begin
    ARect.Bottom := ARect.Top + DefaultRowHeight;
    InflateRect(ARect, GridLineWidth, GridLineWidth);
    with Canvas do
    begin
      Font.Color  := clWindowText;
      Font.Style  := Font.Style + [fsBold];
      Brush.Color := clBtnFace;
      ImageRect.Top := ARect.Bottom - GridLineWidth;
      ImageRect.Bottom := ImageRect.Top + GridLineWidth
          + RowHeights[0] - DefaultRowHeight;
      ImageRect.Left := ARect.Left;
      ImageRect.Right := ARect.Right;
      Brush.Color := FilterColor;
      Rectangle(ImageRect);
      StretchDraw(ImageRect, FilterImage);
    end;
  end;

  {-----------------------------------------------------------------------------
    Draws the title part of a column header cell, leaving space for the filter
    controls below it.
  }
  procedure DrawTitleCell;
  begin
    ARect.Bottom := ARect.Top + DefaultRowHeight;
    InflateRect(ARect, GridLineWidth, GridLineWidth);
    with Canvas do
    begin
      Font.Color  := clWindowText;
      Font.Style  := Font.Style + [fsBold];
      Brush.Color := clBtnFace;
      Rectangle(ARect);
      DrawChoppedText(
          Columns[ACol - 1].Title.Caption,
          Canvas,
          ARect,
          2 * GridLineWidth);
    end;
  end;

begin
  if (not AllowFiltering) or (ARow > 0) then
    inherited
  else if ACol < IndicatorOffset then
    DrawFilterImage
  else
    DrawTitleCell;
end;

{-------------------------------------------------------------------------------
  Have the filter controls been created yet?
}
function TDBFilterGrid.FilterControlsCreated: Boolean;
begin
  Result := FFilterControls.Count > 0;
end;

{-------------------------------------------------------------------------------
  Creates the filter controls have been created for each column.
}
procedure TDBFilterGrid.CreateFilterControls;
var
  lCol: Integer;

  {-----------------------------------------------------------------------------
    Selects the first visible operator in the current column.
  }
  procedure SelectFirstVisibleOperator;
  var
    I: Integer;
  begin
    for I := 0 to FilterOperators.Count - 1 do
      if IsOperatorVisible(lCol, I) then
      begin
        SelectOperator(lCol, I);
        Break;
      end;
  end;

begin
  for lCol := 0 to Columns.Count - 1 do
    CreateFilterControlsForColumn;

  if FFilterOperators.Count > 0 then
  begin
    for lCol := 0 to Columns.Count - 1 do SelectFirstVisibleOperator;
  end;

  if AllowFiltering then
  begin
    UpdateHeaderRow;
    PositionFilterControls;
  end;
end;

{-------------------------------------------------------------------------------
  Creates the filter controls for the next column.
}
procedure TDBFilterGrid.CreateFilterControlsForColumn;
var
  lPanel: TPanel;
  lButtonPanel: TPanel;
  lEdit: TEdit;
  lButton: TButton;
begin
  lEdit := TEdit.Create(Self);

  lPanel := TPanel.Create(Self);
  lPanel.Parent := Self;
  lPanel.Visible := False;
  lPanel.Top := DefaultRowHeight;
  lPanel.Height := lEdit.Height;
  lPanel.Ctl3D := False;
  lPanel.BevelOuter := bvNone;
  lPanel.OnExit := FilterControlsExit;
  FFilterControls.Add(lPanel);

  lButtonPanel := TPanel.Create(Self);
  lButtonPanel.Parent := lPanel;
  lButtonPanel.Align := alLeft;
  lButtonPanel.BevelOuter := bvNone;
  lButtonPanel.Width := BUTTON_WIDTH;
  lButtonPanel.Color := clWindowText; // fake the grid line above + below button

  lButton := TButton.Create(Self);
  lButton.Parent := lButtonPanel;
  lButton.Width := BUTTON_WIDTH;
  lButton.Top := GridLineWidth;
  lButton.Height := lEdit.Height - 2 * GridLineWidth;
  lButton.Font.Name := BUTTON_FONT;
  lButton.OnMouseDown := OperatorButtonMouseDown;

  lEdit.Color := FilterColor;
  lEdit.Parent := lPanel;
  lEdit.Align := alClient;
  lEdit.OnKeyDown := FilterEditKeyDown;
  lEdit.OnChange := FilterEditChange;
end;

{-------------------------------------------------------------------------------
  Gets the image that is displayed in the first column of the filter row.
}
function TDBFilterGrid.GetFilterImage: Graphics.TBitmap;
begin
  if not Assigned(FFilterImage) then
  begin
    FFilterImage := Graphics.TBitmap.Create;
    FFilterImage.LoadFromResourceName(HInstance, FILTER_IMAGE_RESOURCE);
    FFilterImage.Transparent := True;
  end;
  Result := FFilterImage;
end;

{-------------------------------------------------------------------------------
  Gets the panel containing the filter controls for the specified column.
}
function TDBFilterGrid.GetFilterControls(Index: Integer): TPanel;
begin
  Result := TPanel(FFilterControls[Index]);
end;

{-------------------------------------------------------------------------------
  Gets the operator button for the specified column.
}
function TDBFilterGrid.GetOperatorButton(Index: Integer): TButton;
var
  lParent: TPanel;
begin
  lParent := TPanel(FilterControls[Index].Controls[0]);
  Result := TButton(lParent.Controls[0]);
end;

{-------------------------------------------------------------------------------
  Gets the column associated with the given operator button.
}
function TDBFilterGrid.GetColumnFromOperatorButton(Button: TButton): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Columns.Count - 1 do
    if OperatorButton[I] = Button then
    begin
      Result := I;
      Break;
    end;
end;

{-------------------------------------------------------------------------------
  Gets the edit control for the specified column.
}
function TDBFilterGrid.GetEditControl(Index: Integer): TEdit;
begin
  Result := TEdit(FilterControls[Index].Controls[1]);
end;

{-------------------------------------------------------------------------------
  Gets the filter operator that has been selected for the specified column.
}
function TDBFilterGrid.GetFilterOperator(Index: Integer): String;
begin
  Result := FilterOperators.Values[OperatorButton[Index].Caption];
end;

{-------------------------------------------------------------------------------
  Gets the filter text that has been entered for the specified column.
}
function TDBFilterGrid.GetFilterText(Index: Integer): String;
begin
  Result := EditControl[Index].Text;
end;

{-------------------------------------------------------------------------------
  Sets the filter text for the specified column.
}
procedure TDBFilterGrid.SetFilterText(Index: Integer; const Value: String);
begin
  EditControl[Index].Text := Value;
end;

{-------------------------------------------------------------------------------
  Has a filter been applied to the specified column?
}
function TDBFilterGrid.GetColumnFiltered(Index: Integer): Boolean;
begin
  Result := AllowFiltering
      and (IsUnaryOperator(FilterOperator[Index]) or (FilterText[Index] <> ''));
end;

{-------------------------------------------------------------------------------
  Is filtering permitted in this grid?
}
procedure TDBFilterGrid.SetAllowFiltering(Value: Boolean);
begin
  if Value <> FAllowFiltering then
  begin
    FAllowFiltering := Value;
    if Value then
    begin
      UpdateHeaderRow;
      PositionFilterControls;
    end
    else
    begin
      RowHeights[0] := DefaultRowHeight;
      HideFilterControls;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Updates the background colour of the filter fields.
}
procedure TDBFilterGrid.SetFilterColor(Value: TColor);
var
  I: Integer;
begin
  if FilterColor <> Value then
  begin
    FFilterColor := Value;
    for I := 0 to Columns.Count - 1 do EditControl[I].Color := Value;
  end;
end;

{-------------------------------------------------------------------------------
  Sets the list of filter operators (name/value pairs).
}
procedure TDBFilterGrid.SetFilterOperators(Value: TStrings);
begin
  FilterOperators.Assign(Value);
end;

{-------------------------------------------------------------------------------
  Sets the list of unary operators (values in FilterOperators that do not
  take an additional operand).
}
procedure TDBFilterGrid.SetUnaryOperators(Value: TStrings);
begin
  UnaryOperators.Assign(Value);
end;

{-------------------------------------------------------------------------------
  Is the specified operator visible in the specified column?
}
function TDBFilterGrid.IsOperatorVisible(Column, Index: Integer): Boolean;
var
  lOperator: String;
begin
  Result := True;
  lOperator := FilterOperators.ValueFromIndex[Index];
  if Assigned(FOnFilterOperator) then
    FOnFilterOperator(Self, Column, lOperator, Result);
end;

{-------------------------------------------------------------------------------
  Updates the operator menu for the specified column.
}
procedure TDBFilterGrid.UpdateOperatorMenu(Column: Integer);
var
  I: Integer;
begin
  for I := 0 to FilterOperators.Count - 1 do
    FOperatorMenu.Items[I].Visible := IsOperatorVisible(Column, I);
end;

{-------------------------------------------------------------------------------
  Is the given operator a member of UnaryOperators?
}
function TDBFilterGrid.IsUnaryOperator(const Operator: String): Boolean;
begin
  Result := UnaryOperators.IndexOf(Operator) <> -1;
end;

{-------------------------------------------------------------------------------
  Selects the specified operator in the specified column.
}
procedure TDBFilterGrid.SelectOperator(Column, Index: Integer);
var
  lButton: TButton;
  lOperatorName: String;
  lWasUnary: Boolean;
  lIsUnary: Boolean;
begin
  lButton := OperatorButton[Column];
  lOperatorName := FilterOperators.Names[Index];

  if lOperatorName <> lButton.Caption then
  begin
    lWasUnary := IsUnaryOperator(FilterOperator[Column]);
    lIsUnary := IsUnaryOperator(FilterOperators.ValueFromIndex[Index]);
    lButton.Caption := lOperatorName;

    with EditControl[Column] do
    begin
      Enabled := not lIsUnary;
      if Enabled then
        Color := FilterColor
      else
      begin
        Text := '';
        Color := clBtnFace;
      end;
    end;

    if lIsUnary or lWasUnary or (FilterText[Column] <> '') then
      FFilterChanged := True;
  end;
end;

{-------------------------------------------------------------------------------
  Updates the height of the header row to allow for the filter controls.
}
procedure TDBFilterGrid.UpdateHeaderRow;
begin
  RowHeights[0] := DefaultRowHeight + EditControl[0].Height - GridLineWidth;
end;

{-------------------------------------------------------------------------------
  Updates the positions of the filter controls.
}
procedure TDBFilterGrid.PositionFilterControls;
var
  I: Integer;
  lLeft: Integer;
begin
  if FApplyingFilter then Exit;

  for I := IndicatorOffset to LeftCol - 1 do
    FilterControls[I - IndicatorOffset].Visible := False;

  if dgIndicator in Options then
    lLeft := INDICATOR_WIDTH
  else
    lLeft := 0;

  for I := LeftCol to ColCount - 1 do
  begin
    DisplayFilterControlsInColumn(I - IndicatorOffset, lLeft, ColWidths[I]);
    Inc(lLeft, ColWidths[I] + GridLineWidth);
  end;
end;

{-------------------------------------------------------------------------------
  Hides the filter controls.
}
procedure TDBFilterGrid.HideFilterControls;
var
  I: Integer;
begin
  for I := 0 to Columns.Count - 1 do FilterControls[I].Visible := False;
end;

{-------------------------------------------------------------------------------
  Positions and makes visible the filter controls in the specified column,
  given the location of its left-hand edge and its width.
}
procedure TDBFilterGrid.DisplayFilterControlsInColumn(
  Index, ALeft, AWidth: Integer);
var
  lPanel: TPanel;
begin
  lPanel := FilterControls[Index];
  lPanel.Visible := false;
  lPanel.Left := ALeft + GridLineWidth;
  lPanel.Width := AWidth + GridLineWidth;
  lPanel.Visible := True;
end;

{-------------------------------------------------------------------------------
  Raises the FilterChanged event and clears the change flag.
}
procedure TDBFilterGrid.ApplyFilter;
begin
  FFilterChanged := False;
  FApplyingFilter := True;
  try
    DoFilterChange;
  finally
    FApplyingFilter := False;
  end;
  PositionFilterControls;
end;

{-------------------------------------------------------------------------------
  Filter change event dispatcher.
}
procedure TDBFilterGrid.DoFilterChange;
begin
  if Assigned(FOnFilterChange) then FOnFilterChange(Self);
end;

{-------------------------------------------------------------------------------
  Responds to a click on an operator button by opening the operator menu
  beneath the button.
}
procedure TDBFilterGrid.OperatorButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lButton: TButton;
  lWhere: TPoint;
begin
  lButton := TButton(Sender);
  UpdateOperatorMenu(GetColumnFromOperatorButton(lButton));
  lWhere := lButton.Parent.ClientToScreen(Point(
      lButton.Left,
      lButton.BoundsRect.Bottom));
  FOperatorMenu.PopupComponent := lButton;
  FOperatorMenu.Popup(lWhere.X, lWhere.Y);
end;

{-------------------------------------------------------------------------------
  Responds to the user changing the selected operator for a column by updating
  the button and flagging the change for action when the focus leaves the
  filter controls.
}
procedure TDBFilterGrid.OperatorMenuItemClick(Sender: TObject);
var
  lColumn: Integer;
  lOldOperator: String;
  lNewOperator: String;

  function GetItemIndex: Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to FOperatorMenu.Items.Count - 1 do
      if FOperatorMenu.Items[I] = Sender then
      begin
        Result := I;
        Break;
      end;
  end;

begin
  lColumn := GetColumnFromOperatorButton(TButton(FOperatorMenu.PopupComponent));
  lOldOperator := FilterOperator[lColumn];
  SelectOperator(lColumn, GetItemIndex);
  lNewOperator := FilterOperator[lColumn];

  if not IsUnaryOperator(lNewOperator) then
    EditControl[lColumn].SetFocus
  else if lNewOperator <> lOldOperator then
    ApplyFilter;
end;

{-------------------------------------------------------------------------------
  Responds to the user pressing the Enter key in a filter text control by
  applying the current filter.
}
procedure TDBFilterGrid.FilterEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then ApplyFilter;
end;

{-------------------------------------------------------------------------------
  Responds to a change in the filter text for the current column by flagging
  the change for action when the focus leaves the filter controls. 
}
procedure TDBFilterGrid.FilterEditChange(Sender: TObject);
begin
  FFilterChanged := True;
end;

{-------------------------------------------------------------------------------
  Responds to the focus leaving the filter controls for a column by reapplying
  the filter if it has changed.
}
procedure TDBFilterGrid.FilterControlsExit(Sender: TObject);
begin
  if FFilterChanged then ApplyFilter;
end;

{-------------------------------------------------------------------------------
  Creates and initializes an instance of TDBFilterGridOperatorStrings.
}
constructor TDBFilterGridOperatorStrings.Create(Menu: TPopupMenu);
begin
  inherited Create;
  NameValueSeparator := ':';
  FMenu := Menu;
  FStrings := TStringList.Create;
end;

{-------------------------------------------------------------------------------
  Destroys an instance of TDBFilterGridOperatorStrings.
}
destructor TDBFilterGridOperatorStrings.Destroy;
begin
  FreeAndNil(FStrings);
  inherited;
end;

{-------------------------------------------------------------------------------
  Gets the name-value pair at the specified index.
}
function TDBFilterGridOperatorStrings.Get(Index: Integer): string;
begin
  Result := FStrings[Index]
end;

{-------------------------------------------------------------------------------
  Gets the number of operators.
}
function TDBFilterGridOperatorStrings.GetCount: Integer;
begin
  Result := FStrings.Count;
end;

{-------------------------------------------------------------------------------
  Clears the list of operators.
}
procedure TDBFilterGridOperatorStrings.Clear;
begin
  FStrings.Clear;
  FMenu.Items.Clear;
end;

{-------------------------------------------------------------------------------
  Deletes the operator at the specified index.
}
procedure TDBFilterGridOperatorStrings.Delete(Index: Integer);
begin
  FStrings.Delete(Index);
  FMenu.Items[Index].Free;
end;

{-------------------------------------------------------------------------------
  Inserts the given name-value pair at the specified index.
}
procedure TDBFilterGridOperatorStrings.Insert(Index: Integer;
  const S: string);
var
  lGrid: TDBFilterGrid;
  lItem: TMenuItem;
  lCaption: String;
begin
  FStrings.Insert(Index, S);

  lCaption := Names[Index];
  if lCaption <> ValueFromIndex[Index] then
    lCaption := Format('%0:s (%1:s)', [lCaption, ValueFromIndex[Index]]);

  lGrid := TDBFilterGrid(FMenu.Owner);
  lItem := TMenuItem.Create(lGrid);
  lItem.OnClick := lGrid.OperatorMenuItemClick;
  lItem.Caption := lCaption;
  FMenu.Items.Insert(Index, lItem);
end;

(**
 * When a column moves, also move the filter controls to match.
 *)
procedure TDBFilterGrid.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  inherited;
  // -1 from indexes because filtercols does not include the indicator column
  FFilterControls.Move(FromIndex-1, ToIndex-1);
  // note that filteroperators and filtertext are derived from the control so there
  // is no need to move them specifically.
  PositionFilterControls;
  // There seems to be a bug in ADODB that causes it to fail to detect column position
  // changes after checksumming the field list. This results in the record buffer not
  // getting reloaded, so things get out of step (see Mantis 146). This hack
  // forces the record buffer to reload.
  THackDataset(Datasource.Dataset).Reserved := nil;
end;

end.
