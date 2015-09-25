//==============================================================================
//  Unit:        DataStringGrid
//
//  Implements:  TDataStringGrid,
//                 TRowKey, TUpdateRowEvent, TDeleteRowEvent
//
//  Description: Class that can be linked to a query and a string grid, to
//               provide a pseudo data aware grid.
//               Editing is handled to make the grid feel like an Access grid.
//               Use early bound field components attached to a query to setup
//               each column.  Use LookupDatasets to specify which columns are
//               represented as combo boxes.
//               Read only columns are shaded gray.
//               Use a small calculated column called Indicator if you want an
//               indicator.
//               NB/ To get this working with linked edit buttons in the grid,
//               always use a ControlStringGrid.
//
//               INTERNATIONALISED
//
//  Author:      John van Breda
//  Created:     25 February 2000
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 42 $
//    $Date: 17/02/09 12:12 $
//    $Author: Pauldavies $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit DataStringGrid;

interface

uses
  SysUtils, Classes, Grids, JNCCDatasets, StdCtrls, Controls, Windows, Forms,
  Dataclasses, ExceptionForm, ImageListButton, ImgList, Contnrs,
  ControlStringGrid, Messages;

resourcestring
  ResStr_NoAccessToDeleteRow = 'You do not have access rights to delete this row.';
  ResStr_NoContentCannotSave = 'The row has no content so cannot be saved';
  ResStr_PleaseEnterAValueFor = 'Please enter a valid value for the %s.';

type
  EDataStringGridError = class(TExceptionPath);

  TAccessorClass = Class(TStringGrid);

  // record to remember state of current cell, when we need to wind back }
  TCurrentCellState = record
    Text : string;
    SelStart : integer;
    SelLength : integer;
  end;


  { If implementing this event, must change ioRowKey to new key used for inserts }
  TUpdateRowEvent = procedure(var ioRowKey : string; iData : TStringList) of object;

  TDeleteRowEvent = procedure(iRowKey : string) of object;

  TValidateCellEvent = procedure( var ioAccept : boolean; ACol, ARow : integer ) of object;

  TNewBlankRowEvent = procedure(Sender: TObject; ARow: integer) of object;

  { Class to store key information about a row on the grid }
  TRowKey = class
    FKey : string;
    FEdited : boolean;
  private
    FLocked: boolean;
    FCanDelete: boolean;
    procedure SetEdited(const Value: boolean);
    procedure SetKey(const Value: string);
    procedure SetLocked(const Value: boolean);
    procedure SetCanDelete(const Value: boolean);
  public
    constructor Create( iKey : string );
    property Key : string read FKey write SetKey;
    property Edited : boolean read FEdited write SetEdited;
    property Locked : boolean read FLocked write SetLocked;
    property CanDelete : boolean read FCanDelete write SetCanDelete;
  end;

  // Class to hold a key behind a linked edit cell
  TLinkedKey = class
  private
    FKey: string;
  public
    property Key: string read FKey write FKey;
  end;

  

  // Methods to hook linked edit controls
  TFindDataMethod = procedure(const AInitialText: string; var AText, AKey: string) of object;
  TGetDataMethod = procedure of object;

  // Class to store information about a linked edit column
  TLinkedEditCol = class
  private
    FF11Key: string;
    FF11Text: string;
    FKeyFieldName: string;
    FFindDataMethod: TFindDataMethod;
    FGetDataMethod: TGetDataMethod;
    procedure SetF11Key(const Value: string);
    procedure SetF11Text(const Value: string);
    procedure SetFindDataMethod(const Value: TFindDataMethod);
    procedure SetGetDataMethod(const Value: TGetDataMethod);
    procedure SetKeyFieldName(const Value: string);
  public
    property FindDataMethod: TFindDataMethod read FFindDataMethod write SetFindDataMethod;
    property GetDataMethod: TGetDataMethod read FGetDataMethod write SetGetDataMethod;
    property F11Key: string read FF11Key write SetF11Key;
    property F11Text: string read FF11Text write SetF11Text;
    property KeyFieldName: string read FKeyFieldName write SetKeyFieldName;
  end;

  TDataStringGrid = class
  private
    FFieldNames : TStringList;
    FQuery: TJNCCQuery;
    FGrid: TControlStringGrid;
    FKeyField : string;
    FOnUpdateRow: TUpdateRowEvent;
    FOnDeleteRow: TDeleteRowEvent;
    FOnValidateCell: TValidateCellEvent;  // if specified, can apply extra cell validation
    FColDataSizes : TList; //  So we can restrict data entry
    FCurrentCellState : TCurrentCellState;
    FIndicator: boolean; // in case we need to undo a change
    FSaving : boolean;
    FLinkedEditButton: TImageListButton;
    FLinkedEditButtonEditor: TImageListButton;    
    FRowKeys: TObjectList;
    FEnabled: Boolean;
    FOnNewBlankRow: TNewBlankRowEvent;
    FSiteID: string;
    FUserID: string;
    FRestrictFullEdit: boolean;
    FAddOnly: boolean;
    FDataEntryControls: TStringList;
    function GetColLeft(iColIndex: integer): integer;
    function GetRowTop(iRowIndex: integer): integer;
    procedure GridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MoveGridCell(const iToTheRight: Boolean);
    procedure GridComboKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GridComboKeyPress(Sender: TObject; var Key: Char);
    procedure GridComboChange(Sender: TObject);
    procedure FillGrid;
    function FindRowKey(const iKey: string): TRowKey;
    procedure DeleteRow(const iKey: string);
    procedure InsertRow(iRowIndex: integer);
    procedure SaveEditsToRow(const iKey: string);
    procedure SetOnUpdateRow(const Value: TUpdateRowEvent);
    function FindGridRowForKey(const iKey: string): integer;
    procedure SetOnDeleteRow(const Value: TDeleteRowEvent);
    procedure BuildRecordStrings(iRowIndex: integer;
      const oDataList: TStrings);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect;
              State: TGridDrawState);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Longint; const Value: string);
    procedure DrawFixedColumnCell(ACol, ARow: Integer; Rect: TRect; State:
        TGridDrawState);
    function GetRowEdited(iRow: integer): boolean;
    function GetKey(iRow: integer): string;
    function GetInplaceEdit: TInplaceEdit;
    procedure ResetCurrentCell;
    procedure ReadCurrentCellState;
    procedure SetIndicator(const Value: boolean);
    function IsReadOnlyCol(ACol: integer): boolean;
    procedure ValidateRow(iRow: integer);
    procedure SetOnValidateCell(const Value: TValidateCellEvent);
    procedure GridTopLeftChanged (Sender: TObject);
    procedure SetFocusCol(ACol: integer);
    procedure SetGetButtonImageIndex(const Value: integer);
    procedure SetImageList(const Value: TCustomImageList);
    function GetGetButtonImageIndex: integer;
    function GetImageList: TCustomImageList;
    procedure GetButtonClick(Sender: TObject);
    function DoReturnKeyInLinkedEdit: boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetOnNewBlankRow(const Value: TNewBlankRowEvent);
    procedure AddBlankRowKey;
    function GetDataEntryControl(ACol: integer): TObject;
    procedure SetSiteID(const Value: string);
    procedure SetUserID(const Value: string);
    procedure SetRestrictFullEdit(const Value: boolean);
    procedure SetAddOnly(const Value: boolean);
    function GetCellLinkedKey(ACol, ARow: integer): string;
    procedure SetCellLinkedKey(ACol, ARow: integer; const Value: string);
    procedure SetupInPlaceEditor;
  protected
    procedure InitialiseLookup( iFieldIndex, iColIndex : integer );
    property InPlaceEdit : TInplaceEdit read GetInplaceEdit;
  public
    constructor Create( iGrid : TControlStringGrid; iQuery : TJNCCQuery; const iKeyField : string );
    destructor Destroy; override;
    procedure PositionEditControl(const ACol: integer; const ARow: integer);
    procedure PopulateGrid;
    procedure SetupLinkedEdit(const AKeyCol: string; ADisplayCol: integer; AFindDataMethod:
              TFindDataMethod; AGetDataMethod: TGetDataMethod;
              AF11Key: string=''; AF11Text: string = '');
    procedure Save;
    procedure Validate;
    procedure Refresh;
    procedure RefreshRow( const iRowKey : string );
    procedure AddToGrid(Sender : TObject);
    procedure DeleteFromGrid(Sender : TObject);
    procedure PostRow( iRowIndex : integer );
    procedure UpdateLinkedValue(const AText, AKey: string; ACol: integer=-1; ARow: integer=-1);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    function RowContainsData(iRow: integer): boolean;
    function FindRowByKey(AKey: string): integer;
    function FindRowByKeyInColumn(AKey: string; ACol: integer): integer;
    function RowLocked(ARow: integer): Boolean;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Grid : TControlStringGrid read FGrid;
    property Query: TJNCCQuery read FQuery;
    property Indicator : boolean read FIndicator write SetIndicator; // show indicator column?
    property RowEdited[ iRow : integer ] : boolean read GetRowEdited;
    property Key[ iRow : integer ] : string read GetKey;
    property CellLinkedKey[ACol, ARow: integer]: string read GetCellLinkedKey write SetCellLinkedKey;
    property OnUpdateRow : TUpdateRowEvent read FOnUpdateRow write SetOnUpdateRow;
    property OnDeleteRow : TDeleteRowEvent read FOndeleteRow write SetOndeleteRow;
    property OnValidateCell : TValidateCellEvent read FOnValidateCell write SetOnValidateCell;
    property OnNewBlankRow : TNewBlankRowEvent read FOnNewBlankRow write SetOnNewBlankRow;
    property ImageList: TCustomImageList read GetImageList write SetImageList;
    property GetButtonImageIndex: integer read GetGetButtonImageIndex write SetGetButtonImageIndex;
    property SiteID: string read FSiteID write SetSiteID;
    property UserID: string read FUserID write SetUserID;
    property RestrictFullEdit: boolean read FRestrictFullEdit write SetRestrictFullEdit;
    property AddOnly: boolean read FAddOnly write SetAddOnly;
  end;


implementation

uses
  GeneralFunctions, Graphics;

{ TDataStringGrid }

//==============================================================================
constructor TDataStringGrid.Create(iGrid: TControlStringGrid; iQuery: TJNCCQuery;
            const iKeyField : string);
begin
  inherited Create;
  FSaving := false; // are we in middle of a save operation
  FFieldNames := TStringList.Create;
  FQuery := iQuery;
  FGrid := iGrid;
  FKeyField := iKeyField;
  FColDataSizes := TList.Create;
  FRowKeys := TObjectList.Create;
  FRowKeys.OwnsObjects := True;
  FGrid.OnSelectCell   := GridSelectCell;
  FGrid.OnKeyDown      := GridKeyDown;
  FGrid.OnDrawCell     := GridDrawCell;
  FGrid.OnSetEditText  := GridSetEditText;
  FGrid.OnTopLeftChanged := GridTopLeftChanged;
  if FIndicator then
    FGrid.Col := 1; // select first true cell
  FLinkedEditButton := TImageListButton.Create(FGrid.Owner);
  FLinkedEditButton.Parent := FGrid;
  FLinkedEditButton.Visible := False;
  FLinkedEditButton.Width := FGrid.DefaultRowHeight;
  FLinkedEditButton.Height := FGrid.DefaultRowHeight;
  FLinkedEditButton.OnClick := GetButtonClick;
  // create a button for the inplace editor, but we can't create an inplace
  // editor yet so just leave it
  FLinkedEditButtonEditor := TImageListButton.Create(FGrid.Owner);
  FSiteID := '';
  // Declare a string list which holds the column index as the string, and the
  // data entry control as an object, only for columns which have controls
  FDataEntryControls := TStringList.Create;

end;


//==============================================================================
{ Destructor cleans up resources }
destructor TDataStringGrid.Destroy;
var
  i, j : integer;
begin
  { Clear lookup combos }
  for i := 0 to FGrid.Colcount-1 do begin
    // ensure all objects freed
    for j := 1 to FGrid.RowCount-1 do
      if Assigned(FGrid.Objects[i,j]) then
        TObject(FGrid.Objects[i,j]).Free;  // free the combo or linked edit column
  end;
  for i := 0 to FDataEntryControls.Count-1 do
    FDataEntryControls.Objects[i].Free;
  FDataEntryControls.Free;
  FRowKeys.Free;
  FFieldNames.Free;
  FColDataSizes.Free;
  inherited;
end;

//==============================================================================
{ Prepare a combo box for the lookup field we have identified }
procedure TDataStringGrid.InitialiseLookup(iFieldIndex, iColIndex: integer);
var
  lNewCombo : TComboBox;
  lKey : TKey;
begin
  lNewCombo := TComboBox.Create(nil);
  lNewCombo.Visible := False;
  lNewCombo.Parent  := FGrid;
  lNewCombo.OnKeyDown := GridComboKeyDown;
  lNewCombo.OnKeyPress := GridComboKeyPress;
  lNewCombo.OnChange := GridComboChange;
  with FQuery.Fields[iFieldIndex].LookupDataset do begin
    Open;
    try
      while not EOF do begin
        { Put key object into combo box list so we can find key for selected item }
        lKey := TKey.Create;
        lKey.Key := FieldByName(FQuery.Fields[iFieldIndex].LookupKeyFields).AsString;
        lNewCombo.Items.AddObject(
                  FieldByName(FQuery.Fields[iFieldIndex].LookupResultField).AsString, lKey);
        Next;
      end;
    finally
      Close;
    end;// try
  end;
  { Attach combo to first fixed row's data }
  FDataEntryControls.AddObject(IntToStr(iColIndex), lNewCombo);
end;


//==============================================================================
{ Fill up the string grid with query content }
procedure TDataStringGrid.PopulateGrid;
var
  i : integer;
  lCountCols : integer;
  lFixedCols : integer;
begin
  lCountCols := 0;
  lFixedCols := FGrid.FixedCols; // original count
  FColDataSizes.Clear;
  { Find the columns }
  for i := 0 to FQuery.Fields.Count-1 do
    if FQuery.Fields[i].Visible then begin
      FFieldNames.Add(FQuery.Fields[i].FieldName);
      Inc(lCountCols);
      FGrid.ColCount := lCountCols; // resets fixed cols
      if FGrid.FixedRows>0 then
        FGrid.Cells[lCountCols - 1, 0] := FQuery.Fields[i].DisplayName;
      FGrid.ColWidths[lCountCols - 1] := FQuery.Fields[i].DisplayWidth;
      FColDataSizes.Add(Ptr(FQuery.Fields[i].Size));
      if Assigned(FQuery.Fields[i].LookupDataset) then
        InitialiseLookup(i, lCountCols-1);
      if FQuery.Fields[i].ReadOnly then
        FFieldNames.Objects[FFieldNames.Count-1] := Ptr(1); // lo bit flag to state read only
      if FQuery.Fields[i].Required then
        FFieldNames.Objects[FFieldNames.Count-1] :=
                            Ptr(Integer(FFieldNames.Objects[FFieldNames.Count-1]) + 2); // 2nd bit flag to state required

    end;
  FGrid.FixedCols := lFixedCols; // so we set it back
  FillGrid;
end;


//==============================================================================
procedure TDataStringGrid.FillGrid;
var
  i : integer;
  lRowKey : TRowKey;

    // Set up the editability of a new row
    procedure InitRowKey;
    begin
      if AddOnly then begin
        // Add Only users can't touch existing data
        lRowKey.Locked := true;
        lRowKey.CanDelete := False;
      end
      else begin
        if (SiteID<>'') and (FQuery.FieldList.IndexOf('Custodian')<>-1) then
          lRowKey.Locked := FQuery.FieldByName('Custodian').AsString<>SiteID;
        if (UserID <> '') and RestrictFullEdit and (FQuery.FieldList.IndexOf('Entered_By')<>-1) then begin
          lRowKey.Locked := lRowKey.Locked or (FQuery.FieldByName('Entered_By').AsString<>UserID);
          lRowKey.CanDelete := not lRowKey.Locked;
        end;
      end;
    end;

begin
  { Fill the grid }
  with FQuery do begin
    FGrid.RowCount := Max(RecordCount, 1) + FGrid.FixedRows; // don't set less than 1 non-fixed row
    FRowKeys.Clear;
    if RecordCount=0 then
      // clear first blank row for empty grid
      FGrid.Rows[FGrid.FixedRows].CommaText := ''
    else begin
      First;
      while not EOF do begin
        for i := 0 to FGrid.ColCount-1 do begin
          // For linked cells, also store the key
          if Assigned(GetDataEntryControl(i)) then
            if TObject(GetDataEntryControl(i)) is TLinkedEditCol then
              CellLinkedKey[i, RecNo - 1 + FGrid.FixedRows] := FieldByName(TLinkedEditCol(
                  GetDataEntryControl(i)).KeyFieldName).AsString;
          FGrid.Cells[i, RecNo - 1 + FGrid.FixedRows] := FieldByName(FFieldNames[i]).AsString;
        end;
        lRowKey := TRowKey.Create(FieldByName(FKeyField).AsString);
        InitRowKey;
        FRowKeys.Add(lRowKey);
        Next;
      end; // while
    end;
  end; // with FQuery
  // must be at least 1 blank row
  if FQuery.RecordCount = 0 then
    AddBlankRowKey;
end;


//==============================================================================
procedure TDataStringGrid.GridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  i : integer;

    function IsIndicator: boolean;
    begin
      Result := (ACol=0) and FIndicator;
    end;

begin
  if FEnabled then begin
    { Can't select a readonly column - except indicator col }
    if (Integer(FFieldNames.Objects[ACol]) and 1 <> 0) and // lo bit set = readonly
        (not IsIndicator) then
      CanSelect := False
    else
    if (Integer(FFieldNames.Objects[FGrid.Col]) and 2 <> 0) then begin // 2nd bit set = required
      if (FGrid.Cells[FGrid.Col, FGrid.Row] = '') and (FGrid.Row < FGrid.RowCount)
        and (not FSaving) and (FIndicator and (ACol = 0)) then begin
        CanSelect := False;  // force entry for required fields
        ShowInformation(Format(ResStr_PleaseEnterAValueFor, [FGrid.Cells[FGrid.Col, 0]]));
      end;
    end;
    if ARow>0 then
      if RowLocked(ARow) and Enabled and
          (not IsIndicator) then
        CanSelect := False;

    // External validation
    if CanSelect and Assigned(FOnValidateCell) then
      FOnValidateCell(CanSelect, FGrid.Col, FGrid.Row);
    // Can't select a specific cell on a locked row, except the indicator

    if CanSelect then
    begin
      // validate entire row if moving off it (and it still exists)
      if (ARow <> FGrid.Row) and (FGrid.Row < FGrid.RowCount) then
        ValidateRow(FGrid.Row);
      // if in indicator, then turn off editing
      if IsIndicator then
        FGrid.Options := FGrid.Options - [goEditing]
      else
      if FEnabled then
        FGrid.Options := FGrid.Options + [goEditing];

      FLinkedEditButton.Visible := false;
      FLinkedEditButtonEditor.Visible := false;      
      FGrid.Invalidate; // show focused row correctly if required
      { Show combo if required }
      for i := 0 to FGrid.ColCount-1 do begin
        if Assigned(GetDataEntryControl(i)) then
          if GetDataEntryControl(i) is TComboBox then
            TComboBox(GetDataEntryControl(i)).Visible := (i = ACol) and FEnabled;
      end;
      PositionEditControl(ACol,ARow);
      SetFocusCol(ACol);
    end;
    // If changing row, invlidate to redraw the indicator
    if ARow <> FGrid.Row then
      FGrid.Invalidate;
    ReadCurrentCellState;
  end; // if FEnabled
end;

{-------------------------------------------------------------------------------
  Set the focus onto the grid or combo box
}
procedure TDataStringGrid.SetFocusCol(ACol: integer);
begin
  // Focus the grid
  if FGrid.CanFocus then begin
    GetParentForm(FGrid).ActiveControl := FGrid;
    // If we have a combo, then focus that instead
    if Assigned(GetDataEntryControl(ACol)) and FEnabled then
      if GetDataEntryControl(ACol) is TComboBox then
        GetParentForm(FGrid).ActiveControl := TWinControl(GetDataEntryControl(ACol));
  end;
end;


//==============================================================================
function TDataStringGrid.GetColLeft( iColIndex : integer ) : integer;
var
  i : integer;
begin
  Result := 2;
  for i := 0 to FGrid.FixedCols -1 do
    Result := Result + FGrid.ColWidths[i] + 1;
  for i := FGrid.LeftCol to iColIndex - 1 do { ignore this col! }
    Result := Result + FGrid.ColWidths[i] + 1;
end;


//==============================================================================
function TDataStringGrid.GetRowTop( iRowIndex : integer ) : integer;
var
  i : integer;
begin
  Result := 0;
  for i := 0 to FGrid.FixedRows - 1 do
    Result := Result + FGrid.RowHeights[i] + 1;
  for i := FGrid.TopRow to iRowIndex - 1 do { ignore this row! }
    Result := Result + FGrid.Rowheights[i] + FGrid.GridLineWidth;
end;


//==============================================================================
procedure TDataStringGrid.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var lCellMove   : Integer;
begin
  inherited;
  lCellMove := 0; // no movement by default
  if FEnabled then begin
    with TStringGrid(Sender) do begin
      if ((Key = VK_DELETE) or (Key = VK_BACK)) then begin
        if (FIndicator and (Col=0)) or (not (FIndicator or RowContainsData(Row))) then begin
          DeleteFromGrid(FGrid);
          Key := 0;
        end else
        if not FGrid.EditorMode then begin
          // del or backspace on an editable column, but not whilst editing
          FGrid.Cells[Col, Row] := ''; // blank the cell
          TRowKey(FRowKeys[Row - FGrid.FixedRows]).Edited := True;
        end;
      end else
      if Key = VK_RETURN then begin // return key always moves, irrespective of edit text
        if not DoReturnKeyInLinkedEdit then
          if not (ssCtrl in Shift) then
            lCellMove := 1
          else
            lCellMove := -1;
      end else
      if (Key = VK_DOWN) and (FGrid.Row = FGrid.RowCount-1) then
        AddToGrid(nil)
      else
      if (Key = VK_F11) and (goEditing in Options) then
        // CCN248 - handling of F11 shortcut to insert current user details in a name field
        if Assigned(GetDataEntryControl(FGrid.Col)) then
          if TObject(GetDataEntryControl(FGrid.Col)) is TLinkedEditCol then
            if TLinkedEditCol(GetDataEntryControl(FGrid.Col)).F11Key<>'' then begin
              CellLinkedKey[FGrid.Col, FGrid.Row] := TLinkedEditCol(GetDataEntryControl(FGrid.Col)).F11Key;
              FGrid.Cells[FGrid.Col, FGrid.Row]   := TLinkedEditCol(GetDataEntryControl(FGrid.Col)).F11Text;
            end;

      if Assigned(InplaceEdit) then begin
        if Key = VK_RIGHT then begin // right arrow
          if not (goEditing in Options) then                          // cannot edit, so always navigate within grid
            lCellMove := 1 // cell to the right
          else
          if (InplaceEdit.SelStart = Length(InplaceEdit.Text)) or     // editable, but caret at right limit of text
             (InplaceEdit.SelLength = Length(InplaceEdit.Text)) then  // editable, all text selected
            lCellMove := 1; // cell to the right
        end else
        if Key = VK_LEFT then begin // left arrow
          if not (goEditing in Options) then                          // cannot edit, so always navigate within grid
            lCellMove := -1 // cell to the left
          else
          if (InplaceEdit.SelStart = 0) or                            // editable, but caret at left limit of text
             (InplaceEdit.SelLength = Length(InplaceEdit.Text)) then  // editable, all text selected
            lCellMove := -1; // cell to the left
        end;
      end;
      if lCellMove<>0 then begin
        {if FEditingCell then
          UpdateSpeciesValues(Cells[Col, 0], Row);}
        MoveGridCell(lCellMove = 1);
        Key := 0; // no further action on this keypress
      end else
      if not (Key in [VK_RETURN, VK_F11]) and Assigned(FGrid.Objects[FGrid.Col, FGrid.Row]) then
        CellLinkedKey[FGrid.Col, FGrid.Row] := '';
    end;
  end; // if FEnabled
end;  // sgSpeciesKeyDown

//==============================================================================
{ MoveGridCell - navigates the current cell to the right or left in the grid.
     If you hit the end, goes to the next/previous row where one is available }
procedure TDataStringGrid.MoveGridCell(const iToTheRight: Boolean);
begin
  if iToTheRight then begin  // Return
    with FGrid do
      if Col<ColCount-1 then
        Col:=Col+1
      else begin
        if Row<RowCount-1 then begin
          Row := Row + 1;
          if FIndicator then
            Col := FixedCols+1
          else
            Col := FixedCols;
        end;
      end;
  end else begin  // Shift-Return
    with FGrid do
      if Col>0 then
        Col:=Col-1
      else begin
        if Row>FixedRows then begin // don't move into title row
          Row := Row - 1;
          Col := ColCount-1;
        end;
      end;
  end;
end;  // MoveGridCell



//==============================================================================
{ Trap left and right arrow, return and ctrl-return, to provide navigation in
    the combo boxes on the species grid }
procedure TDataStringGrid.GridComboKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  lCellMove : integer;
begin
  inherited;
  lCellMove := 0; // no movement by default
  with TComboBox(Sender) do
    if Key = VK_RIGHT then begin // right arrow
      if (Style = csDropDownList) or        // cannot edit, so always navigate within grid
         (SelStart = Length(Text)) or       // editable, but caret at right limit of text
         (SelLength = Length(Text)) then    // editable, all text selected
        lCellMove := 1; // cell to the right
    end else if Key = VK_LEFT then begin // left arrow
      if (Style = csDropDownList) or        // cannot edit, so always navigate within grid
         (SelStart = 0) or                  // editable, but caret at left limit of text
         (SelLength = Length(Text)) then    // editable, all text selected
        lCellMove := -1; // cell to the left
    end else if (Key = VK_DELETE) or (Key = VK_BACK) then begin // del or backspace on a combo
      if (FIndicator and (FGrid.Col=0)) or (not (FIndicator or RowContainsData(FGrid.Row))) then begin
        DeleteFromGrid(FGrid);
        Key := 0;
      end;    
      FGrid.Cells[FGrid.Col, FGrid.Row] := ''; // blank the cell
      if (Style = csDropDownList) then
        ItemIndex := -1; // blank the combo by clearing selection
    end else if (Key = VK_RETURN) then
      DroppedDown := False;
  if lCellMove<>0 then begin
    MoveGridCell(lCellMove = 1);
    Key := 0; // no further action on this keypress
    // Ensure grid gets focus
    SetFocus(FGrid.Col);
  end; // if
end;  // GridComboKeyDown


//==============================================================================
procedure TDataStringGrid.GridComboKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
  if Key in [#13, #10] then begin// Return or shift return
    MoveGridCell(Key=#13);
    Key := #0;
  end;
end;  // GridComboKeyPress



//==============================================================================
{ when a combo box is edited, update the contents of the cell }
procedure TDataStringGrid.GridComboChange(Sender: TObject);
begin
  if Sender is TComboBox then
    FGrid.Cells[FGrid.Col, FGrid.Row] := TComboBox(Sender).Text;
  TRowKey(FRowKeys[FGrid.Row-FGrid.FixedRows]).Edited := True;
end;  // GridComboChange


//==============================================================================
{ Repopulate the grid, discarding changes }
procedure TDataStringGrid.Refresh;
begin
  FillGrid;
end;


//==============================================================================
{ Refresh a single row from the db }
procedure TDataStringGrid.RefreshRow( const iRowKey : string );
var
  i : integer;
  lRow : integer;
  lRowKey: TRowKey;
begin
  lRow := FindGridRowForKey( iRowKey );
  if lRow = -1 then begin
    // need new row if it has been deletet
    AddToGrid(nil);
    lRow := FGrid.RowCount-1;
    lRowKey := TRowKey.Create(iRowKey);
    FRowKeys.Add(lRowKey);
  end;
  with FQuery do begin
    // reload query
    Close;
    Open;
    First;
    // find single record to refresh
    while not EOF do begin
      if FieldByName(FKeyField).AsString = iRowKey then begin
        for i := 0 to FGrid.ColCount-1 do
          FGrid.Cells[i, lRow] := FieldByName(FFieldNames[i]).AsString;
        break; // from loop
      end;
      Next;
    end;
  end;     
end;

//==============================================================================
{ Save changes to the database }
procedure TDataStringGrid.Save;
var
  lRowKey : TRowKey;
  i : integer;
begin
  FSaving := True;
  try
    // first validate all required cols are populated
    Validate;
    with FQuery do begin
      First;
      while not EOF do begin
        lRowKey := FindRowKey(FieldByName(FKeyField).AsString);
        if Assigned(lRowKey) then begin
          if lRowKey.Edited then begin
            i := FindGridRowForKey(lRowKey.Key);
            // Only save if there is something to save. Or we might get a can't insert null error
            // or similar.
            if RowContainsData(i) then begin
              SaveEditsToRow(FieldByName(FKeyField).AsString);
              lRowkey.Edited := False;
            end else
              DeleteRow(FieldByName(FKeyField).AsString);
          end;
        end else
          DeleteRow(FieldByName(FKeyField).AsString);
        Next;
      end;
    end;
    { Finally look for rows with a new rowkey that have had changes made }
    for i := FGrid.FixedRows to FGrid.RowCount-1 do
      if (TRowKey(FRowKeys[i-FGrid.FixedRows]).Key = '') and TRowKey(FRowKeys[i-FGrid.FixedRows]).Edited
          and RowContainsData(i) then
        InsertRow(i);
  finally
    FSaving := False;
  end; // try
end;


//==============================================================================
{ Validate that required fields in a row are set.  If not, then an exception is
    raised and the relevant cell is focused }
procedure TDataStringGrid.ValidateRow( iRow : integer );
var
  lCol : integer;
  lCellOk: boolean;

    procedure RequireValue;
    begin
      FGrid.Col := lCol;
      FGrid.Row := iRow;
      PositionEditControl(lCol, iRow);
      SetFocusCol(lCol);
      Raise EDataStringGridError.CreateNonCritical(
          Format(ResStr_PleaseEnterAValueFor, [FGrid.Cells[lCol, 0]]));
    end;

begin
  for lCol := 0 to FGrid.ColCount - 1 do begin
    // ignore read only or indicator col
    if ((lCol = 0) and FIndicator) or (Integer(FFieldNames.Objects[lCol]) and 1 <> 0) then
      Continue; // next col
    if RowContainsData(iRow) then begin
      lCellOk := true;
      if Assigned(OnValidateCell) then
        OnValidateCell(lCellOk, lCol, iRow);
      if not lCellOk then
        raise eAbort.Create(''); // validate cell should have displayed the message
      // if required (the "and 2" bit), check populated
      if (Integer(FFieldNames.Objects[lCol]) and 2 <> 0) then
        if (FGrid.Cells[lCol, iRow] = '') then begin
          if FQuery.FieldByName(FFieldNames[lCol]).DefaultExpression <> '' then
            FGrid.Cells[lCol, iRow] := FQuery.FieldByName(FFieldNames[lCol]).DefaultExpression
          else
            RequireValue;
        end
        else if (TObject(GetDataEntryControl(lCol)) is TLinkedEditCol) and
              (CellLinkedKey[lCol, iRow]='') then
          RequireValue
        else if (TObject(GetDataEntryControl(lCol)) is TComboBox) and
              (TComboBox(GetDataEntryControl(lCol)).Items.IndexOf(
              FGrid.Cells[lCol, iRow])=-1) then
          // combo boxes - ensure item is selected from list.
          RequireValue;
    end;
  end; // for lCol
end;

//==============================================================================
{ Return a row key with a matching key, or nil if none found }
function TDataStringGrid.FindRowKey(const iKey : string) : TRowKey;
var
  i : integer;
begin
  Result := nil; // default not found
  for i := 0 to FRowKeys.Count-1 do begin
    if TRowKey(FRowKeys[i]).Key = iKey then begin
      Result := TRowKey(FRowKeys[i]);
      Break; // from loop
    end;
  end;
end;


//==============================================================================
{ Return a grid row index with a matching key, or -1 if none found }
function TDataStringGrid.FindGridRowForKey(const iKey : string) : integer;
var
  i : integer;
begin
  Result := -1; // default not found
  for i := FGrid.FixedRows to FGrid.RowCount-1 do begin
    if TRowKey(FRowKeys[i-FGrid.FixedRows]).Key = iKey then begin
      Result := i;
      Break; // from loop
    end;
  end;
end;


//==============================================================================
{ save edits to the db for a row identified by a key.  This creates a string
     list of name-value pairs for the data record that has changed, and sends
     it to the callback function.  The generic grid does not know how best to
     update the database. }
procedure TDataStringGrid.SaveEditsToRow(const iKey : string);
var
  lRowIndex : integer;
  lDataList : TStringList;
  lKey : string;
begin
  if Assigned(FOnUpdateRow) then begin
    lDataList := TStringList.Create;
    try
      lRowIndex := FindGridRowForKey( iKey );
      if lRowIndex<>-1 then begin
        BuildRecordStrings( lRowIndex, lDataList );
        lKey := iKey; // because next method requires var
        FOnUpdateRow( lKey, lDataList );
        TRowKey(FRowKeys[lRowIndex-FGrid.FixedRows]).Edited := False;
      end;
    finally
      lDataList.Free;
    end;
  end;
end;


//==============================================================================
{ Delete rows as identified by a key }
procedure TDataStringGrid.DeleteRow(const iKey : string);
begin
  if Assigned(FOnDeleteRow) then
    FOnDeleteRow(iKey);
end;


//==============================================================================
{ Insert a new row (at index supplied in grid) into the database, by calling an
     event handler if assigned }
procedure TDataStringGrid.InsertRow(iRowIndex : integer);
var
  lDataList : TStringList;
  lKey : string;
begin
  if Assigned(FOnUpdateRow) then begin
    lDataList := TStringList.Create;
    try
      BuildRecordStrings(iRowIndex, lDataList);
      lKey := '';
      FOnUpdateRow( lKey, lDataList );
      TRowKey(FRowKeys[iRowIndex-FGrid.FixedRows]).Edited := False;
      TRowKey(FRowKeys[iRowIndex-FGrid.FixedRows]).Key := lKey;
    finally
      lDataList.Free;
    end;
  end;
end;


//==============================================================================
{ Allow external access to set the OnUpdateRow event }
procedure TDataStringGrid.SetOnUpdateRow(const Value: TUpdateRowEvent);
begin
  FOnUpdateRow := Value;
end;


//==============================================================================
{ Convert a row of the grid into a string list of name/value pairs }
procedure TDataStringGrid.BuildRecordStrings(iRowIndex : integer; const oDataList : TStrings);
var
  i : integer;
  lItemIndex : integer;
  lIsComboColumn: boolean;
begin
  oDataList.Clear;
  with FGrid do
    for i := 0 to ColCount-1 do begin
      { Don't send read only columns }
      if ((Integer(FFieldNames.Objects[i]) and 1) = 0) then begin
        lIsComboColumn := False; // default
        if Assigned(GetDataEntryControl(i)) then // combo assigned, so a lookup
          if GetDataEntryControl(i) is TComboBox then
            lIsComboColumn := True;
        if lIsComboColumn then
          with TComboBox(GetDataEntryControl(i)) do begin
            // send the key value from the object on the combo
            lItemIndex := Items.IndexOf(Cells[i, iRowIndex]);
            if lItemIndex <> -1 then // don't send a null survey
              oDataList.Add(FQuery.FieldByName(FFieldNames[i]).LookupKeyFields + '=' +
                            TKey(Items.Objects[lItemIndex]).Key);
          end
        else // normal field, send text
          oDataList.Add(FFieldNames[i] + '=' + Cells[i, iRowIndex]);
        if Assigned(GetDataEntryControl(i)) then
          if GetDataEntryControl(i) is TLinkedEditCol then
            oDataList.Add(TLinkedEditCol(GetDataEntryControl(i)).KeyFieldName + '=' +
                          CellLinkedKey[i, iRowIndex]);
      end;
    end; // for
end; // BuildRecordStrings


//==============================================================================
{ Draw cells in the grid.  Read only columns are greyed }
procedure TDataStringGrid.GridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  lRectShape : TPoint;
  lReadOnly: boolean;
begin
  SetupInPlaceEditor;

  lReadOnly := false;
  if IsReadOnlyCol(ACol) then
    lReadOnly := true
  else if ARow>0 then
    if Enabled and RowLocked(ARow) then
      lReadOnly := true;
  with FGrid.Canvas do begin
    if lReadOnly and IsReadOnlyCol(ACol) then
      DrawFixedColumnCell(ACol, ARow, Rect, State)
    else begin
      if (FGrid.Col = 0) and FIndicator and (FGrid.Row = ARow) and
         (not (gdFixed in State)) then
      begin
        // if indicator selected, select row
        if lReadOnly then begin
          Brush.Color := clBtnHighlight;
          Font.Color := clBtnShadow;
        end
        else begin
          Brush.Color := clHighlight;
          Font.Color := clhighlightText;
        end;
      end else
      if gdFixed in State then begin
        Brush.Color := clBtnFace;
        Font.Color := clWindowText;
      end else begin
        Brush.Color := clWindow;
        if lReadOnly or (not Enabled) then
          Font.Color := clGrayText
        else
          Font.Color := clWindowText;
      end;
      FillRect(Rect);
      DrawChoppedText(FGrid.Cells[ACol, ARow], FGrid.Canvas, Rect, 2);
    end;
    { Indicator col shows a triangle if current row }
    if ((ACol = 0) and FIndicator) and (ARow = FGrid.Row) then
    begin
      Pen.Color := clBtnText;
      Brush.Color := clBtnText;
      // find dimensions of the rect, so we can calculate the triangle to draw
      lRectShape.x := Rect.Right-Rect.Left;
      lRectShape.y := Rect.Bottom - Rect.Top;
      Polygon([Point(Rect.Left + lRectShape.x div 4, Rect.Top + lRectShape.y div 5),
               Point(Rect.Right - lRectShape.x div 3, Rect.Top + lRectShape.y div 2),
               Point(Rect.Left + lRectShape.x div 4, Rect.Bottom - lRectShape.y div 5)]);
    end;
  end; // with FGrid.Canvas
  { Show combo if required }
  if (FGrid.Row = ARow) and (FGrid.Col = ACol) and Assigned(GetDataEntryControl(ACol)) then
    PositionEditControl(ACol, ARow);
  ReadCurrentCellState;
end;

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.PositionEditControl(const ACol: integer; const ARow: integer);
var
  lVisibleCellArea: TRect;
begin
  if Assigned(GetDataEntryControl(ACol)) then
    if GetDataEntryControl(ACol) is TComboBox then begin
      TComboBox(GetDataEntryControl(ACol)).Visible := FEnabled;
      with TComboBox(GetDataEntryControl(ACol)) do begin
        lVisibleCellArea.left := GetColLeft(ACol)-1;
        lVisibleCellArea.Top := GetRowTop(ARow)-1;
        if (lVisibleCellArea.left +  FGrid.ColWidths[ACol] - 1) >
            FGrid.Left + FGrid.width then
          lVisibleCellArea.Right := (FGrid.Left + FGrid.width) - lVisibleCellArea.left
        else
          lVisibleCellArea.Right := FGrid.ColWidths[ACol];
        lVisibleCellArea.Bottom := lVisibleCellArea.Top +FGrid.Rowheights[ARow];
        // NOTE the Right and Bottom values are actually the Width and Height respectively
        SetBounds(lVisibleCellArea.left, lVisibleCellArea.Top,
                  lVisibleCellArea.Right, lVisibleCellArea.Bottom);
        ItemIndex := Items.IndexOf(FGrid.Cells[ACol, ARow]);
      end;
    end
    else if (TObject(GetDataEntryControl(ACol)) is TLinkedEditCol)
            and (not RowLocked(ARow)) then begin
      // Ensure button remains on screen if cell clipped
      FLinkedEditButton.Left := Min(
          GetColLeft(ACol) + FGrid.ColWidths[ACol] - FLinkedEditButton.Width - 2,
          FGrid.ClientWidth-FLinkedEditButton.Width);
      FLinkedEditButton.Top := GetRowTop(ARow);
      FLinkedEditButton.Visible := FEnabled;
      FLinkedEditButtonEditor.Visible := FEnabled;
    end;
end;


//==============================================================================
{ Draw read only cells to look like fixed cells. }
procedure TDataStringGrid.DrawFixedColumnCell(ACol, ARow: Integer; Rect: TRect;
    State: TGridDrawState);
begin
  { Read only cells must be filled grey, except in the fixed cells }
  with FGrid.Canvas do begin
    Brush.Color := clBtnFace;
    FillRect(Rect);
    if FGrid.Ctl3d then begin
      DrawEdge(Handle, Rect, BDR_RAISEDINNER, BF_RECT);
      // draw lines around read only column
      Pen.Color := cl3dDkShadow;
      if ARow = FGrid.FixedRows then
        MoveTo(Rect.Left-1, Rect.Top-1)
      else
        MoveTo(Rect.Left-1, Rect.Top);
      LineTo(Rect.Left-1, Rect.Bottom);
      LineTo(Rect.Right, Rect.Bottom);
      LineTo(Rect.Right, Rect.Top);
    end
    else begin
      Pen.Color := clBlack;  // cl3dDkShadow
      FrameRect(Rect);
    end;
    DrawChoppedText(FGrid.Cells[ACol, ARow], FGrid.Canvas, Rect, 2);
  end;
end;


//==============================================================================
function TDataStringGrid.IsReadOnlyCol( ACol : integer ): boolean;
begin
  Result := (Integer(FFieldNames.Objects[ACol]) and 1 <> 0) or ((ACol = 0) and FIndicator);
end;


//==============================================================================
{ Capture edits and set the row key edited flag }
procedure TDataStringGrid.GridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  // if row still exists
  if ARow<FGrid.RowCount then begin
    TRowKey(FRowKeys[ARow-FGrid.FixedRows]).Edited := True;
    // if too much data for field, then wind back
    if Length(Value) > Integer(FColDataSizes[ACol]) then
      ResetCurrentCell
    else
      ReadCurrentCellState;
  end; // if
end;


//==============================================================================
// return the current cell contents to its previous state - e.g. when trying
// to type too much for the field
procedure TDataStringGrid.ResetCurrentCell;
begin
  FGrid.Cells[FGrid.Col, FGrid.Row] := FCurrentCellState.Text;
  if Assigned(InPlaceEdit) then begin
    InPlaceEdit.SelLength := FCurrentCellState.SelLength;
    InPlaceEdit.SelStart := FCurrentCellState.SelStart;
  end;
end;


//==============================================================================
// Read state of current cell every time it changes, so we can wind back
procedure TDataStringGrid.ReadCurrentCellState;
begin
  FCurrentCellState.Text := FGrid.Cells[FGrid.Col, FGrid.Row];
  if Assigned(InPlaceEdit) then begin
    FCurrentCellState.SelLength := InPlaceEdit.SelLength;
    FCurrentCellState.SelStart := InPlaceEdit.SelStart;
  end else begin   // reset for safety
    FCurrentCellState.SelLength := 0;
    FCurrentCellState.SelStart := 0;
  end;
end;

//==============================================================================
{ Accessor method - checks if edited flag is true for a row }
function TDataStringGrid.GetRowEdited(iRow: integer): boolean;
begin
  Result := TRowKey(FRowKeys[iRow-FGrid.FixedRows]).Edited;
end;


//==============================================================================
{ TRowKey }
//==============================================================================


//==============================================================================
constructor TRowKey.Create(iKey: string);
begin
  inherited Create;
  FKey := iKey;
  FEdited := False;
  FLocked := False; // row not locked to read only
  FCanDelete := True; // default, unless full edit (Own data) user or add only user
end;

//==============================================================================
procedure TRowKey.SetCanDelete(const Value: boolean);
begin
  FCanDelete := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TRowKey.SetEdited(const Value: boolean);
begin
  FEdited := Value;
end;


//==============================================================================
// set ondeleterow event handler
procedure TDataStringGrid.SetOnDeleteRow(const Value: TDeleteRowEvent);
begin
  FOndeleteRow := Value;
end;


//==============================================================================
{ Method that can be calledd from popup menu, buttons etc to add a row to the
    grid.  If the last row is blank, then selects that instead }
procedure TDataStringGrid.AddToGrid(Sender: TObject);
var
  lCol : integer;
begin
  if FGrid.Row < FGrid.RowCount then
  begin
    ValidateRow(FGrid.Row);
  // focus first non-fixed col
  if FIndicator then
    lCol := 1
  else
    lCol := 0;
  while (lCol < FGrid.ColCount) and (FFieldNames.Objects[lCol] = Ptr(1)) do
    Inc(lCol);
  FGrid.Col := lCol;
  end;
  if not RowContainsData(FGrid.RowCount-1) then
    FGrid.Row := FGrid.RowCount-1
  else begin
    FGrid.RowCount := FGrid.RowCount + 1;
    // Add blank row key
    AddBlankRowKey;
    FGrid.Row := FGrid.RowCount-1;
    FGrid.Rows[FGrid.Row].CommaText := '';
  end;
  PostMessage(FGrid.Handle, WM_SETFOCUS, 0, 0);
//  GetParentForm(FGrid).ActiveControl := FGrid;
end;

//==============================================================================
{ remove the current line from the grid }
procedure TDataStringGrid.DeleteFromGrid(Sender: TObject);
var
  lRow, lCol : integer;
begin
  with FGrid do begin
    if Row-FGrid.FixedRows<FRowKeys.Count then begin
      if not TRowKey(FRowKeys[Row-FixedRows]).CanDelete then
        raise EDataStringGridError.CreateNonCritical(ResStr_NoAccessToDeleteRow);
      FRowKeys.Delete(Row-FixedRows);
    end;
    // move the later records up 1 row
    for lRow := Row + 1 to RowCount - 1 do begin
      for lCol := 0 to ColCount-1 do begin
        Cells[lCol, lRow-1] := Cells[lCol, lRow];
        Objects[lCol, lRow-1] := Objects[lCol, lRow];
      end;
    end; // for
    FLinkedEditButton.Visible := False;
    FLinkedEditButtonEditor.Visible := False;    
    FGrid.Rows[RowCount -1].CommaText := '';
    // deleting last row, so stick blank rowkey on for blank row
    if RowCount = 1 + FixedRows then begin
      Rows[FixedRows].CommaText := ''; // blank the row
      FRowKeys.Clear;
      AddBlankRowKey;
    end;
    RowCount := Max(RowCount - 1, 1+FixedRows);

    Row := Rowcount - 1;

    // If the only remaining row is locked, disable edit mode.
    if RowLocked(Row) then
      Options := Options - [Grids.goEditing];
  end;
end;


//==============================================================================
{ Public method allowing new rows to be inserted, and edited rows to be saved }
procedure TDataStringGrid.PostRow(iRowIndex: integer);
begin
  ValidateRow(iRowIndex);
  if not RowContainsData(iRowIndex) then
    raise EDataStringGridError.CreateNonCritical(ResStr_NoContentCannotSave);
  if TRowKey(FRowKeys[iRowIndex-FGrid.FixedRows]).Key = '' then
    InsertRow(iRowIndex)
  else
    SaveEditsToRow(TRowKey(FRowKeys[iRowIndex-FGrid.FixedRows]).Key);
end;


//==============================================================================
// Accessor method
procedure TRowKey.SetKey(const Value: string);
begin
  FKey := Value;
end;


//==============================================================================
// Accessor method
function TDataStringGrid.GetKey(iRow: integer): string;
begin
  Result := TRowKey(FRowKeys[iRow-FGrid.FixedRows]).Key;
end;

{-------------------------------------------------------------------------------
  Accessor method.  If locked then row cannot be edited
}
procedure TRowKey.SetLocked(const Value: boolean);
begin
  FLocked := Value;
end;

//==============================================================================
{ Accessor method - find in place editor if there is one - could be NIL }
function TDataStringGrid.GetInplaceEdit: TInplaceEdit;
begin
  Result := TAccessorClass(FGrid).InplaceEditor;
end;


//==============================================================================
procedure TDataStringGrid.SetIndicator(const Value: boolean);
begin
  FIndicator := Value;
end;


//==============================================================================
// Return true if a row in the grid actually has any data
function TDataStringGrid.RowContainsData( iRow : integer ): boolean;
var
  i : integer;
begin
  Result := False;
  for i := 0 to FGrid.ColCount-1 do
    Result := Result or (FGrid.Cells[i, iRow]<>'');
end;


//==============================================================================
procedure TDataStringGrid.SetOnValidateCell(const Value: TValidateCellEvent);
begin
  FOnValidateCell := Value;
end;

{-------------------------------------------------------------------------------
  This is called when the grid is scrolled about. This ensures that the controls
      move with the grid.
}
procedure TDataStringGrid.GridTopLeftChanged(Sender: TObject);
begin
  PositionEditControl(FGrid.Col, FGrid.Row);
  FGrid.Invalidate;
end;

{-------------------------------------------------------------------------------
  Initialise a column to behave as a linked edit cell. If f11Key & Text are
  specified, then they define the key and text value to insert in the field
  if F11 is pressed when focused. Used to insert current user in Name fields.
}
procedure TDataStringGrid.SetupLinkedEdit(const AKeyCol: string; ADisplayCol: integer;
  AFindDataMethod: TFindDataMethod; AGetDataMethod: TGetDataMethod;
  AF11Key: string=''; AF11Text: string = '');
var
  lLinkedEditCol: TLinkedEditCol;
begin
  lLinkedEditCol := TLinkedEditCol.Create;
  with lLinkedEditCol do begin
    FindDataMethod := AFindDataMethod;
    GetDataMethod  := AGetDataMethod;
    KeyFieldName   := AKeyCol;
    KeyFieldName   := AKeyCol;
    F11Key         := AF11Key;
    F11Text        := AF11Text;
  end;
  FDataEntryControls.AddObject(IntToStr(ADisplayCol), lLinkedEditCol);
end;

{-------------------------------------------------------------------------------
  Accessor - PK of default value inserted when f11 pressed
}
procedure TLinkedEditCol.SetF11Key(const Value: string);
begin
  FF11Key := Value;
end;

{-------------------------------------------------------------------------------
  Accessor - caption of default value inserted when f11 pressed
}
procedure TLinkedEditCol.SetF11Text(const Value: string);
begin
  FF11Text := Value;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TLinkedEditCol.SetFindDataMethod(const Value: TFindDataMethod);
begin
  FFindDataMethod := Value;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TLinkedEditCol.SetGetDataMethod(const Value: TGetDataMethod);
begin
  FGetDataMethod := Value;
end;

procedure TLinkedEditCol.SetKeyFieldName(const Value: string);
begin
  FKeyFieldName := Value;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TDataStringGrid.SetGetButtonImageIndex(const Value: integer);
begin
  FLinkedEditButton.ImageIndex := Value;
  FLinkedEditButtonEditor.ImageIndex := Value;  
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TDataStringGrid.SetImageList(const Value: TCustomImageList);
begin
  FLinkedEditButton.ImageList := Value;
  FLinkedEditButtonEditor.ImageList := Value;  
end;

{-------------------------------------------------------------------------------
  Accessor method
}
function TDataStringGrid.GetGetButtonImageIndex: integer;
begin
  Result := FLinkedEditButton.ImageIndex;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
function TDataStringGrid.GetImageList: TCustomImageList;
begin
  Result := FLinkedEditButton.ImageList;
end;

{-------------------------------------------------------------------------------
  Call the GetData method
}
procedure TDataStringGrid.GetButtonClick(Sender: TObject);
begin
  with FGrid do
    if Assigned(GetDataEntryControl(Col)) then
      if GetDataEntryControl(Col) is TLinkedEditCol then
        if Assigned(TLinkedEditCol(GetDataEntryControl(Col)).GetDataMethod) then
          TLinkedEditCol(GetDataEntryControl(Col)).GetDataMethod;
end;

{-------------------------------------------------------------------------------
  Procedure that should be called when Return data functionality response is
     available.  Update is applied to the selected column/row
}
procedure TDataStringGrid.UpdateLinkedValue(const AText, AKey: string;
          ACol: integer=-1; ARow: integer=-1);
var
  lKeyDone: boolean;
  lRow, lCol: integer;
begin
  // if not specified, use the current cell
  if ACol=-1 then
    lCol := FGrid.Col
  else
    lCol := ACol;
  if ARow=-1 then
    lRow := FGrid.Row
  else
    lRow := ARow;

  with FGrid do begin
    TAccessorClass(FGrid).HideEditor;
    Cells[lCol, lRow] := AText;
    // Update the underlying key
    lKeyDone := False;
    if Assigned(Objects[lCol, lRow]) then
      if Objects[lCol, lRow] is TKey then begin
        TKey(Objects[lCol, lRow]).Key := AKey;
        lKeyDone := True;
      end;
    if not lKeyDone then
      CellLinkedKey[lCol, lRow] := AKey;
  end;
  TRowKey(FRowKeys[lRow-FGrid.FixedRows]).Edited := True;
end;

{-------------------------------------------------------------------------------
  If return key pressed, check if in linked edit then call Find method, else
      return false.
}
function TDataStringGrid.DoReturnKeyInLinkedEdit: boolean;
var
  lText, lKey: string;
begin
  Result := False;  // default not a linked edit
  with FGrid do
    if Assigned(GetDataEntryControl(Col)) then
      if GetDataEntryControl(Col) is TLinkedEditCol then
        if Assigned(TLinkedEditCol(GetDataEntryControl(Col)).FindDataMethod) then
        begin
          TLinkedEditCol(GetDataEntryControl(Col)).FindDataMethod(Cells[Col, Row], lText, lKey);
          Result := True;
          UpdateLinkedValue(lText, lKey);
        end;
end;

{-------------------------------------------------------------------------------
  Validate the content without doing anything
}
procedure TDataStringGrid.Validate;
var
  lRow: integer;
begin
  for lRow := FGrid.FixedRows to FGrid.RowCount-1 do
    ValidateRow(lRow);
end;

procedure TDataStringGrid.SetEnabled(const Value: Boolean);
var
  lDummy: Boolean;
begin
  FEnabled := Value;
  if not FEnabled then FGrid.Options := FGrid.Options - [goEditing];
  GridSelectCell(FGrid, FGrid.Col, FGrid.Row, lDummy);
end;

{-------------------------------------------------------------------------------
  Accessor, event triggered when a new blank row inserted
}
procedure TDataStringGrid.SetOnNewBlankRow(const Value: TNewBlankRowEvent);
begin
  FOnNewBlankRow := Value;
end;

{-------------------------------------------------------------------------------
  Add the key for a new blank row.  Also triggers an event allowing defaults
      to be set if required
}
procedure TDataStringGrid.AddBlankRowKey;
begin
  FRowKeys.Add(TRowKey.Create(''));
  if Assigned(FOnNewBlankRow) and FEnabled then
    FOnNewBlankRow(Self, FRowKeys.Count);
end;

{-------------------------------------------------------------------------------
  Accessor method.  If SiteID set, and Custodian present, then rows belonging
     to a different site are always locked.
}
procedure TDataStringGrid.SetSiteID(const Value: string);
begin
  FSiteID := Value;
end;

{-------------------------------------------------------------------------------
  Accessor.  Setting user ID and RestrictFullEdit enables the check so that
    users can only edit their own data, as long as the Entered_By field is
    present.
}
procedure TDataStringGrid.SetUserID(const Value: string);
begin
  FUserID := Value;
end;

{-------------------------------------------------------------------------------
  Accessor.  Setting user ID and RestrictFullEdit enables the check so that
    users can only edit their own data, as long as the Entered_By field is
    present.
}
procedure TDataStringGrid.SetRestrictFullEdit(const Value: boolean);
begin
  FRestrictFullEdit := Value;
end;

{-------------------------------------------------------------------------------
  Accessor method.  Controls if rows can be deleted
}
procedure TDataStringGrid.SetAddOnly(const Value: boolean);
begin
  FAddOnly := Value;
end;

{-------------------------------------------------------------------------------
  Retrieve a row index by its key, or -1 if not present
}
function TDataStringGrid.FindRowByKey(AKey: string): integer;
var
  i: integer;
begin
  Result := -1; // default not found
  for i := 1 to FGrid.RowCount-1 do
    if TRowKey(FRowKeys[i-FGrid.FixedRows]).Key = AKey then begin
      Result := i;
      break;
    end;
end;

{-------------------------------------------------------------------------------
  Retrieve a row index by its key in a particular column, or -1 if not present.
    The first match is returned.
}
function TDataStringGrid.FindRowByKeyInColumn(AKey: string; ACol: integer):
    integer;
var
  i: integer;
begin
  Result := -1; // default not found
  for i := 1 to FGrid.RowCount-1 do
    if FGrid.Objects[ACol, i] is TKey then begin
      if TKey(FGrid.Objects[ACol, i]).Key=AKey then begin
        Result := i;
        break;
      end
    end
    else if CellLinkedKey[ACol, i]=AKey then begin
      Result := i;
      break;
    end;
end;

{-------------------------------------------------------------------------------
  Retrieve the data entry control (e.g. combo or linked edit) for a column
}
function TDataStringGrid.GetDataEntryControl(ACol: integer): TObject;
var
  lCtrlIdx: integer;
begin
  lCtrlIdx := FDataEntryControls.IndexOf(IntToStr(ACol));
  if lCtrlIdx<>-1 then
    Result := FDataEntryControls.Objects[lCtrlIdx]
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------
  Accessor method.  Retrieve a key behind a cell in a linked column, or ''
}
function TDataStringGrid.GetCellLinkedKey(ACol, ARow: integer): string;
begin
  if FGrid.Objects[ACol, ARow] is TLinkedKey then
    Result := TLinkedKey(FGrid.Objects[ACol, ARow]).Key
  else
    Result := '';
end;

{-------------------------------------------------------------------------------
  Returns true if a row is locked
}
function TDataStringGrid.RowLocked(ARow: integer): Boolean;
begin
  if ARow-FGrid.FixedRows<FRowKeys.Count then
    Result := TRowKey(FRowKeys[ARow-FGrid.FixedRows]).Locked
  else
    // a new row with no rowkey yet, so not locked
    Result := False;
end;

{-------------------------------------------------------------------------------
  Accessor to update the linked key for a cell. Frees the object if not required
}
procedure TDataStringGrid.SetCellLinkedKey(ACol, ARow: integer; const Value:
    string);
begin
  if Value='' then begin
    if Assigned(FGrid.Objects[ACol, ARow]) then begin
      TObject(FGrid.Objects[ACol, ARow]).Free;
      FGrid.Objects[ACol, ARow] := nil;
    end;
  end
  else begin
    if not Assigned(FGrid.Objects[ACol, ARow]) then
      FGrid.Objects[ACol, ARow] := TLinkedKey.Create;
    TLinkedKey(FGrid.Objects[ACol, ARow]).Key := Value;
  end;
end;

{-------------------------------------------------------------------------------
  This method ensures that the Inplace edit control is created, allowing us
    to attached a linked edit button to the control on the right.
}
procedure TDataStringGrid.SetupInPlaceEditor;
var
  lWasAlwaysShowEditor: boolean;
  lWasEditing: boolean;
begin
  if not Assigned(InplaceEdit) then begin
    lWasAlwaysShowEditor := goAlwaysShowEditor in FGrid.Options;
    lWasEditing := goEditing in FGrid.Options;
    FGrid.Options := FGrid.Options + [goAlwaysShowEditor, goEditing];
    try
      FLinkedEditButtonEditor.Parent := InplaceEdit;
      FLinkedEditButtonEditor.Visible := False;
      FLinkedEditButtonEditor.Width := FGrid.DefaultRowHeight;
      FLinkedEditButtonEditor.Align := alRight;
      FLinkedEditButtonEditor.OnClick := GetButtonClick;
    finally
      if not lWasAlwaysShowEditor then
        FGrid.Options := FGrid.Options - [goAlwaysShowEditor];
      if not lWasEditing then
        FGrid.Options := FGrid.Options - [goEditing];
      TAccessorClass(FGrid).HideEditor;
    end;
    Exit; // the grid will invalidate and redraw, but now we have our editor
  end;
end;

end.
