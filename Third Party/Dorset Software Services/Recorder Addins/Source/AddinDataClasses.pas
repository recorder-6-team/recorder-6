{===============================================================================
  Unit:        AddinDataClasses

  Defines:

  Description: Used to be called LuxembourgDataClasses, but has been moved
                from the Luxembourg folder to the Addins folder.

  Model:       Addins.mpb

  Created:     September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 18/06/04 17:26 $
    $Author: Anthonysimpson $

===============================================================================}
unit AddinDataClasses;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, Contnrs,
  DataClasses, Grids, ADODB, ADOInt, ResourceStrings, DataTypes, Variants, DssStringGrid,
  ComboListID, LinkedControls, StdCtrls;

type
  EAddinCachedDataError = class (Exception)
  end;
  
  { Forward declaration so TDataItem can hold its owner }
  TAddinCachedDataList = class;

  TCustomGetData = procedure (WinControl: TWinControl; var Text, Key: String) of object;
  TCustomSetData = procedure (WinControl: TWinControl; const Text, Key: String) of object;
  {-----------------------------------------------------------------------------
    Abstract base class for data items used with the TAddinCachedDataList class.  This is used 
    when a list of items is populated on a form, which can be edited but must be cached until 
    the 'Save' or OK button is clicked.
  }
  TAddinCachedDataItem = class (TObject)
  private
    FAdded: Boolean;
    FCustodian: String;
    FDeleted: Boolean;
    FIgnore: Boolean;
    FItemKey: TKeyString;
    FModified: Boolean;
    FOwnerList: TAddinCachedDataList;
    FTimestamp: TSQLSvrTimestamp;
  protected
    procedure InitFromRecord(AFields: Fields); virtual; abstract;
    procedure SetDeleted(const Value: Boolean);
    procedure SetItemKey(const Value: TKeyString); virtual;
    procedure SetModified; virtual;
    procedure ValidateData; virtual;
    property Ignore: Boolean read FIgnore write FIgnore;
  public
    constructor CreateFromRecord(AOwner: TAddinCachedDataList; AFields: Fields); virtual;
    constructor CreateNew(AOwner: TAddinCachedDataList); virtual;
    property Added: Boolean read FAdded;
    property Custodian: String read FCustodian;
    property Deleted: Boolean read FDeleted write SetDeleted;
    property ItemKey: TKeyString read FItemKey;
    property Modified: Boolean read FModified;
    property OwnerList: TAddinCachedDataList read FOwnerList;
    property Timestamp: TSQLSvrTimestamp read FTimestamp;
  end;
  
  { Metaclass to identify the items so that the list class can create them }
  TItemClass = class of TAddinCachedDataItem;

  {-----------------------------------------------------------------------------
    Abstract base class for managing a list of TAddinCachedDataItems
  }
  TAddinCachedDataList = class (TObjectList)
  private
    FChanged: Boolean;
    FItemClass: TItemClass;
    FItemsToAdd: TList;
    FItemsToDelete: TList;
    FItemsToModify: TList;
    FMasterKey: TKeyString;
    function GetDataItems(const Key: TKeyString): TAddinCachedDataItem;
    function GetItemCount: Integer;
    function GetItems(Index: Integer): TAddinCachedDataItem;
    procedure PrepareUpdateLists;
    procedure SetLists;
  protected
    procedure AddToList(AItem: TAddinCachedDataItem; AIndex: integer); virtual;
    procedure DoAddition(AItem: TAddinCachedDataItem); virtual;
    procedure DoDeletion(AItem: TAddinCachedDataItem); virtual;
    procedure DoModification(AItem: TAddinCachedDataItem); virtual;
    function GetRecordset: _Recordset; virtual; abstract;
    procedure PopulateFromRecordset;
    property ItemsToAdd: TList read FItemsToAdd;
    property ItemsToDelete: TList read FItemsToDelete;
    property ItemsToModify: TList read FItemsToModify;
  public
    constructor Create(AItemClass: TItemClass); reintroduce; overload; virtual;
    destructor Destroy; override;
    procedure AddFromRecordset(AFields: Fields);
    procedure AddNew(AItem: TAddinCachedDataItem); virtual;
    procedure DeleteItem(const AIndex: integer); virtual;
    procedure Refresh; virtual;
    procedure Update; virtual;
    procedure ValidateContent; virtual;
    property Changed: Boolean read FChanged;
    property DataItems[const Key: TKeyString]: TAddinCachedDataItem read GetDataItems;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TAddinCachedDataItem read GetItems;
    property MasterKey: TKeyString read FMasterKey write FMasterKey;
  end;

  {-----------------------------------------------------------------------------
    Sub-class which keep the list in synch with an existing TStrings instance (eg on a listbox 
    component).
  }
  TAddinStringDataItem = class (TAddinCachedDataItem)
  protected
    procedure SetModified; override;
  end;
  
  {-----------------------------------------------------------------------------
    Sub-class which keep the list in synch with an existing TStrings instance (eg on a listbox 
    component). Also partly abstract.
  }
  TAddinStringDataList = class (TAddinCachedDataList)
  private
    FStrings: TStrings;
  protected
    procedure AddToList(AItem: TAddinCachedDataItem; AIndex: Integer); override;
    function GetText(AItem: TAddinCachedDataItem): String; virtual; abstract;
  public
    constructor Create(AItemClass: TItemClass; AStrings: TStrings); reintroduce; virtual;
    procedure DeleteItem(const AIndex: Integer); override;
    procedure Refresh; override;
    procedure RefreshItemDisplay(AItem: TAddinStringDataItem);
  end;
  
  {-----------------------------------------------------------------------------
    Sub-class which keep the list in synch with an existing TStringGrid component. Also partly 
    abstract.
  }
  TAddinGridDataItem = class (TAddinCachedDataItem)
  protected
    procedure GetData(const Column: Integer; var AText: String; var AKey: TKeyString); virtual;
        abstract;
    function GetDisplayText(Column: Integer): String; virtual;
    procedure SetData(const Column: Integer; const AText: String; const AKey: TKeyString); 
        virtual;
    procedure SetModified; override;
  public
    property DisplayText[Column: Integer]: String read GetDisplayText;
  end;
  
  {-----------------------------------------------------------------------------
    Sub-class which keep the list in synch with an existing TStringGrid component.
  }
  TAddinGridDataList = class (TAddinCachedDataList)
  private
    FGrid: TDSSStringGrid;
    FGridKeyDown: TKeyEvent;
    FGridRowMoved: TMovedEvent;
    FOnCustomGetData: TCustomGetData;
    FOnCustomSetData: TCustomSetData;
    procedure DoCustomGetData(AWinControl: TWinControl; var Text, Key: String);
    procedure DoCustomSetData(AWinControl: TWinControl; const Text, Key: String);
    function GetItemAtRow(const ARow: Integer): TAddinGridDataItem;
    procedure GridCellCustomKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState; 
        AWinControl: TWinControl);
    procedure GridCellLeaveCustom(Sender: TObject; ACol, ARow: Integer; WinControl: 
        TWinControl);
    procedure GridCellLeaveDefault(Sender: TObject; ACol, ARow: Integer; var Options:
        TGridOptions);
    procedure GridCellSelectedCustom(Sender: TObject; ACol, ARow: Integer; WinControl: 
        TWinControl);
    procedure GridCellSelectedDefault(Sender: TObject; ACol, ARow: Integer; var Options: 
        TGridOptions);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridRowMoved(Sender: TObject; FromIndex, ToIndex: Longint);
  protected
    procedure AddToList(AItem: TAddinCachedDataItem; AIndex: Integer); override;
    function AllowedAddOnKeyDown: Boolean; virtual;
    property Grid: TDSSStringGrid read FGrid;
  public
    constructor Create(AItemClass: TItemClass; AStringGrid: TDSSStringGrid); reintroduce; 
        virtual;
    destructor Destroy; override;
    procedure AddNew(AItem: TAddinCachedDataItem); override;
    procedure DeleteItem(const AIndex: integer); override;
    procedure Refresh; override;
    procedure RefreshItemDisplay(AItem: TAddinGridDataItem);
    procedure ValidateContent; override;
    property OnCustomGetData: TCustomGetData read FOnCustomGetData write FOnCustomGetData;
    property OnCustomSetData: TCustomSetData read FOnCustomSetData write FOnCustomSetData;
  end;
  
//==============================================================================
implementation

{-==============================================================================
    TAddinCachedDataItem
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TAddinCachedDataItem.CreateFromRecord(AOwner: TAddinCachedDataList; AFields: Fields);
begin
  inherited Create;
  FOwnerList := AOwner;
  // Not a newly created record
  FAdded := False;
  // Initialise it
  InitFromRecord(AFields);
  // It may happen this field is null (see metadata).
  FCustodian := VarToStr(AFields['Custodian'].Value);
  FTimestamp := AFields['Timestamp'].Value;
  FModified := False;
  FOwnerList.FChanged := True;
end;  // TAddinCachedDataItem.CreateFromRecord 

{-------------------------------------------------------------------------------
}
constructor TAddinCachedDataItem.CreateNew(AOwner: TAddinCachedDataList);
begin
  inherited Create;
  FOwnerList := AOwner;
  // A newly created record which we need to post into the dataset
  FAdded := True;
  FOwnerList.FChanged := True;
end;  // TAddinCachedDataItem.CreateNew 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataItem.SetDeleted(const Value: Boolean);
begin
  FDeleted := Value;
  // As both classes are in same unit, private fields can be accessed.
  FOwnerList.FChanged := True;
end;  // TAddinCachedDataItem.SetDeleted 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataItem.SetItemKey(const Value: TKeyString);
begin
  if FItemKey <> Value then begin
    FItemKey := Value;
    SetModified;
  end;
end;  // TAddinCachedDataItem.SetItemKey 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataItem.SetModified;
begin
  FModified := True;
  FOwnerList.FChanged := True;
end;  // TAddinCachedDataItem.SetModified 

{-------------------------------------------------------------------------------
  Override when and where necessary.
}
procedure TAddinCachedDataItem.ValidateData;
begin
end;  // TAddinCachedDataItem.ValidateData 

{-==============================================================================
    TAddinCachedDataList
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TAddinCachedDataList.Create(AItemClass: TItemClass);
begin
  inherited Create;
  
  FItemClass      := AItemClass;
  FChanged        := False;
end;  // TAddinCachedDataList.Create 

{-------------------------------------------------------------------------------
}
destructor TAddinCachedDataList.Destroy;
begin
  Clear;
  FItemsToAdd.Free;
  FItemsToModify.Free;
  FItemsToDelete.Free;
  inherited Destroy;
end;  // TAddinCachedDataList.Destroy 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataList.AddFromRecordset(AFields: Fields);
var
  lDataItem: TAddinCachedDataItem;
begin
  lDataItem := FItemClass.CreateFromRecord(Self, AFields);
  AddNew(lDataItem);
end;  // TAddinCachedDataList.AddFromRecordset 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataList.AddNew(AItem: TAddinCachedDataItem);
begin
  if AItem <> nil then
    AddToList(AItem, -1)
  else
    raise EDataListError.Create(ResStr_CannotAddNilItem);
end;  // TAddinCachedDataList.AddNew 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataList.AddToList(AItem: TAddinCachedDataItem; AIndex: integer);
begin
  if AIndex <> -1 then
    Insert(AIndex, AItem)
  else
    Add(AItem);
end;  // TAddinCachedDataList.AddToList 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataList.DeleteItem(const AIndex: integer);
begin
  TAddinCachedDataItem(Items[AIndex]).Deleted := True;
end;  // TAddinCachedDataList.DeleteItem 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataList.DoAddition(AItem: TAddinCachedDataItem);
begin
  // If sub-class need to save items, override this method.
end;  // TAddinCachedDataList.DoAddition 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataList.DoDeletion(AItem: TAddinCachedDataItem);
begin
  // If sub-class need to delete items, override this method.
end;  // TAddinCachedDataList.DoDeletion 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataList.DoModification(AItem: TAddinCachedDataItem);
begin
  // If sub-class need to modify items, override this method.
end;  // TAddinCachedDataList.DoModification 

{-------------------------------------------------------------------------------
}
function TAddinCachedDataList.GetDataItems(const Key: TKeyString): TAddinCachedDataItem;
var
  i: Integer;
begin
  Result := nil; // default value is not found
  for i := 0 to Count - 1 do
    if TAddinCachedDataItem(Items[i]).ItemKey = Key then begin
      Result := TAddinCachedDataItem(Items[i]);
      Exit; // from the loop
    end;
end;  // TAddinCachedDataList.GetDataItems 

{-------------------------------------------------------------------------------
}
function TAddinCachedDataList.GetItemCount: Integer;
var
  i, lTotal: Integer;
begin
  lTotal := 0;
  for i := 0 to Count - 1 do
    if not TAddinCachedDataItem(Items[i]).Deleted then Inc(lTotal);
  Result := lTotal;
end;  // TAddinCachedDataList.GetItemCount 

{-------------------------------------------------------------------------------
}
function TAddinCachedDataList.GetItems(Index: Integer): TAddinCachedDataItem;
begin
  Result := TAddinCachedDataItem(inherited Items[Index]);
end;  // TAddinCachedDataList.GetItems 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataList.PopulateFromRecordset;
var
  lDataItem: TAddinCachedDataItem;
begin
  { Get the list of items from the dataset }
  with GetRecordset do begin
    while not Eof do Begin
      lDataItem := FItemClass.CreateFromRecord(Self, Fields);
      AddToList(lDataItem, -1);
      MoveNext;
    end;
    Close;
  end;
end;  // TAddinCachedDataList.PopulateFromRecordset 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataList.PrepareUpdateLists;
begin
  if FItemsToAdd = nil then FItemsToAdd := TList.Create
                       else FItemsToAdd.Clear;
  
  if FItemsToModify = nil then FItemsToModify := TList.Create
                          else FItemsToModify.Clear;
  
  if FItemsToDelete = nil then FItemsToDelete := TList.Create
                          else FItemsToDelete.Clear;
end;  // TAddinCachedDataList.PrepareUpdateLists 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataList.Refresh;
begin
  //Free existing items
  Clear;
  FChanged := False;
  //Get new items
  PopulateFromRecordset;
end;  // TAddinCachedDataList.Refresh 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataList.SetLists;
var
  i: Integer;
  lItem: TAddinCachedDataItem;
begin
  PrepareUpdateLists;
  // Iterate through each item checking the state - what do we have to do?
  for i := 0 to Count - 1 do
  begin
    lItem := TAddinCachedDataItem(Items[i]);
    if (lItem.Added and lItem.Deleted) or (lItem.Ignore) then
      Continue;  // next iteration - this item never existed in the first place
  
    // Deletion has the highest priority!
    if lItem.Deleted then FItemsToDelete.Add(lItem) else
    // Addition before modification - as long as we add the most recent copy
    if lItem.Added then FItemsToAdd.Add(lItem) else
    // Finally modifications
    if lItem.Modified then FItemsToModify.Add(lItem);
  end; // for
end;  // TAddinCachedDataList.SetLists 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataList.Update;
var
  i: Integer;
  lCursor: TCursor;
begin
  lCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    // ValidateContent sets the lists.
    ValidateContent;
  
    // If we get here, all should be fine.
    for i := 0 to FItemsToAdd.Count -1 do
      DoAddition(FItemsToAdd[i]);
  
    for i := 0 to FItemsToModify.Count - 1 do
      DoModification(FItemsToModify[i]);
  
    for i := 0 to FItemsToDelete.Count - 1 do
      DoDeletion(FItemsToDelete[i]);
  
    // All items properly added/modified/deleted. So now we can get rid of the
    // deleted ones from the main list too. IndexOf uses objects in ObjectList.
    for i := FItemsToDelete.Count - 1 downto 0 do
      Delete(IndexOf(FItemsToDelete[i]));
  finally
    Screen.Cursor := lCursor;
  end; // finally
end;  // TAddinCachedDataList.Update 

{-------------------------------------------------------------------------------
}
procedure TAddinCachedDataList.ValidateContent;
var
  i: Integer;
begin
  SetLists;
  // First validate data for all items to save to the database.
  // Raise a TExceptionPath exception for invalid data.
  for i := 0 to FItemsToAdd.Count -1 do
    TAddinCachedDataItem(FItemsToAdd[i]).ValidateData;
  
  for i := 0 to FItemsToModify.Count - 1 do
    TAddinCachedDataItem(FItemsToModify[i]).ValidateData;
end;  // TAddinCachedDataList.ValidateContent 

{-==============================================================================
    TAddinStringDataList
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TAddinStringDataList.Create(AItemClass: TItemClass; AStrings: TStrings);
begin
  FStrings := AStrings;
  inherited Create(AItemClass);
end;  // TAddinStringDataList.Create 

{-------------------------------------------------------------------------------
}
procedure TAddinStringDataList.AddToList(AItem: TAddinCachedDataItem; AIndex: Integer);
var
  lIndex, lNewIndex: Integer;
  lNonDeletedItemsIndex, i: Integer;
begin
  { Add to the string list first so we can find out where it should go }
  lIndex := FStrings.AddObject(GetText(AItem), AItem);
  { Since FStrings.Count could be less than Items.Count we must check the Items array for
    items marked for deletion and ignore them. i.e deleting item 3 from an initial list of 5
    items will set FStrings.Count = 4 and Items.Count = 5 where TDataItem(Items[3]).Delete =
        true }
  lNonDeletedItemsIndex := -1;
  lNewIndex := -1;
  for i := 0 to Count - 1 do
    if not TAddinCachedDataItem(Items[i]).Deleted then
    begin
      Inc(lNonDeletedItemsIndex);
      if lNonDeletedItemsIndex = lIndex then lNewIndex := i;
    end;
  
  inherited AddToList(AItem, lNewIndex);
end;  // TAddinStringDataList.AddToList 

{-------------------------------------------------------------------------------
}
procedure TAddinStringDataList.DeleteItem(const AIndex: Integer);
var
  lDataItemIndex: Integer;
begin
  if AIndex > FStrings.Count - 1 then
    raise EDataListError.Create(Format(ResStr_ListIndexOutOfBound, [AIndex]));
  { Translate the string list item index to our object list item index }
  lDataItemIndex := IndexOf(FStrings.Objects[AIndex]);
  if lDataItemIndex = -1 then
    raise EDataListError.Create(ResStr_ObjectForItemNotFound);
  FStrings.Delete(AIndex);
  inherited DeleteItem(lDataItemIndex);
end;  // TAddinStringDataList.DeleteItem 

{-------------------------------------------------------------------------------
}
procedure TAddinStringDataList.Refresh;
begin
  FStrings.Clear;
  inherited;
end;  // TAddinStringDataList.Refresh 

{-------------------------------------------------------------------------------
}
procedure TAddinStringDataList.RefreshItemDisplay(AItem: TAddinStringDataItem);
var
  lIdx: Integer;
begin
  lIdx :=  FStrings.IndexOfObject(AItem);
  if lIdx <> -1 then
    FStrings[lIdx] := GetText(AItem);
end;  // TAddinStringDataList.RefreshItemDisplay 

{-==============================================================================
    TAddinStringDataItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TAddinStringDataItem.SetModified;
begin
  if not (FOwnerList is TAddinStringDataList) then
    raise EDataListError.Create(ResStr_OwnerNotStringDataList);
  inherited SetModified;
  TAddinStringDataList(FOwnerList).RefreshItemDisplay(Self);
end;  // TAddinStringDataItem.SetModified 

{-==============================================================================
    TAddinGridDataItem
===============================================================================}
{-------------------------------------------------------------------------------
}
function TAddinGridDataItem.GetDisplayText(Column: Integer): String;
var
  lText: String;
  lKey: TKeyString;
begin
  GetData(Column, lText, lKey);
  Result := lText;
end;  // TAddinGridDataItem.GetDisplayText 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataItem.SetData(const Column: Integer; const AText: String; const AKey: 
    TKeyString);
begin
  // Some grids can be always readonly, therefore removing the need for this method.
  // Override when appropriate.
end;  // TAddinGridDataItem.SetData 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataItem.SetModified;
begin
  if not (FOwnerList is TAddinGridDataList) then
    raise EDataListError.Create(ResStr_OwnerNotGridDataList);
  inherited SetModified;
  TAddinGridDataList(FOwnerList).RefreshItemDisplay(Self);
end;  // TAddinGridDataItem.SetModified 

{-==============================================================================
    TAddinGridDataList
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TAddinGridDataList.Create(AItemClass: TItemClass; AStringGrid: TDSSStringGrid);
begin
  FGrid := AStringGrid;
  if not Assigned(FGrid) then
    raise EAddinCachedDataError.Create(
        'GridDataList component requires reference to existing DssStringGrid control.');
  
  with FGrid do begin
    // Save user-defined event, before hijacking it.
    FGridKeyDown := OnKeyDown;
    FGridRowMoved := OnRowMoved;
    OnKeyDown := GridKeyDown;
    OnRowMoved := GridRowMoved;
    OnCellCustomKeyDown := GridCellCustomKeyDown;
    OnCellLeaveCustom := GridCellLeaveCustom;
    OnCellLeaveDefault := GridCellLeaveDefault;
    OnCellSelectedCustom := GridCellSelectedCustom;
    OnCellSelectedDefault := GridCellSelectedDefault;
  end;
  inherited Create(AItemClass);
end;  // TAddinGridDataList.Create 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataList.AddNew(AItem: TAddinCachedDataItem);
begin
  inherited AddNew(AItem);
  RefreshItemDisplay(TAddinGridDataItem(AItem));
end;  // TAddinGridDataList.AddNew 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataList.AddToList(AItem: TAddinCachedDataItem; AIndex: Integer);
var
  i, lDisplayed: Integer;
begin
  inherited AddToList(AItem, AIndex);
  { Add another one except for when top blank row is kept - we can't lose the
      very top row otherwise our fixed rows go wrong }
  // Get count of displayed items, so we can get the count of rows needed in the grid
  lDisplayed := Count;
  for i := 0 to Count - 1 do
    if TAddinCachedDataItem(Items[i]).Deleted then Dec(lDisplayed);
  
  with FGrid do begin
    if lDisplayed >= RowCount - FixedRows then RowCount := lDisplayed + FixedRows
                                          else RowCount := FixedRows + 1;
    { Populate last row }
    for i := 0 to ColCount - 1 do
      Cells[i, RowCount - 1] := TAddinGridDataItem(AItem).DisplayText[i];
    Rows[RowCount - 1].Objects[0] := AItem;
  end;
end;  // TAddinGridDataList.AddToList 

{-------------------------------------------------------------------------------
  Allow user to override default behaviour which is to add rows to linked grid when the Down 
      key is pressed.
}
function TAddinGridDataList.AllowedAddOnKeyDown: Boolean;
begin
  Result := True;
end;  // TAddinGridDataList.AllowedAddOnKeyDown 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataList.DeleteItem(const AIndex: integer);
var
  lDataItemIndex: Integer;
begin
  if AIndex > FGrid.RowCount - 1 then
    raise EDataListError.Create(Format(ResStr_ListIndexOutOfBound, [AIndex]));
  { Translate the string list item index to our object list item index - note
    we always store the object on the left most cell of the row (item 0) }
  lDataItemIndex := IndexOf(FGrid.Rows[AIndex].Objects[0]);
  if lDataItemIndex = -1 then
    raise EDataListError.Create(ResStr_ObjectForItemNotFound);
  { Select the row to delete }
  FGrid.RemoveRow(AIndex);
  inherited DeleteItem(lDataItemIndex);
end;  // TAddinGridDataList.DeleteItem 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataList.DoCustomGetData(AWinControl: TWinControl; var Text, Key: String);
begin
  if Assigned(FOnCustomGetData) then FOnCustomGetData(AWinControl, Text, Key);
end;  // TAddinGridDataList.DoCustomGetData 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataList.DoCustomSetData(AWinControl: TWinControl; const Text, Key: String);
begin
  if Assigned(FOnCustomSetData) then FOnCustomSetData(AWinControl, Text, Key);
end;  // TAddinGridDataList.DoCustomSetData 

{-------------------------------------------------------------------------------
  Ensure there is always an item returned. Create one if needed, but only if grid not in 
      ReadOnly mode.
}
function TAddinGridDataList.GetItemAtRow(const ARow: Integer): TAddinGridDataItem;
begin
  Result := TAddinGridDataItem(FGrid.Rows[ARow].Objects[0]);
  if (Result = nil) and not FGrid.ReadOnly then begin
    // Create new item
    Result := TAddinGridDataItem(FItemClass.CreateNew(Self));
    // Add it to list
    Add(Result);
    // Link it to grid.
    FGrid.Rows[ARow].Objects[0] := Result;
  end;
end;  // TAddinGridDataList.GetItemAtRow 

{-------------------------------------------------------------------------------
  Trap the Return key for Linked Edit controls if the key is empty, which means that the 
      content 
      hasn't been validated, by simply setting the Key parameter to zero to tell the grid it's 
      been handled.
}
procedure TAddinGridDataList.GridCellCustomKeyDown(Sender: TObject; var Key: Word; Shift: 
    TShiftState; AWinControl: TWinControl);
begin
  if not FGrid.ReadOnly then
    if AWinControl is TLinkedEdit then
      if (Key = VK_RETURN) and (TLinkedEdit(AWinControl).Key = '') then Key := 0;
end;  // TAddinGridDataList.GridCellCustomKeyDown 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataList.GridCellLeaveCustom(Sender: TObject; ACol, ARow: Integer; 
    WinControl: TWinControl);
var
  lItem: TAddinGridDataItem;
  lText, lKey: String;
begin
  if not FGrid.ReadOnly then begin
    lItem := GetItemAtRow(ARow);
    if not Assigned(lItem) then Exit;
  
    if WinControl is TLinkedEdit then
      lItem.SetData(ACol, TLinkedEdit(WinControl).Text, TLinkedEdit(WinControl).Key)
    else
    if WinControl is TIDComboBox then
      lItem.SetData(ACol, TIDComboBox(WinControl).CurrentItem,
                          TIDComboBox(WinControl).CurrentStrID)
    else begin
      // Grab data from custom control
      DoCustomGetData(WinControl, lText, lKey);
      //  Shove it in the grid/object/list.
      lItem.SetData(ACol, lText, lKey);
    end;
  end;
end;  // TAddinGridDataList.GridCellLeaveCustom 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataList.GridCellLeaveDefault(Sender: TObject; ACol, ARow: Integer; var 
    Options: TGridOptions);
begin
  if not FGrid.ReadOnly then
    if Assigned(GetItemAtRow(ARow)) then
      GetItemAtRow(ARow).SetData(ACol, FGrid.Cells[ACol, ARow], '');
end;  // TAddinGridDataList.GridCellLeaveDefault 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataList.GridCellSelectedCustom(Sender: TObject; ACol, ARow: Integer;
    WinControl: TWinControl);
var
  lText: String;
  lKey: TKeyString;
  lItemIndex: Integer;
begin
  if not Assigned(GetItemAtRow(ARow)) then Exit;
  GetItemAtRow(ARow).GetData(ACol, lText, lKey);
  
  if WinControl is TLinkedEdit then begin
    TLinkedEdit(WinControl).Key := lKey;
    TLinkedEdit(WinControl).Text := lText;
  end else
  if WinControl is TIDComboBox then
    with TIDComboBox(WinControl) do
      // DropDownList 'should' have the item, unless it hasn't yet been populated!
      // Or not DropDownList or DropDown, but item has a key, so look for it.
      if (Style in [csDropDownList, csDropDown]) or (lKey <> '') then
        if not Populated then begin
          // Not populated yet, so clear previous single item (if any) and
          // add the new one instead, and select it.
          Clear;
          ItemIndex := Add(lText, lKey);
        end else begin
          // Combo already populated, item may be in the list if it was one
          // of the 10 most recently used terms.
          lItemIndex := IndexOf(lText);
          // Item in the combo box list so can use the text and the key.
          if lItemIndex <> -1 then
            ItemIndex := IndexOf(lText)
          // Item not in combo box list, so just set the text.
          else
            Text := lText;
        end
      else
        // Not DropDownList or DropDown and no key, set Text property directly.
        Text := lText
  else
    // Send data to custom control
    DoCustomSetData(WinControl, lText, lKey);
end;  // TAddinGridDataList.GridCellSelectedCustom 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataList.GridCellSelectedDefault(Sender: TObject; ACol, ARow: Integer; var 
    Options: TGridOptions);
var
  lText: String;
  lKey: TKeyString;
begin
  if not Assigned(GetItemAtRow(ARow)) then Exit;
  // Key not used in this case, but it is a VAR param in method.
  GetItemAtRow(ARow).GetData(ACol, lText, lKey);
  FGrid.Cells[ACol, ARow] := lText;
end;  // TAddinGridDataList.GridCellSelectedDefault 

{-------------------------------------------------------------------------------
  Allow the list to react to key presses happening on the grid.
}
procedure TAddinGridDataList.GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lOptions: TGridOptions;
begin
  // List hijacked grid event, so call user-defined one, if any.
  if Assigned(FGridKeyDown) then FGridKeyDown(Sender, Key, Shift);
  
  if not FGrid.ReadOnly then
    case Key of
      VK_DOWN:
          // On last row, therefore, add new one.
          if (FGrid.Row = FGrid.RowCount - 1) and AllowedAddOnKeyDown then
            AddNew(FItemClass.CreateNew(Self));
  
      VK_ESCAPE:
          with FGrid do
            if ColumnsInfo[Col].ColumnType = ctDefault then
              GridCellSelectedDefault(nil, Col, Row, lOptions)
            else begin
              GridCellSelectedCustom(nil, Col, Row, ColumnsInfo[Col].WinControl);
              ColumnsInfo[Col].WinControl.SetFocus;
              Key := 0;
            end;
    end;
end;  // TAddinGridDataList.GridKeyDown 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataList.GridRowMoved(Sender: TObject; FromIndex, ToIndex: Longint);
var
  Obj: TObject;
  OwnedObjects: Boolean;
begin
  // List hijacked grid event, so call user-defined one, if any.
  if Assigned(FGridRowMoved) then FGridRowMoved(Sender, FromIndex, ToIndex);
  
  Obj := Items[FromIndex - Grid.FixedRows];
  OwnedObjects := OwnsObjects;
  
  OwnsObjects := False;
  try
    Delete(FromIndex - Grid.FixedRows);
    Insert(ToIndex - Grid.FixedRows, Obj);
  finally
    OwnsObjects := OwnedObjects;
  end;
end;  // TAddinGridDataList.GridRowMoved 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataList.Refresh;
begin
  //Clear grid
  with FGrid do begin
    RowCount := FixedRows + 1;
    Rows[RowCount - 1].CommaText := '';
  end;
  inherited;
end;  // TAddinGridDataList.Refresh 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataList.RefreshItemDisplay(AItem: TAddinGridDataItem);
var
  i, lIndex, lRow: Integer;
begin
  lIndex := IndexOf(AItem);
  if lIndex <> -1 then begin
    // Set row index as if nothing had been deleted
    lRow := lIndex + FGrid.FixedRows;
    // Decrease by one for each deleted item before current item. As the deleted
    // items don't appear in the grid, there is a misalignment to deal with
    // But a row might be emptied without it being removed from the grid though.
    for i := lIndex downto 0 do
      if TAddinGridDataItem(Items[i]).Deleted and
         (FGrid.Objects[0, lRow] <> Items[i]) then
        Dec(lRow);
    // Now proceed with the proper row for the item
    for i := 0 to FGrid.ColCount - 1 do
      FGrid.Cells[i, lRow] := AItem.DisplayText[i];
  end;
end;  // TAddinGridDataList.RefreshItemDisplay 

{-------------------------------------------------------------------------------
}
procedure TAddinGridDataList.ValidateContent;
var
  lRow: Integer;
  lCol: Integer;
  lItem: TAddinGridDataItem;
begin
  // Do this to trick the grid. That's for when custom controls are still visible
  // and list is going to be saved, and value in control not updated in list item.
  FGrid.ReadOnly := FGrid.ReadOnly;
  
  // And find out if some items should be completely ignored.
  // Especially for first row in empty grid...
  for lRow := FGrid.FixedRows to FGrid.RowCount - 1 do begin
    lItem := GetItemAtRow(lRow);
    if not Assigned(lItem) then Continue;
    // Assume empty row
    lItem.Ignore := True;
    // Now work out if it is really empty
    for lCol := FGrid.FixedCols to FGrid.ColCount - 1 do
      if FGrid.Cells[lCol, lRow] <> '' then begin
        // Wasn't empty after all...
        lItem.Ignore := False;
        // Stop inner loop to move on to next row.
        Break;
      end;
    // If item was added and cleared, ignoring it is enough.
    // If item came from database, clearing equals deleting it, so flip the switches.
    if lItem.Ignore and not lItem.Added then begin
      lItem.Ignore := False;
      lItem.Deleted := True;
    end;
  end;
  
  inherited ValidateContent;
end;  // TAddinGridDataList.ValidateContent

destructor TAddinGridDataList.Destroy;
begin
  with FGrid do begin
    // Restore events before destruction.
    OnKeyDown := FGridKeyDown;
    OnRowMoved := FGridRowMoved;
  end;
  inherited;
end;

end.
