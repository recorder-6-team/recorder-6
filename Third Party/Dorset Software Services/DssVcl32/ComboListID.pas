{===============================================================================

  Copyright © Dorset Software Services Ltd, 1998

  Component:
    TIDComboBox - 01 July 1998
    TIDListBox - 01 July 1998

  Updates:
    TIDComboBox - Eric Salmon 01 August 2003
    TIDListBox - Eric Salmon 14 March 2000

  Packages:
    InHouse5, Delphi 5 package for in house components.
    InHouse7, Delphi 7 package

  Description:
    TIDComboBox:
      The component can now be used with Integer or String IDs. The type of ID
      stored is determined by the first item added. If the first item's ID is a
      String, all IDs must be strings, and similarily with integers.
      Clear resets the ID type to idfUnknown, so you can switch between sources
      having different ID types that way.
      Added ReadOnly property.

    TIDListBox
      The component can now be used with Integer or String IDs. The type of ID
      stored is determined by the first item added. If the first item's ID is a
      String, all IDs must be strings, and similarily with integers.
      Clear resets the ID type to idfUnknown, so you can switch between sources
      having different ID types that way.

  Additional information:
    To be able to remove Items form the visible properties, the components need
    to descend from the TCustomXXX ones.
    To leave as much functionalities available as possible, all published events
    and properties are also published here, except for Items, which is the
    reason for inheriting from TCustomXXX in the first place.

===============================================================================}
{$I DelphiVersions.inc}

unit ComboListID;

interface

uses
  SysUtils, Windows, Messages, Classes, Controls, StdCtrls, Dialogs;

type
  EInvalidFormat = class(Exception);

  TIDFormat = (idfUnknown, idfString, idfLongint);

  // Object to store an ID with a sting. If the ID is a longint, it's changed
  // to its String representation. It can be recalled as either that String or
  // as the longint it was. If the value is a String and a longint is requested,
  // an EConvertError exception occurs.
  TID = class
  private
    FValue:String;
    function GetIntValue: Longint;
    procedure SetIntValue(const Value: Longint);
  public
    property StrValue:String read FValue write FValue;
    property IntValue:Longint read GetIntValue write SetIntValue;
  end;

  //----------------------------------------------------------------------------
  // Inherit from TCustomListBox to remove Items from Published properties
  TIDListBox = class(TCustomListBox)
  private
    FSaveItems: TList;
    FIDFormat : TIDFormat;
    function GetCurrentItem: String;
    function GetCurrentIntID: Longint;
    function GetCurrentStrID: String;
    function GetItem(Index: Integer): String;
    function GetIntID(Index: Integer): Longint;
    function GetStrID(Index: Integer): String;
    function GetCount: Integer; reintroduce;
    function GetID(Index: Integer): TID;
    procedure SetItem(Index: Integer; const Value: String);
    procedure SetIntID(Index: Integer; const Value: Longint);
    procedure SetStrID(Index: Integer; const Value: String);
    procedure ClearAll;
    property IDs[Index: Integer]: TID read GetID;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IndexOf(const AString: String): Integer;
    function IDIndexOf(const AValue: Longint): Integer; overload;
    function IDIndexOf(const AValue: String): Integer; overload;
    function Add(const NewItem: String; const NewID: Longint): Integer; overload;
    function Add(const NewItem: String; const NewID: String): Integer; overload;
    procedure Delete(const Index: Integer);
    procedure Clear; reintroduce;
    property IDFormat: TIDFormat read FIDFormat;
    property CurrentItem: String read GetCurrentItem;
    property CurrentIntID: Longint read GetCurrentIntID;
    property CurrentStrID: String read GetCurrentStrID;
    property Count: Integer read Getcount;
    property Items[Index: Integer]: String read GetItem write SetItem;
    property IntID[Index: Integer]: Longint read GetIntID write SetIntID;
    property StrID[Index: Integer]: String read GetStrID write SetStrID;
  published
  {$IFDEF DELPHI7UP}
    property AutoComplete;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
  {$ENDIF}
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;  // TIDListBox

  //----------------------------------------------------------------------------
  // Inherit from TCustomComboBox to remove Items from Published properties
  TIDComboBox= class(TCustomComboBox)
  private
    FSaveItems: TList;
    FIDFormat : TIDFormat;
    FReadOnly : Boolean;
    FPopulated: Boolean;
    FPopulating: Boolean;
    FOnPopulate: TNotifyEvent;
    FSelectedKey : String;
    FSelectedText: String;
    function GetCurrentItem: String;
    function GetCurrentIntID: Longint;
    function GetCurrentStrID: String;
    function GetItem(Index: Integer): String;
    function GetIntID(Index: Integer): Longint;
    function GetStrID(Index: Integer): String;
    function GetCount: Integer; reintroduce;
    function GetID(Index: Integer): TID;
    procedure SetItem(Index: Integer; const Value: String);
    procedure SetIntID(Index: Integer; const Value: Longint);
    procedure SetStrID(Index: Integer; const Value: String);
    procedure ClearAll;
    property IDs[Index: Integer]: TID read GetID;
  protected
    procedure CloseUp; override;
    procedure DoPopulate; virtual;
    procedure DropDown; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetReadOnly(const Value: Boolean); virtual;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(const NewItem: String; const NewID: Longint): Integer; overload;
    function Add(const NewItem: String; const NewID: String): Integer; overload;
    procedure Clear; reintroduce;
    procedure Delete(const Index: Integer);
    function IDIndexOf(const AValue: Longint): Integer; overload;
    function IDIndexOf(const AValue: String): Integer; overload;
    function IndexOf(const AString: String): Integer;
    procedure Insert(AIndex: integer; const NewItem: String; const NewID: Longint); overload;
    procedure Insert(AIndex: integer; const NewItem: String; const NewID: String); overload;
    procedure PopulateContent;
    property IDFormat: TIDFormat read FIDFormat;
    property CurrentItem: String read GetCurrentItem;
    property CurrentIntID: Longint read GetCurrentIntID;
    property CurrentStrID: String read GetCurrentStrID;
    property Count: Integer read Getcount;
    property Items[Index: Integer]: String read GetItem write SetItem;
    property IntID[Index: Integer]: Longint read GetIntID write SetIntID;
    property StrID[Index: Integer]: String read GetStrID write SetStrID;
    property Populated: Boolean read FPopulated;   
  published
  {$IFDEF DELPHI7UP}
    property AutoComplete default True;
    property AutoDropDown default False;
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
  {$ENDIF}
    property Style;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDock;
    property OnStartDrag;
    property OnPopulate: TNotifyEvent read FOnPopulate write FOnPopulate;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
  end;  // TIDComboBox

//==============================================================================
implementation

const
  I_UNASSIGNED = 0;  // ID Format decided when first ID stored
  I_STRING     = 1;  // ID is of String format
  I_LONGINT    = 2;  // ID is of longint format

resourcestring
  ResStr_InvalidFormat    = 'Invalid ID format: %s - Expected: %s';
  ResStr_IndexOutOfBounds = 'List index out of bounds (%d)';
  ResStr_NoLongIntFormat  = 'Invalid format request - Available: String';
  ResStr_InvalidInteger   = 'ID ''%s'' is not a valid Integer.';

//==============================================================================
{ TID }
function TID.GetIntValue: Longint;
begin
  try
    Result := StrToInt(FValue);
  except
    raise EConvertError.Create(Format(ResStr_InvalidInteger, [FValue]));
  end;
end;  // TID.GetIntValue

//------------------------------------------------------------------------------
procedure TID.SetIntValue(const Value: Longint);
begin
  FValue := IntToStr(Value);
end;  // TID.SetIntValue

//==============================================================================
//==============================================================================
{ TIDListBox }
constructor TIDListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIDFormat := idfUnknown;  // Unspecified yet
  FSaveItems := TList.Create;
end;  // TIDListBox.Create

//------------------------------------------------------------------------------
destructor TIDListBox.Destroy;
begin
  ClearAll;
  FSaveItems.Free;
  inherited Destroy;
end;  // TIDListBox.Destroy

//------------------------------------------------------------------------------
function TIDListBox.IndexOf(const AString: String): Integer;
begin
  Result := inherited Items.IndexOf(AString);
end;  // TIDListBox.IndexOf

//------------------------------------------------------------------------------
function TIDListBox.IDIndexOf(const AValue: Longint): Integer;
var lIDx:Integer;
begin
  // Default result, in case there is nothing
  Result := -1;
  if Count>0 then begin
    // Check it's the right format
    if IDFormat<>idfLongint then
      raise EInvalidFormat.CreateFmt(ResStr_InvalidFormat,['Longint','String']);
    // Find the ID and return the Index
    for lIdx := 0 to Count-1 do
      if TID(inherited Items.Objects[lIdx]).IntValue=AValue then begin
        Result := lIdx;
        Break;
      end;
  end;
end;  // TIDListBox.IDIndexOf (Longint)

//------------------------------------------------------------------------------
function TIDListBox.IDIndexOf(const AValue: String): Integer;
var lIDx:Integer;
begin
  // Default result, in case there is nothing
  Result := -1;
  if Count>0 then begin
    // No need to check it's the right format in case of a String
    // Find the ID and return the Index
    for lIdx := 0 to Count-1 do
      if TID(inherited Items.Objects[lIdx]).StrValue=AValue then begin
        Result := lIdx;
        Break;
      end;
  end;
end;  // TIDListBox.IDIndexOf (String)

//------------------------------------------------------------------------------
// Store NewID as Longint, and set FIDFormat to idfLongint.
// An exception is raised if IDFormat is already set to idfString.
function TIDListBox.Add(const NewItem: String; const NewID: Longint): Integer;
var lData:TID;
begin
  if IDFormat in [idfUnknown, idfLongint] then begin
    FIDFormat := idfLongint;
    lData := TID.Create;
    lData.IntValue := NewID;
    try
      Result := inherited Items.AddObject(NewItem,lData);
      FSaveItems.Add(lData);  // Only need the reference to the object
    except
      lData.Free;
      raise;
    end;
  end else
    raise EInvalidFormat.CreateFmt(ResStr_InvalidFormat,['Longint','String']);
end;  // TIDListBox.Add (Longint)

//------------------------------------------------------------------------------
// Store NewID as String, and set FIDFormat to idfString.
// An exception is raised if IDFormat is already set to idfLongint.
function TIDListBox.Add(const NewItem: String; const NewID: String): Integer;
var lData:TID;
begin
  if IDFormat in [idfUnknown, idfString] then begin
    FIDFormat := idfString;
    lData := TID.Create;
    lData.StrValue := NewID;
    try
      Result := inherited Items.AddObject(NewItem,lData);
      FSaveItems.Add(lData);  // Only need the reference to the object
    except
      lData.Free;
      raise;
    end;
  end else
    raise EInvalidFormat.CreateFmt(ResStr_InvalidFormat,['String','Longint']);
end;  // TIDListBox.Add (String)

//------------------------------------------------------------------------------
procedure TIDListBox.Delete(const Index: Integer);
var lIdx:Integer;
begin
  with inherited Items do begin
    if (Index <0) or (Index > Count-1) then
      raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
    // Find item in FSaveItems
    lIdx := FSaveItems.IndexOf(Objects[Index]);
    Objects[Index].Free;
    Objects[Index] := nil;
    Delete(Index);
    // Cleanup FSaveItems, to keep in synch.
    FSaveItems.Items[lIdx] := nil;
    FSaveItems.Delete(lIdx);
  end;
end;  // TIDListBox.Delete

//------------------------------------------------------------------------------
procedure TIDListBox.ClearAll;
var lIdx:Integer;
begin
  // Clear all objects, using the references stored in FSaveItems
  // as accessing Items when destroying causes BIG problems.
  with FSaveItems do begin
    for lIdx := 0 to Count-1 do begin
      TID(Items[lIdx]).Free;
      Items[lIdx] := nil;
    end;
    Clear;
  end;
end;  // TIDListBox.Clear

//------------------------------------------------------------------------------
procedure TIDListBox.Clear;
begin
  // Clear objects
  ClearAll;
  // Clear visible list
  inherited Items.Clear;
end;  // TIDListBox.Clear

//------------------------------------------------------------------------------
function TIDListBox.GetCurrentItem: String;
begin
  if ItemIndex = -1 then Result := ''
                    else Result := Items[ItemIndex];
end;  // TIDListBox.GetCurrentItem

//------------------------------------------------------------------------------
function TIDListBox.GetCurrentIntID: Longint;
begin
  // If IDs are strings, can't return a Integer, so raise an exception.
  // It's the programmer's responsibility to access the IDs using the correct
  // property
  if IDFormat<>idfLongint then
    raise EInvalidFormat.Create(ResStr_NoLongIntFormat);

  if ItemIndex=-1 then Result := 0
                  else Result := IDs[ItemIndex].IntValue;
end;  // TIDListBox.GetCurrentIntID

//------------------------------------------------------------------------------
function TIDListBox.GetCurrentStrID: String;
begin
  // Value is store as String internally, so if ID is longint return its String
  // representation.
  if ItemIndex=-1 then Result := ''
                  else Result := IDs[ItemIndex].StrValue
end;  // TIDListBox.GetCurrentStrID

//------------------------------------------------------------------------------
function TIDListBox.GetItem(Index: Integer): String;
begin
  if (Index <0) or (Index > Count-1) then
    raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
  Result := inherited Items[Index];
end;  // TIDListBox.GetItem

//------------------------------------------------------------------------------
procedure TIDListBox.SetItem(Index: Integer; const Value: String);
begin
  if (Index <0) or (Index > Count-1) then
    raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
  inherited Items[Index] := Value;
end;  // TIDListBox.SetItem

//------------------------------------------------------------------------------
function TIDListBox.GetIntID(Index: Integer): Longint;
begin
  if (Index <0) or (Index > Count-1) then
    raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
  if IDFormat<>idfLongint then
    raise EInvalidFormat.Create(ResStr_NoLongIntFormat);

  Result := IDs[Index].IntValue;
end;  // TIDListBox.GetIntID

//------------------------------------------------------------------------------
procedure TIDListBox.SetIntID(Index: Integer; const Value: Longint);
begin
  if (Index <0) or (Index > Count-1) then
    raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
  if IDFormat<>idfLongint then
    raise EInvalidFormat.Create(ResStr_NoLongIntFormat);
  IDs[Index].IntValue := Value;
end;  // TIDListBox.SetIntID

//------------------------------------------------------------------------------
function TIDListBox.GetStrID(Index: Integer): String;
begin
  if (Index <0) or (Index > Count-1) then
    raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
  Result := IDs[Index].StrValue;
end;  // TIDListBox.GetStrID

//------------------------------------------------------------------------------
procedure TIDListBox.SetStrID(Index: Integer; const Value: String);
begin
  if (Index <0) or (Index > Count-1) then
    raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
  IDs[Index].StrValue := Value;
end;  // TIDListBox.SetStrID

//------------------------------------------------------------------------------
function TIDListBox.GetCount: Integer;
begin
  Result := inherited Items.Count;
end;  // TIDListBox.GetCount

//------------------------------------------------------------------------------
function TIDListBox.GetID(Index: Integer): TID;
begin
  if (Index <0) or (Index > Count-1) then
    raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
  Result := TID(inherited Items.Objects[Index]);
end;  // TIDListBox.GetID

//==============================================================================
//==============================================================================
constructor TIDComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIDFormat := idfUnknown;  // Unspecified yet
  FSaveItems := TList.Create;
  FPopulated := False;
  FReadOnly := False;
end;  // TIDComboBox.Create

//------------------------------------------------------------------------------
destructor TIDComboBox.Destroy;
begin
  ClearAll;
  FSaveItems.Free;
  inherited Destroy;
end;  // Destroy

//------------------------------------------------------------------------------
function TIDComboBox.IndexOf(const AString: String): Integer;
begin
  Result := inherited Items.IndexOf(AString);
end;  // TIDComboBox.IndexOf

//------------------------------------------------------------------------------
function TIDComboBox.IDIndexOf(const AValue: Longint): Integer;
var lIDx: Integer;
begin
  // Default result, in case there is nothing
  Result := -1;
  if Count > 0 then begin
    // Check it's the right format
    if IDFormat <> idfLongint then
      raise EInvalidFormat.CreateFmt(ResStr_InvalidFormat, ['Longint', 'String']);
    // Find the ID and return the Index
    for lIdx := 0 to Count - 1 do
      if TID(inherited Items.Objects[lIdx]).IntValue = AValue then begin
        Result := lIdx;
        Break;
      end;
  end;
end;  // TIDComboBox.IDIndexOf (Longint)

//------------------------------------------------------------------------------
function TIDComboBox.IDIndexOf(const AValue: String): Integer;
var lIDx: Integer;
begin
  // Default result, in case there is nothing
  Result := -1;
  if Count > 0 then begin
    // No need to check it's the right format in case of a String
    // Find the ID and return the Index
    for lIdx := 0 to Count - 1 do
      if TID(inherited Items.Objects[lIdx]).StrValue = AValue then begin
        Result := lIdx;
        Break;
      end;
  end;
end;  // TIDComboBox.IDIndexOf (String)

//------------------------------------------------------------------------------
// Store NewID as Longint, and set FIDFormat to idfLongint.
// An exception is raised if IDFormat is already set to idfString.
function TIDComboBox.Add(const NewItem: String; const NewID: Longint): Integer;
var lData: TID;
    lIdx : Integer;
begin
  if IDFormat in [idfUnknown, idfLongint] then begin
    FIDFormat := idfLongint;

    // Check if the item AND key are already in the list. If so, ignore duplicate.
    lIdx := IndexOf(NewItem);
    if (lIdx > -1) and (TID(inherited Items.Objects[lIdx]).IntValue = NewID) then
      Result := lIdx
    else begin
      // New item to add.
      lData := TID.Create;
      lData.IntValue := NewID;
      try
        Result := inherited Items.AddObject(NewItem,lData);
        FSaveItems.Add(lData);  // Only need the reference to the object
      except
        lData.Free;
        raise;
      end;
    end;
  end else
    raise EInvalidFormat.CreateFmt(ResStr_InvalidFormat,['Longint', 'String']);
end;  // TIDComboBox.Add (Longint)

//------------------------------------------------------------------------------
// Store NewID as String, and set FIDFormat to idfString.
// An exception is raised if IDFormat is already set to idfLongint.
function TIDComboBox.Add(const NewItem: String; const NewID: String): Integer;
var lData: TID;
    lIdx : Integer;
begin
  if IDFormat in [idfUnknown, idfString] then begin
    FIDFormat := idfString;

    // Check if the item AND key are already in the list. If so, ignore duplicate.
    lIdx := IndexOf(NewItem);
    if (lIdx > -1) and (TID(inherited Items.Objects[lIdx]).StrValue = NewID) then
      Result := lIdx
    else begin
      // New item to add.
      lData := TID.Create;
      lData.StrValue := NewID;
      try
        Result := inherited Items.AddObject(NewItem,lData);
        FSaveItems.Add(lData);  // Only need the reference to the object
      except
        lData.Free;
        raise;
      end;
    end;
  end else
    raise EInvalidFormat.CreateFmt(ResStr_InvalidFormat,['String', 'Longint']);
end;  // TIDComboBox.Add (String)

//------------------------------------------------------------------------------
procedure TIDComboBox.Delete(const Index: Integer);
var lIdx:Integer;
begin
  with inherited Items do begin
    if (Index <0) or (Index > Count-1) then
      raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
    // Find item in FSaveItems
    lIdx := FSaveItems.IndexOf(Objects[Index]);
    Objects[Index].Free;
    Objects[Index] := nil;
    Delete(Index);
    // Cleanup FSaveItems, to keep in synch.
    FSaveItems.Items[lIdx] := nil;
    FSaveItems.Delete(lIdx);
  end;
end;  // TIDComboBox.Delete

//------------------------------------------------------------------------------
procedure TIDComboBox.ClearAll;
var lIdx:Integer;
begin
  // Clear all objects, using the references stored in FSaveItems
  // as accessing Items when destroying causes BIG problems.
  with FSaveItems do begin
    for lIdx := 0 to Count-1 do begin
      TID(Items[lIdx]).Free;
      Items[lIdx] := nil;
    end;
    Clear;
  end;
end;  // TIDComboBox.ClearAll

//------------------------------------------------------------------------------
procedure TIDComboBox.Clear;
begin
  // Clear objects
  ClearAll;
  // Clear visible list
  inherited Items.Clear;
  FPopulated := False;
  if Assigned(OnChange) then
    OnChange(Self);
end;  // TIDComboBox.Clear;

//------------------------------------------------------------------------------
function TIDComboBox.GetCurrentItem: String;
begin
  // Same function as the one used in TIDListBox, but this one is much simpler.
  Result := Text;
end;  // TIDComboBox.GetCurrentItem

//------------------------------------------------------------------------------
function TIDComboBox.GetCurrentIntID: Longint;
begin
  // If IDs are strings, can't return a Integer, so raise an exception.
  // It's the programmer's responsibility to access the IDs using the correct
  // property
  if IDFormat<>idfLongint then
    raise EInvalidFormat.Create(ResStr_NoLongIntFormat);

  // If dropdown, text can be anything not in list, therefore key value may not exist for it.
  if Style = csDropDown then begin
    if IndexOf(Text) = -1 then
      Result := 0
    else
      Result := IDs[IndexOf(Text)].IntValue;
  end else
  if ItemIndex=-1 then Result := 0
                  else Result := IDs[ItemIndex].IntValue;
end;  // TIDComboBox.GetCurrentIntID

//------------------------------------------------------------------------------
function TIDComboBox.GetCurrentStrID: String;
begin
  // Value is store as String internally, so if ID is longint return its String
  // representation.
  // If dropdown, text can be anything not in list, therefore key value may not exist for it.
  if Style = csDropDown then begin
    if IndexOf(Text) = -1 then
      Result := ''
    else
      Result := IDs[IndexOf(Text)].StrValue;
  end else
  if ItemIndex=-1 then Result := ''
                  else Result := IDs[ItemIndex].StrValue
end;  // TIDComboBox.GetCurrentStrID

//------------------------------------------------------------------------------
function TIDComboBox.GetItem(Index: Integer): String;
begin
  if (Index < 0) or (Index > Count - 1) then
    raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
  Result := inherited Items[Index];
end;  // TIDComboBox.GetItem

//------------------------------------------------------------------------------
procedure TIDComboBox.SetItem(Index: Integer; const Value: String);
begin
  if (Index < 0) or (Index > Count - 1) then
    raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
  inherited Items[Index] := Value;
end;  // TIDComboBox.SetItem

//------------------------------------------------------------------------------
function TIDComboBox.GetIntID(Index: Integer): Longint;
begin
  if (Index < 0) or (Index > Count - 1) then
    raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
  if IDFormat<>idfLongint then
    raise EInvalidFormat.Create(ResStr_NoLongIntFormat);

  Result := IDs[Index].IntValue;
end;  // TIDComboBox.GetIntID

//------------------------------------------------------------------------------
procedure TIDComboBox.SetIntID(Index: Integer; const Value: Longint);
begin
  if (Index < 0) or (Index > Count - 1) then
    raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
  if IDFormat<>idfLongint then
    raise EInvalidFormat.Create(ResStr_NoLongIntFormat);
  IDs[Index].IntValue := Value;
end;  // TIDComboBox.SetIntID

//------------------------------------------------------------------------------
function TIDComboBox.GetStrID(Index: Integer): String;
begin
  if (Index < 0) or (Index > Count - 1) then
    raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
  Result := IDs[Index].StrValue;
end;  // TIDComboBox.GetStrID

//------------------------------------------------------------------------------
procedure TIDComboBox.SetStrID(Index: Integer; const Value: String);
begin
  if (Index < 0) or (Index > Count - 1) then
    raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
  IDs[Index].StrValue := Value;
end;  // TIDComboBox.SetStrID

//------------------------------------------------------------------------------
function TIDComboBox.GetCount: Integer;
begin
  Result := inherited Items.Count;
end;  // TIDComboBox.GetCount

//------------------------------------------------------------------------------
function TIDComboBox.GetID(Index: Integer): TID;
begin
  if (Index < 0) or (Index > Count - 1) then
    raise EListError.CreateFmt(ResStr_IndexOutOfBounds,[Index]);
  Result := TID(inherited Items.Objects[Index]);
end;  // TIDComboBox.GetID

//------------------------------------------------------------------------------
procedure TIDComboBox.SetReadOnly(const Value: Boolean);
begin
  if ReadOnly <> Value then begin
    FReadOnly := Value;
    // Set the edit box part to readonly, or not.
    // Useful thing to know: Ord(False) = 0 and Ord(True) = 1
    SendMessage(GetWindow(Self.Handle, GW_CHILD), EM_SETREADONLY, Ord(FReadOnly), 0)
  end;
end;  //  TIDComboBox.SetReadOnly

//------------------------------------------------------------------------------
procedure TIDComboBox.PopulateContent;
var lItem, lKey: String;
begin
  if not FPopulated then begin
    FPopulating := True;
    // Don't want to lose a default item, if there is one.
    lItem := CurrentItem;
    // Int keys stored as strings
    lKey := CurrentStrID;

    // Clear existing item. Resets FPopulated to False.
    Clear;
    DoPopulate;
    FPopulated := True;
    // Re-select item
    ItemIndex := IDIndexOf(lKey);
     // But if Style is only csDropDown, item can have been typed in by user
    // and not be in the list.
    if Style = csDropDown then
      if ItemIndex = -1 then Text := lItem;
    FPopulating := False;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;  // TIDComboBox.PopulateContent

//------------------------------------------------------------------------------
procedure TIDComboBox.DoPopulate;
begin
  if Assigned(FOnPopulate) then
    FOnPopulate(Self);
end;  // TIDComboBox.DoPopulate

//------------------------------------------------------------------------------
procedure TIDComboBox.DropDown;
begin
  if not FPopulating then PopulateContent;
  if ReadOnly then begin
    FSelectedKey  := CurrentStrID;
    FSelectedText := CurrentItem;
  end;

  // Call inherited to carry on as intended.
  inherited;
end;  // TIDComboBox.DropDown

procedure TIDComboBox.CloseUp;
begin
  if ReadOnly then begin
    ItemIndex := IDIndexOf(FSelectedKey);
    if Style = csDropDown then
      if ItemIndex = -1 then Text := FSelectedText;
  end;
  inherited;
end;  // TIDComboBox.CloseUp

//------------------------------------------------------------------------------
// If ReadOnly and a selection change message comes through, cancel it.
// Otherwise, carry on as normal.
procedure TIDComboBox.WndProc(var Message: TMessage);
begin
  if ReadOnly and (Message.Msg = WM_COMMAND) then
    if TWMCommand(Message).NotifyCode = CBN_SELCHANGE then begin
      Exit;
    end;
  inherited WndProc(Message);
end;  // TIDComboBox.WndProc

//==============================================================================
procedure TIDComboBox.Insert(AIndex: integer; const NewItem: String;
  const NewID: Integer);
var lData: TID;
begin
  if IDFormat in [idfUnknown, idfLongint] then begin
    FIDFormat := idfLongint;
    lData := TID.Create;
    lData.IntValue := NewID;
    try
      inherited Items.InsertObject(AIndex, NewItem, lData);
      FSaveItems.Insert(AIndex, lData);  // Only need the reference to the object
    except
      lData.Free;
      raise;
    end;
  end else
    raise EInvalidFormat.CreateFmt(ResStr_InvalidFormat, ['Longint', 'String']);
end;  // TIDComboBox.Insert

//==============================================================================
procedure TIDComboBox.Insert(AIndex: integer; const NewItem, NewID: String);
var lData: TID;
begin
  if IDFormat in [idfUnknown, idfString] then begin
    FIDFormat := idfString;
    lData := TID.Create;
    lData.StrValue := NewID;
    try
      inherited Items.InsertObject(AIndex, NewItem, lData);
      FSaveItems.Insert(AIndex, lData);  // Only need the reference to the object
    except
      lData.Free;
      raise;
    end;
  end else
    raise EInvalidFormat.CreateFmt(ResStr_InvalidFormat, ['String', 'Longint']);
end;  // TIDComboBox.Insert

//==============================================================================
procedure TIDComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  // When closed but user presses Up or Down key, do a populate.
  if (Key in [VK_DOWN, VK_UP]) and not FPopulating then PopulateContent;
end;  // TIDComboBox.KeyDown

end.
