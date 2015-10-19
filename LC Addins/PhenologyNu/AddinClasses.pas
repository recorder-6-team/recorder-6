// Contains TCOMKeyList and TCOMKeyItem which use IKeyList and IKeyItem to
// pass key lists from the client application to the COM server application

unit AddinClasses;

interface

uses
  SysUtils, Windows, ActiveX, ComObj, classes, Recorder2000_TLB;

const
  MIXED_DATA = 'MIXED'; // indicates a keylist contains several tables worth of data

type
  EKeyListError = class(Exception);
  EExtendedKeyListError = class(EKeyListError);

  { Standard length of database key fields }

  TKeyString = string[16];
  
 { Object to save IDs in StringLists }
  TSaveID = Class(TObject)
    KeyString: TKeyString;
  end;

  { Header for the data }
  TStructHeader = record
    ItemCount : integer;
    TableName : string[30];
  end;

  { Items in the data defined as follows }
  TStructItem = record
    KeyField1 : TKeyString;   // Values to identify the primary key
    KeyField2 : string[30];
  end;

  { Object to pass around the data structures and helpful management methods }
  TKeyList = class(TObject)
  private
    function GetItemTable(const iIndex: integer): string;
  protected
    FHeader : TStructHeader;
    FItems  : array of TStructItem;
    function GetItem(iIndex: integer): TStructItem;
  public
    constructor Create;
    function IndexOf( const iKeyField1 : TKeyString;
                       const iKeyField2 : string ): integer;
    property Header : TStructHeader read FHeader;
    property Items[ iIndex : integer ] : TStructItem read GetItem;
    property ItemTable[ const iIndex : integer ] : string read GetItemTable;
  end;


  { Editable version of the TKeyList object }
  TEditableKeyList = class(TKeyList)
  public
    procedure SetTable( const iTableName : string );
    procedure AddItem(const iKeyField1, iKeyField2 : TKeyString); virtual;
    procedure DeleteItem(iIndex: Integer);
    function IndexOf(iKeyField1, iKeyField2: TKeyString): Integer;
    procedure Clear;
    procedure Assign( iKeyList : TKeyList );
    procedure Append( iKeyList : TKeyList );
    procedure ConvertToMixedData;
    procedure SortByTable;
    constructor Create; overload;
    constructor Create( iKeyListIntf : IKeyList ); overload;
  end;

  TCOMKeyList = class(TComObject, IKeyList)
  private
    FKeyList : TEditableKeyList;
  public
    constructor Create( iKeyList : TKeyList );
    destructor Destroy; override;
    { IUnknown }
    function QueryInterface(const IID:TGuid; out Obj): HResult; stdcall;
    { IDispatch }
    function GetTypeInfoCount( out Count: integer ):HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: integer;
                       DispIDs: pointer):HResult; stdcall;
    function Invoke(DispID: integer; const IID: TGUID; LocaleID: Integer; Flags: Word;
                    var Params; varResult, ExepInfo, ArgErr: Pointer): HResult; stdcall;
    { TCOMKeyList }
    function Get_ItemCount: Integer; safecall;
    function Get_TableName: WideString; safecall;
    function GetKeyItem(iIndex: Integer): IKeyItem; safecall;

    property KeyList : TEditableKeyList read FKeyList;
  end;

  TCOMKeyItem = class(TComObject, IKeyItem)
  private
    FItem : TStructItem;
  protected
    function Get_KeyField1: WideString; safecall;
    function Get_KeyField2: WideString; safecall;
  public
    { IUnknown }
    function QueryInterface(const IID:TGuid; out Obj): HResult; stdcall;
    { IDispatch }
    function GetTypeInfoCount( out Count: integer ):HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: integer;
                       DispIDs: pointer):HResult; stdcall;
    function Invoke(DispID: integer; const IID: TGUID; LocaleID: Integer; Flags: Word;
                    var Params; varResult, ExepInfo, ArgErr: Pointer): HResult; stdcall;
    { TCOMKeyItem }
    constructor Create(iItem: TStructItem);
    property KeyField1: WideString read Get_KeyField1;
    property KeyField2: WideString read Get_KeyField2;
  end;

const
  Class_COMKeyList: TGUID = '{3ADCD3B1-1350-11D3-B6E0-0060979160B0}';
  Class_COMKeyItem: TGUID = '{3ADCD3B2-1350-11D3-B6E0-0060979160B0}';

  { Error strings shared with sub-classes }
  EST_BAD_STRUCTURE = 'Cannot get data item from dataset.  Field missing : ';
  EST_JOIN_CANT_MIX = 'Cannot create a Mixed table keylist from a join-table keylist';

implementation

uses ComServ;

{ TCOMKeyList }

constructor TCOMKeyList.Create(iKeyList: TKeyList);
begin
  inherited Create;
  FKeyList := TEditablekeyList.Create;
  if iKeyList <> nil then
    FKeyList.Assign(iKeyList);
end;

destructor TCOMKeyList.Destroy;
begin
  FKeyList.Free;
  inherited Destroy;
end;

function TCOMKeyList.Get_ItemCount: Integer;
begin
  result := KeyList.Header.ItemCount;
end;

function TCOMKeyList.Get_TableName: WideString;
begin
  result := KeyList.Header.TableName;
end;

function TCOMKeyList.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: integer; DispIDs: pointer): HResult;
begin
  Result := 0;
end;

function TCOMKeyList.GetKeyItem(iIndex: Integer): IKeyItem;
var
  lCOMKeyItem : TCOMKeyItem;
begin
  lCOMKeyItem := TComKeyItem.Create(KeyList.Items[iIndex]);
  result := lCOMKeyItem as IKeyItem;
end;

function TCOMKeyList.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := 0;
end;

function TCOMKeyList.GetTypeInfoCount(out Count: integer): HResult;
begin
  Result := 0;
end;

function TCOMKeyList.Invoke(DispID: integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; varResult, ExepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := 0;
end;

function TCOMKeyList.QueryInterface(const IID: TGuid; out Obj): HResult;
begin
  Result := 0;
end;

{ TCOMKeyItem }

constructor TCOMKeyItem.Create(iItem: TStructItem);
begin
  inherited Create;
  FItem := iItem;
end;

function TCOMKeyItem.Get_KeyField1: WideString;
begin
  result := FItem.KeyField1;
end;

function TCOMKeyItem.Get_KeyField2: WideString;
begin
  result := FItem.KeyField2;
end;

function TCOMKeyItem.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: integer; DispIDs: pointer): HResult;
begin
  Result := 0;
end;

function TCOMKeyItem.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := 0
end;

function TCOMKeyItem.GetTypeInfoCount(out Count: integer): HResult;
begin
  Result := 0;
end;

function TCOMKeyItem.Invoke(DispID: integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; varResult, ExepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := 0;
end;

function TCOMKeyItem.QueryInterface(const IID: TGuid; out Obj): HResult;
begin
  Result := 0;
end;

//==============================================================================
{ TEditableKeyList }
//==============================================================================
constructor TEditableKeyList.Create;
begin
  inherited;
  FHeader.ItemCount := 0;
end;  // Create


{ Overloaded constructor for a key list which uses an IKeyList as its input
     data.  Therefore we can build genuine keylists from data supplied by
     addins }
constructor TEditableKeyList.Create(iKeyListIntf: IKeyList);
var
  i : integer;
  lKeyItemIntf : IKeyItem;
begin
  inherited Create;
  FHeader.ItemCount := 0;
  FHeader.TableName := iKeyListIntf.TableName;
  { Loop through the com objects items }
  for i := 0 to iKeyListIntf.ItemCount-1 do
  begin
    { Read the current item interface pointer }
    lKeyItemIntf := iKeyListIntf.GetKeyItem(i);
    { Add the item to our 'real' list }
    AddItem(lKeyItemIntf.KeyField1, lKeyItemIntf.KeyField2);
  end; // for
end;


//==============================================================================
procedure TEditableKeyList.AddItem(const iKeyField1, iKeyField2 : TKeyString);
begin
  FHeader.ItemCount := FHeader.ItemCount + 1;
  SetLength(FItems, FHeader.ItemCount);
  FItems[High(FItems)].KeyField1 := iKeyField1;
  FItems[High(FItems)].KeyField2 := iKeyField2;
end;  // AddItem

//==============================================================================
procedure TEditableKeyList.DeleteItem(iIndex: Integer);
var
  i: Integer;
begin
  //Move all subsequent items up one place
  for i:= (iIndex + 1) to FHeader.ItemCount - 1 do
  begin
	  FItems[i - 1].KeyField1:= FItems[i].KeyField1;
	  FItems[i - 1].KeyField2:= FItems[i].KeyField2;
  end;

  //Resize to lose last element
  FHeader.ItemCount:= FHeader.ItemCount - 1;
  SetLength(FItems, FHeader.ItemCount);
end;

//==============================================================================
procedure TEditableKeyList.Clear;
begin
  //Delete all items
  FHeader.ItemCount:= 0;
end;

//==============================================================================
function TEditableKeyList.IndexOf(iKeyField1, iKeyField2: TKeyString): Integer;
var
  lItemIndex, lMatchIndex: Integer;
begin
  //Set pessimistic result
  lMatchIndex:= -1;

  //Find first item then break from loop
  for lItemIndex:= 0 to FHeader.ItemCount - 1 do
    if (FItems[lItemIndex].KeyField1 = iKeyField1)
          and (FItems[lItemIndex].KeyField2 = iKeyField2) then
    begin
      lMatchIndex:= lItemIndex;
      break; // from loop - found item
    end;
  Result := lMatchIndex;
end;

//==============================================================================
procedure TEditableKeyList.SetTable(const iTableName: string);
begin
  FHeader.TableName := iTableName;
end;  // SetTable


{ Assign - takes nother keylist and copies it into the current one.  All current
     items are lost }
procedure TEditableKeyList.Assign(iKeyList: TKeyList);
var i : integer;
begin
  SetTable( iKeyList.Header.TableName );
  { Empty the current list }
  SetLength(FItems, 0);
  FHeader.ItemCount := 0;
  { And copy the new one across item by item }
  for i := 0 to iKeyList.Header.ItemCount-1 do
    AddItem( iKeyList.Items[i].KeyField1, iKeyList.Items[i].KeyField2 );
end;


{ Appends a key list to this one.  Converts to MIXED_DATA only if necessary. }
procedure TEditableKeyList.Append(iKeyList: TKeyList);
var i : integer;
begin
  { Convert to mixed list if necessary }
  if Header.TableName <> MIXED_DATA then
    if (iKeyList.Header.TableName = MIXED_DATA) or
       (iKeyList.Header.TableName <> Header.TableName) then
      ConvertToMixedData;
  { And copy the appended one across item by item }
  for i := 0 to iKeyList.Header.ItemCount-1 do
  begin
    if iKeyList.Header.TableName = MIXED_DATA then
      AddItem( iKeyList.Items[i].KeyField1, iKeyList.Items[i].KeyField2 )
    else
      AddItem( iKeyList.Items[i].KeyField1, iKeyList.Header.TableName );
  end; // for
end;

{ ConvertToMixed alters the keylist so that it can accept MIXED_DATA, ie data
     from different tables.  In this case, the KeyField2 is set to the table
     name for each item.  This cannot be used on join tables }
procedure TEditableKeyList.ConvertToMixedData;
var
  i : integer;
begin
  { Only do something if we have to! }
  if Header.TableName <> MIXED_DATA then
  begin
    { Check the first item is not for a join record }
    If Header.ItemCount > 0 then
      if Items[0].KeyField2 <> '' then
        raise EKeyListError.Create(EST_JOIN_CANT_MIX);
    { Alter our items and tablename }
    for i := 0 to Header.ItemCount-1 do
      FItems[i].KeyField2 := Header.TableName;
    FHeader.TableName := MIXED_DATA;
  end;
end;


{ For mixed data lists, this method sorts items by table.  }
procedure TEditableKeyList.SortByTable;
var
  lNewKeyList : TEditableKeyList; // sorted version
  lTableList : TStringList; // list of all tables
  lItem, lTable : integer;
begin
  if Header.TableName <> MIXED_DATA then
    Exit; // nothing to do
  lNewKeyList := TEditableKeyList.Create;
  lNewKeyList.SetTable(MIXED_DATA);
  lTableList := TStringList.Create;
  lTableList.Sorted := True;
  try
    { Find list of all unique tables }
    for lItem := 0 to Header.itemCount-1 do
    begin
      if lTableList.IndexOf(Uppercase(ItemTable[lItem]))=-1 then
        lTableList.Add(Uppercase(ItemTable[lItem]));
    end;
    { Now for each table, copy items into new key list }
    for lTable := 0 to lTableList.Count-1 do
      for lItem := 0 to Header.ItemCount-1 do
        if uppercase(ItemTable[lItem])=lTableList[lTable] then
          lNewKeyList.AddItem(Items[lItem].KeyField1, Items[lItem].KeyField2);
    { Copy the nerw key list into ourselves }
    Assign(lNewKeyList);
  finally
    lTableList.Free;
    lNewKeyList.Free;
  end;
end;


//==============================================================================
{ TKeyList }
//==============================================================================

constructor TKeyList.Create;
begin
  inherited Create;
  { Some initialisation for safety }
  FHeader.ItemCount := 0;
  FHeader.TableName := '';
end;


{ Accessor method for Items property }
function TKeyList.GetItem(iIndex: integer): TStructItem;
begin
  if iIndex <= High(FItems) then
    Result := FItems[iIndex]
  else
    Raise EKeyListError.Create('Trying to read non-existent item from key list');
end;  // GetItem


{ Returns the table associated with an item on the key list.  If the keylist
     is not of mixed tables, then returns the header tablename.  If mixed, then
     looks at KeyField2 to determine the name }
function TKeyList.GetItemTable(const iIndex: integer): string;
begin
  if Header.TableName = MIXED_DATA then
    Result := Items[iIndex].KeyField2
  else
    Result := Header.TableName;
end;



{ Locate an item in the list, if a match exists.  Otherwise, return -1 }
function TKeyList.IndexOf(const iKeyField1: TKeyString;
  const iKeyField2: string): integer;
var
  i : integer;
begin
  Result := -1; // default not found
  for i := 0 to Header.ItemCount-1 do
    if (Items[i].KeyField1 = iKeyField1) and (Items[i].KeyField2 = iKeyField2) then
    begin
      Result := i;
      break; // from loop as item found
    end;
end;

initialization
  TComObjectFactory.Create(ComServer, TCOMKeyList, Class_COMKeyList,
    'COMKeyList', '', ciMultiInstance, tmApartment);
  TComObjectFactory.Create(ComServer, TCOMKeyItem, Class_COMKeyItem,
    'COMKeyItem', '', ciMultiInstance, tmApartment);
end.
