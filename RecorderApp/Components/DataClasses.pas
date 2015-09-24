{ Unit Dataclasses

 TDataItem class.  Abstract base class for data items used with the TCacheDataList
       class.  This is used when a list of items is populated on a form, which
       can be edited but must be cached until the 'Save' or OK button is
       clicked.
 TCacheDataList is an abstract base class for managing this list of TDataItems

 TStringDataItem and TStringDataList are sub-classes which keep the list in
       synch with an existing TStrings instance (eg on a listbox component).
       They are also abstract - need to implement some stuff specific to the
       list.

 TGridDataItem and TGridDataList are sub-classes as above for synching with
       StringGrid components.
     In TGridDataItem derivatives, add private data for each column, and
       implement the GetCell accessor method to return each column according to
       the X position.  Alss implement the InitFromRecord and WriteToRecord
       methods.
     In TGridDataList derivatives, you may like to add a new constructor to make
       setting up the dataet easierimplement the process update method.  The
       only compulsory method is ProcessUpdate.  The BaseClass has a
       DeleteFromTable method to assist deletions.  Also, there are ItemsToAdd
       and ItemsToModify properties which identify which data items you need
       to update. The DoModifications method in the base class may help, but
       it bounces around the dataset alot so is inefficient. }

unit DataClasses;

interface

uses
  Sysutils, Db, Classes, Dialogs, JNCCDataSets, Grids, ExceptionForm,
  Recorder2000_TLB, ComObj, ADODB, ADOInt;

const
  MIXED_DATA = 'MIXED'; // indicates a keylist contains several tables worth of data

resourcestring
  { Error strings shared with sub-classes }
  ResStr_BadStructure = 'Cannot get data item from dataset.  Field missing: ';
  ResStr_JoinCantMix = 'Cannot create a Mixed table keylist from a join-table keylist';
  ResStr_InvalidValue = 'KeyField2 must be at most 30 characters in length.';
  ResStr_InvalidIndex = 'Index out of bounds.';
  ResStr_InvalidQuery = 'Invalid query. Field ''ItemKey'' not found.';



type
  EDataListError = class(Exception);
  EKeyListError = class(TExceptionPath);
  EKeyListCreateError = class(EKeyListError);
  EExtendedKeyListError = class(EKeyListError);
  EDataListAction = class(Exception); // events which are not errors
  ENoDataListAction = class(EDataListAction); // add/find events which are cancelled

  { A general purpose type for arrays of strings }
  TStringArray = array of string;

  { Classes for managing lists of key values to identify records }

  { Standard length of database key fields }
  TKeyString = string[16];

  { Additional information object for extended key lists }
  TExtendedInfo =class(TObject);

  { class for holding key information with items in combo and list boxes }
  { as TKeyString but stores a string with it for convenience }
  TCaptionAndKey = record
    ItemKey : TKeyString;
    Caption : string;
  end;

  TKey = class
  public
    Key : TKeyString;
    constructor Create(iKey: TKeyString = '');
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

  { Record to store a single record's location so it can be relocated. }
  TSingleRecord = record
    Key1 : TKeyString;
    Key2 : TKeyString;
    TableName : String;
  end;

  { Enumerated type used to specify how to search for taxa }
  TTaxonSearchType = (stName, stAbbreviation);
  TReferenceSearchType = (stReference, stKeyword);

  TKeyData = Class(TExtendedInfo)
  private
    FItemKey : TStructItem;
  protected
    function GetItemAdditional: String;
    procedure SetItemAdditional(const Value: String); virtual;
  public
    property ItemKey: TKeyString read FItemKey.KeyField1 write FItemKey.KeyField1;
    property ItemAdditional: String read GetItemAdditional write SetItemAdditional;
  end;

  { Object to pass around the data structures and helpful management methods }
  TKeyList = class(TObject)
  private
    function GetItemTable(const iIndex: integer): string;
  protected
    FHeader : TStructHeader;
    FItems  : array of TStructItem;
    FCapacity: Integer;
    procedure Grow;
    function GetItem(iIndex: integer): TStructItem;
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetItem(Index: Integer; const Value1: TKeyString; const Value2: String);
  public
    constructor Create;
    function IndexOf(const iKeyField1: TKeyString; const iKeyField2: string): integer;
    property Header: TStructHeader read FHeader;
    property Items[iIndex: integer]: TStructItem read GetItem;
    property ItemTable[const iIndex: integer]: string read GetItemTable;
    property Capacity: Integer read GetCapacity;
  end;

  { Editable version of the TKeyList object }
  TEditableKeyList = class(TKeyList)
  public
    procedure AddQueryResults(AQuery: TADOQuery);
    procedure SetTable(const iTableName: string);
    procedure AddItem(const iKeyField1: TKeyString; const iKeyField2: string); virtual;
    procedure Insert(iIndex: integer; const iKeyField1: TKeyString; const iKeyField2: string); virtual;
    procedure Move(CurIndex, NewIndex: integer); virtual;
    procedure DeleteItem(iIndex: Integer);
    procedure Clear;
    procedure Assign(iKeyList: TKeyList);
    procedure Append(iKeyList: TKeyList);
    procedure ConvertToMixedData(AValidateSecondField: boolean=true);
    procedure SortByTable;
    procedure SortTableWithOccsTogether;
    constructor Create; overload;
    constructor Create(iKeyListIntf: IKeyList); overload;
    property Capacity write SetCapacity;
  end;

  { Extended version of the TEditableKeyList object to hold further information along
  with a list of keys }
  TExtendedKeyList = class(TEditableKeyList)
  protected
    FExtendedInfoItems : array of TExtendedInfo;
    function GetExtendedInfo(iIndex: integer): TExtendedInfo;
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    property ExtendedInfoItems[iIndex: integer]: TExtendedInfo read GetExtendedInfo;
    procedure AddItem(const iKeyField1, iKeyField2: TKeyString;
      const iExtendedInfo: TExtendedInfo); reintroduce; overload;
    procedure AddItem(const iKeyField1, iKeyField2: TKeyString); reintroduce; overload;
  end;

  //==============================================================================

  { Forward declaration so TDataItem can hold its owner }
  TCacheDataList = class;

  //==============================================================================
  TDataItem = class(TObject)
  private
    FDeleted  : boolean;
    FAdded    : boolean;
    FModified : boolean;
    FOwnerList: TCacheDataList;
    FCustodian: String;
  protected
    { Needs to be written by sub-classes so protected }
    FItemKey  : TKeyString;
    procedure SetDeleted(const Value: boolean);
    { Method InitFromRecord.  Must implement this to extract required
     data from the iDataSet and setup the TDataItem sub-class properties.}
    procedure InitFromRecord(iDataSet: TDataSet); virtual; abstract;
    procedure WriteToRecord(iDataSet: TDataSet); virtual; abstract;
    procedure SetModified; virtual;
  public
    constructor CreateNew(aOwner: TCacheDataList); virtual;
    constructor CreateFromRecord(aOwner: TCacheDataList; iDataSet: TDataSet); virtual;
    property ItemKey: TKeyString read FItemKey;
    property Added: boolean read FAdded;
    property Modified: boolean read FModified;
    property Deleted: boolean read FDeleted write SetDeleted;
    property OwnerList:TCacheDataList read FOwnerList;
    property Custodian: String read FCustodian;
  end;

  { Possible statuses for changes made to data items. }
  TDataItemStatus = (disNew, disChanged, disDeleted);

  { Event for changes to items }
  TDataItemChangeEvent = procedure (Sender: TObject; Item: TDataItem; Status: TDataItemStatus) of object;

  { Metaclass to identify the items so that the list class can create them }
  TItemClass = class of TDataItem;

  //==============================================================================
  TCacheDataList = class(TList)
  private
    FDataSet : TDataset;
    FSourceKeyField : string;
    FItemsToAdd : TList;
    FItemsToDelete : TList;
    FItemsToModify : TList;
    FChanged : boolean;
    FItemClass : TItemClass;
    FSiteID: String;
    FDataItemChangeEvent: TDataItemChangeEvent;
    procedure ClearItemList;
    function GetItemCount: integer;
    procedure GetSiteID;
  protected
    FOverrideCustodyControl: Boolean;
    function ItemsDisplayName: String; virtual; abstract;
    procedure PopulateFromDataset;
    function GetDataItem(const iIndex: integer): TDataItem;
    procedure AddToList(iDataItem: TDataItem; iIndex: integer); virtual;
    procedure ProcessUpdate; virtual; abstract;
    procedure PrepareUpdateLists;
    { Generic function to provide a means of deletion from tables for
     sub-classes }
    procedure DeleteFromTable(iQuery: TJNCCQuery; const iTableName: string;
      const iKeyField: string); overload;
    procedure DeleteFromTable(iQuery: TJNCCQuery; const iTableName: string;
      const iKeyField1, iKeyField2: string; iKeyValue:TKeyString); overload;
    { Method to assis modifications }
    procedure DoModifications; virtual;
    { protected properties for use by sub-classes }
    property ItemsToAdd: TList read FItemsToAdd;
    property ItemsToDelete: TList read FItemsToDelete;
    property ItemsToModify: TList read FItemsToModify;
  public
    constructor Create(iDataSet: TDataset; const iKeyFieldName: string;
      iItemClass: TItemClass);
    destructor Destroy; override;
    procedure Refresh; virtual;
    procedure AddNew(iDataItem: TDataItem); virtual;
    procedure AddFromDataSet(iDataSet: TDataSet);
    procedure DeleteItem(const iIndex: integer); virtual;
    procedure SetLists;
    { Search the list for a key value - return nil if not found }
    function FindItem(iKey: TKeyString): TDataItem;
    procedure Update;
    property Changed: boolean read FChanged;
    property ItemCount:integer read GetItemCount;
    property OnDataItemChange: TDataItemChangeEvent read FDataItemChangeEvent write FDataItemChangeEvent;
    property SiteID: String read FSiteID;
  end;

  //============================================================================
  { Stuff for TStrings linkup }
  //----------------------------------------------------------------------------
  TStringDataItem = class(TDataItem)
  protected
    procedure SetModified; override;
  end;

  //----------------------------------------------------------------------------
  TStringDataList = class(TCacheDataList)
  private
    FStrings : TStrings;
  protected
    function GetText(iDataItem: TDataItem): string; virtual; abstract;
    procedure AddToList(iDataItem: TDataItem; iIndex: Integer); override;
  public
    constructor Create(iDataSet: TDataset; const iKeyFieldName: string;
      iStrings: TStrings; iItemClass: TItemClass); virtual;
    procedure DeleteItem(const iIndex: integer); override;
    procedure RefreshItemDisplay(iDataItem: TStringDataItem);
    procedure Refresh; override;
  end;

  //============================================================================
  { Stuff for StringGrid linkup }
  //----------------------------------------------------------------------------
  TGridDataItem = class(TDataItem)
  protected
    procedure SetModified; override;
    function GetCell(const iX: integer): string; virtual; abstract;
  public
    property Cells[const iX: integer]: string read GetCell;
  end;

  //----------------------------------------------------------------------------
  TGridDataList = class(TCacheDataList)
  private
    FStringGrid : TStringGrid;
  protected
    procedure AddToList(iDataItem: TDataItem; iIndex: Integer); override;
  public
    constructor Create(iDataSet: TDataset; const iKeyFieldName: string;
      iStringGrid: TStringGrid; iItemClass: TItemClass);
    procedure AddNew(iDataItem: TDataItem); override;
    procedure DeleteItem(const iIndex: integer); override;
    function GetDataItem(const iIndex: integer): TDataItem;    
    procedure RefreshItemDisplay(iDataItem: TGridDataItem);
    procedure Refresh; override;
  end;

  //============================================================================
  TTaxonNames = class(TObject)
  private
    FTaxonListItemKey : TKeyString;
    FTaxonName : string;
    FCommonName: string;
    FEnteredName: string;
    FTNItalic: boolean;
    FCNItalic: boolean;
    FENItalic: boolean;
    FTNAttribute: string;
    FCNAttribute: string;
    FENAttribute: string;
    FTNAuthor: string;
    FENAuthor: string;
  protected
    procedure SetTaxonListItemKey(Value: TKeyString); virtual;
    procedure SetTaxonName(Value: string); virtual;
    procedure SetCommonName(Value: string); virtual;
    procedure SetEnteredName(Value: string); virtual;
    procedure SetTNItalic(Value: boolean); virtual;
    procedure SetCNItalic(Value: boolean); virtual;
    procedure SetENItalic(Value: boolean); virtual;
    procedure SetTNAttribute(Value: string); virtual;
    procedure SetCNAttribute(Value: string); virtual;
    procedure SetENAttribute(Value: string); virtual;
    procedure SetTNAuthor(Value: string); virtual;
    procedure SetENAuthor(Value: string); virtual;
  public
    function IsIdentical(ATaxonNames: TTaxonNames): Boolean;
    property TaxonListItemKey: TKeyString read FTaxonListItemKey write SetTaxonListItemKey;
    property TaxonName: string read FTaxonName write SetTaxonName;
    property CommonName: string read FCommonName write SetCommonName;
    property EnteredName: string read FEnteredName write SetEnteredName;
    property TNItalic: boolean read FTNItalic write SetTNItalic;
    property CNItalic: boolean read FCNItalic write SetCNItalic;
    property ENItalic: boolean read FENItalic write SetENItalic;
    property TNAttribute: string read FTNAttribute write SetTNAttribute;
    property CNAttribute: string read FCNAttribute write SetCNAttribute;
    property ENAttribute: string read FENAttribute write SetENAttribute;
    property TNAuthor: string read FTNAuthor write SetTNAuthor;
    property ENAuthor: string read FENAuthor write SetENAuthor;
  end; // TTaxonNames

  // If a thread object calls a sub-class then a param of this type can be
  // used to allow the subclass to detect thread termination.  See
  // GridSquareExtractor for example.
  TThreadTerminationCheck = function(): boolean of object;

//==============================================================================
{ Global function to convert an id and string to a TCaptionAndKey }
function CaptionAndKey(const iKey: TKeyString; const iCaption: string): TCaptionAndKey;

//==============================================================================
implementation

uses
  Forms, Controls, DisplayFuncs;

resourcestring
  ResStr_CustodyChanged = 'Unable to save changes to %s record. '#13#10 +
                          'The custody of this record has been changed to a different Site ID.';
    { AddNew called with an uninitialised data item }
  ResStr_CantAdd       = 'Cannot add a nil item to a data list';
  { The base class has been instantiated, or the sub-class has called inherited }
  ResStr_InitFromRecord = 'TDataItem.InitFromRecord should not be called.';
  ResStr_IndexTooHigh = 'The TCacheDataList index number requested does not exist.';
  ResStr_ItemNotFound = 'The object for the list item requested is missing';
  { This error could occur if Create was used rather than createNew }
  ResStr_OwnerNotStringDataList = 'String data item not owned by a string data list.';
  ResStr_OwnerNotGridDataList = 'Grid data item not owned by a grid data list.';
  { Call to wrong overloaded method -> }
  ResStr_NeedExtInfo = 'Extended key list needs extended info to add an item';
  ResStr_IKeylistNil = 'Cannot create a keylist from a nil IKeyList';


//==============================================================================
{ Global function to convert an id and string to a TCaptionAndKey }
function CaptionAndKey(const iKey: TKeyString; const iCaption: string): TCaptionAndKey;
begin
  Result.Caption := iCaption;
  Result.ItemKey := iKey;
end;  // CaptionAndKey

//==============================================================================
{ TDataItem }

{ Create a new data item and initialise it from the dataset.  Records that the
     data was derived from a dataset.  Uses the abstract method InitFromRecord
     to set up the data item. }
constructor TDataItem.CreateFromRecord(aOwner: TCacheDataList; iDataSet: TDataSet);
begin
  inherited Create;
  FOwnerList := aOwner;
  { Not a newly created record }
  FAdded := False;
  { Initialise it }
  InitFromRecord(iDataSet);
  FCustodian := iDataSet.FieldByName('Custodian').AsString;
  FOwnerList.FChanged := True;
end;  // TDataItem.CreateFromRecord

//==============================================================================
{ Create a new, unitialised instance of the TDataItem.  Records that the data
		 item is newly added and not derived from a dataset }
constructor TDataItem.CreateNew(aOwner: TCacheDataList);
begin
  inherited Create;
  FOwnerList := aOwner;
  { A newly created record which we need to post into the dataset }
  FAdded := True;
  FOwnerList.FChanged := True;
end;  // TDataItem.CreateNew

//==============================================================================
{ Accessor method for the Deleted property }
procedure TDataItem.SetDeleted(const Value: boolean);
begin
  FDeleted := Value;
  FOwnerList.FChanged := True;
end;  // TDataItem.SetDeleted

//==============================================================================
{ Protected method to allow sub-classes to set the Modified property
     when a field data item is changed }
procedure TDataItem.SetModified;
begin
  FModified := True;
  FOwnerList.FChanged := True;
  if Assigned(FOwnerList.FDataItemChangeEvent) then
    FOwnerList.FDataItemChangeEvent(FOwnerList, Self, disChanged);
end;  // TDataItem.SetModified

//==============================================================================
{ TCacheDataList }
//==============================================================================
procedure TCacheDataList.AddFromDataSet(iDataSet: TDataSet);
var
  lDataItem : TDataItem;
begin
  lDataItem := FItemClass.CreateFromRecord(Self, iDataSet);
  AddNew(lDataItem);
end;  // TCacheDataList.AddFromDataSet

//==============================================================================
procedure TCacheDataList.AddNew(iDataItem: TDataItem);
begin
  if iDataItem <> nil then begin
    AddToList(iDataItem, -1);
    if Assigned(FDataItemChangeEvent) then
      FDataItemChangeEvent(Self, iDataItem, disNew);
  end else
    raise EDataListError.Create(ResStr_CantAdd );
end;  // TCacheDataList.AddNew

//==============================================================================
{ Simple method to add a data item to a dataset.  Only provided to enable
     overriding the addition to the list }
procedure TCacheDataList.AddToList(iDataItem: TDataItem; iIndex: integer);
begin
  if iIndex <> -1 then
    Insert(iIndex, iDataItem)
  else
    Add(iDataItem);
  if Assigned(FDataItemChangeEvent) then
    FDataItemChangeEvent(Self, iDataItem, disNew);
end;  // TCacheDataList.AddToList

//==============================================================================
{ Create the data list from a dataset.  Loops through the dataset and creates
		 items using abtract method implementations.  Keeps the dataset in its
		 original state. }
constructor TCacheDataList.Create(iDataSet: TDataset; const iKeyFieldName: string;
  iItemClass: TItemClass);
begin
  FDataSet       := iDataSet;
  FSourceKeyField:= iKeyFieldName;
  FItemClass     := iItemClass;
  FChanged       := False;
  FOverrideCustodyControl := False;

  GetSiteID;
  PopulateFromDataset;
end;  // TCacheDataList.Create

//==============================================================================
procedure TCacheDataList.GetSiteID;
begin
  // Connection property only available from TCustomADODataset onward.
  if FDataset is TCustomADODataset then
    // Use strored proc to get the Site ID
    with TADOStoredProc.Create(nil) do
      try
        // Use same connection as list's dataset object
        Connection := (FDataset as TCustomADODataset).Connection;
        ProcedureName := 'usp_GetSiteID';
        Parameters.Refresh;
        ExecProc;
        // Param[0] is RESULT value, so Param[1] is output param we want.
        FSiteID := Parameters[1].Value;
      finally
        Free;
      end;
end;  // GetSiteID

//==============================================================================
procedure TCacheDataList.ClearItemList;
var i: integer;
begin
  for i := Count - 1 downto 0 do
    TDataItem(Items[i]).Free;
  Clear;
end;  // TCacheDataList.ClearItemList

//==============================================================================
{Get the list from the dataset}
procedure TCacheDataList.PopulateFromDataset;
var lDataItem        : TDataItem;
    ltfOriginallyOpen: boolean;
begin
  { Get the list of items from the dataset }
  with FDataSet do
  begin
    ltfOriginallyOpen:=Active;
    if not Active then Open;
    First;
    while not Eof do
    begin
      lDataItem := FItemClass.CreateFromRecord(Self, FDataSet);
      AddToList(lDataItem, -1);
      if Assigned(FDataItemChangeEvent) then
        FDataItemChangeEvent(Self, lDataItem, disNew);
      Next;
    end;
    { Reset the dataset state }
    Active:=ltfOriginallyOpen;
  end; // with FDataSet
end;  // TCacheDataList.PopulateFromDataset

//==============================================================================
{Refresh the list from the dataset}
procedure TCacheDataList.Refresh;
var ltfActiveState:boolean;
begin
  //Free existing items
  ClearItemList;
  FChanged := False;
  //Get new items
  ltfActiveState:=FDataSet.Active;
  FDataSet.Close;
  PopulateFromDataset;
  FDataSet.Active:=ltfActiveState;
end;  // TCacheDataList.Refresh

//==============================================================================
{ Utility procedure to allow sub-classes to generically delete a list of key
   values from a table.  Not called in the base class }
procedure TCacheDataList.DeleteFromTable(iQuery: TJNCCQuery; const iTableName: string;
  const iKeyField: string);
var i      : Integer;
    lstKeys: String;
begin
  if ItemsToDelete.Count > 0 then
  begin
    // Always at least one item
    lstKeys := '''' + TDataItem(ItemsToDelete[0]).ItemKey + '''';
    // Add the others, with a ',' in between each
    for i:= 1 to ItemsToDelete.Count-1 do
      lstKeys := lstKeys + ',''' + TDataItem(ItemsToDelete[i]).ItemKey + '''';

    // Prepare query
    iQuery.SQL.Text := Format('DELETE FROM %s WHERE %s IN (%s)',
                              [iTableName, iKeyField, lstKeys]);
    // Execute
    iQuery.ExecuteSQL;
  end; // if count
end;  // TCacheDataList.DeleteFromTable

//==============================================================================
procedure TCacheDataList.DeleteFromTable(iQuery: TJNCCQuery; const iTableName: string;
  const iKeyField1, iKeyField2: string; iKeyValue:TKeyString);
var i      : Integer;
    lstKeys: String;
begin
  if ItemsToDelete.Count > 0 then
  begin
    // Always at least one item
    lstKeys := '''' + TDataItem(ItemsToDelete[0]).ItemKey + '''';
    // Add the others, with a ',' in between each
    for i:= 1 to ItemsToDelete.Count-1 do
      lstKeys := lstKeys + ',''' + TDataItem(ItemsToDelete[i]).ItemKey + '''';

    // Prepare query
    iQuery.SQL.Text := Format('DELETE FROM %s WHERE %s = ''%s'' AND %s IN (%s)',
                              [iTableName, iKeyField1, iKeyValue, iKeyField2, lstKeys]);
    // Execute
    iQuery.ExecuteSQL;
  end; // if count
end;  // TCacheDataList.DeleteFromTable

//==============================================================================
procedure TCacheDataList.DoModifications;
var lDataItem       : TDataItem;
    ltfOriginalState: Boolean;
    lCustodianField : TField;
    lDBCustodian    : String;
  //----------------------------------------------------------------------------
  function GetModifiedItem(const AKey:TKeyString):TDataItem;
  var iCount:integer;
  begin
    Result:=nil;
    for iCount:=0 to ItemsToModify.Count-1 do
      if TDataItem(ItemsToModify.Items[iCount]).ItemKey=AKey then begin
        Result:=TDataItem(ItemsToModify.Items[iCount]);
        Exit;
      end;
  end;  // GetModifiedItem
  //----------------------------------------------------------------------------
begin
  if ItemsToModify.Count > 0 then
  begin
    with FDataSet do begin
      ltfOriginalState:=Active;
      if not Active then Open;
      if FDataset is TCustomADODataset then begin
        // Refresh underlying data before doing the update, this refreshes the Custodian field.
        (FDataset as TCustomADODataset).Requery;
        // The following property is only available and relevant with ADODatasets
        (FDataset as TCustomADODataset).Properties['Update Criteria'].Value := adCriteriaKey;
      end;
      First;
      lCustodianField := FindField('Custodian');

      while not Eof do begin
        if Assigned(lCustodianField) then lDBCustodian := lCustodianField.AsString
                                     else lDBCustodian := '';
        lDataItem := GetModifiedItem(FieldByName(FSourceKeyField).AsString);
        if (lDataItem <> nil) then
          // Custodian same as site id, allowed to modify, so save
          if (lDBCustodian = FSiteID) or FOverrideCustodyControl then begin
            Edit;
            lDataItem.WriteToRecord(FDataSet);
            Post;
          end else
          // If custodian read from DB different from SiteID, shouldn't have been able
          // to edit in the first place, but just to be safe...
          // Also, if custodian read from DB different from cached custodian, custody must
          // have been reassigned while editing, so show a message to tell about it
          if lDBCustodian <> lDataItem.Custodian then
          begin
            // Show message
            MessageDlg(Format(ResStr_CustodyChanged, [ItemsDisplayName]), mtWarning, [mbOk], 0);
            // Refresh the now invalid cached data
            lDataItem.InitFromRecord(FDataSet);
            lDataItem.FCustodian := lDBCustodian;
          end;
        Next;
      end; // while
      Active:=ltfOriginalState;
    end; // with
  end; // if count
end;  // TCacheDataList.DoModifications

//==============================================================================
{ Mark delete an item.  Only when the Update method is called do we do anything
     about this }
procedure TCacheDataList.DeleteItem(const iIndex: integer);
begin
  TDataItem(Items[iIndex]).Deleted := True;
  if Assigned(FDataItemChangeEvent) then
    FDataItemChangeEvent(Self, Items[iIndex], disDeleted);
end;  // TCacheDataList.DeleteItem

//==============================================================================
{ Destructor which automatically walks the list of data items }
destructor TCacheDataList.Destroy;
begin
  ClearItemList;
  FItemsToAdd.Free;
  FItemsToModify.Free;
  FItemsToDelete.Free;
  inherited Destroy;
end;  // TCacheDataList.Destroy

//==============================================================================
{ Search the list for a key value - return nil if not found }
function TCacheDataList.FindItem(iKey: TKeyString): TDataItem;
var iIndex : integer;
begin
  Result := nil; // default value is not found
  for iIndex := 0 to Count-1 do
  begin
    if GetDataItem(iIndex).ItemKey=iKey then begin
      Result:=GetDataItem(iIndex);
      Exit; // from the loop
    end;
  end;
end;  // TCacheDataList.FindItem

//==============================================================================
{ Accessor method for the dataitems in the list }
function TCacheDataList.GetDataItem(const iIndex: integer): TDataItem;
begin
  Result := TDataItem(Items[iIndex]);
end;  // TCacheDataList.GetDataItem

//==============================================================================
{ Prepare the lists to hold items which are to be added/modifed or deleted.
     This involves creating the list, or if the list already exists, just clear
     it }
procedure TCacheDataList.PrepareUpdateLists;
begin
  if FItemsToAdd = nil then FItemsToAdd := TList.Create
                       else FItemsToAdd.Clear;

  if FItemsToDelete = nil then FItemsToDelete := TList.Create
                          else FItemsToDelete.Clear;

  if FItemsToModify = nil then FItemsToModify := TList.Create
                          else FItemsToModify.Clear;
end;  // TCacheDataList.PrepareUpdateLists

//==============================================================================
{ Prepare lists of updated/Modified/deleted items, then call the abstract method
     ProcessUpdates.  This method must act accordingly updating the datasets
     using the contents of the 3 lists. }
procedure TCacheDataList.Update;
var i          : Integer;
    lOrigCursor: TCursor;
begin
  lOrigCursor   := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    SetLists;
    ProcessUpdate;

    // Moved from DeleteFromTable methods. It's all about pointers, remember!!!!
    // Removing the items when there are or possibly can be needed for several
    // tables wasn't the smartest idea.
    // So, now that the items are all deleted from the table(s), we can actually
    // do the removal
    for i := Count - 1 downto 0 do
      if TDataItem(Items[i]).Deleted then
      begin
        TDataItem(Items[i]).Free;
        Delete(i);
      end;
  finally
    Screen.Cursor := lOrigCursor;
  end; // finally
end;  // TCacheDataList.Update

//==============================================================================
procedure TCacheDataList.SetLists;
var
  i : integer;
begin
  PrepareUpdateLists;
  { Iterate through each item checking the state - what do we have to do? }
  for i := 0 to Count-1 do
  begin
    if (TDataItem(Items[i]).Deleted) and (TDataItem(Items[i]).Added) then
      Continue;  // next iteration - this item never existed in the first place

    { Deletion has the highest priority! }
    if TDataItem(Items[i]).Deleted then
      FItemsToDelete.Add(Items[i])
    { Addition before modification - as long as we add the most recent copy }
    else if TDataItem(Items[i]).Added then
      FItemsToAdd.Add(Items[i])
    { finally modifications }
    else if TDataItem(Items[i]).Modified then
      FItemsToModify.Add(Items[i]);
  end; // for
end;  // TCacheDataList.SetLists

//==============================================================================
function TCacheDataList.GetItemCount: integer;
var iCount,iTotal:integer;
begin
  iTotal:=0;
  for iCount:=0 to Count-1 do
    if not TDataItem(Items[iCount]).Deleted then Inc(iTotal);
  Result:=iTotal;
end;  // TCacheDataList.GetItemCount

//==============================================================================
{ TStringDataList }
//==============================================================================
procedure TStringDataList.AddToList(iDataItem: TDataItem; iIndex: integer);
var
  lIndex,lNewIndex : integer;
  lNonDeletedItemsIndex,lCount : Integer;
begin
  { Add to the string list first so we can find out where it should go }
  lIndex := FStrings.AddObject(GetText(iDataItem), iDataItem);
  { Since FStrings.Count could be less than Items.Count we must check the Items array for
  items marked for deletion and ignore them. i.e deleting item 3 from an initial list of 5 items
  will set FStrings.Count = 4 and Items.Count = 5 where TDataItem(Items[3]).Delete = true }
  lNonDeletedItemsIndex := -1;
  lNewIndex := -1;
  for lCount := 0 to Count -1 do
    if not TDataItem(Items[lCount]).Deleted then
    begin
      Inc(lNonDeletedItemsIndex);
      if lNonDeletedItemsIndex = lIndex then lNewIndex := lCount;
    end;
  if lNewIndex = -1 then
    inherited AddToList(iDataItem, -1)
  else         
    inherited AddToList(iDataItem, lNewIndex);
end;  // TStringDataList.AddToList

//==============================================================================
constructor TStringDataList.Create(iDataSet: TDataset; const iKeyFieldName: string;
  iStrings: TStrings; iItemClass: TItemClass);
begin
  FStrings := iStrings;
  inherited Create(iDataSet, iKeyFieldName, iItemClass);
end;  // TStringDataList.Create

//==============================================================================
procedure TStringDataList.DeleteItem(const iIndex: integer);
var
  lDataItemIndex : integer;
begin
  if iIndex > FStrings.Count - 1 then
    raise EDataListError.Create(ResStr_IndexTooHigh);
  { Translate the string list item index to our object list item index }
  lDataItemIndex := IndexOf(FStrings.Objects[iIndex]);
  if lDataItemIndex = -1 then
    raise EDataListError.Create(ResStr_ItemNotFound);
  FStrings.Delete(iIndex);
  inherited DeleteItem(lDataItemIndex);
end;  // TStringDataList.DeleteItem

//==============================================================================
procedure TStringDataList.Refresh;
begin
  FStrings.Clear;
  inherited;
end;  // TStringDataList.Refresh

//==============================================================================
procedure TStringDataList.RefreshItemDisplay(iDataItem: TStringDataItem);
var
  lIndex : integer;
begin
  lIndex :=  FStrings.IndexOfObject(iDataItem);
  if lIndex <> -1 then
    FStrings[lIndex] := GetText(iDataItem);
end;  // TStringDataList.RefreshItemDisplay

//==============================================================================
{ TStringDataItem }
//==============================================================================
procedure TStringDataItem.SetModified;
begin
  if not (FOwnerList is TStringDataList) then
    raise EDataListError.Create(ResStr_OwnerNotStringDataList);
  inherited SetModified;
  TStringDataList(FOwnerList).RefreshItemDisplay(Self);
end;  // TStringDataItem.SetModified

//==============================================================================
{ TGridDataItem }
//==============================================================================
procedure TGridDataItem.SetModified;
begin
  if not (FOwnerList is TGridDataList) then
    raise EDataListError.Create(ResStr_OwnerNotGridDataList);
  inherited SetModified;
  TGridDataList(FOwnerList).RefreshItemDisplay(Self);
end;  // TGridDataItem.SetModified

//==============================================================================
{ TGridDataList }
//==============================================================================
constructor TGridDataList.Create(iDataSet: TDataset; const iKeyFieldName: string;
  iStringGrid: TStringGrid; iItemClass: TItemClass);
begin
  FStringGrid := iStringGrid;
  inherited Create(iDataSet, iKeyFieldName, iItemClass);
end;  // TGridDataList.Create

//==============================================================================
{ The index is ignored - we always add at the end }
procedure TGridDataList.AddToList(iDataItem: TDataItem; iIndex: Integer);
var i,iDisplayed : integer;
begin
  inherited AddToList(iDataItem, iIndex);
  { Add another one except for when top blank row is kept - we can't lose the
      very top row otherwise our fixed rows go wrong }
  // Get count of displayed items, so we can get the count of rows needed in the grid
  iDisplayed:=Count;
  for i:=0 to Count-1 do
    if TDataItem(Items[i]).Deleted then Dec(iDisplayed);

  with FStringGrid do begin
    if iDisplayed>=RowCount-FixedRows then RowCount:=iDisplayed+FixedRows
                                      else RowCount:=FixedRows+1;
    { Populate last row }
    for i:=0 to ColCount-1 do Cells[i,RowCount-1]:=TGridDataItem(iDataItem).Cells[i];
    Rows[RowCount-1].Objects[0]:=iDataItem;
  end;
end;  // TGridDataList.AddToList

//==============================================================================
procedure TGridDataList.AddNew(iDataItem:TDataItem);
begin
  inherited AddNew(iDataItem);
  RefreshItemDisplay(TGridDataItem(iDataItem));
end;  // TGridDataList.AddNew

//==============================================================================
procedure TGridDataList.DeleteItem(const iIndex: integer);
var
  lDataItemIndex : integer;
begin
  if iIndex > FStringGrid.RowCount - 1 then
    raise EDataListError.Create(ResStr_IndexTooHigh);
  { Translate the string list item index to our object list item index - note
    we always store the object on the left most cell of the row (item 0) }
	lDataItemIndex := IndexOf(FStringGrid.Rows[iIndex].Objects[0]);
  if lDataItemIndex = -1 then
    raise EDataListError.Create(ResStr_ItemNotFound);
  { Select the row to delete }
  DelLineInGrid(FStringGrid);
  inherited DeleteItem(lDataItemIndex);
end;  // TGridDataList.DeleteItem

//==============================================================================
procedure TGridDataList.Refresh;
begin
  //Clear grid
  with FStringGrid do begin
    RowCount:=FixedRows+1;
    Rows[RowCount-1].CommaText:='';
  end;
  inherited;
end;  // TGridDataList.Refresh

//==============================================================================
procedure TGridDataList.RefreshItemDisplay(iDataItem: TGridDataItem);
var i, iIndex, iRow  : integer;
begin
  iIndex := IndexOf(iDataItem);
  if iIndex <> -1 then begin
    // Set row index as if nothing had been deleted
    iRow:=iIndex+FStringGrid.FixedRows;
    // Decrease by one for each deleted item before current item. As the deleted
    // items don't appear in the grid, there is a misalignment to deal with
    for i:=iIndex downto 0 do
      if TGridDataItem(Items[i]).Deleted then Dec(iRow);
    // Now proceed with the proper row for the item
    for i := 0 to FStringGrid.ColCount-1 do
      FStringGrid.Cells[i,iRow]:=iDataItem.Cells[i];
  end;
end;  // TGridDataList.RefreshItemDisplay

//==============================================================================
{ TEditableKeyList }
//==============================================================================
constructor TEditableKeyList.Create;
begin
  inherited;
  FHeader.ItemCount := 0;
  FCapacity := 0;
end;  // TEditableKeyList.Create

//==============================================================================
{ Overloaded constructor for a key list which uses an IKeyList as its input
     data.  Therefore we can build genuine keylists from data supplied by
     addins }
constructor TEditableKeyList.Create(iKeyListIntf: IKeyList);
var
  i : integer;
  lKeyItemIntf : IKeyItem;
begin
  inherited Create;
  if iKeyListIntf = nil then
    Raise EKeyListCreateError.Create(ResStr_IKeylistNil);
  FHeader.ItemCount := 0;
  FHeader.TableName := iKeyListIntf.TableName;
  FCapacity := 0;
  { Loop through the com objects items }
  for i := 0 to iKeyListIntf.ItemCount-1 do
  begin
    { Read the current item interface pointer }
    lKeyItemIntf := iKeyListIntf.GetKeyItem(i);
    { Add the item to our 'real' list }
    AddItem(lKeyItemIntf.KeyField1, lKeyItemIntf.KeyField2);
  end; // for
end;  // TEditableKeyList.Create

//==============================================================================
procedure TEditableKeyList.AddItem(const iKeyField1: TKeyString; const iKeyField2: string);
begin
  if (Length(iKeyField2) >= 0) and (Length(iKeyField2) <= 30) then begin
    if FCapacity = FHeader.ItemCount then Grow;
    FHeader.ItemCount := FHeader.ItemCount + 1;
    SetItem(FHeader.ItemCount - 1, iKeyField1, iKeyField2);
  end else
    Raise EKeyListError.CreateNonCritical(ResStr_InvalidValue);
end;  // TEditableKeyList.AddItem

{-------------------------------------------------------------------------------
  Add results from an TADOQuery. The query must have at least a ItemKey field.
  The TableName field is optional.
}
procedure TEditableKeyList.AddQueryResults(AQuery: TADOQuery);
var
  lWasOpen, lHasTableName: Boolean;
begin
  with AQuery do begin
    lWasOpen:= Active;
    if not Active then
      Open;
    lHasTableName := FieldList.IndexOf('TableName') <> -1;
    if FieldList.IndexOf('ItemKey') = -1 then
      raise EKeyListError.Create(Format(ResStr_InvalidQuery, [SQL.Text]));

    if lHasTableName and (Header.TableName <> MIXED_DATA) then
      ConvertToMixedData;
    First;
    while not Eof do begin
      if lHasTableName then
        AddItem(FieldByName('ItemKey').AsString, FieldByName('TableName').AsString)
      else
        AddItem(FieldByName('ItemKey').AsString, '');
      Next;
    end;
    if not lWasOpen then Close;
  end;
end;  // TEditableKeyList.AddQueryResults

{Inserts a TStructItem record into FItems at the index given.  The record
currently with that index moves to one index higher as do all subsequent
records}
procedure TEditableKeyList.Insert(iIndex: integer;
  const iKeyField1: TKeyString; const iKeyField2: string);
var k: integer;
begin
  if (Length(iKeyField2) >= 0) and (Length(iKeyField2) <= 30) then
    if (iIndex >= 0) and (iIndex <= FHeader.ItemCount) then begin
      if FCapacity = FHeader.ItemCount then Grow;
      FHeader.ItemCount := FHeader.ItemCount + 1;
      for k := FHeader.ItemCount - 1 downto iIndex + 1 do
        FItems[k] := FItems[k - 1];
      SetItem(iIndex, iKeyField1, iKeyField2);
    end else raise EKeyListError.Create(ResStr_InvalidIndex)
  else
    Raise EKeyListError.CreateNonCritical(ResStr_InvalidValue);
end;

{Moves a TStructItem in the FItems list from the current specified index to the
new index specified.  The intervening records are shuffled accordingly.}
procedure TEditableKeyList.Move(CurIndex, NewIndex: integer);
var k: integer;
    Holder: TStructItem;
begin
  if (CurIndex < 0) or (NewIndex < 0) or
     (CurIndex > FHeader.ItemCount - 1) or (NewIndex > FHeader.ItemCount - 1) then
    raise EKeyListError.Create(ResStr_InvalidIndex)
  else begin
    Holder := FItems[CurIndex];
    if CurIndex < NewIndex then
      for k := CurIndex + 1 to NewIndex do FItems[k - 1] := FItems[k]
    else if CurIndex > NewIndex then
      for k := CurIndex - 1 downto NewIndex do FItems[k + 1] := FItems[k];
    FItems[NewIndex] := Holder;
  end;
end;

//==============================================================================
procedure TEditableKeyList.DeleteItem(iIndex: Integer);
var
  i: Integer;
begin
  //Move all subsequent items up one place
  for i:= (iIndex + 1) to FHeader.ItemCount - 1 do
    SetItem(i - 1, FItems[i].KeyField1, FItems[i].KeyField2);

  //Resize to lose last element
  FHeader.ItemCount:= FHeader.ItemCount - 1;
end;  // TEditableKeyList.DeleteItem

//==============================================================================
procedure TEditableKeyList.Clear;
begin
  //Delete all items
  FHeader.ItemCount:= 0;
end;  // TEditableKeyList.Clear

//==============================================================================
procedure TEditableKeyList.SetTable(const iTableName: string);
begin
  FHeader.TableName := iTableName;
end;  // TEditableKeyList.SetTable

//==============================================================================
{ Assign - takes nother keylist and copies it into the current one.  All current
     items are lost }
procedure TEditableKeyList.Assign(iKeyList: TKeyList);
var i : integer;
begin
  SetTable(iKeyList.Header.TableName);
  { Empty the current list }
  FHeader.ItemCount := 0;
  FCapacity := 0;
  SetLength(FItems, 0);
  // Set new capacity
  Capacity := iKeyList.Header.ItemCount;
  { And copy the new one across item by item }
  for i := 0 to iKeyList.Header.ItemCount-1 do
    AddItem(iKeyList.Items[i].KeyField1, iKeyList.Items[i].KeyField2);
end;  // TEditableKeyList.Assign

//==============================================================================
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
      AddItem(iKeyList.Items[i].KeyField1, iKeyList.Items[i].KeyField2)
    else
      AddItem(iKeyList.Items[i].KeyField1, iKeyList.Header.TableName);
  end; // for
end;  // TEditableKeyList.Append

//==============================================================================
{ ConvertToMixed alters the keylist so that it can accept MIXED_DATA, ie data
     from different tables.  In this case, the KeyField2 is set to the table
     name for each item.  This cannot be used on join tables }
procedure TEditableKeyList.ConvertToMixedData(AValidateSecondField: boolean=true);
var
  i : integer;
begin
  { Only do something if we have to! }
  if Header.TableName <> MIXED_DATA then
  begin
    { Check the first item is not for a join record }
    If (Header.ItemCount > 0) and AValidateSecondField then
      if Items[0].KeyField2 <> '' then
        raise EKeyListError.Create(ResStr_JoinCantMix);
    { Alter our items and tablename }
    for i := 0 to Header.ItemCount-1 do
      FItems[i].KeyField2 := Header.TableName;
    FHeader.TableName := MIXED_DATA;
  end;
end;  // TEditableKeyList.ConvertToMixedData

//==============================================================================
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
  lNewKeyList.Capacity := Header.ItemCount;  // Set to accomodate all items
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
        if Uppercase(ItemTable[lItem])=lTableList[lTable] then
          lNewKeyList.AddItem(Items[lItem].KeyField1, Items[lItem].KeyField2);
    { Copy the nerw key list into ourselves }
    Assign(lNewKeyList);
  finally
    lTableList.Free;
    lNewKeyList.Free;
  end;
end;  // TEditableKeyList.SortByTable


{ SortTableWithOccsTogether does a table sort, but ensures that taxon and
    biotope occurrence tables are next to each other in the list.  They are
    actually placed in the 'O' position in the list }
procedure TEditableKeyList.SortTableWithOccsTogether;
var
  i : integer;
begin
  { Temporarily set the table name so sort is in correct place }
  for i := 0 to Header.itemCount-1 do
    if (CompareText(Items[i].KeyField2, 'Taxon_occurrence')=0) or
       (CompareText(Items[i].KeyField2, 'Biotope_occurrence')=0) then
      FItems[i].KeyField2 := 'OCCURRENCE_' + Items[i].KeyField2;
  SortByTable;
  { Reset table names }
  for i := 0 to Header.itemCount-1 do
    if Copy(Items[i].KeyField2, 1, 11) = 'OCCURRENCE_' then
      FItems[i].KeyField2 := Copy(Items[i].KeyField2, 12, 255);
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
  FCapacity := 0;
end;  // TKeyList.Create

//==============================================================================
{ Accessor method for Items property }
function TKeyList.GetItem(iIndex: integer): TStructItem;
begin
  if iIndex <= FHeader.ItemCount - 1 then
    Result := FItems[iIndex]
  else
    Raise EKeyListError.Create('Trying to read non-existent item from key list');
end;  // TKeyList.GetItem

//==============================================================================
{ Returns the table associated with an item on the key list.  If the keylist
     is not of mixed tables, then returns the header tablename.  If mixed, then
     looks at KeyField2 to determine the name }
function TKeyList.GetItemTable(const iIndex: integer): string;
begin
  if (Header.TableName = MIXED_DATA) or (Header.TableName = '') then
    Result := Items[iIndex].KeyField2
  else
    Result := Header.TableName;
end;  // TKeyList.GetItemTable

//==============================================================================
{ Locate an item in the list, if a match exists.  Otherwise, return -1 }
function TKeyList.IndexOf(const iKeyField1: TKeyString; const iKeyField2: string): integer;
var
  i : integer;
begin
  Result := -1; // default not found
  for i := 0 to Header.ItemCount-1 do
    if (Items[i].KeyField1 = iKeyField1) and
       SameText(Items[i].KeyField2, iKeyField2) then
    begin
      Result := i;
      break; // from loop as item found
    end;
end;  // TKeyList.IndexOf

function TKeyList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TKeyList.SetCapacity(NewCapacity: Integer);
begin
  // Increase capacity of list if needed.
  if FCapacity < NewCapacity then begin
    SetLength(FItems, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

procedure TKeyList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TKeyList.SetItem(Index: Integer; const Value1: TKeyString; const Value2: String);
begin
  FItems[Index].KeyField1 := Value1;
  FItems[Index].KeyField2 := Value2;
end;

//==============================================================================
//==============================================================================
{ TExtendedKeyList }
//==============================================================================
procedure TExtendedKeyList.AddItem(const iKeyField1,
  iKeyField2: TKeyString; const iExtendedInfo: TExtendedInfo);
var lOldCapacity: Integer;
begin
  lOldCapacity := Capacity;
  inherited AddItem(iKeyField1, iKeyField2);
  // Extend array only if required
  if lOldCapacity < Capacity then
    SetLength(FExtendedInfoItems, Capacity);
  FExtendedInfoItems[FHeader.ItemCount - 1] := iExtendedInfo;
end;  // TExtendedKeyList.AddItem

//==============================================================================
procedure TExtendedKeyList.AddItem(const iKeyField1,
  iKeyField2: TKeyString);
begin
  raise EExtendedKeyListError.Create(ResStr_NeedExtInfo);
end;  // TExtendedKeyList.AddItem

//==============================================================================
{ Accessor method for ExtendedInfoArray property }
function TExtendedKeyList.GetExtendedInfo(iIndex: integer): TExtendedInfo;
begin
  if iIndex <= FHeader.ItemCount - 1 then
    Result := FExtendedInfoItems[iIndex]
  else
    Raise EExtendedKeyListError.Create('Trying to read non-existent extended info from extended key list');
end;  // TExtendedKeyList.GetExtendedInfo

//==============================================================================
procedure TExtendedKeyList.SetCapacity(NewCapacity: Integer);
begin
  inherited SetCapacity(NewCapacity);
  // Don't use NewCapacity, already use to set Capacity to correct value in ancestor.
  SetLength(FExtendedInfoItems, Capacity);
end;

//==============================================================================
//==============================================================================
{ TKeyData }
function TKeyData.GetItemAdditional: String;
begin
  Result := FItemKey.KeyField2;
end;  // TKeyData.GetItemAdditional

//==============================================================================
procedure TKeyData.SetItemAdditional(const Value: String);
begin
  FItemKey.KeyField2 := copy(Value,0,30);
end;  // TKeyData.SetItemAdditional

//==============================================================================

{ TTaxonNames }

function TTaxonNames.IsIdentical(ATaxonNames: TTaxonNames): Boolean;
begin
  Result :=
    (FTaxonListItemKey = ATaxonNames.FTaxonListItemKey) and
    (FTaxonName = ATaxonNames.FTaxonName) and
    (FCommonName = ATaxonNames.FCommonName) and
    (FTNItalic = ATaxonNames.FTNItalic) and
    (FCNItalic = ATaxonnames.FCNItalic);
end;

procedure TTaxonNames.SetCNItalic(Value: boolean);
begin
  FCNItalic := Value;
end;  // SetCNItalic

procedure TTaxonNames.SetENItalic(Value: boolean);
begin
  FENItalic := Value;
end;  // SetENItalic

procedure TTaxonNames.SetTNAttribute(Value: string);
begin
  FTNAttribute := Value;
end;  // SetTNAttribute

procedure TTaxonNames.SetCNAttribute(Value: string);
begin
  FCNAttribute := Value;
end;  // SetCNAttribute

procedure TTaxonNames.SetENAttribute(Value: string);
begin
  FENAttribute := Value;
end;  // SetENAttribute

procedure TTaxonNames.SetTNAuthor(Value: string);
begin
  FTNAuthor := Value;
end;  // SetTNAuthor

procedure TTaxonNames.SetENAuthor(Value: string);
begin
  FENAuthor := Value;
end;  // SetENAuthor

procedure TTaxonNames.SetCommonName(Value: string);
begin
  FCommonName := Value;
end;  // SetCommonName

procedure TTaxonNames.SetEnteredName(Value: string);
begin
  FEnteredName := Value;
end;  // SetEnteredName

procedure TTaxonNames.SetTaxonListItemKey(Value: TKeyString);
begin
  FTaxonListItemKey := Value;
end;  // SetTaxonListItemKey

procedure TTaxonNames.SetTaxonName(Value: string);
begin
  FTaxonName := Value;
end;  // SetTaxonName

procedure TTaxonNames.SetTNItalic(Value: boolean);
begin
  FTNItalic := Value;
end;  // SetTNItalic

//==============================================================================
{ TKey }

constructor TKey.Create(iKey: TKeyString);
begin
  Key := iKey;
end;

{-------------------------------------------------------------------------------
  Make this public as useful to pick item by index
}
function TGridDataList.GetDataItem(const iIndex: integer): TDataItem;
begin
  result := inherited GetDataItem(iIndex);
end;

end.

