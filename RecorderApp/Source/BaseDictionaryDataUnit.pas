//==============================================================================
//  Unit:        BaseDictionaryDataUnit
//
//  Implements:  TBaseDictionaryData
//
//  Description: Defines the base class for dictionary data modules. Implements
//               basic data access functions to populate the dictionaries.
//
//  Author:      Eric Salmon
//  Created:     23 Nov 2001
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 27 $
//    $Date: 18/02/08 18:23 $
//    $Author: Ericsalmon $
//
//==============================================================================

unit BaseDictionaryDataUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  JNCCDatasets, DataClasses, StdCtrls, RapTree, HierarchyNodes, ExceptionForm,
  GeneralFunctions, BaseData, ADODB, DatabaseAccessADO, HTTPApp, HTTPProd;

type
  EBaseDictData = class(TExceptionPath);
  EUndefinedTarget = class(EBaseDictData);

  TDBLocation = (dblCD,dblLocal);

  TBaseDictionaryData = class;
  TDictionaryDataClass = class of TBaseDictionaryData;

  TBaseDictionaryData = class(TBaseDataModule)
    qryList: TJNCCQuery;
    qryTopLevel: TJNCCQuery;
    qryChildLevel: TJNCCQuery;
    qryParent: TJNCCQuery;
    qryPreferredKey: TJNCCQuery;
    ppTopLevel: TPageProducer;
    ppChildLevel: TPageProducer;
    procedure ppPopulateQueryTag(Sender: TObject; Tag: TTag;
      const TagString: String; TagParams: TStrings;
      var ReplaceText: String);
  private
    FList: TComboBox;
    FTree: TRapidTree;
    FNodeToExpand: TFlyNode;
    FListKeys: string;
    FFilterKeys: string;
    FFilterClause: string;
    FSortSQL: string;
    procedure BuildAncestryList(const AKey: TKeyString; AList, ADistinctList: TStringList);
    function GetPrefKey(const AKey: TKeyString): string;
    function GetParentKey(const AItemKey: TKeyString): TKeyString;
    procedure SetListKeys(const Value: string);
    procedure SetFilterClause(const Value: string);
    procedure SetSortSQL(const Value: string);
  protected
    function GetFilteredKeys: String; virtual;
    function GetNodeFilteredState(const key: String): Boolean; virtual;
    function GetNodeHint(const key: String): String; virtual;
    procedure PopulateNodes(const ADataset: TDataset; const AParentNode: TFlyNode); virtual; abstract;
    procedure PopulateTree(const ADataset: TDataset; const AParentNode: TFlyNode);
    procedure SetDatabase(const ADBLocation: TDBLocation);
    procedure SetupChildLevel; virtual;
    property NodeToExpand: TFlyNode read FNodeToExpand;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindItem(const AListItemKey: TKeyString): TKeyList;
    function GetIndexFromKey(const AKey: TKeyString): Integer;
    procedure PopulateCheckList(const ExcludeCDLists: Boolean = false);
    procedure PopulateChildNodes(const AParentNode: TFlyNode);
    function PopulateTopLevel(const AListKeyData: TKeyData): Boolean; virtual; abstract;
    procedure SetFilter(const AKeyList: TKeyList);
    procedure ClearFilter;
    property List: TComboBox read FList write FList;
    property Tree: TRapidTree read FTree write FTree;
    property ListKeys: string read FListKeys write SetListKeys;
    property FilterClause: string read FFilterClause write SetFilterClause;
    property FilterKeys: String read FFilterKeys;
    property SortSQL: string read FSortSQL write SetSortSQL;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, GeneralData, maintbar;

resourcestring
  ResStr_QueryLimits =  'The criteria returns too much information for a simple' +
                     ' filter.  Not all data will be returned.';
  ResStr_WrongDatabaseVersion = 'Incorrect Database Version'#13#10#13#10 +
                               'The database you are trying to use is not valid.';
  ResStr_RequriesTRapidTree = ' requires a TRapidTree for population of children to occur.';
  ResStr_ApplyingFilter = 'Applying filter...';

//==============================================================================
{ TdmDictionaryGenericData }
constructor TBaseDictionaryData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDatabase(dblLocal);
  qryTopLevel.ParseSQL := false;
  qryChildLevel.ParseSQL := false;
  FFilterKeys := '';
end;  // Create

//==============================================================================
destructor TBaseDictionaryData.Destroy;
begin
  // 'Disconnect' the Combobox and Treeview
  FList := nil;
  FTree := nil;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TBaseDictionaryData.SetDatabase(const ADBLocation: TDBLocation);
var liIdx    : Integer;
    loDataset: TADODataset;
begin
  //Examine each component
  for liIdx := 0 to Self.ComponentCount - 1 do
    //If the current component is a TttDBDataset (a Titan TDBDataset) then set the relevant properties
    if Components[liIdx] is TADODataset then begin
      loDataset := TADODataset(Components[liIdx]);
      // Close any opened Dataset before changing Database/Session properties
      if loDataset.Active then loDataset.Close;
      dmDatabase.SetDatabaseLocal([loDataset]);
    end;
end;  // SetDatabase

//==============================================================================
procedure TBaseDictionaryData.BuildAncestryList(const AKey: TKeyString;
  AList, ADistinctList: TStringList);
var lParentKey: TKeyString;
begin
  // Only continue if we have never seen this key before
  if ADistinctList.IndexOf(AKey)=-1 then begin
    ADistinctList.Add(AKey);
    // Move up the hierarchy of locations to the top one
    with qryParent do begin
      // Get the parent key for the current one
      Parameters.ParamByName('Key').Value := AKey;
      Open;
      lParentKey := FieldByName('Parent').AsString;
      Close;
    end;
    // now checks that there isn't a self-parented node, as this would cause an infinite recursion
    if (lParentKey <> '') and (lParentKey <> AKey) then begin
      BuildAncestryList(lParentKey, AList, ADistinctList);  // Get the parent location
      AList.Add(lParentKey);  // Add this locaiton to list
    end;
  end;
end;  // BuildAncestryList

{-------------------------------------------------------------------------------
}
procedure TBaseDictionaryData.SetFilter(const AKeyList: TKeyList);
var lUniqueKeys, lKeys, lDistinctKeys: TStringList;
    i                 : Integer;
    lCursor           : TCursor;
    key: String;
begin
  // Create temp lists
  lUniqueKeys := TStringList.Create;
  lKeys       := TStringList.Create;
  // string list to allow us to prevent any sort of loop when calculating a hierarchy
  lDistinctKeys := TStringList.Create;
  lDistinctKeys.Sorted := True;
  // Set Sorted to true to discard duplicates. Does not work if list already populated
  lUniqueKeys.Sorted     := true;
  lUniqueKeys.Duplicates := dupIgnore;

  lCursor := HourglassCursor;
  frmMain.SetStatus(ResStr_ApplyingFilter);
  try
    with AKeyList do
      for i := 0 to Header.ItemCount - 1 do begin
        frmMain.ProgressBar.TaskPosition := 100 * i div Header.ItemCount;
        // Get the top parent for each item by building the ancestry list
        BuildAncestryList(Items[i].KeyField1, lKeys, lDistinctKeys);
        if qryPreferredKey.SQL.Text <> '' then begin// have a preferred key query to run
          key := GetPrefKey(Items[i].KeyField1);
          if key <> '' then lKeys.Add(key);
        end else
          lKeys.Add(Items[i].KeyField1);  // SHOULD BE PREFERRED NAME
        // Duplicates should be ignored when adding to this list
        lUniqueKeys.AddStrings(lKeys);
        // Clear for next round
        lKeys.Clear;
        // Check for Access Query Limits and stop if reached
        if lUniqueKeys.Count >= 3000 then begin // 64K limit of SQL approaching!
          MessageDlg(ResStr_QueryLimits, mtInformation, [mbOk], 0);
          Break; // from loop - we want no more data
        end;
      end;
    if lUniqueKeys.Count > 0 then
      FFilterKeys := '''' + StringReplace(lUniqueKeys.CommaText, ',', ''',''', [rfReplaceAll]) + '''';
  finally
    lKeys.Free;
    lUniqueKeys.Free;
    lDistinctKeys.Free;
    frmMain.TaskFinished;
    frmMain.SetStatus('');
    DefaultCursor(lCursor);
  end;
end;  // SetFilter

//==============================================================================
function TBaseDictionaryData.GetPrefKey( const AKey: TKeyString): String;
begin
  with qryPreferredKey do
  begin
    Parameters.ParamByName('Key').Value := AKey;
    Open;
    Result := FieldByName('Preferred_Name').AsString;
    Close;
  end;
end;  // GetPrefKey

//==============================================================================
procedure TBaseDictionaryData.PopulateCheckList(const ExcludeCDLists: Boolean = false);
var lsList       : String;
    lNewKey      : TKeyData;
    lIsLocalField: TField;
begin
  if Assigned(FList) then
    with qryList do begin
      Close;
      Open;
      lIsLocalField := FindField('IsLocal');
      First;
      while not Eof do begin
        lNewKey := TKeyData.Create;
        lNewKey.ItemKey := FieldByName('KeyField').AsString; // Store list key
        { if we have a dummy field 'VersionKey' in query then obtain the latest version of the list }

        if Assigned(lIsLocalField) then
        begin
          if lIsLocalField.AsBoolean then
            lNewKey.ItemAdditional := 'LOCAL'
          else
            lNewKey.ItemAdditional := 'CD';
        end else
          // Assume local if nothing to say otherwise
          lNewKey.ItemAdditional := 'LOCAL';

        lsList := FieldByName('DisplayField').AsString;

        // If must exclude CD Lists, free new object and carry on, else add it to combobox
        if ExcludeCDLists and (lNewKey.ItemAdditional = 'CD') then
          lNewKey.Free
        else
          FList.Items.AddObject(lsList, lNewKey);
        Next;
      end; //while
      Close;
      FList.ItemIndex := -1;
    end; //with
end;  // PopulateCheckList

//==============================================================================
function TBaseDictionaryData.GetIndexFromKey(const AKey: TKeyString): Integer;
var lIdx : Integer;
begin
  Result := -1;
  if Assigned(FList) then
    if AKey <> '' then
      with FList.Items do
        for lIdx := 0 to Count - 1 do
          if Assigned(Objects[lIdx]) and (TKeyData(Objects[lIdx]).ItemKey = AKey) then
          begin
            Result := lIdx;
            Break;
          end;
end;  // GetIndexFromKey

//==============================================================================
procedure TBaseDictionaryData.PopulateTree(const ADataset: TDataset;
  const AParentNode: TFlyNode);
begin
  try
    FNodeToExpand := nil;
    ADataset.Close;
    ADataset.Open;
    ADataset.First;
    PopulateNodes(ADataset, AParentNode);
  finally
    ADataset.Close;
  end;
end;  // PopulateTree

//==============================================================================
procedure TBaseDictionaryData.PopulateChildNodes(const AParentNode: TFlyNode);
var loChildNode: TFlyNode;
begin
  loChildNode := AParentNode.GetFirstChild;
  // Check if it is 'fake' child node
  if (loChildNode <> nil) and (loChildNode.Text = '.') then
    AParentNode.DeleteChildren;

  if Tree <> nil then begin
    FNodeToExpand := AParentNode;
    SetupChildLevel;
    PopulateTree(qryChildLevel, AParentNode);
    TDictionaryNode(AParentNode.Data).ChildrenPopulated := true;
  end else
    Raise EUndefinedTarget.Create(ClassName + ResStr_RequriesTRapidTree);
end;  // PopulateChildNodes

//------------------------------------------------------------------------------
procedure TBaseDictionaryData.SetupChildLevel;
begin
  qryChildLevel.SQL.Text := ppChildLevel.Content;
end;  // SetupChildLevel

//==============================================================================
function TBaseDictionaryData.FindItem(const AListItemKey: TKeyString): TKeyList;
var lChainKeys: TEditableKeyList;
    lParentKey: TKeyString;
begin
  Result := nil;
  lChainKeys := TEditableKeyList.Create;     // create new key list
  try
    lChainKeys.AddItem(AListItemKey,'');   // found item is first in list
    // find parent of itemkey and then find parent of this parent
    // until either lParentKey = '' i.e. top level or
    // key is equal to the supplied key iItemKey
    lParentKey := AListItemKey;
    repeat
      lParentKey := GetParentKey(lParentKey); // get parent of found item
      if lParentKey <> '' then lChainKeys.AddItem(lParentKey,'');
    until (lParentKey = '');

    if Assigned(lChainKeys) then
      // return the keylist
      Result := lChainKeys;
  except
    lChainKeys.Free;
  end;
end;  // FindItem

//==============================================================================
// function to get item key of parent of supplied item key
function TBaseDictionaryData.GetParentKey(const AItemKey: TKeyString): TKeyString;
begin
  Result := '';
  with qryParent do
    try
      Parameters.ParamByName('Key').Value := AItemKey;
      Open;
      if not Eof then Result := FieldByName('Parent').AsString;
    finally
      Close;
    end;
end;  // GetParentKey

{-------------------------------------------------------------------------------
}
function TBaseDictionaryData.GetFilteredKeys: String;
begin
  Result := FFilterKeys;
end;

{-------------------------------------------------------------------------------
  Replace tags in the child level query with the actual filters, sorts etc
}
procedure TBaseDictionaryData.ppPopulateQueryTag(Sender: TObject;
  Tag: TTag; const TagString: String; TagParams: TStrings;
  var ReplaceText: String);
var
  filteredKeys: String;
begin
  if TagString = 'ParentKey' then
    ReplaceText := TNodeObject(NodeToExpand.Data).ItemKey
  else if TagString = 'Filter' then begin
    filteredKeys := GetFilteredKeys;
    if filteredKeys = '' then
      ReplaceText := ''
    else
      ReplaceText := Format(FFilterClause, [filteredKeys]);
  end
  else if TagString = 'OrderBy' then
    ReplaceText := FSortSQL
  else if TagString = 'ListKeys' then
    ReplaceText := FListKeys;
end;

{-------------------------------------------------------------------------------
  Accessor - key(s) that identify the current list
}
procedure TBaseDictionaryData.SetListKeys(const Value: string);
begin
  FListKeys := Value;
end;

{-------------------------------------------------------------------------------
  SQL Clause for the filter, inserted only when the filter is in used
}
procedure TBaseDictionaryData.SetFilterClause(const Value: string);
begin
  FFilterClause := Value;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TBaseDictionaryData.SetSortSQL(const Value: string);
begin
  FSortSQL := Value;
end;

{-------------------------------------------------------------------------------
  Removes any applied filter
}
procedure TBaseDictionaryData.ClearFilter;
begin
  FFilterKeys := '';
end;

//==============================================================================
function TBaseDictionaryData.GetNodeFilteredState(const key: String): Boolean;
begin
  Result := False;
end;

//==============================================================================
function TBaseDictionaryData.GetNodeHint(const key: String): String;
begin
  Result := '';
end;

end.
