//==============================================================================
//  Unit:        BaseDictionaryUnit
//
//  Implements:  TBaseDictionary
//
//  Description: Defines the base class for all dictionaries, and implements
//               the common functions available for each dictionary (Search,
//               Sort, Filter, ...)
//
//  Author:      Eric Salmon
//  Created:     23 Nov 2001
//
//  Last Revision Details:
//    $Revision: 45 $
//    $Date: 19/03/10 12:50 $
//    $Author: Robertjohnson $
//
//==============================================================================

unit BaseDictionaryUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseChildUnit, exgrid, RapTree, StdCtrls, ExtCtrls, ComCtrls, Constants,
  GeneralFunctions, BaseDictionaryDataUnit, DataClasses, HierarchyNodes,
  DropSource, Find, ExceptionForm, Menus, CRCommonClasses, ExternalFilter,
  KeyboardRapidTree;

type
  EDictionary = class(TExceptionPath);
  ENoItemFound = class(EDictionary);

  TBaseDictionary = class(TBaseChild)
    pnlSelection: TPanel;
    lblListName: TLabel;
    cmbList: TComboBox;
    btnShowAll: TButton;
    DictSplitter: TSplitter;
    pcBrowser: TPageControl;
    tsTree: TTabSheet;
    tvDictionary: TKeyboardRapidTree;
    pnlDetails: TPanel;
    procedure DictSplitterPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure cmbListChange(Sender: TObject);
    procedure btnShowAllClick(Sender: TObject);
    procedure tvDictionaryChange(Sender: TObject; Node: TFlyNode);
    procedure tvDictionaryCollapsing(Sender: TObject; Node: TFlyNode;
      var AllowCollapse: Boolean);
    procedure tvDictionaryCollapsed(Sender: TObject; Node: TFlyNode);
    procedure tvDictionaryCompare(Sender: TObject; Node1, Node2: TFlyNode;
      var Compare: Integer);
    procedure tvDictionaryExpanding(Sender: TObject; Node: TFlyNode;
      var AllowExpansion: Boolean);
    procedure tvDictionaryDrawCell(Sender: TObject; aCanvas: TCanvas; ACol,
      ARow: Integer; Rect: TRect; State: TExGridDrawState);
  private
    FDictionaryData: TBaseDictionaryData;
    FExcludeCDLists: Boolean;
    FTableName: String;
    FSortOrder: String;
    FCollapsing: Boolean;
    FClearingTree: Boolean;
    FInitialising: Boolean;
    FInitialListKey: TKeyString;
    FCurrentListKey: TKeyString;
    FSelectedNode  : TFlyNode;     // Node selection indicated by OnChange method
    FNoMatchMessage: String;
    function GetListKeyData: TKeyData;
    procedure SetDictionaryData(const Value: TBaseDictionaryData);
    procedure WMShowDetails(var Msg: TMessage); message WM_SHOW_DETAILS;
    procedure UpdateDisplay;
    procedure SetSortOrder(const Value: String);
  protected
    function DefaultSortField: string; virtual; abstract;
    function SortRegistryName: string; virtual; abstract;
    procedure ReadSortOrders; override;
    procedure WriteSortOrders; override;
    function SortNameToSortSQL(const ASortName: string): string; virtual; abstract;
    procedure ClearBrowser; virtual;
    procedure ClearList; virtual;
    function CompareDictionaryItems(const Data1, Data2: TDictionaryNode; const ASortOrder: String): Integer; virtual; abstract;
    procedure FilterDictionary(const ATableName, AFilterMatch, AdditionalFilter: String);
    function FindDictionaryItem(const ADefaultKey: TKeyString; const ASearchTitle: String;
      const ASearchType: TFindType; ASearchString: String = ''): TKeyString; virtual;
    function FindNodeByKey(const AKey: TKeyString; const ANode: TFlyNode): TFlyNode;
    procedure FreeObjects; virtual;
    procedure GetDictionaryNodeData(const Sender: TObject; var oDropSource: TJNCCDropSource);
    function GetExcludeCDLists: Boolean; virtual;
    function GetIndexFromKey(const AKey: TKeyString): Integer;
    function GetListForItem(const AKey: string): string; virtual; abstract;
    procedure PopulateDetails; virtual; abstract;
    procedure SetupDestinationControls; override;
    procedure SetupHelp; virtual; abstract;
    procedure SetupObjects; virtual; abstract;
    procedure ShowDetails(const Reveal: Boolean); virtual;
    procedure SortDictionary(const ASortOrder, ASortSQL: String);
    procedure SortDictionaryAdditional; virtual;
    procedure SynchronizeAdditionalControls; virtual;
    procedure UpdateMenus; virtual; abstract;
    procedure UpdateListKeySetting; virtual;  // Not abstract. Not always needed    
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
        override;
    property DictionaryData: TBaseDictionaryData read FDictionaryData write SetDictionaryData;
    property InitialListKey: TKeyString read FInitialListKey write FInitialListKey;
    property TableName: String read FTableName write FTableName;
    property SortOrder: String read FSortOrder write SetSortOrder;
    function GetTreeView: TRapidTree; override;
    procedure ApplyFilter(AKeyList: TKeyList); override;
    property TreeView: TRapidTree read GetTreeView;
  public
    constructor Create(AOwner: TComponent); override;
    function GetKeyList: TKeyList; override;
    procedure LocateDictionaryItem(const AItemKey: TKeyString; const AMsgText: String); virtual;
    procedure SelectList(const AListKey: TKeyString);
    function SelectNode(const ANodeType, ANodeKey: string): TFlyNode; override;
    procedure ExpandAllNodes; override;
    procedure FilterToKeyList(const AKeyList: TKeyList);
    procedure DisplayFilteredItems;
    function ItemKey: string; override;
    function CustomReportKeyType: TKeyType; override;
    property ListKeyData: TKeyData read GetListKeyData;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  Maintbar, GeneralData, ApplicationSettings;

resourcestring
  ResStr_NotFound = 'The specified %s was not found in the current list.';
  ResStr_NoMatchFound = 'No Items were found matching the search criteria.';
  ResStr_LocatingItemHeir = 'Locating item in hierarchy...';

//==============================================================================
constructor TBaseDictionary.Create(AOwner: TComponent);
var
  lCursor: TCursor;
begin
  FInitialising := true;
  FExcludeCDLists := GetExcludeCDLists;
  FCurrentListKey := '';
  inherited Create(AOwner);
  lCursor := HourglassCursor;
  try
    FInitialising := false;
    with tvDictionary do
      if (Items.Count > 0) then Selected := Items.GetFirstNode;
    SetupObjects;
    SortDictionary(FSortOrder, SortNameToSortSQL(FSortOrder));
    DictionaryData.PopulateCheckList(FExcludeCDLists,false);
    SelectList(InitialListKey);
    SetupHelp;
  finally
    DefaultCursor(lCursor);
  end;
  FormResize(nil);
end;  // Create

//==============================================================================
procedure TBaseDictionary.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  frmMain.ClearContextToolbar(false);
  Action := caFree;
end;  // FormClose

//==============================================================================
procedure TBaseDictionary.FormDestroy(Sender: TObject);
begin
  inherited;
  ClearList;
  ClearBrowser;
  FreeObjects;
  FDictionaryData.Free;
  AppSettings.ClearFilteredRecords([TableName]);
end;  // FormDestroy

//==============================================================================
procedure TBaseDictionary.FormResize(Sender: TObject);
begin
  inherited;
  cmbList.Left := lblListName.Width + 16;
  if btnShowAll.Visible then
    cmbList.Width := pnlSelection.Width - cmbList.Left - btnShowAll.Width - 21
  else
    cmbList.Width := pnlSelection.Width - cmbList.Left - 9;
  btnShowAll.Left := pnlSelection.Width - btnShowAll.Width - 9;
  tvDictionary.Refresh;
end;  // FormResize

//==============================================================================
procedure TBaseDictionary.SetDictionaryData(const Value: TBaseDictionaryData);
begin
  FDictionaryData := Value;
  FDictionaryData.List := cmbList;
  FDictionaryData.Tree := tvDictionary;
end;  // SetDictionaryData

//==============================================================================
// Override in descendents if CD lists have to be excluded
function TBaseDictionary.GetExcludeCDLists: Boolean;
begin
  Result := false;
end;  // GetExcludeCDLists

//==============================================================================
procedure TBaseDictionary.FreeObjects;
begin
end;  // FreeObjects

//==============================================================================
procedure TBaseDictionary.ClearList;
var liCount: integer;
begin
  with cmbList.Items do begin
    for liCount := 0 to Count-1 do
      TKeyData(Objects[liCount]).Free;
    Clear;
  end;
end;  // ClearList

//==============================================================================
procedure TBaseDictionary.ClearBrowser;
var liCount: Integer;
begin
  LockWindowUpdate(Handle);
  FClearingTree := true;  // Flag to stop the tree OnChange event doing anything
  try
    with tvDictionary do begin
      for liCount := 0 to Items.Count - 1 do
        TNodeObject(Items[liCount].Data).Free;
      Items.Clear;
    end;
  finally
    FClearingTree := false;  // Reset flag for tree OnChange event
    LockWindowUpdate(0);
  end;
end;  // ClearBrowser

//==============================================================================
procedure TBaseDictionary.SelectList(const AListKey: TKeyString);
var liIdx: Integer;
begin
  with cmbList do begin
    ItemIndex := GetIndexFromKey(AListKey);
    // Key found and valid
    if ItemIndex <> -1 then
      cmbListChange(nil)
    else if Items.Count > 0 then begin
      // This is in case the list key is missing (='')
      // So find first local and use it by default.
      for liIdx := 0 to Items.Count - 1 do
        if TKeyData(Items.Objects[liIdx]).ItemAdditional = 'LOCAL' then begin
          ItemIndex := liIdx;
          Break;
        end;
      // Refresh only if a local list was found
      if ItemIndex <> -1 then cmbListChange(nil);
    end;
  end;
  with tvDictionary do
    if (Items.Count > 0) and (Selected <> Items.GetFirstNode) then
      Selected := Items.GetFirstNode;
end;  // SelectList

//==============================================================================
procedure TBaseDictionary.DictSplitterPaint(Sender: TObject);
begin
  inherited;
  DrawVertSplitter(Canvas, DictSplitter);
end;  // DictSplitterPaint

//==============================================================================
procedure TBaseDictionary.btnShowAllClick(Sender: TObject);
var lCursor:TCursor;
  nodeChain: TStringList;
begin
  inherited;
  btnShowAll.Visible := false;
  lCursor := HourglassCursor;
  nodeChain := TStringList.Create;
  try
    RememberPathToSelectedNode(tvDictionary, nodeChain);
    DictionaryData.ClearFilter;
    AppSettings.ClearFilteredRecords([TableName]);
    ClearFilter(Format(FILTER_DICTIONARY, [FCurrentListKey]));
    // Reset TreeView and repopulate
    UpdateDisplay;
    RedisplaySelectedNode(tvDictionary, nodeChain);
  finally
    DefaultCursor(lCursor);
    nodeChain.Free;
  end;
  FormResize(nil);
end;  // btnShowAllClick

//==============================================================================
procedure TBaseDictionary.FilterDictionary(const ATableName, AFilterMatch, AdditionalFilter: String);
begin
  inherited;
  FNoMatchMessage := Format(ResStr_NoFilterMatch, [AFilterMatch, StringReplace(Lowercase(lblListName.Caption), ':', '', [])]);
  GetFilter(Format(FILTER_DICTIONARY, [FCurrentListKey]), ATableName, AdditionalFilter);
end;  // FilterDictionary

//==============================================================================
procedure TBaseDictionary.ApplyFilter(AKeyList: TKeyList);
var
  lCursor: TCursor;
begin
  if AKeyList.Header.ItemCount = 0 then
    MessageDlg(FNoMatchMessage, mtInformation, [mbOk], 0)
  else begin
    lCursor := HourglassCursor;
    frmMain.SetStatus(ResStr_FilteringInProgress);
    try
      DictionaryData.SetFilter(AKeyList);
      UpdateDisplay;
      // In some cases (the admin dict) the filter query does not filter to a specific list, because there
      // can be multiple lists in the hierarchy. So we have to check afterwards that the filter resulted
      // in something being displayed.
      if tvDictionary.Items.Count=0 then begin
        MessageDlg(FNoMatchMessage, mtInformation, [mbOk], 0);
      end;
    finally
      DefaultCursor(lCursor);
      frmMain.SetStatus('');
      btnShowAll.Visible := True;
      FormResize(nil);
    end;
  end;
end;

//==============================================================================
procedure TBaseDictionary.ShowDetails(const Reveal: Boolean);
begin
  if Reveal then begin
    pnlDetails.Visible := true;
    DictSplitter.Visible := true;
    // Make sure the splitter falls between tree and panel.
    DictSplitter.Left := pnlDetails.Left - 1;
  end else begin
    pnlDetails.Visible := false;
    DictSplitter.Visible := false;
  end;
end;  // ShowDetails

//==============================================================================
procedure TBaseDictionary.cmbListChange(Sender: TObject);
begin
  inherited;
  // Remove filter before changing list.
  if btnShowAll.Visible then begin
    DictionaryData.ClearFilter;
    btnShowAll.Visible := False;
  end;

  if not AppSettings.RememberFilters then
    UpdateDisplay
  else
  if not LoadFilter(Format(FILTER_DICTIONARY, [ListKeyData.ItemKey]), False) then
    UpdateDisplay;
end;

//==============================================================================
procedure TBaseDictionary.UpdateDisplay;
var liInitialKeyIndex, liCurrentKeyIndex: Integer;
    lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    // Clear any filter before changing list
    tvDictionary.Selected := nil;
    ClearBrowser;
    if (ListKeyData <> nil) and DictionaryData.PopulateTopLevel(ListKeyData) then begin
      with tvDictionary do
        if Items.Count > 0 then
          Selected := Items.GetFirstNode
        else begin
          Selected := nil;
          tvDictionaryChange(tvDictionary, nil); { force call to event handler }
        end;
      FCurrentListKey := ListKeyData.ItemKey;
      UpdateListKeySetting;
    end else
      // If opened the screen with CD list and CD not ready, get the Recorder 3.3 list
      with cmbList do begin
        // Initial key list can be a CD list, so account for this scenario
        liCurrentKeyIndex := GetIndexFromKey(FCurrentListKey);
        liInitialKeyIndex := GetIndexFromKey(FInitialListKey);
        if (ItemIndex <> liCurrentKeyIndex) and (TKeyData(Items.Objects[liCurrentKeyIndex]).ItemAdditional = 'LOCAL') then
          // If list selected is different from previously selected, go back to it
          ItemIndex := liCurrentKeyIndex
        else if (ItemIndex <> liInitialKeyIndex) and (TKeyData(Items.Objects[liInitialKeyIndex]).ItemAdditional = 'LOCAL') then
          // If list selected is different from initial list, go for that one
          ItemIndex := liInitialKeyIndex
        else
          // And if all else failed, go for Recorder 3.3 check list
          ItemIndex := Items.IndexOf('Recorder 3.3 (1998)');

        // Reset the screen, try to get to check list again
        cmbListChange(nil);
      end;
    // Make sure all controls keep up with what is going on
    SynchronizeAdditionalControls;
    // And the menus
    UpdateMenus;
  finally
    DefaultCursor(lCursor);
    FormResize(nil);
  end;
end;  // cmbListChange

//==============================================================================
procedure TBaseDictionary.UpdateListKeySetting;
begin
  // Do nothing here. Override in inherited forms to do something
  // if and when necessary
end;  // UpdateListKeySetting

//==============================================================================
procedure TBaseDictionary.SynchronizeAdditionalControls;
begin
  // Nothing to do here, and not necessarily in descendents either
end;  // SynchronizeAdditionalControls

//==============================================================================
function TBaseDictionary.GetIndexFromKey(const AKey: TKeyString): Integer;
var liIdx: Integer;
begin
  Result := -1;
  if AKey <> '' then
    with cmbList.Items do
      for liIdx := 0 to Count - 1 do
        if Assigned(Objects[liIdx]) and (TKeyData(Objects[liIdx]).ItemKey = AKey) then
        begin
          Result := liIdx;
          Break;
        end;
end;  // GetIndexFromKey

//==============================================================================
function TBaseDictionary.GetListKeyData: TKeyData;
begin
  Result := nil;
  with cmbList do
    if (Items.Count > 0) and (ItemIndex > -1) then
      Result := TKeyData(Items.Objects[ItemIndex]);
end;  // GetListKeyData

//==============================================================================
procedure TBaseDictionary.tvDictionaryChange(Sender: TObject; Node: TFlyNode);
begin
  inherited;
  // Try to limit the number of times the WM_SHOW_DETAILS message is posted
  if not FCollapsing and not FClearingTree and
     not FInitialising and (FSelectedNode <> Node) then
  begin
    PostMessage(Handle, WM_SHOW_DETAILS, 0, 0);
    // Now notify whoever is interested
    FSelectedNode := Node;
    NotifyDataItemChange;
  end;
end;  // tvDictionaryChange

//==============================================================================
procedure TBaseDictionary.tvDictionaryCollapsing(Sender: TObject;
  Node: TFlyNode; var AllowCollapse: Boolean);
begin
  inherited;
  LockWindowUpdate(Handle);
  FCollapsing := true;
end;  // tvDictionaryCollapsing

//==============================================================================
procedure TBaseDictionary.tvDictionaryCollapsed(Sender: TObject;
  Node: TFlyNode);
begin
  inherited;
  FCollapsing := false;
  LockWindowUpdate(0);
end;  // tvDictionaryCollapsed

//==============================================================================
procedure TBaseDictionary.tvDictionaryExpanding(Sender: TObject; Node: TFlyNode;
  var AllowExpansion: Boolean);
var
  lCursor: TCursor;
begin
  inherited;
  // Lock updates, window and tree items
  tvDictionary.Items.BeginUpdate;
  lCursor := HourglassCursor;
  try
    if (Node <> nil) and not TDictionaryNode(Node.Data).ChildrenPopulated then
      DictionaryData.PopulateChildNodes(Node);
  finally
    DefaultCursor(lCursor);
    tvDictionary.Items.EndUpdate;
  end;
end;  // tvDictionaryExpanding

//==============================================================================
function TBaseDictionary.FindNodeByKey(const AKey: TKeyString;
  const ANode: TFlyNode): TFlyNode;
var liIndex : Integer;
begin
  Result := nil;
  if ANode = nil then begin
    with tvDictionary do
      for liIndex := 0 to Items.Count -1 do
        if Assigned(Items[liIndex].Data) then
          if TDictionaryNode(Items[liIndex].Data).ItemKey = AKey then
          begin
            Result := Items[liIndex];
            Break;
          end;
  end else begin
    for liIndex := 0 to ANode.Count -1 do
      if Assigned(ANode.Item[liIndex].Data) then
        if TDictionaryNode(ANode.Item[liIndex].Data).ItemKey = AKey then
        begin
          Result := ANode.Item[liIndex];
          Break;
        end;
  end;
end;  // FindNodeByKey

//==============================================================================
function TBaseDictionary.GetKeyList: TKeyList;
var lklKeys: TEditableKeyList;
begin
  if Assigned(FSelectedNode) and Assigned(FSelectedNode.Data) then
  begin
    lklKeys := TEditableKeyList.Create;
    lklKeys.SetTable(TableName);
    if Assigned(FSelectedNode) then
      if Assigned(FSelectedNode.Data) then
        if TObject(FSelectedNode.Data) is TDictionaryNode then
          lklKeys.AddItem(TDictionaryNode(FSelectedNode.Data).ItemKey, '');
    Result := lklKeys;
  end else
    Result := nil;
end;  // GetKeyList;

//==============================================================================
procedure TBaseDictionary.GetDictionaryNodeData(const Sender: TObject;
  var oDropSource: TJNCCDropSource);
begin
  oDropSource.DropData.SetTable(TableName);
  if tvDictionary.Selected <> nil then
    oDropSource.DropData.AddItem(TDictionaryNode(tvDictionary.Selected.Data).ItemKey, '');
end;  // GetDictionaryNodeData

//==============================================================================
procedure TBaseDictionary.SortDictionary(const ASortOrder, ASortSQL: String);
var lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    FSortOrder := ASortOrder;
    // Do the sort
    tvDictionary.SortType := stData;
    // Update queries
    if Assigned(DictionaryData) then
      DictionaryData.SortSQL := ASortSQL;
  finally
    Defaultcursor(lCursor);
    // deactivate automatic ordering
    tvDictionary.SortType := stNone;
  end;
  // If anything else has to be done after sorting...
  SortDictionaryAdditional;
end;  // SortDictionary

//==============================================================================
procedure TBaseDictionary.SortDictionaryAdditional;
begin
  // Do nothing here. Override in inherited forms to do something
  // if and when necessary
end;  // SortDictionaryAdditional

//==============================================================================
procedure TBaseDictionary.tvDictionaryCompare(Sender: TObject; Node1,
  Node2: TFlyNode; var Compare: Integer);
begin
  inherited;
  Compare := CompareDictionaryItems(Node1.Data, Node2.Data, SortOrder);
end;  // tvDictionaryCompare

//==============================================================================
function TBaseDictionary.FindDictionaryItem(
  const ADefaultKey: TKeyString;
  const ASearchTitle: String;
  const ASearchType: TFindType;
  ASearchstring: String = ''): TKeyString;

begin
  Result := ADefaultKey;   // Default return value
  if  DictionaryData.ListIsVirtual(ListKeyData.ItemKey) then
      AppSettings.SessionTaxonomicSearchRestriction := ResStr_CurrentChecklist;
  with TdlgFind.CreateDialog(Self, ASearchTitle, ASearchType) do begin
    SetSearchText(ASearchString);
    if ShowModal = mrOk then Result := ItemKey;  // Get the associated key
    Free;
  end;
end;  // FindDictionaryItem

//==============================================================================
{ Finds an item in the current view.  If the item is not found a message is
    displayed (AMsgText should be the name of the item type).  If this param
    is empty then no message is displayed. }
procedure TBaseDictionary.LocateDictionaryItem(const AItemKey: TKeyString;
  const AMsgText: String);
var lklFoundPath: TKeyList;
    liIndex     : Integer;
    loNode      : TFlyNode;
begin
  if (AItemKey <> '') then begin
    lklFoundPath := DictionaryData.FindItem(AItemKey);
    if Assigned(lklFoundPath) then begin
      frmMain.SetStatus(ResStr_LocatingItemHeir);
      LockWindowUpdate(Handle);
      tvDictionary.Items.BeginUpdate;
      try
        loNode := nil;
        for liIndex := lklFoundPath.Header.ItemCount - 1 downto 0 do begin
          loNode := FindNodeByKey(lklFoundPath.Items[liIndex].KeyField1, loNode);
          if Assigned(loNode) then begin
            if liIndex <> 0 then begin
              if not loNode.Expanded then loNode.Expand(false);
            end else
              tvDictionary.Selected := loNode;
          end else
            if AMsgText<>'' then
              raise ENoItemFound.CreateNonCritical(Format(ResStr_NotFound, [AMsgText]));
        end;
      finally
        frmMain.SetStatus('');
        lklFoundPath.Free;
        tvDictionary.Items.EndUpdate;
        LockWindowUpdate(0);
      end; // try
    end else
      if AMsgText<>'' then
        MessageDlg(ResStr_NoMatchFound, mtInformation, [mbOK], 0);
  end;
end;  // LocateDictionaryItem

//==============================================================================
procedure TBaseDictionary.SetupDestinationControls;
begin
  RegisterDragComponent(tvDictionary, GetDictionaryNodeData);
end;  // SetupDestinationControls

{-------------------------------------------------------------------------------
}
function TBaseDictionary.SelectNode(const ANodeType,
  ANodeKey: string): TFlyNode;
begin
  LocateDictionaryItem(ANodeKey, ANodeType);
  Result := tvDictionary.Selected;
end;

{-------------------------------------------------------------------------------
  Display only nodes that match a given keylist
}
procedure TBaseDictionary.FilterToKeyList(const AKeyList: TKeyList);
begin
  // Reset TreeView and repopulate
  if AKeyList.Header.ItemCount>0 then
    SelectList(GetListForItem(AKeyList.Items[0].KeyField1));
  DictionaryData.SetFilter(AKeyList);
  cmbListChange(nil);
  btnShowAll.Visible := True;
  FormResize(nil);
  // if only one item, ensure its parents are expanded to show it
  if AKeyList.Header.ItemCount=1 then
    LocateDictionaryItem(AKeyList.Items[0].KeyField1, '');
end;

{-------------------------------------------------------------------------------
}
procedure TBaseDictionary.DisplayFilteredItems;
var
  i: Integer;
  keyList: TEditableKeyList;
  filteredKeys: TStringList;
begin
  keyList := TEditableKeyList.Create;
  try
    keyList.SetTable(MIXED_DATA);
    filteredKeys := AppSettings.GetFilteredRecords(TableName);
    if filteredKeys <> nil then begin
      for i := 0 to filteredKeys.Count - 1 do
        keyList.AddItem(filteredKeys[i], TableName);
      FilterToKeyList(keyList);
    end;
    if (tvDictionary.Items.Count = 0) or (FDictionaryData.FilterKeys = '') then
      btnShowAllClick(nil);
  finally
    keyList.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Retrieve current node type for custom reports
}
function TBaseDictionary.CustomReportKeyType: TKeyType;
begin
  Result := inherited CustomReportKeyType;
  with tvDictionary do
    if Assigned(Selected) then
      if TObject(Selected.Data) is TReportableNode then
        Result := TReportableNode(Selected.Data).ReportKeyType;
end;

{-------------------------------------------------------------------------------
  Retrieve current node key for custom reports
}
function TBaseDictionary.ItemKey: string;
begin
  Result := inherited ItemKey;
  with tvDictionary do
    if Assigned(Selected) then
      if TObject(Selected.Data) is TReportableNode then
        Result := TReportableNode(Selected.Data).ItemKey
end;

{-------------------------------------------------------------------------------
  The try..except is to take care of a bug that occurs when closing Recorder 
  while the form still has some messages left to process.
}
procedure TBaseDictionary.WMShowDetails(var Msg: TMessage);
begin
  try
    PopulateDetails;
  except
  end;
end;  // TBaseDictionary.WMShowDetails

procedure TBaseDictionary.ExpandAllNodes;
var
  lMovingNode: TFlyNode;
begin
//  kjg 26/11/2004

  if Assigned(TreeView) then begin
    try
      TreeView.Items.BeginUpdate;
      lMovingNode := TreeView.Items.GetFirstNode;
      while Assigned(lMovingNode) do begin
        lMovingNode.expand(true);
        lMovingNode := lMovingNode.GetNext;
      end;
    finally
      TreeView.Items.EndUpdate
    end
  end;

end;

function TBaseDictionary.GetTreeView: TRapidTree;
begin
  Result := tvDictionary;
end;

{-------------------------------------------------------------------------------
  Generic dictionary method for retrieving a sort order from the registry
}
procedure TBaseDictionary.ReadSortOrders;
var
  ltfAscending: boolean;
begin
  if not AppSettings.ReadSort(Self, SortRegistryName, FSortOrder, ltfAscending) then
    SortOrder := DefaultSortField;
end;

{-------------------------------------------------------------------------------
  Generic dictionary method for writing a sort order from the registry
}
procedure TBaseDictionary.WriteSortOrders;
begin
  AppSettings.WriteSort(Self, SortRegistryName, SortOrder, true);
end;

{-------------------------------------------------------------------------------
  Accessor method which causes the sort to be set against the dictionary list
}
procedure TBaseDictionary.SetSortOrder(const Value: String);
begin
  FSortOrder := Value;
  SortDictionary(FSortOrder, SortNameToSortSQL(FSortOrder));
end;

{-------------------------------------------------------------------------------
  Draw highlighted nodes with coloured text.
}
procedure TBaseDictionary.tvDictionaryDrawCell(Sender: TObject;
  aCanvas: TCanvas; ACol, ARow: Integer; Rect: TRect;
  State: TExGridDrawState);
var
  lNode: TFlyNode;
  lTree: TRapidTree;
  lRect: TRect;
begin
  inherited;
  lTree := (Sender as TRapidTree);
  lNode := lTree.GetNodeAtRow(ARow);
  if Assigned(lNode) then begin
    // Give highlighted nodes a different text colour, unless they are selected.
    if lNode.Selected then
      ACanvas.Font.Color := clHighlightText
    else
    if Assigned(lNode.Data) and (TObject(lNode.Data) is TDictionaryNode) and
        TDictionaryNode(lNode.Data).IsFiltered then begin
      ACanvas.Brush.Color := MergeColours(clHighlight, clWindow, 30);
      ACanvas.Font.Color := GetContrastColour(ACanvas.Brush.Color);
    end
    else begin
      ACanvas.Brush.Color := clWindow;
      ACanvas.Font.Color := clWindowText;
    end;
    lRect := Rect;
    // Taxon Dictionary nodes are indented more than other nodes.
    if TDictionaryNode(lNode.Data) is TTaxonDictionaryNode then 
      lRect.Left := Rect.Left + lTree.Indent * (lNode.Level + 2 + Ord(lNode.StateIndex <> -1))
    else
      lRect.Left := Rect.Left + lTree.Indent * (lNode.Level + Ord(lNode.StateIndex <> -1));
    ACanvas.FillRect(lRect);
    ACanvas.TextOut(lRect.Left + 4, lRect.Top + 1, lNode.Caption);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TBaseDictionary.DoShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo:
    THintInfo);
var
  X, Y: Integer;
  lNode: TFlyNode;
begin
  if (HintInfo.HintControl = tvDictionary) then begin   
    X := Mouse.CursorPos.X - ClientOrigin.X - tsTree.Left - pcBrowser.Left;
    Y := Mouse.CursorPos.Y - ClientOrigin.Y - tsTree.Top - pcBrowser.Top;
    lNode := tvDictionary.GetNodeAt(X, Y);
    if lNode <> nil then
      HintStr := TNodeObject(lNode.Data).Hint;
    CanShow := HintStr <> '';
    if CanShow then
      HintInfo.ReshowTimeout := 200;
  end else
    inherited;
end;  // TBaseDictionary.DoShowHint

end.

