//==============================================================================
//  Unit:        Locations
//
//  Implements:  TfrmLocations
//
//  Description: Contains the hierarchy of locations and features.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 258 $
//    $Date: 29/10/09 16:38 $
//    $Author: Bonnerearle $
//
//==============================================================================

unit Locations;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Clipbrd,
  BaseChildUnit, ComCtrls, ImgList, Menus, StdCtrls, Buttons, ExtCtrls, Db,
  BaseFormUnit, LocationData, JNCCDatasets, DataClasses, ExceptionForm, DropSource,
  DropStruct, DropTarget, BaseExportableUnit, Map, BaseDockedForm, GeneralData,
  Observations, ActnList, OnlineHelp, Constants, exgrid, RapTree, GeneralFunctions,
  ImageListButton, Recorder2000_TLB, SQLConstants, AdoInt, MapServerLink, MapClasses,
  Variants, ADODb, CommCtrl, ADODB_TLB, OleTools, CRCommonClasses, Contnrs, ExternalFilter,
  KeyboardRapidTree,ResourceStrings,HierarchyFunctions;

resourcestring
  ResStr_InvalidSelectNodeType = 'Invalid data type parameter to SelectNode method - %s';

type
  ELocationError = class(TExceptionPath);

  TfrmLocations = class(TBaseExportable, IProvidesOccurrencesSQL, IRecorderDetailScreenEvents)
    LocationSplitter : TSplitter;
    pnlDetails : TPanel;
    pnlButtons : TPanel;
    pnlButtons2 : TPanel;
    pnlDrill : TPanel;
    pnlLabel : TPanel;
    Label1 : TLabel;
    mnuEdit : TMenuItem;
    pmRelatedData : TPopupMenu;
    mnuRelSurveys : TMenuItem;
    mnuRelEvents : TMenuItem;
    mnuRelSamples : TMenuItem;
    mnuRelIndivOrg : TMenuItem;
    mnuRelOccur : TMenuItem;
    mnuRelDocuments: TMenuItem;
    mnuEditAdd : TMenuItem;
    mnuEditEdit : TMenuItem;
    mnuEditDelete : TMenuItem;
    N1 : TMenuItem;
    mnuEditCut : TMenuItem;
    mnuEditCopy : TMenuItem;
    mnuEditPaste : TMenuItem;
    mnuEditReturnData : TMenuItem;
    N2 : TMenuItem;
    mnuEditFind : TMenuItem;
    mnuEditFilter : TMenuItem;
    mnuEditSortBy : TMenuItem;
    N3 : TMenuItem;
    mnuEditBold : TMenuItem;
    mnuEditItalic : TMenuItem;
    mnuEditUnderline : TMenuItem;
    mnuEditAddSite : TMenuItem;
    mnuEditAddSubSite : TMenuItem;
    mnuEditAddFeature : TMenuItem;
    mnuEditSortLocation : TMenuItem;
    mnuEditSortFeatureName : TMenuItem;
    mnuEditSortFeatureType : TMenuItem;
    pmAdd : TPopupMenu;
    pmAddSite : TMenuItem;
    pmAddSubSite : TMenuItem;
    pmAddFeature : TMenuItem;
    pmSortBy : TPopupMenu;
    pmSortFeatureName : TMenuItem;
    pmSortFeatureType : TMenuItem;
    pmHierarchy : TPopupMenu;
    pmHAdd : TMenuItem;
    pmHAddFeature : TMenuItem;
    pmHAddSubSite : TMenuItem;
    pmHAddSite : TMenuItem;
    pmHSortBy : TMenuItem;
    pmHSortFeatureType : TMenuItem;
    pmHSortFeatureName : TMenuItem;
    pmHSortLocationName : TMenuItem;
    shpLocations : TShape;
    ilLocation : TImageList;
    N4 : TMenuItem;
    btnShowAll: TButton;
    alLocations: TActionList;
    actAddSite: TAction;
    actAddSubSite: TAction;
    actAddFeature: TAction;
    actSortSiteName: TAction;
    actSortFeatName: TAction;
    actSortFeatType: TAction;
    actFilter: TAction;
    pmHPromoteNode: TMenuItem;
    mnuEditPromoteNode: TMenuItem;
    actFind: TAction;
    pmSortLocation: TMenuItem;
    actShowMetadata: TAction;
    mnuEditShowMetadata: TMenuItem;
    actFindOnMap: TAction;
    mnuEditFindOnMap: TMenuItem;
    pmHFindOnMap: TMenuItem;
    tvLocations: TKeyboardRapidTree;
    pmHOccurrencesForPlacesReport: TMenuItem;
    bbRelatedData: TImageListButton;
    bbEdit: TImageListButton;
    bbDelete: TImageListButton;
    bbAdd: TImageListButton;
    pmHQuickReports: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    pmHCut: TMenuItem;
    pmHPaste: TMenuItem;
    pmHCopy: TMenuItem;
    pmHMoveTo: TMenuItem;
    mnuEditMoveto: TMenuItem;
    actPromoteNode: TAction;
    actMoveTo: TAction;
    pmHValidateItem: TMenuItem;
    pmHBatchUpdate: TMenuItem;
    mnuPlaceHolder: TMenuItem;
    pmSortFileCode: TMenuItem;
    actSortFileCode: TAction;
    actSortSpatialRef: TAction;
    pmHSortFileCode: TMenuItem;
    pmHSortSpatialRef: TMenuItem;
    pnSortSpatialRef: TMenuItem;
    procedure LocationSplitterPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvLocationsChange(Sender: TObject; Node: TFlyNode);
    procedure bbRelatedDataClick(Sender: TObject);
    procedure bbAddClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure tvLocationsExpanding(Sender: TObject; Node: TFlyNode;
      var AllowExpansion: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bbDeleteClick(Sender: TObject);
    procedure actAddSiteExecute(Sender: TObject);
    procedure actAddSubSiteExcute(Sender: TObject);
    procedure actAddFeatureExecute(Sender: TObject);
    procedure actSortSiteNameExecute(Sender: TObject);
    procedure actSortFeatNameExecute(Sender: TObject);
    procedure actSortFeatTypeExecute(Sender: TObject);
    procedure mnuRelDocumentsClick(Sender: TObject);
    procedure btnShowAllClick(Sender: TObject);
    procedure mnuRelSurveysClick(Sender: TObject);
    procedure mnuRelEventsClick(Sender: TObject);
    procedure mnuRelSamplesClick(Sender: TObject);
    procedure mnuRelOccurClick(Sender: TObject);
    procedure mnuRelIndivOrgClick(Sender: TObject);
    procedure actFilterExecute(Sender: TObject);
    procedure actPromoteNodeExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure bbEditClick(Sender: TObject);
    procedure actShowMetadataExecute(Sender: TObject);
    procedure actFindOnMapExecute(Sender: TObject);
    procedure tvLocationsCompare(Sender: TObject; Node1, Node2: TFlyNode;
      var Compare: Integer);
    procedure tvLocationsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pmHQuickReportsClick(Sender: TObject);
    procedure tvLocationsExit(Sender: TObject);
    procedure pmHierarchyPopup(Sender: TObject);
    procedure actMoveToExecute(Sender: TObject);
    procedure tvLocationsDrawCell(Sender: TObject; aCanvas: TCanvas; ACol,
      ARow: Integer; Rect: TRect; State: TExGridDrawState);
    procedure pmHBatchUpdateClick(Sender: TObject);
    procedure actSortFileCodeExecute(Sender: TObject);
    procedure actSortSpatialRefExecute(Sender: TObject);
  private
    FDetailForm   : TfrmBaseDockedForm;
    FdmLocation   : TdmLocation;
    FSelectedItem : TFlyNode;
    FClearingTree : Boolean;
    FFilteredKeys : TStringList;
    FAddingNewNode: Boolean;
    FFeatureField : String;
    FSortOrder    : String;
    //--------------------------------------------------------------------------
    FNodeMan: ILocationNodeManager;
    FNodeImagesMap: TStringList;
    FDetailScreen: TOleProxy;
    //--------------------------------------------------------------------------
    procedure AddSiteNode(const iSubSite: Boolean);
    procedure BuildAncestryList(const AKey: TKeyString; AList:TStringList;
      const iLocation:Boolean);
    function CheckDeletedNode(const AMode: TEditMode): Boolean;
    procedure CheckDetailFormClass(const AFormClass: TFormClass);
    procedure CheckValidLocationField;
    procedure CheckValidFeatureField;
    procedure ClearTree;
    procedure DragLocation(const Sender : TObject; var oDropSource : TJNCCDropSource);
    procedure DropLocation(const Sender: TObject; const iFormat: Integer;
      const iSourceData: TKeyList; const iTextStrings: TStringList;
      const iIsPasteOperation: Boolean; var ioHandled: Boolean);
    procedure FreeObjects;
    function MapFindLocation: Boolean;
    procedure PerformNodeChange(ANode: TFlyNode);
    procedure PerformSort;
    procedure PopulateChildNodes(const iParent : TFlyNode);
    procedure PopulateTopLevel;
    procedure PopulateTree(const ARecordset: ADOInt._Recordset;  const iParent:TFlyNode;
      const iSiteType:TLocationNodeType);
    procedure RelatedObservations(const iMsg: String;
      const iRelType: TRelatedData; const SQLType: TStandardSQL);
    procedure SetDetailForm(const Value: TfrmBaseDockedForm);
    procedure SetRelatedDataMenuItems;
    procedure SetupObjects;
    procedure UpdateSubMenus(const iNode : TFlyNode);
    procedure WMDiscardLocation(var Msg:TMessage); message WM_DISCARD_LOCATION;
    procedure WMRefreshTermLists(var Msg:TMessage); message WM_REFRESH_TERM_LISTS;
    procedure WMRefreshSpatialRefSystem(var Msg: TMessage); message WM_REFRESH_SPATIAL_REF_SYSTEM;
    procedure WMRefreshDocuments(var Msg: TMessage); message WM_REFRESH_DOCUMENTS;
    procedure WMRefreshNames(var Msg: TMessage); message WM_REFRESH_NAMES;
    procedure WMRefreshColours(var Msg: TMessage); message WM_REFRESH_COLOURS;
    procedure WMImportedComplete(var Msg: TMessage); message WM_IMPORTED_COMPLETE;
    procedure ResetDetailForm(const ACaption: String;
      const AFormClass: TFormClass);
    procedure DragOverCheck(APoint: TPoint; const ATable, AFieldKey: String;
      var Accept: Boolean);
    //--------------------------------------------------------------------------
    procedure AddinMenuClick(Sender: TObject);
    procedure AddinNewNode(ANode: TFlyNode; const ATypeName: String);
    procedure AddinSubMenuClick(Sender: TObject);
    procedure SetSubNodesProperties(const AParentNode: TFlyNode; const AParentNodeTypeID: Integer);
    procedure CleanupAddinMenuItems(AMenuItem: TMenuItem);
    procedure InitializeNodeInfo(var NodeInfo: TNodeInfo);
    function LoadNodeImage(ImageIndex: Integer; SourceImageList: THandle;
      TargetImageList: TImageList; IndexMap: TStringList): Integer;
    procedure PerformAddinNodeChange(const ANode: TFlyNode);
    procedure PopulateAddinNodes(const AParentNode: TFlyNode; const AParentTypeID: Integer);
    procedure ResetAddinDetailScreen;
    procedure SetNodeProperties(ANode: TFlyNode; NodeInfo: TNodeInfo);
    procedure UpdateSubMenusForAddin(const ANode: TFlyNode; AMenus: Array of TMenuItem);
    procedure SetupAddinNode(const ATypeID: Integer; AParentNode,
      ANodeToRefresh: TFlyNode; AFields: Fields);
    function SetNodeInfo(ANode: TFlyNode): TNodeInfo;
    // IProvidesOccurrencesSQL
    function Get_CanProvideSQL: WordBool; safecall;
    function Get_OccurrencesSQL(const ATypes: WideString): WideString; safecall;
    // IRecorderDetailScreenEvents
    procedure OnEditModeChange; safecall;
    procedure OnRefreshScreenInfo(const AKey: WideString; const ACaption: WideString); safecall;
    procedure KillChildren(ANode: TFlyNode);
    procedure UpdateNodeParentKey(AParentNode, AChildNode: TFlyNode);
    function LookForLocation(LocationOnly: Boolean): TCaptionAndKey;
    function InternalLocateNode(const ANodeKey: TKeyString; const ALocation: Boolean): TFlyNode;
    function GetSelectedItems(IncludeFeatures: Boolean = True): TObjectList;
    procedure GetLocationsForFeatures(const keyList: TKeyList; var locationKeys, featureKeys: String);
    procedure GetLocationsForLocations(const keyList: TKeyList; var locationKeys: String);
    procedure SetFormCaption;
    function SuperCompareText(str1, str2: string): integer;
    procedure Tokenise(str: string; parts: TStringList);
  protected
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
        override;
    procedure SetupDestinationControls; override;
    procedure ReadSortOrders; override;
    procedure WriteSortOrders; override;
    function GetCurrentControlEditMode: TEditMode; override;
    function GetDetailsPageControl: TPageControl; override;
    function GetTreeView: TRapidTree; override;
    function GetCustodian: string; override;
    procedure NotifyDataItemChange; override;

    procedure ApplyFilter(AKeyList: TKeyList); override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure AddSiteFromMapObject(const ABaseMapKey: TKeyString;
      AMapServerLink: TMapServerLink; const ALocationDetails: TQueryDetails);
    procedure ApplySecurity; override;
    procedure DisplayLocations(const ALocationList: TKeyList);
    procedure DisplayFeatures(const AFeatureList: TKeyList);
    procedure DisplayFilteredLocations;
    procedure EditSiteFromMap(const ABaseMapKey: TKeyString;
      AMapServerLink: TMapServerLink; const ALocationDetails : TQueryDetails);
    function FindLocation: Boolean;
    function GetKeyList: TKeyList; override;
    function GetKeyListForValidation: TKeyList; override;
    function GotoSiteFromMapObject(const ALocationDetails: TQueryDetails): Boolean;
    function LocateNode(const iNodeText : String; const iNodeKey : TKeyString;
      const iLocation : Boolean) : Boolean;
    function MapLocateNode(const ANodeText: String; ALocationNameKey: string): 
        Boolean;
    procedure SetMenuState(const ANotEditing: Boolean; ANode: TFlynode = nil);
    procedure SetFeature(const AString: String; const AKey:TKeyString);
    procedure SetSite(const AName, ACode, ASpatialRef: String; const AKey:TKeyString);
    procedure ShowFeatureDetails(const AFeatureName: String);
    procedure ShowLocationDetails(const ALocationName: String);
    function SelectNode(const ANodeType, ANodeKey: string): TFlyNode; override;
    procedure ExpandAllNodes ; override;
    procedure UpdateMapWindowSelector; override;
    function ItemKey: string; override;
    function CustomReportKeyType: TKeyType; override;
    property  DetailForm:TfrmBaseDockedForm read FDetailForm write SetDetailForm;
    property  SelectedItem:TFlyNode read FSelectedItem;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  Maintbar, FormActions, LocationDetails, FeatureDetails, HierarchyNodes, ComObj,
  Find, ApplicationSettings, SpatialRefFuncs, References, IndOrg, DatabaseAccessADO;

const
  SITE_IMAGE_INDEX_0 = 0;
  SITE_IMAGE_INDEX_1 = 1;
  SITE_IMAGE_INDEX_2 = 2;
  SITE_IMAGE_INDEX_3 = 3;
  SITE_IMAGE_INDEX_4 = 4;
  FEATURE_IMAGE_INDEX = 5;

  { Fields in the feature query to sort by }
  FEATURE_NAME = 'Item_Name';
  FEATURE_TYPE = 'Short_Name';

  // Sort names
  LOCATION_NAME_SORT = 'Location_Name';
  LOCATION_FILE_CODE_SORT = 'File_Code';
  LOCATION_SPATIAL_REF_SORT = 'Spatial_Ref';
  FEATURE_NAME_SORT = 'F.' + FEATURE_NAME + ', FT.' + FEATURE_TYPE;
  FEATURE_TYPE_SORT = 'FT.' + FEATURE_TYPE + ', F.' + FEATURE_NAME;

  // Setting Table
  SETTING_NAME = 'PrefLocs';

resourcestring
  ResStr_LocationLinked = 'The location has been linked to the selected boundary';
  ResStr_NoLocationToLink = 'There are no locations to link the selected boundary to';
  ResStr_DeletedFromDatabase =  #13#13'It has been deleted from the database.';
  ResStr_SitesOrFeatures = ' Sites and/or Features.';
  ResStr_AnotherSite = 'another Site or a Feature.';
  ResStr_CannotDeleteSite = 'This Site cannot be deleted as it still contains '#13;
  ResStr_Site = 'site ''%s''.';
  ResStr_Feature = 'feature ''%s''.';
  ResStr_Item = 'item ''%s''.';

  ResStr_DeleteQuestion = 'You are about to PERMANENTLY delete the %s'#13#13 +
                          'Are you sure you want to continue?' ;

  ResStr_CannotDeleteItem = 'Unable to delete this item as it is referenced by other records in the database';
  ResStr_HierarchyTask =  'You can only %s Locations and Features from the hierarchy onto the hierarchy.';
  ResStr_CannotMoveItemNotOwner = 'Item cannot be moved. You do not have ownership of the target location.';
  ResStr_TargetNodeSelect = 'A target node must be selected before pasting.';

  ResStr_PasteLocation  = 'You have copied the location "%s" to the clipboard. ' +
                          'Pasting will cause it to be moved under the current location.'#13#13 +
                          'Do you want to proceed?';

  ResStr_PasteLocations = 'You have copied several locations to the clipboard. ' +
                          'Pasting will cause them to be moved under the current location.'#13#13 +
                          'Do you want to proceed?';

  ResStr_MoveItem  = 'You are about to move the item "%s".'#13#13 +
                     'Do you want to proceed?';

  ResStr_MoveItems = 'You are about to move several items.'#13#13 +
                     'Do you want to proceed?';

  ResStr_CannotRelateLocationType = 'There are no Individuals or Organisations to relate this %s to.';
  ResStr_CannotRelateFeature =  'There are no documents to relate this %s to.';

  ResStr_NoMathingFiltering = 'There are no Locations matching the filtering condition.';

  ResStr_FindLocationName = 'Find Location Name';
  ResStr_FindFeatureName  = 'Find Feature Name';

  //ResStr_Location = 'Location';
  ResStr_LBFeature = 'Feature';
  ResStr_LBItem = 'Item';
  ResStr_Locations = 'Locations';
  ResStr_NewLocation = 'New Location';
  ResStr_NewFeature = 'New Feature';
  ResStr_CutAndPaste = 'cut and paste';
  ResStr_DragAndDrop = 'drag and drop';
  ResStr_NoLocRelation =  'There are no %s to relate this location to.';
  ResStr_CannotMoveLocation =
      'The move you have selected is not valid.'#13#10
      + 'A site cannot be moved under one of its own sub-sites.';

//==============================================================================
constructor TfrmLocations.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  SetupObjects;
  FDetailForm  :=nil;
  FSelectedItem:=nil;
  FClearingTree:=False;
  FormResize(nil);

  tvLocations.Items.Clear;
  PopulateTopLevel;

  UpdateSubMenus(tvLocations.Selected);
  SetMenuState(True);
  if DetailForm <> nil then
    tvLocations.SetFocus;

  //Security
  pmHAdd.Enabled := (AppSettings.UserAccessLevel >= ualAddOnly);
  pmHBatchUpdate.Visible := AppSettings.UserAccessLevel >= ualAdmin;
  SendMessage(Handle, WM_UPDATE_MENU_ICONS, 0, 0);

  //Help Setup
  Self.HelpContext   := IDH_LOCATION;
  mnuEdit.HelpContext:= IDH_EDITMENU;
  FormResize(nil);

  LoadFilter(FILTER_LOCATION);
  tvLocations.Selected:=tvLocations.Items.GetFirstNode;
end;  // Create

//==============================================================================
procedure TfrmLocations.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(True);
  frmMain.SetContextToolbar(Self,mnuEdit,5,[nil,nil,nil,nil,nil,nil,nil,
                                            nil,nil,nil,nil,nil,pmSortBy,
                                            nil,nil,nil,nil,nil,nil]);
  UpdateSubMenus(tvLocations.Selected);
  // Context toolbar updated after Create called. So if tree empty, refresh
  // Menu state and toolbar dropdown buttons
  if tvLocations.Items.Count=0 then SetMenuState(True);
end;  // FormActivate

//==============================================================================
procedure TfrmLocations.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  if DetailForm <> nil then begin
    CanClose := DetailForm.CloseQuery;
    if CanClose and (DetailForm <> nil) then begin
      DetailForm.Close;
      FreeAndNil(FDetailForm);
    end;
  end else
  if Assigned(FNodeMan) then begin
    CanClose := True;  // Assume it's ok.
    // If detail screen there, ask if it's really ok to close.
    if Assigned(FDetailScreen) then
      CanClose := (FDetailScreen.ControlInterface as IRecorderDetailScreen).CanClose;
  end else
    CanClose := True;
end;  // CloseQuery

//==============================================================================
procedure TfrmLocations.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  inherited;
  
  Action:=caFree;
end;  // FormClose

//==============================================================================
destructor TfrmLocations.Destroy;
begin
  FreeObjects;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TfrmLocations.SetupObjects;
begin
  FdmLocation := TdmLocation.Create(nil, FSortOrder, FFeatureField);
  FFilteredKeys := TStringList.Create;

  FNodeImagesMap := TStringList.Create;

  with AppSettings.ComAddins do
    if LocationNodeManagers.Count > 0 then
      FNodeMan := CreateComObject(StringToGUID(LocationNodeManagers[0])) as ILocationNodeManager;
end;  // SetupObjects

//==============================================================================
procedure TfrmLocations.FreeObjects;
begin
  AppSettings.ClearFilteredRecords([TN_LOCATION, TN_LOCATION_FEATURE]);
  ResetAddinDetailScreen;
  FNodeMan := nil;

  ClearTree;
  FreeAndNil(FdmLocation);
  FreeAndNil(FFilteredKeys);
  FreeAndNil(FFilteredKeys);
  FreeAndNil(FNodeImagesMap);
end;  // FreeObjects

//==============================================================================
procedure TfrmLocations.KillChildren(ANode: TFlyNode);
var
  lNode, lNextNode: TFlyNode;
  lAlreadyClearingTree : Boolean;
begin
  lNode := ANode.GetLastChild;
  lAlreadyClearingTree := FClearingTree;
  FClearingTree:=True;
  try
    while Assigned(lNode) do begin
      // Remove all sub nodes before removing this node
      if lNode.HasChildren then
        KillChildren(lNode);

      TObject(lNode.Data).Free;  // Will work even if Data is nil
      lNextNode := lNode.GetPrevSibling;
      lNode.Delete;
      lNode := lNextNode;
    end;
  finally
    FClearingTree := lAlreadyClearingTree;    // reset to previous state
  end;
  TNodeObject(ANode.Data).ChildrenPopulated := False;
end;  // KillChildren

//==============================================================================
procedure TfrmLocations.ClearTree;
var lCursor : TCursor;
    i : Integer;
begin
  lCursor:=HourglassCursor;
  // Lock items update to speed up the process
  LockWindowUpdate(Handle);
  if csDestroying in ComponentState then
    tvLocations.Items.BeginUpdate;
  try
    FClearingTree:=True;
    with tvLocations do begin // walk tree items freeing data
      for i := 0 to Items.Count-1 do
        TObject(Items[i].Data).Free;
      Items.Clear;
    end;
    FClearingTree:=False;
    PerformNodeChange(nil);
  finally
    if csDestroying in ComponentState then
      tvLocations.Items.EndUpdate;
    // Release the lock
    LockWindowUpdate(0);
    DefaultCursor(lCursor);
  end;
end;  // ClearTree

//==============================================================================
procedure TfrmLocations.FormResize(Sender: TObject);
begin
  inherited;
  if LocationSplitter.Left < LocationSplitter.MinSize then begin
    pnlDetails.Width := ClientWidth - LocationSplitter.MinSize - LocationSplitter.Width;
    if pnlDetails.Left + pnlDetails.Width > ClientWidth then
      Redraw;
  end;
  with tvLocations do begin
    Width :=shpLocations.Width-2;
    Height:=shpLocations.Height-2;
    UpdateScrollRange;  // Make sure the scroolbars are properly displayed (or not)
    Refresh;
  end;
end;  // FormResize

//==============================================================================
procedure TfrmLocations.LocationSplitterPaint(Sender: TObject);
begin
  inherited;
  DrawVertSplitter(Canvas, LocationSplitter);
end;  // LocationSplitterPaint

//==============================================================================
procedure TfrmLocations.SetDetailForm(const Value: TfrmBaseDockedForm);
begin
  FDetailForm := Value;
end;  // SetDetailForm

//==============================================================================
function TfrmLocations.CheckDeletedNode(const AMode:TEditMode):Boolean;
var lSelNode: TFlyNode;
    lMessage, lTableName: String;
    lKey: TKeyString;
begin
  Result := True;
  if not FAddingNewNode and (SelectedItem <> nil) then begin
    lKey := TNodeObject(SelectedItem.Data).ItemKey;
    lTableName := TNodeObject(SelectedItem.Data).ItemAdditional;
    if lTableName <> '' then
      Result := dmGeneralData.CheckKeyExists(lTableName, lTableName + '_Key', lKey);

    if not Result then begin
      case AMode of
        emView   : lMessage:=ResStr_CannotAccessRecord;
        emEdit   : lMessage:=ResStr_CannotEditRecord;
        emDelete : lMessage:=ResStr_CannotDeleteRecord;
      end;
      MessageDlg(lMessage + ResStr_DeletedFromDatabase,
                 mtInformation,[mbOk],0);
      with tvLocations do begin
        lSelNode:=SelectedItem.Parent;
        Items.Delete(SelectedItem);
        if lSelNode=nil then Selected:= Items.GetFirstNode
                        else Selected:= lSelNode;
      end;
    end;
  end;
end;  // CheckDeletedNode

{-------------------------------------------------------------------------------
  Accessor override to obtain custodian from detail form
}
function TfrmLocations.GetCustodian: String;
begin
  if Assigned(FDetailForm) then
    Result := FDetailForm.Custodian
  else
    Result := '';
end;

//==============================================================================
{ Pass ANode if in the middle of the NodeChange event and the selected node
   is not necessarily the one we are changing to }
procedure TfrmLocations.SetMenuState(const ANotEditing: Boolean; ANode: TFlynode = nil);
var tfOn, tfSubSite : Boolean;
    lIdx            : Integer;
    lSelectedNode   : TFlynode;
    lData           : TKeyDataSysSupplied;
begin
  if ANotEditing then
    FEditMode := emView
  else
    FEditMode := emEdit;
  { Find the node we will be displaying }
  if Assigned(ANode) then
    lSelectedNode := ANode
  else
    lSelectedNode := tvLocations.Selected;
  with tvLocations do begin
    tfOn:=ANotEditing and (Items.Count>0);
    if lSelectedNode<>nil then
      tfSubSite:=(lSelectedNode.Level>0) and (lSelectedNode.ImageIndex<FEATURE_IMAGE_INDEX)
    else
      tfSubSite := False;
  end;

  lData := GetStateDataFromNode(lSelectedNode);
  bbAdd.Enabled        :=ANotEditing and AddButtonState;
  bbEdit.Enabled       :=tfOn and
                         (EditButtonState(lData, Custodian) or
                         (AppSettings.UserAccessLevel>=ualAddOnly));
  bbDelete.Enabled     :=tfOn and DeleteButtonState(lData);
  // Apply FullEditAccess security
  if bbDelete.Enabled and Assigned(lSelectedNode) then begin
    if lSelectedNode.ImageIndex<FEATURE_IMAGE_INDEX then
      bbDelete.Enabled := bbDelete.Enabled and
                      dmGeneralData.HasFullEditAccess(TN_LOCATION,
                      'LOCATION_KEY', lData.ItemKey)
    else if lSelectedNode.ImageIndex=FEATURE_IMAGE_INDEX then
      bbDelete.Enabled := bbDelete.Enabled and
                      dmGeneralData.HasFullEditAccess(TN_LOCATION_FEATURE,
                      'LOCATION_FEATURE_KEY', lData.ItemKey)
  end;
  // If addin active and node is addin-managed, make sure buttons state OK.
  if Assigned(FNodeMan) and (lData is TAddinLocNode) then begin
    bbEdit.Enabled := bbEdit.Enabled and
                      TAddinLocNode(lData).Editable and
                      not TAddinLocNode(lData).IsSubFolder;
    bbDelete.Enabled := bbDelete.Enabled and
                        TAddinLocNode(lData).Deletable and
                        not TAddinLocNode(lData).IsSubFolder;
  end;

  bbRelatedData.Enabled:=tfOn;
  btnShowAll.Enabled   :=ANotEditing;

  pmHierarchy.AutoPopup:=bbAdd.Enabled;

  mnuEditAdd.Enabled   :=bbAdd.Enabled;
  mnuEditEdit.Enabled  :=bbEdit.Enabled;
  mnuEditDelete.Enabled:=bbDelete.Enabled;

  actFilter.Enabled       := tfOn and (lSelectedNode <> nil) and
                             (lSelectedNode.ImageIndex < FEATURE_IMAGE_INDEX);
  actFindOnMap.Enabled    := actFilter.Enabled and (AppSettings.AvailableMaps.Count > 0);
  actFind.Enabled         := tfOn;
  actShowMetadata.Enabled := tfOn and (lSelectedNode <> nil);

  mnuEditSortBy.Enabled:=tfOn;
  EnableSortToolbutton(tfOn, pmSortBy);
  with frmMain.tbContext do
    for lIdx:=0 to ButtonCount-1 do
      if Buttons[lIdx].DropDownMenu=pmSortBy then begin
        Buttons[lIdx].Enabled:=mnuEditSortBy.Enabled;
        Break;
      end;

  pmHSortBy.Enabled      := tfOn;
  actMoveTo.Enabled      := (AppSettings.UserAccessLevel > ualAddOnly) and
                             bbAdd.Enabled and tfOn;
  actPromoteNode.Enabled := actMoveTo.Enabled and tfSubSite;
end;  // SetMenuState

//==============================================================================
procedure TfrmLocations.UpdateSubMenus(const iNode : TFlyNode);
var tfSite, tfSubSite,
    tfFeature, tfEmptyTree,
    tfAddin, tfAddinExtra  : Boolean;
begin
  if iNode<>nil then
    with iNode do begin
      tfSite    := ImageIndex < FEATURE_IMAGE_INDEX;
      tfFeature := ImageIndex = FEATURE_IMAGE_INDEX;
      tfSubSite := (ImageIndex < FEATURE_IMAGE_INDEX) and (Level > 0);
      tfAddin   := Assigned(FNodeMan);
      tfAddinExtra := tfAddin and not(ImageIndex in [SITE_IMAGE_INDEX_0,SITE_IMAGE_INDEX_1,SITE_IMAGE_INDEX_2, SITE_IMAGE_INDEX_3,SITE_IMAGE_INDEX_4,FEATURE_IMAGE_INDEX]);
    end
  else begin
    tfSite    := True;  // so we can add a first site
    tfFeature := False; // no feature until a site has been added
    tfSubSite := False; // so know if Promote menu to be enabled
    // Addin coded for features for now, so now feature, no addin handling. 
    tfAddin   := False;
    tfAddinExtra := False;
  end;

  tfEmptyTree:=tvLocations.Items.Count=0;
  // Set so can only export site. On screen shutdown, sets actExport.Visible to True,
  // as called after BaseExportable.Destroy.
  // So also need to test if anything in tree as TreeView emptied prior to this
  // call on screen shutdown
  dmFormActions.SetActionVisibility(dmFormActions.actExport, tfSite and not tfEmptyTree);
  // Add sub-menu
  actAddSite.Visible    := tfSite or tfEmptyTree;
  actAddSubSite.Visible := tfSite and not tfEmptyTree;
  actAddFeature.Visible := (tfSite or tfFeature) and not tfEmptyTree;
  // Sort By sub-menu
  actSortSiteName.Visible   := tfSite and not tfEmptyTree;
  pmSortLocation.Default    := actSortSiteName.Visible;
  actSortFeatName.Visible   := tfFeature and not tfEmptyTree;
  actSortFeatType.Visible   := tfFeature and not tfEmptyTree;
  pmSortFeatureName.Default := actSortFeatName.Visible;

  // Remove any Addin menu entries
  CleanupAddinMenuItems(mnuEditAdd);            // Remove from form's Add menu
  CleanupAddinMenuItems(pmHierarchy.Items[0]);  // Remove from Tree's Add menu
  CleanupAddinMenuItems(pmAdd.Items);           // Remove from Add button's menu
  if Assigned(FNodeMan) and (tfFeature or tfAddin or tfAddinExtra) and not tfSite then
    UpdateSubMenusForAddin(iNode, [mnuEditAdd, pmHierarchy.Items[0], pmAdd.Items]);

  dmFormActions.actTransferData.Enabled := Assigned(RequestorForm) and not tfEmptyTree;
  dmFormActions.actPlacesForOccurrencesReport.Enabled := not tfEmptyTree;
  dmFormActions.actOccurrencesForPlacesReport.Enabled := not tfEmptyTree;

  //Need to enable promote menu item only if sub-site selected
  actMoveTo.Enabled      := (AppSettings.UserAccessLevel > ualAddOnly) and (not tfEmptyTree);
  actPromoteNode.Enabled := actMoveTo.Enabled and tfSubSite;

  actSortSiteName.Enabled := tfSite and not tfEmptyTree;
  actSortSpatialRef.Enabled := tfSite and not tfEmptyTree;
  actSortFeatName.Enabled := tfFeature and not tfEmptyTree;
  actSortFeatType.Enabled := tfFeature and not tfEmptyTree;

  if not Assigned(SelectedItem) or not Assigned(DetailForm) then
    dmFormActions.UpdateRTFMenu(False)
  else
  if Assigned(DetailForm) then
    DetailForm.UpdateRTFMenu;
    
  dmFormActions.actCut.Enabled := (ActiveControl = tvLocations) and
                                  (AppSettings.UserAccessLevel >= ualAddOnly);
  // Fix for dynamically populated PopupMenus
  RefreshXPMenu;
end;  // UpdateSubMenus

//==============================================================================
procedure TfrmLocations.PerformNodeChange(ANode:TFlyNode);
begin
  FSelectedItem:=ANode;
  if ANode<>nil then begin
    ResetAddinDetailScreen;
    if CheckDeletedNode(emView) then
      case ANode.ImageIndex of
        SITE_IMAGE_INDEX_0..SITE_IMAGE_INDEX_4 : ShowLocationDetails(ANode.Text);
        FEATURE_IMAGE_INDEX: ShowFeatureDetails(ANode.Text);
      else
        // Further down the tree
        if Assigned(FNodeMan) then
          PerformAddinNodeChange(ANode);
      end;
  end else
  if tvLocations.Items.Count = 0 then begin
    Caption := ResStr_Locations;
    if DetailForm <> nil then begin
      DetailForm.Close;
      FreeAndNil(FDetailForm);
    end;
    ResetAddinDetailScreen;
    SetMenuState(True); //Reset menus and buttons
  end;
  UpdateSubMenus(ANode);
  SetFormCaption;
end;  // PerformNodeChange

//==============================================================================
procedure TfrmLocations.tvLocationsChange(Sender: TObject; Node: TFlyNode);
var ltfEditMode: Boolean;
begin
  inherited;
  if (not FClearingTree) and
     ((Node <> tvLocations.Selected) or
      (not Assigned(DetailForm) and not Assigned(FDetailScreen))) then
  begin
    if Assigned(DetailForm) then
    begin
      if (DetailForm is TfrmBaseDockedForm) then
      begin
        ltfEditMode := TfrmBaseDockedForm(DetailForm).EditMode<>emView;
        if not ltfEditMode then
          PerformNodeChange(Node)
        else
          UpdateSubMenus(Node);
        SetMenuState(not ltfEditMode, Node);
      end;
    end else
    if Assigned(FDetailScreen) then begin
      ltfEditMode := (FDetailScreen.ControlInterface as IRecorderDetailScreen).Editing;
      if not ltfEditMode then
        PerformNodeChange(Node)
      else
        UpdateSubMenus(Node);
      SetMenuState(not ltfEditMode);
    end else begin
      PerformNodeChange(Node);
      SetMenuState(True, Node);
    end;
  end;
end;  // tvLocationsChange

//==============================================================================
procedure TfrmLocations.CheckDetailFormClass(const AFormClass:TFormClass);
begin
  if DetailForm <> nil then
    if not (DetailForm is AFormClass) then begin
      DetailForm.Close;
      FreeAndNil(FDetailForm);
    end;
end;  // CheckDetailFormClass

procedure TfrmLocations.ResetDetailForm(const ACaption: String; const AFormClass: TFormClass);
var
  lComponent: TComponent;
begin
  CheckDetailFormClass(AFormClass);
  if DetailForm = nil then begin
    // Need to cast for DetailForm
    DetailForm := TfrmBaseDockedForm(AFormClass.Create(nil));
    lComponent := DetailForm.FindComponent('scbDetails');
    if not Assigned(lComponent) then
      lComponent := DetailForm.FindComponent('pnlDetails');
    if Assigned(lComponent) then
      TWinControl(lComponent).ManualDock(pnlDetails, nil, alClient);
    Caption := ACaption;
    DetailForm.UpdateMapWindowSelector;
  end;
end;

//==============================================================================
procedure TfrmLocations.ShowLocationDetails(const ALocationName: String);
begin
  bbRelatedData.Enabled := True;
  //CheckDetailFormClass(TfrmLocationDetails);
  ResetDetailForm(ResStr_Location + ': ' + ALocationName, TfrmLocationDetails);
  with TfrmLocationDetails(DetailForm) do begin
    DrillForm:=Self;
    if SelectedItem.Parent=nil then
      ParentKey:=''
    else
      ParentKey:=TSiteNode(SelectedItem.Parent.Data).ItemKey;
    DisplayRecord(TSiteNode(SelectedItem.Data).ItemKey);
  end;
  NotifyDataItemChange;
end;  // ShowLocationDetails

//==============================================================================
procedure TfrmLocations.ShowFeatureDetails(const AFeatureName: String);
begin
  bbRelatedData.Enabled := False;
  ResetDetailForm(ResStr_LBFeature + ': ' + AFeatureName, TfrmFeatureDetails);
  with TfrmFeatureDetails(DetailForm) do begin
    DrillForm:=Self;
    ParentKey:=TSiteNode(SelectedItem.Parent.Data).ItemKey;
    DisplayRecord(TFeatureNode(SelectedItem.Data).ItemKey);
  end;
  NotifyDataItemChange;  
end;  // ShowFeatureDetails

//==============================================================================
procedure TfrmLocations.bbAddClick(Sender: TObject);
var lPos: TPoint;
begin
  inherited;
  lPos := bbAdd.ClientToScreen(Point(0, bbAdd.Height));
  pmAdd.Popup(lPos.X, lPos.Y);
end;  // bbAddClick

//==============================================================================
procedure TfrmLocations.bbEditClick(Sender: TObject);
begin
  inherited;
  if CheckDeletedNode(emEdit) then
    with SelectedItem do
      case ImageIndex of
        SITE_IMAGE_INDEX_0..SITE_IMAGE_INDEX_4 : TfrmLocationDetails(DetailForm).EditRecord;
        FEATURE_IMAGE_INDEX : TfrmFeatureDetails(DetailForm).EditRecord;
      else
        if Assigned(FDetailScreen) then
          (FDetailScreen.ControlInterface as IRecorderDetailScreen).EditRecord;
      end;
end;  // bbEditClick

//==============================================================================
procedure TfrmLocations.bbDeleteClick(Sender: TObject);
var stMsg   :String;
    lKey    :TKeyString;
    tfMany  :Boolean;
    lNode   :TFlyNode;
    DeleteOK:Boolean;
    lCursor :TCursor;
begin
  inherited;
  with SelectedItem do begin
    // Make sure the node to delete doesn't have any child node attached
    if not TNodeObject(Data).ChildrenPopulated then Expand(False);
    // If node has children, can't remove, and it can only be a site node
    if (ImageIndex < FEATURE_IMAGE_INDEX) and HasChildren then begin
      tfMany := (Count > 1);
      if tfMany then stMsg := IntToStr(Count) + ResStr_SitesOrFeatures
                else stMsg := ResStr_AnotherSite;
      MessageDlg(ResStr_CannotDeleteSite + stMsg, mtWarning, [mbOk], 0);
    end else
    if CheckDeletedNode(emDelete) then begin
      lKey := TNodeObject(Data).ItemKey;
      // See if anyone is editing the record, and if not, go on and delete it
      case ImageIndex of
        SITE_IMAGE_INDEX_0..SITE_IMAGE_INDEX_4 : stMsg := Format(ResStr_Site, [Text]);
        FEATURE_IMAGE_INDEX: stMsg := Format(ResStr_Feature, [Text]);
      else
        if Assigned(FNodeMan) then stMsg := Format(ResStr_Item, [Text]);
      end;

      if MessageDlg(Format(ResStr_DeleteQuestion, [stMsg]),
                    mtConfirmation,[mbYes,mbNo],0)=mrYes then begin
        lCursor:=HourglassCursor;
        try
          dmDatabase.ExecuteSQL('SET XACT_ABORT ON');
          dmDatabase.Connection.BeginTrans;
          try
            DeleteOK := False;
            case ImageIndex of
              SITE_IMAGE_INDEX_0..SITE_IMAGE_INDEX_4 : DeleteOK := TfrmLocationDetails(DetailForm).DeleteRecord(lKey);
              FEATURE_IMAGE_INDEX: DeleteOK := TfrmFeatureDetails(DetailForm).DeleteRecord(lKey);
            else
              if Assigned(FDetailScreen) then begin
                DeleteOk := True;
                (FDetailScreen.ControlInterface as IRecorderDetailScreen).DeleteRecord;
              end;
            end;
            if DeleteOK then begin
              dmDatabase.Connection.CommitTrans;
              { ILocationNodeManager deals with features, for now, and deleting a feature
                means also getting rid of potential extra nodes below. Cascade delete in DB
                takes care of the data, but here, it needs to be explicit. }
              KillChildren(SelectedItem);

              TNodeObject(Data).Free;  // Remove Data object of new item
              lNode := GetPrevVisible;   // Select a new node
              // if this is the first node in the heirarchy, will not have a previous
              // node, so grab the next node instead.
              if not Assigned(lNode) then
                lNode := GetNextVisible;
              tvLocations.Selected := lNode;
              Delete;                  // Remove deleted item from the tree
              PerformNodeChange(lNode);
              SetMenuState(True);
              if Assigned(SelectedItem) then
                if TNodeObject(SelectedItem.Data) is TAddinLocNode then
                  OnRefreshScreenInfo(TNodeObject(SelectedItem.Data).ItemKey, '');
            end else
              dmDatabase.Connection.RollbackTrans;
          except
            on E:Exception do begin
              dmDatabase.Connection.RollbackTrans;
              if dmDatabase.CheckError(E, dbeReferentialIntegrity) then
                Raise TExceptionPath.CreateNonCritical(ResStr_CannotDeleteItem, E)
              else
                Raise;
            end;
          end;
        finally
          DefaultCursor(lCursor);
        end;  // try
      end;  // if MessageDlg...
    end;  // if HasChildren...
  end;
end;  // bbDeleteClick

//==============================================================================
procedure TfrmLocations.bbRelatedDataClick(Sender: TObject);
var lPos: TPoint;
begin
  inherited;
  lPos := bbRelatedData.ClientToScreen(Point(0, bbRelatedData.Height));
  SetRelatedDataMenuItems;
  pmRelatedData.Popup(lPos.X, lPos.Y);
end;  // bbRelatedDataClick

//==============================================================================
procedure TfrmLocations.PopulateTopLevel;
begin
  tvLocations.Items.BeginUpdate;
  try
    PopulateTree(FdmLocation.GetRecordset(ntTopSite), nil, ntTopSite);
    // Reapply sort if using file code, since the server-side db sort is alphabetical,
    // but client side sort is an improvement combination of alphabetical and numeric sorting.
    if FSortOrder = LOCATION_FILE_CODE_SORT then begin
      // force a change in sort order to be detected
      tvLocations.SortType := stNone;
      PerformSort;
    end;
  finally
    tvLocations.Items.EndUpdate;
  end;
end;  // PopulateTopLevel

//==============================================================================
procedure TfrmLocations.PopulateChildNodes(const iParent: TFlyNode);
begin
  inherited;
  tvLocations.Items.BeginUpdate;
  try
    CheckValidLocationField;
    CheckValidFeatureField;
    PopulateTree(FdmLocation.GetRecordset(ntChildSite, TKeyData(iParent.Data).ItemKey),
                 iParent,ntChildSite);
    PopulateTree(FdmLocation.GetRecordset(ntFeature, TKeyData(iParent.Data).ItemKey),
                 iParent,ntFeature);
    // Reapply sort if using file code, since the server-side db sort is alphabetical,
    // but client side sort is an improvement combination of alphabetical and numeric sorting.
    if FSortOrder = LOCATION_FILE_CODE_SORT then begin
      // force a change in sort order to be detected
      tvLocations.SortType := stNone;
      PerformSort;
    end;
  finally
    tvLocations.Items.EndUpdate;
  end;
end;  // PopulateChildNodes


{-------------------------------------------------------------------------------
  Description : Populates a generic tree level
  Created : 21/03/2003 }
procedure TfrmLocations.PopulateTree(const ARecordset: ADOInt._Recordset;
  const iParent: TFlyNode; const iSiteType: TLocationNodeType);
var lNodeText : String;
  lSiteNode : TSiteNode;
  lFeatureNode : TFeatureNode;
  lResult : TFlyNode;
  lCursor : TCursor;
  lPreferredCustodians : string;
  lImageIndex : integer;
begin
  lCursor:=HourglassCursor;
  LockWindowUpdate(Handle);
  lResult := nil;
  try
    with ARecordset do begin
      if not Eof then begin
        MoveFirst;
        while not Eof do begin
          case iSiteType of
            ntTopSite,
            ntChildSite   :
                begin
                  lNodeText := Fields['Item_Name'].Value;
                  lSiteNode := TSiteNode.Create;
                  with lSiteNode do begin
                    ItemKey := Fields['Location_Key'].Value;
                    ItemAdditional := TN_LOCATION;
                    SysSupplied:= Fields['System_Supplied_Data'].Value;
                    FileCode   := VarToStr(Fields['File_Code'].Value);
                    SpatialRef := Fields['Spatial_Ref'].Value;
                    IsFiltered := AppSettings.IndexOfFilteredRecord(TN_LOCATION, ItemKey) > -1;
                    Hint       := AppSettings.GetFilteredRecordHint(TN_LOCATION, ItemKey);
                    ChildrenPopulated := False;
                  end;
                  if iParent = nil then
                    lResult := tvLocations.Items.AddObject(nil, lNodeText, lSiteNode)
                  else
                    lResult := tvLocations.Items.AddChildObject(iParent, lNodeText, lSiteNode);
                  tvLocations.Items.AddChild(lResult, '-');
                end;

            ntFeature :
                begin
                  lNodeText := Fields[FEATURE_NAME].Value + ' - ' + Fields[FEATURE_TYPE].Value;
                  lFeatureNode := TFeatureNode.Create;
                  with lFeatureNode do begin
                    ItemKey := Fields['Location_Feature_Key'].Value;
                    ItemAdditional := TN_LOCATION_FEATURE;
                    IsFiltered := AppSettings.IndexOfFilteredRecord(TN_LOCATION_FEATURE, ItemKey) > -1;
                    Hint       := AppSettings.GetFilteredRecordHint(TN_LOCATION_FEATURE, ItemKey);
                  end;
                  lResult := tvLocations.Items.AddChildObject(iParent, lNodeText, lFeatureNode);
                  if Assigned(FNodeMan) then
                    SetSubNodesProperties(lResult, -1);
                  lFeatureNode.ChildrenPopulated := True;
                 end;

          end;

          if lResult <> nil  then begin
            // Here we change the images
            lPreferredCustodians := GetPreferredCustodians(SETTING_NAME);
            lImageindex := TNodeObject(lResult.Data).ImageIndex;
            If TNodeObject(lResult.Data).ImageIndex < FEATURE_IMAGE_INDEX then begin
              if (lPreferredCustodians <> '') then begin
                if  (pos(Fields['Custodian'].Value,lPreferredCustodians) > 0)
                  or (Fields['System_Supplied_Data'].Value = True) then lIMageIndex := SITE_IMAGE_INDEX_1
                else
                  lImageindex := SITE_IMAGE_INDEX_3;
              end;
              lImageindex := lImageIndex + GetLocationStatus(Fields['Location_Key'].Value);
            end;
            lResult.ImageIndex := lImageindex;
            lResult.SelectedIndex := lResult.ImageIndex;
           end;  // end lResult if
          MoveNext;
        end;
      end; // if
    end; // with
  finally
    ARecordset.Close;
    LockWindowUpdate(0);
    DefaultCursor(lCursor);
  end;
end;

//==============================================================================
procedure TfrmLocations.tvLocationsExpanding(Sender: TObject; Node: TFlyNode;
  var AllowExpansion: Boolean);
var
  lCursor: TCursor;
  lNodeData: TNodeObject;
begin
  inherited;
  lCursor := HourglassCursor;
  try
    lNodeData := TNodeObject(Node.Data);
    if not lNodeData.ChildrenPopulated then begin
      Node.DeleteChildren;
      if not (lNodeData is TAddinLocNode) then begin
        PopulateChildNodes(Node);
        lNodeData.ChildrenPopulated:=True;
        // And also add addin-managed nodes
        PopulateAddinNodes(Node, -1);
      end else
        PopulateAddinNodes(Node, TAddinLocNode(lNodeData).TypeID);
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // tvLocationsExpanding

(**
 * Updates the selected feature to set the node text and underlying key.
 *)
procedure TfrmLocations.SetFeature(const AString: String;
  const AKey:TKeyString);
begin
  if SelectedItem<>nil then begin
    if TNodeObject(SelectedItem.Data) is TFeatureNode then begin
      with SelectedItem do begin
        Text:=AString;
        TNodeObject(Data).ItemKey:=AKey;
        SetFormCaption
      end;
    end;
    tvLocations.Selected:=SelectedItem;
  end;
end;  // SetFeature

(**
 * Updates the selected site to set the node text and underlying keys and
   other values.
 *)
procedure TfrmLocations.SetSite(const AName, ACode, ASpatialRef: String; const AKey:TKeyString);
begin
  if SelectedItem<>nil then begin
    if TNodeObject(SelectedItem.Data) is TSiteNode then begin
      with SelectedItem do begin
        Text:=AName;
        TSiteNode(Data).ItemKey:=AKey;
        TSiteNode(Data).SpatialRef:=ASpatialRef;
        TSiteNode(Data).FileCode:=ACode;
        SetFormCaption;
      end;
    end;
    tvLocations.Selected:=SelectedItem;
  end;
end;  // SetSite

{-------------------------------------------------------------------------------
  Sets the caption on the top of the form to display the current Location.
}
procedure TfrmLocations.SetFormCaption;
begin
  if SelectedItem <> nil then
    with SelectedItem do begin
      if ImageIndex<FEATURE_IMAGE_INDEX then Self.Caption := ResStr_Location + ': ' + Text
      else
      if ImageIndex = FEATURE_IMAGE_INDEX then Self.Caption := ResStr_LBFeature + ': ' + Text
      else
        Self.Caption := ResStr_LBItem + ': ' + Text;
    end
  else
    Caption := ResStr_Locations;
end;

//==============================================================================
// Discarding a new node needs to be handled via message as detail form can be
// destroyed before everything is properly cleaned up.
procedure TfrmLocations.WMDiscardLocation(var Msg: TMessage);
begin
  if SelectedItem <> nil then begin
    { With ILocationNodeManager, feature nodes can have sub-nodes already there, so they
      need to be explicitly taken care of. }
    KillChildren(SelectedItem);

    TNodeObject(FSelectedItem.Data).Free;  // Remove new item from the tree
    FSelectedItem.Delete;
    PerformNodeChange(tvLocations.Selected);  // Reselect node and record to display
  end;
  tvLocations.SetFocus;
end;  // WMDiscardLocation

//==============================================================================
procedure TfrmLocations.WMRefreshTermLists(var Msg: TMessage);
begin
  if DetailForm<>nil then
    if DetailForm is TfrmLocationDetails then TfrmLocationDetails(DetailForm).RefreshLists else
    if DetailForm is TfrmFeatureDetails  then TfrmFeatureDetails(DetailForm).RefreshLists;
end;  // WMRefreshTermLists

//==============================================================================
procedure TfrmLocations.actAddSiteExecute(Sender: TObject);
begin
  inherited;
  AddSiteNode(False);
end;  // actAddSiteExecute

//==============================================================================
procedure TfrmLocations.actAddSubSiteExcute(Sender: TObject);
begin
  inherited;
  AddSiteNode(True);
end;  // actAddSubSiteExecute

//==============================================================================
procedure TfrmLocations.AddSiteNode(const iSubSite:Boolean);
var lSiteNode : TSiteNode;
begin
  FAddingNewNode:=True;
  //Add new node with key
  lSiteNode:=TSiteNode.Create;
  lSiteNode.ItemKey := dmGeneralData.GetNextKey(TN_LOCATION, 'Location_Key');
  lSiteNode.ItemAdditional := TN_LOCATION;
  lSiteNode.ChildrenPopulated:= True;
  if iSubSite then begin
    // Expand Site to next level, properly populating if needed.
    SelectedItem.Expand(False);
    FSelectedItem:=tvLocations.Items.AddChildObject(SelectedItem,'< ' + ResStr_NewLocation + ' >',lSiteNode);
    SelectedItem.Parent.Expand(False);  // Make sure it's expanded, if adding first child node.
  end else
    FSelectedItem:=tvLocations.Items.AddObject(SelectedItem,'< ' + ResStr_NewLocation + ' >',lSiteNode);
  // This is where we change the Image Index to 1 if needed                                                 if
  if GetPreferredCustodians(SETTING_NAME)  <> '' then
    SelectedItem.ImageIndex   :=SITE_IMAGE_INDEX_1;
  SelectedItem.SelectedIndex:=SelectedItem.ImageIndex;
  tvLocations.Selected      :=FSelectedItem;   // Change to new item. Display the detail form too
  //Append record
  TfrmLocationDetails(DetailForm).AddRecord(lSiteNode.ItemKey);
  FAddingNewNode:=False;
end;  // AddSiteNode

//==============================================================================
procedure TfrmLocations.actAddFeatureExecute(Sender: TObject);
var lFeatureNode : TFeatureNode;
begin
  inherited;
  FAddingNewNode:=True;
  //Add new node with key
  lFeatureNode:= TFeatureNode.Create;
  lFeatureNode.ItemKey := dmGeneralData.GetNextKey('Location_Feature','Location_Feature_Key');
  lFeatureNode.ItemAdditional := TN_LOCATION_FEATURE;
  if SelectedItem.ImageIndex < FEATURE_IMAGE_INDEX then begin //Add feature as child to location
    // Expand Site to next level, properly populating if needed.
    SelectedItem.Expand(False);
    FSelectedItem := tvLocations.Items.AddChildObject(SelectedItem, '< ' + ResStr_NewFeature + ' >', lFeatureNode);
    SelectedItem.Parent.Expand(False);  // Make sure it's expanded, if adding first child node.
  end else //Add feature as sibling to another feature
    FSelectedItem := tvLocations.Items.AddObject(SelectedItem, '< ' + ResStr_NewFeature + ' >', lFeatureNode);

  if Assigned(FNodeMan) then
    SetSubNodesProperties(FSelectedItem, -1);
  lFeatureNode.ChildrenPopulated := True;

  SelectedItem.ImageIndex   := FEATURE_IMAGE_INDEX;
  SelectedItem.SelectedIndex:= FEATURE_IMAGE_INDEX;
  tvLocations.Selected      := FSelectedItem;   // Change to new item. Display the detail form too
  //Append record
  TfrmFeatureDetails(DetailForm).AddRecord(lFeatureNode.ItemKey);
  FAddingNewNode := False;
end;  // actAddFeatureExecute

//==============================================================================
procedure TfrmLocations.SetupDestinationControls;
begin
  RegisterDragComponent(tvLocations, DragLocation);
  if AppSettings.UserAccessLevel >= ualAddOnly then
    RegisterDropComponentAdvanced(tvLocations, DropLocation,
                                  [TN_LOCATION, TN_LOCATION_FEATURE],
                                  [CF_JNCCDATA], DragOverCheck);
end;  // SetupDestinationControls

//==============================================================================
procedure TfrmLocations.DragLocation(const Sender: TObject; var oDropSource: TJNCCDropSource);
var
  selectedNodes: TObjectList;
  nodeData: TNodeObject;
  i: Integer;
  hasFeature, hasLocation: Boolean;
begin
  // We don't want SelectedItem, but tvLocations.Selected, as they can be different
  // when editing the location or feature details.
  if (AppSettings.UserAccessLevel >= ualRecorder) then begin
    selectedNodes := GetSelectedItems;
    try
      hasFeature := False;
      hasLocation := False;
      for i := 0 to selectedNodes.Count - 1 do begin
        nodeData := TNodeObject(TFlyNode(selectedNodes[i]).Data);
        if nodeData is TFeatureNode then begin
          hasFeature := True;
          oDropSource.DropData.AddItem(nodeData.ItemKey, 'FromTree-' + TN_LOCATION_FEATURE);
        end else begin
          hasLocation := True;
          oDropSource.DropData.AddItem(nodeData.ItemKey, 'FromTree-' + TN_LOCATION);
        end;

        oDropSource.DropData.SetTable(TN_LOCATION);
        if hasFeature and hasLocation then
          oDropSource.DropData.SetTable(MIXED_DATA)
        else
        if hasFeature then
          oDropSource.DropData.SetTable(TN_LOCATION_FEATURE);
      end;
    finally
      selectedNodes.Free;
    end;
  end;
end;  // DragLocation

//==============================================================================
procedure TfrmLocations.DropLocation(const Sender: TObject; const iFormat: Integer;
  const iSourceData: TKeyList; const iTextStrings: TStringList;
  const iIsPasteOperation: Boolean; var ioHandled: Boolean);
var
  lMovingNode, lParentNode: TFlyNode;
  lPos: TPoint;
  lMessage: String;
  i: Integer;

  //----------------------------------------------------------------------------
  function CheckMoveAllowed: Boolean;
  var lNode: TFlyNode;
  begin
    lNode  := lParentNode;
    Result := True;
    while lNode <> nil do begin
      if lNode = lMovingNode then begin
        Result := False;
        Break;
      end;
      lNode := lNode.Parent;
    end;
  end;  // CheckMoveAllowed

  //-----------------------------------------------------------------------------
begin
  ioHandled := True;
  //Use absolute index in second key field to move item, empty if from another form
  if (AppSettings.UserAccessLevel >= ualAddOnly) and
     (iSourceData.Header.ItemCount > 0) and
     bbAdd.Enabled then
  begin
    if iIsPasteOperation then lMessage := ResStr_CutAndPaste
                         else lMessage := ResStr_DragAndDrop;
    ValidateValue(
        SameText(Copy(iSourceData.Items[0].KeyField2, 1, 8), 'FromTree'),
        Format(ResStr_HierarchyTask, [lMessage]));

    if AppSettings.UserAccessLevel = ualAddOnly then
      with dmDatabase.ExecuteSQL('SELECT Entered_By FROM Location WHERE LOCATION_KEY = ''' +
                iSourceData.Items[0].KeyField1 + '''', True) do
        try
          // Ignore as add only user but not your data
          if Fields['Entered_By'].Value <> AppSettings.UserID then
            raise TExceptionPath.CreateNonCritical(ResStr_CannotMoveItemNotOwner);
        finally
          Close;
        end; // try

    if iFormat = CF_JNCCDATA then
    begin
      // Get node for message if only one node moved.
      lMovingNode := InternalLocateNode(
          iSourceData.Items[0].KeyField1,
          SameText(StringReplace(iSourceData.Items[0].KeyField2, 'FromTree-', '', [rfReplaceAll]),
                   TN_LOCATION));
      if iIsPasteOperation then begin
        // If moving to Top level, use "promote" instead.
        lParentNode := tvLocations.Selected;
        if not Assigned(lParentNode) then
          raise TExceptionPath.CreateNonCritical(ResStr_TargetNodeSelect);
        if iSourceData.Header.ItemCount = 1 then
          if not IsCut then lMessage := Format(ResStr_PasteLocation, [lMovingNode.Text])
                       else lMessage := Format(ResStr_MoveItem, [lMovingNode.Text])
        else
          if not IsCut then lMessage := ResStr_PasteLocations
                       else lMessage := ResStr_MoveItems;
      end else begin
        lPos := tvLocations.ScreenToClient(Mouse.CursorPos);
        lParentNode := tvLocations.GetNodeAt(lPos.X, lPos.Y);

        if iSourceData.Header.ItemCount = 1 then
          lMessage := Format(ResStr_MoveItem, [lMovingNode.Text])
        else
          lMessage := ResStr_MoveItems;
      end;

      // So node can be dropped. Get confirmation.
      if MessageDlg(lMessage, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
        Exit;

      for i := 0 to iSourceData.Header.ItemCount - 1 do begin
        lMovingNode := InternalLocateNode(
            iSourceData.Items[i].KeyField1,
            SameText(StringReplace(iSourceData.Items[i].KeyField2, 'FromTree-', '', [rfReplaceAll]),
                     TN_LOCATION));
        if (lParentNode = nil) and not (TNodeObject(lMovingNode.Data) is TFeatureNode) then begin
          UpdateNodeParentKey(nil, lMovingNode);
          lMovingNode.MoveTo(nil, naAdd);
        end else
        if CheckMoveAllowed then begin
          lParentNode.Expand(False);
          lMovingNode.MoveTo(lParentNode, naAddChild);
          UpdateNodeParentKey(lParentNode, lMovingNode)
        end else begin
          ShowInformation(ResStr_CannotMoveLocation);
          ioHandled := False;
        end;
      end;
      FSelectedItem := tvLocations.Items.GetFirstSelectedNode;
      UpdateSubMenus(SelectedItem);
      tvLocations.Selected := SelectedItem;
      if iIsPasteOperation then Clipboard.Clear;
    end;
  end;
end;  // DropLocation

//==============================================================================
{ Populates a KeyList to return data to other forms with the
  Table Name :'LOCATION'
  KeyField1 : Location_Name_Key }

function TfrmLocations.GetKeyList: TKeyList;
var
  lKeyList: TEditableKeyList;
  itemKey: TKeyString;
  node: TFlyNode;
begin
  //Return an editable key list with the selected nodes key
  Result := nil;
  lKeyList:= TEditableKeyList.Create;
  try
    node := tvLocations.Items.GetFirstSelectedNode;
    while Assigned(node) do begin
      itemKey := TNodeObject(node.Data).ItemKey;
      if (TNodeObject(node.Data) is TFeatureNode) then begin
        lKeyList.AddItem(itemKey, TN_LOCATION_FEATURE);
      end else begin
        lKeyList.AddItem(itemKey, TN_LOCATION);
      end;
      node := tvLocations.Items.GetNextSelectedNode(node);
    end;

    lKeyList.SetTable(MIXED_DATA);

    if lKeyList.Header.ItemCount > 0 then
      Result := lKeyList;
  except
    lKeyList.Free;
    raise;
  end;
end;  // GetKeyList

{-------------------------------------------------------------------------------
  Builds a keylist for revalidation purposes. Includes all child items for selected item.
}
function TfrmLocations.GetKeyListForValidation: TKeyList;
var
  keyList: TEditableKeyList;
  node: TFlyNode;
  nodeData: TNodeObject;
begin
  keyList := nil;
  node := tvLocations.Items.GetFirstSelectedNode;
  while Assigned(node) do begin
    nodeData := TNodeObject(node.Data);
    if nodeData.ItemKey <> '' then begin
      keyList := TEditableKeyList.Create;
      try
        keyList.ConvertToMixedData;
        keyList.AddItem(nodeData.ItemKey, nodeData.ItemAdditional);
        // Next bit for locations only, features don't have child nodes.
        if SameText(nodeData.ItemAdditional, TN_LOCATION) then
          with dmDatabase.GetRecordset(
              'usp_Locations_Select_AllChildrenForLevel', ['@Key', nodeData.ItemKey]) do
          begin
            while not Eof do begin
              keyList.AddItem(Fields['ItemKey'].Value, Fields['TableName'].Value);
              MoveNext;
            end;
            Close;
          end;
      except
        keyList.Free;
        raise;
      end;
    end;
    node := tvLocations.Items.GetNextSelectedNode(node);
  end;
  Result := keyList;
end;  // GetKeyListForValidation

//==============================================================================
procedure TfrmLocations.tvLocationsCompare(Sender: TObject; Node1,
  Node2: TFlyNode; var Compare: Integer);
var lstText1, lstText2: String;
begin
  inherited;
  // If trying to compare a site node with a feature node, just put the feature node
  // after the site node, regardless
  if ((Node1.ImageIndex < FEATURE_IMAGE_INDEX) and (Node2.ImageIndex = FEATURE_IMAGE_INDEX))
     or
     (((FSortOrder = LOCATION_NAME_SORT) or (FSortOrder = LOCATION_FILE_CODE_SORT)
         or (FSortOrder = LOCATION_SPATIAL_REF_SORT)) and
      (Node1.ImageIndex = FEATURE_IMAGE_INDEX) and (Node2.ImageIndex = FEATURE_IMAGE_INDEX))
     or
     (((FSortOrder = FEATURE_NAME_SORT) or (FSortOrder = FEATURE_TYPE_SORT)) and
      (Node1.ImageIndex < FEATURE_IMAGE_INDEX) and (Node2.ImageIndex < FEATURE_IMAGE_INDEX)) then
    Compare := -1
  else
  if (Node1.ImageIndex = FEATURE_IMAGE_INDEX) and (Node2.ImageIndex < FEATURE_IMAGE_INDEX) then
    Compare := 1
  else if Node2.Text='-' then
    Compare := -1
  else if Node1.Text='-' then
    Compare := 1
  else
    // Compare site with site, or feature with feature
    if (FSortOrder = LOCATION_NAME_SORT) and
       (Node1.ImageIndex < FEATURE_IMAGE_INDEX) and (Node2.ImageIndex < FEATURE_IMAGE_INDEX) then
      Compare := CompareText(Node1.Text, Node2.Text)
    else if (FSortOrder = LOCATION_FILE_CODE_SORT) and
       (Node1.ImageIndex < FEATURE_IMAGE_INDEX) and (Node2.ImageIndex < FEATURE_IMAGE_INDEX) then
      Compare := SuperCompareText(TSiteNode(Node1.Data).FileCode, TSiteNode(Node2.Data).FileCode)
    else if (FSortOrder = LOCATION_SPATIAL_REF_SORT) and
       (Node1.ImageIndex < FEATURE_IMAGE_INDEX) and (Node2.ImageIndex < FEATURE_IMAGE_INDEX) then
      Compare := CompareText(TSiteNode(Node1.Data).SpatialRef, TSiteNode(Node2.Data).SpatialRef)
    else
    if (Node1.ImageIndex = FEATURE_IMAGE_INDEX) and (Node2.ImageIndex = FEATURE_IMAGE_INDEX) then begin
      // Work out what to compare if feature sort
      if FSortOrder = FEATURE_NAME_SORT then begin
        lstText1 := Trim(Copy(Node1.Text, 1, Pos(' - ', Node1.Text)));
        lstText2 := Trim(Copy(Node2.Text, 1, Pos(' - ', Node2.Text)));
      end else
      if FSortOrder = FEATURE_TYPE_SORT then begin
        lstText1 := Trim(Copy(Node1.Text, Pos(' - ', Node1.Text) + 5, 255));
        lstText2 := Trim(Copy(Node2.Text, Pos(' - ', Node2.Text) + 5, 255));
      end;
      // Compare strings
      if (lstText1 = '') and (lstText2 = '') then Compare := -1
                                             else Compare := CompareText(lstText1, lstText2);
    end;
end;  // tvLocationsCompare

{-------------------------------------------------------------------------------
 A version of CompareText that handles numeric and alpha portions of a code separately
 so that things like H1, H2, H10 can sort properly (this is not alpha or numeric!)
}
function TfrmLocations.SuperCompareText(str1, str2: string): integer;
var
  parts1, parts2: TStringList;
  i, int1, int2: integer;
begin
  result := 0; // default
  // If a simple comparison solves this, then lets not do all the hard stuff
  if CompareText(str1, str2)=0 then
    exit;
  if TryStrToInt(str1, int1) and TryStrToInt(str2, int2) then begin
    // file codes are both numbers so compare them as such
    if int1<int2 then result := -1
    else if int1>int2 then result := 1;
    // else default is 0 as numbers the same
    exit;
  end;
  parts1 := TStringList.Create;
  parts2 := TStringList.Create;
  try
    Tokenise(str1, parts1);
    Tokenise(str2, parts2);
    // work through each pair of tokens, looking for the first to provide a difference
    for i:=0 to min(parts1.Count-1, parts2.count-1) do begin
      if TryStrToInt(parts1[i], int1) and TryStrToInt(parts2[i], int2) then begin
        // file codes are both numbers so compare them as such
        if int1<int2 then result := -1
        else if int1>int2 then result := 1
        else result := 0;
      end else
        // do an alpha comparison
        result := CompareText(parts1[i], parts2[i]);
      if (result<>0) then break; // no need for more tests as we've found a difference
    end;
    if result=0 then begin
      // strings are identical at least for the number of tokens they share. So we can assume that the
      // longest string comes last
      if (Length(str1)>Length(str2)) then
        result := 1
      else if (Length(str1)<Length(str2)) then
        result := -1;
    end;
  finally
    parts1.Free;
    parts2.Free;
  end;
end;

{-------------------------------------------------------------------------------
 Breaks a string into tokens, with each token being a block of numeric or non-numeric
 characters. Used for location file oode sorting.
}
procedure TfrmLocations.Tokenise(str: string; parts: TStringList);
var
  i: integer;
  doingNumbers: boolean;
  current: string;
begin
  doingNumbers := false;
  for i := 1 to Length(str) do begin
    if i=1 then
      doingNumbers := str[i] in ['0'..'9']
    else begin
      if doingNumbers<>(str[i] in ['0'..'9']) then begin
        // switching between numbers and text so store the token
        parts.add(current);
        doingNumbers := str[i] in ['0'..'9'];
        current := '';
      end;
    end;
    current := current + str[i];
  end;
  parts.add(current);
end;

//==============================================================================
procedure TfrmLocations.PerformSort;
var lCursor: TCursor;
begin
  inherited;
  lCursor := HourglassCursor;
  try
    // Do the osrt
    tvLocations.SortType := stData;
  finally
    // Stop automatic sorting
    tvLocations.SortType := stNone;
    DefaultCursor(lCursor);
  end;  // try .. finally
end;  // PerformSort

//==============================================================================
procedure TfrmLocations.actSortSiteNameExecute(Sender: TObject);
begin
  FSortOrder := LOCATION_NAME_SORT;
  PerformSort;
end;  // actSortSiteNameExecute

//==============================================================================
procedure TfrmLocations.actSortFileCodeExecute(Sender: TObject);
begin
  FSortOrder := LOCATION_FILE_CODE_SORT;
  PerformSort;
end;

//==============================================================================
procedure TfrmLocations.actSortSpatialRefExecute(Sender: TObject);
begin
  FSortOrder := LOCATION_SPATIAL_REF_SORT;
  PerformSort;
end;

//==============================================================================
procedure TfrmLocations.actSortFeatNameExecute(Sender: TObject);
begin
  // Update Feature sort order
  FFeatureField := FEATURE_NAME_SORT;
  FdmLocation.FeatureOrder := FFeatureField;

  FSortOrder    := FEATURE_NAME_SORT;
  PerformSort;
end;  // actSortFeatNameExecute

//------------------------------------------------------------------------------
procedure TfrmLocations.actSortFeatTypeExecute(Sender: TObject);
begin
  // Update Feature sort order
  FFeatureField := FEATURE_TYPE_SORT;
  FdmLocation.FeatureOrder := FFeatureField;

  FSortOrder    := FEATURE_TYPE_SORT;
  PerformSort;
end;  // actSortFeatTypeExecute

//==============================================================================
// ADD SITE FROM MAP OBJECT
{ Called by the Map when a location is to be created }
procedure TfrmLocations.AddSiteFromMapObject(const ABaseMapKey: TKeyString;
  AMapServerLink: TMapServerLink; const ALocationDetails: TQueryDetails);
var
  lLatLong : TLatLong;
begin
  ResetDetailForm('Location: ', TfrmLocationDetails);
  { Call the action to add a new site }
  actAddSite.Execute;
  with TfrmLocationDetails(DetailForm) do
  begin
    { Go to GeoInfo and add boundaries }
    pcLocationDetails.ActivePage := tsGeoInfo;
    pcGeoInfo.ActivePage         := tsBoundaries;

    AddBoundary(AMapServerLink.SheetMapSheetKey(ALocationDetails.SheetID),
                IntToStr(ALocationDetails.ObjectStaticID));

    { Now Move to the General Tab }
    pcLocationDetails.ActivePage := tsGeneral;

    { Load Centroid Coordinates into a TStringCoordinate to allow Conversion }
    lLatLong := SpecificENToLatLong(TMapCoord(ALocationDetails.ObjectCentroid),
                    AppSettings.AvailableMaps.ItemsByKey[ABaseMapKey].SpatialSystem);

    SpatialRef.EnteredRef := ConvertfromLatLong(lLatLong, AppSettings.SpatialRefSystem);
    SpatialRef.DisplayRef := SpatialRef.EnteredRef;
    SpatialRef.Qualifier  := ResStr_InternalMap;
  end // with
end;  // AddSiteFromMapObject

//==============================================================================
// EDIT SITE FROM MAP

{ Double click on a boundary
  and pass details to exisiting Location }

procedure TfrmLocations.EditSiteFromMap(const ABaseMapKey: TKeyString;
  AMapServerLink: TMapServerLink; const ALocationDetails: TQueryDetails);
var
  lLatLong : TLatLong;
begin
  ResetDetailForm('Location: ', TfrmLocationDetails);
  { Call the finder }
  if MapFindLocation then
    bbEdit.OnClick(nil)
  else begin
    Close;
    Exit;
  end;

  { Add the boundary into the record }
  with TfrmLocationDetails(DetailForm) do
  begin
    pcLocationDetails.ActivePage := tsGeoInfo;
    pcGeoInfo.ActivePage         := tsBoundaries;

    AddBoundary(AMapServerLink.SheetMapSheetKey(ALocationDetails.SheetID),
                IntToStr(ALocationDetails.ObjectStaticID));

    { Now Move to the General Tab }
    pcLocationDetails.ActivePage := tsGeneral;
    { If spatial ref empty, use polygon centroid }
    if SpatialRef.EnteredRef = '' then begin
      { Load Centroid Coordinates into a TStringCoordinate to allow Conversion }
      lLatLong := SpecificENToLatLong(TMapCoord(ALocationDetails.ObjectCentroid),
                    AppSettings.AvailableMaps.ItemsByKey[ABaseMapKey].SpatialSystem);

      SpatialRef.EnteredRef := ConvertfromLatLong(lLatLong, AppSettings.SpatialRefSystem);
      SpatialRef.DisplayRef := SpatialRef.EnteredRef;
      SpatialRef.Qualifier  := ResStr_InternalMap;
    end;
  end // with
end;

//==============================================================================
// Called when the user double clicks on a map Location
// which has a location associated with it.  Returns False if not found
function TfrmLocations.GotoSiteFromMapObject(const ALocationDetails: TQueryDetails): Boolean;
begin
  ResetDetailForm('Location: ', TfrmLocationDetails);
  Result := LocateNode('', ALocationDetails.ObjectLocationKey, True);
end;  // GotoSiteFromMapObject

//==============================================================================
// FIND LOCATION
function TfrmLocations.FindLocation: Boolean;
var
  locationOnly: Boolean;
  location: TCaptionAndKey;
begin
  Result := False;
  // Check This
  locationOnly := SelectedItem.ImageIndex < FEATURE_IMAGE_INDEX;
  location := LookForLocation(locationOnly);
  if location.ItemKey <> '' then
    Result := LocateNode(location.Caption, location.ItemKey, locationOnly);
end;  // FindLocation

//==============================================================================
function TfrmLocations.LookForLocation(LocationOnly: Boolean) : TCaptionAndKey;
var
  lFinder:TdlgFind;
begin
  Result.ItemKey := '';

  if LocationOnly then
    lFinder := TdlgFind.CreateDialog(nil, ResStr_FindLocationName, ftLocation)
  else
    lFinder := TdlgFind.CreateDialog(nil, ResStr_FindFeatureName, ftFeature);

  with lFinder do
    try
      if ShowModal = mrOk then
      begin
        // Locate the node and update the selection
        Result.ItemKey := ItemKey;
        if LocationOnly then  // Location_Name_Key returned. We need the Location_Key
          Result.ItemKey := dmGeneralData.GetLocKeyFromLocNameKey(Result.ItemKey);
        Result.Caption := ItemText;
      end;
    finally
      Free;
    end;
end;  // LookForLocation

//==============================================================================
function TfrmLocations.InternalLocateNode(const ANodeKey: TKeyString;
  const ALocation: Boolean): TFlyNode;
var
  nodeCount: Integer;
  node: TFlyNode;
  ancestorList: TStringList;
begin
  { Convert to Location Key }
  ancestorList := TStringList.Create;
  try
    { Does the Location have a parent Location? }
    BuildAncestryList(ANodeKey, ancestorList, ALocation);
    ancestorList.Add(ANodeKey);

    { If the SelectedItem is a top level node...
      ie. the treeview itself - nothing selected. }
    node := tvLocations.Items.GetFirstNode;
    if node <> nil then
    begin
      for nodeCount := 0 to ancestorList.Count - 1 do
      begin
        // Find the node on the current level with matching key
        while (TNodeObject(node.Data).ItemKey <> ancestorList[nodeCount]) do
        begin
          node := node.GetNextSibling;
          if node = nil then Break;
        end;
        if node = nil then Break;

        // Expand the node and get first child to go down one level
        if (nodeCount < ancestorList.Count - 1) and (node <> nil) then begin
          node.Expand(False);
          node := node.GetFirstChild;
        end;
      end;
      // Force end updates, otherwise we can't select new nodes as they has no index
      tvLocations.Items.EndUpdate;
      tvLocations.Items.BeginUpdate;
    end;
    Result := node;
  finally
    ancestorList.Free;
  end;
end;  // InternalLocateNode

//==============================================================================
function TfrmLocations.LocateNode(const iNodeText: String;
  const iNodeKey: TKeyString; const iLocation : Boolean) : Boolean;
var
  node: TFlyNode;
begin
  node := InternalLocateNode(iNodeKey, iLocation);
  if node <> nil then
    tvLocations.Selected := node
  else
    tvLocations.Selected := tvLocations.Items.GetFirstNode;

  Result := node <> nil;
end;  // LocateNode

//==============================================================================
procedure TfrmLocations.RelatedObservations(const iMsg: String;
  const iRelType: TRelatedData; const SQLType: TStandardSQL);
var lKeyList: TEditableKeyList;
begin
  lKeyList:=TEditableKeyList.Create;
  dmGeneralData.qryAllPurpose.parseSQL := False;
  try
    dmGeneralData.GetKeyListFromStandardQuery(lKeyList, SQLType, TNodeObject(tvLocations.Selected.Data).ItemKey);
    if lKeyList.Header.ItemCount = 0 then
      MessageDlg(Format(ResStr_NoLocRelation, [iMsg]), mtInformation, [mbOk], 0)
    else
      if dmFormActions.actObservations.Execute then
        TfrmObservations(frmMain.GetForm(TfrmObservations)).DisplayObservations(iRelType, lKeyList);
  finally
    lKeyList.Free;
    dmGeneralData.qryAllPurpose.parseSQL := True;    
  end;
end;  // RelatedObservations

//==============================================================================
procedure TfrmLocations.mnuRelSurveysClick(Sender: TObject);
begin
  inherited;
  RelatedObservations(ResStr_Surveys, rdSurvey, ssSurveysForLocation);
end;  // mnuRelSurveysClick

//==============================================================================
procedure TfrmLocations.mnuRelEventsClick(Sender: TObject);
begin
  inherited;
  RelatedObservations(ResStr_SurveyEvents,rdEvent,ssEventsForLocation);
end;  // mnuRelEventsClck

//==============================================================================
procedure TfrmLocations.mnuRelSamplesClick(Sender: TObject);
begin
  inherited;
  RelatedObservations(ResStr_Samples,rdSample,ssSamplesForLocation);
end;  // mnuRelSamplesClick

//==============================================================================
procedure TfrmLocations.mnuRelOccurClick(Sender: TObject);
begin
  inherited;
  RelatedObservations(ResStr_TaxonOrBiotopeOcc,rdOccurrence,ssOccurrencesForLocation);
end;  // mnuRelOccurClick

//==============================================================================
procedure TfrmLocations.mnuRelIndivOrgClick(Sender: TObject);
var lNameList:TEditableKeyList;
    lNameSearchType : TStandardSQL;
    lLocType : String;
begin
  inherited;
  lNameList:=TEditableKeyList.Create;

  { Determine whether to search for Location or Feature related data }
  if TNodeObject (tvLocations.Selected.Data) is TFeatureNode then begin
    lNameSearchType := ssNamesForFeature;
    lLocType       := 'Feature';
  end else begin
    lNameSearchType := ssNamesForLocation;
    lLocType       := 'Location';
  end;

  try
    dmGeneralData.GetKeyListFromStandardQuery(lNameList, lNameSearchType, TNodeObject(tvLocations.Selected.Data).ItemKey);
    if lNameList.Header.ItemCount=0 then
      MessageDlg(Format(ResStr_CannotRelateLocationType, [lLocType]), mtInformation, [mbOk], 0)
    else
      if dmFormActions.actNames.Execute then
        TfrmIndOrg(frmMain.GetForm(TfrmIndOrg)).DisplayNames(lNameList);
  finally
    lNameList.Free;
  end;
end;  // mnuRelIndivOrgClick

//==============================================================================
procedure TfrmLocations.mnuRelDocumentsClick(Sender: TObject);
var stMsg      :String;
    lSourceList:TEditableKeyList;
begin
  inherited;
  lSourceList:=TEditableKeyList.Create;
  try
    if tvLocations.Selected.ImageIndex<FEATURE_IMAGE_INDEX then begin
      TfrmLocationDetails(DetailForm).Sources.GetInternalSources(lSourceList);
      stMsg:=ResStr_Location;
    end else begin
      TfrmFeatureDetails(DetailForm).Sources.GetInternalSources(lSourceList);
      stMsg:=ResStr_LBFeature;
    end;

    if lSourceList.Header.ItemCount=0 then
      MessageDlg(Format(ResStr_CannotRelateFeature, [stMsg]),mtInformation,[mbOk],0)
    else
      if dmFormActions.actDocuments.Execute then
        TfrmReferences(frmMain.GetForm(TfrmReferences)).DisplaySources(lSourceList);
  finally
    lSourceList.Free;
  end;
end;  // mnuRelDocumentsClick

//==============================================================================
procedure TfrmLocations.BuildAncestryList(const AKey:TKeyString; AList:TStringList;
          const iLocation:Boolean);
var
  lParentKey : TKeyString;
begin
  // Move up the hierarchy of locations to the top one
  with FdmLocation.qryFindParentLoc do begin
    // Get the parent key for the current one
    if iLocation then begin
    //Build SQL for location
      SQL.Clear;
      SQL.Add(
          'SELECT Parent_Key FROM Location WHERE Location_Key = '''
          + AKey + '''');
    end else begin
      SQL.Clear;
      SQL.Add(
          'SELECT Location_Key AS Parent_Key FROM Location_Feature WHERE Location_Feature_Key = '''
          + AKey + '''');
    end;
    Open;
    lParentKey := FieldByName('Parent_Key').AsString;
    Close;
  end;
  if (lParentKey<>'') and (lParentKey<>AKey) then begin
    BuildAncestryList(lParentKey,AList, True);  // Get the parent location.  Note always a location not a feature.
    AList.Add(lParentKey);  // Add this locaiton to list
  end;
end;  // BuildAncestryList

//==============================================================================
procedure TfrmLocations.GetLocationsForLocations(const keyList: TKeyList; var locationKeys: String);
var
  siteKeys: TStringList;
  i: Integer;
begin
  locationKeys := '';
  if keyList.Header.ItemCount = 0 then Exit;

  siteKeys := TStringList.Create;
  try
    with keyList do
      for i := 0 to Header.ItemCount - 1 do begin
        BuildAncestryList(Items[i].KeyField1, siteKeys, True);
        siteKeys.Add(Items[i].KeyField1);
      end;
    locationKeys := '''' + StringReplace(siteKeys.CommaText, ',', ''',''', [rfReplaceAll]) + '''';
  finally
    siteKeys.Free;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.DisplayLocations(const ALocationList: TKeyList);
var
  lKeyList: TStringList;
  lLocationKeys: String;
  lCursor : TCursor;
begin
  if (DetailForm<>nil) and (TfrmBaseDockedForm(DetailForm).EditMode<>emView) then
    MessageDlg(ResStr_CannotProceed,mtWarning,[mbOk],0)
  else begin
    lKeyList := TStringList.Create;
    ClearTree;
    LockWindowUpdate(Handle);
    lCursor := HourglassCursor;
    try
      lLocationKeys := '';
      GetLocationsForLocations(ALocationList, lLocationKeys);
      // Clear previous content and populate Top sites
      FdmLocation.LocationFilter := lLocationKeys;
      PopulateTopLevel;

      if ALocationList.Header.ItemCount = 1 then
        LocateNode('', ALocationList.Items[0].KeyField1, True)
      else
        tvLocations.Selected := tvLocations.Items.GetFirstNode;
      btnShowAll.Visible := True;
    finally
      lKeyList.Free;
      DefaultCursor(lCursor);
      LockWindowUpdate(0);
    end;
  end;
end;  // DisplayLocations

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.GetLocationsForFeatures(const keyList: TKeyList; var locationKeys,
  featureKeys: String);
var
  siteKeys: TStringList;
  i: integer;
begin
  featureKeys := '';
  locationKeys := '';
  if keyList.Header.ItemCount = 0 then Exit;

  siteKeys := TStringList.Create;
  try
    with keyList do
      for i := 0 to Header.ItemCount - 1 do
        AddToCommaSeparatedList(featureKeys, '''' + Items[i].KeyField1 + '''');

    // Convert feature list to hierarchical location list
    with dmDatabase.ExecuteSQL(Format(
        'SELECT DISTINCT Location_Key FROM Location_Feature WHERE Location_Feature_Key IN (%s)',
        [featureKeys]), True) do
    begin
      if not (Eof and Bof) then
        MoveFirst;
      while not Eof do begin
        BuildAncestryList(VarToStr(Fields['Location_Key'].Value), siteKeys, True);
        siteKeys.Add(VarToStr(Fields['Location_Key'].Value));
        MoveNext;
      end;
    end;
    locationKeys := '''' + StringReplace(siteKeys.CommaText, ',', ''',''', [rfReplaceAll]) + '''';
  finally
    siteKeys.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Filters the screen to show a given list of features
}
procedure TfrmLocations.DisplayFeatures(const AFeatureList: TKeyList);
var
  lFeatureKeys: string;
  lLocationKeys: string;
  lCursor: TCursor;
const
  SQL_SELECTION_FEATURE_LOCATIONS = 'SELECT DISTINCT Location_Key FROM Location_Feature '+
      'WHERE Location_Feature_Key IN (%s)';
begin
  if (DetailForm<>nil) and (TfrmBaseDockedForm(DetailForm).EditMode<>emView) then
    MessageDlg(ResStr_CannotProceed,mtWarning,[mbOk],0)
  else begin
    ClearTree;
    LockWindowUpdate(Handle);
    lCursor := HourglassCursor;
    tvLocations.Items.BeginUpdate;
    try
      lFeatureKeys := '';
      lLocationKeys := '';
      GetLocationsForFeatures(AFeatureList, lLocationKeys, lFeatureKeys);

      FdmLocation.LocationFilter := lLocationKeys;
      FdmLocation.FeatureFilter := lFeatureKeys;
      PopulateTopLevel;

      if AFeatureList.Header.ItemCount=1 then
        LocateNode('', AFeatureList.Items[0].KeyField1, False);
      btnShowAll.Visible := True;
    finally
      LockWindowUpdate(0);
      DefaultCursor(lCursor);
      tvLocations.Items.EndUpdate;
    end; // try
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.DisplayFilteredLocations;
var
  i: Integer;
  keyList: TEditableKeyList;
  filteredKeys: TStringList;
  locationKeys, featureKeys, tmp: String;
  cursor: TCursor;
begin
  keyList := TEditableKeyList.Create;
  ClearTree;
  LockWindowUpdate(Handle);
  cursor := HourglassCursor;
  tvLocations.Items.BeginUpdate;
  try
    keyList.SetTable(MIXED_DATA);
    locationKeys := '';
    featureKeys := '';

    filteredKeys := AppSettings.GetFilteredRecords(TN_LOCATION);
    if filteredKeys <> nil then begin
      for i := 0 to filteredKeys.Count - 1 do
        keyList.AddItem(filteredKeys[i], TN_LOCATION);

      GetLocationsForLocations(keyList, locationKeys);
    end;

    keyList.Clear;
    filteredKeys := AppSettings.GetFilteredRecords(TN_LOCATION_FEATURE);
    if filteredKeys <> nil then begin
      for i := 0 to filteredKeys.Count - 1 do
        keyList.AddItem(filteredKeys[i], TN_LOCATION_FEATURE);

      // Don't want to lose the previous location keys, so use a temp var
      GetLocationsForFeatures(keyList, tmp, featureKeys);
      // Then add to the previous lot
      AddToCommaSeparatedList(locationKeys, tmp);
    end;
    
    FdmLocation.LocationFilter := locationKeys;
    FdmLocation.FeatureFilter := featureKeys;
    PopulateTopLevel;
    if tvLocations.Items.Count = 0 then
      btnShowAllClick(nil)
    else
      btnShowAll.Visible := True;
  finally
    keyList.Free;
    LockWindowUpdate(0);
    DefaultCursor(cursor);
    tvLocations.Items.EndUpdate;
    tvLocations.Selected := tvLocations.Items.GetFirstNode;
  end;
end;

//==============================================================================
procedure TfrmLocations.btnShowAllClick(Sender: TObject);
var
  nodeChain: TStringList;
begin
  inherited;
  AppSettings.ClearFilteredRecords([TN_LOCATION, TN_LOCATION_FEATURE]);
  nodeChain := TStringList.Create;
  try
    RememberPathToSelectedNode(tvLocations, nodeChain);
    ClearFilter(FILTER_LOCATION);
    ClearTree;
    FFilteredKeys.Clear;
    FdmLocation.LocationFilter := '';
    FdmLocation.FeatureFilter := '';
    PopulateTopLevel;
    RedisplaySelectedNode(tvLocations, nodeChain);
  finally
    nodeChain.Free;
  end;
  btnShowAll.Visible := False;
end;  // btnShowAllClick

//==============================================================================
procedure TfrmLocations.ApplyFilter(AKeyList: TKeyList);
begin
  if AKeyList.Header.ItemCount = 0 then
    MessageDlg(ResStr_NoMathingFiltering, mtInformation, [mbOk], 0)
  else
    DisplayLocations(AKeyList);
end;

//==============================================================================
{ Pass in a Location Name Key }
function TfrmLocations.MapLocateNode(const ANodeText: String; ALocationNameKey: 
    string): Boolean;
begin
  { Convert to Location Key }
  // Need Location_Key, not Location_Name_Key
  dmGeneralData.GetLocationNameFromLocNameKey(ALocationNameKey);
  Result:=LocateNode(ANodeText, ALocationNameKey, True);
end;  // MapLocateNode

//==============================================================================
// MAP FIND LOCATION

{ Called by the map to find a location }
function TfrmLocations.MapFindLocation: Boolean;
var
  stTitle : String;
  lCOunt : Integer;
  lNode : TFlyNode;
begin
  Result :=False;
  if tvLocations.Items.Count>0 then begin
    stTitle:=ResStr_FindLocationName;
    with TdlgFind.CreateDialog(nil, stTitle, ftLocation) do
      try
        eSearchText.ClearSourceList;
        { If the selected item is a top level node... }
        if SelectedItem.Parent = nil then
        begin
          // Set top site search list
          for lCount:=0 to tvLocations.Items.Count-1 do begin
            lNode := tvLocations.Items[lCount];
            if lNode.Parent = nil then
              eSearchText.AddToSourceList(TNodeObject(lNode.Data).ItemKey, lNode.Text, []);
          end;
        end else begin
          // Set child Site / Feature search list
          lNode := SelectedItem.Parent;
          for lCount:=0 to lNode.Count-1 do
            if lNode.Item[lCount].ImageIndex=SelectedItem.ImageIndex then
              eSearchText.AddToSourceList(TNodeObject(lNode.Item[lCount].Data).ItemKey,
                                          lNode.Item[lCount].Text, []);
        end;
        if ShowModal=mrOk then
          // Locate the node and update the selection
          Result := MapLocateNode(ItemText, ItemKey)
        else
        if tvLocations.Selected=nil then
          Result := False;
      finally
        Free;
      end;
  end else
    MessageDlg(ResStr_NoLocationToLink, mtInformation, [mbOk], 0);
end;  // MapFindLocation

//==============================================================================
procedure TfrmLocations.actFilterExecute(Sender: TObject);
begin
  inherited;
  GetFilter(FILTER_LOCATION, TN_LOCATION);
end;  // actFilterExecute

//==============================================================================
procedure TfrmLocations.actFindExecute(Sender: TObject);
begin
  inherited;
  if (tvLocations.Items.Count > 0) then FindLocation;
end;  // actFindExecute

//==============================================================================
procedure TfrmLocations.WMRefreshSpatialRefSystem(var Msg: TMessage);
begin
  if DetailForm<>nil then
    if DetailForm is TfrmLocationDetails then TfrmLocationDetails(DetailForm).SetDisplaySystem;
end;  // WMRefreshSpatialRefSystem

//==============================================================================
function TfrmLocations.GetSelectedItems(IncludeFeatures: Boolean): TObjectList;
var
  node: TFlyNode;
begin
  Result := TObjectList.Create(False);
  node := tvLocations.Items.GetFirstSelectedNode;
  while Assigned(node) do begin
    if (node.ImageIndex < FEATURE_IMAGE_INDEX) or IncludeFeatures then
      Result.Add(node);
    node := tvLocations.Items.GetNextSelectedNode(node);
  end;
end;  // GetSelectedItems

//==============================================================================
procedure TfrmLocations.actPromoteNodeExecute(Sender: TObject);
var
  i: Integer;
  node: TFlyNode;
  selectedNodes: TObjectList;
begin
  inherited;
  selectedNodes := GetSelectedItems(False);
  try
    for i := 0 to selectedNodes.Count - 1 do begin
      node := TFlyNode(selectedNodes[i]);
      UpdateNodeParentKey(nil, node);
      node.MoveTo(nil, naAdd);
      actSortSiteNameExecute(nil);
    end;
  finally
    selectedNodes.Free;
  end;
  FSelectedItem := tvLocations.Items.GetFirstSelectedNode;
  UpdateSubMenus(SelectedItem);
end;  // actPromoteNodeExecute

//==============================================================================
procedure TfrmLocations.actMoveToExecute(Sender: TObject);
var
  location: TCaptionAndKey;
  parentNode, node: TFlyNode;
  selectedNodes: TObjectList;
  i: Integer;

  //----------------------------------------------------------------------------
  function CheckMoveAllowed: Boolean;
  var checkNode: TFlyNode;
  begin
    checkNode := parentNode;
    Result := True;
    while Assigned(checkNode) do begin
      if checkNode = node then begin
        Result := False;
        Break;
      end;
      checkNode := checkNode.Parent;
    end;
  end;  // CheckMoveAllowed
  {----------------------------------------------------------------------------
   Checks that all the selected locations are allowed to move to the desired location
  }
  function AllNodesMoveable : Boolean;
  var
    j : Integer;
  begin
    Result := True;
    for j := 0 to selectedNodes.Count - 1 do
    begin
      node := TFlyNode (selectedNodes[j]);
      if not CheckMoveAllowed then
      begin
        Result := False;
        break;
      end;
    end;
  end; //AllNodesMoveable
  //----------------------------------------------------------------------------

begin
  inherited;
  location := LookForLocation(True);
  if location.ItemKey <> '' then begin
    parentNode := InternalLocateNode(location.ItemKey, True);

    // Need to gather references to selected nodes, or might end up in endless loop
    // if relying only on GetNextSelectedNode after moving them around.
    selectedNodes := GetSelectedItems;
    //Check if all the nodes are moveable because the selection may have
    //the destination location itself too..
    if AllNodesMoveable then
    begin
      try
        parentNode.Expand(False);
        for i := 0 to selectedNodes.Count - 1 do
        begin
          node := TFlyNode(selectedNodes[i]);
          node.MoveTo(parentNode, naAddChild);
          UpdateNodeParentKey(parentNode, node);
        end;
      finally
        selectedNodes.Free;
      end;
    end else
      ShowInformation(ResStr_CannotMoveLocation);
  end;
  FSelectedItem := tvLocations.Items.GetFirstSelectedNode;
  UpdateSubMenus(SelectedItem);
end;  // actMoveToExecute

//==============================================================================
procedure TfrmLocations.WMRefreshDocuments(var Msg: TMessage);
begin
  if DetailForm<>nil then
    if DetailForm is TfrmLocationDetails then TfrmLocationDetails(DetailForm).Sources.RefreshLists else
    if DetailForm is TfrmFeatureDetails then TfrmFeatureDetails(DetailForm).Sources.RefreshLists;
end;

//==============================================================================
procedure TfrmLocations.WMRefreshNames(var Msg: TMessage);
begin
  if DetailForm<>nil then
    if DetailForm is TfrmLocationDetails then TfrmLocationDetails(DetailForm).RefreshNames else
    if DetailForm is TfrmFeatureDetails then TfrmFeatureDetails(DetailForm).RefreshNames;
end;

//==============================================================================
procedure TfrmLocations.WMRefreshColours(var Msg: TMessage);
begin
  if DetailForm<>nil then
    if DetailForm is TfrmLocationDetails then TfrmLocationDetails(DetailForm).RefreshColours else
    if DetailForm is TfrmFeatureDetails  then TfrmFeatureDetails(DetailForm).RefreshColours;
  Repaint;
end;

//==============================================================================
procedure TfrmLocations.WMImportedComplete(var Msg: TMessage);
begin
  //Commented out as cannot test at present
  //Refresh display
  //PopulateTopLevel;
end;

//==============================================================================
procedure TfrmLocations.actShowMetadataExecute(Sender: TObject);
begin
  DetailForm.ShowMetaData;
end;

//==============================================================================
procedure TfrmLocations.actFindOnMapExecute(Sender: TObject);
begin
  FDetailForm.FindOnMap;
end;

//==============================================================================
{ Override method in base form to read feature sort orders from registry,
     no need fo locations because they always sort by item name }
procedure TfrmLocations.ReadSortOrders;
var lSortAsc: Boolean;
begin
  if not AppSettings.ReadSort(Self, 'Location', FSortOrder, lSortAsc) then
    FSortOrder := LOCATION_NAME_SORT
  else
    CheckValidLocationField;
  if not AppSettings.ReadSort(Self, 'Feature', FFeatureField, lSortAsc) then
    FFeatureField := FEATURE_NAME_SORT
  else
    CheckValidFeatureField;
end;  // ReadSortOrders

//==============================================================================
{ Override method in base form to write feature sort orders to registry }
procedure TfrmLocations.WriteSortOrders;
begin
  AppSettings.WriteSort(Self, 'Location', FSortOrder, True);
  AppSettings.WriteSort(Self, 'Feature', FFeatureField, True);
end;  // WriteSortOrders

//==============================================================================
{ Enables RelatedData PopUpMenu Items depending upon Type of node selected in
  tvLocations }
procedure TfrmLocations.SetRelatedDataMenuItems;
begin
  if TNodeObject(tvLocations.Selected.Data) is TFeatureNode then begin
    mnuRelSurveys.Visible   := False;
    mnuRelEvents.Visible    := False;
    mnuRelSamples.Visible   := False;
    mnuRelOccur.Visible     := False;
    N4.Visible              := False;
    mnuRelIndivOrg.Visible  := True;
    mnuRelDocuments.Visible := True;
  end else begin
    mnuRelSurveys.Visible   := True;
    mnuRelEvents.Visible    := True;
    mnuRelSamples.Visible   := True;
    mnuRelOccur.Visible     := True;
    N4.Visible              := True;
    mnuRelIndivOrg.Visible  := True;
    mnuRelDocuments.Visible := True;
  end;
end;  // SetRelatedDataMenuItems

//==============================================================================
{ Forces Class variable used to sort Features to be a valid value. }
procedure TfrmLocations.CheckValidFeatureField;
begin
  if (FFeatureField <> FEATURE_NAME_SORT) and (FFeatureField <> FEATURE_TYPE_SORT) then
    FFeatureField := FEATURE_NAME_SORT;
end; // CheckValidFeatureField

//==============================================================================
{ Forces Class variable used to sort locations to be a valid value. }
procedure TfrmLocations.CheckValidLocationField;
begin
  if (FSortOrder <> LOCATION_NAME_SORT) and (FSortOrder <> LOCATION_FILE_CODE_SORT) and
      (FSortOrder <> LOCATION_SPATIAL_REF_SORT) then
    FSortOrder := LOCATION_NAME_SORT;
end; // CheckValidLocationField

//==============================================================================
procedure TfrmLocations.tvLocationsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_DELETE: if bbDelete.Enabled then bbDeleteClick(bbDelete);
  end;
end;

// Description : implements GetCurrentControlEditMode required for TBaseExportable
// Created : 15/11/02
function TfrmLocations.GetCurrentControlEditMode: TEditMode;
begin
  if not assigned(DetailForm) then // safety check
    Result := emNone
  else begin
    Result := TfrmBaseDockedForm(DetailForm).EditMode; // default
    // Disable edit lock messages for all sub list grids
    if DetailForm is TfrmLocationDetails then begin
      with TfrmLocationDetails(DetailForm) do begin
        if (pcLocationDetails.ActivePage = tsDesignations) or
           (pcLocationDetails.ActivePage = tsSources) or
           (pcLocationDetails.ActivePage = tsMeasurements) then
          Result := emNone
        else if (pcLocationDetails.ActivePage = tsGeoInfo) then begin
          // grid squares should be editable in first column only
          if (pcGeoInfo.ActivePage = tsBoundaries) or
             (pcGeoInfo.ActivePage = tsAdminAreas) or
             (pcGeoInfo.ActivePage = tsGridSquares) and (sgGridSquares.Col=1) then
            Result := emNone;
        end
        else if pcLocationDetails.ActivePage = tsOtherInfo then
          if (pcOtherInfo.ActivePage = tsTenure) or
             (pcOtherInfo.ActivePage = tsUses) or
             (pcOtherInfo.ActivePage = tsRelations) then
            Result := emNone;

      end;
    end
    else if DetailForm is TfrmFeatureDetails then begin
      with TfrmFeatureDetails(DetailForm) do begin
        if (pcFeatureDetails.ActivePage = tsAims) or
            (pcFeatureDetails.ActivePage = tsDamages) or
             (pcFeatureDetails.ActivePage = tsPotentialThreats) or
             (pcFeatureDetails.ActivePage = tsSources) then
          Result := emNone;
      end;
    end;
  end;
end;

//==============================================================================
procedure TfrmLocations.pmHQuickReportsClick(Sender: TObject);
begin
  inherited;
  BringToFront; // in case popup activated when not the active mdi child, which can happen
  frmMain.PopulateQuickReportSubMenu(tvLocations.Selected, pmHQuickReports);
end;


{-------------------------------------------------------------------------------
  Description : Returns True if a location node is selected
  Created : 11/03/2003 }
function TfrmLocations.Get_CanProvideSQL: WordBool;
begin
  Result := False;
  if Assigned(tvLocations.Selected) then
    if Assigned(tvLocations.Selected.Data) then
      if TObject(tvLocations.Selected.Data) is TSiteNode then
        Result := True;
end;

{-------------------------------------------------------------------------------
  Description : Returns SQL to list occurrences for a location.  Used to
              populate the Quick Reports.
  Created : 11/03/2003 }
function TfrmLocations.Get_OccurrencesSQL(const ATypes: WideString): WideString;
const
  SQL_TAXON_OCCURRENCES = 'SELECT TOCC.TAXON_OCCURRENCE_KEY AS OCCURRENCE_KEY, ''T'' AS TYPE '+
             'FROM TAXON_OCCURRENCE TOCC INNER JOIN SAMPLE TS ON TS.SAMPLE_KEY=TOCC.SAMPLE_KEY ' +
             'WHERE TS.LOCATION_KEY=''%s'' ';
  SQL_BIOTOPE_OCCURRENCES =
             'SELECT BOCC.BIOTOPE_OCCURRENCE_KEY AS OCCURRENCE_KEY, ''B'' AS TYPE '+
             'FROM BIOTOPE_OCCURRENCE BOCC INNER JOIN SAMPLE BS ON BS.SAMPLE_KEY=BOCC.SAMPLE_KEY ' +
             'WHERE BS.LOCATION_KEY=''%s'' ';
begin
  if Assigned(tvLocations.Selected) then
    if Assigned(tvLocations.Selected.Data) then
      if TObject(tvLocations.Selected.Data) is TSiteNode then begin
        Result := '';
        if Pos('T', ATypes)>0 then
          Result := Format(SQL_TAXON_OCCURRENCES,
                           [TSiteNode(tvLocations.Selected.Data).ItemKey]) +
                    SQL_TAXON_RESTRICTION;
        if Pos('B', ATypes)>0 then begin
          if Result = '' then
            Result := Format(SQL_BIOTOPE_OCCURRENCES,
                             [TSiteNode(tvLocations.Selected.Data).ItemKey]) +
                    SQL_BIOTOPE_RESTRICTION
          else
            Result := Result + 'UNION ' +
                      Format(SQL_BIOTOPE_OCCURRENCES,
                             [TSiteNode(tvLocations.Selected.Data).ItemKey]) +
                      SQL_BIOTOPE_RESTRICTION;
        end;
      end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.UpdateMapWindowSelector;
begin
  actFindOnMap.Enabled := actFilter.Enabled and (AppSettings.AvailableMaps.Count > 0);
  if Assigned(DetailForm) then
    DetailForm.UpdateMapWindowSelector;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.DragOverCheck(APoint: TPoint; const ATable, AFieldKey: String;
  var Accept: Boolean);
var
  lPos: TPoint;
  lHitTest: THitTests;
  lNode: TFlyNode;
begin
  inherited;
  if (CompareText(ATable, TN_LOCATION) = 0) or
     (CompareText(ATable, TN_LOCATION_FEATURE) = 0) then
    with tvLocations do begin
      lPos := ScreenToClient(APoint);
      lHitTest := GetHitTestInfoAt(lPos.X, lPos.Y);

      // Expand node if hovering on (+).
      if htOnButton	in lHitTest then begin
        lNode := GetNodeAt(lPos.X, lPos.Y);
        if Assigned(lNode) then
          lNode.Expand(False);
      end else
      // Scroll tree up if hovering just above tree.
      if htAbove in lHitTest then begin
        lNode := TopItem;
        if lNode.GetPrevVisible <> nil then
          TopItem := lNode.GetPrevVisible;
      end else
      // Scroll tree down, if hovering just below tree.
      if htBelow in lHitTest then begin
        // Need to use -1, or we get the wrong last fully visible item.
        lNode := GetNodeAtRow(TopRow + VisibleRowCount - 1);
        if Assigned(lNode) then
          if lNode.GetNextVisible <> nil then
            TopItem := TopItem.GetNextVisible;
      end;
      // Get node under mouse and check it can accept the drop.
      lNode := GetNodeAt(lPos.X, lPos.Y);
      if Assigned(lNode) then
        Accept := lNode.ImageIndex < FEATURE_IMAGE_INDEX
      else
      if Assigned(Selected) then
        // Always allow to drop SITE node on nothing, bring to top level, never FEATURE node!
        Accept := Selected.ImageIndex < FEATURE_IMAGE_INDEX;
    end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.tvLocationsExit(Sender: TObject);
begin
  inherited;
  dmFormActions.actCut.Enabled := False;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.UpdateNodeParentKey(AParentNode, AChildNode: TFlyNode);
var
  lParams: Array of Variant;
begin
  if Assigned(AParentNode) then
    lParams := VarArrayOf(['@Key', TNodeObject(AChildNode.Data).ItemKey,
                           '@ParentKey', TNodeObject(AParentNode.Data).ItemKey,
                           '@UserID', AppSettings.UserID])
  else
    lParams := VarArrayOf(['@Key', TNodeObject(AChildNode.Data).ItemKey,
                           '@ParentKey', Null,
                           '@UserID', AppSettings.UserID]);

  if AChildNode.ImageIndex < FEATURE_IMAGE_INDEX then
    dmDatabase.RunStoredProc('usp_Location_Update_ForDragDrop', lParams)
  else
  if AChildNode.ImageIndex = FEATURE_IMAGE_INDEX then
    dmDatabase.RunStoredProc('usp_LocationFeature_Update_ForDragDrop', lParams);
end;

{-------------------------------------------------------------------------------
}
function TfrmLocations.GetDetailsPageControl: TPageControl;
begin
  if Assigned(DetailForm) then begin
    if DetailForm is TfrmLocationDetails then
      Result := TfrmLocationDetails(DetailForm).pcLocationDetails
    else
      Result := TfrmFeatureDetails(DetailForm).pcFeatureDetails;
  end
  else
    Result := nil;
end;

function TfrmLocations.SelectNode(const ANodeType,
  ANodeKey: string): TFlyNode;
var
  lIsLocation: boolean;
begin
  if (CompareText(ANodeType, 'Location')<>0) and
      (CompareText(ANodeType, 'Feature')<>0) then
    raise ELocationError.Create(Format(ResStr_InvalidSelectNodeType, [ANodeType]));
  lIsLocation := (CompareText(ANodeType, 'Location')=0);
  if LocateNode('', ANodeKey, lIsLocation) then
    Result := tvLocations.Selected
  else
    Result := nil;
end;

procedure TfrmLocations.ExpandAllNodes ;
begin
//  kjg 26/11/2004

  inherited ExpandAllNodes;

end;

{-------------------------------------------------------------------------------
  Hide Quick Reports menu item for Feature nodes.
}
procedure TfrmLocations.pmHierarchyPopup(Sender: TObject);
begin
  inherited;
  if Assigned(tvLocations.Selected) then
    pmHQuickReports.Visible := TObject(tvLocations.Selected.Data) is TSiteNode
  else
    pmHQuickReports.Visible := False;
end;

{-------------------------------------------------------------------------------
  Retrieve current node type for custom reports
}
function TfrmLocations.CustomReportKeyType: TKeyType;
begin
  Result := inherited CustomReportKeyType;
  with tvLocations do
    if Assigned(Selected) then
      if TObject(Selected.Data) is TReportableNode then
        Result := TReportableNode(Selected.Data).ReportKeyType;
end;

{-------------------------------------------------------------------------------
  Retrieve current node key for custom reports
}
function TfrmLocations.ItemKey: string;
begin
  Result := inherited ItemKey;
  with tvLocations do
    if Assigned(Selected) then
      if TObject(Selected.Data) is TReportableNode then
        Result := TReportableNode(Selected.Data).ItemKey
end;

{===============================================================================
  Addin node management.
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfrmLocations.AddinMenuClick(Sender: TObject);
var lCaption: String;
begin
  lCaption := RemoveAmpersands(TMenuItem(Sender).Caption);
  // SelectedItem can be a Sample node or non-folder node at any level.
  if Assigned(SelectedItem) then
    AddinNewNode(SelectedItem, lCaption);
end;  // AddinMenuClick

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.AddinNewNode(ANode: TFlyNode; const ATypeName: String);
var lNodeInfo : TNodeInfo;
    i, lTypeID: Integer;
    lNode     : TFlyNode;
    lParentKey: WideString;
begin
  FAddingNewNode := True;
  for i := 0 to FNodeMan.TypeCount - 1 do begin
    // Get proper TypeID
    lTypeID := FNodeMan.TypeID[i];
    // Compare menu caption with type name, should eventually be one match
    if CompareText(ATypeName, RemoveAmpersands(FNodeMan.TypeName[lTypeID])) = 0 then
    begin
      InitializeNodeInfo(lNodeInfo);

      if (ANode.ImageIndex = FEATURE_IMAGE_INDEX) or TAddinLocNode(ANode.Data).IsSubFolder then
        lParentKey := TNodeObject(ANode.Data).ItemKey
      else
        lParentKey := TNodeObject(ANode.Parent.Data).ItemKey;

      if FNodeMan.AddNode(lTypeID, lParentKey, lNodeInfo) then begin
        if (ANode.ImageIndex = FEATURE_IMAGE_INDEX) or
           TAddinLocNode(ANode.Data).IsSubFolder then
        begin
          // Check if we need to remove any dummy nodes.
          if not TNodeObject(ANode.Data).ChildrenPopulated then begin
            ANode.Expand(False);
            TNodeObject(ANode.Data).ChildrenPopulated := True;
          end;
          lNode := tvLocations.Items.AddChild(ANode, lNodeInfo.Caption);
        end else
          lNode := tvLocations.Items.Add(ANode, lNodeInfo.Caption);

        SetNodeProperties(lNode, lNodeInfo);  // Will add object to node too.
        tvLocations.Selected := lNode;
        bbEditClick(nil);  // Trigger Edit for newly added node.
      end;
      Break;
    end;
  end;
  FAddingNewNode := False;
end;  // AddinNewNode

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.AddinSubMenuClick(Sender: TObject);
var lCaption, lContainerCaption: String;
    lNode                      : TFlyNode;
    lIdx, lTypeIdx, lAddTypeID : Integer;
begin
  lCaption := RemoveAmpersands(TMenuItem(Sender).Caption);

  // Need to work backward to find out the correct sub-node to add to.
  with FNodeMan do
    // For each TypeID implemented by the Addin, try to find which one the item
    // selected in menu belongs to.
    for lTypeIdx := 0 to TypeCount - 1 do begin
      lIdx := 0;
      while GetAddType(TypeID[lTypeIdx], lIdx, lAddTypeID) do begin
        // If the add type name matches the menu item caption, bingo!
        if CompareText(lCaption, RemoveAmpersands(TypeName[lAddTypeID])) = 0 then
        begin
          // Get the sub-node caption to look for from the "parent" type ID.
          lContainerCaption := RemoveAmpersands(TypeName[TypeID[lTypeIdx]]);

          // And now locate the correct sub-node, which will be a folder node, presumably.
          lNode := tvLocations.Selected.GetFirstChild;
          // Locate the correct subfolder
          while Assigned(lNode) and (CompareText(lNode.Text, lContainerCaption) <> 0) do
            lNode := lNode.GetNextSibling;

          if Assigned(lNode) then
            Self.AddinNewNode(lNode, lCaption);

          // Dealt with the node, break out of all loops
          Exit;
        end;
        Inc(lIdx);
      end;
    end;
end;  // AddinSubMenuClick

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.SetSubNodesProperties(const AParentNode: TFlyNode;
  const AParentNodeTypeID: Integer);
var lIdx: Integer;
    lNode: TFlyNode;
    lNodeInfo: TNodeInfo;
begin
  if TNodeObject(AParentNode.Data).ChildrenPopulated then begin
    // Just need to refresh some properties. The ItemKey actually.
    lNode := AParentNode.GetFirstChild;
    while Assigned(lNode) do begin
      if TAddinLocNode(lNode.Data).IsSubFolder then
        TAddinLocNode(lNode.Data).ItemKey := TNodeObject(AParentNode.Data).ItemKey;
      lNode := lNode.GetNextSibling;
    end;
  end else begin
    // All sub nodes need to be added.
    lIdx := 0;
    InitializeNodeInfo(lNodeInfo);
    // Loop until function returns False.
    while FNodeMan.GetSubNodeInfo(AParentNodeTypeID, lIdx, lNodeInfo) do begin
      TNodeObject(AParentNode.Data).ChildrenPopulated := True;
      lNode := tvLocations.Items.AddChild(AParentNode, lNodeInfo.Caption);
      SetNodeProperties(lNode, lNodeInfo);
      TAddinLocNode(lNode.Data).IsSubFolder := True;
      // Set the right key for when sub-items get populated, as folders don't have one of their own.
      TAddinLocNode(lNode.Data).ItemKey := TAddinLocNode(AParentNode.Data).ItemKey;

      // Add fake child node if needed.
      tvLocations.Items.AddChild(lNode, '.');

      Inc(lIdx);
    end;
  end;
end;  // SetSubNodesProperties

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.CleanupAddinMenuItems(AMenuItem: TMenuItem);
var i: Integer;
    lItem: TMenuItem;
begin
  for i := AMenuItem.Count - 1 downto 0 do
    if AMenuItem[i].Tag = -1 then begin
      lItem := AMenuItem[i];
      AMenuItem.Delete(i);
      FreeAndNil(lItem);
    end else
      CleanupAddinMenuItems(AMenuItem[i]);
end;  // CleanupAddinMenuItems

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.InitializeNodeInfo(var NodeInfo: TNodeInfo);
begin
  with NodeInfo do begin
    Caption           := '';
    ChildrenPopulated := True;
    ImageIndex        := -1;
    SelectedIndex     := -1;
    StateIndex        := -1;
    ItemKey           := '';
    TypeID            := -1;
    Editable          := True;
    Deletable         := True;
    TableName         := '';
  end;
end;  // InitializeNodeInfo

{-------------------------------------------------------------------------------
}
function TfrmLocations.LoadNodeImage(ImageIndex: Integer; SourceImageList: THandle;
  TargetImageList: TImageList; IndexMap: TStringList): Integer;
var lImageList: TImageList;
begin
  Result := -1;
  if ImageIndex = -1 then Exit;

  // if image already available, use it, otherwise, load it.
  if IndexMap.Values[IntToStr(ImageIndex)] <> '' then
    Result := StrToInt(IndexMap.Values[IntToStr(ImageIndex)])
  else
  // Image index set, use it, but make sure there is an imagelist available too.
  if SourceImageList <> 0 then begin
    lImageList := TImageList.Create(nil);
    try
      // Need to duplicate the imagelist to access bitmaps properly
      lImageList.Handle := ImageList_Duplicate(HIMAGELIST(SourceImageList));
      if lImageList.Count > 0 then begin
        Result := TargetImageList.AddImage(lImageList, ImageIndex);
        // "Remember" which image from addin maps to which image from tree
        IndexMap.Add(IntToStr(ImageIndex) + '=' + IntToStr(Result));
      end;
    finally
      FreeAndNil(lImageList);
    end;
  end;
end;  // LoadNodeImage

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.PerformAddinNodeChange(const ANode: TFlyNode);
var
  lNodeInfo: TNodeInfo;
  lDetailScreenGUID: TGUID;
begin
  Caption := 'Item: ' + GeneralFunctions.RemoveSubStrings(ANode.Text, ['<i>', '</i>']);
  CheckDetailFormClass(nil);

  lNodeInfo := SetNodeInfo(ANode);

  lDetailScreenGUID := StringToGuid('{00000000-0000-0000-0000-000000000000}');
  // Let addin update node properties and get interface to detail screen
  if FNodeMan.NodeChanged(TNodeObject(ANode.Parent.Data).ItemKey, lNodeInfo, lDetailScreenGUID) then
    // Place screen in detail panel
    if GuidToString(lDetailScreenGUID) <> '{00000000-0000-0000-0000-000000000000}' then begin
      FDetailScreen := TOleProxy.Create(Self, lDetailScreenGUID);

      if Assigned(FDetailScreen) then begin
        FDetailScreen.Parent := pnlDetails;
        FDetailScreen.Align := alClient;
        with (FDetailScreen.ControlInterface as IRecorderDetailScreen) do begin
          Events := Self;
          SelectedItemCaption := lNodeInfo.Caption;
          LoadContent(FNodeMan.TypeName[lNodeInfo.TypeID],
                      TNodeObject(ANode.Parent.Data).ItemKey,
                      lNodeInfo.ItemKey);
        end;
      end;
    end;

  // Update Node properties, and images if needed.
  SetNodeProperties(ANode, lNodeInfo);

  bbEdit.Enabled := bbEdit.Enabled and lNodeInfo.Editable and not TAddinLocNode(ANode.Data).IsSubFolder;
  bbDelete.Enabled := bbDelete.Enabled and lNodeInfo.Deletable and not TAddinLocNode(ANode.Data).IsSubFolder;
end;  // PerformAddinNodeChange

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.PopulateAddinNodes(const AParentNode: TFlyNode; const AParentTypeID: Integer);
var i             : Integer;
    lSQL, lSQLDone: WideString;
    lCmd          : TADOCommand;
begin
  if Assigned(FNodeMan) then begin
    lCmd := TADOCommand.Create(nil);
    lCmd.Connection := dmDatabase.dbLocal;

    lSQLDone := '';
    for i := 0 to FNodeMan.TypeCount - 1 do
    begin
      // Get SQL to run
      if FNodeMan.GetData(AParentTypeID, FNodeMan.TypeID[i],
                          TNodeObject(AParentNode.Data).ItemKey, lSQL) then
      begin
        // If something to do, do it, and also if SQL is different! Don't want
        // several times the same nodes...
        if (lSQL <> '') and (lSQL <> lSQLDone) then begin
          lSQLDone := lSQL;  // For next time round.
          lCmd.CommandText := lSQL;
          with lCmd.Execute do begin  // Use with so don't have to declare recordset
            while not Eof do begin
              SetupAddinNode(FNodeMan.TypeID[i], AParentNode, nil, ADODB_TLB.Fields(Fields));
              MoveNext;
            end;  // while not Eof
            Close;
          end;  // with lCmd...
        end;  // if lSQL...
      end;  // if FNodeMan...
    end;  // for...
  end;
end;  // PopulateAddinNodes

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.ResetAddinDetailScreen;
begin
  if Assigned(FDetailScreen) then begin
    (FDetailScreen.ControlInterface as IRecorderDetailScreen).Events := nil;
    FDetailScreen.Free;
  end;
  FDetailScreen := nil;
end;  // ResetAddinDetailScreen

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.SetNodeProperties(ANode: TFlyNode; NodeInfo: TNodeInfo);
begin
  // If SelectedIndex not set, assign same value as ImageIndex
  if NodeInfo.SelectedIndex = -1 then NodeInfo.SelectedIndex := NodeInfo.ImageIndex;

  ANode.ImageIndex := LoadNodeImage(NodeInfo.ImageIndex, FNodeMan.ImageList,
                                    ilLocation, FNodeImagesMap);
  ANode.SelectedIndex := LoadNodeImage(NodeInfo.SelectedIndex,FNodeMan.ImageList,
                                    ilLocation, FNodeImagesMap);
  ANode.StateIndex := -1;

  // Set NodeData properties
  if not Assigned(ANode.Data) then ANode.Data := TAddinLocNode.Create;

  with TAddinLocNode(ANode.Data) do begin
    ChildrenPopulated := NodeInfo.ChildrenPopulated;
    Deletable         := NodeInfo.Deletable;
    Editable          := NodeInfo.Editable;
    ItemKey           := NodeInfo.ItemKey;
    ItemAdditional    := NodeInfo.TableName;
    TypeID            := NodeInfo.TypeID;
  end;

  // Any subnodes
  SetSubNodesProperties(ANode, TAddinOccNode(ANode.Data).TypeID);
end;  // SetNodeProperties

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.UpdateSubMenusForAddin(const ANode: TFlyNode; AMenus:
  Array of TMenuItem);
var
  lMnuIdx, lParentTypeID, lTypeID, i: Integer;
  lSeparatorAdded : Boolean;

  //----------------------------------------------------------------------------
  procedure AddItemToMenu(AMenu: TMenuItem; TypeID: Integer; ClickHandler: TNotifyEvent);
  var lItem: TMenuItem;
  begin
    lItem := TMenuItem.Create(nil);
    if TypeID = -1 then
      // Separator.
      lItem.Caption := '-'
    else begin
      // Normal menu item.
      lItem.Caption := FNodeMan.TypeName[TypeID];
      lItem.OnClick := ClickHandler;
    end;
    lItem.Tag := -1;
    AMenu.Add(lItem);
  end;
  //----------------------------------------------------------------------------

begin
  if Assigned(ANode) then begin
    // Initialise variables
    lMnuIdx := 0;
    lTypeID := -1;
    lParentTypeID := -1;

    if TNodeObject(ANode.Data) is TAddinLocNode then
      lParentTypeID := TAddinLocNode(ANode.Data).TypeID;

    // Loop until we're told there is nothing more
    while FNodeMan.GetAddType(lParentTypeID, lMnuIdx, lTypeID) do begin
      for i := 0 to High(AMenus) do AddItemToMenu(AMenus[i], lTypeID, AddinMenuClick);
      Inc(lMnuIdx);   // Increment index for next round
      lTypeID := -1;  // Reset to default value
    end;

    lMnuIdx := 0;
    lSeparatorAdded := False;
    // Now see if there are any items that would be added into subfolders below
    // the current node.
    while FNodeMan.GetAddSubType(lParentTypeID, lMnuIdx, lTypeID) do begin
      // Add a separator to "indicate" items won't appear on same level as
      // selected node. Do it only if there are items to add to menu.
      if not lSeparatorAdded then begin
        for i := 0 to High(AMenus) do AddItemToMenu(AMenus[i], -1, nil);
        lSeparatorAdded := True;
      end;

      // Different click handler
      for i := 0 to High(AMenus) do AddItemToMenu(AMenus[i], lTypeID, AddinSubMenuClick);
      Inc(lMnuIdx);   // Increment index for next round
      lTypeID := -1;  // Reset to default value
    end;
  end;
end;  // UpdateSubMenusForAddin

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.SetupAddinNode(const ATypeID: Integer; AParentNode,
  ANodeToRefresh: TFlyNode; AFields: ADODB_TLB.Fields);
var
  lNodeInfo: TNodeInfo;
  lNode : TFlyNode;
  lPopulated: Boolean;
begin
  InitializeNodeInfo(lNodeInfo);
  // Let addin setup stuff for display.
  // Need to cast Fields for call because ADOInt (used by ADODB) and ADODB_TLB
  // have different GUIDs for Fields interface. But it is the same!!!
  if FNodeMan.GetNodeInfo(ATypeID, AFields, lNodeInfo) then begin
    // Setup new TreeNode properties
    if ANodeToRefresh = nil then begin
      lNode := tvLocations.Items.AddChild(AParentNode, lNodeInfo.Caption);
      SetNodeProperties(lNode, lNodeInfo);
    end else
    // Check ItemKey to see if this is the node to refresh.
    if TNodeObject(ANodeToRefresh.Data).ItemKey = lNodeInfo.ItemKey then begin
      lPopulated := TNodeObject(ANodeToRefresh.Data).ChildrenPopulated;
      ANodeToRefresh.Caption := lNodeInfo.Caption;
      // Ensure the ChildrenPopulated flag remains the same, before calling SetNodeProperties.
      lNodeInfo.ChildrenPopulated := lPopulated;
      // Update other properties.
      SetNodeProperties(ANodeToRefresh, lNodeInfo);
    end else
      // NodeToRefresh doesn't match data from GetNodeData, so exit.
      Exit;
  end;
end;  // SetupAddinNode

{-------------------------------------------------------------------------------
}
function TfrmLocations.SetNodeInfo(ANode: TFlyNode): TNodeInfo;
var
  lData: TAddinLocNode;

  function AddinImageIndexFromMapping(RecImageIndex: String; IndexMap: TStringList): Integer;
  var i: Integer;
  begin
    Result := -1;
    for i := 0 to IndexMap.Count - 1 do
      if IndexMap.ValueFromIndex[i] = RecImageIndex then begin
        Result := StrToInt(IndexMap.Names[i]);
        Exit;
      end;
  end;

begin
  lData := TAddinLocNode(ANode.Data);
  with Result do begin
    Caption           := ANode.Text;
    ChildrenPopulated := lData.ChildrenPopulated;
    ImageIndex        := AddinImageIndexFromMapping(IntToStr(ANode.ImageIndex), FNodeImagesMap);
    SelectedIndex     := AddinImageIndexFromMapping(IntToStr(ANode.SelectedIndex), FNodeImagesMap);
    StateIndex        := -1;
    ItemKey           := lData.ItemKey;
    TypeID            := lData.TypeID;
    Editable          := True;
    Deletable         := True;
    TableName         := lData.ItemAdditional;
  end;
end;  // SetNodeInfo

{===============================================================================
  IRecorderDetailScreenEvents
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfrmLocations.OnEditModeChange;
begin
  SetMenuState(not (FDetailScreen.ControlInterface as IRecorderDetailScreen).Editing);
end;  // OnEditModeChange

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.OnRefreshScreenInfo(const AKey: WideString; const ACaption: WideString);
var lCmd: TADOCommand;

  procedure RefreshNode(AParentNode, ANode: TFlyNode);
  var lParentTypeID, lTypeID: Integer;
      lSQL: WideString;
  begin
    // Get the type of the parent node, -1 for features
    if AParentNode.ImageIndex = FEATURE_IMAGE_INDEX then
      lParentTypeID := -1
    else
      lParentTypeID := TAddinLocNode(AParentNode.Data).TypeID;

    // Store node type for easier use.
    lTypeID := TAddinLocNode(ANode.Data).TypeID;
    // Ask addin if there is anything to do.
    if FNodeMan.GetData(lParentTypeID, lTypeID, TNodeObject(AParentNode.Data).ItemKey, lSQL) then
    begin
      // If addin says yes, do it.
      lCmd.CommandText := lSQL;
      with lCmd.Execute do begin
        // Go through recordset and only do stuff on the node we want to refresh.
        while not Eof do begin
          SetupAddinNode(lTypeID, AParentNode, ANode, ADODB_TLB.Fields(Fields));
          MoveNext;
        end;
        Close;
      end;
    end;
    // Go up to refresh parent node too, in case child node had an effect that
    // should be reflected in the tree. Stop at Samples.
    if lParentTypeID <> -1 then RefreshNode(AParentNode.Parent, AParentNode);
  end;

begin
  // Key is empty if adding a new node was cancelled.
  if AKey = '' then begin
    KillChildren(SelectedItem);
    SendMessage(Self.Handle, WM_DISCARD_LOCATION, 0, 0);
    Exit;
  end else
    // Update ItemKey in Data object. Or refresh won't work properly.
    TNodeObject(SelectedItem.Data).ItemKey := AKey;

  lCmd := TADOCommand.Create(nil);
  try
    lCmd.Connection := dmDatabase.dbLocal;
    RefreshNode(SelectedItem.Parent, SelectedItem);
  finally
    lCmd.Free;
  end;
end;  // OnRefreshScreenInfo
    
{-------------------------------------------------------------------------------
}
function TfrmLocations.GetTreeView: TRapidTree;
begin
  Result := tvLocations;
end;
 
{-------------------------------------------------------------------------------
  Ensure highlighted nodes look different to other nodes.
}
procedure TfrmLocations.tvLocationsDrawCell(Sender: TObject;
  aCanvas: TCanvas; ACol, ARow: Integer; Rect: TRect;
  State: TExGridDrawState);
var
  lNode: TFlyNode;
  lTree: TRapidTree;
  lRect: TRect;
  caption: string;
begin
  inherited;
  if csDestroying in ComponentState then
    exit;
  lTree := (Sender as TRapidTree);
  lNode := lTree.GetNodeAtRow(ARow);
  caption := lNode.Caption;
  if Assigned(lNode.Data) and (TObject(lNode.Data) is TSiteNode) then begin
    if AppSettings.IncludeLocationFileCode and (TSiteNode(lNode.Data).FileCode<>'') then
      caption :=caption + ' - ' + TSiteNode(lNode.Data).FileCode;
    if AppSettings.IncludeLocationSpatialRef and (TSiteNode(lNode.Data).SpatialRef<>'') then
      caption :=caption + ' - ' + TSiteNode(lNode.Data).SpatialRef;
  end;

  if Assigned(lNode) then begin
    // Selected nodes always look the same. Non-selected highlighted nodes are different.
    if lNode.Selected then
      ACanvas.Font.Color := clHighlightText
    else
      dmGeneralData.SetCanvasColourForFilterNode(ACanvas, TNodeObject(lNode.Data).IsFiltered);
    lRect := Rect;
    // Some node types are indented more than others.
    if TNodeObject(lNode.Data) is TAddinLocNode then
      lRect.Left := Rect.Left + lTree.Indent * (lNode.Level + 2 + Ord(lNode.StateIndex <> -1))
    else
      lRect.Left := Rect.Left + lTree.Indent * (lNode.Level + 1 + Ord(lNode.StateIndex <> -1));
    ACanvas.FillRect(lRect);
    ACanvas.TextOut(lRect.Left + 4, lRect.Top + 1, caption);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.DoShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo:
    THintInfo);
var
  X, Y: Integer;
  lNode: TFlyNode;
begin
  if (HintInfo.HintControl = tvLocations) then begin
    X:=(Mouse.CursorPos.X)-(ClientOrigin.X) - tvLocations.Left;
    Y:=(Mouse.CursorPos.Y)-(ClientOrigin.Y) - tvLocations.Top;
    lNode:=tvLocations.GetNodeAt(X, Y);
    if lNode <> nil then
      HintStr := TNodeObject(lNode.Data).Hint;
    CanShow := HintStr<>'';
    if CanShow then
      HintInfo.ReshowTimeout := 200;
  end else
    // Show hint as normal
    inherited;
end;  // TfrmLocations.DoShowHint

{-------------------------------------------------------------------------------
}
procedure TfrmLocations.NotifyDataItemChange;
begin
  if FSelectedItem = nil then Exit;
  DoFormEventAddinChanges(
      TNodeObject(FSelectedItem.Data).ItemAdditional,
      TNodeObject(FSelectedItem.Data).ItemKey);
end;
            
{-------------------------------------------------------------------------------
}
procedure TfrmLocations.pmHBatchUpdateClick(Sender: TObject);
begin
  inherited;
  BringToFront; // in case popup activated when not the active mdi child, which can happen
  frmMain.PopulateBatchUpdateSubMenu(tvLocations.Selected, pmHBatchUpdate);
end;
           
{-------------------------------------------------------------------------------
}
procedure TfrmLocations.ApplySecurity;
begin      
  UpdateSubMenus(tvLocations.Selected);
  SetMenuState(True);
  pmHAdd.Enabled := (AppSettings.UserAccessLevel >= ualAddOnly);
  pmHBatchUpdate.Visible := AppSettings.UserAccessLevel >= ualAdmin;
end;

end.

