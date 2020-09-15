//==============================================================================
//  Unit:        TaxonDictBrowser
//
//  Implements:  TfrmTaxonDictBrowser
//
//  Description: Implements the Taxon dictionary browser. The right side
//               panel displays the details about the taxon selected in
//               the tree, in HTML format.
//
//  Author:      Eric Salmon
//  Created:     10 Dec 2001
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 33 $
//    $Date: 17/07/09 14:26 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit TaxonDictBrowser;

interface
                                                        
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseTaxonDictUnit, Htmlview, ComCtrls, Menus, ActnList, ExGrid, RapTree,
  StdCtrls, ExtCtrls, MetaDataPopup, Constants, OnlineHelp, HierarchyNodes,
  TaxonDictBrowserData, HTMLDisplayFuncs, DictionaryHTMLDetails, Variants,
  GeneralFunctions, Recorder2000_TLB, SQLConstants, KeyboardRapidTree, StrUtils;

type
  TfrmTaxonDictBrowser = class(TBaseTaxonDict, IProvidesOccurrencesSQL)
    tsList: TTabSheet;
    lvTaxonList: TListView;
    hvTaxonDetails: THTMLViewer;
    mnuEditReturnData: TMenuItem;
    actShowMetaData: TAction;
    actShowItemDetails: TAction;
    mnuHReport: TMenuItem;
    mnuEditShowMetadata: TMenuItem;
    mnuEditShowDetails: TMenuItem;
    mnuOccurrencesForPlacesReport: TMenuItem;
    pmHQuickReports: TMenuItem;
    pmHBatchUpdate: TMenuItem;
    mnuPlaceHolder: TMenuItem;
    procedure actShowMetaDataExecute(Sender: TObject);
    procedure actShowItemDetailsExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvTaxonListColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvTaxonListCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lvTaxonListDblClick(Sender: TObject);
    procedure lvTaxonListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure pcBrowserChanging(Sender: TObject; var AllowChange: Boolean);
    procedure hvTaxonDetailsHotSpotClick(Sender: TObject;
      const SRC: String; var Handled: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure hvTaxonDetailsHotSpotCovered(Sender: TObject;
      const SRC: String);
    procedure mnuHReportClick(Sender: TObject);
    procedure pmHQuickReportsClick(Sender: TObject);
    procedure pmHBatchUpdateClick(Sender: TObject);
  private
    FTaxonListSort: String;
    FSortDirection: Integer;
    FKeyOfShowingItem : string;  // track item currently in html view
    FDisplayingDetails : boolean; // true whilst in the WMShowDetails handler
    procedure SortTaxonList(const ASortOrder: String);
    procedure WMRefreshColours(var Msg: TMessage); message WM_REFRESH_COLOURS;
  protected
    procedure ClearBrowser; override;
    procedure PopulateDetails; override;
    procedure SetupDestinationControls; override;
    procedure SetupHelp; override;
    procedure SetupObjects; override;
    procedure ShowDetails(const Reveal: Boolean); override;
    procedure SortDictionaryAdditional; override;
    procedure UpdateListKeySetting; override;
    procedure UpdateMenus; override;
    // IProvidesOccurrencesSQL
    function Get_CanProvideSQL: WordBool; safecall;
    function Get_OccurrencesSQL(const ATypes: WideString): WideString; safecall;
  public
    constructor Create(AOwner : TComponent); override;     
    procedure ApplySecurity; override;
    procedure PrintScreen; override;    // method called by file/print facility
    procedure UpdateDictionary;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, FormActions, Maintbar, GeneralData, Rucksack, DatabaseAccessAdo;

resourcestring
  ResStr_HideDictItemDetails  = 'Hide Dictionary Item Details';
  ResStr_ShowDictItemDetails  = 'Show Dictionary Item Details';
  ResStr_MetadataForTaxonList = 'Metadata for Taxon List: ';
  ResStr_MetadataForTaxon     = 'Metadata for Taxon: ';
  ResStr_TaxonListType        = 'Taxon List Type';
  ResStr_TaxonListKey         = 'Taxon List Key';
  ResStr_TaxonListVersionKey  = 'Taxon List Version Key';
  ResStr_TaxonKey             = 'Taxon Key';
  ResStr_TaxonVersionKey      = 'Taxon Version Key';
  ResStr_TaxonListItemKey     = 'Taxon List Item Key';
  ResStr_Description          = 'Description';
  ResStr_UpdateMethod         = 'Update Method';
  ResStr_InstalledLocally     = 'Installed Locally';
  ResStr_Yes                  = 'Yes';
  ResStr_No                   = 'No';
  ResStr_VersionAuthority     = 'Version Authority';
  ResStr_VersionAppliesFrom   = 'Version Applies From';
  ResStr_VersionQuality       = 'Version Quality';
  ResStr_Recommended_TLI_KEY  = 'Recommended Taxon List Item Key';
  ResStr_Recommended_TV_KEY   = 'Recommended Taxon Version Key';
  ResStr_Organism_Key         = 'Organism_Key';
  Restr_Recommended_Name      = 'Recommended_Name';
  ResStr_Recommended_Common_name = 'Recommended_Common_Name';
  ResStr_Recommended_List = 'Recommended_List';
  ResStr_Recommended_Sort_Order = 'Recommended_Sort_Order';
  ResStr_TaxonGroup           = 'Taxon Group Name';
  ResStr_Status               = 'Status';
//==============================================================================
procedure TfrmTaxonDictBrowser.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(true);
  frmMain.SetContextToolbar(Self, mnuEdit, 0, [nil, nil, nil, nil, pmSort, nil, nil]);
  dmFormActions.actPrint.Enabled := tvDictionary.Items.Count>0;
  dmFormActions.actPlacesForOccurrencesReport.Enabled := True;
  dmFormActions.actOccurrencesForPlacesReport.Enabled := True;
  UpdateMenus;
end;  // FormActivate

//==============================================================================
procedure TfrmTaxonDictBrowser.FormDeactivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(false);
  dmFormActions.actPrint.Enabled := false;
  dmFormActions.actPlacesForOccurrencesReport.Enabled := false;
  dmFormActions.actOccurrencesForPlacesReport.Enabled := false;
end;  // FormDeactivate

//==============================================================================
procedure TfrmTaxonDictBrowser.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  FormDeactivate(Sender);
  AppSettings.TaxonViewerVisible := pnlDetails.Visible;
end;  // FormClose;

//==============================================================================
procedure TfrmTaxonDictBrowser.SetupObjects;
begin
  DictionaryData := TdmTaxonDictBrowser.Create(nil);
  TdmTaxonDictBrowser(DictionaryData).ListView := lvTaxonList;
  pcBrowser.ActivePage := tsTree;
end;  // SetupObjects

//==============================================================================
procedure TfrmTaxonDictBrowser.ClearBrowser;
begin
  inherited ClearBrowser;
  lvTaxonList.Items.BeginUpdate;
  try
    lvTaxonList.Items.Clear;
  finally
    lvTaxonList.Items.EndUpdate;
  end;
end;  // ClearBrowser

//==============================================================================
procedure TfrmTaxonDictBrowser.SetupHelp;
begin
  mnuEdit.HelpContext:= IDH_EDITMENU;
  Self.HelpContext   := IDH_TAXONDICT;
end;  // SetupHelp

//==============================================================================
procedure TfrmTaxonDictBrowser.UpdateMenus;
var ltfOn: Boolean;
begin
  with tvDictionary do begin
    if Items.Count=0 then PopupMenu := nil
                     else PopupMenu := pmHierarchy;
    ltfOn := PopupMenu <> nil;
  end;
  actFind.Enabled     := ltfOn;
  actFilter.Enabled   := ltfOn;
  mnuEditSort.Enabled := ltfOn;
  actShowMetadata.Enabled := ltfOn;
  EnableSortToolbutton(ltfOn, pmSort);
end;  // UpdateMenus

//==============================================================================
procedure TfrmTaxonDictBrowser.ShowDetails(const Reveal: Boolean);
begin
  inherited ShowDetails(Reveal);
  if Reveal then begin
    actShowItemDetails.ImageIndex:=56;
    actShowItemDetails.Caption := ResStr_Cap_HideItemDetails;
    actShowItemDetails.Hint := ResStr_HideDictItemDetails;
  end else begin
    actShowItemDetails.ImageIndex:=55;
    actShowItemDetails.Caption := ResStr_Cap_ShowItemDetails;
    actShowItemDetails.Hint := ResStr_ShowDictItemDetails;
  end;
end;  // ShowDetails

//==============================================================================
procedure TfrmTaxonDictBrowser.actShowMetaDataExecute(Sender: TObject);
var
  header, body: String;
  changedby: String;
  redundant: String;
begin
  inherited;
  header := '';
  body   := '';

  // Display metadata for list if control is focused, otherwise it's for a taxon.
  header := ResStr_MetadataForTaxonList + cmbList.Text;
  with dmDatabase.GetRecordset(
      'usp_TaxonList_Select_ForMetadata', ['@Key', ListKeyData.ItemKey]) do
    if not Eof then
    begin
      body := MetaDataPaneItem(ResStr_TaxonListType, Fields['Long_Name'].Value)
          + MetaDataPaneItem(ResStr_TaxonListKey, ListKeyData.ItemKey)
          + MetaDataPaneItem(ResStr_TaxonListVersionKey, Fields['Taxon_List_Version_Key'].Value);
      if (VarToStr(Fields['Description'].Value) <> '') then
        body := body + MetaDataPaneItem(ResStr_Description, Fields['Description'].Value);
      if (VarToStr(Fields['Update_Mechanism'].Value) <> '') then
        body := body + MetaDataPaneItem(ResStr_UpdateMethod, Fields['Update_Mechanism'].Value);
      body := body
          + MetaDataPaneItem(
              ResStr_InstalledLocally,
              IfThen(Fields['Local_Disk'].Value, ResStr_Yes, ResStr_No));
      body := body + MetaDataPaneItem('Version', VarToStr(Fields['Version'].Value));
      if (VarToStr(Fields['Authority'].Value) <> '') then
        body := body + MetaDataPaneItem(ResStr_VersionAuthority, Fields['Authority'].Value);
      if (VarToStr(Fields['Vague_Date_Start'].Value) <> '') then
        body := body + MetaDataPaneItem(ResStr_VersionAppliesFrom, DateToStr(Fields['Vague_Date_Start'].Value));
      if (VarToStr(Fields['Quality'].Value) <> '') then
        body := body + MetaDataPaneItem(ResStr_VersionQuality, Fields['Quality'].Value);
    end;

  if (ActiveControl <> cmbList) and Assigned(tvDictionary.Selected) then
  begin
    with dmDatabase.GetRecordset('usp_Taxon_Select_ForMetadata',
        ['@Key', TTaxonDictionaryNode(tvDictionary.Selected.Data).ItemKey]) do
    begin
      // Override header if item in list or dictionary has focus.
      header := ResStr_MetadataForTaxon + Fields['Item_Name'].Value;

      If VarIsNull(Fields['Changed_By'].Value) then
        changedBy := ''
      else begin
        // Key could potentially be one not in database.
        changedBy := dmGeneralData.GetIndividualName(Fields['Changed_By'].Value);
        if changedBy <> '' then
          changedBy := Format(ResStr_LastChanged,
              [changedBy, DateToStr(Fields['Changed_Date'].Value)]);
      end;
      If VarIsNull(Fields['Redundant_Flag'].Value) then
        redundant := 'Valid taxa'
      else
        redundant := 'This taxa is not considered valid';

      body   := '<P>'
          + Format(ResStr_RecordCreation,
              [dmGeneralData.GetIndividualName(Fields['Entered_By'].Value),
               DateToStr(Fields['Entry_Date'].Value)])
          + changedBy
          + '.</P>'
          + MetadataPaneItem(ResStr_Status, redundant)
          + MetadataPaneItem(ResStr_TaxonKey, Fields['Taxon_Key'].Value)
          + MetadataPaneItem(ResStr_TaxonVersionKey, Fields['Taxon_Version_Key'].Value)
          + MetadataPaneItem(ResStr_TaxonListItemKey, Fields['Taxon_List_Item_Key'].Value)
          + MetadataPaneItem(ResStr_TaxonGroup, Fields['Taxon_Group_Name'].Value)
          + MetaDataPaneItem(ResStr_Recommended_TLI_KEY, Fields['Recommended_TLI_Key'].Value)
          + MetaDataPaneItem(ResStr_Recommended_TV_KEY, Fields['Recommended_TV_Key'].Value)
          + MetaDataPaneItem(ResStr_Organism_Key, Fields['Organism_Key'].Value)
          + MetaDataPaneItem(Restr_Recommended_Name, Fields['Recommended_Name'].Value)
          + MetaDataPaneItem(ResStr_Recommended_Common_name, Fields['Recommended_Common_Name'].Value)
          + MetaDataPaneItem(ResStr_Recommended_List, Fields['Recommended_List'].Value)
          + MetaDataPaneItem(ResStr_Recommended_Sort_Order, Fields['Recommended_Sort_Order'].Value)
          + '<hr>'
          + body;
    end;
  end;

  if (header <> '') then
    with TdlgMetaDataPopup.Create(nil) do
      try
        ShowCustom(header, body);
      finally
        Free;
      end; // try..finally
end;  // actShowMetaDataExecute

//==============================================================================
procedure TfrmTaxonDictBrowser.actShowItemDetailsExecute(Sender: TObject);
begin
  inherited;
  if pnlDetails.Visible then
    ShowDetails(false)
  else begin
    ShowDetails(true);
    PostMessage(Handle, WM_SHOW_DETAILS, 0, 0);
  end;
end;  // actShowItemDetailsExecute

//==============================================================================
procedure TfrmTaxonDictBrowser.WMRefreshColours(var Msg: TMessage);
begin
  Repaint;
end;  // WMRefreshColours

//==============================================================================
procedure TfrmTaxonDictBrowser.PopulateDetails;
var lCursor: TCursor;
begin
  if not pnlDetails.Visible then Exit;

  if not Assigned(tvDictionary.Selected) then
  begin
    FKeyOfShowingItem := '';
    hvTaxonDetails.Clear;
  end else
  // only bother if not already showing same item's HTML
  if (FKeyOfShowingItem<>THTMLDictionaryNode(tvDictionary.Selected.Data).ItemKey) then
  begin
    lCursor := HourglassCursor;
    FDisplayingDetails := true;
    try
      //Add details to the HTML window
      if not (csDestroying in ComponentState) then begin
        FKeyOfShowingItem := THTMLDictionaryNode(tvDictionary.Selected.Data).ItemKey;
        TdmTaxonDictBrowser(DictionaryData).SetHTMLDetails(
            THTMLDictionaryNode(tvDictionary.Selected.Data),
            ListKeyData.ItemKey,
            hvTaxonDetails);
      end;
    finally
      DefaultCursor(lCursor);
      FDisplayingDetails := False;
    end; // try
  end; // if
end;  // PopulateDetails

//==============================================================================
procedure TfrmTaxonDictBrowser.lvTaxonListColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  inherited;
  if Column.Index = 0 then SortTaxonList(SCI_NAME_SORT)
                      else SortTaxonList(COMMON_NAME_SORT);
end;  // lvTaxonListColumnClick

//==============================================================================
procedure TfrmTaxonDictBrowser.SortTaxonList(const ASortOrder: String);
var lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    // Switch sort from ascending to descending or back if same column clicked
    if ASortOrder = FTaxonListSort then FSortDirection := FSortDirection * -1
                                   else FSortDirection := 1;
    FTaxonListSort := ASortOrder;
    lvTaxonList.SortType := stData;
  finally
    DefaultCursor(lCursor);
    lvTaxonList.SortType := stNone;
  end;
end;  // SortTaxonList

//==============================================================================
procedure TfrmTaxonDictBrowser.lvTaxonListCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  inherited;
  Compare := FSortDirection * CompareDictionaryItems(Item1.Data, Item2.Data, FTaxonListSort);
end;  // lvTaxonListCompare

//==============================================================================
procedure TfrmTaxonDictBrowser.SortDictionaryAdditional;
begin
  inherited;
  SortTaxonList(SortOrder);
end;  // SortDictionaryAdditional

//==============================================================================
procedure TfrmTaxonDictBrowser.SetupDestinationControls;
begin
  inherited;
  RegisterDragComponent(lvTaxonList, GetDictionaryNodeData);
end;  // SetupDestinationControls

//==============================================================================
procedure TfrmTaxonDictBrowser.UpdateListKeySetting;
begin
  AppSettings.TaxonListKey := ListKeyData.ItemKey;
end;  // UpdateListKeySetting

//==============================================================================
procedure TfrmTaxonDictBrowser.lvTaxonListDblClick(Sender: TObject);
var
  rucksack: TForm;
begin
  inherited;
  if  Assigned(lvTaxonList.ItemFocused) then begin
    rucksack := frmMain.GetForm(TfrmRucksack);
    if rucksack <> nil then
     TfrmRucksack(rucksack).AddTaxonToRucksack(TDictionaryNode(lvTaxonList.Selected.Data).ItemKey);
  end;
end;  // lvTaxonListDblClick

//==============================================================================
procedure TfrmTaxonDictBrowser.lvTaxonListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  inherited;
  if lvTaxonList.Selected <> nil then
  begin
    LocateDictionaryItem(TDictionaryNode(Item.Data).ItemKey, 'Taxon');
    PostMessage(Handle, WM_SHOW_DETAILS, 0, 0);
    // Now notify whoever is interested
    NotifyDataItemChange;
  end;
end;  // lvTaxonListSelectItem

//==============================================================================
procedure TfrmTaxonDictBrowser.PrintScreen;
begin
  if tvDictionary.Selected <> nil then hvTaxonDetails.Print(1, 9999);
end;  // PrintScreen

//==============================================================================
procedure TfrmTaxonDictBrowser.UpdateDictionary;
begin
  tvDictionary.Selected := nil;
  cmbListChange(nil);
end;  // UpdateDictionary

//==============================================================================
procedure TfrmTaxonDictBrowser.pcBrowserChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  inherited;
  // Switch to reverse order
  FSortDirection := FSortDirection * -1;
  // Then re-order on same field. Will restore current sort order
  SortTaxonList(FTaxonListSort);
end;  // pcBrowserChanging

//==============================================================================
procedure TfrmTaxonDictBrowser.hvTaxonDetailsHotSpotClick(Sender: TObject;
  const SRC: String; var Handled: Boolean);
begin
  inherited;
  if Pos(HEADING_DENOTE_STRING, SRC) <> 0 then
    // Move to show main details
    hvTaxonDetails.PositionTo(HEADING_DENOTE_STRING)
  else
  if SRC[1] = CHECKLIST_DENOTE_CHAR then
    // A checklist name has been clicked on
    TdmTaxonDictBrowser(DictionaryData).ShowCheckListDetails(Copy(SRC, 2, 32), hvTaxonDetails)
  else
    // Some other source has been clicked on
    ShowSource(SRC);
end;  // hvTaxonDetailsHotSpotClick

//==============================================================================
constructor TfrmTaxonDictBrowser.Create(AOwner: TComponent);
begin
  inherited;
  FKeyOfShowingItem := '';  // nothing in HTML pane yet
  FDisplayingDetails := False;
  FTaxonListSort := DEFAULT_SORT;
  FSortDirection := 1;
  if AppSettings.TaxonViewerVisible then ShowDetails(true)
                                    else ShowDetails(false); 
  pmHBatchUpdate.Visible := AppSettings.UserAccessLevel >= ualAdmin;
end;

//==============================================================================
{ Prevent the closure of the screen while it is still loading }
procedure TfrmTaxonDictBrowser.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  if FDisplayingDetails then
    CanClose := False;
end;

//==============================================================================
procedure TfrmTaxonDictBrowser.hvTaxonDetailsHotSpotCovered(
  Sender: TObject; const SRC: String);
begin
  inherited;
  if SRC='' then
     frmMain.SetStatus('')
  else if (Copy(SRC, 1, 1) = ST_SOURCE_DENOT_CHAR) or (Pos(HEADING_DENOTE_STRING, SRC) <> 0) or (SRC[1] = CHECKLIST_DENOTE_CHAR) then
     frmMain.SetStatus('')
  else
     frmMain.SetStatus(SRC);
end;

//==============================================================================
procedure TfrmTaxonDictBrowser.mnuHReportClick(Sender: TObject);
begin
  inherited;

end;

{-------------------------------------------------------------------------------
  Description : Populates the list of available reports in the popup
  Created : 11/03/2003 }
procedure TfrmTaxonDictBrowser.pmHQuickReportsClick(Sender: TObject);
begin
  inherited;
  BringToFront; // in case popup activated when not the active mdi child, which can happen
  frmMain.PopulateQuickReportSubMenu(tvDictionary.Selected, pmHQuickReports);
end;

{-------------------------------------------------------------------------------
  Description : Return true if we have a selected item that we can report on
  Created : 11/03/2003 }
function TfrmTaxonDictBrowser.Get_CanProvideSQL: WordBool;
begin
  Result := FKeyOfShowingItem<>'';
end;

{-------------------------------------------------------------------------------
  Description : SQL to list occurrences for the selected taxon.  Used for the
              Quick Reports
  Created : 11/03/2003 }
function TfrmTaxonDictBrowser.Get_OccurrencesSQL(const ATypes: WideString): WideString;
begin
  if Pos('T', ATypes)>0 then begin
    Result :=
          'SELECT DISTINCT TOCC.TAXON_OCCURRENCE_KEY AS OCCURRENCE_KEY, ''T'' AS TYPE ';
    // Either use the Name_Server (Recommended_Taxon_List_Item_Key) or Index_Taxon_Synonym to
    // resolve synonyms. Also expand down the taxonomic groups.
    if AppSettings.UseRecommendedTaxaNames then
      Result := Result +
          'FROM Index_Taxon_Name AS orig '+
          'INNER JOIN Index_Taxon_Name AS itn2 ON itn2.recommended_taxon_list_item_key = orig.recommended_taxon_list_item_key '+
          'INNER JOIN Index_Taxon_Group AS itg ON itg.taxon_list_item_key = itn2.taxon_list_item_key '+
          'INNER JOIN Index_Taxon_Name AS itn3 ON itn3.taxon_list_item_key = itg.contained_list_item_key ' +
          'INNER JOIN Index_Taxon_Name AS taxfilter ON taxfilter.recommended_taxon_list_item_key = itn3.recommended_taxon_list_item_key '
    else
      Result := Result +
          'FROM Index_Taxon_Synonym AS orig ' +
          'INNER JOIN Index_Taxon_Group AS itg ON itg.taxon_list_item_key = orig.synonym_list_item_key '+
          'INNER JOIN Index_Taxon_Synonym taxfilter ON taxfilter.synonym_list_item_key = itg.contained_list_item_key ';
    Result := Result +
          'INNER JOIN TAXON_DETERMINATION TD ON TD.TAXON_LIST_ITEM_KEY=taxfilter.TAXON_LIST_ITEM_KEY AND td.preferred=1 '+
          'INNER JOIN TAXON_OCCURRENCE TOCC ON TOCC.TAXON_OCCURRENCE_KEY=TD.TAXON_OCCURRENCE_KEY '+
          'WHERE orig.TAXON_LIST_ITEM_KEY=''' + FKeyOfShowingItem + ''' ' + SQL_TAXON_RESTRICTION;
  end
  else
    Result := ''; // cannot run biotope report
end;

{-------------------------------------------------------------------------------
}
procedure TfrmTaxonDictBrowser.pmHBatchUpdateClick(Sender: TObject);
begin
  inherited;
  frmMain.PopulateBatchUpdateSubMenu(tvDictionary.Selected, pmHBatchUpdate);
end;      
    
{-------------------------------------------------------------------------------
}
procedure TfrmTaxonDictBrowser.ApplySecurity;
begin
  pmHBatchUpdate.Visible := AppSettings.UserAccessLevel >= ualAdmin;
end;

end.
