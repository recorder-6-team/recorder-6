//==============================================================================
//  Unit:        AdminAreaDictBrowser
//
//  Implements:  TfrmAdminAreaDictBrowser
//
//  Description: Implements the Admin Area dictionary browser. The right side
//               panel displays the details about the admin area selected in
//               the tree, in HTML format.
//
//  Author:      Eric Salmon
//  Created:     23 Nov 2001
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 26 $
//    $Date: 5/02/08 16:22 $
//    $Author: Johnvanbreda $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit AdminAreaDictBrowser;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseDictionaryUnit, ActnList, Menus, exgrid, RapTree, ComCtrls, StdCtrls,
  ExtCtrls, Constants, OnlineHelp, AdminAreaDictBrowserData, DataClasses,
  ExceptionForm, HierarchyNodes, MetaDataPopup, Htmlview, DictionaryHTMLDetails,
  GeneralFunctions, DatabaseAccessADO, TaxonDictEditor, KeyboardRapidTree;

type
  TfrmAdminAreaDictBrowser = class(TBaseDictionary)
    mnuEdit: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditTransferData: TMenuItem;
    N1: TMenuItem;
    mnuEditFind: TMenuItem;
    mnuEditSort: TMenuItem;
    mnuEditSortCode: TMenuItem;
    mnuEditSortName: TMenuItem;
    mnuEditFilter: TMenuItem;
    mnuEditShowMetadata: TMenuItem;
    mnuEditShowItemDetails: TMenuItem;
    pmHierarchy: TPopupMenu;
    pmHSortBy: TMenuItem;
    pmHSortCode: TMenuItem;
    pmHSortName: TMenuItem;
    pmSort: TPopupMenu;
    pmSortCode: TMenuItem;
    pmSortName: TMenuItem;
    alAdminArea: TActionList;
    actSortCode: TAction;
    actSortName: TAction;
    actFind: TAction;
    actFilter: TAction;
    actShowMetadata: TAction;
    actShowItemDetails: TAction;
    pmHQuickReports: TMenuItem;
    mnuDummy: TMenuItem; 
    hvAdminAreaDetails: THTMLViewer;
    pmHBatchUpdate: TMenuItem;
    mnuPlaceHolder: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure actFilterExecute(Sender: TObject);
    procedure actShowItemDetailsExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSortCodeExecute(Sender: TObject);
    procedure actSortNameExecute(Sender: TObject);
    procedure actShowMetadataExecute(Sender: TObject);
    procedure hvAdminAreaDetailsHotSpotClick(Sender: TObject;
      const SRC: String; var Handled: Boolean);
    procedure pmHQuickReportsClick(Sender: TObject);
    procedure pmHBatchUpdateClick(Sender: TObject);
  private
    procedure WMRefreshColours(var Msg: TMessage); message WM_REFRESH_COLOURS;
  protected
    function CompareDictionaryItems(const Data1, Data2: TDictionaryNode; const ASortOrder: String): Integer; override;
    function GetListForItem(const AKey: string): string; override;
    procedure PopulateDetails; override;
    procedure SetupHelp; override;
    procedure SetupObjects; override;
    procedure ShowDetails(const Reveal: Boolean); override;
    procedure UpdateMenus; override;
    procedure UpdateListKeySetting; override;
    function DefaultSortField: string; override;
    function SortRegistryName: string; override;
    function SortNameToSortSQL(const ASortName: string): string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySecurity; override;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, Maintbar, GeneralData, FormActions, Find, Variants,
  BaseADODataModule;

const
  SHORT_CODE = 'Short_Code';
  ITEM_NAME  = 'Item_Name';

resourcestring
  ResStr_MetadataForAdminType = 'Metadata for Admin Area Type: ';
  ResStr_DateTo = 'Date To:';
  ResStr_DateFrom = 'Date From:';
  ResStr_To = 'To:';
  ResStr_HideItemDetails = 'Hide &Item Details';
  ResStr_HideDictionaryItemDetails = 'Hide Dictionary Item Details';
  ResStr_ShowItemDetails = 'Show &Item Details';
  ResStr_ShowDictionaryItemDetails = 'Show Dictionary Item Details';
  ResStr_FindAdminArea =  'Find Admin Area';
  ResStr_AdministrativeArea = 'administrative area';
  ResStr_DateLastChanged =  'Date Last Changed';
  ResStr_DateOfEntry =  'Date of Entry';
  ResStr_AdministrativeAreas = 'administrative areas';


//==============================================================================
procedure TfrmAdminAreaDictBrowser.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(true);
  frmMain.SetContextToolbar(Self, mnuEdit, 0, [nil, nil, nil, nil, pmSort, nil, nil]);
  UpdateMenus;
end;  // FormActivate

//==============================================================================
procedure TfrmAdminAreaDictBrowser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  AppSettings.AdminAreaViewerVisible := pnlDetails.Visible;
end;  // FormClose

//==============================================================================
procedure TfrmAdminAreaDictBrowser.SetupObjects;
begin
  DictionaryData := TdmAdminAreaDictBrowser.Create(nil);
end;  // SetupObjects

//==============================================================================
procedure TfrmAdminAreaDictBrowser.SetupHelp;
begin
  mnuEdit.HelpContext:= IDH_EDITMENU;
  Self.HelpContext   := IDH_AADICT;
end;  // SetupHelp

//==============================================================================
procedure TfrmAdminAreaDictBrowser.UpdateMenus;
var tfOn:boolean;
begin
  with tvDictionary do begin
    if Items.Count = 0 then PopupMenu := nil
                       else PopupMenu := pmHierarchy;
    tfOn := PopupMenu <> nil;
  end;
  actFind.Enabled     := tfOn;
  actFilter.Enabled   := tfOn;
  mnuEditSort.Enabled := tfOn;
  actShowMetadata.Enabled := tfOn;
  EnableSortToolbutton(tfOn, pmSort);
end;  // UpdateMenus

//==============================================================================
procedure TfrmAdminAreaDictBrowser.UpdateListKeySetting;
begin
  AppSettings.AdminAreaListKey := ListKeyData.ItemKey;
end;  // UpdateListKeySetting

//==============================================================================
procedure TfrmAdminAreaDictBrowser.actFindExecute(Sender: TObject);
begin
  inherited;
  LocateDictionaryItem(FindDictionaryItem('', ResStr_FindAdminArea, ftAdminArea), ResStr_AdministrativeArea);
end;  // actFindExecute

//==============================================================================
procedure TfrmAdminAreaDictBrowser.actFilterExecute(Sender: TObject);
begin
  inherited;
  // Note we don't filter by the admin_type_key here, because an admin area hierarchy
  // can contain several different types.
  FilterDictionary('Admin_Area', ResStr_AdministrativeAreas, '');
end;  // actFilterExecute

//==============================================================================
procedure TfrmAdminAreaDictBrowser.ShowDetails(const Reveal: Boolean);
begin
  inherited ShowDetails(Reveal);
  if Reveal then begin
    actShowItemDetails.ImageIndex := 56;
    actShowItemDetails.Caption := ResStr_HideItemDetails;
    actShowItemDetails.Hint := ResStr_HideDictionaryItemDetails;
  end else begin
    actShowItemDetails.ImageIndex := 55;
    actShowItemDetails.Caption := ResStr_ShowItemDetails;
    actShowItemDetails.Hint := ResStr_ShowDictionaryItemDetails;
  end;
end;  // ShowDetails

//==============================================================================
procedure TfrmAdminAreaDictBrowser.actShowItemDetailsExecute(Sender: TObject);
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
procedure TfrmAdminAreaDictBrowser.actSortCodeExecute(Sender: TObject);
begin
  SortOrder := SHORT_CODE;
end;  // actSortCodeExecute

//==============================================================================
procedure TfrmAdminAreaDictBrowser.actSortNameExecute(Sender: TObject);
begin
  SortOrder := ITEM_NAME;
end;  // actSortNameExecute

//==============================================================================
function TfrmAdminAreaDictBrowser.CompareDictionaryItems(const Data1, Data2: TDictionaryNode;
  const ASortOrder: String): Integer;
var lsItem1, lsItem2: String;
begin
  Result := 0;
  if Assigned(Data1) and Assigned(Data2) then begin
    if ASortOrder = SHORT_CODE then begin
      lsItem1 := TAdminAreaDictionaryNode(Data1).ShortCode;
      lsItem2 := TAdminAreaDictionaryNode(Data2).ShortCode;
    end else begin
      lsItem1 := TAdminAreaDictionaryNode(Data1).ItemName;
      lsItem2 := TAdminAreaDictionaryNode(Data2).ItemName;
    end;

    if (lsItem1 = '') and (lsItem2 = '') then
      Result := -1  // Using this leaves the order unchanged, apparently
    else
      Result := CompareText(lsItem1, lsItem2);
  end;
end;  // CompareDictionaryItems

//==============================================================================
procedure TfrmAdminAreaDictBrowser.actShowMetadataExecute(Sender: TObject);
var lsHeader, lsBody: String;
begin
  inherited;
  lsHeader := '';
  lsBody := '';

  with dmGeneralData.qryAllPurpose do begin
    if Active then Close;
    try
      SQL.Text := 'SELECT * FROM Admin_Type WHERE Admin_Type_Key = ''' +
                  ListKeyData.ItemKey + ''';';
      Open;
      if Eof then
        MessageBeep(0)
      else begin
        lsHeader := ResStr_MetadataForAdminType + FieldByName('Long_Name').AsString;

        if FieldByName('Authority').AsString <> '' then
          lsBody := MetaDataPaneItem('Authority', FieldByName('Authority').AsString);

        if FieldByName('Description').AsString <> '' then
          lsBody := lsBody + MetaDataPaneItem('Description', FieldByName('Description').AsString);

        if FieldByName('Date_From').AsString = '' then
          if FieldByName('Date_To').AsString <> '' then
            lsBody := lsbody + '<P><STRONG>' + ResStr_DateTo + '</STRONG> ' +
                               DateToStr(FieldByName('Date_To').AsDateTime) + '</P'
          else
        else begin
          lsBody := lsBody + '<P><STRONG>' + ResStr_DateFrom + '</STRONG> ' +
                             DateToStr(FieldByName('Date_From').AsDateTime);
          if FieldByName('Date_To').AsString <> '' then
            lsBody := lsbody + ' <STRONG>To:' + ResStr_To + '</STRONG> ' +
                               DateToStr(FieldByName('Date_To').AsDateTime);
          lsBody := lsBody + '</P';
        end;

        if FieldByName('Changed_Date').AsString <> '' then
          lsBody := lsBody + MetaDataPaneItem(ResStr_DateLastChanged, DateToStr(FieldByName('Changed_Date').AsDateTime))
        else
          lsBody := lsBody + MetaDataPaneItem(ResStr_DateOfEntry, DateToStr(FieldByName('Entry_Date').AsDateTime));
      end;
    finally
      Close;
    end;
  end;

  if lsHeader <> '' then
    with TdlgMetaDataPopup.Create(nil) do
      try
        ShowCustom(lsHeader, lsBody);
      finally
        Free;
      end;
end;  // actShowMetadataExecute

//==============================================================================
procedure TfrmAdminAreaDictBrowser.WMRefreshColours(var Msg: TMessage);
begin
  Repaint;
end;  // WMRefreshColours

//==============================================================================
procedure TfrmAdminAreaDictBrowser.PopulateDetails;
var lCursor: TCursor;
begin
  if pnlDetails.Visible then
    if tvDictionary.Selected = nil then
      hvAdminAreaDetails.Clear
    else begin
      lCursor := HourglassCursor;
      try
        //Add details to the HTML window
        if not (csDestroying in ComponentState) then
          TdmAdminAreaDictBrowser(DictionaryData).SetHTMLDetails(TDictionaryNode(tvDictionary.Selected.Data).ItemKey, hvAdminAreaDetails);
      finally
        DefaultCursor(lCursor);
      end;
    end;
end;  // PopulateDetails

//==============================================================================
procedure TfrmAdminAreaDictBrowser.hvAdminAreaDetailsHotSpotClick(
  Sender: TObject; const SRC: String; var Handled: Boolean);
begin
  inherited;
  ShowSource(SRC);
end;  // hvAdminAreaDetailsHotSpotClick

//==============================================================================
procedure TfrmAdminAreaDictBrowser.pmHQuickReportsClick(Sender: TObject);
begin
  inherited;
  frmMain.PopulateQuickReportSubMenu(tvDictionary.Selected, pmHQuickReports);
end;

{-------------------------------------------------------------------------------
  Retrieve the admin area list for an admin area
}
function TfrmAdminAreaDictBrowser.GetListForItem(
  const AKey: string): string;
begin
  Result := VarToStr(dmDatabase.GetStoredProcOutputParam('usp_AdminArea_TopLevelAdminType_Get',
      ['@Key', AKey], '@AdminTypeKey'));
end;

{-------------------------------------------------------------------------------
  Retrieve the name of the sort key in the registry
}
function TfrmAdminAreaDictBrowser.SortRegistryName: string;
begin
  Result := 'AdminArea';
end;

{-------------------------------------------------------------------------------
  Retrieve the field used for a default sort
}
function TfrmAdminAreaDictBrowser.DefaultSortField: string;
begin
  Result := SHORT_CODE;
end;

{-------------------------------------------------------------------------------
  Convert a sort field name to the actual sort SQL used
}
function TfrmAdminAreaDictBrowser.SortNameToSortSQL(const ASortName: string):
    string;
begin
  if ASortName = ITEM_NAME then
    Result := 'ORDER BY A.Item_Name, A.Short_Code'
  else
    Result := 'ORDER BY A.Short_Code, A.Item_Name';
end;

{-------------------------------------------------------------------------------
}
constructor TfrmAdminAreaDictBrowser.Create(AOwner: TComponent);
begin
  TableName := 'ADMIN_AREA';
  InitialListKey := AppSettings.AdminAreaListKey;
  inherited;
  if AppSettings.AdminAreaViewerVisible then ShowDetails(true)
                                        else ShowDetails(false);
  pmHBatchUpdate.Visible := AppSettings.UserAccessLevel >= ualAdmin;
end;
  
{-------------------------------------------------------------------------------
}
procedure TfrmAdminAreaDictBrowser.pmHBatchUpdateClick(Sender: TObject);
begin
  inherited;
  frmMain.PopulateBatchUpdateSubMenu(tvDictionary.Selected, pmHBatchUpdate);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmAdminAreaDictBrowser.ApplySecurity;
begin
  pmHBatchUpdate.Visible := AppSettings.UserAccessLevel >= ualAdmin;
end;

end.
