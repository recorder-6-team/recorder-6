//==============================================================================
//  Unit:        BiotopeDictBrowser
//
//  Implements:  TfrmBiotopeDictBrowser
//
//  Description: Implements the Biotope dictionary browser. The right side
//               panel displays the details about the biotope selected in
//               the tree, in HTML format.
//
//  Author:      Eric Salmon
//  Created:     23 Nov 2001
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 25 $
//    $Date: 17/07/09 14:26 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit BiotopeDictBrowser;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseBiotopeDictUnit, Htmlview, ActnList, Menus, ExGrid, RapTree, ComCtrls,
  StdCtrls, ExtCtrls, Constants, OnlineHelp, MetaDataPopup, HierarchyNodes,
  BiotopeDictBrowserData, DictionaryHTMLDetails, GeneralFunctions,
  Recorder2000_TLB, SQLConstants, KeyboardRapidTree, ADODB, Variants;

type
  TfrmBiotopeDictBrowser = class(TBaseBiotopeDict, IProvidesOccurrencesSQL)
    hvBiotopeDetails: THTMLViewer;
    actShowMetadata: TAction;
    actShowItemDetails: TAction;
    mnuEditShowMetadata: TMenuItem;
    mnuEditShowItemDetails: TMenuItem;
    ReturnData1: TMenuItem;
    PlacesforOccurrencesReport2: TMenuItem;
    pmHierarchyOccurrencesForPlacesReport: TMenuItem;
    pmHQuickReports: TMenuItem;
    pmHBatchUpdate: TMenuItem;
    mnuPlaceHolder: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actShowMetadataExecute(Sender: TObject);
    procedure actShowItemDetailsExecute(Sender: TObject);
    procedure hvBiotopeDetailsHotSpotClick(Sender: TObject;
      const SRC: String; var Handled: Boolean);
    procedure hvBiotopeDetailsHotSpotCovered(Sender: TObject;
      const SRC: String);
    procedure pmHQuickReportsClick(Sender: TObject);
    procedure pmHBatchUpdateClick(Sender: TObject);
  private
    procedure WMRefreshColours(var Msg: TMessage); message WM_REFRESH_COLOURS;
  protected
    procedure PopulateDetails; override;
    procedure SetupHelp; override;
    procedure SetupObjects; override;
    procedure ShowDetails(const Reveal: Boolean); override;
    procedure UpdateListKeySetting; override;
    procedure UpdateMenus; override;
    // IProvidesOccurrencesSQL
    function Get_CanProvideSQL: WordBool; safecall;
    function Get_OccurrencesSQL(const ATypes: WideString): WideString; safecall;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySecurity; override;
    procedure PrintScreen; override;    // method called by file/print facility
    procedure UpdateDictionary;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, FormActions, Maintbar, GeneralData, VagueDate, DatabaseAccessAdo;

resourcestring
  ResStr_HideItemDetails    = 'Hide &Item Details';
  ResStr_HideDictDetails    = 'Hide Dictionary Item Details';
  ResStr_ShowItemDetails    = 'Show &Item Details';
  ResStr_ShowDictDetails    = 'Show Dictionary Item Details';
  ResStr_MetadataForList    = 'Metadata for Biotope Classification: ';
  ResStr_MetadataForBiotope = 'Metadata for Biotope: ';
  ResStr_BiotopeKey         = 'Biotope Key';
  ResStr_BiotopeListItemKey = 'Biotope List Item Key';
  ResStr_Objectives         = 'Objectives';
  ResStr_CreatedBy          = 'Created by %s';
  ResStr_CreatedOn          = ' on %s';
  ResStr_RevisionNumber     = 'Revision Number';
  ResStr_RevisionDate       = 'Revision Date';

//==============================================================================
procedure TfrmBiotopeDictBrowser.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(true);
  frmMain.SetContextToolbar(Self, mnuEdit, 0,[nil, nil, nil, nil, pmSort, nil, nil]);
  with dmFormActions do begin
    actPrint.Enabled := tvDictionary.Items.Count > 0;
    actPlacesForOccurrencesReport.Enabled := true;
    actOccurrencesForPlacesReport.Enabled := true;
  end;
  UpdateMenus;
end;  // FormActivate

//==============================================================================
procedure TfrmBiotopeDictBrowser.FormDeactivate(Sender: TObject);
begin
  inherited;
  with dmFormActions do begin
    actPrint.Enabled := false;
    actPlacesForOccurrencesReport.Enabled := false;
    actOccurrencesForPlacesReport.Enabled := false;
  end;
end;  // FormDeactivate

//==============================================================================
procedure TfrmBiotopeDictBrowser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FormDeactivate(Sender);
  AppSettings.BiotopeViewerVisible := pnlDetails.Visible;
end;  // FormClose

//==============================================================================
procedure TfrmBiotopeDictBrowser.SetupObjects;
begin
  DictionaryData := TdmBiotopeDictBrowser.Create(nil);
end;  // SetupObjects

//==============================================================================
procedure TfrmBiotopeDictBrowser.SetupHelp;
begin
  mnuEdit.HelpContext:= IDH_EDITMENU;
  Self.HelpContext   := IDH_BIOTOPEDICT;
end;  // SetupHelp

//==============================================================================
procedure TfrmBiotopeDictBrowser.UpdateMenus;
var ltfOn: Boolean;
begin
  with tvDictionary do begin
    if Items.Count = 0 then PopupMenu := nil
                       else PopupMenu := pmHierarchy;
    ltfOn := PopupMenu <> nil;
  end;
  actFind.Enabled         := ltfOn;
  actFilter.Enabled       := ltfOn;
  mnuEditsort.Enabled     := ltfOn;
  actShowMetadata.Enabled := ltfOn;
  EnableSortToolbutton(ltfOn, pmSort);
end;  // UpdateMenus

//==============================================================================
procedure TfrmBiotopeDictBrowser.ShowDetails(const Reveal: Boolean);
begin
  inherited ShowDetails(Reveal);
  if Reveal then begin
    actShowItemDetails.ImageIndex := 56;
    actShowItemDetails.Caption    := ResStr_HideItemDetails;
    actShowItemDetails.Hint       := ResStr_HideDictDetails;
  end else begin
    actShowItemDetails.ImageIndex := 55;
    actShowItemDetails.Caption    := ResStr_ShowItemDetails;
    actShowItemDetails.Hint       := ResStr_ShowDictDetails;
  end;
end;  // ShowDetails

//==============================================================================
procedure TfrmBiotopeDictBrowser.UpdateListKeySetting;
begin
  AppSettings.BiotopeListKey := ListKeyData.ItemKey;
end;  // UpdateListKeySetting

//==============================================================================
procedure TfrmBiotopeDictBrowser.actShowMetadataExecute(Sender: TObject);
var
  header, body, changedBy: String;
  rs: _Recordset;
begin
  inherited;
  header := '';
  body   := '';

  header := ResStr_MetadataForList + cmbList.Text;

  rs := dmDatabase.GetRecordset(
      'usp_BiotopeClassification_Select_ForMetadata',
      ['@Key', ListKeyData.ItemKey]);
  if not rs.Eof then
  begin
    if not VarIsNull(rs.Fields['Objectives'].Value) then
      body := MetaDataPaneItem(ResStr_Objectives, rs.Fields['Objectives'].Value);
      if not VarIsNull(rs.Fields['Created_By'].Value) then
      begin
        body := body
            + '<P>'
            + Format(ResStr_CreatedBy, [rs.Fields['Created_By'].Value]);
        if not VarIsNull(rs.Fields['Created_Vague_Date_Start'].Value) then
          body := body
              + Format(
                  ResStr_CreatedOn,
                  [VagueDateToString(dmGeneralData.GetVagueDateFromRecordset(rs, 'Created_'))]);
        body := body + '.</P>';
      end;

      body := body
          + MetaDataPaneItem(ResStr_RevisionNumber, VarToStr(rs.Fields['Revision_Number'].Value));
      if not VarIsNull(rs.Fields['Revision_Date'].Value) then
        body := body
            + MetaDataPaneItem(ResStr_RevisionDate, DateToStr(rs.Fields['Revision_Date'].Value));
  end;

  if (ActiveControl <> cmbList) and Assigned(tvDictionary.Selected) then
  begin
    with dmDatabase.GetRecordset('usp_Biotope_Select_ForMetadata',
        ['@Key', TNodeObject(tvDictionary.Selected.Data).ItemKey]) do
    begin
      header := ResStr_MetadataForBiotope + Fields['DisplayField'].Value;

      if VarIsNull(Fields['Changed_By'].Value) then
        changedBy := ''
      else begin
        // Key could potentially be one not in database.
        changedBy := dmGeneralData.GetIndividualName(Fields['Changed_By'].Value);
        if changedBy <> '' then
          changedBy := Format(ResStr_LastChanged,
              [changedBy, DateToStr(Fields['Changed_Date'].Value)]);
      end;

      body := '<P>'
          + Format(ResStr_RecordCreation,
              [dmGeneralData.GetIndividualName(Fields['Entered_By'].Value),
               DateToStr(Fields['Entry_Date'].Value)])
          + changedBy
          + '.</P>'
          + MetadataPaneItem(ResStr_BiotopeKey, Fields['Biotope_Key'].Value)
          + MetadataPaneItem(ResStr_BiotopeListItemKey, Fields['Biotope_List_Item_Key'].Value)
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
end;  // actShowMetadataExecute

//==============================================================================
procedure TfrmBiotopeDictBrowser.actShowItemDetailsExecute(Sender: TObject);
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
procedure TfrmBiotopeDictBrowser.WMRefreshColours(var Msg: TMessage);
begin
  Repaint;
end;  // WMRefreshColours

//==============================================================================
procedure TfrmBiotopeDictBrowser.PopulateDetails;
var lCursor:TCursor;
begin
  if pnlDetails.Visible and (tvDictionary.Selected <> nil) then
  begin
    lCursor := HourglassCursor;
    try
      //Add details to the HTML window
      if not (csDestroying in ComponentState) then
        TdmBiotopeDictBrowser(DictionaryData).SetHTMLDetails(
            THTMLDictionaryNode(tvDictionary.Selected.Data),
            ListKeyData.ItemKey,
            hvBiotopeDetails);
    finally
      DefaultCursor(lCursor);
    end;
  end;
end;  // PopulateDetails

//==============================================================================
procedure TfrmBiotopeDictBrowser.PrintScreen;
begin
  if tvDictionary.Selected <> nil then hvBiotopeDetails.Print(1, 9999);
end;  // PrintScreen

//==============================================================================
procedure TfrmBiotopeDictBrowser.UpdateDictionary;
begin
  tvDictionary.Selected := nil;
  cmbListChange(nil);
end;  // UpdateDictionary

//==============================================================================
procedure TfrmBiotopeDictBrowser.hvBiotopeDetailsHotSpotClick(
  Sender: TObject; const SRC: String; var Handled: Boolean);
begin
  inherited;
  ShowSource(SRC);
end;  // hvBiotopeDetailsHotSpotClick

//==============================================================================

procedure TfrmBiotopeDictBrowser.hvBiotopeDetailsHotSpotCovered(
  Sender: TObject; const SRC: String);
begin
  inherited;
  if SRC='' then
    frmMain.SetStatus('')
  else
  if (Copy(SRC, 1, 1) = ST_SOURCE_DENOT_CHAR) then
    frmMain.SetStatus('')
  else
    frmMain.SetStatus(SRC);
end;

{-------------------------------------------------------------------------------
  Description : Returns trus if a biotope is selected, thus enabling the
              Quick Reports
  Created : 11/03/2003 }
function TfrmBiotopeDictBrowser.Get_CanProvideSQL: WordBool;
begin
  Result := assigned(tvDictionary.Selected);
end;


{-------------------------------------------------------------------------------
  Description : Provides SQL to list occurrences for the selected biotope to the
              Quick Reports
  Created : 11/03/2003 }
function TfrmBiotopeDictBrowser.Get_OccurrencesSQL(
  const ATypes: WideString): WideString;
begin
  Result:= '';
  if (Pos('B', ATypes)>0) and Assigned(tvDictionary.Selected) then
    if Assigned(tvDictionary.Selected.Data) then
      Result :=
          'SELECT BOCC.BIOTOPE_OCCURRENCE_KEY AS OCCURRENCE_KEY, ''B'' AS TYPE '
          + 'FROM BIOTOPE_OCCURRENCE BOCC '
          + 'INNER JOIN BIOTOPE_DETERMINATION BD ON BD.BIOTOPE_OCCURRENCE_KEY=BOCC.BIOTOPE_OCCURRENCE_KEY '
          + 'WHERE BD.BIOTOPE_LIST_ITEM_KEY=''' + TNodeObject(tvDictionary.Selected.Data).ItemKey+ ''' '
          + 'AND BD.PREFERRED=1 ' + SQL_BIOTOPE_RESTRICTION;
end;

{-------------------------------------------------------------------------------
  Description : Populates the list of available reports in the popup
  Created : 11/03/2003 }
procedure TfrmBiotopeDictBrowser.pmHQuickReportsClick(Sender: TObject);
begin
  inherited;
  BringToFront; // in case popup activated when not the active mdi child, which can happen  
  frmMain.PopulateQuickReportSubMenu(tvDictionary.Selected, pmHQuickReports);
end;

{-------------------------------------------------------------------------------
}
constructor TfrmBiotopeDictBrowser.Create(AOwner: TComponent);
begin
  inherited;
  // TableName set in ancestor
  if AppSettings.BiotopeViewerVisible then ShowDetails(true)
                                      else ShowDetails(false);
  ApplySecurity;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmBiotopeDictBrowser.pmHBatchUpdateClick(Sender: TObject);
begin
  inherited;
  frmMain.PopulateBatchUpdateSubMenu(tvDictionary.Selected, pmHBatchUpdate);
end;
    
{-------------------------------------------------------------------------------
}
procedure TfrmBiotopeDictBrowser.ApplySecurity;
begin
  pmHBatchUpdate.Visible := AppSettings.UserAccessLevel >= ualAdmin;
end;

end.
