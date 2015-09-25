//==============================================================================
//  Unit:        BiotopeDictEditor
//
//  Implements:  TfrmBiotopeDictEditor
//
//  Description: Implements the Biotope dictionary editor. The right side
//               panel displays the details about the biotope selected in
//               the tree. This class allows biotopes to be added, edited or
//               deleted, according to the user's access rights.
//
//  Author:      Eric Salmon
//  Created:     10 Dec 2001
//
//  Changes:     Eric Salmon - 21/03/2002
//               Initialisation of the Sources component requires an extra
//               parameter for RegisterDropComponent.
//
//               Eric Salmon - 27/03/2002
//               Changed the initialisation parameters of the Sources component
//               to accept a database object instead of database name and
//               session name.
//
//  Last Revision Details:
//    $Revision: 50 $
//    $Date: 19/01/09 14:55 $
//    $Author: Pauldavies $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit BiotopeDictEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  BaseBiotopeDictUnit, ActnList, Menus, ExGrid, RapTree, ComCtrls, StdCtrls,
  ExtCtrls, CompositeComponent, Sources, VagueDateEdit, Grids, DBCtrls, Mask,
  Buttons, BiotopeDictEditorData, OnlineHelp, DataClasses, Constants, VagueDate,
  DropSource, DropTarget, HierarchyNodes, ValidationData, ExceptionForm,
  GeneralFunctions, BiotopeDictBrowser, JNCCDatasets, ImageListButton,
  DatabaseAccessADO, KeyboardRapidTree;

type
  EBiotopeDictEditor = class(TExceptionPath);
  EInvalidState = class(EBiotopeDictEditor);

  TfrmBiotopeDictEditor = class(TBaseBiotopeDict)
    pnlButtons: TPanel;
    scbBiotopeDetails: TScrollBox;
    lblNamePrompt: TLabel;
    lblSelectedName: TLabel;
    pcBiotopeDetails: TPageControl;
    tsGeneral: TTabSheet;
    Bevel1: TBevel;
    lblOriginalCode: TLabel;
    lblFullTerm: TLabel;
    lblShortTerm: TLabel;
    dblblEntryDate: TDBText;
    dblblChangedDate: TDBText;
    lblEntryDatePrompt: TLabel;
    lblChangeDatePrompt: TLabel;
    lblChangeByPrompt: TLabel;
    lblEntryByPrompt: TLabel;
    lblEnteredBy: TLabel;
    lblChangedBy: TLabel;
    dbeShortTerm: TDBEdit;
    dbmmFullTerm: TDBMemo;
    dbeOriginalCode: TDBEdit;
    tsFacts: TTabSheet;
    gbFactDetails: TGroupBox;
    lblFact: TLabel;
    lblFactTitle: TLabel;
    lblFactType: TLabel;
    lblFactDate: TLabel;
    Shape1: TShape;
    lblFactsReference: TLabel;
    eFactTitle: TEdit;
    cmbFactType: TComboBox;
    vdeFactDate: TVagueDateEdit;
    eFactReference: TEdit;
    reFact: TMemo;
    tsSources: TTabSheet;
    Sources: TSources;
    pmAdd: TPopupMenu;
    pmAddSibling: TMenuItem;
    pmAddChild: TMenuItem;
    actAddSibling: TAction;
    actAddChild: TAction;
    mnuEditAdd: TMenuItem;
    mnuEditAddSibling: TMenuItem;
    mnuEditAddChild: TMenuItem;
    mnuEditEdit: TMenuItem;
    mnuEditDelete: TMenuItem;
    N2: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditPaste: TMenuItem;
    N3: TMenuItem;
    mnuEditBold: TMenuItem;
    mnuEditItalic: TMenuItem;
    mnuEditUnderline: TMenuItem;
    pmHAdd: TMenuItem;
    pmHAddSibling: TMenuItem;
    pmHAddChild: TMenuItem;
    btnDelete: TImageListButton;
    btnEdit: TImageListButton;
    btnAdd: TImageListButton;
    btnCancel: TImageListButton;
    btnSave: TImageListButton;
    btnFactReferenceFind: TImageListButton;
    btnFactCancel: TImageListButton;
    btnFactOK: TImageListButton;
    splFacts: TSplitter;
    pnlFactsTop: TPanel;
    btnFactDelete: TImageListButton;
    btnFactEdit: TImageListButton;
    btnFactAdd: TImageListButton;
    sgFacts: TStringGrid;
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure sgFactsClick(Sender: TObject);
    procedure btnFactAddClick(Sender: TObject);
    procedure btnFactEditClick(Sender: TObject);
    procedure btnFactDeleteClick(Sender: TObject);
    procedure btnFactReferenceFindClick(Sender: TObject);
    procedure btnFactCancelClick(Sender: TObject);
    procedure btnFactOKClick(Sender: TObject);
    procedure vdeFactDateExit(Sender: TObject);
    procedure eFactReferenceKeyPress(Sender: TObject; var Key: Char);
    procedure actAddSiblingExecute(Sender: TObject);
    procedure actAddChildExecute(Sender: TObject);
    procedure dbeTermCodeChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure pcBiotopeDetailsChange(Sender: TObject);
    procedure pcBiotopeDetailsChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure eFactReferenceDblClick(Sender: TObject);
    procedure tvDictionaryKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pnlDetailsResize(Sender: TObject);
  private
    FAdd: Boolean;
    FDetailsMode: Boolean;
    FClosingForm: Boolean;
    FAddingNewNode: Boolean;

    FBiotopeFactsList : TBiotopeFactsList;
    FCurrentFact      : TBiotopeFactsItem;
    FCurrentFactSource: TKeyString;
    procedure AddItem(const ANewNode: TFlyNode);
    procedure ClearFactsFields;
    procedure ClearFields;
    function CheckDeletedNode(const AMode: TEditMode): Boolean;
    function CheckReference: Boolean;
    function DeleteItem: Boolean;
    procedure DropReference(const Sender: TObject; const iFormat: integer;
      const iSourceData: TKeyList; const iTextStrings: TStringList;
      var ioHandled: boolean);
    procedure EnableDetails(const ANewMode: TEditMode);
    function GetSortCode(const ANewNode: TFlyNode): Integer;
    procedure PopulateFactsItem;
    procedure ReferenceFactsUpdate(KeyList: TKeyList);
    procedure RefreshDictionaryBrowser;
    procedure RefreshLists(const ADictNode: TBiotopeDictionaryNode);
    procedure RefreshNode(const ANode: TFlyNode);
    procedure SaveFactsItem;
    procedure SaveItem;
    procedure SetFields;
    procedure SetKeyFields;
    procedure UpdateNodesSortCode(const ANode: TFlyNode);
    procedure ValidateFactDate;
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
    procedure WMRefreshColours(var Msg: TMessage); message WM_REFRESH_COLOURS;
    class function GetBiotopeListItemCustodian(const AKey: TKeyString): string;
  protected
    procedure FreeObjects; override;    
    function GetCurrentControlEditMode:TEditMode; override;
    procedure PopulateDetails; override;
    procedure SetupDestinationControls; override;
    procedure SetupHelp; override;
    procedure SetupObjects; override;
    procedure SwitchToDetails(sgGrid: TStringGrid; iAdd, iEdit, iDel, iSave,
      iCancel: TControl; gbDetails: TGroupBox; tfDetails: Boolean); override;
    procedure UpdateMenus; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySecurity; override;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, GeneralData, Maintbar, FormActions, References, Find,
  Rucksack;

resourcestring
  ResStr_NewBiotope = 'New Biotope';
  ResStr_SeveralBiotope = 'several Biotopes.';
  ResStr_AnotherBiotope = 'another Biotope.';
  ResStr_CannotDeleteBiotope =  'This Biotope cannot be deleted as it still contains ';
  ResStr_PermanentDeleteBiotope = 'You are about to PERMANENTLY delete the biotope "%s".'#13#13 +
                                  'Are you sure you want to continue?';
  ResStr_InvalidVagueDate = 'The vague date you have entered is not valid';
  ResStr_TypeRequired = 'A Type is required. Select one from the list.';
  ResStr_DetailsRequired =  'Details are required. Enter the Fact Details.';
  ResStr_FactDocumentInvalid =  'The Fact Document is invalid. Select a valid Fact Document or leave blank.';
  ResStr_DeleteFact = 'Are you sure you want to delete this Fact?';
  ResStr_DeleteFromDatabase = #13#13'It has been deleted from the database.';
  ResStr_FullTermRequired = 'The Full Term is required for the Biotope. Please enter a Full Term before saving.';
  ResStr_FindDocument = 'Find Document';
  ResStr_CannotDeleteBiotopeRef = 'The Biotope cannot be deleted as it is referenced in an observation.';
  ResStr_SetFieldsCallInvalid =  'SetFields was called when cursor was in an invalid state.';
  

//==============================================================================
procedure TfrmBiotopeDictEditor.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(true);
  frmMain.SetContextToolbar(Self, mnuEdit, 4,[nil, nil, nil, nil, nil, nil, nil, nil,
                                              nil, pmSort, nil, nil, nil, nil, nil]);
  // Context toolbar updated after Create called. So if tree empty, refresh
  // Menu state and toolbar dropdown buttons
  UpdateMenus;
end;  // FormActivate

//==============================================================================
procedure TfrmBiotopeDictEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  CanClose := false;
  if EditMode <> emView then begin
    Beep;
    case ConfirmSaveAndClose of
      mrYes: begin
               if btnFactOk.Enabled then btnFactOkClick(nil);  // Editing facts
               btnSaveClick(nil);
               CanClose := true;
             end;
      mrNo:  begin
               FClosingForm := true;
               if btnFactCancel.Enabled then btnFactCancelClick(nil);  // Editing facts
               btnCancelClick(nil);
               CanClose := true;
               FClosingForm := false;
             end;
    end
  end else
    CanClose := true;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmBiotopeDictEditor.SetupHelp;
begin
  mnuEdit.HelpContext          := IDH_EDITMENU;
  Self.HelpContext             := IDH_NEWBIOTOPE;
  pcBiotopeDetails.HelpContext := IDH_NEWBIOTOPEGENERAL;
  tsGeneral.HelpContext        := IDH_NEWBIOTOPEGENERAL;
  tsFacts.HelpContext          := IDH_NEWBIOTOPEFACTS;
  tsSources.HelpContext        := IDH_NEWBIOTOPESOURCES;
end;  // SetupHelp

//==============================================================================
procedure TfrmBiotopeDictEditor.SetupObjects;
begin
  // Data Modules
  DictionaryData := TdmBiotopeDictEditor.Create(nil);

  // Fact lists
  FBiotopeFactsList := TBiotopeFactsList.Create(TdmBiotopeDictEditor(DictionaryData).qryFacts, 'FactKey', sgFacts);

  // Sources
  Sources.Init(dmDatabase.LocalDatabase, 'Biotope_Sources',
               AppSettings.UserAccessLevel, dmGeneralData.GetNextKey,
               RegisterDropComponent, AppSettings.SiteID, AppSettings.ExternalFilePath);
  Sources.OnFindInternal := dmGeneralData.SourcesFindInternal;
  Sources.OnAddInternal  := dmGeneralData.SourcesAddInternal;
  Sources.OnShowSource :=dmGeneralData.SourcesShowInternal;

  // Databound controls
  with TdmBiotopeDictEditor(DictionaryData) do begin
    dbeShortTerm.Datasource    := dsGeneral;
    dbmmFullTerm.Datasource    := dsGeneral;
    dbeOriginalCode.Datasource := dsGeneral;

    // Labels
    dblblEntryDate.Datasource   := dsGeneral;
    dblblChangedDate.Datasource := dsGeneral;
  end;
end;  // SetupObjects

//==============================================================================
procedure TfrmBiotopeDictEditor.FreeObjects;
begin
  FBiotopeFactsList.Free;
  FBiotopeFactsList := nil;
end;  // FreeObjects

//==============================================================================
procedure TfrmBiotopeDictEditor.SetupDestinationControls;
begin
  inherited;
  RegisterDropComponent(eFactReference, DropReference,
      ['REFERENCE'], [CF_JNCCDATA, CF_TEXT]);
end;  // SetupDestinationControls

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.DropReference(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
begin
  if iSourceData.Header.ItemCount > 0 then
    if (iFormat = CF_JNCCDATA) then begin
      ioHandled := true;
      FCurrentFactSource  := iSourceData.Items[0].KeyField1;
      eFactReference.Text := dmGeneralData.GetReferenceText(FCurrentFactSource);
    end else
      ioHandled := false;
end;  // DropReference

//==============================================================================
procedure TfrmBiotopeDictEditor.UpdateMenus;
var ltfOn: Boolean;
    lData: TKeyDataSysSupplied;
begin
  ltfOn := (EditMode = emView) and (tvDictionary.Items.Count > 0);
  cmbList.Enabled      := EditMode = emView;
  btnShowAll.Enabled   := cmbList.Enabled;
  tvDictionary.Enabled := cmbList.Enabled;

  lData := GetStateDataFromNode(tvDictionary.Selected);
  btnAdd.Enabled        := (EditMode = emView) and AddButtonState;
  if Assigned(tvDictionary.Selected) then begin
    btnEdit.Enabled       := ltfOn and
                             (EditButtonState(lData, GetBiotopeListItemCustodian) or
                              (AppSettings.UserAccessLevel >= ualAddOnly));
    btnDelete.Enabled     := ltfOn and DeleteButtonState(lData) and
        dmGeneralData.HasFullEditAccess('Biotope_List_Item',
        'Biotope_List_Item_Key', TNodeObject(tvDictionary.Selected.Data).ItemKey);
  end
  else begin
    btnEdit.Enabled := False;
    btnDelete.Enabled := False;
  end;

  mnuEditAdd.Enabled    := btnAdd.Enabled;
  mnuEditEdit.Enabled   := btnEdit.Enabled;
  mnuEditDelete.Enabled := btnDelete.Enabled;

  actAddChild.Visible := ltfOn;
  actFind.Enabled     := ltfOn;
  actFilter.Enabled   := ltfOn;
  pmHSortBy.Visible   := ltfOn;
  mnuEditSort.Enabled := ltfOn;
  EnableSortToolbutton(ltfOn, pmSort);
  // no RichEdit controls on this form
  dmFormActions.UpdateRTFMenu(false);
end;  // UpdateMenus

//==============================================================================
procedure TfrmBiotopeDictEditor.EnableDetails(const ANewMode: TEditMode);
var ltfOn: Boolean;
begin
  FEditMode := ANewMode;
  ltfOn := EditMode <> emView;

  SetRequiredFieldsColourState(ltfOn, [dbmmFullTerm]);
  btnFactAdd.Enabled := ltfOn;
  sgFactsClick(nil);

  Sources.EditMode := ANewMode;

  btnSave.Enabled   := ltfOn;
  btnCancel.Enabled := ltfOn;

  UpdateMenus;
end;  // EnableDetails

//==============================================================================
procedure TfrmBiotopeDictEditor.ClearFields;
begin
  ClearFactsFields;
  lblChangedBy.Caption := '';
  lblEnteredBy.Caption := '';
end;  // ClearFields

//==============================================================================
procedure TfrmBiotopeDictEditor.RefreshDictionaryBrowser;
var loForm: TForm;
begin
  // If dictionary is openned in the background, refreshes the tree/browser
  // but only if the same classification list is selected.
  loForm := frmMain.GetForm(TfrmBiotopeDictBrowser, false, false);
  if (loForm <> nil) and (ListKeyData.ItemKey = TfrmBiotopeDictBrowser(loForm).ListKeyData.ItemKey) then
    TfrmBiotopeDictBrowser(loForm).UpdateDictionary;
  // If rucksack opened, refresh the biotopes.
  loForm:=frmMain.GetForm(TfrmRucksack, false, false);
  if loForm <> nil then
    TfrmRucksack(loForm).UpdateBiotopeList;
end;  // RefreshDictionary

//==============================================================================
procedure TfrmBiotopeDictEditor.pcBiotopeDetailsChange(Sender: TObject);
begin
  inherited;
  pcBiotopeDetails.HelpContext := pcBiotopeDetails.ActivePage.HelpContext;
end;  // pcBiotopeDetailsChange

//==============================================================================
procedure TfrmBiotopeDictEditor.pcBiotopeDetailsChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  inherited;
  AllowChange:= FDetailsMode;
end;  // pcBiotopeDetailsChanging

//==============================================================================
procedure TfrmBiotopeDictEditor.btnAddClick(Sender: TObject);
var lrPosPopup : TPoint;
begin
  inherited;
  lrPosPopup := ClientToScreen(Point(btnAdd.Left, pnlButtons.Top + btnAdd.Top + btnAdd.Height));
  pmAdd.Popup(lrPosPopup.X, lrPosPopup.Y);
end;  // btnAddClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.actAddSiblingExecute(Sender: TObject);
var loNewNode: TFlyNode;
begin
  inherited;
  FAddingNewNode := true;
  with tvDictionary do
    if Items.Count = 0 then
      actAddChildExecute(Sender) // Assume a root node is required
    else if not Assigned(Selected) then
      raise EBiotopeDictEditor.Create(ResStr_TreeAddFail)
    else begin
      loNewNode := Items.Add(Selected, ResStr_NewBiotope);
      try
        AddItem(loNewNode);
      except
        // on failure remove newly added node
        loNewNode.Delete;
        raise;
      end;
    end; // if
end;  // actAddSiblingExecute

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.actAddChildExecute(Sender: TObject);
var loNewNode: TFlyNode;
begin
  inherited;
  FAddingNewNode := true;
  with tvDictionary do begin
    // if no node is selected then .Selected = nil but add child will still work by adding root node
    if Assigned(Selected) then Selected.Expand(False); // ensure node is expanded first before adding new node
    loNewNode := Items.AddChild(Selected, ResStr_NewBiotope);
    try
      AddItem(loNewNode);
    except
      // on failure remove newly added node
      loNewNode.Delete;
      raise;
    end;
  end; // with
end;  // actAddChildExecute

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.AddItem(const ANewNode: TFlyNode);
var loNewDictNode: TBiotopeDictionaryNode;
begin
  inherited;
  ClearFields;
  lblSelectedName.Caption := '';
  {need to select new node without calling the populatedetails() routine in the change event handler}
  tvDictionary.Selected := ANewNode;  // selects node

  //Create a new dictionary node object add it to Data of the new node
  loNewDictNode := TBiotopeDictionaryNode.Create;
  loNewDictNode.ItemKey     := dmGeneralData.GetNextKey('BIOTOPE_LIST_ITEM', 'Biotope_List_Item_Key');  // Get a new key for the new List Item
  loNewDictNode.BiotopeKey  := dmGeneralData.GetNextKey('BIOTOPE', 'Biotope_Key'); // Get a new key for the new Biotope
  loNewDictNode.SysSupplied := False; // since we are adding this item
  ANewNode.Data := loNewDictNode;

  //Append the new Biotope/Biotope_version/Biotope_list_item to the database
 	with TdmBiotopeDictEditor(DictionaryData).qryGeneral do
    try
      //if query is not open then open it with the newly generated list item key to ensure no records are brought back
      if not Active then begin
        // open a result set anyway for appending new records
        Parameters.ParamByName('Key').Value := loNewDictNode.ItemKey;
        Active := True;
      end;
      Append;  // put record into addition state
      dmGeneralData.SetNameIDAndDate(TdmBiotopeDictEditor(DictionaryData).qryGeneral, 'BLI_Entered_By', 'BLI_Entry_Date');
      lblEnteredBy.Caption := dmGeneralData.GetIndividualName(AppSettings.UserID);
      lblChangedBy.Caption := '';
      SetKeyFields;
      RefreshLists(loNewDictNode);
    except
      Cancel;
      raise;
    end;
  pcBiotopeDetails.ActivePage := tsGeneral;
  EnableDetails(emAdd);
end;  // AddItem

//==============================================================================
procedure TfrmBiotopeDictEditor.btnEditClick(Sender: TObject);
var
  lItemKey: String;
begin
  inherited;
  lItemKey := TNodeObject(tvDictionary.Selected.Data).ItemKey;
  if dmGeneralData.HasFullEditAccess('Biotope_List_Item', 'Biotope_List_Item_Key', lItemKey) and
     HaveCustody then
    try
      if CheckDeletedNode(emEdit) then begin
        TdmBiotopeDictEditor(DictionaryData).qryGeneral.Edit;
        dmGeneralData.SetNameIDAndDate(TdmBiotopeDictEditor(DictionaryData).qryGeneral, 'BLI_Changed_By', 'BLI_Changed_Date');
        lblChangedBy.Caption := dmGeneralData.GetIndividualName(AppSettings.UserID);
        EnableDetails(emEdit);
      end;
    except
      on E:Exception do
        if dmDatabase.CheckError(E, dbeRecordLocked) then begin
          MessageDlg(ResStr_CannotEditRecord + #13#13 +
                     dmDatabase.GetErrorMessage(E.Message, dbeRecordLocked),
                     mtInformation, [mbOk], 0);
          tvDictionary.SetFocus;
        end else
          Raise;
    end  // Try...Except
  else
    EnableDetails(emEdit);
end;  // btnEditClick

//==============================================================================
procedure TfrmBiotopeDictEditor.btnDeleteClick(Sender: TObject);
var loNode: TFlyNode;
    lsMsg : String;
begin
  inherited;
  loNode := tvDictionary.Selected;
  if loNode<>nil then begin
    DictionaryData.PopulateChildNodes(loNode);
    if loNode.HasChildren then begin
      if loNode.Count > 1 then lsMsg := ResStr_SeveralBiotope
                          else lsMsg := ResStr_AnotherBiotope;
      MessageDlg(ResStr_CannotDeleteBiotope + lsMsg, mtWarning, [mbOk], 0);
    end else if CheckDeletedNode(emDelete) then begin
      try
        // See if anyone is editing the record, and if not, go ahead and delete it
        with TdmBiotopeDictEditor(DictionaryData).qryGeneral do begin
          Edit;
          Cancel;
        end;

        if MessageDlg(Format(ResStr_PermanentDeleteBiotope, [loNode.Text]),
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
          loNode := loNode.GetPrevVisible;
          if DeleteItem then begin
            RefreshDictionaryBrowser;
            frmMain.BroadcastMessage(WM_REFRESH_BIOTOPE_DIC);
            // In case first node was deleted, no previous visible!!!!
            if loNode = nil then loNode := tvDictionary.Items.GetFirstNode;
            tvDictionary.Selected := loNode;
            if loNode = nil then begin
              btnEdit.Enabled   := false;
              btnDelete.Enabled := false;
            end;
          end;
        end;
      except
        on E:Exception do
          if dmDatabase.CheckError(E, dbeRecordLocked) then
            MessageDlg(ResStr_CannotDeleteRecord + #13#13 +
                       dmDatabase.GetErrorMessage(E.Message, dbeRecordLocked),
                       mtInformation, [mbOk], 0)
          else
            Raise;
      end;  // Try...Except
    end;
    // Refresh the details side
    PostMessage(Handle, WM_SHOW_DETAILS, 0, 0);
    // Now notify whoever is interested
    NotifyDataItemChange;
  end;
end;  // btnDeleteClick

//==============================================================================
function TfrmBiotopeDictEditor.DeleteItem: Boolean;
var loNodeData: TDictionaryNode;
    loNode    : TFlyNode;
    lsKey     : TKeyString;
begin
  loNode     := tvDictionary.Selected;
  loNodeData := TDictionaryNode(loNode.Data);
  Result     := false;
  with TdmBiotopeDictEditor(DictionaryData) do
    with qryGeneral do
      if not ReferencedInTable(FieldByName('BLI_BIOTOPE_LIST_ITEM_KEY').AsString) then begin
        // Update the Sort code fields for the list items following the inserted item
        // subtract 1 from all items after the deleted node
        UpdateSortCodeDel(FieldByName('BLI_SORT_CODE').AsInteger,
                          FieldByName('BLI_PARENT').AsString,
                          FieldByName('BLI_BT_CL_VERSION_KEY').AsString);
        lsKey := FieldByName('B_BIOTOPE_KEY').AsString;
        DelFacts(lsKey);
        dmGeneralData.DelSources('Biotope_Sources', 'Biotope_Key', lsKey);
        // Delete Biotope from Biotope_List_Item Table
        dmGeneralData.ExecuteSQL('DELETE FROM Biotope_List_Item ' +
                                 'WHERE Biotope_List_Item_Key=''' + FieldByName('BLI_BIOTOPE_LIST_ITEM_KEY').AsString + '''',
                                 ResStr_DelFail + ' - BIOTOPE table');
        // Delete Biotope from Biotope Table
        dmGeneralData.ExecuteSQL('DELETE FROM Biotope WHERE Biotope_Key=''' + lsKey + '''',
                                 ResStr_DelFail + ' - BIOTOPE table');
        if Assigned(loNodeData) then loNodeData.Free;
        loNode.Delete;
        Result := true;
      end else
        MessageDlg(ResStr_CannotDeleteBiotopeRef, mtInformation, [mbOk], 0);
end;  // DeleteItem

//==============================================================================
procedure TfrmBiotopeDictEditor.btnSaveClick(Sender: TObject);
var lTab   : TTabSheet;
    lCursor: TCursor;
begin
  inherited;
  lTab := pcBiotopeDetails.ActivePage;
  pcBiotopeDetails.ActivePage := tsGeneral;
  ValidateValue(dbmmFullTerm.Text <> '', ResStr_FullTermRequired, dbmmFullTerm);
  pcBiotopeDetails.ActivePage := lTab;
  lCursor := HourglassCursor;
  try
    // Save the data
    SaveItem;
    // In case the dictionary browser form is up, or the rucksack
    RefreshDictionaryBrowser;
  finally
    DefaultCursor(lCursor);
  end;
  //Send message to all open forms that the Biotope Dictionary has changed
  frmMain.BroadcastMessage(WM_REFRESH_BIOTOPE_DIC);
end;  // btnSaveClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.SaveItem;
begin
  if (TdmBiotopeDictEditor(DictionaryData).qryGeneral.State in [dsInsert, dsEdit]) then
  begin
    try
      SetFields;
      TdmBiotopeDictEditor(DictionaryData).qryGeneral.Post;
    except
      // reset the key fields if in add state as access removes the failed keys of the failed table
      if EditMode = emAdd then SetKeyFields;
      raise;
    end;
  end;

  try
    // Update Facts
    FBiotopeFactsList.Update;
    FBiotopeFactsList.Refresh;
    // Sources
    Sources.Post;
    Sources.Refreshlists;
    RefreshNode(tvDictionary.Selected);

    EnableDetails(emView);
    FAddingNewNode := false;
    // Refresh the details side
    PostMessage(Handle, WM_SHOW_DETAILS, 0, 0);
    // Now notify whoever is interested
    NotifyDataItemChange;
  except
    // at this point qryGeneral.Post succeeded but status/facts list failed so
    // we should put qryGeneral back into edit state for the next save operation
    if EditMode = emAdd then
      TdmBiotopeDictEditor(DictionaryData).qryGeneral.Edit;
    raise;
  end;
end;  // SaveItem

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.SetFields;
begin
  with TdmBiotopeDictEditor(DictionaryData) do
    case EditMode of
      emAdd:
          begin
            with qryGeneral do begin
              // Update the Sort code fields for the list items following the inserted item
              UpdateSortCodeAdd(FieldByName('BLI_SORT_CODE').AsInteger,
                                FieldByName('BLI_PARENT').AsString,
                                FieldByName('BLI_BT_CL_VERSION_KEY').AsString);
              FieldByName('B_TERM_CURRENT').AsBoolean := True;
              FieldByName('BLI_SYSTEM_SUPPLIED_DATA').AsBoolean := False;
            end;
            dmGeneralData.SetNameIDAndDate(qryGeneral,'B_Entered_By','B_Entry_Date');
            dmGeneralData.SetNameIDAndDate(qryGeneral,'BLI_Entered_By','BLI_Entry_Date');
          end;
      emEdit:
          if qryGeneral.State = dsEdit then begin
            qryGeneral.FieldByName('B_TERM_CURRENT').AsBoolean := True;
            dmGeneralData.SetNameIDAndDate(qryGeneral,'B_Changed_By','B_Changed_Date');
            dmGeneralData.SetNameIDAndDate(qryGeneral,'BLI_Changed_By','BLI_Changed_Date');
          end;
      else
          raise EInvalidState.Create(ResStr_SetFieldsCallInvalid);
    end; // case
end;  // SetFields

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.RefreshNode(const ANode: TFlyNode);
var lqryGeneral: TJNCCQuery;
begin
  if ANode <> nil then
    with TBiotopeDictionaryNode(ANode.Data) do begin
      lqryGeneral := TdmBiotopeDictEditor(DictionaryData).qryGeneral;
      {Set dictionary node object properties}
      ShortTerm    := lqryGeneral.FieldByName('B_SHORT_TERM').AsString; // Name
      LongName     := lqryGeneral.FieldByName('B_FULL_TERM').AsString; // Authority
      SortCode     := lqryGeneral.FieldByName('BLI_SORT_CODE').AsInteger; // Sort Code (see also **SORT CODE** below)
      OriginalCode := lqryGeneral.FieldByName('B_ORIGINAL_CODE').AsString; // Sort Code (see also **SORT CODE** below)

      {Set TTreenode properties}
      ANode.Text := OriginalCode + ' - ' + ShortTerm;

      UpdateNodesSortCode(ANode); //**SORT CODE**

      lblEnteredBy.Caption := dmGeneralData.GetIndividualName(lqryGeneral.FieldByName('BLI_ENTERED_BY').AsString);
      lblChangedBy.Caption := dmGeneralData.GetIndividualName(lqryGeneral.FieldBYName('BLI_CHANGED_BY').AsString);
      lblSelectedName.Caption := ANode.Text;

      // Make sure the node's text is properly updated in the tree
      tvDictionary.Refresh;
    end; // with
end;  // RefreshNode

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.UpdateNodesSortCode(const ANode: TFlyNode);
var loNode: TFlyNode;
    liInc : Integer;
begin
  case EditMode of
    emAdd   : liInc := 1;
    emDelete: liInc := -1;
  else
    liInc := 0;
  end;

  // Must update SortCode settings for nodes after the inserted one
  loNode := ANode.GetNextSibling;
  while Assigned(loNode) do begin
    with TBiotopeDictionaryNode(loNode.Data) do SortCode := SortCode + liInc;
    loNode := loNode.GetNextSibling;
  end;
end;  // UpdateNodeSortCodes

//==============================================================================
procedure TfrmBiotopeDictEditor.btnCancelClick(Sender: TObject);
begin
  inherited;
  TdmBiotopeDictEditor(DictionaryData).qryGeneral.Cancel;
  // If it was a new node, remove from the tree now
  if EditMode = emAdd then
    with tvDictionary do begin
      if Selected <> nil then begin
        if Assigned(Selected.Data) then TNodeObject(Selected.Data).Free;
        Selected.Delete;
      end;
      Selected := Items.GetFirstNode;
    end;
  EnableDetails(emView);
  FAddingNewNode := false;
  // Refresh the details side
  SendMessage(Handle, WM_SHOW_DETAILS, 0, 0);
  // Now notify whoever is interested
  NotifyDataItemChange;
end;  // btnCancelClick

//==============================================================================
procedure TfrmBiotopeDictEditor.SetKeyFields;
var loParentDictNode, loDictNode: TBiotopeDictionaryNode;
begin
  // Get Dictionary object from the parent node
  if Assigned(tvDictionary.Selected.Parent) then
    loParentDictNode := TBiotopeDictionaryNode(tvDictionary.Selected.Parent.Data)
  else
    loParentDictNode := nil;

  // Get Dictionary object from the parent node
  loDictNode := TBiotopeDictionaryNode(tvDictionary.Selected.Data);

 	with TdmBiotopeDictEditor(DictionaryData) do begin
    { set the new item keys }
    qryGeneral.FieldByName('B_BIOTOPE_KEY').AsString := loDictNode.BiotopeKey;
    qryGeneral.FieldByName('BLI_BIOTOPE_LIST_ITEM_KEY').AsString := loDictNode.ItemKey;

    { New Biotope List Item Fields}
    qryGeneral.FieldByName('BLI_BIOTOPE_KEY').AsString := loDictNode.BiotopeKey;
    if Assigned(loParentDictNode) then
      qryGeneral.FieldByName('BLI_PARENT').AsString := loParentDictNode.ItemKey
    else
      qryGeneral.FieldByName('BLI_PARENT').Clear;

    qryGeneral.FieldByName('BLI_SORT_CODE').AsInteger := GetSortCode(tvDictionary.Selected);

    {Biotope List Item Fields}
    qryGeneral.FieldByName('BLI_BT_CL_VERSION_KEY').AsString := LatestVersion;
  end;
end;  // SetKeyFields

//==============================================================================
function TfrmBiotopeDictEditor.GetSortCode(const ANewNode: TFlyNode): Integer;
var loPreviousSib, loNextSib: TFlyNode;
begin
  loNextSib := ANewNode.GetNextSibling;
  loPreviousSib := ANewNode.GetPrevSibling;

  if (not Assigned(loNextSib)) and (not Assigned(loPreviousSib)) then
    Result := 1       // only node in the group
  else if Assigned(loNextSib) then
    Result := TBiotopeDictionaryNode(loNextSib.Data).SortCode  // we have nodes after this node
  else
    Result := TBiotopeDictionaryNode(loPreviousSib.Data).SortCode + 1; // it is the last node in the group
end;  // GetSortCode

//==============================================================================
procedure TfrmBiotopeDictEditor.WMRefreshColours(var Msg: TMessage);
begin
  if EditMode <> emView then begin
    SetRequiredFieldsColourState(true, [dbmmFullTerm]);
    SetRequiredFieldsColourState(btnFactOk.Enabled, [cmbFactType,reFact]);
  end;
  Repaint;
end;  // WMRefreshColours

//==============================================================================
procedure TfrmBiotopeDictEditor.WMTransferDone(var Msg: TMessage);
begin
  Show;  // Show the form, bring back to the top
end;  // WMTransferDone

//==============================================================================
procedure TfrmBiotopeDictEditor.SwitchToDetails(sgGrid: TStringGrid; iAdd,
  iEdit, iDel, iSave, iCancel: TControl; gbDetails: TGroupBox; tfDetails: Boolean);
begin
  inherited;
  SetRequiredFieldsColourState(tfDetails, [cmbFactType, reFact]);
end;  // SwitchToDetails

//==============================================================================
procedure TfrmBiotopeDictEditor.PopulateDetails;
var lsKey          : String;
    lCurrentDicNode: TBiotopeDictionaryNode;
    lCursor        : TCursor;
begin
  if (not FAddingNewNode) and (tvDictionary.Selected <> nil) and CheckDeletedNode(emView) then begin
    lCursor := HourglassCursor;
    try
      lCurrentDicNode := TBiotopeDictionaryNode(tvDictionary.Selected.Data);
      lsKey := lCurrentDicNode.ItemKey;
      with TdmBiotopeDictEditor(DictionaryData).qryGeneral do begin
        {Re-populate Biotope tab}
        Close; // Close existing query
        Parameters.ParamByName('Key').Value := lsKey; // Set query to new id
        Open; // reopen query

        // Set name labels
        lblEnteredBy.Caption:= dmGeneralData.GetIndividualName(FieldByName('BLI_ENTERED_BY').AsString);
        lblChangedBy.Caption:= dmGeneralData.GetIndividualName(FieldBYName('BLI_CHANGED_BY').AsString);
        lblSelectedName.Caption := tvDictionary.Selected.Text;

        RefreshLists(lCurrentDicNode);
        FCustodian := FieldByName('CUSTODIAN').AsString;
      end;
    finally
      DefaultCursor(lCursor);
    end;
  end;
  UpdateMenus;
end;  // PopulateDetails

//==============================================================================
procedure TfrmBiotopeDictEditor.RefreshLists(const ADictNode: TBiotopeDictionaryNode);
begin
  // set details queries parameters
  TdmBiotopeDictEditor(DictionaryData).qryFacts.Parameters.ParamByName('Key').Value := ADictNode.BiotopeKey; // Set query to new id
  FBiotopeFactsList.BiotopeListItemKey := ADictNode.ItemKey;
  FBiotopeFactsList.BiotopeKey := ADictNode.BiotopeKey;
  FBiotopeFactsList.Refresh;
  sgFactsClick(nil);

  // Sources
  Sources.SourcesFromKey := ADictNode.BiotopeKey;
  Sources.RefreshLists;
end;  // RefreshLists

//==============================================================================
function TfrmBiotopeDictEditor.CheckDeletedNode(const AMode:TEditMode): Boolean;
var loSelNode: TFlyNode;
    lsMessage: String;
begin
  Result := true;
  loSelNode := tvDictionary.Selected;
  if loSelNode <> nil then
    if not FAddingNewNode and
       not dmGeneralData.CheckKeyExists('Biotope_List_Item', 'Biotope_List_Item_Key', TNodeObject(loSelNode.Data).ItemKey) then
    begin
      Result := false;
      case AMode of
        emView   : lsMessage := ResStr_CannotAccessRecord;
        emEdit   : lsMessage := ResStr_CannotEditRecord;
        emDelete : lsMessage := ResStr_CannotDeleteRecord;
      end;
      MessageDlg(lsMessage + ResStr_DeleteFromDatabase, mtInformation, [mbOk], 0);
      with tvDictionary do begin
        Items.Delete(loSelNode);
        Selected:= Items.GetFirstNode;
      end;
    end;
end;  // CheckDeletedNode

//==============================================================================
procedure TfrmBiotopeDictEditor.dbeTermCodeChange(Sender: TObject);
begin
  inherited;
  if EditMode<>emView then begin
    lblSelectedName.Caption := dbeOriginalCode.Text + ' - ' + dbeShortTerm.Text;
    tvDictionary.Selected.Text := dbeOriginalCode.Text + ' - ' + dbeShortTerm.Text;
  end;
end;  // dbeTermCodeChange

//==============================================================================
procedure TfrmBiotopeDictEditor.ClearFactsFields;
begin
  { Facts Tab }
  eFactTitle.Text       := '';
  vdeFactDate.Text      := '';
  cmbFactType.Text      := '';
  cmbFactType.ItemIndex := -1;
  reFact.Lines.Clear;
  eFactReference.Text   := '';
end;  // ClearFactsFields

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.sgFactsClick(Sender: TObject);
begin
  inherited;
  with sgFacts do
    FCurrentFact := TBiotopeFactsItem(Objects[0, Row]);

  if FCurrentFact = nil then begin
    btnFactEdit.Enabled   := false;
    btnFactDelete.Enabled := false;
    ClearFactsFields;
  end else begin
    PopulateFactsItem;
    with FCurrentFact do
      EnableDetailsAddEditButtons(TN_BIOTOPE_FACT, 'Biotope_Fact_Key', ItemKey,
            Custodian, EditMode, Added, btnFactEdit, btnFactDelete);
    btnFactEdit.Enabled := btnFactEdit.Enabled and not FCurrentFact.SystemSupplied;
    btnFactDelete.Enabled := btnFactDelete.Enabled and not FCurrentFact.SystemSupplied;
  end;
end;  // sgFactsClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.PopulateFactsItem;
var liIdx: Integer;
begin
  ClearFactsFields;
  with cmbFactType do begin
    ItemIndex := -1;
    for liIdx := 0 to Items.Count - 1 do
      if Copy(Items[liIdx], 1, 1) = FCurrentFact.FactType then
      begin
        ItemIndex := liIdx;
        Break; // from loop
      end;
  end;
  vdeFactDate.Text    := FCurrentFact.Date;
  eFactTitle.Text     := FCurrentFact.Title;
  reFact.Text         := FCurrentFact.Facts;
  FCurrentFactSource  := FCurrentFact.SourceKey;
  eFactReference.Text := dmGeneralData.GetReferenceText(FCurrentFactSource);
end;  // PopulateFactsItem

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.btnFactAddClick(Sender: TObject);
begin
  inherited;
  FAdd := True;
  SwitchToDetails(sgFacts, btnFactAdd, btnFactEdit, btnFactDelete,
                           btnSave, btnCancel, gbFactDetails, True);
  ClearFactsFields;
  FCurrentFact := TBiotopeFactsItem.CreateNew(FBiotopeFactsList);
  FDetailsMode := False; // flag for allowing change of tab pages
end;  // btnFactAddClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.btnFactEditClick(Sender: TObject);
begin
  inherited;
  FAdd := False;
  SwitchToDetails(sgFacts, btnFactAdd, btnFactEdit, btnFactDelete,
                           btnSave, btnCancel, gbFactDetails, True);
  FDetailsMode := False; // flag for allowing change of tab pages
end;  // btnFactEditClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.btnFactDeleteClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteFact,
                mtConfirmation, [mbNo, mbYes], 0) = mrYes then
  begin
    FBiotopeFactsList.DeleteItem(sgFacts.Row);
    sgFactsClick(nil);
  end;
end;  // btnFactDeleteClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.btnFactOKClick(Sender: TObject);
begin
  inherited;
  // Sender is nil if called from CloseQuery method.
  if Sender = nil then ValidateFactDate;

  ValidateValue(cmbFactType.Text <> '', ResStr_TypeRequired, cmbFactType);
  Validatevalue(reFact.Text <> '', ResStr_DetailsRequired, reFact);
  ValidateValue((eFactReference.Text = '') or CheckReference, ResStr_FactDocumentInvalid, eFactReference);
  SaveFactsItem;
  if FAdd then FBiotopeFactsList.AddNew(FCurrentFact);
  SwitchToDetails(sgFacts, btnFactAdd, btnFactEdit, btnFactDelete,
                           btnSave, btnCancel, gbFactDetails, False);
  FDetailsMode := True; // flag for allowing change of tab pages
  sgFactsClick(nil);
end;  // bbFactOkClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.btnFactCancelClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgFacts, btnFactAdd, btnFactEdit, btnFactDelete,
                           btnSave, btnCancel, gbFactDetails, false);
  if FAdd then FCurrentFact.Free;
  FDetailsMode := True; // flag for allowing change of tab pages
  sgFactsClick(nil);
end;  // bbFactCancelClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.SaveFactsItem;
begin
  if Assigned(FCurrentFact) then
    with FCurrentFact do begin
      Date := vdeFactDate.Text;
      if cmbFactType.ItemIndex <> -1 then
        FactType := Copy(cmbFactType.Items[cmbFactType.ItemIndex], 1, 1)
      else
        FactType := '';
      Title := eFactTitle.Text;
      Facts := reFact.Text;
      if eFactReference.Text = '' then SourceKey := ''
                                  else SourceKey := FCurrentFactSource;
    end;
  sgFacts.Refresh;
end;  // SaveFactsItem

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.vdeFactDateExit(Sender: TObject);
var lrCancelPos:TPoint;
begin
  inherited;
  lrCancelPos := btnFactCancel.ScreenToClient(Mouse.CursorPos);
  if (lrCancelPos.X in [0..btnFactCancel.Width]) and (lrCancelPos.Y in [0..btnFactCancel.Height]) then
    btnFactCancelClick(nil)
  else if not FClosingForm then
    ValidateFactDate;
end; // vdeFactDateExit

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.ValidateFactDate;
begin
  if vdeFactDate.Text <> '' then begin
    ValidateValue(IsVagueDate(vdeFactDate.Text),
                  ResStr_InvalidVagueDate, vdeFactDate);
    ValidateValue(CheckVagueDate(vdeFactDate.Text),
                  InvalidDate(ResStr_FactDate, true, true), vdeFactDate);
    vdeFactDate.Text := VagueDateToString(vdeFactDate.VagueDate);
  end;
end;  // ValidateFactDate

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.eFactReferenceKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key = #13 then begin
    CheckReference;
    Key := #0;
  end;
end;  // eFactReferenceKeyPress

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.btnFactReferenceFindClick(Sender: TObject);
var lfrmReferences: TfrmReferences;
begin
  inherited;
  if dmFormActions.actDocuments.Execute then
  begin
    lfrmReferences := TfrmReferences(frmMain.GetForm(TfrmReferences));
    SetupLink(lfrmReferences, Self, ReferenceFactsUpdate);
    if FCurrentFactSource <> '' then
      lfrmReferences.FindAndDisplaySource(FCurrentFactSource);
  end;
end;  // btnFactReferenceFindClick

//------------------------------------------------------------------------------
function TfrmBiotopeDictEditor.CheckReference: Boolean;
var lFind: TdlgFind;
begin
  Result:= true;
  lFind := TdlgFind.CreateDialog(nil, ResStr_FindDocument, ftReference);
  with lFind do
    try
      if FindUnique(eFactReference.Text) then begin
        eFactReference.Text := ItemText;
        FCurrentFactSource  := ItemKey;
      end else
      if not eSearchText.NoSourceItems then begin
        if ShowModal = mrOk then begin
          eFactReference.Text := ItemText;
          FCurrentFactSource  := ItemKey;
        end else
          Result := false;
      end else begin
        MessageDlg(ResStr_NoReferenceItems, mtInformation, [mbOK], 0);
        Result := false;
      end;
    finally
      Release;
    end;
end;  // CheckReference

//------------------------------------------------------------------------------
procedure TfrmBiotopeDictEditor.ReferenceFactsUpdate(KeyList: TKeyList);
begin
  try
    if (KeyList <> nil) and (KeyList.Header.ItemCount > 0) then begin
      FCurrentFactSource := KeyList.Items[0].KeyField1;
      eFactReference.Text := dmGeneralData.GetReferenceText(FCurrentFactSource);
    end;
  finally
    KeyList.Free;
  end;
end;  // ReferenceFactsUpdate

//==============================================================================
procedure TfrmBiotopeDictEditor.eFactReferenceDblClick(Sender: TObject);
begin
  inherited;
  if FCurrentFactSource<>'' then
    dmGeneralData.SourcesShowInternal(Sender, FCurrentFactSource);
end;

function TfrmBiotopeDictEditor.GetCurrentControlEditMode:TEditMode;
begin
  Result := EditMode; // default
  // Disable edit lock messages for all sub list grids
  if  pcBiotopeDetails.ActivePage<>tsGeneral then
        Result :=emNone;

end;

//==============================================================================
procedure TfrmBiotopeDictEditor.tvDictionaryKeyDown(Sender: TObject;
                                                    var Key: Word;
                                                    Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_DELETE: if btnDelete.Enabled then btnDeleteClick(btnDelete);
  end;
end;

{-------------------------------------------------------------------------------
}
class function TfrmBiotopeDictEditor.GetBiotopeListItemCustodian(const AKey: TKeyString): string;
begin
  Result := dmGeneralData.Custodian('Biotope_List_Item', 'Biotope_List_Item_Key', AKey);
end;

{-------------------------------------------------------------------------------
}
constructor TfrmBiotopeDictEditor.Create(AOwner: TComponent);
begin
  inherited;
  SetGridColumnTitles(sgFacts, [ResStr_Type, ResStr_Date, ResStr_Title]);
  SwitchToDetails(sgFacts, btnFactAdd, btnFactEdit, btnFactDelete,
                           btnSave, btnCancel, gbFactDetails, false);
  EnableDetails(emView);
  pcBiotopeDetails.ActivePage := tsGeneral;
  FDetailsMode := true; // flag for allowing change of tab pages
end;
      
{-------------------------------------------------------------------------------
}
procedure TfrmBiotopeDictEditor.ApplySecurity;
begin
  UpdateMenus;
end;

{-------------------------------------------------------------------------------
  When the details frame is resized, resizes its contents to fit.
}
procedure TfrmBiotopeDictEditor.pnlDetailsResize(Sender: TObject);
var
  deltaWidth, deltaHeight : Integer;
begin
  inherited;
  deltaWidth := pnlDetails.Width - scbBiotopeDetails.Width;
  deltaHeight := pnlDetails.Height - scbBiotopeDetails.Height;

  scbBiotopeDetails.Width := pnlDetails.Width;
  scbBiotopeDetails.Height := pnlDetails.Height;

  Sources.Width := Sources.Width + deltaWidth;
  Sources.Height := Sources.Height + deltaHeight;
end;

end.