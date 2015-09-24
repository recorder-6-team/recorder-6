//==============================================================================
//  Unit:        EnhancedTermLists
//
//  Implements:  TfrmEnhancedTermLists
//
//  Description: Implements a screen allowing the list of Terms for
//                 references to be configured.
//
//  Author:      John van Breda
//  Created:     December 2005
//
//  Last Revision Details:
//    $Revision: 35 $
//    $Date: 4/08/08 14:25 $
//    $Author: Ericsalmon $
//==============================================================================
unit EnhancedTermLists;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseChildUnit, Menus, ExtCtrls, StdCtrls, ComboListID,
  BaseCompositeComponent, LinkedControls, exgrid, RapTree, ImageListButton,
  ComCtrls, SortListView, DropSource, TreeColl, ExceptionForm, DataStringGrid,
  Grids, JNCCDatasets, EnhancedTermListsData, ActnList, KeyboardRapidTree,
  DataClasses, OnlineHelp, ControlStringGrid, Constants;

resourcestring
  ResStr_NodeNotTermNode = 'Node is not of type TTermNode.';
  ResStr_NodeNotAvailable = 'There is no node available.';
  ResStr_NewTerm = 'New Term';
  ResStr_CannotBeDeletedTermHasChildren = 'This term cannot be deleted because it has child terms.';
  ResStr_CyclicRelationship =
      'The ''%s'' relationship cannot be created as it would cause a cycle to exist in the hierarchy.';
  ResStr_Parent = 'Parent';
  ResStr_PleaseSpecifyATerm = 'Please specify a term.';
  ResStr_CanOnlyDropSingleTerm = 'You can only drop a single term at a time onto this list.';
  ResStr_CanOnlyDropSameConceptGroup =
       'You can only drop a term from the same term list onto this list.';
  ResStr_DroppedConceptDoesNotExist = 'The dropped concept does not exist.';
  ResStr_ConceptAndSynonyms = 'the selected term and all its synonyms';
  ResStr_CanAddEditOwnSynonyms =
      'However, you are allowed to add and edit your own synonyms.';
  ResStr_CantDropNodeOntoSelf = 'You cannot drop a node onto itself.';
  ResStr_RequestedTermListNotFound =
      'The term list for %s cannot be found.';

type
  EEnhancedTermLists = class(TExceptionPath);

  TTermNode = class(TFlyNode)
  private
    FConceptKey: string;
    FConceptTerm: string;
    FPopulated: Boolean;
    procedure SetPopulated(Value: Boolean);
  public
    constructor Create(AOwner: TTreeCollection); override;
    destructor Destroy; override;
    procedure Initialise(const AConceptKey, ATerm: string);
    property ConceptKey: string read FConceptKey write FConceptKey;
    property ConceptTerm: string read FConceptTerm;
    property Populated: Boolean read FPopulated write SetPopulated;
  end;

  TfrmEnhancedTermLists = class(TBaseChild)
    pnlTerms: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    pnlDetails: TPanel;
    btnDelete: TImageListButton;
    btnEdit: TImageListButton;
    btnAdd: TImageListButton;
    Label1: TLabel;
    tvTerms: TKeyboardRapidTree;
    btnSave: TImageListButton;
    btnCancel: TImageListButton;
    Label2: TLabel;
    Label3: TLabel;
    eTerm: TLinkedEdit;
    cmbLanguage: TIDComboBox;
    Label4: TLabel;
    mnuEdit: TMenuItem;
    mnuEditAdd: TMenuItem;
    mnuMainAddSibling: TMenuItem;
    mnuMainAddChild: TMenuItem;
    mnuEditEdit: TMenuItem;
    mnuEditDelete: TMenuItem;
    sgSynonyms: TControlStringGrid;
    btnAddSynonym: TImageListButton;
    btnRemoveSynonym: TImageListButton;
    pmAdd: TPopupMenu;
    alEnhancedTermLists: TActionList;
    actAddTerm: TAction;
    actAddChildTerm: TAction;
    actEdit: TAction;
    actDelete: TAction;
    pmAddAddTerm: TMenuItem;
    pmAddAddChildTerm: TMenuItem;
    N1: TMenuItem;
    mnuEditReturnData: TMenuItem;
    pmTreeview: TPopupMenu;
    actRename: TAction;
    mnuEditRename: TMenuItem;
    mnuTreeViewRename: TMenuItem;
    mnuTreeViewAddChildTerm: TMenuItem;
    mnuTreeViewEdit: TMenuItem;
    mnuTreeViewDelete: TMenuItem;
    N2: TMenuItem;
    mnuTreeViewAddTerm: TMenuItem;
    Label5: TLabel;
    cmbConceptGroup: TIDComboBox;
    procedure pnlDetailsResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvTermsExpanding(Sender: TObject; Node: TFlyNode;
      var AllowExpansion: Boolean);
    procedure btnAddClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure eTermExit(Sender: TObject);
    procedure cmbLanguagePopulate(Sender: TObject);
    procedure cmbLanguageChange(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actAddTermExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure tvTermsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure tvTermsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure tvTermsChange(Sender: TObject; Node: TFlyNode);
    procedure actRenameExecute(Sender: TObject);
    procedure tvTermsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvTermsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tvTermsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure cmbConceptGroupChange(Sender: TObject);
    procedure cmbConceptGroupPopulate(Sender: TObject);
    procedure tvTermsExit(Sender: TObject);
  private
    FHierarchyRelationTypeKeys: TStringList;
    FTimestamp: TSQLSvrTimestamp;
    FDefaultLanguageKey: string;
    FGridManager : TDataStringGrid;
    FdmEnhancedTermLists: TdmEnhancedTermLists;
    FRenameLock: Boolean;
    procedure SetEditMode(AEditMode: TEditMode; ANode: TFlynode=nil);
    procedure GetTermNodeData(const Sender: TObject; var oDropSource:
        TJNCCDropSource);
    function AddChildNode(AParentNode: TFlyNode; const AConceptKey,
      AItemName: string;
      AHasChildren: boolean): TTermNode;
    procedure DragOverCheck(APoint: TPoint; const ATable, AFieldKey: String; var Accept: Boolean);
    procedure DropTerms(const Sender: TObject; const iFormat: integer;
        const iSourceData: TKeyList; const iTextStrings: TStringList;
        const iIsPasteOperation: Boolean; var ioHandled: Boolean);
    procedure ExpandNode(ANode: TTermNode);
    procedure CorrectAnchoring;
    function GetCurrentConceptKey: string;
    procedure AddOrModifyTerm;
    procedure SetCurrentConceptKey(const Value: string);
    procedure CheckForExistingTerm;
    procedure ClearSynonymsGrid;
    procedure DropCurrentNode;
    procedure ClearDetails;
    procedure JoinNodeToParent;
    procedure PopulateTopLevel;
    procedure GetDefaultLanguage;
    procedure CreateGridManager;
    procedure FinishEditingNode;
    procedure AddTerm(Sender: TObject);
    procedure AddNewNodeForDirectEntry;
    function GetConceptGroupKey: string;
    function GetHierarchyRelationTypeKey: string;
    procedure SelectNewNode(ANode: TFlyNode);
    procedure MoveNodeTo(ANode, ATarget: TFlyNode);
    procedure SelectNodeWithoutRename(ANode: TFlyNode);
  protected
    procedure SetUpDestinationControls; override;
    property ConceptGroupKey: string read GetConceptGroupKey;
    property HierarchyRelationTypeKey: string read GetHierarchyRelationTypeKey;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplySecurity; override;
    function GetKeyList: TKeyList; override;
    procedure SelectConceptGroup(const conceptGroupKey: TKeyString; const termListName: String);
    property CurrentConceptKey: string read GetCurrentConceptKey write SetCurrentConceptKey;
  end;

implementation

{$R *.dfm}

uses DatabaseAccessADO, ADODb, GeneralFunctions, ApplicationSettings,
     ComObj, BaseADODataModule, GeneralData, BaseDragFormUnit, DropTarget,
     FormActions, Maintbar;

{-------------------------------------------------------------------------------
  Remove formatting from the term as not supported in this simple editor
}
function StripItalics(const AText: string): string;
begin
  Result := StringReplace(AText, '<i>', '', [rfReplaceAll]);
  Result := StringReplace(Result, '</i>', '', [rfReplaceAll]);
end;

{-==============================================================================
    TTermNode
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor sets default property values.
}
constructor TTermNode.Create(AOwner: TTreeCollection);
begin
  inherited Create(AOwner);
  
  FPopulated := False;
end;  // TTermNode.Create 

{-------------------------------------------------------------------------------
}
destructor TTermNode.Destroy;
begin
  inherited;
end;  // TTermNode.Destroy

{-------------------------------------------------------------------------------
  Initialises the node.  The concept key and term for the list preferred concept should be
      passed in.
}
procedure TTermNode.Initialise(const AConceptKey, ATerm: string);
begin
  FConceptKey := AConceptKey;
  FConceptTerm := ATerm;
  Caption := StripItalics(ATerm);
end;  // TTermNode.Initialise

{-------------------------------------------------------------------------------
}
procedure TTermNode.SetPopulated(Value: Boolean);
begin
  FPopulated := Value;
end;  // TTermNode.SetPopulated


{-==============================================================================
    TfrmEnhancedTermlists
===============================================================================}

{-------------------------------------------------------------------------------
}
constructor TfrmEnhancedTermLists.Create(AOwner: TComponent);
begin
  inherited;
  // set flag to inform other selection code not to start renaming a node yet
  FHierarchyRelationTypeKeys := TStringList.Create;  
  FRenameLock := true;
  cmbConceptGroup.PopulateContent;
  cmbConceptGroup.ItemIndex := 0;
  PopulateTopLevel;
  FdmEnhancedTermLists := TdmEnhancedTermLists.Create(nil);
  cmbLanguage.PopulateContent;
  GetDefaultLanguage;
  SendMessage(Handle,WM_UPDATE_MENU_ICONS,0,0);
  CorrectAnchoring;
  SetEditMode(emView);
  // force first node to select properly
  SelectNodeWithoutRename(tvTerms.Selected);
  // flag to track if node selection is via mouse or keyboard
  FRenameLock := False;
  // Max length for Term.
  eTerm.MaxLength := 150;
  Self.HelpContext := IDH_EnhancedTermLists;
end;

{-------------------------------------------------------------------------------
  Retrieve data from a node when dragged.
}
procedure TfrmEnhancedTermLists.GetTermNodeData(const Sender: TObject; var
    oDropSource: TJNCCDropSource);
begin
  if Assigned(tvTerms.Selected) then begin
    Assert(tvTerms.Selected is TTermNode);
    oDropSource.DropData.SetTable('Concept');
    oDropSource.DropData.AddItem(TTermNode(tvTerms.Selected).ConceptKey,
       IntToStr(Integer(tvTerms.Selected)));
  end;
end;

{-------------------------------------------------------------------------------
  Some resize code, because anchoring does not work properly for dynamically
     resized forms
}
procedure TfrmEnhancedTermLists.pnlDetailsResize(Sender: TObject);
begin
  inherited;
  // lock eTerm to the combo box width as the combo box has anchoring
  eTerm.Width := cmbLanguage.Width;
end;

{-------------------------------------------------------------------------------
  Register drag and drop
}
procedure TfrmEnhancedTermLists.SetUpDestinationControls;
begin
  RegisterDragComponent(tvTerms, GetTermNodeData);
  if AppSettings.UserAccessLevel>ualAddOnly then
    RegisterDropComponentAdvanced(tvTerms, DropTerms, [TN_CONCEPT], [CF_JNCCDATA], DragOverCheck);
  // Correct column sizes depending on space available on screen.  This is
  // done here because the screen is now in its final display size.
  // set first column to 3/4, 2nd to 1/4 available width, excluding scrollbar
  sgSynonyms.ColWidths[2] :=
      (sgSynonyms.ClientWidth - GetSystemMetrics(SM_CXHSCROLL) - sgSynonyms.ColWidths[0]) div 4;
  sgSynonyms.ColWidths[1] := sgSynonyms.ColWidths[2] * 3;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmEnhancedTermLists.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
end;

{-------------------------------------------------------------------------------
  If expanding a node that contains children but is not populated, then
    find the children
}
procedure TfrmEnhancedTermLists.tvTermsExpanding(Sender: TObject;
  Node: TFlyNode; var AllowExpansion: Boolean);
begin
  if not (Node is TTermNode) then
    raise EEnhancedTermLists.Create(ResStr_NodeNotTermNode);
  if not TTermNode(Node).Populated then begin
    ExpandNode(TTermNode(Node));
  end;
end;

{-------------------------------------------------------------------------------
  Perform the actual expansion of a node.
}
procedure TfrmEnhancedTermLists.ExpandNode(ANode: TTermNode);
var
  lCursor: TCursor;
  lRecordset: _Recordset;
begin
  lCursor := HourglassCursor;
  tvTerms.Items.BeginUpdate;
  try
    lRecordset := dmDatabase.GetRecordset('usp_Concept_Select_ForParent', [
          '@ParentConceptKey', TTermNode(ANode).ConceptKey,
          '@HierarchyRelationTypeKey', HierarchyRelationTypeKey]);
    with lRecordset do
      while not EOF do begin
        AddChildNode(ANode, Fields['Concept_Key'].Value,
              Fields['Item_Name'].Value, Fields['HasChildren'].Value=1);
        MoveNext;
        if Application.Terminated then Break;
      end;
  finally
    tvTerms.Items.EndUpdate;
    DefaultCursor(lCursor);
  end;
  TTermNode(ANode).Populated := True;
end;  // TfraThesaurusNavigator.ExpandNode

{-------------------------------------------------------------------------------
  Adds a child node to the node.  Input parameters are atomised rather than
      passing a recordset in the overloaded version of this.
}
function TfrmEnhancedTermLists.AddChildNode(AParentNode: TFlyNode; const AConceptKey,
    AItemName: string; AHasChildren: boolean): TTermNode;
begin
  Result := TTermNode(tvTerms.Items.AddTypedChild(AParentNode,
      TTermNode));
  with Result do begin
    Initialise(AConceptKey, AItemName);
    HasChildren := AHasChildren;
  end; // with
end;  // TfraThesaurusNavigator.AddChildNode

{-------------------------------------------------------------------------------
  Make sure anchored controls are correctly positioned to start with, as this
     does not work well with different scales otherwise
}
procedure TfrmEnhancedTermLists.CorrectAnchoring;
begin
  btnSave.Top := pnlDetails.Height - btnSave.Height - 6;
  btnCancel.Top := pnlDetails.Height - btnSave.Height - 6;
  sgSynonyms.Height := btnSave.Top - sgSynonyms.Top - 9;
  sgSynonyms.Width := pnlDetails.Width - eTerm.Left - btnAddSynonym.Width - 9;
  cmbLanguage.Width := sgSynonyms.Width;
  eTerm.Width := sgSynonyms.Width;
  tvTerms.Height := pnlTerms.Height - tvTerms.Top - 4;
  btnAddSynonym.Left := sgSynonyms.Left + sgSynonyms.Width;
  btnRemoveSynonym.Left := sgSynonyms.Left + sgSynonyms.Width;
  btnCancel.Left := btnAddSynonym.Left + btnAddSynonym.Width - btnCancel.Width;
  btnSave.Left := btnCancel.Left - btnSave.Width - 9;
end;

{-------------------------------------------------------------------------------
  Cleanup
}
destructor TfrmEnhancedTermLists.Destroy;
begin
  FdmEnhancedTermLists.Free;
  FHierarchyRelationTypeKeys.Free;  
  inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmEnhancedTermLists.SetEditMode(AEditMode: TEditMode; ANode: TFlynode=nil);
var
  lNode: TFlyNode;
  lCanEditNode: boolean;
begin
  if Assigned(ANode) then
    lNode := ANode
  else
    lNode := tvTerms.Selected;
  Assert((lNode is TTermNode) or not Assigned(lNode));
  FEditMode := AEditMode;
  btnAdd.Enabled := AEditMode = emView;
  if Assigned(lNode) then
    actEdit.Enabled := AEditMode = emView
  else
    actEdit.Enabled := False;
  if Assigned(lNode) then
    lCanEditNode := (dmGeneralData.SessionHasFullEditAccess(
        'CONCEPT', 'CONCEPT_KEY', TTermNode(lNode).ConceptKey) and
        HaveCustody) or (TTermNode(lNode).ConceptKey='')
  else
    lCanEditNode := False;
  actDelete.Enabled := (AEditMode = emView) and Assigned(lNode) and
      (AppSettings.UserAccessLevel>ualAddOnly);
  actRename.Enabled := actEdit.Enabled and lCanEditNode;
  tvTerms.Enabled := AEditMode = emView;
  cmbConceptGroup.Enabled := AEditMode = emView;
  btnSave.Enabled := AEditMode = emEdit;
  btnCancel.Enabled := AEditMode = emEdit;
  btnAddSynonym.Enabled := AEditMode = emEdit;
  btnRemoveSynonym.Enabled := (AEditMode = emEdit) and (AppSettings.UserAccessLevel>ualAddOnly);
  eTerm.Enabled := (AEditMode = emEdit) and lCanEditNode;
  cmbLanguage.Enabled := (AEditMode = emEdit) and lCanEditNode;
  if Assigned(FGridManager) then
    FGridManager.Enabled := (AEditMode = emEdit);
  if (AEditMode = emEdit) and lCanEditNode then
    eTerm.SetFocus
  else if (AEditMode = emEdit) then
    sgSynonyms.SetFocus;
  if (AEditMode = emEdit) and (not lCanEditNode) and (not (AppSettings.UserAccessLevel <= ualAddOnly)) then begin
    if HaveCustody then // must be another users data, user is Full Edit (Own Data)
      ShowInformation(ResStr_NotAllowedToEditOtherPersonsData +
          #13#10 + ResStr_CanAddEditOwnSynonyms)
    else // another site's data
      ShowInformation(ResStr_NotAllowedToEditOtherSitesData +
          #13#10 + ResStr_CanAddEditOwnSynonyms);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmEnhancedTermLists.btnAddClick(Sender: TObject);
var
  lPosPopup: TPoint;
begin
  if not Assigned(tvTerms.Selected) then
    actAddTerm.Execute
  else begin
    // Work out where to show the popupmenu so that it appears just under
    // the button
    lPosPopup := btnAdd.ClientToScreen(Point(0, btnAdd.Height));
    pmAdd.Popup(lPosPopup.X, lPosPopup.Y);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmEnhancedTermLists.btnSaveClick(Sender: TObject);
var
  lParams: Array of Variant;
  lNewConcept: boolean;
begin
  eTermExit(Self); // Make sure the contents has been trimmed.
  Assert(Assigned(tvTerms.Selected));
  ValidateValue(eTerm.Text <> '', ResStr_PleaseSpecifyATerm);
  if (eTerm.Key = '') then
    AddOrModifyTerm;

  lParams := VarArrayOf(['@Key', CurrentConceptKey,
                          '@TermKey', eTerm.Key,
                            '@ConceptGroupKey', ConceptGroupKey,
                            '@Preferred', true,
                            '@ConceptRankKey', Null,
                            '@NameTypeConceptKey', CONCEPT_KEY_COMMON,
                            '@SortCode', Null,
                            '@ListCode', Null,
                            '@Timestamp', FTimestamp,
                            '@SessionID', AppSettings.SessionID
                          ]);

  dmDatabase.Connection.BeginTrans;
  // Empty key means new record.
  lNewConcept := CurrentConceptKey = '';
  try
    if lNewConcept then begin
      CurrentConceptKey := VarToStr(dmDatabase.RunInsertStoredProc('Concept',
                   'usp_ConceptSimple_Insert', lParams, '@Key'));
      // if a child, then insert the relationship
      if Assigned(tvTerms.Selected.Parent) then
        JoinNodeToParent;
    end
    else
      dmDatabase.RunUpdateStoredProc('usp_ConceptSimple_Update', lParams);
    FdmEnhancedTermLists.SelectedConceptKey := CurrentConceptKey;
    FGridManager.Save;
    dmDatabase.Connection.CommitTrans;
  except
    on E:Exception do begin
      dmDatabase.Connection.RollbackTrans;
      // if rolling back a new concept, forget the key
      if lNewConcept then
        CurrentConceptKey := '';
      if CompareText(E.Message, 'Record updated by another user') = 0 then
        raise EEnhancedTermLists.CreateNonCritical(ResStr_AnotherUserUpdated, E)
      else
        raise;
    end;
  end;
  tvTerms.Selected.Caption := eTerm.Text;
  SetEditMode(emView);
  // refresh the timestamp
  SelectNewNode(tvTerms.Selected);
  tvTerms.Invalidate;
end;

{-------------------------------------------------------------------------------
  Join a child node to its parent with a relationship
}
procedure TfrmEnhancedTermLists.JoinNodeToParent;
begin
  Assert(tvTerms.Selected is TTermNode);
  Assert(tvTerms.Selected.Parent is TTermNode);
  try
    dmDatabase.RunInsertStoredProc('Concept',
            'usp_ConceptRelation_Insert',
            ['@FromConceptKey', TTermNode(tvTerms.Selected.Parent).ConceptKey,
            '@ToConceptKey', TTermNode(tvTerms.Selected).ConceptKey,
            '@ThesaurusRelationTypeKey', HierarchyRelationTypeKey],
            '@Key');
  except
    on E:EOleException do begin
      if CompareText(E.Message, 'Cyclical relationship')=0 then
        raise EEnhancedTermLists.CreateNonCritical(Format(
            ResStr_CyclicRelationship, [ResStr_Parent]), E)
      else
        raise E;
    end;
  end;
end;


{-------------------------------------------------------------------------------
}
procedure TfrmEnhancedTermLists.btnCancelClick(Sender: TObject);
begin
  SetEditMode(emView);
  if Assigned(tvTerms.Selected) then begin
    Assert(tvTerms.Selected is TTermNode);
    if TTermNode(tvTerms.Selected).ConceptKey='' then
      // Added node, so remove it
      DropCurrentNode
    else
      // reload current node
      SelectNewNode(tvTerms.Selected);
  end;
end;

{-------------------------------------------------------------------------------
  Trim the term on exit
}
procedure TfrmEnhancedTermLists.eTermExit(Sender: TObject);
begin
  inherited;
  eTerm.Text := TrimLeft(eTerm.Text);
  eTerm.Text := TrimRight(eTerm.Text);
end;

{-------------------------------------------------------------------------------
  Comes here if the term or the language has changed for the concept.
}
procedure TfrmEnhancedTermLists.AddOrModifyTerm;
begin
  if eTerm.Key = '' then
    CheckForExistingTerm;
  if eTerm.Key = '' then
    eTerm.Key := VarToStr(dmDatabase.RunInsertStoredProc('Term',
                                    'usp_Term_Insert',
                                    [ '@LanguageKey', cmbLanguage.CurrentStrID,
                                      '@ItemName', eTerm.Text,
                                      '@Plaintext', StripItalics(eTerm.Text),
                                      '@SessionID', AppSettings.SessionID],
                                    '@Key'));
end;  // TfraConceptGeneral.AddOrModifyTerm

{-------------------------------------------------------------------------------
  Retrieve the concept key of the current term node
}
function TfrmEnhancedTermLists.GetCurrentConceptKey: string;
begin
  if Assigned(tvTerms.Selected) then begin
    if not (tvTerms.Selected is TTermNode) then
      raise EEnhancedTermLists.Create(ResStr_NodeNotTermNode);
    Result := TTermNode(tvTerms.Selected).ConceptKey;
  end
  else
    raise EEnhancedTermLists.Create(ResStr_NodeNotAvailable);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmEnhancedTermLists.SetCurrentConceptKey(const Value: string);
begin
  if Assigned(tvTerms.Selected) then begin
    if not (tvTerms.Selected is TTermNode) then
      raise EEnhancedTermLists.Create(ResStr_NodeNotTermNode);
    TTermNode(tvTerms.Selected).ConceptKey := Value;
  end
  else
    raise EEnhancedTermLists.Create(ResStr_NodeNotAvailable);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmEnhancedTermLists.cmbLanguagePopulate(Sender: TObject);
begin
  with dmDatabase.GetRecordset('usp_Languages_Select', []) do
    while not EOF do begin
      cmbLanguage.Add(
          VarToStr(Fields['Item_Name'].Value),
          VarToStr(Fields['Language_Key'].Value));
      MoveNext;
    end;
end;

{-------------------------------------------------------------------------------
  Change language implies a new term
}
procedure TfrmEnhancedTermLists.cmbLanguageChange(Sender: TObject);
begin
  eTerm.Key := '';
end;

{-------------------------------------------------------------------------------
  If a new term has been entered, check if a suitable term record already exists
     and link to that
}
procedure TfrmEnhancedTermLists.CheckForExistingTerm;
begin
  with dmDatabase.GetRecordset('usp_Terms_Select_ForSearch',
      ['@SearchText', eTerm.Text, '@SearchKey', cmbLanguage.CurrentStrID]) do begin
    if (RecordCount > 0) and
            (CompareText(VarToStr(Fields['SearchTerm'].Value), eTerm.Text) = 0) then
      eTerm.Key := Fields['Item_Key'].Value;
  end;
end;

{-------------------------------------------------------------------------------
  Edit action
}
procedure TfrmEnhancedTermLists.actEditExecute(Sender: TObject);
begin
  // When entering edit mode, ensure latest data is loaded
  SelectNewNode(tvTerms.Selected);
  SetEditMode(emEdit);
end;

{-------------------------------------------------------------------------------
  Action to add a top level OR a child term
}
procedure TfrmEnhancedTermLists.actAddTermExecute(Sender: TObject);
begin
  AddTerm(Sender);
  SetEditMode(emEdit);
end;

{-------------------------------------------------------------------------------
  Clear the grid of synonyms
}
procedure TfrmEnhancedTermLists.ClearSynonymsGrid;
begin
  FdmEnhancedTermLists.qrySynonyms.Close;
  FdmEnhancedTermLists.qrySynonyms.Parameters.ParamByName('@Key').Value := '';
  FdmEnhancedTermLists.qrySynonyms.Open;
  if Assigned(FGridManager) then
    FGridManager.Refresh
  else
    CreateGridManager;
end;

{-------------------------------------------------------------------------------
  Deletion Action
}
procedure TfrmEnhancedTermLists.actDeleteExecute(Sender: TObject);
var
  lCursor: TCursor;
begin
  if Assigned(tvTerms.Selected) then begin
    Assert(tvTerms.Selected is TTermNode);
    if TTermNode(tvTerms.Selected).ConceptKey<>'' then begin
      ValidateValue(tvTerms.Selected.HasChildren = False,
                    ResStr_CannotBeDeletedTermHasChildren);
      if ConfirmDeletionYesNo(ResStr_ConceptAndSynonyms)=mrYes then begin
        lCursor := HourglassCursor;
        try
          if TTermNode(tvTerms.Selected).ConceptKey <> '' then begin
            try
              dmDatabase.RunDeleteStoredProc('usp_Concept_Delete',
                   ['@Key', TTermNode(tvTerms.Selected).ConceptKey,
                    '@Timestamp', FTimestamp,
                    '@SyncTaxonDict', 0,
                    '@DeleteUnlinkedSynonyms', 1]);
            except on E:EUpdatedException do begin
                // reload the node
                SelectNewNode(tvTerms.Selected);
                // inform user failure because record was updated by another user
                raise;
              end;
            end;
          end;
          DropCurrentNode;
        finally
          DefaultCursor(lCursor);
        end;
      end else
        // if Delete triggered because user had cleared caption, then reload
        if tvTerms.Selected.Caption = '' then
          tvTerms.Selected.Caption := eTerm.Text;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Remove a current node, and select the previous one
}
procedure TfrmEnhancedTermLists.DropCurrentNode;
var
  lNextNode: TFlyNode;
begin
  // try to find an appropriate node to select after the deletion,
  // preferably the previous node
  lNextNode := tvTerms.Selected.GetPrev;
  if not Assigned(lNextNode) then begin
    lNextNode := tvTerms.Selected.GetNext;
    if not Assigned(lNextNode) then
      lNextNode := tvTerms.Items.GetFirstNode;
  end;
  tvTerms.Selected.Free;
  SelectNodeWithoutRename(lNextNode);
  if not Assigned(tvTerms.Selected) then
    ClearDetails;
  SetEditMode(FEditMode);
end;

{-------------------------------------------------------------------------------
  Clear the concept and synonym details on the right
}
procedure TfrmEnhancedTermLists.ClearDetails;
begin
  ClearSynonymsGrid;
  eTerm.Text := '';
  eTerm.Key := '';
  FTimestamp := 0;
  cmbLanguage.ItemIndex := cmbLanguage.IdIndexOf(FDefaultLanguageKey);
end;

{-------------------------------------------------------------------------------
  Record key down as this affects selection behaviour
}
procedure TfrmEnhancedTermLists.tvTermsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  FRenameLock := true;
  if Assigned(tvTerms.Selected) then begin
    if (Key=VK_F5) and (not (ExGrid.goEditing in tvTerms.Options)) then begin
      PopulateTopLevel;
      // force first node to select properly
      SelectNodeWithoutRename(tvTerms.Selected);
    end
    else if (Key=VK_RETURN) and (ExGrid.goEditing in tvTerms.Options) then begin
      if tvTerms.Selected.Caption = '' then
        FinishEditingNode
      else
        AddNewNodeForDirectEntry;
    end
    else if (Key=VK_ESCAPE) and (ExGrid.goEditing in tvTerms.Options) and
        Assigned(tvTerms.Selected) then begin
      Assert(tvTerms.Selected is TTermNode);
      if TTermNode(tvTerms.Selected).ConceptKey = '' then
        DropCurrentNode;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Fill in the top level
}
procedure TfrmEnhancedTermLists.PopulateTopLevel;
var
  lNewNode: TTermNode;
begin
  tvTerms.Items.BeginUpdate;
  try
    tvTerms.Items.Clear;
    with dmDatabase.GetRecordset('usp_Concept_Select_ForTopLevel',
        ['@ConceptGroupKey', ConceptGroupKey,
        '@HierarchyRelationTypeKey', HierarchyRelationTypeKey
        ]) do begin
      while not EOF do begin
        lNewNode := TTermNode(tvTerms.Items.AddTypedChild(nil, TTermNode));
        with lNewNode do begin
          Initialise(Fields['Concept_Key'].Value, Fields['Item_Name'].Value);
          HasChildren := Fields['HasChildren'].Value;
        end; // with
        MoveNext;
      end;
    end; // with
  finally
    tvTerms.Items.EndUpdate;
  end; // try
end;

{-------------------------------------------------------------------------------
  Retrieve the default language for the concept group
}
procedure TfrmEnhancedTermLists.GetDefaultLanguage;
begin
  FDefaultLanguageKey := dmDatabase.GetStoredProcOutputParam('usp_ConceptGroupLanguage_Get',
      ['@Key', ConceptGroupKey], '@LanguageKey');
  FdmEnhancedTermLists.qrySynonymsLanguage.DefaultExpression :=
    cmbLanguage.Items[cmbLanguage.IdIndexOf(FDefaultLanguageKey)];
end;



{-------------------------------------------------------------------------------
  Create and setup the synonyms grid manager
}
procedure TfrmEnhancedTermLists.CreateGridManager;
begin
  FGridManager := TDataStringGrid.Create(sgSynonyms, FdmEnhancedTermLists.qrySynonyms, 'Item_Key');
  // Enable locking of rows belonging to different custodian
  FGridManager.SiteID := AppSettings.SiteID;
  FGridManager.UserID := AppSettings.UserId;
  FGridManager.RestrictFullEdit := AppSettings.RestrictFullEdit;
  FGridManager.AddOnly := AppSettings.UserAccessLevel <= ualAddOnly;
  FGridManager.PopulateGrid;
  btnAddSynonym.OnClick := FGridManager.AddToGrid;
  btnRemoveSynonym.OnClick := FGridManager.DeleteFromGrid;
  FGridManager.OnUpdateRow := FdmEnhancedTermLists.UpdateRow;
  FGridManager.OnDeleteRow := FdmEnhancedTermLists.DeleteRow;
  FGridManager.Indicator := True;
end;

{-------------------------------------------------------------------------------
  Check if editing then offer to save data before closing
}
procedure TfrmEnhancedTermLists.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (FEditMode = emEdit) then begin
    case ConfirmSaveAndClose of
      mrYes: begin
        btnSaveClick(nil);
        CanClose := true;
      end;
      mrNo: begin
        btnCancelClick(nil);
        CanClose := true;
      end;
      mrCancel: CanClose := False;
    end;
  end else
    CanClose := true;
end;

{-------------------------------------------------------------------------------
  Remove edit mode when selecting a new node
}
procedure TfrmEnhancedTermLists.tvTermsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  // click on same cell twice enabled edit mode
  if (tvTerms.Selected = tvTerms.GetNodeAtRow(ARow)) and
      (not (ExGrid.goEditing in tvTerms.Options)) and (not FRenameLock) then
    actRename.Execute
  else begin
    if ExGrid.goEditing in tvTerms.Options then begin
      FinishEditingNode;
      tvTerms.Options := tvTerms.Options - [ExGrid.goEditing];
    end;
    SelectNewNode(tvTerms.GetNodeAtRow(ARow));
  end;
end;

{-------------------------------------------------------------------------------
  when the cell loses focus, it should be force to end the editing mode
}




{-------------------------------------------------------------------------------
  When finishing editing of a node, save the details and remove the edit mode
     option
}
procedure TfrmEnhancedTermLists.FinishEditingNode;
begin
  if Assigned(tvTerms.Selected) and (not tvTerms.Selected.Deleting) then begin
    if tvTerms.Selected.Caption = '' then
        actDeleteExecute(nil)
    else begin
        eTerm.Text := tvTerms.Selected.Caption;
        eTerm.Key := '';
        btnSaveClick(nil);
    end;
  end;
  tvTerms.Options := tvTerms.Options - [ExGrid.goEditing];
end;

{-------------------------------------------------------------------------------
  Add a term.  If sender is actAddTerm, add top level. If sender is
     actChildTerm then add a child.  Else, add a sibling
}
procedure TfrmEnhancedTermLists.AddTerm(Sender: TObject);
begin
  if Sender = actAddTerm then
    tvTerms.Selected := tvTerms.Items.AddTypedChild(nil, TTermNode)
  else if Sender = actAddChildTerm then
    tvTerms.Selected := tvTerms.Items.AddTypedChild(tvTerms.Selected, TTermNode)
  else
    // Add a sibling
    tvTerms.Selected := tvTerms.Items.AddTypedChild(tvTerms.Selected.Parent, TTermNode);
  ClearDetails;
  tvTerms.Selected.Caption := '<' + ResStr_NewTerm + '>';
end;

{-------------------------------------------------------------------------------
  Allow drag/drop only for hierarchical lists.
}
procedure TfrmEnhancedTermLists.DragOverCheck(APoint: TPoint; const ATable, AFieldKey: String;
  var Accept: Boolean);
begin
  Accept := HierarchyRelationTypeKey <> '';
end;

{-------------------------------------------------------------------------------
  Handle dragging of terms to reorganise the hierarchy
}
procedure TfrmEnhancedTermLists.DropTerms(const Sender: TObject; const iFormat: integer;
  const iSourceData: TKeyList; const iTextStrings: TStringList;
  const iIsPasteOperation: Boolean; var ioHandled: Boolean);
var
  lRecursionPassed: Boolean;
  lClickPoint: TPoint;
  lIsPasteAsTopLevel: boolean;
  lTargetNode: TFlyNode;
  lDroppedConceptKey: string;
  lDraggedNode: TTermNode;
  lParentConceptKey: variant;
begin
  lClickPoint := tvTerms.ScreenToClient(Mouse.CursorPos);
  lTargetNode := tvTerms.GetNodeAt(lClickPoint.X, lClickPoint.Y);
  // If user dropped node on empty space, then paste as top level.
  lIsPasteAsTopLevel := not Assigned(lTargetNode);
  if iSourceData.Header.ItemCount <> 1 then
    raise EEnhancedTermLists.CreateNonCritical(ResStr_CanOnlyDropSingleTerm);
  lDroppedConceptKey := iSourceData.Items[0].KeyField1;
  if VarToStr(dmDatabase.GetStoredProcOutputParam(
                'usp_ConceptGroupKey_Get_ForConcept', ['@Key', lDroppedConceptKey],
                '@ConceptGroupKey')) <> ConceptGroupKey then
    raise EEnhancedTermLists.CreateNonCritical(ResStr_CanOnlyDropSameConceptGroup);
  // Can only drop from within this hierarchy, so find dragged node
  if iSourceData.Items[0].KeyField2 <> '' then begin
    if IsInt(iSourceData.Items[0].KeyField2) then begin
      if TObject(StrToInt(iSourceData.Items[0].KeyField2)) is TTermNode then begin
        lDraggedNode := TTermNode(StrToInt(iSourceData.Items[0].KeyField2));
        if lDraggedNode = lTargetNode then
          raise EEnhancedTermLists.CreateNonCritical(ResStr_CantDropNodeOntoSelf);
        tvTerms.OnSelectCell := nil;
        if lIsPasteAsTopLevel then
          lParentConceptKey := null
        else
          lParentConceptKey := TTermNode(lTargetNode).ConceptKey;
        // Check for and prevent circular reference.
        if lParentConceptKey <> null then begin
          lRecursionPassed := (dmDatabase.GetStoredProcOutputParam(
                          'usp_Concept_RecursionCheck_Get',
                          ['@PotentialChildKey', lDroppedConceptKey,
                           '@PotentialParentKey', lParentConceptKey],
                          '@RecursionExists') = 0);
          ValidateValue(lRecursionPassed, Format(ResStr_CyclicRelationship, [ResStr_Parent]));
        end;
        if Assigned(lTargetNode) then
          if not TTermNode(lTargetNode).Populated then
            ExpandNode(TTermNode(lTargetNode));
        lDraggedNode.ConceptKey := dmDatabase.GetStoredProcOutputParam('usp_Concept_Paste',
            ['@ConceptKey', lDroppedConceptKey,
            '@DestConceptGroupKey', ConceptGroupKey,
            '@DestParentConceptKey', lParentConceptKey,
            '@IsCut', 1,
            '@SessionID', AppSettings.SessionID],
            '@DestConceptKey');
        MoveNodeTo(lDraggedNode, lTargetNode);
        // reload timestamp
        SelectNewNode(tvTerms.Selected);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Saves the currently edited node.  If this is the last node in a set of
      siblings the adds a new node and puts it into direct entry mode.
}
procedure TfrmEnhancedTermLists.AddNewNodeForDirectEntry;
var
  lNextSibling: TFlyNode;
begin
  FinishEditingNode;
  lNextSibling := tvTerms.Selected.GetNextSibling;
  if not Assigned(lNextSibling) then begin
    AddTerm(nil);
    tvTerms.SetFocus;
    // Start editing the new node.  For some reason, altering the options doesn't work
    PostMessage(tvTerms.Handle, WM_KEYDOWN, VK_F2, 0);
    PostMessage(tvTerms.Handle, WM_KEYUP, VK_F2, 0);
  end
  else
    SelectNodeWithoutRename(lNextSibling);
end;

{-------------------------------------------------------------------------------
  Retrieve the keylist for return data purposes
}
function TfrmEnhancedTermLists.GetKeyList: TKeyList;
var
  lList: TEditableKeylist;
begin
  lList := TEditableKeylist.Create;
  lList.SetTable('Concept');
  if Assigned(tvTerms.Selected) then
    lList.AddItem(TTermNode(tvTerms.Selected).ConceptKey,
         IntToStr(Integer(tvTerms.Selected)));
  Result := lList;
end;

{-------------------------------------------------------------------------------
  Select the next node and populate its data
}
procedure TfrmEnhancedTermLists.SelectNewNode(ANode: TFlyNode);
begin
  if Assigned(ANode) then begin
    if ANode is TTermNode then begin
      if TTermNode(ANode).ConceptKey='' then
        ClearDetails
      else begin
        with dmDatabase.GetRecordset('usp_Concept_Select',
          ['@ConceptKey', TTermNode(ANode).ConceptKey]) do begin
          eTerm.Text := StripItalics(Fields['Item_Name'].Value);
          ANode.Caption := eTerm.Text;
          eTerm.Key := Fields['Item_Key'].Value;
          FTimestamp    := Fields['Timestamp'].Value;
          cmbLanguage.ItemIndex := cmbLanguage.IdIndexOf(VarToStr(Fields['Language_Key'].Value));
        end; // with
      end;
      FdmEnhancedTermLists.qrySynonyms.Close;
      FdmEnhancedTermLists.qrySynonyms.Parameters.ParamByName('@Key').Value := TTermNode(ANode).ConceptKey;
      FdmEnhancedTermLists.qrySynonyms.Open;
      if not Assigned(FGridManager) then
        CreateGridManager
      else
        FGridManager.Refresh;
      FCustodian := dmGeneralData.Custodian('Concept', 'Concept_Key', TTermNode(ANode).ConceptKey);
    end; // if
  end else
    ClearDetails;
  if AppSettings.UserAccessLevel >= ualAddOnly then
    SetEditMode(FEditMode, ANode);
end;

{-------------------------------------------------------------------------------
  Move a node.  Disables the OnSelectCell handler since we don't want this
      to trigger save operations or selection of the parent node.
}
procedure TfrmEnhancedTermLists.MoveNodeTo(ANode, ATarget: TFlyNode);
begin
  tvTerms.OnSelectCell := nil;
  try
    ANode.MoveTo(ATarget, naAddChild);
  finally
    tvTerms.OnSelectCell := tvTermsSelectCell;
  end; // try
end;

{-------------------------------------------------------------------------------
  Enable the action to add a child if a node is selected, but if the list does
      not support parent/child relationships, then disable the action
}
procedure TfrmEnhancedTermLists.tvTermsChange(Sender: TObject;
  Node: TFlyNode);
begin
  actAddChildTerm.Enabled := Assigned(Node) and (HierarchyRelationTypeKey<>'');
end;

{-------------------------------------------------------------------------------
  Put treee into edit mode
}
procedure TfrmEnhancedTermLists.actRenameExecute(Sender: TObject);
begin
  tvTerms.SetFocus;
  tvTerms.Options := tvTerms.Options + [ExGrid.goEditing];
  tvTerms.EditorMode := True;
end;

{-------------------------------------------------------------------------------
  Track if key pressed to select a cell, or mouse.  Also handle various keys
}
procedure TfrmEnhancedTermLists.tvTermsKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  FRenameLock := false;
  if Assigned(tvTerms.Selected) then begin
    if (Key=VK_DELETE) and not (ExGrid.goEditing in tvTerms.Options)
        and actDelete.Enabled then begin
      actDeleteExecute(nil);
      Key := 0;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Track right mouse button so that it isn't used to go into rename mode
}
procedure TfrmEnhancedTermLists.tvTermsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then
    FRenameLock := true;
end;

{-------------------------------------------------------------------------------
  Track right mouse button so that it isn't used to go into rename mode
}
procedure TfrmEnhancedTermLists.tvTermsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then
    FRenameLock := false;


end;

{-------------------------------------------------------------------------------
  Select a node, but without going into rename mode if the node is already
      selected.
}
procedure TfrmEnhancedTermLists.SelectNodeWithoutRename(ANode: TFlyNode);
begin
  FRenameLock := true;
  try
    if tvTerms.Selected <> ANode then
      tvTerms.Selected := ANode;
  finally
    FRenameLock := false;
  end;
end;

{-------------------------------------------------------------------------------
  Setup context toolbar on activation
}
procedure TfrmEnhancedTermLists.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(False);
  frmMain.SetContextToolbar(Self, mnuEdit, 0, []);
  dmFormActions.UpdateRTFMenu(False);
end;

{-------------------------------------------------------------------------------
  Accessor for internal property to return the current list's key
}
function TfrmEnhancedTermLists.GetConceptGroupKey: string;
begin
  Result := cmbConceptGroup.CurrentStrID;
end;

{-------------------------------------------------------------------------------
  Accessor for internal property to return the current list's hierarchy
    relation type key (maybe a blank string)
}
function TfrmEnhancedTermLists.GetHierarchyRelationTypeKey: string;
begin
  Result := FHierarchyRelationTypeKeys[cmbConceptGroup.ItemIndex];
end;

{-------------------------------------------------------------------------------
  Refresh the list when the combo item changes
}
procedure TfrmEnhancedTermLists.cmbConceptGroupChange(Sender: TObject);
begin
  if (cmbConceptGroup.ItemIndex <> -1) then begin
    PopulateTopLevel;
    // force the tag to appear selected
    tvTerms.Selected := tvTerms.Selected;
    SelectNewNode(tvTerms.Selected);
    SetEditMode(emView);
  end;
end;

{-------------------------------------------------------------------------------
  Populate the list of concept groups
}
procedure TfrmEnhancedTermLists.cmbConceptGroupPopulate(Sender: TObject);
begin
  FHierarchyRelationTypeKeys.Clear;
  with dmDatabase.GetRecordset('usp_ConceptGroup_SelectForAnyApplication', []) do
    while not EOF do begin
      cmbConceptGroup.Add(Fields['Item_Name'].Value, VarToStr(Fields['Concept_Group_Key'].Value));
      FHierarchyRelationTypeKeys.Add(VarToStr(Fields['Hierarchy_Relation_Type_Key'].Value));
      MoveNext;
    end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmEnhancedTermLists.ApplySecurity;
var
  lNode: TFlyNode;
  lCanEditNode: Boolean;
begin
  lNode := tvTerms.Selected;
  btnAdd.Enabled := AppSettings.UserAccessLevel >= ualAddOnly;
  actEdit.Enabled := (AppSettings.UserAccessLevel >= ualAddOnly) and Assigned(lNode);
  actDelete.Enabled := (AppSettings.UserAccessLevel > ualAddOnly) and Assigned(lNode);
  if Assigned(lNode) then
    lCanEditNode := (dmGeneralData.SessionHasFullEditAccess(
        'CONCEPT', 'CONCEPT_KEY', TTermNode(lNode).ConceptKey) and
        HaveCustody) or (TTermNode(lNode).ConceptKey='')
  else
    lCanEditNode := False;
  actRename.Enabled := actEdit.Enabled and lCanEditNode;
  if AppSettings.UserAccessLevel >= ualAddOnly then
    tvTerms.PopupMenu := pmTreeview
  else
    tvTerms.PopupMenu := nil;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmEnhancedTermLists.SelectConceptGroup(const conceptGroupKey: TKeyString;
  const termListName: String);
var
  i: Integer;
begin
  i := cmbConceptGroup.IDIndexOf(conceptGroupKey);
  if i = -1 then
    ShowInformation(Format(ResStr_RequestedTermListNotFound, [termListName]))
  else begin
    cmbConceptGroup.ItemIndex := i;
    cmbConceptGroupChange(nil);
  end;
end;

{-------------------------------------------------------------------------------
  // when the tvTerms loses the focus, it will force the cell to end the editing
}
procedure TfrmEnhancedTermLists.tvTermsExit(Sender: TObject);
begin
  inherited;
  if ExGrid.goEditing in tvTerms.Options then begin
    FinishEditingNode;
    tvTerms.Options := tvTerms.Options - [ExGrid.goEditing];
  end;
end;

end.
