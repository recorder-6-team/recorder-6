//==============================================================================
//  Unit:        References
//
//  Implements:  TfrmReferences
//
//  Description: Implements the functionalities to deal with various document
//               references, allowing them to be viewed and/ot edited.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     DBRichEdit text attributes reset before displaying records.
//               Fixes the text attribute persistence problem.
//
//  Last Revision Details:
//    $Revision: 225 $
//    $Date: 7/05/09 17:27 $
//    $Author: Pauldavies $
//
//==============================================================================

{$I '..\..\Third Party\Dorset Software Services\DssVcl32\DelphiVersions.Inc'}

unit References;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseChildUnit, StdCtrls, Buttons, Grids, ExtCtrls, Menus, VagueDateEdit, Db,
  ComCtrls, BaseFormUnit, EasyShell, Registry, ExceptionForm, GeneralFunctions,
  DBCtrls, Mask, DataClasses, HierarchyNodes, ReferencesData, DBListCombo,
  DropSource, VagueDate, Find, GeneralData, Observations, OnlineHelp, Constants,
  ValidationData, ActnList, DBGrids, RTFGrid, JNCCGrid, JNCCDatasets, ImageListButton,
  DatabaseAccessADO, Variants, ADODB, RapTree, SQLConstants, ResourceStrings,
  BaseCompositeComponent, LinkedControls, DataStringGrid, ControlStringGrid, ExternalFilter;

type
  EReferencesError = class(TExceptionPath);
  EReferenceNotFound = class(EReferencesError);

  TSortRefsBy = (sbAuthor, sbYear, sbTitle);
  TReferenceType = (rtNotSelected, rtJournal, rtBook, rtSymposium, rtBulletin,
                    rtMemoirs, rtPublishedReport, rtUnpublishedReport,
                    rtThesis, rtNotebook, rtEphemera, rtOther);
                    { NB: rtNotSelected must come first; otherwise order is not
                          significant (but see also REFERENCE_TYPES below). }

  // provide access to protected TDBGrid
  TDBGridAccessor = class(TDBGrid);

  TfrmReferences = class(TBaseChild)
    pnlButtons: TPanel;
    pmRelatedData: TPopupMenu;
    mnuRelSurveys: TMenuItem;
    mnuRelEvents: TMenuItem;
    mnuRelSamples: TMenuItem;
    mnuRelIndivOrg: TMenuItem;
    mnuRelOccur: TMenuItem;
    mnuRelLocations: TMenuItem;
    pnlButtons2: TPanel;
    bbRelatedData: TBitBtn;
    ReferenceSplitter: TSplitter;
    pnlReferences: TPanel;
    pnlList: TPanel;
    pnlLabel: TPanel;
    lblDocuments: TLabel;
    mnuEdit: TMenuItem;
    mnuEditAdd: TMenuItem;
    mnuEditEdit: TMenuItem;
    mnuEditDelete: TMenuItem;
    N3: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuEditTransferData: TMenuItem;
    N1: TMenuItem;
    mnuEditFind: TMenuItem;
    N2: TMenuItem;
    mnuEditBold: TMenuItem;
    mnuEditItalic: TMenuItem;
    mnuEditUnderline: TMenuItem;
    pmGrid: TPopupMenu;
    mnuEditSimpleFilter: TMenuItem;
    N4: TMenuItem;
    dlgOpen: TOpenDialog;
    pmAdd: TMenuItem;
    bbShowAll: TButton;
    alDocuments: TActionList;
    actFind: TAction;
    actFilter: TAction;
    actShowMetadata: TAction;
    actEditShowMetadata: TMenuItem;
    dbgReferences: TDBJNCCGrid;
    dsReferences: TDataSource;
    bbAdd: TImageListButton;
    bbEdit: TImageListButton;
    bbDelete: TImageListButton;
    pmHValidateItem: TMenuItem;
    pnlReferenceDetails: TPanel;
    pnlInner: TPanel;
    lblReference: TLabel;
    lblReferencePrompt: TLabel;
    pcReferenceDetails: TPageControl;
    tsGeneral: TTabSheet;
    bvlGeneralFrame: TBevel;
    lblAuthor: TLabel;
    lblYear: TLabel;
    lblFullReference: TLabel;
    lblRefType: TLabel;
    Label1: TLabel;
    lbAuthors: TListBox;
    dbreReference: TDBRichEdit;
    bbAuthorAdd: TImageListButton;
    bbAuthorEdit: TImageListButton;
    bbAuthorDel: TImageListButton;
    cmbReferenceType: TComboBox;
    eStorageLocation: TDBEdit;
    eYear: TVagueDateEdit;
    tsDetails: TTabSheet;
    bvlDetailsFrame: TBevel;
    lblJournalDesc: TLabel;
    lblVolume: TLabel;
    lblPart: TLabel;
    lblNumber: TLabel;
    lblEdition: TLabel;
    lblSupplement: TLabel;
    lblPages: TLabel;
    lblSymposium: TLabel;
    lblEditor: TLabel;
    lblPublisher: TLabel;
    lblPlace: TLabel;
    lblTitle: TLabel;
    lbEditors: TListBox;
    dbreTitle: TDBRichEdit;
    dblcJournal: TDBLookupComboBox;
    dbeVolume: TDBEdit;
    dbePart: TDBEdit;
    dbeNumber: TDBEdit;
    dbeSupplement: TDBEdit;
    dbeEdition: TDBEdit;
    dbeSymposium: TDBEdit;
    dbePublisher: TDBEdit;
    dbePublicationPlace: TDBEdit;
    dbePages: TDBEdit;
    bbEditorAdd: TImageListButton;
    bbEditorEdit: TImageListButton;
    bbEditorDel: TImageListButton;
    tsKeywords: TTabSheet;
    btnAddKeyword: TImageListButton;
    btnRemoveKeyword: TImageListButton;
    sgKeywords: TControlStringGrid;
    tsNumbers: TTabSheet;
    bvlNumbersFrame: TBevel;
    lblNumbers: TLabel;
    lblOriginalFile: TLabel;
    sgNumbers: TStringGrid;
    cmbNumberType: TComboBox;
    eOriginalFile: TEdit;
    bbNumberAdd: TImageListButton;
    bbNumberDel: TImageListButton;
    bbOriginalFileView: TImageListButton;
    bbOriginalFileAdd: TImageListButton;
    bbSave: TImageListButton;
    bbCancel: TImageListButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbRelatedDataClick(Sender: TObject);
    procedure bbAddClick(Sender: TObject);
    procedure bbEditClick(Sender: TObject);
    procedure ReferenceSplitterCanResize(Sender: TObject;
      var NewSize: Integer; var Accept: Boolean);
    procedure FormResize(Sender: TObject);
    procedure ReferenceSplitterPaint(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure sgNumbersDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure cmbNumberTypeExit(Sender: TObject);
    procedure bbOriginalFileAddClick(Sender: TObject);
    procedure bbOriginalFileViewClick(Sender: TObject);
    procedure bbAuthorAddClick(Sender: TObject);
    procedure bbEditorAddClick(Sender: TObject);
    procedure bbAuthorEditClick(Sender: TObject);
    procedure bbEditorEditClick(Sender: TObject);
    procedure bbAuthorDelClick(Sender: TObject);
    procedure bbEditorDelClick(Sender: TObject);
    procedure bbNumberAddClick(Sender: TObject);
    procedure bbNumberDelClick(Sender: TObject);
    procedure bbDeleteClick(Sender: TObject);
    procedure reEnter(Sender: TObject);
    procedure reExit(Sender: TObject);
    procedure cmbReferenceTypeChange(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure pnlReferenceDetailsResize(Sender: TObject);
    procedure sgNumbersClick(Sender: TObject);
    procedure dbePartNumberChange(Sender: TObject);
    procedure mnuRelLocationsClick(Sender: TObject);
    procedure mnuRelSurveysClick(Sender: TObject);
    procedure mnuRelEventsClick(Sender: TObject);
    procedure mnuRelSamplesClick(Sender: TObject);
    procedure mnuRelOccurClick(Sender: TObject);
    procedure mnuRelIndivOrgClick(Sender: TObject);
    procedure bbShowAllClick(Sender: TObject);
    procedure eOriginalFileChange(Sender: TObject);
    procedure actFilterExecute(Sender: TObject);
    procedure pcReferenceDetailsChange(Sender: TObject);
    procedure pcReferenceDetailsChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure lbAuthorsClick(Sender: TObject);
    procedure lbEditorsClick(Sender: TObject);
  //  procedure sgReferencesTopLeftChanged(Sender: TObject);
    procedure actShowMetadataExecute(Sender: TObject);
    procedure dbgReferencesKeyPress(Sender: TObject; var Key: Char);
    procedure dbComboKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dbComboKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dbComboClick(Sender: TObject);
    procedure dbgReferencesAfterScroll(DataSet: TDataSet);
    procedure dbgReferencesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dbgReferencesGetCellColor(Sender: TObject; DataCol: Integer; Column:
        TColumn; State: TGridDrawState);
    procedure sgKeywordsClick(Sender: TObject);
    procedure btnAddKeywordClick(Sender: TObject);
    procedure btnRemoveKeywordClick(Sender: TObject);
    procedure tsKeywordsShow(Sender: TObject);
  private
    FdmReferences  : TdmReferences;
    FDeletingItem  : boolean;
    FAuthorList    : TAuthorList;
    FEditorList    : TEditorList;
    FISBNList      : TISBNList;
    FReferenceText : string;
    FReferenceType : integer;
    FCurrentReference : TKeyDataSysSupplied;
    FWhereClause : string;
    FKeyDown: Boolean;
    FKeywordsGridManager: TDataStringGrid;
    FMappedReferenceTypes: TStringList;
    procedure EnableDetails(const NewMode:TEditMode);
    procedure SetMenuState;
    procedure PopulateGrid;
    procedure PopulateDetail(const AKey: TKeyString);
    procedure ShowDetails(const Journal, Part, Number, Pages, Supplement, Edition,
      Symposium, Editor, Publisher, Place: Boolean);
    procedure DragReference(const Sender : TObject; var oDropSource : TJNCCDropSource);
    procedure CreateFullReference;
    procedure SetupObjects;
    procedure FreeObjects;
    procedure RelatedObservations(const iMsg:string; const iRelType:TRelatedData;
      const SQLType:TStandardSQL);
    procedure WMRefreshColours(var Msg: TMessage); message WM_REFRESH_COLOURS;
    procedure WMRefreshTermLists(var Msg: TMessage); message WM_REFRESH_TERM_LISTS;
    procedure RemoveLineBreaksFromTitle;
    function CheckDeletedRef(const AMode: TEditMode): boolean;
    procedure HideAppropriateDetailControls;
    class function GetReferenceCustodian(const AKey: TKeyString): string;
    procedure PopulateKeywords(const ARefKey: string);
    procedure KeywordFindData(const AInitialText: string; var AText, AKey: string);
    procedure KeywordGetData;
    procedure KeywordUpdateRow(var ioRowKey : string; iData : TStringList);
    procedure ValidateKeywords;
    procedure WMTransferDone(var Msg: TMessage); message WM_TRANSFER_DONE;
    procedure InitPeopleList(AList: TPeopleList);
    procedure PopulateReferenceTypes;
    procedure UpdateKeyword(Keylist: TKeylist);
    procedure KeywordDeleteRow(iRowKey : string);
    procedure DropKeyword(const Sender: TObject; const iFormat: integer; const
        iSourceData: TKeyList; const iTextStrings: TstringList; var ioHandled:
        boolean);
  protected
    procedure SetupDestinationControls; override;
    procedure RefreshReferencesGrid;
    procedure DisplayDetailsAtSelectedRow;
    procedure DisplayFirstReferenceItem;
    function GetPopulateSQL : string;
    function GetCurrentControlEditMode: TEditMode; override;
    function SelectedReferenceType: TReferenceType; virtual;
    function GetDetailsPageControl: TPageControl; override;

    procedure ApplyFilter(AKeyList: TKeyList); override;
    procedure DoShowHint(var HintStr: String; var CanShow: Boolean; var HintInfo: THintInfo);
        override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySecurity; override;
    function GetKeyList: TKeyList; override;
    function GetKeyListForValidation: TKeyList; override;
    procedure FindAndDisplaySource(iSourceKey : TKeyString);
    procedure DisplaySources(const iSourceList:TKeyList);
    procedure DisplayFilteredSources;
    function SelectNode(const ANodeType, ANodeKey: string): TFlyNode; override;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, TextInput, Maintbar, ApplicationSettings, Locations, IndOrg,
  MetadataPopup, DropTarget, Types;

// START resource string wizard section
resourcestring
  SReferences_ADocumentTypeMustBeSelectedForEa = 'A Document Type must be selected for each document.';
  SReferences_AreYouSureYouWantToDeleteThisAut = 'Are you sure you want to delete this Author?';
  SReferences_AreYouSureYouWantToDeleteThisDoc = 'Are you sure you want to delete this Document?';
  SReferences_AreYouSureYouWantToDeleteThisEdi = 'Are you sure you want to delete this Editor?';
  SReferences_AreYouSureYouWantToDeleteThisNum = 'Are you sure you want to delete this Number?';
  SReferences_ATitleMustBeEnteredForEachDocume = 'A Title must be entered for each document.';
  SReferences_AtLeastOneAuthorMustBeEnteredFor = 'At least one author must be entered for each document.';
  SReferences_AYearMustBeEnteredForEachDocumen = 'A year must be entered for each document.';
  SReferences_CannotDisplayFirstItemInGrid = 'Cannot display first item in grid: ';
  SReferences_ErrorOnPopulatingReferencesGrid = 'Error on populating references grid: ';
  SReferences_InOrderToEnterFurtherDetailsTheD = 'In order to enter further details, the Document Type has to be specified.';
  SReferences_TheDate = 'The Date ';
  SReferences_TheDocumentWasNotFoundInTheList = 'The document was not found in the list.';
  SReferences_TheNumberAndTheTypeAreBothRequir = 'The Number and the Type are both required for all document numbers.';
  SReferences_ThereAreNoDocumentsMatchingTheFi = 'There are no Documents matching the filtering condition.';
  SReferences_ThereAreNoIndividualsOrOrganisat = 'There are no Individuals or Organisations to relate this document to.';
  SReferences_ThereAreNoLocationsToRelateThisD = 'There are no Locations to relate this document to.';
  SReferences_ThisNameIsAlreadyPresentInTheLis = 'This name is already present in the list.';
  SReferences_Keywords                         = 'Keywords';
  SReferences_SpecifyValidKeyword              = 'Please specify a valid keyword.';
  SReferences_ThisKeywordIsAlreadyPresentInThe = 'This keyword is already present in the list.';

  ResStr_DeletedFromDB = #13#13'It has been deleted from the database.';
  ResStr_NotValidInteger =  '%s is not a valid integer value.';
  ResStr_TooBigNumber = 'The value ''%s'' is too big. The maximum limit '+
                        'is 32767. Make sure the number entered doesn''t exceed this limit.';

  ResStr_NoDocToRelate =  'There are no %s to relate this document to.';
  ResStr_NoReferenceGrid =  'Query for references grid cannot run: Query string absent.';

  ResStr_Publisher           = 'Publisher';
  ResStr_Org                 = 'Org.';
  ResStr_AwardingInstitution = 'Awarding Institution';
  ResStr_PlaceOfPublication  = 'Place of Publication';
  ResStr_Place               = 'Place';
  ResStr_Volume              = 'Volume';
  ResStr_Part                = 'Part';
  ResStr_Pages               = 'Pages';
  ResStr_Supplement          = 'Supplement';
  ResStr_Edition             = 'Edition';
  ResStr_Editors             = 'Editors';
  ResStr_EditorSurname       = 'Editor''s Surname';
  ResStr_Authors             = 'Authors';
  ResStr_AuthorSurname       = 'Author''s Surname';
  ResStr_NewDocument         = '<New document>';
  ResStr_Journal             = 'Journal';
  ResStr_Symposium           = 'Symposium';

  // Transalatable versions of Reference Types.
  ResStr_ReferenceTypes =
      'Journal,Book,Symposium,Bulletin,Memoirs,Published,Unpublished,Thesis,Notebook,Ephemera,Other';

const
  // Original (untranslated) values for Reference Types. Used in DB.
  ST_REFERENCE_TYPES =
      'Journal,Book,Symposium,Bulletin,Memoirs,Published,Unpublished,Thesis,Notebook,Ephemera,Other';

function ListName(const initials: string; const name: string): string;
begin
  Result := name;
  if initials <> '' then Result := Result + ', ' + initials;
end;

//==============================================================================
constructor TfrmReferences.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  dbgReferences.OnGetCellColor := dbgReferencesGetCellColor;

  SetupObjects;
  FReferenceType := -1;

  { Set up the numbers grid }
  SetGridColumnTitles(sgNumbers, [ResStr_Number, ResStr_Type]);
  sgKeywords.Cells[0,0] := SReferences_Keywords;

  { Set up the References Grid }
  FWhereClause := '';
  FdmReferences.qryPopulate.SQL.Text := GetPopulateSQL;
  PopulateGrid;
  PopulateReferenceTypes;
  DisplayFirstReferenceItem;

  //Set mode
  EnableDetails(emView);
  pcReferenceDetails.ActivePage := tsGeneral;

  //Security
  pmAdd.Enabled:= (AppSettings.UserAccessLevel >= ualAddOnly);
  SendMessage(Handle,WM_UPDATE_MENU_ICONS,0,0);

  //Help Setup
  mnuEdit.HelpContext   := IDH_EDITMENU;
  Self.HelpContext      := IDH_DOCS;
  tsGeneral.HelpContext := IDH_DOCSGENERAL;
  tsDetails.HelpContext := IDH_DOCSDETAILS;
  tsNumbers.HelpContext := IDH_DOCSOTHER;
  tsKeywords.HelpContext:= IDH_DOCSKEYWORDS;
  pcReferenceDetails.HelpContext := IDH_DOCSGENERAL;

  FKeyDown := False;

  LoadFilter(FILTER_REFERENCE);
end;  // Create

//==============================================================================
procedure TfrmReferences.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(true);
  frmMain.SetcontextToolbar(Self,mnuEdit,4,[]);
  SetMenuState;

  dmFormActions.SetValidateSelectedAction(True);
end;  // FormActivate

//==============================================================================
procedure TfrmReferences.FormDeactivate(Sender: TObject);
begin
  inherited;
  dmFormActions.SetValidateSelectedAction(False);
end;  // FormDeactivate

//==============================================================================
procedure TfrmReferences.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  CanClose:=false;
  if EditMode<>emView then begin  // Add button disabled if adding or editing
    Beep;
    case ConfirmSaveAndClose of
      mrYes : begin
                bbSaveClick(nil);
                CanClose:=true;
              end;
      mrNo  : begin
                bbCancelClick(nil);
                CanClose:=true;
              end;
    end;
  end else
    CanClose:=true;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmReferences.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  // Close query for datagrid
  FdmReferences.qryPopulate.Close;
  FreeObjects;
  Action:=caFree;
end;  // FormClose

//==============================================================================
procedure TfrmReferences.SetupObjects;
begin
  //Load data module
  FdmReferences:= TdmReferences.Create(nil);
  //Open static data sets
  FdmReferences.qryJournal.Open;
  //Create data lists
  FAuthorList:= TAuthorList.Create(FdmReferences.qryAuthors, 'AUTHOR_KEY', lbAuthors.Items);
  FEditorList:= TEditorList.Create(FdmReferences.qryEditors, 'EDITOR_KEY', lbEditors.Items);
  InitPeopleList(FAuthorList);
  InitPeopleList(FEditorList);
  FISBNList  := TISBNList.Create(FdmReferences.qryISBNs, 'NUMBER_KEY', sgNumbers);
  with FISBNList do begin
    UserID := AppSettings.UserID;
    RestrictFullEdit := AppSettings.RestrictFullEdit;
    SiteID := AppSettings.SiteID;
    AddOnly := AppSettings.UserAccessLevel=ualAddOnly;
  end; // with
  // Create object to store key for the current reference
  FCurrentReference := TKeyDataSysSupplied.Create;
  FCurrentReference.ItemKey := '';
  FCurrentReference.SysSupplied := True;
  FdmReferences.qryPopulate.AfterScroll := dbgReferencesAfterScroll;

  FMappedReferenceTypes := TStringList.Create;
end;  // SetupObjects

//==============================================================================
procedure TfrmReferences.FreeObjects;
begin
  //Free data lists
  FreeAndNil(FISBNList);
  FreeAndNil(FEditorList);
  FreeAndNil(FAuthorList);

  //Free data module
  FdmReferences.qryPopulate.AfterScroll := nil;
  FreeAndNil(FdmReferences);

  // Free storage object
  FreeAndNil(FCurrentReference);

  FMappedReferenceTypes.Free;

  AppSettings.ClearFilteredRecords([TN_REFERENCE]);
end;  // FreeObjects

//==============================================================================
procedure TfrmReferences.ReferenceSplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  inherited;
  Accept := (NewSize > ReferenceSplitter.MinSize) and
            ((ClientWidth - NewSize > dbgReferences.Columns[1].Width + ReferenceSplitter.Width + 4) or
             ((pnlReferences.Width < dbgReferences.Columns[1].Width + 4) and
              (NewSize <= pnlReferenceDetails.Width)));
end;  // ReferenceSplitterCanResize

//==============================================================================
procedure TfrmReferences.FormResize(Sender: TObject);
begin
  inherited;
  if ReferenceSplitter.Left < ReferenceSplitter.MinSize then begin
    pnlReferenceDetails.Width := ClientWidth - ReferenceSplitter.MinSize - ReferenceSplitter.Width;
    if pnlReferenceDetails.Left + pnlReferenceDetails.Width > ClientWidth then
      Redraw;
  end;
  pnlReferenceDetailsResize(Sender);
end;  // FormResize

//==============================================================================
function TfrmReferences.CheckDeletedRef(const AMode:TEditMode):boolean;
var AMessage :string;
begin
  if dmGeneralData.CheckKeyExists(TN_SOURCE, 'Source_Key', FCurrentReference.ItemKey)
  then
    Result := True
  else begin
    Result:=false;
    case AMode of
      emView   : AMessage:=ResStr_CannotAccessRecord;
      emEdit   : AMessage:=ResStr_CannotEditRecord;
      emDelete : AMessage:=ResStr_CannotDeleteRecord;
    end;
    MessageDlg(AMessage + ResStr_DeletedFromDB,
               mtInformation,[mbOk],0);
    dbgReferences.Refresh;
    EnableDetails(emView);
  end; // if not CheckKeyExists
end;  // CheckDeletedRef

//==============================================================================
procedure TfrmReferences.ReferenceSplitterPaint(Sender: TObject);
begin
  inherited;
  DrawVertSplitter(Canvas, ReferenceSplitter);
end;  // ReferenceSplitterPaint

//==============================================================================
procedure TfrmReferences.bbRelatedDataClick(Sender: TObject);
var PosPopup:TPoint;
begin
  inherited;
  PosPopup:=ClientToScreen(Point(pnlButtons2.Left+bbRelatedData.Left,
                                 pnlButtons.Top+bbRelatedData.Top+bbRelatedData.Height));
  pmRelatedData.Popup(PosPopup.X,PosPopup.Y);
end;  // bbRelatedDataClick

class function TfrmReferences.GetReferenceCustodian(const AKey: TKeyString): string;
begin
  Result := dmGeneralData.Custodian(TN_SOURCE, 'SOURCE_KEY', AKey);
end;

//==============================================================================
procedure TfrmReferences.SetMenuState;
var tfOn : boolean;
    lKeyData : TKeyDataSysSupplied;
begin
  if Assigned(FCurrentReference) and
     (FCurrentReference.ItemKey <> '')
  then
    lKeyData := FCurrentReference
  else
    lKeyData := nil;

  tfOn := (EditMode=emView) and Assigned(lKeyData);

  pmGrid.AutoPopup := (EditMode=emView);

  bbAdd.Enabled         := (EditMode=emView) and AddButtonState;
  if Assigned(lKeyData) then begin
    bbEdit.Enabled        := tfOn and
                           (EditButtonState(lKeyData, GetReferenceCustodian) or
                           (AppSettings.UserAccessLevel >= ualAddOnly));
    bbDelete.Enabled      := tfOn and DeleteButtonState(lKeyData) and
        dmGeneralData.HasFullEditAccess(TN_REFERENCE, 'SOURCE_KEY', lKeyData.ItemKey);
  end
  else begin
    bbEdit.Enabled := False;
    bbDelete.Enabled := False;
  end;
  bbRelatedData.Enabled := tfOn;
  bbShowAll.Enabled     := tfOn;

  mnuEditAdd.Enabled    := bbAdd.Enabled;
  mnuEditEdit.Enabled   := bbEdit.Enabled;
  mnuEditDelete.Enabled := bbDelete.Enabled;

  actFind.Enabled         := tfOn;
  actFilter.Enabled       := tfOn;
  actShowMetadata.Enabled := tfOn;
  dmFormActions.actTransferData.Enabled := tfOn and
                                           Assigned(RequestorForm);

  { Show reference details if current reference is valid }
  if Assigned(lKeyData) then
    pcReferenceDetails.Enabled := true
  else begin
    pcReferenceDetails.ActivePage := tsGeneral;
    pcReferenceDetails.Enabled    := false;
  end;

end;  // SetMenuState

//==============================================================================
procedure TfrmReferences.EnableDetails(const NewMode:TEditMode);
var tfOn:boolean;
begin
  FEditMode := NewMode;
  tfOn := (EditMode<>emView);

  SetRequiredFieldsColourState(tfOn, [lbAuthors, eYear, cmbReferenceType, dbreTitle]);
  cmbReferenceType.Enabled := (NewMode=emAdd) or ((NewMode=emEdit) and HaveCustody);
  eYear.ReadOnly := not cmbReferenceType.Enabled;
  bbAuthorAdd.Enabled := tfOn;
  lbAuthorsClick(nil);

  lbEditors.Enabled   := tfOn;
  bbEditorAdd.Enabled := tfOn;
  lbEditorsClick(nil);

  with sgNumbers do
    if tfOn then Options:=Options-[goRowSelect]
            else Options:=Options+[goRowSelect];

  bbNumberAdd.Enabled:=tfOn;
  sgNumbersClick(nil);

  eOriginalFile.ReadOnly := not tfOn;
  bbOriginalFileAdd.Enabled := tfOn;

  if tfOn then dbreTitle.PopupMenu:=dmFormActions.pmRTF
          else dbreTitle.PopupMenu:=nil;

  bbSave.Enabled   :=tfOn;
  bbCancel.Enabled :=tfOn;

  //Additional Pages
  SetAdditionalPagesState(tfOn);

  SetMenuState;
  sgKeywords.Row := 1; // Select the first row (0 is the header).
  FKeywordsGridManager.Enabled := tfOn;
  btnAddKeyword.Enabled := tfOn;
  btnRemoveKeyword.Enabled := tfOn
      and not FKeywordsGridManager.RowLocked(sgKeywords.Row);
  sgKeywords.Refresh;
end;  // EnableDetails

//==============================================================================
procedure TfrmReferences.bbAddClick(Sender: TObject);
var lNewKey: TKeyString;
begin
  inherited;
  pcReferenceDetails.ActivePage := tsGeneral;

  // Get a new key for the Source
  lNewKey := dmGeneralData.GetNextKey(TN_SOURCE, 'Source_Key');
  FCurrentReference.ItemKey := lNewKey; // Store it for use later
  FReferenceType := -1;

  { Add COM addin page }
  AddAdditionalPages;

  with FdmReferences.qryReference do begin
    if not Active then
      Open;
    Append;
    FieldByName('SOURCE_KEY').AsString  := lNewKey;
  end;
  ResetDBRichEditControls([dbreReference, dbreTitle]);
  dmGeneralData.SetNameIDAndDate(FdmReferences.qryReference, 'Entered_By', 'Entry_Date');

  lblReference.Caption:= ResStr_NewDocument;
  FdmReferences.qryAuthors.Parameters.ParamByName('Key').Value := lNewKey;
  FdmReferences.qryEditors.Parameters.ParamByName('Key').Value := lNewKey;
  FdmReferences.qryISBNs.Parameters.ParamByName('Key').Value   := lNewKey;
  FAuthorList.NameKey:= lNewKey;
  FEditorList.NameKey:= lNewKey;
  FISBNList.NameKey  := lNewKey;
  FAuthorList.Refresh;
  FEditorList.Refresh;
  FISBNList.Refresh;
  cmbReferenceType.ItemIndex := -1;
  eYear.Text := '';
  eOriginalFile.Text := '';

  EnableDetails(emAdd);
end;  // bbAddClick

//==============================================================================
procedure TfrmReferences.bbEditClick(Sender: TObject);
begin
  inherited;
  if dmGeneralData.HasFullEditAccess(TN_REFERENCE, 'Source_Key', FCurrentReference.ItemKey) and
      HaveCustody then
    try
      if CheckDeletedRef(emEdit) then begin
        FdmReferences.qryReference.Edit;
        dmGeneralData.SetNameIDAndDate(FdmReferences.qryReference,'Changed_By','Changed_Date');
        EnableDetails(emEdit);
      end;
    except
      on E:Exception do
        if dmDatabase.CheckError(E, dbeRecordLocked) then begin
          MessageDlg(ResStr_CannotEditRecord + #13#13 +
                     dmDatabase.GetErrorMessage(E.Message, dbeRecordLocked),
                     mtInformation, [mbOk], 0);
          dbgReferences.Refresh;
        end else
          Raise;
    end
  else
    EnableDetails(emEdit);
end;  // bbEditClick

//==============================================================================
procedure TfrmReferences.bbDeleteClick(Sender: TObject);
var lKeyToDelete, lKeyForDisplay: TKeyString;
begin
  inherited;
  if CheckDeletedRef(emDelete) then
    try
      with FdmReferences do begin
        // See if anyone is editing the record, and if not, go on and delete it
        qryReference.Edit;
        qryReference.Cancel;

        if MessageDlg(SReferences_AreYouSureYouWantToDeleteThisDoc,
                      mtConfirmation,[mbYes,mbNo],0)=mrYes then
        begin
          // Remember the key to delete, as next few lines change content of FCurrentReference!
          lKeyToDelete   := FCurrentReference.ItemKey;
          // Use this to set current selection to previous item, if not at top of grid
          lKeyForDisplay := '';
          if not qryPopulate.Bof then begin
            qryPopulate.Prior;                                       // Move up one
            lKeyForDisplay := qryPopulate.FieldByName('Source_Key').AsString;  // Get key
          end;

          DeleteReference(lKeyToDelete);
          PopulateGrid;
          if lKeyForDisplay = '' then
            DisplayFirstReferenceItem  // Deleted first record, so display first in grid
          else
            FindAndDisplaySource(lKeyForDisplay);  // Display record that was above deleted one.
        end;
      end;
    except
      on E:Exception do
        if dmDatabase.CheckError(E, dbeRecordLocked) then begin
          MessageDlg(ResStr_CannotDeleteRecord + #13#13 +
                     dmDatabase.GetErrorMessage(E.Message, dbeRecordLocked),
                     mtInformation, [mbOk], 0);
          dbgReferences.Refresh;
        end else
          Raise;
    end;
end;  // bbDeleteClick

//==============================================================================
procedure TfrmReferences.bbSaveClick(Sender: TObject);
var lCount      : Integer;
    lCursor     : TCursor;
    lCurrentTab : TTabSheet;
    lHaveCustody: Boolean;
begin
  inherited;
  // Sender is nil if called from CloseQuery method.
  (************if (Sender = nil) then ValidateDate;*)

  { Check required fields }
  lCurrentTab := pcReferenceDetails.ActivePage;
  pcReferenceDetails.ActivePage := tsGeneral;
  ValidateValue( (lbAuthors.Items.Count > 0),
                 SReferences_AtLeastOneAuthorMustBeEnteredFor,
                 bbAuthorAdd );

  ValidateValue( (cmbReferenceType.Text <> ''),
                 SReferences_ADocumentTypeMustBeSelectedForEa,
                 cmbReferenceType );
  ValidateValue( (eYear.Text <> ''),
                 SReferences_AYearMustBeEnteredForEachDocumen,
                 eYear );
  ValidateValue(IsVagueDate(eYear.Text),
              InvalidDate(SReferences_TheDate,true,false),eYear);
  pcReferenceDetails.ActivePage := tsDetails;
  ValidateValue( (dbreTitle.Lines.Text <> ''),
                 SReferences_ATitleMustBeEnteredForEachDocume,
                 dbreTitle );

  ValidateKeywords;

  //Check all rows have both values
  pcReferenceDetails.ActivePage := tsNumbers;
  for lCount:=0 to FISBNList.ItemCount-1 do
    ValidateValue( (TISBNItem(FISBNList[lCount]).Number<>'') and
                   (TISBNItem(FISBNList[lCount]).TypeString<>''),
                   SReferences_TheNumberAndTheTypeAreBothRequir,
                   sgNumbers );
  pcReferenceDetails.ActivePage := lCurrentTab;

  { Call to validate COM addin page... }
  if not CheckAdditionalPageCanSave then Exit;

  lCursor := HourglassCursor;
  try
    lHaveCustody := dmGeneralData.HaveCustody(TN_SOURCE, 'SOURCE_KEY', FCurrentReference.ItemKey);
    { Build the data for the new reference and post it to the database }
    if FdmReferences.qryReference.State in [dsInsert, dsEdit] then
    begin
      RemoveLineBreaksFromTitle;
      CreateFullReference;
      // Insert record in Source table only if adding new reference
      if (EditMode = emAdd) then
        with FdmReferences.qrySource do begin
          Parameters.ParamByName('Key').Value := FCurrentReference.ItemKey;
          ExecSQL;
          // Ensure addins know new key
          DoFormEventAddinChanges(TN_REFERENCE, FCurrentReference.ItemKey);
        end; // with FdmReferences.qrySource

      with FdmReferences.qryReference do begin
        FieldByName('Original_File').AsString := eOriginalFile.Text;
        FieldByName('Year_Vague_Date_Start').Text := eYear.Text;
        Post;
      end; // with FdmReferences.qryReference
    end else
    if (EditMode = emEdit) and (lHaveCustody <> HaveCustody) then begin
      FdmReferences.qryReference.Cancel;
      MessageDlg(Format(ResStr_CustodyChanged, ['Reference']), mtWarning, [mbOk], 0);
    end;

    FAuthorList.Update;
    FEditorList.Update;
    FISBNList.Update;
    FKeywordsGridManager.Save;
    // Additional Pages
    SaveAdditionalPages;

    EnableDetails(emView);
    RefreshReferencesGrid;
    FindAndDisplaySource(FCurrentReference.ItemKey);
  finally
    DefaultCursor(lCursor);
    // Send message to all open forms that the documents window has changed
    frmMain.BroadcastMessage(WM_REFRESH_DOCUMENTS);
  end;
end;  // bbSaveClick

{-------------------------------------------------------------------------------
}
procedure TfrmReferences.bbCancelClick(Sender: TObject);
begin
  inherited;
  // Forget about any incipient source record
  if (FdmReferences.qryReference.State = dsInsert) then
    FCurrentReference.ItemKey := '';
  FdmReferences.qryReference.Cancel;
  eOriginalFile.Text := FdmReferences.qryReference.FieldByName('ORIGINAL_FILE').AsString;
  //Additional Pages
  CancelAdditionalPages;
  EnableDetails(emView);
  FindAndDisplaySource(FCurrentReference.ItemKey);
end;  // bbCancelClick

//==============================================================================
procedure TfrmReferences.sgNumbersClick(Sender: TObject);
var
  lDataItem: TISBNItem;
  lCanEdit: Boolean;
begin
  inherited;
  if (EditMode = emView) then begin
    bbNumberDel.Enabled   := false;
    cmbNumberType.Visible := false;
  end
  else begin
    // Add a new default item only if event triggered by user
    if (FISBNList.ItemCount=0) and (Sender is TStringGrid) then begin
      lDataItem:=TISBNItem.CreateNew(FISBNList);
      if cmbNumberType.Items.Count>0 then
        lDataItem.TypeString := cmbNumberType.Items[0];
      FISBNList.AddNew(lDataItem);
      sgNumbers.Row := sgNumbers.RowCount - 1;
      sgNumbers.Col := 0;
    end;

    lDataItem := TISBNItem(sgNumbers.Objects[0, sgNumbers.Row]);
    if lDataItem = nil then begin
      bbNumberDel.Enabled := False;
      sgNumbers.Options := sgNumbers.Options - [goEditing];
    end
    else begin
      cmbNumberType.Visible := false;
      bbNumberDel.Enabled := AppSettings.AllowEdit(EditMode) and
           lDataItem.CanDelete and (lDataItem.Added or
           (lDataItem.Custodian = AppSettings.SiteID));
      if (not bbNumberDel.Enabled) or (sgNumbers.Col = 1) then
        sgNumbers.Options := sgNumbers.Options - [goEditing]
      else begin
        { check for custody of number record }
        lCanEdit := lDataItem.Added or (lDataItem.Custodian = AppSettings.SiteID)
                 and lDataItem.CanEdit;
        if lCanEdit then
          sgNumbers.Options := sgNumbers.Options + [goEditing]
        else
          sgNumbers.Options := sgNumbers.Options - [goEditing];
      end;
    end; // lDataItem <> nil
  end;  // EditMode <> emView
end;  // sgNumbersClick

//------------------------------------------------------------------------------
procedure TfrmReferences.sgNumbersDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var lDataItem: TISBNItem;
begin
  inherited;
  if (EditMode <> emView) and (FISBNList.ItemCount > 0) then begin
    lDataItem := sgNumbers.Objects[0, ARow] as TISBNItem;
    if Assigned(lDataItem) and (not FDeletingItem) then begin
      if sgNumbers.Cells[0, ARow] <> lDataItem.Number then
          lDataItem.Number := sgNumbers.Cells[0, ARow];
    end;

    if FDeletingItem or (ACol <> 1) or (ARow = 0) or
       (not (gdFocused in State)) then
      cmbNumberType.Visible := False
    else begin
      { TODO: this could be optimised away by calculating custody
              when grid is loaded, and storing in a hidden column }
      cmbNumberType.Visible := (lDataItem.Added or
                               dmGeneralData.HaveCustody('REFERENCE_NUMBER',
                                                         'NUMBER_KEY',
                                                         lDataItem.ItemKey))
                               and lDataItem.CanEdit;
      if cmbNumberType.Visible then begin
        cmbNumberType.Left     := Rect.Left + sgNumbers.Left + 1;
        cmbNumberType.Top      := Rect.Top + sgNumbers.Top + 1;
        cmbNumberType.Width    := Rect.Right - Rect.Left + 2;
        cmbNumberType.ItemIndex :=
            cmbNumberType.Items.IndexOf(sgNumbers.Cells[1, ARow]);
        cmbNumberType.Text :=
            TISBNItem(sgNumbers.Objects[0, ARow]).TypeString;
      end;
    end;
  end;
  sgNumbers.Canvas.FillRect(Rect);
  DrawChoppedText(sgNumbers.Cells[ACol, ARow], sgNumbers.Canvas, Rect, 2);
end;  // sgNumbersDrawCell

//------------------------------------------------------------------------------
procedure TfrmReferences.bbNumberAddClick(Sender: TObject);
var lNewISBN: TISBNItem;
begin
  inherited;
  with sgNumbers do
    if Cells[0,RowCount-1]<>'' then begin
      lNewISBN:= TISBNItem.CreateNew(FISBNList);
      if cmbNumberType.Items.Count>0 then
        lNewISBN.TypeString:=cmbNumberType.Items[0];
	    FISBNList.AddNew(lNewISBN);
    end;
end;  // bbNumberAddClick

//------------------------------------------------------------------------------
procedure TfrmReferences.bbNumberDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(SReferences_AreYouSureYouWantToDeleteThisNum,
                mtConfirmation,[mbYes,mbNo],0)=mrYes then begin
    FDeletingItem:=true;
    FISBNList.DeleteItem(sgNumbers.Row);
    FDeletingItem:=false;
    sgNumbersClick(nil);
  end;
end;  // bbNumberDelClick

//==============================================================================
procedure TfrmReferences.cmbNumberTypeExit(Sender: TObject);
var
  item: TISBNItem;
begin
  inherited;
  cmbNumberType.Visible := False;
  if Assigned(sgNumbers.Objects[0, sgNumbers.Row]) then
  begin
    item := TISBNItem(sgNumbers.Objects[0, sgNumbers.Row]);
    if cmbNumberType.Text <> item.TypeString then
      item.TypeString := cmbNumberType.Text;
  end;
end;  // cmbNumberTypeExit

//==============================================================================
procedure TfrmReferences.bbOriginalFileAddClick(Sender: TObject);
begin
  inherited;
  if dlgOpen.Execute then
    eOriginalFile.Text:= dlgOpen.FileName;
end;  // bbOriginalFileAddClick

//==============================================================================
procedure TfrmReferences.bbOriginalFileViewClick(Sender: TObject);
begin
  ShellFile( eOriginalFile.Text );
end;  // bbOriginalFileViewClick

//==============================================================================
procedure TfrmReferences.PopulateGrid;
var
  lCursor: TCursor;
  oldkey: TKeyString;
begin
  lCursor := HourglassCursor;
  with FdmReferences.qryPopulate do begin
    try
      if not Active then
        Open
      else begin  // rebuild sql in case we now have more/less maximum authors
        oldkey := FCurrentReference.ItemKey;
        Close;
        SQL.Text := GetPopulateSQL;
        Open;
        FdmReferences.qryPopulate.Locate('SOURCE_KEY', oldkey, []);
      end;
    except
      on E:Exception do begin
        Close;
        SQL.Text := GetPopulateSQL;
        raise EReferenceNotFound.Create(SReferences_ErrorOnPopulatingReferencesGrid+E.message);
      end; // on exception
    end; // try..finally
  end; // with qryPopulate

  // Hide the first column (which contains the key for the source)
  with dbgReferences do begin
    Columns[0].Visible       := False; // Key for Reference
    Columns[1].Width         := 100;
    Columns[1].Title.Caption := ResStr_Cap_Author ;
    Columns[2].Width         := 70;
    Columns[2].Title.Caption := ResStr_Cap_Year;
    Columns[3].Width         := Max(dbgReferences.width - 140, 50);
    Columns[3].Title.Caption := ResStr_Cap_Title;
    Columns[4].Visible       := False; // Whether Reference is System Supplied
  end;
  if FdmReferences.qryPopulate.RecordCount=0 then
    PopulateDetail('');

  DefaultCursor(lCursor);
end;  // PopulateGrid

//==============================================================================
procedure TfrmReferences.PopulateDetail(const AKey: TKeyString);
var
  lReferenceType: String;
begin
  with FdmReferences do begin
    with qryReference do begin
      Close;
      Parameters.ParamByName('Key').Value := AKey;
      Open;
      FieldByName('Volume').Alignment := taLeftJustify;
      FieldByName('Part').Alignment   := taLeftJustify;
      FieldByName('number').Alignment := taLeftJustify;
      lReferenceType     := FieldByName('REFERENCE_TYPE').AsString;
      eOriginalFile.Text := FieldByName('ORIGINAL_FILE').AsString;
      eYear.Text         := FieldByName('Year_Vague_Date_Start').Text;
    end;
    FCustodian := GetReferenceCustodian(AKey);

    qryAuthors.Parameters.ParamByName('Key').Value := AKey;
    qryEditors.Parameters.ParamByName('Key').Value := AKey;
    qryISBNs.Parameters.ParamByName('Key').Value   := AKey;

    FAuthorList.NameKey := AKey;
    FEditorList.NameKey := AKey;
    FISBNList.NameKey   := AKey;
    FAuthorList.Refresh;
    FEditorList.Refresh;
    FISBNList.Refresh;

    FReferenceType := cmbReferenceType.Items.IndexOf(FMappedReferenceTypes.Values[lReferenceType]);
    cmbReferenceType.ItemIndex := FReferenceType;

    eOriginalFile.ReadOnly := True;
  end;
  PopulateKeywords(AKey);
  if (AKey<>'') then begin
    ChangeAdditionalPage(pcReferenceDetails);
    NotifyDataItemChange;  // Notify COM Addins of a change in data items in grid
  end;
end;  // PopulateDetails

//==============================================================================
procedure TfrmReferences.lbAuthorsClick(Sender: TObject);
var Selected: TAuthorItem;
begin
  inherited;
  if FAuthorList.ItemCount=0 then begin
    bbAuthorEdit.Enabled:=false;
    bbAuthorDel.Enabled :=false;
  end
  else begin
    if lbAuthors.ItemIndex=-1 then lbAuthors.ItemIndex := 0;
    bbAuthorDel.Enabled := AppSettings.AllowEdit(EditMode);
    if not bbAuthorDel.Enabled then
      bbAuthorEdit.Enabled := False
    else begin
      { check for custody of author record }
      Selected := lbAuthors.Items.Objects[lbAuthors.ItemIndex] as TAuthorItem;
      bbAuthorEdit.Enabled := Selected.Added or (Selected.Custodian = AppSettings.SiteID)
                           and Selected.CanEdit;
      bbAuthorDel.Enabled := bbAuthorDel.Enabled and Selected.CanDelete
          and (Selected.Added or (Selected.Custodian = AppSettings.SiteID));
    end;
  end;
end;  // lbAuthorsClick

//==============================================================================
procedure TfrmReferences.bbAuthorAddClick(Sender: TObject);
var lNewAuthor: TAuthorItem;
begin
  inherited;
  with TdlgTextInput.Create(nil) do
    try
      Caption:= ResStr_Authors;
      lblSubject.Caption:= ResStr_AuthorSurname + ':';

      if ShowModal = mrOk then begin
        // Check the name is not already in the list
        if lbAuthors.Items.IndexOf(ListName(eInitials.Text, eName.Text))=-1 then
        begin
          lNewAuthor:= TAuthorItem.CreateNew(FAuthorList);
          lNewAuthor.Name    := eName.Text;
          lNewAuthor.Initials:= eInitials.Text;
          FAuthorList.AddNew(lNewAuthor);
        end else
          MessageDlg(SReferences_ThisNameIsAlreadyPresentInTheLis,mtInformation,[mbOk],0);
        lbAuthorsClick(nil);
      end;
    finally
      Free;
    end;
end;  // bbAuthorAddClick

//==============================================================================
procedure TfrmReferences.bbAuthorEditClick(Sender: TObject);
var lCurrentAuthor:TAuthorItem;
    lIdx, lIdx2   :integer;
begin
  inherited;
  lIdx:=lbAuthors.ItemIndex;
  if lIdx<>-1 then begin
    lCurrentAuthor:= TAuthorItem(lbAuthors.Items.Objects[lbAuthors.ItemIndex]);
    with TdlgTextInput.Create(nil) do
      try
        Caption:= ResStr_Authors;
        lblSubject.Caption:= ResStr_AuthorSurname + ':';
        eName.Text    := lCurrentAuthor.Name;
        eInitials.Text:= UpperCase(lCurrentAuthor.Initials);

        if ShowModal = mrOK then begin
          // Check the name is not already in the list
          lIdx2:=lbAuthors.Items.IndexOf(ListName(eInitials.Text, eName.Text));
          if (lIdx2=-1) or (lIdx2=lIdx) then begin
            lCurrentAuthor.Name:= eName.Text;
            lCurrentAuthor.Initials:= UpperCase(eInitials.Text);
          end else
            MessageDlg(SReferences_ThisNameIsAlreadyPresentInTheLis,mtInformation,[mbOk],0);
          lbAuthorsClick(nil);
        end;
      finally
        Free;
      end;
  end;
end;  // bbAuthorEditClick

//==============================================================================
procedure TfrmReferences.bbAuthorDelClick(Sender: TObject);
begin
  inherited;
  if lbAuthors.ItemIndex<>-1 then
    if MessageDlg(SReferences_AreYouSureYouWantToDeleteThisAut,
                  mtConfirmation,[mbYes,mbNo],0)=mrYes then
    begin
      FAuthorList.DeleteItem(lbAuthors.ItemIndex);
      lbAuthorsClick(nil);
    end;
end;  // bbAuthorDelClick

//==============================================================================
procedure TfrmReferences.lbEditorsClick(Sender: TObject);
var Selected: TEditorItem;
begin
  inherited;
  if FEditorList.ItemCount = 0 then begin
    bbEditorEdit.Enabled := False;
    bbEditorDel.Enabled := False;
  end
  else begin
    if lbEditors.ItemIndex=-1 then lbEditors.ItemIndex := 0;
    bbEditorDel.Enabled := AppSettings.AllowEdit(EditMode);
    if not bbEditorDel.Enabled then
      bbEditorEdit.Enabled := False
    else begin
      { check for custody of editor record }
      Selected := lbEditors.Items.Objects[lbEditors.ItemIndex] as TEditorItem;
      bbEditorEdit.Enabled := Selected.Added or (Selected.Custodian = AppSettings.SiteID)
                           and Selected.CanEdit;
      bbEditorDel.Enabled := bbEditorDel.Enabled and Selected.CanDelete;
    end;
  end;
end;  // lbEditorsClick

//==============================================================================
procedure TfrmReferences.bbEditorAddClick(Sender: TObject);
var lNewEditor: TEditorItem;
begin
  inherited;
  with TdlgTextInput.Create(nil) do
    try
      Caption:= ResStr_Editors;
      lblSubject.Caption:= ResStr_EditorSurname + ':';

      if ShowModal = mrOk then
        // Check the name is not already in the list
        if lbEditors.Items.IndexOf(ListName(eInitials.Text, eName.Text))=-1 then begin
          lNewEditor:= TEditorItem.CreateNew(FEditorList);
          lNewEditor.Name:= eName.Text;
          lNewEditor.Initials:= eInitials.Text;
          FEditorList.AddNew(lNewEditor);
        end else
          MessageDlg(SReferences_ThisNameIsAlreadyPresentInTheLis,mtInformation,[mbOk],0);
        lbEditorsClick(nil);
    finally
      Free;
    end;
end;  // bbEditorAddClick

//==============================================================================
procedure TfrmReferences.bbEditorEditClick(Sender: TObject);
var lCurrentEditor: TEditorItem;
begin
  inherited;
  if lbEditors.ItemIndex<>-1 then begin
    lCurrentEditor:= TEditorItem(lbEditors.Items.Objects[lbEditors.ItemIndex]);
    with TdlgTextInput.Create(nil) do
      try
        Caption:= ResStr_Editors;
        lblSubject.Caption:= ResStr_EditorSurname + ':';
        eName.Text:= lCurrentEditor.Name;
        eInitials.Text:= lCurrentEditor.Initials;

        if ShowModal = mrOK then
        begin
          // Check the name is not already in the list
          if lbEditors.Items.IndexOf(ListName(eInitials.Text, eName.Text))=-1 then
          begin
            lCurrentEditor.Name:= eName.Text;
            lCurrentEditor.Initials:= eInitials.Text;
          end else
            MessageDlg(SReferences_ThisNameIsAlreadyPresentInTheLis,mtInformation,[mbOk],0);
          lbEditorsClick(nil);
        end;
      finally
        Free;
      end;
  end;
end;  // bbEditorEditClick

//==============================================================================
procedure TfrmReferences.bbEditorDelClick(Sender: TObject);
begin
  inherited;
  if lbEditors.ItemIndex<>-1 then
    if MessageDlg(SReferences_AreYouSureYouWantToDeleteThisEdi,
                  mtConfirmation,[mbYes,mbNo],0)=mrYes then
    begin
      FEditorList.DeleteItem(lbEditors.ItemIndex);
      lbEditorsClick(nil);
    end;
end;  // bbEditorDelClick

//==============================================================================
procedure TfrmReferences.reEnter(Sender: TObject);
var ltfEnabled: Boolean;
begin
  inherited;
  // As there is only one richtext on this form and  it's a DB one...
  ltfEnabled := FdmReferences.qryReference.State in [dsEdit,dsInsert];
  with dmFormActions do begin
    actCut.Enabled      := ltfEnabled;
    actBold.Enabled     := ltfEnabled;
    actItalic.Enabled   := ltfEnabled;
    actUnderline.Enabled:= ltfEnabled; 
  end;
end;  // reEnter

//==============================================================================
procedure TfrmReferences.reExit(Sender: TObject);
begin
  inherited;
  with dmFormActions do begin
    actCut.Enabled      := false;
    actBold.Enabled     := false;
    actItalic.Enabled   := false;
    actUnderline.Enabled:= false;
  end;
end;  // reExit

//==============================================================================
procedure TfrmReferences.ShowDetails(const Journal, Part, Number, Pages, Supplement,
  Edition, Symposium, Editor, Publisher, Place: Boolean);
begin
  //Journal
  lblJournalDesc.Visible:= Journal;
  dblcJournal.Visible:= Journal;

  //Part
  lblPart.Visible:= Part;
  dbePart.Visible:= Part;

  //Number
  lblNumber.Visible:= Number;
  dbeNumber.Visible:= Number;

  //Pages
  lblPages.Visible:= Pages;
  dbePages.Visible:= Pages;

  //Supplement
  lblSupplement.Visible:= Supplement;
  dbeSupplement.Visible:= Supplement;

  //Edition
  lblEdition.Visible:= Edition;
  dbeEdition.Visible:= Edition;

  //Symposium
  lblSymposium.Visible:= Symposium;
  dbeSymposium.Visible:= Symposium;

  //Editor
  lblEditor.Visible:= Editor;
  lbEditors.Visible:= Editor;
  bbEditorAdd.Visible:= Editor;
  bbEditorEdit.Visible:= Editor;
  bbEditorDel.Visible:= Editor;

  //Publisher
  lblPublisher.Visible:= Publisher;
  case SelectedReferenceType of
    rtBook, rtJournal, rtSymposium, rtBulletin, rtMemoirs, rtOther:
        lblPublisher.Caption:= ResStr_Publisher + ':';
    rtPublishedReport, rtUnpublishedReport, rtEphemera:
        lblPublisher.Caption:= ResStr_Org + ':';
    rtThesis:
        lblPublisher.Caption:= ResStr_AwardingInstitution + ':';
  end;
  dbePublisher.Visible:= Publisher;

  //Place
  lblPlace.Visible:= Place;
  case SelectedReferenceType of
    rtBook, rtJournal, rtSymposium, rtBulletin, rtMemoirs, rtOther:
        lblPlace.Caption:= ResStr_PlaceOfPublication + ':';
    rtPublishedReport, rtUnpublishedReport, rtEphemera:
        lblPlace.Caption:= ResStr_Place + ':';
  end;
  dbePublicationPlace.Visible:= Place;
end;  // ShowDetails

//==============================================================================
procedure TfrmReferences.cmbReferenceTypeChange(Sender: TObject);
begin
  inherited;
  if EditMode = emView then
    cmbReferenceType.ItemIndex := FReferenceType
  else
  if (cmbReferenceType.ItemIndex <> FReferenceType) then
  begin
    FReferenceType := cmbReferenceType.ItemIndex;
    dblcJournal.KeyValue:= '';
    while FEditorList.ItemCount > 0 do
      FEditorList.DeleteItem(0);
    with FdmReferences.qryReference do begin
      if State in [dsInsert,dsEdit] then begin
        FieldByName('Reference_Type').Value         :=
            FMappedReferenceTypes.Names[FMappedReferenceTypes.IndexOfObject(
                cmbReferenceType.Items.Objects[FReferenceType])];
        FieldByName('Journal_Key').Value            := Null;
        FieldByName('Volume').AsString              := '';
        FieldByName('Part').AsString                := '';
        FieldByName('Number').AsString              := '';
        FieldByName('Pages').AsString               := '';
        FieldByName('Supplement').AsString          := '';
        FieldByName('Edition').AsString             := '';
        FieldByName('Symposium_Title').AsString     := '';
        FieldByName('Publisher').AsString           := '';
        FieldByName('Place_Of_Publication').AsString:= '';
      end;
    end;
  end;

  HideAppropriateDetailControls;
end;

//==============================================================================
{ Procedure to turn on and off the various controls according to the journal
     type selected }
procedure TfrmReferences.HideAppropriateDetailControls;
begin
    //Enable/disable detail controls
  case SelectedReferenceType of
    //                   ShowDetails(Journ, Part , Numbe, Pages, Suppl, Editi, Sympo, Edito, Publi, Place);
    rtJournal:           ShowDetails(True , True , True , True , True , True , False, False, True , True );
    rtBook:              ShowDetails(False, False, False, True , True , True , False, False, True , True );
    rtSymposium:         ShowDetails(True , True , True , True , True , False, True , True , True , True );
    rtBulletin:          ShowDetails(True , True , True , True , True , True , False, False, True , True );
    rtMemoirs:           ShowDetails(True , True , True , True , True , True , False, True , True , True );
    rtPublishedReport:   ShowDetails(False, True , True , True , False, False, False, False, True , True );
    rtUnpublishedReport: ShowDetails(False, True , True , True , False, False, False, False, True , True );
    rtThesis:            ShowDetails(False, False, False, True , False, False, False, False, True , False);
    rtNotebook:          ShowDetails(False, False, True , False, False, False, False, False, False, False);
    rtEphemera:          ShowDetails(False, True , True , True , False, False, False, False, True , True );
    rtOther:             ShowDetails(True , True , True , True , True , True , True , True , True , True );
  end;
end;         

//==============================================================================
procedure TfrmReferences.SetupDestinationControls;
begin
  RegisterDragComponent(dbgReferences, DragReference);
  RegisterDropComponent(sgKeywords, DropKeyword, ['CONCEPT'], [CF_JNCCDATA]);
end;  // SetupDestinationControls

//==============================================================================
procedure TfrmReferences.DragReference(const Sender: TObject; var oDropSource: TJNCCDropSource);
begin
  { Set the source table for the drag }
  oDropSource.DropData.SetTable(TN_REFERENCE);
  { Add selected reference to the keylist }
  oDropSource.DropData.AddItem(FCurrentReference.ItemKey, '');
end;  // DragReference

//==============================================================================
{ Returns the key for the source currently selected in the grid }
function TfrmReferences.GetKeyList: TKeyList;
var lNewKeyList : TEditableKeyList;
begin
  lNewKeyList:= TEditableKeyList.Create;
  lNewKeyList.SetTable(TN_REFERENCE);
  lNewKeyList.AddItem(FCurrentReference.ItemKey, '');

  Result:=lNewKeyList;
end;  // GetKeyList

{-------------------------------------------------------------------------------
  Builds a keylist for revalidation purposes. For references, it's the same as
  normal keylist, since there is no hierarchy to deal with.
}
function TfrmReferences.GetKeyListForValidation: TKeyList;
begin
  Result := GetKeyList;
end;  // GetKeyListForValidation

//==============================================================================
procedure TfrmReferences.CreateFullReference;
begin
  //Make full reference sum of detail controls
  with dbreReference.Lines do
  begin
    Clear;
    AddStrings(dbreTitle.Lines); //Title
    if dblcJournal.Visible and (dblcJournal.Text <> '') then
      Add(ResStr_Journal + ': ' + dblcJournal.Text); //Journal
    if dbeVolume.Visible and (dbeVolume.Text <> '') then
      Add(ResStr_Volume + ': ' + dbeVolume.Text); //Volume
    if dbePart.Visible and (dbePart.Text <> '') then
      Add(ResStr_Part + ': ' + dbePart.Text); //Part
    if dbeNumber.Visible and (dbeNumber.Text <> '') then
      Add(ResStr_Number + ': ' + dbeNumber.Text); //Number
    if dbePages.Visible and (dbePages.Text <> '') then
      Add(ResStr_Pages + ': ' + dbePages.Text); //Pages
    if dbeSupplement.Visible and (dbeSupplement.Text <> '') then
      Add(ResStr_Supplement + ': ' + dbeSupplement.Text); //Supplement
    if dbeEdition.Visible and (dbeEdition.Text <> '') then
      Add(ResStr_Edition + ': ' + dbeEdition.Text); //Edition
    if dbeSymposium.Visible and (dbeSymposium.Text <> '') then
      Add(ResStr_Symposium + ': ' + dbeSymposium.Text); //Symposium

    //Editors
    if FEditorList.Count > 0 then
      Add(ResStr_Editors + ': ' + FEditorList.CreatePeopleString);

    if dbePublisher.Visible and (dbePublisher.Text <> '') then
      Add(lblPublisher.Caption + ' ' + dbePublisher.Text); //Publisher
    if dbePublicationPlace.Visible and (dbePublicationPlace.Text <> '') then
      Add(lblPlace.Caption + ' ' + dbePublicationPlace.Text); //Publication Place
  end;
end;  // CreateFullReference

//==============================================================================
{ Looks up a given source key in the grid and displays it }
procedure TfrmReferences.FindAndDisplaySource(iSourceKey: TKeyString);
begin
  if FdmReferences.qryPopulate.Locate('Source_Key', iSourceKey, [])
  then
    { Display details for located reference }
    DisplayDetailsAtSelectedRow
  else begin
    { Move to the first item in the grid and raise an error }
    DisplayFirstReferenceItem;
    EReferenceNotFound.Create(SReferences_TheDocumentWasNotFoundInTheList);
  end;
end;  // FindAndDisplaySource

//==============================================================================
procedure TfrmReferences.actFindExecute(Sender: TObject);
begin
  inherited;
  with TdlgFind.CreateDialog(nil, ResStr_FindDocument, ftReference) do begin
    try
      eSearchText.ClearSourceList;
      if ShowModal = mrOk then
        try
          FindAndDisplaySource(ItemKey);
        except
          on E:Exception do begin
            DisplayFirstReferenceItem;
            raise;
          end; // on E:Exception
        end; // try..except
    finally
      Free;
    end; // try..finally
  end; // with TdlgFind...
end;  // actFindExecute

//==============================================================================
procedure TfrmReferences.pnlReferenceDetailsResize(Sender: TObject);
var
  availableHeight, halfHeight: Integer;
begin
  inherited;
  lblReference.Caption:=GetTextWithinLimit(Canvas,FReferenceText,
                              pnlReferenceDetails.Width-lblReference.Left-8);

  // For some reason, the automatic resizing doesn't work properly. Using a
  // second panel within the main panel and sizing it manually solves this, but
  // is a bit of a hack.
  pnlInner.Width := pnlReferenceDetails.Width;
  pnlInner.Height := pnlReferenceDetails.Height;

  availableHeight := bvlGeneralFrame.Height
                            - lbAuthors.Top + bvlGeneralFrame.Top
                            - eYear.Height
                            - lblFullReference.Height
                            - eStorageLocation.Height
                            - 4 * 4   // Spacing between controls
                            - 8;      // Spacing at the bottom
  halfHeight := availableHeight div 2;
  lbAuthors.Height := halfHeight;
  eYear.Top := lbAuthors.Top + lbAuthors.Height + 4;
  lblYear.Top := eYear.Top + 3;
  cmbReferenceType.Top := eYear.Top;
  lblRefType.Top := lblYear.Top;
  lblFullReference.Top := eYear.Top + eYear.Height + 4;
  dbreReference.Top := lblFullReference.Top + lblFullReference.Height + 4;
  dbreReference.Height := halfHeight;

  if pcReferenceDetails.ActivePage = tsKeywords then
    with sgKeywords do
      ColWidths[0] := ClientWidth;
end;  // pnlReferenceDetailsResize

//==============================================================================
procedure TfrmReferences.dbePartNumberChange(Sender: TObject);
var lVal,lErr:integer;
    stVal:string;
begin
  inherited;
  stVal:=TDBEdit(Sender).Text;
  if TDBEdit(Sender).Visible and (stVal<>'') then begin
    Val(stVal,lVal,lErr);
    if (lErr<>0) or (lVal>32767) then
      TDBEdit(Sender).Text:=FdmReferences.qryReference.FieldByName(TDBEdit(Sender).DataField).AsString;
    ValidateValue(lErr=0, Format(ResStr_NotValidInteger, [stVal]), TDBEdit(Sender));
    ValidateValue(lVal<=32767,Format(ResStr_TooBigNumber, [stVal]), TDBEdit(Sender));
  end;
end;  // dbeVolumePartNumberChange

//==============================================================================
procedure TfrmReferences.RelatedObservations(const iMsg:string;
  const iRelType:TRelatedData; const SQLType:TStandardSQL);
var lKeyList:TEditableKeyList;
begin
  lKeyList:=TEditableKeyList.Create;
  try
    dmGeneralData.GetKeyListFromStandardQuery(lKeyList, SQLType, FCurrentReference.ItemKey);
    if lKeyList.Header.ItemCount=0 then
      MessageDlg(Format(ResStr_NoDocToRelate, [iMsg]), mtInformation,[mbOk],0)
    else
    if dmFormActions.actObservations.Execute then
      TfrmObservations(frmMain.GetForm(TfrmObservations)).DisplayObservations(iRelType,lKeyList);
  finally
    lKeyList.Free;
  end;
end;  // RelatedObservations

//------------------------------------------------------------------------------
procedure TfrmReferences.mnuRelSurveysClick(Sender: TObject);
begin
  inherited;
  RelatedObservations(ResStr_Surveys,rdSurvey,ssSurveysForReference);
end;  // mnuRelSurveysClick

//------------------------------------------------------------------------------
procedure TfrmReferences.mnuRelEventsClick(Sender: TObject);
begin
  inherited;
  RelatedObservations(ResStr_SurveyEvents,rdEvent,ssEventsForReference);
end;  // mnuRelEventsClick

//------------------------------------------------------------------------------
procedure TfrmReferences.mnuRelSamplesClick(Sender: TObject);
begin
  inherited;
  RelatedObservations(ResStr_Samples,rdSample,ssSamplesForReference);
end;  // mnuRelSamplesClick

//------------------------------------------------------------------------------
procedure TfrmReferences.mnuRelOccurClick(Sender: TObject);
begin
  inherited;
  RelatedObservations(ResStr_TaxonOrBiotopeOcc,rdOccurrence,ssOccurrencesForReference);
end;  // mnuRelOccurClick

//==============================================================================
procedure TfrmReferences.mnuRelLocationsClick(Sender: TObject);
var lLocationList:TEditableKeyList;
begin
  inherited;
  lLocationList:=TEditableKeyList.Create;
  try
    dmGeneralData.GetKeyListFromStandardQuery(lLocationList, ssLocationsForReference, FCurrentReference.ItemKey);
    // Find and display the location or feature if there is one to
    if lLocationList.Header.ItemCount=0 then
      MessageDlg(SReferences_ThereAreNoLocationsToRelateThisD,mtInformation,[mbOk],0)
    else
      if dmFormActions.actLocations.Execute then
        TfrmLocations(frmMain.GetForm(TfrmLocations)).DisplayLocations(lLocationList);
  finally
    lLocationList.Free;
  end;
end;  // mnuRelLocationsClick

//==============================================================================
procedure TfrmReferences.mnuRelIndivOrgClick(Sender: TObject);
var lNameList:TEditableKeyList;
begin
  inherited;
  lNameList:=TEditableKeyList.Create;
  try
    dmGeneralData.GetKeyListFromStandardQuery(lNameList, ssNamesForReference, FCurrentReference.ItemKey);
    if lNameList.Header.ItemCount=0 then
      MessageDlg(SReferences_ThereAreNoIndividualsOrOrganisat,mtInformation,[mbOk],0)
    else
      if dmFormActions.actNames.Execute then
        TfrmIndOrg(frmMain.GetForm(TfrmIndOrg)).DisplayNames(lNameList);
  finally
    lNameList.Free;
  end;
end;  // mnuRelIndivOrgClick

//==============================================================================
procedure TfrmReferences.DisplaySources(const iSourceList: TKeyList);
var lCount: integer;
begin
  if EditMode<>emView then
    MessageDlg(ResStr_CannotProceed,mtWarning,[mbOk],0)
  else begin
    if iSourceList<>nil then begin
      // Prepare the additional filtering clause
      FWhereClause := '''' + iSourceList.Items[0].KeyField1 + '''';
      for lCount := 1 to iSourceList.Header.ItemCount-1 do
        FWhereClause := FWhereClause + ','''+iSourceList.Items[lCount].KeyField1+'''';
      // Update SQL used to populate grid
      with FdmReferences.qryPopulate do begin
        if Active then Close;
        SQL.Clear;
        SQL.Text := GetPopulateSQL;
      end;
      // Populate grid with modified SQL
      PopulateGrid; 

      DisplayFirstReferenceItem;
      bbShowAll.Visible:=true;
    end;
  end;
end;  // DisplaySources

//==============================================================================
procedure TfrmReferences.DisplayFilteredSources;
var
  i: Integer;
  keyList: TEditableKeyList;
  filteredKeys: TStringList;
begin
  keyList := TEditableKeyList.Create;
  try
    keyList.SetTable(MIXED_DATA);
    filteredKeys := AppSettings.GetFilteredRecords(TN_REFERENCE);
    if filteredKeys <> nil then
      for i := 0 to filteredKeys.Count - 1 do
        keyList.AddItem(filteredKeys[i], TN_REFERENCE);

    DisplaySources(keyList);
    if FdmReferences.qryPopulate.RecordCount = 0 then
      bbShowAllClick(nil);
  finally
    keyList.Free;
  end;
end;

//==============================================================================
procedure TfrmReferences.bbShowAllClick(Sender: TObject);
var
  currentRow: TKeyString;
begin
  inherited;
  // Find and remove the extra filtering clause
  AppSettings.ClearFilteredRecords([TN_REFERENCE]);
  currentRow := FCurrentReference.ItemKey;
  ClearFilter(FILTER_REFERENCE);
  with FdmReferences.qryPopulate do begin
    if Active then Close;
    SQL.Clear;
    FWhereClause := '';
    SQL.Text := GetPopulateSQL;
  end;
  PopulateGrid;
  FindAndDisplaySource(currentRow);
  // Done its duty, hide it now
  bbShowAll.Visible:=false;
end;  // bbShowAllClick

//==============================================================================
{ Shell file button enabled if there is a file to shell - do not have to be in
    edit mode }
procedure TfrmReferences.eOriginalFileChange(Sender: TObject);
begin
  inherited;
  bbOriginalFileView.Enabled:=FileExists(eOriginalFile.Text);
end;  // dbeOriginalFileChange

//==============================================================================
procedure TfrmReferences.actFilterExecute(Sender: TObject);
begin
  inherited;
  GetFilter(FILTER_REFERENCE, TN_REFERENCE);
end;  // actFilterExecute

//==============================================================================
procedure TfrmReferences.ApplyFilter(AKeyList: TKeyList);
begin
  if AKeyList.Header.ItemCount = 0 then
    MessageDlg(SReferences_ThereAreNoDocumentsMatchingTheFi, mtInformation, [mbOk], 0)
  else
    DisplaySources(AKeyList);
end;

//==============================================================================
procedure TfrmReferences.pcReferenceDetailsChange(Sender: TObject);
begin
  inherited;
  pcReferenceDetails.HelpContext := pcReferenceDetails.ActivePage.HelpContext;
end;  // pcReferenceDetailsChange

//==============================================================================
procedure TfrmReferences.WMRefreshColours(var Msg: TMessage);
begin
  SetRequiredFieldsColourState(Editmode <> emView, [lbAuthors, eYear, cmbReferenceType, dbreTitle]);
  Repaint;
end;  // WMRefreshColours

//==============================================================================
procedure TfrmReferences.WMRefreshTermLists(var Msg: TMessage);
var lKey:TKeyString;
begin
  FdmReferences.qryJournal.Refresh;
  lKey:=dblcJournal.KeyValue;
  // Clear selection and reselect with the key to update the text
  dblcJournal.KeyValue:='';
  dblcJournal.KeyValue:=lKey;
end;  // WMRefreshTermLists

//==============================================================================
procedure TfrmReferences.RemoveLineBreaksFromTitle;
begin
  while ((Pos(#13, dbreTitle.Text) = 1) or (Pos(#10, dbreTitle.Text) = 1)) and (dbreTitle.Text <> '') do
    dbreTitle.Text := Copy(dbreTitle.Text, 2, Length(dbreTitle.Text)-1);
end;

//==============================================================================
procedure TfrmReferences.pcReferenceDetailsChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  inherited;
  AllowChange:= cmbReferenceType.Text<>'';
  if not AllowChange then begin
    MessageDlg(SReferences_InOrderToEnterFurtherDetailsTheD, mtWarning,[mbOk],0);
    cmbReferenceType.SetFocus;
  end;
end;

//==============================================================================
procedure TfrmReferences.actShowMetadataExecute(Sender: TObject);
var
  lSourceKey: TKeyString;
  lQry: TJNCCQuery;
begin
  lSourceKey := FdmReferences.qryReference.FieldByName('Source_Key').AsString;
  lQry := FdmReferences.qryReferenceWithCustodian;
  lQry.Parameters.ParamValues['Key'] := lSourceKey;
  lQry.Open;
  try
    with TdlgMetaDataPopup.Create(nil) do
      try
        ShowStandard(ResStr_Document, lblReference.Caption, lSourceKey, lQry);
      finally
        Free;
      end;
  finally
    lQry.Close;
  end;                                        
end;

//==============================================================================
//==============================================================================
{ Populates the details half of the documents screen, according to the index of
  the row currently selected in the dbgreferences grid }
procedure TfrmReferences.DisplayDetailsAtSelectedRow;
begin
  FCurrentReference.ItemKey := '';
  ResetDBRichEditControls([dbreReference, dbreTitle]);
  with FdmReferences.qryPopulate do
    if not (BOF and EOF) then begin
      // Store the key for the selected source
      FCurrentReference.ItemKey     := FieldByName('Source_Key').AsString;
      FCurrentReference.SysSupplied := FieldByName('System_Supplied_Data').AsBoolean;
      // Display the details for the reference
      PopulateDetail(FCurrentReference.ItemKey);
      // Build string and display in label above the details pane
      FReferenceText := DuplicateCharacters(FieldByName('Author').AsString, '&') + ' - ' +
                        FieldByName('Year_Vague_Date_Start').Text + ', ' +
                        dmGeneralData.ConvertRTFFieldToText(FieldByName('Title'));
      lblReference.Caption := GetTextWithinLimit( Canvas, FReferenceText,
                                                  pnlReferenceDetails.Width - lblReference.Left-8 );
      // Display controls for the reference
      HideAppropriateDetailControls;
      SetMenuState;
    end else begin { BOF and EOF }
      // Clear the details window
      PopulateDetail('');
      lblReference.Caption := '';
    end; // if
end; // DisplayDetailsAtSelectedRow

//==============================================================================
procedure TfrmReferences.DisplayFirstReferenceItem;
begin
  with FdmReferences.qryPopulate do begin
    try
      if not Active then Open;
      First;
      if not EOF then
        DisplayDetailsAtSelectedRow;
    except on E:Exception do
      EReferencesError.Create(SReferences_CannotDisplayFirstItemInGrid + E.Message);
    end; // try..except
  end;
end;  // DisplayFirstReferenceItem

//==============================================================================
{ Calls refresh for the query populating the grid (dbgReferences).
  This is basically a wrapper for the TJNCCQuery Refresh method here,
  but gives a transparent view of what's going on. }
procedure TfrmReferences.RefreshReferencesGrid;
begin
  with FdmReferences.qryPopulate do begin
    if Active then PopulateGrid
    else if (SQL.Text <> '') then PopulateGrid
    else EReferencesError.Create( ResStr_NoReferenceGrid );
  end; // with
end;  // RefreshReferencesGrid

//==============================================================================
procedure TfrmReferences.dbgReferencesKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
  if (Key = #13) and (FEditMode = emView) then
    DisplayDetailsAtSelectedRow;
end;  // dbgReferencesKeyPress

//==============================================================================
procedure TfrmReferences.dbgReferencesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_DELETE: if bbDelete.Enabled then bbDeleteClick(bbDelete);
  end;
end;

//==============================================================================
function TfrmReferences.GetPopulateSQL: String;
begin
  if FWhereClause = '' then
    Result := Format(SQL_REFERENCE_AUTHORS_NO_WHERE,
                     ['RA.Author', 'VW_REFERENCE_AUTHORS', 'RA.Author'])
  else
    Result := Format(SQL_REFERENCE_AUTHORS,
                     ['RA.Author', 'VW_REFERENCE_AUTHORS', FWhereClause, 'RA.Author']);
end;  // GetPopulateSQL

//==============================================================================
// Description : implements GetCurrentControlEditMode required for TBaseExportable
// Created : 15/11/02
function TfrmReferences.GetCurrentControlEditMode: TEditMode;
begin
  Result := FEditMode;
end;

//==============================================================================
//These functions control whether BaseChild warns the user that the data
//in the combo boxes is not
//editable because the data is from another site.
procedure TfrmReferences.dbComboKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  FKeyDown :=true;
end;

//==============================================================================
procedure TfrmReferences.dbComboKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  FKeyDown :=false;
end;

//==============================================================================
procedure TfrmReferences.dbComboClick(Sender: TObject);
begin
  inherited;
  //If this came from a key then no need to send the message.
  //Otherwise pretend a key has been pressed.
  if not FKeyDown then
  begin
    PostMessage(TWinControl(Sender).Handle, WM_KEYDOWN, VK_Return,0);
    FKeyDown:=false;
  end;
end;

//End of warning functions
//=============================================================================

//This function is called when the pointer changes in the References grid.
{ Displays the contents for the selected cell in the details pane }
procedure TfrmReferences.dbgReferencesAfterScroll(DataSet: TDataSet);
begin
  if (EditMode = emView) then begin
    // Find location of row in which cell lies
    if not (DataSet.Eof and Dataset.Bof) then
      DisplayDetailsAtSelectedRow;
  end; // if EditMode = emView
end;

//==============================================================================
function TfrmReferences.SelectedReferenceType: TReferenceType;
begin
  Result := rtNotSelected;
  with cmbReferenceType do
    if Text <> '' then
      Result := TReferenceType(Items.Objects[ItemIndex]);
end;

{-------------------------------------------------------------------------------
  Accessor for Details TPageControl
}
function TfrmReferences.GetDetailsPageControl: TPageControl;
begin
  Result := pcReferenceDetails;
end;

{-------------------------------------------------------------------------------
  Implement TBaseChild method allowing document to be selected in a consistent
      way.
}
function TfrmReferences.SelectNode(const ANodeType,
  ANodeKey: string): TFlyNode;
begin
  FindAndDisplaySource(ANodeKey);
  Result := nil;
end;

{-------------------------------------------------------------------------------
  Receive data for keyword return data link
}
procedure TfrmReferences.UpdateKeyword(Keylist: TKeylist);
begin
  try
    if Keylist.Header.ItemCount = 1 then
      if SameText(Keylist.Header.TableName, TN_CONCEPT) then
        // ignore duplicates
        if FKeyWordsGridManager.FindRowByKeyInColumn(Keylist.Items[0].KeyField1, 0) = -1 then
        begin
          with dmDatabase.GetRecordset('usp_Concept_Select',
              ['@ConceptKey', Keylist.Items[0].KeyField1]) do
            FKeywordsGridManager.UpdateLinkedValue(
                Fields['Item_Name'].Value,
                Keylist.Items[0].KeyField1);
        end else
          ShowInformation(SReferences_ThisKeywordIsAlreadyPresentInThe);
  finally
    Keylist.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Populate the keywords tab
}
procedure TfrmReferences.PopulateKeywords(const ARefKey: string);
begin
  FdmReferences.qryKeywords.Close;
  FdmReferences.qryKeywords.Parameters.ParamByName('@Key').Value := ARefKey;
  FdmReferences.qryKeywords.Open;
  if not Assigned(FKeywordsGridManager) then begin
    FKeywordsGridManager := TDataStringGrid.Create(sgKeywords,
        FdmReferences.qryKeywords, 'Reference_Keyword_Key');
    with FKeywordsGridManager do begin
      UserID := AppSettings.UserID;
      RestrictFullEdit := AppSettings.RestrictFullEdit;
      SiteID := AppSettings.SiteID;
      AddOnly := AppSettings.UserAccessLevel=ualAddOnly;
      PopulateGrid;
      SetupLinkedEdit('Concept_Key', 0, KeywordFindData, KeywordGetData);
      ImageList := dmFormActions.ilButtons;
      GetButtonImageIndex := 5;
      OnUpdateRow := KeywordUpdateRow;
      OnDeleteRow := KeywordDeleteRow;
    end;
  end
  else
    FKeywordsGridManager.Refresh;
end;

{-------------------------------------------------------------------------------
  Implement find dialog for the keywords grid
}
procedure TfrmReferences.KeywordFindData(const AInitialText: string; var AText,
    AKey: string);
var
  lKey: TKeyString;
  lDuplicateRow: integer;
begin
  AText := AInitialText;
  dmGeneralData.CheckConcept(ResStr_FindKeyword, ResStr_NoKeywords, ftKeyword, AText, lKey);
  lDuplicateRow := FKeyWordsGridManager.FindRowByKeyInColumn(lKey, 0);
  if (lDuplicateRow = -1) or (FKeyWordsGridManager.CellLinkedKey[0, sgKeywords.Row]=lKey) then
    AKey := lKey
  else begin
    AKey := '';
    ShowInformation(SReferences_ThisKeywordIsAlreadyPresentInThe);
  end;
end;

{-------------------------------------------------------------------------------
  Implement return data function for the keywords grid
}
procedure TfrmReferences.KeywordGetData;
begin
  dmFormActions.actEnhancedTermlists.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateKeyword);
end;

{-------------------------------------------------------------------------------
  Update or insert a new link to a keyword
}
procedure TfrmReferences.KeywordUpdateRow(var ioRowKey : string; iData :
    TStringList);
begin
  if ioRowKey<>'' then
    dmDatabase.RunUpdateStoredProc('usp_ReferenceKeyword_Update',
        ['@Key', ioRowKey,
        '@ConceptKey', iData.Values['Concept_Key'],
        '@SessionID', AppSettings.SessionID])
  else
    ioRowKey := dmDatabase.RunInsertStoredProc('Reference_Keyword',
        'usp_ReferenceKeyword_Insert',
        ['@SourceKey', FCurrentReference.ItemKey,
        '@ConceptKey', iData.Values['Concept_Key'],
        '@SessionID', AppSettings.SessionID],
        '@Key');
end;

{-------------------------------------------------------------------------------
  Delete a link to a keyword
}
procedure TfrmReferences.KeywordDeleteRow(iRowKey : string);
begin
  dmDatabase.RunDeleteStoredProc('usp_ReferenceKeyword_Delete', ['@Key', iRowKey]);
end;

{-------------------------------------------------------------------------------
  Check all rows in the keywords grid have a key
}
procedure TfrmReferences.ValidateKeywords;
var
  i: integer;
  lKey: TKeyString;
  lKeyword: string;
  lDuplicateRow: integer;

    // remove a row, either because its a duplicate or empty
    procedure DeleteRow(ARow: integer);
    begin
      sgKeywords.Row := i;
      FKeywordsGridManager.DeleteFromGrid(nil);
    end;

begin
  for i := sgKeywords.Rowcount-1 downto 1 do begin
    if FKeywordsGridManager.RowContainsData(i) then begin
      if FKeywordsGridManager.CellLinkedKey[0, i]='' then begin
        // data present, but no key
        lKeyword := sgKeywords.Cells[0,i];
        dmGeneralData.CheckConcept(ResStr_FindKeyword, ResStr_NoKeywords, ftKeyword, lKeyword, lKey);
        if lKey='' then begin
          // no matched item found
          pcReferenceDetails.ActivePage := tsKeywords;
          raise EReferencesError.CreateValidation(SReferences_SpecifyValidKeyword, sgKeywords);
        end
        else begin
          // matched a keyword, is it a duplicate?
          lDuplicateRow := FKeyWordsGridManager.FindRowByKeyInColumn(lKey, 0);
          if (lDuplicateRow = -1) or (FKeyWordsGridManager.CellLinkedKey[0, sgKeywords.Row]=lKey) then
            FKeywordsGridManager.UpdateLinkedValue(lKeyword, lKey, -1, i)
          else
            // remove the duplicate row
            DeleteRow(i);
        end;
      end;
    end else
      // remove the empty row
      DeleteRow(i);
  end;
  FKeywordsGridManager.Validate;
end;

{-------------------------------------------------------------------------------
  Code to handle dropping onto the keywords grid
}
procedure TfrmReferences.DropKeyword(const Sender: TObject; const iFormat:
    integer; const iSourceData: TKeyList; const iTextStrings: TstringList; var
    ioHandled: boolean);
var
  lExistingRow: integer;
begin
  if FKeywordsGridManager.Enabled then begin
    if (CompareText(iSourceData.Header.TableName, 'Concept')=0) and
        (iSourceData.Header.ItemCount>0) then begin
      lExistingRow := FKeywordsGridManager.FindRowByKeyInColumn(
          iSourceData.Items[0].KeyField1, 0);
      if lExistingRow <> -1 then
        sgKeywords.Row := lExistingRow
      else begin
        FKeywordsGridManager.AddToGrid(nil);
        with dmDatabase.GetRecordset('usp_Concept_Select', [
            '@ConceptKey', iSourceData.Items[0].KeyField1]) do
          FKeywordsGridManager.UpdateLinkedValue(
              Fields['Item_Name'].Value, iSourceData.Items[0].KeyField1);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  After a return data operation, need to bring self to front
}
procedure TfrmReferences.WMTransferDone(var Msg: TMessage);
begin
  BringToFront;
end;

{-------------------------------------------------------------------------------
  Set up the security attributes of a list of authors or editors
}
procedure TfrmReferences.InitPeopleList(AList: TPeopleList);
begin
  AList.UserID := AppSettings.UserID;
  AList.RestrictFullEdit := AppSettings.RestrictFullEdit;
  AList.SiteID := AppSettings.SiteID;
  AList.AddOnly := AppSettings.UserAccessLevel=ualAddOnly;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmReferences.DoShowHint(var HintStr: String; var CanShow: Boolean; var HintInfo:
    THintInfo); 
var
  X, Y, lHoverRow, lOldRecord: Integer;
  lCoord: TGridCoord; 
  lKey: String;
begin
  if HintInfo.HintControl = dbgReferences then begin
    X := Mouse.CursorPos.X - ClientOrigin.X - dbgReferences.Left - pnlList.Left;
    Y := Mouse.CursorPos.Y - ClientOrigin.Y - dbgReferences.Top - pnlList.Top;
    lCoord := dbgReferences.MouseCoord(X, Y);
    // The -1 makes lHoverRow a 0-based index
    lHoverRow := lCoord.Y - 1;
    if lHoverRow >= 0 then begin
      // We set the hint of the panel rather than the grid because setting the
      // hint of the grid doesn't work.
      lOldRecord := TDBGridAccessor(dbgReferences).DataLink.ActiveRecord;
      TDBGridAccessor(dbgReferences).DataLink.ActiveRecord := lHoverRow;
      try
        lKey := VarToStr(dbgReferences.Columns[0].Field.Value);
        HintStr := AppSettings.GetFilteredRecordHint(TN_REFERENCE, lKey);
      finally
        TDBGridAccessor(dbgReferences).DataLink.ActiveRecord := lOldRecord;
      end;
      CanShow := HintStr <> '';
      if CanShow then
        HintInfo.ReshowTimeout := 200
      else
        inherited;  // Call inherited in this case.
    end else
      inherited;  // Call inherited in this case too.
  end else
    inherited;  // Call inherited in this case as well. Yeah, I know, it gets old quick.
end;  // TfrmReferences.DoShowHint

{-------------------------------------------------------------------------------
  Map reference types from DB to TReferenceType
}
procedure TfrmReferences.PopulateReferenceTypes;
var
  i: Integer;
  originals, translated: TStringList;
begin
  originals := TStringList.Create;
  translated := TStringList.Create;
  try
    originals.CommaText  := ST_REFERENCE_TYPES;
    translated.CommaText := ResStr_ReferenceTypes;

    with FMappedReferenceTypes do begin
      Clear;
      // Map default values to their translated counterparts. Must be same position in both.
      for i := 0 to originals.Count - 1 do
        AddObject(originals[i] + '=' + translated[i], TObject(Ord(i + 1)));

      // Add any other found at the end of translated list, if any.
      for i := originals.Count to translated.Count - 1 do
        AddObject(translated[i] + '=' + translated[i], TObject(Ord(rtOther)));

      // Add all user-imported types found in DB.
      with dmDatabase.ExecuteSQL('SELECT DISTINCT Reference_Type FROM Reference', True) do begin
        while not Eof do begin
          // If value not already in list, add it.
          if IndexOfName(Fields['Reference_Type'].Value) = -1 then
            AddObject(
                Fields['Reference_Type'].Value + '=' + Fields['Reference_Type'].Value,
                TObject(Ord(rtOther)));
          MoveNext;
        end;
        Close;
      end;

      // Put everything in combo.
      for i := 0 to Count - 1 do
        cmbReferenceType.Items.AddObject(ValueFromIndex[i], Objects[i]);
    end;
  finally
    originals.Free;
    translated.Free;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmReferences.ApplySecurity;
begin
  SetMenuState;
  pmAdd.Visible := (AppSettings.UserAccessLevel >= ualAddOnly);
  pmHValidateItem.Visible := AppSettings.UserAccessLevel = ualAdmin;
  // Changing the UserAccessLevel can involve closing and re-opening the database connection,
  // so we need to repopulate the grid here.
  PopulateGrid;
end;

{-------------------------------------------------------------------------------
  Filtered row coloration
}
procedure TfrmReferences.dbgReferencesGetCellColor(Sender: TObject; DataCol:
    Integer; Column: TColumn; State: TGridDrawState);
var
  key: string;
begin
  inherited;
  if not dbgReferences.Columns[0].Field.IsNull then begin
    key := dbgReferences.Columns[0].Field.Value;
    dmGeneralData.SetCanvasColourForFilterNode(
        dbgReferences.Canvas,
        (AppSettings.IndexOfFilteredRecord(TN_REFERENCE, key) > -1) and ([gdSelected] <> State));
  end;
end;

{-------------------------------------------------------------------------------
  Handles click events on the Keywords Grid
}
procedure TfrmReferences.sgKeywordsClick(Sender: TObject);
begin
  btnRemoveKeyword.Enabled := not FKeywordsGridManager.RowLocked(sgKeywords.Row)
      and (sgKeywords.Row < sgKeywords.RowCount) and (EditMode <> emView);
end;

{-------------------------------------------------------------------------------
  Adds a new keyword to the grid.
}
procedure TfrmReferences.btnAddKeywordClick(Sender: TObject);
begin
  inherited;
  FKeywordsGridManager.AddToGrid(Sender);
  btnRemoveKeyword.Enabled := True;
  with sgKeywords do
    ColWidths[0] := ClientWidth;
end;

{-------------------------------------------------------------------------------
  Deletes the current keyword from the grid, disables the Remove button if the
  newly selected keyword cannot be delted.
}
procedure TfrmReferences.btnRemoveKeywordClick(Sender: TObject);
begin
  inherited;
  FKeywordsGridManager.DeleteFromGrid(Sender);
  if (sgKeywords.Row >= sgKeywords.RowCount) or
     FKeywordsGridManager.RowLocked(sgKeywords.Row) then
  begin
      btnRemoveKeyword.Enabled := False;
  end;
  with sgKeywords do
    ColWidths[0] := ClientWidth;
end;

{-------------------------------------------------------------------------------
  When the references page is shown, resize the keywords column to fit the box.
}
procedure TfrmReferences.tsKeywordsShow(Sender: TObject);
begin
  inherited;
  with sgKeywords do
    ColWidths[0] := ClientWidth;
end;

end.

