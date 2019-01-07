//==============================================================================
//  Unit:        TaxonDictEditor
//
//  Implements:  TfrmTaxonDictEditor
//
//  Description: Implements the Taxon dictionary editor. The right side
//               panel displays the details about the taxon selected in
//               the tree. This class allows taxa to be added, edited or
//               deleted, according to the user's access rights.
//
//  Author:      Eric Salmon
//  Created:     10 Dec 2001
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//               Eric Salmon 19/03/2002
//               Taxonomic Groups - Items not expanded when added to the list,
//               only when saving. Also, delete all selected items in list working.
//
//               Eric Salmon - 21/03/2002
//               Initialisation of the Sources component requires an extra
//               parameter for RegisterDropComponent.
//
//               Eric Salmon - 27/03/2002
//               Changed the initialisation parameters of the Sources component
//               to accept a database object instead of database name and
//               session name.
//
//  Last Revision Details:
//    $Revision: 97 $
//    $Date: 19/01/09 14:39 $
//    $Author: Pauldavies $
//
//  $History: TaxonDictEditor.pas $
//  
//  *****************  Version 97  *****************
//  User: Pauldavies   Date: 19/01/09   Time: 14:39
//  Updated in $/JNCC/Development/Build/Source
//  Added some splitters, increased the size of some buttons and made a few
//  other minor alignment changes.
//  
//  *****************  Version 96  *****************
//  User: Pauldavies   Date: 5/01/09    Time: 12:30
//  Updated in $/JNCC/Development/Build/Source
//  Incident 18366
//  CCN 272
//  
//  Added proper resizing to the right hand panel.
//  
//  *****************  Version 95  *****************
//  User: Johndurman   Date: 23/05/08   Time: 15:51
//  Updated in $/JNCC/Development/Build/Source
//  VI 17177 - CCN263 - Limiting deletion of record attributes where the
//  current system is not the custodian
//  
//  *****************  Version 94  *****************
//  User: Ericsalmon   Date: 26/03/08   Time: 18:05
//  Updated in $/JNCC/Development/Build/Source
//  VI16723. Fixed hardcoded "Internal Map" string problem. Also changed
//  hardcoded gird column titles to individual resource strings.
//  
//  *****************  Version 93  *****************
//  User: Johnvanbreda Date: 11/03/08   Time: 14:52
//  Updated in $/JNCC/Development/Build/Source
//  15899
//  Fixed rucksack from pinging to front
//  
//  *****************  Version 92  *****************
//  User: Johnvanbreda Date: 22/01/08   Time: 16:47
//  Updated in $/JNCC/Development/Build/Source
//  VI15275
//  Fixed issue where saving a taxon/biotope brings the browser to the
//  front.
//  
//  *****************  Version 91  *****************
//  User: Davidkelly   Date: 22/01/08   Time: 14:30
//  Updated in $/JNCC/Development/Build/Source
//  VI 14779 (CCN 225b): Implemented ApplySecurity. Moved EditMode property
//  to TBaseChild.
//  
//  *****************  Version 90  *****************
//  User: Johnvanbreda Date: 21/01/08   Time: 14:27
//  Updated in $/JNCC/Development/Build/Source
//  IR15282
//  Fixed rapidtree behaviuor
//  
//  *****************  Version 89  *****************
//  User: Rickyshrestha Date: 4/01/08    Time: 15:38
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 88  *****************
//  User: Rickyshrestha Date: 28/12/07   Time: 11:04
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 87  *****************
//  User: Rickyshrestha Date: 27/12/07   Time: 16:10
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 86  *****************
//  User: Rickyshrestha Date: 27/12/07   Time: 14:01
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 85  *****************
//  User: Rickyshrestha Date: 20/12/07   Time: 13:16
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 84  *****************
//  User: Rickyshrestha Date: 20/12/07   Time: 11:01
//  Updated in $/JNCC/Development/Build/Source
//  Changed some constants to resourcestring
//  
//  *****************  Version 83  *****************
//  User: Rickyshrestha Date: 19/12/07   Time: 13:38
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardocded strings and constant to resourcestring
//  ResStr_DeletedFromDatabase
//    ResStr_UserStandardCommonName
//    ResStr_CorrectRankNeeded
//    ResStr_SeveralTaxa
//    ResStr_AnotherTaxon
//    ResStr_CannotDeleteTaxon
//    ResStr_PermanentlyDeleteTaxon
//    ResStr_CannotDeleteTaxonRefObvser
//    ResStr_TaxonNameRequiredEnter
//    ResStr_TaxonRankRequiredSelect
//    ResStr_PersonalTaxonNameBlank
//    ResStr_SetFieldsCalledInvalid
//    ResStr_DeleteStatus
//    ResStr_StatusTypeRequired
//    ResStr_InvalidStatusDocument
//    ResStr_StartContainEndDate
//    ResStr_EndContainStartDate
//    ResStr_EndComesBeforeStartDate
//    ResStr_DeleteFact
//    ResStr_FactTypeRequired
//    ResStr_FactDetailsRequired
//    ResStr_FactDocInvalidSelect
//    ResStr_InvalidVagueDate
//  
//  *****************  Version 82  *****************
//  User: Johnvanbreda Date: 25/01/06   Time: 13:03
//  Updated in $/JNCC/Development/Build/Source
//  IR10862
//  Fixed taxon names list
//  
//  *****************  Version 81  *****************
//  User: Johnvanbreda Date: 24/01/06   Time: 15:02
//  Updated in $/JNCC/Development/Build/Source
//  IR10817
//  Own Data access level fixes.
//  
//  *****************  Version 80  *****************
//  User: Johnvanbreda Date: 20/01/06   Time: 14:30
//  Updated in $/JNCC/Development/Build/Source
//  IR10812
//  Review of security
//  
//  *****************  Version 79  *****************
//  User: Johnvanbreda Date: 19/01/06   Time: 13:03
//  Updated in $/JNCC/Development/Build/Source
//  10814
//  Biotope fact saving fixed, plus refactoring
//  
//  *****************  Version 78  *****************
//  User: Johnvanbreda Date: 18/01/06   Time: 15:27
//  Updated in $/JNCC/Development/Build/Source
//  IR10809
//  Edit/Delete access on observations for Own Data access
//  + some code refactoring
//  
//  *****************  Version 77  *****************
//  User: Johnvanbreda Date: 20/12/05   Time: 12:02
//  Updated in $/JNCC/Development/Build/Source
//  CCN132
//  Restructuring custodian field handling
//  
//  *****************  Version 76  *****************
//  User: Ericsalmon   Date: 12/12/05   Time: 13:59
//  Updated in $/JNCC/Development/Build/Source
//  CCN132. Full use restriction to own data.
//  
//  *****************  Version 75  *****************
//  User: Kevingiles   Date: 17/12/04   Time: 14:00
//  Updated in $/JNCC/Development/Build/Source
//  Selects correct data keys using sub select for the deletion from taxon
//  name based on recommended name key. This was working before on one DB
//  but not another - not sure why? It now works on both Test and
//  Collections test DBs.
//  
//  *****************  Version 74  *****************
//  User: Johnvanbreda Date: 16/12/04   Time: 14:23
//  Updated in $/JNCC/Development/Build/Source
//  IR8140
//  Fixed cancelling of first node
//  
//  *****************  Version 73  *****************
//  User: Kevingiles   Date: 15/12/04   Time: 8:43
//  Updated in $/JNCC/Development/Build/Source
//  There was a problem with deletions of new taxa caused by the new
//  foreign key constraint on recommended taxon name key.
//  
//  *****************  Version 72  *****************
//  User: Johnvanbreda Date: 13/12/04   Time: 8:24
//  Updated in $/JNCC/Development/Build/Source
//  Application of nameserver stored proc to Index_Taxon_Name
//  
//  *****************  Version 71  *****************
//  User: Ericsalmon   Date: 10/11/04   Time: 12:08
//  Updated in $/JNCC/Development/Build/Source
//  ID 7568. Fixed Taxon User Names deletion, and a couple other fixes.
//  
//  *****************  Version 70  *****************
//  User: Johnvanbreda Date: 1/11/04    Time: 12:20
//  Updated in $/JNCC/Development/Build/Source
//  CCN117
//  Preferred flag in Index_Taxon_Name
//  
//  *****************  Version 69  *****************
//  User: Ericsalmon   Date: 26/08/04   Time: 10:41
//  Updated in $/JNCC/Development/Build/Source
//  Forced nil on freed objects, trying to prevent AVs when closing app
//  while forms are still loading.
//  
//  *****************  Version 68  *****************
//  User: Ericsalmon   Date: 24/08/04   Time: 11:25
//  Updated in $/JNCC/Development/Build/Source
//  ID 3469. Fix for pending messages causing a bit of trouble when closing
//  the app before they're processed.
//  
//  *****************  Version 67  *****************
//  User: Andrewkemp   Date: 29/06/04   Time: 17:23
//  Updated in $/JNCC/Development/Build/Source
//  VI 4571
//  When registering drop controls include CF_TEXT as an acceptable format
//  for text boxes and other appropriate controls.
//  
//  *****************  Version 66  *****************
//  User: Ericsalmon   Date: 22/03/04   Time: 13:48
//  Updated in $/JNCC/Development/Build/Source
//  ID 4465. Allow editing of leaf node when rank missing/incorrect.
//  
//  *****************  Version 65  *****************
//  User: Ericsalmon   Date: 17/03/04   Time: 9:47
//  Updated in $/JNCC/Development/Build/Source
//  ID 4465. Can't edit a taxon without rank.
//  
//  *****************  Version 64  *****************
//  User: Ericsalmon   Date: 21/01/04   Time: 15:08
//  Updated in $/JNCC/Development/Build/Source
//  Multiple maps development.
//  
//  *****************  Version 63  *****************
//  User: Ericsalmon   Date: 26/02/03   Time: 11:50
//  Updated in $/JNCC/Source
//  IR 504 - Change of custodian check.
//
//  *****************  Version 62  *****************
//  User: Ericsalmon   Date: 21/02/03   Time: 14:30
//  Updated in $/JNCC/Source
//  Formatting of some status messages.
//  
//  *****************  Version 61  *****************
//  User: Johnvanbreda Date: 17/02/03   Time: 12:10
//  Updated in $/JNCC/Source
//  Removed redundant registry use
//  
//  *****************  Version 60  *****************
//  User: Ericsalmon   Date: 13/02/03   Time: 17:10
//  Updated in $/JNCC/Source
//  Update.
//  
//  *****************  Version 59  *****************
//  User: Pollyshaw    Date: 10/02/03   Time: 10:04
//  Updated in $/JNCC/Source
//  Made groups work properly again after change in meaning of
//  Index_Taxon_Name.System_Supplied.
//  
//  *****************  Version 58  *****************
//  User: Ericsalmon   Date: 7/02/03    Time: 16:02
//  Updated in $/JNCC/Source
//  Fix for new taxa.
//  
//  *****************  Version 57  *****************
//  User: Pollyshaw    Date: 6/02/03    Time: 15:40
//  Updated in $/JNCC/Source
//  IR 377: Fixed the duplicates in the system supplied box.
//  
//  *****************  Version 55  *****************
//  User: Pollyshaw    Date: 27/01/03   Time: 12:18
//  Updated in $/JNCC/Source
//  IR 378: fixed AddToArbitraryTaxonGroup.
//  
//  *****************  Version 54  *****************
//  User: Pollyshaw    Date: 27/01/03   Time: 11:22
//  Updated in $/JNCC/Source
//  Made rucksack menu into XP style.
//  
//  *****************  Version 53  *****************
//  User: Ericsalmon   Date: 20/01/03   Time: 17:28
//  Updated in $/JNCC/Source
//  Make use of the FindUnique function of the Find dialog to simplify the
//  code for a few edit boxes.
//  
//  *****************  Version 52  *****************
//  User: Andrewkemp   Date: 20/01/03   Time: 17:17
//  Updated in $/JNCC/Source
//  IR326: Restricted some of the UPDATEs on INDEX_TAXON_NAME according to
//  user level; however I am not sure about the rest, so this incident is
//  still open.
//  
//  *****************  Version 51  *****************
//  User: Ericsalmon   Date: 17/01/03   Time: 17:27
//  Updated in $/JNCC/Source
//  IR 302 - Additional fix when rucksack content is invalid.
//  Fix for Status Details and RTF menu items state.
//  
//  *****************  Version 50  *****************
//  User: Andrewkemp   Date: 17/01/03   Time: 17:07
//  Updated in $/JNCC/Source
//  IR302: Behaves more gracefully if selected rucksack returns no items.
//  
//  *****************  Version 49  *****************
//  User: Andrewkemp   Date: 17/01/03   Time: 16:51
//  Updated in $/JNCC/Source
//  IR301: Language for new Taxon Names defaults to 'en - English' as per
//  the TSD.
//
//  *****************  Version 48  *****************
//  User: Ericsalmon   Date: 17/01/03   Time: 11:32
//  Updated in $/JNCC/Source
//  IR 211 fixed.
//
//  *****************  Version 47  *****************
//  User: Ericsalmon   Date: 17/01/03   Time: 10:47
//  Updated in $/JNCC/Source
//  IR 210 fixed.
//
//  *****************  Version 46  *****************
//  User: Ericsalmon   Date: 16/01/03   Time: 12:07
//  Updated in $/JNCC/Source
//  IR 143
//  Fixed.
//
//  *****************  Version 45  *****************
//  User: Pollyshaw    Date: 15/01/03   Time: 12:40
//  Updated in $/JNCC/Source
//  IR 256: Removed alias from Delete statement in delete taxon name.
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit TaxonDictEditor;

interface
                                         
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseTaxonDictUnit, Menus, ActnList, ExGrid, RapTree, ComCtrls, StdCtrls, Db,
  ExtCtrls, OnlineHelp, TaxonDictEditorData, CompositeComponent, Sources,
  VagueDateEdit, Grids, DBListCombo, Buttons, DBGlyphCtrls, Mask, DBCtrls,
  Constants, DataClasses, DropTarget, TaxonDictBrowser, HierarchyNodes,
  ExceptionForm, VagueDate, ValidationData, JNCCDatasets, GeneralFunctions,
  SQLConstants, ImageListButton, DatabaseAccessADO, StrUtils,
  KeyboardRapidTree;

type
  ETaxonDictEditor = class(TExceptionPath);
  EInvalidState = class(ETaxonDictEditor);

  TfrmTaxonDictEditor = class(TBaseTaxonDict)
    scbTaxonDetails: TScrollBox;
    lblNamePrompt: TLabel;
    lblSelectedName: TLabel;
    pcTaxonDetails: TPageControl;
    tsGeneral: TTabSheet;
    bvlGeneralFram: TBevel;
    lblRank: TLabel;
    lblAuthority: TLabel;
    lblName: TLabel;
    lblEntryByPrompt: TLabel;
    lblEntryDatePrompt: TLabel;
    lblChangeByPrompt: TLabel;
    lblChangeDatePrompt: TLabel;
    dblblEntryDate: TDBText;
    dblblChangedDate: TDBText;
    lblEnteredBy: TLabel;
    lblChangedBy: TLabel;
    dbeName: TDBEdit;
    dbeAuthority: TDBEdit;
    dbcmbRank: TDBGlyphLookupComboBox;
    tsPersonalNames: TTabSheet;
    cmbEnterLanguage: TComboBox;
    tsStatuses: TTabSheet;
    gbStatusDetails: TGroupBox;
    lblStatusType: TLabel;
    lblStatusFrom: TLabel;
    lblStatusTo: TLabel;
    lblStatusGeoArea: TLabel;
    lblStatusConstraints: TLabel;
    lblStatusDetails: TLabel;
    Shape4: TShape;
    lblGeneralReference: TLabel;
    eStatusGeoArea: TEdit;
    eStatusConstraints: TEdit;
    reStatusDetails: TRichEdit;
    eStatusFrom: TEdit;
    eStatusTo: TEdit;
    eStatusReference: TEdit;
    cmbStatusType: TDBListCombo;
    tsFacts: TTabSheet;
    gbFactDetails: TGroupBox;
    Shape1: TShape;
    lblFactsReference: TLabel;
    lblFactTitle: TLabel;
    lblFactType: TLabel;
    lblFact: TLabel;
    lblFactDate: TLabel;
    eFactReference: TEdit;
    cmbFactType: TComboBox;
    eFactTitle: TEdit;
    vdeFactDate: TVagueDateEdit;
    reFact: TMemo;
    tsSources: TTabSheet;
    Sources: TSources;
    pnlButtons: TPanel;
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
    pmAdd: TPopupMenu;
    pmAddSibling: TMenuItem;
    pmAddChild: TMenuItem;
    pmHAdd: TMenuItem;
    pmHAddSibling: TMenuItem;
    pmHAddChild: TMenuItem;
    sgPersonalNames: TStringGrid;
    pmTaxonNames: TPopupMenu;
    pmTaxonNamesInsert: TMenuItem;
    pmTaxonNamesRename: TMenuItem;
    pmTaxonNameDelete: TMenuItem;
    chkStandardCommonName: TCheckBox;
    tsTaxonGroups: TTabSheet;
    pmRucksacks: TPopupMenu;
    pnlArbitraryGroup: TPanel;
    shpTaxa: TShape;
    lbArbitraryGroup: TListBox;
    lblArbitraryGroup: TLabel;
    btnLoadFromRucksack: TBitBtn;
    Splitter1: TSplitter;
    pnlSystemGroup: TPanel;
    lbSystemGroup: TListBox;
    lblSystemGroup: TLabel;
    btnDelete: TImageListButton;
    btnEdit: TImageListButton;
    btnAdd: TImageListButton;
    btnSave: TImageListButton;
    btnCancel: TImageListButton;
    btnStatusesReferenceFind: TImageListButton;
    btnStatusOK: TImageListButton;
    btnStatusCancel: TImageListButton;
    btnFactReferenceFind: TImageListButton;
    btnFactCancel: TImageListButton;
    btnFactOK: TImageListButton;
    splStatuses: TSplitter;
    Panel1: TPanel;
    sgStatuses: TStringGrid;
    btnStatusAdd: TImageListButton;
    btnStatusEdit: TImageListButton;
    btnStatusDel: TImageListButton;
    splFacts: TSplitter;
    pnlFactsTop: TPanel;
    sgFacts: TStringGrid;
    btnFactDelete: TImageListButton;
    btnFactEdit: TImageListButton;
    btnFactAdd: TImageListButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnAddClick(Sender: TObject);
    procedure actAddSiblingExecute(Sender: TObject);
    procedure actAddChildExecute(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnStatusesReferenceFindClick(Sender: TObject);
    procedure sgStatusesClick(Sender: TObject);
    procedure btnStatusAddClick(Sender: TObject);
    procedure btnStatusEditClick(Sender: TObject);
    procedure btnStatusDelClick(Sender: TObject);
    procedure btnStatusOKClick(Sender: TObject);
    procedure btnStatusCancelClick(Sender: TObject);
    procedure eStatusReferenceKeyPress(Sender: TObject; var Key: Char);
    procedure eStatusFromExit(Sender: TObject);
    procedure eStatusToExit(Sender: TObject);
    procedure reEnterRTF(Sender: TObject);
    procedure reExitRTF(Sender: TObject);
    procedure sgFactsClick(Sender: TObject);
    procedure btnFactAddClick(Sender: TObject);
    procedure btnFactEditClick(Sender: TObject);
    procedure btnFactDeleteClick(Sender: TObject);
    procedure btnFactReferenceFindClick(Sender: TObject);
    procedure btnFactOKClick(Sender: TObject);
    procedure btnFactCancelClick(Sender: TObject);
    procedure vdeFactDateExit(Sender: TObject);
    procedure eFactReferenceKeyPress(Sender: TObject; var Key: Char);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure pcTaxonDetailsChange(Sender: TObject);
    procedure pcTaxonDetailsChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure cmbEnterLanguageKeyPress(Sender: TObject; var Key: Char);
    procedure sgPersonalNamesClick(Sender: TObject);
    procedure sgPersonalNamesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sgPersonalNamesTopLeftChanged(Sender: TObject);
    procedure sgPersonalNamesKeyPress(Sender: TObject; var Key: Char);
    procedure sgPersonalNamesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cmbEnterLanguageChange(Sender: TObject);
    procedure pmTaxonNamesInsertClick(Sender: TObject);
    procedure pmTaxonNamesRenameClick(Sender: TObject);
    procedure pmTaxonNameDeleteClick(Sender: TObject);
    procedure chkStandardCommonNameClick(Sender: TObject);
    procedure lbArbitraryGroupDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnLoadFromRucksackClick(Sender: TObject);
    procedure lbArbitraryGroupKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure eStatusReferenceDblClick(Sender: TObject);
    procedure eFactReferenceDblClick(Sender: TObject);
    procedure dbComboKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dbComboKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dbComboClick(Sender: TObject);
    procedure tvDictionaryKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pnlDetailsResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FAdd          : Boolean;
    FDetailsMode  : Boolean;
    FClosingForm  : Boolean;
    FAddingNewNode: Boolean;

    FTaxonListItemKey : TKeyString;
    FTaxonVersionKey  : TKeyString;

    FTaxonStatusList    : TTaxonStatusList;
    FCurrentStatus      : TTaxonStatusItem;
    FCurrentStatusSource: TKeyString;

    FTaxonFactsList   : TTaxonFactsList;
    FCurrentFact      : TTaxonFactsItem;
    FCurrentFactSource: TKeyString;

    FDeletedUserNameKeys: TStringList;
    FNavigatingGrid     : Boolean;
    FMovingGridCell     : Boolean;
    FArbitraryGroupsEdited : boolean; // save unnecessary saves to DB
    FKeyDown               : Boolean; //indicates whether a key has been pressed on a combo.
    FHaveCustody: Boolean;
    procedure AddItem(const ANewNode: TFlyNode);
    function CheckDeletedNode(const AMode: TEditMode): Boolean;
    procedure CheckPreferredTaxonName(const ARow: Integer);
    function CheckReference(const AStatus: Boolean): Boolean;
    procedure ClearFactsFields;
    procedure ClearFields;
    procedure ClearStatusFields;
    procedure ClearTaxonNames;
    procedure ClearTaxonGroupPage;
    function DeleteItem: Boolean;
    procedure DropStatusReference(const Sender: TObject; const iFormat: integer;
      const iSourceData: TKeyList; const iTextStrings: TStringList;
      var ioHandled: boolean);
    procedure DropFactReference(const Sender: TObject; const iFormat: integer;
      const iSourceData: TKeyList; const iTextStrings: TStringList;
      var ioHandled: boolean);
    procedure DropTaxonIntoGroup(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TStringList; var ioHandled : boolean);
    procedure EnableDetails(const ANewMode: TEditMode);
    function GetSortCode(const ANewNode: TFlyNode): Integer;
    function GetLanguageFromCode(const ALangCode: String): String;
    procedure MoveGridCell(const AToTheRight: Boolean);
    procedure PopulateFactsItem;
    procedure PopulateStatusItem;
    procedure PopulatePersonalTaxonNames;
    procedure PopulateTaxonGroup;
    procedure ReferenceFactsUpdate(KeyList: TKeyList);
    procedure ReferenceStatusUpdate(KeyList: TKeyList);
    procedure RefreshDictionaryBrowser;
    procedure RefreshLists(const ADictNode: TTaxonDictionaryNode);
    procedure RefreshNode(const ANode: TFlyNode);
    procedure SaveItem;
    procedure SaveFactsItem;
    procedure SaveStatusItem;
    procedure SaveTaxonUserNames;
    procedure SaveArbitraryGroupInfo;
    procedure SetActiveCellState(const AChar:Char);
    procedure SetFields;
    procedure SetKeyFields;
    procedure SetRankCombo(const ANode: TFlyNode);
    procedure UpdateNodesSortCode(const ANode: TFlyNode);
    procedure ValidateFactDate;
    procedure ValidateStatusFromDate;
    procedure ValidateStatusToDate;
    function ValidateTaxonNames: Boolean;
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
    procedure WMRefreshColours(var Msg: TMessage); message WM_REFRESH_COLOURS;
    procedure RucksackPopupClick(Sender : TObject);
    procedure ClearSpecificGroupList(iListBox: TListBox);
    procedure LoadCurrentRucksackIntoGroup;
    procedure LoadRucksackFileIntoGroup(const iRucksackName: String);
    procedure LoadKeylistIntoGroup(iKeyList: TKeyList);
    procedure AddToArbitraryTaxonGroup(const iTLIKeys: String);
    procedure InsertNewTaxonToParentGroups(const iKey: String;
      iParentNode: TFlyNode; var iLevel: Integer);
    procedure InsertNewSynonym(const ATLIKey, ASynonymKey: TKeyString);
    procedure InsertNewIndexTaxonName;
    procedure UpdateIndexTaxonName;
    class function GetTaxonListItemCustodian(const AKey: TKeyString): string;
    function CanEditTaxonNameRow(ARow: integer): Boolean;
  protected
    procedure FreeObjects; override;
    function GetExcludeCDLists: Boolean; override;
    procedure PopulateDetails; override;
    procedure SetupDestinationControls; override;
    procedure SetupHelp; override;
    procedure SetupObjects; override;
    procedure SwitchToDetails(sgGrid: TStringGrid; iAdd, iEdit, iDel, iSave,
      iCancel: TControl; gbDetails: TGroupBox; tfDetails: Boolean); override;
    procedure SynchronizeAdditionalControls; override;
    procedure UpdateMenus; override;
    function GetCurrentControlEditMode: TEditMode; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure ApplySecurity; override;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, GeneralData, FormActions, Maintbar, References, Find,
  Rucksack, ADODB, BaseChildUnit;

const
  DEFAULT_LANGUAGE = 'en - English';
  ST_NEW_NODE_TEXT = 'New Taxon';

  SQL_TAXA_LIST_GROUP =
      'SELECT distinct TLI.Taxon_List_Item_Key, ITN.Preferred_Name, ITN.Preferred_Name_Italic, ' +
      'ITN.Common_Name, ITN.Common_Name_Italic, ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Sort_Order ' +
      'FROM Taxon_List_Item TLIP '+
      'INNER JOIN Taxon_List_Version TLVP ON TLVP.Taxon_List_Version_Key=TLIP.Taxon_List_Version_Key '+
      'INNER JOIN Index_Taxon_Group ITG ON ITG.Taxon_List_Item_Key=TLIP.Taxon_List_Item_Key '+
      'INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITG.Contained_List_Item_Key ' +
      'INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key ' +
      // ensure child is in same list, unless we are doing arbitrary group contents
      'INNER JOIN Taxon_List_Version TLVC ON TLVC.Taxon_List_Version_Key=ITN.Taxon_List_Version_Key	'+
          'AND (TLVC.Taxon_List_Key=TLVP.Taxon_List_Key OR ITG.SYSTEM_SUPPLIED_DATA=0)'+
      'WHERE  ITG.Taxon_List_Item_Key = ''%s'' ' +
      'AND %s ' +
      'ORDER BY ITN.Sort_Order, ITN.Preferred_Name';
  NONE_GIVEN_TAXON_NAME_TYPE_KEY = 'NBNSYS0000000000';

resourcestring
  ResStr_DeletedFromDatabase =  'It has been deleted from the database.';
  ResStr_UserStandardCommonName = 'Use standard common name (%s)';

  ResStr_CorrectRankNeeded =  'You cannot edit this taxon.  The rank sequence of its parent is lower or ' +
                              'equal to the rank sequence position of its children.  Therefore there are no ' +
                              'valid ranks that can be selected.   Please correct the rank of the parent ' +
                              'and/or children first.';

  ResStr_SeveralTaxa =  'several Taxa.';
  ResStr_AnotherTaxon = 'another Taxon.';
  ResStr_CannotDeleteTaxon =  'This Taxon cannot be deleted as it still contains ';

  ResStr_PermanentlyDeleteTaxon = 'You are about to PERMANENTLY delete the taxon "%s".'#13#13 +
                                  'Are you sure you want to continue?';

  ResStr_CannotDeleteTaxonRefObvser = 'The Taxon cannot be deleted as it is referenced in an observation.';

  ResStr_TaxonNameRequiredEnter = 'The Taxon Name is required. Please enter a name.';
  ResStr_TaxonRankRequiredSelect = 'The Taxon Rank is required. Please select a value.';
  ResStr_PersonalTaxonNameBlank = 'Some Personal Taxon Names are blank.  Please enter a name or remove the entry.';
  ResStr_SetFieldsCalledInvalid = 'SetFields was called when cursor was in an invalid state.';
  ResStr_DeleteStatus = 'Are you sure you want to delete this Status?';
  ResStr_StatusTypeRequired = 'A Status Type is required. Select a type from the list.' ;
  ResStr_InvalidStatusDocument =  'The Status Document is invalid. Select a valid Document or leave blank.';
  ResStr_StartContainEndDate =  'The Start Date cannot contain the End Date.';
  ResStr_EndContainStartDate =  'The End Date cannot contain the Start Date.';
  ResStr_EndComesBeforeStartDate =  'The End Date cannot come before the Start Date.';
  ResStr_DeleteFact = 'Are you sure you want to delete this Fact?';
  ResStr_FactTypeRequired = 'A Fact Type is required. Select a type from the list.';
  ResStr_FactDetailsRequired =  'Fact Details are required. Enter some Details.';
  ResStr_FactDocInvalidSelect = 'The Fact Document is invalid. Select a valid Document or leave blank.';
  ResStr_InvalidVagueDate = 'The vague date you have entered is not valid';
  ResStr_SelectedItem = 'the selected item';
  ResStr_SelectedItems = 'the selected items';


  //Status
  ResStr_LoadingTaxonList = 'Loading taxonomic list...';

type
  // class to hold a key on a grid, along with a flag indicating if the row is
  // editable
  TKeyDataEditFlag = class(TKeyData)
  private
    FCanEdit: boolean;
    procedure SetCanEdit(Value: boolean);
  public
    property CanEdit: boolean read FCanEdit write SetCanEdit;
  end;

//==============================================================================
function TfrmTaxonDictEditor.GetExcludeCDLists: Boolean;
begin
  Result := true;
end; // GetExcludeCDLists

//==============================================================================
procedure TfrmTaxonDictEditor.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(true);
  frmMain.SetContextToolbar(Self, mnuEdit, 0, [nil, nil, nil, nil, nil, nil, nil, nil,
                                               nil, pmSort, nil, nil, nil, nil, nil]);
  // Context toolbar updated after Create called. So if tree empty, refresh
  // Menu state and toolbar dropdown buttons
  UpdateMenus;
end;  // FormActivate

//==============================================================================
procedure TfrmTaxonDictEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  CanClose := False;
  if EditMode<>emView then begin
    Beep;
    case ConfirmSaveAndClose of
      mrYes: begin
               FClosingForm:=True;
               if btnStatusOk.Enabled then btnStatusOkClick(nil) else
               if btnFactOk.Enabled then btnFactOkClick(nil);
               btnSaveClick(nil);
               CanClose := True;
               FClosingForm :=false;
             end;
      mrNo:  begin
               FClosingForm:=true;
               if btnStatusCancel.Enabled then btnStatusCancelClick(nil) else
               if btnFactCancel.Enabled then btnFactCancelClick(nil);
               btnCancelClick(nil);
               CanClose := True;
               FClosingForm := false;
             end;
    end;
  end else
    CanClose := True;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmTaxonDictEditor.SetupObjects;
begin
  FDeletedUserNameKeys := TStringList.Create;

  // Data module
  DictionaryData := TdmTaxonDictEditor.Create(nil);

  // Lists
  FTaxonStatusList := TTaxonStatusList.Create(TdmTaxonDictEditor(DictionaryData).qryStatus, 'DesignationKey', sgStatuses);
  FTaxonFactsList := TTaxonFactsList.Create(TdmTaxonDictEditor(DictionaryData).qryFacts, 'FactKey', sgFacts);

  // Sources
  Sources.Init(dmDatabase.LocalDatabase, 'Taxon_Sources',
               AppSettings.UserAccessLevel, dmGeneralData.GetNextKey,
               RegisterDropComponent, AppSettings.SiteID, AppSettings.ExternalFilePath);
  Sources.OnFindInternal:=dmGeneralData.SourcesFindInternal;
  Sources.OnAddInternal :=dmGeneralData.SourcesAddInternal;
  Sources.OnShowSource := dmGeneralData.SourcesShowInternal;

  with TdmTaxonDictEditor(DictionaryData) do begin
    dbcmbRank.Datasource := dsGeneral;
    dbcmbRank.Listsource := dsRank;

    dbeAuthority.Datasource  := dsGeneral;
    dbeName.Datasource       := dsGeneral;
    cmbStatusType.Datasource := dsStatusType;

    // Labels
    dblblEntryDate.Datasource   := dsGeneral;
    dblblChangedDate.Datasource := dsGeneral;
  end;
  // Populate combo
  cmbStatusType.Active := True;
end;  // SetupObjects

//==============================================================================
procedure TfrmTaxonDictEditor.SetupHelp;
begin
  mnuEdit.HelpContext        := IDH_EDITMENU;
  Self.HelpContext           := IDH_NEWTAXA;
  pcTaxonDetails.HelpContext := IDH_NEWTAXAGENERAL;
  tsGeneral.HelpContext      := IDH_NEWTAXAGENERAL;
  tsStatuses.HelpContext     := IDH_NEWTAXASTATUSES;
  tsFacts.HelpContext        := IDH_NEWTAXAFACTS;
  tsSources.HelpContext      := IDH_NEWTAXASOURCES;
  tsTaxonGroups.Helpcontext  := IDH_TAXONGROUPS;
  tsPersonalNames.HelpContext:= IDH_TAXONNAMES;
end;  // SetupHelp

//==============================================================================
procedure TfrmTaxonDictEditor.FreeObjects;
begin
  ClearTaxonNames;
  ClearTaxonGroupPage;
  FDeletedUserNameKeys.Free;
  FDeletedUserNameKeys := nil;

  FTaxonStatusList.Free;
  FTaxonStatusList := nil;
  FTaxonFactsList.Free;
  FTaxonFactsList := nil;
end;  // FreeObjects

//==============================================================================
procedure TfrmTaxonDictEditor.SynchronizeAdditionalControls;
begin
  inherited;
  Sources.SetDatabase(dmDatabase.LocalDatabase);
end;  // SynchronizeAdditionalControls

//==============================================================================
procedure TfrmTaxonDictEditor.SetupDestinationControls;
begin
  inherited;
  RegisterDropComponent(eStatusReference,DropStatusReference,
      ['REFERENCE'],[CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(eFactReference,DropFactReference,
      ['REFERENCE'],[CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(lbArbitraryGroup, DropTaxonIntoGroup,
      ['TAXON_LIST_ITEM'], [CF_JNCCDATA]);
end;  // SetupDestinationControls

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.DropStatusReference(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
begin
  if iSourceData.Header.ItemCount > 0 then
    if (iFormat = CF_JNCCDATA) then begin
      ioHandled := true;
      FCurrentStatusSource  := iSourceData.Items[0].KeyField1;
      eStatusReference.Text := dmGeneralData.GetReferenceText(FCurrentStatusSource);
    end else
      ioHandled := false;
end;  // DropStatusReference

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.DropFactReference(const Sender: TObject;
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
end;  // DropFactReference

//==============================================================================
{ Drop taxa into the arbitrary taxonomic group list }
procedure TfrmTaxonDictEditor.DropTaxonIntoGroup(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TStringList; var ioHandled : boolean);
begin
  if (iFormat = CF_JNCCDATA) and (btnLoadFromRucksack.Enabled) then begin
    LoadKeyListIntoGroup(iSourceData);
    ioHandled := True;
  end else // if accepted
    ioHandled := False;
end;  // DropTaxonIntoGroup

//==============================================================================
procedure TfrmTaxonDictEditor.pcTaxonDetailsChange(Sender: TObject);
begin
  inherited;
  pcTaxonDetails.HelpContext := pcTaxonDetails.ActivePage.HelpContext;
end;  // pcTaxonDetailsChange

//==============================================================================
procedure TfrmTaxonDictEditor.pcTaxonDetailsChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  inherited;
  AllowChange:= FDetailsMode;
end;  // pcTaxonDetailsChanging

//==============================================================================
procedure TfrmTaxonDictEditor.reEnterRtf(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(not TRichEdit(Sender).ReadOnly);
end;  // reEnterRtf

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.reExitRtf(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(false);
end;  // reExitRtf

//==============================================================================
procedure TfrmTaxonDictEditor.UpdateMenus;
var ltfOn: Boolean;
    ltfLocalList: Boolean;
    lData: TKeyDataSysSupplied;
begin
  ltfOn := (EditMode = emView) and (tvDictionary.Items.Count > 0);
  ltfLocalList := (ListKeyData <> nil) and (ListKeyData.ItemAdditional = 'LOCAL');

  cmbList.Enabled      := EditMode = emView;
  btnShowAll.Enabled   := cmbList.Enabled;
  tvDictionary.Enabled := cmbList.Enabled;

  lData := GetStateDataFromNode(tvDictionary.Selected);
  btnAdd.Enabled        := (EditMode = emView) and ltfLocalList and AddButtonState;
  btnEdit.Enabled       := ltfOn and ltfLocalList and
                           (EditButtonState(lData, GetTaxonListItemCustodian) or
                            (AppSettings.UserAccessLevel >= ualAddOnly));
  btnDelete.Enabled     := ltfOn and ltfLocalList and
                           DeleteButtonState(lData) and
                           dmGeneralData.HasFullEditAccess(TN_TAXON_LIST_ITEM,
                           'Taxon_List_Item_Key', lData.ItemKey);
  mnuEditAdd.Enabled    := btnAdd.Enabled;
  mnuEditEdit.Enabled   := btnEdit.Enabled;
  mnuEditDelete.Enabled := btnDelete.Enabled;

  actAddChild.Visible := ltfOn;
  actFind.Enabled     := ltfOn;
  actFilter.Enabled   := ltfOn;
  pmHAdd.Visible      := ltfLocalList;
  pmHSortBy.Visible   := ltfOn;
  mnuEditSort.Enabled := ltfOn;
  EnableSortToolbutton(ltfOn, pmSort);
  // RichEdit control on detail part of status tab. Reached only if editing main record
  // therefore no additional check needed
  if ActiveControl is TRichEdit then
    dmFormActions.UpdateRTFMenu(not TRichEdit(ActiveControl).ReadOnly)
  else
    dmFormActions.UpdateRTFMenu(false);
end;  // UpdateMenus

//==============================================================================
procedure TfrmTaxonDictEditor.ClearFields;
begin
  ClearStatusFields;
  ClearFactsFields;
  lblChangedBy.Caption := '';
  lblEnteredBy.Caption := '';
end;  // ClearFields

//==============================================================================
procedure TfrmTaxonDictEditor.EnableDetails(const ANewMode: TEditMode);
var ltfOn: boolean;
begin
  FEditMode := ANewMode;
  ltfOn := EditMode <> emView;

  SetRequiredFieldsColourState(ltfOn,[dbeName, dbcmbRank]);
  dbcmbRank.ReadOnly := not (TdmTaxonDictEditor(DictionaryData).qryGeneral.State in [dsEdit, dsInsert]);

  chkStandardCommonName.Enabled := ltfOn;
  sgPersonalNames.Enabled := ltfOn;
  if ltfOn and
     (sgPersonalNames.RowCount = 2) and
     (sgPersonalNames.Cells[1, 1] = '') then
      sgPersonalNames.Cells[2, 1] := DEFAULT_LANGUAGE;
  cmbEnterLanguage.Visible := False;

  btnStatusAdd.Enabled := ltfOn;
  sgStatusesClick(nil);
  btnFactAdd.Enabled := ltfOn;
  sgFactsClick(nil);

  btnLoadFromRucksack.Enabled := ltfOn;

  Sources.EditMode := ANewMode;

  btnSave.Enabled   := ltfOn;
  btnCancel.Enabled := ltfOn;

  UpdateMenus;
end;  // EnableDetails

//==============================================================================
procedure TfrmTaxonDictEditor.RefreshDictionaryBrowser;
var loForm: TForm;
begin
  // If dictionary is openned in the background, refreshes the tree/browser
  // but only if the same classification list is selected.
  loForm := frmMain.GetForm(TfrmTaxonDictBrowser, false, false);
  if (loForm <> nil) and (ListKeyData.ItemKey = TfrmTaxonDictBrowser(loForm).ListKeyData.ItemKey) then
    TfrmTaxonDictBrowser(loForm).UpdateDictionary;
  // If rucksack opened, refresh the biotopes.
  loForm:=frmMain.GetForm(TfrmRucksack, false, false);
  if loForm <> nil then
    TfrmRucksack(loForm).UpdateBiotopeList;
end;  // RefreshDictionary

//==============================================================================
procedure TfrmTaxonDictEditor.WMRefreshColours(var Msg: TMessage);
begin
  if EditMode <> emView then begin
    SetRequiredFieldsColourState(true,[dbeName, dbcmbRank]);
    SetRequiredFieldsColourState(btnStatusOk.Enabled, [cmbStatusType]);
    SetRequiredFieldsColourState(btnFactOk.Enabled, [cmbFactType, reFact]);
  end;
  Repaint;
end;  // WMRefreshColours

//==============================================================================
procedure TfrmTaxonDictEditor.WMTransferDone(var Msg: TMessage);
begin
  Show;  // Show the form, bring back to the top
end;  // WMTransferDone

//==============================================================================
procedure TfrmTaxonDictEditor.SwitchToDetails(sgGrid: TStringGrid; iAdd,
  iEdit, iDel, iSave, iCancel: TControl; gbDetails: TGroupBox;
  tfDetails: Boolean);
begin
  inherited; // base form does most of work
  if sgGrid = sgStatuses then
    SetRequiredFieldsColourState(tfDetails, [cmbStatusType])
  else
    SetRequiredFieldsColourState(tfDetails, [cmbFactType, reFact]);
end;  // SwitchToDetails

//==============================================================================
function TfrmTaxonDictEditor.CheckDeletedNode(const AMode: TEditMode): Boolean;
var loSelNode: TFlyNode;
    lsMessage: String;
begin
  Result := true;
  loSelNode := tvDictionary.Selected;
  if not FAddingNewNode and not dmGeneralData.CheckKeyExists('Taxon_List_Item',
                        'Taxon_List_Item_Key', TNodeObject(loSelNode.Data).ItemKey) then
  begin
    Result := false;
    case AMode of
      emView   : lsMessage := ResStr_CannotAccessRecord;
      emEdit   : lsMessage := ResStr_CannotEditRecord;
      emDelete : lsMessage := ResStr_CannotDeleteRecord;
    end;
    MessageDlg(lsMessage + #13#13 + ResStr_DeletedFromDatabase,
               mtInformation, [mbOk], 0);
    with tvDictionary do begin
      Items.Delete(loSelNode);
      Selected := Items.GetFirstNode;
      OnChange(Self, Selected);
    end;
  end;
end;  // CheckDeletedNode

//==============================================================================
procedure TfrmTaxonDictEditor.PopulateDetails;
var lsKey           : String;
    lCurrentDictNode: TTaxonDictionaryNode;
    lCursor         : TCursor;
begin
  if not FAddingNewNode and (tvDictionary.Selected <> nil) and CheckDeletedNode(emView) then begin
    lCursor := HourglassCursor;
    try
      lCurrentDictNode := TTaxonDictionaryNode(tvDictionary.Selected.Data);
      lsKey := lCurrentDictNode.ItemKey;
      with TdmTaxonDictEditor(DictionaryData).qryGeneral do begin
        {Re-populate taxon tab}
        SetRankCombo(tvDictionary.Selected);
        Close; // Close existing query
        Parameters.ParamByName('Key').Value := lsKey; // Set query to new id
        Open; // reopen query
        // Set name labels
        lblEnteredBy.Caption := dmGeneralData.GetIndividualName(FieldByName('TLI_ENTERED_BY').AsString);
        lblChangedBy.Caption := dmGeneralData.GetIndividualName(FieldByName('TLI_CHANGED_BY').AsString);
        lblSelectedName.Caption := lCurrentDictNode.Title;
        FCustodian := FieldByName('CUSTODIAN').AsString;
        RefreshLists(lCurrentDictNode);
      end;  // with
      Application.ProcessMessages;
    finally
      DefaultCursor(lCursor);
    end;
  end;
  UpdateMenus;
end;  // PopulateDetails

//==============================================================================
procedure TfrmTaxonDictEditor.SetRankCombo(const ANode: TFlyNode);
var liParentSequence, liChildSequence: Integer;
    lsKey                            : TKeyString;
begin
  // we default to maximum rank sequence
  liChildSequence := dmFormActions.TaxonTypeList.GetMaxSequence + 1;

  with TdmTaxonDictEditor(DictionaryData) do begin
    // Get sequence of parent to enable us to populate the rank combo
    if Assigned(ANode.Parent) then
      liParentSequence := TTaxonDictionaryNode(ANode.Parent.Data).Sequence
    else
      liParentSequence := 0;

    lsKey := TTaxonDictionaryNode(ANode.Data).ItemKey;

    // get min sequence number of children nodes
    qryRankMinChild.Parameters.ParamByName('Parent').Value := lsKey;
    try
      qryRankMinChild.Open;
      if not qryRankMinChild.Eof then liChildSequence := qryRankMinChild.FieldByName('MinSequence').AsInteger;
    finally
      qryRankMinChild.Close;
    end;

    qryRank.Close; // reset rank items
    qryRank.Parameters.ParamByName('Sequence_Min').Value := liParentSequence;
    qryRank.Parameters.ParamByName('Sequence_Max').Value := liChildSequence;
    qryRank.Open;
  end;
end;  // SetRankCombo

//==============================================================================
procedure TfrmTaxonDictEditor.RefreshLists(const ADictNode: TTaxonDictionaryNode);
begin
  FTaxonListItemKey := ADictNode.ItemKey;
  FTaxonVersionKey  := ADictNode.TaxonVersionKey;

  // set details queries parameters
  with TdmTaxonDictEditor(DictionaryData) do begin
    qryStatus.Parameters.ParamByName('Key').Value     := FTaxonListItemKey; // Set query to new id
    qryFacts.Parameters.ParamByName('Key').Value      := FTaxonVersionKey; // Set query to new id
    qryTaxonNames.Parameters.ParamByName('Key').Value := FTaxonListItemKey;
  end;

  // Free up and recreate list objects
  FTaxonStatusList.TaxonListItemKey:= FTaxonListItemKey;
  FTaxonStatusList.TaxonVersionKey := FTaxonVersionKey;

  FTaxonFactsList.TaxonListItemKey:= FTaxonListItemKey;
  FTaxonFactsList.TaxonVersionKey := FTaxonVersionKey;

  // Refresh string grids
  FTaxonStatusList.Refresh;
  FTaxonFactsList.Refresh;

  sgFactsClick(nil);
  sgStatusesClick(nil);

  // Refresh Taxon Names listview
  PopulatePersonalTaxonNames;
  PopulateTaxonGroup;

  // Sources
  Sources.SourcesFromKey := ADictNode.TaxonKey;
  Sources.RefreshLists;
end;  // RefreshLists

//==============================================================================
procedure TfrmTaxonDictEditor.SetKeyFields;
var loParentDictNode, loDictNode: TTaxonDictionaryNode;
begin
  // Get Dictionary object from the parent node
  if Assigned(tvDictionary.Selected.Parent) then
    loParentDictNode := TTaxonDictionaryNode(tvDictionary.Selected.Parent.Data)
  else
    loParentDictNode := nil;

  // Get Dictionary object from the parent node
  loDictNode := TTaxonDictionaryNode(tvDictionary.Selected.Data);

 	with TdmTaxonDictEditor(DictionaryData) do begin
    // Set the new item keys
    qryGeneral.FieldByName('T_TAXON_KEY').AsString := loDictNode.TaxonKey;
    qryGeneral.FieldByName('TV_TAXON_VERSION_KEY').AsString := loDictNode.TaxonVersionKey;
    qryGeneral.FieldByName('TLI_TAXON_LIST_ITEM_KEY').AsString := loDictNode.ItemKey;
    qryGeneral.FieldByName('TV_TAXON_KEY').AsString := loDictNode.TaxonKey;
    qryGeneral.FieldByName('TLI_TAXON_VERSION_KEY').AsString := loDictNode.TaxonVersionKey;

    // Default Common Name record
    //qryCommonName.FieldByName('TAXON_LIST_ITEM_KEY').AsString := loDictNode.ItemKey;
    //qryCommonName.FieldByName('TAXON_VERSION_KEY').AsString := loDictNode.TaxonVersionKey;

    // New Taxon List Item Fields
    if Assigned(loParentDictNode) then
      qryGeneral.FieldByName('TLI_PARENT').AsString:= loParentDictNode.ItemKey
    else
      qryGeneral.FieldByName('TLI_PARENT').Clear;

    qryGeneral.FieldByName('TLI_SORT_CODE').AsInteger := GetSortCode(tvDictionary.Selected);

    // Set Preferred Name to itself for new taxon list items
    qryGeneral.FieldByName('TLI_PREFERRED_NAME').AsString := loDictNode.ItemKey;

    // New Taxon Fields
    qryGeneral.FieldByName('T_LANGUAGE').AsString := 'La';

    // New Taxon Version Fields
    qryGeneral.FieldByName('TV_DATE_FROM').AsDateTime := SysUtils.Date;
    qryGeneral.FieldByName('TV_VALIDATION_LEVEL').AsInteger := 1;
    qryGeneral.FieldByName('TV_UK_NATIVE').AsBoolean := False;

    // Taxon List Item Fields
    qryGeneral.FieldByName('TLI_TAXON_LIST_VERSION_KEY').AsString:= LatestVersion;
  end;
end;  // SetKeyFields

//==============================================================================
function TfrmTaxonDictEditor.GetSortCode(const ANewNode: TFlyNode): Integer;
var loPreviousSib, loNextSib: TFlyNode;
begin
  loNextSib := ANewNode.GetNextSibling;
  loPreviousSib := ANewNode.GetPrevSibling;

  if (not Assigned(loNextSib)) and (not Assigned(loPreviousSib)) then
    Result := 1       // only node in the group
  else if Assigned(loNextSib) then
    Result := TTaxonDictionaryNode(loNextSib.Data).SortCode  // we have nodes after this node
  else
    Result := TTaxonDictionaryNode(loPreviousSib.Data).SortCode + 1; // it is the last node in the group
end;  // GetSortCode

//==============================================================================
procedure TfrmTaxonDictEditor.ClearTaxonNames;
var liIdx: Integer;
begin
  with sgPersonalNames do begin
    for liIdx := 1 to RowCount - 1 do begin
      if Objects[0, liIdx] <> nil then TKeyData(Objects[0, liIdx]).Free;
      Rows[liIdx].Clear;
    end;
    RowCount := 2;
  end;
  // Clear the deleted keys list too.
  FDeletedUserNameKeys.Clear;
end;  // ClearTaxonNames;

//==============================================================================
{ Empty the Taxonomic Groups window }
procedure TfrmTaxonDictEditor.ClearTaxonGroupPage;
begin
  ClearSpecificGroupList(lbSystemGroup);
  ClearSpecificGroupList(lbArbitraryGroup);
end;  // ClearTaxonGroupPage

//==============================================================================
{ Clear a specific list on the taxon groups window }
procedure TfrmTaxonDictEditor.ClearSpecificGroupList(iListBox: TListBox);
var
  lCount : integer;
begin
  with iListBox.Items do
    for lCount := 0 to Count-1 do Objects[lCount].Free;
  iListBox.Clear;
end;  // ClearSpecificGroupList

//==============================================================================
procedure TfrmTaxonDictEditor.PopulatePersonalTaxonNames;
var lKey         : TKeyDataEditFlag;
    ltfTUNChecked: Boolean;
begin
  ClearTaxonNames;
  with dmGeneralData do begin
    SetupStandardQuery(qryAllPurpose, ssPreferredCommonName, FTaxonListItemKey);
    qryAllPurpose.Open;
    chkStandardCommonName.Caption := Format(ResStr_UserStandardCommonName,
                                     [qryAllPurpose.FieldByName('OfficialCommonName').AsString]);
    qryAllPurpose.Close;
  end;
  with sgPersonalNames, TdmTaxonDictEditor(DictionaryData).qryTaxonNames do begin
    RowCount := 2;
    if not Active then Open;
    First;
    ltfTUNChecked := false;
    while not Eof do begin
      with sgPersonalNames do begin
        // Save Item key in grid too
        lKey := TKeyDataEditFlag.Create;
        lKey.ItemKey := FieldByName('Taxon_User_Name_Key').AsString;
        lKey.CanEdit := (not AppSettings.RestrictFullEdit) or
            (FieldByName('Entered_By').AsString=AppSettings.UserID);
        Objects[0, RowCount - 1] := lKey;
        // Fill in the cells
        if FieldByName('Preferred').AsBoolean then begin
          Cells[0, RowCount - 1] := '+';
          ltfTUNChecked := true;
        end;
        Cells[1, RowCount - 1] := FieldByName('Item_Name').AsString;
        Cells[2, RowCount - 1] := GetLanguageFromCode(FieldByName('Language').AsString);
        RowCount := RowCount + 1;
      end;
      Next;
    end;
    // Close query;
    Close;
    // If no TaxonUserNames checked, check the official common one.
    chkStandardCommonName.Checked := not ltfTUNChecked;
    // If more than 2 rows, some records have been added from the database,
    // and we ended with an extra blank row, so remove that.
    if RowCount > 2 then RowCount := RowCount - 1;
  end;
end;  // PopulatePersonalTaxonNames

//==============================================================================
{ Fill the Taxon Groups page for the current taxa }
procedure TfrmTaxonDictEditor.PopulateTaxonGroup;
  //----------------------------------------------------------------------------
  procedure FillGroupList(iListBox: TListBox);
  var lTaxonName: TTaxonNames;
  begin
    with dmGeneralData.qryAllPurpose do begin
      Open;
      try
        { Loop through taxa listed in this group, create a taxon name object for each }
        while not Eof do begin
          lTaxonName := TTaxonNames.Create;
          with lTaxonName do begin
            TaxonListItemKey := FieldByName('Taxon_List_Item_Key').AsString;
            TaxonName        := FieldByName('Preferred_Name').AsString;
            CommonName       := FieldByName('Common_Name').AsString;
            TNItalic := FieldByName('Preferred_Name_Italic').AsBoolean;
            CNItalic := FieldByName('Common_Name_Italic').AsBoolean;
          end;
          // add into box }
          iListBox.Items.AddObject(lTaxonName.TaxonListItemKey, lTaxonName);
          Next;
        end;
      finally
        Close;
      end;
    end;
  end;  // FillGroupList
  //----------------------------------------------------------------------------
begin
  ClearTaxonGroupPage;
  dmGeneralData.qryAllPurpose.SQL.Text :=Format(SQL_TAXA_LIST_GROUP, [FTaxonListItemKey,
    'ITN.SYSTEM_SUPPLIED_DATA = 1 and ITG.SYSTEM_SUPPLIED_DATA=1']);
  FillGroupList(lbSystemGroup);
  dmGeneralData.qryAllPurpose.SQL.Text :=Format(SQL_TAXA_LIST_GROUP, [FTaxonListItemKey,
  'ITG.SYSTEM_SUPPLIED_DATA = 0']);
  FillGroupList(lbArbitraryGroup);
  FArbitraryGroupsEdited := False;
end;  // PopulateTaxonGroup

//------------------------------------------------------------------------------
{ Finds the language (eg. for Common Names) specified by the input two letter
  language code.
  Returns: If match found - a string containing the code and name of the language
           If not found   - a string flaggin the language as "unknown" }
function TfrmTaxonDictEditor.GetLanguageFromCode(const ALangCode: String): String;
var liIdx: Integer;
begin
  Result := '';
  { Find match between input code and the first 2 letters in the combo's list }
  for liIdx := 0 to cmbEnterLanguage.Items.Count - 1 do
    if CompareText(Copy(cmbEnterLanguage.Items[liIdx], 1, 2), ALangCode) = 0 then
    begin
      Result := cmbEnterLanguage.Items[liIdx];
      Exit; // Terminate search
    end;
  // if no match found, flag language as unknown
  if Result = '' then Result := '<unknown>';
end;  // GetLanguageFromCode

//==============================================================================
procedure TfrmTaxonDictEditor.btnAddClick(Sender: TObject);
var lrPosPopup: TPoint;
begin
  inherited;
  lrPosPopup := ClientToScreen(Point(btnAdd.Left, pnlButtons.Top + btnAdd.Top + btnAdd.Height));
  pmAdd.Popup(lrPosPopup.X, lrPosPopup.Y);
end;  // btnAddClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.actAddSiblingExecute(Sender: TObject);
var loNewNode: TFlyNode;
begin
  inherited;
  FAddingNewNode := true;
  with tvDictionary do
    if Items.Count = 0 then
      actAddChildExecute(Sender) // Assume a root node is required
    else if not Assigned(Selected) then
      raise ETaxonDictEditor.Create(ResStr_TreeAddFail)
    else begin
      loNewNode := Items.Add(Selected, ST_NEW_NODE_TEXT);
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
procedure TfrmTaxonDictEditor.actAddChildExecute(Sender: TObject);
var loNewNode: TFlyNode;
begin
  inherited;
  FAddingNewNode := true;
  with tvDictionary do begin
    // if no node is selected then .Selected = nil but add child will still work by adding root node
    if Assigned(Selected) then Selected.Expand(False); // ensure node is expanded first before adding new node
    loNewNode := Items.AddChild(Selected, ST_NEW_NODE_TEXT);
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
procedure TfrmTaxonDictEditor.AddItem(const ANewNode: TFlyNode);
var loNewDictNode: TTaxonDictionaryNode;
begin
  inherited;
  ClearFields;
  lblSelectedName.Caption := '';
  {need to select new node without calling the populatedetails() routine in the change event handler}
  tvDictionary.Selected  := ANewNode;  // selects node
  ANewNode.ImageIndex    := -1;    // set image to blank
  ANewNode.SelectedIndex := -1;

  //Create a new dictionary node object add it to Data of the new node
  loNewDictNode := TTaxonDictionaryNode.Create(nil);
  with loNewDictNode do begin
    ItemKey         := dmGeneralData.GetNextKey('TAXON_LIST_ITEM', 'Taxon_List_Item_Key');  // Get a new key for the new List Item
    TaxonVersionKey := dmGeneralData.GetNextKey('TAXON_VERSION', 'Taxon_Version_Key');  // Get a new key for the new taxon version
    TaxonKey        := dmGeneralData.GetNextKey('TAXON', 'Taxon_Key');
    SysSupplied     := False; // since we are adding this item
  end;
  ANewNode.Data := loNewDictNode;

  // Set contents of the Rank combo which is dependent on the parent node
  SetRankCombo(ANewNode);

  //Append the new taxon/taxon_version/taxon_list_item to the database
 	with TdmTaxonDictEditor(DictionaryData) do
    try
      //if query is not open then open it with the newly generated list item key to ensure no records are brought back
      if not qryGeneral.Active then begin
        // open a result set anyway for appending new records
        qryGeneral.Parameters.ParamByName('Key').Value := loNewDictNode.ItemKey;
        qryGeneral.Active := True;
      end;
      qryGeneral.Append;  // put record into addition state
      //qryCommonName.Append;
      dmGeneralData.SetNameIDAndDate(qryGeneral,'TLI_Entered_By','TLI_Entry_Date');
      lblEnteredBy.Caption := dmGeneralData.GetIndividualName(AppSettings.UserID);
      lblChangedBy.Caption := '';
      SetKeyFields;
      TdmTaxonDictEditor(DictionaryData).qryRank.First;
      qryGeneral.FieldByName('TLI_Taxon_Rank_Key').AsString := TdmTaxonDictEditor(DictionaryData).qryRank.FieldByName('Taxon_Rank_Key').AsString;

      RefreshLists(loNewDictNode);
    except
      qryGeneral.Cancel;
      raise;
    end;
  pcTaxondetails.ActivePage := tsGeneral;
  EnableDetails(emAdd);
end;  // AddItem

class function TfrmTaxonDictEditor.GetTaxonListItemCustodian(const AKey: TKeyString): string;
begin
  Result := dmGeneralData.Custodian('Taxon_List_Item', 'Taxon_List_Item_Key', AKey);
end;

//==============================================================================
procedure TfrmTaxonDictEditor.btnEditClick(Sender: TObject);
var
  lItemKey: String;
begin
  inherited;
  if not Assigned(tvDictionary.Selected) then Exit;

  if tvDictionary.Selected.Count <> 0 then
    if dbcmbRank.Text = '' then begin
      MessageDlg(ResStr_CorrectRankNeeded, mtInformation, [mbOk], 0);
      Exit;
    end;
  lItemKey := TNodeObject(tvDictionary.Selected.Data).ItemKey;
  FHaveCustody := HaveCustody;
  if dmGeneralData.HasFullEditAccess('Taxon_List_Item', 'Taxon_List_Item_Key', lItemKey) and
      HaveCustody then
    try
      if CheckDeletedNode(emEdit) then begin
        TdmTaxonDictEditor(DictionaryData).qryGeneral.Edit;
        dmGeneralData.SetNameIDAndDate(TdmTaxonDictEditor(DictionaryData).qryGeneral, 'TLI_Changed_By', 'TLI_Changed_Date');
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
procedure TfrmTaxonDictEditor.btnDeleteClick(Sender: TObject);
var loNode: TFlyNode;
    lsMsg : String;
begin
  inherited;
  loNode := tvDictionary.Selected;
  if loNode<>nil then begin
    DictionaryData.PopulateChildNodes(loNode);
    if loNode.HasChildren then begin
      if loNode.Count > 1 then lsMsg := ResStr_SeveralTaxa
                          else lsMsg := ResStr_AnotherTaxon;
      MessageDlg(ResStr_CannotDeleteTaxon + lsMsg, mtWarning, [mbOk], 0);
    end else if CheckDeletedNode(emDelete) then begin
      try
        // See if anyone is editing the record, and if not, go ahead and delete it
        with TdmTaxonDictEditor(DictionaryData).qryGeneral do begin
          Edit;
          Cancel;
        end;

        if MessageDlg(Format(ResStr_PermanentlyDeleteTaxon, [TTaxonDictionaryNode(loNode.Data).Title]),
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
          loNode := loNode.GetPrevVisible;
          if DeleteItem then begin
            RefreshDictionaryBrowser;
            frmMain.BroadcastMessage(WM_REFRESH_TAXON_DIC);
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

//------------------------------------------------------------------------------
function TfrmTaxonDictEditor.DeleteItem: Boolean;
var loNodeData: TDictionaryNode;
    loNode    : TFlyNode;
    lsKey     : TKeyString;
    lCursor: TCursor;
begin
  loNode     := tvDictionary.Selected;
  loNodeData := TDictionaryNode(loNode.Data);
  Result     := false;
  lCursor := HourglassCursor;
  try
    with TdmTaxonDictEditor(DictionaryData) do
      with qryGeneral do begin
        lsKey := FieldByName('T_TAXON_KEY').AsString;
        if not ReferencedInTable(lsKey) then begin
          // Update the Sort code fields for the list items following the inserted item
          // subtract 1 from all items after the deleted node
          UpdateSortCodeDel(FieldByName('TLI_SORT_CODE').AsInteger,
                            FieldByName('TLI_PARENT').AsString,
                            FieldByName('TLI_TAXON_LIST_VERSION_KEY').AsString);

          DelTaxonUserNames(FieldByName('TLI_TAXON_LIST_ITEM_KEY').AsString);
          DelStatuses(lsKey);
          DelFacts(lsKey);

          CleanGroupIndex(FieldByName('TLI_TAXON_LIST_ITEM_KEY').AsString);
          dmGeneralData.qryAllPurpose.Connection.Execute('Set xact_abort on begin tran');
          dmGeneralData.DelSources('Taxon_Sources', 'Taxon_Key', lsKey);
          // Delete list item
          dmGeneralData.ExecuteSQL('Delete from Taxon_Common_Name where Taxon_Version_Key in ' +
                    '(Select Taxon_Version_Key from Taxon_Version where Taxon_Key =' + QuotedStr(lsKey) + ')',
              ResStr_DelFail + ' - TAXON_COMMON_NAME table');
          dmGeneralData.ExecuteSQL('Delete from Index_Taxon_Synonym where Taxon_List_Item_Key in(' +
            'Select Taxon_List_Item_key from Taxon_List_Item where Taxon_Version_Key in ' +
                    '(Select Taxon_Version_Key from Taxon_Version where Taxon_Key =' + QuotedStr(lsKey) + '))',
              ResStr_DelFail + ' - INDEX_TAXON_Synonym table');
          //  KJG 15/12/2004
          //  VI8136 problem highlighted this when I was testing it.
          //  There is now a constraint based on recommended taxon name key.
          dmGeneralData.ExecuteSQL(
            'delete from INDEX_TAXON_NAME where RECOMMENDED_TAXON_LIST_ITEM_KEY in ' +
              ' ( Select taxon_list_item_key from Taxon_List_Item where Taxon_Version_Key in ' +
                '	( Select taxon_version_key from taxon_Version where Taxon_Key =' +
                QuotedStr(lsKey) + ' ) )',
                                   ResStr_DelFail + ' - INDEX_TAXON_NAME table (Recommended Name)');
          dmGeneralData.ExecuteSQL('Delete from Taxon_List_Item where Taxon_Version_Key in ' +
                    '(Select Taxon_Version_Key from Taxon_Version where Taxon_Key =' + QuotedStr(lsKey) + ')',
              ResStr_DelFail + ' - TAXON_LIST_ITEM table');
          //Delete;
          dmGeneralData.ExecuteSQL('Delete from Taxon_Version where Taxon_Key = ' + QuotedStr(lsKey),
              ResStr_DelFail + ' - TAXON_VERSION table');

          // Delete Taxon item form Taxon Table
          dmGeneralData.ExecuteSQL('DELETE FROM Taxon WHERE Taxon_Key=''' + lsKey + '''',
                                   ResStr_DelFail + ' - TAXON table');
          // Remove records from Index_Taxon_Name table
          dmGeneralData.ExecuteSQL('DELETE FROM Index_Taxon_Name WHERE Taxon_List_Item_Key = ''' +
                                   FTaxonListItemKey + '''',
                                   ResStr_DelFail + ' - INDEX_TAXON_NAME table');
          // Remove records from Index_Taxon_Synonym table
          dmGeneralData.ExecuteSQL('DELETE FROM Index_Taxon_Synonym ' +
                                   'WHERE Taxon_List_Item_Key = ''' + FTaxonListItemKey + ''' ' +
                                   'OR    Synonym_List_Item_Key = ''' + FTaxonListItemKey + ''' ',
                                   ResStr_DelFail + ' - INDEX_TAXON_NAME table');
          dmGeneralData.qryAllPurpose.Connection.Execute('Commit tran');
          if Assigned(loNodeData) then loNodeData.Free;
          //Refresh;
          loNode.Delete;
          Result := true;
        end else
          MessageDlg(ResStr_CannotDeleteTaxonRefObvser,
                     mtInformation, [mbOk], 0);
      end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // DeleteItem

//==============================================================================
procedure TfrmTaxonDictEditor.btnSaveClick(Sender: TObject);
var lTab   : TTabSheet;
    lCursor: TCursor;
begin
  inherited;
  lTab := pcTaxonDetails.ActivePage;
  pcTaxonDetails.ActivePage := tsGeneral;
  ValidateValue(dbeName.Text <> '',ResStr_TaxonNameRequiredEnter, dbeName);
  ValidateValue(dbcmbRank.Text <> '', ResStr_TaxonRankRequiredSelect, dbcmbRank);
  pcTaxonDetails.ActivePage := tsPersonalNames;
  ValidateValue(ValidateTaxonNames, ResStr_PersonalTaxonNameBlank, sgPersonalNames);
  pcTaxonDetails.ActivePage := lTab;
  lCursor := HourglassCursor;
  try
    // Save the data
    SaveItem;
    // In case the dictionary browser form is up, or the rucksack
    RefreshDictionaryBrowser;
  finally
    DefaultCursor(lCursor);
  end;
  //Send message to all open forms that the Taxon Dictionary has changed
  frmMain.BroadcastMessage(WM_REFRESH_TAXON_DIC);
  cmbEnterLanguage.Visible := false;
end;  // btnSaveClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.SaveItem;
var
  lLevel : integer;
  ltfAdd : boolean;
  lHaveCustody: Boolean;
begin
  lHaveCustody := dmGeneralData.Custodian('Taxon_List_Item', 'Taxon_List_Item_Key',
      TNodeObject(tvDictionary.Selected.Data).ItemKey) = AppSettings.SiteID;
  if TdmTaxonDictEditor(DictionaryData).qryGeneral.State in [dsEdit, dsInsert] then
  begin
    try
      SetFields;
      with TdmTaxonDictEditor(DictionaryData).qryGeneral do
      begin
        ltfAdd :=(rsNew in TdmTaxonDictEditor(DictionaryData).qryGeneral.RecordStatus);
        Post;
        if ltfAdd then
          Connection.Execute('INSERT INTO Taxon_Common_Name (Taxon_List_Item_Key, Taxon_Version_Key)' +
              'VALUES (' +
              QuotedStr(FieldValues['TLI_Taxon_List_Item_Key']) + ', ' +
              QuotedStr(FieldValues['TLI_Taxon_Version_Key']) + ')');
      end;
    except
      // reset the key fields if in add state as access removes the failed keys of the failed table
      if EditMode = emAdd then SetKeyFields;
      raise;
    end;
  end else
  if (EditMode = emEdit) and (lHaveCustody <> FHaveCustody) then begin
    TdmTaxonDictEditor(DictionaryData).qryGeneral.Cancel;
    MessageDlg(Format(ResStr_CustodyChanged, ['Taxon']), mtWarning, [mbOk], 0);
  end;

  try
    // Update lists
    FTaxonStatusList.Update;
    FTaxonStatusList.Refresh;
    FTaxonFactsList.Update;
    FTaxonFactsList.Refresh;
    // Sources
    Sources.Post;
    Sources.RefreshLists;
    // User defined Taxon Names
    SaveTaxonUserNames;
    // User defined taxonomic group info
    SaveArbitraryGroupInfo;
    // Add new items to taxonomic group of their parents
    if EditMode = emAdd then begin
      lLevel := 0; // need a var
      InsertNewTaxonToParentGroups(FTaxonListItemKey, tvDictionary.Selected, lLevel);
      // Insert new record in Index_Taxon_Name
      InsertNewIndexTaxonName;
      // New taxon is its own synonym
      InsertNewSynonym(FTaxonListItemKey, FTaxonListItemKey);
    end else
      UpdateIndexTaxonName;

    // Reload the display names
    TTaxonDictionaryNode(tvDictionary.Selected.Data).RefreshNamesObjects;

    RefreshNode(tvDictionary.Selected);

    EnableDetails(emView);
    FAddingNewNode := false;
    // Refresh the details side, unless the form is closing.
    if not FClosingForm then PostMessage(Handle, WM_SHOW_DETAILS, 0, 0);
    // Now notify whoever is interested
    NotifyDataItemChange;
  except
    // At this point qryGeneral.Post succeeded but status/facts/user names list failed so
    // we should put qryGeneral back into edit state for the next save operation
    if EditMode = emAdd then
      TdmTaxonDictEditor(DictionaryData).qryGeneral.Edit;
    raise;
  end;
end;  // SaveItem

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.SetFields;
begin
  with TdmTaxonDictEditor(DictionaryData) do
    case EditMode of
      emAdd:
          begin
            with qryGeneral do begin
              // Update the Sort code fields for the list items following the inserted item
              UpdateSortCodeAdd(FieldByName('TLI_SORT_CODE').AsInteger,
                                FieldByName('TLI_PARENT').AsString,
                                FieldByName('TLI_TAXON_LIST_VERSION_KEY').AsString);
              FieldByName('TLI_SYSTEM_SUPPLIED_DATA').AsBoolean := False;
              FieldByName('T_TAXON_NAME_TYPE_KEY').AsString := NONE_GIVEN_TAXON_NAME_TYPE_KEY;
            end;
            dmGeneralData.SetNameIDAndDate(qryGeneral, 'T_Entered_By', 'T_Entry_Date');
            dmGeneralData.SetNameIDAndDate(qryGeneral, 'TV_Entered_By', 'TV_Entry_Date');
            dmGeneralData.SetNameIDAndDate(qryGeneral, 'TLI_Entered_By', 'TLI_Entry_Date');
          end;
      emEdit:
          if qryGeneral.State = dsEdit then begin
            dmGeneralData.SetNameIDAndDate(qryGeneral, 'T_Changed_By', 'T_Changed_Date');
            dmGeneralData.SetNameIDAndDate(qryGeneral, 'TV_Changed_By', 'TV_Changed_Date');
            dmGeneralData.SetNameIDAndDate(qryGeneral, 'TLI_Changed_By', 'TLI_Changed_Date');
          end;
      else
          raise EInvalidState.Create(ResStr_SetFieldsCalledInvalid);
    end; // case
end;  // SetFields

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.RefreshNode(const ANode: TFlyNode);
var lqryGeneral: TJnccQuery;
begin
  if ANode <> nil then
    with TTaxonDictionaryNode(ANode.Data) do begin
      lqryGeneral := TdmTaxonDictEditor(DictionaryData).qryGeneral;
      {Set dictionary node object properties}
      ItemName  := lqryGeneral.FieldByName('T_ITEM_NAME').AsString; // Name
      Authority := lqryGeneral.FieldByName('T_AUTHORITY').AsString; // Authority
      SortCode  := lqryGeneral.FieldByName('TLI_SORT_CODE').AsInteger; // Sort Code (see also **SORT CODE** below)

      // find rank item by key and set sequence number
      RankKey := lqryGeneral.FieldByName('TLI_TAXON_RANK_KEY').AsString;

      {Set TTreenode properties}
      if AppSettings.DisplayTaxonCommonNames then  // add node with correct default name to search by
        ANode.Text := TaxonNamesObject.CommonName
      else
        ANode.Text := ItemName;
      ANode.ImageIndex    := ImageIndex;
      ANode.SelectedIndex := ImageIndex;

      UpdateNodesSortCode(ANode); //**SORT CODE**

      lblEnteredBy.Caption := dmGeneralData.GetIndividualName(lqryGeneral.FieldByName('TLI_ENTERED_BY').AsString);
      lblChangedBy.Caption := dmGeneralData.GetIndividualName(lqryGeneral.FieldByName('TLI_CHANGED_BY').AsString);
      lblSelectedName.Caption := Title;

      // Make sure the node's text is properly updated in the tree
      tvDictionary.Refresh;
    end; // with
end;  // RefreshNode

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.UpdateNodesSortCode(const ANode: TFlyNode);
var loNode: TFlyNode;
    liInc : Integer;
begin
  case EditMode of
    emAdd   : liInc := 1;
    emDelete: liInc := -1;
  else
    liInc := 0;
  end;

  {Must update SortCode settings for nodes after the inserted one}
  loNode := ANode.GetNextSibling;
  while Assigned(loNode) do begin
    with TTaxonDictionaryNode(loNode.Data) do
      SortCode := SortCode + liInc;
    loNode := loNode.GetNextSibling;
  end;
end;  // UpdateNodesSortCode

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.SaveTaxonUserNames;
var lqryTaxonNames: TJnccQuery;
    liIdx         : Integer;
    lqryDelTUN    : TJnccQuery;
  //----------------------------------------------------------------------------
  // Locate the record to edit in the query, otherwise
  function LocateRecord(const ATUNKey: TKeyString): Boolean;
  begin
    Result := false;
    with lqryTaxonNames do begin
      First;
      while not Result and not Eof do begin
        if FieldByName('Taxon_User_Name_Key').AsString = ATUNKey then begin
          Result := true;
          Exit;
        end;
        Next;
      end;
    end;
  end;
  //----------------------------------------------------------------------------
begin
  lqryTaxonNames := TdmTaxonDictEditor(DictionaryData).qryTaxonNames;
  with sgPersonalNames, lqryTaxonNames do begin
    if not Active then Open;
    for liIdx := 1 to RowCount - 1 do begin
      if (Objects[0, liIdx] = nil) and (Cells[1, liIdx] = '') then
        Continue; // next in loop - nothing in this row
      // Try to locate the record with the TUN key, otherwise, we would always edit the first record!!
      if (Objects[0, liIdx] <> nil) and LocateRecord(TKeyData(Objects[0, liIdx]).ItemKey) then begin
        if (AppSettings.UserAccessLevel > ualAddOnly) then begin
          Edit;
          FieldByName('Changed_By').AsString     := AppSettings.UserID;
          FieldByName('Changed_Date').AsDateTime := Now;

          // Setup query before overwriting the previous TUN, or we would lose the
          // only link to the right record in ITN table.
          dmGeneralData.qryAllPurpose.SQL.Text :=
              'UPDATE Index_Taxon_Name ' +
              'SET    Actual_Name = ''' + SafeQuotes(Cells[1, liIdx]) + ''' ' +
              'WHERE  Taxon_List_Item_Key = ''' + FTaxonListItemKey + ''' ' +
              'AND    Actual_Name = ''' + SafeQuotes(FieldByName('Item_Name').AsString) + '''';
        end;
      end
      else begin
        Append;
        FieldByName('Taxon_User_Name_Key').AsString := dmGeneralData.GetNextKey('Taxon_User_Name', 'Taxon_User_Name_Key');
        FieldByName('Taxon_List_Item_Key').AsString := FTaxonListItemKey;
        FieldByName('Entered_By').AsString   := AppSettings.UserID;
        FieldByName('Entry_Date').AsDateTime := Now;

        // Setup query to insert new record in ITN table, with most of the data
        // from the preferred name for the taxon.
        dmGeneralData.qryAllPurpose.SQL.Text :=
            'INSERT INTO Index_Taxon_Name (Taxon_List_Item_Key, Taxon_List_Version_Key, ' +
                        'Actual_Name, Actual_Name_Italic, Common_Name, Common_Name_Italic, ' +
                        'Preferred_Name, Preferred_Name_Italic, System_Supplied_Data) ' +
            'SELECT DISTINCT Taxon_List_Item_Key, Taxon_List_Version_Key, ''' + SafeQuotes(Cells[1, liIdx])
                 + ''', 0, ' +
                   'Common_Name, Common_Name_Italic, Preferred_Name, Preferred_Name_Italic, 0 ' +
            'FROM Index_Taxon_Name ITN WHERE ITN.Taxon_List_Item_Key = ''' + FTaxonListItemKey + '''';
      end;
      if lqryTaxonNames.State in [dsEdit, dsInsert] then begin
        FieldByName('Item_Name').AsString  := Cells[1, liIdx];
        FieldByName('Language').AsString   := Copy(Cells[2, liIdx], 1, 2);
        FieldByName('Preferred').AsBoolean := Cells[0, liIdx] = '+';
        Post;
        // Run query for update/insert
        dmGeneralData.qryAllPurpose.ExecuteSQL;
      end;
    end;
    Close;
  end;
  // Any record to delete?
  if FDeletedUserNameKeys.Count > 0 then begin
    lqryDelTUN := TJnccQuery.Create(nil);
    try
      dmDatabase.SetDatabaseLocal([lqryDelTUN]);
      for liIdx := 0 to FDeletedUserNameKeys.Count - 1 do begin
        try
          // Delete records from ITN table, while we can still have access to the TUN
          dmGeneralData.ExecuteSQL('DELETE FROM Index_Taxon_Name ' +
                                   'WHERE Index_Taxon_Name.Taxon_List_Item_Key = ''' + FTaxonListItemKey + ''' ' +
                                   'AND   Index_Taxon_Name.Actual_Name in (' +
                                          'SELECT TUN.Item_Name ' +
                                          'FROM Taxon_User_Name TUN ' +
                                          'WHERE TUN.Taxon_User_Name_Key = ''' + FDeletedUserNameKeys[liIdx] + ''')',
                                   ResStr_DelFail + ' - INDEX_TAXON_NAME table');
          // Now remove record from TUN table
          lqryDelTUN.SQL.Text := 'DELETE FROM Taxon_User_Name ' +
                                 'WHERE Taxon_User_Name_Key = ''' + FDeletedUserNameKeys[liIdx] + ''' ';
          lqryDelTUN.ExecSQL;
        except on E:Exception do
          raise ETaxonDictEditor.Create('Error saving user added names for taxon: ' + E.Message);
        end; // try..finally
      end;
    finally
      lqryDelTUN.Free;
    end;
  end;
  // Need to update Common_Name in all records in ITN table if a TUN has been checked
  liIdx := sgPersonalNames.Cols[0].IndexOf('+');

  if (AppSettings.UserAccessLevel > ualAddOnly) then
    with dmGeneralData.qryAllPurpose do begin
      // The next queries are SQL Server syntax, not Access
      ParseSQL := false;
      if liIdx > -1 then
        SQL.Text :=
            'UPDATE Index_Taxon_Name ' +
            'SET Common_Name = ''' + SafeQuotes(sgPersonalNames.Cells[1, liIdx]) + ''', ' +
            '    Common_Name_Italic = 0 ' +
            'FROM Index_Taxon_Name AS ITN ' +
            'INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key ' +
            'WHERE TLI.Preferred_Name = ''' + FTaxonListItemKey + ''''
      else begin
        SQL.Text :=
            'UPDATE Index_Taxon_Name ' +
            'SET Common_Name = T.Item_Name, ' +
            '    Common_Name_Italic = CASE WHEN TR.List_Font_Italic = 1 AND T.Language=''La'' THEN 1 ELSE 0 END ' +
            'FROM (((((Index_Taxon_Name ITN ' +
            'INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key) ' +
            'INNER JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key) ' +
            'INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key = TCN.Taxon_Version_Key) ';
        SQL.Add(
            'INNER JOIN Taxon T ON T.Taxon_Key = TV.Taxon_Key) ' +
            'INNER JOIN Taxon_List_Item AS TLI2 ON TLI2.Taxon_List_Item_Key = TLI.Preferred_Name) ' +
            'INNER JOIN Taxon_Rank AS TR ON TR.Taxon_Rank_Key = TLI2.Taxon_Rank_Key ' +
            'WHERE TLI.Preferred_Name = ''' + FTaxonListItemKey + '''');
      end;
      ExecuteSQL;
      ParseSQL := true;
    end;
end;  // SaveTaxonUserNames

//==============================================================================
procedure TfrmTaxonDictEditor.btnCancelClick(Sender: TObject);
var
  lAddNodeCancelled: boolean;
begin
  inherited;
  lAddNodeCancelled := False;
  TdmTaxonDictEditor(DictionaryData).qryGeneral.Cancel;
  // If it was a new node, remove from the tree now
  if EditMode = emAdd then
    with tvDictionary do begin
      if Selected <> nil then begin
        lAddNodeCancelled := True;
        if Assigned(Selected.Data) then TNodeObject(Selected.Data).Free;
        Selected.Delete;
      end;
      Selected := Items.GetFirstNode;
    end;
  EnableDetails(emView);
  FAddingNewNode := false;
  // Refresh the details side
  SendMessage(Handle, WM_SHOW_DETAILS, 0, 0);
  // Now notify whoever is interested, only if the node was pre-existing
  if not lAddNodeCancelled then
    NotifyDataItemChange;
end;  // btnCancelClick

//==============================================================================
function TfrmTaxonDictEditor.CheckReference(const AStatus: Boolean): Boolean;
var lsKey   : TKeyString;
    ldlgFind: TdlgFind;
    lsText  : String;
begin
  Result := true;
  if AStatus then lsText := eStatusReference.Text
             else lsText := eFactReference.Text;

  ldlgFind := TdlgFind.CreateDialog(nil, ResStr_FindDocument, ftReference);
  with ldlgFind do
    try
      if FindUnique(lsText) then begin
        lsText := ItemText;
        lsKey  := ItemKey;
      end else
      if not eSearchText.NoSourceItems then begin
        if ShowModal = mrOk then begin
          lsText := ItemText;
          lsKey  := ItemKey;
        end else
          Result := false;
      end else begin
        MessageDlg(ResStr_NoReferenceItems, mtInformation, [mbOK], 0);
        Result := false;
      end;
    finally
      Release;
    end;

  if Result then
    if AStatus then begin
      FCurrentStatusSource  := lsKey;
      eStatusReference.Text := lsText;
    end else begin
      FCurrentFactSource  := lsKey;
      eFactReference.Text := lsText; 
    end;
end;  // CheckReference

//==============================================================================
procedure TfrmTaxonDictEditor.ClearStatusFields;
begin
  cmbStatusType.KeyValue  := '';
  eStatusFrom.Text        := '';
  eStatusTo.Text          := '';
  eStatusGeoArea.Text     := '';
  eStatusConstraints.Text := '';
  reStatusDetails.Clear;
  eStatusReference.Text   := '';
  FCurrentStatusSource    := '';
end;  // ClearStatusFields

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.sgStatusesClick(Sender: TObject);
begin
  inherited;
  with sgStatuses do
    FCurrentStatus := TTaxonStatusItem(Objects[0, Row]);

  if FCurrentStatus = nil then begin
    btnStatusEdit.Enabled := false;
    btnStatusDel.Enabled  := false;
    ClearStatusFields;
  end else begin
    PopulateStatusItem;
    with FCurrentStatus do
      EnableDetailsAddEditButtons(TN_TAXON_DESIGNATION,
            'Taxon_Designation_Key', ItemKey,
            Custodian, EditMode, Added, btnStatusEdit, btnStatusDel);
    btnStatusEdit.Enabled := btnStatusEdit.Enabled and not FCurrentStatus.SystemSupplied;
    btnStatusDel.Enabled := btnStatusDel.Enabled and not FCurrentStatus.SystemSupplied;
  end;
end;  // sgStatusesClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.PopulateStatusItem;
begin
  with FCurrentStatus do begin
    cmbStatusType.KeyValue  := StatusTypeKey;
    eStatusFrom.Text        := DateFromAsString;
    eStatusTo.Text          := DateToAsString;
    eStatusGeoArea.Text     := GeographicArea;
    eStatusConstraints.Text := Constraints;
    Details.Position := 0;
    reStatusDetails.Lines.LoadFromStream(Details);
    FCurrentStatusSource    := SourceKey;
    eStatusReference.Text   := dmGeneralData.GetReferenceText(FCurrentStatusSource);
  end;
end;  // PopulateStatusItem

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.btnStatusAddClick(Sender: TObject);
begin
  inherited;
  FAdd := True;
  SwitchToDetails(sgStatuses, btnStatusAdd, btnStatusEdit, btnStatusDel,
                              btnSave, btnCancel, gbStatusDetails, True);
  ClearStatusFields;
  FCurrentStatus := TTaxonStatusItem.CreateNew(FTaxonStatusList);
  FDetailsMode   := False; // flag for allowing change of tab pages
end;  // btnStatusAddClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.btnStatusEditClick(Sender: TObject);
begin
  inherited;
  FAdd := False;
  SwitchToDetails(sgStatuses, btnStatusAdd, btnStatusEdit, btnStatusDel,
                              btnSave, btnCancel, gbStatusDetails, True);
  FDetailsMode := False; // flag for allowing change of tab pages
end;  // btnStatusEditClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.btnStatusDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteStatus, mtConfirmation, [mbNo, mbYes], 0) = mrYes then
  begin
    FTaxonStatusList.DeleteItem(sgStatuses.Row);
    sgStatusesClick(nil);
  end;
end;  // btnStatusDelClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.btnStatusOKClick(Sender: TObject);
begin
  inherited;
  // Sender is nil if called from ClooseQuery method.
  if Sender = nil then begin
    ValidateStatusFromDate;
    ValidateStatusToDate;
  end;

  ValidateValue(cmbStatusType.KeyValue <> '', ResStr_StatusTypeRequired, cmbStatusType);
  ValidateValue((eStatusReference.Text = '') or CheckReference(true), ResStr_InvalidStatusDocument, eStatusReference);

  SaveStatusItem;
  if FAdd then FTaxonStatusList.AddNew(FCurrentStatus);
  SwitchToDetails(sgStatuses, btnStatusAdd, btnStatusEdit, btnStatusDel,
                              btnSave, btnCancel, gbStatusDetails, False);
  FDetailsMode := True; // flag for allowing change of tab pages
  sgStatusesClick(nil);
end;  // btnStatusOkClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.btnStatusCancelClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgStatuses, btnStatusAdd, btnStatusEdit, btnStatusDel,
                              btnSave, btnCancel, gbStatusDetails, false);
  if FAdd then FCurrentStatus.Free;
  FDetailsMode := True; // flag for allowing change of tab pages
  sgStatusesClick(nil);
end;  // btnStatusCancelClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.SaveStatusItem;
begin
  if Assigned(FCurrentStatus) then
    with FCurrentStatus do begin
      DateFromAsString := eStatusFrom.Text;
      DateToAsString   := eStatusTo.Text;
      GeographicArea   := eStatusGeoArea.Text;
      Constraints      := eStatusConstraints.Text;
      Details.Position := 0;
      reStatusDetails.Lines.SaveToStream(Details);  
      StatusType       := cmbStatusType.Text;
      StatusTypeKey    := cmbStatusType.KeyValue;
      if eStatusReference.Text = '' then SourceKey := ''
                                    else SourceKey := FCurrentStatusSource;
    end;
end;  // SaveStatusItem

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.eStatusFromExit(Sender: TObject);
var lrCancelPos: TPoint;
begin
  inherited;
  lrCancelPos := btnStatusCancel.ScreenToClient(Mouse.CursorPos);
  if (lrCancelPos.X in [0..btnStatusCancel.Width]) and (lrCancelPos.Y in [0..btnStatusCancel.Height]) then
    btnStatusCancelClick(nil)
  else if not FClosingForm then
    ValidateStatusFromDate;
end;  // eStatusFromExit

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.ValidateStatusFromDate;
begin
  if eStatusFrom.Text <> '' then begin
    ValidateValue(IsDate(eStatusFrom.Text) and (StrToDate(eStatusFrom.Text) <= Date),
                  InvalidDate(ResStr_StartDate, false, true), eStatusFrom);
    eStatusFrom.Text := DateToStr(StrToDate(eStatusFrom.Text));
  end;
end;  // ValidateStatusFromDate

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.eStatusToExit(Sender: TObject);
var lrCancelPos: TPoint;
begin
  inherited;
  lrCancelPos := btnStatusCancel.ScreenToClient(Mouse.CursorPos);
  if (lrCancelPos.X in [0..btnStatusCancel.Width]) and (lrCancelPos.Y in [0..btnStatusCancel.Height]) then
    btnStatusCancelClick(nil)
  else if not FClosingForm then
    ValidateStatusToDate;
end;  // eStatusToExit

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.ValidateStatusToDate;
begin
  if eStatusTo.Text <> '' then begin
    ValidateValue(IsDate(eStatusTo.Text) and (StrToDate(eStatusTo.Text) <= Date),
                  InvalidDate(ResStr_EndDate, false, true), eStatusTo);
    eStatusTo.Text := DateToStr(StrToDate(eStatusTo.Text));
    ValidateValue(not IsVagueDateInVagueDate(StringToVagueDate(eStatusTo.Text), StringToVagueDate(eStatusFrom.Text)),
                  ResStr_StartContainEndDate, eStatusTo);
    ValidateValue(not IsVagueDateInVagueDate(StringToVagueDate(eStatusFrom.Text), StringToVagueDate(eStatusTo.Text)),
                  ResStr_EndContainStartDate, eStatusTo);
    ValidateValue(CompareVagueDateToVagueDate(StringToVagueDate(eStatusTo.Text), StringToVagueDate(eStatusFrom.Text)) >= 0,
                  ResStr_EndComesBeforeStartDate, eStatusTo);
  end;
end;  // ValidateStatusToDate

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.eStatusReferenceKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key = #13 then begin
    CheckReference(true);
    Key := #0;
  end;
end;  // eStatusReferenceKeyPress

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.btnStatusesReferenceFindClick(Sender: TObject);
var lfrmReferences: TfrmReferences;
begin
  inherited;
  if dmFormActions.actDocuments.Execute then
  begin
    lfrmReferences := TfrmReferences(frmMain.GetForm(TfrmReferences));
    SetupLink(lfrmReferences, Self, ReferenceStatusUpdate);
    if FCurrentStatusSource <> '' then
      lfrmReferences.FindAndDisplaySource(FCurrentStatusSource);
  end;
end;  // btnStatusesReferenceFindClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.ReferenceStatusUpdate(KeyList: TKeyList);
begin
  try
    if (KeyList <> nil) and (KeyList.Header.ItemCount > 0) then begin
      FCurrentStatusSource  := KeyList.Items[0].KeyField1;
      eStatusReference.Text := dmGeneralData.GetReferenceText(FCurrentStatusSource);
    end;
  finally
    KeyList.Free;
  end;
end;  // ReferenceStatusUpdate

//==============================================================================
procedure TfrmTaxonDictEditor.ClearFactsFields;
begin
  eFactTitle.Text       := '';
  cmbFactType.Text      := '';
  cmbFactType.ItemIndex := -1;
  vdeFactDate.Text      := '';
  reFact.Clear;
  eFactReference.Text   := '';
  FCurrentFactSource    := '';
end;  // ClearFactsFields

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.sgFactsClick(Sender: TObject);
begin
  inherited;
  with sgFacts do
    FCurrentFact := TTaxonFactsItem(Objects[0, Row]);

  if FCurrentFact = nil then begin
    btnFactEdit.Enabled   := false;
    btnFactDelete.Enabled := false;
    ClearFactsFields;
  end else begin
    PopulateFactsItem;
    with FCurrentFact do
      EnableDetailsAddEditButtons(TN_TAXON_FACT, 'Taxon_Fact_Key', ItemKey,
            Custodian, EditMode, Added, btnFactEdit, btnFactDelete);
    btnFactEdit.Enabled   := btnFactEdit.Enabled and not FCurrentFact.SystemSupplied;
    btnFactDelete.Enabled := btnFactDelete.Enabled and not FCurrentFact.SystemSupplied;
  end;
end;  // sgFactsClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.PopulateFactsItem;
var liIdx:integer;
begin
  ClearFactsFields;
  with cmbFactType do begin
    ItemIndex:=-1;
    for liIdx := 0 to Items.Count-1 do
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
procedure TfrmTaxonDictEditor.btnFactAddClick(Sender: TObject);
begin
  inherited;
  FAdd := True;
  SwitchToDetails(sgFacts, btnFactAdd, btnFactEdit, btnFactDelete,
                           btnSave, btnCancel, gbFactDetails, True);
  ClearFactsFields;
  FCurrentFact := TTaxonFactsItem.CreateNew(FTaxonFactsList);
  FDetailsMode := False; // flag for allowing change of tab pages
end;  // btnFactAddClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.btnFactEditClick(Sender: TObject);
begin
  inherited;
  FAdd := False;
  SwitchToDetails(sgFacts, btnFactAdd, btnFactEdit, btnFactDelete,
                           btnSave, btnCancel, gbFactDetails, True);
  FDetailsMode := False; // flag for allowing change of tab pages
end;  // btnFactEditClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.btnFactDeleteClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteFact, mtConfirmation, [mbNo, mbYes], 0) = mrYes then
  begin
    FTaxonFactsList.DeleteItem(sgFacts.Row);
    sgFactsClick(nil);
  end;
end;  // btnFactDeleteClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.btnFactOKClick(Sender: TObject);
begin
  inherited;
  // Sender is nil if called from CloseQuery method.
  if Sender = nil then ValidateFactDate;

  ValidateValue(cmbFactType.Text <> '', ResStr_FactTypeRequired, cmbFactType);
  ValidateValue(reFact.Text <> '', ResStr_FactDetailsRequired, reFact);
  ValidateValue((eFactReference.Text = '') or CheckReference(false), ResStr_FactDocInvalidSelect, eFactReference);
  SaveFactsItem;
  if FAdd then FTaxonFactsList.AddNew(FCurrentFact);
  SwitchToDetails(sgFacts, btnFactAdd, btnFactEdit, btnFactDelete,
                           btnSave, btnCancel, gbFactDetails, False);
  FDetailsMode := True; // flag for allowing change of tab pages
  sgFactsClick(nil);
end;  // btnFactOkClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.btnFactCancelClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgFacts, btnFactAdd, btnFactEdit, btnFactDelete,
                           btnSave, btnCancel, gbFactDetails,false);
  if FAdd then FCurrentFact.Free;
  FDetailsMode := True; // flag for allowing change of tab pages
  sgFactsClick(nil);
end;  // btnFactCancelClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.SaveFactsItem;
begin
  if Assigned(FCurrentFact) then
    with FCurrentFact do
    begin
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
procedure TfrmTaxonDictEditor.vdeFactDateExit(Sender: TObject);
var lrCancelPos: TPoint;
begin
  inherited;
  lrCancelPos := btnFactCancel.ScreenToClient(Mouse.CursorPos);
  if (lrCancelPos.X in [0..btnFactCancel.Width]) and (lrCancelPos.Y in [0..btnFactCancel.Height]) then
    btnFactCancelClick(nil)
  else if not FClosingForm then
    ValidateFactDate;
end;  // vdeFactDateExit

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.ValidateFactDate;
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
procedure TfrmTaxonDictEditor.eFactReferenceKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key = #13 then begin
    CheckReference(false);
    Key := #0;
  end;
end;  // eFactReferenceKeyPress

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.btnFactReferenceFindClick(Sender: TObject);
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
end;  // bbFactReferenceFindClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.ReferenceFactsUpdate(KeyList: TKeyList);
begin
  try
    if (KeyList <> nil) and (KeyList.Header.ItemCount > 0) then begin
      FCurrentFactSource  := KeyList.Items[0].KeyField1;
      eFactReference.Text := dmGeneralData.GetReferenceText(FCurrentFactSource);
    end;
  finally
    KeyList.Free;
  end;
end;  // ReferenceFactsUpdate

//==============================================================================
function TfrmTaxonDictEditor.ValidateTaxonNames: Boolean;
var liIdx: Integer;
begin
  Result := true;
  with sgPersonalNames do begin
    // If first row empty with no KeyData object, grid is empty and all is OK
    if (RowCount = 2) and (Objects[0, 1] = nil) then Exit;
    // Otherwise, check for missing stuff
    for liIdx := 1 to RowCount - 1 do
      if Cells[1, liIdx] = '' then begin
        Result := false;
        Exit;
      end;
  end;
end;  // ValidateTaxonNames

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.cmbEnterLanguageKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
  if Key in [#10, #13] then begin
    FNavigatingGrid := true;
    MoveGridCell(Key = #13);
    Key := #0;
  end;
end;  // cmbEnterLanguageKeyPress

//==============================================================================
procedure TfrmTaxonDictEditor.CheckPreferredTaxonName(const ARow: Integer);
var liIdx: Integer;
begin
  with sgPersonalNames do begin
    if Cells[0, ARow] = '+' then
      Cells[0, ARow] := ''
    else begin
      for liIdx := 1 to RowCount - 1 do
        Cells[0, liIdx] := '';
      Cells[0, ARow] := '+';
      // Checked a personal name, so uncheck the official name
      chkStandardCommonName.Checked := false;
    end;
  end;
end;  // CheckPreferredTaxonName

//==============================================================================
procedure TfrmTaxonDictEditor.SetActiveCellState(const AChar:Char);
begin
  with sgPersonalNames do begin
    if (Col = 0) and (AChar = #32) then begin
      CheckPreferredTaxonName(Row);
    end;
    if (Col = 1) and (not (goEditing in Options)) and CanEditTaxonNameRow(Row) then begin
      if AChar >= #32 then Cells[Col, Row] := AChar else
      if AChar <> #0  then Cells[Col, Row] := '';
      Options := Options + [goEditing];
    end else if (Col = 2) and cmbEnterLanguage.Visible then
      cmbEnterLanguage.SetFocus;
  end;
end;  // SetActiveCellState

//==============================================================================
{ MoveGridCell - navigates the current cell to the right or left in the grid.
     If you hit the end, goes to the next/previous row where one is available }
procedure TfrmTaxonDictEditor.MoveGridCell(const AToTheRight: Boolean);
begin
  FMovingGridCell := true;
  sgPersonalNames.SetFocus;
  if AToTheRight then begin  // Return
    with sgPersonalNames do
      if Col < ColCount - 1 then
        Col := Col + 1
      else begin
        if Row < RowCount - 1 then begin
          Row := Row + 1;
          Col := 0;
        end;
      end;
  end else begin  // Ctrl-Return
    with sgPersonalNames do
      if Col > 0 then
        Col := Col - 1
      else begin
        if Row > 1 then begin // don't move into title row
          Row := Row - 1;
          Col := ColCount - 1;
        end;
      end;
  end;
  FMovingGridCell := false;
end;  // MoveGridCell

//==============================================================================
procedure TfrmTaxonDictEditor.sgPersonalNamesClick(Sender: TObject);
begin
  inherited;
  cmbEnterLanguage.Visible := false;
  with sgPersonalNames do begin
    // Clicked on check box. User RowCount-1 to skip the '<new name>' row!
    if not FMovingGridCell and not FNavigatingGrid and
       (Col = 0) and (Row in [1..RowCount - 1]) then
      CheckPreferredTaxonName(Row);
    if Col = 1 then begin
      cmbEnterLanguage.Visible := false;
      if CanEditTaxonNameRow(sgPersonalNames.Row) then
        Options := Options + [goEditing]
      else
        Options := Options - [goEditing];
    end else
      Options := Options - [goEditing];
  end;
  FNavigatingGrid := false;
end;  // sgPersonalNamesClick

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.sgPersonalNamesDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var x, y: Integer;
begin
  inherited;
  with sgPersonalNames do begin
    if ((gdFocused in State) or (gdSelected in State)) and
       (ACol = 2) and (ARow in [1.. RowCount - 1]) and CanEditTaxonNameRow(ARow) then begin
      EditorMode := false;

      cmbEnterLanguage.Left := Rect.Left + Left + 1;
      cmbEnterLanguage.Top  := Rect.Top + Top + 1;
      cmbEnterLanguage.Width:= Rect.Right - Rect.Left + 2;
      cmbEnterLanguage.ItemIndex := cmbEnterLanguage.Items.IndexOf(Cells[ACol, ARow]);
      cmbEnterLanguage.Visible := true;
      cmbEnterLanguage.SetFocus;
    end;

    if (ACol = 0) and (ARow in [1.. RowCount - 1]) then begin
      EditorMode := false;
      Canvas.FillRect(Rect);
      x:=Rect.Left + (ColWidths[0] - 13) div 2;
      y:=Rect.Top + (RowHeights[ARow] - 13) div 2;
      DrawCheckBox(Canvas, x, y, Cells[ACol, ARow] = '+');
    end else begin
      Canvas.FillRect(Rect);
      DrawChoppedText(Cells[ACol, ARow], Canvas, Rect, 2);
    end;
  end;
end;  // sgPersonalNamesDrawCell

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.sgPersonalNamesTopLeftChanged(Sender: TObject);
begin
  inherited;
  cmbEnterLanguage.Visible := false;
end;  // sgPersonalNamesTopLeftChanged

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.sgPersonalNamesKeyPress(Sender: TObject; var Key: Char);
begin
  with sgPersonalNames do
    if Key in [#10, #13] then begin // return or shift return
      if Col in [0, 2] then begin
        Options := Options - [goEditing];
        MoveGridCell(Key = #13);
        Key := #0;
      end else
        MoveGridCell(Key = #13);
    end else
      SetActiveCellState(Key);
end;  // sgPersonalNamesKeyPress

//------------------------------------------------------------------------------
procedure TfrmTaxonDictEditor.sgPersonalNamesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  cmbEnterLanguage.Visible := false;
  FNavigatingGrid := (Key in [VK_HOME, VK_END, VK_LEFT, VK_RIGHT,
                              VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT]);
end;  // sgPersonalNamesKeyDown

//==============================================================================
procedure TfrmTaxonDictEditor.cmbEnterLanguageChange(Sender: TObject);
begin
  inherited;
  with sgPersonalNames do
    Cells[Col, Row] := cmbEnterLanguage.Text;
end;  // cmbEnterLanguageChange

//==============================================================================
procedure TfrmTaxonDictEditor.pmTaxonNamesInsertClick(Sender: TObject);
begin
  inherited;
  with sgPersonalNames do
    if Cells[1, RowCount - 1] <> '' then begin
      RowCount := RowCount + 1;
      Row := RowCount - 1;
      Col := 1;
      Cells[2, RowCount - 1] := DEFAULT_LANGUAGE;
      SetActiveCellState(#0);
    end;
end;  // pmTaxonNamesInsertClick

//==============================================================================
procedure TfrmTaxonDictEditor.pmTaxonNamesRenameClick(Sender: TObject);
begin
  inherited;
  sgPersonalNames.Col := 1;
  SetActiveCellState(#0);
end;  // pmTaxonNamesRenameClick

//==============================================================================
procedure TfrmTaxonDictEditor.pmTaxonNameDeleteClick(Sender: TObject);
var liIdx: Integer;
begin
  inherited;
  with sgPersonalNames do begin
    // If new user name being deleted, just discard it,
    // otherwise remember the key to update database
    if Objects[0, Row] <> nil then begin
      FDeletedUserNameKeys.Add(TKeyData(Objects[0, Row]).ItemKey);
      TKeyData(Objects[0, Row]).Free;
    end;
    // Move all rows up one notch
    for liIdx := Row to RowCount - 2 do
      Rows[liIdx].Assign(Rows[liIdx + 1]);
    // Reduce RowCount, only if more than 2 rows. Wnat to keep titles row
    Rows[RowCount - 1].Clear;
    if RowCount > 2 then RowCount := RowCount - 1;
  end;
end;  // pmTaxonNameDeleteClick

//==============================================================================
procedure TfrmTaxonDictEditor.chkStandardCommonNameClick(Sender: TObject);
var liIdx: Integer;
begin
  inherited;
  // If Official common name used, uncheck all user names
  if chkStandardCommonName.Checked then
    with sgPersonalNames do
      for liIdx := 1 to RowCount - 1 do Cells[0, liIdx] := '';
end;  // chkStandardCommonNameClick

//==============================================================================
{ Draw common names correctly }
procedure TfrmTaxonDictEditor.lbArbitraryGroupDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var lTaxonNames: TTaxonNames;
begin
  inherited;
  if not (csDestroying in Control.ComponentState) and
     Assigned(TListBox(Control).Items.Objects[Index]) then
  begin
    lTaxonNames := TTaxonNames(TListBox(Control).Items.Objects[Index]);
    GenericDrawTaxonNames(TListBox(Control).Canvas, lTaxonNames, Rect, False);
  end;
end;  // lbArbitraryGroupDrawItem

//==============================================================================
{ Display the Rucksacks popup menu when Load from Rucksack clicked.  The list
    is filled dynamically according to the available rucksacks }
procedure TfrmTaxonDictEditor.btnLoadFromRucksackClick(Sender: TObject);
var
  lScreenPoint : TPoint;
  lSearchRec: TSearchRec;
  //----------------------------------------------------------------------------
  procedure AddPopupItem(const iName : string);
  begin
    with pmRucksacks do begin
      Items.Add(TMenuItem.Create(nil));
      Items[Items.Count-1].Caption := iName;
      Items[Items.Count-1].OnClick := RucksackPopupClick;
    end;
  end;  // AddPopupItem
  //----------------------------------------------------------------------------
begin
  inherited;
  while pmRucksacks.Items.Count > 0 do
    pmRucksacks.Items.Delete(0);
  // Add in the Current rucksack
  if dmFormActions.GetFormInstance(TfrmRucksack) <> nil then
    AddPopupItem(STR_RUCKSACK);
  //Find the first rucksack file
  if FindFirst(Appsettings.RucksackPath+'*.ruk', 0, lSearchRec) = 0 then
  begin
    //Add the rucksack to the menu
    AddPopupItem(DuplicateCharacters(ExtractWithoutExt(lSearchRec.Name), '&'));
    //Add a new menu item for each remaining file
    while FindNext(lSearchRec) = 0 do
      AddPopupItem(DuplicateCharacters(ExtractWithoutExt(lSearchRec.Name), '&'));
    //Free the search results
    FindClose(lSearchRec);
  end;
  { Find bottom left of button in screen coords }
  lScreenPoint := btnLoadFromRucksack.ClientToScreen(Point(0, btnLoadFromRucksack.Height));
  RefreshXPMenu;
  { Popup menu attached to button }
  pmRucksacks.Popup(lScreenPoint.X, lScreenPoint.Y);
end;  // btnLoadFromRucksackClick

//==============================================================================
{ Event handler for rucksack popup menu items.  First, asks for confirmation.
     Then loads the content of the current rucksack into the arbitrary groups
     list }
procedure TfrmTaxonDictEditor.RucksackPopupClick(Sender: TObject);
var
  lRucksackName : String;
  lGoAhead : Boolean;

resourcestring
  ResStr_Confirm = 'This will replace the list of taxa selected as within the taxonomic '+
                'group %s with the list of taxa in the %s.  Do you want to continue?';
begin
  // Ensure the rucksack name has 'Rucksack' after it, so the message looks right
  if TMenuItem(Sender).Caption = STR_RUCKSACK then
    lRucksackName := STR_RUCKSACK
  else
    lRucksackName := TMenuItem(Sender).Caption + ResStr_Rucksack;
  // get confirmation to proceed if something is already assigned
  if lbArbitraryGroup.Items.Count = 0 then
    lGoAhead := True
  else
    lGoAhead := ConfirmYesNo(Format(ResStr_Confirm,
                             [lblSelectedName.Caption, lRucksackName])) = mrYes;
  if lGoAhead then begin
    ClearSpecificGroupList(lbArbitraryGroup);
    if lRucksackName = STR_RUCKSACK then
      LoadCurrentRucksackIntoGroup
    else
      LoadRucksackFileIntoGroup(StringReplace(TMenuItem(Sender).Caption, '&&', '&', [rfReplaceAll]));
  end;
end;  // RucksackPopupClick

//==============================================================================
{ Loads the content of the current rucksack into the arbitrary groups page }
procedure TfrmTaxonDictEditor.LoadCurrentRucksackIntoGroup;
var
  lCursor : TCursor;
begin
  lCursor := HourglassCursor;
  try
    LoadKeyListIntoGroup(AppSettings.CurrentRucksack.TaxonList)
  finally
    DefaultCursor(lCursor);
  end;
end;  // LoadCurrentRucksackIntoGroup

//==============================================================================
procedure TfrmTaxonDictEditor.LoadKeylistIntoGroup(iKeyList: TKeyList);
var
  i : Integer;
  lKeyStrings : String; // bulk list of ids to load into list box
  lCursor : TCursor;
begin
  LockWindowUpdate(lbArbitraryGroup.Handle);
  frmMain.SetStatus(ResStr_LoadingTaxonList);
  lCursor := HourglassCursor;
  try
    with iKeyList do begin
      for i := 0 to Header.ItemCount - 1 do
        // handle in blocks of 1000, or very last item
        if ((i mod 1000 = 0) and (i > 0)) or (i = Header.ItemCount - 1) then begin
          lKeyStrings := lKeyStrings + '''' + Items[i].KeyField1 + ''''; // last item - no comma
          AddToArbitraryTaxonGroup(lKeyStrings);
          lKeyStrings := ''; // start new list
        end else
          lKeyStrings := lKeyStrings + '''' + Items[i].KeyField1 + ''','
      end;
    FArbitraryGroupsEdited := True;
  finally
    LockWindowUpdate(0);
    DefaultCursor(lCursor);
    frmMain.TaskFinished;
  end;
end;  // LoadKeylistIntoGroup

//==============================================================================
{ Loads the content of a specified rucksack file into the arbitrary groups page }
procedure TfrmTaxonDictEditor.LoadRucksackFileIntoGroup(const iRucksackName: String);
var
  lCursor : TCursor;
begin
  lCursor := HourglassCursor;
  LockWindowUpdate(lbArbitraryGroup.Handle);
  try
    try
      AddToArbitraryTaxonGroup(dmGeneralData.GetRucksackTaxaAsCommaList(iRucksackName));
      FArbitraryGroupsEdited := True;
    except
      raise ETaxonDictEditor.CreateNonCritical(Format(ResStr_BadRucksack, [iRucksackName]));
    end;
  finally
    DefaultCursor(lCursor);
    LockWindowUpdate(0);
  end; // try
end;  // LoadRucksackFileIntoGroup

//==============================================================================
{ Add a taxon list item + ALL its children into the arbitrary group list box.
       iTLIKeys should be a comma separated list of keys, each item wrapped
       in single quotes. }
procedure TfrmTaxonDictEditor.AddToArbitraryTaxonGroup(const iTLIKeys: String);
var lTaxonName: TTaxonNames;
    lslKeys   : TStringList;
    liIdx     : Integer;
    key: TKeyString;
begin
  lslKeys := TStringList.Create;
  try
    lslKeys.QuoteChar := '''';
    lslKeys.Delimiter := ',';
    lslKeys.DelimitedText := iTLIKeys;
    for liIdx := 0 to lslKeys.Count - 1 do begin
      key := lslKeys[liIdx];
      if key <> '' then begin
        lTaxonName := dmGeneralData.GetTaxonNamesObject(key);
        // Add key to list so we can stop duplicates.  Add object to define
        //display of name

        lbArbitraryGroup.Items.AddObject(lTaxonName.TaxonListItemKey,
                                         lTaxonName);
      end;
    end;
    // Select last one added
    lbArbitraryGroup.ItemIndex := lbArbitraryGroup.Items.Count - 1;
  finally
    lslKeys.Free;
  end;  // try .. finally
end;  // AddToArbitraryTaxonGroup

//==============================================================================
{ Delete key in group list box primpts for confirmation, then removes all the
  selected items. }
procedure TfrmTaxonDictEditor.lbArbitraryGroupKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var lstMsg: String;
    liIdx : Integer;
begin
  inherited;
  if (Key = VK_DELETE) and (btnLoadFromRucksack.Enabled) then  // allowed to delete
    with lbArbitraryGroup do
      if ItemIndex <> -1 then begin  // something selected
        lstMsg := ResStr_SelectedItem;
        if SelCount > 1 then lstMsg := ResStr_SelectedItems;

        if ConfirmDeletionYesNo(lstMsg) = mrYes then
          for liIdx := Items.Count - 1 downto 0 do
            if Selected[liIdx] then begin
              TTaxonNames(Items.Objects[liIdx]).Free;
              Items.Delete(liIdx);
              FArbitraryGroupsEdited := True;
            end;
      end;
end;  // lbArbitraryGroupKeyDown

//==============================================================================
{ Save user defined taxonomic group information to the database }
procedure TfrmTaxonDictEditor.SaveArbitraryGroupInfo;
var i : integer;
    lslKeys: TStringList;
const
  SQL_INSERT_INTO_GROUP = 'INSERT INTO Index_Taxon_Group ' +
                          '(Taxon_List_Item_Key, Contained_List_Item_Key, System_Supplied_Data, Item_Level) '+
                          'VALUES (''%s'', ''%s'', 0, %s)';
begin
  if FArbitraryGroupsEdited then begin // skip if no changes made
    lslKeys := TStringList.Create;
    try
      with dmGeneralData.qryAllPurpose do begin
        // Get all the child keys for the taxa we are adding as arbitrary children of the current taxon 
        for i := 0 to lbArbitraryGroup.Items.Count - 1 do begin
          SQL.Text := SQL_SUBQUERY_TAXON_GROUPS + '(''' + TTaxonNames(lbArbitraryGroup.Items.Objects[i]).TaxonListItemKey + ''')';
          Open;
          try
            while not Eof do begin
              // Skip duplicates
              if lslKeys.IndexOf(FieldByName('CONTAINED_LIST_ITEM_KEY').AsString) = -1 then begin
                lslKeys.Add(FieldByName('CONTAINED_LIST_ITEM_KEY').AsString);
              end;
              Next;
            end;
          finally
            Close;
          end; // try
        end;
        // A bit of cleanup
        SQL.Text := 'DELETE FROM Index_Taxon_Group ' +
                    'WHERE System_Supplied_Data=0 ' +
                    'AND Taxon_List_Item_Key=''' + FTaxonListItemKey + '''';
        ExecSQL;
        // Now process all keys
        for i := 0 to lslKeys.Count-1 do begin
          SQL.Text := Format(SQL_INSERT_INTO_GROUP,
                             [FTaxonListItemKey, lslKeys[i], IntToStr(i)]);
          ExecSQL;
        end; // for
      end; // with
    finally
      lslKeys.Free;
    end;  // try .. finally
  end;
end;  // SaveArbitraryGroupInfo

//==============================================================================
{ When adding a new taxon, ensure it gets listed in the taxonomic groups of all
    nodes above it in the heirarchy.  Recursively moves up the hierarchy, and does
    the top items last so we can get the level right }
procedure TfrmTaxonDictEditor.InsertNewTaxonToParentGroups(const iKey: String;
  iParentNode: TFlyNode; var iLevel: Integer);
begin
  if Assigned(iParentNode) then begin  // break out at top!
    Inc(iLevel);
    with dmGeneralData.qryAllPurpose do begin
      SQL.Text := 'INSERT INTO Index_Taxon_Group ' +
                  '(TAXON_LIST_ITEM_KEY, CONTAINED_LIST_ITEM_KEY, SYSTEM_SUPPLIED_DATA, ITEM_LEVEL) ' +
                  'Values (''' + TNodeObject(iParentNode.Data).ItemKey + ''', ''' + iKey + ''', ' +
                  '1, ' + IntToStr(iLevel) + ')';
      ExecSQL;
    end;
    // and up a level
    InsertNewTaxonToParentGroups(iKey, iParentNode.Parent, iLevel);
  end;
end;  // InsertNewTaxonToParentGroups

//==============================================================================
procedure TfrmTaxonDictEditor.InsertNewIndexTaxonName;
var lstItalic: String;
begin
  // New taxon records are added with Latin as default language
  with dmGeneralData.qryAllPurpose do begin
    // Find out if the Italic fields should be set
    SQL.Text := 'SELECT TR.List_Font_Italic ' +
                'FROM Taxon_Rank TR INNER JOIN Taxon_List_Item TLI ON TR.Taxon_Rank_Key = TLI.Taxon_Rank_Key ' +
                'WHERE TLI.Taxon_List_Item_Key = ''' + FTaxonListItemKey + '''';
    Open;
    if not Eof then lstItalic := SQLBoolString[FieldByName('List_Font_Italic').AsBoolean];
    Close;
    // Now insert in Index_Taxon_Name
    SQL.Text := 'INSERT INTO Index_Taxon_Name (Taxon_List_Item_Key, Taxon_List_Version_Key, ' +
                'Actual_Name, Actual_Name_Italic, Common_Name, Common_Name_Italic, ' +
                'Preferred_Name, Preferred_Name_Italic, Authority, System_Supplied_Data) ';
    SQL.Add(Format('VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', 1)',
                   [FTaxonListItemKey, TdmTaxonDictEditor(DictionaryData).qryGeneral.FieldByName('TLI_TAXON_LIST_VERSION_KEY').AsString,
                    SafeQuotes(dbeName.Text), lstItalic, SafeQuotes(dbeName.Text), lstItalic,
                    SafeQuotes(dbeName.Text), lstItalic, SafeQuotes(dbeAuthority.Text)]));
    ExecSQL;
    dmDatabase.RunStoredProc('usp_IndexTaxonName_ApplyNameServer_SingleRecord',
        ['@TLIKey', FTaxonListItemKey]);


  end;
end;  // InsertNewIndexTaxonName

//==============================================================================
procedure TfrmTaxonDictEditor.UpdateIndexTaxonName;
var lstItalic: String;
begin
  // New taxon records are added with Latin as default language
  with dmGeneralData.qryAllPurpose do begin
    // Find out if the Italic fields should be set
    SQL.Text := 'SELECT TR.List_Font_Italic ' +
                'FROM Taxon_Rank TR '+
                'INNER JOIN Taxon_List_Item TLI ON TR.Taxon_Rank_Key = TLI.Taxon_Rank_Key ' +

                'WHERE TLI.Taxon_List_Item_Key = ''' + FTaxonListItemKey + '''';
    Open;
    if not Eof then lstItalic := SQLBoolString[FieldByName('List_Font_Italic').AsBoolean];
    Close;
    // Now update in Index_Taxon_Name. We assume (!!!) the user is not going to
    // enter a TUN identical to the Preferred_Name.
    SQL.Text := Format('UPDATE Index_Taxon_Name ' +
                       'SET Actual_Name = ''%s'', Actual_Name_Italic = %s, ' +
                       '    Preferred_Name = ''%s'', Preferred_Name_Italic = %s, ' +
                       '    Authority = ''%s'' ' +
                       'WHERE Taxon_List_Item_Key = ''%s'' ' +
                       'AND   Actual_Name = Preferred_Name',
                       [SafeQuotes(dbeName.Text), lstItalic, SafeQuotes(dbeName.Text),
                        lstItalic, SafeQuotes(dbeAuthority.Text), FTaxonListItemKey]);
    ExecuteSQL;
    // Need to update the Preferred_Name for all TUNs if any are present
    SQL.Text := Format('UPDATE Index_Taxon_Name ' +
                       'SET Preferred_Name = ''%s'' ' +
                       'WHERE Taxon_List_Item_Key = ''%s''',
                       [SafeQuotes(dbeName.Text), FTaxonListItemKey]);
    ExecuteSQL;
  end;
  // Ensure that Recommended_Taxon_List_Item_Key and other fields are updated for new names
  dmDatabase.RunStoredProc('usp_IndexTaxonName_ApplyNameServer_SingleRecord',
        ['@TLIKey', FTaxonListItemKey]);
end;  // UpdateIndexTaxonName


//==============================================================================
procedure TfrmTaxonDictEditor.InsertNewSynonym(const ATLIKey: TKeyString;
  const ASynonymKey: TKeyString);
begin
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := 'INSERT INTO Index_Taxon_Synonym (Taxon_List_Item_Key, Synonym_List_Item_Key) ' +
                'VALUES (''' + ATLIKey + ''', ''' + ASynonymKey + ''')';
    ExecuteSQL;
  end;
end;  // InsertNewSynonym

//==============================================================================
{Shows the references screen when this box is clicked}
procedure TfrmTaxonDictEditor.eStatusReferenceDblClick(Sender: TObject);
begin
  inherited;
  if FCurrentStatusSource <>'' then
    dmGeneralData.SourcesShowInternal(Sender, FCurrentStatusSource);
end;

//==============================================================================
{Shows the references screen when this box is clicked}
procedure TfrmTaxonDictEditor.eFactReferenceDblClick(Sender: TObject);
begin
  inherited;
  if FCurrentFactSource<>'' then
    dmGeneralData.SourcesShowInternal(Sender, FCurrentFactSource);
end;

function TfrmTaxonDictEditor.GetCurrentControlEditMode:TEditMode;
begin
  Result := EditMode; // default
  // Disable edit lock messages for all sub list grids
  if  pcTaxonDetails.ActivePage<>tsGeneral then
        Result :=emNone;
end;

//==============================================================================
//These functions control whether BaseChild warns the user that the data
//in the combo boxes is not editable because the data is from another site.
procedure TfrmTaxonDictEditor.dbComboKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  FKeyDown :=true;
end;

procedure TfrmTaxonDictEditor.dbComboKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  FKeyDown :=false;
end;

procedure TfrmTaxonDictEditor.dbComboClick(Sender: TObject);
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
//==============================================================================
procedure TfrmTaxonDictEditor.tvDictionaryKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_DELETE: if btnDelete.Enabled then btnDeleteClick(btnDelete);
  end;
end;

//==============================================================================
constructor TfrmTaxonDictEditor.Create(AOwner: TComponent);
begin
  inherited;
  // Captions for Personal Taxon Names
  SetGridColumnTitles(sgPersonalNames, ['', ResStr_TaxonName, ResStr_Language]);
  // Captions for statuses
  SetGridColumnTitles(sgStatuses, [ResStr_Type, ResStr_From, ResStr_To, ResStr_GeoArea, ResStr_Constraints]);
  // Captions for Facts
  SetGridColumnTitles(sgFacts, [ResStr_Type, ResStr_Date, ResStr_Title]);

  SwitchToDetails(sgStatuses, btnStatusAdd, btnStatusEdit, btnStatusDel,
                              btnSave, btnCancel, gbStatusDetails, false);
  SwitchToDetails(sgFacts, btnFactAdd, btnFactEdit, btnFactDelete,
                           btnSave, btnCancel, gbFactDetails, false);
  EnableDetails(emView);
  pcTaxonDetails.ActivePage:= tsGeneral;
  FDetailsMode := true;
end;

{-------------------------------------------------------------------------------
}
function TfrmTaxonDictEditor.CanEditTaxonNameRow(ARow: integer): Boolean;
begin
  if Assigned(sgPersonalNames.Objects[0, ARow]) then begin
    Assert(sgPersonalNames.Objects[0, ARow] is TKeyDataEditFlag);
    Result := TKeyDataEditFlag(sgPersonalNames.Objects[0, ARow]).CanEdit
  end else
    // no key on the row, so its a new name
    Result := true;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmTaxonDicteditor.ApplySecurity;
begin
  UpdateMenus;
end;

{------------------------------------------------------------------------------}
{ TKeyDataEditFlag }
{------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
}
procedure TKeyDataEditFlag.SetCanEdit(Value: boolean);
begin
  FCanEdit := Value;
end;

{-------------------------------------------------------------------------------
  When the right hand side panel is resized, resizes its contents to fit.
}
procedure TfrmTaxonDictEditor.pnlDetailsResize(Sender: TObject);
var
  deltaWidth, deltaHeight : Integer;
begin
  inherited;
  deltaWidth := pnlDetails.Width - scbTaxonDetails.Width;
  deltaHeight := pnlDetails.Height - scbTaxonDetails.Height;

  scbTaxonDetails.Width := pnlDetails.Width;
  scbTaxonDetails.Height := pnlDetails.Height;

  Sources.Width := Sources.Width + deltaWidth;
  Sources.Height := Sources.Height + deltaHeight;
end;

{-------------------------------------------------------------------------------
  When the form resizes, resize its contents.
}
procedure TfrmTaxonDictEditor.FormResize(Sender: TObject);
begin
  inherited;
  pnlDetailsResize(Sender)
end;

end.
