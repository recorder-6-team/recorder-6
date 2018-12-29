//==============================================================================
//  Unit:        IndOrg
//
//  Implements:  TfrmIndOrg
//
//  Description: Implements the functionalities to deal with individuals and
//               organisations, and allow details of a selected individual or
//               organisation to be viewed and/or edited.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     Eric Salmon - 07/02/2002 
//               DBRichEdit text attributes reset before displaying records.
//               Fixes the text attribute persistence problem.
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
//    $Revision: 264 $
//    $Date: 5/10/09 12:01 $
//    $Author: Bonnerearle $
//
//==============================================================================

{$I '..\..\Third Party\Dorset Software Services\DssVcl32\DelphiVersions.Inc'}

unit IndOrg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseChildUnit, StdCtrls, ComCtrls, ExtCtrls, Mask, Buttons, DBCtrls, Menus,
  Grids, BaseFormUnit, CompositeComponent, Sources, VagueDateEdit, Constants,
  DataClasses, DropStruct, DropSource, HierarchyNodes, NameData, DropTarget,
  ExceptionForm, BaseExportableUnit, VagueDate, GeneralData, Observations, Db,
  ValidationData, OnlineHelp, ActnList, Exgrid, RapTree, JNCCDatasets,
  GeneralFunctions, ImageListButton, DatabaseAccessADO, Variants, ComboListID,
  DataStringGrid, CRCommonClasses, SQLConstants, ControlStringGrid, ExternalFilter,
  KeyboardRapidTree, AddinCompositeComponent, AddinLinkedControls,ResourceStrings,
  ImgList,HierarchyFunctions;

resourcestring
  ResStr_DeleteFromDatabase = #13#13'It has been deleted from the database.';
  ResStr_PopTreeViewChild = ' - Populate Tree View Children';
  ResStr_ItemToDelete = 'Please select an item to delete';
  ResStr_RemoveAssocs = 'You cannot remove this Individual. Remove its associations first.';
  ResStr_OrgRemoveAssocs = 'You cannot remove this Organisation. Remove its associations first.';
  ResStr_DeleteItem = 'Are you sure you want to delete the selected item?';

  ResStr_IndExistsSave =  'An individual called %s %s already exists.'#13#13 +
                          'Do you still want to save?';

  ResStr_SurnameRequired =  'A Surname is required for every new Individual';
  ResStr_FullNameRequired = 'A full name is required for every Organisation';
  ResStr_CannotObjectAddress =  'Unable to obtain address for selected name';
  ResStr_DeleteAddress =  'Are you sure you want to delete this Address?';
  ResStr_AddressRequired =  'An Address is required for every individual.';
  ResStr_StartDateRequired =  'A start date is required for every Address.';
  ResStr_StartContainEndDate = 'The Start Date cannot contain the End Date.';
  ResStr_EndContainStartDate =  'The End Date cannot contain the Start Date.';
  ResStr_EndBeforeStartDate = 'The End Date cannot come before the Start Date.';
  ResStr_DeleteContactNum = 'Are you sure you want to delete this Contact Number?';
  ResStr_ContactTypeRequired =  'A Type is required for every contact number.';
  ResStr_NumberRequired = 'A number is required for every contact number.';
  ResStr_DeleteCommunication =  'Are you sure you want to delete this Communication?';
  ResStr_EmptyNameField = 'The Name field is empty. You have to enter a valid name.';
  ResStr_InvalidName =  'The Name is invalid. You have to enter a valid name.';
  ResStr_CommMissing =  'The Type of communication is missing. Enter a type or select one from the list.';
  ResStr_DateMissing =  'The Date is missing. Enter a valid date.';
  ResStr_DeleteAssoc =  'Are you sure you want to delete this Association?';
  ResStr_RoleMissing =  'The Role is missing. Enter a role or select one from the list.';
  ResStr_InvalidVagueDate = 'The vague date you have entered is not valid';
  ResStr_SpecifyFoundationDate =  'A Foundation Date must be specified before you can set an End Date.';
  ResStr_FoundContainEndDate =  'The Foundation Date cannot contain the End Date.';
  ResStr_EndContainFoundDate =  'The End Date cannot contain the Foundation Date.';
  ResStr_EndDateBeforeFoundDate = 'The End Date cannot come before the Foundation Date.';
  ResStr_DateOfDeathBirth = 'The Date of Death cannot be entered without a Date of Birth';
  ResStr_DateOfBirthDeath = 'The Date of Birth cannot contain the Date of Death.';
  ResStr_DeathContainBirthDate =  'The Date of Death cannot contain the Date of Birth.';
  ResStr_DeathBeforeBirth = 'The Date of Death cannot come before the Date of Birth.';
  ResStr_NoIMsg = 'There are no %s to relate this %s to.';
  ResStr_Individual = 'Individual';
  ResStr_Organisation = 'Organisation';
  ResStr_NoDocument = 'There are no documents to relate this %s to.';
  ResStr_NoMatch =  'There are no %s' + 's matching the filtering condition.';

  ResStr_UnselectIndividual = 'Unable to delete department.'#13#13 +
                              'The department you are trying to delete is linked to one or more individuals and cannot'#13 +
                              'be deleted until you unselect this department for those individuals';
  ResStr_DeleteDepartment = 'Are you sure you want to delete this department?';
  ResStr_CannotPopulateSelectedPage = 'Unable to populate the selected page';
  ResStr_FourLinesForAddress =  'Only four lines can be saved for an address.';
  ResStr_NoLocationToRelate = 'There are no Locations to relate this %s to.';
  ResStr_NotAllowedToDeleteOtherSitesDept = 'You are not allowed to delete this department '+
      'because it was entered on a different site and you do not have custody of the data.';



type
  ENameError = class(TExceptionPath);

  TNameMode = (nmIndividual, nmOrganisation);

  //frmIndOrg - Individuals and Organisations
  TfrmIndOrg = class(TBaseExportable)
    pmRelatedData        : TPopupMenu;
    mnuRelSurveys        : TMenuItem;
    mnuRelEvents         : TMenuItem;
    mnuRelSamples        : TMenuItem;
    mnuRelLocations      : TMenuItem;
    mnuRelOccur          : TMenuItem;
    mnuRelDocuments      : TMenuItem;
    pnlButtons           : TPanel;
    pnlButtons2          : TPanel;
    splIndOrg            : TSplitter;
    pnlIndOrg            : TPanel;
    pnlLabel             : TPanel;
    lblName              : TLabel;
    sbIndividuals        : TSpeedButton;
    sbOrganisations      : TSpeedButton;
    pnlLists             : TPanel;
    mnuEdit              : TMenuItem;
    mnuEditCut           : TMenuItem;
    mnuEditCopy          : TMenuItem;
    mnuEditPaste         : TMenuItem;
    mnuEditTransferData  : TMenuItem;
    mnuEditSep2          : TMenuItem;
    mnuEditFind          : TMenuItem;
    mnuEditSortBy        : TMenuItem;
    mnuEditSep3          : TMenuItem;
    mnuEditBold          : TMenuItem;
    mnuEditItalic        : TMenuItem;
    mnuEditUnderline     : TMenuItem;
    mnuEditAdd           : TMenuItem;
    mnuEditEdit          : TMenuItem;
    mnuEditDelete        : TMenuItem;
    mnuEditSep1          : TMenuItem;
    mnuEditFilter        : TMenuItem;
    pmAdd                : TPopupMenu;
    mnuEditAddIndividual : TMenuItem;
    mnuEditAddOrganisation: TMenuItem;
    pmAddIndividual      : TMenuItem;
    pmAddOrganisation    : TMenuItem;
    mnuEditSortForename  : TMenuItem;
    mnuEditSortSurname   : TMenuItem;
    mnuEditSortAcronym   : TMenuItem;
    mnuEditSortFullName  : TMenuItem;
    pmSortBy             : TPopupMenu;
    pmSortForename       : TMenuItem;
    pmSortSurname        : TMenuItem;
    pmSortAcronym        : TMenuItem;
    pmSortFullName       : TMenuItem;
    pmHierarchy          : TPopupMenu;
    pmHAdd               : TMenuItem;
    pmHAddOrganisation   : TMenuItem;
    pmHAddIndividual     : TMenuItem;
    pmHSortBy            : TMenuItem;
    pmHSortFullName      : TMenuItem;
    pmHSortAcronym       : TMenuItem;
    pmHSortSurname       : TMenuItem;
    pmHSortForename      : TMenuItem;
    N4                   : TMenuItem;
    bbShowAll            : TButton;
    alNames              : TActionList;
    actAddIndividual     : TAction;
    actAddOrganisation   : TAction;
    actSortForename      : TAction;
    actSortSurname       : TAction;
    actSortAcronym       : TAction;
    actSortFullName      : TAction;
    actFind              : TAction;
    actFilter            : TAction;
    actShowMetadata: TAction;
    mnuEditShowMetadata: TMenuItem;
    tvIndividuals: TKeyboardRapidTree;
    tvOrganisations: TKeyboardRapidTree;
    bbEdit: TImageListButton;
    bbDelete: TImageListButton;
    bbAdd: TImageListButton;
    bbRelatedData: TImageListButton;
    pmHQuickReports: TMenuItem;
    mnuDummy: TMenuItem;
    pmHValidateItem: TMenuItem;
    pmHBatchUpdate: TMenuItem;
    mnuPlaceHolder: TMenuItem;
    pnlIndOrgDetails: TPanel;
    pnlInner: TPanel;
    lblNamePrompt: TLabel;
    lblSelectedName: TLabel;
    bbSave: TImageListButton;
    bbCancel: TImageListButton;
    ilNames: TImageList;
    pcNameDetails: TPageControl;
    tsIndividual: TTabSheet;
    bvlIndividual: TBevel;
    lblITitle: TLabel;
    lblIForenames: TLabel;
    lblIIntitials: TLabel;
    lblISurname: TLabel;
    lblIAddress: TLabel;
    Label1: TLabel;
    mmIAddress: TMemo;
    dbeIForenames: TDBEdit;
    dbeIInitials: TDBEdit;
    dbeISurname: TDBEdit;
    dbcbTitle: TDBComboBox;
    cmbDept: TIDComboBox;
    tsOrganisation: TTabSheet;
    bvlOrganisation: TBevel;
    lblOAcronym: TLabel;
    lblType: TLabel;
    lblOFounded: TLabel;
    lblEnded: TLabel;
    Label40: TLabel;
    lblAddress: TLabel;
    mmOAddress: TMemo;
    dbeOAcronym: TDBEdit;
    dbeOFullName: TDBEdit;
    dbeODateFounded: TDBEdit;
    dbeODateEnded: TDBEdit;
    dbreComment: TDBRichEdit;
    dblcOOrgType: TDBLookupComboBox;
    tsAddresses: TTabSheet;
    splAddresses: TSplitter;
    gbAddressDetails: TGroupBox;
    Label19: TLabel;
    Label18: TLabel;
    Label21: TLabel;
    Label20: TLabel;
    Label22: TLabel;
    Label38: TLabel;
    eACountry: TEdit;
    rbAHome: TRadioButton;
    rbAWork: TRadioButton;
    cbAPreferred: TCheckBox;
    mmAAddress: TMemo;
    reAComments: TRichEdit;
    eAFrom: TVagueDateEdit;
    eATo: TVagueDateEdit;
    bbAAccept: TImageListButton;
    bbACancel: TImageListButton;
    eAPostCode: TEdit;
    pnlAddressesTop: TPanel;
    sgAAddresses: TStringGrid;
    bbAAdd: TImageListButton;
    bbAEdit: TImageListButton;
    bbADel: TImageListButton;
    tsContacts: TTabSheet;
    splContactNos: TSplitter;
    gbContactDetails: TGroupBox;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    eCPrefix: TEdit;
    eCNumber: TEdit;
    cbCPreferred: TCheckBox;
    cmbCType: TComboBox;
    reCConstraints: TRichEdit;
    bbCAccept: TImageListButton;
    bbCCancel: TImageListButton;
    pnlContactNosTop: TPanel;
    sgCContactNumbers: TStringGrid;
    bbCAdd: TImageListButton;
    bbCEdit: TImageListButton;
    bbCDel: TImageListButton;
    tsComms: TTabSheet;
    splComms: TSplitter;
    gbCODetails: TGroupBox;
    Label27: TLabel;
    Label29: TLabel;
    lblCommDate: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    cmbCOType: TComboBox;
    eCORef: TEdit;
    reCOContent: TRichEdit;
    eCODate: TVagueDateEdit;
    bbCOAccept: TImageListButton;
    bbCOCancel: TImageListButton;
    eCOName: TNameLinkedEdit;
    pnlCommsTop: TPanel;
    Shape3: TShape;
    sgCOComms: TStringGrid;
    bbCOAdd: TImageListButton;
    bbCOEdit: TImageListButton;
    bbCODel: TImageListButton;
    tsAssoc: TTabSheet;
    splAssocs: TSplitter;
    gbASDetails: TGroupBox;
    Label34: TLabel;
    Label37: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label28: TLabel;
    Label36: TLabel;
    cmbASRole: TComboBox;
    eASNameCode: TEdit;
    reASComment: TRichEdit;
    eASDateFrom: TVagueDateEdit;
    eASDateTo: TVagueDateEdit;
    bbASAccept: TImageListButton;
    bbASCancel: TImageListButton;
    eASName: TNameLinkedEdit;
    pnlAssocsTop: TPanel;
    Shape4: TShape;
    sgASAssocs: TStringGrid;
    bbASDel: TImageListButton;
    bbASEdit: TImageListButton;
    bbASAdd: TImageListButton;
    tsBiography: TTabSheet;
    Bevel3: TBevel;
    lblHHonorifics: TLabel;
    lblHDateOfBirth: TLabel;
    lblHDateOfDeath: TLabel;
    lblHFloreat: TLabel;
    lblHGeneralComments: TLabel;
    lblHActiveFrom: TLabel;
    dbeHHonorifics: TDBEdit;
    dbeHFloreat: TDBEdit;
    dbreComments: TDBRichEdit;
    eHDateOfBirth: TVagueDateEdit;
    eHDateOfDeath: TVagueDateEdit;
    eHActivePeriod: TVagueDateEdit;
    tsDepartments: TTabSheet;
    Label2: TLabel;
    sgDepartments: TControlStringGrid;
    btnDeptDel: TImageListButton;
    btnDeptAdd: TImageListButton;
    tsSources: TTabSheet;
    Sources: TSources;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbRelatedDataClick(Sender: TObject);
    procedure sbIndividualsClick(Sender: TObject);
    procedure sbOrganisationsClick(Sender: TObject);
    procedure tvChange(Sender: TObject; Node: TFlyNode);
    procedure bbAAddClick(Sender: TObject);
    procedure bbAAcceptClick(Sender: TObject);
    procedure bbCAddClick(Sender: TObject);
    procedure bbCAcceptClick(Sender: TObject);
    procedure bbCOAddClick(Sender: TObject);
    procedure bbCOAcceptClick(Sender: TObject);
    procedure bbASAddClick(Sender: TObject);
    procedure bbASAcceptClick(Sender: TObject);
    procedure splIndOrgCanResize(Sender: TObject;
      var NewSize: Integer; var Accept: Boolean);
    procedure FormResize(Sender: TObject);
    procedure splIndOrgPaint(Sender: TObject);
    procedure bbAddClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure bbEditClick(Sender: TObject);
    procedure tvExpanding(Sender: TObject; Node: TFlyNode;
      var AllowExpansion: Boolean);
    procedure bbCancelClick(Sender: TObject);
    procedure sgAAddressesClick(Sender: TObject);
    procedure pcNameDetailsChange(Sender: TObject);
    procedure sgCContactNumbersClick(Sender: TObject);
    procedure bbACancelClick(Sender: TObject);
    procedure bbAEditClick(Sender: TObject);
    procedure bbADelClick(Sender: TObject);
    procedure mmAAddressChange(Sender: TObject);
    procedure reEnter(Sender: TObject);
    procedure reExit(Sender: TObject);
    procedure bbCEditClick(Sender: TObject);
    procedure bbCDelClick(Sender: TObject);
    procedure bbCCancelClick(Sender: TObject);
    procedure sgCOCommsClick(Sender: TObject);
    procedure bbCOEditClick(Sender: TObject);
    procedure bbCODelClick(Sender: TObject);
    procedure bbCOCancelClick(Sender: TObject);
    procedure sgASAssocsClick(Sender: TObject);
    procedure bbASEditClick(Sender: TObject);
    procedure bbASDelClick(Sender: TObject);
    procedure bbASCancelClick(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DrawCellChoppedText(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure bbDeleteClick(Sender: TObject);
    procedure actAddIndividualExecute(Sender: TObject);
    procedure actAddOrganisationExecute(Sender: TObject);
    procedure pcNameDetailsChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure actSortForenameExecute(Sender: TObject);
    procedure actSortSurnameExecute(Sender: TObject);
    procedure actSortAcronymExecute(Sender: TObject);
    procedure actSortFullNameExecute(Sender: TObject);
    procedure actFilterExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure dbeODateEndedExit(Sender: TObject);
    procedure eAToExit(Sender: TObject);
    procedure eASDateToExit(Sender: TObject);
    procedure eHDateOfDeathExit(Sender: TObject);
    procedure eAFromExit(Sender: TObject);
    procedure eASDateFromExit(Sender: TObject);
    procedure dbeODateFoundedExit(Sender: TObject);
    procedure eCODateExit(Sender: TObject);
    procedure eHDateOfBirthExit(Sender: TObject);
    procedure eHActivePeriodExit(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuRelDocumentsClick(Sender: TObject);
    procedure mnuRelSurveysClick(Sender: TObject);
    procedure bbShowAllClick(Sender: TObject);
    procedure mnuRelEventsClick(Sender: TObject);
    procedure mnuRelSamplesClick(Sender: TObject);
    procedure mnuRelOccurClick(Sender: TObject);
    procedure mnuRelLocationsClick(Sender: TObject);
    procedure actShowMetadataExecute(Sender: TObject);
    procedure tvCompare(Sender: TObject; Node1,
      Node2: TFlyNode; var Compare: Integer);
    procedure tvIndividualsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cmbDeptPopulate(Sender: TObject);
    procedure tsDepartmentsShow(Sender: TObject);
    procedure pmHQuickReportsClick(Sender: TObject);
    procedure btnDeptAddClick(Sender: TObject);
    procedure btnDeptDelClick(Sender: TObject);
    procedure tvDrawCell(Sender: TObject; aCanvas: TCanvas;
      ACol, ARow: Integer; Rect: TRect; State: TExGridDrawState);
    procedure pmHBatchUpdateClick(Sender: TObject);
    procedure eCONameFindData(Sender: TObject);
    procedure eASNameFindData(Sender: TObject);
    procedure sgDepartmentsClick(Sender: TObject);
    procedure pnlIndOrgDetailsResize(Sender: TObject);
    procedure btnAdditionalInfoClick(Sender: TObject);
  private
    FdmName         :TdmName;
    FClosingForm    :boolean;
    FSelectedTree   :TRapidTree;
    FCurrentItemType:TNameMode;
    FOSelectedItem: TFlyNode;
    FISelectedItem: TFlyNode;
    FSelectedItem   :TFlyNode;
    FAddingNewNode  :boolean;
    FAddressList    :TAddressList;
    FCurrentAddress :TAddressItem;
    FContactList    :TContactList;
    FCurrentContact :TContactItem;
    FCommsList      :TCommsList;
    FCurrentComms   :TCommsItem;
    FAssocList      :TAssocList;
    FCurrentAssoc   :TAssocItem;
    FCurrentKey     :TKeyString;
    FParentKey      :TKeyString;
    FAdd            :Boolean;
    FIndSQL         :string;
    FOrgSQL         :string;
    FClearingTrees  :boolean;
    FAddressesShown :boolean;
    FContactsShown  :boolean;
    FCommsShown     :boolean;
    FAssocsShown    :boolean;
    FIndividualSort     : string;
    FOrganisationSort   : string;
    FOrgSortSQL         : string;
    FIndSortSQL         : string;
    FSortOrder          : String;
    FNeedOrgRefresh     : boolean; // need a refresh of the organisations tree?
    FNeedIndRefresh     : boolean; // need a refresh of the individuals tree?
    FDeptKey            : string;
    FDepartmentsPopulated : boolean;
    FDepartmentGridManager : TDataStringGrid;
    FIndTreeFiltered    : Boolean;
    FOrgTreeFiltered    : Boolean;
    FLoadedTabs         : TStringList;
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
    procedure WMRefreshDocuments(var Msg: TMessage); message WM_REFRESH_DOCUMENTS;
    procedure WMRefreshColours(var Msg: TMessage); message WM_REFRESH_COLOURS;
    procedure WMImportedComplete(var Msg: TMessage); message WM_IMPORTED_COMPLETE;

    procedure EnableDetails(const NewMode:Constants.TEditMode);
    procedure UpdateSubMenus;
    procedure BuildTreeView(TreeView: TRapidTree; const iSQLWhere:string);
    procedure PopulateChildren(AParent: TFlyNode);
    procedure PopulatePage(const PageIndex: Integer);
    procedure PopulateAddress(mmOutput: TMemo);
    procedure PopulateDepartment;
    procedure PopulateAssociatesRole;
    procedure RepopulateRecord;
    procedure BlankAddress;
    procedure SaveAddress;
    procedure BlankContact;
    procedure SaveContact;
    procedure BlankComms;
    procedure SaveComms;
    procedure BlankAssoc;
    procedure SaveAssoc;
    procedure AddItem(const ItemType: TNameMode);
    procedure DropCOName(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TstringList; var ioHandled : boolean);
    procedure DropASName(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TstringList; var ioHandled : boolean);
    procedure DropComm(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TstringList; var ioHandled : boolean);
    procedure DropAssoc(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TstringList; var ioHandled : boolean);
    procedure DragIndTV( const Sender : TObject;
      var oDropSource : TJNCCDropSource);
    procedure DragOrgTV( const Sender : TObject;
      var oDropSource : TJNCCDropSource);
    procedure SetCurrentItemType(const Value: TNameMode);
    procedure SetSelectedTree(const Value: TRapidTree);

    procedure FreeObjects;
    procedure SetupObjects;
    procedure ClearTrees(const Ind, Org:boolean);
    procedure RelatedObservations(const iMsg:string; const iRelType:TRelatedData;
      const SQLType:TStandardSQL);
    function GetNameKey: TKeyString;
    procedure KillChildren(iNode: TFlyNode);
    procedure PerformNodeChange(ANode: TFlyNode);
    function CheckDeletedNode(const AMode:Constants.TEditMode): boolean;
    procedure RefreshShownLists;

    property SelectedTree: TRapidTree read FSelectedTree write SetSelectedTree;
    property CurrentItemType: TNameMode read FCurrentItemType write SetCurrentItemType;
    procedure ValidateAddressFromDate;
    procedure ValidateAddressToDate;
    procedure ValidateAssocFromDate;
    procedure ValidateAssocToDate;
    procedure ValidateCommDate;
    procedure ValidateDateEnded;
    procedure ValidateDateFounded;
    procedure ValidateDateOfBirth;
    procedure ValidateDateOfDeath;
    procedure ValidateActivePeriod;
    procedure SetIndividualSort(const Value: string);
    procedure SetOrganisationSort(const Value: string);
    procedure PerformSort;
    class function GetNameCustodian(const AKey: TKeyString): string;
    procedure DeleteDepartment(ioRowKey : string);
    procedure UpdateDepartment(var ioRowKey : string; iData : TStringList);
    procedure PopulateBiographyDates(AQuery: TJNCCQuery);
    procedure RemoveFilter(treeview: TRapidTree);
    procedure ValidateCompareBirthToDeathDate;
  protected
    procedure SwitchToDetails(sgGrid : TStringGrid;iAdd, iEdit, iDel,
      iSave, iCancel:TControl; gbDetails:TGroupBox; tfDetails:boolean); override;
    procedure SetupDestinationControls; override;
    procedure ReadSortOrders; override;
    procedure WriteSortOrders; override;
    function GetCurrentControlEditMode: Constants.TEditMode; override;
    property IndividualSort : string read FIndividualSort write SetIndividualSort;
    property OrganisationSort : string read FOrganisationSort write SetOrganisationSort;
    function GetDetailsPageControl: TPageControl; override;
    function GetTreeView: TRapidTree; override;
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
      override;

    procedure ApplyFilter(AKeyList: TKeyList); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySecurity; override;
    procedure DisplayNames(const iNameList: TKeyList);
    procedure DisplayFilteredNames;
    function GetKeyList: TKeyList; override;
    function GetKeyListForValidation: TKeyList; override;
    function SelectNode(const ANodeType, ANodeKey: string): TFlyNode; override;
    function ItemKey: string; override;
    function CustomReportKeyType: TKeyType; override;
    procedure ExpandAllNodes; override;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, Maintbar, Find, ApplicationSettings, References, Locations,
  MetadataPopup, ADODb;


const
  FORENAME_SORT = 'Fore_Name';
  SURNAME_SORT  = 'Surname';
  FULLNAME_SORT = 'Full_Name';
  ACRONYM_SORT  = 'Acronym';

  ORDER_BY_FORENAME = 'ORDER BY I.Forename, I.Initials, I.Surname';
  ORDER_BY_SURNAME  = 'ORDER BY I.Surname, I.Initials, I.Forename';
  ORDER_BY_FULLNAME = 'ORDER BY O.Full_Name, O.Acronym';
  ORDER_BY_ACRONYM  = 'ORDER BY O.Acronym, O.Full_Name';

  SETTING_NAME = 'PrefNames';
//==============================================================================
constructor TfrmIndOrg.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FISelectedItem:=nil;
  FOSelectedItem:=nil;
  FSelectedItem :=nil;
  SetupObjects;
  FClearingTrees:=false;

  //Addresses
  SetGridColumnTitles(sgAAddresses, ['', ResStr_Address, ResStr_Postcode]);
  //Contact Numbers
  SetGridColumnTitles(sgCContactNumbers, ['', ResStr_Type, ResStr_Prefix, ResStr_Number]);
  //Communications
  SetGridColumnTitles(sgCOComms, [ResStr_Date, ResStr_With, ResStr_Type]);
  //Associations
  SetGridColumnTitles(sgASAssocs, [ResStr_With, ResStr_Role, ResStr_NameCode, ResStr_DateFrom, ResStr_DateTo]);

  lblSelectedName.Caption:='';
  //Enable/Disable controls
  SwitchToDetails(sgAAddresses, bbAAdd, bbAEdit, bbADel,
                  bbSave, bbCancel, gbAddressDetails, False);
  SwitchToDetails(sgCContactNumbers, bbCAdd, bbCEdit, bbCDel,
                  bbSave, bbCancel, gbContactDetails, False);
  SwitchToDetails(sgCOComms, bbCOAdd, bbCOEdit, bbCODel,
                  bbSave, bbCancel, gbCODetails, False);
  SwitchToDetails(sgASAssocs, bbASAdd, bbASEdit, bbASDel,
                  bbSave, bbCancel, gbAsDetails, False);

  //Set mode
  CurrentItemType         := nmIndividual;
  pcNameDetails.ActivePage:= tsIndividual;
  FSelectedTree           := tvIndividuals;
  tvIndividuals.Selected  := tvIndividuals.Items.GetFirstNode;

  //Get the organisation types
  FdmName.qryOrganisationType.Open;
  // Set menu state
  EnableDetails(Constants.emView);

  //Security
  pmHAdd.Enabled:= (AppSettings.UserAccessLevel >= ualAddOnly);
  pmHBatchUpdate.Visible := AppSettings.UserAccessLevel >= ualAdmin;
  SendMessage(Handle,WM_UPDATE_MENU_ICONS,0,0);

  //Help Setup
  Self.HelpContext           := IDH_NAMEADDR;
  mnuEdit.HelpContext        := IDH_EDITMENU;
  tsIndividual.HelpContext   := IDH_NAMEADDRINDIVS;
  tsOrganisation.HelpContext := IDH_NAMEADDRORGS;
  tsAddresses.HelpContext    := IDH_NAMEADDRADDRS;
  tsContacts.HelpContext     := IDH_NAMEADDRCONTACT;
  tsComms.HelpContext        := IDH_NAMEADDRCOMMS;
  tsAssoc.HelpContext        := IDH_NAMEADDRASSOCS;
  tsBiography.HelpContext    := IDH_NAMEADDRBIOG;
  tsSources.HelpContext      := IDH_NAMEADDRSOURCES;
  tsDepartments.HelpContext  := IDH_NAMEDEPTS;
  pcNameDetails.HelpContext  := IDH_NAMEADDRINDIVS;

  LoadFilter(FILTER_INDIVIDUAL);
  PopulateAssociatesRole();
end;  // Create

//==============================================================================
procedure TfrmIndOrg.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(true);
  frmMain.SetContextToolbar(Self,mnuEdit,4,[nil,nil,nil,nil,nil,nil,
                                            nil,nil,nil,nil,pmSortBy,
                                            nil,nil,nil,nil,nil,nil]);
  UpdateSubMenus;
end;  // FormActivate

//==============================================================================
procedure TfrmIndOrg.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  CanClose:=false;
  if EditMode <> emView then begin  // Add button disabled if adding or editing
    Beep;
    case ConfirmSaveAndClose of
      mrYes : begin
                if bbAAccept.Enabled then bbAAcceptClick(nil) else
                if bbCAccept.Enabled then bbCAcceptClick(nil) else
                if bbCOAccept.Enabled then bbCOAcceptClick(nil) else
                if bbASAccept.Enabled then bbASAcceptClick(nil);
                bbSaveClick(nil);
                CanClose:=true;
              end;
      mrNo  : begin
                FClosingForm:=true;
                if bbACancel.Enabled then bbACancelClick(nil) else
                if bbCCancel.Enabled then bbCCancelClick(nil) else
                if bbCOCancel.Enabled then bbCOCancelClick(nil) else
                if bbASCancel.Enabled then bbASCancelClick(nil);
                bbCancelClick(nil);
                CanClose:=true;
                FClosingForm:=false;
              end;
    end;
  end else
    CanClose:=true;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmIndOrg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FreeObjects;
  Action:=caFree;
end;  //FormClose

//==============================================================================
procedure TfrmIndOrg.SetupObjects;
begin
  // Data Module
  FdmName:= TdmName.Create(nil);
  // Setup a list to track tabs that don't need reloading
  FLoadedTabs := TStringList.Create;

  // keeps track of which treeview has the filter applied
  FIndTreeFiltered := false;
  FOrgTreeFiltered := false;  

  //Create data aware grids
  FAddressList:=TAddressList.Create(FdmName.qryAddresses, 'ADDRESS_KEY', sgAAddresses);
  FContactList:=TContactList.Create(FdmName.qryContacts, 'CONTACT_NUMBER_KEY', sgCContactNumbers);
  FCommsList  :=TCommsList.Create(FdmName.qryComms, 'COMMUNICATION_KEY', sgCOComms);
  FAssocList  :=TAssocList.Create(FdmName.qryAssocs, 'NAME_RELATION_KEY', sgASAssocs);

  //Populate tree views, no restrictive where clause at this point.
  FIndSQL:='';
  FOrgSQL:='';
  ClearTrees(true,true);
  BuildTreeView(tvIndividuals,'');
  SetIndividualSort(IndividualSort);  // Force resort after tree populated
  FNeedOrgRefresh := True;  // orgs not populated when screen opens
  // Sources
  Sources.Init(dmDatabase.LocalDatabase, 'Name_Sources',
               AppSettings.UserAccessLevel, dmGeneralData.GetNextKey,
               RegisterDropComponent, AppSettings.SiteId, AppSettings.ExternalFilePath);
  Sources.OnFindInternal:=dmGeneralData.SourcesFindInternal;
  Sources.OnAddInternal :=dmGeneralData.SourcesAddInternal;
  Sources.OnShowSource :=dmGeneralData.SourcesShowInternal;
end;  // SetupObjects

//==============================================================================
procedure TfrmIndOrg.FreeObjects;
begin
  ClearTrees(True, True);
  // Lists
  FContactList.Free;
  FContactList := nil;
  FAddressList.Free;
  FAddressList := nil;
  FCommsList.Free;
  FCommsList := nil;
  FAssocList.Free;
  FAssocList := nil;

  FDepartmentGridManager.Free;
  FDepartmentGridManager := nil;

  // Data Module
  FdmName.Free;
  FdmName:=nil;

  AppSettings.ClearFilteredRecords([TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION]);
  FLoadedTabs.Free;
end;  // FreeObjects

//==============================================================================
procedure TfrmIndOrg.ClearTrees(const Ind, Org:boolean);
var
  lCursor:TCursor;

    procedure ClearTree(ATree: TRapidTree);
    var
      liIdx  : Integer;
      lChildIdx: integer;
    begin
      with ATree do begin
        for liIdx := 0 to Items.Count - 1 do begin
          if Items[liIdx].HasChildren then
            for lChildIdx := 0 to Items[liIdx].Count-1 do
              TObject(Items[liIdx].Items[lChildIdx].Data).Free;
          TObject(Items[liIdx].Data).Free;
        end;
        Items.Clear;
      end; // with
    end;

begin
  lCursor:=HourglassCursor;
  FClearingTrees:=true;
  LockWindowUpdate(Handle);
  if csDestroying in ComponentState then
  begin
    tvOrganisations.Items.BeginUpdate;
    tvIndividuals.Items.BeginUpdate;
  end;
  try
    //Free all organisation nodes
    if Org then begin
      ClearTree(tvOrganisations);
      if FSelectedItem=FOSelectedItem then FSelectedItem:=nil;
      FOSelectedItem:=nil;
    end;
    //Free all individual nodes
    if Ind then begin
      ClearTree(tvIndividuals);
      if FSelectedItem=FISelectedItem then FSelectedItem:=nil;
      FISelectedItem:=nil;
    end;
  finally
    if csDestroying in ComponentState then
    begin
      tvOrganisations.Items.EndUpdate;
      tvIndividuals.Items.EndUpdate;
    end;
    LockWindowUpdate(0);
    FClearingTrees:=false;
    DefaultCursor(lCursor);
  end;
end;  // ClearTrees

//==============================================================================
procedure TfrmIndOrg.KillChildren(iNode:TFlyNode);
var lNode,lNextNode:TFlyNode;
begin
  lNode := iNode.GetLastChild;
  while Assigned(lNode) do begin
    // Remove all sub nodes before removing this node
    if lNode.HasChildren then
      KillChildren(lNode);

    TObject(lNode.Data).Free;  // Will work even if Data is nil
    lNextNode := lNode.GetPrevSibling;
    lNode.Delete;
    lNode := lNextNode;
  end;
  TNodeObject(iNode.Data).ChildrenPopulated:=false;
end;  // KillChildren

//==============================================================================
procedure TfrmIndOrg.SetSelectedTree(const Value: TRapidTree);
var
  lFirstShow: boolean;
begin
  //Swap modes if required
  if Value <> FSelectedTree then
  begin
    LockWindowUpdate(Handle);
    try
      if Assigned(Value.Selected) then lFirstShow := False
                                  else lFirstShow := True;
      // Hide Tree
      if FSelectedTree<>nil then begin 
        SelectedTree.Visible:=false;
        SelectedTree.SendToBack;
      end;
      // Change Tree
      FSelectedTree:=Value;
      // Show Tree
      FSelectedTree.Visible:=true;
      FSelectedTree.BringToFront;
      
      FSelectedTree.SetFocus;
    finally
      LockWindowUpdate(0);
    end;
    if SelectedTree = tvIndividuals then
    begin
      sbIndividuals.Down := true;
      sbOrganisations.Down := false;
    end
    else
    begin
      sbIndividuals.Down := false;
      sbOrganisations.Down := true;
    end;
    if not lFirstShow then
      if SelectedTree=tvIndividuals then PerformNodeChange(FISelectedItem)
                                    else PerformNodeChange(FOSelectedItem);
    // If some editing is already taking place, leave it that way
    if EditMode=emView then EnableDetails(Constants.emView);
  end;
end;  // SetSelectedTree

//==============================================================================
procedure TfrmIndOrg.sbIndividualsClick(Sender: TObject);
var
  lCursor : TCursor;
begin
  inherited;
  lCursor := HourglassCursor;
  try
    if FNeedIndRefresh then begin
      ClearTrees(true, false);
      BuildTreeView(tvIndividuals,'');
      FNeedIndRefresh := False;
    end;
    SelectedTree:=tvIndividuals;
    if EditMode=emView then begin
      LoadFilter(FILTER_INDIVIDUAL, False);
      if FISelectedItem = nil then begin
        SelectedTree.Selected := SelectedTree.Items.GetFirstNode;
        FISelectedItem := SelectedTree.Selected;
      end;
      FSelectedItem:=FISelectedItem;
      EnableDetails(emView);
    end;
    pcNameDetails.HelpContext := IDH_NAMEADDRINDIVS;
  finally
    DefaultCursor(lCursor);
  end;
end;  // sbIndividualsClick

//==============================================================================
procedure TfrmIndOrg.sbOrganisationsClick(Sender: TObject);
var
  lCursor : TCursor;
begin
  inherited;
  lCursor := HourglassCursor;
  try
    if FNeedOrgRefresh then begin
      ClearTrees(false, true);
      BuildTreeView(tvOrganisations,'');
      FNeedOrgRefresh := False;
    end;
    SelectedTree:=tvOrganisations;
    if EditMode=emView then begin
      LoadFilter(FILTER_ORGANISATION, False);
      if FOSelectedItem = nil then begin
        SelectedTree.Selected := SelectedTree.Items.GetFirstNode;
        FOSelectedItem := SelectedTree.Selected;
      end;
      FSelectedItem:=FOSelectedItem;
      EnableDetails(emView);
    end;
    pcNameDetails.HelpContext := IDH_NAMEADDRORGS;
  finally
    DefaultCursor(lCursor);
  end;
end;  // sbOrganisationsClick

//==============================================================================
procedure TfrmIndOrg.SetCurrentItemType(const Value: TNameMode);
begin
  //Swap modes if required
  if Value <> FCurrentItemType then
  begin
    //Show/Hide appropriate pages
    tsIndividual.TabVisible  := (Value = nmIndividual);
    tsOrganisation.TabVisible:= (Value = nmOrganisation);
    tsDepartments.TabVisible := (Value = nmOrganisation);
    if Value = nmIndividual then begin
      pcNameDetails.ActivePage:= tsIndividual;
      pcNameDetails.HelpContext := IDH_NAMEADDRINDIVS;
    end else begin
      pcNameDetails.ActivePage:= tsOrganisation;
      pcNameDetails.HelpContext := IDH_NAMEADDRORGS;
    end;
    //Show/Hide appropriate buttons
    rbAHome.Visible:= (Value = nmIndividual);
    rbAWork.Visible:= (Value = nmIndividual);
    tsBiography.TabVisible:= (Value = nmIndividual);
    FCurrentItemType := Value;
    UpdateSubMenus;
  end;
end;  // SetCurrentItemType

//==============================================================================
function TfrmIndOrg.CheckDeletedNode(const AMode:Constants.TEditMode):boolean;
var lSelNode  : TFlyNode;
    lstMessage: String;
begin
  Result := true;
  if not dmGeneralData.CheckKeyExists(TN_NAME, 'Name_Key', TNameNode(FSelectedItem.Data).ItemKey) then
  begin
    Result := false;
    case AMode of
      Constants.emView   : lstMessage := ResStr_CannotAccessRecord;
      Constants.emEdit   : lstMessage := ResStr_CannotEditRecord;
      Constants.emDelete : lstMessage := ResStr_CannotDeleteRecord;
    end;
    MessageDlg(lstMessage + ResStr_DeleteFromDatabase,
               mtInformation, [mbOk], 0);

    with SelectedTree do begin
      lSelNode := FSelectedItem.Parent;
      Items.Delete(FSelectedItem);
      if lSelNode = nil then Selected := Items.GetFirstNode
                        else Selected := lSelNode;
    end;
  end;
end;  // CheckDeletedNode

//==============================================================================
procedure TfrmIndOrg.BuildTreeView(TreeView:TRapidTree; const iSQLWhere:string);
var
  lslCurrentSQL: TStringList;
  lNewObject   : TNameNode;
  lNewNode     : TFlyNode;
  lCursor      : TCursor;
  lOrgOffset   : integer;
  tableName    : String;
  lPreferredCustodians : string;
begin
  //Set-up the query
  lCursor:=HourglassCursor;
  TreeView.Items.BeginUpdate; // for performance
  lPreferredCustodians := GetPreferredCustodians(SETTING_NAME);
  try
    with FdmName.qryPopulate do
    begin
      ParseSQL := false;
      if TreeView = tvIndividuals then begin
        lslCurrentSQL := FdmName.CreateTopLevelSQL(IND_TYPE, iSQLWhere, FIndSortSQL);
        FNeedIndRefresh := False; // inds are populated
        lOrgOffset := 0;
        tableName := TN_INDIVIDUAL;
      end
      else begin
        lslCurrentSQL := FdmName.CreateTopLevelSQL(ORG_TYPE, iSQLWhere, FOrgSortSQL);
        FNeedOrgRefresh := False; // orgs are populated
        lOrgOffset := 2;
        tableName := TN_ORGANISATION;
      end;
      SQL.Clear;
      SQL.AddStrings(lslCurrentSQL);
      lslCurrentSQL.Free;

      //Open the query
      try
        Open;
      except
        on E:Exception do
          raise ENameError.Create(ResStr_ReadFail + ' - Populate Tree View', E);
      end;

      //Add each record to the tree view
      while not Eof do
      begin
        lNewObject:= TNameNode.Create;
        lNewObject.ItemKey        := FieldByName('Name_Key').AsString;
        lNewObject.ItemAdditional := tableName;
        lNewObject.Text           := FieldByName('Node_Text').AsString;
        lNewObject.SysSupplied    := FieldByName('System_Supplied_Data').AsBoolean;
        lNewObject.IsFiltered     := False;
        if AppSettings.IndexOfFilteredRecord(TN_NAME, lNewObject.ItemKey) > -1 then begin
          lNewObject.IsFiltered := True;
          lNewObject.Hint       := AppSettings.GetFilteredRecordHint(TN_NAME, lNewObject.ItemKey);
        end else
        if AppSettings.IndexOfFilteredRecord(tableName, lNewObject.ItemKey) > -1 then begin
          lNewObject.IsFiltered := True;
          lNewObject.Hint       := AppSettings.GetFilteredRecordHint(tableName, lNewObject.ItemKey);
        end;
        lNewObject.ChildrenPopulated:=false;
        lNewNode:= TreeView.Items.AddObject(nil, FieldByName('Full_Name').AsString, lNewObject);
        if (pos (FieldByName('Custodian').value,lPreferredCustodians) <> 0)
          or (FieldByName('System_Supplied_Data').AsBoolean)
          or (AppSettings.UseOriginalIcons) then
            lnewNode.ImageIndex := 0 + lOrgOffSet
        else   lnewNode.ImageIndex := 1 + lOrgOffset;
        lnewnode.SelectedIndex := lnewNode.ImageIndex;
        // Add dummy child
        TreeView.Items.AddChild(lNewNode, '');
        Next;
      end;
      Close;
      ParseSQL := true;
    end;
    if (TreeView=tvIndividuals) then
      FIndTreeFiltered := (iSQlWhere<>'')
    else if (TreeView=tvOrganisations) then
      FOrgTreeFiltered := (iSQlWhere<>'');
  finally
    TreeView.Items.EndUpdate;
    DefaultCursor(lCursor);
  end;
end;  // BuildTreeView

//==============================================================================
procedure TfrmIndOrg.PopulateChildren(AParent: TFlyNode);
var lslCurrentSQL: TStringList;
    lNewObject   : TNameNode;
    lCursor      : TCursor;
    tableName    : String;
    lOrgOffset  : integer;
    lNewNode     : TFlyNode;
begin
  lCursor:=HourglassCursor;
  LockWindowUpdate(Handle);
  try
    //Set-up the query
    with FdmName.qryPopulate do
    begin
      ParseSQL := false;
      if SelectedTree = tvIndividuals then begin
        lOrgOffset := 2;
        tableName := TN_ORGANISATION;
        if (FOrgSQL <> '') and
           FdmName.HasFilteredChildNodes(IND_TYPE, TNameNode(AParent.Data).ItemKey, FOrgSQL) then
          lslCurrentSQL:= FdmName.CreateChildSQL(IND_TYPE, FOrgSortSQL, FOrgSQL)
        else
          lslCurrentSQL:= FdmName.CreateChildSQL(IND_TYPE, FOrgSortSQL);
      end else begin
        tableName := TN_INDIVIDUAL;
        lOrgOffset := 0;
        if (FIndSQL <> '') and
           FdmName.HasFilteredChildNodes(ORG_TYPE, TNameNode(AParent.Data).ItemKey, FIndSQL) then
          lslCurrentSQL:= FdmName.CreateChildSQL(ORG_TYPE, FIndSortSQL, FIndSQL)
        else
          lslCurrentSQL:= FdmName.CreateChildSQL(ORG_TYPE, FIndSortSQL);
      end;
      SQL.Clear;
      SQL.AddStrings(lslCurrentSQL);
      lslCurrentSQL.Free;

      Parameters.ParamByName('Parent').Value := TNameNode(AParent.Data).ItemKey;

      //Open the query
      try
        Open;
      except
        on E:Exception do
          raise ENameError.Create(ResStr_ReadFail + ResStr_PopTreeViewChild, E);
      end;

      //Populate the tree view
      while not Eof do
      begin
        lNewObject:= TNameNode.Create;
        lNewObject.ItemKey          := FieldByName('Name_Key').AsString;
        lNewObject.ItemAdditional   := tableName;
        lNewObject.Text             := FieldByName('Node_Text').AsString;
        lNewObject.SysSupplied      := FieldByName('System_Supplied_Data').AsBoolean;
        lNewObject.ChildrenPopulated:=true;
        lNewObject.IsFiltered := False;
        if AppSettings.IndexOfFilteredRecord(TN_NAME, lNewObject.ItemKey) > -1 then begin
          lNewObject.IsFiltered := True;
          lNewObject.Hint       := AppSettings.GetFilteredRecordHint(TN_NAME, lNewObject.ItemKey);
        end else
        if AppSettings.IndexOfFilteredRecord(tableName, lNewObject.ItemKey) > -1 then begin
          lNewObject.IsFiltered := True;
          lNewObject.Hint       := AppSettings.GetFilteredRecordHint(tableName, lNewObject.ItemKey);
        end;
        lNewNode := SelectedTree.Items.AddChildObject(AParent, FieldByName('Full_Name').AsString, lNewObject);
        if (pos (FieldByName('Custodian').value,GetPreferredCustodians(SETTING_NAME)) <> 0)
          or (FieldByName('System_Supplied_Data').AsBoolean)
          or (AppSettings.UseOriginalIcons) then
          lNewNode.ImageIndex := 0 + lOrgOffSet
        else   lnewNode.ImageIndex := 1 + lOrgOffset;
        lnewnode.SelectedIndex := lnewNode.ImageIndex;
        Next;
      end;
      Close;
      TNameNode(AParent.Data).ChildrenPopulated:=true;
      ParseSQL := true;
    end;
  finally
    LockWindowUpdate(0);
    DefaultCursor(lCursor);
  end;
end;  // PopulateChildren

//==============================================================================
procedure TfrmIndOrg.tvExpanding(Sender: TObject; Node: TFlyNode; var AllowExpansion: Boolean);
begin
  inherited;
  //If a dummy node exists, delete it and add the correct children
  if (Node<>nil) and not TNameNode(Node.Data).ChildrenPopulated then begin
    Node.DeleteChildren;
    PopulateChildren(Node);
  end;
end;  // tvExpanding

//==============================================================================
procedure TfrmIndOrg.PerformNodeChange(ANode:TFlyNode);
var lCursor:TCursor;
begin
  if EditMode=emView then begin
    FLoadedTabs.Clear;
    FSelectedItem :=ANode;
    if ANode<>nil then begin
      if not FAddingNewNode and CheckDeletedNode(emView) then begin
        lCursor:=HourglassCursor;
        LockWindowUpdate(Handle);
        try
          lblSelectedName.Caption:=DuplicateCharacters(ANode.Text, Char('&'));
          // Set current key
          FCurrentKey := TNameNode(ANode.Data).ItemKey;
          FDepartmentsPopulated := False;
          if pcNameDetails.ActivePage = tsDepartments then
            tsDepartmentsShow(nil);
          FCustodian := GetNameCustodian(FCurrentKey);
          if ((ANode.Level=0) and (SelectedTree=tvIndividuals)) or
             ((ANode.Level=1) and (SelectedTree=tvOrganisations)) then
          begin
            // Individual node selected
            CurrentItemType:=nmIndividual;
            PopulatePage(0);  // Always populate first page, i.e. individual's details
            if pcNameDetails.ActivePage.PageIndex<>0 then  // And current page
              PopulatePage(pcNameDetails.ActivePage.PageIndex);  // And current page
          end else begin
            // Organisation node selected
            CurrentItemType:=nmOrganisation;
            PopulatePage(1);  // Always populate first page, i.e. organisation's details
            if pcNameDetails.ActivePage.PageIndex<>1 then
              PopulatePage(pcNameDetails.ActivePage.PageIndex);  // And current page
          end;

  //        if Assigned(tvIndividuals.Selected) and
  //           Assigned(tvOrganisations.Selected) then begin//TODO - hack to get no items fixed
          
          ChangeAdditionalPage(pcNameDetails); // Notify additional COM pages of newly selected node
          NotifyDataItemChange;                // Notify COM addins of data item change

          // Update buttons state
          EnableDetails(emView);
        finally
          LockWindowUpdate(0);
          DefaultCursor(lCursor);
        end;
      end
    end else begin
      FCurrentKey := '';
      FCustodian := '';
      lblSelectedName.Caption:='';
      if SelectedTree=tvIndividuals then begin
        FISelectedItem :=nil;
        CurrentItemType:=nmIndividual;
        pcNameDetails.ActivePage:=tsIndividual;
        pcNameDetails.HelpContext := IDH_NAMEADDRINDIVS;
        PopulatePage(0);
      end else begin
        FOSelectedItem :=nil;
        CurrentItemType:=nmOrganisation;
        pcNameDetails.ActivePage:=tsOrganisation;
        pcNameDetails.HelpContext := IDH_NAMEADDRORGS;
        PopulatePage(1);
      end;
      EnableDetails(emView);
    end;
  end;
end;  // PerformNodeChange

//==============================================================================
procedure TfrmIndOrg.tvChange(Sender: TObject; Node: TFlyNode);
begin
  inherited;
  if not FClearingTrees then begin
    if Sender=tvIndividuals then begin
      FISelectedItem:=Node;
      PerformNodeChange(FISelectedItem);
    end else begin
      FOSelectedItem:=Node;
      PerformNodeChange(FOSelectedItem);
    end;
  end else begin
    // clearing, so no longer got a selected node
    FCurrentKey := '';

  end;
end;  // tvChange

//==============================================================================
procedure TfrmIndOrg.EnableDetails(const NewMode:Constants.TEditMode);
var tfOn:boolean;
    lData: TKeyDataSysSupplied;
  //----------------------------------------------------------------------------
  procedure SetOnePage(const iPage:TTabSheet);
  var iCount:integer;
      lCtrl :TControl;
  begin
    with iPage do
      for iCount:=0 to ControlCount-1 do begin
        lCtrl:=Controls[iCount];
        if not (lCtrl is TLabel) and not (lCtrl is TDBText) then
          lCtrl.Enabled:=tfOn;
      end;
  end;  // SetOnePage
  //----------------------------------------------------------------------------
begin
  FEditMode := NewMode;
  tfOn := EditMode <> emView;

  //Enable/Disable controls
  SetOnePage(tsIndividual);
  SetOnePage(tsOrganisation);
  pmHierarchy.AutoPopup:=not tfOn;

  if tfOn then begin
    dbreComments.PopupMenu:=dmFormActions.pmRTF;
    dbreComment.PopupMenu :=dmFormActions.pmRTF;
  end else begin
    dbreComments.PopupMenu:=nil;
    dbreComment.PopupMenu:=nil;
  end;

  SetRequiredFieldsColourState(tfOn, [dbeISurname,dbeOFullName]);
  SetOnePage(tsBiography);
  // Addresses
  bbAAdd.Enabled :=tfOn;
  sgAAddressesClick(nil);
  // Contact Numbers
  bbCAdd.Enabled :=tfOn;
  sgCContactNumbersClick(nil);
  // Comms
  bbCOAdd.Enabled :=tfOn;
  sgCOCommsClick(nil);
  // Assocs
  bbASAdd.Enabled :=tfOn;
  sgASAssocsClick(nil);
  // Biography
  eHDateOfBirth.ReadOnly := not (tfOn and HaveCustody);
  eHDateOfDeath.ReadOnly := eHDateOfBirth.ReadOnly;
  eHActivePeriod.ReadOnly := eHDateOfBirth.ReadOnly;
  // Sources
  Sources.EditMode:=EditMode;

  bbSave.Enabled  :=tfOn and not FEditDetails;
  bbCancel.Enabled:=tfOn and not FEditDetails;
  // Departments
  if Assigned(FDepartmentGridManager) then begin
    FDepartmentGridManager.Enabled := tfOn;
    btnDeptAdd.Enabled := tfOn;
    btnDeptDel.Enabled := tfOn and
      not FDepartmentGridManager.RowLocked(sgDepartments.Row);
  end;

  //Additional pages
  SetAdditionalPagesState(tfOn);

  // Buttons on bottom panels and menu item states
  bbAdd.Enabled   :=not tfOn and AddButtonState;
  if Assigned(FSelectedItem) then begin
    lData := GetStateDataFromNode(FSelectedItem);
    bbEdit.Enabled  :=not tfOn and (SelectedTree.Items.Count>0) and
                      (EditButtonState(lData, GetNameCustodian) or
                       (AppSettings.UserAccessLevel>=ualAddOnly));
    bbDelete.Enabled:=not tfOn and (SelectedTree.Items.Count>0) and
                      DeleteButtonState(lData) and
                      dmGeneralData.HasFullEditAccess(TN_NAME, 'NAME_KEY',
                      TNameNode(FSelectedItem.Data).ItemKey);
  end
  else begin
    bbEdit.Enabled := false;
    bbDelete.Enabled := false;
  end;

  mnuEditSortBy.Enabled:=not tfOn and (SelectedTree.Items.Count > 0);
  pmHSortBy.Enabled    :=mnuEditSortBy.Enabled;

  // Menu items
  mnuEditAdd.Enabled   :=bbAdd.Enabled;
  mnuEditEdit.Enabled  :=bbEdit.Enabled;
  mnuEditDelete.Enabled:=bbDelete.Enabled;
  // other buttons
  bbRelatedData.Enabled:=not tfOn;
  bbShowAll.Enabled    :=not tfOn;

  UpdateSubMenus;
end;  // EnableDetails

//==============================================================================
procedure TfrmIndOrg.UpdateSubMenus;
begin
  // Sort By sub-menu
  actSortForename.Visible:=(CurrentItemType = nmIndividual);
  actSortSurname.Visible :=mnuEditSortForename.Visible;
  pmSortForename.Default :=mnuEditSortForename.Visible;
  actSortAcronym.Visible :=(CurrentItemType = nmOrganisation);
  actSortFullName.Visible:=mnuEditSortAcronym.Visible;
  pmSortAcronym.Default  :=mnuEditSortAcronym.Visible;
  actFind.Enabled  :=(EditMode=emView) and
                     Assigned(SelectedTree) and (SelectedTree.Items.Count>0);
  actFilter.Enabled:=actFind.Enabled;
  actShowMetadata.Enabled := actFind.Enabled;
  dmFormActions.actTransferData.Enabled:=Assigned(RequestorForm) and actFind.Enabled;
  mnuEditSortBy.Enabled:=actFind.Enabled;
  EnableSortToolbutton(actFind.Enabled, pmSortBy);
  dmFormActions.SetActionVisibility(dmFormActions.actExport, actFind.Enabled);

  if not Assigned(FdmName) then
    dmFormActions.UpdateRTFMenu(false)
  else
    dmFormActions.UpdateRTFMenu(((ActiveControl is TDBRichEdit) and
                                 ((FdmName.qryIndividual.State in [dsEdit,dsInsert]) or
                                  (FdmName.qryOrganisation.State in [dsEdit,dsInsert]))) or
                                (ActiveControl is TRichEdit));
end;  // UpdateSubMenus


//==============================================================================
procedure TfrmIndOrg.SwitchToDetails(sgGrid : TStringGrid;iAdd, iEdit, iDel,
          iSave, iCancel:TControl; gbDetails:TGroupBox; tfDetails:boolean);
begin
  inherited;  // Base form does majority of work
  //Show required colour
  if sgGrid = sgAAddresses then
    SetRequiredFieldsColourState(tfDetails, [mmAAddress,eAFrom]);
  if sgGrid = sgCContactNumbers then
    SetRequiredFieldsColourState(tfDetails, [cmbCType,eCNumber]);
  if sgGrid = sgCOComms then
    SetRequiredFieldsColourState(tfDetails, [eCOName,cmbCOType,eCODate]);
  if sgGrid = sgASAssocs then
    SetRequiredFieldsColourState(tfDetails, [eASName,cmbASRole]);
end;  // SwitchToDetails


//==============================================================================
procedure TfrmIndOrg.pcNameDetailsChange(Sender: TObject);
begin
  inherited;
  with pcNameDetails do begin
    if FLoadedTabs.IndexOf(IntToStr(ActivePage.PageIndex))=-1 then
      PopulatePage(ActivePage.PageIndex);
    HelpContext := ActivePage.HelpContext;
  end;
end;  // pnNameDetailsChange

//==============================================================================
procedure TfrmIndOrg.PopulatePage(const PageIndex: Integer);
var qryCurrent: TJNCCQuery;
    ldbreCtrl : TDBRichEdit;
    lCursor   : TCursor;
begin
  lCursor   := HourglassCursor;
  ldbreCtrl := nil;
  try
    //Select appropriate query
    case PageIndex of
      0: qryCurrent:= FdmName.qryIndividual;   // Individual
      1: begin
           qryCurrent:= FdmName.qryOrganisation; // Organisation
           ldbreCtrl := dbreComment;
         end;
      2: qryCurrent:= FdmName.qryAddresses;    // Address
      3: qryCurrent:= FdmName.qryContacts;     // Contact
      4: qryCurrent:= FdmName.qryComms;        // Communications
      5: qryCurrent:= FdmName.qryAssocs;       // Associations
      6: begin
           qryCurrent:= FdmName.qryIndividual;   // Biography
           ldbreCtrl := dbreComments;
           PopulateBiographyDates(qryCurrent);
         end;
      7: qryCurrent:=nil;
    else
      qryCurrent:= nil;
    end;

    // Always set the Sources component properties. Very important!!!!
    Sources.SourcesFromKey:=FCurrentKey;
    Sources.RefreshLists;

    //If the query is not already on the correct record, re-run the query
    if Assigned(qryCurrent) then
    begin
      with qryCurrent do begin
        if not Active or (Parameters.ParamByName('Key').Value <> FCurrentKey) then
        begin
          Close;
          if ldbreCtrl <> nil then ResetDBRichEditControls([ldbreCtrl]);
          Parameters.ParamByName('Key').Value := FCurrentKey;
          try
            Open;
          except
            Raise Exception.Create(ResStr_CannotPopulateSelectedPage);
          end;
        end;
      end; // with
      //Populate non data-aware controls
      case PageIndex of
        0: begin
          PopulateAddress(mmIAddress);
          PopulateDepartment;
        end;
        1: PopulateAddress(mmOAddress);
        2: begin
             FAddressList.Refresh;
             FAddressList.NameKey:= FCurrentKey;
             bbAAdd.Enabled := (EditMode <> emView);
             sgAAddressesClick(nil);
             FAddressesShown:=true;
           end;
        3: begin
             FContactList.Refresh;
             FContactList.NameKey:= FCurrentKey;
             bbCAdd.Enabled := (EditMode <> emView);
             sgCContactNumbersClick(nil);
             FContactsShown:=true;
           end;
        4: begin
             FCommsList.Refresh;
             FCommsList.NameKey:= FCurrentKey;
             bbCOAdd.Enabled := (EditMode <> emView);
             sgCOCommsClick(nil);
             FCommsShown:=true;
           end;
        5: begin
             FAssocList.Refresh;
             FAssocList.NameKey:= FCurrentKey;
             bbASAdd.Enabled := (EditMode <> emView);
             sgASAssocsClick(nil);
             FAssocsShown:=true;
           end;
      end; // case
    end;
    FLoadedTabs.Add(IntToStr(PageIndex));
  finally
    DefaultCursor(lCursor);
  end;
end;  // PopulatePage

//==============================================================================
procedure TfrmIndOrg.RepopulateRecord;
begin
  //Force repopulation of all pages
  FLoadedTabs.Clear;
  with FdmName do
  begin
    qryIndividual.Close;
    qryOrganisation.Close;
    qryPrefAddress.Close;
    qryPrefContact.Close;
    qryAddresses.Close;
    qryContacts.Close;
    qryComms.Close;
    qryAssocs.Close;
  end;
  PerformNodeChange(FSelectedItem);
end;  // RepopulateRecord

//==============================================================================
procedure TfrmIndOrg.bbAddClick(Sender: TObject);
var PosPopup : TPoint;
begin
  inherited;
  PosPopup:=ClientToScreen(Point(bbAdd.Left,pnlButtons.Top+bbAdd.Top+bbAdd.Height));
  pmAdd.Popup(PosPopup.X,PosPopup.Y);
  { Add COM addin page }
  AddAdditionalPages;
end;  // bbAddClick

//==============================================================================
procedure TfrmIndOrg.actAddIndividualExecute(Sender: TObject);
begin
  inherited;
  FLoadedTabs.Clear;
  pcNameDetails.ActivePage:=tsIndividual;
  AddItem(nmIndividual);
end;  // actAddIndividualExecute

//==============================================================================
procedure TfrmIndOrg.actAddOrganisationExecute(Sender: TObject);
begin
  inherited;
  FLoadedTabs.Clear;
  pcNameDetails.ActivePage:=tsOrganisation;
  FDepartmentsPopulated := False;
  AddItem(nmOrganisation);
end;  // actAddOrganisationExecute

//==============================================================================
procedure TfrmIndOrg.AddItem(const ItemType: TNameMode);
var lParentNode, lNewNode: TFlyNode;
    lNewObject: TNameNode;
    lQuery: TJNCCQuery;
    lCursor:TCursor;
    lSelectedType:TNameMode;
begin
  inherited;
  lCursor:=HourglassCursor;
  try
    //Locate the correct tree to add the item to
    if SelectedTree = tvIndividuals then
      lSelectedType:=nmIndividual
    else
      lSelectedType:=nmOrganisation;

    //If we are not adding an item to its own tree view we need a parent node
    if ItemType = lSelectedType then
    begin
      lParentNode:= nil;
      FParentKey := '';
    end else
      if not Assigned(FSelectedItem) then
        raise TExceptionPath.CreateNonCritical(ResStr_TreeAddFail)
      else begin
        if FSelectedItem.Level = 0 then begin
          lParentNode:= FSelectedItem;
          lParentNode.Expand(False); //Expand first time to get rid of dummy node and populate proper children
        end else
          lParentNode:= FSelectedItem.Parent;
        FParentKey:= TNameNode(lParentNode.Data).ItemKey;
      end;

    FAddingNewNode := true;
    //Create a new key item and add a new node to the correct tree view
    lNewObject := TNameNode.Create;
    lNewObject.ItemKey := dmGeneralData.GetNextKey(TN_NAME,'Name_Key');
    FCurrentKey := lNewObject.ItemKey;
    FCustodian  := AppSettings.SiteID;
    //must call create on selected item or automatically triggered by Addchild object, and access violation
    lNewNode := SelectedTree.Items.AddChildObject(lParentNode, ResStr_NewItem, lNewObject);
    lblSelectedName.Caption := '';
    SelectedTree.Selected:=lNewNode;
    if Assigned(lParentNode) then
      lParentNode.Expand(False); //Expand second time to deal with no previous children

    //Populate the detail with the new item (Must be in view mode here so that the populate will work)
    FAddingNewNode:=false;

    CurrentItemType:=ItemType;
    //Append the new item to the database
    if CurrentItemType = nmIndividual then begin
      PopulatePage(0);
      lQuery:= FdmName.qryIndividual;
      mmIAddress.Clear;
    end else begin
      PopulatePage(1);
      lQuery:= FdmName.qryOrganisation;
      mmOAddress.Clear;
    end;
    with lQuery do begin
      Append;
      FieldByName('Name_Key').AsString := FCurrentKey;
      dmGeneralData.SetNameIDAndDate(lQuery,'Entered_By','Entry_Date');
      if CurrentItemType = nmOrganisation then begin
        dblcOOrgType.KeyValue:=NONE_RECORD_KEY;
        FieldByName('Organisation_Type_Key').AsString:=NONE_RECORD_KEY;
      end;
    end;
    EnableDetails(emAdd);
  finally
    DefaultCursor(lCursor);
  end;
end;  // AddItem

//==============================================================================
procedure TfrmIndOrg.bbEditClick(Sender: TObject);
begin
  inherited;
  //Edit dataset, only if FullUser or Admin
  RepopulateRecord;
  try
    if CheckDeletedNode(Constants.emEdit) then begin
      if dmGeneralData.HasFullEditAccess(TN_NAME, 'Name_Key',
          TNameNode(FSelectedItem.Data).ItemKey) and HaveCustody then
        if CurrentItemType = nmIndividual then begin
          FdmName.qryIndividual.Edit;
          dmGeneralData.SetNameIDAndDate(FdmName.qryIndividual,'Changed_By','Changed_Date');
        end else begin
          FdmName.qryOrganisation.Edit;
          dmGeneralData.SetNameIDAndDate(FdmName.qryOrganisation,'Changed_By','Changed_Date');
        end;
      //Set current node
      EnableDetails(Constants.emEdit);
    end;
  except
    on E:Exception do
      if dmDatabase.CheckError(E, dbeRecordLocked) then
        MessageDlg(ResStr_CannotEditRecord + #13#13 +
                   dmDatabase.GetErrorMessage(E.Message, dbeRecordLocked),
                   mtInformation, [mbOk], 0);
      else
        Raise;
  end;
end;  // bbEditclick

//==============================================================================
procedure TfrmIndOrg.bbDeleteClick(Sender: TObject);
var lDelResult:boolean;
    lCursor   :TCursor;
begin
  inherited;
  //If an item has been selected, delete it
  if not Assigned(FSelectedItem) then
    MessageDlg(ResStr_ItemToDelete, mtInformation, [mbOK], 0)
  else begin
    // Make sure the node is properly populated before attempting to remove it
    if not TNameNode(FSelectedItem.Data).ChildrenPopulated then begin
      FSelectedItem.DeleteChildren;
      PopulateChildren(FSelectedItem);
    end;

    if FSelectedItem.HasChildren then
      if SelectedTree=tvIndividuals then
        MessageDlg(ResStr_RemoveAssocs,mtInformation,[mbOk],0)
      else
        MessageDlg(ResStr_OrgRemoveAssocs,mtInformation,[mbOk],0)
    else if CheckDeletedNode(emDelete) then begin
      try
        // See if anyone is editing the record, and if not, go on and delete it
        if CurrentItemType = nmIndividual then begin
          FdmName.qryIndividual.Edit;
          FdmName.qryIndividual.Cancel;
        end else begin
          FdmName.qryOrganisation.Edit;
          FdmName.qryOrganisation.Cancel;
        end;
        //Confirm delete
        if MessageDlg(ResStr_DeleteItem,
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          lCursor:=HourglassCursor;
          try
            //Delete from database
            if CurrentItemType = nmIndividual then
              lDelResult:=FdmName.DelIndividual(TNameNode(FSelectedItem.Data).ItemKey)
            else
              lDelResult:=FdmName.DelOrganisation(TNameNode(FSelectedItem.Data).ItemKey);

            if lDelResult then begin
              //Delete from tree and select a new item
              with Selectedtree do begin
                Items.Delete(FSelectedItem);
                Selected:= Items.GetFirstNode;
              end;
            end;  // Deletion successful
          finally
            DefaultCursor(lCursor);
          end;
        end;  // Delete confirmed
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
  end;
  if SelectedTree=tvIndividuals then begin
    sbIndividualsClick(nil);
    sbIndividuals.Down:=true;
    ClearTrees(false,true);
    BuildTreeView(tvOrganisations,FOrgSQL);  // Rebuild tree, with any filter already applied
  end else begin
    sbOrganisationsClick(nil);
    sbOrganisations.Down:=true;
    ClearTrees(true,false);
    BuildTreeView(tvIndividuals,FIndSQL);  // Rebuild tree, with any filter already applied
  end;
  if SelectedTree.Selected=nil then PerformNodeChange(nil);
  RefreshShownLists;
  RepopulateRecord;
end;  // bbDeleteClick

//==============================================================================
procedure TfrmIndOrg.bbSaveClick(Sender: TObject);
var lQuery      : TJNCCQuery;
    lCurrentPage: TTabSheet;
    lCustodian  : String;
    lCursor     : TCursor;
    lSQL        : String;
begin
  inherited;
  ValidateDateFounded;
  ValidateDateEnded;
  ValidateDateOfBirth;
  ValidateDateOfDeath;
  ValidateActivePeriod;
  //Check required fields
  lCurrentPage:=pcNameDetails.ActivePage;
  if CurrentItemType = nmIndividual then
  begin
    pcNameDetails.ActivePage:=tsIndividual;
    if Editmode = emAdd then begin
      //check that there isn't someone else with the same name
      lSQL := 'SELECT * FROM Individual WHERE Surname = ' + QuotedStr(dbeISurname.Text);
      if dbeIForenames.Text = '' then
        lSQL := lSQL + ' AND Forename IS NULL'
      else
        lSQL := lSQL + ' AND Forename = ' + QuotedStr(dbeIForenames.Text);
      with dmDatabase.ExecuteSQL(lSQL, True) do
        if not(Eof and Bof) then begin
          case ConfirmYesNoCancel(Format(ResStr_IndExistsSave, [dbeIForenames.Text, dbeISurname.Text])) of
            mrNo: begin
                    bbCancelClick(nil);
                    Exit;
                  end;
            mrCancel:
                begin
                  if dbeIForenames.CanFocus then dbeIForenames.SetFocus;
                  Exit;
                end;
          end;
        end;
      end;
    ValidateValue(dbeISurname.Text<>'', ResStr_SurnameRequired, dbeISurname);
  end else begin
    pcNameDetails.ActivePage:=tsOrganisation;
    ValidateValue(dbeOFullName.Text<>'', ResStr_FullNameRequired, dbeOFullName);  ;
  end;
  pcNameDetails.ActivePage:=lCurrentPage;

  { Call to validate COM addin page... }
  if not CheckAdditionalPageCanSave then
    Exit;

  lCursor:=HourglassCursor;
  try
    lCustodian := GetNameCustodian(FCurrentKey);
    if CurrentItemType = nmIndividual then
      lQuery:= FdmName.qryIndividual
    else
      lQuery:= FdmName.qryOrganisation;
    if lQuery.State in [dsEdit, dsInsert] then
    begin
      if CurrentItemType = nmIndividual then begin
        // Clear fields where there is nothing to save.
        if dbcbTitle.Text = ''     then dbcbTitle.Field.Value     := Null;
        if dbeIForenames.Text = '' then dbeIForenames.Field.Value := Null;
        if dbeIInitials.Text = ''  then dbeIInitials.Field.Value  := Null;
        if cmbDept.CurrentStrID<>'' then
          lQuery.FieldByName('Organisation_Department_Key').AsString :=
              cmbDept.CurrentStrID
        else
          lQuery.FieldByName('Organisation_Department_Key').Clear;
      end else begin
        // Clear fields where there is nothing to save.
        if dbeOAcronym.Text = '' then dbeOAcronym.Field.Value := Null;
        // Need to validate departments before saving anything or we get a
        // query not in editmode error later on. The Validate throws an exception if if fails.
        if Assigned(FDepartmentGridManager) then
          FDepartmentGridManager.Validate;
      end;

       //Add relationships if required
      if EditMode = emAdd then
      begin
        FdmName.AddName(FCurrentKey, CurrentItemType = nmOrganisation);
        if FParentKey <> '' then
          //Add relations to database
          FdmName.AddRelation(FParentKey, FCurrentKey);
      end;
      if CurrentItemType=nmIndividual then begin
        // Store biography dates
        lQuery.FieldByName('Born_Vague_Date_Start').Text := eHDateOfBirth.Text;
        lQuery.FieldByName('Died_Vague_Date_Start').Text := eHDateOfDeath.Text;
        lQuery.FieldByName('Active_Vague_Date_Start').Text := eHActivePeriod.Text;
      end;
      //Save Changes
      lQuery.Post;
      if CurrentItemType = nmIndividual then
      begin
        if dbeIForenames.Text <> '' then
          FSelectedItem.Text:= dbeIForenames.Text + ' ' + dbeISurname.Text
        else if dbeIInitials.Text <> '' then
          FSelectedItem.Text:= dbeIInitials.Text + ' ' + dbeISurname.Text
        else if dbcbTitle.Text <> '' then
          FSelectedItem.Text:= dbcbTitle.Text + ' '+ dbeISurname.Text
        else
          FSelectedItem.Text:= dbeISurname.Text;
        // Update object field for the sort to continue to work properly
        TNameNode(FSelectedItem.Data).Text := dbeIForenames.Text + ';' + dbeIInitials.Text + ';' + dbeISurname.Text;
      end else begin
        FSelectedItem.Text:= dbeOAcronym.Text;
        if dbeOAcronym.Text <> '' then FSelectedItem.Text:= FSelectedItem.Text + ', ';
        FSelectedItem.Text:= FSelectedItem.Text + dbeOFullName.Text;
        // Update object field for the sort to continue to work properly
        TNameNode(FSelectedItem.Data).Text := dbeOAcronym.Text + ';' + dbeOFullName.Text;
      end;
    end else
    // Only relevant when editing existing data
    if (EditMode = Constants.emEdit) and (lCustodian <> FCustodian) then begin
      if CurrentItemType = nmIndividual then
        FdmName.qryIndividual.Cancel
      else
        FdmName.qryOrganisation.Cancel;
      MessageDlg(Format(ResStr_CustodyChanged, ['Name']), mtWarning, [mbOk], 0);
    end;
    // Save lists
    FAddressList.Update;
    sgAAddressesClick(Self);
    FContactList.Update;
    sgCContactNumbersClick(Self);
    FCommsList.Update;
    sgCOCommsClick(Self);
    FAssocList.Update;
    sgASAssocsClick(Self);
    if (CurrentItemType = nmOrganisation) and Assigned(FDepartmentGridManager) then
      FDepartmentGridManager.Save;
    RefreshShownLists;
    // Sources
    Sources.Post;
    Sources.RefreshLists;

    //Additional pages
    SaveAdditionalPages;

    // Set flag indicating other tree view needs a refresh.
    if FSelectedItem.Tree = tvIndividuals then begin
      FNeedOrgRefresh := true;
    end else begin
      FNeedIndRefresh := true;
    end;
    //Set mode back to previous
    EnableDetails(Constants.emView);

    KillChildren(FSelectedItem);
    PopulateChildren(FSelectedItem);
    RepopulateRecord;
    FSelectedItem.Tree.Invalidate;
  finally
    DefaultCursor(lCursor);
    //Send message to all open forms that the name has changed
    frmMain.BroadcastMessage(WM_REFRESH_NAMES);
  end;
end;  // bbSaveClick

//==============================================================================
procedure TfrmIndOrg.bbCancelClick(Sender: TObject);
begin
  inherited;
  //Cancel dataset
  if CurrentItemType = nmIndividual then
    FdmName.qryIndividual.Cancel
  else
    FdmName.qryOrganisation.Cancel;

  // Selecte the node's tree
  if FSelectedItem.Tree=tvIndividuals then begin
    sbIndividualsClick(nil);
    sbIndividuals.Down:=true;
  end else begin
    sbOrganisationsClick(nil);
    sbOrganisations.Down:=true;
  end;

  // Remove new node from the tree
  if EditMode = emAdd then begin
    SelectedTree.Items.Delete(FSelectedItem);
    FSelectedItem:=SelectedTree.Selected;
    if SelectedTree=tvIndividuals then FISelectedItem:=FSelectedItem
                                  else FOSelectedItem:=FSelectedItem;
  end else
    SelectedTree.Selected:=FSelectedItem;

  //Additional pages
  CancelAdditionalPages;
  EnableDetails(emView);
  RepopulateRecord;
end;  // bbCancelClick

//==============================================================================
procedure TfrmIndOrg.bbRelatedDataClick(Sender: TObject);
var PosPopup : TPoint;
begin
  inherited;
  PosPopup:=ClientToScreen(Point(pnlButtons2.Left+bbRelatedData.Left,
                           pnlButtons.Top+bbRelatedData.Top+bbRelatedData.Height));
  pmRelatedData.Popup (PosPopup.X,PosPopup.Y);
end;  // bbRelatedDataClick

//==============================================================================
procedure TfrmIndOrg.PopulateAddress(mmOutput: TMemo);
var i: Integer;
begin
  //Open querys and populate memo with results
  with FdmName do begin
    qryPrefAddress.Close;
    qryPrefContact.Close;

    qryPrefAddress.Parameters.ParamByName('Key').Value := FCurrentKey;
    qryPrefContact.Parameters.ParamByName('Key').Value := FCurrentKey;

    try
      qryPrefAddress.Open;
      qryPrefContact.Open;
    except
      Raise Exception.Create(ResStr_CannotObjectAddress);
    end;

    mmOutput.Lines.Clear;
    with qryPrefAddress do
      if not Eof then begin
        mmOutput.Lines.Add(FieldByName('Address_1').AsString);
        if FieldByName('Address_2').AsString<>'' then
          mmOutput.Lines.Add(FieldByName('Address_2').AsString);
        if FieldByName('Address_3').AsString<>'' then
          mmOutput.Lines.Add(FieldByName('Address_3').AsString);
        if FieldByName('Address_4').AsString<>'' then
          mmOutput.Lines.Add(FieldByName('Address_4').AsString);
        i:= mmOutput.Lines.Add(FieldByName('Address_Country').AsString);
        if FieldByName('Address_Country').AsString <> '' then mmOutput.Lines[i]:= mmOutput.Lines[i] + ', ';
        mmOutput.Lines[i]:= mmOutput.Lines[i] + FieldByName('Address_Postcode').AsString;
      end;

    with qryPrefContact do
      if not Eof then
        mmOutput.Lines.Add(FieldByName('CONTACT_NUMBER_TYPE').AsString + ': ' + FieldByName('Prefix').AsString + ' ' + FieldByName('Number').AsString);
    // Remove blank line at the top
    with mmOutput do
      while (Lines.Count>0) and (Lines[0]='') do Lines.Delete(0);
  end;
end;  // PopulateAddress

//==============================================================================
procedure TfrmIndOrg.reEnter(Sender: TObject);
begin
  inherited;
  if Sender is TRichEdit then
    dmFormActions.UpdateRTFMenu(((FdmName.qryIndividual.State in [dsEdit,dsInsert])  or
                                (FdmName.qryOrganisation.State in [dsEdit,dsInsert])) and
                                not TRichEdit(Sender).ReadOnly)
  else if Sender is TDBRichEdit then
    dmFormActions.UpdateRTFMenu(((FdmName.qryIndividual.State in [dsEdit,dsInsert])  or
                                (FdmName.qryOrganisation.State in [dsEdit,dsInsert])) and
                                not TDBRichEdit(Sender).ReadOnly)
  else
    dmFormActions.UpdateRTFMenu(true);
end;  // reEnter

//==============================================================================
procedure TfrmIndOrg.reExit(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(false);
end;  // reExit

//==============================================================================
procedure TfrmIndOrg.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  x, y : integer;
begin
  inherited;
  with TStringGrid(Sender) do
    if (ACol=0) and (ARow>0) then
    begin
      Canvas.FillRect(Rect);
      x:=Rect.Left+(ColWidths[0]-13) div 2;
      y:=Rect.Top+(RowHeights[Row]-13) div 2;
      DrawCheckBox(Canvas,x,y,Cells[ACol,ARow]='+');
    end else begin
      Canvas.FillRect(Rect);
      DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
    end;
end;  // GridDrawCell

//==============================================================================
procedure TfrmIndOrg.DrawCellChoppedText(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  with TStringGrid(Sender) do begin
    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
  end;
end;  // DrawCellChoppedText

//==============================================================================
procedure TfrmIndOrg.sgAAddressesClick(Sender: TObject);
begin
  inherited;
  with sgAAddresses do
    FCurrentAddress:=TAddressItem(Objects[0,Row]);

  if FCurrentAddress=nil then begin
    bbAEdit.Enabled:=false;
    bbADel.Enabled :=false;
    BlankAddress;
  end else begin
    //Populate detail fields with data from the list
    with mmAAddress.Lines do
    begin
      Clear;
      Add(FCurrentAddress.Address1);
      Add(FCurrentAddress.Address2);
      Add(FCurrentAddress.Address3);
      Add(FCurrentAddress.Address4);
      Text:= Copy(Text, 0, Length(Text) - 2);
    end;
    with FCurrentAddress do begin
      eACountry.Text := Country;
      eaPostcode.Text:= Postcode;
      eAFrom.Text    := DateFrom;
      eATo.Text      := DateTo;

      Comment.Position:= 0;
      reAComments.Lines.LoadFromStream(Comment);

      rbAHome.Checked     := not WorkAddress;
      rbAWork.Checked     := WorkAddress;
      cbAPreferred.Checked:= Preferred;

      EnableDetailsAddEditButtons(TN_ADDRESS, 'Address_Key', ItemKey,
          Custodian, EditMode, Added, bbAEdit, bbADel, True);
    end;
  end;
end;  // sgAddressesClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.BlankAddress;
begin
  mmAAddress.Lines.Clear;
  eAFrom.Text    := '';
  eATo.Text      := '';
  eAPostcode.Text:= '';
  eACountry.Text := '';
  reAComments.Lines.Clear;
  rbAHome.Checked:= True;
  cbAPreferred.Checked:= False;
end;  // BlankAddress

//------------------------------------------------------------------------------
procedure TfrmIndOrg.SaveAddress;
var lCount: Integer;
begin
  //Save the address detail fields to the list
  with FCurrentAddress do begin
    Address1:=mmAAddress.Lines[0];
    Address2:=mmAAddress.Lines[1];
    Address3:=mmAAddress.Lines[2];
    Address4:=mmAAddress.Lines[3];
    DateFrom:=eAFrom.Text;
    DateTo  :=eATo.Text;
    Country :=eACountry.Text;
    PostCode:=eAPostcode.Text;

    Comment.Position:= 0;
    reAComments.Lines.SaveToStream(Comment);

    WorkAddress:= rbAWork.Checked;
  end;

  //If preferred has been checked, uncheck the preferred flag of all other records
  if cbAPreferred.Checked then begin
    with FAddressList do
      for lCount:= 0 to Count - 1 do
        if not TAddressItem(Items[lCount]).Deleted and TAddressItem(Items[lCount]).Preferred then
	        TAddressItem(Items[lCount]).Preferred:= False;

    FCurrentAddress.Preferred:= cbAPreferred.Checked;
  end;
  sgAAddresses.Refresh;
end;  // SaveAddress

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbAAddClick(Sender: TObject);
begin
  inherited;
  //Add new address
  SwitchToDetails(sgAAddresses,bbAAdd,bbAEdit,bbADel,
                  bbSave, bbCancel, gbAddressDetails,true);
  BlankAddress;
  FAdd:= True;
  FCurrentAddress:= TAddressItem.CreateNew(FAddressList);

  //If this is the first address, make it the preferred address
  if not Assigned(sgAAddresses.Objects[0,1]) then
  begin
    cbAPreferred.Checked:= True;
    cbAPreferred.Enabled:= False;
  end;
end;  // bbAddrAddClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbAEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgAAddresses,bbAAdd,bbAEdit,bbADel,
                  bbSave, bbCancel, gbAddressDetails,true);
  FAdd:= False;
  cbAPreferred.Enabled:= not FCurrentAddress.Preferred;
end;  // bbAEditClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbADelClick(Sender: TObject);
var lResetPreferred: Boolean;
begin
  inherited;
  if MessageDlg(ResStr_DeleteAddress,
                mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    with sgAAddresses do begin
      lResetPreferred:= TAddressItem(Objects[0,Row]).Preferred;
      FAddressList.DeleteItem(Row);

      if lResetPreferred then
        if Assigned(Objects[0,1]) then
          TAddressItem(Objects[0,1]).Preferred:= True;
    end;
    sgAAddressesClick(nil);
  end;
end;  // bbADelClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbAAcceptClick(Sender: TObject);
begin
  inherited;
  // Sender is nil if called from CloseQuery method.
  if Sender=nil then begin
    ValidateAddressFromDate;
    ValidateAddressToDate;
  end;

  ValidateValue(mmAAddress.Text<>'', ResStr_AddressRequired, mmAAddress);
  ValidateValue(eAFrom.Text<>'', ResStr_StartDateRequired, eAFrom);

  SaveAddress;
  if FAdd then FAddressList.AddNew(FCurrentAddress);
  SwitchToDetails(sgAAddresses, bbAAdd, bbAEdit, bbADel,
                  bbSave, bbCancel, gbAddressDetails, false);
  sgAAddressesClick(nil);
end;  // bbAAcceptClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbACancelClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgAAddresses,bbAAdd,bbAEdit,bbADel,
                  bbSave, bbCancel, gbAddressDetails,false);
  if FAdd then FCurrentAddress.Free;
  sgAAddressesClick(nil);
end;  // bbACancelClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.mmAAddressChange(Sender: TObject);
var lStrings: TStringList;
begin
  inherited;
  lStrings := TStringList.Create;
  try
    // The strings won't be wrapped in the stringlist!!
    lStrings.Text := mmAAddress.Lines.Text;
    if lStrings.Count > 4 then begin
      MessageDlg(ResStr_FourLinesForAddress, mtInformation, [mbOK], 0);
      // Start by removing blank lines at the top if any
      while lStrings[0] = '' do lStrings.Delete(0);
      // Now trim the bottom
      while lStrings.Count > 4 do lStrings.Delete(4);
      // And shove it all back in the memo.
      mmAAddress.Lines.Text := lStrings.Text;
    end;
  finally
    lStrings.Free;
  end;
end;  // mmAAdressChange

//------------------------------------------------------------------------------
procedure TfrmIndOrg.eAFromExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbACancel.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbACancel.Width]) and (lCancelPos.Y in [0..bbACancel.Height]) then
    bbACancelClick(nil)
  else if not FClosingForm then
    ValidateAddressFromDate;
end;  // eAFromExit

procedure TfrmIndOrg.ValidateAddressFromDate;
begin
  if eAFrom.Text <> '' then begin
    ValidateValue(CheckVagueDate(eaFrom.Text),InvalidDate(ResStr_StartDate,true,false),eaFrom);
    eAFrom.Text:=VagueDateToString(eAFrom.VagueDate);
  end;
end;  // ValidateAddressFromDate

//------------------------------------------------------------------------------
procedure TfrmIndOrg.eAToExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbACancel.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbACancel.Width]) and (lCancelPos.Y in [0..bbACancel.Height]) then
    bbACancelClick(nil)
  else if not FClosingForm then
    ValidateAddressToDate;
end;  // eAToExit

procedure TfrmIndOrg.ValidateAddressToDate;
begin
  if eATo.Text <> '' then begin
    ValidateValue(CheckVagueDate(eATo.Text),InvalidDate(ResStr_EndDate,true,true),eATo);
    eATo.Text:=VagueDateToString(eATo.VagueDate);
    ValidateValue(not IsVagueDateInVagueDate(StringToVagueDate(eATo.Text), StringToVagueDate(eAFrom.Text)),
                  ResStr_StartContainEndDate,eATo);
    ValidateValue(not IsVagueDateInVagueDate(StringToVagueDate(eAFrom.Text), StringToVagueDate(eATo.Text)),
                  ResStr_EndContainStartDate,eATo);
    ValidateValue(CompareVagueDateToVagueDate(StringToVagueDate(eATo.Text),StringToVagueDate(eAFrom.Text)) >= 0,
                  ResStr_EndBeforeStartDate,eATo);
  end;
end;  // ValidateAddressToDate

//==============================================================================
procedure TfrmIndOrg.sgCContactNumbersClick(Sender: TObject);
begin
  inherited;
  with sgCContactNumbers do
    FCurrentContact:=TContactItem(Objects[0,Row]);

  if FCurrentContact=nil then begin
    bbCEdit.Enabled:=false;
    bbCDel.Enabled :=false;
    BlankContact;
  end else
    //Populate the details fields with information from the list
    with FCurrentContact do begin
      cmbCType.Text:= ContactType;
      eCPrefix.Text:= Prefix;
      eCNumber.Text:= Number;

      Constraints.Position:= 0;
      reCConstraints.Lines.LoadFromStream(Constraints);

      cbCPreferred.Checked:=Preferred;
      EnableDetailsAddEditButtons(TN_CONTACT_NUMBER,
            'Contact_Number_Key', ItemKey, Custodian, EditMode, Added, bbCEdit, bbCDel, True);
    end;
end;  // sgCContactNumbersClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.BlankContact;
begin
  cmbCType.ItemIndex:= -1;
  eCPrefix.Text:= '';
  eCNumber.Text:= '';
  reCConstraints.Lines.Clear;
  cbCPreferred.Checked:= False;
end;  // BlankContact

//------------------------------------------------------------------------------
procedure TfrmIndOrg.SaveContact;
var lCount: Integer;
begin
  with FCurrentContact do begin
    ContactType:= cmbCType.Text;
    Prefix     := eCPrefix.Text;
    Number     := eCNumber.Text;

    Constraints.Position:= 0;
    reCConstraints.Lines.SaveToStream(Constraints);

    //If preferred has been checked, uncheck the preferred flag of all other records
    if cbCPreferred.Checked then begin
      with FContactList do
        for lCount:= 0 to Count - 1 do
          if not TContactItem(Items[lCount]).Deleted and TContactItem(Items[lCount]).Preferred then
            TContactItem(Items[lCount]).Preferred:= False;

      Preferred:= cbCPreferred.Checked;
      sgCContactNumbers.Refresh;
    end;
  end;
end;  // SaveContact

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbCAddClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgCContactNumbers,bbCAdd,bbCEdit,bbCDel,
                  bbSave, bbCancel, gbContactDetails,true);
  BlankContact;
  FAdd:= True;
  FCurrentContact:= TContactItem.CreateNew(FContactList);

  //If this is the first number, make it the preferred number
  if not Assigned(sgCContactNumbers.Objects[0,1]) then
  begin
    cbCPreferred.Checked:= True;
    cbCPreferred.Enabled:= False;
  end;
end;  // bbCAddClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbCEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgCContactNumbers,bbCAdd,bbCEdit,bbCDel,
                  bbSave, bbCancel, gbContactDetails,true);
  FAdd:= False;
  cbCPreferred.Enabled:= not FCurrentContact.Preferred;
end;  // bbCEditClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbCDelClick(Sender: TObject);
var lResetPreferred: Boolean;
begin
  inherited;
  if MessageDlg(ResStr_DeleteContactNum,
                mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    with sgCContactNumbers do begin
      lResetPreferred:= TContactItem(Objects[0,Row]).Preferred;
      FContactList.DeleteItem(Row);

      if lResetPreferred then
        if Assigned(Objects[0,1]) then
          TContactItem(Objects[0,1]).Preferred:= True;
    end;
    sgCContactNumbersClick(nil);
  end;
end;  // bbCDelClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbCAcceptClick(Sender: TObject);
begin
  inherited;
  //Validate required fields
  ValidateValue(cmbCType.Text<>'',ResStr_ContactTypeRequired,cmbCType);
  ValidateValue(eCNumber.Text<>'', ResStr_NumberRequired, eCNumber);

  SaveContact;
  if FAdd then FContactList.AddNew(FCurrentContact);
	SwitchToDetails(sgCContactNumbers,bbCAdd,bbCEdit,bbCDel,
                        bbSave, bbCancel, gbContactDetails,false);
  sgCContactNumbersClick(nil);
end;  // bbContactAcceptClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbCCancelClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgCContactNumbers,bbCAdd,bbCEdit,bbCDel,
                  bbSave, bbCancel, gbContactDetails,false);
  if FAdd then FCurrentContact.Free;
  sgCContactNumbersClick(nil);
end;  // bbCCancelClick

//==============================================================================
procedure TfrmIndOrg.sgCOCommsClick(Sender: TObject);
begin
  inherited;
  with sgCOComms do
    FCurrentComms:=TCommsItem(Objects[0,Row]);

  if FCurrentComms=nil then begin
    bbCOEdit.Enabled:=false;
    bbCODel.Enabled :=false;
    BlankComms;
  end else
    //Populate the details fields with information from the list
    with FCurrentComms do begin
      eCOName.Text  := WithString;
      eCOName.Key   := WithID;
      cmbCOType.Text:= CommsType;
      eCODate.Text  := Date;

      Content.Position:= 0;
      reCOContent.Lines.LoadFromStream(Content);

      eCORef.Text:= FileRef;

      EnableDetailsAddEditButtons(TN_COMMUNICATION, 'Communication_Key',
          ItemKey, Custodian, EditMode, Added, bbCOEdit, bbCODel, True);
    end;
end;  // sgCOCommsClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.BlankComms;
begin
  eCOName.Text  := '';
  cmbCOType.Text:= '';
  eCODate.Text  := '';
  reCOContent.Lines.Clear;
  eCORef.Text   := '';
end;  // BlankComms

//------------------------------------------------------------------------------
procedure TfrmIndOrg.SaveComms;
begin
  //Save the comms detail fields to the list
  with FCurrentComms do begin
    WithString:= eCOName.Text;
    WithId    := eCOName.Key;
    CommsType := cmbCoType.Text;
    Date      := eCODate.Text;

    Content.Position:= 0;
    reCOContent.Lines.SaveToStream(Content);

    FileRef:= eCoRef.Text;
  end;
end;  // SaveComms

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbCOAddClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgCOComms,bbCOAdd,bbCOEdit,bbCODel,
                  bbSave, bbCancel, gbCODetails,true);
  BlankComms;
  FAdd:= True;
  FCurrentComms:= TCommsItem.CreateNew(FCommsList);
end;  // bbCOAddClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbCOEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgCOComms,bbCOAdd,bbCOEdit,bbCODel,
                  bbSave, bbCancel, gbCoDetails,true);
  FAdd:= False;
end;  // bbCOEditClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbCODelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteCommunication,
                mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    FCommsList.DeleteItem(sgCOComms.Row);
    sgCOCommsClick(nil);
  end;
end;  // bbCODelClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbCOAcceptClick(Sender: TObject);
begin
  inherited;
  // Sender is nil if called from CloseQuery
  if Sender=nil then
    ValidateCommDate;
    
  ValidateValue(eCOName.Text<>'',ResStr_EmptyNameField,eCOName);
  ValidateValue(dmGeneralData.CheckName(eCOName),ResStr_InvalidName,eCOName);
  ValidateValue(cmbCOType.Text<>'',ResStr_CommMissing, cmbCOType);
  ValidateValue(eCODate.Text<>'', ResStr_DateMissing, eCODate);

  SaveComms;
  if FAdd then FCommsList.AddNew(FCurrentComms);
  SwitchToDetails(sgCOComms,bbCOAdd,bbCOEdit,bbCODel,
                  bbSave, bbCancel, gbCODetails,false);
  sgCOCommsClick(nil);
end;  // bbCOAcceptClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbCOCancelClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgCOComms,bbCOAdd,bbCOEdit,bbCODel,
                  bbSave, bbCancel, gbCoDetails,false);
  if FAdd then FCurrentComms.Free;
  sgCOCommsClick(nil);
end;  // bbCOCancelClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.eCODateExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbCOCancel.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbCOCancel.Width]) and (lCancelPos.Y in [0..bbCOCancel.Height]) then
    bbCOCancelClick(nil)
  else if not FClosingForm then
    ValidateCommDate;
end;  // eCODateExit

{-------------------------------------------------------------------------------
}
procedure TfrmIndOrg.ValidateCommDate;
begin
  if eCODate.Text<>'' then begin
    ValidateValue(CheckVagueDate(eCODate.Text),InvalidDate(ResStr_TheDate,true,false),eCODate);
    eCODate.Text:=VagueDateToString(eCODate.VagueDate);
  end;
end;  // ValidateCommDate

//==============================================================================
procedure TfrmIndOrg.sgASAssocsClick(Sender: TObject);
begin
	inherited;
  with sgASAssocs do
    FCurrentAssoc:=TAssocItem(Objects[0,Row]);

  if FCurrentAssoc=nil then begin
    bbASEdit.Enabled:=false;
    bbASDel.Enabled :=false;
    BlankAssoc;
  end else
    //Populate the details fields with information from the list
    with FCurrentAssoc do begin
      eASName.Text    := WithString;
      eASName.Key     := WithId;
      cmbASRole.Text  := Role;
      eASNameCode.Text:= NameCode;

      Comment.Position:= 0;
      reASComment.Lines.LoadFromStream(Comment);

      eASDateFrom.Text:= DateFrom;
      eASDateTo.Text  := DateTo;
      EnableDetailsAddEditButtons(TN_NAME_RELATION,
          'Name_Relation_Key', ItemKey, Custodian, EditMode, Added, bbASEdit, bbASDel, True);
    end;
end;  // sgASAssocsClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.BlankAssoc;
begin
  eASName.Text    := '';
  cmbASRole.Text  := '';
  eASNameCode.Text:= '';
  reASComment.Lines.Clear;
  eASDateTo.Text  := '';
  eASDateFrom.Text:= '';
end;  // BlankAssoc

//------------------------------------------------------------------------------
procedure TfrmIndOrg.SaveAssoc;
begin
  //Save the comms detail fields to the list
  with FCurrentAssoc do begin
    WithString:= eASName.Text;
    WithId    := eASName.Key;
    Role      := cmbASRole.Text;
    NameCode  := eASNameCode.Text;

    Comment.Position:= 0;
    reASComment.Lines.SaveToStream(Comment);

    DateFrom:= eASDateFrom.Text;
    DateTo  := eASDateTo.Text;
  end;
end;  // SaveAssoc

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbASAddClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgASAssocs,bbASAdd,bbASEdit,bbASDel,
                  bbSave, bbCancel, gbAsDetails,true);
  BlankAssoc;
  FAdd:= True;
  FCurrentAssoc:= TAssocItem.CreateNew(FAssocList);
end;  // bbASAddClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbASEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgASAssocs,bbASAdd,bbASEdit,bbASDel,
                  bbSave, bbCancel, gbASDetails,true);
  FAdd:= False;
end;  // bbASEditClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbASDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteAssoc,
                mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    FAssocList.DeleteItem(sgASAssocs.Row);
    sgASAssocsClick(nil);
  end;
end;  // bbASDelClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbASAcceptClick(Sender: TObject);
begin
  inherited;
  // Sender is nil if called from CloseQuery
  if Sender=nil then begin
    ValidateAssocFromDate;
    ValidateAssocToDate;
  end;
  
  ValidateValue(eASName.Text<>'',ResStr_EmptyNameField, eASName);
  ValidateValue(dmGeneralData.CheckName(eASName),ResStr_InvalidName, eASName);
  ValidateValue(cmbASRole.Text<>'',ResStr_RoleMissing, cmbASRole);

  SaveAssoc;
  if FAdd then FAssocList.AddNew(FCurrentAssoc);
  SwitchToDetails(sgASAssocs,bbASAdd,bbASEdit,bbASDel,
                  bbSave, bbCancel, gbAsDetails,false);
  sgASAssocsClick(nil);
end;  // bbASAcceptClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.bbASCancelClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgASAssocs,bbASAdd,bbASEdit,bbASDel,
                  bbSave, bbCancel, gbASDetails,false);
  if FAdd then FCurrentAssoc.Free;
  sgASAssocsClick(nil);
end;  // bbASCancelClick

//------------------------------------------------------------------------------
procedure TfrmIndOrg.eASDateFromExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbASCancel.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbASCancel.Width]) and (lCancelPos.Y in [0..bbASCancel.Height]) then
    bbASCancelClick(nil)
  else if not FClosingForm then
    ValidateAssocFromDate;
end;  // eASDateFromExit

procedure TfrmIndOrg.ValidateAssocFromDate;
begin
  if eASDateFrom.Text <> '' then begin
    ValidateValue(CheckVagueDate(eASDateFrom.Text),InvalidDate(ResStr_StartDate,true,false),eASDateFrom);
    eASDateFrom.Text:=VagueDateToString(eASDateFrom.VagueDate);
  end;
end;  // ValidateAssocFromDate

//------------------------------------------------------------------------------
procedure TfrmIndOrg.eASDateToExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbASCancel.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbASCancel.Width]) and (lCancelPos.Y in [0..bbASCancel.Height]) then
    bbASCancelClick(nil)
  else if not FClosingForm then
    ValidateAssocToDate;
end;  // eASDateToExit

procedure TfrmIndOrg.ValidateAssocToDate;
begin
  if eASDateTo.Text <> '' then begin
    ValidateValue(CheckVagueDate(eASDateTo.Text),InvalidDate(ResStr_EndDate,true,true),eASDateTo);
    eASDateTo.Text:=VagueDateToString(eASDateTo.VagueDate);
    ValidateValue(not IsVagueDateInVagueDate(StringToVagueDate(eASDateTo.Text), StringToVagueDate(eASDateFrom.Text)),
                  ResStr_StartContainEndDate, eASDateTo);
    ValidateValue(not IsVagueDateInVagueDate(StringToVagueDate(eASDateFrom.Text), StringToVagueDate(eASDateTo.Text)),
                  ResStr_EndContainStartDate,eASDateTo);
    ValidateValue(CompareVagueDateToVagueDate(StringToVagueDate(eASDateTo.Text),StringToVagueDate(eASDateFrom.Text)) >= 0,
                  ResStr_EndBeforeStartDate, eASDateTo);
  end;
end;  // ValidateAssocToDate

//==============================================================================
procedure TfrmIndOrg.splIndOrgCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  inherited;
  Accept:=(ClientWidth-NewSize>bbAdd.Width+bbEdit.Width+splIndOrg.Width) or
          ((pnlIndOrg.Width<bbAdd.Width+bbEdit.Width) and (NewSize<=pnlIndOrgDetails.Width));
end;  // splIndorgCanResize

//==============================================================================
procedure TfrmIndOrg.FormResize(Sender: TObject);
var
  availableHeight, halfHeight: Integer;
begin
  inherited;
  if splIndOrg.Left < splIndOrg.MinSize then begin
    pnlIndOrgDetails.Width := ClientWidth - splIndOrg.MinSize - splIndOrg.Width;
    if pnlIndOrgDetails.Left + pnlIndOrgDetails.Width > ClientWidth then
      Redraw;
  end;
  tvOrganisations.Refresh;
  tvIndividuals.Refresh;

  availableHeight := bvlOrganisation.Height - dbreComment.Top
                                            + bvlOrganisation.Top
                                            - 4 - 8; // Spacing between controls
  halfHeight := availableHeight div 2;

  dbreComment.Height := halfHeight;
  mmOAddress.Top := dbreComment.Top + halfHeight + 4;
  mmOAddress.Height := halfHeight;
  lblAddress.Top := mmOAddress.Top + 3;

  pnlIndOrgDetailsResize(Sender);
end;  // FormResize

//==============================================================================
procedure TfrmIndOrg.splIndOrgPaint(Sender: TObject);
begin
  inherited;
  DrawVertSplitter(Canvas, splIndorg);
end;  // splIndOrgPaint

//==============================================================================
function TfrmIndOrg.GetKeyList: TKeyList;
var lNewKeyList: TEditableKeyList;
    lTableName : String;
begin
  //Return an editable key list with the selected nodes key
  lNewKeyList := TEditableKeyList.Create;
  lNewKeyList.ConvertToMixedData;
  if SelectedTree.Selected = nil then
    Beep
  else begin
    if SelectedTree = tvIndividuals then begin
      if SelectedTree.Selected.Level = 0 then lTableName := TN_INDIVIDUAL
                                         else lTableName := TN_ORGANISATION;
    end else begin // tvOrganisations
      if SelectedTree.Selected.Level = 0 then lTableName := TN_ORGANISATION
                                         else lTableName := TN_INDIVIDUAL;
    end;
    lNewKeyList.AddItem(TNameNode(SelectedTree.Selected.Data).ItemKey, lTableName);
  end;
  Result:= lNewKeyList;
end;  // GetKeyList

{-------------------------------------------------------------------------------
}
function TfrmIndOrg.GetKeyListForValidation: TKeyList;
var
  keyList: TEditableKeyList;
  nodeData: TNameNode;
begin
  Result := nil;
  if Assigned(SelectedTree.Selected) then begin
    nodeData := TNameNode(SelectedTree.Selected.Data);
    keyList := TEditableKeyList.Create;
    try
      keyList.ConvertToMixedData;
      keyList.AddItem(nodeData.ItemKey, nodeData.ItemAdditional);

      // If top level, need to fetch all child nodes. If not top level, item already in keylist.
      if SelectedTree.Selected.Level = 0 then begin
        with dmDatabase.GetRecordset(
            'usp_Names_Select_AllChildrenForLevel',
            ['@Key', nodeData.ItemKey, '@IsOrg',
             SameText(nodeData.ItemAdditional, TN_ORGANISATION)]) do
        begin
          while not Eof do begin
            keyList.AddItem(Fields['ItemKey'].Value, Fields['TableName'].Value);
            MoveNext;
          end;
          Close;
        end;
      end;
      Result := keyList;
    except
      keyList.Free;
      raise;
    end;
  end;
end;  // GetKeyListForValidation

//==============================================================================
procedure TfrmIndOrg.pcNameDetailsChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  inherited;
  AllowChange:= not FEditDetails;
end;  // pcNameDetailsChanging

//==============================================================================
{ Setup anything we can drag or drop }
procedure TfrmIndOrg.SetupDestinationControls;
begin
  RegisterDragComponent(tvIndividuals, DragIndTV);
  RegisterDragComponent(tvOrganisations, DragOrgTV);
  RegisterDropComponent(eCOName, DropCOName,
      [TN_NAME,TN_INDIVIDUAL, TN_ORGANISATION], [CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(eASName, DropASName,
      [TN_NAME,TN_INDIVIDUAL, TN_ORGANISATION], [CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(sgCOComms, DropComm, [TN_NAME,TN_INDIVIDUAL, TN_ORGANISATION], [CF_JNCCDATA]);
  RegisterDropComponent(sgASAssocs, DropAssoc, [TN_NAME,TN_INDIVIDUAL, TN_ORGANISATION], [CF_JNCCDATA]);
end;  // SetupDestinationControls

//==============================================================================
{ Drag method handler for the tree view }
procedure TfrmIndOrg.DragIndTV(const Sender: TObject;
  var oDropSource: TJNCCDropSource);
var lTableName:string;
begin
  if Assigned(FISelectedItem) then
  begin
    if FISelectedItem.Level = 0 then
      lTableName:=TN_INDIVIDUAL
    else
      lTableName:=TN_ORGANISATION;

    oDropSource.DropData.SetTable(lTableName);
    oDropSource.DropData.AddItem(TNameNode(FISelectedItem.Data).ItemKey, lTableName);
  end;
end;  // DragIndTV

//==============================================================================
procedure TfrmIndOrg.DragOrgTV(const Sender: TObject; var oDropSource: TJNCCDropSource);
var
  lTableName: string;
begin
  if Assigned(FOSelectedItem) then
  begin
    if FOSelectedItem.Level = 0 then
      lTableName := TN_ORGANISATION
    else
      lTableName := TN_INDIVIDUAL;

    oDropSource.DropData.SetTable(lTableName);
    oDropSource.DropData.AddItem(TNameNode(FOSelectedItem.Data).ItemKey,lTableName);
  end;
end;  // DragOrgTV

//==============================================================================
procedure TfrmIndOrg.DropCOName(const Sender: TObject; const iFormat: integer;
  const iSourceData: TKeyList; const iTextStrings: TstringList; var ioHandled: boolean);
begin
  if (EditMode <> emView) and FEditDetails then
    ioHandled := dmGeneralData.DropLinkedEditText(
        eCOName,
        iFormat,
        iSourceData,
        dmGeneralData.GetName,
        iTextStrings)
  else
    ioHandled := True;
end;  // DropCOName

//==============================================================================
procedure TfrmIndOrg.DropComm(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TstringList; var ioHandled: boolean);
var lNewComm:TCommsItem;
    lCount  :integer;
begin
  if (EditMode<>emView) and not FEditDetails then begin
    if (iSourceData.Header.ItemCount>0) and (iFormat=CF_JNCCDATA) then
      if EditMode =emView then
        ioHandled:= False
      else begin
        with iSourceData do
          for lCount:=0 to Header.ItemCount-1 do begin
            //Add new communication
            lNewComm:= TCommsItem.CreateNew(FCommsList);

            //Set properties
            lNewComm.WithID    :=Items[lCount].KeyField1;
            lNewComm.WithString:=dmGeneralData.GetName(lNewComm.WithID);
            lNewComm.CommsType :='Unknown';
            lNewComm.Date      :=DateToStr(Date);

            //Add to list
            FCommsList.AddNew(lNewComm);
          end;
        sgCOComms.Row:=sgCOComms.RowCount - 1;
        sgCOCommsClick(nil);
      end;
  end else
    ioHandled:=true;
end;  // DropComm

//==============================================================================
procedure TfrmIndOrg.DropASName(const Sender: TObject; const iFormat: integer;
  const iSourceData: TKeyList; const iTextStrings: TstringList; var ioHandled: boolean);
begin
  if (EditMode<>emView) and FEditDetails then
    ioHandled := dmGeneralData.DropLinkedEditText(
        eASName,
        iFormat,
        iSourceData,
        dmGeneralData.GetName,
        iTextStrings)
  else
    ioHandled := True;
end;  // DropASName

//==============================================================================
procedure TfrmIndOrg.DropAssoc(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TstringList; var ioHandled: boolean);
var lNewAssoc:TAssocItem;
    lCount   :integer;
begin
  if (EditMode<>emView) and not FEditDetails then begin
    if (iSourceData.Header.ItemCount>0) and (iFormat=CF_JNCCDATA) then
      if EditMode=emView then
        ioHandled:= False
      else begin
        with iSourceData do
          for lCount:=0 to Header.ItemCount-1 do begin
            //Add new association
            lNewAssoc:= TAssocItem.CreateNew(FAssocList);

            //Set properties
            lNewAssoc.WithID    :=Items[lCount].KeyField1;
            lNewAssoc.WithString:=dmGeneralData.GetName(lNewAssoc.WithID);
            lNewAssoc.Role      :='Unknown';
            lNewAssoc.DateFrom  :=DateToStr(Date);

            //Add to list
            FAssocList.AddNew(lNewAssoc);
          end;
        sgASAssocs.Row:= sgASAssocs.RowCount - 1;
        sgASAssocsClick(nil);
      end;
  end else
    ioHandled:=true;
end;  // DropAssoc

//==============================================================================
procedure TfrmIndOrg.actSortForenameExecute(Sender: TObject);
begin
  inherited;
  IndividualSort := FORENAME_SORT;
end;  // actSortForenameExecute

//------------------------------------------------------------------------------
procedure TfrmIndOrg.actSortSurnameExecute(Sender: TObject);
begin
  inherited;
  IndividualSort := SURNAME_SORT;
end;  // actSortSurnameExecute


//==============================================================================
procedure TfrmIndOrg.actSortAcronymExecute(Sender: TObject);
begin
  inherited;
  OrganisationSort := ACRONYM_SORT;
end;  // actSortAcronymExecute

//------------------------------------------------------------------------------
procedure TfrmIndOrg.actSortFullNameExecute(Sender: TObject);
begin
  inherited;
  OrganisationSort := FULLNAME_SORT;
end;  // actSortFullNameExecute


//==============================================================================
procedure TfrmIndOrg.actFindExecute(Sender: TObject);
var lNode  :TFlyNode;
    stTitle:string;
    lCount :integer;
    lType : TFindType;
const
  FIND_ORG = 'Find Organisation';
  FIND_IND = 'Find Individual';
begin
  inherited;
  if SelectedTree=tvIndividuals then begin
    if FSelectedItem.Parent=nil then stTitle:=FIND_IND
                                else stTitle:=FIND_ORG;
  end else begin
    if FSelectedItem.Parent=nil then stTitle:=FIND_ORG
                                else stTitle:=FIND_IND;
  end;
  if stTitle=FIND_IND then
    lType := ftIndividual
  else
    lType := ftOrganisation;
  with TdlgFind.CreateDialog(nil,stTitle,lType) do
    try
      eSearchText.ClearSourceList;
      if ShowModal=mrOk then begin
        if FSelectedItem.Parent=nil then begin
          for lCount:=0 to SelectedTree.Items.Count-1 do begin
            lNode:=SelectedTree.Items[lCount];
            if (lNode.Text=ItemText) and (TNameNode(lNode.Data).ItemKey=ItemKey) then begin
              SelectedTree.Selected:=lNode;
              Break;
            end;  // if
          end;  // for
        end else begin
          lNode:=FSelectedItem.Parent;
          for lCount:=0 to lNode.Count-1 do
            if (lNode.Item[lCount].Text=ItemText) and
               (TNameNode(lNode.Item[lCount].Data).ItemKey=ItemKey) then begin
              SelectedTree.Selected:=lNode.Item[lCount];
              Break;
            end;  // if
        end;  // if .. else
      end;  // if ShowModal
    finally
      Free;
    end;
end;  // actFindExecute

//==============================================================================
procedure TfrmIndOrg.dbeODateFoundedExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbCancel.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbCancel.Width]) and (lCancelPos.Y in [0..bbCancel.Height]) then
    bbCancelClick(nil)
  else
    ValidateDateFounded;
end;  // dbeODateFoundedExit

procedure TfrmIndOrg.ValidateDateFounded;
begin
  ValidateValue(IsVagueDate(dbeODateFounded.Text),
                ResStr_InvalidVagueDate,dbeODateFounded);
  if dbeODateFounded.Text<>'' then
    ValidateValue(CheckVagueDate(dbeODateFounded.Text),
                  InvalidDate(ResStr_FoundationDate,true,true),dbeODateFounded);
end;  // dbeODateFoundedExit

//------------------------------------------------------------------------------
procedure TfrmIndOrg.dbeODateEndedExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbCancel.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbCancel.Width]) and (lCancelPos.Y in [0..bbCancel.Height]) then
    bbCancelClick(nil)
  else
    ValidateDateEnded;
end;  // dbeODateEndedExit

procedure TfrmIndOrg.ValidateDateEnded;
begin
  ValidateValue(IsVagueDate(dbeODateEnded.Text),
                ResStr_InvalidVagueDate,dbeODateEnded);
  if dbeODateEnded.Text <> '' then begin
    ValidateValue(dbeODateFounded.Text<>'',ResStr_SpecifyFoundationDate, dbeODateEnded);
    ValidateValue(CheckVagueDate(dbeODateEnded.Text),
                  InvalidDate(ResStr_EndDate,true,true),dbeODateEnded);
    ValidateValue(not IsVagueDateInVagueDate(StringToVagueDate(dbeODateEnded.Text), StringToVagueDate(dbeODateFounded.Text)),
                  ResStr_FoundContainEndDate,dbeODateEnded);
    ValidateValue(not IsVagueDateInVagueDate(StringToVagueDate(dbeODateFounded.Text), StringToVagueDate(dbeODateEnded.Text)),
                  ResStr_EndContainFoundDate,dbeODateEnded);
    ValidateValue(CompareVagueDateToVagueDate(StringToVagueDate(dbeODateEnded.Text),StringToVagueDate(dbeODateFounded.Text)) >= 0,
                  ResStr_EndDateBeforeFoundDate, dbeODateEnded);
  end;
end;  // dbeODateEndedExit

//==============================================================================
procedure TfrmIndOrg.eHDateOfBirthExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbCancel.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbCancel.Width]) and (lCancelPos.Y in [0..bbCancel.Height]) then
    bbCancelClick(nil)
  else
    ValidateDateOfBirth;
end;  // eHDateOfBirthExit
//==============================================================================
procedure TfrmIndOrg.eHActivePeriodExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbCancel.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbCancel.Width]) and (lCancelPos.Y in [0..bbCancel.Height]) then
    bbCancelClick(nil)
  else
    ValidateActivePeriod;
end;  // eHActivePeriodExit
//==============================================================================
procedure TfrmIndOrg.ValidateDateOfBirth;
begin
  ValidateValue(IsVagueDate(eHDateOfBirth.Text),
                ResStr_InvalidVagueDate, eHDateOfBirth);
  if eHDateOfBirth.Text<>'' then
    ValidateValue(CheckVagueDate(eHDateOfBirth.Text),
                  InvalidDate(ResStr_DateOfBirth,true,false),eHDateOfBirth);
end;  // TfrmIndOrg.ValidateDateOfBirth
//==============================================================================
procedure TfrmIndOrg.ValidateActivePeriod;
begin
  ValidateValue(IsVagueDate(eHActivePeriod.Text),
                ResStr_InvalidVagueDate, eHActivePeriod);
end;  // TfrmIndOrg.ValidateDateOfBirth

// ---------------------------------------------------------------------------
procedure TfrmIndOrg.eHDateOfDeathExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbCancel.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbCancel.Width]) and (lCancelPos.Y in [0..bbCancel.Height]) then
    bbCancelClick(nil)
  else
    ValidateDateOfDeath;
end;  // eHDateOfDeathExit

//------------------------------------------------------------------------------
procedure TfrmIndOrg.ValidateDateOfDeath;
begin
  ValidateValue(IsVagueDate(eHDateOfDeath.Text),
                ResStr_InvalidVagueDate, eHDateOfDeath);
  if eHDateOfDeath.Text <> '' then begin
    ValidateValue(eHDateOfBirth.Text<> '',ResStr_DateOfDeathBirth, eHDateOfDeath);
    ValidateValue(CheckVagueDate(eHDateOfDeath.Text),
                  InvalidDate(ResStr_DateOfDeath,true,true),eHDateOfDeath);
    ValidateCompareBirthToDeathDate;
  end;
end;  // TfrmIndOrg.ValidateDateOfDeath

//==============================================================================
// Perform validation that compares the values for date of birth and death.
//==============================================================================
procedure TfrmIndOrg.ValidateCompareBirthToDeathDate;
begin
  ValidateValue(not IsVagueDateInVagueDate(StringToVagueDate(eHDateOfDeath.Text), StringToVagueDate(eHDateOfBirth.Text)),
                ResStr_DateOfBirthDeath, eHDateOfDeath);
  ValidateValue(not IsVagueDateInVagueDate(StringToVagueDate(eHDateOfBirth.Text), StringToVagueDate(eHDateOfDeath.Text)),
                ResStr_DeathContainBirthDate, eHDateOfDeath);
  ValidateValue(CompareVagueDateToVagueDate(StringToVagueDate(eHDateOfDeath.Text),StringToVagueDate(eHDateOfBirth.Text)) >= 0,
                ResStr_DeathBeforeBirth, eHDateOfDeath);
end;

//==============================================================================
procedure TfrmIndOrg.DisplayNames(const iNameList: TKeyList);
const
  SQL_IS_ORG = 'SELECT Organisation FROM Name WHERE Name_Key = ''%s''';

var
  i: Integer;
  lCursor: TCursor;
begin
  if EditMode<>emView then
    MessageDlg(ResStr_CannotProceed,mtWarning,[mbOk],0)
  else begin
    lCursor := HourglassCursor;
    try
      // Clear previous keys before adding the new ones.
      if (FIndSQL <> '') or (FOrgSQL <> '') then begin
        FIndSQL := '';
        FOrgSQL := '';
      end;

      // Extract only the Keys, and add an extra
      with iNameList do begin
        for i := 0 to Header.ItemCount - 1 do
          if CompareText(Items[i].KeyField2, TN_INDIVIDUAL) = 0 then
            FIndSQL := FIndSQL + ',''' + Items[i].KeyField1 + ''''
          else
          if CompareText(Items[i].KeyField2, TN_ORGANISATION) = 0 then
            FOrgSQL := FOrgSQL + ',''' + Items[i].KeyField1 + ''''
          else
            // KeyField2 doesn't match Ind/Org. Try through Name...
            with dmDatabase.ExecuteSQL(Format(SQL_IS_ORG, [Items[i].KeyField1]), True) do
            begin
              if RecordCount > 0 then
                if Fields['Organisation'].Value = 0 then
                  FIndSQL := FIndSQL + ',''' + Items[i].KeyField1 + ''''
                else
                  FOrgSQL := FOrgSQL + ',''' + Items[i].KeyField1 + '''';
              Close;
            end;
      end;

      // Update the trees only if necessary
      if FIndSQL<>'' then 
        FIndSQL:='WHERE N.Name_Key IN ('+Copy(FIndSQL,2,Length(FIndSQL))+')';
      if FOrgSQL<>'' then
        FOrgSQL:='WHERE N.Name_Key IN ('+Copy(FOrgSQL,2,Length(FOrgSQL))+')';
      // As the trees were cleared, restore them taking filters into account
      ClearTrees(true, true);
      BuildTreeView(tvIndividuals, FIndSQL);  // Rebuild tree, with any filter already applied
      BuildTreeView(tvOrganisations, FOrgSQL);  // Rebuild tree, with any filter already applied

      // Focus on first node of each tree
      FISelectedItem:=tvIndividuals.Items.GetFirstNode;
      FOSelectedItem:=tvOrganisations.Items.GetFirstNode;
      if (FIndSQL<>'') or (FOrgSQL='') then
        // default to show individuals, unless the only filter we just applied was organisations.
        SelectedTree := tvIndividuals
      else
        SelectedTree := tvOrganisations;
      FSelectedItem := SelectedTree.Items.GetFirstNode;
      SelectedTree.Selected := FSelectedItem;
    finally
      bbShowAll.Visible:=true;
      DefaultCursor(lCursor);
    end;
  end;
end;  // DisplayNames   

//==============================================================================
procedure TfrmIndOrg.DisplayFilteredNames;
var
  i: Integer;
  keyList: TEditableKeyList;
  filteredKeys: TStringList;
begin
  keyList := TEditableKeyList.Create;
  try
    keyList.SetTable(MIXED_DATA);
    filteredKeys := AppSettings.GetFilteredRecords(TN_NAME);
    if filteredKeys <> nil then
      for i := 0 to filteredKeys.Count - 1 do
        keyList.AddItem(filteredKeys[i], '');

    filteredKeys := AppSettings.GetFilteredRecords(TN_INDIVIDUAL);
    if filteredKeys <> nil then
      for i := 0 to filteredKeys.Count - 1 do
        keyList.AddItem(filteredKeys[i], TN_INDIVIDUAL);

    filteredKeys := AppSettings.GetFilteredRecords(TN_ORGANISATION);
    if filteredKeys <> nil then
      for i := 0 to filteredKeys.Count - 1 do
        keyList.AddItem(filteredKeys[i], TN_ORGANISATION);

    DisplayNames(keyList);
    if (FIndSQL = '') and (FOrgSQL = '') then
      bbShowAllClick(nil);
  finally
    keyList.Free;
  end;
end;

//==============================================================================
function TfrmIndOrg.GetNameKey:TKeyString;
begin
  Result:='';
  if FSelectedItem<>nil then
    Result:=TNodeObject(FSelectedItem.Data).ItemKey;
end;  // GetNameKey

//==============================================================================
procedure TfrmIndOrg.RelatedObservations(const iMsg:string;
  const iRelType:TRelatedData; const SQLType:TStandardSQL);
var lKeyList:TEditableKeyList;
    stMsg   :string;
    lNameKey:TKeyString;
begin
  lNameKey:=GetNameKey;
  if lNameKey<>'' then begin
    lKeyList:=TEditableKeyList.Create;
    try
      dmGeneralData.GetKeyListFromStandardQuery(lKeyList,SQLType,lNameKey);
      if tsIndividual.Visible then stMsg:=ResStr_Individual
                              else stMsg:=ResStr_Organisation;
      if lKeyList.Header.ItemCount=0 then
        MessageDlg(Format(ResStr_NoIMsg, [iMsg, stMsg]),mtInformation,[mbOk],0)
      else
        if dmFormActions.actObservations.Execute then
          TfrmObservations(frmMain.GetForm(TfrmObservations)).DisplayObservations(iRelType,lKeyList);
    finally
      lKeyList.Free;
    end;
  end;
end;  // RelatedObservations

//==============================================================================
procedure TfrmIndOrg.mnuRelSurveysClick(Sender: TObject);
begin
  inherited;
  RelatedObservations(ResStr_Surveys,rdSurvey,ssSurveysForName);
end;  // mnuRelSurveysClick

//==============================================================================
procedure TfrmIndOrg.mnuRelEventsClick(Sender: TObject);
begin
  inherited;
  RelatedObservations(ResStr_SurveyEvents,rdEvent,ssEventsForName);
end;  // mnuRelEventsClick

//==============================================================================
procedure TfrmIndOrg.mnuRelSamplesClick(Sender: TObject);
begin
  inherited;
  RelatedObservations(ResStr_Samples,rdSample,ssSamplesForName);
end;  // mnuRelSamplesClick

//==============================================================================
procedure TfrmIndOrg.mnuRelOccurClick(Sender: TObject);
begin
  inherited;
  RelatedObservations(ResStr_TaxonOrBiotopeOcc,rdOccurrence,ssOccurrencesForName);
end;  // mnuRelOccurClick

//==============================================================================
procedure TfrmIndOrg.mnuRelLocationsClick(Sender: TObject);
var lLocationList:TEditableKeyList;
    lNameKey     :TKeyString;
    stMsg        :string;
begin
  inherited;
  lNameKey:=GetNameKey;
  if lNameKey<>'' then begin
    lLocationList:=TEditableKeyList.Create;
    try
      dmGeneralData.GetKeyListFromStandardQuery(lLocationList,ssLocationsForName,lNameKey);
      if tsIndividual.Visible then stMsg := ResStr_Individual
                              else stMsg := ResStr_Organisation;
      if lLocationList.Header.ItemCount=0 then
        MessageDlg(Format(ResStr_NoLocationToRelate, [stMsg]),mtInformation,[mbOk],0)
      else
        if dmFormActions.actLocations.Execute then
          TfrmLocations(frmMain.GetForm(TfrmLocations)).DisplayLocations(lLocationList);
    finally
      lLocationList.Free;
    end;
  end;
end;  // mnuRelLocationsClick

//==============================================================================
procedure TfrmIndOrg.mnuRelDocumentsClick(Sender: TObject);
var stMsg      :string;
    lSourceList:TEditableKeyList;
begin
  inherited;
  lSourceList:=TEditableKeyList.Create;
  try
    if FSelectedItem<>nil then begin
      // Get the sources for the displayed Individual/Organisation
      Sources.GetInternalSources(lSourceList);
      if tsIndividual.Visible then stMsg:=ResStr_Individual
                              else stMsg:=ResStr_Organisation;
      if lSourceList.Header.ItemCount=0 then
        MessageDlg(Format( ResStr_NoDocument, [stMsg]), mtInformation,[mbOk],0)
      else
        if dmFormActions.actDocuments.Execute then
          TfrmReferences(frmMain.GetForm(TfrmReferences)).DisplaySources(lSourceList);
    end;
  finally
    lSourceList.Free;
  end;
end;  // mnuRelDocumentsClick

//==============================================================================
procedure TfrmIndOrg.bbShowAllClick(Sender: TObject);
var
  lCursor:TCursor;
begin
  inherited;
  lCursor := HourglassCursor;
  try
    if FIndTreeFiltered then
      RemoveFilter(tvIndividuals);
    if FOrgTreeFiltered then
      RemoveFilter(tvOrganisations);
    FIndTreeFiltered := false;
    FOrgTreeFiltered := false;
  finally
    DefaultCursor(lCursor);
  end;
  bbShowAll.Visible := false;
end;

(**
  Removes a filter from one of the 2 trees 
*)
procedure TfrmIndOrg.RemoveFilter(treeview: TRapidTree);
var
  nodeChain: TStringList;
begin
  nodeChain := TStringList.Create;
  try
    FIndSQL := '';
    FOrgSQL := '';
    RememberPathToSelectedNode(treeview, nodeChain);
    AppSettings.ClearFilteredRecords([TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION]);
    if treeview = tvIndividuals then begin
      ClearFilter(FILTER_INDIVIDUAL);
      ClearTrees(true, false);
    end
    else begin
      ClearFilter(FILTER_ORGANISATION);
      ClearTrees(false, true);
    end;
    BuildTreeView(treeview, '');  // Rebuild trees, without filters
    // select the old filtered nodes.
    RedisplaySelectedNode(treeview, nodeChain);
    // if the current tree view is not the one we just cleared the filter from,
    // reselect the current node
    if SelectedTree<>treeview then
      tvChange(SelectedTree, SelectedTree.Selected);
  finally
    nodeChain.Free;
  end;
end;  // bbShowAll

//==============================================================================
procedure TfrmIndOrg.actFilterExecute(Sender: TObject);
begin
  inherited;
  if SelectedTree = tvIndividuals then GetFilter(FILTER_INDIVIDUAL, 'Individual')
                                  else GetFilter(FILTER_ORGANISATION, 'Organisation');
end;  // actFilterExecute

//==============================================================================
procedure TfrmIndOrg.ApplyFilter(AKeyList: TKeyList);
var
  lTable: String;
begin
  if SelectedTree = tvIndividuals then lTable := TN_INDIVIDUAL
                                  else lTable := TN_ORGANISATION;
  if AKeyList.Header.ItemCount = 0 then
    MessageDlg(Format(ResStr_NoMatch, [lTable]), mtInformation, [mbOk], 0)
  else begin
    DisplayNames(AKeyList);
    if lTable = TN_INDIVIDUAL then
      SelectedTree := tvIndividuals
    else
      SelectedTree := tvOrganisations;
  end;
end;

//==============================================================================
procedure TfrmIndOrg.WMTransferDone(var Msg: TMessage);
begin
  Show;
end;  // WMTransferDone

//==============================================================================
procedure TfrmIndOrg.WMRefreshDocuments(var Msg: TMessage);
begin
  Sources.RefreshLists;
end;  // WMRefreshDocuments

//==============================================================================
procedure TfrmIndOrg.WMRefreshColours(var Msg: TMessage);
begin
  // Change colours only where needed.
  if EditMode <> emView then begin
    if SelectedTree=tvIndividuals then SetRequiredFieldsColourState(true,[dbeISurname])
                                  else SetRequiredFieldsColourState(true,[dbeOFullName]);
    // Adding/editing address
    if bbAAccept.Enabled then SetRequiredFieldsColourState(true,[mmAAddress,eAFrom]);
    // Adding/editing Contact Numbers
    if bbCAccept.Enabled then SetRequiredFieldsColourState(true,[cmbCType,eCNumber]);
    // Adding/editing Communications
    if bbCOAccept.Enabled then SetRequiredFieldsColourState(true,[eCOName,cmbCOType,eCODate]);
    // Adding/editing Associations
    if bbASAccept.Enabled then SetRequiredFieldsColourState(true,[eASName,cmbASRole]);
  end;
  Repaint;
end;  // WMRefreshColours

//==============================================================================
procedure TfrmIndOrg.WMImportedComplete(var Msg: TMessage);
begin
  //COmmented out as cannot test at present
  //Refresh display. Uses same code as create method to repopulate
{  ClearTrees(true,true);
  BuildTreeView(tvIndividuals,'');
  BuildTreeView(tvOrganisations,'');}
end;
//==============================================================================
procedure TfrmIndOrg.RefreshShownLists;
begin
  if FAddressesShown then FAddressList.Refresh;
  if FContactsShown  then FContactList.Refresh;
  if FCommsShown     then FCommsList.Refresh;
  if FAssocsShown    then FAssocList.Refresh;
  FAddressesShown:=false;
  FContactsShown :=false;
  FCommsShown    :=false;
  FAssocsShown   :=false;
end;  // RefreshShownLists

//==============================================================================
procedure TfrmIndOrg.actShowMetadataExecute(Sender: TObject);
var lTableName: String;
    lqry      : TJnccQuery;
begin
  if ((FSelectedTree = tvIndividuals) and (FSelectedItem.Level = 0)) or
     ((FSelectedTree = tvOrganisations) and (FSelectedItem.Level = 1)) then
    lTableName := TN_INDIVIDUAL
  else
    lTableName := TN_ORGANISATION;

  lqry := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqry]);
    lqry.SQL.Text := 'SELECT * FROM ' + lTableName + ' IO ' +
                     'INNER JOIN Name N ON N.Name_Key = IO.Name_Key ' +
                     'WHERE IO.Name_Key = ''' + FCurrentKey + '''';

    with TdlgMetaDataPopup.Create(nil) do
      try
        ShowStandard(lTableName, lblSelectedName.Caption, FCurrentKey, lqry);
    finally
      Free;
    end;
  finally
    if lqry.Active then lqry.Close;
    lqry.Free;
  end;
end;  // actShowMetadataExecute

//==============================================================================
{ Read individual and organisation sorts }
procedure TfrmIndOrg.ReadSortOrders;
var
  lDummy : boolean;
  lSortField : string;
begin
  { Read individuals }
  if not AppSettings.ReadSort(self, 'Individual', lSortField, lDummy) then
    IndividualSort := SURNAME_SORT // default for first time
  else
    IndividualSort := lSortField;
  { Read organisations }
  if not AppSettings.ReadSort(self, 'Organisation', lSortField, lDummy) then
    OrganisationSort := FULLNAME_SORT // default for first time
  else
    OrganisationSort := lSortField;
end;

//==============================================================================
{ Write individual and organisation sorts }
procedure TfrmIndOrg.WriteSortOrders;
begin
  AppSettings.WriteSort(self, 'Individual', IndividualSort, True);
  AppSettings.WriteSort(self, 'Organisation', OrganisationSort, True);
end;


// Description : implements GetCurrentControlEditMode required for TBaseExportable
// Created : 15/11/02
function TfrmIndOrg.GetCurrentControlEditMode: Constants.TEditMode;
begin
  // for pages with sub-lists, the edit mode is not relevant.
  if (pcNameDetails.ActivePage = tsAddresses) or
     (pcNameDetails.ActivePage = tsContacts) or
     (pcNameDetails.ActivePage = tsComms) or
     (pcNameDetails.ActivePage = tsAssoc) or
     (pcNameDetails.ActivePage = tsSources) then
    Result := emNone // no edit lock messages are necessary
  else
    Result := FEditMode;
end;

//==============================================================================
{ Accessor method for IndividualSort - resorts the selected list.  If we
    are looking at individuals then updates the current individual sort order
    (not if we are looking at the contents of an organisation) }
procedure TfrmIndOrg.SetIndividualSort(const Value: string);
begin
  FIndividualSort := Value;
  if Value = FORENAME_SORT then
    FSortOrder := FORENAME_SORT  // UpdateIndividualOrder(ORDER_BY_FORENAME)
  else if Value = SURNAME_SORT then
    FSortOrder := SURNAME_SORT  // UpdateIndividualOrder(ORDER_BY_SURNAME)
  else begin
    FSortOrder := FORENAME_SORT;  // UpdateIndividualOrder(ORDER_BY_FORENAME); // default if an invalid item passe
    if SelectedTree=tvIndividuals then
      FIndividualSort := FORENAME_SORT;
  end;
  if FSortOrder = FORENAME_SORT then FIndSortSQL := ORDER_BY_FORENAME
                                else FIndSortSQL := ORDER_BY_SURNAME;
  PerformSort;
end;  // SetIndividualSort

//==============================================================================
{ Accessor method for OrganisationSort - as SetIndividualSrot}
procedure TfrmIndOrg.SetOrganisationSort(const Value: string);
begin
  FOrganisationSort := Value;
  if Value = FULLNAME_SORT then
    FSortOrder := FULLNAME_SORT  // UpdateOrganisationOrder(ORDER_BY_FULLNAME)
  else if Value = ACRONYM_SORT then
    FSortOrder := ACRONYM_SORT  // UpdateOrganisationOrder(ORDER_BY_ACRONYM)
  else begin
    FSortOrder := ACRONYM_SORT; // UpdateOrganisationOrder(ORDER_BY_ACRONYM); // default
    if SelectedTree=tvOrganisations then
      FOrganisationSort := ACRONYM_SORT;
  end;
  if FSortOrder = FULLNAME_SORT then FOrgSortSQL := ORDER_BY_FULLNAME
                                else FOrgSortSQL := ORDER_BY_ACRONYM;
  PerformSort;
end;  // SetOrganisationSort

//==============================================================================
procedure TfrmIndOrg.PerformSort;
var lCursor: TCursor;
begin
  inherited;
  lCursor := HourglassCursor;
  try
    // PerformSort is called before SelectedTree is properly set in constructor,
    // due to ReadSortOrders. So check and set if needed
    if SelectedTree = nil then FSelectedTree := tvIndividuals;
    // Do the sort
    SelectedTree.SortType := stData;
  finally
    // Stop automatic sorting
    SelectedTree.SortType := stNone;
    FSelectedItem := SelectedTree.Selected;
    DefaultCursor(lCursor);
  end;  // try .. finally
end;  // PerformSort

//==============================================================================
procedure TfrmIndOrg.tvIndividualsKeyDown(Sender: TObject;
                                          var Key: Word;
                                          Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_DELETE: if bbDelete.Enabled then bbDeleteClick(bbDelete);
  end;
end;

//==============================================================================
procedure TfrmIndOrg.tvCompare(Sender: TObject; Node1,
  Node2: TFlyNode; var Compare: Integer);
var lstBuffer, lstText1, lstText2: String;
begin
  inherited;
  if Assigned(Node1.Data) and Assigned(Node2.Data) and (Node1.Parent = Node2.Parent)
      and (Node1.Level = Node2.Level)and
      // Sorting individuals, so only order the nodes if they are Individual nodes!
     ((((FSortOrder = FORENAME_SORT) or (FSortOrder = SURNAME_SORT)) and
       (((SelectedTree = tvIndividuals) and (Node1.Level = 0)) or
        ((SelectedTree = tvOrganisations) and (Node1.Level = 1))))
      or
      // Sorting organisations, so only move the nodes if they are organisation nodes!!
      (((FSortOrder = ACRONYM_SORT) or (FSortOrder = FULLNAME_SORT)) and
       (((SelectedTree = tvIndividuals) and (Node1.Level = 1)) or
        ((SelectedTree = tvOrganisations) and (Node1.Level = 0))))) then
  begin
    if (FSortOrder = SURNAME_SORT) or (FSortOrder = FULLNAME_SORT) then begin
      // In this case, switch first and last parts around
      lstText1 := TNameNode(Node1.Data).Text;
      lstText2 := TNameNode(Node2.Data).Text;
      // First part
      lstBuffer := Copy(lstText1, 1, Pos(';', lstText1) - 1);
      // Remove first part
      lstText1  := Copy(lstText1, Pos(';', lstText1) + 1, 255);
      // Second part + first part
      lstBuffer := Copy(lstText1, 1, Pos(';', lstText1) - 1) + ';' + lstBuffer;
      // Third part + Second + First
      lstText1  := Copy(lstText1, Pos(';', lstText1) + 1, 255) + ';' + lstBuffer;

      // First part
      lstBuffer := Copy(lstText2, 1, Pos(';', lstText2) - 1);
      // Remove first part
      lstText2  := Copy(lstText2, Pos(';', lstText2) + 1, 255);
      // Second part + first part
      lstBuffer := Copy(lstText2, 1, Pos(';', lstText2) - 1) + ';' + lstBuffer;
      // Third part + Second + First
      lstText2  := Copy(lstText2, Pos(';', lstText2) + 1, 255) + ';' + lstBuffer;
    end else begin
      lstText1 := Node1.Text;
      lstText2 := Node2.Text;
    end;
    Compare := CompareText(lstText1, lstText2);
  end else
    Compare := Node1.GetRow - Node2.GetRow;  // Don't move the nodes around.
end;  // tvCompare

//==============================================================================
class function TfrmIndOrg.GetNameCustodian(const AKey: TKeyString): string;
begin
  Result := dmGeneralData.Custodian(TN_NAME, 'Name_Key', AKey);
end;

{-------------------------------------------------------------------------------
  Populate the contents of the department combo when it is first dropped dowm
}
procedure TfrmIndOrg.cmbDeptPopulate(Sender: TObject);
begin
  cmbDept.Add(ResStr_None, '');
  with dmDatabase.GetRecordset('usp_OrganisationDepartments_Select_ForIndividual',
      ['@Key', FCurrentKey]) do
    while not EOF do begin
      cmbDept.Add(VarToStr(Fields['Item_Name'].Value),
          VarToStr(Fields['Organisation_Department_Key'].Value));
      MoveNext;
    end; // while
  cmbDept.ItemIndex := cmbDept.IDIndexOf(FDeptKey);
end;

{-------------------------------------------------------------------------------
  Populate the department combo with the current selection for the user
}
procedure TfrmIndOrg.PopulateDepartment;
var
  lKey: string;
begin
  cmbDept.Clear;
  // select the currently linked department
  with dmDatabase.GetRecordset('usp_OrganisationDepartment_Select_ForIndividual',
      ['@Key', FCurrentKey]) do
    if not EOF then begin
      lKey := VarToStr(Fields['Organisation_Department_Key'].Value);
      if cmbDept.Populated then
        cmbDept.ItemIndex := cmbDept.IDIndexOf(lKey)
      else begin
        cmbDept.Add(VarToStr(Fields['Item_Name'].Value), lKey);
        cmbDept.ItemIndex := 0;
      end;
    end
    else begin
      cmbDept.Add(ResStr_None, '');
      cmbDept.ItemIndex := 0;
    end;
  cmbDept.ReadOnly := (FCustodian <> AppSettings.SiteID);
end;

{-------------------------------------------------------------------------------
  Set up the departments grid when the tab is first shown
}
procedure TfrmIndOrg.tsDepartmentsShow(Sender: TObject);
var
  lRow: integer;
begin
  inherited;
  if (not FDepartmentsPopulated) and Assigned(FdmName) then begin
    // Clear the grid
    for lRow := 1 to sgDepartments.RowCount-1 do
      sgDepartments.Rows[lRow].CommaText := '';
    with FdmName.qryDepartments do begin
      Close;
      Sql.Text :=
          'exec usp_Departments_Select_ForOrganisation ''' + FCurrentKey + '''';
      Open;
      FieldByName('Acronym').DisplayWidth := (sgDepartments.Width - 17) div 3; // 1/3
      FieldByName('Item_Name').DisplayLabel := 'Name';
      FieldByName('Item_Name').DisplayWidth := 2 * (sgDepartments.Width - 17) div 3; // 2/3
      FieldByName('Organisation_Department_Key').Visible := False;
      FieldByName('Custodian').Visible := False;
      FieldByName('Acronym').Required := False;
      FieldByName('Item_Name').Required := True;
      FieldByName('Entered_By').Visible := False;
    end;
    if not Assigned(FDepartmentGridManager) then begin
      FDepartmentGridManager := TDataStringGrid.Create(sgDepartments,
          FdmName.qryDepartments, 'Organisation_Department_Key');
      with FDepartmentGridManager do begin
        OnDeleteRow := DeleteDepartment;
        OnUpdateRow := UpdateDepartment;
        UserID := AppSettings.UserID;
        RestrictFullEdit := AppSettings.RestrictFullEdit;
        SiteID := AppSettings.SiteID;
        AddOnly := AppSettings.UserAccessLevel=ualAddOnly;
        PopulateGrid;
      end;
    end
    else
      FDepartmentGridManager.Refresh;
    FDepartmentsPopulated := True;
  end; // if
end;

{-------------------------------------------------------------------------------
  When the departments data string grid notifies a deletion event, record the
      key so that the record can be deleted when the data is saved.
}
procedure TfrmIndOrg.DeleteDepartment(ioRowKey: string);
begin
  if FdmName.ReferencedInTable(ioRowKey, TN_INDIVIDUAL, ['Organisation_Department_Key']) then
    MessageDlg( ResStr_UnselectIndividual, mtInformation, [mbOk], 0)
  else
    dmDatabase.RunStoredProc('usp_OrganisationDepartment_Delete', ['@Key', ioRowKey]);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmIndOrg.UpdateDepartment(var ioRowKey: string; iData: TStringList);
begin
  if ioRowKey='' then
    ioRowKey := dmDatabase.GetStoredProcOutputParam('usp_OrganisationDepartment_Insert',
        ['@Acronym', iData.Values['Acronym'],
        '@NameKey', FCurrentKey,
        '@ItemName', iData.Values['Item_Name'],
        '@EnteredBy', AppSettings.UserId],
        '@Key')
  else
    dmDatabase.RunStoredProc('usp_OrganisationDepartment_Update',
        ['@Key', ioRowKey,
        '@Acronym', iData.Values['Acronym'],
        '@ItemName', iData.Values['Item_Name'],
        '@ChangedBy', AppSettings.UserID]);
end;

{-------------------------------------------------------------------------------
  Accessor for the details TPageControl
}
function TfrmIndOrg.GetDetailsPageControl: TPageControl;
begin
  Result := pcNameDetails;
end;

{-------------------------------------------------------------------------------
  Allow external code to select a node
}
function TfrmIndOrg.SelectNode(const ANodeType,
  ANodeKey: string): TFlyNode;
var
  i: integer;
  lNode: TFlyNode;
begin
  with dmDatabase.ExecuteSQL('SELECT Organisation FROM [Name]', True) do
    if Fields['Organisation'].Value then
      SelectedTree := tvOrganisations
    else
      SelectedTree := tvIndividuals;
  lNode := nil;
  for i:=0 to SelectedTree.Items.Count-1 do begin
    lNode:=SelectedTree.Items[i];
    if (TNameNode(lNode.Data).ItemKey=ANodeKey) then begin
      SelectedTree.Selected:=lNode;
      Break;
    end;  // if
  end;  // for
  Result := lNode;
end;

{-------------------------------------------------------------------------------
  Populate Quick Reports menu when submenu opened
}
procedure TfrmIndOrg.pmHQuickReportsClick(Sender: TObject);
begin
  inherited;
  BringToFront; // in case popup activated when not the active mdi child, which can happen
  frmMain.PopulateQuickReportSubMenu(FSelectedTree.Selected, pmHQuickReports);
end;

{-------------------------------------------------------------------------------
  Retrieve current node type for custom reports
}
function TfrmIndOrg.CustomReportKeyType: TKeyType;
begin
  Result := inherited CustomReportKeyType;
  with FSelectedTree do
    if Assigned(Selected) then
      if TObject(Selected.Data) is TReportableNode then
        Result := TReportableNode(Selected.Data).ReportKeyType;
end;

{-------------------------------------------------------------------------------
  Retrieve current node key for custom reports
}
function TfrmIndOrg.ItemKey: string;
begin
  Result := inherited ItemKey;
  with FSelectedTree do
    if Assigned(Selected) then
      if TObject(Selected.Data) is TReportableNode then
        Result := TReportableNode(Selected.Data).ItemKey
end;

//  Get appropriate tree for current view
function TfrmIndOrg.GetTreeView: TRapidTree;
begin
  if sbIndividuals.down then
    result := tvIndividuals
  else
    result := tvOrganisations;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmIndOrg.ExpandAllNodes;
begin
  if FOrgSQL <> '' then begin
    sbOrganisations.click;
    inherited;
  end
  else begin
    sbOrganisations.click;
    inherited;
  end;
end;

{-------------------------------------------------------------------------------
  Add a new department row, if the last one isn't empty.
}
procedure TfrmIndOrg.btnDeptAddClick(Sender: TObject);
begin
  inherited;
  if Assigned(FDepartmentGridManager) then begin
    FDepartmentGridManager.AddToGrid(Sender);
    sgDepartments.SetFocus;
    // Force edit mode - see VI 17206
    sgDepartments.Options := sgDepartments.Options + [Grids.goEditing];
    btnDeptDel.Enabled := True;
  end;
end;

{-------------------------------------------------------------------------------
  Delete the selected row. If it's the last row, only the content is cleared.
}
procedure TfrmIndOrg.btnDeptDelClick(Sender: TObject);
var
  lKey: String;
begin
  inherited;
  if Assigned(FDepartmentGridManager) then begin
    if (sgDepartments.Row >= sgDepartments.RowCount) or
       FDepartmentGridManager.RowLocked(sgDepartments.Row) then
    begin
      MessageDlg(ResStr_NotAllowedToDeleteOtherSitesDept, mtInformation, [mbOK], 0);
      btnDeptDel.Enabled := False;
    end else begin
      lKey := FDepartmentGridManager.Key[sgDepartments.Row];
      if FdmName.ReferencedInTable(lKey, TN_INDIVIDUAL, ['Organisation_Department_Key']) then
        MessageDlg(ResStr_UnselectIndividual, mtInformation, [mbOk], 0)
      else begin
        if not FDepartmentGridManager.RowContainsData(sgDepartments.Row) or
           (MessageDlg(ResStr_DeleteDepartment, mtConfirmation, [mbNo, mbYes], 0) = mrYes) then
        begin
          FDepartmentGridManager.DeleteFromGrid(Sender);
          if (sgDepartments.Row >= sgDepartments.RowCount) or
             FDepartmentGridManager.RowLocked(sgDepartments.Row) then
            btnDeptDel.Enabled := False;
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Draw highlighted cells with coloured text
}
procedure TfrmIndOrg.tvDrawCell(Sender: TObject;
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
    // Selected nodes always look the same, whether highlighted or not
    if lNode.Selected then
      ACanvas.Font.Color := clHighlightText
    else
      dmGeneralData.SetCanvasColourForFilterNode(ACanvas, TNodeObject(lNode.Data).IsFiltered);
    lRect := Rect;
    lRect.Left := Rect.Left + lTree.Indent * (lNode.Level + 1);
    ACanvas.FillRect(lRect);
    ACanvas.TextOut(lRect.Left + 4, lRect.Top + 1, lNode.Caption);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmIndOrg.DoShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo:
    THintInfo);
var
  X, Y: Integer;
  lNode: TFlyNode;
begin
  if (HintInfo.HintControl = tvIndividuals) or (HintInfo.HintControl = tvOrganisations) then begin 
    X := Mouse.CursorPos.X - ClientOrigin.X - HintInfo.HintControl.Left - pnlLists.Left;
    Y := Mouse.CursorPos.Y - ClientOrigin.Y - HintInfo.HintControl.Top - pnlLists.Top;
    lNode := TRapidTree(HintInfo.HintControl).GetNodeAt(X, Y);
    if lNode <> nil then
      HintStr := TNodeObject(lNode.Data).Hint;
    CanShow := HintStr <> '';
    if CanShow then
      HintInfo.ReshowTimeout := 200;
  end else
    inherited;
end;  // TfrmIndOrg.DoShowHint

{-------------------------------------------------------------------------------
}
procedure TfrmIndOrg.pmHBatchUpdateClick(Sender: TObject);
begin
  inherited;
  BringToFront; // in case popup activated when not the active mdi child, which can happen
  frmMain.PopulateBatchUpdateSubMenu(FSelectedTree.Selected, pmHBatchUpdate);
end;
    
{-------------------------------------------------------------------------------
}
procedure TfrmIndOrg.ApplySecurity;
begin
  EnableDetails(FEditMode);
  pmHAdd.Enabled := (AppSettings.UserAccessLevel >= ualAddOnly);
  pmHBatchUpdate.Visible := AppSettings.UserAccessLevel >= ualAdmin;
end;

{-------------------------------------------------------------------------------
  Setup Find data link for Communication With field
}
procedure TfrmIndOrg.eCONameFindData(Sender: TObject);
begin
  inherited;
  dmGeneralData.CheckName(eCOName);
end;

{-------------------------------------------------------------------------------
  Fidn dialog setup for Association With field.
}
procedure TfrmIndOrg.eASNameFindData(Sender: TObject);
begin
  inherited;
  dmGeneralData.CheckName(eASName);
end;

{-------------------------------------------------------------------------------
  Display Date of Birth and Date of Death on biography page
}
procedure TfrmIndOrg.PopulateBiographyDates(AQuery: TJNCCQuery);
begin
  eHDateOfBirth.Text := AQuery.FieldByName('Born_Vague_Date_Start').Text;
  eHDateOfDeath.Text := AQuery.FieldByName('Died_Vague_Date_Start').Text;
  eHActivePeriod.Text := AQuery.FieldByName('Active_Vague_Date_Start').Text;
end;

{-------------------------------------------------------------------------------
  Handles click events on the Department Grid
}
procedure TfrmIndOrg.sgDepartmentsClick(Sender: TObject);
begin
  btnDeptDel.Enabled := not FDepartmentGridManager.RowLocked(sgDepartments.Row)
      and (sgDepartments.Row < sgDepartments.RowCount)
      and (EditMode <> emView);
end;

{-------------------------------------------------------------------------------
  When the details panel is resized, resizes its contents to fit.
}
procedure TfrmIndOrg.pnlIndOrgDetailsResize(Sender: TObject);
var
  deltaWidth, deltaHeight : Integer;
begin
  inherited;
  // Works out the change in width and height of the panel since the last
  // resize.
  deltaWidth := pnlIndOrgDetails.Width - pnlInner.Width;
  deltaHeight := pnlIndOrgDetails.Height - pnlInner.Height;

  // For some reason, the automatic resizing doesn't work properly. Using a
  // second panel within the main panel and sizing it manually solves this, but
  // is a bit of a hack.
  pnlInner.Width := pnlIndOrgDetails.Width;
  pnlInner.Height := pnlIndOrgDetails.Height;

  // Sets the dimensions of components which do not have anchors.
  eCOName.Width := eCOName.Width + deltaWidth;
  eASName.Width := eASName.Width + deltaWidth;
  Sources.Width := Sources.Width + deltaWidth;
  Sources.Height := Sources.Height + deltaHeight;
end;

{
  Populates the Roles dropdown on the Assocs tab from the
  Relation_Role_Type table
}
procedure TfrmIndOrg.PopulateAssociatesRole;
begin
  with dmDatabase.GetRecordset('usp_RelationRoleType_Select', []) do
    while not EOF do begin
      cmbASRole.AddItem(VarToStr(Fields['Short_Name'].Value), nil);
      MoveNext;
    end;
end;

procedure TfrmIndOrg.btnAdditionalInfoClick(Sender: TObject);
var ExtraInfo : string;
begin
  inherited;
  with dmDatabase.GetRecordset('usp_Names_Extra_Info', ['@key',FCurrentKey,'@Detail',1]) DO
    begin
      while not Eof do begin
         ExtraInfo := Fields['DETAILS'].Value;
         MessageDlg(ExtraInfo, MtInformation, [mbOk], 0);
         movenext;
       end;
     end;
   end;

end.
