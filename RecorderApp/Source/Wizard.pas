//==============================================================================
//  Unit:        Wizard
//
//  Implements:  TdlgWizard
//               TWizardKeyObject
//               TTabSequence
//
//  Description: Allows users to filter the NBN biological data by means of an
//               easy to use, step-by-step wizard.  When the wizard is opened,
//               so is frmResultWindow but this is minimised at the foot of the
//               MDI main window, frmMain.
//
//               TWizardKeyObject and TTabSequence
//               Helper classes.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 302 $
//    $Date: 13/10/09 9:23 $
//    $Author: Simonlewis $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit Wizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls, CheckLst, BaseFormUnit, ExceptionForm,
  DBListCombo, WizardData, DB, HierarchyNodes, DataClasses, Finder, Filter,
  JNCCDatasets, AddFilterContainer, SpatialRefFuncs, ImgList, OnlineHelp,
  ComplexFilter, Constants, Rucksack, MapServerLink, MapPolygonScanner, exgrid,
  KeyboardRapidTree, RapTree, GeneralFunctions, AdminAreaDictBrowserData,
  TaxonDictBrowserData, BiotopeDictBrowserData, ImageListButton, ReportGenerator,
  DatabaseAccessADO, Menus, ReportWizardSettings;

resourcestring
  ResStr_FindingLocations = 'Finding locations in bounding box';
  ResStr_OldReportFile = 'This report file is in Recorder 2000 format. It may not be loaded into' +
                         ' the report wizard but may be run using Reports --> Run.';
  ResStr_SelectSource = 'At least one source should be selected.';
  ResStr_SelectDesignation = 'At least one designation should be selected.';
  ResStr_SelectAdminArea =  'At least one Administrative Area should be selected.';
  ResStr_SelectItem = 'At least one item should be selected.';
  ResStr_EnterBBCoord = 'Please enter bounding box co-ordinates.';
  ResStr_SelectLocation = 'At least one location should be selected.';
  ResStr_SelectPolygon =  'At least one polygon should be selected';
  ResStr_SelectFile = 'If choosing an existing layout you must select a file';
  ResStr_SelectReport = 'No report has been selected.';
  ResStr_SelectList = 'Please select a list.';
  ResStr_NoMatchingLocation = 'No Locations Match the given area.';
  ResStr_SelectAttribute =  'At least one attribute must be selected.';
  ResStr_IndexOutOfBound =  'Index out of bounds.';
  ResStr_InvalidPolygonFilter = 'Invalid Polygon Filter File.';
  ResStr_FilterFilename = 'You must enter a file name to save the filter.';
  ResStr_InvalidFilenameChars = 'A filename may not contain any of the following characters:'#13#10 +
                                '|*\/:"?<>';

  ResStr_InvalidPolygonRef =  'Invalid Data. Polygon reference expected.';
  ResStr_InvalidWizFile = '''%s'' is not a valid report file';
  // These must match the respective group names in the report_attribute table.
  ResStr_GroupTaxon = 'Taxon';
  ResStr_GroupBiotope = 'Biotope';
  ResStr_GroupObservation = 'Observation';
  ResStr_GroupSurvey = 'Survey';
  ResStr_GroupEvent = 'Event';
  ResStr_GroupSample = 'Sample';
  ResStr_GroupLocation = 'Location';
  // Michael Weideli  Mantis 473
  ResStr_NotExpandable = 'The taxanomic groups listed below are not suitable for expanding. Are you sure you want to proceed' + #13#10;

type
  EWizard = class(TExceptionPath);
  EInvalid_Selection = class(EWizard);

  ETabSequence = class(TExceptionPath);
  EInvalidIndex = class(ETabSequence);

  TReportOccurrenceType = (otTaxa, otBiotopes);
  TReportOccurrenceTypes = set of TReportOccurrenceType;

  //----------------------------------------------------------------------------
  TWizardKeyObject = class(TObject)
  private
    FParentKey: TKeyString;
    FKey: TKeyString;
    FText: String;
    FAdditional: String;
  protected
    function GetAdditional: String;
    procedure SetAdditional(const Value: String);
  public
    constructor Create(const AKey: TKeyString); overload;
    property ParentKey: TKeyString read FParentKey write FParentKey;
    property Key: TKeyString read FKey write FKey;
    property Text: String read FText write FText;
    property Additional: String read GetAdditional write SetAdditional;
  end;  // TWizardKeyObject

  //----------------------------------------------------------------------------
  TTabSequence = class(TList)
  private
    FSequence: Integer;
    function GetTab: TTabSheet;
  public
    constructor Create;
    procedure Clear; override;
    procedure AddToSequence(itsSheet: TTabSheet);
    procedure InsertIntoSequence(iiIndex: Integer; itsSheet: TTabSheet);
    procedure RemoveTab(itsSheet: TTabSheet);
    procedure MoveOn;
    procedure MoveBack;
    function JumpTo(itsSheet: TTabSheet): Boolean;
    property Sequence: Integer read FSequence write FSequence;
    property CurrentTab: TTabSheet read GetTab;
  end;  // TTabSequence

  //----------------------------------------------------------------------------
  TdlgWizard = class(TBaseForm)
    ilWizard: TImageList;
    pmMapForBoundingBox: TPopupMenu;
    pmMapForPolygons: TPopupMenu;
    pnlOuter: TPanel;
    pnlInner: TPanel;
    bbNext: TImageListButton;
    bbBack: TImageListButton;
    bbCancel: TImageListButton;
    pcWizard: TPageControl;
    tsReportSelector: TTabSheet;
    tsSourcesSelector: TTabSheet;
    tsSourcesSelection: TTabSheet;
    tsAdminSelection: TTabSheet;
    tsTaxaBioSelection: TTabSheet;
    tsPlacesSelector: TTabSheet;
    tsPlacesSelection: TTabSheet;
    tsGridRefSelection: TTabSheet;
    tsPolygonSelection: TTabSheet;
    tsInfoSelector: TTabSheet;
    tsInfoSelection: TTabSheet;
    tsLayout: TTabSheet;
    tsSelectAttributes: TTabSheet;
    tsSortAttributes: TTabSheet;
    tsSummary: TTabSheet;
    gbStyle: TGroupBox;
    Panel3: TPanel;
    Image2: TImage;
    cbRestrictSource: TCheckBox;
    cbRestrictArea: TCheckBox;
    cbRestrictBounds: TCheckBox;
    cbRestrictPolygons: TCheckBox;
    rbPlaceReport: TRadioButton;
    rbBiotopesReport: TRadioButton;
    rbSpeciesReport: TRadioButton;
    Label14: TLabel;
    gbSources: TGroupBox;
    Panel6: TPanel;
    Image5: TImage;
    Label15: TLabel;
    rbSourcesReferences: TRadioButton;
    rbSourcesIndividuals: TRadioButton;
    rbSourcesSurveys: TRadioButton;
    rbSourcesTags: TRadioButton;
    gbPlaces: TGroupBox;
    Panel4: TPanel;
    Image3: TImage;
    Label19: TLabel;
    rbByPlaceName: TRadioButton;
    rbByGridRef: TRadioButton;
    gbSpatial: TGroupBox;
    pnlSpatialRefImage: TPanel;
    imgExplanation: TImage;
    rbExcludeOverlaps: TRadioButton;
    rbIncludeOverlaps: TRadioButton;
    gbGridRefCorners: TGroupBox;
    Label23: TLabel;
    Label30: TLabel;
    btnGetBoundingBox: TImageListButton;
    eSWCorner: TEdit;
    eNECorner: TEdit;
    btnGetBoundingBoxDropDown: TButton;
    Label22: TLabel;
    gbInformation: TGroupBox;
    Panel1: TPanel;
    imgInformation: TImage;
    rbPlacesInfo: TRadioButton;
    rbBiotopeInfo: TRadioButton;
    rbSpeciesInfo: TRadioButton;
    Label2: TLabel;
    gbInformationSelection: TGroupBox;
    Panel2: TPanel;
    Image1: TImage;
    Label17: TLabel;
    rbInfoSelect: TRadioButton;
    rbInfoAll: TRadioButton;
    gbLayout: TGroupBox;
    Panel5: TPanel;
    Image4: TImage;
    rbLayoutNew: TRadioButton;
    rbLayoutExisting: TRadioButton;
    cmbLayout: TComboBox;
    lblReportFile: TLabel;
    btnAdvanced: TButton;
    Label6: TLabel;
    Label4: TLabel;
    Label12: TLabel;
    Label1: TLabel;
    Label5: TLabel;
    Label24: TLabel;
    Label21: TLabel;
    gbSourceSelect: TGroupBox;
    lbSourcesAvailable: TListBox;
    Label27: TLabel;
    fndSources: TFinder;
    cmbSourcesType: TComboBox;
    Label26: TLabel;
    lblSourcesType: TLabel;
    pnlButtonsSources: TPanel;
    bbSourceAddAll: TImageListButton;
    bbSourceClearAll: TImageListButton;
    bbSourceRemove: TImageListButton;
    bbSourceAdd: TImageListButton;
    pnlSourcesSelected: TPanel;
    Label28: TLabel;
    lbSourcesSelected: TListBox;
    gbAdminSelection: TGroupBox;
    Label10: TLabel;
    Label3: TLabel;
    cmbType: TComboBox;
    Label16: TLabel;
    tvAdminAvailable: TKeyboardRapidTree;
    pnlButtonsAdmin: TPanel;
    bbAdminAdd: TImageListButton;
    bbAdminRemove: TImageListButton;
    bbAdminClearAll: TImageListButton;
    pnlAdminSelected: TPanel;
    lblAdminSelected: TLabel;
    lbAdminSelected: TListBox;
    gbTaxaBioSelection: TGroupBox;
    lblTaxaBioSelection: TLabel;
    lblActiveBioTaxa: TLabel;
    cmbCategories: TComboBox;
    lblTaxaBioType: TLabel;
    lblTaxaBioFind: TLabel;
    fndTaxaBioFinder: TFinder;
    chkIncludeListTxBt: TCheckBox;
    cbIncludeAllTxBt: TCheckBox;
    lblAvailable: TLabel;
    lbAvailable: TListBox;
    pnlButtonsTaxaBio: TPanel;
    bbAdd: TImageListButton;
    bbRemove: TImageListButton;
    bbAddAll: TImageListButton;
    bbClearAll: TImageListButton;
    pnlSelected: TPanel;
    lblSelected: TLabel;
    lbSelected: TListBox;
    cbExpandTaxa: TCheckBox;
    gbPlacesSelection: TGroupBox;
    Label7: TLabel;
    Label31: TLabel;
    fndPlaceFinder: TFinder;
    Label8: TLabel;
    lbPlacesAvailable: TListBox;
    pnlButtonsPlaces: TPanel;
    bbPlacesAdd: TImageListButton;
    bbPlacesRemove: TImageListButton;
    bbPlacesAddAll: TImageListButton;
    bbPlacesClearAll: TImageListButton;
    pnlPlacesSelected: TPanel;
    Label9: TLabel;
    lbPlacesSelected: TListBox;
    cbIncludeSubsites: TCheckBox;
    gbPolygon: TGroupBox;
    btnFindPolygon: TImageListButton;
    btnFindPolygonDropDown: TButton;
    tvPolygonLayers: TKeyboardRapidTree;
    rbPolygonExcludeSquares: TRadioButton;
    rbPolygonIncludeSquares: TRadioButton;
    btnSaveFilter: TButton;
    cmbPredefinedFilters: TComboBox;
    lblPredefinedFilter: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    gbAttributes: TGroupBox;
    tvAttributes: TKeyboardRapidTree;
    Label29: TLabel;
    Label25: TLabel;
    gbSortAttributes: TGroupBox;
    Label32: TLabel;
    Label33: TLabel;
    bbAscending: TImageListButton;
    bbDescending: TImageListButton;
    bbNoSort: TImageListButton;
    tvSort: TKeyboardRapidTree;
    gbSummary: TGroupBox;
    Label13: TLabel;
    lblFilterApplied: TLabel;
    tvSummary: TKeyboardRapidTree;
    cbRestrictDesignations: TCheckBox;
    tsDesignations: TTabSheet;
    gbDesignations: TGroupBox;
    lblDesignations: TLabel;
    lblDesignationClassification: TLabel;
    lblDesignationAvailable: TLabel;
    pnlDesignationButtons: TPanel;
    bbDesignationAdd: TImageListButton;
    bbDesignationRemove: TImageListButton;
    bbDesignationClearAll: TImageListButton;
    pnlDesignationSelected: TPanel;
    lblDesignationSelected: TLabel;
    lbDesignationSelected: TListBox;
    bbDesignationsAddAll: TImageListButton;
    lblDesignationList: TLabel;
    lbDesignationAvailable: TListBox;
    cmbDesignationSet: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbNextClick(Sender: TObject);
    procedure bbBackClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAdvancedClick(Sender: TObject);
    procedure bbAscendingClick(Sender: TObject);
    procedure bbDescendingClick(Sender: TObject);
    procedure bbNoSortClick(Sender: TObject);
    procedure bbSourceAddClick(Sender: TObject);
    procedure bbSourceRemoveClick(Sender: TObject);
    procedure bbSourceClearAllClick(Sender: TObject);
    procedure lbSourcesSelectedClick(Sender: TObject);
    procedure bbAdminAddClick(Sender: TObject);
    procedure bbAdminRemoveClick(Sender: TObject);  
    procedure bbDesignationRemoveClick(Sender: TObject);
    procedure bbAdminClearAllClick(Sender: TObject);
    procedure bbAddClick(Sender: TObject);
    procedure bbDesignationAddClick(Sender: TObject);
    procedure bbRemoveClick(Sender: TObject);
    procedure bbClearAllClick(Sender: TObject);
    procedure bbPlacesAddClick(Sender: TObject);
    procedure bbPlacesRemoveClick(Sender: TObject);
    procedure bbPlacesClearAllClick(Sender: TObject);
    procedure bbDesignationsAddAllClick(Sender: TObject);  
    procedure bbDesignationClearAllClick(Sender: TObject);
    procedure lbAdminSelectedClick(Sender: TObject);
    procedure lbSelectedClick(Sender: TObject);
    procedure lbDesignationListClick(Sender: TObject);
    procedure btnGetBoundingBoxClick(Sender: TObject);
    procedure cmbTypeChange(Sender: TObject);
    procedure cmbSourcesTypeChange(Sender: TObject);
    procedure cmbCategoriesChange(Sender: TObject);
    procedure cmbDesignationSetChange(Sender: TObject);
    procedure fndPlaceFinderChange(Sender: TObject);
    procedure fndSourcesChange(Sender: TObject);
    procedure fndTaxaBioFinderChange(Sender: TObject);
    procedure rbSourcesClick(Sender: TObject);
    procedure lbPlacesSelectedClick(Sender: TObject);
    procedure rbReportClick(Sender: TObject);
    procedure rbSelectedInfoClick(Sender: TObject);
    procedure rbInfoClick(Sender: TObject);
    procedure lbAvailableKeyPress(Sender: TObject; var Key: Char);
    procedure lbSelectedKeyPress(Sender: TObject; var Key: Char);
    procedure lbSourcesAvailableKeyPress(Sender: TObject; var Key: Char);
    procedure lbSourcesSelectedKeyPress(Sender: TObject; var Key: Char);
    procedure tvAdminAvailableKeyPress(Sender: TObject; var Key: Char);
    procedure lbAdminSelectedKeyPress(Sender: TObject; var Key: Char);
    procedure lbPlacesAvailableKeyPress(Sender: TObject; var Key: Char);
    procedure lbPlacesSelectedKeyPress(Sender: TObject; var Key: Char);
    procedure rbOverlapsClick(Sender: TObject);
    procedure bbSourceAddAllClick(Sender: TObject);
    procedure tvAdminAvailableClick(Sender: TObject);
    procedure bbAddAllClick(Sender: TObject);
    procedure bbPlacesAddAllClick(Sender: TObject);
    procedure cbAreaBoundsClick(Sender: TObject);
    procedure eNECornerExit(Sender: TObject);
    procedure eSWCornerExit(Sender: TObject);
    procedure eCornersChange(Sender: TObject);
    procedure fndTaxaBioFinderPopulateList(Sender: TObject);
    procedure fndTaxaBioFinderPopulateStop(Sender: TObject);
    procedure fndPlaceFinderPopulateStop(Sender: TObject);
    procedure fndPlaceFinderPopulateList(Sender: TObject);
    procedure fndSourcesPopulateStop(Sender: TObject);
    procedure fndSourcesPopulateList(Sender: TObject);
    procedure lbAvailableClick(Sender: TObject);
    procedure rbPlaceSelectionTypeClick(Sender: TObject);
    procedure cmbPredefinedFiltersChange(Sender: TObject);
    procedure lbDesignationAvailableKeyPress(Sender: TObject; var Key: Char);
    procedure lbDesignationSelectedKeyPress(Sender: TObject; var Key: Char);
    procedure btnSaveFilterClick(Sender: TObject);
    procedure btnFindPolygonClick(Sender: TObject);
    procedure tvAdminAvailableExpanding(Sender: TObject; Node: TFlyNode;
      var AllowExpansion: Boolean);
    procedure cbIncludeAllTxBtClick(Sender: TObject);
    procedure tvAttributesImageClick(Sender: TObject; Node: TFlyNode);
    procedure rbLayoutExistingClick(Sender: TObject);
    procedure tvSortDrawCell(Sender: TObject; aCanvas: TCanvas; ACol,
      ARow: Integer; Rect: TRect; State: TExGridDrawState);
    procedure rbLayoutNewClick(Sender: TObject);
    procedure tvPolygonLayersImageClicked(Sender: TObject; Node: TFlyNode);
    procedure TreeSort(Sender: TObject; Node1,
  Node2: TFlyNode; var Compare: Integer);
    procedure btnFindPolygonDropDownClick(Sender: TObject);
    procedure btnGetBoundingBoxDropDownClick(Sender: TObject);
    procedure chkIncludeListTxBtClick(Sender: TObject);
    procedure rbInfoAllClick(Sender: TObject);
    procedure cmbLayoutChange(Sender: TObject);
    procedure pnlOuterResize(Sender: TObject);
    procedure TaxonListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure tvSummaryDrawCell(Sender: TObject; aCanvas: TCanvas; ACol,
      ARow: Integer; Rect: TRect; State: TExGridDrawState);
  private
    FSettings: TReportWizardSettings;
    hlpWizard: TOnlineHelp;
    FUnionRequired: Boolean;
    FTabSequence  : TTabSequence;
    FFilterFields : TList;
    FWizardInProgress: Boolean;

    FdmWizard : TdmWizard; // made into a public property
    FdmTaxa   : TdmTaxonDictBrowser;
    FdmAdmin  : TdmAdminAreaDictBrowser;
    FdmBiotope: TdmBiotopeDictBrowser;

// Add a list to store the advanced filter Objects
// This is populated from the ComplexFilter dialog
// see btnAdvancedClick
    FAdvancedFilter : TList; // made into a public property
//  We need to generate a title for the report.
// This is NOT generated until the rest of the criteria have been set
// See function BuildTitle
    FTitleText         :String;
    FReportChanged     :Boolean;
    FSourcesChanged    :Boolean;
    FTaxaBioChanged    :Boolean;
    FInformationChanged:Boolean;
    FSpatialRefChanged :Boolean;
    FAttributesChanged :Boolean;
    FReportGenerated   :Boolean;
// variables for storing the entered spatial references as opposed to those seen
// on screen.  Ie you can enter a spatial reference in a different system than that
// set on the options screen.
    FValidEnteredSW : String;
    FValidEnteredNE : String;

    FMapCalled      : Boolean;
    FMapAlreadyPresent : Boolean;
    // To use with tvPolygonLayers, to remember the sheet file names
    FMapSheetFileNames: TStringList;
    FSpatialSystemForBoundingBox: String;
    procedure SetupDataModules;
    procedure FreeDataModules;
    procedure ClearAndFreeListBoxes;
    procedure ClearTaxaBiotopesCategories;
    procedure ExpandAdminNode(const AStartNode: TFlyNode);
    procedure PopulateAdminListBox(const ANode: TFlyNode);
    procedure ReportSelectorLeavePage;
    procedure SetSourcesSelectionPage;
    procedure SetAdminSelectionPage;
    procedure SetGridRefSelectionPage;
    procedure SetPolygonSelectionPage;
    procedure SetTaxaBioSelectionPage;
    procedure SetPlacesSelectionPage; 
    procedure SetDesignationsPage;
    procedure SetSelectAttributes;
    procedure SetSortAttributes;
    procedure SetSummaryScreen;
    procedure SetLayoutScreen;
    procedure SetupTabSequence;
    procedure CheckButtons(ATreeList:TWinControl; BList: TListBox;
      AddBtn, RemoveBtn, AddAllBtn, ClearAllBtn: TImageListButton);
    procedure FinishWizard;
    procedure AddTableToQuery(iTableLinkInfo:TTableLinkInfo;
      const iAvailableTables:TStringList; ioRequiredTables:TStringList);
    procedure AddToWhereClause(var ioWhere: String; const ACondition: String);
    procedure CheckSources(const iAvailableTables:TStringList;
      var ioRequiredTables:TStringList; const iSelect:String;
      var ioWhere:String);
    procedure DoAdvancedFilterTables(const iSelect:String;
      const iAvailableTables:TStringList; var ioRequiredTables:TStringList;
      const itfTaxon:Boolean);
    procedure BuildSelectedCriteria(var ioWhere:String; const iKeyLink:String; const iList:TListBox);
    // Michael Weideli  Mantis 473
    function  CheckTaxaAreExpandable(const iList:TListBox):boolean;

    procedure BuildFrom(var ioFrom:String; const iRequiredTables:TStringList);
    procedure BuildWhere(var ioWhere:String; const iRequiredTables:TStringList);
    procedure BuildAdvancedCriteria(const iSelect:String; var ioWhere:String;
      const itfTaxon:Boolean);
    procedure AddAdvancedCriteria(var ioFilters:String; const ATableName:String;
      const AFilter:TObject);
    function GetTextFilter(const AField:TReportField;
      const ACondition:TCondition; const ATableName, ACriteria:String):String;
    function GetBooleanFilter(const AField: TReportField;
      const ACondition: TCondition; const ATableName: String; ACriteria:String): String;
    function GetVagueDateFilter(const AField:TReportField;
      const ACondition:TCondition; const ATableName, ACriteria:String):String;
    procedure UpdateBoundingBox(KeyList:TKeyList);
    procedure UpdatePolygonList(KeyList: TKeyList);
    function BuildSpeciesReport:String;
    function BuildBiotopesReport:String;
    function BuildPlaceReport:String;
    procedure DoStandardAdditionalFilter;
    procedure DoComAdditionalFilter;
    function BuildTitle:String;
    procedure SetTitle(TitleString:String);
    function TaxaBioLeavePage: Boolean;
    function SourcesLeavePage: Boolean;
    function DesignationsLeavePage: Boolean;
    function PlacesLeavePage:Boolean;
    function GridRefLeavePage:Boolean;
    function SelectAttributesLeavePage: Boolean;
    function AdminLeavePage: Boolean;
    function PolygonLeavePage: Boolean;
    procedure InfoSelectLeavePage;
    procedure SetActiveControl;
    function GetNumberFilter(const AField: TReportField; const ACondition: TCondition;
      const ATableName: String; ACriteria: String): String;
    procedure ClearPlacesSelected;
    procedure SelectPolygon(ANode: TFlyNode; const ASelect: Boolean;
      AUpdateParent: Boolean = True);
    procedure ClearPolygonLayers;
    procedure ClearAdminTree;
    function ReportOccurrenceTypes: TReportOccurrenceTypes;
    procedure AssignSortToSelectedItem(Sort: TSortType);
    procedure LoadLocationsForOccurrencesReport;
    procedure LoadOccurrencesForLocationsReport;
    procedure RemoveInappropriateFilters;
    procedure MapForBoundingBoxClick(Sender: TObject);
    procedure MapForPolygonClick(Sender: TObject);
    procedure RequestMapForReturnData(const ABaseMapKey: TKeyString;
      ACallbackFunction: TRequestorUpdateFunction; AWantBoundingBox: Boolean); overload;
    procedure RequestMapForReturnData(const ABaseMapKey: TKeyString;
      ACallbackFunction: TRequestorUpdateFunction; AWantBoundingBox,
      AWantInitialBoundingBox: Boolean); overload;
    procedure AddHierarchicalAttributesToTreeView(const TreeView :
        TKeyboardRapidTree; const IncludeMemos : Boolean; const DefaultImageIndex : 
        Integer);
    function FindNode(const ATreeView: TKeyboardRapidTree; ANodeName: String;
        AStartNode: TFlyNode): TFlyNode;
    function ExtractNodeNames(AString: String): TStringList;
    procedure AddFolderNodes(const ATreeView: TKeyboardRapidTree; ANodeNames:
        TStringList);
    procedure AddLeafNodes(const ATreeView: TKeyboardRapidTree; ANodeNames: 
        TStringList; AAttribute: TAttribute; ADefaultImageIndex: Integer;
        AIncludeMemos: Boolean);
    function FindTopFolder: string;
    procedure AddMeasurementFolderNodes(ATreeView: TKeyboardRapidTree; AFlyNode: 
        TFlyNode; ANodeName: String);
    procedure AddNamedTableToQuery(const ATableName: string; AAvailableTableList,
        ARequiredTableList: TStringList);
    function GetMeasurementFolderNode(ATreeView: TKeyboardRapidTree; AAttribute:
        TAttribute): TFlyNode;
    procedure RemoveEmptyFolders(ATreeView: TKeyboardRapidTree);
    procedure SetUpImageIndex(ATreeView: TKeyboardRapidTree; AFlyNode: TFlyNode;
        ADefaultImageIndex: Integer);
    procedure FilterOnSurveys(const iAvailableTables : TStringList;
        const ioRequiredTables : TStringList; var ioWhere : String);
    procedure FilterOnDesignations(const iAvailableTables : TStringList;
        const ioRequiredTables : TStringList; var ioWhere : String);
    procedure ResizeColumns(leftColumn, midColumn, rightColumn, container : TControl);
  public
    constructor Create(AOwner:TComponent; ASettings: TReportWizardSettings); reintroduce; overload;
    destructor Destroy; override;
    function GetWizardInProgress : Boolean;
    property dmWizard: TdmWizard read FdmWizard write FdmWizard;
    property FilterFields : TList read FFilterFields write FFilterFields;
    property TitleText : String read FTitleText write SetTitle;
    property AdvancedFilter : TList read FAdvancedFilter;
    property ReportChanged: Boolean read FReportChanged write FReportChanged;
    property ReportGenerated: Boolean read FReportGenerated;
    procedure LoadWizardFile(AstWizardFile: String);
  end;

//==============================================================================
implementation

uses
  FormActions, FilterResult, Maintbar, Search, BaseChildUnit,
  ApplicationSettings, LocationDetailsData, Recorder2000_TLB, GeneralData, Map,
  ValidationData, VagueDate, xmldom, XMLIntf, msxmldom, XMLDoc, Math, Variants,
  StrUtils, SQLConstants, Treecoll, ProjectSpecificAccess;

{$R *.DFM}
{$R Overlap_Bmps.Res}

const
  // Images indexes for the tree nodes
  CHECKED_IMG_IDX      =  6;
  UNCHECKED_IMG_IDX    =  8;
  CHECKED_GREY_IMG_IDX =  9;
  SORT_UP_IMG_IDX      = 10;
  SORT_DOWN_IMG_IDX    = 11;
  BLANK_IMG_IDX        = 12;
  WORLD_IMG_IDX        = 13;

resourcestring
  ResStr_FindNames              = '&Find Ind/Org:';
  ResStr_FindDocs               = '&Find Document:';
  ResStr_SurveyType             = 'Survey &Type:';
  ResStr_FindTags               = '&Find Tags:';
  ResStr_UnlinkedPolygon        = 'Unlinked Polygon';
  ResStr_NewFilter              = 'new filter';
  ResStr_Location               = 'Location';
  ResStr_GridReferences         = 'Grid References';
  ResStr_BoundingBox            = 'Bounding Box';
  ResStr_NorthEastCorner        = 'North-east corner';
  ResStr_SouthWestCorner        = 'South-west corner';
  ResStr_Over1000namesSelected  = 'Over 1000 names selected';
  ResStr_AllIn                  = 'All in';
  ResStr_All                    = 'All';
  ResStr_Attributes             = 'Attributes';
  ResStr_SortOrder              = 'Sort Order';
  ResStr_PolygonSelectionActive = 'Polygon Selection Active';
  ResStr_Sources                = 'Sources';
  ResStr_AdministrativeAreas    = 'Administrative Areas';
  ResStr_SaveFilterToFile       = 'Save Filter To File';
  ResStr_FilterName             = 'Filter Name';
  ResStr_StandardLayout         = 'Standard layout';

  //resourcestring to support BuildTitle
  ResStr_Local_ReportAbout = 'Report About ';
  ResStr_Local_Selected = 'selected ';
  ResStr_Local_Species = 'Species ';
  ResStr_Local_All = 'all ';
  ResStr_Local_Biotopes = 'Biotopes ';
  ResStr_Local_Places = 'Places ';
  ResStr_Local_WithAll = 'with all ';
  ResStr_Local_WithSelected = 'with selected ';
  ResStr_Local_SpeciesAndBiotopes = 'Species and Biotopes ';
  ResStr_Local_RefecencedBy = 'referenced by ';
  ResStr_Local_References = 'References ';
  ResStr_Local_Within = 'within ';
  ResStr_Local_Surveys = 'Surveys ';
  ResStr_Local_RunBy = 'run by ';
  ResStr_Local_IndOrg = 'Individuals / Organisations ';
  ResStr_Local_SelectedAdminArea = 'in selected Admin Areas ';
  ResStr_Local_AllAdminArea = 'in all Admin Areas';
  ResStr_Selected = '&Selected (%d):';

  //Status
  ResStr_ObtainingSampleForPoly = 'Obtaining samples for polygon: %s';

var
  MapWindow: TfrmMap;

//==============================================================================
//    Procedure Name: TdlgWizard.Create
//           Purpose: To create a new wizard object.
//      Side Effects: Initialises the FUnionRequired property.
//     Special Notes: Actually most initialisation is done in the FormCreate.
//------------------------------------------------------------------------------
constructor TdlgWizard.Create(AOwner:TComponent; ASettings: TReportWizardSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
  FUnionRequired     :=False;
  FReportChanged     :=True;
  FReportGenerated   :=False;
  FSourcesChanged    :=True;
  FTaxaBioChanged    :=True;
  FInformationChanged:=True;
  FSpatialRefChanged :=True;
  FAttributesChanged :=True;
  rbOverlapsClick(nil);
  // set default value according to previous use of the expand taxa checkbox
  cbExpandTaxa.Checked := AppSettings.ExpandTaxaInWizard;
  FMapSheetFileNames := TStringList.Create;
  tvPolygonLayers.StateImages := ilWizard;

  AppSettings.UpdateMapMenu(Self, pmMapForBoundingBox.Items, True, MapForBoundingBoxClick);
  AppSettings.UpdateMapMenu(Self, pmMapForPolygons.Items, True, MapForPolygonClick);
  RefreshXPMenu;
  
  FdmWizard.PopulateTaxonDesignationSets(cmbDesignationSet);

  pnlOuterResize(Self);
end;  // Create

//==============================================================================
//    Procedure Name: TdlgWizard.FormCreate
//           Purpose: To initialise the internal lists used by the wizard and
//                    the data Objects used.
//      Dependencies: Indirectly calls the dictionary units.
//      Side Effects: Populates dictionary Objects. See SetupDataModules and
//                    SetupDictCtrlObjects.
//------------------------------------------------------------------------------
procedure TdlgWizard.FormCreate(Sender: TObject);
var iCount:Integer;
begin
  inherited;
  SetupDataModules;
  FTabSequence      := TTabSequence.Create;
  FFilterFields     := TList.Create;
  FAdvancedFilter   := TList.Create;
  FWizardInProgress := True;

  for iCount := 0 to pcWizard.PageCount - 1 do
    pcWizard.Pages[iCount].TabVisible := False;

  pcWizard.ActivePage:=pcWizard.Pages[0];
  bbNext.Enabled := True;
  bbBack.Enabled := False;
  CheckButtons(lbSourcesAvailable, lbSourcesSelected, bbSourceAdd, bbSourceRemove, bbSourceAddAll, bbSourceClearAll);
  CheckButtons(tvAdminAvailable, lbAdminSelected, bbAdminAdd, bbAdminRemove, nil, bbAdminClearAll);
  CheckButtons(lbAvailable, lbSelected, bbAdd, bbRemove, bbAddAll, bbClearAll);
  CheckButtons(lbPlacesAvailable, lbPlacesSelected, bbPlacesAdd, bbPlacesRemove, bbPlacesAddAll, bbPlacesClearAll);
  //Set database and session for finder components
  fndTaxaBioFinder.SetDatabase(dmDatabase.LocalDatabase);
  fndPlaceFinder.SetDatabase(dmDatabase.LocalDatabase);
  fndSources.SetDatabase(dmDatabase.LocalDatabase);
  //Help Setup
  hlpWizard  := TOnlineHelp.Create(Self.Handle);
  OnHelp     := hlpWizard.OnHelpReplacement;
  HelpContext:= IDH_WIZARDSTYLE;
  tsReportSelector.HelpContext  := IDH_WIZARDSTYLE;
  tsSourcesSelector.HelpContext := IDH_WIZARDSSELECTOR;
  tsSourcesSelection.HelpContext:= IDH_WIZARDSSELECTION;
  tsAdminSelection.HelpContext  := IDH_WIZARDAA;
  tsPlacesSelector.HelpContext  := IDH_WIZARDPLACESSELECTOR;
  tsPlacesSelection.HelpContext := IDH_WIZARDPLACESSELECTION;
  tsGridRefSelection.HelpContext:= IDH_WIZARDSPATIAL;
  tsInfoSelector.HelpContext    := IDH_WIZARDINFOSELECTOR;
  tsInfoSelection.HelpContext   := IDH_WIZARDINFOSELECTION;
  tsLayout.HelpContext          := IDH_WIZARDLAYOUT;
  tsSelectAttributes.HelpContext:= IDH_WIZARDSELECTATTRIB;
  tsSortAttributes.HelpContext  := IDH_WIZARDSORTATTRIB;
  tsSummary.HelpContext         := IDH_WIZARDSUMMARY;
  tsPolygonSelection.HelpContext:= IDH_SELECTBYPOLYGON;
end;  // FormCreate

//==============================================================================
//    Procedure Name: TdlgWizard.FormClose
//           Purpose: To tidy up the wizard Objects and close the form.
//------------------------------------------------------------------------------
procedure TdlgWizard.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if FMapCalled then begin
    ModalResult:=mrIgnore;
    FMapCalled:=False;
  end;

  if ModalResult<>mrIgnore then
    with TfrmFilterResult(Owner) do begin
      Enabled:=True;
      WindowState:=wsNormal;
    end;
  Action:=caHide;
  FWizardInProgress := False;
end;  // FormClose

//==============================================================================
//    Procedure Name: TdlgWizard.Destroy
//           Purpose: Clean up memory and remove the wizard object.
//      Side Effects: Clears all Objects in the list properties FFilterFields,
//                    FTabSequence, and FAdvancedFilter
//------------------------------------------------------------------------------
destructor TdlgWizard.Destroy;
var i:Integer;
begin
  hlpWizard.Free;
  hlpWizard := nil;

  if Assigned(FFilterFields) then
    with FFilterFields do begin
      for i:=0 to Count-1 do begin
        TReportField(Items[i]).Free;
        Items[i]:=nil;
      end;
      Free;
    end;
  // Clear FTabSequence. Do not use Free on the Objects, they are pointers
  // to the PageControl tab sheets!!!! And they contain many controls too.
  // Freeing them would get rid of the controls that the wizard is responsible for.
  if Assigned(FTabSequence) then
  begin
    FTabSequence.Clear;
    FTabSequence.Free;
    FTabSequence := nil;
  end;
  if Assigned(FAdvancedFilter) then
    with FAdvancedFilter do begin
      for i:=0 to Count-1 do begin
        TFilter(Items[i]).Free;
        Items[i]:=nil;
      end;
      Clear;
      Free;
    end;
  FAdvancedFilter:=nil;

  // Free the rest of the Objects
  ClearAndFreeListBoxes;
  ClearPolygonLayers;
  FMapSheetFileNames.Free;
  FMapSheetFileNames := nil;
  FreeDataModules;
  inherited Destroy;
end;  // Destroy

//==============================================================================
//    Procedure Name: TdlgWizard.SetupDataModules
//           Purpose: Prepare data module result sets ready for later use
//      Dependencies: Uses WizardData, TaxonDictionData, BiotopeDictionData,
//                    AdminAreaDictData units.
//------------------------------------------------------------------------------
procedure TdlgWizard.SetupDataModules;
begin
  FdmWizard       := TdmWizard.Create(Self);
  FdmTaxa         := TdmTaxonDictBrowser.Create(nil);
  FdmTaxa.List    := cmbCategories;
  FdmBiotope      := TdmBiotopeDictBrowser.Create(nil);
  FdmBiotope.List := cmbCategories;
  FdmAdmin        := TdmAdminAreaDictBrowser.Create(nil);
  FdmAdmin.List   := cmbType;
  FdmAdmin.Tree   := tvAdminAvailable;
end;  // SetupDataModules

//==============================================================================
//    Procedure Name: TdlgWizard.FreeDataModules
//           Purpose: Clear memory used internally by the wizard
//------------------------------------------------------------------------------
procedure TdlgWizard.FreeDataModules;
begin
  FdmWizard.Free;
  FdmWizard:=nil;
  FdmTaxa.Free;
  FdmTaxa:=nil;
  FdmAdmin.Free;
  FdmAdmin:=nil;
  FdmBiotope.Free;
  FdmBiotope:=nil;
end;  // FreeDataModules

//==============================================================================
//    Procedure Name: TdlgWizard.ClearAndFreeListBoxes
//           Purpose: To make sure that all Objects associated with all lists
//                    used in the wizard are cleared and will not leak memory.
//     Special Notes: Note that it would be better practice for all the
//                    individual lists to clear themselves on destruction.
//                    However, since the lists do not receive a destroy
//                    message, we will do the lot here for safety.
//------------------------------------------------------------------------------
procedure TdlgWizard.ClearAndFreeListBoxes;
var lCount: Integer;
begin
  with cmbLayout.Items do begin
    for lCount := 0 to Count - 1 do
      Objects[lCount].Free;
    Clear;
  end;

  with cmbType.Items do begin
    for lCount:= 0 to Count - 1 do
      if Objects[lCount] <> nil then TKeyData(Objects[lCount]).Free;
    Clear;
  end;
  // lbSourcesAvailable - Due to double usage of this list, deal with the
  // destruction of the Objects here, and whatever the content, clear the items
  with lbSourcesAvailable.Items do begin
    for lCount := 0 to Count -1 do
      if (Objects[lCount] is TSurveyNode) then begin
        TSurveyNode(Objects[lCount]).Free;
        Objects[lCount]:=nil;
      end;
    Clear;
  end;
  fndSources.ClearSourceList;
  // lbSourcesSelected
  with lbSourcesSelected.Items do begin
    for lCount := 0 to Count -1 do begin
      if Objects[lCount] is TSurveyNode then
        TWizardKeyObject(Objects[lCount]).Free
      else
        TKeyData(Objects[lCount]).Free;
      Objects[lCount]:=nil;
    end;
    Clear;
  end;
  CheckButtons(lbSourcesAvailable, lbSourcesSelected, bbSourceAdd, bbSourceRemove, bbSourceAddAll, bbSourceClearAll);
  ClearAdminTree;
  // lbAdminSelected
  with lbAdminSelected.Items do begin
    for lCount := 0 to Count -1 do begin
      TWizardKeyObject(Objects[lCount]).Free;
      Objects[lCount]:=nil;
    end;
    Clear;
  end;
  CheckButtons(tvAdminAvailable, lbAdminSelected, bbAdminAdd, bbAdminRemove, nil, bbAdminClearAll);
  // cmbCategories
  ClearTaxaBiotopesCategories;
  // lbAvailable
  lbAvailable.Clear;
  fndTaxaBioFinder.ClearSourceList;
  // lbSelected
  with lbSelected.Items do begin
    for lCount := 0 to Count -1 do begin
      TReportField(Objects[lCount]).Free;
      Objects[lCount]:=nil;
    end;
    Clear;
  end;
  lblSelected.Caption  := Format(ResStr_Selected, [lbSelected.Items.Count]);
  CheckButtons(lbAvailable, lbSelected, bbAdd, bbRemove, bbAddAll, bbClearAll);
  // lbPlacesAvailable
  lbPlacesAvailable.Clear;
  fndPlaceFinder.ClearSourceList;
  // lbPlacesSelected
  ClearPlacesSelected;
  CheckButtons(lbPlacesAvailable, lbPlacesSelected, bbPLacesAdd, bbPlacesRemove, bbPlacesAddAll, bbPlacesClearAll);
  tvAttributes.Items.Clear;
  // lbSortAttributes. Do not free the objects here, just reset some properties!!!
  if Assigned(FdmWizard) then
    if Assigned(FdmWizard.ReportGenerator) then
      with FdmWizard.ReportGenerator do begin
        if Assigned(Attributes) then
        begin
          for lCount:=0 to Attributes.Count-1 do
            TAttribute(Attributes.Objects[lCount]).Sort:=stNone;
          tvSort.Items.Clear;
          Tag:=0;
        end;
      end;
end;  // ClearAndFreeListBoxes

//==============================================================================
// Called from various places
procedure TdlgWizard.ClearTaxaBiotopesCategories;
var lCount: Integer;
begin
  if FTaxaBioChanged then
    with cmbCategories.Items do begin
      for lCount := 0 to Count-1 do
        TKeyData(Objects[lCount]).Free;
      Clear;
    end;
end;  // ClearTaxaBiotopesCategories;

//==============================================================================
{ Free all items on the admin areas tree, and clear the tree }
procedure TdlgWizard.ClearAdminTree;
var
  lCount : Integer;
begin
  // tvAdminAvailable
  with tvAdminAvailable do begin
    for lCount := 0 to Items.Count - 1 do
      TNodeObject(Items[lCount].Data).Free;
    Items.Clear;
  end;
end;

//==============================================================================
{ Clears and frees objects on lbPlacesSelected }
procedure TdlgWizard.ClearPlacesSelected;
var
  lCount : Integer;
begin
  with lbPlacesSelected.Items do begin
    for lCount := 0 to Count -1 do begin
      TWizardKeyObject(Objects[lCount]).Free;
      Objects[lCount]:=nil;
    end;
    Clear;
  end;
end;

//==============================================================================
procedure TdlgWizard.ClearPolygonLayers;
var loNode: TFlyNode;
begin
  loNode := tvPolygonLayers.Items.GetFirstNode;
  while loNode <> nil do begin
    TKeyData(loNode.Data).Free;
    loNode := loNode.GetNextSibling;
  end;
  tvPolygonLayers.Items.Clear;
  if Assigned(FMapSheetFileNames) then
    FMapSheetFileNames.Clear;
end;

//==============================================================================
//    Procedure Name: TdlgWizard.bbCancelClick
//           Purpose: To set the result of running the wizard.
//      Side Effects: Sets the modal result and therefore closes the dialog.
//     Special Notes: Most of the object tidy-up is done in FormClose.
//------------------------------------------------------------------------------
procedure TdlgWizard.bbCancelClick(Sender: TObject);
begin
  inherited;
  // stop further population of search lists
  dmSearch.Terminate;
  ModalResult:=mrCancel;
  Close;
end;  // bbCancelClick

//==============================================================================
//    Procedure Name: TdlgWizard.bbNextClick
//           Purpose: To ensure the correct next page is shown an to
//                    initialise the pages depending on previous choices.
//      Dependencies: FTabSequence should already exist.
//      Side Effects: Adjust FTabSequence as a consequence of user selection.
//     Special Notes: Calls several other functions to populate lists and
//                    combo boxes.This routine should be handled with care as
//                    it is the crux of the User navigation through the wizard
//------------------------------------------------------------------------------
procedure TdlgWizard.bbNextClick(Sender: TObject);
var
  lCursor:TCursor;
  lBoundingBox : TValidBoundingBox;
begin
  dmSearch.Terminate;
  with pcWizard do
  begin
    if ActivePage = tsReportSelector then
    begin //First tab, set up as many tabs as we can...
      lCursor:=HourGlassCursor;
      try
        ReportSelectorLeavePage;
        SetupTabSequence;
        if cbRestrictSource.Checked then
        begin //Add both Source selection tabs to sequence.
          FTabSequence.AddToSequence(tsSourcesSelector);
          FTabSequence.AddToSequence(tsSourcesSelection);
        end;
        if cbRestrictArea.Checked then
        begin //Add the admin area selection tab.
          FTabSequence.AddToSequence(tsAdminSelection);
        end;
        if cbRestrictBounds.Checked then
        begin //Add the bounding box/grid ref selection tab.
          FTabSequence.AddToSequence(tsGridRefSelection);
        end;
        if cbRestrictPolygons.Checked then
        begin //Add the polygon selection tab.
          FTabSequence.AddToSequence(tsPolygonSelection);
        end;
        if rbPlaceReport.Checked then
        begin //Add the places selector
          FTabSequence.AddToSequence(tsPlacesSelector);
          FTabSequence.AddToSequence(tsInfoSelector);
        end;
        if rbSpeciesReport.Checked or rbBiotopesReport.Checked then
        begin //Add the Taxa/Bio tab
          FTabSequence.AddToSequence(tsTaxaBioSelection);
        end;
        if cbRestrictDesignations.Checked then
        begin
          FTabSequence.AddToSequence(tsDesignations);
        end;
        //Add all remaining tabs, in order.
        FTabSequence.AddToSequence(tsLayout);
        FTabSequence.AddToSequence(tsSelectAttributes);
        FTabSequence.AddToSequence(tsSortAttributes);
        FTabSequence.AddToSequence(tsSummary);
      finally
        DefaultCursor(lCursor);
      end;
    end else if FTabSequence.CurrentTab = tsSourcesSelection then
    begin
      if not SourcesLeavePage then begin
        MessageDlg(ResStr_SelectSource, mtInformation,[ mbOK],0);
        Exit;  // Make sure there is some selection before allowing to continue
      end;
    end else if FTabSequence.CurrentTab = tsDesignations then
    begin
      if not DesignationsLeavePage then begin
        MessageDlg(ResStr_SelectDesignation, mtInformation,[ mbOK],0);
        Exit;  // Make sure there is some selection before allowing to continue
      end;
    end else if FTabSequence.CurrentTab = tsAdminSelection then
    begin
      if not AdminLeavePage then begin
        MessageDlg(ResStr_SelectAdminArea, mtInformation,[ mbOK],0);
        Exit;  // Make sure there is some selection before allowing to continue
      end;
    end else if FTabSequence.CurrentTab = tsTaxaBioSelection then
    begin
      if not TaxaBioLeavePage then begin
        MessageDlg(ResStr_SelectItem, mtInformation,[ mbOK],0);
        Exit;  // Make sure there is some selection before allowing to continue
      end;
      // check and warn on Taxa which can't be expanded
      // Michael Weideli  Mantis 473
      if not CheckTaxaAreExpandable(lbSelected) then
        Exit;
    end else if FTabSequence.CurrentTab = tsPlacesSelector then
    begin
      if rbByPlaceName.Checked then begin
        FTabSequence.RemoveTab(tsGridRefSelection);
        FTabSequence.InsertIntoSequence(FTabSequence.Sequence + 1, tsPlacesSelection);
      end;
      if rbByGridRef.Checked then begin
        FTabSequence.RemoveTab(tsPlacesSelection);
        FTabSequence.InsertIntoSequence(FTabSequence.Sequence + 1, tsGridRefSelection);
      end;
    end  else if FTabSequence.CurrentTab = tsGridRefSelection then
    begin
      if (FValidEnteredSW<>'') and (FValidEnteredNE <> '') then begin
        lBoundingBox:=CheckBoundingBox(FValidEnteredSW, FValidEnteredNE, DetermineSpatialRefSystem(FValidEnteredSW));
        if lBoundingBox.Valid then begin
          if not cbRestrictBounds.Checked then
            if not GridRefLeavePage then
              Exit; // Do not move on unless there is a valid grid ref
        end else
          raise EWizard.CreateValidation(lBoundingBox.Error, eSWCorner);
      end else
        if FValidEnteredNE = '' then
          raise EWizard.CreateValidation(ResStr_EnterBBCoord, eNECorner)
        else
          raise EWizard.CreateValidation(ResStr_EnterBBCoord, eSWCorner);
    end else if FTabSequence.CurrentTab = tsInfoSelection then
    begin
      if rbInfoSelect.Checked then
      begin
        FTabSequence.RemoveTab(tsTaxaBioSelection);
        FTabSequence.InsertIntoSequence(FTabSequence.Sequence + 1, tsTaxaBioSelection);
      end else
        FTabSequence.RemoveTab(tsTaxaBioSelection);
    end else if FTabSequence.CurrentTab = tsInfoSelector then
    begin
      InfoSelectLeavePage;
      if rbSpeciesInfo.Checked or rbBiotopeInfo.Checked then
      begin
        FTabSequence.RemoveTab(tsInfoSelection);
        FTabSequence.InsertIntoSequence(FTabSequence.Sequence + 1, tsInfoSelection);
      end else begin
        FTabSequence.RemoveTab(tsInfoSelection);
        FTabSequence.RemoveTab(tsTaxaBioSelection);
      end;
    end else if FTabSequence.CurrentTab = tsPlacesSelection then
    begin
      if not PlacesLeavePage then begin
        MessageDlg(ResStr_SelectLocation, mtInformation,[ mbOK],0);
        Exit;  // Make sure there is some selection before allowing to continue
      end;
    end else if FTabSequence.CurrentTab = tsPolygonSelection then
    begin
      if not PolygonLeavePage then begin
        MessageDlg(ResStr_SelectPolygon, mtInformation, [mbOk], 0);
        Exit;  // Make sure a polygon is selected before allowing to continue
      end;
    end else if FTabSequence.CurrentTab = tsLayout then
    begin
      if rbLayoutExisting.Checked and (cmbLayout.ItemIndex = -1) then
      begin
        MessageDlg(ResStr_SelectFile, mtInformation, [mbOk], 0);
        ActiveControl := cmbLayout;
        Exit;
      end;
    end else if FTabSequence.CurrentTab = tsSelectAttributes then
    begin
      if not SelectAttributesLeavePage then
        Exit;  // Make sure there is some selection before allowing to continue
    end;

    if (FTabSequence.CurrentTab = tsLayout) then
    begin
      if rbLayoutExisting.Checked and (cmbLayout.ItemIndex <> -1) then
      begin
        if not FSettings.StandardReport then
        begin
          SetSelectAttributes;
          SetSortAttributes; //in case the user moves back to these places
        end;
        FTabSequence.JumpTo(tsSummary);
      end else begin
        if FSettings.WizardFile <> '' then dmWizard.ReportGenerator.Clear;
        FSettings.WizardFile := '';
        FTabSequence.MoveOn;
      end;
    end else
      //Move the sequence on to the next tab.
      FTabSequence.MoveOn;
    //Check the sequence is still valid (ie. we haven't reached the end).
    if FTabSequence.Sequence >= FTabSequence.Count then
    begin
      FTabSequence.JumpTo(tsSummary);
      FinishWizard;
    end else
    begin //Check the identity of the next tab and do any pre-processing.
      if FTabSequence.CurrentTab = tsSourcesSelection then
        SetSourcesSelectionPage;
      if FTabSequence.CurrentTab = tsAdminSelection then
        SetAdminSelectionPage;
      if FTabSequence.CurrentTab = tsGridRefSelection then
        SetGridRefSelectionPage;
      if FTabSequence.CurrentTab = tsPolygonSelection then
        SetPolygonSelectionPage;
      if FTabSequence.CurrentTab = tsTaxaBioSelection then
        SetTaxaBioSelectionPage;
      if FTabSequence.CurrentTab = tsPlacesSelection then
        SetPlacesSelectionPage;
      if FTabSequence.CurrentTab = tsSelectAttributes then
        SetSelectAttributes;
      if FTabSequence.CurrentTab = tsSortAttributes then
        SetSortAttributes;
      if FTabSequence.CurrentTab = tsSummary then
        SetSummaryScreen;
      if FTabSequence.CurrentTab = tsLayout then
        SetLayoutScreen;
      if FTabSequence.CurrentTab = tsDesignations then
        SetDesignationsPage;

      //Do the activepage assignment.
      ActivePage := FTabSequence.CurrentTab;
      SetActiveControl;
    end;
    bbBack.Enabled := ActivePage <> tsReportSelector;
  end; //with pcWizard
  // Help Setup
  HelpContext := pcWizard.ActivePage.HelpContext;
end; // pbNextClick

//==============================================================================
//    Procedure Name: TdlgWizard.bbBackClick
//           Purpose: To move the user "back" to the correct page in the Tab
//                    sequence.
//      Dependencies: FTabSequence should already be populated.
//------------------------------------------------------------------------------
procedure TdlgWizard.bbBackClick(Sender: TObject);
begin
  dmSearch.Terminate;
  with pcWizard do
  begin
    // In case we came back from FilterResult screen, and the query is still open
    if FdmWizard.qryResults.Active then FdmWizard.qryResults.Close;
    if (ActivePage = tsSummary) and FSettings.StandardReport then
      FTabSequence.JumpTo(tsLayout) //don't want to edit the standard reports
    else
      FTabSequence.MoveBack;
    ActivePage := FTabSequence.CurrentTab;

    bbBack.Enabled := ActivePage <> tsReportSelector;
  end;

  //Help Setup
  HelpContext := pcWizard.ActivePage.HelpContext;
end;  // bbBackClick

//==============================================================================
//    Procedure Name: TdlgWizard.rbReportClick
//           Purpose: Sets the FReportChanged flag. Used to determine if a new
//                    report is requested, or if the user just went back from
//                    an ongoing one.
//     Special Notes: Both flags are set here as there could be pages to keep
//                    the state of between the report selector and the Taxa
//                    Biotope selection page.
//------------------------------------------------------------------------------
procedure TdlgWizard.rbReportClick(Sender: TObject);
begin
  inherited;
  FReportChanged     :=True;
  FTaxaBioChanged    :=True;
  FInformationChanged:=True;
  cbRestrictSource.Checked  :=False;
  cbRestrictArea.Checked    :=False;
  cbRestrictBounds.Checked  :=False;
  cbRestrictPolygons.Checked:=False;
  cbRestrictDesignations.Checked:=False;
  { Disble polygon or bounding box selection for place report }
  cbRestrictBounds.Enabled:=Sender<>rbPlaceReport;
  cbRestrictPolygons.Enabled := Sender<>rbPlaceReport;
  cbRestrictDesignations.Enabled:=Sender<>rbBiotopesReport;
end;  // rbReportClick

//==============================================================================
procedure TdlgWizard.cbAreaBoundsClick(Sender: TObject);
begin
  inherited;
  // if Taxon/Biotope report selected, don't allow the three checkboxes to be
  // checked at the same time, only one allowed.
  if not rbPlaceReport.Checked then begin
    if (Sender=cbRestrictArea) and cbRestrictArea.Checked then begin
      cbRestrictBounds.Checked := False;
      cbRestrictPolygons.Checked := False;
    end else if (Sender=cbRestrictBounds) and cbRestrictBounds.Checked then begin
      cbRestrictArea.Checked := False;
      cbRestrictPolygons.Checked := False;
    end else if (Sender=cbRestrictPolygons) and cbRestrictPolygons.Checked then begin
      cbRestrictArea.Checked := False;
      cbRestrictBounds.Checked := False;
    end;
  end;
end;  // cbAreaBoundsClick

//==============================================================================
//    Procedure Name: TdlgWizard.ReportSelectorLeavePage
//           Purpose: To prepare the "next" page of the wizard according to the
//                    user's primary selection of Taxon, Biotope or Place
//      Side Effects: Also initialises the main (Advanced filter) constraints
//                    which should be applied to all queries unless overriden.
//------------------------------------------------------------------------------
procedure TdlgWizard.ReportSelectorLeavePage;
var lCount  :Integer;
    lstSQL  :String;
    FieldObj:TReportField;
    lCursor :TCursor;
begin
  if FReportChanged then begin
    lCursor:=HourglassCursor;
    try
      // Clear ALL lists as we may have returned from a later page and we
      // do not want dross lying about.
      ClearAndFreeListBoxes;
      dmWizard.ReportGenerator.Clear;
      FFilterFields.Clear;
      with FdmWizard do begin
        qryFieldFinder.SQL.Clear;
        // We need to select all Usable Fields
        qryFieldFinder.SQL.Add('SELECT UF.* FROM Usable_Field UF WHERE UF.Apply_To IN (');
        // For each type of report we should create an "in" clause
        // based upon the tables available
        if rbSpeciesReport.Checked then
        begin
          qryFieldFinder.SQL.Add('''A'',''T'') AND UF.Table_Name IN (');
          for lCount:= 0 to TaxonTables.Count - 1 do
            qryFieldFinder.SQL.Add(' ''' + TTableLinkInfo(TaxonTables.Objects[lCount]).TableName + ''', ');
        end else if rbBiotopesReport.Checked then
        begin
          qryFieldFinder.SQL.Add('''A'',''B'') AND UF.Table_Name IN (');
          for lCount:= 0 to BiotopeTables.Count - 1 do
            qryFieldFinder.SQL.Add(' ''' + TTableLinkInfo(BiotopeTables.Objects[lCount]).TableName + ''', ');
        end else if rbPlaceReport.Checked then
        begin
          exit;
          //Postpone until InfoSelect page.
        end else
          raise EInvalid_Selection.Create(ResStr_SelectReport);
        // then close the "in" clause
        // first strip trailing ", "
        lstSQL := qryFieldFinder.SQL.Strings[ qryFieldFinder.SQL.Count -1];
        lstSQL := Copy(lstSQL, 0, Length(lstSQL) -2);
        qryFieldFinder.SQL.Strings[ qryFieldFinder.SQL.Count -1] := lstSQL + ')';
        // then close the braces

        // and run it
        with qryFieldFinder do begin
          Open;
          while not Eof do begin
            FieldObj := TReportField.Create;
            FieldObj.ItemKey     := FieldByName('Usable_Field_Key').AsString;
            FieldObj.TableName   := UpperCase(FieldByName('Table_Name').AsString);
            FieldObj.SQL         := FieldByName('Calculation_SQL').AsString;
            FieldObj.FieldName   := FieldByName('Field_Name').AsString;
            FieldObj.DisplayName := FieldByName('Field_Description').AsString;
            FieldObj.Selectable  := FieldByName('Selectable').AsBoolean;
            FieldObj.Filter      := FieldByName('Filterable').AsBoolean;
            FieldObj.ApplyTo     := FieldByName('Apply_To').Text[1];
            FieldObj.FieldType   := FieldByName('Field_Type').AsString;
            FieldObj.Union       := uAlways; // by default
            FFilterFields.Add(FieldObj);
            Next;
          end;  // for...
          Close;
        end;  // with qryFieldFinder
        // Now let's set up overall constraints
        // (These can be altered in additional filters Screen)
        RemoveInappropriateFilters;
      end;  // with FdmWizard
    finally
      DefaultCursor(lCursor);
    end;
    FReportChanged:=False;
  end;
end;  // ReportSelectorLeavePage

//==============================================================================
//    Procedure Name: TdlgWizard.SetSourcesSelectionPage
//           Purpose: Prepares the finder boxe for use depending on requested
//                    report.
//      Dependencies: Uses Search and DictionaryControl units
//      Side Effects: lbSourcesAvailable and lbSourcesSelected are cleared
//------------------------------------------------------------------------------
procedure TdlgWizard.SetSourcesSelectionPage;
var stCaption:String;
  //----------------------------------------------------------------------------
  //    Procedure Name: ClearListBoxes
  //           Purpose: Clear Items and Objects if necessary of lbSourcesAvailable
  //                    and lbSourcesSelected
  //----------------------------------------------------------------------------
  procedure ClearListboxes;
  var lIdx: Integer;
  begin
    // Do not free Objects from Finder. Finder takes care of that itself!!
    // But due to double use of the list, free the SurveyNode Objects.
    with lbSourcesAvailable.Items do begin
      for lIdx := 0 to Count - 1 do
        if Assigned(Objects[lIdx]) and (Objects[lIdx] is TSurveyNode) then begin
          Objects[lIdx].Free;
          Objects[lIdx]:=nil;
        end;
      Clear;
    end;
    // Everything in the selected list can be freed
    with lbSourcesSelected.Items do begin
      for lIdx := 0 to Count - 1 do
        if Assigned(Objects[lIdx]) then begin
          Objects[lIdx].Free;
          Objects[lIdx]:=nil;
        end;
      Clear;
    end;
  end;  // ClearListBoxes
  //----------------------------------------------------------------------------
begin
  lbSourcesAvailable.Enabled := True; // in case previously used this tab
  // Proceed with repopulating only if required
  if FSourcesChanged then begin
    ClearListBoxes;
    // Show or hide controls according to selection on previous tab "tsSourcesSelector"
    cmbSourcesType.Visible := rbSourcesSurveys.Checked;
    fndSources.Visible     := not cmbSourcesType.Visible;
    if rbSourcesReferences.Checked then begin
      fndSources.HandleDuplicate := sfReference;
      stCaption := ResStr_FindDocs;
      fndSources.Text := '';
    end else
    if rbSourcesIndividuals.Checked then begin
      fndSources.HandleDuplicate := sfName;
      stCaption := ResStr_FindNames;
      fndSources.Text := '';
    end else
    if rbSourcesSurveys.Checked then begin
      fndSources.HandleDuplicate := sfNone;
      stCaption := ResStr_SurveyType;
      FdmWizard.PopulateSurveyTypes(cmbSourcesType);
    end else
    if rbSourcesTags.Checked then begin
      fndSources.HandleDuplicate := sfNone;
      stCaption := ResStr_FindTags;
      fndSources.Text := '';
    end;
    lblSourcesType.Caption := stCaption;
    FSourcesChanged := False;
  end;
  if rbSourcesSurveys.Checked then
    cmbSourcesTypeChange(Self);
  CheckButtons(
      lbSourcesAvailable, lbSourcesSelected,
      bbSourceAdd, bbSourceRemove,
      bbSourceAddAll, bbSourceClearAll);
end;  // SetSourcesSelectionPage

//==============================================================================
//    Procedure Name: TdlgWizard.SetDesignationsPage
//           Purpose: Prepares the designations page for designation selection.
//      Dependencies: DictionaryControl unit
//      Side Effects: lbDesignationsAvailable and lbDesignationsSelected are cleared
//------------------------------------------------------------------------------
procedure TdlgWizard.SetDesignationsPage;
begin
  cmbDesignationSetChange(Self);
  CheckButtons(
      lbDesignationAvailable, lbDesignationSelected,
      bbDesignationAdd, bbDesignationRemove,
      bbDesignationsAddAll, bbDesignationClearAll);
end;  // SetDesignationsPage

//==============================================================================
//     Function Name: SourcesLeavePage
//           Purpose: Forces the user to make some selection before continuing
//      Return Value: Boolean - True if there is some selection
//------------------------------------------------------------------------------
function TdlgWizard.SourcesLeavePage:Boolean;
begin
  Result:=lbSourcesSelected.Items.Count>0;
end;  // SourcesLeavePage

//==============================================================================
//     Function Name: DesignationsLeavePage
//           Purpose: Forces the user to make some selection before continuing
//      Return Value: Boolean - True if there is some selection
//------------------------------------------------------------------------------
function TdlgWizard.DesignationsLeavePage:Boolean;
begin
  Result:=lbDesignationSelected.Items.Count>0;
end;  // DesignationsLeavePage

//==============================================================================
//    Procedure Name: TdlgWizard.SetAdminSelectionPage
//           Purpose: To prepare the finder with a list of Admin Areas
//      Dependencies: Uses funciton from DictionaryControl unit
//------------------------------------------------------------------------------
procedure TdlgWizard.SetAdminSelectionPage;
var liIdx: Integer;
begin
  with cmbType.Items do begin
    for liIdx := 0 to Count - 1 do
      if Objects[liIdx] <> nil then
        TKeyData(Objects[liIdx]).Free;
    Clear;
  end;
  FdmAdmin.PopulateCheckList;
  CheckButtons(tvAdminAvailable, lbAdminSelected, bbAdminAdd, bbAdminRemove, nil, bbAdminClearAll);
  if cmbType.ItemIndex = -1 then cmbType.ItemIndex := 0;
  cmbTypeChange(nil);
end;  // SetAdminSelectionPage

//==============================================================================
//    Procedure Name: TdlgWizard.SetDesignationSelectionPage
//           Purpose: To prepare the finder with a list of Admin Areas
//      Dependencies: Uses funciton from DictionaryControl unit
//------------------------------------------------------------------------------
{procedure TdlgWizard.SetDesignationSelectionPage;
var liIdx: Integer;
begin
  with cmbType.Items do begin
    for liIdx := 0 to Count - 1 do
      if Objects[liIdx] <> nil then
        TKeyData(Objects[liIdx]).Free;
    Clear;
  end;
  //FdmAdmin.PopulateCheckList;
  CheckButtons(lbDesignationAvailable, lbDesignationSelected, bbDesignationAdd,
      bbDesignationRemove, bbDesignationsAddAll, bbDesignationClearAll);
  if cmbType.ItemIndex = -1 then cmbType.ItemIndex := 0;
  cmbTypeChange(nil);
end;  // SetDesignationSelectionPage  }

//==============================================================================
//    Procedure Name: TdlgWizard.SetGridRefSelectionPage
//           Purpose: Prepare the page depending on the route taken
//                    Show or Hide inclusion/exclusion controls accordingly
//------------------------------------------------------------------------------
procedure TdlgWizard.SetGridRefSelectionPage;
var tfOn:Boolean;
begin
  tfOn:=not cbRestrictBounds.Checked;
  rbIncludeOverlaps.Visible :=tfOn;
  rbExcludeOverlaps.Visible :=tfOn;
  imgExplanation.Visible    :=tfOn;
  pnlSpatialRefImage.Visible:=tfOn;
end;  // SetGridRefSelectionPage

//==============================================================================
//    Procedure Name: TdlgWizard.SetPolygonSelectionPage
//           Purpose: Prepare the tree view with a list of polygon layers and
//                    polygons within each layer.
//------------------------------------------------------------------------------
procedure TdlgWizard.SetPolygonSelectionPage;
var lNode: TFlyNode;
    lMapServerLink: TMapServerLink;
    lSearchRec: TSearchRec;
    lKey: TKeyData;
    lCursor: TCursor;
    i: Integer;

  //----------------------------------------------------------------------------
  procedure AddPolygons(AParentNode: TFlyNode; const AKey: TKeyData);
    //AMapSheetKey: TKeyString; const ASheetFileName: String);
  var
    lSheetID: Integer;
    lIdx: Integer;
    lStaticId: Integer;
    lLinked: Byte;
    lLocationKey: TKeyString;
    lLocationName: String;
    lMaxDisplayLength: Integer;
    lDisplayID: String;
  begin
    // Get the Sheet Id, from the filename in map dataset.
    lSheetID := lMapServerLink.SheetIDByFileName(AppSettings.ObjectSheetFilePath +
                                                 AKey.ItemAdditional);

    // Get all objects in the sheet and find out if it is linked to
    // Location Boundary, Admin Area Boundary, or is just Unlinked
    lMaxDisplayLength := Length(IntToStr(lMapServerLink.ObjectTotal[lSheetID] - 1));
    for lIdx := 0 to lMapServerLink.ObjectTotal[lSheetID] - 1 do begin
      lStaticId := lMapServerLink.StaticIDforObject(lSheetID, lIdx);
      // Assume unlinked
      lLinked := 0;

      // Find out if polygon linked to location.
      with dmDatabase.ExecuteSQL(Format('SELECT Location_Key FROM Location_Boundary ' +
                                        'WHERE Map_Sheet_Key = ''%s'' AND Object_Id = %d',
                                        [AKey.ItemKey, lStaticId]), True) do
      begin
        // Check if polygon linked to Location Boundary, only need the Location_Key
        if not Eof then begin
          lLocationKey  := Fields['Location_Key'].Value;
          lLocationName := dmGeneralData.GetLocationName(lLocationKey);
          lLinked := 1;
        end;
        Close;
      end;

      // Check if polygon linked to Admin Area Boundary, if not already linked
      if lLinked = 0 then
        // Only need the Admin Area name
        with dmDatabase.ExecuteSQL(Format('SELECT AA.Item_Name FROM Admin_Boundary AB ' +
                   'INNER JOIN Admin_Area AA ON AA.Admin_Area_Key = AB.Admin_Area_Key ' +
                   'WHERE AB.Map_Sheet_Key = ''%s'' AND AB.Object_Id = %d',
                   [AKey.ItemKey, lStaticId]), True) do
        begin
          if not Eof then begin
            lLocationName := Fields['Item_Name'].Value;
            lLinked := 2;
          end;
          Close;
        end;

      case lLinked of
        // Polygon linked to a location
        1: lNode := tvPolygonLayers.Items.AddChildObject(AParentNode,
                                                         lLocationName,
                                                         Pointer(lStaticId));
        // Polygon linked to an admin area
        2: lNode := tvPolygonLayers.Items.AddChildObject(AParentNode,
                                                         lLocationName,
                                                         Pointer(lStaticId));
        // Unlinked polygon
        else begin
          // This will get the unlinked polygons ordered properly.
          lDisplayID := IntToStr(lStaticID);
          while Length(lDisplayID) < lMaxDisplayLength do lDisplayID := ' ' + lDisplayID;
          lNode := tvPolygonLayers.Items.AddChildObject(
              AParentNode,
              ResStr_UnlinkedPolygon + ' [ID: ' + lDisplayID + ']',
              Pointer(lStaticId));
        end;
      end;
      lNode.ImageIndex    := UNCHECKED_IMG_IDX;
      lNode.SelectedIndex := UNCHECKED_IMG_IDX;
    end;
  end;

  //----------------------------------------------------------------------------
  procedure AddLayers(AParentNode: TFlyNode; const ABaseMapKey: TKeyString);
  begin
    // Need to activate correct map dataset before accessing the sheets.
    lMapServerLink.ActiveDataset := ABaseMapKey + '.gds';
    with dmDatabase.ExecuteSQL('SELECT * FROM Map_Sheet WHERE Sheet_Type=3 ' +
                               'AND Base_Map_Key=''' + ABaseMapKey + '''', True) do
    begin
      while not Eof do begin
        lKey := TKeyData.Create;
        lKey.ItemKey        := Fields['Map_Sheet_Key'].Value;
        lKey.ItemAdditional := VarToStr(Fields['Dataset_Sheet_FileName'].Value);
        lNode := tvPolygonLayers.Items.AddChildObject(AParentNode, Fields['Sheet_Name'].Value, lKey);
        lNode.ImageIndex    := UNCHECKED_IMG_IDX;
        lNode.SelectedIndex := UNCHECKED_IMG_IDX;
        if lKey.ItemAdditional <> '' then
          AddPolygons(lNode, lKey);
        MoveNext;
      end;
      Close;
    end;
  end;

begin
  lCursor := HourglassCursor;
  try
    ClearPolygonLayers;

    lMapServerLink := TMapServerLink.Create(nil);
    try
      with AppSettings.AvailableMaps do
        for i := 0 to Count - 1 do begin
          if Count > 1 then begin
            lKey := TKeyData.Create;
            lKey.ItemKey := Items[i].BaseMapKey;
            lNode := tvPolygonLayers.Items.AddObject(nil, Items[i].DisplayName, lKey);
            lNode.ImageIndex    := UNCHECKED_IMG_IDX;
            lNode.SelectedIndex := UNCHECKED_IMG_IDX;
            lNode.StateIndex    := WORLD_IMG_IDX;
          end else
            lNode := nil;  // No "top" basemap node if only one there.
          AddLayers(lNode, Items[i].BaseMapKey);
        end;
        tvPolygonLayers.SortType := ComCtrls.stNone;
        tvPolygonLayers.SortType := ComCtrls.stText;
    finally
      lMapServerLink.Free;
    end;

    // Prepare list of predefined filters
    with cmbPredefinedFilters do begin
      Clear;
      Items.Add(Format('<%s>', [ResStr_NewFilter]));
      ItemIndex := 0;
      if FindFirst(AppSettings.PolygonFilterPath + '*.pgf', faAnyFile, lSearchRec) = 0 then begin
        Items.Add(Copy(lSearchRec.Name, 1, Length(lSearchRec.Name) - 4));
        while FindNext(lSearchRec) = 0 do
          Items.Add(Copy(lSearchRec.Name, 1, Length(lSearchRec.Name) - 4));
        FindClose(lSearchRec);
      end;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // SetPolygonSelectionPage

//==============================================================================
function TdlgWizard.PolygonLeavePage: Boolean;
var loNode: TFlyNode;
begin
  // Assume nothing selected
  Result := False;
  loNode := tvPolygonLayers.Items.GetFirstNode;
  while loNode <> nil do begin
    // look for any selected polygon (must have a parent which is not a map level)
    if Assigned(loNode.Parent) then
      if  (loNode.ImageIndex <> UNCHECKED_IMG_IDX) and
          (loNode.Parent.StateIndex<>WORLD_IMG_IDX) then begin
        Result := True;
        Exit;
      end;
    loNode := loNode.GetNext;
  end;
end;

//==============================================================================
//     Function Name: SourcesLeavePage
//           Purpose: Forces the user to make some selection before continuing
//      Return Value: Boolean - True if there is some selection
//------------------------------------------------------------------------------
function TdlgWizard.AdminLeavePage:Boolean;
begin
  Result:=lbAdminSelected.Items.Count>0;
end;  // AdminLeavePage

//==============================================================================
//    Procedure Name: TdlgWizard.SetTaxaBioSelectionPage
//           Purpose: Populate the fndTaxaBioFinder and cmbCategories Objects
//                    according to the user selected report type.
//      Dependencies: FBiotopeDictCtrl.populatechecklists and
//                    FTaxaDictCtrl.populatechecklists are used
//------------------------------------------------------------------------------
procedure TdlgWizard.SetTaxaBioSelectionPage;
var lNewKey : TKeyData;
    lCount  : Integer;
    lidxList: Integer;
    lListKey: TKeyString;
    lCursor : TCursor;
begin
  // Proceed only if a new report has been requested
  if FTaxaBioChanged then begin
    lCursor:=HourglassCursor;
    // Clear lists and finder
    // lbAvailable
    lbAvailable.Clear;
    fndTaxaBioFinder.ClearSourceList;
    fndTaxaBioFinder.Text:='';
    // lbSelected
    with lbSelected.Items do begin
      for lCount := 0 to Count -1 do begin
        TReportField(Objects[lCount]).Free;
        Objects[lCount]:=nil;
      end;
      Clear;
    end;
    // Update buttons state
    lblSelected.Caption  := Format(ResStr_Selected, [lbSelected.Items.Count]);
    CheckButtons(lbAvailable, lbSelected, bbAdd, bbRemove, bbAddAll, bbClearAll);
    // Proceed with changing the page appearance
    try
      if ((pcWizard.ActivePage = tsInfoSelection) and (rbSpeciesInfo.Checked)) or
         ((pcWizard.ActivePage <> tsInfoSelection) and (rbSpeciesReport.Checked)) then
      begin
        lblTaxaBioSelection.Caption := ResStr_Cap_TaxaSelection;
        cbIncludeAllTxBt.Caption := ResStr_Cap_IncludeAllTaxa;
        chkIncludeListTxBt.Caption := ResStr_Cap_AllTaxaFromList;
        cbExpandTaxa.Visible := True;
        lbSelected.Height := lbAvailable.Height - 20;
        //Help Setup
        tsTaxaBioSelection.HelpContext := IDH_WIZRADTAXASELECT;
      end else begin
        lblTaxaBioSelection.Caption := ResStr_Cap_BiotopesSelection;
        cbIncludeAllTxBt.Caption := ResStr_Cap_IncludeAllBiotopes ;
        chkIncludeListTxBt.Caption := ResStr_Cap_AllBiotopesFromList;
        cbExpandTaxa.Visible := False;
        lbSelected.Height := lbAvailable.Height;
        //Help Setup
        tsTaxaBioSelection.HelpContext := IDH_WIZRADBIOSELECT;
      end;

      // Clear the Categories box, so that we don't have Biotopes lists mixed in
      // with Taxon checklists.
      ClearTaxaBiotopesCategories;

      if lblTaxaBioSelection.Caption = ResStr_Cap_TaxaSelection then begin
        // Taxon requested
        lblTaxaBioType.Caption := ResStr_Cap_CheckList;
        lblTaxaBioFind.Caption := ResStr_Cap_FindTaxon;
        fndTaxaBioFinder.HandleDuplicate := sfTaxon;
        fndTaxaBioFinder.PartialTaxonSearch := AppSettings.PartialTaxonSearch;
        FdmTaxa.PopulateCheckList(True);
        // Set the taxon list selected to the currently active taxon list
        lListKey := AppSettings.TaxonListKey;
      end else begin
         // Biotope requested
        lblTaxaBioType.Caption := ResStr_Cap_Classification;
        lblTaxaBioFind.Caption := ResStr_Cap_FindBiotope;
        fndTaxaBioFinder.HandleDuplicate := sfnone;
        FdmBiotope.PopulateCheckList;
        // Set the Biotope list selected to the currently active biotope list
        lListKey := AppSettings.BiotopeListKey;
      end;
      // Select active list in combobox
      if lListKey <> '' then
        for lIdxList := 0 to cmbCategories.Items.Count-1 do
          if (TKeyData(cmbCategories.Items.Objects[lIdxList]).ItemKey = lListKey) then begin
            cmbCategories.ItemIndex := lIdxList;
            Break; // from for loop
          end;

      // Adds the rucksack only if it is opened
      if (frmMain.GetForm(TfrmRucksack) <> nil) and
         ((AppSettings.CurrentRucksack.TaxonList.Header.ItemCount > 0) or
          (AppSettings.CurrentRucksack.BiotopeList.Header.ItemCount > 0)) then
      begin
        lNewKey := TKeyData.Create;
        lNewKey.ItemKey       := '';  // STR_RUCKSACK; // Store list key
        lNewKey.ItemAdditional:= 'LOCAL';
        cmbCategories.Items.AddObject(STR_RUCKSACK, lNewKey);
        lNewKey := TKeyData.Create;
        lNewKey.ItemKey       := ''; // STR_RUCKSACK; // Store list key
        lNewKey.ItemAdditional:= 'LOCAL';
        cmbCategories.Items.AddObject(STR_RUCKSACK_EXP, lNewKey);
      end;
    finally
      DefaultCursor(lCursor);
    end;
    FTaxaBioChanged:=False;
  end else
    { Force the repopulation of the search lists on existing text, as this was
       previously terminated - so search text starts from scratch }
    fndTaxaBioFinder.Text := '';
end;  // SetTaxaBioSelectionPage

                              //==============================================================================
//     Function Name: TaxaBioLeavePage
//           Purpose: Forces the user to make some selection before continuing
//      Return Value: Boolean - True if there is some selection
//------------------------------------------------------------------------------
function TdlgWizard.TaxaBioLeavePage:Boolean;
begin
  ValidateValue(not (chkIncludeListTxBt.Checked and chkIncludeListTxBt.Enabled and
      (cmbCategories.ItemIndex=-1)), ResStr_SelectList, cmbCategories);
  Result := (lbSelected.Items.Count > 0) or
            cbIncludeAllTxBt.Checked or chkIncludeListTxBt.Checked;
end;  // TaxaBioLeavePage

//==============================================================================
//    Procedure Name: TdlgWizard.SetPlacesSelectionPage
//           Purpose: To prepare the place names finder
//      Dependencies: Uses Finder and Search unit
//      Side Effects: Clears the available places list
//------------------------------------------------------------------------------
procedure TdlgWizard.SetPlacesSelectionPage;
begin
  fndPlaceFinder.HandleDuplicate := sfLocation;
  fndPlaceFinder.SetSearchText('');
end;  // SetPlacesSelectionPage

//==============================================================================
//     Function Name: TdlgWizard.PlacesLeavePage
//           Purpose: To tell if any places have been selected by the user.
//      Return Value: Boolean. True if at least one place is selected.
//     Special Notes: Used when the list is populated by the map
//------------------------------------------------------------------------------
function TdlgWizard.PlacesLeavePage : Boolean;
begin
  Result := lbPlacesSelected.Items.Count>0;
end;  // PlacesLeavePage

//==============================================================================
//     Function Name: TdlgWizard.GridRefLeavePage
//           Purpose: To check if any places exist within the grid reference area selected
//      Return Value: Boolean - True is at least one location is in the region chosen
//      Dependencies: Uses Map unit
//      Side Effects: Populates lbPlacesSelected with any locations which match the region.
//                    This allows the rest of the wizard to be the same.
//------------------------------------------------------------------------------
function TdlgWizard.GridRefLeavePage: Boolean;
var
  // Bottom Left of the database square
  GBL:TLatLong;
  // Top RIght of the database square
  GTR:TLatLong;
  i            :Integer;
  lTargetObj   :TWizardKeyObject;
  lLocationKeys:TStringList;
  lCursor      :TCursor;

  function CreateLocationHitList: Boolean;
  begin
    frmMain.SetStatus(ResStr_FindingLocations + '...');
    try
      Result := dmValidation.CreateLocationHitList(GBL, GTR, lLocationKeys,
                                                   rbExcludeOverlaps.Checked,
                                                   frmMain.SetProgress);
    finally
      frmMain.SetStatus('');
      frmMain.SetProgress(0);
    end;
  end;

begin
  Result := True;
  if FSpatialRefChanged then begin
    ClearPlacesSelected;
    lCursor := HourglassCursor;
    try
      GBL := ConvertToLatLong(eSWCorner.Text, AppSettings.SpatialRefSystem);
      GTR := ConvertToLatLong(eNECorner.Text, AppSettings.SpatialRefSystem);
      // Get a key list ready
      lLocationKeys := TStringList.Create;
      try
        Result := CreateLocationHitList;
        if not Result then
          MessageDlg(ResStr_NoMatchingLocation, mtInformation, [mbOK],0)
        else begin
          // loop through the keys supplied in Items and create Objects
          // which can be used as if we had selected these places manually
          for i := 0 to lLocationKeys.Count -1 do
          begin
            lTargetObj := TWizardKeyObject.Create;
            lTargetObj.ParentKey := lLocationKeys[i];
            lTargetObj.Key       := lLocationKeys[i];
            lbPlacesSelected.Items.AddObject(lLocationKeys[i],lTargetObj);
          end;
        end;
      finally
        lLocationKeys.Free;
      end;
    finally
      DefaultCursor(lCursor);
    end;
    FSpatialRefChanged:= not Result; // Force re-try if nothing found
  end;
end;  // GridRefLeavePage

//==============================================================================
procedure TdlgWizard.rbInfoClick(Sender: TObject);
begin
  inherited;
  FInformationChanged:=True;
  // In case of going back and changing only this page, next page is not updated
  // so we need this extra line
  FTaxaBioChanged := (rbSpeciesInfo.Checked or rbBiotopeInfo.Checked) and
                     rbInfoSelect.Checked;
  FReportChanged := True;
end;  // rbInfoClick

//==============================================================================
procedure TdlgWizard.rbSelectedInfoClick(Sender: TObject);
begin
  inherited;
  FTaxaBioChanged:=True;
  FReportChanged := True;
end;  // rbSelectedInfoClick

//==============================================================================
//    Procedure Name: TdlgWizard.SetSelectAttributes
//           Purpose: Prepare the Attribute list
//     Special Notes: The codes used to know which attributes to display are:
//                    A: Valid for all kind of reports
//                    B: Valid for Biotope reports only
//                    T: Valid for Taxon reports only
//                    Y: Valid for Place or Biotope on Place reports
//                    Z: Valid for Place or Taxon on Place reports
//------------------------------------------------------------------------------
procedure TdlgWizard.SetSelectAttributes;
begin
  //if FInformationChanged then begin
    tvAttributes.Items.Clear;
    FAttributesChanged:=True;
    AddHierarchicalAttributesToTreeView(tvAttributes, True, 8);
    FInformationChanged:=False;
  //end;
end;  // SetSelectAttributes

{-------------------------------------------------------------------------------
  Returns a set of otTaxa, otBiotopes depending on which occurrence types may
  be present in the report
  Created : 4/12/2002
}
function TdlgWizard.ReportOccurrenceTypes : TReportOccurrenceTypes;
begin
  if rbSpeciesReport.Checked then
    Result := [otTaxa]
  else if rbBiotopesReport.Checked then
    Result := [otBiotopes]
  else if rbPlaceReport.Checked then begin
    if rbSpeciesInfo.Checked then
      Result := [otTaxa]
    else if rbBiotopeInfo.Checked then
      Result := [otBiotopes]
    else
      Result := [otTaxa, otBiotopes];
  end;
end;

//==============================================================================
//     Function Name: TdlgWizard.SelectAttributesLeavePage
//           Purpose: To make sure that at least one attribute is selected for
//                    reporting.
//      Return Value: Boolean - True if at least one attribute selected and no
//                    other failure (see notes), False if an invalid combination
//                    of fields or no fields chosen
//     Special Notes: This routine also checks for combinations of field types
//                    which would cause errors in the SQL later. This can occur
//                    if a memo field is chosen.
//------------------------------------------------------------------------------
function TdlgWizard.SelectAttributesLeavePage: Boolean;
var haveSelection: Boolean;
    lAttribute     : TAttribute;
    i              : Integer;
begin
  Result := True;

  // Quick initial check to ensure that they have selected a field
  haveSelection := False;
  for i := 0 to dmWizard.ReportGenerator.Attributes.Count -1 do begin
    lAttribute := TAttribute(dmWizard.ReportGenerator.Attributes.Objects[i]);
    if lAttribute.Selected then begin
      haveSelection := True;
      break;
    end;
  end;

  if not haveSelection then begin
    MessageDlg(ResStr_SelectAttribute, mtInformation, [mbOk], 0);
    Result := False;
    Exit;
  end;
end;  // SelectAttributesLeavePage

//==============================================================================
//    Procedure Name: TdlgWizard.SetSortAttributes
//           Purpose: To populate the list of attributes which can be use for
//                    sorting.
//     Special Notes: See version 23 for an implementation which allows ANY
//                    fields from selected Tables.
//                    This version limits the User to sort only fields which
//                    are in the select. This is because of the requirements
//                    for Union queries.
//------------------------------------------------------------------------------
procedure TdlgWizard.SetSortAttributes;
begin
  if FAttributesChanged then begin
    tvSort.Items.Clear;
    AddHierarchicalAttributesToTreeView(tvSort, False, BLANK_IMG_IDX);  // default is a blank icon
    tvSort.Repaint;
    FAttributesChanged:=False;
  end;
end;  // SetSortAttributes

//==============================================================================
//    Procedure Name: TdlgWizard.SetSummaryScreen
//           Purpose: Sets up a display of the user's choices as a tree view.
//      Side Effects: populates tvSummary
//     Special Notes: Several local procedures are used as required. These are
//                    declared at the start of this procedure.
//------------------------------------------------------------------------------
procedure TdlgWizard.SetSummaryScreen;
  //----------------------------------------------------------------------------
  //    Procedure Name: AddChildrenFromList
  //           Purpose: Add item from lists or checkbox lists to a particular
  //                    tree node.
  //        Parameters: tnParent - The node to which the names should be added
  //                    objList - the list from which the names should be obtained
  //      Dependencies: Uses tvSummary
  //     Special Notes: Uses the "IS" keyword to find the type of objList
  //----------------------------------------------------------------------------
  procedure AddChildrenFromList(tnParent:TFlyNode; objList:TObject);
  var iIndex :Integer;
      tnChild:TFlyNode;
  begin
    if objList is TListBox then
      with TListBox(objList) do
        for iIndex:=0 to Items.Count-1 do begin
          tnChild:=tvSummary.Items.AddChild(tnParent,Items[iIndex]);
          tnChild.ImageIndex   :=-1;
          tnChild.SelectedIndex:=-1;
        end
    else
      with TCheckListBox(objList) do
        for iIndex:=0 to Items.Count-1 do
          if Checked[iIndex] then begin
            tnChild:=tvSummary.Items.AddChild(tnParent,Items[iIndex]);
            tnChild.ImageIndex   :=-1;
            tnChild.SelectedIndex:=-1;
          end;
  end;  // AddChildrenFromList

  //----------------------------------------------------------------------------
  procedure AddAttributesToNode(tnAttributeNode, tnSortNode:TFlyNode);
  var iIndex :Integer;
      tnChild:TFlyNode;
  begin
    for iIndex:=0 to dmWizard.ReportGenerator.Attributes.Count-1 do
      with TAttribute(dmWizard.ReportGenerator.Attributes.Objects[iIndex]) do
      begin
        if Selected then begin
          tnChild := tvSummary.Items.AddChild(tnAttributeNode, Name);
          tnChild.ImageIndex   :=-1;
          tnChild.SelectedIndex:=-1;
        end;

        if Sort <> stNone then
        begin
          tnChild := tvSummary.Items.AddChild(tnSortNode, Name);
          if Sort = stAsc then tnChild.ImageIndex := SORT_UP_IMG_IDX
                          else tnChild.ImageIndex := SORT_DOWN_IMG_IDX;
          tnChild.SelectedIndex := tnChild.ImageIndex;
        end;
      end;
  end;  // AddAttributesToNode

  //----------------------------------------------------------------------------
  //    Procedure Name: AddPlaces
  //           Purpose: Copies place names from a list into a tree node
  //      Dependencies: Uses tvSummary and rbByplaceName
  //----------------------------------------------------------------------------
  procedure AddPlaces;
  var
    parentNode, childNode: TFlyNode;
  begin
    if rbByPlaceName.Checked then begin
      parentNode := tvSummary.Items.Add(nil, ResStr_Location);
      parentNode.ImageIndex := 2;
    end else begin
      parentNode := tvSummary.Items.Add(nil, ResStr_GridReferences);
      parentNode.ImageIndex := 7;
    end;
    parentNode.SelectedIndex := parentNode.ImageIndex;

    if rbByPlaceName.Checked then
      AddChildrenFromList(parentNode, lbPlacesSelected)
    else begin
      childNode := tvSummary.Items.AddChild(parentNode, ResStr_NorthEastCorner + ': ' + eNECorner.Text);
      childNode.ImageIndex    := -1;
      childNode.SelectedIndex := -1;
      childNode := tvSummary.Items.AddChild(parentNode, ResStr_SouthWestCorner + ': ' + eSWCorner.Text);
      childNode.ImageIndex    := -1;
      childNode.SelectedIndex := -1;
    end;
  end;  // AddPlaces;

  //----------------------------------------------------------------------------
  //    Procedure Name: AddSpecies
  //           Purpose: Copies names of species into a tree node
  //      Dependencies: Uses rbSpeciesInfo, rbSpeciesReport, rbPlaceReport,
  //                    rbByPlaceName, rbInfoAll, rbPlacesInfo, tvSummary
  //----------------------------------------------------------------------------
  procedure AddSpeciesBiotopes;
  var tnNode:TFlyNode;
      ltfTaxa, ltfBios: Boolean;
  begin
    ltfTaxa := rbSpeciesReport.Checked or (rbPlaceReport.Checked and rbSpeciesInfo.Checked);
    ltfBios := rbBiotopesReport.Checked or (rbPlaceReport.Checked and rbBiotopeInfo.Checked);

    if ltfTaxa then begin
      tnNode:=tvSummary.Items.Add(nil, ResStr_Local_Species);
      tnNode.ImageIndex:=3;
    end else if ltfBios then begin
      tnNode:=tvSummary.Items.Add(nil, ResStr_Local_Biotopes);
      tnNode.ImageIndex:=4;
    end else begin
      tnNode:=tvSummary.Items.Add(nil, ResStr_Local_Species + ' / ' + ResStr_Local_Biotopes);
      tnNode.ImageIndex:=5;
    end;
    tnNode.SelectedIndex:=tnNode.ImageIndex;
    if (rbPlaceReport.Checked and rbByPlaceName.Checked and
        (rbPlacesInfo.Checked or rbInfoAll.Checked)) or
       ((ltfTaxa or ltfBios) and (cbIncludeAllTxBt.Checked or chkIncludeListTxBt.Checked)) then
    begin
      if ((ltfTaxa or ltfBios) and chkIncludeListTxBt.Checked) then
        tnNode:=tvSummary.Items.AddChild(tnNode, ResStr_AllIn + ' ' + cmbCategories.Text)
      else
        tnNode:=tvSummary.Items.AddChild(tnNode, ResStr_All);
      tnNode.ImageIndex   := -1;
      tnNode.SelectedIndex:= -1;
    end else
    if lbSelected.Items.Count > 1000 then begin
      tnNode:=tvSummary.Items.AddChild(tnNode, ResStr_Over1000NamesSelected);
      tnNode.ImageIndex   := -1;
      tnNode.SelectedIndex:= -1;
    end else
      AddChildrenFromList(tnNode,lbSelected);
  end;  // AddSpeciesBiotopes

  //----------------------------------------------------------------------------
  //    Procedure Name: AddSelection
  //           Purpose: General routine to ensure the correct bitmap are set
  //        Parameters: stType - the String to be displayed
  //                    iImageIndex - the index of the image to be displayed
  //----------------------------------------------------------------------------
  procedure AddSelection(stType:String; iImageIndex:Integer; List:TControl);
  var tnNode:TFlyNode;
  begin
    tnNode:=tvSummary.Items.Add(nil,stType);
    tnNode.ImageIndex   :=iImageIndex;
    tnNode.SelectedIndex:=iImageIndex;
    AddChildrenFromList(tnNode,List);
  end;  // AddSelection

  //----------------------------------------------------------------------------
  procedure AddAttributes;
  var tnAttributeNode, tnSortNode : TFlyNode;
  begin
    tnAttributeNode:=tvSummary.Items.Add(nil, ResStr_Attributes);
    tnAttributeNode.ImageIndex   :=6;
    tnAttributeNode.SelectedIndex:=6;
    tnSortNode := tvSummary.Items.Add(nil, ResStr_SortOrder);
    tnSortNode.ImageIndex := 6;
    tnSortNode.SelectedIndex := 6;
    AddAttributesToNode(tnAttributeNode, tnSortNode);
    //remove sort node if empty
    if tnSortNode.getFirstChild  = nil then
      tvSummary.Items.Delete(tnSortNode);
  end;

  //----------------------------------------------------------------------------
  procedure AddBoundingBox;
  var tnNode:TFlyNode;
  begin
    tnNode:=tvSummary.Items.Add(nil, ResStr_BoundingBox);
    tnNode.ImageIndex   :=7;
    tnNode.SelectedIndex:=7;
    tnNode:=tvSummary.Items.AddChild(tnNode, ResStr_NorthEastCorner + ': ' + eNECorner.Text);
    tnNode.ImageIndex   :=-1;
    tnNode.SelectedIndex:=-1;
    tnNode:=tvSummary.Items.Add(tnNode, ResStr_SouthWestCorner + ': ' + eSWCorner.Text);
    tnNode.ImageIndex   :=-1;
    tnNode.SelectedIndex:=-1;
  end;  // AddBoundingBox
  //----------------------------------------------------------------------------
  //    Procedure Name: AddPolygons
  //           Purpose: Adds the selected polygons on the Polygon Selection tab
  //                    tree view.
  //      Dependencies: Uses tvPolygonLayers
  //----------------------------------------------------------------------------
  procedure AddPolygons;
  var tnNode: TFlyNode;
  begin
    tnNode := tvSummary.Items.Add(nil, ResStr_PolygonSelectionActive);
    tnNode.ImageIndex := 6;
    tnNode.SelectedIndex := 6;
  end;
  //----------------------------------------------------------------------------

begin
  tvSummary.Items.Clear;
  if cbRestrictSource.Checked then AddSelection(ResStr_Sources, 0, lbSourcesSelected);
  if cbRestrictArea.Checked   then AddSelection(ResStr_AdministrativeAreas, 1, lbAdminSelected);
  if cbRestrictBounds.Checked then AddBoundingBox;
  if cbRestrictPolygons.Checked then AddPolygons;
  if rbPlaceReport.Checked    then AddPlaces;
  AddSpeciesBiotopes;
  AddAttributes;
end;  // SetSummaryScreen

//==============================================================================
//    Procedure Name: TdlgWizard.SetUpTabSequence
//           Purpose: Sets up the initial order of the report wizard tabbed
//                    dialog display
//------------------------------------------------------------------------------
procedure TdlgWizard.SetupTabSequence;
begin
  FTabSequence.Clear;
  //Add start tab.
  FTabSequence.AddToSequence(tsReportSelector);
  //Init sequence.
  FTabSequence.Sequence := 0;
end;  // SetupTabSequence

//==============================================================================
//    Procedure Name: TdlgWizard.CheckButtons
//           Purpose: Adjust the state of up to three buttons dependent upon
//                    the number of entries in up to two lists.
//        Parameters: AList, BList are the "available" and "selected" lists
//                    respectively AddBtn is made active if there are any
//                    Items available RemoveBtn and ClearAllBtn are made
//                    active if any Items have been selected
//------------------------------------------------------------------------------
procedure TdlgWizard.CheckButtons(ATreeList:TWinControl; BList: TListBox;
  AddBtn, RemoveBtn, AddAllBtn, ClearAllBtn: TImageListButton);
begin
  // That's for the Admin TreeView
  if ATreeList is TListBox then
    AddBtn.Enabled := (TListBox(ATreeList).Items.Count > 0) and TListBox(ATreeList).Enabled
  else if ATreeList is TTreeView then
    AddBtn.Enabled := (TTreeView(ATreeList).Items.Count > 0) and TTreeView(ATreeList).Enabled
  else if ATreeList is TKeyboardRapidTree then
    AddBtn.Enabled := (TKeyboardRapidTree(ATreeList).Items.Count > 0) and TKeyboardRapidTree(ATreeList).Enabled;

  RemoveBtn.Enabled  := BList.Items.Count > 0;
  if AddAllBtn<>nil then
    AddAllBtn.Enabled:=AddBtn.Enabled;
  ClearAllBtn.Enabled:= RemoveBtn.Enabled;
end;  // CheckButtons

//==============================================================================
//==============================================================================
//    Procedure Name: TdlgWizard.rbSourcesClick
//           Purpose: Set the FSourcesChanged to True. Used to update the
//                    Sources selection page if a different selection is made
//                    after going back to the selector page.
//------------------------------------------------------------------------------
procedure TdlgWizard.rbSourcesClick(Sender: TObject);
begin
  inherited;
  FSourcesChanged:=True;
end;  // rbSourcesClick

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.cmbSourcesTypeChange
//           Purpose: Re-populate the available sources when the user choose
//                    a new type
//      Side Effects: Items AND Objects cleared in PopulateTopLevel.
//------------------------------------------------------------------------------
procedure TdlgWizard.cmbSourcesTypeChange(Sender: TObject);
var lCount,lIdx: Integer;
begin
  inherited;
  FdmWizard.PopulateSurveys(cmbSourcesType, lbSourcesAvailable);
  with lbSourcesAvailable.Items do
    // Find and remove already selected Items. In this case, destroy associated Objects as well
    for lCount := 0 to lbSourcesSelected.Items.Count - 1 do
    begin
      lIdx:= IndexOf(lbSourcesSelected.Items[lCount]);
      if lIdx<> -1 then
      begin
        TNodeObject(Objects[lIdx]).Free;
        Objects[lIdx]:=nil;
        Delete(lIdx);
      end;
    end;
  CheckButtons(lbSourcesAvailable, lbSourcesSelected, bbSourceAdd, bbSourceRemove, bbSourceAddAll, bbSourceClearAll);
end;  // cmbSourcesTypeChange  

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.cmbDesignationSetChange
//           Purpose: Re-populate the available DesignationSet when the user choose
//                    a new type
//      Side Effects: Items AND Objects cleared in PopulateTopLevel.
//------------------------------------------------------------------------------
procedure TdlgWizard.cmbDesignationSetChange(Sender: TObject);
var lCount,lIdx: Integer;
begin
  inherited;
  FdmWizard.PopulateTaxonDesignationSetItems(cmbDesignationSet, lbDesignationAvailable);
  with lbDesignationAvailable.Items do
    // Find and remove already selected Items. In this case, destroy associated Objects as well
    for lCount := 0 to lbDesignationSelected.Items.Count - 1 do
    begin
      lIdx:= IndexOf(lbDesignationSelected.Items[lCount]);
      if lIdx<> -1 then
      begin
        TNodeObject(Objects[lIdx]).Free;
        Objects[lIdx]:=nil;
        Delete(lIdx);
      end;
    end;
  CheckButtons(lbDesignationAvailable, lbDesignationSelected, bbDesignationAdd,
      bbDesignationRemove, bbDesignationsAddAll, bbDesignationClearAll);
end;  // cmbDesignationSetChange

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.fndSourcesChange
//           Purpose: Populate the list of SourceTypes available depending on the
//                    partial name supplied by the user
//      Dependencies: uses the TFinder object
//      Side Effects: Removes any SourceTypes already selected from the available
//                    list
//------------------------------------------------------------------------------
procedure TdlgWizard.fndSourcesChange(Sender: TObject);
var lCount,lIdx: Integer;
begin
  inherited;
  with lbSourcesAvailable.Items do
    for lCount:=0 to lbSourcesSelected.Items.Count - 1 do
    begin
      lIdx := IndexOf(lbSourcesSelected.Items[lCount]);
      if lIdx <> -1 then
        Delete(lIdx);
    end;
  CheckButtons(lbSourcesAvailable, lbSourcesSelected, bbSourceAdd, bbSourceRemove, bbSourceAddAll, bbSourceClearAll);
end;  // fndSourceTypesChange

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.bbSourceAddClick
//           Purpose: Moves Sources from available list to selected list
//      Side Effects: Updates buttons state
//------------------------------------------------------------------------------
procedure TdlgWizard.bbSourceAddClick(Sender: TObject);
var lKeyObj    :TWizardKeyObject;
    lSourceObj :TSurveyNode;
    lSourceData:TKeyData;
    lIndex     :Integer;
    lCursor    : TCursor;
begin
  with lbSourcesAvailable do begin
    lCursor := HourglassCursor;
    Items.BeginUpdate;
    lbSourcesSelected.Items.BeginUpdate;
    try
      for lIndex:=Items.Count-1 downto 0 do
        if Selected[lIndex] then begin
          //Setup a new object.
          if rbSourcesSurveys.Checked then
          begin
            lSourceObj := TSurveyNode(Items.Objects[lIndex]);
            lKeyObj    := TWizardKeyObject.Create;
            lKeyObj.Text     :=lSourceObj.Text;
            lKeyObj.ParentKey:=lSourceObj.ParentKey;
            lKeyObj.Key      :=lSourceObj.ItemKey;
            //Free the original object and delete the item.
            lSourceObj.Free;
            Items.Objects[lIndex]:=nil;
          end else begin
            lSourceData := TKeyData(Items.Objects[lIndex]);  // Don't destroy this object, Finder needs it
            lKeyObj     := TWizardKeyObject.Create;
            lKeyObj.Text:=Items[lIndex];
            lKeyObj.Key :=lSourceData.ItemKey;
            // ItemAdditional contains INDIVIDUAL or ORGANISATION, according to selected item
            lKeyObj.ParentKey:=TKeyData(Items.Objects[lIndex]).ItemAdditional;
          end;
          // remove item from available list
          Items.Delete(lIndex);
          //Add the new object to the other list.
          lbSourcesSelected.Items.AddObject(lKeyObj.Text, lKeyObj);
        end;
    finally
      lbSourcesSelected.Items.EndUpdate;
      Items.EndUpdate;
      DefaultCursor(lCursor);
    end;
  end;
  CheckButtons(lbSourcesAvailable, lbSourcesSelected, bbSourceAdd, bbSourceRemove, bbSourceAddAll, bbSourceClearAll);
  FReportChanged := True;
end;  // bbSourceAddClick

//------------------------------------------------------------------------------
procedure TdlgWizard.lbSourcesAvailableKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=' ' then bbSourceAddClick(nil);
end;  // lbSourcesAvailableKeyPress

//------------------------------------------------------------------------------
procedure TdlgWizard.lbDesignationAvailableKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=' ' then bbDesignationAddClick(nil);
end;  // lbDesignationRemoveKeyPress

//------------------------------------------------------------------------------
procedure TdlgWizard.lbDesignationSelectedKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=' ' then bbDesignationRemoveClick(nil);
end;  // lbSourcesAvailableKeyPress

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.bbSourceRemoveClick
//           Purpose: moves Sources from selected list to available list
//      Side Effects: Updates buttons state
//------------------------------------------------------------------------------
procedure TdlgWizard.bbSourceRemoveClick(Sender: TObject);
var
  lSourceObj:TSurveyNode;
  lKeyObj   :TWizardKeyObject;
  lIndex    :Integer;
  lCursor   : TCursor;
begin
  with lbSourcesSelected do begin
    lCursor := HourglassCursor;
    Items.BeginUpdate;
    lbSourcesAvailable.Items.BeginUpdate;
    try
      for lIndex:=Items.Count-1 downto 0 do
        if Selected[lIndex] then begin
          lKeyObj := TWizardKeyObject(Items.Objects[lIndex]);
          if rbSourcesSurveys.Checked then
          begin
            //Check this item belongs with the item in the combobox;
            if lKeyObj.ParentKey = TKeyData(cmbSourcesType.Items.Objects[cmbSourcesType.ItemIndex]).ItemKey then
            begin
              //Create a new object of TSurveyNode as in source list.
              lSourceObj := TSurveyNode.Create;
              lSourceObj.Text     := lKeyObj.Text;
              lSourceObj.ParentKey:= lKeyObj.ParentKey;
              lSourceObj.ItemKey  := lKeyObj.Key;
              //Add the object to the source list;
              lbSourcesAvailable.Items.AddObject(lSourceObj.Text, lSourceObj);
              lbSourcesAvailable.Sorted:=True;  // Reorder
            end;
            lKeyObj.Free;
            Items.Objects[lIndex]:=nil;
            Items.Delete(lIndex);
          end else begin
            fndSources.ReInsert(Items[lIndex],lKeyObj.Key);
            lKeyObj.Free;
            Items.Objects[lIndex]:=nil;
            Items.Delete(lIndex);
          end;
        end;
    finally
      lbSourcesAvailable.Items.EndUpdate;
      Items.EndUpdate;
      DefaultCursor(lCursor);
    end;
  end;
  CheckButtons(lbSourcesAvailable, lbSourcesSelected, bbSourceAdd, bbSourceRemove, bbSourceAddAll, bbSourceClearAll);
  FReportChanged := True;
end;  // bbSourceRemoveClick

//------------------------------------------------------------------------------
procedure TdlgWizard.lbSourcesSelectedKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=' ' then bbSourceRemoveClick(nil);
end;  // lbSourcesSelectedKeyPress

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.bbSourceClearAllClick
//           Purpose: Adds all available Sources
//------------------------------------------------------------------------------
procedure TdlgWizard.bbSourceAddAllClick(Sender: TObject);
var lIdx:Integer;
begin
  inherited;
  with lbSourcesAvailable do begin
    Items.BeginUpdate;
    try
      for lIdx:=0 to Items.Count-1 do Selected[lIdx]:=True;
    finally
      Items.EndUpdate;
    end;
  end;
  bbSourceAddClick(nil);
end;  // bbSourcesAddAllClick

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.bbSourceClearAllClick
//           Purpose: Clears all selected Sources
//      Side Effects: Resets Sources in available list and buttons state.
//------------------------------------------------------------------------------
procedure TdlgWizard.bbSourceClearAllClick(Sender: TObject);
var lIdx:Integer;
begin
  inherited;
  with lbSourcesSelected do begin
    Items.BeginUpdate;
    try
      for lIdx:=0 to Items.Count-1 do Selected[lIdx]:=True;
    finally
      Items.EndUpdate;
    end;
  end;
  bbSourceRemoveClick(nil);

  if not rbSourcesSurveys.Checked then begin
    fndSources.SetSearchText(fndSources.Text);
    fndSourcesChange(nil);
  end;
end;  // bbSourceClearAllClick

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.lbSourcesSelectedClick
//           Purpose: Reset button states to allow for removal is item
//------------------------------------------------------------------------------
procedure TdlgWizard.lbSourcesSelectedClick(Sender: TObject);
begin
  CheckButtons(lbSourcesAvailable, lbSourcesSelected, bbSourceAdd, bbSourceRemove, bbSourceAddAll, bbSourceClearAll);
end;  // lbSourcesSelectedClick

//==============================================================================
//==============================================================================
//    Procedure Name: TdlgWizard.cmbTypeChange
//           Purpose: To ensure that the available admin areas list is
//                    correctly populated according to the type selected
//------------------------------------------------------------------------------
procedure TdlgWizard.cmbTypeChange(Sender: TObject);
begin
  inherited;
  ClearAdminTree;
  with cmbType do
    FdmAdmin.PopulateTopLevel(TKeyData(Items.Objects[ItemIndex]));
  CheckButtons(tvAdminAvailable, lbAdminSelected, bbAdminAdd, bbAdminRemove, nil, bbAdminClearAll);
end;  // cmbTypeChange

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.tvAdminAvailableClick
//                    TdlgWizard.bbAdminAddClick
//                    TdlgWizard.bbAdminRemoveClick
//                    TdlgWizard.bbAdminClearAllClick
//           Purpose: Move the Admin area entries between the tree of
//                    available areas and the list of selected areas
//------------------------------------------------------------------------------
procedure TdlgWizard.tvAdminAvailableClick(Sender: TObject);
begin
  inherited;
  CheckButtons(tvAdminAvailable, lbAdminSelected, bbAdminAdd, bbAdminRemove, nil, bbAdminClearAll);
end;  // tvAdminAvailableClisk

//------------------------------------------------------------------------------
procedure TdlgWizard.bbAdminAddClick(Sender: TObject);
begin
  with tvAdminAvailable do
    if Assigned(Selected) then begin
      ExpandAdminNode(Selected);
      PopulateAdminListbox(Selected);
    end;
  CheckButtons(tvAdminAvailable, lbAdminSelected, bbAdminAdd, bbAdminRemove, nil, bbAdminClearAll);
  FReportChanged := True;
end;  // bbAdminAddClick

//------------------------------------------------------------------------------
procedure TdlgWizard.tvAdminAvailableKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=' ' then begin
    bbAdminAddClick(nil);
    Key:=#0;  // Stops the beep
  end;
end;  // tvAdminAvailableKeyPress

//------------------------------------------------------------------------------
procedure TdlgWizard.bbAdminRemoveClick(Sender: TObject);
var lIdx:Integer;
begin
  with lbAdminSelected do
    for lIdx:=Items.Count-1 downto 0 do 
      if Selected[lIdx] then begin
        TWizardKeyObject(Items.Objects[lIdx]).Free;
        Items.Objects[lIdx]:=nil;
        Items.Delete(lIdx);
      end;
  CheckButtons(tvAdminAvailable, lbAdminSelected, bbAdminAdd, bbAdminRemove, nil, bbAdminClearAll);
  FReportChanged := True;
end;  // bbAdminRemoveClick

//------------------------------------------------------------------------------
procedure TdlgWizard.lbAdminSelectedKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=' ' then bbAdminRemoveClick(nil);
end;  // lbAdminSelectedKeyPress

//------------------------------------------------------------------------------
procedure TdlgWizard.bbAdminClearAllClick(Sender: TObject);
var lCount:Integer;
begin
  // Nodes have not been removed from the tree, so only clear the listbox
  with lbAdminSelected.Items do begin
    for lCount:=0 to Count-1 do begin
      TWizardKeyObject(Objects[lCount]).Free;
      Objects[lCount]:=nil;
    end;
    Clear;
  end;
  CheckButtons(tvAdminAvailable, lbAdminSelected, bbAdminAdd, bbAdminRemove, nil, bbAdminClearAll);
  FReportChanged := True;
end;  // bbAdminClearAllClick

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.ExpandAdminNode
//           Purpose: "It does what it says on the box"
//        Parameters: iStartNode is the parent node entry
//------------------------------------------------------------------------------
procedure TdlgWizard.ExpandAdminNode(const AStartNode: TFlyNode);
var loNewNode: TFlyNode;
begin
  FdmAdmin.PopulateChildNodes(AStartNode);
  if AStartNode.HasChildren then
  begin
    loNewNode := AStartNode.GetFirstChild;
    repeat
      ExpandAdminNode(loNewNode);
      loNewNode := AStartNode.GetNextChild(loNewNode);
    until loNewNode = nil;
  end;
end;  // ExpandAdminNode

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.tvAdminAvailableExpanding
//           Purpose: Ensure that the child nodes are correctly populated for
//                    a selected admin area.
//      Dependencies: The tree view should have been populated
//     Special Notes: Node is passed on to another function AllowExpansion is ignored
//------------------------------------------------------------------------------
procedure TdlgWizard.tvAdminAvailableExpanding(Sender: TObject;
  Node: TFlyNode; var AllowExpansion: Boolean);
begin
  inherited;
  FdmAdmin.PopulateChildNodes(Node);
end;  // tvAdminAvailableExpanding

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.lbAdminSelectedClick
//           Purpose: Changes the button states when a selected admin area is clicked
//------------------------------------------------------------------------------
procedure TdlgWizard.lbAdminSelectedClick(Sender: TObject);
begin
  CheckButtons(tvAdminAvailable, lbAdminSelected, bbAdminAdd, bbAdminRemove, nil, bbAdminClearAll);
end;  // lbAdminSelectedClick

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.PopulateAdminListbox
//           Purpose: Makes sure that all required Admin areas are placed in
//                    the list according to category.
//        Parameters: iNode - the main branch of the admin areas tree (?)
//      Dependencies: Uses the cmbType object to determine which entries to display
//------------------------------------------------------------------------------
procedure TdlgWizard.PopulateAdminListBox(const ANode: TFlyNode);
var lKeyObj :TWizardKeyObject;
    lNewNode:TFlyNode;
begin
  if lbAdminSelected.Items.IndexOf(ANode.Text)=-1 then begin
    lKeyObj := TWizardKeyObject.Create;
    lKeyObj.Text     :=TAdminAreaDictionaryNode(ANode.Data).Text;
    lKeyObj.Key      :=TAdminAreaDictionaryNode(ANode.Data).ItemKey;
    lKeyObj.ParentKey:=TKeyData(cmbType.Items.Objects[cmbType.ItemIndex]).ItemKey;
    lbAdminSelected.Items.AddObject(lKeyObj.Text, lKeyObj);
  end;

  if ANode.HasChildren then begin
    lNewNode := ANode.GetFirstChild;
    repeat
      PopulateAdminListBox(lNewNode);
      lNewNode := ANode.GetNextChild(lNewNode);
    until lNewNode = nil;
  end;
end;  // PopulateAdminListBox

//==============================================================================
//==============================================================================
//    Procedure Name: TdlgWizard.cmbCategoriesChange
//           Purpose: To make sure that the Finder edit box is correctly
//                    populated when the category of Taxon or Biotope is altered.
//      Dependencies: Relies on queries in the Search Unit
//      Side Effects: Populates the Taxon or Biotopefinder.
//------------------------------------------------------------------------------
procedure TdlgWizard.cmbCategoriesChange(Sender: TObject);
var lstCategory: String;
begin
  inherited;
  dmSearch.Terminate;
  lbAvailable.Clear;
  fndTaxaBiofinder.ClearSourceList;
  lblActiveBioTaxa.Caption := '';

  lstCategory := cmbCategories.Items[cmbCategories.ItemIndex];
  if (lstCategory = STR_RUCKSACK_EXP) or (lstCategory = STR_RUCKSACK) then
  begin
    fndTaxaBiofinder.SearchMode := smAlways;
    fndTaxaBioFinder.Text :='';
    if Assigned(AppSettings.CurrentRucksack) then begin
      if SameText(lblTaxaBioSelection.Caption, ResStr_Cap_TaxaSelection) then
        dmSearch.InitFromTaxonList(
            fndTaxaBioFinder,
            AppSettings.CurrentRucksack.TaxonList,
            True,
            lstCategory = STR_RUCKSACK_EXP)
      else
        dmSearch.InitFromBiotopeList(
            fndTaxaBioFinder,
            AppSettings.CurrentRucksack.BiotopeList,
            lstCategory = STR_RUCKSACK_EXP)
    end; // if assigned
    CheckButtons(lbAvailable, lbSelected, bbAdd, bbRemove, bbAddAll, bbClearAll);
  end else begin
    fndTaxaBiofinder.SearchMode := smMinChars; // default
    fndTaxaBioFinder.Text   :=''; // must do this last so we don't trigger premature search
  end;
  FReportChanged := True;
end;  // cmbCategoriesChange

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.fndTaxaBioFinderChange
//           Purpose: To update the list of available Items when a new partial
//                    Taxon or Biotope name is typed.
//      Dependencies: Uses the TFinder object
//      Side Effects: Resets the list of available Items.
//                    Makes sure that any Items already selected (which may
//                    appear in more than one list) are removed from the available list.
//     Special Notes: Most of this functionality is inherited.
//------------------------------------------------------------------------------
procedure TdlgWizard.fndTaxaBioFinderChange(Sender: TObject);
var lCount:Integer;
    lIdx  :Integer;
    lKey  :TKeyString;
begin
//  dmSearch.Terminate;
  inherited;
  with lbAvailable.Items do
    // Find and remove the already selected Items
    for lCount:= 0 to lbSelected.Items.Count - 1 do begin
      lIdx := IndexOf(lbSelected.Items[lCount]);
      // As there can be same taxa in multiple lists, check the key too
      lKey := TKeyData(lbSelected.Items.Objects[lCount]).ItemKey;
      if (lIdx <> -1) and (TKeyData(Objects[lIdx]).ItemKey=lKey) then
        Delete(lIdx);
    end;
  lblSelected.Caption  := Format(ResStr_Selected, [lbSelected.Items.Count]);
  CheckButtons(lbAvailable, lbSelected, bbAdd, bbRemove, bbAddAll, bbClearAll);
end;  // fndTaxonBioFinderChange

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.bbAddClick
//           Purpose: Moves available Taxon or Biotope into the selected
//------------------------------------------------------------------------------
procedure TdlgWizard.bbAddClick(Sender: TObject);
var lKeyObj : TWizardKeyObject;
    lListKey: TKeyString;
    lIdx    : Integer;
    lCursor : TCursor;
begin
  with lbAvailable do begin
    lCursor := HourglassCursor;
    Items.BeginUpdate;
    lbSelected.Items.BeginUpdate;
    try
      // Get the key once, instead of everytime in the loop
      lListKey := TKeyData(cmbCategories.Items.Objects[cmbCategories.ItemIndex]).ItemKey;
      for lIdx:=Items.Count-1 downto 0 do
        if Selected[lIdx] then begin
          //Create new instance of the object so the original can be freed;
          lKeyObj := TWizardKeyObject.Create;
          lKeyObj.ParentKey := lListKey;
          lKeyObj.Key       := TKeyData(Items.Objects[lIdx]).ItemKey;
          lKeyObj.Text      := Items[lIdx];
          lbSelected.Items.AddObject(lKeyObj.Text, lKeyObj);
          // Object in list is only pointer to object in Finder. Object freed by Finder!!!
          Items.Delete(lIdx);
        end;
    finally
      lbSelected.Items.EndUpdate;
      Items.EndUpdate;
      DefaultCursor(lCursor);
    end;
  end;
  lblSelected.Caption  := Format(ResStr_Selected, [lbSelected.Items.Count]);
  CheckButtons(lbAvailable, lbSelected, bbAdd, bbRemove, bbAddAll, bbClearAll);
  FReportChanged := True;
end;  // bbAddClick     

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.bbDesignationAddClick
//           Purpose: Moves available Designation into the selected list
//------------------------------------------------------------------------------
procedure TdlgWizard.bbDesignationAddClick(Sender: TObject);
var lKeyObj : TWizardKeyObject;
    lListKey: TKeyString;
    lIdx    : Integer;
    lCursor : TCursor;
begin
  with lbDesignationAvailable do begin
    lCursor := HourglassCursor;
    Items.BeginUpdate;
    lbDesignationSelected.Items.BeginUpdate;
    try
      // Get the key once, instead of everytime in the loop
      lListKey := TKeyData(cmbDesignationSet.Items.Objects[cmbDesignationSet.ItemIndex]).ItemKey;
      for lIdx:=Items.Count-1 downto 0 do
        if Selected[lIdx] then begin
          //Create new instance of the object so the original can be freed;
          lKeyObj := TWizardKeyObject.Create;
          lKeyObj.ParentKey := lListKey;
          lKeyObj.Key       := TKeyData(Items.Objects[lIdx]).ItemKey;
          lKeyObj.Text      := Items[lIdx];
          lbDesignationSelected.Items.AddObject(lKeyObj.Text, lKeyObj);
          // Object in list is only pointer to object in Finder. Object freed by Finder!!!
          Items.Delete(lIdx);
        end;
    finally
      lbDesignationSelected.Items.EndUpdate;
      Items.EndUpdate;
      DefaultCursor(lCursor);
    end;
  end;
  lblDesignationSelected.Caption  := Format(ResStr_Selected, [lbDesignationSelected.Items.Count]);
  CheckButtons(lbDesignationAvailable, lbDesignationSelected, bbDesignationAdd,
      bbDesignationRemove, bbDesignationsAddAll, bbDesignationClearAll);
  FReportChanged := True;
end;  // bbDesignationAddClick

//------------------------------------------------------------------------------
procedure TdlgWizard.lbAvailableKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=' ' then bbAddClick(nil);
end;  // lbAvailableKeyPress

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.bbRemoveClick
//           Purpose: Removes a selected taxon or biotope to the available list
//     Special Notes: Uses ReInsert in Finder to "restore" the item in the
//                    available list. It stops emptying AND repopulating the
//                    list each time an item is removed from the selection.
//------------------------------------------------------------------------------
procedure TdlgWizard.bbRemoveClick(Sender: TObject);
var lKeyObj: TWizardKeyObject;
    lIdx   : Integer;
    lCursor: TCursor;
begin
  with lbSelected do begin
    lCursor := HourglassCursor;
    Items.BeginUpdate;
    lbAvailable.Items.BeginUpdate;  // Will be updated through Finder component
    try
      for lIdx:=Items.Count-1 downto 0 do
        if Selected[lIdx] then begin
          lKeyObj:= TWizardKeyObject(Items.Objects[lIdx]);
          fndTaxaBioFinder.ReInsert(lKeyObj.Text,lKeyObj.Key);  // Reinsert in lbAvailable
          // Free this object
          lKeyObj.Free;
          Items.Objects[lIdx] := nil;
          // and remove item from list
          Items.Delete(lIdx);
        end;
    finally
      lbAvailable.Items.EndUpdate;
      Items.EndUpdate;
      DefaultCursor(lCursor);
    end;
  end;
  lblSelected.Caption  := Format(ResStr_Selected, [lbSelected.Items.Count]);
  CheckButtons(lbAvailable, lbSelected, bbAdd, bbRemove, bbAddAll, bbClearAll);
  FReportChanged := True;
end;  // bbRemoveClick 

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.bbDesignationRemoveClick
//           Purpose: Removes a selected designation to the available list
//     Special Notes: Uses ReInsert in Finder to "restore" the item in the
//                    available list. It stops emptying AND repopulating the
//                    list each time an item is removed from the selection.
//------------------------------------------------------------------------------
procedure TdlgWizard.bbDesignationRemoveClick(Sender: TObject);
var lKeyObj: TWizardKeyObject;
    lNewKey: TKeyData;
    lIdx   : Integer;
    lCursor: TCursor;
begin
  with lbDesignationSelected do begin
    lCursor := HourglassCursor;
    Items.BeginUpdate;
    lbDesignationAvailable.Items.BeginUpdate;  // Will be updated through Finder component
    try
      for lIdx:=Items.Count-1 downto 0 do
        if Selected[lIdx] then begin
          lKeyObj:= TWizardKeyObject(Items.Objects[lIdx]);
          //Check this item belongs with the item in the combobox;
          if cmbDesignationSet.ItemIndex > -1 then
            if lKeyObj.ParentKey = TKeyData(cmbDesignationSet.Items.Objects[cmbDesignationSet.ItemIndex]).ItemKey then
            begin
              //Create a new object of TSurveyNode as in source list.
              lNewKey := TKeyData.Create;
              lNewKey.ItemKey  := lKeyObj.Key;
              //Add the object to the source list;
              lbDesignationAvailable.Items.AddObject(lKeyObj.Text, lNewKey);
              lbDesignationAvailable.Sorted:=True;  // Reorder
            end;
          // Free this object
          lKeyObj.Free;
          Items.Objects[lIdx] := nil;
          // and remove item from list
          Items.Delete(lIdx);
        end;
    finally
      lbDesignationAvailable.Items.EndUpdate;
      Items.EndUpdate;
      DefaultCursor(lCursor);
    end;
  end;
  lblDesignationSelected.Caption  := Format(ResStr_Selected, [lbDesignationSelected.Items.Count]);
  CheckButtons(lbDesignationAvailable, lbDesignationSelected, bbDesignationAdd,
      bbDesignationRemove, bbDesignationsAddAll, bbDesignationClearAll);
  FReportChanged := True;
end;  // bbDesignationRemoveClick

//------------------------------------------------------------------------------
procedure TdlgWizard.lbSelectedKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=' ' then bbRemoveClick(nil)
end;  // lbSelectedKeyPress

//------------------------------------------------------------------------------
procedure TdlgWizard.bbAddAllClick(Sender: TObject);
var lIdx   : Integer;
    lCursor: TCursor;
begin
  inherited;
  with lbAvailable do begin
    lCursor := HourglassCursor;
    Items.BeginUpdate;
    try
      for lIdx:=0 to Items.Count-1 do Selected[lIdx]:=True;
    finally
      Items.EndUpdate;
      DefaultCursor(lCursor);
    end;
  end;
  bbAddClick(nil);
end;  // bbAddAllClick    

//------------------------------------------------------------------------------
procedure TdlgWizard.bbDesignationsAddAllClick(Sender: TObject);
var lIdx   : Integer;
    lCursor: TCursor;
begin
  inherited;
  with lbDesignationAvailable do begin
    lCursor := HourglassCursor;
    Items.BeginUpdate;
    try
      for lIdx:=0 to Items.Count-1 do Selected[lIdx]:=True;
    finally
      Items.EndUpdate;
      DefaultCursor(lCursor);
    end;
  end;
  bbDesignationAddClick(nil);
end;  // bbDesignationsAddAllClick

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.bbClearAllClick
//           Purpose: Removes all the selected Taxa and/or Biotopes from the
//                    selected list to the available list.
//     Special Notes: Uses the ReInsert functioin in Finder to "restore" the
//                    selected items in the available list.
//------------------------------------------------------------------------------
procedure TdlgWizard.bbClearAllClick(Sender: TObject);
var lIdx   : Integer;
    lCursor: TCursor;
begin
  with lbSelected do begin
    lCursor := HourglassCursor;
    Items.BeginUpdate;
    try
      for lIdx:=0 to Items.Count-1 do Selected[lIdx]:=True;
    finally
      Items.EndUpdate;
      DefaultCursor(lCursor);
    end;
  end;
  bbRemoveClick(nil);
end;  // bbClearAllClick    

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.bbClearAllClick
//           Purpose: Removes all the selected Designations from the
//                    selected list to the available list.
//     Special Notes: Uses the ReInsert functioin in Finder to "restore" the
//                    selected items in the available list.
//------------------------------------------------------------------------------
procedure TdlgWizard.bbDesignationClearAllClick(Sender: TObject);
var lIdx   : Integer;
    lCursor: TCursor;
begin
  with lbDesignationSelected do begin
    lCursor := HourglassCursor;
    Items.BeginUpdate;
    try
      for lIdx:=0 to Items.Count-1 do Selected[lIdx]:=True;
    finally
      Items.EndUpdate;
      DefaultCursor(lCursor);
    end;
  end;
  bbDesignationRemoveClick(nil);
end;  // bbDesignationClearAllClick

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.lbSelectedClick
//           Purpose: Updates button states when the list is clicked
//------------------------------------------------------------------------------
procedure TdlgWizard.lbSelectedClick(Sender: TObject);
begin
  CheckButtons(lbAvailable, lbSelected, bbAdd, bbRemove, bbAddAll, bbClearAll);
end;  // lbSelectedClick

//==============================================================================
//==============================================================================
//    Procedure Name: TdlgWizard.fndPlaceFinderChange
//           Purpose: Initialise the list of available places given a partial place name
//      Dependencies: Uses TFinder
//      Side Effects: Removes places already selected from the available list
//------------------------------------------------------------------------------
procedure TdlgWizard.fndPlaceFinderChange(Sender: TObject);
var lIdx,lCount:Integer;
begin
  inherited;
  with lbPlacesAvailable.Items do
    for lCount:= 0 to lbPlacesSelected.Items.Count - 1 do begin
      lIdx:= IndexOf(lbPlacesSelected.Items[lCount]);
      if lIdx<>-1 then
        Delete(lIdx);
    end;
  CheckButtons(lbPlacesAvailable, lbPlacesSelected, bbPlacesAdd, bbPlacesRemove, bbPlacesAddAll, bbPlacesClearAll);
end;  // fndPlaceFinderChange

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.bbPlacesAddClick
//                    TdlgWizard.bbPlacesRemoveClick
//                    TdlgWizard.bbPlacesClearAllClick
//           Purpose: To move place names between the available and selected lists
//                    as appropriate. bbPlacesClearAllClick clears all entries selected.
//------------------------------------------------------------------------------
procedure TdlgWizard.bbPlacesAddClick(Sender: TObject);
var lKeyObj: TWizardKeyObject;
    lIdx   : Integer;
    lCursor: TCursor;
begin
  with lbPlacesAvailable do begin
    lCursor := HourglassCursor;
    Items.BeginUpdate;
    lbPlacesSelected.Items.BeginUpdate;
    try
      for lIdx:=Items.Count-1 downto 0 do
        if Selected[lIdx] then begin
          lKeyObj:= TWizardKeyObject.Create;
          lKeyObj.ParentKey:=TKeyData(Items.Objects[lIdx]).ItemKey;
          lKeyObj.Key      :=lKeyObj.ParentKey;
          lKeyObj.Text     :=Items[lIdx];
          lbPlacesSelected.Items.AddObject(lKeyObj.Text,lKeyObj);
          Items.Delete(lIdx);
        end;
    finally
      lbPlacesSelected.Items.EndUpdate;
      Items.EndUpdate;
      DefaultCursor(lCursor);
    end;
  end;
  CheckButtons(lbPlacesAvailable, lbPlacesSelected, bbPLacesAdd, bbPlacesRemove, bbPlacesAddAll, bbPlacesClearAll);
  FReportChanged := True;
end;  // bbPlacesAddClick

//------------------------------------------------------------------------------
procedure TdlgWizard.lbPlacesAvailableKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=' ' then bbPlacesAddClick(nil);
end;  // lbPlacesAvailableKeyPress

//------------------------------------------------------------------------------
procedure TdlgWizard.bbPlacesRemoveClick(Sender: TObject);
var lKeyObj: TWizardKeyObject;
    lIdx   : Integer;
    lCursor: TCursor;
begin
  with lbPlacesSelected do begin
    lCursor := HourglassCursor;
    Items.BeginUpdate;
    lbPlacesAvailable.Items.BeginUpdate;  // Updated through Finder component
    try
      for lIdx:=Items.Count-1 downto 0 do
        if Selected[lIdx] then begin
          lKeyObj:=TWizardKeyObject(Items.Objects[lIdx]);
          fndPlaceFinder.ReInsert(lKeyObj.Text,lKeyObj.Key);
          lKeyObj.Free;
          Items.Objects[lIdx]:=nil;
          Items.Delete(lIdx);
        end;
    finally
      lbPlacesAvailable.Items.EndUpdate;
      Items.EndUpdate;
      DefaultCursor(lCursor);
    end;
  end;
  CheckButtons(lbPlacesAvailable, lbPlacesSelected, bbPLacesAdd, bbPlacesRemove, bbPlacesAddAll, bbPlacesClearAll);
  FReportChanged := True;
end;  // bbPlacesRemoveClick

//------------------------------------------------------------------------------
procedure TdlgWizard.lbPlacesSelectedKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=' ' then bbPlacesRemoveClick(nil);
end;  // lbPlacesSelectedKeyPress

//------------------------------------------------------------------------------
procedure TdlgWizard.bbPlacesAddAllClick(Sender: TObject);
var lIdx   : Integer;
    lCursor: TCursor;
begin
  inherited;
  with lbPlacesAvailable do begin
    lCursor := HourglassCursor;
    Items.BeginUpdate;
    try
      for lIdx:=0 to Items.Count-1 do Selected[lIdx]:=True;
    finally
      Items.EndUpdate;
      DefaultCursor(lCursor);
    end;
  end;
  bbPlacesAddClick(nil);
end;  // bbPlacesAddAllClick

//------------------------------------------------------------------------------
procedure TdlgWizard.bbPlacesClearAllClick(Sender: TObject);
var lIdx   : Integer;
    lCursor: TCursor;
begin
  with lbPlacesSelected do begin
    lCursor := HourglassCursor;
    Items.BeginUpdate;
    try
      for lIdx:=0 to Items.Count-1 do Selected[lIdx]:=True;
    finally
      Items.EndUpdate;
      DefaultCursor(lCursor);
    end;
  end;
  bbPlacesRemoveClick(nil);
end;  // bbPlacesClearAllClick

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.lbPlacesSelectedClick
//           Purpose: Updates button states when the list is clicked
//------------------------------------------------------------------------------
procedure TdlgWizard.lbPlacesSelectedClick(Sender: TObject);
begin
  inherited;
  CheckButtons(lbPlacesAvailable, lbPlacesSelected, bbPLacesAdd, bbPlacesRemove, bbPlacesAddAll, bbPlacesClearAll);
end;  // lbPlacesSelectedClick

//==============================================================================
//==============================================================================
//    Procedure Name: TdlgWizard.bbMapClick
//           Purpose: To allow the User to select an area on the map which is
//                    then used to select locations for display
//      Dependencies: Map unit and uses SetupLink from BaseFormUnit
//                    Directly uses dlgWizard.Parent
//      Side Effects: Hides the wizard, unhidden in TransferDone procedure
//     Special Notes: This routine does NOT wait for the area to be selected
//                    before returning. The wizard is hidden and when the transfer
//                    is complete, it is made visible again.
//------------------------------------------------------------------------------
procedure TdlgWizard.btnGetBoundingBoxClick(Sender: TObject);
begin
  RequestMapForReturnData(AppSettings.AvailableMaps.DefaultMap.BaseMapKey,
                          UpdateBoundingBox, True, True);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgWizard.btnGetBoundingBoxDropDownClick(Sender: TObject);
var
  lPos: TPoint;
begin
  inherited;
  lPos := btnGetBoundingBox.ClientToScreen(Point(0, btnGetBoundingBox.Height));
  pmMapForBoundingBox.Popup(lPos.X, lPos.Y);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgWizard.MapForBoundingBoxClick(Sender: TObject);
begin
  RequestMapForReturnData(AppSettings.AvailableMaps[TMenuItem(Sender).Tag].BaseMapKey,
                          UpdateBoundingBox, True, True);
end;

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.UpdateBoundingBox
//           Purpose: To put the selected area references into the edit boxes on
//                    the dialog
//        Parameters: KeyList - a TKeylist which contains the two references NE
//                    followed by SW
//------------------------------------------------------------------------------
procedure TdlgWizard.UpdateBoundingBox(KeyList: TKeyList);
begin
  try
    if Assigned(KeyList) and (KeyList.Header.ItemCount > 0) then
      if CompareText(KeyList.Header.TableName,'SPATIAL_REF') = 0 then
      begin
        FValidEnteredNE := KeyList.Items[0].KeyField2;
        eNECorner.Text  := FValidEnteredNE;
        FValidEnteredSW := KeyList.Items[1].KeyField2;
        eSWCorner.Text  := FValidEnteredSW;
        FSpatialSystemForBoundingBox := AppSettings.AvailableMaps.ItemsByKey[MapWindow.BaseMapKey].SpatialSystem;
      end;
  finally
    KeyList.Free;
  end;

  if FMapAlreadyPresent then begin
    // Post message to show Wizard, without closing map.
    PostMessage(MapWindow.FilterResultScreen.Handle,WM_RUN_WIZARD,0,0);
    MapWindow.RefreshMap;
  end else begin
    MapWindow.Release;  // Get rid of the map window
    frmMain.Repaint;
  end;
end; // UpdateBoundingBox

//==============================================================================
//      Procedure Name: TdlgWizard.btnAdvancedClick
//             Purpose: Handle the click event of the additional filter button.
//                      What happens depends on installed addins
//       Special Notes: If a COM form is installed for the advanced filter, then
//                      a special handler is called (DoComAdditionalFilter).
//------------------------------------------------------------------------------
procedure TdlgWizard.btnAdvancedClick(Sender: TObject);
begin
  inherited;
  if AppSettings.ComAddins.HasAdditionalFilter then
    DoComAdditionalFilter
  else
    DoStandardAdditionalFilter;
end;  // btnAdvancedClick

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.DoComAdditionalFilter
//           Purpose: Displays the COM filter formand then gathers the filters from it.
//      Dependencies: Uses TdlgAddFilterContainer
//      Side Effects: Populates FAdvancedFilter list with TFilter Objects
//     Special Notes: Gets field names from the COM form and looks up other
//                    information directly from the database USABLE_FIELDS table.
//------------------------------------------------------------------------------
procedure TdlgWizard.DoComAdditionalFilter;
var lDialogContainer:TdlgAddFilterContainer;
    lCount          :Integer;
    lFilter         :TFilter;
    lComFilter      :IAdditionalFilter;
begin
  lDialogContainer:=TdlgAddFilterContainer.Create(Self,
                        AppSettings.ComAddins.AdditionalFilterClass);
  if lDialogContainer.ShowModal = mrOK then
  begin
    for lCount:= 0 to lDialogContainer.FilterList.Count-1 do
    begin
      lComFilter := IAdditionalFilter(lDialogContainer.FilterList.Items[lCount]);
      lFilter := TFilter.Create;
      with lFilter, FdmWizard.qryFieldFinder do begin
        FieldName:=lComFilter.FieldKey;
        // find the field name from the key
        SQL.Clear;
        SQL.Add('SELECT * FROM Usable_Field WHERE Usable_Field_Key = :ParamKey');
        Parameters.ParambyName('ParamKey').Value := lComFilter.FieldKey;
        Open;
        First;
        FieldName            :=FieldByName('Field_Name').AsString;
        ReportField.TableName:=FieldByName('Table_Name').AsString;
        // Following function is declared in ComplexFilter unit but
        // is not associated with a particular class
        Condition:=EncodeCondition(lComFilter.Condition);
        Criteria :=lComFilter.Criteria;
        Close;
      end; // with lFilter, FdmWizard.qryFieldFinder
      FAdvancedFilter.Add(lFilter);
    end;
  end;
  lDialogContainer.Free;
end;  // DoComAdditionalFilter

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.DoStandardAdditionalFilter
//           Purpose: Display the standard (non-COM) additional filter dialog
//      Side Effects: Populates the available fields list in the dialog before display
//     Special Notes: The dialog is created using an existing filter list as a
//                    parameter. This allows the dialog to show the User
//                    anything they have already filtered on.
//------------------------------------------------------------------------------
procedure TdlgWizard.DoStandardAdditionalFilter;
var ldlgComplex :TdlgComplexFilter;
    lCount      :Integer;
    lReportField:TReportField;
begin
  ldlgComplex :=TdlgComplexFilter.Create(Self, FAdvancedFilter, FSettings);
  with ldlgComplex do begin
    // Clear Items only, Objects are dealt with in FSelectFields
    lbAvailableFields.Items.Clear;
    for lCount := 0 to FFilterFields.Count - 1 do begin
      lReportField:=TReportField(FFilterFields.Items[lCount]);
      if lReportField.Filter then // and lReportField.Selectable
        lbAvailableFields.Items.AddObject(lReportField.DisplayName,lReportField);
    end;
    ShowModal;
  end; // with
  ldlgComplex.Free;
  // 4 for the constraints
  if FAdvancedFilter.Count <= 4 then
    lblFilterApplied.Visible := False
  else
    lblFilterApplied.Visible := True;
end;  // DoStandardAdditionalFilter



//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.bbAscendingClick
//                    TdlgWizard.bbDescendingClick
//           Purpose: Makes sure that the sort priority and direction are set
//                    for an item when the user clicks the sort order button
//      Side Effects: Changes the tag on lbSortAttributes to reflect the number
//                    of fields selected for sorting.
//                    SortOrder number is set in the list box field object
//     Special Notes: Only 9 fields may be selected for sorting.
//------------------------------------------------------------------------------
procedure TdlgWizard.bbAscendingClick(Sender: TObject);
begin
  AssignSortToSelectedItem(stAsc);
end;  // bbAscendingClick

{===============================================================================
 Description : As above for descending
 Created : unknown }
procedure TdlgWizard.bbDescendingClick(Sender: TObject);
begin
  AssignSortToSelectedItem(stDesc);
end;  // bbDescendingClick

{===============================================================================
 Description : Takes the selected attribute in the sort list box, and assigns
     a given sort direction to it.  Works out the sort order.
 Created : 5/12/2002 }
procedure TdlgWizard.AssignSortToSelectedItem(Sort : TSortType);
var
  liAttrIndex : Integer;
  lAttribute : TAttribute;
begin
  inherited;
  with tvSort do
    if Assigned(Selected) then
      if Assigned(Selected.Data) then
      begin
        // if initially not sorted, then assign an index
        if TAttribute(Selected.Data).Sort=stNone then
          TAttribute(Selected.Data).SortOrder := FdmWizard.ReportGenerator.SortAttributeCount+1
        else if (TAttribute(Selected.Data).Sort<>stNone) and (Sort=stNone) then // removing a sort
          // Update remaining SortOrders
          for liAttrIndex:= 0 to FdmWizard.ReportGenerator.Attributes.Count-1 do begin
            lAttribute:=TAttribute(FdmWizard.ReportGenerator.Attributes.Objects[liAttrIndex]);
            // check each item for a sort order
            // check if this was in the order AFTER the removed item
            if lAttribute.SortOrder>TAttribute(Selected.Data).SortOrder then
              lAttribute.SortOrder:=lAttribute.SortOrder -1;
          end; // check for existing sort orders
        TAttribute(tvSort.Selected.Data).Sort := Sort;
        case Sort of
          stNone : tvSort.Selected.ImageIndex := BLANK_IMG_IDX; // blank
          stAsc : tvSort.Selected.ImageIndex := SORT_UP_IMG_IDX;
          stDesc : tvSort.Selected.ImageIndex := SORT_DOWN_IMG_IDX;
        end;
        tvSort.Selected.SelectedIndex := tvSort.Selected.ImageIndex;

        FReportChanged := True;
      end;
end;

//------------------------------------------------------------------------------
//    Procedure Name: TdlgWizard.bbNoSortClick
//           Purpose: Clears the sort order and reduces the priority of any
//                    subsequent sorted fields
//      Side Effects: Reduces the tag for lbSortAttributes.
//                    Resets the list box field object's SortOrder property
//     Special Notes: This routine will cycle through all the sortable fields
//                    and reduce the sort priority of any which are higher than
//                    that which is being deselected.
//------------------------------------------------------------------------------
procedure TdlgWizard.bbNoSortClick(Sender: TObject);
var lAttribute :TAttribute;
begin
  inherited;
  if Assigned(tvSort.Selected) then
    if Assigned (tvSort.Selected.Data) then
    begin
      lAttribute:=TAttribute(tvSort.Selected.Data);
      if lAttribute.Sort<>stNone then begin
        // Get the SortOrder so the ones above can be decreased by one
        AssignSortToSelectedItem(stNone);
        tvSort.Repaint;
    end;
  end;
end;  // bbNoSortClick


//==============================================================================
//    Procedure Name: TdlgWizard.FinishWizard
//           Purpose: To build and run the appropriate SQL according to the
//                    user's choices
//      Dependencies: The whole of the wizard tab sheet
//      Side Effects: Sets the TitleText property. Saves the SQL in a file for
//                    later review if required
//     Special Notes: Uses BuildSpeciesReport, BuildBiotopesReport and
//                    BuildPlaceReport to produce the correct SQL.
//                    This is then run and the dialog is closed
//------------------------------------------------------------------------------
procedure TdlgWizard.FinishWizard;
var
  topNode: TFlyNode;
  cursor: TCursor;

  //----------------------------------------------------------------------------
  procedure GetPolygons(const AMapKey: String; AParentNode: TFlyNode);
  var
    layerNode, polygonNode: TFlyNode;
  begin
    if Assigned(AParentNode) then layerNode := AParentNode.GetFirstChild
                             else layerNode := tvPolygonLayers.Items.GetFirstNode;
    while Assigned(layerNode) do begin
      if layerNode.ImageIndex <> UNCHECKED_IMG_IDX then begin
        polygonNode := layerNode.GetFirstChild;
        while Assigned(polygonNode) do begin
          if polygonNode.ImageIndex = CHECKED_IMG_IDX then begin
            dmWizard.ReportGenerator.AddToPolygonSelection(
                AMapKey,
                TKeyData(layerNode.Data).ItemKey,
                Integer(polygonNode.Data));
          end;
          polygonNode := polygonNode.GetNextSibling;
        end;
      end;
      layerNode := layerNode.GetNextSibling;
    end;
  end;
  //----------------------------------------------------------------------------

begin
  cursor := HourglassCursor;
  try
    // store default setting of checkbox
    AppSettings.ExpandTaxaInWizard := cbExpandTaxa.Checked;
    if rbSpeciesReport.Checked then
      FSettings.FilterSQL := BuildSpeciesReport
    else if rbBiotopesReport.Checked then
      FSettings.FilterSQL := BuildBiotopesReport
    else if rbPlaceReport.Checked then
      FSettings.FilterSQL := BuildPlaceReport;

    // Store all about selected polyons, if any
    dmWizard.ReportGenerator.PolygonSelection.Clear;
    FSettings.PolygonIncludeOverlap := False;
    if cbRestrictPolygons.Checked then begin
      FSettings.PolygonIncludeOverlap := rbPolygonIncludeSquares.Checked;
      // If one map, top node is NOT a base map.
      if AppSettings.AvailableMaps.Count = 1 then
        GetPolygons(AppSettings.AvailableMaps[0].BaseMapKey, nil)
      else begin
        topNode := tvPolygonLayers.Items.GetFirstNode;
        while Assigned(topNode) do begin
          if topNode.ImageIndex <> UNCHECKED_IMG_IDX then
            GetPolygons(TKeyData(topNode.Data).ItemKey, topNode);
          topNode := topNode.GetNextSibling;
        end;
      end;
    end;

    //Stores the designations which are selected
    if cbRestrictDesignations.Checked then
      dmWizard.ReportGenerator.DesignationTypes := lbDesignationSelected.Items;

    ModalResult:=mrOK;
    TitleText  :=BuildTitle;
  finally
    DefaultCursor(cursor);
    FReportGenerated := True;
  end;
end;  // FinishWizard

//==============================================================================
//    Procedure Name: TdlgWizard.AddTableToQuery
//           Purpose: This makes sure that all tables required for linkage across
//                    the database are in the "required tables" list.
//        Parameters: iTableLinkInfo - this contains the information about which
//                    table links to which and on what fields
//      Dependencies: TTableLinkInfo Objects are used and should have been
//                    populated (in WizardData)
//------------------------------------------------------------------------------
procedure TdlgWizard.AddTableToQuery(iTableLinkInfo:TTableLinkInfo;
  const iAvailableTables:TStringList; ioRequiredTables:TStringList);
begin
  // if not already in the list of selected tables, add the table
  if ioRequiredTables.IndexOf(iTableLinkInfo.TableName) = -1 then
  begin
    ioRequiredTables.AddObject(iTableLinkInfo.TableName, iTableLinkInfo);
    // Find if we need another table to which the current is linked
    if (iTableLinkInfo.LinkTable <> '') then
      // check that the specified link table is present in the list supplied
      AddNamedTableToQuery(iTableLinkInfo.LinkTable, iAvailableTables, ioRequiredTables);
  end;
end;  // AddTableToQuery

//==============================================================================
//      Procedure Name: TdlgWizard.CheckSources
//             Purpose: To add the appropriate "in" clause to the where clause
//                      for Sources and the appropriate fields for the Source
//                      key selection
//          Parameters: iAvailableTables - the list of all possible tables
//                      ioRequiredTables - The list of tables known to be
//                                         required SO FAR
//                      ioSelect - The select clause so far
//                      ioWhere - the where clause so far
//        Side Effects: Adds appropriate table names and fields to the table
//                      lists and select
//------------------------------------------------------------------------------
procedure TdlgWizard.CheckSources(const iAvailableTables:TStringList;
  var ioRequiredTables:TStringList; const iSelect:String; var ioWhere:String);
var lTblIdx, i: Integer;
    lLatLong      : TLatLong;
    lBoundingBox  : TValidBoundingBox;
    lMapServerLink: TMapServerLink;
    lTopNode      : TFlyNode;
    liTblIndex    : Integer;
    lFormatSettings: TFormatSettings;
    lSurveyKeys: TStringList;   

  //----------------------------------------------------------------------------
  procedure PutKeysInMultipleTables(AItems: TStrings; tableNamesKeys: Array of String);
  var i, j: integer;
  begin
    AddToWhereClause(ioWhere, '(');
    for i := 0 to Length(tableNamesKeys) div 2 - 1 do
      with AItems do begin
        if i > 0 then ioWhere := ioWhere + ' OR ';
        ioWhere := ioWhere + '(';
        ioWhere := ioWhere + tableNamesKeys[i * 2 + 1] + ' IN (''' + TWizardKeyObject(Objects[0]).Key + '''';
        for j := 1 to Count - 1 do
          ioWhere := ioWhere + ',''' + TWizardKeyObject(Objects[j]).Key + '''';
        ioWhere := ioWhere + '))';
        lTblIdx := iAvailableTables.IndexOf(Uppercase(tableNamesKeys[i * 2]));
        if lTblIdx > -1 then
          AddNamedTableToQuery(tableNamesKeys[i * 2], iAvailableTables, ioRequiredTables);
      end;
    ioWhere := ioWhere + ')';
  end;  // PutKeysInSql

  //----------------------------------------------------------------------------
  procedure PutKeysInSql(AItems: TStrings; const ATableName, AKeyField:String);
  begin
    PutKeysInMultipleTables(AItems, [ATableName, AKeyField]);
  end;  // PutKeysInSql

  //----------------------------------------------------------------------------
  procedure GetSamplesFromPolygons(AParentNode: TFLyNode; const BaseMapSpatialSystem: String);
  var lSNode, lPNode: TFlyNode;
      lScanner: TPolygonScanner;
  begin
    if Assigned(AParentNode) then lSNode := AParentNode.GetFirstChild
                             else lSNode := tvPolygonLayers.Items.GetFirstNode;
    while Assigned(lSNode) do begin
      if lSNode.ImageIndex <> UNCHECKED_IMG_IDX then begin
        // At least one polygon on this layer, so create the PolygonScanner, we'll need it.
        lScanner := TPolygonScanner.Create(lMapServerLink.MapHandle, BaseMapSpatialSystem);
        try
          lPNode := lSNode.GetFirstChild;
          while Assigned(lPNode) do begin
            frmMain.SetStatus(Format(ResStr_ObtainingSampleForPoly,[lPNode.Caption]));
            if lPNode.ImageIndex = CHECKED_IMG_IDX then begin
              lScanner.GetSamplesForPolygon(
                      lMapServerLink.SheetIDByFileName(AppSettings.ObjectSheetFilePath +
                                                       TKeyData(lSNode.Data).ItemAdditional),
                      Integer(lPNode.Data),
                      dmWizard.connReport,
                      rbPolygonIncludeSquares.Checked);
            end;
            // Next polygon node.
            lPNode := lPNode.GetNextSibling;
          end;
        finally
          lScanner.Free;
        end;
      end;
      // Next map sheet node.
      lSNode := lSNode.GetNextSibling;
    end;
  end;
  //----------------------------------------------------------------------------

begin
  //Check if the user selected a sub-set of Sources.
  if cbRestrictSource.Checked then
  begin
    with lbSourcesSelected.Items do
      if Count > 0 then begin
        if rbSourcesIndividuals.Checked then
        begin
          liTblIndex := iAvailableTables.IndexOf(TN_SURVEY_EVENT_RECORDER);
          if liTblIndex > -1 then
          begin
            with TTableLinkInfo(iAvailableTables.Objects[liTblIndex]) do
              PutKeysInSQL(lbSourcesSelected.Items, TN_SURVEY_EVENT_RECORDER, 'NAME_KEY');
            AddTableToQuery(TTableLinkInfo(iAvailableTables.Objects[liTblIndex]),
                            iAvailableTables, ioRequiredTables);
          end;
        end else
        if rbSourcesReferences.Checked then
          PutKeysInSql(lbSourcesSelected.Items, TN_REFERENCE, 'Reference.Source_Key')
        else
        if rbSourcesSurveys.Checked then
          PutKeysInSql(lbSourcesSelected.Items, TN_SURVEY, 'Survey.Survey_Key')
        else
        if rbSourcesTags.Checked then begin
          lSurveyKeys := TStringList.Create;
          try
            for i := 0 to Count - 1 do 
              with dmDatabase.GetRecordset(
                  'usp_Surveys_Select_ForSurveyTag',
                  ['@Key', TWizardKeyObject(Objects[i]).Key, '@UserNameKey', AppSettings.UserID]) do
              begin
                while not Eof do begin
                  if lSurveyKeys.IndexOf(Fields['Survey_Key'].Value) = -1 then
                    lSurveyKeys.AddObject(
                        Fields['Survey_Key'].Value,
                        TWizardKeyObject.Create(Fields['Survey_Key'].Value));
                  MoveNext;
                end;
                Close;
              end;
            // If there are no surveys for the tag(s), add a fake key to filter everything.
            if lSurveyKeys.Count = 0 then lSurveyKeys.AddObject('', TWizardKeyObject.Create(''));
            PutKeysInSql(lSurveyKeys, TN_SURVEY, 'Survey.Survey_Key');
            for i := 0 to lSurveyKeys.Count - 1 do
              lSurveyKeys.Objects[i].Free;
          finally
            lSurveyKeys.Free;
          end;
        end;
      end;
  end; // check if source selection required

  //Check if the user selected a sub-set of Areas.
  if cbRestrictArea.Checked then
    if lbAdminSelected.Items.Count > 0 then
      PutKeysInMultipleTables(lbAdminSelected.Items, ['Admin_Area ALoc', 'ALoc.Admin_Area_Key',
                                                      'Admin_Area ASam', 'ASam.Admin_Area_Key']);

  if cbRestrictBounds.Checked then begin
    // we have some Sources so set up the where clause
    AddToWhereClause(ioWhere, '(');
    // Get the formatting and see if the bounding box is actually a valid one
    lBoundingBox := CheckBoundingBox(FValidEnteredSW, FValidEnteredNE, DetermineSpatialRefSystem(FValidEnteredSW));
    if lBoundingBox.Valid then begin
      GetLocaleFormatSettings(LOCALE_USER_DEFAULT, lFormatSettings);
      lFormatSettings.DecimalSeparator := '.';
      // Get the South-West corner
      lLatLong := ConvertToLatLong(lBoundingBox.FormattedSouthWestSR,AppSettings.SpatialRefSystem);
      ioWhere := ioWhere + 'Round(Sample.Lat,6) >= Round(' + FloatToStr(lLatLong.Lat, lFormatSettings) + ',6)' +
                           ' AND Round(Sample.Long,6) >= Round(' + FloatToStr(lLatLong.Long, lFormatSettings) + ',6)';
      // Get the North-East corner
      lLatLong := ConvertToLatLong(lBoundingBox.FormattedNorthEastSR,AppSettings.SpatialRefSystem);
      ioWhere := ioWhere + ' AND Round(Sample.Lat,6) <= Round(' + FloatToStr(lLatLong.Lat, lFormatSettings) + ',6)' +
                           ' AND Round(Sample.Long,6) <= Round(' + FloatToStr(lLatLong.Long, lFormatSettings) + ',6))';

      //Check for restriction by bounding box, and add Sample table if needed
      AddNamedTableToQuery(TN_SAMPLE, iAvailableTables,ioRequiredTables);
    end;
  end;

  if cbRestrictPolygons.Checked then begin
    // we have some Sources so set up the where clause
    AddToWhereClause(ioWhere, '(Sample.Sample_Key IN (');
    lMapServerLink := TMapServerLink.Create(nil);
    try
      // If one map, top node is NOT a base map.
      if AppSettings.AvailableMaps.Count = 1 then
        with AppSettings.AvailableMaps[0] do begin
          lMapServerLink.ActiveDataset := BaseMapKey + '.gds';
          GetSamplesFromPolygons(nil, SpatialSystem);
        end
      else begin
        lTopNode := tvPolygonLayers.Items.GetFirstNode;
        while Assigned(lTopNode) do begin
          if lTopNode.ImageIndex <> UNCHECKED_IMG_IDX then
            with TKeyData(lTopNode.Data) do begin
              lMapServerLink.ActiveDataset := ItemKey + '.gds';
              GetSamplesFromPolygons(lTopNode,
                                     AppSettings.AvailableMaps.ItemsByKey[ItemKey].SpatialSystem);
            end;
          lTopNode := lTopNode.GetNextSibling;
        end;
      end;
    finally
      lMapServerLink.Free;
    end;

    ioWhere := ioWhere + 'SELECT RecordKey FROM #Templist' + '))';

    // Add Sample table if needed
    AddNamedTableToQuery(TN_SAMPLE, iAvailableTables, ioRequiredTables);
  end;
end;  // CheckSources


//==============================================================================
//    Procedure Name: TdlgWizard.DoAdvancedFilterTables
//           Purpose: To put the table required for any advanced filters into
//                    the required list
//        Parameters: iAvailableTables - all available tables information
//                    ioRequiredTables - list of tables required so far, updated
//                    as required
//     Special Notes: If a filter contains the word "CONSTRAINT" as its table
//                    name, it is necessary to include specific tables dependent
//                    upon the radio buttons This ensures that, even if unions
//                    are required, the tables are properly included for the
//                    constraints to work.
//------------------------------------------------------------------------------
procedure TdlgWizard.DoAdvancedFilterTables(const iSelect:String;
  const iAvailableTables:TStringList; var ioRequiredTables:TStringList;
  const itfTaxon:Boolean);
var lCount:Integer;
    lField        :TReportField;
    lsTableName   :String;
    ltfInd, ltfOrg:Boolean;
begin
  if FAdvancedFilter.Count > 0 then
  begin
    // Now for each of the specified conditions
    for lCount := 0 to FAdvancedFilter.Count-1 do
    begin
      // Set a local field object to save typing
      lField :=  TFilter(FAdvancedFilter.Items[lCount]).Reportfield ;
      // Add the fieldname preposition
      // Special case is when the table name is CONSTRAINT
      if lField.TableName <> 'CONSTRAINT' then begin
        ltfInd:=(Pos('IDET',UpperCase(iSelect))>0) or (Pos('IRUNBY',UpperCase(iSelect))>0);
        ltfOrg:=(Pos('ODET',UpperCase(iSelect))>0) or (Pos('ORUNBY',UpperCase(iSelect))>0);

        // Exclude Biotope tables when dealing with Taxon, and vice versa.
        // also exclude individual tables when dealing with orgs and vice versa
        if ((ltfInd and not (lField.TableName[1]='O')) or
            (ltfOrg and not (lField.TableName[1]='I')) or
            (not ltfInd and not ltfOrg)) and
           ((itfTaxon and (Pos('BIOTOPE',lField.TableName)=0)) or
            (not itfTaxon and (Pos('TAXON',lField.TableName)=0))) then
          AddNamedTableToQuery(lField.TableName, iAvailableTables, ioRequiredTables);
      end else begin
        if itfTaxon then lsTableName:=TN_TAXON_OCCURRENCE
                    else lsTableName:=TN_BIOTOPE_OCCURRENCE;
        AddNamedTableToQuery(lsTableName, iAvailableTables, ioRequiredTables);
      end;
    end;  // for...
  end;  // if...
end;  // DoAdvancedFilterTables

//==============================================================================
//    Procedure Name: TdlgWizard.BuildFrom
//           Purpose: To put the required tables into the form clause
//        Parameters: ioFrom - the from clause to be built (should start empty)
//      Side Effects: Existing from clause (there should not be one!) is overwritten
//------------------------------------------------------------------------------
procedure TdlgWizard.BuildFrom(var ioFrom:String; const iRequiredTables:TStringList);
var
  lCount     : Integer;
  lJoinChain : String;
  lTableInfo : TTableLinkInfo;
  lslOrderedTables : TStringList;
begin
  lJoinChain := ''; // holds the tables joined together - precursor to ioFrom

  // Create stringlist to store the order for the joins between tables
  lslOrderedTables:= TStringList.Create;
  lslOrderedTables.Sorted     := True;
  lslOrderedTables.Duplicates := dupAccept;

  try
    { Order the list of tables according to their JoinOrder values }
    for lCount := 0 to (iRequiredTables.Count -1) do begin
      lTableInfo := TTableLinkInfo(iRequiredTables.Objects[lCount]);
      lslOrderedTables.AddObject(IntToStr(lTableInfo.JoinOrder), lTableInfo)
    end;

    { Build the chain of linked tables }
    for lCount := 0 to lslOrderedTables.Count-1 do begin
      lTableInfo := TTableLinkInfo(lslOrderedTables.Objects[lCount]);
      if lJoinChain = '' then
        lJoinChain := lTableInfo.TableName
      else begin
        // Need to do this as SQL Server doesn't accept a single table name
        // surrounded by (...), i.e. (Taxon_Occurrence) would be invalid.
        // Therefore, add (...) when at least one join already there,
        // otherwise, don't bother.
        if Pos('JOIN', lJoinChain) > 0 then lJoinChain := '(' + lJoinChain + ')';
        // Now add the rest
        lJoinChain := lJoinChain +' LEFT JOIN ' +
                      lTableInfo.TableName + ' ON ' +
                      lTableInfo.LinkSQL + ' ';
      end;
    end; // for
    // Set FROM clause for output
    ioFrom := #13#10'FROM ' + lJoinChain;
  finally
    lslOrderedTables.Free;
  end; // try..finally
end;  // BuildFrom

//==============================================================================
//    Procedure Name: TdlgWizard.BuildWhere
//           Purpose: To add in the constraints into the WHERE clause
//        Parameters: ioWhere - the where clause to be built (should start empty)
//      Side Effects: Existing where clause (there should not be one!) is overwritten
//------------------------------------------------------------------------------
procedure TdlgWizard.BuildWhere(var ioWhere: String; const iRequiredTables: TStringList);var lCount     : Integer;
    lTableInfo : TTableLinkInfo;
begin
  for lCount := 0 to iRequiredTables.Count-1 do begin
    lTableInfo := TTableLinkInfo(iRequiredTables.Objects[lCount]);
    if (lTableInfo.WhereSQL <> '') then
      AddToWhereClause(ioWhere, lTableInfo.WhereSQL);
  end; // for
end;  // BuildWhere

//==============================================================================
//    Function Name: TdlgWizard.CheckTaxaAreExpandable
//           Purpose: Display a warning message if taxa are included which are not suitable for expanding
//        Parameters: iKeyLink - the key field to which the criteria are to be applied
//                    iList - the list box of selected criteria
//        Return false if user does not wish to continue. True if the user wishes to continue or all Ok
//    Michael Weideli  Mantis 473
//------------------------------------------------------------------------------
Function TdlgWizard.CheckTaxaAreExpandable(const iList:TListBox): boolean;
Var   lstKeys     : String;
      lCount : integer;
      lListOfTaxa : string;
Begin
  Result := true; // default result
  // Are we actually expanding taxa? Don't bother if not.
  if ((cbExpandTaxa.Checked) and (not cbIncludeAllTxBT.Checked) and (not chkIncludeListTxBt.Checked) and
      (rbSpeciesInfo.Checked)) then begin
    with iList.Items do begin
      lstKeys := '''' + TWizardKeyObject(Objects[0]).Key + '''';
      for lCount:=1 to Count-1 do
        lstKeys := lstKeys + ',''' + TWizardKeyObject(Objects[lCount]).Key + '''';
    end;
    with dmDatabase.ExecuteSQL('SELECT Actual_Name FROM Index_Taxon_Name ' +
         'WHERE Can_Expand = 0 AND Taxon_List_Item_Key IN  (' + lstKeys + ')', True ) do begin
      // Add whatever is returned to the list of taxa that can't be expanded
      while not Eof do begin
        lListofTaxa := lListofTaxa + Fields['Actual_Name'].Value + #13#10;
        MoveNext;
      end;
    end;
    if length(lListOfTaxa) > 0 then begin
      if MessageDlg(ResStr_NotExpandable + #13#10 + lListofTaxa, mtWarning, [mbNo, mbYes], 0)=mrNo then begin
        Result := false;
      end;
    end;
  end;
end; //  End CheckTaxaAreExpandable

//==============================================================================
//    Procedure Name: TdlgWizard.BuildSelectedCriteria
//           Purpose: Adds to the where clause any specifically selected places,
//                    taxa or biotopes
//        Parameters: ioWhere - the where clause so far
//                    iKeyLink - the key field to which the criteria are to be applied
//                    iList - the list box of selected criteria
//------------------------------------------------------------------------------
procedure TdlgWizard.BuildSelectedCriteria(var ioWhere:String;
  const iKeyLink:String; const iList:TListBox);
var lCount       : Integer;
    lLocationKeys: TStringList;
    lstKeys      : String;

  // Convert the list of location (or loc name) keys to a list including subsites
  procedure GetSubSites(const ALocationNameKeys: boolean);
  var
    i: integer;
    lKey: string;
  begin
    // Create a temp table to act as input parameter to the stored proc
    dmDatabase.ExecuteSQL(SQL_TEMPLIST_CREATE);
    try
      for i := 0 to iList.Count-1 do begin
        if ALocationNameKeys then
          lKey := dmGeneralData.GetLocKeyFromLocNameKey(TKeyData(
               iList.Items.Objects[i]).ItemKey)
        else
          lKey := TKeyData(iList.Items.Objects[i]).ItemKey;
        dmDatabase.ExecuteSQL(Format(SQL_TEMPLIST_INSERT_DISTINCT, [lKey, lKey]));
      end;
      with dmDatabase.GetRecordset('usp_LocationsSubSites_Select', []) do begin
        while not EOF do begin
          lLocationKeys.Add(Fields['RecordKey'].Value);
          MoveNext;
        end;
      end;
    finally
      dmDatabase.ExecuteSQL(SQL_TEMPLIST_DROP);
    end; // try
  end;  // AddSubSiteKeys
  //----------------------------------------------------------------------------
begin
  with iList.Items do begin
    // Special case for lbSelected when dealing with Taxon IDs
    if (((not cbIncludeAllTxBT.Checked) and (not chkIncludeListTxBt.Checked)) or
       (chkIncludeListTxBt.Checked and
       ((cmbCategories.Items[cmbCategories.ItemIndex] = STR_RUCKSACK) or
       (cmbCategories.Items[cmbCategories.ItemIndex] = STR_RUCKSACK_EXP)))) and
       (iList=lbSelected) and (rbSpeciesReport.Checked or
       (rbPlaceReport.Checked and rbSpeciesInfo.Checked and rbInfoSelect.Checked)) then
    begin
      // There is at least one, as the user can't move tab until something is selected
      // Use a String to build the list of keys, as it is very fast compared to insert in stinglist
      lstKeys := '''' + TWizardKeyObject(Objects[0]).Key + '''';
      for lCount:=1 to Count-1 do
        lstKeys := lstKeys + ',''' + TWizardKeyObject(Objects[lCount]).Key + '''';
      AddToWhereClause(ioWhere, iKeyLink + ' IN (');
      ioWhere:=ioWhere + lstKeys + ')';
    end else  // Deal with locations. Need to get all sub sites as well, if they are wanted.
    if (iList=lbPlacesSelected) and (Count>0) and cbIncludeSubsites.Checked then begin
      lLocationKeys:=TStringList.Create;
      try
        // For all selected sites/locations, get all the subsites
        GetSubSites(rbByPlaceName.Checked);
        // Now set the Where bit
        AddToWhereClause(ioWhere, iKeyLink + ' IN (''' + lLocationKeys[0] + ''''); // Always at least 1 location
        // Add all the other keys
        for lCount:=1 to lLocationKeys.Count-1 do
          ioWhere:=ioWhere+','''+lLocationKeys[lCount]+'''';
        ioWhere:=ioWhere+')';
      finally
        lLocationKeys.Free;
      end;
    end else  // Deal with the other lists
    if (Count > 0) then begin
      if iList = lbPlacesSelected then begin
        // Like GetSubSites but just get the selected sites
        if rbByPlaceName.Checked then begin
          AddToWhereClause(ioWhere, iKeyLink + ' IN (''' + dmGeneralData.GetLocKeyFromLocNameKey(TKeyData(iList.Items.Objects[0]).ItemKey) + '''');
          for lCount:=1 to Count-1 do
            ioWhere:=ioWhere+','''+dmGeneralData.GetLocKeyFromLocNameKey(TKeyData(iList.Items.Objects[lCount]).ItemKey)+'''';
        end else begin
          AddToWhereClause(ioWhere, iKeyLink + ' IN (''' + TKeyData(iList.Items.Objects[0]).ItemKey + '''');
          for lCount:=1 to Count-1 do
            ioWhere:=ioWhere+','''+TKeyData(iList.Items.Objects[lCount]).ItemKey+'''';
        end;
      // put in the conjunction as this will definitely be required
      end else begin
        AddToWhereClause(ioWhere, iKeyLink + ' IN (''' + TWizardKeyObject(Objects[0]).Key + '''');
        for lCount:= 1 to Count-1 do
          ioWhere:=ioWhere+','''+TWizardKeyObject(Objects[lCount]).Key+'''';
      end;
      ioWhere:=ioWhere+')';
    end;
  end;
end;  // BuildSelectedCriteria

//==============================================================================
//    Procedure Name: TdlgWizard.BuildAdvancedCriteria
//           Purpose: To put the correct conditions according to the advanced
//                    filter screen
//        Parameters: ioWhere - The where clause so far, updated as necessary
//                    itfTaxon - Indicates if the criteria will apply to the
//                    Taxon or the Biotope occurrences. very useful as some some
//                    default contraints apply only to the Taxon.
//      Dependencies: FAdvancedFilter the list of filters should already be
//                    populated Usee AddAdvancedCriteria procedure described below.
//     Special Notes: Takes care of Constraints and unions (see code)
//------------------------------------------------------------------------------
procedure TdlgWizard.BuildAdvancedCriteria(const iSelect:String; var ioWhere:String;
  const itfTaxon:Boolean);
var lCount                  : Integer;
    lField                  : TReportField;
    lstFilters, lstTableName: String;
    ltfInd, ltfOrg          : Boolean;
begin
  if FAdvancedFilter.Count > 0 then
  begin
    // Used to build the advanced filter String
    lstFilters := '';
    // Now for each of the specified conditions
    for lCount:=0 to FAdvancedFilter.Count-1 do
    begin
      lField :=  TFilter(FAdvancedFilter.Items[lCount]).ReportField ;
      // Special case is when the table name is CONSTRAINT
      if lField.TableName <> 'CONSTRAINT' then
      begin
        ltfInd := (Pos('IDET', UpperCase(iSelect)) > 0) or (Pos('IRUNBY', UpperCase(iSelect)) > 0);
        ltfOrg := (Pos('ODET', UpperCase(iSelect)) > 0) or (Pos('ORUNBY', UpperCase(iSelect)) > 0);
        // Exclude Biotope tables when dealing with Taxon, and vice versa.
        if ((ltfInd and (not (lField.TableName[1] = 'O'))) or
            (ltfOrg and (not (lField.TableName[1] = 'I'))) or
            ((not ltfInd) and (not ltfOrg))) and
           ((itfTaxon and (Pos('BIOTOPE', lField.TableName) = 0)) or
            (not itfTaxon and (Pos('TAXON', lField.TableName) = 0))) then
        begin
          //Special case for Sample recorders because it is not a real column
          if (CompareText(lField.TableName + '.' + lField.FieldName, 'Sample.Recorders')=0) then
          begin
            lstTableName := 'dbo';
            try
              //want to use a function instead of the field (which doesn't exist)
              lField.FieldName := 'FormatEventRecorders(Sample.Sample_Key)';
              AddAdvancedCriteria(lstFilters, lstTableName, FAdvancedFilter.Items[lCount]);
            finally
            //remember to change it back 
              lField.FieldName := 'Recorders';
            end;
          end
          else
          begin
          // No Space in TableName means no alias
            if Pos(' ',lField.TableName)=0 then
              lstTableName := lField.TableName
            else  // Space found, get the table alias
              lstTableName := Copy(lField.TableName, Pos(' ', lField.TableName) + 1, 255);
            // Build and add the filter
            AddAdvancedCriteria(lstFilters, lstTableName, FAdvancedFilter.Items[lCount]);
          end;
        end;
      end else begin
        // Special processing for the Constraint table, as the Confidential and
        // Zero_Abundance apply only to Taxon occurrences!!!
        if itfTaxon then
          AddAdvancedCriteria(lstFilters, TN_TAXON_OCCURRENCE, FAdvancedFilter.Items[lCount])
        else
          if (lField.FieldName = 'Checked') or (lField.FieldName = 'Verified') then
            AddAdvancedCriteria(lstFilters, TN_BIOTOPE_OCCURRENCE, FAdvancedFilter.Items[lCount]);
      end;
    end;  // for...
    if lstFilters <> '' then
      AddToWhereClause(ioWhere, '(' + lstFilters + ')');
  end;
end;  // BuildAdvancedCriteria

//==============================================================================
//    Procedure Name: TdlgWizard.AddAdvancedCriteria
//           Purpose: To create the correct SQL statement based on the condition
//                    type for an attribute
//        Parameters: ioWhere - the where clause so far
//                    iFilter - the filter object to be applied
//     Special Notes: Creates "LIKE", "BETWEEN" and other condition phrases as
//                    required
//------------------------------------------------------------------------------
procedure TdlgWizard.AddAdvancedCriteria(var ioFilters:String; const ATableName:String;
  const AFilter:TObject);
var lstFilterToAdd: String;
begin
  // Add the conditional part
  with TFilter(AFilter) do begin
    // Call the appropriate function to get the filter
    if ReportField.FieldType=TYPE_VAGUE_DATE then
      lstFilterToAdd := GetVagueDateFilter(ReportField,Condition,ATableName,Criteria)
    else if ReportField.FieldType=TYPE_BOOLEAN then
      lstFilterToAdd := GetBooleanFilter(ReportField,Condition,ATableName,Criteria)
    else if (ReportField.FieldType=TYPE_TEXT)
         or (ReportField.FieldType=TYPE_RICH_TEXT) then
      lstFilterToAdd := GetTextFilter(ReportField,Condition,ATableName,Criteria)
    else
    if (ReportField.FieldType=TYPE_SPATIAL_REF) then
      lstFilterToAdd := GetTextFilter(ReportField, Condition, ATableName, DelocaliseSpatialRef(Criteria))
    else // Other types, numbers
      lstFilterToAdd := GetNumberFilter(ReportField,Condition,ATableName,Criteria);

    // As we don;t want a "... AND () ..." bit that would break the query
    if lstFilterToAdd <> '' then begin
      // Don't add AND/OR if it's the first additional filter
      if ioFilters <> '' then
        // Add AND/OR condition, before the filter itself
        if (ReportField.Union = uALways) then 
          ioFilters := ioFilters + ' AND '
        else  // (ReportField.Union=uOmit)
          ioFilters := ioFilters + ' OR ';
      ioFilters := ioFilters + '(' + lstFilterToAdd + ')';
    end;
  end;
end;  // AddAdvancedCriteria

//==============================================================================
function TdlgWizard.GetBooleanFilter(const AField: TReportField;
  const ACondition: TCondition; const ATableName: String; ACriteria:String): String;
var lFieldName:String;
begin
  lFieldName:=ATableName+'.'+AField.FieldName;
  // Add the filtering condition, =/<>
  case ACondition of
    cdEqual    : Result:=lFieldName+' = '+IfThen((CompareText(ACriteria ,'Yes')=0) or
      (CompareText(ACriteria, '1') = 0) or (CompareText(ACriteria, 'True')= 0)
    , '1', '0');
    cdNotEqual : Result:='(' + lFieldName+' <> '+IfThen((CompareText(ACriteria ,'Yes')=0) or
      (CompareText(ACriteria, '1') = 0) or (CompareText(ACriteria, 'True')= 0), '1', '0') +
      ' or ' + lFieldName + ' is Null)';
  end;
end;  // GetBooleanFilter

//------------------------------------------------------------------------------
function TdlgWizard.GetNumberFilter(const AField: TReportField;
  const ACondition: TCondition; Const ATableName : String; ACriteria:String): String;
var lFieldName      :String;
    lBetweenCriteria:TBetweenCriteria;
    ltfIsLatLong : Boolean;
    ltfIsTime : Boolean;
    lFormatSettings: TFormatSettings;
begin
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, lFormatSettings);
  lFormatSettings.DecimalSeparator := '.';

  ltfIsTime := False;
  lFieldName:=ATableName+'.'+AField.FieldName;
  if ACondition=cdBetween then
    lBetweenCriteria:=SplitBetweenCriteria(ACriteria);
  //Latitude and longitude are stored as strings but must be compared as numbers.
  if (CompareText(AField.FieldName, 'Lat') = 0) or (CompareText(AField.FieldName, 'Long') = 0) then
  begin
    ltfIsLatLong := True;
    lFieldName := 'Convert(Decimal(7,4), ' + lFieldName + ')';
    ACriteria := FloatToStr(StrToFloat(ACriteria), lFormatSettings);
  end
  else
  begin
    ltfIsLatLong := False;
    if CompareText(AField.FieldName, 'Time') = 0 then
    begin
      ltfIsTime := True;
      //Compare fractional parts. Assume date time is not more than 9 days off 0
      lFieldName := 'Convert(Decimal(5,4), ' + lFieldName + ') - Floor(Convert(Decimal(5,4), ' + lFieldName + '))';
      if Acondition = cdBetween then
      begin
        lBetweenCriteria.Criteria1 := FloatToStr(Frac(StrToDateTime(lBetweenCriteria.Criteria1)), lFormatSettings);
        lBetweenCriteria.Criteria2 := FloatToStr(Frac(StrToDateTime(lBetweenCriteria.Criteria2)), lFormatSettings);
      end else
        ACriteria := FloatToStr(Frac(StrToDateTime(ACriteria)), lFormatSettings);
    end
    else
      ltfIsTime := False;
  end;

  // Add the filtering condition, =/<>/>/</Between
  case ACondition of
    cdEqual    :
      if ltfIsLatLong then
        Result := 'Abs(' + lFieldName + ' - ' + ACriteria + ') < 0.0001'
      else if ltfIsTime then
        Result := 'Abs(' + lFieldName + ' - ' + ACriteria + ') < 0.0007'//a minute
     else
        Result := lFieldName + ' = ' + ACriteria;
    cdNotEqual : Result := '(' + lFieldName + ' <> ' + ACriteria + ' or ' + lFieldName + ' is null)';
    cdGreater  : Result := lFieldName + ' > ' + ACriteria;
    cdLessThan : Result := lFieldName + ' < ' + ACriteria;
    cdBetween  : Result := lFieldName + ' BETWEEN ' + lBetweenCriteria.Criteria1 +
                                        ' AND ' + lBetweenCriteria.Criteria2;
  end; // case Condition of
end;  // GetNumberFilter

//------------------------------------------------------------------------------
function TdlgWizard.GetTextFilter(const AField: TReportField;
  const ACondition: TCondition; const ATableName, ACriteria:String): String;
var stTemp,lCriteria:String;
    lCount          :Integer;
    lFieldName      :String;
begin
  // Get TableName, or alias if there is one
  lFieldName:=ATableName+'.'+AField.FieldName;

  lCriteria:=ACriteria;
  // Double the quotes characters, if any, so SQL doesn't break
  if Pos('''',lCriteria)>0 then begin
    stTemp:=lCriteria;
    lCriteria:='';
    for lCount:=1 to Length(stTemp) do begin
      lCriteria:=lCriteria+stTemp[lCount];
      if stTemp[lCount]='''' then lCriteria:=lCriteria+'''';
    end;
  end;

  // Add the filtering condition, =/<>/Contains/Starts with
  // For Contains and Starts With, use the ConvertSearchString function on the
  // original text, otherwise, " chars would be doubled again!
  case ACondition of
    cdEqual    : Result:=lFieldName+' = '''+lCriteria+'''';
    cdNotEqual : Result:= '((' + lFieldName+' <> '''+lCriteria+''') or ' + lFieldName + ' is null)';
  {$IFDEF USE_TITAN}
    cdContains : Result:=lFieldName+' LIKE ''*'+ConvertSearchString(ACriteria)+ '*''';
    cdStarts   : Result:=lFieldName+' LIKE ''' +ConvertSearchString(ACriteria)+ '*''';
  {$ELSE}
    cdContains : Result:=lFieldName+' LIKE ''%'+ConvertSearchString(ACriteria)+ '%''';
    cdStarts   : Result:=lFieldName+' LIKE ''' +ConvertSearchString(ACriteria)+ '%''';
  {$ENDIF}
  end; // case Condition of
end;  // GetTextFilter

//------------------------------------------------------------------------------
function TdlgWizard.GetVagueDateFilter(const AField: TReportField;
  const ACondition: TCondition; const ATableName, ACriteria:String): String;
var lStart, lEnd, lType,
    lFieldStub, lFieldName: String;
    lVague1, lVague2      : TVagueDate;
    lBetweenCriteria      : TBetweenCriteria;
begin
  lFieldName:=ATableName+'.'+AField.FieldName;
  lFieldStub:=Copy(lFieldName,1,Pos('_START',lFieldName)-1);

  // Special case for Between and vague dates
  if ACondition=cdBetween then begin
    lBetweenCriteria := SplitBetweenCriteria(ACriteria);
    lVague1 := StringToVagueDate(lBetweenCriteria.Criteria1);
    lVague2 := StringToVagueDate(lBetweenCriteria.Criteria2);
    lEnd    := IntToStr(Trunc(lVague2.EndDate));
  end else begin
    lVague1 := StringToVagueDate(ACriteria);
    lEnd    := IntToStr(Trunc(lVague1.EndDate));
  end;
  lStart := IntToStr(Trunc(lVague1.StartDate));
  lType  := '''' + lVague1.DateTypeString + '''';

  // Add the filtering condition, =/<>/>/</Between
  case ACondition of
    cdEqual    : Result:=lFieldStub+'_START = '+lStart+' AND '+
                         lFieldStub+'_END = '+lEnd+' AND '+
                         lFieldStub+'_TYPE = '+lType;
    cdNotEqual : Result:='NOT ('+lFieldStub+'_START = '+lStart+' AND '+
                         lFieldStub+'_END = '+lEnd+' AND '+
                         lFieldStub+'_TYPE = '+lType+') OR '+
                         lFieldStub+'_START Is NULL';
    cdGreater  : Result:=lFieldStub+'_START > '+lEnd;
    cdLessThan : Result:=lFieldStub+'_END < '+lStart;
    cdBetween  : Result:=lFieldStub+'_START >= '+lStart+' AND '+
                         lFieldStub+'_END <= '+lEnd;
  end; // case Condition of
end;  // GetVagueDateFilter            

//==============================================================================
//     Function Name: TdlgWizard.BuildSpeciesReport
//           Purpose: To build the entire SQL for a Taxon based report
//      Return Value: String - The full SQL for the selected criteria
//      Dependencies: Many
//------------------------------------------------------------------------------
function TdlgWizard.BuildSpeciesReport : String;
var
  lstSQLSelect: String;
  lstSQLFrom  : String;
  lstSQLWhere : String;
  lstSQLA     : String;
  liTblIndex  : Integer;
  liUnionPart : Integer;
  lstlTables  : TStringList;
  lCursor     : TCursor;
  lAlias      : string;
begin
  lCursor:=HourglassCursor;
  try
    lAlias := '';
    liUnionPart := 0;
    lstlTables := TStringList.Create;
    repeat //always do it once!!
      lstlTables.Clear;
      // Start Select with fields which should always be present...
      lstSQLSelect := ' Distinct Taxon_Occurrence.Taxon_Occurrence_Key AS Occurrence_Key, ' +
                      '''T'' AS Type '; //space required
      litblIndex := -1;
      // find the correct table chain required in the query
      if cbIncludeAllTxBt.Checked then
        liTblIndex := FdmWizard.TaxonTables.IndexOf(TN_TAXON_OCCURRENCE)
      else if chkIncludeListTxBt.Checked and
            (cmbCategories.Items[cmbcategories.ItemIndex] <> STR_RUCKSACK) and
            (cmbCategories.Items[cmbCategories.ItemIndex] <> STR_RUCKSACK_EXP) then begin
        if AppSettings.UseRecommendedTaxaNames then
          lAlias := 'TL'
        else
          lAlias := 'TL2';
        liTblIndex := FdmWizard.TaxonTables.IndexOf('TAXON_LIST ' + lAlias);
      end
      else begin
        // Find the alias of the table we need to query against.  This defines the
        // chain of joins that make the query
        if cmbCategories.Items[cmbCategories.ItemIndex] = STR_RUCKSACK_EXP then
          cbExpandTaxa.Checked := true;
        if AppSettings.UseRecommendedTaxaNames and cbExpandTaxa.Checked then
          lAlias := 'ITN5'
        else if (not AppSettings.UseRecommendedTaxaNames) and cbExpandTaxa.Checked then
          lAlias := 'ITS2'
        else if AppSettings.UseRecommendedTaxaNames and (not cbExpandTaxa.Checked) then
          lAlias := 'ITN2'
        else if (not AppSettings.UseRecommendedTaxaNames) and (not cbExpandTaxa.Checked) then
          lAlias := 'ITS';
        if CompareText(Copy(lAlias, 1, 3), 'ITS')=0 then
          liTblIndex := FdmWizard.TaxonTables.IndexOf('Index_Taxon_Synonym ' + lAlias)
        else if CompareText(Copy(lAlias, 1, 3), 'ITN')=0 then
          liTblIndex := FdmWizard.TaxonTables.IndexOf('Index_Taxon_Name ' + lAlias)
      end;
      if liTblIndex > -1 then
        AddTableToQuery(TTableLinkInfo(FdmWizard.TaxonTables.Objects[liTblIndex]),
                          FdmWizard.TaxonTables,
                          lstlTables);

      //  Add in the Advanced Filter tables if there are any
      DoAdvancedFilterTables(lstSQLSelect, FdmWizard.TaxonTables, lstlTables, True);

      lstSQLWhere := '' ; // Note that for unions we need to clear each time
      // Add in any conditions for the tables used in the from clause
      BuildWhere(lstSQLWhere, lstlTables);

      FilterOnSurveys(FdmWizard.TaxonTables, lstlTables, lstSQLWhere);

      if cbRestrictDesignations.Checked then
        FilterOnDesignations(FdmWizard.TaxonTables, lstlTables, lstSQLWhere);

      CheckSources(FdmWizard.TaxonTables, lstlTables, lstSQLSelect, lstSQLWhere);

      // Setup the From clause
      BuildFrom(lstSQLFrom, lstlTables);
      if chkIncludeListTxBt.Checked and (not cbIncludeAllTxBt.Checked) then
        with cmbCategories do
          if (Items[ItemIndex] = STR_RUCKSACK) then
          begin
            bbAddAllClick(nil);
            BuildSelectedCriteria(lstSQLWhere, lAlias + '.Taxon_List_Item_Key', lbSelected);
          end else if (Items[ItemIndex] = STR_RUCKSACK_EXP) then
          begin
            ItemIndex := Items.Count - 2;
            cmbCategoriesChange(nil);
            bbAddAllClick(nil);
            BuildSelectedCriteria(lstSQLWhere, lAlias + '.Taxon_List_Item_Key', lbSelected);
            ItemIndex := Items.Count - 1;
            cmbCategoriesChange(nil);
          end else
            AddToWhereClause(lstSQLWhere, lAlias + '.Taxon_List_Key = ''' +
                                        TKeyData(Items.Objects[ItemIndex]).ItemKey + ''' ')
      else
        BuildSelectedCriteria(lstSQLWhere,lAlias + '.Taxon_List_Item_Key',lbSelected);

      //  Add in the Advanced Filters if there are any
      BuildAdvancedCriteria(lstSQLSelect, lstSQLWhere, True);

      lstSQLSelect := 'SELECT ' + lstSQLSelect;

      if FUnionRequired and (liUnionPart = 0) then
        lstSQLA := lstSQLSelect + lstSQLFrom + lstSQLWhere;

      Inc(liUnionPart);
    until not FUnionRequired or (liUnionPart > 1);
    lstlTables.Free;

    if FUnionRequired then
      lstSQLA := lstSQLA + ' UNION ' + lstSQLSelect + lstSQLFrom + lstSQLWhere
    else
      lstSQLA := lstSQLSelect + lstSQLFrom + lstSQLWhere;
    Result :=lstSQLA;
  finally
    DefaultCursor(lCursor);
  end;
end;  // BuildSpeciesReport

//==============================================================================
//     Function Name: TdlgWizard.BuildBiotopesReport
//           Purpose: To build the entire SQL for a Biotope based report
//      Return Value: String - The full SQL for the selected criteria
//      Dependencies: Many
//------------------------------------------------------------------------------
function TdlgWizard.BuildBiotopesReport:String;
var
  lstSQLSelect: String;
  lstSQLFrom: String;
  lstSQLWhere: String;
  lstSQLA: String;
  liUnionPart: Integer;
  lstlTables: TStringList;
  lCursor   :TCursor;
begin
  lCursor:=HourglassCursor;
  try
    liUnionPart:= 0;
    lstlTables := TStringList.Create;
    repeat //always do it once!!
      lstlTables.Clear;
      // Start Select with fields which should always be present...
      lstSQLSelect := ' Distinct Biotope_Occurrence.Biotope_Occurrence_Key AS Occurrence_Key, ' +
                      '''B'' AS Type '; //space required

      //Check if the user selected a sub-set of BIOTOPE List Items.
      if cbIncludeAllTxBt.Checked then
        AddNamedTableToQuery(TN_BIOTOPE_OCCURRENCE, FdmWizard.BiotopeTables, lstlTables)
      else if chkIncludeListTxBt.Checked then
        AddNamedTableToQuery('BIOTOPE_CLASSIFICATION', FdmWizard.BiotopeTables, lstlTables)
      else
        // This field is not required except as part of the where clause
        //Make sure the BIOTOPE_item_list table is included...
        AddNamedTableToQuery(TN_BIOTOPE_LIST_ITEM, FdmWizard.BiotopeTables, lstlTables);

      //  Add in the Advanced Filter tables if there are any
      DoAdvancedFilterTables(lstSQLSelect, FdmWizard.BiotopeTables, lstlTables, False);

      lstSQLWhere := '' ; // Note that for unions we need to clear each time
      // Add in any conditions for the tables used in the from clause
      BuildWhere(lstSQLWhere, lstlTables);

      FilterOnSurveys(FdmWizard.BiotopeTables, lstlTables, lstSQLWhere);

      if chkIncludeListTxBt.Checked then
        with cmbCategories do
          AddToWhereClause(lstSQLWhere, 'Biotope_Classification.Biotope_Classification_Key = ''' +
                                        TKeyData(Items.Objects[ItemIndex]).ItemKey + ''' ');

      CheckSources(FdmWizard.BiotopeTables,lstlTables,lstSQLSelect,lstSQLWhere);

      // Setup the From clause
      BuildFrom(lstSQLFrom, lstlTables);

      //Add in where conditions for selected biotopes, if any.
      BuildSelectedCriteria(lstSQLWhere,'Biotope_List_Item.Biotope_List_Item_Key',lbSelected);

      //  Add in the Advanced Filters if there are any
      BuildAdvancedCriteria(lstSQLSelect, lstSQLWhere,False);

     lstSQLSelect := 'SELECT ' + lstSQLSelect;

      if FUnionRequired and (liUnionPart = 0) then
        lstSQLA := lstSQLSelect + lstSQLFrom + lstSQLWhere;

      Inc(liUnionPart);
    until not FUnionRequired or (liUnionPart > 1);
    lstlTables.Free;

    if FUnionRequired then
      lstSQLA:=lstSQLA+' UNION '+lstSQLSelect+lstSQLFrom+lstSQLWhere
    else
      lstSQLA:=lstSQLSelect+lstSQLFrom+lstSQLWhere;
    Result :=lstSQLA;
  finally
    DefaultCursor(lCursor);
  end;
end;  // BuildBiotopesReport

//==============================================================================
//     Function Name: TdlgWizard.BuildPlaceReport
//           Purpose: To build the entire SQL for a Location based report
//      Return Value: String - The full SQL for the selected criteria
//      Dependencies: Many
//     Special Notes:
//        Ok, let's begin with our explanation of the target and how we hoped to
//        achieve it
//
//        Current queries must cope with the possibility of a Union being required
//        when dealing with the Organisation / Individual name field
//
//        To add complexity to this, when dealing with places we have to link to
//        either Taxon_Occurrence or Biotope_Occurrence OR BOTH, giving rise
//        once again to a Union.
//
//        We now have the possibility of the following
//          SELECT ... FROM Individual, Taxon_Occurrence WHERE ...
//          UNION
//          SELECT ... FROM Organisation, Taxon_occurrence WHERE ...
//        UNION
//          SELECT ... FROM Individual, Biotope_Occurrence WHERE ...
//          UNION
//          SELECT ... FROM Organisation, Biotope_Occurrence WHERE ...
//
//        Bear in mind that this means that all necessary joining tables must
//        also be included in each where and from clause
//
//        To achieve this monumental task (without re-writing the whole wizard)
//        it was decided (by me kjg) to use the existing code twice,
//        once for Taxon connections and once for Biotopes.
//        to facilitate this, the loading of the Places table is delayed to here
//        and it is loaded first with bio then, if required with taxon table joins.
//
//        For details of how this table is loaded see
//        PopulatePlaceTaxonTablesList and
//        PopulatePlaceBiotopeTablesList in WizardData unit
//
//        Here we go...... !!!!!
//------------------------------------------------------------------------------
function TdlgWizard.BuildPlaceReport:String;
var
  lstSQLSelect:String;
  lstSQLFrom  :String;
  lstSQLWhere :String;
  lstSQLA     :String;
  liUnionPart :Integer;
  lstlTables  :TStringList;
  iButtonFlag :Integer;
  lCursor     :TCursor;
begin
  lCursor:=HourglassCursor;
  try
    // First decide which table links are required
    if rbPlacesInfo.Checked then
      iButtonFlag := 3 // set both bits of the flag
    else if rbSpeciesInfo.Checked then
      iButtonFlag := 2  // set species flag
    else
      iButtonFlag := 1;  // set biotope flag

    lstlTables := TStringList.Create;
    // Do this in a loop which terminates AFTER Taxa have been done
    repeat
      // First decide which table links are required
      if iButtonFlag >= 2 then
        FdmWizard.PopulatePlaceTaxonTablesList // Taxon chosen
      else
        FdmWizard.PopulatePlaceBiotopeTablesList;  // Biotope chosen

      // liUnionPart is used to find out which part of the Union is being constructed
      // when liUnionPart = 0 the first part is done for individuals
      // then the second part for organisations
      liUnionPart := 0;
      repeat
        lstlTables.Clear;
        // Now decide which table keys are required
        if iButtonFlag >= 2 then
        begin
          // Start Select with fields which should always be present...
          lstSQLSelect := 'SELECT DISTINCT Taxon_Occurrence.Taxon_Occurrence_Key AS Occurrence_Key, '+
                          '''T'' AS Type ';
          //Check if the user selected a sub-set of Taxon List Items.
          if rbInfoSelect.Checked and (not cbIncludeAllTxBt.Checked) then begin
            if chkIncludeListTxBt.Checked then
              //Make sure the taxon_item_list table is included...
              AddNamedTableToQuery('TAXON_LIST TL', FdmWizard.TaxonTables, lstlTables)
            else
              //Make sure the taxon_item_list table is included...
              AddNamedTableToQuery(TN_TAXON_LIST_ITEM, FdmWizard.TaxonTables,lstlTables);
          end;
        end else begin
          // Start Select with fields which should always be present...
          lstSQLSelect := 'SELECT DISTINCT Biotope_Occurrence.Biotope_Occurrence_Key AS Occurrence_Key, '+
                          '''B'' AS Type ';
          //Check if the user selected a sub-set of BIOTOPE List Items.
          if rbInfoSelect.Checked and (not cbIncludeAllTxBt.Checked)then begin
            if chkIncludeListTxBt.Checked then
              //Make sure the taxon_item_list table is included...
              AddNamedTableToQuery('BIOTOPE_CLASSIFICATION',
                                FdmWizard.BiotopeTables, lstlTables)
            else
              //Make sure the BIOTOPE_item_list table is included...
              AddNamedTableToQuery(TN_BIOTOPE_LIST_ITEM,
                                FdmWizard.BiotopeTables,lstlTables);
          end;
        end;
        if rbInfoSelect.Checked and rbSpeciesInfo.Checked then begin
          if cbExpandTaxa.Checked then
            AddNamedTableToQuery('Index_Taxon_Name ITN5',FdmWizard.TaxonTables, lstlTables)
          else
            AddNamedTableToQuery('Index_Taxon_Name ITN2',FdmWizard.TaxonTables, lstlTables)
        end;
        //Make sure the location name table is included...
        // since that is what will be used to identify the place
        AddNamedTableToQuery('LOCATION_NAME LNSAMP', FdmWizard.PlaceTables,lstlTables);

        //  Add in the Advanced Filter tables if there are any
        DoAdvancedFilterTables(lstSQLSelect, FdmWizard.PlaceTables, lstlTables, iButtonFlag>=2);

        lstSQLWhere := '' ; // Note that for unions we need to clear each time
        // Add in any conditions for the tables used in the from clause
        BuildWhere(lstSQLWhere, lstlTables);

        FilterOnSurveys(FdmWizard.PlaceTables, lstlTables, lstSQLWhere);  

        if (iButtonFlag >= 2) and cbRestrictDesignations.Checked then
          FilterOnDesignations(FdmWizard.TaxonTables, lstlTables, lstSQLWhere);

        if chkIncludeListTxBt.Checked then
          with cmbCategories do
            if iButtonFlag >= 2 then
              AddToWhereClause(lstSQLWhere, 'TL.Taxon_List_Key = ''' +
                                            TKeyData(Items.Objects[ItemIndex]).ItemKey + ''' ')
            else
              AddToWhereClause(lstSQLWhere, 'Biotope_Classification.Biotope_Classification_Key = ''' +
                                            TKeyData(Items.Objects[ItemIndex]).ItemKey + ''' ');

        CheckSources(FdmWizard.PlaceTables, lstlTables, lstSQLSelect, lstSQLWhere);

        // Setup the From clause
        BuildFrom(lstSQLFrom,lstlTables);

        { Add in where conditions for selected Places.  This list is either built
         automatically, for bounding boxes, or by manual place selection }
        BuildSelectedCriteria(lstSQLWhere, 'LSamp.Location_Key', lbPlacesSelected);
        if rbSpeciesInfo.Checked and rbInfoSelect.Checked then begin
          // Add selected Taxa
          if cbExpandTaxa.Checked then
            BuildSelectedCriteria(lstSQLWhere, 'ITN5.Taxon_List_Item_Key', lbSelected)
          else
            BuildSelectedCriteria(lstSQLWhere, 'ITN2.Taxon_List_Item_Key', lbSelected);
        end
        else if rbBiotopeInfo.Checked and rbInfoSelect.Checked then
          // Add selected biotopes
          BuildSelectedCriteria(lstSQLWhere,'Biotope_List_Item.Biotope_List_Item_Key',lbSelected);

        //  Add in the Advanced Filters if there are any
        BuildAdvancedCriteria(lstSQLSelect, lstSQLWhere, iButtonFlag >= 2);

        if FUnionRequired and (liUnionPart = 0) then
          // we are building the first select statement of a union
          // we must prepend existing SQL as this may be from the Taxon Union
          lstSQLA := lstSQLA + lstSQLSelect + lstSQLFrom + lstSQLWhere + ' UNION '
        else
          // we are building the second select statement of the Union
          lstSQLA := lstSQLA + lstSQLSelect + lstSQLFrom + lstSQLWhere;

        Inc(liUnionPart);
      until not FUnionRequired or (liUnionPart > 1);

      if iButtonFlag>=2 then iButtonFlag := iButtonFlag - 2  // clear the species flag
                        else iButtonFlag := iButtonFlag - 1; // clear the biotope flag

      // we will be adding the second Union to to the first Union
      if iButtonFlag<>0 then lstSQLA :=lstSQLA+ ' UNION ';
    until iButtonFlag = 0; // both taxon and/or biotope done
    lstlTables.Free;
    Result := lstSQLA;
  finally
    DefaultCursor(lCursor);
  end;
end;  // BuildPlaceReport

//==============================================================================
//     Function Name: TdlgWizard.BuildTitle
//           Purpose: Using the primary checkboxes and radio buttons this routine
//                    will produce a title for the report.
//      Return Value: Stirng - Contains the description/purpose of the report
//------------------------------------------------------------------------------
function TdlgWizard.BuildTitle:String;
var lsNoun:String;
begin
  Result := ResStr_Local_ReportAbout;
  if (lbSelected.Items.Count > 0) or (lbPlacesSelected.Items.Count > 0) then
    Result := Result + ResStr_Local_Selected
  else
    Result := Result + ResStr_Local_All;

  if rbSpeciesReport.Checked then
    Result := Result + ResStr_Local_Species
  else if rbBiotopesReport.Checked then
    Result := Result + ResStr_Local_Biotopes
  else
  begin
    Result := Result + ResStr_Local_Places;
    if rbPlacesInfo.Checked then
      Result := Result + ResStr_Local_WithAll
    else
    begin
      if (lbSelected.Items.Count > 0) then
        Result := Result + ResStr_Local_WithSelected
      else
        Result := Result + ResStr_Local_WithAll;
    end;
    if rbSpeciesinfo.Checked then
      Result := Result + ResStr_Local_Species
    else if rbBiotopeInfo.Checked then
      Result := Result + ResStr_Local_Biotopes
    else
      Result := Result + ResStr_Local_SpeciesAndBiotopes;
  end;

  if cbRestrictSource.Checked then
  begin
    if rbSourcesReferences.Checked then
    begin
      Result := Result + ResStr_Local_RefecencedBy;
      lsNoun := ResStr_Local_References;
    end
    else if rbSourcesSurveys.Checked then
    begin
      Result := Result + ResStr_Local_Within;
      lsNoun := ResStr_Local_Surveys;
    end
    else
    begin
      Result := Result + ResStr_Local_RunBy;
      lsNoun := ResStr_Local_IndOrg;
    end;
    if lbSourcesSelected.Items.Count > 0 then
      Result := Result + ResStr_Local_Selected+ lsNoun
    else
      Result := Result + ResStr_Local_All + lsNoun;
  end;

  if cbRestrictArea.Checked then
  begin
    if lbAdminSelected.Items.Count >0 then
      Result := Result + ResStr_Local_SelectedAdminArea
    else
      Result := Result + ResStr_Local_AllAdminArea;
  end;
end;  // BuildTitle  

//==============================================================================
procedure TdlgWizard.SetTitle(TitleString: String);
begin
  FTitleText := TitleString ;
end;  // SetTitle

//==============================================================================
function TdlgWizard.GetWizardInProgress: Boolean;
begin
  Result := FWizardInProgress;
end;  // GetWizardInProgress

//==============================================================================
{ TTabSequence }
//------------------------------------------------------------------------------
constructor TTabSequence.Create;
begin
  inherited Create;
  FSequence := -1;
end;  // TTabSequence.Create

//------------------------------------------------------------------------------
procedure TTabSequence.Clear;
begin
  inherited Clear;
  FSequence := -1;
end;  // TTabSequence.Clear

//------------------------------------------------------------------------------
procedure TTabSequence.AddToSequence(itsSheet: TTabSheet);
begin
  if Assigned(itsSheet) then
    if IndexOf(itsSheet) = -1 then
      Add(itsSheet);
end;  // TTabSequence.AddToSequence

//------------------------------------------------------------------------------
procedure TTabSequence.InsertIntoSequence(iiIndex: Integer;
  itsSheet: TTabSheet);
begin
  if Assigned(itsSheet) then
    if IndexOf(itsSheet) = -1 then
      Insert(iiIndex, itsSheet);
end;  // TTabSequence.InsertIntoSequence

//------------------------------------------------------------------------------
procedure TTabSequence.RemoveTab(itsSheet: TTabSheet);
var
  liIndex: Integer;
begin
  liIndex := IndexOf(itsSheet);
  if liIndex <> -1 then
  begin
    Delete(liIndex);
    if liIndex < FSequence then
      Dec(FSequence);
  end;
end;  // TTabSequence.RemoveTab

//------------------------------------------------------------------------------
procedure TTabSequence.MoveOn;
begin
  Inc(FSequence);
end;  // TTabSequence.MoveOn

//------------------------------------------------------------------------------
procedure TTabSequence.MoveBack;
begin
  Dec(FSequence);
end;  // TTabSequence.MoveBack

//------------------------------------------------------------------------------
function TTabSequence.GetTab: TTabSheet;
begin
  if (FSequence > -1) and (FSequence < Count) then
    Result := TTabSheet(Items[FSequence])
  else
    raise EInvalidIndex.Create(ResStr_IndexOutOfBound);
end;  // TTabSequence.GetTab

//------------------------------------------------------------------------------
function TTabSequence.JumpTo(itsSheet: TTabSheet): Boolean;
begin
  Result := False;
  if IndexOf(itsSheet) <> -1 then
  begin
    FSequence := IndexOf(itsSheet);
    Result := True;
  end;
end;  // TTabSequence.JumpTo

//==============================================================================
{ TWizardKeyObject }
//------------------------------------------------------------------------------
constructor TWizardKeyObject.Create(const AKey: TKeyString);
begin
  inherited Create;
  FKey := AKey;
end;

//------------------------------------------------------------------------------
function TWizardKeyObject.GetAdditional: String;
begin
  Result := FAdditional;
end;  // TWizardKeyObject.GetAdditional

//------------------------------------------------------------------------------
procedure TWizardKeyObject.SetAdditional(const Value: String);
begin
  FAdditional := Copy(Value,0,30);
end;  // TWizardKeyObject.SetAdditional

//==============================================================================
procedure TdlgWizard.rbOverlapsClick(Sender: TObject);
begin
  inherited;
  FSpatialRefChanged := True;
  if rbIncludeOverlaps.Checked then
    imgExplanation.Picture.Bitmap.LoadFromResourceName(hInstance, 'OVERLAP')
  else
    imgExplanation.Picture.Bitmap.LoadFromResourceName(hInstance, 'NO_OVERLAP');
  FReportChanged := True;
end;  // rbOverlapsClick

//==============================================================================
procedure TdlgWizard.SetActiveControl;
begin
  with pcWizard do begin
    if ActivePage=tsSourcesSelection then
      if cmbSourcesType.Visible then cmbSourcesType.SetFocus
                                else fndSources.SetFocus
    else if ActivePage=tsAdminSelection then
      cmbType.SetFocus
    else if ActivePage=tsTaxaBioSelection then
      if cbIncludeAllTxBt.Checked then
        cbIncludeAllTxBt.SetFocus   // don't focus other controls as they are disabled
      else if chkIncludeListTxBt.Checked then
        chkIncludeListTxBt.SetFocus
      else if cmbCategories.Text='' then
        cmbCategories.SetFocus
      else
        fndTaxaBioFinder.SetFocus
    else if ActivePage=tsPlacesSelection then
      fndPlaceFinder.SetFocus
    else if ActivePage=tsGridRefSelection then
      eSWCorner.SetFocus;
  end;
end;  // SetActiveControl

//==============================================================================
procedure TdlgWizard.eNECornerExit(Sender: TObject);
begin
  inherited;
  ValidateSpatialRefEntry(eNECorner, AppSettings.SpatialRefSystem, FValidEnteredNE);
end;  // eNECornerExit

//==============================================================================
procedure TdlgWizard.eSWCornerExit(Sender: TObject);
begin
  inherited;
  ValidateSpatialRefEntry(eSWCorner, AppSettings.SpatialRefSystem, FValidEnteredSW);
end;  // eSWCornerExit

//==============================================================================
procedure TdlgWizard.eCornersChange(Sender: TObject);
begin
  inherited;
  FSpatialRefChanged:=True;
  FReportChanged := True;
end;  // eCornersChanged

//==============================================================================
{ Triggered by the Finder component when it has enough characters to start
    filling up the search lists for taxa or biotopes }
procedure TdlgWizard.fndTaxaBioFinderPopulateList(Sender: TObject);
begin
  inherited;
  if SameText(lblTaxaBioSelection.Caption, ResStr_Cap_TaxaSelection) then begin
    // Taxon Check Lists
    with cmbCategories do
    begin // not strictly necessary but much easier to look at
      if (Text <> STR_RUCKSACK) and (Text <> STR_RUCKSACK_EXP) then
        if ItemIndex >= 0 then begin
          dmSearch.SetDatabaseName(TKeyData(Items.Objects[ItemIndex]).ItemAdditional);
          dmSearch.InitFromTaxonQuery(fndTaxaBioFinder,
                                       TKeyData(Items.Objects[ItemIndex]).ItemKey,
                                       AppSettings.DisplayTaxonCommonNames, stName,
                                       ResStr_CurrentChecklist);
          dmSearch.SetDatabaseName('LOCAL');
        end;
    end;
  end else
    // Biotope Classifications
    with cmbCategories do
      if (Text <> STR_RUCKSACK) and (Text <> STR_RUCKSACK_EXP) then
        if ItemIndex >= 0 then
          dmSearch.InitFromBiotopeQuery(fndTaxaBioFinder,TKeyData(Items.Objects[ItemIndex]).ItemKey);
end; // fndTaxaBioFinderPopulateList

//==============================================================================
{ Triggered by the Finder component when it wants the search lists to stop
     filling up - for taxon/biotope search }
procedure TdlgWizard.fndTaxaBioFinderPopulateStop(Sender: TObject);
begin
  inherited;
  dmSearch.Terminate;
end; // fndTaxaBioFinderPopulateStop

//==============================================================================
{ Triggered by the Finder component when it wants the search lists to stop
     filling up - for location search }
procedure TdlgWizard.fndPlaceFinderPopulateStop(Sender: TObject);
begin
  inherited;
  dmSearch.Terminate;
end; // fndPlaceFinderPopulateStop

//==============================================================================
{ Triggered by the Finder component when it has enough characters to start
    filling up the search lists for locations }
procedure TdlgWizard.fndPlaceFinderPopulateList(Sender: TObject);
begin
  inherited;
  dmSearch.InitFromLocationQuery(fndPlaceFinder);
end; // fndPlaceFinderPopulateList

//==============================================================================
{ Triggered by the Finder component when it wants the search lists to stop
     filling up - for source search }
procedure TdlgWizard.fndSourcesPopulateStop(Sender: TObject);
begin
  inherited;
  dmSearch.Terminate;
end; // fndSourcesPopulateStop

//==============================================================================
procedure TdlgWizard.fndSourcesPopulateList(Sender: TObject);
begin
  inherited;
  if lblSourcesType.Caption = ResStr_FindDocs then
    dmsearch.InitFromReferenceQuery(fndSources)
  else
  if lblSourcesType.Caption = ResStr_FindNames then
  begin
    dmsearch.InitFromIndividualQuery(fndSources);
    dmsearch.InitFromOrganisationQuery(fndSources, False); //Don't clear the individuals
  end else
  if lblSourcesType.Caption = ResStr_FindTags then
    dmSearch.InitFromConceptQuery(ACG_SURVEY_TAGS, fndSources);
end; // fndSourcesPopulateList

//==============================================================================
{ When selecting an item (taxa/biotope), enable the add button }
procedure TdlgWizard.lbAvailableClick(Sender: TObject);
begin
  inherited;
  CheckButtons(lbAvailable, lbSelected, bbAdd, bbRemove, bbAddAll, bbClearAll);
end; // lbAvailableClick

//==============================================================================
{ When selecting an designation, enable the add button }
procedure TdlgWizard.lbDesignationListClick(Sender: TObject);
begin
  inherited;
  CheckButtons(lbDesignationAvailable, lbDesignationSelected, bbDesignationAdd,
      bbDesignationRemove, bbDesignationsAddAll, bbDesignationClearAll);
end; // lbDesignationAvailableClick

//==============================================================================
procedure TdlgWizard.rbPlaceSelectionTypeClick(Sender: TObject);
begin
  inherited;
  ClearPlacesSelected;
  FReportChanged := True;
end; // rbPlaceSelectionTypeClick

//==============================================================================
{ Select a polygon in the tree view.  If iUpdateParent is False, then the parent
     node is not scanned to set grey ticks for partial update- useful for
     optimising the load process }
procedure TdlgWizard.SelectPolygon(ANode: TFlyNode; const ASelect: Boolean;
  AUpdateParent: Boolean = True);
var
  lNode, lChildNode: TFlyNode;
  lAllSelected, lSomeSelected: Boolean;
begin
  if Assigned(ANode) then begin
    // If no change in state, ignore.
    if ((ANode.ImageIndex = CHECKED_IMG_IDX) and ASelect) or
       ((ANode.ImageIndex = UNCHECKED_IMG_IDX) and not ASelect) then Exit;

    // Otherwise, process.
    if ASelect then ANode.ImageIndex := CHECKED_IMG_IDX
               else ANode.ImageIndex := UNCHECKED_IMG_IDX;
    ANode.SelectedIndex := ANode.ImageIndex;

    // Update all sub-nodes, but don't get back to parent.
    lNode := ANode.GetFirstChild;
    while Assigned(lNode) do begin
      SelectPolygon(lNode, ASelect, False);  // Use recursion for child nodes.
      lNode := lNode.GetNextSibling;
    end;

    // Now deal with parent nodes.
    lNode := ANode.Parent;
    while Assigned(lNode) do begin
      lChildNode := lNode.GetFirstChild;
      lAllSelected  := True;
      lSomeSelected := False;
      // Check state of all child nodes
      while Assigned(lChildNode) do begin
        // If some unchecked or greyed, not all nodes are selected.
        if lChildNode.ImageIndex in [UNCHECKED_IMG_IDX, CHECKED_GREY_IMG_IDX] then
          lAllSelected := False;
        // If some checked or greyed, some nodes are selected.
        if lChildNode.ImageIndex in [CHECKED_IMG_IDX, CHECKED_GREY_IMG_IDX] then
          lSomeSelected := True;
        lChildNode := lChildNode.GetNextSibling;
      end;
      // Update parent node state image accordingly.
      if lAllSelected then lNode.ImageIndex := CHECKED_IMG_IDX else
      if lSomeSelected then lNode.ImageIndex := CHECKED_GREY_IMG_IDX
                       else lNode.ImageIndex := UNCHECKED_IMG_IDX;
      lNode.SelectedIndex := lNode.ImageIndex;
      // Go up again.
      lNode := lNode.Parent;
    end;
  end;
end;  // SelectPolygon

//==============================================================================
{ Load the filter if the user selects a predefined filter }
procedure TdlgWizard.cmbPredefinedFiltersChange(Sender: TObject);
var lData: TStringList;
    i, lStaticId: Integer;
    lMapSheetKey, lBuff: String;
    lCursor: TCursor;
    lNode: TFlyNode;

  //----------------------------------------------------------------------------
  procedure ClearAllPolygonChecks;
  var lNode: TFlyNode;
  begin
    lNode := tvPolygonLayers.Items.GetFirstNode;
    while Assigned(lNode) do begin
      SelectPolygon(lNode, False);
      lNode := lNode.GetNextSibling;
    end;
  end;

  //----------------------------------------------------------------------------
  function LocateLayerNode(const AMapSheetKey: String): TFlyNode;
  var lNode: TFlyNode;
  begin
    Result := tvPolygonLayers.Items.GetFirstNode;
    if AppSettings.AvailableMaps.Count = 1 then
      while Assigned(Result) do begin
        if TKeyData(Result.Data).ItemKey = AMapSheetKey then Exit;  // Found the map
        Result := Result.GetNextSibling;
      end
    else begin
      lNode := Result;
      // Multiple base maps, go through one at a time to locate map sheet.
      while Assigned(lNode) do begin
        Result := lNode.GetFirstChild;
        while Assigned(Result) do begin
          if TKeyData(Result.Data).ItemKey = AMapSheetKey then Exit;  // Found the map
          Result := Result.GetNextSibling;
        end;
        lNode := lNode.GetNextSibling;
      end;
    end;
  end;

  //----------------------------------------------------------------------------
  procedure LocateAndCheckPolygon(const AParentNode: TFlyNode; const AStaticId: Integer);
  var lNode: TFlyNode;
  begin
    // Only process the polygons if the layer was actually found
    if Assigned(AParentNode) then begin
      lNode := AParentNode.GetFirstChild;
      while Assigned(lNode) do begin
        if Integer(lNode.Data) = AStaticId then begin
          SelectPolygon(lNode, True);
          Exit;
        end;
        lNode := lNode.GetNextSibling;
      end;
    end;
  end;

  //----------------------------------------------------------------------------
  procedure EnableButtons(AEnabled: Boolean);
  begin
    bbCancel.Enabled := AEnabled;
    bbNext.Enabled   := AEnabled;
    bbBack.Enabled   := AEnabled;
  end;

  //----------------------------------------------------------------------------

begin
  inherited;
  // Don't try to open file if '<new filter>' selected
  if cmbPredefinedFilters.ItemIndex = 0 then Exit;

  lCursor := HourglassCursor;
  EnableButtons(False);  // disable buttons so user doesn't try to go to next page
  lData := TStringList.Create;
  // Load Predefined Filter if valid pgf file.
  try
    lData.LoadFromFile(AppSettings.PolygonFilterPath + cmbPredefinedFilters.Text + '.pgf');
    // Check if valid polygon filter file
    try
      if lData[0] <> '<PolygonFilter>' then
        raise TExceptionPath.CreateNonCritical(ResStr_InvalidPolygonFilter)
    except
      on EStringListError do
        raise TExceptionPath.CreateNonCritical(ResStr_InvalidPolygonFilter)
    end;

    // Clear all selection before applying the filter
    ClearAllPolygonChecks;

    i := 0;
    while lData[i] <> '</PolygonFilter>' do begin
      if Copy(lData[i], 1, 9) = '<MapSheet' then begin
        // MapSheetKey is 16 chars long.
        lMapSheetKey := Copy(lData[i], Pos('"', lData[i]) + 1, 16);
        lNode := LocateLayerNode(lMapSheetKey);
        Inc(i);
        while lData[i] <> '</MapSheet>' do begin
          // Should be on a <Polygon.../> line
          lBuff := Copy(lData[i], Pos('"', lData[i]) + 1, 255);
          lStaticId := StrToInt(Copy(lBuff, 1, Pos('"', lBuff) - 1));
          LocateAndCheckPolygon(lNode, lStaticId);
          Inc(i);
        end;
      end else
        Inc(i);
    end;
  finally
    lData.Free;
    DefaultCursor(lCursor);
    EnableButtons(True);
  end;  // try .. finally
end;  // cmbPredefinedFiltersChange

//==============================================================================
  // Save current polygon selection as a new filter (pgf) file as XML formatted
  // data:
  // <PolygonFilter>
  //   <MapSheet Key=" ">
  //     <Polygon StaticId=" "/>
  //     ...
  //   </MapSheet>
  //   ...
  // </PolygonFilter>
procedure TdlgWizard.btnSaveFilterClick(Sender: TObject);
var lTopNode: TFlyNode;
    lData: TStringList;
    lFileName: String;

  procedure ProcessPolygons(AParentNode: TFlyNode);
  var lNode: TFlyNode;
  begin
    lNode := AParentNode.GetFirstChild;
    while Assigned(lNode) do begin
      // Polygon is either checked or unchecked, no greyed state.
      if lNode.ImageIndex = CHECKED_IMG_IDX then
        lData.Add('<Polygon StaticId="' + IntToStr(Integer(lNode.Data)) + '"/>');
      lNode := lNode.GetNextSibling;
    end;
  end;

  procedure ProcessLayers(AParentNode: TFlyNode);
  var lNode: TFlyNode;
  begin
    if Assigned(AParentNode) then lNode := AParentNode.GetFirstChild
                             else lNode := tvPolygonLayers.Items.GetFirstNode;
    while Assigned(lNode) do begin
      if lNode.ImageIndex <> UNCHECKED_IMG_IDX then begin
        lData.Add('<MapSheet Key="' + TKeyData(lNode.Data).ItemKey + '">');
        ProcessPolygons(lNode);
        lData.Add('</MapSheet>');
      end;
      lNode := lNode.GetNextSibling;
    end;
  end;

begin
  inherited;
  // Request filter name before saving
  lFileName := '';
  while lFileName = '' do begin
    if not InputQuery(ResStr_SaveFilterToFile, ResStr_FilterName + ':', lFileName) then Exit;

    if lFileName = '' then
      ShowInformation(ResStr_FilterFilename)
    else
    if (Pos('|', lFileName) or Pos('*', lFileName) or Pos('\', lFileName) or
        Pos('/', lFileName) or Pos('>', lFileName) or Pos('<', lFileName) +
        Pos('?', lFileName) or Pos(':', lFileName) or Pos('"', lFileName)) <> 0 then
    begin
      ShowInformation(ResStr_InvalidFilenameChars);
      lFileName := '';
    end;
  end;

  // Write filter to stringlist before saving to file in one go.
  lData := TStringList.Create;
  try
    lData.Add('<PolygonFilter>');
    if AppSettings.AvailableMaps.Count = 1 then
      // Only one base map, first node is map sheet
      ProcessLayers(nil)
    else begin
      // Multiple base maps to process.
      lTopNode := tvPolygonLayers.Items.GetFirstNode;
      while Assigned(lTopNode) do begin
        // Only go through if something has been selected.
        if lTopNode.ImageIndex <> UNCHECKED_IMG_IDX then ProcessLayers(lTopNode);
        lTopNode := lTopNode.GetNextSibling;
      end;
    end;
    lData.Add('</PolygonFilter>');
    // Save to file
    lData.SaveToFile(AppSettings.PolygonFilterPath + lFileName + '.pgf');
    // Add new name to combo box.
    cmbPredefinedFilters.ItemIndex := cmbPredefinedFilters.Items.Add(lFileName);
  finally
    lData.Free;
  end;
end;

//==============================================================================
procedure TdlgWizard.btnFindPolygonClick(Sender: TObject);
begin
  inherited;
  RequestMapForReturnData(AppSettings.AvailableMaps.DefaultMap.BaseMapKey,
                          UpdatePolygonList, False);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgWizard.btnFindPolygonDropDownClick(Sender: TObject);
var
  lPos: TPoint;
begin
  inherited;
  lPos := btnFindPolygon.ClientToScreen(Point(0, btnFindPolygon.Height));
  pmMapForPolygons.Popup(lPos.X, lPos.Y);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgWizard.MapForPolygonClick(Sender: TObject);
begin
  RequestMapForReturnData(AppSettings.AvailableMaps[TMenuItem(Sender).Tag].BaseMapKey,
                          UpdatePolygonList, False);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgWizard.RequestMapForReturnData(const ABaseMapKey: TKeyString;
  ACallbackFunction: TRequestorUpdateFunction; AWantBoundingBox: Boolean);
begin
  // Order of the next few lines is critical otherwise the wizard stays
  // pinned to the map.
  // only made visible when the UpdateBoundingBox routine comes back
  FMapAlreadyPresent := False;
  MapWindow := dmFormActions.MapWindow(ABaseMapKey);
  if not Assigned(MapWindow) then
    MapWindow := dmFormActions.MapWindow(ABaseMapKey, True)
  else
    FMapAlreadyPresent := True;

  if Assigned(MapWindow) then begin
    if AWantBoundingBox then
      MapWindow.SelectArea
    else
      MapWindow.actPointerExecute(nil);

    SetupLink(TBaseForm(MapWindow), Self, ACallbackFunction);
    MapWindow.CalledFromWizard   := True;
    MapWindow.FilterResultScreen := TfrmFilterResult(Owner);
    MapWindow.BringToFront;
    FMapCalled := True;
    Close;  // Close wizard temporarily, hide it really
  end else begin
    ModalResult := mrNone; // don't close
    bbBackClick(nil);
  end;
end;

procedure TdlgWizard.RequestMapForReturnData(const ABaseMapKey: TKeyString;
  ACallbackFunction: TRequestorUpdateFunction; AWantBoundingBox,
  AWantInitialBoundingBox: Boolean);
var
  SW, NE: TLatLong;
begin
  // Order of the next few lines is critical otherwise the wizard stays
  // pinned to the map.
  // only made visible when the UpdateBoundingBox routine comes back
  FMapAlreadyPresent := False;
  MapWindow := dmFormActions.MapWindow(ABaseMapKey);
  if not Assigned(MapWindow) then
    MapWindow := dmFormActions.MapWindow(ABaseMapKey, True)
  else
    FMapAlreadyPresent := True;

  if Assigned(MapWindow) then begin
    if AWantBoundingBox then
      MapWindow.SelectArea
    else
      MapWindow.actPointerExecute(nil);

    if AWantInitialBoundingBox and (eSWCorner.Text <> '') and
        (eNECorner.Text <> '') then
    begin
      // Values will be valid if focus has left either field - which it must do
      // in order for the user to click the map button. The exception is where
      // the value to be converted is "beyond system limits".
      // Check for relative positions of SW and NE corners is not made
      // until the user attempts to progress to the next wizard page - this
      // should be changed.
      try
        SW := ConvertToLatLong(eSWCorner.Text, AppSettings.SpatialRefSystem);
        NE := ConvertToLatLong(eNECorner.Text, AppSettings.SpatialRefSystem);
        MapWindow.SelectArea(SW, NE);
      except
        MapWindow.SelectArea;
      end;
    end;

    SetupLink(TBaseForm(MapWindow), Self, ACallbackFunction);
    MapWindow.CalledFromWizard   := True;
    MapWindow.FilterResultScreen := TfrmFilterResult(Owner);
    MapWindow.BringToFront;
    FMapCalled := True;
    Close;  // Close wizard temporarily, hide it really
  end else begin
    ModalResult := mrNone; // don't close
    bbBackClick(nil);
  end;
end;


//==============================================================================
procedure TdlgWizard.UpdatePolygonList(KeyList:TKeyList);
var lMapServerLink: TMapServerLink;
    lMapSheetKey: String;
    lNode: TFlyNode;
    lObjectID: Integer;
begin
  // Find the selected polygon from the keylist in the tree
  if KeyList.Header.TableName <> 'POLYGON' then
    MessageDlg(ResStr_InvalidPolygonRef, mtWarning, [mbOk], 0)
  else begin
    lMapServerLink := TMapServerLink.Create(nil);
    try
      // Use MapServerLink to work out the MapSheet from the given SheetID
      with lMapServerLink do begin
        ActiveDataset := MapWindow.BaseMapKey + '.gds';
        lMapSheetKey := SheetMapSheetKey(StrToInt(KeyList.Items[0].KeyField1));
      end;
      lNode := tvPolygonLayers.Items.GetFirstNode;
      // Locate BaseMap, but only if there are several to choose from.
      if AppSettings.AvailableMaps.Count > 1 then begin
        while Assigned(lNode) do begin
          if TKeyData(lNode.Data).ItemKey = MapWindow.BaseMapKey then Break;
          lNode := lNode.GetNextSibling;
        end;
        if Assigned(lNode) then
          lNode := lNode.GetFirstChild;
      end;
      // Locate MapSheet
      while Assigned(lNode) do begin
        if TKeyData(lNode.Data).ItemKey = lMapSheetKey then Break;
        lNode := lNode.GetNextSibling;
      end;
      // Locate Polygon
      if Assigned(lNode) then begin
        lNode := lNode.GetFirstChild;
        lObjectID := StrToInt(KeyList.Items[0].KeyField2);
        while Assigned(lNode) do begin
          // If found, select it.
          if Integer(lNode.Data) = lObjectID then begin
            lNode.Expand(True);
            lNode.Tree.Selected := lNode;
            SelectPolygon(lNode, True);
            Break;
          end;
          lNode := lNode.GetNextSibling;
        end;
      end;
    finally
      lMapServerLink.Free;
    end;
  end;

  if FMapAlreadyPresent then begin
    // Post message to show Wizard, without closing map.
    PostMessage(MapWindow.FilterResultScreen.Handle, WM_RUN_WIZARD, 0, 0);
    MapWindow.RefreshMap;
  end else begin
    MapWindow.Release;  // Get rid of the map window
    frmMain.Repaint;
  end;
end;

//==============================================================================
{ When cbIncludeAll is checked, the taxa or biotope selections become irrelevant
    so we disable all the controls }
procedure TdlgWizard.cbIncludeAllTxBtClick(Sender: TObject);
var
  i : Integer;
begin
  inherited;
  // One or the other, or none, but not both.
  if cbIncludeAllTxBT.Checked then chkIncludeListTxBt.Checked := False;

  // Now update the controls.
  for i := 0 to gbTaxaBioSelection.ControlCount-1 do
    with gbTaxaBioSelection do
      // All tag = 2 stay enabled, regardless.
      if Controls[i].Tag < 2 then
        Controls[i].Enabled := not cbIncludeAllTxBT.Checked;
  bbClearAllClick(nil);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgWizard.chkIncludeListTxBtClick(Sender: TObject);
var
  i : Integer;
begin
  inherited;
  // One or the other, or none, but not both.
  if chkIncludeListTxBt.Checked then cbIncludeAllTxBT.Checked := False;

  // Now update the controls.
  for i := 0 to gbTaxaBioSelection.ControlCount-1 do
    with gbTaxaBioSelection do
      // All tag = 1/2 stay enabled
      if Controls[i].Tag = 0 then
        Controls[i].Enabled := not chkIncludeListTxBt.Checked;
  bbClearAllClick(nil);
end;

//==============================================================================
procedure TdlgWizard.AddToWhereClause(var ioWhere: String; const ACondition: String);
begin
  if ioWhere = '' then ioWhere := #13#10'WHERE ' + ACondition
                  else ioWhere := ioWhere + ' AND ' + ACondition;
end;  // AddToWhereClause

//==============================================================================
{ Description : Click the attributes list - if click is over the checkbox then toggle
     selection state
  Created : 05/12/2002 }
procedure TdlgWizard.tvAttributesImageClick(Sender: TObject; Node: TFlyNode);
begin
  inherited;
  // toggle selection
  if Assigned(Node.Data) then
  begin
    TAttribute(Node.Data).Selected :=
             not TAttribute(Node.Data).Selected;
    if TAttribute(Node.Data).Selected then begin
      Node.ImageIndex := 6;
      Node.SelectedIndex := 6;
    end
    else begin
      Node.ImageIndex := 8;
      Node.SelectedIndex := 8;
    end;
  end;
  FReportChanged := True;
end;

{-------------------------------------------------------------------------------
  Enable/disable the layout combo when checked.
  Created : 05/12/2002 }
procedure TdlgWizard.rbLayoutExistingClick(Sender: TObject);
begin
  inherited;
  lblReportFile.Enabled := rbLayoutExisting.Checked;
  cmbLayout.Enabled := rbLayoutExisting.Checked;
  FReportChanged := True;
end;

{===============================================================================
 Description : Draw the selected items attributes in bold in the sort list.
             Also draws a small number to indicate the sort order of sorted
             attributes.
 Created : 6/12/2002}
procedure TdlgWizard.tvSortDrawCell(Sender: TObject; aCanvas: TCanvas;
  ACol, ARow: Integer; Rect: TRect; State: TExGridDrawState);
var
  lNode      : TFlyNode;
  lRect      : TRect;
  lAttribute : TAttribute;
    // draw a small number to indicate sort sequence of an attribute
    procedure DrawSortOrder;
    begin
      aCanvas.Font.Name := 'Small fonts';
      aCanvas.Font.Size := 6;
      aCanvas.TextOut(lRect.Left-4, lRect.Top, IntToStr(lAttribute.SortOrder));
      lRect.Left := lRect.Left + aCanvas.TextWidth(IntToStr(lAttribute.SortOrder));
      // Reset old order
      aCanvas.Font.Name := tvSort.Font.Name;
      aCanvas.Font.Size := tvSort.Font.Size;
    end;
begin
  inherited;
  lNode := tvSort.GetNodeAtRow(ARow);
  if Assigned(lNode) then // safety check
    if (TObject(lNode.Data) is TAttribute) then begin
      lAttribute := TAttribute(lNode.Data);
      lRect := Rect;
      lRect.Left := lRect.Left + (lNode.Level+2) * tvSort.Indent; //line up with original text
      aCanvas.FillRect(lRect);
      lRect.Left := lRect.Left + 4;  // space for small sort precedence number
      if lAttribute.Selected then
        // bold any selected items
        aCanvas.Font.Style := [fsBold]
      else
        aCanvas.Font.Style := [];
      if lAttribute.Sort<>stNone then
        DrawSortOrder;
      DrawChoppedText(lAttribute.Name, aCanvas, lRect);
    end;
end;

{-------------------------------------------------------------------------------
  Fills the combo box with the report wizard files.
}
procedure TdlgWizard.SetLayoutScreen;
var lCursor: TCursor;
    lData: TStringClass;
begin
  // Populate only once.
  if cmbLayout.Items.Count = 0 then begin
    lcursor := HourglassCursor;
    try
      dmGeneralData.PopulateWizardReportCombo(cmbLayout, AppSettings.ReportPath);
      //Add the 2 standard reports
      lData := TStringClass.Create;
      lData.Item := ResStr_PlacesForOccurrences;
      cmbLayout.Items.AddObject(ResStr_PlacesForOccurrences + ' (' + ResStr_StandardLayout + ')', lData);
      lData := TStringClass.Create;
      lData.Item := ResStr_OccurrencesForPlaces;
      cmbLayout.Items.AddObject(ResStr_OccurrencesForPlaces + ' (' + ResStr_StandardLayout + ')', lData);
    finally
      DefaultCursor(lCursor);
    end; // try finally
  end;
  if FSettings.WizardFile <> '' then begin
    cmbLayout.ItemIndex := cmbLayout.Items.IndexOf(
        dmGeneralData.GetReportCaption(FSettings.WizardFile));
    rbLayoutExisting.Checked:= True;
  end else
    rbLayoutNew.Checked := True;
end;

{-------------------------------------------------------------------------------
 Gets the attributes from a wizard file.
}
procedure TdlgWizard.LoadWizardFile(AstWizardFile: String);
var lXMLDoc             : IXMLDocument;
    lXMLNode            : IXMLNode;
    liAttributeIndex    : Integer;
    ltfNullStrictConvert: Boolean;
    lstrlFile           : TStringList;
begin
  if ExtractFileName(AstWizardFile) = ResStr_PlacesForOccurrences then
    LoadLocationsForOccurrencesReport
  else if ExtractFileName(AstWizardFile) = ResStr_OccurrencesForPlaces then
    LoadOccurrencesForLocationsReport
  else
  begin
    lstrlFile := TStringList.Create;
    try
      lstrlFile.LoadFromFile(AstWizardFile);
      if lstrlFile.Count > 0 then
      begin
        if (CompareText(lstrlFile[0], '<template>')=0) or
            (CompareText(lstrlFile[0], '<SQL>')=0) then
          Raise EWizard.CreateNonCritical(ResStr_OldReportFile);
      end
      else
        raise EWizard.CreateNonCritical(Format(ResStr_InvalidWizFile, [AstWizardFile]));
    finally
      lstrlFile.Free;
    end;
    ltfNullStrictConvert := NullStrictConvert;
    NullStrictConvert := False;
    try
      try
        lXMLDoc := LoadXMLDocument(AstWizardFile);
        dmWizard.ReportGenerator.Clear;

        FSettings.LoadAdditionalFilters(
            lXmlDoc.DocumentElement,
            dmWizard.ReportGenerator.PolygonSelection);
        With lXMLDoc.DocumentElement do
        begin
          FSettings.FilterSQL := ChildNodes['SQL'].Text;
          lXMLNode := ChildNodes['Attributes'].ChildNodes.First;
          while Assigned(lXMLNode) do
          begin
            if CompareText(String(lXMLNode.Attributes['type']),'standard') = 0 then
            begin
              liAttributeIndex := dmWizard.ReportGenerator.Attributes.IndexOf(
                  lXMLNode.Attributes['key']);
              if liAttributeIndex <> -1 then
                while TAttribute(dmWizard.ReportGenerator.Attributes.Objects[liAttributeIndex]).Key = lXMLNode.Attributes['key'] do
                  with TAttribute(dmWizard.ReportGenerator.Attributes.Objects[liAttributeIndex]) do
                    if AttrType = atAttribute then
                    begin
                      if VarType(lXMLNode.Attributes['index']) <>varNull then
                      begin
                        if CompareText(String(lXMLNode.Attributes['direction']), 'desc') = 0 then
                          Sort := stDesc
                        else
                          Sort := stAsc;
                        sortOrder := StrToInt(lXMLNode.Attributes['index']);
                      end;
                      Selected := lXMLNode.Attributes['visible'];
                      Position := lXMLNode.Attributes['position'];
                      break;
                    end
                    else
                    begin
                      Inc(liAttributeIndex);
                      if liAttributeIndex = dmWizard.ReportGenerator.Attributes.Count then break;
                    end;
            end
            else
              liAttributeIndex := dmWizard.ReportGenerator.Attributes.IndexOf(
                  lXMLNode.Attributes['key']);
              if liAttributeIndex <> -1 then
                while TAttribute(dmWizard.ReportGenerator.Attributes.Objects[liAttributeIndex]).Key = lXMLNode.Attributes['key'] do
                  with TAttribute(dmWizard.ReportGenerator.Attributes.Objects[liAttributeIndex]) do
                    if (AttrType = atMeasurement) and (CompareText(lXMLNode.Attributes['context'],MeasurementContextTable) =0) then
                    begin
                      if VarType(lXMLNode.Attributes['index']) <> varNull then
                      begin
                        if CompareText(lXMLNode.Attributes['direction'], 'desc') =0 then
                          Sort := stDesc
                        else
                          Sort := stAsc;
                        sortOrder := StrToInt(lXMLNode.Attributes['index']);
                      end;
                      Selected := lXMLNode.Attributes['visible'];
                      Position := lXMLNode.Attributes['position'];
                      break;
                    end
                    else if (AttrType = atDesignation) and (CompareText(lXMLNode.Attributes['parameters'],DesignationParameters) =0) then
                    begin
                      if VarType(lXMLNode.Attributes['index']) <> varNull then
                      begin
                        if CompareText(lXMLNode.Attributes['direction'], 'desc') =0 then
                          Sort := stDesc
                        else
                          Sort := stAsc;
                        sortOrder := StrToInt(lXMLNode.Attributes['index']);
                      end;
                      Selected := lXMLNode.Attributes['visible'];
                      Position := lXMLNode.Attributes['position'];
                      break;
                    end
                    else
                    begin
                      Inc(liAttributeIndex);
                      if liAttributeIndex = dmWizard.ReportGenerator.Attributes.Count then break;
                    end;

            lXMLNode := lXMLNode.NextSibling;
          end;//while
          FSettings.TemplateFile :=
              String(ChildNodes['Output'].ChildNodes['Template'].Attributes['file']);
          FSettings.SnapshotFile :=
              String(ChildNodes['Output'].ChildNodes['Snapshot'].Attributes['file']);
        end;// With lXMLDoc.DocumentElement do
        FSettings.WizardFile := AstWizardFile;
      except
        on EXMLDocError do
          raise EWizard.CreateNonCritical(Format(ResStr_InvalidWizFile, [AstWizardFile]));
        on EDOMParseError do
          raise EWizard.CreateNonCritical(Format(ResStr_InvalidWizFile, [AstWizardFile]));
      end;
    finally
      NullStrictConvert := ltfNullStrictConvert;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgWizard.rbLayoutNewClick(Sender: TObject);
begin
  inherited;
  lblReportFile.Enabled := rbLayoutExisting.Checked;
  cmbLayout.Enabled := rbLayoutExisting.Checked;
end;

{-------------------------------------------------------------------------------
  This function loads the attributes that are to be shown and sorted when the standard report
  Places for Occurrences is chosen.
}
procedure TdlgWizard.LoadLocationsForOccurrencesReport;
var lAttribute : TAttribute;
begin
  dmWizard.ReportGenerator.Clear;

  lAttribute := dmWizard.ReportGenerator.GetAttributeByName('Sample Location');
  if Assigned(lAttribute) then
  begin
    lAttribute.Selected := True;
    lAttribute.Sort := stAsc;
    lAttribute.SortOrder :=3;
  end;
  lAttribute := dmWizard.ReportGenerator.GetAttributeByName('Sample Spatial Reference');
  if Assigned(lAttribute) then lAttribute.Selected := True;
  lAttribute := dmWizard.ReportGenerator.GetAttributeByName('Sample Date');
  if Assigned(lAttribute) then lAttribute.Selected := True;
  if AppSettings.DisplayTaxonCommonNames then
    lAttribute := dmWizard.ReportGenerator.GetAttributeByName('Taxon Common Name')
  else
    lAttribute := dmWizard.ReportGenerator.GetAttributeByName('Taxon Name');
  if Assigned(lAttribute) then
  begin
    lAttribute.Selected := True;
    lAttribute.Sort := stAsc;
    lAttribute.SortOrder := 2;
  end;

  lAttribute := dmWizard.ReportGenerator.GetAttributeByName('Biotope Short Term');
  if Assigned(lAttribute) then
  begin
    lAttribute.Selected := True;
    lAttribute.Sort := stAsc;
    lAttribute.SortOrder := 2;
  end;
  lAttribute := dmWizard.ReportGenerator.GetAttributeByName('Taxon Sort Order');
  if Assigned(lAttribute) then
  begin
    lAttribute.Sort := stAsc;
    lAttribute.SortOrder := 1;
  end;
  FSettings.WizardFile := ResStr_PlacesForOccurrences;
end;

//This function loads the attributes that are to be shown and sorted when the standard report
//Occurrences For Places is chosen.
procedure TdlgWizard.LoadOccurrencesForLocationsReport;
  var lAttribute : TAttribute;
begin
  dmWizard.ReportGenerator.Clear;

  lAttribute := dmWizard.ReportGenerator.GetAttributeByName('Sample Location');
  if Assigned(lAttribute) then
  begin
    lAttribute.Selected := True;
    lAttribute.Sort := stAsc;
    lAttribute.SortOrder :=1;
  end;
  lAttribute := dmWizard.ReportGenerator.GetAttributeByName('Sample Spatial Reference');
  if Assigned(lAttribute) then lAttribute.Selected := True;
  lAttribute := dmWizard.ReportGenerator.GetAttributeByName('Sample Date');
  if Assigned(lAttribute) then lAttribute.Selected := True;
  if AppSettings.DisplayTaxonCommonNames then
    lAttribute := dmWizard.ReportGenerator.GetAttributeByName('Taxon Common Name')
  else
    lAttribute := dmWizard.ReportGenerator.GetAttributeByName('Taxon Name');
  if Assigned(lAttribute) then
  begin
    lAttribute.Selected := True;
    lAttribute.Sort := stAsc;
    lAttribute.SortOrder := 3;
  end;
  lAttribute := dmWizard.ReportGenerator.GetAttributeByName('Biotope Short Term');
  if Assigned(lAttribute) then
  begin
    lAttribute.Selected := True;
    lAttribute.Sort := stAsc;
    lAttribute.SortOrder := 3;
  end;

  lAttribute := dmWizard.ReportGenerator.GetAttributeByName('Taxon Sort Order');
  if Assigned(lAttribute) then
  begin
    lAttribute.Sort := stAsc;
    lAttribute.SortOrder := 2;
  end;
  FSettings.WizardFile := ResStr_OccurrencesForPlaces;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgWizard.tvPolygonLayersImageClicked(Sender: TObject; Node: TFlyNode);
begin
  inherited;
  SelectPolygon(tvPolygonLayers.Selected, tvPolygonLayers.Selected.ImageIndex = UNCHECKED_IMG_IDX);
  FReportChanged := True;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgWizard.InfoSelectLeavePage;
var
  lCount : Integer;
  lstSQL : String;
  FieldObj : TReportField;
begin
  FFilterFields.Clear;
  with FdmWizard do begin
    qryFieldFinder.SQL.Clear;
    // We need to select all Usable Fields
    qryFieldFinder.SQL.Add('SELECT UF.* FROM Usable_Field UF WHERE UF.Apply_To IN (');
    // For each type of report we should create an "in" clause
    // based upon the tables available

    qryFieldFinder.SQL.Add('''A'',''Y'',''Z'') AND UF.Table_Name IN (');
    // YUK!! kjg
    // must put two sets of tables into the list before getting their fields
    if rbSpeciesInfo.Checked or rbPlacesInfo.Checked then
    begin
      PopulatePlaceTaxonTablesList;
      for lCount := 0 to PlaceTables.Count - 1 do
        qryFieldFinder.SQL.Add(' ''' + TTableLinkInfo(PlaceTables.Objects[lCount]).TableName + ''', ');
    end;
    if rbPlacesInfo.Checked or rbBiotopeInfo.Checked then
    begin
      PopulatePlaceBiotopeTablesList;
      for lCount := 0 to PlaceTables.Count - 1 do
        qryFieldFinder.SQL.Add(' ''' + TTableLinkInfo(PlaceTables.Objects[lCount]).TableName + ''', ');
    end;
        // then close the "in" clause
    // first strip trailing ", "
    lstSQL := qryFieldFinder.SQL.Strings[ qryFieldFinder.SQL.Count -1];
    lstSQL := Copy(lstSQL, 0, Length(lstSQL) -2);
    qryFieldFinder.SQL.Strings[ qryFieldFinder.SQL.Count -1] := lstSQL + ')';
    // then close the braces

    // and run it
    with qryFieldFinder do begin
      Open;
      while not Eof do begin
        FieldObj := TReportField.Create;
        FieldObj.ItemKey     := FieldByName('Usable_Field_Key').AsString;
        FieldObj.TableName   := UpperCase(FieldByName('Table_Name').AsString);
        FieldObj.SQL         := FieldByName('Calculation_SQL').AsString;
        FieldObj.FieldName   := FieldByName('Field_Name').AsString;
        FieldObj.DisplayName := FieldByName('Field_Description').AsString;
        FieldObj.Selectable  := FieldByName('Selectable').AsBoolean;
        FieldObj.Filter      := FieldByName('Filterable').AsBoolean;
        FieldObj.ApplyTo     := FieldByName('Apply_To').Text[1];
        FieldObj.FieldType   := FieldByName('Field_Type').AsString;
        FieldObj.Union       := uAlways; // by default
        FFilterFields.Add(FieldObj);
        Next;
      end;  // for...
      Close;
    end;  // with qryFieldFinder
  end;  // with FdmWizard
end;

procedure TdlgWizard.TreeSort(Sender: TObject; Node1,
  Node2: TFlyNode; var Compare: Integer);
begin
  if (Node1.Parent = Node2.Parent) then
    if Node1.HasChildren then
    begin
      if Node2.HasChildren then
        Compare := CompareText(Node1.Text, Node2.Text)
      else
        Compare := 1;
    end
    else
    begin
      if Node2.HasChildren then
        Compare := -1
      else
        Compare := CompareText(Node1.Text, Node2.Text);
    end
  else
    Compare := 0;
end;


//This function removed filters from FAdvancedFilter whose fields aren't listed
// in FFilterFields
procedure TdlgWizard.RemoveInappropriateFilters;
  var i, j: Integer;
  lReportField : TReportField;
begin
  for i := FAdvancedFilter.Count -1 downto 0 do
  begin
    lReportField := TFilter(FAdvancedFilter[i]).ReportField;
    //constraints are always valid
    if CompareText(lReportField.TableName, 'Constraint') = 0 then continue;
    for j := 0 to FFilterFields.Count -1 do
      if TReportField(FFilterFields[j]).ItemKey = lReportField.ItemKey then
        break;
    if j = FFilterFields.Count then
      FAdvancedFilter.Delete(i);
  end;
end;

{-------------------------------------------------------------------------------
  This method calls methods to create the top level group (folder) nodes, the
  other folder nodes and then the main attributes (leaf nodes).
}
procedure TdlgWizard.AddHierarchicalAttributesToTreeView(const TreeView :
    TKeyboardRapidTree; const IncludeMemos : Boolean; const DefaultImageIndex :
    Integer);
var
  lIndex: Integer;
  lNodeNames: TStringList;
  lTopFolder: string;
begin
  lNodeNames := TStringList.Create;
  try
    lTopFolder := FindTopFolder;
    with dmWizard.ReportGenerator do begin
      // First do the folder that is specified as being at the top
      for lIndex := 0 to Groups.Count-1 do
        if CompareText(Groups[lIndex], lTopFolder)=0 then begin
          lNodeNames := ExtractNodeNames(Groups[lIndex]);
          AddFolderNodes(TreeView, lNodeNames);
        end;
      // Now do the rest of the folder nodes
      for lIndex := 0 to Groups.Count-1 do
        // If starts taxon, but doing a biotope only report then drop group,
        // and vice versa.
        if not (((CompareText(Copy(Groups[lIndex], 1, 5), ResStr_GroupTaxon)=0) and
              (not (otTaxa in ReportOccurrenceTypes))) or
                  ((CompareText(Copy(Groups[lIndex], 1, 7), ResStr_GroupBiotope)=0) and
              (not (otBiotopes in ReportOccurrenceTypes)))) then
          if CompareText(Groups[lIndex], lTopFolder)<>0 then begin
            lNodeNames := ExtractNodeNames(Groups[lIndex]);
            AddFolderNodes(TreeView, lNodeNames);
          end;

      // Now do main attributes
      for lIndex := Attributes.Count-1 downto 0 do begin
        lNodeNames := ExtractNodeNames(TAttribute(Attributes.Objects[lIndex]).Group);
        AddLeafNodes(TreeView, lNodeNames, TAttribute(Attributes.Objects[lIndex]), DefaultImageIndex, IncludeMemos);
      end;

      RemoveEmptyFolders(TreeView);
    end;
  finally
    lNodeNames.Free;
  end;
end;

{-------------------------------------------------------------------------------
  This method is used to see if a node exists. It takes a node and checks its
  siblings to see if a node with the caption to be searched for can be found.
  If no start node is specified, it starts at the first top level node.
}
function TdlgWizard.FindNode(const ATreeView: TKeyboardRapidTree; ANodeName:
    String; AStartNode: TFlyNode): TFlyNode;
var
  lCurrentNode: TFlyNode;
begin
  Result := nil;

  if Assigned(AStartNode) then lCurrentNode := AStartNode.getFirstChild
                          else lCurrentNode := ATreeView.Items.GetFirstNode;

  while Assigned(lCurrentNode) do begin
    if lCurrentNode.Text = ANodeName then
      Result := lCurrentNode;
    lCurrentNode := lCurrentNode.GetNextSibling;
  end;
end;

{-------------------------------------------------------------------------------
  Takes the value from the 'Item_Group' field in the 'Report_Attribute' table
  and splits it up if required. It then puts the results into a string list.
}
function TdlgWizard.ExtractNodeNames(AString: String): TStringList;
var
  lIndex: Integer;
begin
  Result := TStringList.Create;
  // A 'feature' of DelimitedText in TStringList is that spaces are treated as
  // delimitters. Therefore, replace them, then put spaces back later.
  AString := StringReplace(AString, ' ', '¬', [rfReplaceAll]);
  with Result do begin
    Delimiter := '\';
    DelimitedText := AString;
    for lIndex := 0 to Count - 1 do
      Strings[lIndex] := StringReplace(Strings[lIndex], '¬', ' ', [rfReplaceAll]);
  end;
end;

{-------------------------------------------------------------------------------
  Add folder/sub folder nodes to the treeview.
}
procedure TdlgWizard.AddFolderNodes(const ATreeView: TKeyboardRapidTree;
    ANodeNames: TStringList);
var
  lIndex: Integer;
  lFlyNode, lParentNode: TFlyNode;
begin
  lParentNode := nil;
  for lIndex := 0 to ANodeNames.Count - 1 do begin
    lFlyNode := FindNode(ATreeView, ANodeNames.Strings[lIndex], lParentNode);
    if not Assigned(lFlyNode) then begin
      if lIndex = 0 then begin
        lFlyNode := ATreeView.Items.Add(nil, ANodeNames.Strings[lIndex]);
        AddMeasurementFolderNodes(ATreeView, lFlyNode, ANodeNames.Strings[lIndex]);
      end
      else
        lFlyNode := ATreeView.Items.AddChild(lParentNode, ANodeNames.Strings[lIndex]);
      lFlyNode.ImageIndex := -1;
      lFlyNode.SelectedIndex := -1;
    end;
    lParentNode := lFlyNode;
  end;
end;

{-------------------------------------------------------------------------------
  Add a leaf node to the tree.
}
procedure TdlgWizard.AddLeafNodes(const ATreeView: TKeyboardRapidTree;
    ANodeNames: TStringList; AAttribute: TAttribute; ADefaultImageIndex:
    Integer; AIncludeMemos: Boolean);
var
  lIndex: Integer;
  lFlyNode, lParentNode: TFlyNode;
begin
  lParentNode := nil;

  with AAttribute do begin
    // Must have a field to be valid
    if AttributeFields.Count > 0 then
      // Check first field is sortable (not a blob/memo)
      if (CompareText(TAttributeField(AttributeFields[0]).FieldType, 'TEXT') <> 0) or AIncludeMemos then
        if (AttrType = atAttribute) then
          for lIndex := 0 to ANodeNames.Count - 1 do begin
            lFlyNode := FindNode(ATreeView, ANodeNames.Strings[lIndex], lParentNode);
            lParentNode := lFlyNode;
          end
        else if (AttrType = atMeasurement) then
          lParentNode := GetMeasurementFolderNode(ATreeView, AAttribute)
        else if (AttrType = atDesignation) then
        begin
          lFlyNode := FindNode(ATreeView, ResStr_RptGenTaxon, nil);
          lFlyNode := FindNode(ATreeView, ResStr_RptGenDesignationSets, lFlyNode);
          lParentNode := FindNode(ATreeView, Group, lFlyNode);
        end;
  end;

  lFlyNode := ATreeView.Items.AddChildObjectFirst(lParentNode,
                                            AAttribute.Name, AAttribute);

  SetUpImageIndex(ATreeView, lFlyNode, ADefaultImageIndex);
end;

{-------------------------------------------------------------------------------
  The user can configure a group that will always appear at the top of the
  treeview by putting a value into the Setting table in the database. This
  method sees if a group has been set up to appear at the top, and returns its
  name as a string if it has.
}
function TdlgWizard.FindTopFolder: string;
begin
  Result := '';
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := 'SELECT Data FROM Setting WHERE Name=''Attr Grp 1''';
    Open;
    try
      if (RecordCount>0) then
        Result := FieldByName('DATA').AsString
    finally
      Close;
    end; // try
  end; // with dmGeneralData.qryAllPurpose
end;

{-------------------------------------------------------------------------------
  Adds the measurement folder nodes to the tree where necessary (also designation nodes).
}
procedure TdlgWizard.AddMeasurementFolderNodes(ATreeView: TKeyboardRapidTree;
    AFlyNode: TFlyNode; ANodeName: String);
var
  lFlyNode: TFlyNode;
  i: Integer;
begin
  lFlyNode := nil;
  // Add child nodes for measurements
  if ANodeName = ResStr_RptGenTaxon then
  begin
    lFlyNode := ATreeView.Items.AddChild(AFlyNode, ResStr_RptGenMeasurements);
    lFlyNode.ImageIndex := -1;
    lFlyNode.SelectedIndex := -1;
    lFlyNode := ATreeView.Items.AddChild(AFlyNode, ResStr_RptGenDesignationSets);
    AddMeasurementFolderNodes(ATreeView, lFlyNode, ResStr_RptGenDesignationSets);
  end else
  if ANodeName = ResStr_RptGenBiotope then
    lFlyNode := ATreeView.Items.AddChild(AFlyNode, ResStr_RptGenMeasurements)
  else
  if ANodeName = ResStr_RptGenSample then begin
    lFlyNode := ATreeView.Items.AddChild(AFlyNode, ResStr_RptGenSampleMeasurements);
    lFlyNode.ImageIndex := -1;
    lFlyNode.SelectedIndex := -1;
    lFlyNode := ATreeView.Items.AddChild(AFlyNode, ResStr_RptGenLocationMeasurements);
  end else
  if ANodeName = ResStr_RptGenDesignationSets then
  begin
    lFlyNode := ATreeView.Items.AddChild(AFlyNode, ResStr_RptGenAllDesignations);
    lFlyNode.ImageIndex := -1;
    lFlyNode.SelectedIndex := -1;
    lFlyNode := ATreeView.Items.AddChild(AFlyNode, ResStr_RptGenUserFilteredDesignations);
    with cmbDesignationSet do
      for i := 0 to Items.Count - 1 do
      begin
        lFlyNode.ImageIndex := -1;
        lFlyNode.SelectedIndex := -1;
        lFlyNode := ATreeView.Items.AddChild(AFlyNode, Items[i]);
      end;
  end;
  if Assigned(lFlyNode) then begin
    lFlyNode.ImageIndex := -1;
    lFlyNode.SelectedIndex := -1;
  end;
end;

{-------------------------------------------------------------------------------
  Add a table to the list of joins for the query
}
procedure TdlgWizard.AddNamedTableToQuery(const ATableName: string;
    AAvailableTableList, ARequiredTableList: TStringList);
var
  liTblIndex: integer;
begin
  liTblIndex := AAvailableTableList.IndexOf(Uppercase(ATableName));
  if liTblIndex > -1 then
    AddTableToQuery(TTableLinkInfo(AAvailableTableList.Objects[liTblIndex]),
                    AAvailableTableList,ARequiredTableList);
end;

{-------------------------------------------------------------------------------
  If an attribute is a measurement, this method retrieves the node that will
  be its parent in the tree.
}
function TdlgWizard.GetMeasurementFolderNode(ATreeView: TKeyboardRapidTree;
    AAttribute: TAttribute): TFlyNode;
var
  lFlyNode: TFlyNode;
begin
  lFlyNode := nil;
  with AAttribute do begin
    if MeasurementContextTable = TN_TAXON_OCCURRENCE_DATA then begin
      lFlyNode := FindNode(ATreeView, ResStr_RptGenTaxon, nil);
      lFlyNode := FindNode(ATreeView, ResStr_RptGenMeasurements, lFlyNode);
    end
    else if MeasurementContextTable = TN_BIOTOPE_OCCURRENCE_DATA then begin
      lFlyNode := FindNode(ATreeView, ResStr_RptGenBiotope, nil);
      lFlyNode := FindNode(ATreeView, ResStr_RptGenMeasurements, lFlyNode);
    end
    else if MeasurementContextTable = TN_SAMPLE_DATA then begin
      lFlyNode := FindNode(ATreeView, ResStr_RptGenSample, nil);
      lFlyNode := FindNode(ATreeView, ResStr_RptGenSampleMeasurements, lFlyNode);
    end
    else if MeasurementContextTable = TN_LOCATION_DATA then begin
      lFlyNode := FindNode(ATreeView, ResStr_RptGenSample, nil);
      lFlyNode := FindNode(ATreeView, ResStr_RptGenLocationMeasurements, lFlyNode);
    end;
  end;
  Result := lFlyNode;
end;

{-------------------------------------------------------------------------------
  This method removes groups (top level 'folder' nodes) that have no children.
}
procedure TdlgWizard.RemoveEmptyFolders(ATreeView: TKeyboardRapidTree);
var
  lCurrentNode, lNextNode: TFlyNode;
begin
  lCurrentNode := ATreeView.Items.GetFirstNode;
  lNextNode := lCurrentNode;

  while Assigned(lNextNode) do begin
    lNextNode := lCurrentNode.GetNextSibling;
    if not lCurrentNode.HasChildren then
      lCurrentNode.Free;
    lCurrentNode := lNextNode;
  end;
end;

{-------------------------------------------------------------------------------
  When a node is created, its image index needs to be set up appropriately.
}
procedure TdlgWizard.SetUpImageIndex(ATreeView: TKeyboardRapidTree; AFlyNode:
    TFlyNode; ADefaultImageIndex: Integer);
begin
  if ATreeview = tvAttributes then begin
    if Assigned(AFlyNode.Data) then begin
      if TAttribute(AFlyNode.Data).Selected then begin
        AFlyNode.ImageIndex := 6;
        AFlyNode.SelectedIndex := 6;
      end
      else begin
        AFlyNode.ImageIndex := 8;
        AFlyNode.SelectedIndex := 8;
      end;
    end;
  end else if ATreeview = tvSort then begin
    case TAttribute(AFlyNode.Data).Sort of
      stNone : AFlyNode.ImageIndex := ADefaultImageIndex; // blank
      stAsc : AFlyNode.ImageIndex := SORT_UP_IMG_IDX;
      stDesc : AFlyNode.ImageIndex := SORT_DOWN_IMG_IDX;
    end;
    AFlyNode.SelectedIndex := AFlyNode.ImageIndex;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgWizard.rbInfoAllClick(Sender: TObject);
begin
  inherited;
  FReportChanged := True;
end;

procedure TdlgWizard.cmbLayoutChange(Sender: TObject);
begin
  inherited;
  if rbLayoutExisting.Checked and (cmbLayout.ItemIndex <> -1) then
    LoadWizardFile(IncludeTrailingPathDelimiter(AppSettings.ReportPath) +
                       TStringClass(cmbLayout.Items.Objects[cmbLayout.ItemIndex]).Item);
  FReportChanged := True;
end;

{-------------------------------------------------------------------------------
  Adds filtering on Surveys to the Report.
}
procedure TdlgWizard.FilterOnSurveys(const iAvailableTables : TStringList;
    const ioRequiredTables : TStringList; var ioWhere : String);
begin
  AddNamedTableToQuery('Survey', iAvailableTables, ioRequiredTables);
  AddToWhereClause(
      ioWhere,
      Format(
          'Survey.Survey_Key NOT IN ('
          + 'SELECT Survey_Key FROM User_Survey_Restriction USR WHERE  USR.Name_Key = ''%s'')',
          [AppSettings.UserID]));
end;

{-------------------------------------------------------------------------------
  Applies a filter so that only the selected designation Types are displayed.
}
procedure TdlgWizard.FilterOnDesignations(const iAvailableTables : TStringList;
    const ioRequiredTables : TStringList; var ioWhere : String);
var
  i: integer;
  designationTypeKeyList: string;
begin
  // Builds a list of all the selected designations
  designationTypeKeyList :=
      '''' + TWizardKeyObject(lbDesignationSelected.Items.Objects[0]).Key + '''';
  for i := 1 to lbDesignationSelected.Items.Count - 1 do
  begin
    designationTypeKeyList := designationTypeKeyList +
        ',''' + TWizardKeyObject(lbDesignationSelected.Items.Objects[i]).Key + '''';
  end;
  AddNamedTableToQuery('Index_Taxon_Designation', iAvailableTables, ioRequiredTables);
  AddToWhereClause(ioWhere, 'Index_Taxon_Designation.Taxon_Designation_Type_Key IN ('
                                  + designationTypeKeyList + ')');
end;

{-------------------------------------------------------------------------------
  When the outer panel is resized, the inner panel is resized to fit it.
}
procedure TdlgWizard.pnlOuterResize(Sender: TObject);
begin
  inherited;
  pnlInner.Width := pnlOuter.Width;
  pnlInner.Height := pnlOuter.Height;

  ResizeColumns(lbSourcesAvailable, pnlButtonsSources, pnlSourcesSelected, gbSourceselect);
  ResizeColumns(tvAdminAvailable, pnlButtonsAdmin, pnlAdminSelected, gbAdminSelection);
  ResizeColumns(lbAvailable, pnlButtonsTaxaBio, pnlSelected, gbTaxaBioSelection);
  ResizeColumns(lbPlacesAvailable, pnlButtonsPlaces, pnlPlacesSelected, gbPlacesSelection);
  ResizeColumns(lbDesignationAvailable, pnlDesignationButtons, pnlDesignationSelected, gbPlacesSelection);
end;

{-------------------------------------------------------------------------------
  Resizes a two column layout evenly, with a small panel in between.
}
procedure TdlgWizard.ResizeColumns(leftColumn, midColumn, rightColumn, container : TControl);
var
  availableWidth, halfWidth: Integer;
begin
  availableWidth := container.Width - 8 * 4 - midColumn.Width;

  halfWidth := availableWidth div 2;

  leftColumn.Width := halfWidth;
  midColumn.Left := leftColumn.Left + leftColumn.Width + 8;
  rightColumn.Left := midColumn.Left + midColumn.Width + 8;
  rightColumn.Width := halfWidth;
end;

(**
 * Draw handler for the taxon search box, to ensure italics show correctly.
 *)
procedure TdlgWizard.TaxonListBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  dmInterface.DrawTerm(TListBox(Control).Canvas, Rect, TListBox(Control).Items[Index], odSelected in State);
end;

(**
 * Handle drawing of the summary tree view, in order that italics can be displayed
 * properly in HTML species names. Mantis 286.
 *)
procedure TdlgWizard.tvSummaryDrawCell(Sender: TObject; aCanvas: TCanvas;
  ACol, ARow: Integer; Rect: TRect; State: TExGridDrawState);
var
  lNode      : TFlyNode;
  lRect      : TRect;
  shift: Integer;
begin
  inherited;
  lNode := tvSummary.GetNodeAtRow(ARow);
  if Assigned(lNode) then begin

    // Set the font colour to differentiate between highlighted and non-highlighted nodes
    if lNode.Selected then
      ACanvas.Font.Color := clHighlightText
    else
      ACanvas.Font.Color := clWindowText;
    lRect := Rect;
    shift      := 1 + Ord(lNode.StateIndex > 0) + Ord(lNode.ImageIndex <> -1);
    lRect.Left := lRect.Left + (lNode.Level + shift) * tvSummary.Indent;
    ACanvas.FillRect(lRect);
    dmInterface.DrawTerm(tvSummary.Canvas, lRect, lNode.Text, lNode.Selected);
  end;;
end;

end.
