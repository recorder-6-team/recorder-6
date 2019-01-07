//==============================================================================
//  Unit:        Observations
//
//  Implements:  TfrmObservations
//
//  Description: Displays the hierarchy of observations from Surveys down to
//               Biotope and Taxon Occurrences. The right hand side panel
//               display a different form depending on the type of node selected
//               in the hierarchy.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 360 $
//    $Date: 8/04/10 17:16 $
//    $Author: Andrewkemp $
//
//==============================================================================

unit Observations;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseFormUnit, ComCtrls, StdCtrls, Buttons, ExtCtrls, BaseExportableUnit,
  ImgList, Db, ObservationData, DataClasses, JNCCDatasets, DropSource, ActnList,
  BaseChildUnit, BaseDockedForm, ExceptionForm, VagueDate, OnlineHelp, Menus,
  SpatialRefFuncs, Find, Finder, Constants, exgrid, RapTree, GeneralFunctions,
  ImageListButton, KeyboardRapidTree, Recorder2000_TLB, SQLConstants, ClipBrd,
  ADODb, Variants, CommCtrl, ADODB_TLB, OleTools, ActiveX, CRCommonClasses,
  Grids, HierarchyNodes, ExternalFilter, TempData_ADO;

type
  EObservations = class(TExceptionPath);

  TRelatedData = (rdNone, rdSurvey, rdEvent, rdSample, rdOccurrence);

  TNodeType = (ntTaxonOcc, ntBiotopeOcc, ntSample, ntEvent, ntSurvey, ntSurveyTag);

  TGetParentKeysForChildKeys = procedure(childKeys: TKeyList; parentKeys: TEditableKeyList) of object;

  TfrmObservations = class(TBaseExportable, IProvidesOccurrencesSQL, IRecorderDetailScreenEvents)
    DrillSplitter            : TSplitter;
    pnlDetails               : TPanel;
    pnlButtons               : TPanel;
    pnlDrill                 : TPanel;
    pnlLabel                 : TPanel;
    Label1                   : TLabel;
    pnlButtons2              : TPanel;
    bbCheckAll               : TBitBtn;
    mnuEdit                  : TMenuItem;
    mnuEditAdd               : TMenuItem;
    mnuEditEdit              : TMenuItem;
    mnuEditDelete            : TMenuItem;
    N3                       : TMenuItem;
    mnuEditCut               : TMenuItem;
    mnuEditCopy              : TMenuItem;
    mnuEditPaste             : TMenuItem;
    N1                       : TMenuItem;
    mnuEditFind              : TMenuItem;
    mnuEditSortBy            : TMenuItem;
    N2                       : TMenuItem;
    mnuEditBold              : TMenuItem;
    mnuEditItalic            : TMenuItem;
    mnuEditUnderline         : TMenuItem;
    mnuEditFilter            : TMenuItem;
    mnuEditAddSurvey         : TMenuItem;
    mnuEditAddEvent          : TMenuItem;
    mnuEditAddSample         : TMenuItem;
    mnuEditAddTaxonOcc       : TMenuItem;
    mnuEditAddBioOcc         : TMenuItem;
    mnuEditSortSurveyName    : TMenuItem;
    mnuEditSortSurveyRunBy   : TMenuItem;
    mnuEditSortEventDate     : TMenuItem;
    mnuEditSortLocation: TMenuItem;
    mnuEditSortSampleRef     : TMenuItem;
    mnuEditSortSampleDate    : TMenuItem;
    mnuEditSortSampleType    : TMenuItem;
    mnuEditSortTaxScientific : TMenuItem;
    mnuEditSortTaxCommon     : TMenuItem;
    mnuEditSortBioCode       : TMenuItem;
    mnuEditSortBioShortName  : TMenuItem;
    pmRelatedData            : TPopupMenu;
    mnuRelEvents             : TMenuItem;
    mnuRelSamples            : TMenuItem;
    mnuRelOccur              : TMenuItem;
    mnuRelLocations          : TMenuItem;
    mnuRelIndivOrg           : TMenuItem;
    mnuRelDocuments: TMenuItem;
    mnuRelSep                : TMenuItem;
    pmSortBy                 : TPopupMenu;
    pmSortSurveyName         : TMenuItem;
    pmSortSurveyRunBy        : TMenuItem;
    pmSortEventDate          : TMenuItem;
    pmSortLocation           : TMenuItem;
    pmSortSampleRef          : TMenuItem;
    pmSortSampleDate         : TMenuItem;
    pmSortSampleType         : TMenuItem;
    pmSortTaxScientific      : TMenuItem;
    pmSortTaxCommon          : TMenuItem;
    pmSortBioCode            : TMenuItem;
    pmSortBioShortName       : TMenuItem;
    pmAdd                    : TPopupMenu;
    pmAddSurvey              : TMenuItem;
    pmAddEvent               : TMenuItem;
    pmAddSample              : TMenuItem;
    pmAddTaxonOcc            : TMenuItem;
    pmAddBioOcc              : TMenuItem;
    pmHierarchy              : TPopupMenu;
    pmHAdd                   : TMenuItem;
    pmHAddBioOcc             : TMenuItem;
    pmHAddTaxonOcc           : TMenuItem;
    pmHAddSample             : TMenuItem;
    pmHAddEvent              : TMenuItem;
    pmHAddSurvey             : TMenuItem;
    pmHSortBy                : TMenuItem;
    pmHSortBioShortName      : TMenuItem;
    pmHSortBioCode           : TMenuItem;
    pmHSortTaxCommon         : TMenuItem;
    pmHSortTaxScientific     : TMenuItem;
    pmHSortSampleType        : TMenuItem;
    pmHSortSampleDate        : TMenuItem;
    pmHSortSampleRef         : TMenuItem;
    pmHSortLocation          : TMenuItem;
    pmHSortEventDate         : TMenuItem;
    pmHSortSurveyRunBy       : TMenuItem;
    pmHSortSurveyName        : TMenuItem;
    ilObservation            : TImageList;
    ilObservationState       : TImageList;
    shpObservations          : TShape;
    bbShowAll: TButton;
    alObservations: TActionList;
    actAddSurvey: TAction;
    actAddEvent: TAction;
    actAddSample: TAction;
    actAddTaxonOcc: TAction;
    actAddBioOcc: TAction;
    actSortSurveyName: TAction;
    actSortSurveyRunBy: TAction;
    actSortEventDate: TAction;
    actSortEventLocation: TAction;
    actSortSampleReference: TAction;
    actSortSampleDate: TAction;
    actSortSampleType: TAction;
    actSortTaxonScientific: TAction;
    actSortTaxonCommon: TAction;
    actSortBioCode: TAction;
    actSortBioName: TAction;
    actFind: TAction;
    actFilter: TAction;
    mnuEditReturnData: TMenuItem;
    actShowMetadata: TAction;
    mnuEditShowMetadata: TMenuItem;
    actFindOnMap: TAction;
    mnuEditFindOnMap: TMenuItem;
    pmHFindOnMap: TMenuItem;
    tvObservations: TKeyboardRapidTree;
    OccurrencesforPlacesReport1: TMenuItem;
    PlacesforOccurrencesReport1: TMenuItem;
    bbAdd: TImageListButton;
    bbEdit: TImageListButton;
    bbDelete: TImageListButton;
    bbRelatedData: TImageListButton;
    pmHQuickReports: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    pmHCut: TMenuItem;
    pmHPaste: TMenuItem;
    pmHCopy: TMenuItem;
    pmHValidateItem: TMenuItem;
    pmHBatchUpdate: TMenuItem;
    mnuPlaceHolder: TMenuItem;
    pmAddSurveyTag: TMenuItem;
    actAddSurveyTag: TAction;
    SurveyTag1: TMenuItem;
    SurveyTag2: TMenuItem;
    pmHSepLoad: TMenuItem;
    pmHLoadIntoRecordCard: TMenuItem;
    pmSortTaxSortOrder: TMenuItem;
    actSortTaxonSortOrder: TAction;
    procedure FormResize(Sender: TObject);
    procedure DrillSplitterPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvObservationsChange(Sender: TObject; Node: TFlyNode);
    procedure bbRelatedDataClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure bbAddClick(Sender: TObject);
    procedure actFilterExecute(Sender: TObject);
    procedure mnuRelEventsClick(Sender: TObject);
    procedure mnuRelSamplesClick(Sender: TObject);
    procedure mnuRelOccurClick(Sender: TObject);
    procedure actAddTaxonOccExecute(Sender: TObject);
    procedure actAddBioOccExecute(Sender: TObject);
    procedure bbCheckAllClick(Sender: TObject);
    procedure tvObservationsExpanding(Sender: TObject; Node: TFlyNode;
      var AllowExpansion: Boolean);
    procedure actAddSurveyExecute(Sender: TObject);
    procedure actAddEventExecute(Sender: TObject);
    procedure actAddSampleExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bbDeleteClick(Sender: TObject);
    procedure actSortSurveyNameExecute(Sender: TObject);
    procedure actSortSurveyRunByExecute(Sender: TObject);
    procedure actSortEventDateExecute(Sender: TObject);
    procedure actSortEventLocationExecute(Sender: TObject);
    procedure actSortSampleReferenceExecute(Sender: TObject);
    procedure actSortSampleDateExecute(Sender: TObject);
    procedure actSortSampleTypeExecute(Sender: TObject);
    procedure actSortBioCodeExecute(Sender: TObject);
    procedure actSortBioNameExecute(Sender: TObject);
    procedure actSortTaxonScientificExecute(Sender: TObject);
    procedure actSortTaxonCommonExecute(Sender: TObject);
    procedure mnuRelDocumentsClick(Sender: TObject);
    procedure bbShowAllClick(Sender: TObject);
    procedure mnuRelLocationsClick(Sender: TObject);
    procedure mnuRelIndivOrgClick(Sender: TObject);
    procedure bbEditClick(Sender: TObject);
    procedure actShowMetadataExecute(Sender: TObject);
    procedure actFindOnMapExecute(Sender: TObject);
    procedure tvObservationsDrawCell(Sender: TObject; ACanvas: TCanvas;
      ACol, ARow: Integer; Rect: TRect; State: TExGridDrawState);
    procedure FormDeactivate(Sender: TObject);
    procedure tvObservationsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvObservationsStateImageClicked(Sender: TObject; Node: TFlyNode);
    procedure pmHQuickReportsClick(Sender: TObject);
    procedure tvObservationsExit(Sender: TObject);
    procedure pmHBatchUpdateClick(Sender: TObject);
    procedure actAddSurveyTagExecute(Sender: TObject);
    procedure actSortTaxonSortOrderExecute(Sender: TObject);
  private
    FDetailForm    :TfrmBaseDockedForm;
    FdmObservation :TdmObservation;
    FSelectedItem  :TFlyNode;
    FRelatedData   :TRelatedData;
    FClearingTree  :Boolean;
    FAddingNewNode :Boolean;
    FSurveyTagKeys :TStringList;
    FSurveyKeys    :TStringList;
    FEventKeys     :TStringList;
    FSampleKeys    :TStringList;
    FTaxonOccKeys  :TStringList;
    FBioOccKeys    :TStringList;
    FSurveyField     : String;
    FEventField      : String;
    FSampleField     : String;
    FTaxonOccField   : String;
    FBiotopeOccField : String;
    FSurveyToReselect: TKeyString;
    FSurveyTagToReselect: TKeyString;
    //--------------------------------------------------------------------------
    FNodeMan: IOccurrenceNodeManager;
    FNodeImagesMap: TStringList;
    FNodeStateImagesMap: TStringList;
    FDetailScreen: TOleProxy;
    FDynamicMenuLists: TInterfaceList;
    //--------------------------------------------------------------------------
    procedure SetDetailForm(const Value: TfrmBaseDockedForm);
    procedure UpdateSubMenus(const ANode: TFlyNode = nil);
    procedure CheckDetailFormClass(const AFormClass: TFormClass);
    procedure MergeAddinMenusIntoHierarchyPopup;
    procedure ResetDetailForm(const ACaption: String; const AFormClass: TFormClass);
    procedure PerformNodeChange(Node: TFlyNode);
    procedure PopulateSurveyTagLevel;
    procedure PopulateSurveyLevel(surveyTagNode: TFlyNode);
    procedure PopulateEventLevel(surveyNode: TFlyNode);
    procedure PopulateSampleLevel(eventNode: TFlyNode);
    procedure PopulateBiotopeOccurrenceLevel(sampleNode: TFlyNode);
    procedure PopulateTaxonOccurrenceLevel(sampleNode: TFlyNode);
    procedure PopulateOccurrenceLevel(sampleNode: TFlyNode);
    procedure ClearTree;
    procedure KillChildren(ANode:TFlyNode);
    procedure WMDiscardObservation(var Msg:TMessage); message WM_DISCARD_OBSERVATION;
    procedure WMRefreshObservations(var Msg: Messages.TMessage); message WM_REFRESH_OBSERVATIONS;
    procedure WMRefreshTermLists(var Msg:TMessage); message WM_REFRESH_TERM_LISTS;
    procedure WMRefreshSpatialRefSystem(var Msg: TMessage); message WM_REFRESH_SPATIAL_REF_SYSTEM;
    procedure WMRefreshSpatialRef(var Msg: TMessage); message WM_REFRESH_SPATIAL_REF;
    procedure WMRefreshDocuments(var Msg: TMessage); message WM_REFRESH_DOCUMENTS;
    procedure WMRefreshNames(var Msg: TMessage); message WM_REFRESH_NAMES;
    procedure WMRefreshLocationDetails(var Msg: TMessage); message WM_REFRESH_LOCATION_DETAILS;
    procedure WMRefreshColours(var Msg: TMessage); message WM_REFRESH_COLOURS;
    procedure WMRefreshTaxonDic(var Msg: TMessage); message WM_REFRESH_TAXON_DIC;
    procedure WMRefreshBiotopeDic(var Msg: TMessage); message WM_REFRESH_BIOTOPE_DIC;
    procedure WMImportedComplete(var Msg: TMessage); message WM_IMPORTED_COMPLETE;
    procedure SetupObjects;
    procedure FreeObjects;
    procedure DeleteItem(ANode : TFlyNode);
    procedure DisplaySurveys; overload;
    procedure DisplaySurveys(const ASurveyList: TKeyList); overload;
    procedure DisplayEvents(const AEventList:TKeyList);
    procedure DisplaySamples(const ASampleList:TKeyList);
    procedure DisplayOccurrences(const AOccurrenceList:TKeyList);
    procedure UpdateOrder(ANode: TFlyNode);
    procedure UpdateSampleCardState(ANode: TFlyNode);
    function CheckDeletedNode(const AMode: TEditMode): Boolean;
    procedure DropNode(const Sender: TObject; const iFormat: integer;
      const iSourceData: TKeyList; const iTextStrings: TStringList;
      const iIsPasteOperation: Boolean; var ioHandled: Boolean);
    procedure DropTaxonOcc(const AKey: TKeyString; ASourceNode, ADestNode: TFlyNode; const AMsg:
        String);
    procedure DropBiotopeOcc(const AKey: TKeyString; ASourceNode, ADestNode: TFlyNode; const AMsg:
        String);
    procedure DropSample(const AKey: TKeyString; ASourceNode, ADestNode: TFlyNode; const AMsg:
        String);
    procedure DropEvent(const AKey: TKeyString; ASourceNode, ADestNode: TFlyNode; const AMsg:
        String);
    procedure DropSurvey(const AKey: TKeyString; ASourceNode, ADestNode: TFlyNode; move: Boolean;
        const AMsg: String);
    function AllowedMove(const AKey: TKeyString; ANodeType: TNodeType): Boolean;
    procedure GetNodeData(const Sender: TObject; var oDropSource: TJNCCDropSource);
    function GetTaxonNameForOccurrence(const AKey: TKeyString): String;
    function GetBiotopeNameForOccurrence(const AKey: TKeyString): String;
    function GetLocationName(Recordset: ADODB._Recordset): String; overload;
    function GetSampleName(const AKey: TKeyString): String; overload;
    function GetSampleName(Recordset: ADODB._Recordset): String; overload;
    function GetEventName(const AKey: TKeyString): String; overload;
    function GetEventName(Recordset: ADODB._Recordset): String; overload;
    function GetVagueDate(const ATable: String; const AKey: TKeyString): TVagueDate;
    function GetDetDate(const ATable: String; const AKey: TKeyString): TVagueDate;
    function GetSampleKeyForTaxonOcc(const AKey: TKeyString): String;
    function GetSampleKeyForBiotopeOcc(const AKey: TKeyString): String;
    function GetEventKeyForSample(const AKey: TKeyString): String;
    procedure SetupNameQuery(qry: TJNCCQuery; NameFor: TNodeType;
      const AKey: String);
    procedure MoveTaxonOcc(const TaxonOccKey, SampleKey: TKeyString);
    procedure MoveBiotopeOcc(const BiotopeOccKey, SampleKey: TKeyString);
    procedure MoveSample(const SampleKey, EventKey: TKeyString);
    procedure MoveEvent(const EventKey, SurveyKey: TKeyString);
    procedure GetSpatialRefForSample(const AKey: TKeyString;
      out sRef, LocKey, sRefSys, LocName: String);
    procedure GetSpatialRefForEvent(const AKey: TKeyString;
      out SRef, sRefSys, LocName: String);
    function CheckRecordersInEvent(const ASampleKey, AnEventKey: TKeyString): Boolean;
    procedure AddRecordersToNewEvent(const ASampleKey, AnEventKey: TKeyString);
    procedure ReSortSurvey(const ASort: String);
    function CheckedSelectedNode(ANode: TFlyNode; NodeType: TNodeType;
      const AKey: TKeyString): TFlyNode;
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
    procedure DragOverCheck(APoint: TPoint; const ATable, AFieldKey: String;
      var Accept: Boolean);
    // IProvidesOccurrencesSQL
    function Get_CanProvideSQL: WordBool; safecall;
    function Get_OccurrencesSQL(const ATypes: WideString): WideString; safecall;
    // IRecorderDetailScreenEvents
    procedure OnEditModeChange; safecall;
    procedure OnRefreshScreenInfo(const AKey: WideString; const ACaption: WideString); safecall;
    function StripFormatting(const AText: string): string;
    function SelectBioOcc(const ANodeKey: string): boolean;
    function SelectEvent(const ANodeKey: string): boolean;
    function SelectSample(const ANodeKey: string): boolean;
    function SelectSurvey(const ANodeKey: string): boolean;
    function SelectSurveyTag(const ANodeKey: String): Boolean;
    function SelectTaxOcc(const ANodeKey: string): boolean;
    procedure GetSampleKeysForBiotopeOccurrences(biotopeOccurrenceKeys: TKeyList; sampleKeys: TEditableKeyList);
    procedure GetSampleKeysForTaxonOccurrences(taxonOccurrenceKeys: TKeyList; sampleKeys: TEditableKeyList);
    procedure GetEventKeysForSamples(sampleKeys: TKeyList; eventKeys: TEditableKeyList);
    procedure GetSurveyKeysForEvents(eventKeys: TKeyList; surveyKeys: TEditableKeyList);
    procedure GetSurveyTagsForSurveys(surveyKeys: TKeyList; surveyTagKeys: TEditableKeyList);
    function HasFilteredChildNodes(const parentData: TNodeObject; const childTableName: String;
      filteredChildKeys: TStringList; parentKeysForChildKeysProc: TGetParentKeysForChildKeys): Boolean;
    procedure DoDeleteNode;
    procedure DoUncheckedDelete;
    procedure LoadSampleIntoCard(Sender: TObject);
  protected
    procedure SetupDestinationControls; override;
    procedure ReadSortOrders; override;
    procedure WriteSortOrders; override;
    function GetCurrentControlEditMode: TEditMode; override;
    function GetDetailsPageControl: TPageControl; override;
    procedure DoShowHint(var HintStr: String; var CanShow: Boolean; var HintInfo: THintInfo);
        override;

    procedure ApplyFilter(AKeyList: TKeyList); override;
    procedure ClearFilter(const AName: String); override;
    function GetCustodian: string; override;
    procedure WMDeleteNode(var Message: TMessage); message WM_DELETE_NODE;
    procedure WMUncheckedDeleteNode(var Message: TMessage); message WM_UNCHECKED_DELETE_NODE;
    procedure WMUpdateSampleLoadCardMenu(var Message: TMessage); message WM_UPDATE_SAMPLE_LOAD_CARD;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplySecurity; override;
    procedure CheckedOccurrence;
    procedure DisplayObservations(const AType:TRelatedData; const AKeyList:TKeyList);
    procedure DisplayFilteredObservations;
    function GetKeyList: TKeyList; override;
    function GetKeyListForValidation: TKeyList; override;
    function GetSurveyKeyForEvent(const AKey: TKeyString): String;
    function GetSurveyKeyForSample(const AKey: TKeyString): String;
    procedure RefreshSamples;
    procedure RefreshSurvey(const ASurveyKey, AEventKey, ASampleKey : TKeyString);
    procedure ShowSurveyTagDetails(const surveyTag: String);
    procedure ShowSurveyDetails(const ASurveyName:String);
    procedure ShowEventDetails(const AEventName: String);
    procedure ShowSampleDetails(const ASampleName: String);
    procedure ShowTaxonOccurrence(const ATaxonName: String);
    procedure ShowBiotopeOccurrence(const ABiotopeName: String);
    procedure SetItemTextAndKey(const AString: String; const AKey:TKeyString;
      const AnotherKey:TKeyString='');
    procedure SetMenuState(const tfNotEditing: Boolean);
    procedure UpdateMapWindowSelector; override;
    function SelectNode(const ANodeType, ANodeKey: string): TFlyNode; override;
    function ItemKey: string; override;
    function CustomReportKeyType: TKeyType; override;
    property  DetailForm:TfrmBaseDockedForm read FDetailForm write SetDetailForm;
    property  SelectedItem:TFlyNode read FSelectedItem;
    procedure PopulateAllSamples(AEventNode: TFlyNode);
    procedure PopulateAllEvents(ASurveyNode: TFlyNode);
    procedure UpdateSampleLoadCardMenu;
    property SurveyToReselect: TKeyString read FSurveyToReselect write FSurveyToReselect;
    property SurveyTagToReselect: TKeyString read FSurveyTagToReselect write FSurveyTagToReselect;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, GeneralData, ApplicationSettings, Maintbar, SurveyDetails,
  EventDetails, SampleDetails, TaxonOccur, BiotopeOccur, SurveyTagDetails,
  References, Locations, IndOrg, GenFuncs, DropTarget, ValidationData,
  DatabaseAccessADO, ComObj, Map, CsProp,  SpatialRef, DBCtrls, CheckLst,
  ResourceStrings, PlaceCard, ComAddinUnit;

const
  // For sort orders. Use these to save to and read from registry, instead of
  // the actual list of fields in the ORDER BY clause of the queries.
  // This means the field names can be changed without breaking what comes from
  // existing registry entries.
  SORT_SURVEY_NAME        = 'Survey Name';
  SORT_SURVEY_RUN_BY      = 'Survey Run By';
  SORT_EVENT_DATE         = 'Event Date';
  SORT_EVENT_LOCATION     = 'Event Location';
  SORT_SAMPLE_REF         = 'Sample Reference';
  SORT_SAMPLE_DATE        = 'Sample Date';
  SORT_SAMPLE_TYPE        = 'Sample Type';
  SORT_TAXON_SCI_NAME     = 'Taxon Sci Name';
  SORT_TAXON_COMMON_NAME  = 'Taxon Common Name';
  SORT_TAXON_SORT_ORDER   = 'Taxon Sort Order';
  SORT_BIOTOPE_CODE       = 'Biotope Code';
  SORT_BIOTOPE_SHORT_NAME = 'Biotope Short Name';

resourcestring
  ResStr_NodeNotSampleEventOrSurvey = 'Internal Error.  Node not a sample, event or survey';
  ResStr_FindAddinNode =  'Find requested on addin-managed node';
  ResStr_NoFilteringCondition = 'There are no %ss matching the filtering condition.';

  ResStr_CheckAllOccurence =  'Are you sure you wish to check ALL occurrences'#10+
                              'for the selected sample?';

  ResStr_CutAndPaste = 'cut and paste ';
  ResStr_DragAndDrop =  'drag and drop';
  ResStr_HierarchyOperation = 'You can only %s items from the hierarchy onto the hierarchy.';
  ResStr_SelectTarget = 'A target node must be selected before pasting.';

  ResStr_ClipboardCopy =  'You have copied the item "%s" to the clipboard. ' +
                          'Pasting will cause it to be moved to "%s".'#13#13 +
                          'Do you want to proceed?';

  ResStr_MoveItem = 'You are about to move the item "%s" to "%s".'#13#13 +
                    'Do you want to proceed?';

  ResStr_DragSample = 'Samples must be dragged to a Survey Event.';
  ResStr_CannotMoveBiotope =  'Biotope Occurrence cannot be moved.  ';
  ResStr_BiotopeDragToSample =  'Biotope Occurrences must be dragged to a Sample.';
  ResStr_CannotMoveTaxon =  'Taxon Occurrence cannot be moved.  ';
  ResStr_DragTaxonOccureceSample =  'Taxon Occurrences must be dragged to a Sample.';
  ResStr_DragAndDropLimitation =  'You can only perform drag and drop operations within the Observations hierarchy';
  ResStr_HierarchyItemNotAvailable =  'Hierarchy item is not available';
  ResStr_NoEventsToRelate = 'There are no events to relate this node to.';
  ResStr_NoOccurenceToRelate =  'There are no occurrences to relate this node to.';
  ResStr_NoLocationToRelate = 'There are no Locations to relate this %s to.';
  ResStr_NoIndOrgToRelate = 'There are no Individuals or Organisations to relate this %s to.';
  ResStr_NoDocumentToRelate = 'There are no documents to relate this %s to.';

  ResStr_ClearTagFromSurveys =
      'You are about to clear the Survey Tag ''%s'' from all associated surveys.'#13#10 +
      'Are you sure you want to continue?';

  ResStr_NewSurveyTag         = 'New Survey Tag';
  ResStr_NewSurvey            = 'New Survey';
  ResStr_NewEvent             = 'New Event';
  ResStr_NewSample            = 'New Sample';
  ResStr_NewTaxonOccurrence   = 'New Taxon Occurrence';
  ResStr_NewBiotopeOccurrence = 'New Biotope Occurrence';

//==============================================================================
constructor TfrmObservations.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetupObjects;
  FDetailForm   := nil;
  FSelectedItem := nil;
  FRelatedData  := rdNone;
  FClearingTree := False;

  ilObservation.AddImages(dmFormActions.ilSampleTypes);  // Add Sample bitmaps at the end
  FormResize(Self);

  ClearTree;
  PopulateSurveyTagLevel;
  PopulateSurveyLevel(nil);
  with tvObservations do begin
    Selected    := Items.GetFirstNode;
    StateImages := ilObservationState;
  end;

  UpdateSampleLoadCardMenu;
  UpdateSubMenus;
  SetMenuState(True);
  tvObservations.SetFocus;

  //Security
  pmHAdd.Enabled := (AppSettings.UserAccessLevel >= ualAddOnly);
  pmHBatchUpdate.Visible := AppSettings.UserAccessLevel >= ualAdmin;

  SendMessage(Handle,WM_UPDATE_MENU_ICONS,0,0);

  //Help Setup
  Self.HelpContext    := IDH_OBSERVATIONS;
  mnuEdit.HelpContext := IDH_EDITMENU;
  // As the observations hierarchy can contain formatting, warn the treeview
  // so it can strip formatting from the hints
  tvObservations.HasHTMLFormatting := True;

  LoadFilter(FILTER_OBSERVATION);
end;  // Create

//==============================================================================
procedure TfrmObservations.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(True);
  frmMain.SetContextToolbar(Self,mnuEdit,4,[nil,nil,nil,nil,nil,
                                            nil,nil,nil,nil,nil,pmSortBy,
                                            nil,nil,nil,nil,nil]);
  UpdateSubMenus;
  // Context toolbar updated after Create called. So if tree empty, refresh
  // Menu state and toolbar dropdown buttons
  if tvObservations.Items.Count = 0 then SetMenuState(True);
end;  // FormActivate

//==============================================================================
procedure TfrmObservations.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  if DetailForm <> nil then // Safety check
  begin
    CanClose := DetailForm.CloseQuery;
    if CanClose then begin
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
procedure TfrmObservations.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  frmMain.ClearContextToolbar(False);
  Action:=caFree;
  // Change the state of the Export menu Item
end;  // FormClose

//==============================================================================
destructor TfrmObservations.Destroy;
begin
  FreeObjects;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TfrmObservations.SetupObjects;
var lstSurvey, lstEvent, lstSample, lstTaxon, lstBiotope: String;
begin
  // Defaults to Survey Name, unless Run By sort was found
  if FSurveyField = SORT_SURVEY_RUN_BY then lstSurvey := 'FullName'
                                       else lstSurvey := 'SurveyName';

  // Defaults to Event Date, unless Location sort was found
  if FEventField = SORT_EVENT_LOCATION then lstEvent := 'LN.Item_Name, E.Spatial_Ref, E.Location_Name'
                                       else lstEvent := 'Vague_Date_Start';

  // Defaults to Sample Ref, unless either Date or Type sort was found
  if FSampleField = SORT_SAMPLE_DATE then lstSample := 'Vague_Date_Start' else
  if FSampleField = SORT_SAMPLE_TYPE then lstSample := 'Short_Name'
                                     else lstSample := 'Sample_Reference';

  // Defaults to Taxon Sci Name, unless Common Name sort was found
  if FTaxonOccField = SORT_TAXON_COMMON_NAME then lstTaxon := 'ITN.Common_Name' else
  if FTaxonOccField = SORT_TAXON_SORT_ORDER then lstTaxon := 'ITN.Sort_Order'
                                             else lstTaxon := 'ITN.Preferred_Name';

  // Defaults to Biotope Original Code, unless Short Term sort was found
  if FBiotopeOccField = SORT_BIOTOPE_SHORT_NAME then lstBiotope := 'B.Short_Term'
                                                else lstBiotope := 'B.Original_Code';

  FdmObservation := TdmObservation.Create(nil, lstSurvey, lstEvent, lstSample, lstTaxon, lstBiotope);

  FSurveyTagKeys := TStringList.Create;
  FSurveyKeys    := TStringList.Create;
  FEventKeys     := TStringList.Create;
  FSampleKeys    := TStringList.Create;
  FTaxonOccKeys  := TStringList.Create;
  FBioOccKeys    := TStringList.Create;

  FNodeImagesMap := TStringList.Create;
  FNodeStateImagesMap := TStringList.Create;

  FDynamicMenuLists := TInterfaceList.Create;

  with AppSettings.ComAddins do
    if OccurrenceNodeManagers.Count > 0 then
      FNodeMan := CreateComObject(StringToGUID(OccurrenceNodeManagers[0])) as IOccurrenceNodeManager;
end;  // SetupDataModule

//==============================================================================
procedure TfrmObservations.FreeObjects;
var lCount : integer;
begin
  ResetAddinDetailScreen;
  FNodeMan := nil;

  ClearTree;
  for lCount := pnlDetails.ControlCount - 1 downto 0 do
    pnlDetails.Controls[lCount].Free;

  FreeAndNil(FdmObservation);
  FreeAndNil(FSurveyTagKeys);
  FreeAndNil(FSurveyKeys);
  FreeAndNil(FEventKeys);
  FreeAndNil(FSampleKeys);
  FreeAndNil(FTaxonOccKeys);
  FreeAndNil(FBioOccKeys);

  FreeAndNil(FNodeImagesMap);
  FreeAndNil(FNodeStateImagesMap);

  FreeAndNil(FDynamicMenuLists);
  
  AppSettings.ClearFilteredRecords(
      [TN_SURVEY, TN_SURVEY_EVENT, TN_SAMPLE, TN_TAXON_OCCURRENCE, TN_BIOTOPE_OCCURRENCE]);
end;  // FreeObjects

//==============================================================================
procedure TfrmObservations.ClearTree;
var lCursor:TCursor;
    i : integer;
begin
  lCursor:=HourglassCursor;
  LockWindowUpdate(Handle);
  if csDestroying in ComponentState then
    tvObservations.Items.BeginUpdate;
  try
    FClearingTree:=True;
    with tvObservations do begin
      for i := 0 to tvObservations.Items.Count-1 do
        TObject(tvObservations.Items[i].Data).Free;
      tvObservations.Items.Clear;
    end;
    FClearingTree:=False;
    PerformNodeChange(nil);
  finally
    LockWindowUpdate(0);
    if csDestroying in ComponentState then
      tvObservations.Items.EndUpdate;
    DefaultCursor(lCursor);
  end;
end;  // ClearTree;

//==============================================================================
procedure TfrmObservations.KillChildren(ANode:TFlyNode);
var lNode,lNextNode:TFlyNode;
    lAlreadyClearingTree : Boolean;
begin
  lNode := ANode.GetLastChild;
  lAlreadyClearingTree := FClearingTree;
  FClearingTree := True;
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
procedure TfrmObservations.FormResize(Sender: TObject);
begin
  inherited;
  if DrillSplitter.Left < DrillSplitter.MinSize then begin
    pnlDetails.Width := ClientWidth - DrillSplitter.MinSize - DrillSplitter.Width;
    if pnlDetails.Left + pnlDetails.Width > ClientWidth then
      Redraw;
  end;
  with tvObservations do begin
    Width  := shpObservations.Width - 2;  // Size TreeView to show Drag/Drop shape
    Height := shpObservations.Height - 2;
    UpdateScrollRange;  // Make sure the scroolbars are properly displayed (or not)
    Refresh;
  end;
end;  // FormResize

//==============================================================================
procedure TfrmObservations.DrillSplitterPaint(Sender: TObject);
begin
  inherited;
  DrawVertSplitter(Canvas, DrillSplitter);
end;  // DrillSplitterPaint

//==============================================================================
procedure TfrmObservations.SetDetailForm(const Value: TfrmBaseDockedForm);
begin
  FDetailForm := Value;
end;  // SetDetailForm

{-------------------------------------------------------------------------------
  Override default accessor to retrieve custodian from detail form
}
function TfrmObservations.GetCustodian: string;
begin
  if Assigned(FDetailForm) then
    Result := FDetailForm.Custodian
  else
    Result := '';
end;

//==============================================================================
procedure TfrmObservations.SetMenuState(const tfNotEditing:Boolean);
var
  tfOn: Boolean;
  lData: TNodeObject;
  lNodeInfo: TNodeInfo;
  lFullEditAccess, isTagNode: Boolean;
begin
  if tfNotEditing then
    FEditMode := emView
  else
    FEditMode := emEdit;
  tfOn := tfNotEditing and (tvObservations.Items.Count > 0);

  lData := TNodeObject(GetStateDataFromNode(SelectedItem));
  bbAdd.Enabled := tfNotEditing and AddButtonState;
  if Assigned(SelectedItem) then begin
    isTagNode := lData is TSurveyTagNode;

    lNodeInfo := SetNodeInfo(SelectedItem);
    if isTagNode and (lNodeInfo.ItemKey <> '') then
      lFullEditAccess := dmGeneralData.SessionHasFullEditAccess(
          lNodeInfo.TableName,
          dmDatabase.GetPrimaryKey(lNodeInfo.TableName, False),
          lNodeInfo.ItemKey)
    else
    if (not (lData is TAddinOccNode)) and (lNodeInfo.ItemKey <> '') then
      lFullEditAccess := dmGeneralData.HasFullEditAccess(
          lNodeInfo.TableName,
          dmDatabase.GetPrimaryKey(lNodeInfo.TableName, False),
          lNodeInfo.ItemKey)
    else
      // addin nodes don't currently support Own Data access.
      lFullEditAccess := True;

    bbEdit.Enabled   := tfOn and (AppSettings.UserAccessLevel >= ualAddOnly);
    bbDelete.Enabled := tfOn and DeleteButtonState(lData) and lFullEditAccess and
                        (not isTagNode or (not lData.ChildrenPopulated or (SelectedItem.Count > 0)));
  end else begin
    isTagNode := False;
    bbEdit.Enabled := False;
    bbDelete.Enabled := False;
  end;

  // If addin active and node is addin-managed, make sure buttons state OK.
  if Assigned(FNodeMan) and (lData is TAddinOccNode) then begin
    bbEdit.Enabled := bbEdit.Enabled and
                      TAddinOccNode(lData).Editable and
                      not TAddinOccNode(lData).IsSubFolder;
    bbDelete.Enabled := bbDelete.Enabled and
                        TAddinOccNode(lData).Deletable and
                        (not TAddinOccNode(lData).IsSubFolder);
  end;
  bbRelatedData.Enabled := tfOn and not isTagNode;
  bbShowAll.Enabled     := tfOn;
  pmHierarchy.AutoPopup := bbAdd.Enabled;
  mnuEditAdd.Enabled    := bbAdd.Enabled;
  mnuEditEdit.Enabled   := bbEdit.Enabled;
  mnuEditDelete.Enabled := bbDelete.Enabled;

  actFind.Enabled         := tfOn;
  actFilter.Enabled       := tfOn;
  actShowMetadata.Enabled := tfOn and not isTagNode and Assigned(DetailForm);
  mnuEditSortBy.Enabled   := tfOn;
  EnableSortToolbutton(tfOn, pmSortBy);
  pmHSortBy.Enabled       := tfOn;

  if Assigned(SelectedItem) and bbCheckAll.Visible then
    bbCheckAll.Enabled := tfOn and (SelectedItem.Count > 0) and
                                   (AppSettings.UserAccessLevel > ualAddOnly);
end;  // SetMenuState

//==============================================================================
procedure TfrmObservations.UpdateSubMenus(const ANode: TFlyNode = nil);
var tfSurvey, tfEvent, tfSample,
    tfTaxon, tfBiotope, tfAddin,
    tfEmptyTree                 : Boolean;
    lTaxonData                  : TTaxonOccNode;
    lSampleData                 : TSampleNode;
    lNodeData, lParentNodeData  : TNodeObject;
begin
  lTaxonData      := nil;
  lSampleData     := nil;
  lNodeData       := nil;
  lParentNodeData := nil;

  // Work out the parent node data to help with determining what items to show on menu.
  if not Assigned(ANode) then begin
    with tvObservations do
      if Assigned(Selected) then begin
        lNodeData := TNodeObject(Selected.Data);
        if Assigned(Selected.Parent) then
          lParentNodeData := TNodeObject(Selected.Parent.Data);
      end
  end else begin
    lNodeData := TNodeObject(ANode.Data);
    if Assigned(ANode.Parent) then
      lParentNodeData := TNodeObject(ANode.Parent.Data);
  end;

  tfEmptyTree := tvObservations.Items.Count = 0;
  tfSurvey  := (lNodeData is TSurveyNode) or (lNodeData is TSurveyTagNode);
  tfEvent   := lNodeData is TEventNode;
  tfSample  := lNodeData is TSampleNode;
  tfBiotope := lNodeData is TBiotopeOccNode;
  tfTaxon   := lNodeData is TTaxonOccNode;
  tfAddin   := Assigned(FNodeMan) and (lNodeData is TAddinOccNode);

  // Additional info for taxon/sample
  if tfTaxon then lTaxonData := TTaxonOccNode(lNodeData);
  if tfSample then lSampleData := TSampleNode(lNodeData);

  // Add sub-menu
  actAddSurveyTag.Visible:= (tfSurvey or tfEmptyTree) and AppSettings.OrganiseSurveysByTag;
  actAddSurvey.Visible   := tfSurvey or tfEmptyTree;
  actAddEvent.Visible    := not (lNodeData is TSurveyTagNode) and (tfSurvey or tfEvent);
  actAddSample.Visible   := tfEvent  or tfSample;
  actAddTaxonOcc.Visible := tfSample or tfTaxon or tfBiotope
                            or (tfAddin and (lParentNodeData is TSampleNode));
  actAddBioOcc.Visible   := actAddTaxonOcc.Visible;

  // Sort By sub-menu
  pmSortSurveyName.Default       := tfSurvey;
  actSortSurveyName.Visible      := tfSurvey;
  actSortSurveyRunBy.Visible     := tfSurvey;
  pmSortEventDate.Default        := tfEvent;
  actSortEventDate.Visible       := tfEvent;
  actSortEventLocation.Visible   := tfEvent;
  pmSortSampleRef.Default        := tfSample;
  actSortSampleReference.Visible := tfSample;
  actSortSampleDate.Visible      := tfSample;
  actSortSampleType.Visible      := tfSample;
  pmHLoadIntoRecordCard.Visible  := tfSample;
  pmHSepLoad.Visible             := tfSample;
  pmSortTaxScientific.Default    := tfTaxon;
  actSortTaxonScientific.Visible := tfTaxon;
  actSortTaxonCommon.Visible     := tfTaxon;
  actSortTaxonSortOrder.Visible  := tfTaxon;
  pmSortBioCode.Default          := tfBiotope;
  actSortBioCode.Visible         := tfBiotope;
  actSortBioName.Visible         := tfBiotope;

  // Remove any Addin menu entries
  CleanupAddinMenuItems(mnuEditAdd);            // Remove from form's Add menu
  CleanupAddinMenuItems(pmAdd.Items);           // Remove from Add button's menu
  CleanupAddinMenuItems(pmHierarchy.Items);

  MergeAddinMenusIntoHierarchyPopup;

  if Assigned(FNodeMan) and
     (tfSample or tfBiotope or tfTaxon or tfAddin) then //or tfAddinExtra) then
    UpdateSubMenusForAddin(ANode, [mnuEditAdd, pmHierarchy.Items[0], pmAdd.Items]);

  with dmFormActions do begin
    SetActionVisibility(actExport, not tfEmptyTree);
    { Standard reports need a node in the tree to work on }
    if Assigned(lTaxonData) then
      // Reports disabled for confidential occurrences or occurrences with no location.
      actPlacesForOccurrencesReport.Enabled := lTaxonData.HasLocation and not lTaxonData.Confidential
    else
    if Assigned(lSampleData) then
      // Reports disabled for samples with no location.
      actPlacesForOccurrencesReport.Enabled := lSampleData.HasLocation
    else
      actPlacesForOccurrencesReport.Enabled := not tfEmptyTree;
    // Enable other report menu item
    actOccurrencesForPlacesReport.Enabled := actPlacesForOccurrencesReport.Enabled;
  end;

  // Related Data
  mnuRelEvents.Visible  := tfSurvey;
  mnuRelSamples.Visible := tfSurvey or tfEvent;
  mnuRelOccur.Visible   := tfSurvey or tfEvent or tfSample;
  mnuRelSep.Visible     := tfSurvey or tfEvent or tfSample;

  actFindOnMap.Enabled := not (tfSurvey or tfEmptyTree) and
                          (AppSettings.AvailableMaps.Count > 0);

  if not Assigned(SelectedItem) or not Assigned(DetailForm) then
    dmFormActions.UpdateRTFMenu(False)
  else
  if Assigned(DetailForm) then
    DetailForm.UpdateRTFMenu;

  dmFormActions.actCut.Enabled := (ActiveControl = tvObservations) and
                                  (AppSettings.UserAccessLevel >= ualAddOnly) and
                                  not (lNodeData is TSurveyTagNode);

  // Fix for dynamically populated PopupMenus
  RefreshXPMenu;
end;  // UpdateSubMenus

//==============================================================================
function TfrmObservations.CheckDeletedNode(const AMode:TEditMode):Boolean;
var lMessage, lTableName: String;
    lKey                : TKeyString;
begin
  Result := True;
  if not FAddingNewNode and Assigned(SelectedItem) then begin
    lKey := TNodeObject(SelectedItem.Data).ItemKey;
    lTableName := TNodeObject(SelectedItem.Data).ItemAdditional;
    if lTableName <> '' then
      Result := dmGeneralData.CheckKeyExists(lTableName, lTableName + '_Key', lKey);

    if not Result then begin
      case AMode of
        emView   : lMessage := ResStr_CannotAccessRecord;
        emEdit   : lMessage := ResStr_CannotEditRecord;
        emDelete : lMessage := ResStr_CannotDeleteRecord;
      end;
      MessageDlg(lMessage + #13#13 + ResStr_ItsDeletedFromDB,
                 mtInformation, [mbOk], 0);
      // Post a message to delete the node later, otherwise the node free's
      // during it's own event handler which is bad. See VI 17031
      PostMessage(Handle, WM_UNCHECKED_DELETE_NODE, 0, 0);
    end;
  end;
end;  // CheckDeletedNode

//==============================================================================
procedure TfrmObservations.PerformNodeChange(Node:TFlyNode);
var
  i: Integer;
  nodeData: TNodeObject;
begin
  // IF this gets called when form is being destroyed, exit now, or there would be trouble...
  if csDestroying in ComponentState then Exit;

  FSelectedItem := Node;     // Used to update node's text if changed in Details
  if Node <> nil then begin
    nodeData := TNodeObject(Node.Data);
    ResetAddinDetailScreen;
    if CheckDeletedNode(emView) then begin
      if nodeData is TSurveyTagNode  then ShowSurveyTagDetails(Node.Text)  else
      if nodeData is TSurveyNode     then ShowSurveyDetails(Node.Text)     else
      if nodeData is TEventNode      then ShowEventDetails(Node.Text)      else
      if nodeData is TSampleNode     then ShowSampleDetails(Node.Text)     else
      if nodeData is TBiotopeOccNode then ShowBiotopeOccurrence(Node.Text) else
      if nodeData is TTaxonOccNode   then ShowTaxonOccurrence(Node.Text)   else
      if Assigned(FNodeMan)          then PerformAddinNodeChange(Node);
    end;
    NotifyDataItemChange;
  end else
  if tvObservations.Items.Count = 0 then begin
    Caption := ResStr_Observations;
    if DetailForm <> nil then begin
      DetailForm.Close;
      FreeAndNil(FDetailForm);
    end;
    ResetAddinDetailScreen;
    for i := pnlDetails.ControlCount - 1 downto 0 do
      pnlDetails.Controls[i].Free;
    SetMenuState(True);
  end;
  UpdateSubMenus(Node);
end;  // PerformNodeChange

//==============================================================================
procedure TfrmObservations.tvObservationsChange(Sender: TObject; Node: TFlyNode);
var ltfEditMode: Boolean;
begin
  inherited;
  if (not FClearingTree) and
     ((Node <> tvObservations.Selected) or
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
        SetMenuState(not ltfEditMode);
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
      SetMenuState(True);
    end;
  end;
end;  // tvObservationChange

//==============================================================================
procedure TfrmObservations.CheckDetailFormClass(const AFormClass: TFormClass);
begin
  if DetailForm <> nil then
    if not (DetailForm is AFormClass) then begin
      DetailForm.Close;
      FreeAndNil(FDetailForm);
    end;

  if not Assigned(DetailForm) then
    with pnlDetails do begin
      while ComponentCount > 0 do Components[ComponentCount - 1].Free;
      while ControlCount > 0 do Controls[ControlCount - 1].Free;
    end;
end;  // CheckDetailFormClass

//------------------------------------------------------------------------------
procedure TfrmObservations.ResetDetailForm(const ACaption: String; const AFormClass: TFormClass);
var
  lComponent: TComponent;
begin
  Caption := ACaption;
  bbCheckAll.Visible := False;
  CheckDetailFormClass(AFormClass);
  if DetailForm = nil then begin
    // Need to cast for DetailForm
    DetailForm := TfrmBaseDockedForm(AFormClass.Create(nil));
    lComponent := DetailForm.FindComponent('scbDetails');
    if not Assigned(lComponent) then
      lComponent := DetailForm.FindComponent('pnlDetails');
    if Assigned(lComponent) then
      TWinControl(lComponent).ManualDock(pnlDetails, nil, alClient);
    DetailForm.UpdateMapWindowSelector;
  end;
end;

//==============================================================================
procedure TfrmObservations.ShowSurveyTagDetails(const surveyTag: String);
begin
  ResetDetailForm(ResStr_SurveyTag + ': ' + surveyTag, TfrmSurveyTagDetails);
  with TfrmSurveyTagDetails(DetailForm) do begin
    DrillForm := Self;
    DisplayRecord(TNodeObject(SelectedItem.Data).ItemKey);
  end;
end;

//==============================================================================
procedure TfrmObservations.ShowSurveyDetails(const ASurveyName: String);
begin
  ResetDetailForm(ResStr_Survey + ': ' + ASurveyName, TfrmSurveyDetails);
  with TfrmSurveyDetails(DetailForm) do begin
    DrillForm := Self;
    DisplayRecord(TSurveyNode(SelectedItem.Data).ItemKey);
  end;
end;  // ShowSurveyDetails

//==============================================================================
procedure TfrmObservations.ShowEventDetails(const AEventName: String);
begin
  ResetDetailForm(ResStr_Event + ': ' + AEventName, TfrmEventDetails);
  with TfrmEventDetails(DetailForm) do begin
    DrillForm := Self;
    DisplayRecord(TEventNode(SelectedItem.Data).ItemKey);
  end;
end;  // ShowEventDetails

//==============================================================================
procedure TfrmObservations.ShowSampleDetails(const ASampleName: String);
begin
  ResetDetailForm(ResStr_Sample + ': ' + ASampleName, TfrmSampleDetails);
  bbCheckAll.Visible := True;
  bbCheckAll.Enabled := (SelectedItem.Count > 0) and
                        (AppSettings.UserAccessLevel > ualAddOnly);
  with TfrmSampleDetails(DetailForm) do begin
    DrillForm := Self;
    DisplayRecord(TSampleNode(SelectedItem.Data).ItemKey);
  end;
end;  // ShowSampleDetails

//==============================================================================
procedure TfrmObservations.ShowTaxonOccurrence(const ATaxonName: String);
begin
  ResetDetailForm(ResStr_TaxonOccurrence + ': ' + ATaxonName, TfrmTaxonOccurrences);
  with TfrmTaxonOccurrences(DetailForm) do begin
    DrillForm:=Self;
    DisplayRecord(TTaxonOccNode(SelectedItem.Data).ItemKey);
  end;
end;  // ShowTaxonOccurrence

//==============================================================================
procedure TfrmObservations.ShowBiotopeOccurrence(const ABiotopeName: String);
begin
  ResetDetailForm(ResStr_BiotopeOccurrence + ': ' + ABiotopeName, TfrmBiotopeOccurrences);
  with TfrmBiotopeOccurrences(DetailForm) do begin
    DrillForm := Self;
    DisplayRecord(TBiotopeOccNode(SelectedItem.Data).ItemKey);
  end;
end;  // ShowBiotopeOccurrence

//==============================================================================
procedure TfrmObservations.bbAddClick(Sender: TObject);
var
  lPos: TPoint;
  i, lVisibleItems, lItemIndex: Integer;
begin
  inherited;
  lPos := bbAdd.ClientToScreen(Point(0, bbAdd.Height));
  if AppSettings.UserAccessLevel >= ualAddOnly then
    with pmAdd do begin
      lVisibleItems := 0;
      lItemIndex := 0;
      for i := 0 to Items.Count - 1 do
        if Items[i].Visible then begin
          Inc(lVisibleItems);
          lItemIndex := i;
        end;

      if lVisibleItems = 1 then
        Items[lItemIndex].Click
      else
        Popup(lPos.X, lPos.Y);
    end;
end;  // bbAddClick

//==============================================================================
procedure TfrmObservations.bbEditClick(Sender: TObject);
var
  nodeData: TNodeObject;
begin
  inherited;
  if CheckDeletedNode(emEdit) then
    with SelectedItem do begin
      nodeData := TNodeObject(Data);
      if nodeData is TSurveyTagNode  then TfrmSurveyTagDetails(DetailForm).EditRecord   else
      if nodeData is TSurveyNode     then TfrmSurveyDetails(DetailForm).EditRecord      else
      if nodeData is TEventNode      then TfrmEventDetails(DetailForm).EditRecord       else
      if nodeData is TSampleNode     then TfrmSampleDetails(DetailForm).EditRecord      else
      if nodeData is TBiotopeOccNode then TfrmBiotopeOccurrences(DetailForm).EditRecord else
      if nodeData is TTaxonOccNode   then TfrmTaxonOccurrences(DetailForm).EditRecord   else
      if Assigned(FDetailScreen) then
        (FDetailScreen.ControlInterface as IRecorderDetailScreen).EditRecord;
    end;
end;  // bbEditClick

//==============================================================================
procedure TfrmObservations.bbDeleteClick(Sender: TObject);
begin
  DoDeleteNode;
end;

{-------------------------------------------------------------------------------
  Message handler which allows us to delay a node deletion event
}
procedure TfrmObservations.WMDeleteNode(var Message: TMessage);
begin
  DoDeleteNode;
end;

{-------------------------------------------------------------------------------
  Handle node deletion
}
procedure TfrmObservations.DoDeleteNode;
var lMsg : string;
    lMany, lIsTag: Boolean;
    lType, lContains: string;
    lNodeData: TNodeObject;
    lTagKey: TKeyString;
begin
  inherited;
  // Safeguards to prevent possible access violations - VI14555
  if SelectedItem = nil then Exit;
  if SelectedItem.Data = nil then Exit;
  if not (TObject(SelectedItem.Data) is TNodeObject) then Exit;

  lNodeData := TNodeObject(SelectedItem.Data);

  // Make sure the node to delete doesn't have any child node attached
  if not lNodeData.ChildrenPopulated then
    SelectedItem.Expand(False);

  // If node has children, can't remove
  if SelectedItem.HasChildren  and
     ((lNodeData is TSurveyNode) or (lNodeData is TEventNode) or (lNodeData is TSampleNode)) then
  begin
    lMany := (SelectedItem.Count > 1);
    if lNodeData is TSurveyNode then begin
      lType := ResStr_Survey;
      if lMany then lContains := IntToStr(SelectedItem.Count) + ' ' + ResStr_Events
      else          lContains := ResStr_AnEvent;
    end else
    if lNodeData is TEventNode then begin
      lType := ResStr_Event;
      if lMany then lContains := IntToStr(SelectedItem.Count) + ' ' + ResStr_Samples
      else          lContains := ResStr_ASample;
    end else
    if lNodeData is TSampleNode then begin
      lType := ResStr_Sample;
      if lMany then lContains := IntToStr(SelectedItem.Count) + ' ' + ResStr_Occurrences
      else          lContains := ResStr_AnOccurrence;
    end;
    MessageDlg(
        Format(ResStr_CannotDeleteXAsContainsY, [lType, lContains]),
        mtWarning, [mbOk], 0);
  end else
  if CheckDeletedNode(emDelete) then begin
    if lNodeData is TSurveyTagNode then begin
      if SelectedItem.Count = 0 then begin
        SetMenuState(FEditMode = emView);
        Exit;
      end;
      lMsg := Format(ResStr_ClearTagFromSurveys, [SelectedItem.Text]);
      lTagKey := lNodeData.ItemKey;
      lIsTag := True;
    end else begin
      lIsTag := False;
      if lNodeData is TSurveyNode     then lType := ResStr_Survey            else
      if lNodeData is TEventNode      then lType := ResStr_Event             else
      if lNodeData is TSampleNode     then lType := ResStr_Sample            else
      if lNodeData is TBiotopeOccNode then lType := ResStr_BiotopeOccurrence else
      if lNodeData is TTaxonOccNode   then lType := ResStr_TaxonOccurrence   else
      if Assigned(FNodeMan) then
        if Assigned(SelectedItem.Parent) and
           (TNodeObject(SelectedItem.Parent.Data) is TSampleNode) then
          lType := ResStr_Occurrence
        else
          lType := ResStr_Item;
      lMsg := Format(ResStr_PermanentDeletionConfirm, [lType + ' ''' + SelectedItem.Text + '''.'])
    end;

    if MessageDlg(lMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      DoUncheckedDelete;
      if lIsTag then SelectSurveyTag(lTagKey);
    end;  // if MessageDlg...
  end;  // if HasChildren...
end;  // bbDeleteClick

//==============================================================================
procedure TfrmObservations.DeleteItem(ANode:TFlyNode);
var lKey  :TKeyString;
    lNode : TFlyNode;
    lCursor:TCursor;
    lData : TNodeObject;
begin
  lCursor := HourglassCursor;
  try
    lData := TNodeObject(ANode.Data);
    lKey := lData.ItemKey;
    dmDatabase.ExecuteSQL('SET XACT_ABORT ON');
    dmDatabase.Connection.BeginTrans;
    try
      if lData is TSurveyTagNode  then TfrmSurveyTagDetails(DetailForm).DeleteRecord(lKey)   else
      if lData is TSurveyNode     then TfrmSurveyDetails(DetailForm).DeleteRecord(lKey)      else
      if lData is TEventNode      then TfrmEventDetails(DetailForm).DeleteRecord(lKey)       else
      if lData is TSampleNode     then TfrmSampleDetails(DetailForm).DeleteRecord(lKey)      else
      if lData is TBiotopeOccNode then TfrmBiotopeOccurrences(DetailForm).DeleteRecord(lKey) else
      if lData is TTaxonOccNode   then TfrmTaxonOccurrences(DetailForm).DeleteRecord(lKey)   else
      if Assigned(FDetailScreen)  then
        (FDetailScreen.ControlInterface as IRecorderDetailScreen).DeleteRecord;
      dmDatabase.Connection.CommitTrans;
    except
      on E:Exception do begin
        dmDatabase.Connection.RollbackTrans;
        if dmDatabase.CheckError(E, dbeReferentialIntegrity) then
          Raise TExceptionPath.CreateNonCritical(ResStr_UnableToDeleteReferential, E)
        else
          Raise;
      end;
    end;

    lNode := ANode.GetPrevVisible;   // Select a new node
    if Assigned(lNode) and (lNode <> ANode) then
      tvObservations.Selected := lNode
    else
      lNode := nil;
    // Remove Survey Tag node. Some surveys may not have tags anymore.
    // Easier to clear and repopulate
    if lData is TSurveyTagNode then
      if SameText(FSurveyField, SORT_SURVEY_NAME) then
        actSortSurveyNameExecute(nil)
      else
        actSortSurveyRunByExecute(nil)
    else begin
      lData.Free; // throw away associated data
      ANode.Delete;
      PerformNodeChange(lNode);
      if Assigned(SelectedItem) then
        if TNodeObject(SelectedItem.Data) is TAddinOccNode then
          OnRefreshScreenInfo(TNodeObject(SelectedItem.Data).ItemKey, '');
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // DeleteItem

//==============================================================================
procedure TfrmObservations.bbRelatedDataClick(Sender: TObject);
var PosPopup:TPoint;
begin
  inherited;
  PosPopup := bbRelatedData.ClientToScreen(Point(0, bbRelatedData.Height));
  pmRelatedData.Popup(PosPopup.X,PosPopup.Y);
end;  // bbRelatedDataClick

//==============================================================================
procedure TfrmObservations.actFindExecute(Sender: TObject);
var
  stTitle: String;
  i, j: Integer;
  node, subNode: TFlyNode;
  nodeData: TNodeObject;
  find: TdlgFind;
begin
  inherited;
  // Set the title for the search dialog
  nodeData := TNodeObject(SelectedItem.Data);
  if (nodeData is TSurveyNode) or
     (nodeData is TSurveyTagNode) then stTitle := ResStr_FindSurvey  else
  if nodeData is TEventNode       then stTitle := ResStr_FindEvent   else
  if nodeData is TSampleNode      then stTitle := ResStr_FindSample  else
  if nodeData is TBiotopeOccNode  then stTitle := ResStr_FindBiotope else
  if nodeData is TTaxonOccNode    then stTitle := ResStr_FindTaxon   else
  if Assigned(SelectedItem.Parent) and (TNodeObject(SelectedItem.Parent.Data) is TSampleNode) then
    stTitle := ResStr_FindOccurrence
  else
    Exit;

  // Initialize the dialog and the search list
  find := TdlgFind.CreateDialog(nil, stTitle, ftNone);
  with find do
    try
      eSearchText.SearchMode := smAlways;
      eSearchText.ClearSourceList;
      if stTitle = ResStr_FindOccurrence then begin
        ShowInformation(ResStr_FindAddinNode);
        Exit;
      end else
      if stTitle = ResStr_FindSurvey then begin
        // Set Survey search list
        for i := 0 to tvObservations.Items.Count - 1 do begin
          node := tvObservations.Items[i];
          if TNodeObject(node.Data) is TSurveyNode then
            eSearchText.AddToSourceList(TNodeObject(node.Data).ItemKey, node.Text, [])
          else
          if TNodeObject(node.Data) is TSurveyTagNode then begin
            // Make sure all the surveys are available for search
            if not TNodeObject(node.Data).ChildrenPopulated then begin
              node.Expand(False);
              node.Collapse(False);
            end;
            // Now get the surveys.
            for j := 0 to node.Count - 1 do begin
              subNode := node.Item[j];
              if (TNodeObject(subNode.Data) is TSurveyNode) then
                eSearchText.AddToSourceListWithAddition(
                    TNodeObject(subNode.Data).ItemKey,
                    '<' + node.Text + '> ' + subNode.Text,
                    [],
                    TNodeObject(node.Data).ItemKey);
            end;
          end;
        end;
      end else
      if stTitle = ResStr_FindBiotope then begin
        // Set Biotope search list
        node:=SelectedItem.Parent;
        for i:=0 to node.Count-1 do
          if TNodeObject(node.Item[i].Data) is TBiotopeOccNode then
            eSearchText.AddToSourceList(TNodeObject(node.Item[i].Data).ItemKey,
                                        node.Item[i].Text, []);
      end else
      if stTitle = ResStr_FindTaxon then begin
        // Set taxon search list
        node:=SelectedItem.Parent;
        for i:=0 to node.Count-1 do
          if TNodeObject(node.Item[i].Data) is TTaxonOccNode then
            eSearchText.AddToSourceList(TNodeObject(node.Item[i].Data).ItemKey,
                                        node.Item[i].Text, []);
      end else begin
        // Set Event and Sample search list
        node:=SelectedItem.Parent;
        for i:=0 to node.Count-1 do
          eSearchText.AddToSourceList(TNodeObject(node.Item[i].Data).ItemKey,
                                      node.Item[i].Text, []);
      end;
      eSearchText.ForcePopulate;
      if ShowModal=mrOk then begin
        // Locate the node and update the selection. Slightly different process for Surveys
        if stTitle=ResStr_FindSurvey then begin
          for i:=0 to tvObservations.Items.Count-1 do begin
            node := tvObservations.Items[i];
            nodeData := TNodeObject(node.Data);
            if (nodeData is TSurveyNode) and (nodeData.ItemKey = ItemKey) then
            begin
              tvObservations.Selected := node;
              Break;
            end else
            // ItemAdditional has SuveyTag node key, if node is of that type.
            if (nodeData is TSurveyTagNode) and (nodeData.ItemKey = ItemAdditional) then
              for j := 0 to node.Count - 1 do begin
                subNode := node.Item[j];
                if (TNodeObject(subNode.Data) is TSurveyNode) and
                   (TNodeObject(subNode.Data).ItemKey = ItemKey) then
                begin
                  tvObservations.Selected := subNode;
                  Exit;
                end;
              end;
          end;
        end else begin
          node:=SelectedItem.Parent;
          for i:=0 to node.Count-1 do
            if (node.Item[i].Text=ItemText) and
               (TNodeObject(node.Item[i].Data).ItemKey=ItemKey) then begin
              tvObservations.Selected:=node.Item[i];
              Break;
            end;
        end;
      end;
    finally
      Free;
    end;
end;  // actFindExecute

//==============================================================================
procedure TfrmObservations.actFilterExecute(Sender: TObject);
begin
  inherited;
  if TNodeObject(SelectedItem.Data) is TSurveyTagNode then
    GetFilter(FILTER_OBSERVATION, TN_SURVEY)
  else
    GetFilter(FILTER_OBSERVATION, TNodeObject(SelectedItem.Data).ItemAdditional);
end;  // actFilterExecute

//==============================================================================
procedure TfrmObservations.ApplyFilter(AKeyList: TKeyList);
begin
  if SameText(DataFilter.TableName, TN_SURVEY) then
    FRelatedData := rdSurvey
  else
  if SameText(DataFilter.TableName, TN_SURVEY_EVENT) then
    FRelatedData := rdEvent
  else
  if SameText(DataFilter.TableName, TN_SAMPLE) then
    FRelatedData := rdSample
  else
  if SameText(DataFilter.TableName, TN_BIOTOPE_OCCURRENCE) or
     SameText(DataFilter.TableName, TN_TAXON_OCCURRENCE) then
    FRelatedData := rdOccurrence
  else
    FRelatedData := rdNone;

  if AKeyList.Header.ItemCount = 0 then
    MessageDlg(Format(
        ResStr_NoFilteringCondition,
        [ReadableFormat(DataFilter.TableName)]),
        mtInformation, [mbOk], 0)
  else
    DisplayObservations(FRelatedData, AKeyList);
end;

//==============================================================================
class function HaveOccurrenceCustody(ANode: TOccurrenceNode): Boolean;
begin
  Result := dmGeneralData.HaveCustody(ANode.ItemAdditional, ANode.ItemAdditional + '_Key', ANode.ItemKey);
end;

//==============================================================================
procedure TfrmObservations.CheckedOccurrence;
var lData: TOccurrenceNode;
begin
  with SelectedItem do begin
    lData := TOccurrenceNode(Data);
    if HaveOccurrenceCustody(lData) then begin
      if StateIndex = STATE_UNCHECKED then StateIndex := STATE_CHECKED
                                      else StateIndex := STATE_UNCHECKED;

      // Changing Checked property will update Sample state image
      lData.Checked := not lData.Checked;

      // Save changes to Biotope/Taxon occurrence table
      if (ImageIndex >= 18) and (ImageIndex <= 21) then begin // Biotope occurrence
        TfrmBiotopeOccurrences(DetailForm).ChangeChecked(StateIndex = STATE_CHECKED);
        TfrmBiotopeOccurrences(DetailForm).DisplayRecord(TfrmBiotopeOccurrences(DetailForm).BiotopeOccKey);
      end else if (ImageIndex >= 5) and (ImageIndex <= 17) then begin // Taxon occurrence
        TfrmTaxonOccurrences(DetailForm).ChangeChecked(StateIndex = STATE_CHECKED);
        TfrmTaxonOccurrences(DetailForm).DisplayRecord(TfrmTaxonOccurrences(DetailForm).TaxonOccKey);
      end else begin
        // Addin-managed nodes.
        if lData.Checked then
            // Update checked state of occurrence
            with dmDatabase.dbLocal do
              Execute('UPDATE ' + lData.ItemAdditional +
                      ' SET Checked=1, Checked_By=''' + AppSettings.UserID + ''', ' +
                          'Checked_Date=GetDate() ' +
                      ' WHERE ' + lData.ItemAdditional + '_Key = ''' + lData.ItemKey + '''')
          else
            with dmDatabase.dbLocal do
              Execute('UPDATE ' + lData.ItemAdditional +
                      ' SET Checked=0, Checked_By=NULL, Checked_Date=NULL ' +
                      ' WHERE ' + lData.ItemAdditional + '_Key = ''' + lData.ItemKey + '''');
        // Need to refresh DetailForm too.
        if Assigned(FDetailScreen) then // Should already be assigned.
          with (FDetailScreen.ControlInterface as IRecorderDetailScreen) do
            LoadContent(FNodeMan.TypeName[TAddinOccNode(lData).TypeID],
                        TNodeObject(SelectedItem.Parent.Data).ItemKey,
                        lData.ItemKey);
      end;
      UpdateSampleCardState(Parent);
    end;
  end;
end;  // CheckedOccurrence

//==============================================================================
procedure TfrmObservations.UpdateSampleCardState(ANode:TFlyNode);
begin
  // Save the new state in Sample table
  with FdmObservation.qrySampleState do begin
    Parameters.ParamByName('KeyParameter').Value := TSampleNode(ANode.Data).ItemKey;
    Open;
    First;
    Edit;
    case TSampleNode(ANode.Data).OutstandingCard of
      rcNone     : FieldByName('Outstanding_Card').AsInteger := 0;
      rcUnchecked: FieldByName('Outstanding_Card').AsInteger := 1;
      rcChecked  : FieldByName('Outstanding_Card').AsInteger := 2;
    end;
    Post;
    Close;
  end;
end;  // UpdateSampleCardState

//==============================================================================
procedure TfrmObservations.bbCheckAllClick(Sender: TObject);
var lNode  : TFlyNode;
    lData  : TOccurrenceNode;
    lCursor: TCursor;
begin
  inherited;
  // Make sure the node has been populated
  if not SelectedItem.Expanded then SelectedItem.Expand(False);
  lNode := SelectedItem.GetFirstChild;
  if lNode = nil then
    bbCheckAll.Enabled := False
  else
    if MessageDlg(ResStr_CheckAllOccurence, mtConfirmation, [mbNo, mbYes], 0) = mrYes then
    begin
      lCursor := HourglassCursor;
      try
        while lNode <> nil do begin
          // Get object in Data.
          lData := TOccurrenceNode(lNode.Data);
          if HaveOccurrenceCustody(lData) and (lNode.StateIndex = STATE_UNCHECKED) then
          begin
            lNode.StateIndex := STATE_CHECKED;
            // Changing Checked property will update Sample state image
            lData.Checked := True;
            // Update checked state of occurrence
            with dmDatabase.dbLocal do
              Execute('UPDATE ' + lData.ItemAdditional +
                      ' SET Checked=1, Checked_By=''' + AppSettings.UserID + ''', ' +
                          'Checked_Date=GetDate() ' +
                      ' WHERE ' + lData.ItemAdditional + '_Key = ''' + lData.ItemKey + '''');
          end;
          lNode := lNode.GetNextSibling;
        end;
        // Update state of sample
        UpdateSampleCardState(SelectedItem);
      finally
        DefaultCursor(lCursor);
      end;
    end;
end;  // bbCheckAllClick

//==============================================================================
procedure TfrmObservations.PopulateSurveyTagLevel;
var
  node: TFlyNode;
  nodeData: TSurveyTagNode;
  nodeText: String;
  cursor: TCursor;
begin
  if not AppSettings.OrganiseSurveysByTag then Exit;

  cursor := HourglassCursor;
  try
    with dmDatabase.GetRecordset('usp_SurveyTags_Select', []) do
      if not Eof then begin
        MoveFirst;
        while not Eof do begin
          if ((FSurveyTagKeys.Count = 0) and (FSurveyKeys.Count = 0)) or
             (FSurveyTagKeys.IndexOf(Fields['Concept_Key'].Value) > -1) then
          begin
            nodeText := Fields['PlainText'].Value;
            nodeData := TSurveyTagNode.Create;
            with nodeData do begin
              ItemKey := Fields['Concept_Key'].Value;
              ItemAdditional := TN_CONCEPT;
            end;
            node := tvObservations.Items.AddObject(nil, nodeText, nodeData);
            with node do begin
              ImageIndex    := nodeData.ImageIndex;
              SelectedIndex := ImageIndex;
            end;
            tvObservations.Items.AddChild(node, '-');
          end;
          MoveNext;
        end;
        Close;
      end;
  finally
    DefaultCursor(cursor);
  end;
end;

//==============================================================================
procedure TfrmObservations.PopulateSurveyLevel(surveyTagNode: TFlyNode);
var
  node: TFlyNode;
  nodeData: TSurveyNode;
  nodeText: String;
  cursor: TCursor;
  rs: ADODB._Recordset;
  addedSurveyKey: TStringList;
begin
  addedSurveyKey := TStringList.Create;
  cursor := HourglassCursor;
  try
    if Assigned(surveyTagNode) and (TNodeObject(surveyTagNode.Data) is TSurveyTagNode) then
      rs := dmDatabase.GetRecordset(
          'usp_Surveys_Select_ForSurveyTag',
          ['@Key', TNodeObject(surveyTagNode.Data).ItemKey,
           '@Order', Ord(FSurveyField = SORT_SURVEY_RUN_BY),
           '@UserNameKey', AppSettings.UserID])
    else
      rs := dmDatabase.ExecuteSQL(FdmObservation.qrySurvey.SQL.Text, True);

    with rs do
      if not Eof then begin
        MoveFirst;
        while not Eof do begin
          if (addedSurveyKey.IndexOf(Fields['Survey_Key'].Value) = -1) and
             ((FSurveyKeys.Count = 0) or
              (FSurveyKeys.IndexOf(Fields['Survey_Key'].Value) > -1)) and
             // Skip surveys organised in tags (if option on)
             (Assigned(surveyTagNode) or
              not AppSettings.OrganiseSurveysByTag or
              (Fields['HasTag'].Value = 0)) then
          begin
            // Don't want to see twice the same survey, especially under a "synonymed" tag.
            addedSurveyKey.Add(Fields['Survey_Key'].Value);
            nodeText := Fields['SurveyName'].Value + ' - ' + Fields['FullName'].Value;
            nodeData := TSurveyNode.Create;
            with nodeData do begin
              ItemKey        := Fields['Survey_Key'].Value;
              ItemAdditional := TN_SURVEY;
              IsFiltered     := AppSettings.IndexOfFilteredRecord(TN_SURVEY, ItemKey) > -1;
              Hint           := AppSettings.GetFilteredRecordHint(TN_SURVEY, ItemKey);
            end;
            if Assigned(surveyTagNode) then
              node := tvObservations.Items.AddChildObject(surveyTagNode, nodeText, nodeData)
            else
              node := tvObservations.Items.AddObject(nil, nodeText, nodeData);
            with node do begin
              ImageIndex    := nodeData.ImageIndex;
              SelectedIndex := ImageIndex;
            end;
            tvObservations.Items.AddChild(node, '-');
          end;
          MoveNext;
        end;
        Close;
      end;
  finally
    DefaultCursor(cursor);
    addedSurveyKey.Free;
  end;
end;  // PopulateSurveyLevel

//------------------------------------------------------------------------------
procedure TfrmObservations.PopulateEventLevel(surveyNode: TFlyNode);
var
  node: TFlyNode;
  nodeData: TEventNode;
  nodeText: String;
  cursor: TCursor;
  rs: ADODB._Recordset;
begin
  cursor := HourglassCursor;
  try
    rs := dmDatabase.ExecuteSQL(FdmObservation.qryEvent.SQL.Text, True);
    with rs do
      if not Eof then begin
        MoveFirst;
        while not Eof do begin
          nodeText := GetEventName(rs);

          nodeData := TEventNode.Create;
          with nodeData do begin
            ItemKey        := Fields['Survey_Event_Key'].Value;
            ItemAdditional := TN_SURVEY_EVENT;
            IsFiltered     := AppSettings.IndexOfFilteredRecord(TN_SURVEY_EVENT, ItemKey) > -1;
            Hint           := AppSettings.GetFilteredRecordHint(TN_SURVEY_EVENT, ItemKey);
            HasLocation    := not VarIsNull(Fields['Location_Key'].Value);
          end;
          node := tvObservations.Items.AddChildObject(surveyNode, nodeText, nodeData);
          with node do begin
            ImageIndex    := nodeData.ImageIndex;
            SelectedIndex := ImageIndex;
          end;
          tvObservations.Items.AddChild(node, '-');

          MoveNext;
        end;
        Close;
      end;
  finally
    DefaultCursor(cursor);
  end;
end;  // PopulateEventLevel

//------------------------------------------------------------------------------
procedure TfrmObservations.PopulateSampleLevel(eventNode: TFlyNode);
var
  node: TFlyNode;
  nodeData: TSampleNode;
  nodeText: String;
  cursor: TCursor;
  rs: ADODB._Recordset;
begin
  cursor := HourglassCursor;
  try
    rs := dmDatabase.ExecuteSQL(FdmObservation.qrySample.SQL.Text, True);
    with rs do
      if not Eof then begin
        MoveFirst;
        while not Eof do begin
          nodeText := GetSampleName(rs);
          nodeData := TSampleNode.Create;
          with nodeData do begin
            ItemKey              := Fields['Sample_Key'].Value;
            ItemAdditional       := TN_SAMPLE;
            IsFiltered           := AppSettings.IndexOfFilteredRecord(TN_SAMPLE, ItemKey) > -1;
            Hint                 := AppSettings.GetFilteredRecordHint(TN_SAMPLE, ItemKey);
            SampleType.ItemKey   := Fields['Sample_Type_Key'].Value;
            SampleType.ShortName := Fields['Short_Name'].Value;
            HasLocation          := not VarIsNull(Fields['Location_Key'].Value);
            // Shift sample type image by DEF_SAMPLE_IMAGE
            ImageIndex := dmFormActions.GetSampleImageIndex(SampleType.ItemKey) + DEF_SAMPLE_IMAGE;
            // if should be -1 (shifted by DEF_SAMPLE_IMAGE), then make it so
            if ImageIndex = DEF_SAMPLE_IMAGE - 1 then ImageIndex := -1;
            // 0: not outstanding card. 1: card but not all checked. 2: card and all checked
            case Fields['Outstanding_Card'].Value of
              0: OutstandingCard := rcNone;
              1: OutstandingCard := rcUnchecked;
              2: OutstandingCard := rcChecked;
            end;
          end;
          node := tvObservations.Items.AddChildObject(eventNode, nodeText, nodeData);
          with node do begin
            ImageIndex    := nodeData.ImageIndex;
            SelectedIndex := ImageIndex;
            StateIndex    := nodeData.StateImage;
          end;
          tvObservations.Items.AddChild(node, '-');

          MoveNext;
        end;
        Close;
      end;
  finally
    DefaultCursor(cursor);
  end;
end;  // PopulateSampleLevel

//------------------------------------------------------------------------------
procedure TfrmObservations.PopulateBiotopeOccurrenceLevel(sampleNode: TFlyNode);
var
  node: TFlyNode;
  nodeData: TBiotopeOccNode;
  parentData: TSampleNode;
  nodeText: String;
  cursor: TCursor;
begin
  cursor := HourglassCursor;
  try
    parentData := TSampleNode(sampleNode.Data);

    with dmDatabase.ExecuteSQL(FdmObservation.qryBiotopeOcc.SQL.Text, True) do
      if not Eof then begin
        MoveFirst;
        while not Eof do begin
          nodeText := Fields['Item_Name'].Value;
          nodeData := TBiotopeOccNode.Create;
          with nodeData do begin
            ParentNode     := sampleNode;  // To use with Checked property and Sample state
            ItemKey        := Fields['Biotope_Occurrence_Key'].Value;
            ItemAdditional := TN_BIOTOPE_OCCURRENCE;
            IsFiltered     := AppSettings.IndexOfFilteredRecord(TN_BIOTOPE_OCCURRENCE, ItemKey) > -1;
            Hint           := AppSettings.GetFilteredRecordHint(TN_BIOTOPE_OCCURRENCE, ItemKey);
            Checked        := Fields['Checked'].Value;
            HasLocation    := parentData.HasLocation;
            ChildrenPopulated := True;  // Can't go further down the tree
            if not AppSettings.UseOriginalIcons then
              imageindex :=   18 + 1 + Fields['Verified'].Value
          end;
          node := tvObservations.Items.AddChildObject(sampleNode, nodeText, nodeData);
          with node do begin
            ImageIndex    := nodeData.ImageIndex;
            SelectedIndex := ImageIndex;
            StateIndex    := nodeData.StateImage;
          end;
          MoveNext;
        end;
        Close;
      end;
  finally
    DefaultCursor(cursor);
  end;
end;  // PopulateBiotopeOccurrenceLevel

//------------------------------------------------------------------------------
procedure TfrmObservations.PopulateTaxonOccurrenceLevel(sampleNode: TFlyNode);
var
  node: TFlyNode;
  nodeData: TTaxonOccNode;
  parentData: TSampleNode;
  nodeText: String;
  cursor: TCursor;
begin
  cursor := HourglassCursor;
  try
    parentData := TSampleNode(sampleNode.Data);
    with dmDatabase.ExecuteSQL(FdmObservation.qryTaxonOcc.SQL.Text, True) do
      if not Eof then begin
        MoveFirst;
        while not Eof do begin
          if (AppSettings.DisplayTaxonCommonNames) and
             (not VarIsNull(Fields['Common_Name'].Value)) then
            nodeText := Fields['Common_Name'].Value
          else
            nodeText := Fields['Preferred_Name'].Value;

          nodeData := TTaxonOccNode.Create;
          with nodeData do begin
            ParentNode     := sampleNode;  // To use with Checked property and Sample state
            ItemKey        := Fields['Taxon_Occurrence_Key'].Value;
            ItemAdditional := TN_TAXON_OCCURRENCE;
            IsFiltered     := AppSettings.IndexOfFilteredRecord(TN_TAXON_OCCURRENCE, ItemKey) > -1;
            Hint           := AppSettings.GetFilteredRecordHint(TN_TAXON_OCCURRENCE, ItemKey);
            Confidential   := Fields['Confidential'].Value;
            ZeroAbundance  := Fields['Zero_Abundance'].Value;
            Verified       := Fields['Verified'].Value;
            Checked        := Fields['Checked'].Value;
            HasLocation    := parentData.HasLocation;
            ChildrenPopulated := True;  // Can't go further down the tree
            // TaxonNames
            TaxonNameObject := TTaxonNames.Create;
            TaxonNameObject.TaxonListItemKey := Fields['Taxon_List_Item_Key'].Value;
            TaxonNameObject.TaxonName        := Fields['Preferred_Name'].Value;
            TaxonNameObject.CommonName       := nodeText;
            TaxonNameObject.EnteredName      := Fields['Actual_Name'].Value;
            TaxonNameObject.TNItalic := Fields['Preferred_Name_Italic'].Value;
            TaxonNameObject.CNItalic := Fields['Common_Name_Italic'].Value;
            TaxonNameObject.ENItalic := Fields['Actual_Name_Italic'].Value;
            TaxonNameObject.TNAttribute := VarToStr(Fields['Preferred_Name_Attribute'].Value);
            TaxonNameObject.CNAttribute := VarToStr(Fields['Common_Name_Attribute'].Value);
            TaxonNameObject.ENAttribute := VarToStr(Fields['Actual_Name_Attribute'].Value);
            TaxonNameObject.TNAuthor := VarToStr(Fields['Preferred_Name_Authority'].Value);
            TaxonNameObject.ENAuthor := VarToStr(Fields['Authority'].Value);
            // Change Image based on Verified/Confidential and ZeroAbundance
            if Not AppSettings.UseOriginalIcons then
              ImageIndex :=  6 + (Verified * 4) + ord(Confidential) + (ord(ZeroAbundance) * 2)
          end;
          node := tvObservations.Items.AddChildObject(sampleNode, nodeText, nodeData);
          with node do begin
            ImageIndex    := nodeData.ImageIndex;
            SelectedIndex := ImageIndex;
            StateIndex    := nodeData.StateImage;
          end;
          MoveNext;
        end;
        Close;
      end;
  finally
    DefaultCursor(cursor);
  end;
end;  // PopulateTaxonOccurrenceLevel

//------------------------------------------------------------------------------
procedure TfrmObservations.PopulateOccurrenceLevel(sampleNode:TFlyNode);
begin
  PopulateTaxonOccurrenceLevel(sampleNode);
  PopulateBiotopeOccurrenceLevel(sampleNode);
end;  // PopulateOccurrenceLevel

//------------------------------------------------------------------------------
function TfrmObservations.HasFilteredChildNodes(const parentData: TNodeObject;
  const childTableName: String; filteredChildKeys: TStringList;
  parentKeysForChildKeysProc: TGetParentKeysForChildKeys): Boolean;
var
  parentKeys, childKeys: TEditableKeyList;
  i: Integer;
begin
  Result := False;
  if filteredChildKeys.Count = 0 then Exit;

  parentKeys := TEditableKeyList.Create;
  childKeys := TEditableKeyList.Create;
  try
    parentKeys.SetTable(parentData.ItemAdditional);
    for i := 0 to filteredChildKeys.Count - 1 do
      childKeys.AddItem(filteredChildKeys[i], childTableName);
    parentKeysForChildKeysProc(childKeys, parentKeys);
    Result := parentKeys.IndexOf(parentData.ItemKey, '') > -1;
  finally
    childKeys.Free;
    parentKeys.Free;
  end;
end;  // HasFilteredChildNodes

//==============================================================================
procedure TfrmObservations.tvObservationsExpanding(Sender: TObject;
  Node: TFlyNode; var AllowExpansion: Boolean);
var
  lNodeData: TNodeObject;
begin
  inherited;
  lNodeData := TNodeObject(Node.Data);
  if not lNodeData.ChildrenPopulated then begin
    Node.DeleteChildren;  // Remove the dummy child before getting the proper stuff
    if lNodeData is TSurveyTagNode then begin
      PopulateSurveyLevel(Node);
    end else
    if lNodeData is TSurveyNode then begin
      // If survey node, find the events
      FdmObservation.EventSurvey := lNodeData.ItemKey;
      if HasFilteredChildNodes(lNodeData, TN_SURVEY_EVENT, FEventKeys, GetSurveyKeysForEvents) then
        FdmObservation.qryEvent.SQL[QRYEVENT_REPLACE_ROW] := 'and E.Survey_Event_Key in (' +
            '''' + StringReplace(FEventKeys.CommaText, ',', ''',''', [rfReplaceAll]) + ''')'
      else
        FdmObservation.qryEvent.SQL[QRYEVENT_REPLACE_ROW] := '';
      PopulateEventLevel(Node);
    end else
    if lNodeData is TEventNode then begin
      // if event node, find the samples
      FdmObservation.SampleEvent := lNodeData.ItemKey;
      if HasFilteredChildNodes(lNodeData, TN_SAMPLE, FSampleKeys, GetEventKeysForSamples) then
        FdmObservation.qrySample.SQL[QRYSAMPLE_REPLACE_ROW] := 'and S.Sample_Key in (' +
            '''' + StringReplace(FSampleKeys.CommaText, ',', ''',''', [rfReplaceAll]) + ''')'
      else
        FdmObservation.qrySample.SQL[QRYSAMPLE_REPLACE_ROW] := '';
      PopulateSampleLevel(Node);
    end else
    if lNodeData is TSampleNode then begin
      // if sample node, find the taxon and then the biotope occurrences
      FdmObservation.OccurrenceSample := lNodeData.ItemKey;
      if HasFilteredChildNodes(lNodeData, TN_TAXON_OCCURRENCE, FTaxonOccKeys, GetSampleKeysForTaxonOccurrences)
      or HasFilteredChildNodes(lNodeData, TN_BIOTOPE_OCCURRENCE, FBioOccKeys, GetSampleKeysForBiotopeOccurrences)
      then begin
        FdmObservation.qryTaxonOcc.SQL[QRYTAXOCC_REPLACE_ROW] := 'and TXO.Taxon_Occurrence_Key in (' +
            '''' + StringReplace(FTaxonOccKeys.CommaText, ',', ''',''', [rfReplaceAll]) + ''')';
        FdmObservation.qryBiotopeOcc.SQL[QRYBIOOCC_REPLACE_ROW] := 'and BO.Biotope_Occurrence_Key in (' +
            '''' + StringReplace(FBioOccKeys.CommaText, ',', ''',''', [rfReplaceAll]) + ''')';
      end
      else begin
        FdmObservation.qryTaxonOcc.SQL[QRYTAXOCC_REPLACE_ROW] := '';
        FdmObservation.qryBiotopeOcc.SQL[QRYBIOOCC_REPLACE_ROW] := '';
      end;
      PopulateOccurrenceLevel(Node);
      if Node=SelectedItem then
        bbCheckAll.Enabled:=(Node.Count>0) and
                            (AppSettings.UserAccessLevel>ualAddOnly);
      // And also add addin-managed nodes
      PopulateAddinNodes(Node, -1);
    end else
    if lNodeData is TAddinOccNode then
      PopulateAddinNodes(Node, TAddinOccNode(lNodeData).TypeID);

    lNodeData.ChildrenPopulated:=True;
  end; //if not ChildrenPopulated
  SetMenuState(FEditMode = emView);
end;  // tvObservationsExpanding

//==============================================================================
// Update node's text and itemkey after addition or modification
procedure TfrmObservations.SetItemTextAndKey(const AString: String;
  const AKey:TKeyString; const AnotherKey: TKeyString = '');
var
  nodeData: TNodeObject;
begin
  if SelectedItem <> nil then
    with SelectedItem do begin
      Text := AString;
      nodeData := TNodeObject(Data);
      nodeData.ItemKey := AKey;
      if nodeData is TTaxonOccNode then begin
        with TTaxonOccNode(nodeData), dmGeneralData do begin
          { Refresh the taxon names, easier to do it that way, and just as quick }
          TaxonNameObject.Free;
          TaxonNameObject := nil;
          SetupStandardQuery(qryAllPurpose, ssTaxonListItemForOccurrence, AKey);
          try
            qryAllPurpose.Open;
            if not qryAllPurpose.Eof then
              TaxonNameObject :=
                  GetTaxonNamesObject(qryAllPurpose.FieldByName('TAXON_LIST_ITEM_KEY').AsString);
          finally
            qryAllPurpose.Close;
          end;
        end;
      end;

      { Caption set in 'Self' because TFlynode also supports caption property }
      if nodeData is TSurveyNode then
        Self.Caption := ResStr_Survey + ': ' + AString
      else
      if nodeData is TEventNode then
        Self.Caption := ResStr_Event + ': ' + AString
      else
      if nodeData is TSampleNode then begin
        ImageIndex := dmFormActions.GetSampleImageIndex(AnotherKey) + DEF_SAMPLE_IMAGE;
        if ImageIndex = DEF_SAMPLE_IMAGE - 1 then ImageIndex := -1; //sample with no image;
        SelectedIndex := ImageIndex;
      end else
      if nodeData is TBiotopeOccNode then begin
        StateIndex   := nodeData.StateImage;
        ImageIndex   := nodeData.ImageIndex;
        SelectedIndex := ImageIndex;
        Self.Caption := ResStr_BiotopeOcc + ': ' + AString
      end else
      if nodeData is TTaxonOccNode then begin
        ImageIndex    := nodeData.ImageIndex;
        SelectedIndex := ImageIndex;
        StateIndex    := nodeData.StateImage;
        Self.Caption  := ResStr_TaxonOcc + ': ' + AString;
      end;
    end;
  PerformNodeChange(SelectedItem);  // Make sure the menus are properly enabled
end;  // SetItemTextAndKey

//==============================================================================
// Discarding a new node needs to be handled via message as detail form can be
// destroyed before everything is properly cleaned up.
procedure TfrmObservations.WMDiscardObservation(var Msg: TMessage);
var lNode: TFlyNode;
begin
  if Assigned(SelectedItem) then begin
    // Focus on previous sibling
    lNode := SelectedItem.GetPrevSibling;
    // Or first previous visible, which would be the parent node, unless tree empty.
    if not Assigned(lNode) then lNode := SelectedItem.GetPrevVisible;

    TNodeObject(FSelectedItem.Data).Free;  // Remove new item from the tree
    FSelectedItem.Delete;
    if Assigned(lNode) then
      tvObservations.Selected := lNode
    else
      PerformNodeChange(tvObservations.Selected);  // Reselect node and record to display
    FSelectedItem := tvObservations.Selected;
  end;
  tvObservations.SetFocus;
end;  // WMDiscardObservation

//==============================================================================
procedure TfrmObservations.WMRefreshObservations(var Msg: Messages.TMessage);
begin
  ClearTree;
  PopulateSurveyTagLevel;
  PopulateSurveyLevel(nil);
  with tvObservations do begin
    Selected := Items.GetFirstNode;
    SetFocus;
  end;

  if SurveyTagToReselect <> '' then
    SelectNode(TN_SURVEY_TAG, SurveyTagToReselect);

  if SurveyToReselect <> '' then begin
    if SelectNode(TN_SURVEY, SurveyToReselect) <> nil then
      TfrmSurveyDetails(DetailForm).pcSurveyDetails.ActivePageIndex := Msg.WParam;
  end;
  SurveyToReselect := '';
  SurveyTagToReselect := '';
end;  // WMRefreshObservations

//==============================================================================
procedure TfrmObservations.WMRefreshTermLists(var Msg: TMessage);
begin
  Application.ProcessMessages;
  if DetailForm<>nil then
    if DetailForm is TfrmSurveyDetails then TfrmSurveyDetails(DetailForm).RefreshLists else
    if DetailForm is TfrmEventDetails  then TfrmEventDetails(DetailForm).RefreshLists else
    if DetailForm is TfrmSampleDetails then TfrmSampleDetails(DetailForm).RefreshLists else
    if DetailForm is TfrmTaxonOccurrences then TfrmTaxonOccurrences(DetailForm).RefreshLists else
    if DetailForm is TfrmBiotopeOccurrences then TfrmBiotopeOccurrences(DetailForm).RefreshLists;
end;  // WMRefreshTermLists

//==============================================================================
procedure TfrmObservations.actAddSurveyTagExecute(Sender: TObject);
var nodeData: TSurveyTagNode;
    node: TFlyNode;
begin
  inherited;
  FAddingNewNode := True;
  nodeData := TSurveyTagNode.Create;
  nodeData.ItemKey := '';
  nodeData.ItemAdditional := TN_CONCEPT;
  nodeData.ChildrenPopulated := True;
  node := tvObservations.Items.AddObject(nil, Format('< %s >', [ResStr_NewSurveyTag]), nodeData);
  node.ImageIndex    := nodeData.ImageIndex;
  node.SelectedIndex := nodeData.ImageIndex;
  tvObservations.Selected := node;   // Change to new item. Display the detail form too
  TfrmSurveyTagDetails(DetailForm).AddRecord;
  FAddingNewNode := False;
end;

//==============================================================================
procedure TfrmObservations.actAddSurveyExecute(Sender: TObject);
var lSurveyNode: TSurveyNode;
    lNode      : TFlyNode;
begin
  inherited;
  FAddingNewNode := True;
  lSurveyNode := TSurveyNode.Create;
  lSurveyNode.ItemKey := '';
  lSurveyNode.ItemAdditional := TN_SURVEY;
  lSurveyNode.ChildrenPopulated := True;
  // Add new Survey at the top of the tree.
  lNode := tvObservations.Items.AddObject(nil, Format('< %s >', [ResStr_NewSurvey]), lSurveyNode);
  tvObservations.Selected := lNode;   // Change to new item. Display the detail form too
  TfrmSurveyDetails(DetailForm).AddRecord;
  FAddingNewNode := False;
end;  // actAddSurveyExecute

//==============================================================================
procedure TfrmObservations.actAddEventExecute(Sender: TObject);
var lEventNode: TEventNode;
    lNode     : TFlyNode;
begin
  inherited;
  FAddingNewNode := True;
  lEventNode := TEventNode.Create;
  lEventNode.ItemKey := '';
  lEventNode.ItemAdditional := TN_SURVEY_EVENT;
  lEventNode.ChildrenPopulated := True;
  if TNodeObject(SelectedItem.Data) is TSurveyNode then begin
    // Expand the Survey to Event level, populating properly if needed
    SelectedItem.Expand(False);
    lNode := tvObservations.Items.AddChildObject(
        SelectedItem,
        Format('< %s >', [ResStr_NewEvent]),
        lEventNode);
    lNode.Parent.Expand(False);  // Make sure it's expanded, if adding first child node.
  end else
    lNode := tvObservations.Items.AddObject(
        SelectedItem,
        Format('< %s >', [ResStr_NewEvent]),
        lEventNode);
  lNode.ImageIndex    := lEventNode.ImageIndex;
  lNode.SelectedIndex := lEventNode.ImageIndex;
  tvObservations.Selected := lNode;   // Change to new item. Display the detail form too
  TfrmEventDetails(DetailForm).AddRecord(TNodeObject(SelectedItem.Parent.Data).ItemKey);
  FAddingNewNode := False;
end;  // actAddEventExecute

//==============================================================================
procedure TfrmObservations.actAddSampleExecute(Sender: TObject);
var lSampleNode: TSampleNode;
    lNode      : TFlyNode;
begin
  inherited;
  FAddingNewNode := True;
  lSampleNode := TSampleNode.Create;
  lSampleNode.ItemKey := '';
  lSampleNode.ItemAdditional := TN_SAMPLE;
  lSampleNode.ChildrenPopulated := True;
  if TNodeObject(SelectedItem.Data) is TEventNode then begin
    // Expand the Event to the Sample level, populating properly if needed.
    SelectedItem.Expand(False);
    lNode := tvObservations.Items.AddChildObject(
        SelectedItem,
        Format('< %s >', [ResStr_NewSample]),
        lSampleNode);
    lNode.Parent.Expand(False);  // Make sure it's expanded, if adding first child node.
  end else
    lNode := tvObservations.Items.AddObject(
        SelectedItem,
        Format('< %s >', [ResStr_NewSample]),
        lSampleNode);
  lNode.ImageIndex    := -1;  // No picture until the sample type has been properly selected
  lNode.SelectedIndex := -1;
  lNode.StateIndex    := SAMPLE_STATE_CARD_NONE;  // Value of 5 = blank state (different to 0 or -1 which means no state drawn)
  tvObservations.Selected := lNode;  // Change to new item. Display the detail form too
  TfrmSampleDetails(DetailForm).AddRecord(TNodeObject(SelectedItem.Parent.Data).ItemKey);
  FAddingNewNode:=False;
end;  // actAddSampleExecute

//==============================================================================
procedure TfrmObservations.actAddTaxonOccExecute(Sender: TObject);
var lTaxonOccNode: TTaxonOccNode;
    lNode        : TFlyNode;
begin
  inherited;
  FAddingNewNode := True;
  lTaxonOccNode := TTaxonOccNode.Create;
  lTaxonOccNode.ItemKey := '';
  lTaxonOccNode.ItemAdditional := TN_TAXON_OCCURRENCE;
  lTaxonOccNode.ChildrenPopulated := True;
  if TNodeObject(SelectedItem.Data) is TSampleNode then begin
    // Expand the Sample to the Occurrences level, properly populating if needed.
    SelectedItem.Expand(False);
    lNode := tvObservations.Items.AddChildObject(
        SelectedItem,
        Format('< %s >', [ResStr_NewTaxonOccurrence]),
        lTaxonOccNode);
    lNode.Parent.Expand(False);
  end else
    lNode := tvObservations.Items.AddObject(
        SelectedItem,
        Format('< %s >', [ResStr_NewTaxonOccurrence]),
        lTaxonOccNode);
  lNode.ImageIndex    := lTaxonOccNode.ImageIndex;
  lNode.SelectedIndex := lTaxonOccNode.ImageIndex;
  lNode.StateIndex    := lTaxonOccNode.StateImage;
  tvObservations.Selected := lNode;  // Change to new item. Display the detail form too
  Application.ProcessMessages;
  TfrmTaxonOccurrences(DetailForm).AddRecord(TNodeObject(SelectedItem.Parent.Data).ItemKey);
  FAddingNewNode := False;
end;  // actAddTaxonOccExecute

//==============================================================================
procedure TfrmObservations.actAddBioOccExecute(Sender: TObject);
var lBioOccNode: TBiotopeOccNode;
    lNode      : TFlyNode;
begin
  inherited;
  FAddingNewNode := True;
  lBioOccNode := TBiotopeOccNode.Create;
  lBioOccNode.ItemKey := '';
  lBioOccNode.ItemAdditional := TN_BIOTOPE_OCCURRENCE;
  lBioOccNode.ChildrenPopulated := True;
  if TNodeObject(SelectedItem.Data) is TSampleNode then begin
    // Expand the Sample to the Occurrences level, properly populating if needed.
    SelectedItem.Expand(False);
    lNode := tvObservations.Items.AddChildObject(
        SelectedItem,
        Format('< %s >', [ResStr_NewBiotopeOccurrence]),
        lBioOccNode);
    lNode.Parent.Expand(False);  // Make sure it's expanded, if adding first child node.
  end else
    lNode := tvObservations.Items.AddObject(
        SelectedItem,
        Format('< %s >', [ResStr_NewBiotopeOccurrence]),
        lBioOccNode);
  lNode.ImageIndex    := lBioOccNode.ImageIndex;
  lNode.SelectedIndex := lBioOccNode.ImageIndex;
  lNode.StateIndex    := lBioOccNode.StateImage;
  tvObservations.Selected := lNode;   // Change to new item. Display the detail form too
  Application.ProcessMessages;
  TfrmBiotopeOccurrences(DetailForm).AddRecord(TNodeObject(SelectedItem.Parent.Data).ItemKey);
  FAddingNewNode := False;
end;  // actAddBioOccExecute

//==============================================================================
// Register Drag/Drop coomponents and event handlers
procedure TfrmObservations.SetupDestinationControls;
begin
  RegisterDragComponent(tvObservations, GetNodeData);
  if AppSettings.UserAccessLevel >= ualAddOnly then
    RegisterDropComponentAdvanced(tvObservations, DropNode,
                                  [TN_SURVEY, TN_SURVEY_EVENT, TN_SAMPLE,
                                   TN_BIOTOPE_OCCURRENCE, TN_TAXON_OCCURRENCE],
                                  [CF_JNCCDATA], DragOverCheck);
end;  // SetupDestinationControls

{-------------------------------------------------------------------------------
  Determines which node (in tvObservations) has been dropped onto then determines
  what is being dropped and passes ToNode to the appropriate procedure
}
procedure TfrmObservations.DropNode(const Sender: TObject; const iFormat: integer;
  const iSourceData: TKeyList; const iTextStrings: TStringList;
  const iIsPasteOperation: Boolean; var ioHandled: Boolean);
var lPos: TPoint;
    lTableName, lMessage: String;
    lKey: TKeyString;
    lDestNode, lSourceNode: TFlyNode;
begin
  if (AppSettings.UserAccessLevel >= ualAddOnly) and (iSourceData.Header.ItemCount>0) and
      bbAdd.Enabled then
  begin
    if iIsPasteOperation then lMessage := ResStr_CutAndPaste
                         else lMessage := ResStr_DragAndDrop;
    ValidateValue(iSourceData.Items[0].KeyField2 = 'FromTree',
        Format(ResStr_HierarchyOperation, [lMessage]));

    if iFormat = CF_JNCCDATA then
    begin
      if iIsPasteOperation then begin
        // Can't move any node to "top" level, only surveys allowed and they can't move.
        lDestNode := tvObservations.Selected;
        if not Assigned(lDestNode) then begin
          MessageDlg(ResStr_SelectTarget, mtInformation, [mbOK], 0);
          Exit;
        end;

        lSourceNode := tvObservations.Items.GetFirstNode;
        while Assigned(lSourceNode) do begin
          if (TNodeObject(lSourceNode.Data).ItemKey = iSourceData.Items[0].KeyField1) and
             (TNodeObject(lSourceNode.Data).ItemAdditional = iSourceData.Header.TableName) then
            Break;
          lSourceNode := lSourceNode.GetNextVisible;
        end;
        // Something propbably went wrong somewhere.
        if not Assigned(lSourceNode) then Exit;

        // Only accept through "Cut" operation.
        if not IsCut then
          lMessage := ResStr_ClipboardCopy
        else
          lMessage := ResStr_MoveItem;
      end else begin
        lSourceNode := tvObservations.Selected;
        lPos := tvObservations.ScreenToClient(Mouse.CursorPos);
        lDestNode := tvObservations.GetNodeAt(lPos.X, lPos.Y);
        lMessage := ResStr_MoveItem;
      end;

      if Assigned(lDestNode) then begin
        lTableName := iSourceData.Header.TableName;
        lKey := iSourceData.Items[0].KeyField1;

        if SameText(lTableName, TN_TAXON_OCCURRENCE) and AllowedMove(lKey, ntTaxonOcc) then
          DropTaxonOcc(lKey, lSourceNode, lDestNode, lMessage)
        else
        if SameText(lTableName, TN_BIOTOPE_OCCURRENCE) and AllowedMove(lKey, ntBiotopeOcc) then
          DropBiotopeOcc(lKey, lSourceNode, lDestNode, lMessage)
        else
        if SameText(lTableName, TN_SAMPLE) and AllowedMove(lKey, ntSample) then
          DropSample(lKey, lSourceNode, lDestNode, lMessage)
        else
        if SameText(lTableName, TN_SURVEY_EVENT) and AllowedMove(lKey, ntEvent) then
          DropEvent(lKey, lSourceNode, lDestNode, lMessage)
        else
        if SameText(lTableName, TN_SURVEY) and (TNodeObject(lDestNode.Data) is TSurveyTagNode) and
           AllowedMove(lKey, ntSurvey) then
          if iIsPasteOperation then
            DropSurvey(lKey, lSourceNode, lDestNode, IsCut, lMessage)
          else
            DropSurvey(lKey, lSourceNode, lDestNode, ssCtrl in KeyboardStateToShiftState, lMessage);
      end;
      tvObservations.Selected := lSourceNode;
      if iIsPasteOperation then Clipboard.Clear;
    end;
  end;
end;  // DropNode

//==============================================================================
{ Add survey tag to survey, or remove its current tag and add the new one in}
procedure TfrmObservations.DropSurvey(const AKey: TKeyString; ASourceNode, ADestNode: TFlyNode;
  move: Boolean; const AMsg: String);
var
  idx: Integer;
begin
  // CTRL indicates a move. So remove current tag first
  if move then
    dmDatabase.RunStoredProc('usp_SurveyTag_Delete_ForSurvey', ['@SurveyKey', AKey]);

  // Add new tag
  dmDatabase.RunInsertStoredProc('Survey_Tag',
          'usp_SurveyTag_Insert',
          ['@SurveyKey', Akey,
           '@ConceptKey', TSurveyTagNode(ADestNode.Data).ItemKey,
           '@SessionID', AppSettings.SessionID],
           '@Key');
  SurveyToReselect := AKey;
  idx := 0;
  if DetailForm is TfrmSurveyDetails then
    idx := TfrmSurveyDetails(DetailForm).pcSurveyDetails.ActivePageIndex;
  PostMessage(Handle, WM_REFRESH_OBSERVATIONS, idx, 0);
end;

//==============================================================================
{Determines whether the Event can be moved here and moves it if it can}
procedure TfrmObservations.DropEvent(const AKey: TKeyString; ASourceNode, ADestNode: TFlyNode;
    const AMsg: String);
var lDestData,lSourceData: TSurveyNode;
    lSurveyNode: TFlyNode;
    sRef, sRefSys, LocName: String;
    lValidResult : TValidationResult;
begin
  //If not dropped on a Survey then change ADestNode to its ultimate parent
  //and try to drop the Event there
  while not (TNodeObject(ADestNode.Data) is TSurveyNode) do ADestNode := ADestNode.Parent;
  lDestData := TSurveyNode(ADestNode.Data);
  lSurveyNode := ASourceNode;
  while not (TNodeObject(lSurveyNode.Data) is TSurveyNode) do lSurveyNode := lSurveyNode.Parent;
  lSourceData := TSurveyNode(lSurveyNode.Data);
  if lDestData.ItemKey <> GetSurveyKeyForEvent(AKey) then begin
    ValidateValue(dmValidation.CheckEventDateAgainstSurvey(lDestData.ItemKey,
                                                           GetVagueDate(TN_SURVEY_EVENT, AKey)),
                  ResStr_EventCannotMove + ResStr_EventDateNotInSurvey);
    GetSpatialRefForEvent(AKey, sRef, sRefSys, LocName);

    if (sRef <> '') or (LocName = '') then begin
      lValidResult := dmValidation.CheckEventInSurvey(lDestData.ItemKey, sRef, sRefSys, '');
      ValidateValue(lValidResult.Success, ResStr_EventCannotMove + lValidResult.Message);
    end;

    // Event can't be moved from a temporary survey
    lValidResult := dmValidation.CheckIsTemporary(lSourceData.ItemKey);
    ValidateValue(not(lValidResult.Success), ResStr_TemporaryCannotMoveFrom);

    // Event can't be be moved to a temporary survey
    lValidResult := dmValidation.CheckIsTemporary(lDestData.ItemKey);
    ValidateValue(not(lValidResult.Success), ResStr_TemporaryCannotMoveTo);

    //All validation complete move the Event
    if MessageDlg(Format(AMsg, [GetEventName(AKey), ADestNode.Text]),
                  mtConfirmation, [mbOK, mbCancel], 0) = idOK then
    begin
      ADestNode.Expand(False);
      CheckedSelectedNode(ASourceNode, ntEvent, AKey).MoveTo(ADestNode, naAddChild);
      MoveEvent(AKey, lDestData.ItemKey);
    end;
  end;
end;

//------------------------------------------------------------------------------
{Determines whether the Sample can be moved here and moves it if it can}
procedure TfrmObservations.DropSample(const AKey: TKeyString; ASourceNode, ADestNode: TFlyNode;
    const AMsg: String);
var lDestData: TEventNode;
    sRef, sRefSys, LocKey, LocName,DestSurvey,FromSurvey: String;
    lValidResult : TValidationResult;
begin
  //If dropped on a Biotope Occurrence/Taxon Occurrence or another Sample
  //then change ADestNode to its parent (or parent's parent)
  //and try to drop the Sample there
  while not ((TNodeObject(ADestNode.Data) is TEventNode) or
      (TNodeObject(ADestNode.Data) is TSurveyNode) or
      (TNodeObject(ADestNode.Data) is TSurveyTagNode)) do
    ADestNode := ADestNode.Parent;
  if TNodeObject(ADestNode.Data) is TEventNode then begin
    lDestData := TEventNode(ADestNode.Data);
    if lDestData.ItemKey <> GetEventKeyForSample(AKey) then begin
      FromSurvey :=  GetSurveyKeyForSample(AKey);
      // Sample  can't be moved from a temporary survey
      lValidResult := dmValidation.CheckIsTemporary(FromSurvey);
      ValidateValue(not(lValidResult.Success), ResStr_TemporaryCannotMoveFrom);
      DestSurvey := getSurveyKeyforEvent(lDestData.ItemKey);
      // Sample can't be be moved to a temporary survey
      lValidResult := dmValidation.CheckIsTemporary(DestSurvey);
      ValidateValue(not(lValidResult.Success), ResStr_TemporaryCannotMoveTo);
      ValidateValue(dmValidation.CheckSampleDateAgainstEvent(lDestData.ItemKey,
              GetVagueDate(TN_SAMPLE, AKey)),
              ResStr_SampleCannotMove + ResStr_SampleDateAgainstEvent);
      GetSpatialRefForSample(AKey, sRef, LocKey, sRefSys, LocName);
      if (sRef <> '') or (LocKey <> '') or (LocName = '') then begin
        lValidResult := dmValidation.CheckSampleInEvent(lDestData.ItemKey, sRef, LocKey, sRefSys);
        ValidateValue(lValidResult.Success, ResStr_SampleCannotMove + lValidResult.Message);
      end;
     //Mike
      If Not CheckRecordersInEvent(AKey, lDestData.ItemKey) then begin
        if MessageDlg(ResStr_RecorderNotInSample, mtConfirmation,[mbNo, mbYes], 0) = mrYes then
          AddRecordersToNewEvent(AKey, lDestData.ItemKey);
      end;
      ValidateValue(CheckRecordersInEvent(AKey, lDestData.ItemKey),ResStr_SampleCannotMove+ResStr_RecorderNotInEvent);
      //All validation complete move the Sample
      if MessageDlg(Format(AMsg, [GetSampleName(AKey), ADestNode.Text]),
                    mtConfirmation, [mbOK, mbCancel], 0) = idOK then
      begin
        ADestNode.Expand(False);
        CheckedSelectedNode(ASourceNode, ntSample, AKey).MoveTo(ADestNode, naAddChild);
        MoveSample(AKey, lDestData.ItemKey);
      end;
    end;
  end else
    MessageDlg(ResStr_DragSample, mtInformation, [mbOK], 0);
end;

//------------------------------------------------------------------------------
{Determines whether the Biotope Occurrence can be moved here
and moves it if it can}
procedure TfrmObservations.DropBiotopeOcc(const AKey: TKeyString; ASourceNode, ADestNode: 
    TFlyNode; const AMsg: String);
var lDestData: TSampleNode;
begin
  //If dropped on a Taxon Occurrence or another Biotope Occurrence then change
  //ADestNode to its parent and try to drop the Biotope Occurrence there
  if (TNodeObject(ADestNode.Data) is TBiotopeOccNode) or
     (TNodeObject(ADestNode.Data) is TTaxonOccNode) then ADestNode := ADestNode.Parent;

  if TNodeObject(ADestNode.Data) is TSampleNode then begin
    lDestData := TSampleNode(ADestNode.Data);
    if lDestData.ItemKey <> GetSampleKeyForBiotopeOcc(AKey) then begin
      ValidateValue(dmValidation.CheckDeterminationDateAgainstSample(lDestData.ItemKey,
                                                                     GetDetDate('Biotope', AKey)),
                    ResStr_CannotMoveBiotope + ResStr_DetDateAgainstSample);

      //All validation complete move the Biotope Occurrence
      if MessageDlg(Format(AMsg, [GetBiotopeNameForOccurrence(AKey), ADestNode.Text]),
                    mtConfirmation, [mbOK, mbCancel], 0) = idOK then
      begin
        ADestNode.Expand(False);
        CheckedSelectedNode(ASourceNode, ntBiotopeOcc, AKey).MoveTo(ADestNode, naAddChild);
        MoveBiotopeOcc(AKey, lDestData.ItemKey);
      end;
    end;
  end else
    MessageDlg(ResStr_BiotopeDragToSample, mtInformation, [mbOK], 0);
end;

//------------------------------------------------------------------------------
{Determines whether the Taxon Occurrence can be moved here
and moves it if it can}
procedure TfrmObservations.DropTaxonOcc(const AKey: TKeyString; ASourceNode, ADestNode: 
    TFlyNode; const AMsg: String);
var lDestData: TSampleNode;
begin
  //If dropped on a Biotope Occurrence or another Taxon Occurrence then change
  //ADestNode to its parent and try to drop the Taxon Occurrence there
  if (TNodeObject(ADestNode.Data) is TBiotopeOccNode) or
     (TNodeObject(ADestNode.Data) is TTaxonOccNode) then ADestNode := ADestNode.Parent;

  if TNodeObject(ADestNode.Data) is TSampleNode then begin
    lDestData := TSampleNode(ADestNode.Data);
    if lDestData.ItemKey <> GetSampleKeyForTaxonOcc(AKey) then begin
      ValidateValue(dmValidation.CheckDeterminationDateAgainstSample(lDestData.ItemKey,
                                                                     GetDetDate('Taxon', AKey)),
                    ResStr_CannotMoveTaxon + ResStr_DetDateAgainstSample);

      //All validation complete move the Taxon Occurrence
      if MessageDlg(Format(AMsg, [GetTaxonNameForOccurrence(AKey), ADestNode.Text]),
                    mtConfirmation, [mbOK, mbCancel], 0) = idOK then
      begin
        ADestNode.Expand(False);
        CheckedSelectedNode(ASourceNode, ntTaxonOcc, AKey).MoveTo(ADestNode, naAddChild);
        MoveTaxonOcc(AKey, lDestData.ItemKey);
      end;
    end;
  end else
    MessageDlg(ResStr_DragTaxonOccureceSample, mtInformation, [mbOK], 0);
end;

//==============================================================================
{Moves a Taxon Occurrence between Samples in the database.}
procedure TfrmObservations.MoveTaxonOcc(const TaxonOccKey, SampleKey: TKeyString);
begin
  dmDatabase.RunStoredProc('usp_TaxonOccurrence_Update_ForDragDrop',
      ['@Key', TaxonOccKey, '@ParentKey', SampleKey, '@UserID', AppSettings.UserID]);
end;

{Moves a Biotope Occurrence between Samples in the database.}
procedure TfrmObservations.MoveBiotopeOcc(const BiotopeOccKey, SampleKey: TKeyString);
begin
  dmDatabase.RunStoredProc('usp_BiotopeOccurrence_Update_ForDragDrop',
      ['@Key', BiotopeOccKey, '@ParentKey', SampleKey, '@UserID', AppSettings.UserID]);
end;

{Moves an Event between Surveys in the database.}
procedure TfrmObservations.MoveEvent(const EventKey, SurveyKey: TKeyString);
begin
  dmDatabase.RunStoredProc('usp_SurveyEvent_Update_ForDragDrop',
      ['@Key', EventKey, '@ParentKey', SurveyKey, '@UserID', AppSettings.UserID]);
end;

{Moves a sample between events in the database.}
procedure TfrmObservations.MoveSample(const SampleKey, EventKey: TKeyString);
var AStringList, AnotherStringList: TStringList;
    k: integer;
begin
  dmDatabase.RunStoredProc('usp_Sample_Update_ForDragDrop',
      ['@Key', SampleKey, '@ParentKey', EventKey, '@UserID', AppSettings.UserID]);

  with dmGeneralData.qryAllPurpose do begin
    // Now alter the Sample_Recorder table
    AStringList := TStringList.Create;
    AnotherStringList := TStringList.Create;
    try
      // Make two String lists with the old SE_Recorder_Keys and
      // corresponding Name_Keys
      SQL.Text :=
         'SELECT SER.Name_Key, SER.SE_Recorder_Key ' +
         'FROM Survey_Event_Recorder SER INNER JOIN Sample_Recorder SR ON ' +
         '     SER.SE_Recorder_Key = SR.SE_Recorder_Key ' +
         'WHERE SR.Sample_Key = ' + QuotedStr(SampleKey);
      try
        Open;
        while not Eof do begin
          AStringList.Add(FieldByName('Name_Key').AsString);
          AnotherStringList.Add(FieldByName('SE_Recorder_Key').AsString);
          Next;
        end;
      finally
        Close;
      end;

      // Replace the Name_Keys in AStringList with the corresponding
      // SE_Recorder_Key for the new Event
      SQL.Clear;
      SQL.Add('SELECT SE_Recorder_Key ');
      SQL.Add('FROM Survey_Event_Recorder ');
      SQL.Add('WHERE Survey_Event_Key = ''' + EventKey + ''' AND ');
      SQL.Add(';');
      for k := AStringList.Count - 1 downto 0 do try
        SQL.Strings[3] := 'Name_Key = ' + QuotedStr(AStringList.Strings[k]);
        Open;
        AStringList.Strings[k] := FieldByName('SE_Recorder_Key').AsString;
      finally
        Close;
      end;

      // Replace the SE_Recorder_Keys in the Sample_Recorder table
      // with the appropriate SE_Recorder_Key for the new event
      SQL.Clear;
      SQL.Add('UPDATE Sample_Recorder ');
      SQL.Add('SET SE_Recorder_Key = ');
      SQL.Add(''''' ');
      SQL.Add('WHERE Sample_Key = ''' + SampleKey + ''' AND ');
      SQL.Add('SE_Recorder_Key = ');
      SQL.Add('''''');
      for k := AStringList.Count - 1 downto 0 do begin
        SQL.Strings[2] := QuotedStr(AStringList.Strings[k]) + ' ';
        SQL.Strings[5] := QuotedStr(AnotherStringList.Strings[k]);
        ExecSQL;
      end;
    finally
      AStringList.Free;
      AnotherStringList.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
function TfrmObservations.CheckedSelectedNode(ANode: TFlyNode; NodeType: TNodeType;
  const AKey: TKeyString): TFlyNode;
var
  lNodeData: TObservationNode;
begin
  with tvObservations do
  begin
    Result := ANode;
    if Assigned(ANode) then begin
      lNodeData := TObservationNode(ANode.Data);
      case NodeType of
        ntTaxonOcc:
          if (not (lNodeData is TTaxonOccNode)) or (lNodeData.ItemKey <> AKey) then
            Result := nil;
        ntBiotopeOcc:
          if (not (lNodeData is TBiotopeOccNode)) or (lNodeData.ItemKey <> AKey) then
            Result := nil;
        ntSample:
          if (not (lNodeData is TSampleNode)) or (lNodeData.ItemKey <> AKey) then
            Result := nil;
        ntEvent:
          if (not (lNodeData is TEventNode)) or (lNodeData.ItemKey <> AKey) then
            Result := nil;
        ntSurvey:
          if (not (lNodeData is TSurveyNode)) or (lNodeData.ItemKey <> AKey) then
            Result := nil;
      end; // case
    end;
    if Result = nil then // selected node is not the correct node
      raise EObservations.CreateNonCritical(ResStr_DragAndDropLimitation);
  end; // with tvObservations
end;

{Returns True if the key given matches a record with the current SiteID as its Custodian}
function TfrmObservations.AllowedMove(const AKey: TKeyString; ANodeType: TNodeType): Boolean;
var
  lTableName: String;
begin
  Result := False;
  case ANodeType of
    ntTaxonOcc  : lTableName := TN_TAXON_OCCURRENCE;
    ntBiotopeOcc: lTableName := TN_BIOTOPE_OCCURRENCE;
    ntSample    : lTableName := TN_SAMPLE;
    ntEvent     : lTableName := TN_SURVEY_EVENT;
    ntSurvey    : lTableName := TN_SURVEY;
  else
    Exit;
  end;

  // Check user is allowed to do anything with node first.
  if AppSettings.UserAccessLevel = ualAddOnly then
    with dmDatabase.ExecuteSQL(Format('SELECT Entered_By FROM %s WHERE %s_Key = ''%s''',
                               [lTableName, lTableName, AKey]), True) do
      try
        // Ignore as add only user but not your data
        if Fields['Entered_By'].Value <> AppSettings.UserID then
          Exit;
      finally
        Close;
      end; // try

  if lTableName = TN_SURVEY then
    Result := True
  else
    // If user allowed, check has custody also.
    Result := dmGeneralData.HaveCustody(lTableName, lTableName + '_Key', AKey);
end;

{Returns the scientific name of the Taxon Occurrence}
function TfrmObservations.GetTaxonNameForOccurrence(const AKey: TKeyString): String;
begin
  Result:='';
  SetupNameQuery(dmGeneralData.qryAllPurpose, ntTaxonOcc, AKey);
  with dmGeneralData.qryAllPurpose do begin
    Open;
    if not Eof then Result := FieldByName('Preferred_Name').AsString;
    Close;
  end;
end;

{Gets the vague date of the Survey/Survey_Event/Sample}
function TfrmObservations.GetVagueDate(const ATable: String; const AKey: TKeyString): TVagueDate;
begin
  with dmGeneralData.qryAllPurpose do
  try
    SQL.Text :=
      'SELECT Vague_Date_Start, Vague_Date_End, Vague_Date_Type ' +
      'FROM  ' + ATable + ' ' +
      'WHERE ' + ATable + '_Key = ' + QuotedStr(AKey);
    Open;
    Result.StartDate := FieldByName('Vague_Date_Start').AsInteger;
    Result.EndDate := FieldByName('Vague_Date_End').AsInteger;
    Result.DateTypeString := FieldByName('Vague_Date_Type').AsString;
  finally
    Close;
  end;
end;

{Returns the determination date for a Biotope or Taxon Occurrence}
function TfrmObservations.GetDetDate(const ATable: String; const AKey: TKeyString): TVagueDate;
begin
  with dmGeneralData.qryAllPurpose do
  try
    SQL.Text :=
      'SELECT Vague_Date_Start, Vague_Date_End, Vague_Date_Type ' +
      'FROM  ' + ATable + '_Determination ' +
      'WHERE ' + ATable + '_Occurrence_Key = ' + QuotedStr(AKey);
    Open;
    Result.StartDate := FieldByName('Vague_Date_Start').AsInteger;
    Result.EndDate := FieldByName('Vague_Date_End').AsInteger;
    Result.DateTypeString := FieldByName('Vague_Date_Type').AsString;
  finally
    Close;
  end;
end;

{Returns the Sample Key for a Taxon Occurrence}
function TfrmObservations.GetSampleKeyForTaxonOcc(const AKey: TKeyString): String;
begin
  with dmGeneralData.qryAllPurpose do
  try
    SQL.Text :=
      'SELECT Sample_Key ' +
      'FROM Taxon_Occurrence ' +
      'WHERE Taxon_Occurrence_Key = ' + QuotedStr(AKey) ;
    Open;
    Result := FieldByName('Sample_Key').AsString;
  finally
    Close;
  end;
end;

{Returns the Sample Key for a Biotope Occurrence}
function TfrmObservations.GetSampleKeyForBiotopeOcc(const AKey: TKeyString): String;
begin
  with dmGeneralData.qryAllPurpose do
  try
    SQL.Text :=
      'SELECT Sample_Key ' +
      'FROM Biotope_Occurrence ' +
      'WHERE Biotope_Occurrence_Key = ' + QuotedStr(AKey);
    Open;
    Result := FieldByName('Sample_Key').AsString;
  finally
    Close;
  end;
end;

{Returns the Survey Event Key for a Sample}
function TfrmObservations.GetEventKeyForSample(const AKey: TKeyString): String;
begin
  with dmGeneralData.qryAllPurpose do
  try
    SQL.Text :=
      'SELECT Survey_Event_Key ' +
      'FROM Sample ' +
      'WHERE Sample_Key = ' + QuotedStr(AKey);
    Open;
    Result := FieldByName('Survey_Event_Key').AsString;
  finally
    Close;
  end;
end;

{Returns the Survey Key for a Survey Event}
function TfrmObservations.GetSurveyKeyForEvent(const AKey: TKeyString): String;
begin
  with dmGeneralData.qryAllPurpose do
  try
    SQL.Text :=
      'SELECT Survey_Key ' +
      'FROM Survey_Event ' +
      'WHERE Survey_Event_Key = ' + QuotedStr(AKey);
    Open;
    Result := FieldByName('Survey_Key').AsString;
  finally
    Close;
  end;
end;
{Returns the Survey Key for a Sample}
function TfrmObservations.GetSurveyKeyForSample(const AKey: TKeyString): String;
begin
  with dmGeneralData.qryAllPurpose do
  try
    SQL.Text :=
      'SELECT Survey_Key ' +
      'FROM Survey_Event ' +
      'INNER JOIN SAMPLE ON SAMPLE.SURVEY_EVENT_KEY ' +
      ' =  SURVEY_EVENT.SURVEY_EVENT_KEY ' +
      'WHERE Sample_Key = ' + QuotedStr(AKey);
    Open;
    Result := FieldByName('Survey_Key').AsString;
  finally
    Close;
  end;
end;
{Outputs the Spatial Reference and Location Key for a Sample}
procedure TfrmObservations.GetSpatialRefForSample(const AKey: TKeyString;
  out sRef, LocKey, sRefSys, LocName: String);
begin
  with dmGeneralData.qryAllPurpose do
  try
    SQL.Text :=
      'SELECT Spatial_Ref, Spatial_Ref_System, Location_Key, Lat, Long, ' +
      '       Location_Name ' +
      'FROM Sample ' +
      'WHERE Sample_Key = ' + QuotedStr(AKey);
    Open;
    sRef    := FieldByName('Spatial_Ref').AsString;
    sRefSys := FieldByName('Spatial_Ref_System').AsString;
    LocKey  := FieldByName('Location_Key').AsString;
    LocName := FieldByName('Location_Name').AsString;
  finally
    Close;
  end;
end;

{Outputs the Spatial Reference for a Survey Event}
procedure TfrmObservations.GetSpatialRefForEvent(const AKey: TKeyString;
  out SRef, sRefSys, LocName: String);
begin
  with dmGeneralData.qryAllPurpose do
  try
    SQL.Text := 'SELECT Spatial_Ref, Spatial_Ref_System, Lat, Long, ' +
                '       Location_Name ' +
                'FROM Survey_Event ' +
                'WHERE Survey_Event_Key = ' + QuotedStr(AKey);
    Open;
    sRef    := FieldByName('Spatial_Ref').AsString;
    sRefSys := FieldByName('Spatial_Ref_System').AsString;
    LocName := FieldByName('Location_Name').AsString;
  finally
    Close;
  end;
end;

{Checks that the recorders listed for the Sample and Recorders for the Event
it is trying to move to.}
function TfrmObservations.CheckRecordersInEvent(const ASampleKey, AnEventKey: TKeyString): Boolean;
var AStringList: TStringList;
    k: integer;
begin
  AStringList := TStringList.Create;
  try
    with dmGeneralData.qryAllPurpose do
    try
      //Get the recorders Name Keys
      SQL.Text :=
         'SELECT SER.Name_Key ' +
         'FROM Sample_Recorder SR INNER JOIN Survey_Event_Recorder SER ' +
         '     ON SR.SE_Recorder_Key = SER.SE_Recorder_Key ' +
         'WHERE Sample_Key = ' + QuotedStr(ASampleKey);
      Open;
      while not Eof do begin
        AStringList.Add(FieldByName('Name_Key').AsString);
        Next;
      end;
      Close;

      //Check the Name Keys against the Survey Event being moved to
      Result := True;
      SQL.Clear;
      SQL.Add('SELECT * FROM Survey_Event_Recorder ');
      SQL.Add('WHERE Survey_Event_Key = ''' + AnEventKey + ''' AND Name_Key = ');
      SQL.Add('''''');
      for k := AStringList.Count - 1 downto 0 do begin
        SQL.Strings[2] := QuotedStr(AStringList.Strings[k]);
        Open;
        if Eof then Result := False;
        Close;
      end;

    finally
      Close;
    end;
  finally
    AStringList.Free;
  end;
end;

{Returns the name of the Biotope Occurrence}
function TfrmObservations.GetBiotopeNameForOccurrence(
  const AKey: TKeyString): String;
begin
  Result:='';
  SetupNameQuery(dmGeneralData.qryAllPurpose, ntBiotopeOcc, AKey);
  with dmGeneralData.qryAllPurpose do begin
    Open;
    if not Eof then Result := FieldByName('Original_Code').AsString + ', ' +
                              FieldByName('Short_Term').AsString;
    Close;
  end;
end;

{-------------------------------------------------------------------------------
  Gets the displayed name of the location.
}
function TfrmObservations.GetLocationName(Recordset: ADODB._Recordset): String;
var
  location, sref, locationName: string;
begin
  location := Trim(VarToStr(Recordset.Fields['Item_Name'].Value));
  locationName := Trim(VarToStr(Recordset.Fields['Location_Name'].Value));
  sref := Trim(VarToStr(Recordset.Fields['Spatial_Ref'].Value));
  Result := GetLocalityLabelFromFields(location, locationName, sref);
end;

{Returns the name of the Sample}
function TfrmObservations.GetSampleName(const AKey: TKeyString): String;
begin
  Result:='';
  SetupNameQuery(dmGeneralData.qryAllPurpose, ntSample, AKey);
  with dmGeneralData.qryAllPurpose do begin
    Open;
    if not Eof then Result := GetSampleName(Recordset);
    Close;
  end;
end;

function TfrmObservations.GetSampleName(Recordset: ADODB._Recordset): String;
var
  SampleRef: string;
begin
  Result :=
      VagueDateToString(dmGeneralData.GetVagueDateFromRecordset(Recordset))
          + ' - ' + GetLocationName(Recordset)
          + ' - ' + Recordset.Fields['Short_Name'].Value;

  SampleRef := VarToStr(Recordset.Fields['Sample_Reference'].Value);
  if SampleRef <> '' then Result := SampleRef + ' - ' + Result;
end;

{Returns the name of the Event}
function TfrmObservations.GetEventName(const AKey: TKeyString): String;
begin
  Result:='';
  SetupNameQuery(dmGeneralData.qryAllPurpose, ntEvent, AKey);
  with dmGeneralData.qryAllPurpose do begin
    Open;
    Result := GetEventName(Recordset);
    Close;
  end;
end;

function TfrmObservations.GetEventName(Recordset: ADODB._Recordset): string;
begin
  Result :=
      VagueDateToString(dmGeneralData.GetVagueDateFromRecordset(Recordset))
      + ' - ' + GetLocationName(Recordset);
end;

//==============================================================================
{Returns the name of the Survey}
{Sets up some standard name queries}
procedure TfrmObservations.SetupNameQuery(qry: TJNCCQuery;  NameFor: TNodeType;
  const AKey: String);
begin
  with qry.SQL do begin
    Clear;
    case NameFor of
      ntTaxonOcc: begin
        Text := 'SELECT ITN.Preferred_Name ' +
                'FROM Taxon_Determination TD INNER JOIN Index_Taxon_Name ITN ' +
                '     ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key ' +
                'WHERE TD.Taxon_Occurrence_Key = ' + QuotedStr(AKey);
      end;
      ntBiotopeOcc: begin
        Add('SELECT B.Short_Term, B.Original_Code ');
        Add('FROM (Biotope B INNER JOIN Biotope_List_Item BLI ON ' +
            '     B.Biotope_Key = BLI.Biotope_Key) INNER JOIN ' +
            '     Biotope_Determination BD ON BLI.Biotope_List_Item_Key = ' +
            '     BD.Biotope_List_Item_Key ');
        Add('WHERE BD.Biotope_Occurrence_Key = ' + QuotedStr(AKey));
      end;
      ntSample: begin
        Add('SELECT S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type, ' +
            '       S.Sample_Reference, ST.Short_Name, LN.Item_Name, ' +
            '       S.Spatial_Ref, S.Location_Name ');
        Add('FROM Sample S INNER JOIN Sample_Type ST ON ' +
            '     ST.Sample_Type_Key = S.Sample_Type_Key ' +
            'LEFT JOIN Location_Name LN ON ' +
            '     LN.Location_Key = S.Location_Key ');
        Add('WHERE S.Sample_Key = ' + QuotedStr(AKey));
      end;
      ntEvent: begin
        Add('SELECT SE.Vague_Date_Start, SE.Vague_Date_End, ' +
            '       SE.Vague_Date_Type, LN.Item_Name, ' +
            '       SE.Spatial_Ref, SE.Location_Name ');
        Add('FROM Survey_Event SE LEFT JOIN Location_Name LN ON ' +
            '     SE.Location_Key = LN.Location_Key ');
        Add('WHERE SE.Survey_Event_Key = ' + QuotedStr(AKey));
      end;
    end;
  end;
end;

//==============================================================================
// Fill Drag/Drop structure with selected node info
procedure TfrmObservations.GetNodeData(const Sender: TObject; var oDropSource: TJNCCDropSource);
begin
  if AppSettings.UserAccessLevel >= ualAddOnly then
    if Assigned(tvObservations.Selected) then
      with tvObservations.Selected do begin
        if TNodeObject(Data).ItemKey = '' then
          oDropSource.DropData.SetTable('InvalidRecordForDragDrop')
        else
          oDropSource.DropData.SetTable(TNodeObject(Data).ItemAdditional);
        oDropSource.DropData.AddItem(TNodeObject(Data).ItemKey, 'FromTree');
      end;
end;  // GetNodeData

//==============================================================================
{ Overriden method which all exportable forms must support.  Creates and returns
    a key list indicating the selection in the hierarchy }
function TfrmObservations.GetKeyList: TKeyList;
var
  lNode : TObservationNode;
  lKeyList : TEditableKeyList;
begin
  Result := nil;
  if Assigned(SelectedItem) then begin
    lNode := TObservationNode(SelectedItem.Data);
    if lNode.ItemKey <> '' then begin
      lKeyList := TEditableKeyList.Create;
      try
        lKeyList.SetTable(lNode.ItemAdditional);
        lKeyList.AddItem(lNode.ItemKey, '');
        Result := lKeyList;
      except
        lKeyList.Free;
        raise
      end;
    end;
  end;
end;  // GetKeyList

{-------------------------------------------------------------------------------
  Builds a keylist for revalidation purposes. Includes all child items for selected item.
}
function TfrmObservations.GetKeyListForValidation: TKeyList;
var
  keyList: TEditableKeyList;
  nodeData: TObservationNode;
begin
  Result := nil;
  if Assigned(SelectedItem) then begin
    nodeData := TObservationNode(SelectedItem.Data);
    if nodeData.ItemKey <> '' then begin
      keyList := TEditableKeyList.Create;
      try
        keyList.ConvertToMixedData;
        keyList.AddItem(nodeData.ItemKey, nodeData.ItemAdditional);
        with dmDatabase.GetRecordset(
            'usp_Observations_Select_AllChildrenForLevel',
            ['@Key', nodeData.ItemKey, '@Type', nodeData.ItemAdditional]) do
          if State = adStateOpen then begin
            while not Eof do begin
              keyList.AddItem(Fields['ItemKey'].Value, Fields['TableName'].Value);
              MoveNext;
            end;
            Close;
          end;
        Result := keyList;
      except
        keyList.Free;
        raise;
      end;
    end;
  end;
end;  // GetKeyListForValidation

//==============================================================================
procedure TfrmObservations.ReSortSurvey(const ASort: String);
var
  nodeKey: TKeyString;
  tableName: String;
begin
  with FdmObservation.qrySurvey do SQL[SQL.Count - 1] := 'ORDER BY ' + ASort;
  nodeKey := TKeyData(SelectedItem.Data).ItemKey;
  tableName := TKeyData(SelectedItem.Data).ItemAdditional;
  ClearTree;
  PopulateSurveyTagLevel;
  PopulateSurveyLevel(nil);
  // Since it could have been a Tag selected, use SelectNode.
  SelectNode(tableName, nodeKey);
end;  // ReSortSurvey

//------------------------------------------------------------------------------
procedure TfrmObservations.actSortSurveyNameExecute(Sender: TObject);
begin
  FSurveyField := SORT_SURVEY_NAME;
  ReSortSurvey('SurveyName');
end;  // actSortSurveyNameExecute

//------------------------------------------------------------------------------
procedure TfrmObservations.actSortSurveyRunByExecute(Sender: TObject);
begin
  FSurveyField := SORT_SURVEY_RUN_BY;
  ReSortSurvey('FullName');
end;  // actSortSurveyRunByExecute

//==============================================================================
procedure TfrmObservations.UpdateOrder(ANode:TFlyNode);
var
  parentNode: TFlyNode;
  cursor: TCursor;
  tableName: String;
  nodeKey: TKeyString;
begin
  LockWindowUpdate(Handle);
  tvObservations.Items.BeginUpdate;
  cursor := HourglassCursor;
  try
    with tvObservations do begin
      nodeKey   := TNodeObject(ANode.Data).ItemKey;
      tableName := TNodeObject(ANode.Data).ItemAdditional;
      parentNode := ANode.Parent;
      KillChildren(parentNode);
      Items.AddChild(parentNode, '-');  // To force expanding event to take place
      SelectNode(tableName, nodeKey);
    end;
  finally
    DefaultCursor(cursor);
    tvObservations.Items.EndUpdate;
    LockWindowUpdate(0);
  end;
  tvObservations.SetFocus;
end;  // UpdateOrder

//==============================================================================
procedure TfrmObservations.actSortEventDateExecute(Sender: TObject);
begin
  FEventField := SORT_EVENT_DATE;
  with FdmObservation.qryEvent do
    SQL[SQL.Count-1]:='ORDER BY Vague_Date_Start';
  UpdateOrder(tvObservations.Selected);
end;  // actSortEventDateExecute

//------------------------------------------------------------------------------
procedure TfrmObservations.actSortEventLocationExecute(Sender: TObject);
begin
  FEventField := SORT_EVENT_LOCATION;
  with FdmObservation.qryEvent do
    SQL[SQL.Count-1]:='ORDER BY LN.Item_Name, E.Spatial_Ref, E.Location_Name';
  UpdateOrder(tvObservations.Selected);
end;  // actSortEventLocationExecute

//==============================================================================
procedure TfrmObservations.actSortSampleReferenceExecute(Sender: TObject);
begin
  FSampleField := SORT_SAMPLE_REF;
  with FdmObservation.qrySample do
    SQL[SQL.Count-1]:='ORDER BY Sample_Reference';
  UpdateOrder(tvObservations.Selected);
end;  // actSortSampleReferenceExecute

//------------------------------------------------------------------------------
procedure TfrmObservations.actSortSampleDateExecute(Sender: TObject);
begin
  FSampleField := SORT_SAMPLE_DATE;
  with FdmObservation.qrySample do
    SQL[SQL.Count-1]:='ORDER BY Vague_Date_Start';
  UpdateOrder(tvObservations.Selected);
end;  // actSortSampleDateExecute

//------------------------------------------------------------------------------
procedure TfrmObservations.actSortSampleTypeExecute(Sender: TObject);
begin
  FSampleField := SORT_SAMPLE_TYPE;
  with FdmObservation.qrySample do
    SQL[SQL.Count-1]:='ORDER BY Short_Name';
  UpdateOrder(tvObservations.Selected);
end;  // actSortSampleTypeExecute

//==============================================================================
procedure TfrmObservations.actSortTaxonScientificExecute(Sender: TObject);
begin
  FTaxonOccField := SORT_TAXON_SCI_NAME;
  with FdmObservation.qryTaxonOcc do
    SQL[SQL.Count-1]:='ORDER BY ITN.Preferred_Name';
  UpdateOrder(tvObservations.Selected);
end;  // actSortTaxonScientificExecute

//------------------------------------------------------------------------------
procedure TfrmObservations.actSortTaxonCommonExecute(Sender: TObject);
begin
  FTaxonOccField := SORT_TAXON_COMMON_NAME;
  with FdmObservation.qryTaxonOcc do
    SQL[SQL.Count-1]:='ORDER BY ITN.Common_Name';
  UpdateOrder(tvObservations.Selected);
end;  // actSortTaxonCommonExecute

//==============================================================================
procedure TfrmObservations.actSortBioCodeExecute(Sender: TObject);
begin
  FBiotopeOccField := SORT_BIOTOPE_CODE;
  with FdmObservation.qryBiotopeOcc do
    SQL[SQL.Count-1]:='ORDER BY B.Original_Code';
  UpdateOrder(tvObservations.Selected);
end;  // actSortBioCodeExecute

//------------------------------------------------------------------------------
procedure TfrmObservations.actSortBioNameExecute(Sender: TObject);
begin
  FBiotopeOccField := SORT_BIOTOPE_SHORT_NAME;
  with FdmObservation.qryBiotopeOcc do
    SQL[SQL.Count-1]:='ORDER BY B.Short_Term';
  UpdateOrder(tvObservations.Selected);
end;  // actSortBioNameExecute

//==============================================================================
procedure TfrmObservations.RefreshSurvey(const ASurveyKey, AEventKey, ASampleKey : TKeyString);
var lNodeText             :String;
    lSNode,lNode,lSampNode:TFlyNode;
    lEventNode            :TEventNode;
    lSampleNode           :TSampleNode;
    lCursor               :TCursor;
begin
  lCursor := HourglassCursor;
  with tvObservations do
    try
      lSNode := Items.GetFirstNode;
      while Assigned(lSNode) do begin
        // Find Survey to refresh, and refresh only if node already expanded
        if (TNodeObject(lSNode.Data).ItemKey = ASurveyKey) then
          if lSNode.Expanded or TNodeObject(lSNode.Data).ChildrenPopulated then
          begin
            lNode := lSNode.GetFirstChild;
            while Assigned(lNode) and (TNodeObject(lNode.Data).ItemKey <> AEventKey) do
              lNode := lNode.GetNextSibling;

            // Event node exists and is expanded, check if sample exists
            if Assigned(lNode) then
            begin
              if lNode.Expanded or TNodeObject(lNode.Data).ChildrenPopulated then
              begin
                lSampNode := lNode.GetFirstChild;
                while Assigned(lSampNode) and (TNodeObject(lSampNode.Data).ItemKey <> ASampleKey) do
                  lSampNode := lSampNode.GetNextSibling;

                with FdmObservation.qrySample do begin
                  SQL[QRYSAMPLE_REPLACE_ROW] := '';
                  FdmObservation.SampleEvent := AEventKey;
                  Open;
                  // Record exists as it has been just added by the record card
                  while FieldByName('Sample_Key').AsString <> ASampleKey do
                    Next;
                  // but just in case...
                  if not Eof then begin
                    lNodeText  := GetSampleName(Recordset);
                    lSampleNode := TSampleNode.Create;
                    with lSampleNode do begin
                      ItemKey   := FieldByName('Sample_Key').AsString;
                      SampleType.ItemKey   := FieldByName('Sample_Type_Key').AsString;
                      SampleType.ShortName := FieldByName('Short_Name').AsString;
                      ItemAdditional       := TN_SAMPLE;
                      ImageIndex :=
                          dmFormActions.GetSampleImageIndex(SampleType.ItemKey) + DEF_SAMPLE_IMAGE;
                      // 0: not outstanding card. 1: card but not all checked. 2: card and all checked
                      case FieldByName('Outstanding_Card').AsInteger of
                        0: OutstandingCard := rcNone;
                        1: OutstandingCard := rcUnchecked;
                        2: OutstandingCard := rcChecked;
                      end;
                    end;
                    // Add new sample as event child
                    if Assigned(lSampNode) then begin
                      lNode := Items.InsertObject(lSampNode, lNodeText, lSampleNode);
                      lNode.ImageIndex    := TObservationNode(lSampNode.Data).ImageIndex;
                      lNode.SelectedIndex := lSampNode.ImageIndex;
                      lNode.StateIndex    := TObservationNode(lSampNode.Data).StateImage;
                      Items.AddChild(lNode, '-');
                      lSampNode.Delete;
                    end else begin
                      lNode := Items.AddChildObject(lNode, lNodeText, lSampleNode);
                      lNode.ImageIndex    := TObservationNode(lNode.Data).ImageIndex;
                      lNode.SelectedIndex := lNode.ImageIndex;
                      lNode.StateIndex    := TObservationNode(lNode.Data).StateImage;
                      Items.AddChild(lNode, '-');
                    end;
                  end;
                  Close;
                end;
              end;
            end else begin
              // Event not found, new one, add it to the tree
              with FdmObservation.qryEvent do begin
                SQL[QRYEVENT_REPLACE_ROW]  := '';
                FdmObservation.EventSurvey := ASurveyKey;
                Open;
                // Record exists as it has been just added by the record card
                while FieldByName('Survey_Event_Key').AsString <> AEventKey do
                  Next;
                // but just in case...
                if not Eof then begin
                  lNodeText  := GetEventName(Recordset);
                  lEventNode := TEventNode.Create;
                  lEventNode.ItemKey := FieldByName('Survey_Event_Key').AsString;
                  lEventNode.ItemAdditional := TN_SURVEY_EVENT;
                  // Add as Survey child node
                  lNode := Items.AddChildObject(lSNode, lNodeText, lEventNode);
                  lNode.ImageIndex    := TObservationNode(lNode.Data).ImageIndex;
                  lNode.SelectedIndex := lNode.ImageIndex;
                  lNode.StateIndex    := TObservationNode(lNode.Data).StateImage;
                  Items.AddChild(lNode, '-');
                end;
                Close;
              end;
            end;
          Break;
        end;
        lSNode := lSNode.GetNextSibling;
      end;
    finally
      DefaultCursor(lCursor);
    end;
end;  //RefreshSurvey

//==============================================================================
procedure TfrmObservations.RefreshSamples;
var lSurveyNode, lEventNode, lSampleNode:TFlyNode;
    lSampleData:TSampleNode;
    lSurveyData:TNodeObject;
    lIdx:integer;
begin
  // Refresh the imagelist used by the treeview
  for lIdx := ilObservation.Count - 1 downto DEF_SAMPLE_IMAGE do
    ilObservation.Delete(lIdx);
  // Add Sample bitmaps, including new ones, or without deleted ones, at the end
  ilObservation.AddImages(dmFormActions.ilSampleTypes);

  lSurveyNode := tvObservations.Items.GetFirstNode;
  if Assigned(lSurveyNode) then begin
    lSurveyData := TNodeObject(lSurveyNode.Data);

    // For each survey
    while lSurveyNode <> nil do begin
      // If content populated then check each event
      if ((lSurveyData.ChildrenPopulated) and (not(lSurveyData is TSurveyTagNode))) then begin
        lEventNode := lSurveyNode.GetFirstChild;
        while (lEventNode <> nil) and Assigned(lEventNode.Data) do begin
          // If event populated, update image index of each sample node
          if TNodeObject(lEventNode.Data).ChildrenPopulated then begin
            lSampleNode:=lEventNode.GetFirstChild;
            // Now update the sample glyph
            while lSampleNode <> nil do begin
              lSampleData := TSampleNode(lSampleNode.Data);
              lSampleData.ImageIndex :=
                  dmFormActions.GetSampleImageIndex(lSampleData.SampleType.ItemKey) + DEF_SAMPLE_IMAGE;
              lSampleNode.ImageIndex    := lSampleData.ImageIndex;
              lSampleNode.SelectedIndex := lSampleData.ImageIndex;
              lSampleNode := lSampleNode.GetNextSibling;
            end;  // while lSampleNode<>nil
          end;
          lEventNode := lEventNode.GetNextSibling;
        end;  // while lEventNode<>nil
      end;
      lSurveyNode := lSurveyNode.GetNextSibling;
    end;  // while lSurveyNode<>nil
  end;
  Repaint;
end;  // RefreshSamples

//==============================================================================
procedure TfrmObservations.DisplayObservations(const AType:TRelatedData; const AKeyList:TKeyList);
begin
  if (DetailForm<>nil) and (TfrmBaseDockedForm(DetailForm).EditMode<>emView) then
    MessageDlg(ResStr_CannotProceed, mtWarning, [mbOk], 0)
  else begin
    // Make sure all filtering removed before filtered again on something different
    FRelatedData := rdOccurrence;
    if bbShowAll.Visible then ClearFilter(FILTER_OBSERVATION); 
    // Now proceed
    FRelatedData:=AType;
    case AType of
      rdSurvey    : DisplaySurveys(AKeyList);
      rdEvent     : DisplayEvents(AKeyList);
      rdSample    : DisplaySamples(AKeyList);
      rdOccurrence: DisplayOccurrences(AKeyList);
    end;
    if AKeyList.Header.ItemCount=1 then begin
      // select first item in list
      case AType of
        rdSurvey    : SelectNode(TN_SURVEY, AKeyList.Items[0].KeyField1);
        rdEvent     : SelectNode(TN_SURVEY_EVENT, AKeyList.Items[0].KeyField1);
        rdSample    : SelectNode(TN_SAMPLE, AKeyList.Items[0].KeyField1);
        rdOccurrence: SelectNode(AKeyList.ItemTable[0], AKeyList.Items[0].KeyField1);
      end;
    end;
  end;
end;  // DisplayObservations

//==============================================================================
procedure TfrmObservations.DisplayFilteredObservations;
var
  childKeys, parentKeys: TEditableKeyList;

  procedure PopulateKeyFilter(const tableName: String; localKeys, filteredKeys: TStringList);
  var
    i: Integer;
  begin
    if filteredKeys <> nil then
      for i := 0 to filteredKeys.Count - 1 do
        childKeys.AddItem(filteredKeys.Names[i], tableName);
    if childKeys.Header.ItemCount > 0 then
      // Since keyList can already contain keys, add the whole lot now to local list
      for i := 0 to childKeys.Header.ItemCount - 1 do
        localKeys.Add(childKeys.Items[i].KeyField1)
    else
      localKeys.Add('-');
  end;

  function ValidSurveys(childKeys: TKeyList): Boolean;
  var
    validKeys: TEditableKeyList;
  begin
    validKeys := TEditableKeyList.Create;
    try
      dmGeneralData.ConvertListUsingSQL(childKeys, validKeys,
          'SELECT DISTINCT Survey_Key AS ItemKey FROM Survey WHERE Survey_Key IN ');
      Result := validKeys.Header.ItemCount > 0;
    finally
      validKeys.Free;
    end;
  end;

begin
  ClearFilter(FILTER_OBSERVATION);
  FRelatedData := rdNone;

  childKeys := TEditableKeyList.Create;
  parentKeys := TEditableKeyList.Create;
  try
    // Get samples related to filtered occurrences
    childKeys.SetTable(MIXED_DATA);
    PopulateKeyFilter(
        TN_TAXON_OCCURRENCE,
        FTaxonOccKeys,
        AppSettings.GetFilteredRecords(TN_TAXON_OCCURRENCE));
    GetSampleKeysForTaxonOccurrences(childKeys, parentKeys);
    childKeys.Clear;
    PopulateKeyFilter(
        TN_BIOTOPE_OCCURRENCE,
        FBioOccKeys,
        AppSettings.GetFilteredRecords(TN_BIOTOPE_OCCURRENCE));
    // Don't clear sample keys found for taxon occurrences
    GetSampleKeysForBiotopeOccurrences(childKeys, parentKeys);
    childKeys.Assign(parentKeys);  // Keep the identified required samples
    PopulateKeyFilter(
        TN_SAMPLE,
        FSampleKeys,
        AppSettings.GetFilteredRecords(TN_SAMPLE));

    // Get related events
    parentKeys.Clear;
    GetEventKeysForSamples(childKeys, parentKeys);
    childKeys.Assign(parentKeys);  // Keep the identified required events
    PopulateKeyFilter(
        TN_SURVEY_EVENT,
        FEventKeys,
        AppSettings.GetFilteredRecords(TN_SURVEY_EVENT));

    // Get related surveys
    parentKeys.Clear;
    GetSurveyKeysForEvents(childKeys, parentKeys);
    childKeys.Assign(parentKeys); // Keep the identified required surveys
    PopulateKeyFilter(
        TN_SURVEY,
        FSurveyKeys,
        AppSettings.GetFilteredRecords(TN_SURVEY));

    if ValidSurveys(childKeys) then begin
      // Get related survey tags
      parentKeys.Clear;
      GetSurveyTagsForSurveys(childKeys, parentKeys);
      childKeys.Assign(parentKeys);
      PopulateKeyFilter(
          TN_SURVEY_TAG,
          FSurveyTagKeys,
          nil);
      DisplaySurveys;
    end else
      bbShowAllClick(nil);
  finally
    childKeys.Free;
    parentKeys.Free;
  end;
end;

//==============================================================================
procedure TfrmObservations.GetSurveyTagsForSurveys(surveyKeys: TKeyList; surveyTagKeys: TEditableKeyList);
begin
  dmGeneralData.ConvertListUsingSQL(surveyKeys, surveyTagKeys,
      'SELECT DISTINCT Concept_Key AS ItemKey FROM Survey_Tag WHERE Survey_Key IN ');
end;

//==============================================================================
procedure TfrmObservations.DisplaySurveys(const ASurveyList: TKeyList);
var
  i: Integer;
  tagKeys: TEditableKeyList;
begin
  with ASurveyList do begin
    FSurveyKeys.Clear;
    for i := 0 to Header.ItemCount - 1 do FSurveyKeys.Add(Items[i].Keyfield1);
  end;
  // Need to filter the survey tags too
  tagKeys := TEditableKeyList.Create;
  try
    GetSurveyTagsForSurveys(ASurveyList, tagKeys);
    with tagKeys do begin
      FSurveyTagKeys.Clear;
      for i := 0 to Header.ItemCount - 1 do FSurveyTagKeys.Add(Items[i].Keyfield1);
    end;
  finally
    tagKeys.Free;
  end;

  DisplaySurveys;
end;

procedure TfrmObservations.DisplaySurveys;
var
  cursor: TCursor;
begin
  cursor := HourglassCursor;
  try
    ClearTree;
    PopulateSurveyTagLevel;
    PopulateSurveyLevel(nil);
    with tvObservations do begin
      Selected := Items.GetFirstNode;
      if Assigned(Selected) then
        TopItem  := Selected;
    end;
  finally
    bbShowAll.Visible := True;
    DefaultCursor(cursor);
  end;
end;  // DisplaySurveys

//==============================================================================
procedure TfrmObservations.GetSurveyKeysForEvents(eventKeys: TKeyList; surveyKeys: TEditableKeyList);
begin
  dmGeneralData.ConvertListUsingSQL(eventKeys, surveyKeys,
      'SELECT DISTINCT Survey_Key AS ItemKey FROM Survey_Event WHERE Survey_Event_Key IN ');
end;

{-------------------------------------------------------------------------------
}
procedure TfrmObservations.DisplayEvents(const AEventList:TKeyList);
var lSurveyKeys:TEditableKeyList;
    lCursor    :TCursor;
    lIdx       :integer;
begin
  lCursor:=HourglassCursor;
  try
    // Find the Surveys for the Events
    lSurveyKeys:=TEditableKeyList.Create;
    try
      GetSurveyKeysForEvents(AEventList, lSurveyKeys);
      // Get the Survey keys
      DisplaySurveys(lSurveyKeys);
    finally
      lSurveyKeys.Free;
    end;

    // Store found Event keys in stringlist for use in PopulateTreeLevel
    with AEventList do begin
      FEventKeys.Clear;
      for lIdx:=0 to Header.ItemCount-1 do FEventKeys.Add(Items[lIdx].KeyField1);
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // DisplayEvents

//==============================================================================
procedure TfrmObservations.GetEventKeysForSamples(sampleKeys: TKeyList; eventKeys: TEditableKeyList);
begin
  dmGeneralData.ConvertListUsingSQL(sampleKeys, eventKeys,
      'SELECT DISTINCT Survey_Event_Key AS ItemKey FROM Sample WHERE Sample_Key IN ');
end;

{-------------------------------------------------------------------------------
}
procedure TfrmObservations.DisplaySamples(const ASampleList:TKeyList);
var lEventKeys   :TEditableKeyList;
    lCursor      :TCursor;
    lIdx         :integer;
begin
  lCursor:=HourglassCursor;
  try
    // Find the Events for the Samples
    lEventKeys:=TEditableKeyList.Create;
    try
      GetEventKeysForSamples(ASampleList, lEventKeys);
      // And go and get the Survey keys
      DisplayEvents(lEventKeys);
    finally
      lEventKeys.Free;
    end;

    // Store found Sample keys in stringlist for use in PopulateTreeLevel
    with ASampleList do begin
      FSampleKeys.Clear;
      for lIdx:=0 to Header.ItemCount-1 do FSampleKeys.Add(Items[lIdx].KeyField1);
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // DisplaySamples

//==============================================================================
procedure TfrmObservations.GetSampleKeysForTaxonOccurrences(taxonOccurrenceKeys: TKeyList;
  sampleKeys: TEditableKeyList);
begin
  // Get the taxon related samples
  dmGeneralData.ConvertListUsingSQL(taxonOccurrenceKeys, sampleKeys,
      'SELECT DISTINCT Sample_Key AS ItemKey FROM Taxon_Occurrence WHERE Taxon_Occurrence_Key IN ');
end;

{-------------------------------------------------------------------------------
}
procedure TfrmObservations.GetSampleKeysForBiotopeOccurrences(biotopeOccurrenceKeys: TKeyList;
  sampleKeys: TEditableKeyList);
begin
  // Get the biotope related samples
  dmGeneralData.ConvertListUsingSQL(biotopeOccurrenceKeys, sampleKeys,
      'SELECT DISTINCT Sample_Key AS ItemKey FROM Biotope_Occurrence WHERE Biotope_Occurrence_Key IN ');
end;

{-------------------------------------------------------------------------------
}
procedure TfrmObservations.DisplayOccurrences(const AOccurrenceList:TKeyList);
var lIdx       : Integer;
    lCursor    : TCursor;
    lTxKeys, lBioKeys, lSampleKeys: TEditableKeyList;
    lFilterTaxa, lFilterBiotopes: Boolean;
begin
  lCursor:=HourglassCursor;
  lFilterTaxa := False;
  lFilterBiotopes := False;
  lTxKeys :=TEditableKeyList.Create;
  lBioKeys:=TEditableKeyList.Create;
  try
    // Separate Taxon from Biotope occurrence Keys
    with AOccurrenceList do begin
      FTaxonOccKeys.Clear;
      FBioOccKeys.Clear;
      for lIdx:=0 to Header.ItemCount-1 do
        if CompareText(AOccurrenceList.ItemTable[lIdx], TN_TAXON_OCCURRENCE)=0 then begin
          lFilterTaxa := True;
          lTxKeys.AddItem(Items[lIdx].KeyField1, TN_TAXON_OCCURRENCE);
          FTaxonOccKeys.Add(Items[lIdx].KeyField1);
        end else begin
          lFilterBiotopes := True;
          lBioKeys.AddItem(Items[lIdx].KeyField1, TN_BIOTOPE_OCCURRENCE);
          FBioOccKeys.Add(Items[lIdx].KeyField1);
        end;
      // These 2 lines stop taxon appearing when filtering on biotopes and vice-versa.
      // The resaon is that the tree is populated with all taxa/biotopes if the
      // keylist is empty, so we add something that will never be found!
      if not lFilterTaxa then FTaxonOccKeys.Add('-');
      if not lFilterBiotopes then FBioOccKeys.Add('-');
    end;

    // Find the Samples for the Taxon Occurrences
    lSampleKeys:=TEditableKeyList.Create;
    try
      // Get the taxon related samples
      GetSampleKeysForTaxonOccurrences(lTxKeys, lSampleKeys);
      // Get the biotope related samples
      GetSampleKeysForBiotopeOccurrences(lBioKeys, lSampleKeys);
      // And go and get the Sample keys
      DisplaySamples(lSampleKeys);
    finally
      lSampleKeys.Free;
    end;
  finally
    lTxKeys.Free;
    lBioKeys.Free;
    DefaultCursor(lCursor);
  end;
end;  // DisplayOccurrences

//==============================================================================
procedure TfrmObservations.ClearFilter(const AName: String);
var
  lCursor: TCursor;
begin
  inherited;
  lCursor := HourglassCursor;
  LockWindowUpdate(Handle);
  try
    FEventKeys.Clear;
    FSampleKeys.Clear;
    FTaxonOccKeys.Clear;
    FBioOccKeys.Clear;
    FSurveyKeys.Clear;
    FSurveyTagKeys.Clear;
    ClearTree;
    PopulateSurveyTagLevel;
    PopulateSurveyLevel(nil);

    bbShowAll.Visible := False;
    with tvObservations do begin
      Selected := Items.GetFirstNode;
      SetFocus;
    end;
  finally
    LockWindowUpdate(0);
    DefaultCursor(lCursor);
  end;
end;

//==============================================================================
procedure TfrmObservations.bbShowAllClick(Sender: TObject);
var
  nodeChain: TStringList;
begin
  inherited;
  AppSettings.ClearFilteredRecords(
      [TN_SURVEY, TN_SURVEY_EVENT, TN_SAMPLE, TN_TAXON_OCCURRENCE, TN_BIOTOPE_OCCURRENCE]);
  nodeChain := TStringList.Create;
  try
    RememberPathToSelectedNode(tvObservations, nodeChain);
    ClearFilter(FILTER_OBSERVATION);
    RedisplaySelectedNode(tvObservations, nodeChain);
  finally
    nodeChain.Free;
  end;
  FRelatedData := rdNone;
end;  // bbShowAllClick

//==============================================================================
procedure TfrmObservations.mnuRelEventsClick(Sender: TObject);
var lCursor: TCursor;
    lEvents: TEditableKeyList;

    procedure GetEvents(ANode: TFlyNode; AEvents: TEditableKeyList);
    var
      lChild: TFlyNode;
      lNodeData: TNodeObject;
    begin
      lNodeData := TNodeObject(ANode.Data);
      if lNodeData is TEventNode then
        AEvents.AddItem(lNodeData.ItemKey, TN_SURVEY_EVENT)
      else
      if (lNodeData is TSurveyNode) then begin
        lChild := ANode.GetFirstChild;
        while Assigned(lChild) do begin
          GetEvents(lChild, AEvents);
          lChild := ANode.GetNextChild(lChild);
        end;
      end;
    end;

begin
  inherited;
  lCursor := HourglassCursor;
  try
    if Assigned(SelectedItem) then begin
      LockWindowUpdate(Handle);
      try
        SelectedItem.Expand(True); { ensures occurrences nodes are loaded }
        lEvents := TEditableKeyList.Create();
        try
          GetEvents(SelectedItem, lEvents);
          if lEvents.Header.ItemCount = 0 then
            MessageDlg(ResStr_NoEventsToRelate, mtInformation, [mbOk], 0)
          else
            DisplayObservations(rdEvent, lEvents);
        finally
          lEvents.Free;
        end;
      finally
        LockWindowUpdate(0);
      end;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // mnuRelEventsClick

//==============================================================================
procedure TfrmObservations.mnuRelSamplesClick(Sender: TObject);
var lCursor : TCursor;
    lSamples: TEditableKeyList;

    procedure GetSamples(ANode: TFlyNode; ASamples: TEditableKeyList);
    var
      lChild: TFlyNode;
      lNodeData: TNodeObject;
    begin
      lNodeData := TNodeObject(ANode.Data);
      if lNodeData is TSampleNode then
        ASamples.AddItem(lNodeData.ItemKey, TN_SAMPLE)
      else
      if (lNodeData is TSurveyNode) or (lNodeData is TEventNode) then begin
        lChild := ANode.GetFirstChild;
        while Assigned(lChild) do begin
          GetSamples(lChild, ASamples);
          lChild := ANode.GetNextChild(lChild);
        end;
      end;
    end;

begin
  inherited;
  lCursor := HourglassCursor;
  try
    if Assigned(SelectedItem) then begin
      LockWindowUpdate(Handle);
      try
        SelectedItem.Expand(True); { ensures occurrences nodes are loaded }
        lSamples := TEditableKeyList.Create;
        try
          GetSamples(SelectedItem, lSamples);
          if lSamples.Header.ItemCount = 0 then
            MessageDlg(ResStr_NoOccurenceToRelate, mtInformation, [mbOk], 0)
          else
            DisplayObservations(rdSample, lSamples);
        finally
          lSamples.Free;
        end;
      finally
        LockWindowUpdate(0);
      end;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // mnuRelSamplesClick

//==============================================================================
procedure TfrmObservations.mnuRelOccurClick(Sender: TObject);
var lCursor     : TCursor;
    lOccurrences: TEditableKeyList;

    procedure GetOccurrences(ANode: TFlyNode; AOccurrences: TEditableKeyList);
    var lChild: TFlyNode;
        lNodeData: TNodeObject;
    begin
      lNodeData := TNodeObject(ANode.Data);
      if lNodeData is TBiotopeOccNode then
        AOccurrences.AddItem(TNodeObject(ANode.Data).ItemKey, TN_BIOTOPE_OCCURRENCE)
      else
      if lNodeData is TTaxonOccNode then
        AOccurrences.AddItem(TNodeObject(ANode.Data).ItemKey, TN_TAXON_OCCURRENCE)
      else begin
        lChild := ANode.GetFirstChild;
        while Assigned(lChild) do begin
          GetOccurrences(lChild, AOccurrences);
          lChild := ANode.GetNextChild(lChild);
        end;
      end;
    end;

begin
  inherited;
  lCursor := HourglassCursor;
  try
    if Assigned(SelectedItem) then begin
      LockWindowUpdate(Handle);
      try
        SelectedItem.Expand(True); { ensures occurrences nodes are loaded }
        lOccurrences := TEditableKeyList.Create;
        try
          GetOccurrences(SelectedItem, lOccurrences);
          if lOccurrences.Header.ItemCount = 0 then
            MessageDlg(ResStr_NoOccurenceToRelate, mtInformation, [mbOk], 0)
          else
            DisplayObservations(rdOccurrence, lOccurrences);
        finally
          lOccurrences.Free;
        end;
      finally
        LockWindowUpdate(0);
      end;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // mnuRelOccurClick

//==============================================================================
procedure TfrmObservations.mnuRelLocationsClick(Sender: TObject);
var lNode      :TObservationNode;
    stMsg      :String;
    lKey       :TKeyString;
    lLocationList:TEditableKeyList;
begin
  inherited;
  lLocationList:=TEditableKeyList.Create;
  try
    if SelectedItem<>nil then begin
      lNode:=SelectedItem.Data;
      lKey :=TNodeObject(lNode).ItemKey;
      if lNode is TEventNode then begin
        dmGeneralData.GetKeyListFromStandardQuery(lLocationList,ssLocationsForEvent,lKey);
        stMsg:=ResStr_SurveyEvent;
      end else if lNode is TSampleNode then begin
        dmGeneralData.GetKeyListFromStandardQuery(lLocationList,ssLocationsForSample,lKey);
        stMsg:=ResStr_Sample;
      end else if (lNode is TTaxonOccNode) or (lNode is TBiotopeOccNode) then begin
        // Get sample key, key for parent node, to find locations
        lKey:=TNodeObject(SelectedItem.Parent.Data).ItemKey;
        dmGeneralData.GetKeyListFromStandardQuery(lLocationList,ssLocationsForSample,lKey);
        if lNode is TTaxonOccNode then stMsg:=ResStr_TaxonOcc
                                  else stMsg:=ResStr_BiotopeOcc;
      end else if lNode is TSurveyNode then begin
        with dmGeneralData do begin
          with qryAllPurpose do begin
            if Active then Close;
            SQL.Text := 'SELECT SE.Location_Key AS ItemKey FROM Survey_Event AS SE '+
                        'WHERE SE.Survey_Key = ''' + lKey + ''' '+
                        'AND SE.Location_Key IS NOT NULL ' +
                        'UNION ' +
                        'SELECT S.Location_Key FROM Sample AS S '+
                        'INNER JOIN Survey_Event AS SE ON S.Survey_Event_Key = SE.Survey_Event_Key ' +
                        'WHERE SE.Survey_Key = ''' + lKey + ''' ' +
                        'AND S.Location_Key IS NOT NULL';
            lLocationList.AddQueryResults(qryAllPurpose);
          end;  // with qryAllPurpose
        end;  // with dmGeneralData
        stMsg:=ResStr_Survey;
      end;
      if lLocationList.Header.ItemCount=0 then
        MessageDlg(Format(ResStr_NoLocationToRelate, [stMsg]),mtInformation,[mbOk],0)
      else
        TfrmLocations(dmFormActions.DisplayForm(TfrmLocations)).DisplayLocations(lLocationList);
    end;
  finally
    lLocationList.Free;
  end;
end;  // mnuRelLocationsClick

//==============================================================================
procedure TfrmObservations.mnuRelIndivOrgClick(Sender: TObject);
var lNode      :TObservationNode;
    stMsg      :String;
    lKey       :TKeyString;
    lNameList:TEditableKeyList;
begin
  inherited;
  lNameList:=TEditableKeyList.Create;
  try
    if SelectedItem<>nil then begin
      lNode:=SelectedItem.Data;
      lKey :=TNodeObject(lNode).ItemKey;
      if lNode is TSurveyNode then begin
        dmGeneralData.GetKeyListFromStandardQuery(lNameList,ssNamesForSurvey,lKey);
        stMsg:=ResStr_Survey;
      end else if lNode is TEventNode then begin
        dmGeneralData.GetKeyListFromStandardQuery(lNameList,ssNamesForEvent,lKey);
        stMsg:=ResStr_SurveyEvent;
      end else if lNode is TSampleNode then begin
        dmGeneralData.GetKeyListFromStandardQuery(lNameList,ssNamesForSample,lKey);
        stMsg:=ResStr_Sample;
      end else if lNode is TTaxonOccNode then begin
        dmGeneralData.GetKeyListFromStandardQuery(lNameList,ssNamesForTaxonOcc,lKey);
        stMsg:=ResStr_TaxonOcc;
      end else if lNode is TBiotopeOccNode then begin
        dmGeneralData.GetKeyListFromStandardQuery(lNameList,ssNamesForBiotopeOcc,lKey);
        stMsg:=ResStr_BiotopeOcc;
      end;

      if lNameList.Header.ItemCount=0 then
        MessageDlg(Format(ResStr_NoIndOrgToRelate, [stMsg]),mtInformation,[mbOk],0)
      else
        if dmFormActions.actNames.Execute then
          TfrmIndOrg(frmMain.GetForm(TfrmIndOrg)).DisplayNames(lNameList);
    end;
  finally
    lNameList.Free;
  end;
end;  // mnuRelIndivOrgClick

//==============================================================================
procedure TfrmObservations.mnuRelDocumentsClick(Sender: TObject);
var lNode      :TObservationNode;
    stMsg      :String;
    lSourceList:TEditableKeyList;
begin
  inherited;
  lSourceList:=TEditableKeyList.Create;
  try
    if SelectedItem<>nil then begin
      lNode:=SelectedItem.Data;
      if lNode is TSurveyNode then begin
        TfrmSurveyDetails(DetailForm).Sources.GetInternalSources(lSourceList);
        stMsg:=ResStr_Survey;
      end else if lNode is TEventNode then begin
        TfrmEventDetails(DetailForm).Sources.GetInternalSources(lSourceList);
        stMsg:=ResStr_SurveyEvent;
      end else if lNode is TSampleNode then begin
        TfrmSampleDetails(DetailForm).Sources.GetInternalSources(lSourceList);
        stMsg:=ResStr_Sample;
      end else if lNode is TTaxonOccNode then begin
        TfrmTaxonOccurrences(DetailForm).GetInternalSources(lSourceList);
        stMsg:=ResStr_Taxonocc;
      end else if lNode is TBiotopeOccNode then begin
        TfrmBiotopeOccurrences(DetailForm).GetInternalSources(lSourceList);
        stMsg:=ResStr_BiotopeOcc;
      end;

      if lSourceList.Header.ItemCount=0 then
        MessageDlg(Format(ResStr_NoDocumentToRelate, [stMsg]), mtInformation,[mbOk],0)
      else
        if dmFormActions.actDocuments.Execute then
          TfrmReferences(frmMain.GetForm(TfrmReferences)).DisplaySources(lSourceList);
    end;
  finally
    lSourceList.Free;
  end;
end;  // mnuRelDocumentsClick


//==============================================================================
procedure TfrmObservations.WMRefreshSpatialRefSystem(var Msg: TMessage);
begin
  if DetailForm<>nil then
    if DetailForm is TfrmSurveyDetails then TfrmSurveyDetails(DetailForm).SetDisplaySystem else
    if DetailForm is TfrmEventDetails  then TfrmEventDetails(DetailForm).SetDisplaySystem else
    if DetailForm is TfrmSampleDetails then TfrmSampleDetails(DetailForm).SetDisplaySystem;
end;  // WMRefreshSpatialRefSystem

//==============================================================================
procedure TfrmObservations.WMRefreshSpatialRef(var Msg: TMessage);
begin
  if DetailForm<>nil then
    if DetailForm is TfrmEventDetails  then TfrmEventDetails(DetailForm).RefreshSpatialRef else
    if DetailForm is TfrmSampleDetails then TfrmSampleDetails(DetailForm).RefreshSpatialRef;
end;  // WMRefreshSpatialRef

//==============================================================================
procedure TfrmObservations.WMRefreshDocuments(var Msg: TMessage);
begin
  if DetailForm<>nil then
    if DetailForm is TfrmSurveyDetails then TfrmSurveyDetails(DetailForm).Sources.RefreshLists else
    if DetailForm is TfrmEventDetails  then TfrmEventDetails(DetailForm).Sources.RefreshLists else
    if DetailForm is TfrmSampleDetails then TfrmSampleDetails(DetailForm).Sources.RefreshLists;
end;  // WMRefreshDocuments

//==============================================================================
procedure TfrmObservations.WMRefreshNames(var Msg: TMessage);
begin
  if DetailForm<>nil then
    if DetailForm is TfrmSurveyDetails then TfrmSurveyDetails(DetailForm).RefreshNames else
    if DetailForm is TfrmEventDetails  then TfrmEventDetails(DetailForm).RefreshNames else
    if DetailForm is TfrmSampleDetails then TfrmSampleDetails(DetailForm).RefreshNames else
    if DetailForm is TfrmTaxonOccurrences   then TfrmTaxonOccurrences(DetailForm).RefreshNames else
    if DetailForm is TfrmBiotopeOccurrences then TfrmBiotopeOccurrences(DetailForm).RefreshNames;
end;  // WMRefreshNames

//==============================================================================
procedure TfrmObservations.WMRefreshLocationDetails(var Msg: TMessage);
begin
  if DetailForm<>nil then
    if DetailForm is TfrmEventDetails  then TfrmEventDetails(DetailForm).RefreshEventLocation else
    if DetailForm is TfrmSampleDetails then TfrmSampleDetails(DetailForm).RefreshSampleLocation;
end;  // WMRefreshLocationDetails

//==============================================================================
procedure TfrmObservations.WMRefreshColours(var Msg: TMessage);
begin
  //need to test if user has option to edit and is in process of editting
  if DetailForm<>nil then
    if DetailForm is TfrmSurveyDetails then TfrmSurveyDetails(DetailForm).RefreshColours else
    if DetailForm is TfrmEventDetails  then TfrmEventDetails(DetailForm).RefreshColours else
    if DetailForm is TfrmSampleDetails then TfrmSampleDetails(DetailForm).RefreshColours else
    if DetailForm is TfrmTaxonOccurrences then TfrmTaxonOccurrences(DetailForm).RefreshColours else
    if DetailForm is TfrmBiotopeOccurrences then TfrmBiotopeOccurrences(DetailForm).RefreshColours;
  Repaint;
end;  // WMRefreshColours

//==============================================================================
procedure TfrmObservations.WMRefreshTaxonDic(var Msg: TMessage);
begin
  if DetailForm<>nil then
    if DetailForm is TfrmTaxonOccurrences then
      TfrmTaxonOccurrences(DetailForm).RefreshTaxonName;
end;  // WMRefreshTaxonDic

//==============================================================================
procedure TfrmObservations.WMRefreshBiotopeDic(var Msg: TMessage);
begin
  if DetailForm<>nil then
    if DetailForm is TfrmBiotopeOccurrences then
      TfrmBiotopeOccurrences(DetailForm).RefreshBiotopeName;
end;  // WMRefreshBiotopeDic

//==============================================================================
procedure TfrmObservations.WMImportedComplete(var Msg: TMessage);
begin
  //Need to test what happens after importing....
  //PopulateSurveyLevel(FdmObservation.qrySurvey);
end;  // WMImportedComplete

//==============================================================================
procedure TfrmObservations.actShowMetadataExecute(Sender: TObject);
begin
  DetailForm.ShowMetaData;
end;  // actShowMetadataExecute

//==============================================================================
procedure TfrmObservations.actFindOnMapExecute(Sender: TObject);
var
  lNode: TFlyNode;
  lNodeData: TNodeObject;
  lLocationKey, lSpatialRef, lSRefSys: String;
begin
  if Assigned(DetailForm) then
    FDetailForm.FindOnMap
  else begin
    lNode := tvObservations.Selected;
    lNodeData := TNodeObject(lNode.Data);

    if (lNodeData is TBiotopeOccNode) or (lNodeData is TTaxonOccNode) or (lNodeData is TAddinOccNode) then
    begin
      while not (TNodeObject(lNode.Data) is TSampleNode) do lNode := lNode.Parent;

      with dmDatabase.ExecuteSQL('SELECT Location_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long ' +
                                 'FROM Sample WHERE Sample_Key = ''' +
                                 TNodeObject(lNode.Data).ItemKey + '''', True) do
      begin
        if not Eof then begin
          lLocationKey := VarToStr(Fields['Location_Key'].Value);
          lSpatialRef  := VarToStr(Fields['Spatial_Ref'].Value);
          lSRefSys     := VarToStr(Fields['Spatial_Ref_System'].Value);
        end;
        Close;
      end;
      TfrmMap.CentreOnPlace(lLocationKey, tvObservations.Selected.Caption, lSpatialRef, lSRefSys);
    end;
  end;
end;  // actFindOnMapExecute

//==============================================================================
procedure TfrmObservations.ReadSortOrders;
var SomeBool: Boolean;
begin
  // Read Sort for Surveys
  if not AppSettings.ReadSort(Self, 'Survey', FSurveyField, SomeBool) then
    FSurveyField := SORT_SURVEY_NAME
  else if (FSurveyField <> SORT_SURVEY_NAME) and (FSurveyField <> SORT_SURVEY_RUN_BY) then
    FSurveyField := SORT_SURVEY_NAME;
  // Read Sort for Events
  if not AppSettings.ReadSort(Self, 'Event', FEventField, SomeBool) then
    FEventField := SORT_EVENT_DATE
  else if (FEventField <> SORT_EVENT_DATE) and (FEventField <> SORT_EVENT_LOCATION) then
    FEventField := SORT_EVENT_DATE;
  // Read Sort for Samples
  if not AppSettings.ReadSort(Self, 'Sample', FSampleField, SomeBool) then
    FSampleField := SORT_SAMPLE_REF
  else if (FSampleField <> SORT_SAMPLE_REF) and (FSampleField <> SORT_SAMPLE_DATE) and (FSampleField <> SORT_SAMPLE_TYPE) then
    FSampleField := SORT_SAMPLE_REF;
  // Read Sort for Taxa
  if not AppSettings.ReadSort(Self, 'TaxonOcc', FTaxonOccField, SomeBool) then
    FTaxonOccField := SORT_TAXON_SCI_NAME
  else if (FTaxonOccField <> SORT_TAXON_SCI_NAME) and (FTaxonOccField <> SORT_TAXON_COMMON_NAME) and (FTaxonOccField <> SORT_TAXON_SORT_ORDER)  then
    FTaxonOccField := SORT_TAXON_SCI_NAME;
  // Read Sort for Biotopes
  if not AppSettings.ReadSort(Self, 'BiotopeOcc', FBiotopeOccField, SomeBool) then
    FBiotopeOccField := SORT_BIOTOPE_CODE
  else if (FBiotopeOccField <> SORT_BIOTOPE_CODE) and (FBiotopeOccField <> SORT_BIOTOPE_SHORT_NAME) then
    FBiotopeOccField := SORT_BIOTOPE_CODE;
end;  // ReadSortOrders

//------------------------------------------------------------------------------
procedure TfrmObservations.WriteSortOrders;
begin
  AppSettings.WriteSort(Self, 'Survey', FSurveyField, True);
  AppSettings.WriteSort(Self, 'Event', FEventField, True);
  AppSettings.WriteSort(Self, 'Sample', FSampleField, True);
  AppSettings.WriteSort(Self, 'TaxonOcc', FTaxonOccField, True);
  AppSettings.WriteSort(Self, 'BiotopeOcc', FBiotopeOccField, True);
end;  // WriteSortOrders

//==============================================================================
procedure TfrmObservations.tvObservationsDrawCell(Sender: TObject;
  ACanvas: TCanvas; ACol, ARow: Integer; Rect: TRect; State: TExGridDrawState);
var
  lNode      : TFlyNode;
  lNodeData  : TNodeObject;
  lTaxOccNode: TTaxonOccNode;
  lRect      : TRect;
  shift: Integer;

  procedure DrawTerm(ARect: TRect; const ATerm: string; ASelected: boolean);
  var
    lCurrentText: string;
    lCurrentPos, lFormatPos: Integer;
    lXPos: Integer;
    lOldFontName: String;
  begin
    with ACanvas do
    begin
      // Arial is good for italic, but remember current one in use so we can switch back.
      lOldFontName := Font.Name;

      FillRect(ARect);
      lCurrentPos  := 1;
      lCurrentText := ATerm;
      lFormatPos   := Pos('<i>', lCurrentText);
      lXPos        := ARect.Left + 4;

      while lFormatPos > 0 do
      begin
        TextOut(lXPos, ARect.Top + 1, Copy(lCurrentText, lCurrentPos, lFormatPos - 1));

        lXPos        := lXPos + TextWidth(Copy(lCurrentText, lCurrentPos, lFormatPos - 1));
        lCurrentText := Copy(lCurrentText, lFormatPos + 3, 255);
        lFormatPos   := Pos('</i>', lCurrentText);
        Font.Name    := 'Arial';
        Font.Style   := Font.Style + [fsItalic];
        TextOut(lXPos, ARect.Top + 1, Copy(lCurrentText, lCurrentPos, lFormatPos - 1));

        lXPos        := lXPos + TextWidth(Copy(lCurrentText, lCurrentPos, lFormatPos - 1)) + 2;
        lCurrentText := Copy(lCurrentText, lFormatPos + 4, 255);
        lFormatPos   := Pos('<i>', lCurrentText);
        Font.Name    := lOldFontName;
        ACanvas.Font.Style := Font.Style - [fsItalic];
      end;
      TextOut(lXPos, ARect.Top + 1, lCurrentText);
      Font.Name := lOldFontName;
    end;
  end;

begin
  inherited;
  lNode := tvObservations.GetNodeAtRow(ARow);
  if Assigned(lNode) then begin
    lNodeData := TNodeObject(lNode.Data);

    // Set the font colour to differentiate between highlighted and non-highlighted nodes
    if lNode.Selected then
      ACanvas.Font.Color := clHighlightText
    else
      dmGeneralData.SetCanvasColourForFilterNode(ACanvas, TNodeObject(lNode.Data).IsFiltered);
    lRect := Rect;

    if (lNodeData is TTaxonOccNode) then
    begin
      lTaxOccNode := TTaxonOccNode(lNodeData);
      lRect.Left  := lRect.Left + (lNode.Level + 3) * tvObservations.Indent;
      ACanvas.FillRect(lRect);         // Ensure background shading properly aligned.
      lRect.Left  := lRect.Left + 4;   // Line up with original text.
      if lTaxOccNode.TaxonNameObject <> nil then
        GenericDrawTaxonNames(ACanvas, lTaxOccNode.TaxonNameObject, lRect, True);
    end else begin
      // Work out where to start displaying the caption.
      shift      := 1 + Ord(lNode.StateIndex > 0) + Ord(lNode.ImageIndex <> -1);
      lRect.Left := lRect.Left + (lNode.Level + shift) * tvObservations.Indent;
      DrawTerm(lRect, lNode.Caption, lNode.Selected);
    end;
  end;
end;  // tvObservationsDrawCell

//------------------------------------------------------------------------------
procedure TfrmObservations.tvObservationsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_DELETE: if bbDelete.Enabled then
      // Note - this MUST be handled by a posted message, otherwise the treeview
      // control still has a reference to the missing node during the event handler.
      // See VI 14555
      PostMessage(Handle, WM_DELETE_NODE, 0, 0);
  end;
end;

//==============================================================================
{ This method ensures that the ancestral deactivate is called. }
procedure TfrmObservations.FormDeactivate(Sender: TObject);
begin
  inherited;
  // Do nothing
end;

function TfrmObservations.GetCurrentControlEditMode: TEditMode;
begin
  if not Assigned(DetailForm) then // safety check
    Result := emNone
  else
  begin
    Result := TfrmBaseDockedForm(DetailForm).EditMode; // default
    // Disable edit lock messages for all sub list grids
    if DetailForm is TfrmSurveyDetails then
      with TfrmSurveyDetails(DetailForm) do
        if (pcSurveyDetails.ActivePage = tsSources) then
          Result :=emNone;

    if DetailForm is TfrmEventDetails then
      with TfrmEventDetails(DetailForm) do
        if (pcEventDetails.ActivePage = tsSources) then
          Result := emNone;

    if DetailForm is TfrmSampleDetails then
      with TfrmSampleDetails(DetailForm) do
        if (pcSampleDetails.ActivePage = tsMeasurements) or
           (pcSampleDetails.ActivePage = tsRelations) or
           (pcSampleDetails.ActivePage = tsSources) or
           (pcSampleDetails.ActivePage = tsAdminAreas) then
          Result:= emNone;

    if DetailForm is TfrmTaxonOccurrences then
      with TfrmTaxonOccurrences(DetailForm) do
        if (pcTaxonOccurrence.ActivePage = tsDeterminations) or
           (pcTaxonOccurrence.ActivePage = tsRelatedOccurrences) or
           (pcTaxonOccurrence.ActivePage = tsSpecimens) or
           (pcTaxonOccurrence.ActivePage = tsSources) or
           (pcTaxonOccurrence.ActivePage = tsPrivate) or
           (pcTaxonOccurrence.ActivePage = tsMeasurements)then
          Result:= emNone;

    if DetailForm is TfrmBiotopeOccurrences then
      with TfrmBiotopeOccurrences(DetailForm) do
        if (pcBiotopeOccurrence.ActivePage = tsDeterminations) or
           (pcBiotopeOccurrence.ActivePage = tsSources) or
           (pcBiotopeOccurrence.ActivePage = tsMeasurements) then
          Result:= emNone;
  end;
end;

//==============================================================================
// Find the check box to check or uncheck. And update the Sample State image too.
procedure TfrmObservations.tvObservationsStateImageClicked(Sender: TObject;
  Node: TFlyNode);
begin
  inherited;
  if (bbAdd.Enabled) and (AppSettings.UserAccessLevel >= ualFullUser) then
    if (tvObservations.Items.Count > 0) and
       (SelectedItem <> nil) and
       (SelectedItem.Parent <> nil) and
       (TNodeObject(SelectedItem.Parent.Data) is TSampleNode) then
      CheckedOccurrence;
end;

{-------------------------------------------------------------------------------
  Description : Implements IProvidesOccurrencesSQL.CanProvideSql
              Returns True if a node is selected
  Created : 11/3/2003 }
function TfrmObservations.Get_CanProvideSQL: WordBool;
begin
  Result := Assigned(SelectedItem);
end;

{-------------------------------------------------------------------------------
  Description : Implements IProvidesOccurrencesSQL.CanProvideSql
              Returns SQL that lists occurrences for the current node.  This
              enables the Quick Report functionality
  Created : 11/3/2003 }
function TfrmObservations.Get_OccurrencesSQL(const ATypes: WideString): WideString;
var
  lTaxonSQL, lBiotopeSQL : String;
  nodeData: TNodeObject;
const
  SQL_TAXON_OCCURRENCES =
      'SELECT TOCC.TAXON_OCCURRENCE_KEY AS OCCURRENCE_KEY, ''T'' AS TYPE '+
      'FROM TAXON_OCCURRENCE TOCC %s ' +
      'WHERE %s=''%s'' ';
  SQL_BIOTOPE_OCCURRENCES =
      'SELECT BOCC.BIOTOPE_OCCURRENCE_KEY AS OCCURRENCE_KEY, ''B'' AS TYPE '+
      'FROM BIOTOPE_OCCURRENCE BOCC %s ' +
      'WHERE %s=''%s'' ';
  // union the various queries together as required
  function AddQueryToUnion(const AOrigQuery, ANewQuery: String): String;
  begin
    if AOrigQuery = '' then
      Result := ANewQuery
    else
      Result := AOrigQuery + ' UNION ' + ANewQuery;
  end;

begin
  nodeData := TNodeObject(SelectedItem.Data);
  if nodeData is TSurveyNode then begin
    lTaxonSQL := Format(SQL_TAXON_OCCURRENCES, [
            'INNER JOIN SAMPLE TSAM ON TOCC.SAMPLE_KEY=TSAM.SAMPLE_KEY '+
            'INNER JOIN SURVEY_EVENT TSE ON TSAM.SURVEY_EVENT_KEY=TSE.SURVEY_EVENT_KEY',
            'TSE.SURVEY_KEY',
            nodeData.ItemKey]);
    lBiotopeSQL := Format(SQL_BIOTOPE_OCCURRENCES, [
            'INNER JOIN SAMPLE BSAM ON BOCC.SAMPLE_KEY=BSAM.SAMPLE_KEY '+
            'INNER JOIN SURVEY_EVENT BSE ON BSAM.SURVEY_EVENT_KEY=BSE.SURVEY_EVENT_KEY',
            'BSE.SURVEY_KEY',
            nodeData.ItemKey]);
  end else
  if nodeData is TEventNode then begin
    lTaxonSQL := Format(SQL_TAXON_OCCURRENCES, [
            'INNER JOIN SAMPLE TSAM ON TOCC.SAMPLE_KEY=TSAM.SAMPLE_KEY',
            'TSAM.SURVEY_EVENT_KEY',
            nodeData.ItemKey]);
    lBiotopeSQL := Format(SQL_BIOTOPE_OCCURRENCES, [
            'INNER JOIN SAMPLE BSAM ON BOCC.SAMPLE_KEY=BSAM.SAMPLE_KEY',
            'BSAM.SURVEY_EVENT_KEY',
            nodeData.ItemKey]);
  end else
  if nodeData is TSampleNode then begin
    lTaxonSQL := Format(SQL_TAXON_OCCURRENCES, [
            '', 'TOCC.SAMPLE_KEY',
            nodeData.ItemKey]);
    lBiotopeSQL := Format(SQL_BIOTOPE_OCCURRENCES, [
            '', 'BOCC.SAMPLE_KEY',
            nodeData.ItemKey]);
  end else
  if nodeData is TBiotopeOccNode then begin
    lBiotopeSQL := Format(SQL_BIOTOPE_OCCURRENCES, [
            '', 'BOCC.BIOTOPE_OCCURRENCE_KEY',
            nodeData.ItemKey])
  end else
  if nodeData is TTaxonOccNode then begin
    lTaxonSQL := Format(SQL_TAXON_OCCURRENCES, [
            '', 'TOCC.TAXON_OCCURRENCE_KEY',
            nodeData.ItemKey]);
  end;
  Result := '';
  if (Pos('T', ATypes) >0) and (lTaxonSQL<>'') then
    Result := AddQueryToUnion(Result, lTaxonSQL + SQL_TAXON_RESTRICTION);
  if (Pos('B', ATypes) >0) and (lBiotopeSQL<>'') then
    Result := AddQueryToUnion(Result, lBiotopeSQL + SQL_BIOTOPE_RESTRICTION);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmObservations.pmHQuickReportsClick(Sender: TObject);
begin
  inherited;
  BringToFront; // in case popup activated when not the active mdi child, which can happen
  frmMain.PopulateQuickReportSubMenu(tvObservations.Selected, pmHQuickReports);
end;

//==============================================================================
//==============================================================================
procedure TfrmObservations.InitializeNodeInfo(var NodeInfo: TNodeInfo);
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

//------------------------------------------------------------------------------
function TfrmObservations.SetNodeInfo(ANode: TFlyNode): TNodeInfo;
var lData: TAddinOccNode;

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
  FillChar(Result, SizeOf(TNodeInfo), #0);
  lData := TAddinOccNode(ANode.Data);
  with Result do begin
    Caption           := ANode.Text;
    ImageIndex        := AddinImageIndexFromMapping(IntToStr(ANode.ImageIndex), FNodeImagesMap);
    SelectedIndex     := AddinImageIndexFromMapping(IntToStr(ANode.SelectedIndex), FNodeImagesMap);
    StateIndex        := AddinImageIndexFromMapping(IntToStr(ANode.StateIndex), FNodeStateImagesMap);
    Editable          := True;
    Deletable         := True;
    if Assigned(lData) then begin
      ChildrenPopulated := lData.ChildrenPopulated;
      ItemKey           := lData.ItemKey;
      TypeID            := lData.TypeID;
      TableName         := lData.ItemAdditional;
    end;
  end;
end;  // SetNodeInfo

//------------------------------------------------------------------------------
procedure TfrmObservations.ResetAddinDetailScreen;
begin
  if Assigned(FDetailScreen) then begin
    (FDetailScreen.ControlInterface as IRecorderDetailScreen).Events := nil;
    FDetailScreen.Free;
  end;
  FDetailScreen := nil;
end;  // ResetAddinDetailScreen

{-------------------------------------------------------------------------------
  Remove HTML formatting from addin occurrence nodes.
}
function TfrmObservations.StripFormatting(const AText: string): string;
begin
  Result := StringReplace(AText, '<i>', '', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '</i>', '', [rfReplaceAll, rfIgnoreCase]);
end;

//------------------------------------------------------------------------------
procedure TfrmObservations.PerformAddinNodeChange(const ANode: TFlyNode);
var lNodeInfo: TNodeInfo;
    lDetailScreenGUID: TGUID;

begin
  Caption := ResStr_Occurrence + ': ' + StripFormatting(ANode.Text);
  bbCheckAll.Visible := False;
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
        FDetailScreen.Align  := alClient;
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

  bbEdit.Enabled := bbEdit.Enabled and lNodeInfo.Editable and not TAddinOccNode(ANode.Data).IsSubFolder;
  bbDelete.Enabled := bbDelete.Enabled and lNodeInfo.Deletable and not TAddinOccNode(ANode.Data).IsSubFolder;
end;  // PerformAddinNodeChange

//------------------------------------------------------------------------------
function TfrmObservations.LoadNodeImage(ImageIndex: Integer; SourceImageList: THandle;
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

//------------------------------------------------------------------------------
procedure TfrmObservations.SetupAddinNode(const ATypeID: Integer; AParentNode,
  ANodeToRefresh: TFlyNode; AFields: ADODB_TLB.Fields);
var lNodeInfo: TNodeInfo;
    lNode : TFlyNode;
    i: Integer;
    lPopulated: Boolean;
begin
  InitializeNodeInfo(lNodeInfo);
  // Let addin setup stuff for display.
  // Need to cast Fields for call because ADOInt (used by ADODB) and ADODB_TLB
  // have different GUIDs for Fields interface. But it is the same!!!
  if FNodeMan.GetNodeInfo(ATypeID, AFields, lNodeInfo) then begin
    // Setup new TreeNode properties
    if ANodeToRefresh = nil then begin
      lNode := tvObservations.Items.AddChild(AParentNode, lNodeInfo.Caption);
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
      // Set to local var for additional properties.
      lNode := ANodeToRefresh;
    end else
      // NodeToRefresh doesn't match data from GetNodeData, so exit.
      Exit;

    // Setup additional properties. For Occurrence nodes mainly.
    with TAddinOccNode(lNode.Data) do begin
      if TNodeObject(AParentNode.Data) is TSampleNode then // ParentNode is Sample
        HasLocation := TSampleNode(AParentNode.Data).HasLocation;
      // Expected additional fields for Occurrences
      for i := 0 to AFields.Count - 1 do begin
        if CompareText(AFields[i].Name, 'Checked') = 0 then begin
          Checked := AFields['Checked'].Value;
          if Checked then lNode.StateIndex := STATE_CHECKED
                     else lNode.StateIndex := STATE_UNCHECKED;
        end;
        if CompareText(AFields[i].Name, 'Confidential') = 0 then
          Confidential := AFields['Confidential'].Value;
      end;
    end;
  end;
end;  // SetupAddinNode

//------------------------------------------------------------------------------
procedure TfrmObservations.PopulateAddinNodes(const AParentNode: TFlyNode; const AParentTypeID: Integer);
var i             : Integer;
    lSQL, lSQLDone: WideString;
    lCmd          : TADOCommand;
begin
  if Assigned(FNodeMan) then begin
    lCmd := TADOCommand.Create(nil);
    lCmd.Connection := dmDatabase.dbLocal;

    lSQLDone := '';
    for i := 0 to FNodeMan.TypeCount - 1 do begin
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

//------------------------------------------------------------------------------
procedure TfrmObservations.SetNodeProperties(ANode: TFlyNode; NodeInfo: TNodeInfo);
begin
  // If SelectedIndex not set, assign same value as ImageIndex
  if NodeInfo.SelectedIndex = -1 then NodeInfo.SelectedIndex := NodeInfo.ImageIndex;

  ANode.ImageIndex := LoadNodeImage(NodeInfo.ImageIndex, FNodeMan.ImageList,
                                    ilObservation, FNodeImagesMap);
  ANode.SelectedIndex := LoadNodeImage(NodeInfo.SelectedIndex,FNodeMan.ImageList,
                                    ilObservation, FNodeImagesMap);
  ANode.StateIndex := LoadNodeImage(NodeInfo.StateIndex, FNodeMan.StateImageList,
                                    ilObservationState, FNodeStateImagesMap);

  // Set NodeData properties
  if not Assigned(ANode.Data) then ANode.Data := TAddinOccNode.Create;

  with TAddinOccNode(ANode.Data) do begin
    ChildrenPopulated := NodeInfo.ChildrenPopulated;
    Deletable         := NodeInfo.Deletable;
    Editable          := NodeInfo.Editable;
    ItemKey           := NodeInfo.ItemKey;
    ItemAdditional    := NodeInfo.TableName;
    TypeID            := NodeInfo.TypeID;
  end;

  // Any subnodes
  SetSubNodesProperties(ANode, TAddinOccNode(ANode.Data).TypeID);

  if (ANode.Parent <> nil) and (TNodeObject(ANode.Parent.Data) is TSampleNode) then
    if TAddinOccNode(ANode.Data).Checked then ANode.StateIndex := STATE_CHECKED
                                         else ANode.StateIndex := STATE_UNCHECKED;
end;  // SetNodeProperties

//------------------------------------------------------------------------------
procedure TfrmObservations.SetSubNodesProperties(const AParentNode: TFlyNode;
  const AParentNodeTypeID: Integer);
var lIdx: Integer;
    lNode: TFlyNode;
    lNodeInfo: TNodeInfo;
begin
  if TNodeObject(AParentNode.Data).ChildrenPopulated then begin
    // Just need to refresh some properties. The ItemKey actually.
    lNode := AParentNode.GetFirstChild;
    while Assigned(lNode) do begin
      if TAddinOccNode(lNode.Data).IsSubFolder then
        TAddinOccNode(lNode.Data).ItemKey := TNodeObject(AParentNode.Data).ItemKey;
      lNode := lNode.GetNextSibling;
    end;
  end else begin
    // All sub nodes need to be added.
    lIdx := 0;
    InitializeNodeInfo(lNodeInfo);
    // Loop until function returns False.
    while FNodeMan.GetSubNodeInfo(AParentNodeTypeID, lIdx, lNodeInfo) do begin
      TNodeObject(AParentNode.Data).ChildrenPopulated := True;
      lNode := tvObservations.Items.AddChild(AParentNode, lNodeInfo.Caption);
      SetNodeProperties(lNode, lNodeInfo);
      TAddinOccNode(lNode.Data).IsSubFolder := True;
      // Set the right key for when sub-items get populated, as folders don't have one of their own.
      TAddinOccNode(lNode.Data).ItemKey := TAddinOccNode(AParentNode.Data).ItemKey;

      // Add fake child node if needed.
      tvObservations.Items.AddChild(lNode, '.');

      Inc(lIdx);
    end;
  end;
end;  // SetSubNodesProperties

//------------------------------------------------------------------------------
procedure TfrmObservations.CleanupAddinMenuItems(AMenuItem: TMenuItem);
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

//------------------------------------------------------------------------------
procedure TfrmObservations.UpdateSubMenusForAddin(const ANode: TFlyNode; AMenus:
  Array of TMenuItem);
var lMnuIdx, lParentTypeID, lTypeID, i: Integer;
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

    if TNodeObject(ANode.Data) is TAddinOccNode then
      lParentTypeID := TAddinOccNode(ANode.Data).TypeID;

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

//------------------------------------------------------------------------------
procedure TfrmObservations.AddinMenuClick(Sender: TObject);
var lCaption: String;
begin
  lCaption := RemoveAmpersands(TMenuItem(Sender).Caption);
  // SelectedItem can be a Sample node or non-folder node at any level.
  if Assigned(SelectedItem) then
    AddinNewNode(SelectedItem, lCaption);
end;  // AddinMenuClick

//------------------------------------------------------------------------------
procedure TfrmObservations.AddinSubMenuClick(Sender: TObject);
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
          lNode := tvObservations.Selected.GetFirstChild;
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

//------------------------------------------------------------------------------
// node added below a Sample node
// node added inside a subfolder, if current node is a subfolder.
// node added on the same level as selected node.
procedure TfrmObservations.AddinNewNode(ANode: TFlyNode; const ATypeName: String);
var lNodeInfo : TNodeInfo;
    i, lTypeID: Integer;
    lNode     : TFlyNode;
    lParentKey: WideString;
    lNodeData : TNodeObject;
begin
  FAddingNewNode := True;
  lNodeData := TNodeObject(ANode.Data);

  for i := 0 to FNodeMan.TypeCount - 1 do begin
    // Get proper TypeID
    lTypeID := FNodeMan.TypeID[i];
    // Compare menu caption with type name, should eventually be one match
    if SameText(ATypeName, RemoveAmpersands(FNodeMan.TypeName[lTypeID])) then
    begin
      InitializeNodeInfo(lNodeInfo);

      if (lNodeData is TSampleNode) or TAddinOccNode(lNodeData).IsSubFolder then
        lParentKey := lNodeData.ItemKey
      else
        lParentKey := TNodeObject(ANode.Parent.Data).ItemKey;

      if FNodeMan.AddNode(lTypeID, lParentKey, lNodeInfo) then begin
        if (lNodeData is TSampleNode) or TAddinOccNode(lNodeData).IsSubFolder then begin
          // Check if we need to remove any dummy nodes.
          if not lNodeData.ChildrenPopulated then begin
            ANode.Expand(False);
            lNodeData.ChildrenPopulated := True;
          end;
          lNode := tvObservations.Items.AddChild(ANode, lNodeInfo.Caption);
        end else
          lNode := tvObservations.Items.Add(ANode, lNodeInfo.Caption);

        SetNodeProperties(lNode, lNodeInfo);  // Will add object to node too.
        tvObservations.Selected := lNode;
        bbEditClick(nil);  // Trigger Edit for newly added node.
      end;
      Break;
    end;
  end;
  FAddingNewNode := False;
end;  // AddinNewNode

//------------------------------------------------------------------------------
procedure TfrmObservations.OnEditModeChange;
begin
  SetMenuState(not (FDetailScreen.ControlInterface as IRecorderDetailScreen).Editing);
end;  // OnEditModeChange

//------------------------------------------------------------------------------
procedure TfrmObservations.OnRefreshScreenInfo(const AKey: WideString; const ACaption: WideString);
var lCmd: TADOCommand;

  procedure RefreshNode(AParentNode, ANode: TFlyNode);
  var lParentTypeID, lTypeID: Integer;
      lSQL: WideString;
  begin
    // Get the type of the parent node, -1 for Samples.
    if TNodeObject(AParentNode.Data) is TSampleNode then
      lParentTypeID := -1
    else
      lParentTypeID := TAddinOccNode(AParentNode.Data).TypeID;

    // Store node type for easier use.
    lTypeID := TAddinOccNode(ANode.Data).TypeID;
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
    SendMessage(Self.Handle, WM_DISCARD_OBSERVATION, 0, 0);
    Exit;
  end else
    // Update ItemKey in Data object. Or refresh won't work properly.
    TNodeObject(SelectedItem.Data).ItemKey := AKey;

  lCmd := TADOCommand.Create(nil);
  try
    lCmd.Connection := dmDatabase.dbLocal;
    // Need to refresh nodes right up to Sample level, in case one child node
    // has a side-effect affecting the display of its parent.
    // For instance, Occurrences have the Checked flag, so we have to get it
    // from the database, but that is provided through the SQL returned by addin.
    RefreshNode(SelectedItem.Parent, SelectedItem);
  finally
    lCmd.Free;
  end;
end;  // OnRefreshScreenInfo

{-------------------------------------------------------------------------------
}
procedure TfrmObservations.UpdateMapWindowSelector;
var
  lEmpty, lSurvey: Boolean;
begin
  with tvObservations do begin
    lEmpty := Items.Count = 0;
    lSurvey := False;
    if Assigned(Selected) then
      lSurvey := TNodeObject(Selected.Data) is TSurveyNode
  end;

  actFindOnMap.Enabled := not (lSurvey or lEmpty) and
                          (AppSettings.AvailableMaps.Count > 0);
  if Assigned(DetailForm) then
    DetailForm.UpdateMapWindowSelector;
end;  // UpdateMapWindowSelector

{-------------------------------------------------------------------------------
}
procedure TfrmObservations.DragOverCheck(APoint: TPoint; const ATable, AFieldKey: String;
  var Accept: Boolean);
var
  lPos: TPoint;
  lHitTest: THitTests;
  lNode: TFlyNode;
begin
  inherited;
  if SameText(ATable, TN_SURVEY) or SameText(ATable, TN_SURVEY_EVENT) or SameText(ATable, TN_SAMPLE)
     or SameText(ATable, TN_BIOTOPE_OCCURRENCE) or SameText(ATable, TN_TAXON_OCCURRENCE) then
    with tvObservations do begin
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
      if SameText(ATable, TN_SURVEY) then
        Accept := Assigned(lNode) and
                  Assigned(lNode.Data) and (TNodeObject(lNode.Data) is TSurveyTagNode)
      else
        Accept := Assigned(lNode)and
                  Assigned(lNode.Data) and not (TNodeObject(lNode.Data) is TSurveyTagNode);
    end
  else
    Accept := False;
end;  // DragOverCheck

{-------------------------------------------------------------------------------
}
procedure TfrmObservations.tvObservationsExit(Sender: TObject);
begin
  inherited;
  dmFormActions.actCut.Enabled := False;
end;  // tvObservationsExit

{-------------------------------------------------------------------------------
  Return the current item's details TPageControl
}
function TfrmObservations.GetDetailsPageControl: TPageControl;
begin
  if Assigned(FDetailForm) then begin
    if FDetailForm is TfrmSurveyDetails then
      Result := TfrmSurveyDetails(FDetailForm).pcSurveyDetails
    else if FDetailForm is TfrmEventDetails then
      Result := TfrmEventDetails(FDetailForm).pcEventDetails
    else if FDetailForm is TfrmSampleDetails then
      Result := TfrmSampleDetails(FDetailForm).pcSampleDetails
    else if FDetailForm is TfrmTaxonOccurrences then
      Result := TfrmTaxonOccurrences(FDetailForm).pcTaxonOccurrence
    else if FDetailForm is TfrmBiotopeOccurrences then
      Result := TfrmBiotopeOccurrences(FDetailForm).pcBiotopeOccurrence
    else
      Result := nil;
  end
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------
  Select a single specific node
}
function TfrmObservations.SelectNode(const ANodeType, ANodeKey: string): TFlyNode;
var
  lOldEvent: TFTVChangedEvent;
  lNodeType: string;
begin
  // Unhook the OnChange method so unnecessary details don't appear
  lOldEvent := tvObservations.OnChange;
  tvObservations.OnChange := nil;
  tvObservations.Items.BeginUpdate;
  FSelectedItem := nil;
  lNodeType := StringReplace(ANodeType, '_', ' ', []);
  try
    if (SameText(lNodeType, 'Survey Tag') or SameText(lNodeType, 'Concept')) and
       AppSettings.OrganiseSurveysByTag then
      SelectSurveyTag(ANodeKey)
    else
    if SameText(lNodeType, 'Survey') then
      SelectSurvey(ANodeKey)
    else
    if SameText(lNodeType, 'Event') or SameText(lNodeType, 'Survey Event') then
      SelectEvent(ANodeKey)
    else
    if SameText(lNodeType, 'Sample') then
      SelectSample(ANodeKey)
    else
    if SameText(lNodeType, 'Taxon Occurrence') then
      SelectTaxOcc(ANodeKey)
    else
    if SameText(lNodeType, 'Biotope Occurrence') then
      SelectBioOcc(ANodeKey);
  finally
    Result := tvObservations.Selected;
    tvObservations.Items.EndUpdate;
    tvObservations.OnChange := lOldEvent;
    PerformNodeChange(tvObservations.Selected);
  end;
end;

{-------------------------------------------------------------------------------
  Select a single survey tag at the top level
}
function TfrmObservations.SelectSurveyTag(const ANodeKey: String): Boolean;
var
  i: Integer;
  node: TFlyNode;
  nodeData: TNodeObject;
begin
  Result := False;
  for i := 0 to tvObservations.Items.Count - 1 do begin
    node := tvObservations.Items[i];
    nodeData := TNodeObject(node.Data);
    if (nodeData is TSurveyTagNode) and (nodeData.ItemKey = ANodeKey) then begin
      tvObservations.Selected := node;
      Result := True;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Select a single survey at the top level
}
function TfrmObservations.SelectSurvey(const ANodeKey: String): Boolean;
var
  i, j: Integer;
  tagKeys: TStringList;
  node, subNode: TFlyNode;
  nodeData: TNodeObject;
begin
  Result := False;
  // Grab tag keys for survey.
  tagKeys := TStringList.Create;
  try
    if AppSettings.OrganiseSurveysByTag then begin
      // We want the preferred keys, not the synonym ones.
      with dmDatabase.GetRecordset('usp_SurveyTags_Select_ForSurvey', ['@Key', ANodeKey]) do begin
        while not Eof do begin
          tagKeys.Add(Fields['Preferred_Concept_Key'].Value);
          MoveNext;
        end;
        Close;
      end;
    end;

    for i := 0 to tvObservations.Items.Count - 1 do begin
      node := tvObservations.Items[i];
      nodeData := TNodeObject(node.Data);
      if (nodeData is TSurveyNode) and (nodeData.ItemKey = ANodeKey) then
      begin
        tvObservations.Selected := node;
        Result := True;
        Break;
      end else
      if (nodeData is TSurveyTagNode) and (tagKeys.IndexOf(nodeData.ItemKey) > -1) then begin
        node.Expand(False);
        for j := 0 to node.Count - 1 do begin
          subNode := node.Item[j];
          if (TNodeObject(subNode.Data) is TSurveyNode) and
             (TNodeObject(subNode.Data).ItemKey = ANodeKey) then
          begin
            tvObservations.Selected := subNode;
            FSelectedItem := subNode;
            SetMenuState(FEditMode = emView);
            Result := True;
            Exit;  // 2 loops to break from. Use Exit.
          end;
        end;
      end;
    end;
  finally
    tagKeys.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Select a single event
}
function TfrmObservations.SelectEvent(const ANodeKey: string): boolean;
var
  lParentKey: string;
  i: integer;
begin
  Result := False;
  with dmDatabase.ExecuteSQL('SELECT Survey_Key FROM Survey_Event '+
      'WHERE Survey_Event_Key='''+ANodeKey+'''', True) do
    if not (BOF and EOF) then begin
      lParentKey := VarToStr(Fields['Survey_Key'].Value);
      if SelectSurvey(lParentKey) then begin
        tvObservations.Selected.Expand(False);
        for i := 0 to tvObservations.Selected.Count do begin
          if TObservationNode(tvObservations.Selected.Item[i].Data).ItemKey=ANodeKey then begin
            tvObservations.Selected := tvObservations.Selected.Item[i];
            Result := True;
            break;
          end;
        end; // for
      end; // if
    end;
end;

{-------------------------------------------------------------------------------
  Select a single sample
}
function TfrmObservations.SelectSample(const ANodeKey: string): boolean;
var
  lParentKey: string;
  i: integer;
begin
  Result := False;
  with dmDatabase.ExecuteSQL('SELECT Survey_Event_Key FROM Sample '+
      'WHERE Sample_Key='''+ANodeKey+'''', True) do
    if not (BOF and EOF) then begin
      lParentKey := VarToStr(Fields['Survey_Event_Key'].Value);
      if SelectEvent(lParentKey) then  begin
        tvObservations.Selected.Expand(False);
        for i := 0 to tvObservations.Selected.Count do begin
          if TObservationNode(tvObservations.Selected.Item[i].Data).ItemKey=ANodeKey then begin
            tvObservations.Selected := tvObservations.Selected.Item[i];
            Result := True;
            break;
          end;
        end; // for
      end; // if
    end;
end;

{-------------------------------------------------------------------------------
  Select a single taxon occurrence
}
function TfrmObservations.SelectTaxOcc(const ANodeKey: string): boolean;
var
  lParentKey: string;
  i: integer;
begin
  Result := False;
  with dmDatabase.ExecuteSQL('SELECT Sample_Key FROM Taxon_Occurrence '+
      'WHERE Taxon_Occurrence_Key='''+ANodeKey+'''', True) do
    if not (BOF and EOF) then begin
      lParentKey := VarToStr(Fields['Sample_Key'].Value);
      if SelectSample(lParentKey) then begin
        tvObservations.Selected.Expand(False);
        for i := 0 to tvObservations.Selected.Count do begin
          if (TObservationNode(tvObservations.Selected.Item[i].Data).ItemKey=ANodeKey)
              and (TObject(tvObservations.Selected.Item[i].Data) is TTaxonOccNode) then
          begin
            tvObservations.Selected := tvObservations.Selected.Item[i];
            Result := True;
            break;
          end;
        end; // for
      end; // if
    end;
end;

{-------------------------------------------------------------------------------
  Select a single biotope occurrence
}
function TfrmObservations.SelectBioOcc(const ANodeKey: string): boolean;
var
  lParentKey: string;
  i: integer;
begin
  Result := False;
  with dmDatabase.ExecuteSQL('SELECT Sample_Key FROM Biotope_Occurrence '+
      'WHERE Biotope_Occurrence_Key='''+ANodeKey+'''', True) do
    if not (BOF and EOF) then begin
      lParentKey := VarToStr(Fields['Sample_Key'].Value);
      if SelectSample(lParentKey) then begin
        tvObservations.Selected.Expand(False);
        for i := 0 to tvObservations.Selected.Count do begin
          if (TObservationNode(tvObservations.Selected.Item[i].Data).ItemKey=ANodeKey)
              and (TObject(tvObservations.Selected.Item[i].Data) is TBiotopeOccNode) then
          begin
            tvObservations.Selected := tvObservations.Selected.Item[i];
            Result := True;
            break;
          end;
        end;
      end;
    end;
end;

{-------------------------------------------------------------------------------
  Retrieve current node type for custom reports
}
function TfrmObservations.CustomReportKeyType: TKeyType;
begin
  Result := inherited CustomReportKeyType;
  with tvObservations do
    if Assigned(Selected) then
      if TObject(Selected.Data) is TReportableNode then
        Result := TReportableNode(Selected.Data).ReportKeyType;
end;

{-------------------------------------------------------------------------------
  Retrieve current node key for custom reports
}
function TfrmObservations.ItemKey: string;
begin
  Result := inherited ItemKey;
  with tvObservations do
    if Assigned(Selected) then
      if TObject(Selected.Data) is TReportableNode then
        Result := TReportableNode(Selected.Data).ItemKey
end;

{-------------------------------------------------------------------------------
}
procedure TfrmObservations.DoShowHint(var HintStr: String; var CanShow: Boolean; var HintInfo:
    THintInfo);
var
  lCol, lRow, iIndex: Integer;
  lItemPoint: TPoint;
  lRect: TRect;
  lCellText: String;
  lNode: TFlyNode;
begin
  if (HintInfo.HintControl is TStringGrid) then
    with TStringGrid(HintInfo.HintControl) do begin
      if Hint = '' then begin
        MouseToCell(HintInfo.CursorPos.X, HintInfo.CursorPos.Y, lCol, lRow);
        if (lRow <> -1) and (lCol <> -1) then begin
          // Find out if the text shown is shorter than the text stored.
          lRect := CellRect(lCol, lRow);
          lCellText := GetTextWithinLimit(Canvas, Cells[lCol, lRow],
                                          lRect.Right - lRect.Left - 4);
          //  in the sample form for Related Samples the Grid has graphics
          if HintInfo.HintControl.name = 'sgRelatedSamples' then
            //  We may have a graphic somewhere in this cell
            if lCol = 2 then
              lCellText := GetTextWithinLimit
                (Canvas, Cells[lCol, lRow],
                  (
                  (lRect.Right - lRect.Left) -
                  //  Use image width and 2 for border on image
                  (dmFormActions.ilSampleTypes.width + 2)
                  )
                  //  use A for border of blank at edges of cell
                  - 6
                );
          if lCellText <> Cells[lCol, lRow] then begin
            HintStr := Cells[lCol, lRow];
            HintInfo.ReshowTimeout := 200;
          end else
            HintStr := '';
        end;
      end;
    end
  else
  // Display truncated list box items as hints
  if HintInfo.HintControl is TListBox then
    with TListBox(HintInfo.HintControl) do
    begin
      lItemPoint.X:=(Mouse.CursorPos.X)-(ClientOrigin.X);
      lItemPoint.Y:=(Mouse.CursorPos.Y)-(ClientOrigin.Y);
      iIndex:=ItemAtPos(lItemPoint,True);
      if iIndex<>-1 then begin
        if Canvas.TextWidth(Items.Strings[iIndex]) > (ClientRect.Right - ClientRect.Left) then
        begin
          HintInfo.ReShowTimeout := 200;
          HintStr := Items.Strings[iIndex];
          CanShow := True;
        end else
          CanShow := False; // not necessary because item fits in box
      end;
    end
  else
  if HintInfo.HintControl = tvObservations then begin
    lItemPoint.X := Mouse.CursorPos.X - ClientOrigin.X - tvObservations.Left;
    lItemPoint.Y := Mouse.CursorPos.Y - ClientOrigin.Y - tvObservations.Top;
    lNode := TRapidTree(HintInfo.HintControl).GetNodeAt(lItemPoint.X, lItemPoint.Y);
    if lNode <> nil then
      HintStr := TNodeObject(lNode.Data).Hint;
    CanShow := HintStr <> '';
    if CanShow then
      HintInfo.ReshowTimeout := 200;
  end else
    inherited;
end;  // FormShowHint

{-------------------------------------------------------------------------------
}
procedure TfrmObservations.PopulateAllSamples(AEventNode: TFlyNode);
begin
  PopulateSampleLevel(AEventNode);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmObservations.PopulateAllEvents(ASurveyNode: TFlyNode);
begin
  PopulateEventLevel(ASurveyNode);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmObservations.pmHBatchUpdateClick(Sender: TObject);
begin
  inherited;
  BringToFront; // in case popup activated when not the active mdi child, which can happen
  frmMain.PopulateBatchUpdateSubMenu(tvObservations.Selected, pmHBatchUpdate);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmObservations.ApplySecurity;
begin
  UpdateSubMenus;
  SetMenuState(True);
  pmHAdd.Enabled:= (AppSettings.UserAccessLevel >= ualAddOnly);
  pmHBatchUpdate.Visible := AppSettings.UserAccessLevel >= ualAdmin;
end;

{-------------------------------------------------------------------------------
  Performs the actual deletion of a node once the other checks have been done
}
procedure TfrmObservations.DoUncheckedDelete;
begin
  DeleteItem(SelectedItem);
  DeleteAdditionalPages;
  // Update the state of all menus/sub menus/buttons
  UpdateSubMenus;
  SetMenuState(True);
end;

{-------------------------------------------------------------------------------
  Message handler which allows us to delay a node deletion event
}
procedure TfrmObservations.WMUncheckedDeleteNode(var Message: TMessage);
begin
  DoUncheckedDelete;
end;

{-------------------------------------------------------------------------------
  Message handler to respond to changes to recording cards.
}
procedure TfrmObservations.WMUpdateSampleLoadCardMenu(var Message: TMessage);
begin
  UpdateSampleLoadCardMenu;
end;

{-------------------------------------------------------------------------------
  Refreshes the submenu for loading samples into recording cards.
}
procedure TfrmObservations.UpdateSampleLoadCardMenu;
var
  i: Integer;
  newItem, cardItem: TMenuItem;
begin
  pmHLoadIntoRecordCard.Clear;

  for i := 0 to frmMain.mnuDataEntrySpecies.Count - 3 do begin
    cardItem := frmMain.mnuDataEntrySpecies.Items[i];

    newItem            := TMenuItem.Create(pmHierarchy);
    newItem.Caption    := cardItem.Caption;
    newItem.ImageIndex := cardItem.ImageIndex;
    newItem.Enabled    := cardItem.Enabled;
    newItem.OnClick    := LoadSampleIntoCard;

    pmHLoadIntoRecordCard.Add(newItem);
  end;

  // If no cards, add a disabled "None" item.
  if pmHLoadIntoRecordCard.Count = 0 then begin
    newItem            := TMenuItem.Create(pmHierarchy);
    newItem.Caption    := ResStr_None;
    newItem.Enabled    := False;
    pmHLoadIntoRecordCard.Add(newItem);
  end;

  RefreshXPMenu;
end;

{-------------------------------------------------------------------------------
  Refreshes the submenu for loading samples into recording cards.
}
procedure TfrmObservations.LoadSampleIntoCard(Sender: TObject);
begin
  // Get main form to do most of the work to get the card up.
  frmMain.mnuRecordingCardClick(Sender);
  // force a refresh on the node children
  KillChildren(SelectedItem);
  tvObservations.Items.AddChild(SelectedItem, '-');
  // Now load the sample into it.
  TfrmPlaceCard(frmMain.ActiveMDIChild).LoadFromSample(
      TNodeObject(SelectedItem.Parent.Parent.Data).ItemKey,  // Survey key
      TNodeObject(SelectedItem.Parent.Data).ItemKey,         // Event key
      TNodeObject(SelectedItem.Data).ItemKey,                // Sample key
      SelectedItem.Text);
end;

{-------------------------------------------------------------------------------
  Adds menu items into the pmHierarchy popup menu (if any) as defined by
  any installed addins that implement INodeMenuManager.
}
procedure TfrmObservations.MergeAddinMenusIntoHierarchyPopup;
var
  i, j: integer;
  lSelectedNodeInfo: TNodeInfo;
  lDynamicMenuList: IDynamicMenuList;
begin
  if FSelectedItem <> nil then
  begin
    lSelectedNodeInfo := SetNodeInfo(FSelectedItem);

    FDynamicMenuLists.Clear;

    //Get the IDynamicMenuLists defined by each INodeMenuManager
    for i := 0 to Appsettings.ComAddins.NodeMenuManagers.Count - 1 do
    begin
      FDynamicMenuLists.Add(
          (Appsettings.ComAddins.NodeMenuManagers[i] as INodeMenuManager)
              .GetMenuItemsForNode(lSelectedNodeInfo));
    end;

    // Process each of the IDynamicMenuLists to merge the new menu items into
    // the popup.
    for i := 0 to FDynamicMenuLists.Count - 1 do
    begin
      lDynamicMenuList := (FDynamicMenuLists.Items[i] as IDynamicMenuList);
      if lDynamicMenuList <> nil then
      begin
        for j:= 0 to lDynamicMenuList.Count - 1 do
        begin
          Appsettings.ComAddins.ProcessTopLevelMenu(
              lDynamicMenuList,
              j,
              nil,
              pmHierarchy.Items);
        end;
      end;
    end;
  end;
end;

procedure TfrmObservations.actSortTaxonSortOrderExecute(Sender: TObject);
begin
  inherited;
  FTaxonOccField := SORT_TAXON_SORT_ORDER;
  with FdmObservation.qryTaxonOcc do
    SQL[SQL.Count-1]:='ORDER BY ITN.Sort_Order';
  UpdateOrder(tvObservations.Selected);
end;

{Adds the Recorders from the old event to the new event}
Procedure TfrmObservations.AddRecordersToNewEvent(const ASampleKey, AnEventKey: TKeyString);
var  AStringList:TStringList;
     k :integer;
begin
  AStringList := TStringList.Create;
  try
    with dmGeneralData.qryAllPurpose do
    try
      //Get the recorders Name Keys
      SQL.Text :=
         'SELECT SER.Name_Key ' +
         'FROM Sample_Recorder SR INNER JOIN Survey_Event_Recorder SER ' +
         '     ON SR.SE_Recorder_Key = SER.SE_Recorder_Key ' +
         'WHERE Sample_Key = ' + QuotedStr(ASampleKey);
      Open;
      while not Eof do begin
        AStringList.Add(FieldByName('Name_Key').AsString);
        Next;
      end;
      Close;

      for k := AStringList.Count - 1 downto 0 do
        dmDatabase.RunStoredProc('usp_SER_Sample_Recorder_Insert',
        ['@SampleKey', ASampleKey, '@NameKey', AStringList.strings[k], '@SEKey', AnEventKey]);

    finally
      Close;
    end;
  finally
    AStringList.Free;
  end;
end;


end.

