//==============================================================================
//  Unit:        ApplicationSettings
//
//  Implements:  TApplilcationSettings
//               TAutoApplicationSettings
//               TNewAutoObjectFactory
//               TRucksack
//
//  Description:
//
//  Author:      Paul Thomas
//  Created:     4 November 1999
//
//  Last Revision Details:
//    $Revision: 263 $
//    $Date: 21/03/13 15:58 $
//    $Author: Michaelcaptain $
//
//==============================================================================

{$I '..\..\Third Party\Dorset Software Services\DssVcl32\DelphiVersions.Inc'}

unit ApplicationSettings;

interface

uses
  Windows, ActiveX, ComObj, SysUtils, Classes, Registry, Forms, Graphics,
  ExtCtrls, JPeg, ComCtrls, ActnList, DataClasses, Menus, Recorder2000_TLB,
  COMClasses, ComAddinUnit, StdCtrls, JNCCDatasets, DropTarget, APIUtils,
  ComServ, Dialogs, SpatialRefFuncs, FileCtrl, Constants, ExceptionForm,
  Html2RTF, VagueDate, BaseFormUnit, VagueDateLatLong, XPToolButton, StdVcl,
  SQLConverter, Math, Contnrs, MapClasses, CRReportIndex, ResourceStrings,
  XmlReportList, ADODB, CRConstants, StrUtils, LastSearchedRucksack, SHFolder;

const
  // COM object class Ids
  Class_COMApplicationSettings: TGUID = '{B09F3231-17FC-11D3-B6E3-0060979160B0}';

  { Folder inside temp directory to store import dbs }
  TEMP_DB_FOLDER = 'NBN Import\';

resourcestring
  ResStr_NoMapAvailableToRequestData = 'Please initialise a map before requesting a spatial reference.';
  ResStr_DisplayDataTooManyFields =
      'The SQL supplied to the DisplayDataFromSQL and '
      + 'DisplayTab methods must have a single field in the results set.';
  ResStr_DisplayTabNotFound = 'Tab %s was requested in call to DisplayTab but could not be found.';
  ResStr_NoItemRecord       = 'Parameter AItemSQL returned no records in call to DisplayTab.';
  ResStr_NoListRecords      = 'SQL returned no records in call to DisplayTab or DisplayDataFromSQL.';
  ResStr_ItemNotFound       = 'Item identified by parameter AItemSQL not available in the list.';
  ResStr_InvalidDisplayDataKeyField =
      'An addin requested data to be displayed using SQL '
      + 'but provided an incorrect field for the data type.  The field provided was ''%s'' '
      + 'but the system expected ''%s''';

  ResStr_RegistryNotFound =
      'Local Machine Registry Settings not found. %s is not installed correctly on this machine.';
  ResStr_RegKeySettingIncorrect =
      'The %s setting is incorrect as expected files are missing (%s).  Please correct.';
  ResStr_Locate = 'Locate ';
  ResStr_UnrecognisedRegFile = 'GetRegFile called with unrecognised create rule';
  ResStr_RegKeySettingMissingOrIncorrect =
      'The %s setting is missing or incorrect. Recorder is not installed properly and cannot start.';
  ResStr_ChooseLocalDataFolder =
      'Please select the folder you would like to use to store local data in';
  ResStr_LocalDataFolder = 'Application cannot start without a local data folder';
  ResStr_DialogSearchNotRecognised = 'Find dialog search method not recognised';
  ResStr_AddinClosedConnection =
      'Addin requested SetApplicationSecurity on a closed connection.  Connection must be opened first.';
  ResStr_CannotFindHelpFile = 'Help file cannot be found.';
  ResStr_MissingFile = 'The file %s is missing. Recorder does not appear to be correctly installed and cannot start.';
  ResStr_CantCopyBaseMaps = 'There are updates to the base map files available. Recorder cannot copy them to your '+
      'local disk because you do not have permissions. Run Recorder as Administrator (by right clicking on the icon '+
      'to allow the files to be updated. Or you can delete all the files in the local base maps folder %s '+
      'then restart Recorder as a normal user to prevent this happening again in future.';

type
  EApplicationSettingsError = class(TExceptionPath);

  //----------------------------------------------------------------------------
  TApplicationSettings = class;

  //----------------------------------------------------------------------------
  TFilePathType = (fptInstallFolder, fptNetworkData, fptNetworkDocs, fptLocalData, fptLocalDocs);

  //----------------------------------------------------------------------------
  TRucksack = class
  private
    FFileName: String;
    FAppSettings: TApplicationSettings;

    FTaxonList: TEditableKeyList;
    FBiotopeList: TEditableKeyList;
    FLocationList: TEditableKeyList;
    FNameList: TEditableKeyList;
    FDocumentList: TEditableKeyList;
    FTaxonSearchCodeList: TEditableKeyList;
  protected
    function Get_TaxonList: TEditableKeyList;
    function Get_BiotopeList: TEditableKeyList;
    function Get_LocationList: TEditableKeyList;
    function Get_NameList: TEditableKeyList;
    function Get_DocumentList: TEditableKeyList;
    function Get_TaxonSearchCodeList: TEditableKeyList;

    procedure Set_TaxonList(const iTaxonList: TEditableKeyList);
    procedure Set_BiotopeList(const iBiotopeList: TEditableKeyList);
    procedure Set_LocationList(const iLocationList: TEditableKeyList);
    procedure Set_NameList(const iNameList: TEditableKeyList);
    procedure Set_DocumentList(const iDocumentList: TEditableKeyList);
    procedure Set_TaxonSearchCodeList(const Value: TEditableKeyList);

    procedure Set_AppSettings(const iAppSettings: TApplicationSettings);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(iRucksack: TRucksack);
    procedure UpdateAppSettings;

    property FileName: String read FFileName write FFileName;
    property AppSettingsObject: TApplicationSettings write Set_AppSettings;

    property TaxonList: TEditableKeyList read Get_TaxonList write Set_TaxonList;
    property BiotopeList: TEditableKeyList read Get_BiotopeList write Set_BiotopeList;
    property LocationList: TEditableKeyList read Get_LocationList write Set_LocationList;
    property NameList: TEditableKeyList read Get_NameList write Set_NameList;
    property DocumentList: TEditableKeyList read Get_DocumentList write Set_DocumentList;
    property TaxonSearchCodeList: TEditableKeyList read Get_TaxonSearchCodeList write Set_TaxonSearchCodeList;
  end;

  //----------------------------------------------------------------------------
  TNewAutoObjectFactory = class(TAutoObjectFactory)
  private
    FComObject: TComObject;
  public
    function CreateComObject(const Controller: IUnknown): TComObject; override;
  end;

  //----------------------------------------------------------------------------
  // rules for how missing registry paths are handled
  TPathCreateRules = (pcrNoCreate, pcrCreateAny, pcrIgnore);

  // additional columns that may be displayed when searching for locations
  TLocationSearchColumn = (lscLocationType, lscSpatialReference, lscFileCode);
  TLocationSearchColumns = set of TLocationSearchColumn;

  TApplicationSettings = class
  private
    FMainWindowState: TWindowState;
    FMainWindowPos: TRect;
    FShowContextToolbar: Boolean;
    FShowMenuIcons: Boolean;
    FGraduatedMenus: Boolean;
    FShowMainToolbar: Boolean;
    FShowWelcomeAtStart: Boolean;
    FLastSessionWindows: Boolean;
    FDocsToOccurrence: Boolean;
    FDateCutYear:Integer;
    FIgnoreRememberedMatches: Boolean;
    FDragDestColour: TColor;
    FMandatoryColour: TColor;
    FDragSourceColour: TColor;
    FDisableDragDropFrames: Boolean;
    FFileImportTypeIndex: integer;
    FPlainBackground: Boolean;
    FBackBitmapName: String;
    FBackImage: TPicture;
    FSpatialRefSystem: String;
    FDTDPath: String;  // directory containing local XML DTDs
    FErrorPath: string;
    FShowToolTips: Boolean;
    FUserID: TKeyString;
    FSiteID: String;
    FDictionaryVersion: String;
    FDatabaseVersion: String;
    FRecordingCards: TPopupMenu;
    FRecordingCardPath: String;
    FCustomSpeciesCardPath: string;
    FRucksackPath: String;
    FAddinPath: String;
    FComAddins: TComAddins;
    FCutOffDate: TDateTime;
    FMapFilePath: String;
    FReportTemplatePath: String;
    FExportTemplatePath: String;
    FReportPath: String;
    FBaseMapPath: String;
    FLocalImagesPath: String;
    FHelpPath: String;
    FPolygonFilterPath: String;
    FBatchUpdatePath: String;
    FExternalFilePath: String;
    FSnapshotPath: String;
    FRucksack: TRucksack;
    FUserAccessLevel: TUserAccessLevel;
    FMaximizedChildWindows: Boolean;
    FSearchForTaxonBy: TTaxonSearchType;
    FSearchForReferenceBy: TReferenceSearchType;

    FTaxonListKey: TKeyString;
    FBiotopeListKey: TKeyString;
    FAdminAreaListKey: TKeyString;
    FResultList: TEditableKeyList;
    FObjectSheetFilePath: String;
    FDictImagesPath: String;
    FPlaceCardToLoad: String;
    FConvertor: THTMLRTFConversion;

    FTaxonViewerVisible: Boolean;
    FBiotopeViewerVisible: Boolean;
    FAdminAreaViewerVisible: Boolean;
    FCanExportSystemSupplied: Boolean;
    FMapImageFilePath: String;
    FDisplayCommonNames: Boolean;
    FDisplayEnteredNames: Boolean;
    FDisplayAuthors: Boolean;
    FAutoSchemeEmail: Boolean;
    FExpandTaxaInWizard: Boolean;
    FUseRecommendedTaxaNames: Boolean;
    FTaxonomicSearchRestriction: String;
    FSessionTaxonomicSearchRestriction: String;
    FWorkstationInstallFormStart: TDateTime;

    FServerName: String;       // Name of SQL Server
    FDatabaseName: String;     // Name of database on server (NBNData?)
    FTrustedSecurity: Boolean;
    FtfMapSingleDataset: Boolean;// NT Authentication mode?

    FAvailableMaps: TAvailableMaps;
    FExportConfidentialOcc: Boolean;
    FUseOriginalIcons: Boolean;
    FImportTemplatePath: String;
    FCRReportIndex: TCRReportIndex;
    FBatchUpdateIndex: TBUReportIndex;
    FRememberFilters: Boolean;
    FConfidentialAccessLevel: TUserAccessLevel;
    FConfidentialFullEdit: Boolean;
    FLocalDataPath: string;
    FStandalone: boolean;
    FRestrictFullEdit: Boolean;
    FSessionID: string;
    FSessionStart: TDateTime;
    FReferencesSearchKeywords: boolean;
    FPartialTaxonSearch: boolean;
    FAutoCompleteSearch: boolean;
    FUsePreferredTaxa :boolean;
    FIncludeLocationSpatialRef: boolean;
    FIncludeLocationFileCode: boolean;
    FRapidEntryDelimiter: String;
    FExternalFilters: TStringList;
    FHasUncommittedUpdates: Boolean;
    FBatchUpdateConnection: TADOConnection;
    FLastSearchedRucksack: TLastSearchedRucksack;
    FLoginUserAccessLevel: TUserAccessLevel;
    FOrganiseSurveysByTag: Boolean;
    FUseOldImportWizard: Boolean;
    FUserName: string;
    FGridRefsAsSquares: Boolean;
    FExtraLocationSearchColumns: TLocationSearchColumns;
    FDictionaryUpgradepath: string;

    FIWSurveyKey    : TKeyString;
    FIWIsTempSurvey : Boolean;
    FIWTempNameKey  : String;

    procedure ApplySecurityChange;
    procedure ReadRegistrySettings;
    procedure SetDragDestColour(const Value: TColor);
    procedure SetDragSourceColour(const Value: TColor);
    procedure SetMandatoryColour(const Value: TColor);
    procedure SetBackBitmapName(const Value: String);
    procedure SetMenuIcons(const Value: Boolean);
    procedure SetDragColoursEverywhere;
    procedure CleanupOldImportDatabases;

    { procedures for dealing with COM objects }
//    procedure CreateObjectsToTest;
    procedure SetRecordingCards(const Value: TPopupMenu);
    procedure SetRecordingCardPath(const Value: String);
    procedure SetCustomSpeciesCardPath(const Value: String);
    procedure SetRucksackPath(const Value: String);
    procedure SetMapFilePath(const Value: String);

    procedure SetSpatialRefSystem(const Value: String);
    procedure SetGridRefsAsSquares(const Value: Boolean);
    procedure SetReportPath(const Value: String);
    procedure SetBatchUpdatePath(const Value: String);
    procedure SetExternalFilePath(const Value: String);
    procedure SetReportTemplatePath(const Value: String);
    procedure SetExportTemplatePath(const Value: String);
    function GetMapDatasetPresent: Boolean;
    procedure SetUserID(const AID:TKeyString);
    procedure SetObjectSheetFilePath(const Value: String);
    procedure SetDisableDragDropFrames(const Value: Boolean);
    procedure SetShowToolTips(const Value: Boolean);
    procedure SetDateCutYear(const Value:Integer);
    function GetConvertor: THTMLRTFConversion;
    function GetSiteID: String;
    function GetDictionaryVersion: String;
    function GetDatabaseVersion: String;
    procedure LoadSiteIDFromIni;
    procedure SetCanExportSystemSupplied(const Value: Boolean);
    procedure SetDisplayCommonNames(const Value: Boolean);
    procedure SetDisplayEnteredNames(const Value: Boolean);
    procedure SetDisplayAuthors(const Value: Boolean);
    procedure SetExpandTaxaInWizard(const Value: Boolean);
    procedure SetUseRecommendedTaxaNames(const Value: Boolean);
    procedure SetTaxonomicSearchRestriction(const Value: String);
    function FindRegPath(const iRegKey, iSuggestedFolder: string;
        iReg: TRegistry; pathType: TFilePathType; filesToCheck: array of string;
        optional: boolean=false; canCreate: boolean=false): string;
    procedure CopyBaseMaps(iReg: TRegistry);
    function ReadStringDefault(iReg: TRegistry;
      const iValueName, iDefault: String): String;
    function ReadBoolDefault(iReg: TRegistry; const iValueName: String;
      iDefault: Boolean): Boolean;
    function ReadColourDefault(iReg: TRegistry; const iValueName: String;
      iDefault: TColor): TColor;
    procedure ReadExtraLocationSearchColumns;
    procedure WriteExtraLocationSearchColumns;
    procedure ShowIfWorkstationInstall(iReg: TRegistry);
    procedure FreeWorkstationInstallForm;
    procedure ShowInstallProgressForm;
    function CopyFiles(iFileName, iPath: String): boolean;
    procedure SetGraduatedMenus(const Value: Boolean);
    function GetComputerID: String;
    procedure SetAutoSchemeEmail(const Value: Boolean);
    procedure SetSnapshotPath(const Value: String);
    procedure SetUserAccessLevel(const Value: TUserAccessLevel);
    procedure SetMapSingleDataset(const Value: Boolean);
    procedure ReadServerSettingsFromFile;
    procedure CheckAccessDatasePath(AReg: TRegistry);
    procedure SetFileImportTypeIndex(const Value: integer);
    function GetExportConfidentialOccurrences: Boolean;
    function GetUseOriginalIcons: Boolean;
    procedure SetSessionID(const Value: string);
    procedure CloseSession;
    procedure OpenSession;
    procedure SetReferencesSearchKeywords(const Value: boolean);
    function GetTaxonListKey: TKeyString;
    procedure SetTaxonListKey(const Value: TKeyString);
    procedure SetPartialTaxonSearch(const Value: boolean);
    procedure SetAutoCompleteSearch(const Value: boolean);
    procedure SetUsePreferredTaxa(const Value: boolean);
    procedure SetIncludeLocationFileCode(const Value: boolean);
    procedure SetIncludeLocationSpatialRef(const Value: boolean);
    procedure SetRapidEntryDelimiter(const Value: String);
    procedure ReadHelpFilePath(AReg: TRegistry);
    function HelpFileFound(AHelpPath: string): boolean;
    function GetExternalFilters: TStringList;
    procedure SetHasUncommittedUpdates(const Value: Boolean);
    function GetLastSearchedRucksack: TLastSearchedRucksack;
    procedure SetLoginUserAccessLevel(const Value: TUserAccessLevel);
    function GetFolderLocation(pathType: TFilePathType): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure InitComAddins;
    procedure DiscardComAddins;
    procedure ResetGeneralOptions;
    procedure ResetDefaultAppearance;
    procedure ResetGridRefSystem;
    procedure GetRecordingCards;
    procedure ReadDatabaseSettings;
    procedure WriteDatabaseSettings;
    procedure WriteRegistrySettings;

    function GetCDDrive: String;

    procedure ReadMainBandSettings(Sender:TObject; const AToolbar: TToolbar; const ActList: TActionList);
    procedure WriteMainBandSettings(const AToolBar: TToolbar);
    procedure ReadContextBandSettings(Sender:TObject; const AToolbar: TToolbar);
    procedure WriteContextBandSettings(const AToolbar: TToolbar);

    function GetUserAccessLevelAsString: String;
    function AllowEdit(const AMode:TEditMode):Boolean;

    function ReadSort(Sender: TBaseForm; const ListName: String;
      out FieldName: String; out Ascending: Boolean): Boolean;
    procedure WriteSort(Sender: TBaseForm; const ListName, FieldName: String;
      Ascending: Boolean);

    procedure DisplayDataForList(const ADataType: string; AItems: TKeyList; AComItems: IKeyList=nil);
    procedure TryCOMActiveFormDisplayData(const ADataType: WideString;
        const AItems: IKeyList);

    procedure DisplayFilteredData;
    procedure AddFilteredRecord(const tableName, key, hint: String);
    procedure ClearAllFilteredRecords;
    procedure ClearFilteredRecords(tableNames: array of String);
    function GetFilteredRecordHint(const tableName, key: String): String;
    function GetFilteredRecords(const tableName: String): TStringList;
    function IndexOfFilteredRecord(const tableName, key: String): Integer;
    function ConfidentialAccessGranted(accessLevel: TUserAccessLevel): Boolean;
    procedure SetConfidentialAccess(accessLevel: TUserAccessLevel; fullEditUser: Boolean);

    property MainWindowState: TWindowState read FMainWindowState write FMainWindowState;
    property MainWindowPos: TRect read FMainWindowPos write FMainWindowPos;
    property MaximizedChildWindows: Boolean read FMaximizedChildWindows write FMaximizedChildWindows;
    property ShowLastSessionWindows: Boolean read FLastSessionWindows write FLastSessionWindows;
    property AddDocsToOccurrence: Boolean read FDocsToOccurrence write FDocsToOccurrence;
    property ShowMenuIcons: Boolean read FShowMenuIcons write SetMenuIcons;
    property GraduatedMenus: Boolean read FGraduatedMenus write SetGraduatedMenus;
    property ShowMainToolbar: Boolean read FShowMainToolbar write FShowMainToolbar;
    property ShowContextToolbar: Boolean read FShowContextToolbar write FShowContextToolbar;
    property ShowWelcomeAtStart: Boolean read FShowWelcomeAtStart write FShowWelcomeAtStart;
    property DateCutYear: Integer read FDateCutYear write SetDateCutYear;
    property ExportConfidentialOccurrences: Boolean read
        GetExportConfidentialOccurrences write FExportConfidentialOcc;
    property UseOriginalIcons: Boolean  read GetUseOriginalIcons write FUseOriginalIcons;
    property MapSingleDataset: Boolean read FtfMapSingleDataset write SetMapSingleDataset;

    property MandatoryColour: TColor read FMandatoryColour write SetMandatoryColour;
    property DragSourceColour: TColor read FDragSourceColour write SetDragSourceColour;
    property DragDestColour: TColor read FDragDestColour write SetDragDestColour;
    property PlainBackground: Boolean read FPlainBackground write FPlainBackground;
    property BackBitmapName: String read FBackBitmapName write SetBackBitmapName;
    property BackImage: TPicture read FBackImage;
    property DisableDragDropFrames: Boolean read FDisableDragDropFrames write SetDisableDragDropFrames;
    property FileImportTypeIndex: integer read FFileImportTypeIndex write SetFileImportTypeIndex;
    property ShowToolTips:Boolean read FShowToolTips write SetShowToolTips;
    property SpatialRefSystem: String read FSpatialRefSystem write SetSpatialRefSystem;
    property GridRefsAsSquares: Boolean read FGridRefsAsSquares write SetGridRefsAsSquares;
    property CDDrive: String read GetCDDrive;
    property DTDPath: String read FDTDPath;
    property AddinPath: String read FAddinPath;
    property RucksackPath: String read FRucksackPath write SetRucksackPath;
    property RecordingCardPath: String read FRecordingCardPath write SetRecordingCardPath;
    property CustomSpeciesCardPath: String read FCustomSpeciesCardPath write SetCustomSpeciesCardPath;
    property DictImagesPath: String read FDictImagesPath write FDictImagesPath;
    property SiteID: String read GetSiteID;
    property DatabaseVersion: String read GetDatabaseVersion;
    property DictionaryVersion: String read GetDictionaryVersion;
    property UserID: TKeyString read FUserID write SetUserID;
    property UserAccessLevel: TUserAccessLevel read FUserAccessLevel write SetUserAccessLevel;
    property UserAccessLevelAsString: String read GetUserAccessLevelAsString;
    property RecordingCards: TPopupMenu read FRecordingCards write SetRecordingCards;
    property CurrentRucksack: TRucksack read FRucksack write FRucksack;
    property ComAddins: TComAddins read FComAddins;
    property CutOffDate: TDateTime read FCutOffDate write FCutOffDate;
    property MapFilePath: String read FMapFilePath write SetMapFilePath;
    property ObjectSheetFilePath: String read FObjectSheetFilePath write SetObjectSheetFilePath;
    property ReportTemplatePath: String read FReportTemplatePath write SetReportTemplatePath;
    property ExportTemplatePath: String read FExportTemplatePath write SetExportTemplatePath;
    property ReportPath: String read FReportPath write SetReportPath;
    property Standalone: Boolean read FStandalone;
    property BaseMapPath: String read FBaseMapPath;
    property SnapshotPath: String read FSnapshotPath write SetSnapshotPath;
    property LocalImagesPath: String read FLocalImagesPath write FLocalImagesPath;
    property PolygonFilterPath: String read FPolygonFilterPath write FPolygonFilterPath;
    property BatchUpdatePath: String read FBatchUpdatePath write SetBatchUpdatePath;
    property ExternalFilePath: String read FExternalFilePath write SetExternalFilePath;
    property MapDatasetPresent: Boolean read GetMapDatasetPresent;
    property PlaceCardToLoad: String read FPlaceCardToLoad write FPlaceCardToLoad;
    property CanExportSystemSupplied: Boolean read FCanExportSystemSupplied write SetCanExportSystemSupplied;
    property ExpandTaxaInWizard: Boolean read FExpandTaxaInWizard write SetExpandTaxaInWizard;
    //  KJG 2/12/2004 This property enables a link in lookups through recommended name key in index_taxon_name
    property UseRecommendedTaxaNames: Boolean read FUseRecommendedTaxaNames write SetUseRecommendedTaxaNames;
    property TaxonomicSearchRestriction: String read FTaxonomicSearchRestriction write SetTaxonomicSearchRestriction;
    property SessionTaxonomicSearchRestriction: String read FSessionTaxonomicSearchRestriction
        write FSessionTaxonomicSearchRestriction;
    property Convertor: THTMLRTFConversion read GetConvertor;

    property FindTaxonCriterion: TTaxonSearchType read FSearchForTaxonBy write FSearchForTaxonBy;
    property FindReferenceCriterion: TReferenceSearchType read FSearchForReferenceBy write FSearchForReferenceBy;
    property TaxonViewerVisible: Boolean read FTaxonViewerVisible write FTaxonViewerVisible;
    property BiotopeViewerVisible: Boolean read FBiotopeViewerVisible write FBiotopeViewerVisible;
    property AdminAreaViewerVisible: Boolean read FAdminAreaViewerVisible write FAdminAreaViewerVisible;
    property DisplayTaxonCommonNames: Boolean read FDisplayCommonNames write SetDisplayCommonNames;
    property DisplayTaxonEnteredNames: Boolean read FDisplayEnteredNames write SetDisplayEnteredNames;
    property DisplayTaxonAuthors: Boolean read FDisplayAuthors write SetDisplayAuthors;
    property AutoSchemeEmail: Boolean read FAutoSchemeEmail Write SetAutoSchemeEmail;

    { Online Help property }
    property HelpPath: String read FHelpPath write FHelpPath;

    { CurrentSettings properties }
    property TaxonListKey: TKeyString read GetTaxonListKey write SetTaxonListKey;
    property BiotopeListKey: TKeyString read FBiotopeListKey write FBiotopeListKey;
    property AdminAreaListKey: TKeyString read FAdminAreaListKey write FAdminAreaListKey;
    property ResultList: TEditableKeyList read FResultList write FResultList;
    property MapImageFilePath: String read FMapImageFilePath write FMapImageFilePath;
    property ComputerID: String read GetComputerID;

    property ServerName: String read FServerName;
    property DatabaseName: String read FDatabaseName;
    property TrustedSecurity: Boolean read FTrustedSecurity;

    procedure ExportKeyList(KeyList: TKeyList; Destination: String; _Type : String);
    function GetFilterItems(ExportFilterKey: String): TKeyList;
    procedure UpdateMapWindowSelectors;
    procedure UpdateMapMenu(Sender: TForm; AMenuItem: TMenuItem; ASetDefault: Boolean = False;
      AUserClickEvent: TNotifyEvent = nil);
    property AvailableMaps: TAvailableMaps read FAvailableMaps;
    property ImportTemplatePath: String read FImportTemplatePath
      write FImportTemplatePath;
    property CRReportIndex: TCRReportIndex read FCRReportIndex;
    property BatchUpdateIndex: TBUReportIndex read FBatchUpdateIndex;
    property RememberFilters: Boolean read FRememberFilters write FRememberFilters;
    property ConfidentialAccessLevel: TUserAccessLevel read FConfidentialAccessLevel;
    property ConfidentialFullEdit: Boolean read FConfidentialFullEdit;
    property RestrictFullEdit: Boolean read FRestrictFullEdit write FRestrictFullEdit;
    property SessionID: string read FSessionID write SetSessionID;
    property SessionStartTime: TDateTime read FSessionStart;
    property ReferencesSearchKeywords: boolean read FReferencesSearchKeywords write SetReferencesSearchKeywords;
    property PartialTaxonSearch: boolean read FPartialTaxonSearch write SetPartialTaxonSearch;
    property AutoCompleteSearch: boolean read FAutoCompleteSearch write SetAutoCompleteSearch;
    property UsePreferredTaxa: boolean read FUsePreferredTaxa write SetUsePreferredTaxa;
    property IncludeLocationSpatialRef: boolean read FIncludeLocationSpatialRef write SetIncludeLocationSpatialRef;
    property IncludeLocationFileCode: boolean read FIncludeLocationFileCode write SetIncludeLocationFileCode;
    property RapidEntryDelimiter: String read FRapidEntryDelimiter write SetRapidEntryDelimiter;
    property ExternalFilters: TStringList read GetExternalFilters;
    property HasUncommittedUpdates: Boolean read FHasUncommittedUpdates write SetHasUncommittedUpdates;
    property BatchUpdateConnection: TADOConnection read FBatchUpdateConnection write FBatchUpdateConnection;
    property LastSearchedRucksack: TLastSearchedRucksack read GetLastSearchedRucksack;
    property LoginUserAccessLevel: TUserAccessLevel read FLoginUserAccessLevel
        write SetLoginUserAccessLevel;
    property OrganiseSurveysByTag: Boolean read FOrganiseSurveysByTag write FOrganiseSurveysByTag;
    property UseOldImportWizard: Boolean read FUseOldImportWizard
        write FUseOldImportWizard;
    property IgnoreRememberedMatches: Boolean read FIgnoreRememberedMatches
        write FIgnoreRememberedMatches;
    property UserName: string read FUserName;
    property ExtraLocationSearchColumns: TLocationSearchColumns
        read FExtraLocationSearchColumns write FExtraLocationSearchColumns;

    property IWSurveyKey   : TKeyString read FIWSurveyKey write FIWSurveyKey;
    property IWTempNameKey : String read FIWTempNameKey write FIWTempNameKey;
    property IWIsTempSurvey : Boolean read FIWIsTempSurvey write FIWIsTempSurvey;
    property DictionaryUpgradePath : String read FDictionaryUpgradePath write FDictionaryUpgradePath;
  end;

  //----------------------------------------------------------------------------
  TAutoApplicationSettings = class(TAutoObject, IAutoApplicationSettings,
                           ICurrentSettings, ICurrentSettings6,
                           IRucksack, IRecorder2000, IRecorder6,
                           IRecorderFunctions,
                           IRecorderMainForm, IRecorderMap)
  private
    FProgressBarReferences: Integer;
    FFirstDisplayDataItem: string;
    { procedures for dealing with ICurrentSettings }
    function Get_TaxonListKey: WideString; safecall;
    function Get_BiotopeListKey: WideString; safecall;
    function Get_AdminAreaListKey: WideString; safecall;
    function Get_ResultList: IKeyList; safecall;
    function Get_UserIDKey: WideString; safecall;
    // ICurrentSettings6
    function Get_DragDestinationColour: Integer; safecall;
    function Get_DragSourceColour: Integer; safecall;
    function Get_MandatoryColour: Integer; safecall;
    function Get_DisableDragDropFrames: WordBool; safecall;
    function Get_AvailableMapCount: Integer; safecall;
    function Get_AvailableMap(Index: Integer): IAvailableMap; safecall;
    function Get_DefaultMap: IAvailableMap; safecall;
    function Get_SessionID: WideString; safecall;
    function Get_PartialTaxonSearch: WordBool; safecall;

    { procedures for dealing with IRecorder2000 }
    function Get_CurrentSettings: ICurrentSettings6; safecall;
    function Get_Rucksack: IRucksack; safecall;
    function Get_JNCCFormat: Integer; safecall;
    procedure MenuOptionClick(const iItemName: WideString); safecall;
    procedure DisplayData(const iDataType: WideString; const iItems: IKeyList); safecall;
    procedure RequestData(const Requestor: IRequestor; const DataType: WideString); safecall;
    function ShowActiveForm(iForm: TGUID): IUnknown; safecall;
    function Find(const iTitle: WideString; const iType: WideString;
             const iInitialText: WideString): WideString; safecall;
    function GetNextKey(const iTableName: WideString): WideString; safecall;
    procedure MergeDatabase(const iDatabasePath: WideString); safecall;
    function Get_RecorderFunctions: IRecorderFunctions; safecall;
    procedure CheckDatabase(const iPath: WideString; const iDetails: WideString); safecall;
    function Get_RecorderMainForm: IRecorderMainForm; safecall;
    function Get_CanExportSystemSupplied: WordBool; safecall;
    procedure Set_CanExportSystemSupplied(Value: WordBool); safecall;
    function Get_RecorderMap: IRecorderMap; safecall;
    function Get_Version: WideString; safecall;
    function AccessSQLtoSQLServerSQL(const iSQLStatement: WideString): WideString; safecall;
    function Get_ConnectionString: WideString; safecall;
    procedure SetApplicationSecurity(const Connection: IUnknown); safecall;
    procedure ExportKeyList(const KeyList: IKeyList; const Destination: WideString; const _Type : WideString); safecall;
    function Get_ReportResults: IReportResults; safecall;
    function RequestCOMData(const ARequestor: IRequestor; ASupplierGUID: TGUID): IUnknown; safecall;
    //IRecorder6
    procedure DisplayDataFromSQL(const ADataType: WideString;
             const ASQL: WideString) safecall;
    procedure DisplayTab(const ADataType: WideString; const ASQL: WideString;
                         const AItemSQL: WideString; const ATab: WideString; AExpand: WordBool;
                         AExpandRecurse: WordBool); safecall;
    function GetAvailableXmlReports(const AKeytype: WideString): IXmlReportList; safecall;
    // IRecorderFunctions
    function HTMLtoRTF(const iHTML: WideString): WideString; safecall;
    function DecodeSpatialRef(const iSpatialRef: WideString): ILatLong; safecall;
    function GetSpatialRefFromRecord(const IKeyList: IKeyList; const iFieldSpecifier: WideString): WideString; safecall;
    function DecodeVagueDate(const iVagueDate: WideString): IVagueDate; safecall;
    function GetDateFromRecord(const iKeyList: IKeyList; const iFieldSpecifier: WideString): WideString; safecall;
    function EncodeVagueDate(const iVagueDate: IVagueDate): WideString; safecall;
    function Get_SpatialRefSystem: WideString; safecall;
    function EncodeSpatialRef(const iReference: ILatLong): WideString; safecall;
    function IdentifySpatialRefSystem(const iSpatialRef: WideString): WideString; safecall;
    function GetFilterItems(const ExportFilterKey : WideString): IKeyList; safecall;

    // IRucksack
    function Get_BiotopeList: IKeyList; safecall;
    function Get_DocumentList: IKeyList; safecall;
    function Get_LocationList: IKeyList;safecall;
    function Get_NameList: IKeyList; safecall;
    function Get_RucksackFile: WideString; safecall;
    function Get_TaxonList: IKeyList; safecall;

    { IRecorderMainForm methods }
    function Get_StatusText: WideString; safecall;
    procedure Set_StatusText(const Value: WideString); safecall;
    procedure StartProgressBar; safecall;
    procedure StopProgressBar; safecall;
    function Get_Progress: Integer; safecall;
    procedure Set_Progress(Value: Integer); safecall;

    { IRecorderMap methods }
    function Get_Scale: WideString; safecall;
    procedure PanMap(const iSpatialReference: WideString); safecall;
    function Get_BaseMapRefSystem: WideString; safecall;
    function RefreshSheets: WordBool; safecall;
    procedure CheckDisplayDataType(const AType, AFieldName: string);
  public
    procedure Initialize; override;
  end;

//==============================================================================

var
  AppSettings: TApplicationSettings;

//==============================================================================
implementation

uses
  FormActions, Maintbar, Map, MapData, GenFuncs, Observations, IndOrg, Controls,
  Locations, References, TaxonDictBrowser, BiotopeDictBrowser, AdminAreaDictBrowser,
  EnhancedTermLists, OleTools, OleContainer, Find, GeneralData, DBMerger, DataImport,
  Db, VersionInfo, FolderBrowser, GeneralFunctions, InstallProgress, Addin,
  DatabaseAccessADO, ADOInt, DataExport, ExportFilters, VCLUnzip, FilterResult, MapBrowser,
  Variants, RapTree, BaseChildUnit, SQLConstants, DefaultPaths;

const
  // Registry key names
  OPT_TAXON_DICT_VIEWER          = 'Taxon Dict Viewer';
  OPT_BIOTOPE_DICT_VIEWER        = 'Biotope Dict Viewer';
  OPT_ADMINAREA_DICT_VIEWER      = 'AdminArea Dict Viewer';
  OPT_CURRENT_ADMINTYPE          = 'Current Admin Type';
  OPT_DISPLAY_COMMON             = 'Display Common Names';
  OPT_DISPLAY_ENTERED            = 'Display Taxon Names as Entered';
  OPT_DISPLAY_AUTHOR             = 'Display Taxon Authors';  
  OPT_AUTO_SCHEME_EMAIL          = 'Automatic Scheme Email';
  OPT_EXPAND_TAXA_WIZARD         = 'Expand Taxa in Wizard';
  OPT_USE_RECOMMENDED_TAXA       = 'Use Recommended Taxon Names';
  OPT_SEARCH_RESTRICT            = 'Taxonomic Search Restriction';
  OPT_CURRENT_CLASSIFICATION     = 'Current Classification';
  OPT_DICT_IMAGES_PATH           = 'Dict Images Path';
  OPT_MAP_DATASET_SYSTEM         = 'Map Dataset System';
  OPT_LOCAL_DATA_PATH            = 'Local data Path';
  OPT_DICT_UPGRADE_PATH          = 'Dictionary Path';
  OPT_FILE_IMPORT_TYPE_INDEX     = 'File Import Type Index';
  OPT_REMEMBER_FILTERS           = 'Remember Filters';
  OPT_REFERENCES_SEARCH_KEYWORDS = 'References Search by Keywords';
  OPT_REFERENCES_SEARCHED_BY     = 'References Searched By';
  OPT_TAXA_SEARCHED_BY           = 'Taxa Searched By';
  OPT_PARTIAL_TAXON_SEARCH       = 'Partial Taxon Search';
  OPT_AUTO_COMPLETE_SEARCH       = 'Auto-Complete Search in Import Wizard';
  OPT_USE_PREFERRED_TAXA         = 'Only use Preferred Taxa in Import Wizard';
  OPT_INCLUDE_LOCATION_SPATIAL_REF = 'Include Location Spatial Ref in hierarchy';
  OPT_INCLUDE_LOCATION_FILE_CODE = 'Include Location File Code in hierarchy';
  OPT_RAPID_ENTRY_DELIMITER      = 'Rapid Entry Delimiter';
  OPT_ORGANISE_SURVEYS_BY_TAG    = 'Organise Surveys By Tag';
  OPT_USE_OLD_IMPORT_WIZARD      = 'Use Old Import Wizard';
  OPT_IGNORE_REMEMBERED_MATCHES  = 'Ignore Remembered Matches';
  OPT_GRID_REFS_AS_SQUARES       = 'Grid References As Squares';
  OPT_DTD_PATH                   = 'DTD Path';
  OPT_ERROR_PATH                 = 'Error Path';
  OPT_BATCH_UPDATE_PATH          = 'Batch Update Path';
  OPT_EXTERNAL_FILE_PATH         = 'External File Path';
  OPT_IMPORT_TEMPLATE_PATH       = 'Import Template Path';
  OPT_LOCAL_IMAGES_FILE_PATH     = 'Local Images File Path';
  OPT_POLYGON_FILTER_PATH        = 'Polygon Filter Path';
  OPT_RECORDING_CARD_PATH        = 'Recording Card Path';
  OPT_CUSTOM_SPECIES_CARD_PATH   = 'Custom Species Card Path';
  OPT_REPORT_PATH                = 'Report Path';
  OPT_REPORT_TEMPLATE_PATH       = 'Report Template Path';
  OPT_EXPORT_TEMPLATE_PATH       = 'Export Template Path';
  OPT_RUCKSACK_PATH              = 'Rucksack Path';
  OPT_SNAPSHOT_PATH              = 'Snapshot Path';
  OPT_OBJECT_SHEET_FILE_PATH     = 'Object Sheet File Path';
  OPT_BASE_MAP_PATH              = 'Base Map Path';
  OPT_MAP_FILE_PATH              = 'Map File Path';

(**
 * Clone of the Delphi FileOpen method which does not have possible range check
 * errors due to possible typecasting from THandle (LongWord) to integer, i.e.
 * unsigned to signed.
 *)
function SafeFileOpen(const FileName: string; Mode: LongWord): THandle;
const
  AccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  if ((Mode and 3) > fmOpenReadWrite) or
    ((Mode and $F0) > fmShareDenyNone) then
    raise EApplicationSettingsError.Create('Call to SafeFileOpen failed due to incorrect Mode');
  result := CreateFile(PChar(FileName), AccessMode[Mode and 3],
      ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0);
end;

//----------------------------------------------------------------------------
// Sort stringlist as if contains integers - to order registry keys
function IntSort(List: TStringList; Index1, Index2: Integer): Integer;
var
  lIntValue1, lIntValue2 : Integer;
  lCode1, lCode2 : Integer;
begin
  // read the strings as integers
  Val(List[Index1], lIntValue1, lCode1);
  Val(List[Index2], lIntValue2, lCode2);
  // if first value not a number, treat it as an infinitely low number
  if (lCode1<>0) and (lCode2=0) then
    Result := -1
  // if second value not a number, treat it as an infinitely low number
  else if (lCode2<>0) and (lCode1=0) then
    Result := 1
  // if both not numbers, leave sort order as it is
  else if (lCode1<>0) and (lCode2<>0) then
    Result := 0
  // both are numbers, so compare
  else
    Result := 0-CompareValue(lIntValue1, lIntValue2);
end;

//==============================================================================
{ TApplicationSettings }
constructor TApplicationSettings.Create;
begin
  inherited Create;
  frmInstallProgress             := nil; // Safety, in case it is never shown
  FMaximizedChildWindows         := False;
  FRucksack                      := TRucksack.Create;
  FRucksack.AppSettingsObject    := Self;
  FResultList                    := TEditableKeyList.Create;
  FBackImage                     := TPicture.Create;
  FRecordingCards                := TPopupmenu.Create(frmMain);
  FCanExportSystemSupplied       := False;
  FMapImageFilePath              := '';

  ResetGeneralOptions;
  ResetDefaultAppearance;
  ResetGridRefSystem;
  GetRecordingCards;
  ReadRegistrySettings;
  CleanupOldImportDatabases;

  FAvailableMaps                 := TAvailableMaps.Create;
  FCRReportIndex                 := TCRReportIndex.Create(FReportPath);
  FBatchUpdateIndex              := TBUReportIndex.Create(FBatchUpdatePath);

  FExternalFilters               := TStringList.Create;
  FExternalFilters.CaseSensitive := False;
  FHasUncommittedUpdates         := False;
end;  // Create

//==============================================================================
{ We need a separate procedure to init the COM addins, as AppSettings is
     required.  This initialises the COM Addin reading. }
procedure TApplicationSettings.InitComAddins;
var
  i: Integer;
  guid: TGUID;
  intf: IUnknown;
begin
  FComAddins := TComAddins.Create;
  // Setup the link with for the COM spatial reference systems.
  SetCommAddInLink(FComAddIns.SpatialSystems, FComAddIns.SpatialSystemInterfaces);

  // Default
  dmFormActions.SetActionVisibility(dmFormActions.actDatabaseValidateAll, False);

  for i := 0 to ComAddins.Validators.Count - 1 do begin
    guid := StringToGuid(ComAddins.Validators[i]);
    intf := CreateComObject(guid);

    // Only need to find one with IValidation6
    if Supports(intf, IID_IValidation6) then begin
      dmFormActions.SetActionVisibility(
          dmFormActions.actDatabaseValidateAll,
          UserAccessLevel = ualAdmin);
      Exit;
    end;
  end;
end;  // InitComAddins

{ Discard the COM addins, this must be done before the application starts to
      shut down properly }
procedure TApplicationSettings.DiscardComAddins;
begin
  FreeAndNil(FComAddins);
end;

//==============================================================================
destructor TApplicationSettings.Destroy;
begin
  FreeAndNil(FAvailableMaps);
  FreeAndNil(FComAddins);
  FreeAndNil(FBackImage);
  FreeAndNil(FRecordingCards);
  FreeAndNil(FResultList);
  FreeAndNil(FRucksack);
  FreeAndNil(FCRReportIndex);
  FreeAndNil(FBatchUpdateIndex);
  FreeAndNil(FLastSearchedRucksack);

  ClearAllFilteredRecords;
  FExternalFilters.Free;

  if Assigned(FBatchUpdateConnection) then
    FBatchUpdateConnection.Free;

  CloseSession;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TApplicationSettings.ResetGeneralOptions;
begin
  AddDocsToOccurrence := DEFAULT_ADD_DOCS_TO_OCCURRENCE;
  ShowLastSessionWindows := DEFAULT_SHOW_LAST_SESSION_WINDOWS;
  ShowMenuIcons          := DEFAULT_SHOW_MENU_ICONS;
  GraduatedMenus         := DEFAULT_GRADUATED_MENUS;
  ShowMainToolbar        := DEFAULT_SHOW_MAIN_TOOLBAR;
  ShowContextToolbar     := DEFAULT_SHOW_CONTEXT_TOOLBAR;
  ShowWelcomeAtStart     := DEFAULT_SHOW_WELCOME_AT_START;
end;  // ResetGeneralOptions

//==============================================================================
procedure TApplicationSettings.ResetDefaultAppearance;
begin
  DragSourceColour      := DEFAULT_DRAG_SOURCE_COLOUR;
  DragDestColour        := DEFAULT_DRAG_DEST_COLOUR;
  MandatoryColour       := DEFAULT_MANDATORY_COLOUR;
  BackBitmapName        := DEFAULT_BACKGROUND_PICTURE;  // Sets PlainBackground and BackImage too
  DisableDragDropFrames := DEFAULT_DISABLE_DRAG_DROP_FRAME;
  ShowToolTips          := DEFAULT_SHOW_TOOL_TIPS;
end;  // ResetDefaultAppearance

//==============================================================================
procedure TApplicationSettings.ResetGridRefSystem;
begin
  SpatialRefSystem := DEFAULT_SPATIAL_REF_SYSTEM;
end;  // ResetSpatialRefSystem

//==============================================================================
procedure TApplicationSettings.ReadDatabaseSettings;
var
  value: String;

  function ReadSettingValue(const AName: String): String;
  begin
    Result := VarToStr(dmDatabase.GetStoredProcOutputParam(
        'usp_Setting_Get',
        ['@Name', AName],
        '@Value'));
  end;

begin
  // Confidential Access Level defaults to Full Edit
  value := ReadSettingValue('LevelConf');
  if value = '' then FConfidentialAccessLevel := DEFAULT_CONFIDENTIAL_ACCESS_LEVEL
                else FConfidentialAccessLevel := TUserAccessLevel(StrToInt(value) - 1);
  if FConfidentialAccessLevel = ualFullUser then
  begin
    value := ReadSettingValue('ConfFull');
    if value = '' then FConfidentialFullEdit := True
                  else FConfidentialFullEdit := value = '1';
  end;
end; // ReadDatabaseSettings;

//==============================================================================
procedure TApplicationSettings.ReadRegistrySettings;
var
  lReg:TRegistry;
begin
  lReg := TRegistry.Create;
  with lReg do
    try
      ShowIfWorkstationInstall(lReg);

      // Read Addin path from Local Machine registry section
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(REG_KEY_RECORDER) then
      begin
        FDictionaryUpgradePath := ReadString('Dictionary Path');
        FAddinPath     := ReadString('Addin Path');
        FLocalDataPath := ReadString('Local Data Path');
        if (ValueExists('Standalone')) then
          FStandalone := ReadBool('Standalone')
        else
          FStandalone := false;
        if ValueExists('Server Name') then
        begin
          FServerName      := ReadString('Server Name');
          FDatabaseName    := ReadString('Database Name');
          FTrustedSecurity := ReadBool('Trusted Security');
        end else
          ReadServerSettingsFromFile;
      end else
        raise EApplicationSettingsError.Create(Format(ResStr_RegistryNotFound, [Application.Title]));
      CloseKey;

      // Get the rest from the Current User registry section
      {$IFDEF DELPHI5UP}
      Access := KEY_ALL_ACCESS;
      {$ENDIF}
      RootKey := HKEY_CURRENT_USER;
      if OpenKey(REG_KEY_SETTINGS, True) then begin
        { The master copy of Addin Path is now in current machine, but legacy
            addins still require it in local user. }
        if AddinPath<>'' then
          WriteString('Addin Path',AddinPath);
        CheckAccessDatasePath(lReg);
        { Read String values }
        BackBitmapName      := ReadStringDefault(lReg, 'Back Bitmap', DEFAULT_BACKGROUND_PICTURE);
        FCutOffDate         := EncodeDate(StrToInt(ReadStringDefault(lReg, 'Cut Off Date', '1980')), 1, 1);
        FtfMapSingleDataset := ReadBoolDefault(lReg, 'Map Single Database', False);
        TaxonomicSearchRestriction := ReadStringDefault(lReg, OPT_SEARCH_RESTRICT, ResStr_PreferredLists);
        FTaxonListKey       := ReadStringDefault(lReg, 'Current Checklist', '');
        FBiotopeListKey     := ReadStringDefault(lReg, OPT_CURRENT_CLASSIFICATION, '');
        FAdminAreaListKey   := ReadStringDefault(lReg, OPT_CURRENT_ADMINTYPE, '');
        SpatialRefSystem    := ReadStringDefault(lReg, 'Spatial Ref System', DEFAULT_SPATIAL_REF_SYSTEM);
        GridRefsAsSquares   := ReadBoolDefault(lReg, OPT_GRID_REFS_AS_SQUARES, DEFAULT_GRID_REFS_AS_SQUARES);
        RapidEntryDelimiter := ReadStringDefault(lReg, OPT_RAPID_ENTRY_DELIMITER, '');

        { Read Boolean values }
        if ReadBoolDefault(lReg, 'Maximized', False) then
          MainWindowState:=wsMaximized
        else
          MainWindowState := wsNormal;
        if ValueExists('Position') and (MainWindowState<>wsMaximized) then // not relevant for maximised window
          ReadBinaryData('Position', FMainWindowPos, SizeOf(TRect))
        else
          FMainWindowPos := Rect(50, 50, Screen.Width - 100, Screen.Height - 100);
        ShowLastSessionWindows := ReadBoolDefault(lReg, 'Last Session Windows', DEFAULT_SHOW_LAST_SESSION_WINDOWS);
        AddDocsToOccurrence := ReadBoolDefault(lReg, 'Add Docs To Occurrence', DEFAULT_ADD_DOCS_TO_OCCURRENCE);
        ShowMenuIcons          := ReadBoolDefault(lReg, 'ShowMenuIcons', DEFAULT_SHOW_MENU_ICONS);
        GraduatedMenus         := ReadBoolDefault(lReg, 'Graduated Menus', DEFAULT_GRADUATED_MENUS);
        ShowWelcomeAtStart     := ReadBoolDefault(lReg, 'ShowWelcomeAtStart', DEFAULT_SHOW_WELCOME_AT_START);
        ExportConfidentialOccurrences := ReadBoolDefault(lReg, 'Export Confidential Occurrences', DEFAULT_EXPORT_CONFIDENTIAL_OCC);
        UseOriginalIcons := ReadBoolDefault(lreg, 'Original Icons', DEFAULT_USE_ORIGINAL_ICONS);
        FSearchForTaxonBy      := TTaxonSearchType(ReadBoolDefault(lReg, OPT_TAXA_SEARCHED_BY, Boolean(stName)));
        FSearchForReferenceBy  := TReferenceSearchType(ReadBoolDefault(lReg, OPT_REFERENCES_SEARCHED_BY, Boolean(stName)));
        PlainBackground        := ReadBoolDefault(lReg, 'Plain Background', True);
        DisableDragDropFrames  := ReadBoolDefault(lReg, 'Drag And Drop', DEFAULT_DISABLE_DRAG_DROP_FRAME);
        if ValueExists(OPT_FILE_IMPORT_TYPE_INDEX) then
          FileImportTypeIndex:=ReadInteger(OPT_FILE_IMPORT_TYPE_INDEX)
        else
          FileImportTypeIndex:=0;
        ShowToolTips              := ReadBoolDefault(lReg, 'Tool Tips',                 DEFAULT_SHOW_TOOL_TIPS);
        FDisplayCommonNames       := ReadBoolDefault(lReg, OPT_DISPLAY_COMMON,          DEFAULT_SHOW_COMMON_NAMES);
        FDisplayEnteredNames      := ReadBoolDefault(lReg, OPT_DISPLAY_ENTERED,         DEFAULT_SHOW_ENTERED_NAMES);
        FDisplayAuthors           := ReadBoolDefault(lReg, OPT_DISPLAY_AUTHOR,          DEFAULT_SHOW_AUTHORS);
        FAutoSchemeEmail          := ReadBoolDefault(lReg, OPT_AUTO_SCHEME_EMAIL,       DEFAULT_AUTO_SCHEME_EMAIL);
        FReferencesSearchKeywords := ReadBoolDefault(lReg,
            OPT_REFERENCES_SEARCH_KEYWORDS, DEFAULT_REFERENCES_SEARCH_KEYWORDS);
        FExpandTaxaInWizard       := ReadBoolDefault(lReg, OPT_EXPAND_TAXA_WIZARD,      True);
        FUseRecommendedTaxaNames  := ReadBoolDefault(lReg, OPT_USE_RECOMMENDED_TAXA,    True);
        TaxonViewerVisible        := ReadBoolDefault(lReg, OPT_TAXON_DICT_VIEWER,       True);
        BiotopeViewerVisible      := ReadBoolDefault(lReg, OPT_BIOTOPE_DICT_VIEWER,     True);
        AdminAreaViewerVisible    := ReadBoolDefault(lReg, OPT_ADMINAREA_DICT_VIEWER,   True);
        RememberFilters           := ReadBoolDefault(lReg, OPT_REMEMBER_FILTERS,        False);
        PartialTaxonSearch        := ReadBoolDefault(lReg, OPT_PARTIAL_TAXON_SEARCH,    DEFAULT_PARTIAL_TAXON_SEARCH);
        AutoCompleteSearch        := ReadBoolDefault(lReg, OPT_AUTO_COMPLETE_SEARCH,    DEFAULT_AUTO_COMPLETE_SEARCH);
        UsePreferredTaxa          := ReadBoolDefault(lReg, OPT_USE_PREFERRED_TAXA,   DEFAULT_USE_PREFERRED_TAXA);
        IncludeLocationSpatialRef := ReadBoolDefault(lReg, OPT_INCLUDE_LOCATION_SPATIAL_REF, DEFAULT_INCLUDE_LOCATION_SPATIAL_REF);
        IncludeLocationFileCode   := ReadBoolDefault(lReg, OPT_INCLUDE_LOCATION_FILE_CODE, DEFAULT_INCLUDE_LOCATION_FILE_CODE);
        OrganiseSurveysByTag      := ReadBoolDefault(lReg, OPT_ORGANISE_SURVEYS_BY_TAG, DEFAULT_ORGANISE_SURVEYS_BY_TAG);
        UseOldImportWizard        := False;
        IgnoreRememberedMatches      := ReadBoolDefault(lReg,
            OPT_IGNORE_REMEMBERED_MATCHES, DEFAULT_IGNORE_REMEMBERED_MATCHES);

        { Read colour values }
        FMandatoryColour  := ReadColourDefault(lReg, 'Mandatory',        DEFAULT_MANDATORY_COLOUR);
        FDragSourceColour := ReadColourDefault(lReg, 'Drag Source',      DEFAULT_DRAG_SOURCE_COLOUR);
        FDragDestColour   := ReadColourDefault(lReg, 'Drag Destination', DEFAULT_DRAG_DEST_COLOUR);

        if ValueExists('CenturyCutOff') then DateCutYear := ReadInteger('CenturyCutOff')
                                        else DateCutYear := DEFAULT_CUT_YEAR;

        { Read file and path settings }
         ReadHelpFilePath(lReg);

        // note on a standalone install, the installer points dict images to the CD.
        FDictImagesPath        := FindRegPath(OPT_DICT_IMAGES_PATH,       PATH_DICT_IMAGES,      lReg, fptInstallFolder, [], true);
        FDTDPath               := FindRegPath(OPT_DTD_PATH,               PATH_DTD,              lReg, fptInstallFolder, ['nbndata.dtd', 'exportstart.xml']);
        FErrorPath             := FindRegPath(OPT_ERROR_PATH,             PATH_ERRORS,           lReg, fptNetworkDocs, []);
        madExceptErrorPath     := FErrorPath;
        FDictionaryUpgradePath := FindRegPath(OPT_DICT_UPGRADE_PATH,      PATH_DICT_UPGRADE,     lReg, fptNetworkDocs, []);
        FBatchUpdatePath       := FindRegPath(OPT_BATCH_UPDATE_PATH,      PATH_BATCH_UPDATES,    lReg, fptNetworkDocs, []);
        FExternalFilePath      := FindRegPath(OPT_EXTERNAL_FILE_PATH,     PATH_EXTERNAL_FILES,   lReg, fptNetworkDocs, []);
        FImportTemplatePath    := FindRegPath(OPT_IMPORT_TEMPLATE_PATH,   PATH_IMPORT_TEMPLATES, lReg, fptNetworkDocs, []);
        FLocalImagesPath       := FindRegPath(OPT_LOCAL_IMAGES_FILE_PATH, PATH_LOCAL_IMAGES,     lReg, fptNetworkDocs, []);
        FPolygonFilterPath     := FindRegPath(OPT_POLYGON_FILTER_PATH,    PATH_POLYGON_FILTERS,  lReg, fptNetworkDocs, []);
        FRecordingCardPath     := FindRegPath(OPT_RECORDING_CARD_PATH,    PATH_RECORDING_CARDS,  lReg, fptNetworkDocs, []);
        FCustomSpeciesCardPath := FindRegPath(OPT_CUSTOM_SPECIES_CARD_PATH, PATH_CUSTOM_SPECIES_CARDS, lReg, fptNetworkDocs, []);
        FReportPath            := FindRegPath(OPT_REPORT_PATH,            PATH_REPORTS,          lReg, fptNetworkDocs, []);
        FReportTemplatePath    := FindRegPath(OPT_REPORT_TEMPLATE_PATH,   PATH_REPORT_TEMPLATES, lReg, fptNetworkDocs, []);
        FExportTemplatePath    := FindRegPath(OPT_EXPORT_TEMPLATE_PATH,   PATH_EXPORT_TEMPLATES, lReg, fptNetworkDocs, []);
        FRucksackPath          := FindRegPath(OPT_RUCKSACK_PATH,          PATH_RUCKSACKS,        lReg, fptNetworkDocs, []);
        FSnapshotPath          := FindRegPath(OPT_SNAPSHOT_PATH,          PATH_SNAPSHOTS,        lReg, fptNetworkDocs, []);
        FObjectSheetFilePath   := FindRegPath(OPT_OBJECT_SHEET_FILE_PATH, PATH_OBJECT_SHEETS,    lReg, fptNetworkData, []);
        FMapFilePath           := FindRegPath(OPT_MAP_FILE_PATH,          PATH_MAP_FILES,        lReg, fptLocalData, []);
        // base maps are kept in the install folder for standalone (as they don't change) but
        // are copied to local data for networked installs to improve load performance.
        if FStandalone then
          FBaseMapPath := FindRegPath(OPT_BASE_MAP_PATH, PATH_BASE_MAPS, lReg, fptInstallFolder, [])
        else
          // in the server setup, set canCreate to true so that it does not auto-detect the server
          // base maps folder. We want it local!
          FBaseMapPath := FindRegPath(OPT_BASE_MAP_PATH, PATH_BASE_MAPS, lReg, fptLocalData, [], false, true);
        CopyBaseMaps(lReg); // special code, as if server/workstation install then need to obtain a local copy

        CloseKey;

        ReadExtraLocationSearchColumns;

        // Initialisation required according to settings obtained
        if not FileExists(BackBitmapName) then PlainBackground := True;
        { Dict Images path doesn't need a slash }
        if (FDictImagesPath <> '') and (FDictImagesPath[Length(FDictImagesPath)] = '\') then
          FDictImagesPath := Copy(FDictImagesPath, 1, Length(FDictImagesPath) - 1);
      end;  // if lReg.OpenKey...
    finally
      Free;
      FreeWorkstationInstallForm;
    end;
end;  // ReadRegistrySettings

{------------------------------------------------------------------------------}
function TApplicationSettings.HelpFileFound(AHelpPath: string): boolean;
begin
  Result := FileExists(AHelpPath);
  if Result then
    FHelpPath := AHelpPath;
end;

{-------------------------------------------------------------------------------}
procedure TApplicationSettings.ReadHelpFilePath(AReg: TRegistry);
var
  langId, helpPathTemplate: String;
begin
  //Finding the current language id of recorder
  langId := Languages.Ext[Languages.IndexOf(Syslocale.DefaultLCID)];
  helpPathTemplate := ExtractFilePath(Application.ExeName) + 'Help\Rec20HLP%s.chm';
  if not HelpFileFound(Format(helpPathTemplate, ['.' + langId])) then
    if not HelpFileFound(Format(helpPathTemplate, ['.' + Copy(langId, 1, 2)])) then
      if not HelpFileFound(Format(helpPathTemplate, [''])) then
        ShowInformation(ResStr_CannotFindHelpFile);
end;

{-------------------------------------------------------------------------------
  Description : Checks that the old access database path still exists and
  points to the right place.  AReg must be prepared pointing to the Settings
  Created : 24/03/2003 }
procedure TApplicationSettings.CheckAccessDatasePath(AReg : TRegistry);
var
  lDBPath : String;
begin
  if not AReg.ValueExists('Database Path') then begin
    lDBPath := ExtractFilePath(Application.Exename) + 'Database\nbndata.mdb';
    if FileExists(lDBPath) then
      AReg.WriteString('Database Path', lDBPath);
  end;
end;

{===============================================================================
 Description: For networked workstations, pick up server settings from an ini
             file
 Created: 21/1/2003 }
procedure TApplicationSettings.ReadServerSettingsFromFile;
var
  lFile: TStringList;
begin
  if FileExists(ExtractFilePath(Application.Exename) + 'ServerDB.ini') then
  begin
    lFile := TStringList.Create;
    try
      lFile.LoadFromFile(ExtractFilePath(Application.Exename) + 'ServerDB.ini');
      FServerName      := lFile.Values['Server Name'];
      FDatabaseName    := lFile.Values['Database Name'];
      FTrustedSecurity := lFile.Values['Trusted Security'] = '1';
    finally
      lFile.Free;
    end
  end else
    raise EApplicationSettingsError.Create(Format(ResStr_RegistryNotFound, [Application.Title]));
end;

//==============================================================================
procedure TApplicationSettings.WriteDatabaseSettings;
begin
  // Only admin can do this.
  if LoginUserAccessLevel = ualAdmin then
  begin
    dmDatabase.RunUpdateStoredProc(
        'usp_Setting_Update',
        ['@Name', 'LevelConf', '@Value', IntToStr(Integer(ConfidentialAccessLevel) + 1)]);

    if ConfidentialAccessLevel = ualFullUser then
      dmDatabase.RunUpdateStoredProc(
          'usp_Setting_Update',
          ['@Name', 'ConfFull', '@Value', IfThen(ConfidentialFullEdit, '1', '0')]);
  end;
end;  // WriteDatabaseSettings

//==============================================================================
procedure TApplicationSettings.WriteRegistrySettings;
var lReg:TRegistry;
    lDummy: word; // for decode date information we don't need
    lYear: word;
begin
  lReg:=TRegistry.Create;
  with lReg do
    try
      if OpenKey(REG_KEY_SETTINGS, True) then begin
        // Save Global Settings
        WriteBool('Maximized', MainWindowState = wsMaximized);
        if MainWindowState <> wsMaximized then
          WriteBinaryData('Position', FMainWindowPos, SizeOf(TRect));

        // General Options
        WriteBool('Last Session Windows',            ShowLastSessionWindows);
        WriteBool('Add Docs To Occurrence',         AddDocsToOccurrence);
        WriteBool('ShowMenuIcons',                   ShowMenuIcons);
        WriteBool('Graduated Menus',                 GraduatedMenus);
        WriteBool('ShowWelcomeAtStart',              ShowWelcomeAtStart);
        WriteInteger('CenturyCutOff',                FDateCutYear);
        // Only admin can change this. Anyone else and this is not changed from what was loaded.
        if UserAccessLevel = ualAdmin then
          WriteBool('Export Confidential Occurrences', ExportConfidentialOccurrences);
        WriteBool(OPT_REMEMBER_FILTERS,              RememberFilters);
        WriteBool(OPT_PARTIAL_TAXON_SEARCH,          PartialTaxonSearch);
        WriteBool(OPT_AUTO_COMPLETE_SEARCH,          AutoCompleteSearch);
        WriteBool(OPT_USE_PREFERRED_TAXA,            UsePreferredTaxa);
        WriteBool(OPT_INCLUDE_LOCATION_SPATIAL_REF,  IncludeLocationSpatialRef);
        WriteBool(OPT_INCLUDE_LOCATION_FILE_CODE,    IncludeLocationFileCode);
        WriteString(OPT_RAPID_ENTRY_DELIMITER,       RapidEntryDelimiter);
        WriteBool(OPT_ORGANISE_SURVEYS_BY_TAG,       OrganiseSurveysByTag);
        WriteBool(OPT_USE_OLD_IMPORT_WIZARD,         UseOldImportWizard);
        WriteBool(OPT_IGNORE_REMEMBERED_MATCHES,     IgnoreRememberedMatches);
        WriteExtraLocationSearchColumns;
        // Show main and context toolbars done with toolbar settings

        // Appearance
        WriteBinaryData('Mandatory',             FMandatoryColour,SizeOf(TColor));
        WriteBinaryData('Drag Source',           FDragSourceColour,SizeOf(TColor));
        WriteBinaryData('Drag Destination',      FDragDestColour,SizeOf(TColor));
        WriteBool('Plain Background',            PlainBackground);
        WriteString('Back Bitmap',               BackBitmapName);
        WriteBool('Drag And Drop',               DisableDragDropFrames);
        WriteInteger(OPT_FILE_IMPORT_TYPE_INDEX, FileImportTypeIndex);
        WriteBool('Tool Tips',                   ShowToolTips);
        WriteBool('Original Icons',             UseOriginalIcons);

        // File Locations
        if RucksackPath <> '' then
          WriteString(OPT_RUCKSACK_PATH,RucksackPath);
        if RecordingCardPath <> '' then
          WriteString(OPT_RECORDING_CARD_PATH,RecordingCardPath);
        if CustomSpeciesCardPath <> '' then
          WriteString(OPT_CUSTOM_SPECIES_CARD_PATH, CustomSpeciesCardPath);
        if MapFilePath <> '' then
          WriteString(OPT_MAP_FILE_PATH, MapFilePath);
        if ObjectSheetFilePath <> '' then
          WriteString(OPT_OBJECT_SHEET_FILE_PATH, ObjectSheetFilePath);
        if ReportTemplatePath <> '' then
          WriteString(OPT_REPORT_TEMPLATE_PATH, ReportTemplatePath);
        if ExportTemplatePath <> '' then
          WriteString(OPT_EXPORT_TEMPLATE_PATH, ExportTemplatePath);
        if ReportPath <> '' then
          WriteString(OPT_REPORT_PATH,ReportPath);
        if LocalImagesPath <> '' then
          WriteString(OPT_LOCAL_IMAGES_FILE_PATH, LocalImagesPath);
        if PolygonFilterPath <> '' then
          WriteString(OPT_POLYGON_FILTER_PATH, PolygonFilterPath);
        if FSnapshotPath <> '' then
          WriteString(OPT_SNAPSHOT_PATH, FSnapshotPath);
        if FImportTemplatePath <> '' then
          WriteString(OPT_IMPORT_TEMPLATE_PATH, FImportTemplatePath);
        if FBatchUpdatePath <> '' then
          WriteString(OPT_BATCH_UPDATE_PATH, FBatchUpdatePath);
        if FExternalFilePath <> '' then
          WriteString(OPT_EXTERNAL_FILE_PATH, FExternalFilePath);
        if FErrorPath <> '' then
          WriteString(OPT_ERROR_PATH, FErrorPath);
        if FDictionaryUpgradePath <> '' then
          WriteString(OPT_DICT_UPGRADE_PATH, FDictionaryUpgradePath);

        // Spatial Ref System
        WriteString('Spatial Ref System',   SpatialRefSystem);
        WriteBool(OPT_GRID_REFS_AS_SQUARES, GridRefsAsSquares);

        // Cut Off Date
        DecodeDate(FCutoffDate, lYear, lDummy, lDummy);
        WriteString('Cut Off Date',     IntToStr(lYear));
        WriteBool('Map Single Dataset', FtfMapSingleDataset);

        // User ID
        WriteString('Last User',UserID);

        // Current Settings
        WriteString ('Current Checklist',            TaxonListKey);
        WriteInteger(OPT_TAXA_SEARCHED_BY,           Integer(FindTaxonCriterion));
        WriteInteger(OPT_REFERENCES_SEARCHED_BY,     Integer(FindReferenceCriterion));
        WriteString (OPT_CURRENT_CLASSIFICATION,     BiotopeListKey);
        WriteString (OPT_CURRENT_ADMINTYPE,          AdminAreaListKey);
        WriteBool   (OPT_TAXON_DICT_VIEWER,          TaxonViewerVisible);
        WriteBool   (OPT_BIOTOPE_DICT_VIEWER,        BiotopeViewerVisible);
        WriteBool   (OPT_ADMINAREA_DICT_VIEWER,      AdminAreaViewerVisible);
        WriteBool   (OPT_DISPLAY_COMMON,             DisplayTaxonCommonNames);
        WriteBool   (OPT_DISPLAY_ENTERED,            DisplayTaxonEnteredNames);
        WriteBool   (OPT_DISPLAY_AUTHOR,             DisplayTaxonAuthors);
        WriteBool   (OPT_AUTO_SCHEME_EMAIL,          AutoSchemeEmail);
        WriteBool   (OPT_REFERENCES_SEARCH_KEYWORDS, ReferencesSearchKeywords);
        WriteBool   (OPT_EXPAND_TAXA_WIZARD,         ExpandTaxaInWizard);
        WriteBool   (OPT_USE_RECOMMENDED_TAXA,       UseRecommendedTaxaNames);
        WriteString (OPT_SEARCH_RESTRICT,            TaxonomicSearchRestriction);

        // Map Stuff
        WriteString(OPT_BASE_MAP_PATH,    BaseMapPath);
        WriteString(OPT_OBJECT_SHEET_FILE_PATH,    ObjectSheetFilePath);
        WriteString(OPT_MAP_FILE_PATH,    MapFilePath);
        WriteString(OPT_DICT_IMAGES_PATH, FDictImagesPath);
        CloseKey;
      end;
    finally
      Free;
    end;
end;  // WriteRegistrySettings

{-------------------------------------------------------------------------------
  If running a workstation for the first time, show a box to indicate that the
  install process is taking place, otherwise returns nil.
  Registry object is passed to save it creating a new one }
procedure TApplicationSettings.ShowIfWorkstationInstall(iReg: TRegistry);
begin
  { If can't find local user settings }
  if not iReg.OpenKey(REG_KEY_SETTINGS, False) then begin
    ShowInstallProgressForm;
  end else
    frmInstallProgress := nil;
end;

{-------------------------------------------------------------------------------
  Displays the Install Progress splash screen.  Does nothing if already shown }
procedure TApplicationSettings.ShowInstallProgressForm;
begin
  if not Assigned(frmInstallProgress) then begin
    frmInstallProgress := TfrmInstallProgress.Create(nil);
    frmInstallProgress.Show;
    Application.ProcessMessages;
    FWorkstationInstallFormStart := Now;
  end; // if
end;

{-------------------------------------------------------------------------------
  If the workstation install form was displayed at all, then free it but only
  after a minimum of 2 seconds }
procedure TApplicationSettings.FreeWorkstationInstallForm;
begin
  if Assigned(frmInstallProgress) then begin
    while Now - FWorkstationInstallFormStart < 1 / (24 * 60 * 30) do
      Sleep(100);
    FreeAndNil(frmInstallProgress);
  end;
end;

{-------------------------------------------------------------------------------
  Returns the String value from the registry (which must already point to the
  correct place), or returns the default if none found }
function TApplicationSettings.ReadStringDefault(iReg: TRegistry;
    const iValueName, iDefault: String): String;
begin
  Result := '';
  if iReg.ValueExists(iValueName) then
    Result := iReg.ReadString(iValueName);
  if Result = '' then
  begin
    Result := iDefault;
    iReg.WriteString(iValueName, Result);
  end;
end;

{-------------------------------------------------------------------------------
  Returns the Boolean value from the registry (which must already point to the
  correct place), or returns the default if none found }
function TApplicationSettings.ReadBoolDefault(iReg: TRegistry;
    const iValueName: String; iDefault: Boolean): Boolean;
begin
  if iReg.ValueExists(iValueName) then
    Result := iReg.ReadBool(iValueName)
  else begin
    Result := iDefault;
    iReg.WriteBool(iValueName, Result);
  end;
end;

{-------------------------------------------------------------------------------
  Returns the Boolean value from the registry (which must already point to the
  correct place), or returns the default if none found }
function TApplicationSettings.ReadColourDefault(iReg: TRegistry;
    const iValueName: String; iDefault: TColor): TColor;
begin
  if iReg.ValueExists(iValueName) then
    iReg.ReadBinaryData(iValueName, Result, SizeOf(TColor))
  else begin
    Result := iDefault;
    iReg.WriteBinaryData(iValueName, Result, SizeOf(TColor));
  end;
end;

//==============================================================================
{ Load the base map path from the registry.  If not present, and a server
     install is detected, then copy the server folder down to the workstation }
procedure TApplicationSettings.CopyBaseMaps(iReg: TRegistry);
begin
  // If a network install, copy any new base map files over to local data
  if ExtractFilePath(Application.Exename) + PATH_BASE_MAPS <> FBaseMapPath then begin
    if not CopyFiles(
        ExtractFilePath(Application.Exename) + PATH_BASE_MAPS + '*.*',
        FBaseMapPath) then
      ShowInformation(Format(ResStr_CantCopyBaseMaps, [FBaseMapPath]));
  end;
end;

{-------------------------------------------------------------------------------
  Reads the selection of extra location search columns from the registry.
}
procedure TApplicationSettings.ReadExtraLocationSearchColumns;
var
  lColumns: TLocationSearchColumns;
  lReg: TRegistry;
begin
  lColumns := [];
  
  lReg := TRegistry.Create;
  try
    if lReg.OpenKeyReadOnly(REG_KEY_LOCATION_SEARCH_COLS) then
    begin
      if lReg.ReadBool('Location Type') then
        Include(lColumns, lscLocationType);
      if lReg.ReadBool('Spatial Reference') then
        Include(lColumns, lscSpatialReference);
      if lReg.ReadBool('File Code') then
        Include(lColumns, lscFileCode);
    end;
  finally
    lReg.Free;
  end;

  ExtraLocationSearchColumns := lColumns;
end;

{-------------------------------------------------------------------------------
  Writes the selection of extra location search columns to the registry.
}
procedure TApplicationSettings.WriteExtraLocationSearchColumns;
var
  lReg: TRegistry;
begin
  lReg := TRegistry.Create;
  try
    if lReg.OpenKey(REG_KEY_LOCATION_SEARCH_COLS, True) then
    begin
      lReg.WriteBool(
          'Location Type',
          lscLocationType in ExtraLocationSearchColumns);
      lReg.WriteBool(
          'Spatial Reference',
          lscSpatialReference in ExtraLocationSearchColumns);
      lReg.WriteBool('File Code', lscFileCode in ExtraLocationSearchColumns);
    end;
  finally
    lReg.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Function to copy files that match iFileName to a given path
  Returns true if all files copied OK. Returns false if there was a permissions
  error.
}
function TApplicationSettings.CopyFiles(iFileName, iPath: String): boolean;
var
  lSearchRecName: TSearchRec;
  srcFile, destFile: THandle;
  copyThisFile: boolean;
begin
  result := true; // default result
  if FindFirst(iFileName, Sysutils.faReadOnly + faArchive, lSearchRecName) = 0 then
    try
      repeat
        // default is to copy every file, but won't bother if it exists with same timestamp.
        copyThisFile := true;
        srcFile := SafeFileOpen(ExtractFilePath(iFileName) + lSearchRecName.Name, fmOpenRead);
        if FileExists(iPath + ExtractFileName(lSearchRecName.Name)) then begin
          destFile := FileOpen(iPath + ExtractFileName(lSearchRecName.Name), fmOpenRead);
          copyThisFile := FileGetDate(destFile)<>FileGetDate(srcFile);
          FileClose(destFile);
        end;
        fileClose(srcFile);
        if copyThisFile then
          try
            gpcApiCheck(CopyFile(
              PChar(ExtractFilePath(iFileName) + lSearchRecName.Name),
              PChar(iPath + ExtractFileName(lSearchRecName.Name)),
              False));
          except
            on E:EAPIError do begin
              result := false;
            end;
          end;
      until FindNext(lSearchRecName) <> 0;
    finally
      FindClose(lSearchRecName);
    end;
end;  // CopyFiles

//==============================================================================
procedure TApplicationSettings.ReadMainBandSettings(Sender:TObject;
  const AToolbar: TToolbar; const ActList: TActionList);
var slValNames: TStringList;
    i: Integer;
    lPos: TPoint;
  //----------------------------------------------------------------------------
  procedure AddToolButton(AIndex: Integer);
  var lNewButton: TXPToolButton;
      lAction: TAction;
  begin
    lNewButton := TXPToolButton.Create(AToolbar);
    if AIndex = -1 then begin
      lNewButton.Style := tbsSeparator;
      lNewButton.Width := 8;  // Set new width, as default one is too big
    end else
    if AIndex < ActList.ActionCount then begin
      lAction := TAction(ActList.Actions[AIndex]);
      if (lAction = dmFormActions.actSpeciesForPlace) or
         (lAction = dmFormActions.actMapWindow) then
      begin
        lNewButton.Style  := tbsDropDown;
        lNewButton.Width  := 39;  // Set new width, as default one is too small
        lNewButton.Action := lAction;
        // Link to event according to action.
        if lAction = dmFormActions.actSpeciesForPlace then
          lNewButton.DropDownMenu := TfrmMain(Sender).pmRecordCards
        else
          lNewButton.DropDownMenu := TfrmMain(Sender).pmMapWindow;
      end else
        lNewButton.Action := lAction;
    end else
      lNewButton.Tag := AIndex; // so we can detect the button - action link when the addin loads
    lNewButton.Parent := AToolbar;
  end;  // AddToolButton

  //----------------------------------------------------------------------------
  procedure AddAddinToolButton(AKey: String);
  var
    lIndex: Integer;
  begin
    lIndex := StrToInt(Copy(AKey, 7, 255));
    AddToolButton(lIndex + dmFormActions.StandardActionCount);
  end;

begin
  with TRegistry.Create do
    try
      // Get Simple Standard Toolbar Settings
      if OpenKey(REG_KEY_STANDARD_TOOLBAR, False) then begin
        if ValueExists('Coolbar Break') then
          frmMain.CoolBarMain.Bands.FindBand(frmMain.tbMainToolbar).Break := ReadBool('Coolbar Break');
        ReadBinaryData('Position', lPos, SizeOf(TPoint));
        AToolbar.Left   := lPos.x;
        AToolbar.Top    := lPos.y;
        ShowMainToolbar := ReadBool('Visible');

        slValNames := TStringList.Create;
        try
          for i := AToolbar.ButtonCount - 1 downto 0 do AToolbar.Buttons[i].Free;
          GetValueNames(slValNames);
          slValNames.CustomSort(IntSort);
          for i := 0 to slValNames.Count - 1 do
            if Length(slValNames[i]) < 3 then begin  // Assume no more than 99 buttons possible
              // standard buttons are stored as ints, addins as String keys
              if GetDatatype(slValNames[i]) = rdInteger then
                AddToolButton(ReadInteger(slValNames[i]))
              else
                AddAddinToolButton(ReadString(slValNames[i]));
            end;
        finally
          slValNames.Free;
        end;  // try
        CloseKey;
      end else
        for i := AToolbar.ButtonCount - 1 downto 0 do AToolbar.Buttons[i].Free;
      // Disable the action if not enough access rights
      if UserAccessLevel < ualRecorder then dmFormActions.actSpeciesForPlace.Enabled := False;
    finally
      Free;
    end;
end;  // ReadMainBandSettings

//==============================================================================
procedure TApplicationSettings.WriteMainBandSettings(const AToolbar: TToolbar);
var lReg    :TRegistry;
    iCount  :Integer;
    PosPoint:TPoint;
begin
  lReg    := TRegistry.Create;
  with lReg do
    try
      if OpenKey(REG_KEY_SETTINGS, True) then begin
        // Save Simple Standard Toolbar Settings
        DeleteKey(REG_KEY_STANDARD_TOOLBAR);
        if OpenKey(REG_KEY_STANDARD_TOOLBAR,True) then begin
          WriteBool('Coolbar Break',frmMain.CoolBarMain.Bands.FindBand(frmMain.tbMainToolbar).Break);
          PosPoint:=Point(AToolbar.Left,AToolbar.Top);
          WriteBinaryData('Position',PosPoint,SizeOf(TPoint));
          WriteBool('Visible',ShowMainToolbar);

          for iCount:=AToolbar.ButtonCount-1 downto 0 do
            if AToolbar.Buttons[iCount].Style=tbsSeparator then
              WriteInteger(IntToStr(iCount),-1)
            else if AToolbar.Buttons[iCount].Action<> nil then begin //safety check
              if TAction(AToolbar.Buttons[iCount].Action).Index<dmFormActions.StandardActionCount then
                WriteInteger(IntToStr(iCount), TAction(AToolbar.Buttons[iCount].Action).Index)
              else
                // write index of an addin to registry, so if count of actions changes we can still match up
                WriteString(IntToStr(iCount), 'Addin:' +
                             IntToStr(TAction(AToolbar.Buttons[iCount].Action).Index-
                             dmFormActions.StandardActionCount));
            end;
          Closekey;
        end;  // if lReg.OpenKey...
      end;  // if lReg.OpenKey...
    finally
      Free;
    end; // first try
end;  // WriteMainBandSettings

//==============================================================================
procedure TApplicationSettings.ReadContextBandSettings(Sender:TObject;
  const AToolbar: TToolbar);
var lReg    :TRegistry;
    PosPoint:TPoint;
begin
  lReg    := TRegistry.Create;
  with lReg do
    try
      // Get Context Sensitive Toolbar Settings
      if OpenKey(REG_KEY_CONTEXT_TOOLBAR,False) then begin
        if ValueExists('Coolbar Break') then
          frmMain.CoolBarMain.Bands.FindBand(frmMain.tbContext).Break:=ReadBool('Coolbar Break');
        ReadBinaryData('Position',PosPoint,SizeOf(TPoint));
        AToolbar.Left:=PosPoint.x;
        AToolbar.Top:=PosPoint.y;
        ShowContextToolbar:=lReg.ReadBool('Visible');
        CloseKey;
      end;  // if lReg.OpenKey...
    finally
      Free;
    end; // first try
end;  // ReadContextBandSettings

//==============================================================================
procedure TApplicationSettings.WriteContextBandSettings(const AToolbar: TToolbar);
var lReg    :TRegistry;
    PosPoint:TPoint;
begin
  lReg    := TRegistry.Create;
  with lReg do
    try
      // Save Context Sensitive Toolbar Settings
      if lReg.OpenKey(REG_KEY_CONTEXT_TOOLBAR,True) then begin
        WriteBool('Coolbar Break',frmMain.CoolBarMain.Bands.FindBand(AToolbar).Break);
        PosPoint:=Point(AToolbar.Left,AToolbar.Top);
        WriteBinaryData('Position',PosPoint,SizeOf(TPoint));
        WriteBool('Visible',ShowContextToolbar);
        CloseKey;
      end;  // if lReg.OpenKey...
    finally
      Free;
    end; // first try
end;  // WriteContextBandSettings

//==============================================================================
procedure TApplicationSettings.SetDateCutYear(const Value:Integer);
var lYear,lMonth,lDay:word;
begin
  FDateCutYear:=Value;
  DecodeDate(Date, lYear, lMonth, lDay);
  TwoDigitYearCenturyWindow:=(lYear-Value) mod 100;
end;  // SetDateCutYear

//==============================================================================
function TApplicationSettings.GetConvertor: THTMLRTFConversion;
begin
  { Create the convertor when needed, but keep it for future }
  if FConvertor=nil then
    FConvertor := THTMLRTFConversion.Create;
  Result := FConvertor;
end;

//==============================================================================
{ Accessor method for site id.  Only get this as required to avoid creation
    order problems between appsettings and general data. }
function TApplicationSettings.GetSiteID: String;
var
  lReg: TRegistry;
begin
  if FSiteID='' then begin
    //Avoid using dmGeneral.GetRecordset as potentially it could infinitely loop
    //by calling back to AppSettings.
    with dmDatabase.ExecuteSQL(Format(SQL_SELECT_SETTING, ['SiteID']), true) do begin
      try
        if (RecordCount>0) and (not (Fields['DATA'].Value = null)) then
          FSiteID := Fields['DATA'].Value
        else
        if FileExists(ExtractFilePath(Application.Exename) + 'InstallSettings.ini') then
          LoadSiteIDFromIni
        else
          raise EApplicationSettingsError.Create(ResStr_SiteIDMissing + ' - No SiteID record or ini file');
      except on E:EDatabaseError do
        raise EApplicationSettingsError.Create(ResStr_SiteIDMissing + ' - No SETTING table', E);
      end;
    end;
  end;
  Result := FSiteID;
  // Registry entry needed by osm Addins and OS Map installation tools.
  lReg := TRegistry.Create;
  with lReg do
    try
      if OpenKey(REG_KEY_SETTINGS, True) then begin
        WriteString('Site ID', FSiteID);
        CloseKey;
      end;
    finally
      Free;
    end;  // try .. finally
end;  // GetSiteID

//==============================================================================
{ Accessor method for dictionary version.  }
function TApplicationSettings.GetDictionaryVersion: String;
begin
  if FDictionaryVersion = '' then begin
    //Avoid using dmGeneral.GetRecordset as potentially it could infinitely loop
    //by calling back to AppSettings.
    with dmDatabase.ExecuteSQL(Format(SQL_SELECT_SETTING, ['Dict Seq']), true) do begin
      try
        if (RecordCount>0) and (not (Fields['DATA'].Value = null)) then
          FDictionaryVersion := Fields['DATA'].Value;
      except on E:EDatabaseError do
        raise EApplicationSettingsError.Create(ResStr_RecordMissing +
            'DB Seq - No SETTING table', E);
      end;
    end;
  end;
  Result := FDictionaryVersion;
end;  // GetDictionaryVersion

//==============================================================================
{ Accessor method for Database version.  }
function TApplicationSettings.GetDatabaseVersion: String;
begin
  if FDataBaseVersion = '' then begin
    //Avoid using dmGeneral.GetRecordset as potentially it could infinitely loop
    //by calling back to AppSettings.
    with dmDatabase.ExecuteSQL(Format(SQL_SELECT_SETTING, ['DB Seq']), true) do begin
      try
        if (RecordCount>0) and (not (Fields['DATA'].Value = null)) then
          FDatabaseVersion := Fields['DATA'].Value;
      except on E:EDatabaseError do
        raise EApplicationSettingsError.Create(ResStr_RecordMissing +
            'DB Seq - No SETTING table', E);
      end;
    end;
  end;
  Result := FDataBaseVersion;
end;  // GetDictionaryVersion



//==============================================================================
{ After initial installation, the SiteID is placed in an ini file beside the
    Recorder2000.exe.  This is because the install kit cannot update the
    database. Read this ini setting and place it in the DB so the db is
    'labelled'}
procedure TApplicationSettings.LoadSiteIDFromIni;
var
  lFileStrings: TStringlist;
begin
  lFileStrings := TStringlist.Create;
  try
    lFileStrings.LoadFromFile(ExtractFilePath(Application.Exename) + 'InstallSettings.ini');
    FSiteID := lFileStrings.Values['SiteID'];
    // place setting into DB
    dmDatabase.ExecuteSQL(Format(SQL_UPDATE_SETTING, [FSiteID, 'SiteID']), false);
  finally
    lFileStrings.Free;
  end; // try
end;

//==============================================================================
procedure TApplicationSettings.SetDragDestColour(const Value: TColor);
begin
  FDragDestColour:=Value;
  SetDragColoursEverywhere;
end;  // SetDragDestColour

//==============================================================================
procedure TApplicationSettings.SetDragSourceColour(const Value: TColor);
begin
  FDragSourceColour:=Value;
  SetDragColoursEverywhere;
end;  // SetDragSourceColour

//==============================================================================
procedure TApplicationSettings.SetDisableDragDropFrames(const Value: Boolean);
begin
  FDisableDragDropFrames := Value;
  SetDragColoursEverywhere;
end;  // SetDisableDragDropFrames

//==============================================================================
procedure TApplicationSettings.SetDragColoursEverywhere;
var liFormIndex: Integer;
begin
  if Application.MainForm<>nil then
    for liFormIndex := 0 to Screen.FormCount - 1 do
      if Screen.Forms[liFormIndex] is TBaseForm then
        TBaseForm(Screen.Forms[liFormIndex]).SetDragColours;
end;  // SetDragColoursEverywhere

//==============================================================================
procedure TApplicationSettings.SetShowToolTips(const Value: Boolean);
var liFormIndex: Integer;
begin
  FShowToolTips := Value;
  if Application.MainForm<>nil then
    for liFormIndex := 0 to Screen.FormCount - 1 do
      if Screen.Forms[liFormIndex] is TBaseForm then
        TBaseForm(Screen.Forms[liFormIndex]).ShowHint:=Value;
  if Assigned(MapBrowserWindow()) then MapBrowserWindow.ShowHint := Value;
end;  // SetShowToolTips

//==============================================================================
procedure TApplicationSettings.SetMandatoryColour(const Value: TColor);
begin
  FMandatoryColour := Value;
end;  // SetMandatoryColour

//==============================================================================
procedure TApplicationSettings.SetBackBitmapName(const Value: String);
var bmp: TBitmap;
begin
  FBackBitmapName := Value;
  // Check if the picture exists
  if not FileExists(Value) or (BackBitmapName = '') then
    PlainBackground := True
  else begin
    try
      FBackImage.LoadFromFile(FBackBitmapName);
    except
      on EInvalidGraphic do
        FBackImage.Graphic := nil;
    end;

    if FBackImage.Graphic is TJPEGImage then begin
      bmp := TBitmap.Create;
      try
         bmp.Assign(TJPEGImage(FBackImage.Graphic));
         FBackImage.Bitmap.Assign(bmp);
      finally
        bmp.Free;
      end;
    end;
  end;
  if Application.MainForm <> nil then
    InvalidateRect(Application.MainForm.ClientHandle, nil, True);
end;  // SetBackBitmapName

//==============================================================================
procedure TApplicationSettings.SetMenuIcons(const Value: Boolean);
begin
  FShowMenuIcons := Value;
  { Ask for a refresh, if frmMain present.  Also broadcast to MDI Children }
  if frmMain<>nil then begin
    PostMessage(frmMain.Handle, WM_UPDATE_MENU_ICONS, 0, 0);
    frmMain.BroadcastMessage(WM_UPDATE_MENU_ICONS);
  end;
end;  // SetMenuIcons

//==============================================================================
{ Accessor method }
procedure TApplicationSettings.SetGraduatedMenus(const Value: Boolean);
begin
  FGraduatedMenus := Value;
  { Ask for a refresh, if frmMain present.  Also broadcast to MDI Children }
  if frmMain<>nil then begin
    PostMessage(frmMain.Handle, WM_UPDATE_MENU_ICONS, 0, 0);
    frmMain.BroadcastMessage(WM_UPDATE_MENU_ICONS);
  end;
end;

//==============================================================================
// Description: Accessor method.  Also causes database to reconnect, and the
// map sheet computer ids to be updated if required.
procedure TApplicationSettings.SetUserID(const AID:TKeyString);
begin
  FUserID:=AID;
  if Assigned(dmDatabase) then
    dmDatabase.UserID := AID;
  OpenSession;
  FUserName := dmGeneralData.GetIndividualName(AId);
end;  // SetUserID

//==============================================================================
procedure TApplicationSettings.SetRecordingCards(const Value: TPopupMenu);
begin
  FRecordingCards := Value;
end;

//==============================================================================
procedure TApplicationSettings.GetRecordingCards;
var
  i: Integer;
  lSearchRec: TSearchRec;
  lNewItem: TMenuItem;
begin
  //Free existing menu items
  for i:= RecordingCards.Items.Count - 1 downto 0 do
    RecordingCards.Items.Remove(RecordingCards.Items[i]);

  //Find the first file.
  if FindFirst(RecordingCardPath + '*.crd', 0, lSearchRec) = 0 then
  begin
    if SameText(ExtractFileExt(lSearchRec.Name), '.crd') then begin
      //Add the file to the menu
      lNewItem:= TMenuItem.Create(RecordingCards);
      lNewItem.Caption:= copy(lSearchRec.Name,0,length(lSearchRec.Name) - 4);
      lNewItem.ImageIndex:= 12;
      RecordingCards.Items.Add(lNewItem);
    end;

    //Add a new menu item for each remaining file
	  while FindNext(lSearchRec) = 0 do
	    if SameText(ExtractFileExt(lSearchRec.Name), '.crd') then begin
        lNewItem:= TMenuItem.Create(RecordingCards);
        lNewItem.Caption:= copy(lSearchRec.Name,0,length(lSearchRec.Name) - 4);
        lNewItem.ImageIndex:= 12;
        RecordingCards.Items.Add(lNewItem);
      end;

    //Free the search results
	  FindClose(lSearchRec);
  end;
end;

//==============================================================================
procedure TApplicationSettings.SetRecordingCardPath(const Value: String);
begin
  FRecordingCardPath := Value;
end;

//==============================================================================
procedure TApplicationSettings.SetCustomSpeciesCardPath(const Value: String);
begin
  FCustomSpeciesCardPath := Value;
end;

//==============================================================================
procedure TApplicationSettings.SetRucksackPath(const Value: String);
begin
  FRucksackPath := Value;
end;

//==============================================================================
procedure TApplicationSettings.SetSpatialRefSystem(const Value: String);
begin
  FSpatialRefSystem := Value;
  { Ensure dataset assistor stays in synch }
  gAssistor.System := Value;
  // Update the reference system used by SpatialRefFuncs
  SetCurrentSpatialRefSystem(Value);
  // Display the reference system on the status bar
  If (frmMain <> nil) and (frmMain.Status <> nil) and
      (frmMain.Status.Panels[1] <> nil) then
    frmMain.SetReferenceSystemText;
end;  // SetSpatialRefSystem

{-------------------------------------------------------------------------------
}
procedure TApplicationSettings.SetGridRefsAsSquares(const Value: Boolean);
var
  i: Integer;
begin
  FGridRefsAsSquares := Value;
  // Refresh any displayed map. There can be multiple ones.
  if Assigned(Application.MainForm) then
    with Application.MainForm do
      for i := 0 to MDIChildCount - 1 do
        if MDIChildren[i] is TfrmMap then TfrmMap(MDIChildren[i]).RefreshMap;
end;  // SetGridRefsAsSquares

//------------------------------------------------------------------------------
procedure TApplicationSettings.SetMapFilePath(const Value: String);
begin
  FMapFilePath := Value;
end;

//------------------------------------------------------------------------------
function TApplicationSettings.GetCDDrive: String;
var
  Drive: Char;
begin
  Result := '';

  for Drive := 'A' to 'Z' do  { iterate through all possible drives }
  begin
    If GetDriveType(PChar(Drive + ':\')) = DRIVE_CDROM then
    begin
       Result := Drive + ':\';
       Break;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TApplicationSettings.SetReportPath(const Value: String);
begin
  FReportPath := Value;
  if Assigned(CRReportIndex) then
    CRReportIndex.RebuildIndex(FReportPath);
end;

//------------------------------------------------------------------------------
procedure TApplicationSettings.SetBatchUpdatePath(const Value: String);
begin
  FBatchUpdatePath := Value;
  if Assigned(BatchUpdateIndex) then
    BatchUpdateIndex.RebuildIndex(FBatchUpdatePath);
end;

//------------------------------------------------------------------------------
procedure TApplicationSettings.SetExternalFilePath(const Value: String);
begin
  FExternalFilePath := Value;
end;

//------------------------------------------------------------------------------
procedure TApplicationSettings.SetReportTemplatePath(const Value: String);
begin
  FReportTemplatePath := Value;
end;

//------------------------------------------------------------------------------
procedure TApplicationSettings.SetExportTemplatePath(const Value: String);
begin
  FExportTemplatePath := Value;
end;


{ Temporary Import databases are stored in the windows temp directory, but
    cannot be removed whilst the current DAO workspace is still open.  Therefore,
    each time the app starts up we delete the files from the last session }
procedure TApplicationSettings.CleanupOldImportDatabases;
var
  lPath: String;

  procedure DeleteFiles(const Pattern: String);
  var
    lRc: Integer;
    lSearchRec: TSearchRec;
  begin
    lRc := FindFirst(lPath + Pattern, faAnyFile, lSearchRec);
    try
      while lRc = 0 do
      begin
        DeleteFile(lPath + lSearchRec.Name);
        lRc := FindNext(lSearchRec);
      end;
    finally
      FindClose(lSearchRec);
    end;
  end;

begin
  lPath := gfnGetTempPath + TEMP_DB_FOLDER;
  if DirectoryExists(lPath) then
  begin
    DeleteFiles('*.mdb');
    DeleteFiles('*.ldb');
    DeleteFiles('IMP*.tmp');
  end;
end;

//==============================================================================
function TApplicationSettings.GetUserAccessLevelAsString: String;
begin
  case FUserAccessLevel of
    ualAdmin:    Result := ResStr_SystemManager;
    ualFullUser: IfThen(FRestrictFullEdit, ResStr_FullEditOwn, ResStr_FullEdit);
    ualAddOnly:  Result := ResStr_AddOnly;
    ualRecorder: Result := ResStr_RecordCardsOnly;
    ualReadOnly: Result := ResStr_ReadOnly;
  else
    Result := 'Unknown';
  end;
end;

//==============================================================================
function TApplicationSettings.AllowEdit(const AMode:TEditMode):Boolean;
begin
  Result := (AMode = emAdd) or ((AMode = emEdit) and (UserAccessLevel > ualAddOnly));
end;

//==============================================================================
// Description: Check if map is set up Ok.  Dataset must be present, plus a
// base map in  MAP_SHEET for this computer
// Created: Unknown
function TApplicationSettings.GetMapDatasetPresent: Boolean;
begin
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := 'Select MAP_SHEET_KEY from MAP_SHEET ' +
             'WHERE COMPUTER_ID=''' +  ComputerID + ''' AND SHEET_TYPE = 0';
    Open;
    try
      Result :=  (FileExists(MapFilePath + MAP_DS_FILENAME)) and (RecordCount > 0);
    finally
      Close;
    end;
  end ; // with
end;

//==============================================================================
procedure TApplicationSettings.SetObjectSheetFilePath(const Value: String);
begin
  FObjectSheetFilePath := Value;
end;

//==============================================================================
function TApplicationSettings.ReadSort(Sender: TBaseForm;
  const ListName: String; out FieldName: String;
  out Ascending: Boolean): Boolean;
begin
  Result := False; // default
  with TRegistry.Create do try
    if OpenKey(REG_KEY_FORMS + '\' +
               Copy(Sender.ClassName, 5, Length(Sender.ClassName) - 4), False) and
       ValueExists(ListName + ' Field') and
       ValueExists(ListName + ' Ascending') then
    begin
      FieldName := ReadString(ListName + ' Field');
      Ascending := ReadBool(ListName + ' Ascending');
      Result := True;
    end;
  finally
    Free;
  end;
end;

//==============================================================================
procedure TApplicationSettings.WriteSort(Sender: TBaseForm; const ListName,
  FieldName: String; Ascending: Boolean);
begin
  with TRegistry.Create do try
    if OpenKey(REG_KEY_FORMS + '\' +
               Copy(Sender.ClassName, 5, Length(Sender.ClassName) - 4), True) then begin
      WriteString(ListName + ' Field', FieldName);
      WriteBool(ListName + ' Ascending', Ascending);
    end;
  finally
    Free;
  end;
end;  

{-------------------------------------------------------------------------------
  Procedure that filters a screen by a given keylist. The items in the keylist
  can have hints attached or be formatted to stand out from the other items on the screen
}
procedure TApplicationSettings.DisplayDataForList(const ADataType: string;
    AItems: TKeyList; AComItems: IKeyList=nil);
var
  lComItems: IKeyList;
  lComKeyList: TCOMKeyList;
begin
  { Check for each possible related data thing and return it }
  if SameText(ADataType, 'Survey') then
    TfrmObservations(frmMain.GetForm(TfrmObservations, True)).DisplayObservations(rdSurvey, AItems)
  else if SameText(ADataType, 'Event') then
    TfrmObservations(frmMain.GetForm(TfrmObservations, True)).DisplayObservations(rdEvent, AItems)
  else if SameText(ADataType, 'Sample') then
    TfrmObservations(frmMain.GetForm(TfrmObservations, True)).DisplayObservations(rdSample, AItems)
  else if SameText(ADataType, 'Occurrence') then
    TfrmObservations(frmMain.GetForm(TfrmObservations, True)).DisplayObservations(rdOccurrence, AItems)
  else if SameText(ADataType, 'Name') then
    TfrmIndOrg(frmMain.GetForm(TfrmIndOrg, True)).DisplayNames(AItems)
  else if SameText(ADataType, 'Location') then
    TfrmLocations(frmMain.GetForm(TfrmLocations, True)).DisplayLocations(AItems)
  else if SameText(ADataType, 'Feature') then
    TfrmLocations(frmMain.GetForm(TfrmLocations, True)).DisplayFeatures(AItems)
  else if SameText(ADataType, 'Document') then
    TfrmReferences(frmMain.GetForm(TfrmReferences, True)).DisplaySources(AItems)
  else if SameText(ADataType, 'Admin') then
    TfrmAdminAreaDictBrowser(frmMain.GetForm(TfrmAdminAreaDictBrowser, True)).FilterToKeyList(AItems)
  else if SameText(ADataType, 'Taxon') then
    TfrmTaxonDictBrowser(frmMain.GetForm(TfrmTaxonDictBrowser, True)).FilterToKeyList(AItems)
  else if SameText(ADataType, 'Biotope') then
    TfrmBiotopeDictBrowser(frmMain.GetForm(TfrmBiotopeDictBrowser, True)).FilterToKeyList(AItems)
  else begin
    if Assigned(AComItems) then
      lComItems := AComItems
    else begin
      lComKeyList := TComKeyList.Create(AItems);
      lComItems := lComKeyList as IKeyList;
    end;
    TryCOMActiveFormDisplayData(ADataType, lComItems);
  end;
end;

{-------------------------------------------------------------------------------
  Try and display the requested data in the current active form.
}
procedure TApplicationSettings.TryCOMActiveFormDisplayData(
    const ADataType: WideString; const AItems: IKeyList);
var
  lDisplayDataIntf: IDisplayData;
  lHandled: boolean;
begin
  lHandled := False;
  if frmMain.ActiveMDIChild is TfrmMDIContainer then begin
    if Supports(TfrmMDIContainer(frmMain.ActiveMDIChild).OleObject,
        IID_IDisplayData, lDisplayDataIntf) then
      if lDisplayDataIntf.SupportsTable(ADataType) then begin
        lDisplayDataIntf.DisplayData(AItems);
        lHandled := True;
      end;
  end;
  if not lHandled then
    raise EApplicationSettingsError.Create(Format(ResStr_DisplayDataTypeInvalid,
        [ADataType]));
end;  // TryCOMActiveFormDisplayData

//==============================================================================
procedure TApplicationSettings.SetCanExportSystemSupplied(
  const Value: Boolean);
begin
  FCanExportSystemSupplied := Value;
end;

//==============================================================================
{ Set display common names must refresh all screens! }
procedure TApplicationSettings.SetDisplayCommonNames(const Value: Boolean);
begin
  if FDisplayCommonNames <> Value then begin
    FDisplayCommonNames := Value;
    if Assigned(dmDatabase) then
      dmDatabase.DisplayCommonNames := FDisplayCommonNames;
    frmMain.Invalidate;
  end;
end;

//==============================================================================
procedure TApplicationSettings.SetDisplayEnteredNames(const Value: Boolean);
begin
  if FDisplayEnteredNames <> Value then begin
    FDisplayEnteredNames := Value;
    // Set display entered names must refresh all screens!
    frmMain.Invalidate;
  end;
end;

//==============================================================================
procedure TApplicationSettings.SetDisplayAuthors(const Value: Boolean);
begin
  if FDisplayAuthors <> Value then begin
    FDisplayAuthors := Value;
    // Set display authors must refresh all screens!
    frmMain.Invalidate;
  end;
end;

//==============================================================================
// Accessor method
procedure TApplicationSettings.SetExpandTaxaInWizard(const Value: Boolean);
begin
  FExpandTaxaInWizard := Value;
end;
//==============================================================================
// Accessor method
//  KJG 2/12/2004
procedure TApplicationSettings.SetUseRecommendedTaxaNames(const Value: Boolean);
begin
  FUseRecommendedTaxaNames := Value;
end;

//==============================================================================
{ Accessor method }
procedure TApplicationSettings.SetTaxonomicSearchRestriction(const Value: String);
begin
  FTaxonomicSearchRestriction := Value;
end;

//==============================================================================
{ Accessor method }
procedure TApplicationSettings.SetPartialTaxonSearch(const Value: boolean);
begin
  FPartialTaxonSearch := Value;
end;

//==============================================================================
{ Accessor method }
procedure TApplicationSettings.SetAutoCompleteSearch(const Value: boolean);
begin
  FAutoCompleteSearch := Value;
end;

//==============================================================================
{ Accessor method }
procedure TApplicationSettings.SetUsePreferredTaxa(const Value: boolean);
begin
  FUsePreferredTaxa := Value;
end;

//==============================================================================
{ Accessor method }
procedure TApplicationSettings.SetIncludeLocationFileCode(
  const Value: boolean);
begin
  FIncludeLocationFileCode := Value;
end;

//==============================================================================
{ Accessor method }
procedure TApplicationSettings.SetIncludeLocationSpatialRef(
  const Value: boolean);
begin
  FIncludeLocationSpatialRef := Value;
end;

//==============================================================================
{ Accessor method }
procedure TApplicationSettings.SetRapidEntryDelimiter(const Value: String);
begin
  FRapidEntryDelimiter := Value;
end;

{ Use the API to obtain a unique identifier for the computer }
function TApplicationSettings.GetComputerID: String;
var
  len: DWORD;
begin
  len := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength (Result, len);
  gpcApiCheck(Windows.GetComputerName (PChar (Result), len));
  SetLength (Result, len);
end;


{ Description: Accessor method
  Created: 21/11/2002 }
procedure TApplicationSettings.SetAutoSchemeEmail(const Value: Boolean);
begin
  FAutoSchemeEmail := Value;
end;

procedure TApplicationSettings.SetSnapshotPath(const Value: String);
begin
  FSnapshotPath := Value;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TApplicationSettings.SetReferencesSearchKeywords(
  const Value: boolean);
begin
  FReferencesSearchKeywords := Value;
end;

// Description: Accessor method.  Causes database connection to be recreated
// Created: 2/1/2003
procedure TApplicationSettings.SetUserAccessLevel(
  const Value: TUserAccessLevel);
begin
  FUserAccessLevel := Value;
  if Assigned(dmDatabase) then
    dmDatabase.UserAccessLevel := Value;
  ApplySecurityChange;
end;

procedure TApplicationSettings.SetMapSingleDataset(const Value: Boolean);
begin
  FtfMapSingleDataset := Value;
end;

//==============================================================================
//Procedure ExportKeyList
//Description
//Exports the key list given as a parameter.
//Destination is the path that the process exports to.
//_Type is the type of export (as in cmbExportType) that will be created.
//If Destination and _Type are both non-empty and valid then the export will be
//created immediately.
//Otherwise dlgDataExport will be shown.
procedure TApplicationSettings.ExportKeyList(KeyList: TKeyList;
  Destination, _Type: String);
var
  ldlgDataExport : TdlgDataExport;
begin
  ldlgDataExport := TdlgDataExport.Create(nil, KeyList, KeyList, False);
  with ldlgDataExport do
    try
      eDestination.Text := Destination;
      cmbExportType.ItemIndex := cmbExportType.Items.IndexOf(_Type);
      if bbOk.Enabled then
        // export can proceed
        bbOKClick(bbOK)
      else
        // needs more information to proceed.  Type or destination wrong or missing
        ShowModal;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
}
procedure TApplicationSettings.UpdateMapWindowSelectors;
var
  i: Integer;
begin
  // Refresh the list of keys for Map Browsers
  AvailableMaps.Refresh;
  with frmMain do begin
    UpdateMapWindowSelector;
    for i := 0 to MDIChildCount - 1 do
      if MDIChildren[i] is TBaseForm then
        TBaseForm(MDIChildren[i]).UpdateMapWindowSelector;
  end;

  // Find the MapBrowser window, if it's there.
  if Assigned(MapBrowserWindow()) then
    if AvailableMaps.Count = 0 then MapBrowserWindow.Free
                               else MapBrowserWindow.UpdateMapWindowSelector;

  dmFormActions.actMapWindow.Enabled  := AvailableMaps.Count > 0;
  dmFormActions.actMapBrowser.Enabled := AvailableMaps.Count > 0;
end;

{-------------------------------------------------------------------------------
}
procedure TApplicationSettings.UpdateMapMenu(Sender: TForm; AMenuItem: TMenuItem;
  ASetDefault: Boolean = False; AUserClickEvent: TNotifyEvent = nil);
var
  lIdx: Integer;
  lMenu: TMenuItem;
  lClickEvent: TNotifyEvent;
begin
  if Assigned(AUserClickEvent) then lClickEvent := AUserClickEvent
                               else lClickEvent := dmFormActions.MapWindowMenuClick;
  AMenuItem.Clear;
  // Add as many items as there are initialised maps.
  with AvailableMaps do
    for lIdx := 0 to Count - 1 do begin
      lMenu := TMenuItem.Create(AMenuItem);
      // Skip the "Default" marker
      lMenu.Caption    := AvailableMaps[lIdx].DisplayName;
      lMenu.ImageIndex := 21;
      lMenu.Tag        := lIdx;
      lMenu.Default    := ASetDefault and AvailableMaps[lIdx].IsDefault;
      lMenu.OnClick    := lClickEvent;
      AMenuItem.Add(lMenu);
    end;

  AMenuItem.Enabled := AMenuItem.Count > 0;

  // Reset XPMenu to refresh sub-menus and popups.
  if Sender is TfrmMain then TfrmMain(Sender).RefreshXPMenu
  else
  if Sender is TBaseForm then TBaseForm(Sender).RefreshXPMenu;
end;

{-==============================================================================
    TAutoApplicationSettings
===============================================================================}
{-------------------------------------------------------------------------------
}
function TAutoApplicationSettings.Get_AdminAreaListKey: WideString;
begin
  Result := AppSettings.AdminAreaListKey;
end;

//==============================================================================
function TAutoApplicationSettings.Get_BiotopeListKey: WideString;
begin
  Result := AppSettings.BiotopeListKey;
end;

//==============================================================================
function TAutoApplicationSettings.Get_ResultList: IKeyList;
var
  lCOMKeyList: TCOMKeyList;
begin
  lCOMKeyList := TComKeyList.Create(AppSettings.ResultList);
  Result := lCOMKeyList as IKeyList;
end;

//==============================================================================
function TAutoApplicationSettings.Get_TaxonListKey: WideString;
begin
  Result := AppSettings.TaxonListKey;
end;

//==============================================================================
function TAutoApplicationSettings.Get_BiotopeList: IKeyList;
var
  lCOMKeyList: TCOMKeyList;
begin
  lCOMKeyList := TComKeyList.Create(AppSettings.CurrentRucksack.BiotopeList);
  Result := lCOMKeyList as IKeyList;
end;

//==============================================================================
function TAutoApplicationSettings.Get_DocumentList: IKeyList;
var
  lCOMKeyList: TCOMKeyList;
begin
  lCOMKeyList := TComKeyList.Create(AppSettings.CurrentRucksack.DocumentList);
  Result := lCOMKeyList as IKeyList;
end;

//==============================================================================
function TAutoApplicationSettings.Get_LocationList: IKeyList;
var
  lCOMKeyList: TCOMKeyList;
begin
  lCOMKeyList := TComKeyList.Create(AppSettings.CurrentRucksack.LocationList);
  Result := lCOMKeyList as IKeyList;
end;

//==============================================================================
function TAutoApplicationSettings.Get_NameList: IKeyList;
var
  lCOMKeyList: TCOMKeyList;
begin
  lCOMKeyList := TComKeyList.Create(AppSettings.CurrentRucksack.NameList);
  Result := lCOMKeyList as IKeyList;
end;

//==============================================================================
function TAutoApplicationSettings.Get_TaxonList: IKeyList;
var
  lCOMKeyList: TCOMKeyList;
begin
  lCOMKeyList := TComKeyList.Create(AppSettings.CurrentRucksack.TaxonList);
  Result := lCOMKeyList as IKeyList;
end;

//==============================================================================
function TAutoApplicationSettings.Get_RucksackFile: WideString;
begin
  Result := AppSettings.CurrentRucksack.FileName;
end;

//==============================================================================
function TAutoApplicationSettings.Get_CurrentSettings: ICurrentSettings6;
begin
  Result := Self as ICurrentSettings6;
end;

//==============================================================================
function TAutoApplicationSettings.Get_Rucksack: IRucksack;
begin
  Result := Self as IRucksack;
end;

//==============================================================================
function TAutoApplicationSettings.Get_JNCCFormat: Integer;
begin
  Result := CF_JNCCDATA;
end;

{ Function called by COM Addins when they want to call a menu option.  Input
     String could be something like 'Tools;Options...' }
procedure TAutoApplicationSettings.MenuOptionClick(
  const iItemName: WideString);
var
  idx, lCaptionIndex: Integer;
  lMenuItem: TMenuItem;
  lMenuCaptions: TStringList;
  ltfFoundItem: Boolean;
begin
  lMenuCaptions := TStringList.Create;
  try
    ParseSemiColonString(StripAmpersands(iItemName), lMenuCaptions);
    lMenuItem := nil;
    { Locate the top menu item }
    ltfFoundItem := False;
    // Look for untranslated caption.
    idx := AppSettings.ComAddins.FindMenu(lMenuCaptions[0], frmMain.mnuMain.Items);
    if idx <> -1 then
    begin
      lMenuItem := frmMain.mnuMain.Items[idx];
      ltfFoundItem := True;
    end;
    if not ltfFoundItem then
      Raise EApplicationSettingsError.CreateNonCritical(Format(ResStr_NoMenuWithName,[iItemName]));

    { Now iterate through sub-menus to find the one to click }
    for lCaptionIndex := 1 to lMenuCaptions.Count - 1 do
    begin
      ltfFoundItem := False;
      // Look for untranslated caption.
      idx := AppSettings.ComAddins.FindMenu(lMenuCaptions[lCaptionIndex], lMenuItem);
      if idx <> -1 then begin
        lMenuItem := lMenuItem.Items[idx];
        ltfFoundItem := True;
      end;
      if not ltfFoundItem then
        Raise EApplicationSettingsError.CreateNonCritical(Format(ResStr_NoMenuWithName,[iItemName]));
    end;
    lMenuItem.Click;
  finally
    lMenuCaptions.Free;
  end;
end;

{ Procedure called by com addins to filter a form by a given key list }
procedure TAutoApplicationSettings.DisplayData(const iDataType: WideString;
  const iItems: IKeyList);
var
  lKeyList: TEditableKeyList;
begin
  { Convert the COM key list into a normal one }
  lKeyList := TEditableKeyList.Create(iItems);
  try
    AppSettings.DisplayDataForList(iDataType, lKeyList, iItems);
  finally
    lKeyList.Free;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TAutoApplicationSettings.RequestData(const Requestor: IRequestor;
  const DataType: WideString);
var
  lRequestorForm: TBaseForm;
  lMapWindow: TfrmMap;
  i: Integer;
  lDataType, lMapIndex: String;
begin
  lDataType := UpperCase(DataType);
  { Find the form requesting data }
  if not (frmMain.ActiveMDIChild is TBaseForm) then
    raise EApplicationSettingsError.CreateNonCritical(ResStr_LinkSourceNotBase);
  lRequestorform := TBaseForm(frmMain.ActiveMDIChild);
  if (lDataType = 'SURVEY') or (lDataType = 'EVENT') or
     (lDataType = 'SAMPLE') or (lDataType = 'OCCURRENCE') then
    dmFormActions.DisplayForm(TfrmObservations)
  else if (lDataType = 'NAME') then
    dmFormActions.DisplayForm(TfrmIndOrg)
  else if (lDataType = 'LOCATION') then
    dmFormActions.DisplayForm(TfrmLocations)
  else if (lDataType = 'DOCUMENT') then
    dmFormActions.DisplayForm(TfrmReferences)
  else if (lDataType = 'TAXON') then
    dmFormActions.DisplayForm(TfrmTaxonDictBrowser)
  else if (lDataType = 'BIOTOPE') then
    dmFormActions.DisplayForm(TfrmBiotopeDictBrowser)
  else if (lDataType = 'ADMIN') then
    dmFormActions.DisplayForm(TfrmAdminAreaDictBrowser)
  else if (lDataType = 'TERM') then
    dmFormActions.DisplayForm(TfrmEnhancedTermLists)
  else
  if (Copy(lDataType, 1, 8) = 'MAPPOINT') or (Copy(lDataType, 1, 6) = 'MAPBOX') then
  begin
    // Change of handling, the index of the map to use is appended at the end of the
    // DataType. That means addins can use the Return Data on any maps properly now.
    if Copy(lDataType, 1, 8) = 'MAPPOINT' then i := 9
                                          else i := 7;
    lMapIndex := Trim(Copy(lDataType, i, Length(lDataType)));
    lDataType := Copy(lDataType, 1, i - 1);

    // Make sure there are maps to select from, or do nothing.
    lMapWindow := nil;
    if Get_AvailableMapCount > 0 then
      // No index means use default. Safety for addins not aware of this "feature".
      if lMapIndex = '' then
        lMapWindow := dmFormActions.DefaultMapWindow
      else begin
        // Got an index, find the corresponding map.
        i := StrToInt(lMapIndex);
        if (i >= 0) and (i < Get_AvailableMapCount) then
        lMapWindow := dmFormActions.MapWindow(Get_AvailableMap(i).Key, True);
      end;
    // All good, show the map and get on with it.
    if Assigned(lMapWindow) then begin
      lMapWindow.BringToFront;
      lMapWindow.SetFocus;
      if lDataType = 'MAPBOX' then
        lMapWindow.SelectArea;
    end else
      ShowInformation(ResStr_NoMapAvailableToRequestData);
  end;
  lRequestorForm.SetupCOMLink(TBaseForm(frmMain.ActiveMDIChild), lRequestorForm, Requestor);
  Application.BringToFront; // ensure we get focus away from addin
end;  // RequestData

{-------------------------------------------------------------------------------
  Method allows COM addins to call up a blank MDI child, and embed an ActiveX
    onto it of their own choice }
function TAutoApplicationSettings.ShowActiveForm(iForm: TGUID): IUnknown;
var
  lOleObject: IOleObject;
  lOleProxy: TOleProxy;
  lfrmMDIContainer: TfrmMDIContainer;
  i: Integer;
begin
  lfrmMDIContainer := nil;
  lOleObject := nil;
  { First look for an instance of the form }
  for i:=0 to frmMain.MDIChildCount-1 do begin
    if (frmMain.MDIChildren[i] is TfrmMDIContainer) then
      if CompareGuids(TfrmMDIContainer(frmMain.MDIChildren[i]).Guid, iForm) then begin
        lfrmMDIContainer := TfrmMDIContainer(frmMain.MDIChildren[i]);
        Break;
      end;
  end; //for
  if lfrmMDIContainer = nil then begin  // no existing form available
    { now check for existing ComObject already linked to a form }
    for i:=0 to frmMain.MDIChildCount-1 do begin
      if (frmMain.MDIChildren[i] is TBaseForm) then
        if TBaseForm(frmMain.MDIChildren[i]).GetRecorderFormEvents(iForm) <> nil then begin
          lOleObject := TBaseForm(frmMain.MDIChildren[i]).GetRecorderFormEvents(iForm) as IOleObject;
          break;
        end;
      end;
    if lOleObject = nil then
      lOleProxy := TOleProxy.Create(Application, iForm)
    else // we have an existing instance so link to that
      lOleProxy := TOleProxy.Create(Application, iForm, lOleObject);
    lfrmMDIContainer := TfrmMDIContainer.Create(Application, lOleProxy);
  end;
  lfrmMDIContainer.BringToFront;
  Result := lfrmMDIContainer.OleObject;
end;


{ COM interface to the find dialog, which only works for dictionary searches }
function TAutoApplicationSettings.Find(const iTitle, iType,
  iInitialText: WideString): WideString;
var
  lFinder: TdlgFind;
  lType: TFindType;
begin
  if SameText(iType, 'Taxon') then
    lType := ftTaxon
  else
  if SameText(iType, 'Biotope') then
    lType := ftBiotope
  else
  if SameText(iType, 'Admin') then
    lType := ftAdminArea
  else
    Raise EApplicationSettingsError.Create(ResStr_DialogSearchNotRecognised);

  lFinder := TdlgFind.CreateDialog(nil, True, iTitle, lType);
  Result  := '';
  with lFinder do
    try
      SetSearchText(iInitialText, True);
      if not eSearchText.NoSourceItems then
        if ShowModal = mrOk then
          Result := ItemKey;
    finally
      Free;
    end;
end;

{ IRecorder2000 function to generate a new key for a table.  Puts the key into
    LAST_KEY }
function TAutoApplicationSettings.GetNextKey(
  const iTableName: WideString): WideString;
begin
  Result := dmGeneralData.{IDGenerator.}GetNextKey(Uppercase(iTableName), '');
end;

{ Procedure to allow COM addins to invoke the merge of an external Access
     database into the main database, table by table
  Unzips database if it is zipped}
{Throws EFOpenError if supplied with an invalid path}
procedure TAutoApplicationSettings.MergeDatabase(const iDatabasePath: WideString);
var
  lstTempFile : String;
  lVCLUnzip : TVclUnzip;
begin
  lVCLUnzip := TVCLUnzip.Create(nil);
  try
    try
      lVCLUnzip.ZipName       := iDatabasePath;
      lVCLUnzip.DestDir       := GetWindowsTempDir;
      lVCLUnzip.OverwriteMode := Always;
      lVCLUnzip.ReadZip;
      lstTempFile             := lVCLUnzip.FileName[0];
      lVCLUnzip.DoAll         := True;
      lVCLUnzip.Unzip;
      with TDBMerger.Create(
          'Provider=Microsoft.Jet.OLEDB.4.0;Data Source='
              + IncludeTrailingPathDelimiter(lVCLUnzip.DestDir)
              + lstTempFile,
          frmMain.SetProgress,
          frmMain.SetStatus) do
        try
          DoMerge(nil, nil, nil, 0);
        finally
          Free;
        end; // try
      DeleteFile(IncludeTrailingPathDelimiter(lVCLUnzip.DestDir) + lstTempFile);
    except
      on EListError do
        with TDBMerger.Create('Provider=Microsoft.Jet.OLEDB.4.0;Data Source=' + iDatabasePath,
            frmMain.SetProgress, frmMain.SetStatus) do
          try
            DoMerge(nil, nil, nil, 0);
          finally
            Free;
          end // try    end
    end;
  finally
    lVCLUnzip.Free;
  end;
end;

//==============================================================================
{ Function to return a general purpose functions interface for COM objects
     to use }
function TAutoApplicationSettings.Get_RecorderFunctions: IRecorderFunctions; safecall;
begin
  Result := Self as IRecorderFunctions;
end;

//==============================================================================
{ Method to call the import code for an existing import database.  Displays the
     Import dialog with duplicate records allowing the user to resolve any
     issues.
     Unzips the database if it is zipped}
{Throws EFOpenError if supplied with an invalid path}
procedure TAutoApplicationSettings.CheckDatabase(const iPath,
  iDetails: WideString);
var
  ldlgDataImport: TdlgDataImport;
  lstTempFile : String;
  lVCLUnzip : TVclUnzip;
begin

  lVCLUnzip := TVCLUnzip.Create(nil);
  try
    try
      lVCLUnzip.ZipName       := iPath;
      lVCLUnzip.DestDir       := GetWindowsTempDir;
      lVCLUnzip.OverwriteMode := Always;
      lVCLUnzip.ReadZip;
      lstTempFile             := lVCLUnzip.FileName[0];
      lVCLUnzip.DoAll         := True;
      lVCLUnzip.Unzip;
      ldlgDataImport := TdlgDataImport.CreateCheckOnly(
          nil,
          IncludeTrailingPathDelimiter(lVCLUnzip.DestDir) + lstTempFile,
          iDetails);
      DeleteFile(IncludeTrailingPathDelimiter(lVCLUnzip.DestDir) + lstTempFile);
    except
      on EListError do
        ldlgDataImport := TdlgDataImport.CreateCheckOnly(nil, iPath, iDetails)
    end;
    ldlgDataImport.ShowModal;
    ldlgDataImport.Free;
  finally
    lVCLUnzip.Free;
  end;
end;

//==============================================================================
{ IRecorderFunctions method to convert supplied HTML text to RTF }
function TAutoApplicationSettings.HTMLtoRTF(
  const iHTML: WideString): WideString;
begin
  Result := Appsettings.Convertor.ConvertReadString(iHTML);
end;

//==============================================================================
function TAutoApplicationSettings.Get_UserIDKey: WideString;
begin
  Result := AppSettings.UserID;
end;

//==============================================================================
{This function queries a row in a database and returns a
 spatial reference from that row in the spatial reference sytem
 that is currently being used.}
function TAutoApplicationSettings.GetSpatialRefFromRecord(const iKeyList: IKeyList;
  const iFieldSpecifier: WideString): WideString;
var lSQLString, lPrefix: String;
begin
  if iFieldSpecifier = '' then lPrefix := ''
                          else lPrefix := iFieldSpecifier + '_';

  lSQLString := 'SELECT '
      + lPrefix + 'SPATIAL_REF, '
      + 'SPATIAL_REF_SYSTEM, '
      + lPrefix + 'LAT, '
      + lPrefix + 'LONG, '
      + lPrefix + 'SPATIAL_REF_QUALIFIER FROM '
      + iKeyList.TableName + ' WHERE '
      + dmDatabase.GetPrimaryKey(iKeyList.TableName, False) + ' = '
      + '''' + iKeyList.GetKeyItem(0).KeyField1 + '''';

  with dmGeneralData.qryAllPurpose do
  begin
    if Active then Close;
    SQL.Clear;
    SQL.Text:=lSQLString;
    try
      try
        Open;
        Result := FieldByName(lPrefix + 'SPATIAL_REF').Text;
      finally
        Close;
      end; //try..finally
    except on E:Exception do
      Result := Format(ResStr_ERRORMessage,[E.Message]);
    end;  // try..except
  end;  // with qryAllPurpose
  if Result = '' then Result := ResStr_ERROR;
end;

{-------------------------------------------------------------------------------
  This function attempts to convert the String in iSpatialRef to an ILatLong.
  Recorder 2000 attempts to identify the spatial referenbce system in use,
  where this is not possible an error is returned in ILatLong.ErrorMsg.
}
function TAutoApplicationSettings.DecodeSpatialRef(const iSpatialRef: WideString): ILatLong;
var
  lLatLong: TLatLong;
  lstSystem: String;
begin
  lstSystem := DetermineSpatialRefSystem(iSpatialRef);
  if (lstSystem <> ResStr_SystemUnknown) and (ispatialRef <> ResStr_BeyondSystemLimits) then
    try
      lLatLong := ConvertToLatLong(iSpatialRef, lstSystem);
      Result   := TAppLatLong.Create(lLatLong.Lat, lLatLong.Long, '');
    except
      // If failed to convert, trap and proceed.
      on ESpatialRefError do
        Result :=
            TAppLatLong.Create(0, 0, SpatialRefFuncs.ValidSpatialRef(iSpatialRef, LAT_LONG).Error);
    end
  else
    // use function that determines the error
    Result :=
        TAppLatLong.Create(0, 0, SpatialRefFuncs.ValidSpatialRef(iSpatialRef, LAT_LONG).Error);
end;

{-------------------------------------------------------------------------------
  Returns a 4 character code identitying the currently selected
  spatial reference syustem.
}
function TAutoApplicationSettings.Get_SpatialRefSystem: WideString;
begin
  Result:=AppSettings.SpatialRefSystem;
end;

{ Encodes a supplied lat long into a display String }
function TAutoApplicationSettings.EncodeSpatialRef(
  const iReference: ILatLong): WideString;
var
  lLatLong: TLatLong;
begin
  lLatLong.Lat  := IReference.Latitude;
  lLatLong.Long := IReference.Longitude;
  Result        := ConvertFromLatLong(lLatLong, AppSettings.SpatialRefSystem);
end;

{ Returns the system that the supplied spatial reference uses. Grid System
     Unknown returned if spatial reference unrecognised }
function TAutoApplicationSettings.IdentifySpatialRefSystem(const iSpatialRef: WideString): WideString; safecall;
begin
  Result := DetermineSpatialRefSystem(iSpatialRef);
end;

{ Return a pointer to the application settings as an IRecorderMainForm.  This
allows addins to control staus bar stuff }
function TAutoApplicationSettings.Get_RecorderMainForm: IRecorderMainForm;
begin
  Result := Self as IRecorderMainForm;
end;

{Takes a widestring parameter, which is a vague date display String.
Decodes it into a start date, end date and date type.}
function TAutoApplicationSettings.DecodeVagueDate(
  const iVagueDate: WideString): IVagueDate;
var
  lVagueDate:TVagueDate;
begin
  try
    lVagueDate:=VagueDate.StringToVagueDate(iVagueDate);
    Result:=TAppVagueDate.Create(lVagueDate.StartDate,
            lVagueDate.EndDate, lVagueDate.DateTypeString, '');
  except
  on E: EVagueDateError do
    Result:=TAppVagueDate.Create(0,0,'','ERROR: ' + E.Message);
  end;
end;

{Takes an IKeyList parameter which identoifies  the record for
which a date String should be returned. A second parameter identified the prefix fo the field requried when 2 or more date fields are
 presemt in the record. This function only returned vague dates from records.
 For exampel. wjem a surveu event recored is queried it is necessafy to specify
 'from' or 'to' in the specifier to identify which date is required.

 Dates returened are strings in a suitable format for display e,
 e.g `Spring 1985'.}
function TAutoApplicationSettings.GetDateFromRecord(
  const iKeyList: IKeyList; const iFieldSpecifier: WideString): WideString;
var
  lSQLString:String;
  lPrefix:String;
begin
  if iFieldSpecifier = '' then lPrefix := ''
                          else lPrefix := iFieldSpecifier + '_';
  lSQLString := 'SELECT '
      + lPrefix + 'VAGUE_DATE_START, '
      + lPrefix + 'VAGUE_DATE_END, '
      + lPrefix + 'VAGUE_DATE_TYPE FROM '
      + iKeyList.TableName + ' WHERE '
      + dmDatabase.GetPrimaryKey(iKeyList.TableName, False) + ' = '
      + '''' + iKeyList.GetKeyItem(0).KeyField1 + '''';

  With dmGeneralData.qryAllPurpose do
  begin
    if Active then Close;
    SQL.Clear;
    SQL.Text := lSQLString;
    try
      try
        Open;
        Result := dmGeneralData.qryAllPurpose.FieldByName(lPrefix + 'VAGUE_DATE_START').Text;
      finally
        Close;
      end; //try..finally
    except
      on E:Exception do
        Result := 'ERROR: ' + E.Message;
    end;//try..except
  end;//with

  if Result = '' then Result := ResStr_ERROR
end;

//==============================================================================
function TAutoApplicationSettings.EncodeVagueDate(const iVagueDate: IVagueDate): WideString;
var
  lVagueDate: TVagueDate;
begin
  lVagueDate.StartDate      := iVagueDate.StartDate;
  lVagueDate.EndDate        := iVagueDate.EndDate;
  lVagueDate.DateTypeString := iVagueDate.DateType;
  Result := VagueDate.VagueDateToString(lVagueDate);
  if Result = '' then Result := 'ERROR';
end;

//==============================================================================
function TAutoApplicationSettings.Get_Progress: Integer;
begin
  if FProgressBarReferences = 0 then
    Result := frmMain.ProgressBar.Position
  else
    Result := frmMain.COMProgressbar.Position;
end;  // Get_Progress

//==============================================================================
function TAutoApplicationSettings.Get_StatusText: WideString;
begin
  Result := frmMain.Status.Panels[0].Text;
end;  // Get_StatusText

//==============================================================================
procedure TAutoApplicationSettings.Set_Progress(Value: Integer);
begin
  frmMain.SetCOMProgress(Value);
end;  // Set_Progress

//==============================================================================
procedure TAutoApplicationSettings.Set_StatusText(const Value: WideString);
begin
  frmMain.SetStatus(Value, False);
  frmMain.Status.Repaint; // force immediate refresh
end;  // Set_StatusText

//==============================================================================
procedure TAutoApplicationSettings.StartProgressBar;
begin
  { Reference count the number of addins that want a progress bar, because
     they get the same one }
  if FProgressBarReferences = 0 then
    frmMain.EnableCOMProgressBar;
  Inc(FProgressBarReferences);
end;  // StartProgressBar

//==============================================================================
procedure TAutoApplicationSettings.StopProgressBar;
begin
  if FProgressBarReferences > 0 then
  begin
    Dec(FProgressBarReferences);
    if FProgressBarReferences = 0 then
      frmMain.DisableCOMProgressBar;
  end;
end;  // StopProgessBar

//==============================================================================
procedure TAutoApplicationSettings.Initialize;
begin
  inherited;
  FProgressBarReferences := 0;
  FFirstDisplayDataItem := '';
end;  // Create

//==============================================================================
function TAutoApplicationSettings.Get_CanExportSystemSupplied: WordBool;
begin
  Result := AppSettings.CanExportSystemSupplied;
end;  // Get_CanExportSystemSupplied

//==============================================================================
procedure TAutoApplicationSettings.Set_CanExportSystemSupplied(
  Value: WordBool);
begin
  AppSettings.CanExportSystemSupplied := Value;
end;  // Set_CanExportSystemSupplied

//==============================================================================
{ Return an IRecorderMap instance for Addins }
function TAutoApplicationSettings.Get_RecorderMap: IRecorderMap;
begin
  Result := Self as IRecorderMap;
end;

{ Return the application's version stamp }
function TAutoApplicationSettings.Get_Version: WideString;
begin
  Result := GetFileVersion;
end;

{ Return the zoom scale of the map.  See COM documentation for more info }
function TAutoApplicationSettings.Get_Scale: WideString;
begin
  if frmMain.ActiveMDIChild is TfrmMap then
    Result := TfrmMap(frmMain.ActiveMDIChild).ThreePoints
  else
    Result := '';
end;

{ Pan the map to the supplied spatial reference }
procedure TAutoApplicationSettings.PanMap(const iSpatialReference: WideString);
begin
  TfrmMap.CentreOnRef(iSpatialReference, DetermineSpatialRefSystem(iSpatialReference));
end;

//==============================================================================
{ Return system of current base map to requesting addin }
function TAutoApplicationSettings.Get_BaseMapRefSystem: WideString; safecall;
begin
  if AppSettings.AvailableMaps.Count > 0 then
    Result := AppSettings.AvailableMaps.DefaultMap.SpatialSystem
  else
    Result := '';
end;

//==============================================================================
{ Refresh the map and the loaded sheets (by looking for changes in MAP_SHEET)
      when requested by an addin.  Return False if the map isn't visible }
function TAutoApplicationSettings.RefreshSheets: WordBool;
var
  lMapWindow: TfrmMap;
begin
  lMapWindow := dmFormActions.DefaultMapWindow;
  if Assigned(lMapWindow) then begin
    lMapWindow.RefreshMapSheets;
    Result := True;
  end else
    Result := False;
end;

//==============================================================================
function TAutoApplicationSettings.AccessSQLtoSQLServerSQL(const iSQLStatement: WideString): WideString;
begin
  Result := ConvertSQL(iSQLStatement, ctSQLServer);
end;

//==============================================================================
// Obtains an appropriate connection String for addins to use
function TAutoApplicationSettings.Get_ConnectionString: WideString;
begin
  Result := dmDatabase.GetNewConnectionString;
end;

//==============================================================================
procedure TAutoApplicationSettings.SetApplicationSecurity(
  const Connection: IInterface);
begin
  if (Connection as _Connection).State<>adStateOpen then
    raise EApplicationSettingsError.Create(ResStr_AddinClosedConnection);
  dmDatabase.SetApplicationRole(Connection as _Connection);
end;

//==============================================================================
//==============================================================================
procedure TAutoApplicationSettings.ExportKeyList(const KeyList: IKeyList;
  const Destination, _Type: WideString);
var lEditableKeyList : TEditableKeyList;
begin
  lEditableKeyList := TEditableKeyList.Create(KeyList);
  try
    AppSettings.ExportKeyList(lEditableKeyList, Destination, _Type)
  finally
    lEditableKeyList.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Description : Returns the Report filter results screen interface, or nil
              if it doesn't exist.
  Created : 07/03/2003 }
function TAutoApplicationSettings.Get_ReportResults: IReportResults; safecall;
var
  lfrmFilterResult : TForm;
begin
  lfrmFilterResult := dmFormActions.GetFormInstance(TfrmFilterResult);
  if assigned(lfrmFilterResult) then
    Result := lfrmFilterResult as IReportResults
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------
  Description : Implement GetAvailableXmlReports for IRecorder6
  Created : 23/10/2007 }
function TAutoApplicationSettings.GetAvailableXmlReports(const AKeytype: WideString): IXmlReportList;
var
  lReportPath: String;
begin
  if AppSettings.ReportPath = '' then
    lReportPath := ExtractFilePath(Application.ExeName)
  else
    lReportPath := AppSettings.ReportPath;

  Result := TXmlReportList.Create(lReportPath, AKeyType) as IXmlReportList;
end;

//==============================================================================
//Function: GetFilterItems
//Description: Returns a list of the Taxon and Biotope occurrence keys that pass
//through the export filter specified in ExportFilterKey.
//Raises EExportFilterError if the parameter isn't a valid Export Filter key
function TApplicationSettings.GetFilterItems(ExportFilterKey: String): TKeyList;
var
  lExportFilter : TExportFilter;
  lEditableKeyList : TEditableKeyList;
  i : Integer;
begin
  lExportFilter := TExportFilter.CreateFromDatabase(ExportFilterKey);
  lEditableKeyList := TEditableKeyList.Create();
  try
    //0 will certainly be before the first record in the database
    lEditableKeyList.Assign(lExportFilter.KeyList(0));
    for i := lEditableKeyList.Header.ItemCount -1 downto 0 do
    //remove everything except taxon and biotope occurrences
      if (CompareText(lEditableKeyList.ItemTable[i], TN_BIOTOPE_OCCURRENCE) <> 0) and
         (CompareText(lEditableKeyList.ItemTable[i], TN_TAXON_OCCURRENCE) <> 0) then
        lEditableKeyList.DeleteItem(i);
    Result := lEditableKeyList;
  finally
    lExportFilter.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Description : Implement GetFilterItems for IRecorderFunctions
  Created : 20/02/2003 }
function TAutoApplicationSettings.GetFilterItems(const ExportFilterKey: WideString): IKeyList;
var
  lKeyList: TKeyList;
begin
  lKeyList := AppSettings.GetFilterItems(ExportFilterKey);
  Result   := TComKeyList.Create(lKeyList);
  lKeyList.Free;
end;

function TAutoApplicationSettings.Get_DisableDragDropFrames: WordBool;
begin
  Result := AppSettings.DisableDragDropFrames;
end;

function TAutoApplicationSettings.Get_PartialTaxonSearch: WordBool;
begin
  Result := AppSettings.PartialTaxonSearch;
end;

function TAutoApplicationSettings.Get_DragDestinationColour: Integer;
begin
  Result := AppSettings.DragDestColour;
end;

function TAutoApplicationSettings.Get_DragSourceColour: Integer;
begin
  Result := AppSettings.DragSourceColour;
end;

function TAutoApplicationSettings.Get_MandatoryColour: Integer;
begin
  Result := AppSettings.MandatoryColour;
end;

function TAutoApplicationSettings.Get_AvailableMap(Index: Integer): IAvailableMap;
begin
  Result := TComAvailableMap.Create(AppSettings.AvailableMaps[Index]) as IAvailableMap;
end;

function TAutoApplicationSettings.Get_AvailableMapCount: Integer;
begin
  Result := AppSettings.AvailableMaps.Count;
end;

function TAutoApplicationSettings.Get_DefaultMap: IAvailableMap;
begin
  Result := TComAvailableMap.Create(AppSettings.AvailableMaps.DefaultMap) as IAvailableMap;
end;

{-------------------------------------------------------------------------------
  Retrieve the current session ID
}
function TAutoApplicationSettings.Get_SessionID: WideString;
begin
  Result := AppSettings.SessionID;
end;


{-------------------------------------------------------------------------------
  Request data that is being supplied by a COM form.  This is different to
    normal because a GUID is supplied which determines the form to display to
    get data from.
}
function TAutoApplicationSettings.RequestCOMData(
  const ARequestor: IRequestor; ASupplierGUID: TGUID): IUnknown;
var
  lRequestorForm: TBaseForm;
begin
  { Find the form requesting data }
  if not (frmMain.ActiveMDIChild is TBaseForm) then
    raise EApplicationSettingsError.CreateNonCritical(ResStr_LinkSourceNotBase);

  lRequestorform := TBaseForm(frmMain.ActiveMDIChild);
  Result         := ShowActiveForm(ASupplierGUID);

  lRequestorForm.SetupCOMLink(
      TBaseForm(frmMain.ActiveMDIChild),
      lRequestorForm,
      ARequestor);
end;

{-------------------------------------------------------------------------------
  Allow addins to display a specific item in a list of a screen, using SQL
}
procedure TAutoApplicationSettings.DisplayDataFromSQL(const ADataType,
  ASQL: WideString);
var
  lKeyList: TEditableKeyList;
  lDataType: string;
  lKeyField2: string;
begin
  if ASQL = '' then Exit;

  FFirstDisplayDataItem := '';
  lKeyList := TEditableKeyList.Create;
  // convert the DataType into one compatible with the DisplayData method
  if SameText(ADataType, 'Taxon_Occurrence') or SameText(ADataType, 'Biotope_Occurrence') then
  begin
    lDataType := 'Occurrence';
    lKeyList.SetTable(MIXED_DATA);
    // convert the data type to the table name
    lKeyField2 := StringReplace(ADataType, ' ', '_', []);
  end
  else begin
    lDataType := ADataType;
    lKeyField2 := '';
  end;

  with dmDatabase.ExecuteSQL(ASQL, True) do begin
    if Fields.Count<>1 then
      raise EApplicationSettingsError.CreateNonCritical(ResStr_DisplayDataTooManyFields);

    CheckDisplayDataType(ADataType, Fields[0].Name);
    if not (EOF and BOF) then begin
      MoveFirst;
      // remember first item in list, in case user has not specified item sql
      // in call to displayTab
      FFirstDisplayDataItem := VarToStr(Fields[0].Value);
    end else
      ShowInformation(ResStr_NoListRecords);

    while not EOF do begin
      if not VarIsEmpty(Fields[0].Value) then
        lKeyList.AddItem(VarToStr(Fields[0].Value), lKeyField2);
      MoveNext;
    end;
  end;

  try
    if FFirstDisplayDataItem<>'' then
      AppSettings.DisplayDataForList(lDataType, lKeyList);
  finally
    lKeyList.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Checks that when the addin supplies a key field using SQL to
      DisplayDataFromSQL or DisplayTab
}
procedure TAutoApplicationSettings.CheckDisplayDataType(const AType, AFieldName: string);

    procedure CheckFieldName(const AExpected: string);
    begin
      if CompareText(AExpected, AFieldName)<>0 then
        raise EApplicationSettingsError.Create(Format(ResStr_InvalidDisplayDataKeyField,
            [AFieldName, AExpected]));
    end;

begin
  // only check values that are supposedly keys
  if SameText(RightStr(AFieldName, 4), '_key') then
  begin
    if SameText(AType, 'Survey') or
       SameText(AType, 'Sample') or
       SameText(AType, 'Taxon_Occurrence') or
       SameText(AType, 'Biotope_Occurrence') or
       SameText(AType, 'Name') or
       SameText(AType, 'Location') then
      CheckFieldName(AType+'_Key')
    else
    if SameText(AType, 'Event') then
      CheckFieldName('Survey_Event_Key')
    else
    if SameText(AType, 'Feature') then
      CheckFieldName('Location_Feature_Key')
    else
    if SameText(AType, 'Document') then
      CheckFieldName('Source_Key')
    else
    if SameText(AType, 'Admin') then
      CheckFieldName('Admin_Area_Key')
    else
    if SameText(AType, 'Taxon') then
      CheckFieldName('Taxon_List_Item_Key')
    else
    if SameText(AType, 'Biotope') then
      CheckFieldName('Biotope_List_Item_Key');
  end;
end;

{-------------------------------------------------------------------------------
  Allow addins to display a specific tab on a specific screen, for a specific
     node in a specific list.
}
procedure TAutoApplicationSettings.DisplayTab(const ADataType, ASQL,
  AItemSQL, ATab: WideString; AExpand, AExpandRecurse: WordBool);
var
  lNode: TFlyNode;
  lFieldKey: string;
begin
  DisplayDataFromSQL(ADataType, ASQL);
  if frmMain.ActiveMDIChild is TBaseChild then
    if AExpandRecurse and not AExpand then
    // We must expand all parent nodes
      TBaseChild(frmMain.ActiveMDIChild).ExpandAllNodes;
  if Trim(AItemSQL)<>'' then
    with dmDatabase.ExecuteSQL(AItemSQL, True) do begin
      if BOF and EOF then begin
        ShowInformation(ResStr_NoItemRecord);
        Exit;
      end;
      movefirst;
      if Fields.Count<>1 then begin
        ShowInformation(ResStr_DisplayDataTooManyFields);
        Exit;
      end;
      lFieldKey := VarToStr(Fields[0].Value);
    end
  else
    lFieldKey := FFirstDisplayDataItem;
  lNode := nil;
  if frmMain.ActiveMDIChild is TBaseChild then begin
    try
      lNode := TBaseChild(frmMain.ActiveMDIChild).SelectNode(ADataType, lFieldKey)
    except
      on Exception do begin
        ShowInformation(ResStr_ItemNotFound);
        lNode := nil;
      end;
    end;
    if Trim(ATab)<>'' then
      if not TBaseChild(frmMain.ActiveMDIChild).SelectTab(Trim(ATab)) then
        ShowInformation(Format(ResStr_DisplayTabNotFound, [ATab]));
  end;
  if AExpand and Assigned(lNode) then
    lNode.Expand(AExpandRecurse);
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TApplicationSettings.SetFileImportTypeIndex(const Value: integer);
begin
  FFileImportTypeIndex := Value;
end;

{-------------------------------------------------------------------------------
  Accessor
}
function TApplicationSettings.GetTaxonListKey: TKeyString;
begin
  Result := FTaxonListKey;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TApplicationSettings.SetTaxonListKey(const Value: TKeyString);
begin
  FTaxonListKey := Value;
end;

//==============================================================================
// Checks UserAccessLevel before allowing export of confidential data.
function TApplicationSettings.GetExportConfidentialOccurrences: Boolean;
begin
  Result := FExportConfidentialOcc and (UserAccessLevel = ualAdmin);
end;

//==============================================================================
function TApplicationSettings.GetUseOriginalIcons: Boolean;
begin
  Result := FUseOriginalIcons;
end;

{-------------------------------------------------------------------------------
  Populates the log out time in Session.
}
procedure TApplicationSettings.CloseSession;
var
  lCmd: TADOCommand;
begin
  if FSessionID<>'' then begin
    lCmd := TADOCommand.Create(nil);
    try
      //Avoid using dmGeneral.GetRecordset as potentially it could infinitely loop
      //by calling back to AppSettings.
      with lCmd do begin
        Connection := dmDatabase.Connection;
        CommandType := cmdStoredProc;
        CommandText := 'usp_Session_Close';
        Parameters.CreateParameter('@SessionID', ftString, pdInput, 16, FSessionID);
        Execute;
      end;
    finally
      lCmd.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Creates a Session record and sets FSessionID.
}
procedure TApplicationSettings.OpenSession;
var
  lCmd: TADOCommand;
begin
  lCmd := TADOCommand.Create(nil);
  try
    //Avoid using dmGeneral.GetRecordset as potentially it could infinitely loop
    //by calling back to AppSettings.
    with lCmd do begin
      Connection      := dmDatabase.Connection;
      CommandType     := cmdStoredProc;
      CommandTimeOut  := 0;
      CommandText     := 'usp_Session_Insert';
      ExecuteOptions := [eoExecuteNoRecords];
      Parameters.CreateParameter('@Key', ftString, pdOutput, 16, FSessionID);
      Parameters.CreateParameter('@UserID', ftString, pdInput, 16, UserID);
      Execute;
      FSessionID := lCmd.Parameters.ParamByName('@Key').Value;
      dmDatabase.SessionID := FSessionID;
    end;
  finally
    lCmd.Free;
  end;
end;  // TCollectionsModuleSettings.OpenSession

{-------------------------------------------------------------------------------
}
procedure TApplicationSettings.SetSessionID(const Value: string);
begin
  FSessionID := Value;
  dmDatabase.SessionID := FSessionID;
  FSessionStart := Now;
end;

{-------------------------------------------------------------------------------
}
function TApplicationSettings.GetExternalFilters: TStringList;
begin
  Result := FExternalFilters;
end;

{-------------------------------------------------------------------------------
}
procedure TApplicationSettings.AddFilteredRecord(const tableName, key, hint: String);
var
  idx: Integer;
begin
  // find the filter for this table, or create one if not in existance
  idx := FExternalFilters.IndexOf(tableName);
  if idx < 0 then
    idx := FExternalFilters.AddObject(UpperCase(tableName), TStringList.Create);
  // make sure the entry exists for this key, and the hint is set correctly
  // concatenating them if there are 2 or more hints
  with TStringList(FExternalFilters.Objects[idx]) do
  begin
    if IndexOfName(key) = -1 then
      Add(key + '=' + hint)
    else
    if (hint <> '') and (Pos(hint, Values[key]) = 0) then
      Values[key] := Values[key] + IfThen((Values[key]=''), '', #13#10) + hint;
  end;
end;

{-------------------------------------------------------------------------------
  If the user access level changes, inform all the screens.
}
procedure TApplicationSettings.ApplySecurityChange;
var
  i: Integer;
begin
  if Assigned(frmMain) then begin
    frmMain.ApplySecurity;
    for i := 0 to frmMain.MDIChildCount - 1 do
      if frmMain.MDIChildren[i] is TBaseChild then
        TBaseChild(frmMain.MDIChildren[i]).ApplySecurity;
  end;
  if Assigned(dmFormActions) then
    dmFormActions.ApplySecurity;
end;

{-------------------------------------------------------------------------------
}
function TApplicationSettings.IndexOfFilteredRecord(const tableName, key: String): Integer;
var
  idx: Integer;
begin
  idx := FExternalFilters.IndexOf(tableName);
  if idx < 0 then
    Result := -1
  else
    Result := TStringList(FExternalFilters.Objects[idx]).IndexOfName(key);
end;

{-------------------------------------------------------------------------------
}
function TApplicationSettings.GetFilteredRecords(const tableName: String): TStringList;
var
  idx: Integer;
begin
  idx := FExternalFilters.IndexOf(tableName);
  if idx < 0 then
    Result := nil
  else
    Result := TStringList(FExternalFilters.Objects[idx]);
end;

{-------------------------------------------------------------------------------
}
function TApplicationSettings.GetFilteredRecordHint(const tableName, key: String): String;
var
  idx: Integer;
begin
  idx := FExternalFilters.IndexOf(tableName);
  if idx < 0 then
    Result := ''
  else
    Result := TStringList(FExternalFilters.Objects[idx]).Values[key];
end;

{-------------------------------------------------------------------------------
}
procedure TApplicationSettings.ClearAllFilteredRecords;
begin
  if Assigned(FExternalFilters) then begin
    while FExternalFilters.Count > 0 do begin
      FExternalFilters.Objects[0].Free;
      FExternalFilters.Delete(0);
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TApplicationSettings.ClearFilteredRecords(tableNames: Array of String);
var
  i, idx: Integer;
begin
  for i := 0 to High(tableNames) do begin
    idx := FExternalFilters.IndexOf(tableNames[i]);
    if idx > -1 then begin
      FExternalFilters.Objects[idx].Free;
      FExternalFilters.Delete(idx);
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TApplicationSettings.DisplayFilteredData;
begin
  if (FExternalFilters.IndexOf(TN_SURVEY) > -1) or
     (FExternalFilters.IndexOf(TN_SURVEY_EVENT) > -1) or
     (FExternalFilters.IndexOf(TN_SAMPLE) > -1) or
     (FExternalFilters.IndexOf(TN_TAXON_OCCURRENCE) > -1) or
     (FExternalFilters.IndexOf(TN_BIOTOPE_OCCURRENCE) > -1) then
    TfrmObservations(frmMain.GetForm(TfrmObservations, True)).DisplayFilteredObservations;

  if (FExternalFilters.IndexOf(TN_NAME) > -1) or
     (FExternalFilters.IndexOf(TN_INDIVIDUAL) > -1) or
     (FExternalFilters.IndexOf(TN_ORGANISATION) > -1) then
    TfrmIndOrg(frmMain.GetForm(TfrmIndOrg, True)).DisplayFilteredNames;

  if (FExternalFilters.IndexOf(TN_LOCATION) > -1) or
     (FExternalFilters.IndexOf(TN_LOCATION_FEATURE) > -1) then
    TfrmLocations(frmMain.GetForm(TfrmLocations, True)).DisplayFilteredLocations;

  if (FExternalFilters.IndexOf(TN_REFERENCE) > -1) then
    TfrmReferences(frmMain.GetForm(TfrmReferences, True)).DisplayFilteredSources;

  if (FExternalFilters.IndexOf(TN_ADMIN_AREA) > -1) then
    TfrmAdminAreaDictBrowser(frmMain.GetForm(TfrmAdminAreaDictBrowser, True)).DisplayFilteredItems;

  if (FExternalFilters.IndexOf(TN_TAXON_LIST_ITEM) > -1) then
    TfrmTaxonDictBrowser(frmMain.GetForm(TfrmTaxonDictBrowser, True)).DisplayFilteredItems;

  if (FExternalFilters.IndexOf(TN_BIOTOPE_LIST_ITEM) > -1) then
    TfrmBiotopeDictBrowser(frmMain.GetForm(TfrmBiotopeDictBrowser, True)).DisplayFilteredItems;
end;

{-------------------------------------------------------------------------------
}
procedure TApplicationSettings.SetHasUncommittedUpdates(const Value: Boolean);
begin
  FHasUncommittedUpdates := Value;
  with dmFormActions do begin
    SetActionVisibility(actCommitUpdates, Value);
    SetActionVisibility(actCancelUpdates, Value);
  end;

  // Note we only change the user access levels in the AppSettings & screens,
  // not in the connection. This is because it may not be possible to close
  // the connection if certain screens are left open.
  if Value then begin
    FUserAccessLevel := ualReadOnly;
    dmDatabase.ExecuteSQL(SQL_READUNCOMMITTED, False);
  end else begin
    FUserAccessLevel := LoginUserAccessLevel;
    dmDatabase.ExecuteSQL(SQL_READCOMMITTED, False);
  end;
  ApplySecurityChange;
end;

{-------------------------------------------------------------------------------
  Accessor method for last searched rucksack
}
function TApplicationSettings.GetLastSearchedRucksack: TLastSearchedRucksack;
begin
  if not Assigned(FLastSearchedRucksack) then
    FLastSearchedRucksack := TLastSearchedRucksack.Create;
  Result := FLastSearchedRucksack;
end;

{-------------------------------------------------------------------------------
  Accessor method for obtaining user access level associated with the login,
    which may be different to the current applied user access level if there
    is a batch update under review.
}
procedure TApplicationSettings.SetLoginUserAccessLevel(const Value: TUserAccessLevel);
begin
  FLoginUserAccessLevel := Value;
  // this is set by the login dialog, so initially the applied user access
  // level is the same
  UserAccessLevel := Value;
end;

{-------------------------------------------------------------------------------
  Determines whether the access to condifidential data is allowed, based on the
  CondifentialAccessLevel and ConfidentialFullEdit values.
}
function TApplicationSettings.ConfidentialAccessGranted(accessLevel: TUserAccessLevel): Boolean;
begin
  if (accessLevel = ualFullUser) and (ConfidentialAccessLevel = ualFullUser) then
    Result := not (RestrictFullEdit and ConfidentialFullEdit)
  else
    Result := accessLevel >= ConfidentialAccessLevel;
end;

{-------------------------------------------------------------------------------
  Updates the settings for confidential access.
}
procedure TApplicationSettings.SetConfidentialAccess(accessLevel: TUserAccessLevel;
  fullEditUser: Boolean);
begin
  FConfidentialAccessLevel := accessLevel;
  FConfidentialFullEdit    := fullEditUser;
end;

//==============================================================================
{ TRucksack }
//==============================================================================
procedure TRucksack.Assign(iRucksack: TRucksack);
begin
  FBiotopeList.Assign(iRucksack.BiotopeList);
  FDocumentList.Assign(iRucksack.DocumentList);
  FLocationList.Assign(iRucksack.LocationList);
  FNameList.Assign(iRucksack.NameList);
  FTaxonList.Assign(iRucksack.TaxonList);
  FTaxonSearchCodeList.Assign(iRucksack.TaxonSearchCodeList);
end;

//==============================================================================
constructor TRucksack.Create;
begin
  inherited Create;
  FTaxonList := TEditableKeyList.Create;
  FTaxonList.SetTable(TN_TAXON_LIST_ITEM);
  FBiotopeList := TEditableKeyList.Create;
  FBiotopeList.SetTable(TN_BIOTOPE_LIST_ITEM);
  FLocationList := TEditableKeyList.Create;
  FLocationList.SetTable(TN_LOCATION);
  FNameList := TEditableKeyList.Create;
  FNameList.SetTable(TN_NAME);
  FDocumentList := TEditableKeyList.Create;
  FDocumentList.SetTable(TN_REFERENCE);
  FTaxonSearchCodeList := TEditableKeyList.Create;
  FTaxonSearchCodeList.SetTable(TN_TAXON_LIST_ITEM);
end;

//==============================================================================
destructor TRucksack.Destroy;
begin
  FreeAndNil(FTaxonList);
  FreeAndNil(FBiotopeList);
  FreeAndNil(FLocationList);
  FreeAndNil(FNameList);
  FreeAndNil(FDocumentList);
  FreeAndNil(FTaxonSearchCodeList);
  inherited Destroy;
end;

//==============================================================================
function TRucksack.Get_BiotopeList: TEditableKeyList;
begin
  Result := FBiotopeList;
end;

//==============================================================================
function TRucksack.Get_DocumentList: TEditableKeyList;
begin
  Result := FDocumentList;
end;

//==============================================================================
function TRucksack.Get_LocationList: TEditableKeyList;
begin
  Result := FLocationList;
end;

//==============================================================================
function TRucksack.Get_NameList: TEditableKeyList;
begin
  Result := FNameList;
end;

//==============================================================================
function TRucksack.Get_TaxonList: TEditableKeyList;
begin
  Result := FTaxonList;
end;

//==============================================================================
function TRucksack.Get_TaxonSearchCodeList: TEditableKeyList;
begin
  Result := FTaxonSearchCodeList;
end;

//==============================================================================
procedure TRucksack.Set_AppSettings(
  const iAppSettings: TApplicationSettings);
begin
  FAppSettings := iAppSettings;
  UpdateAppSettings;
end;

//==============================================================================
procedure TRucksack.Set_BiotopeList(const iBiotopeList: TEditableKeyList);
begin
  FBiotopeList := iBiotopeList;
  UpdateAppSettings;
end;

//==============================================================================
procedure TRucksack.Set_DocumentList(
  const iDocumentList: TEditableKeyList);
begin
  FDocumentList := iDocumentList;
  UpdateAppSettings;
end;

//==============================================================================
procedure TRucksack.Set_LocationList(
  const iLocationList: TEditableKeyList);
begin
  FLocationList := iLocationList;
  UpdateAppSettings;
end;

//==============================================================================
procedure TRucksack.Set_NameList(const iNameList: TEditableKeyList);
begin
  FNameList := iNameList;
  UpdateAppSettings;
end;

//==============================================================================
procedure TRucksack.Set_TaxonList(const iTaxonList: TEditableKeyList);
begin
  FTaxonList := iTaxonList;
  UpdateAppSettings;
end;

//==============================================================================
procedure TRucksack.Set_TaxonSearchCodeList(const Value: TEditableKeyList);
begin
  FTaxonSearchCodeList := Value;
  UpdateAppSettings;
end;

//==============================================================================
procedure TRucksack.UpdateAppSettings;
begin
  FAppSettings.CurrentRucksack.Assign(Self);
end;

//==============================================================================
{ TNewAutoObjectFactory }
//==============================================================================

function TNewAutoObjectFactory.CreateComObject(const Controller: IUnknown): TComObject;
begin
  if FComObject = nil then
    FComObject := inherited CreateComObject(Controller);
  Result := FComObject;
end;

(**
 * Check if reg setting exists. If not, or reg setting points to missing folder, then try the following paths to find it:
 *
 * (User Files only) User's common documents folder, /Recorder 6/...
 * User's programdata (common data) folder /Recorder 6/...
 * Direct under the RecorderApp.exe location
 *
 * Actual folders checked depend on the pathType parameter. If not found but defaultPath is provided create the folder, else
 * raises an exception.
 * optional - if true then no exception is raised if the folder is missing. A valid use of this is
 * for any folders that are left on the install CD like the dictionary images.
 * canCreate - if true, then will create the directory in the preferred location if it does
 * not exist. If false, then will scan for the directory in other possible locations.
 *)
function TApplicationSettings.FindRegPath(const iRegKey, iSuggestedFolder: string; iReg: TRegistry;
    pathType: TFilePathType; filesToCheck: array of string; optional: boolean=false; canCreate: boolean=false): string;
var
  currentPathToCheck: TFilePathType;

    procedure CheckFiles;
    var i: integer;
    begin
      for i := 0 to High(filesToCheck)-1 do begin
        if not FileExists(result + filesToCheck[i]) then begin
          ShowInformation(Format(ResStr_MissingFile, [result + filesToCheck[i]]));
          Abort; // prevents app startup
        end;
      end;
    end;

begin
  result := '';
  if iReg.ValueExists(iRegKey) then begin
    result := iReg.ReadString(iRegKey);
    if optional or DirectoryExists(result) then begin
      // Registry setting already correct. Just check required files are present.
      CheckFiles;
      exit;
    end;
  end;
  // Try each possible path in the order in which they are preferred, until we find
  // the folder or run out of options.
  currentPathToCheck := pathType;
  if not canCreate then while (result='') do begin
    if DirectoryExists(GetFolderLocation(currentPathToCheck)+iSuggestedFolder) then
      result := GetFolderLocation(currentPathToCheck)+iSuggestedFolder;
    if currentPathToCheck = fptInstallFolder then break;
    currentPathToCheck := TFilePathType(Ord(currentPathToCheck) - 1);
  end;
  if result = '' then begin
    // Folder cannot be found so create it in the default location
    result := GetFolderLocation(pathType)+iSuggestedFolder;
    ForceDirectories(result);
  end
  else if not (optional or DirectoryExists(result)) then
    raise EApplicationSettingsError.Create(Format(ResStr_RegKeySettingMissingOrIncorrect, [iRegKey]));
  CheckFiles;
end;

(**
 * Retrieves a standard folder location in which files can be stored. Used to find
 * things like where the User Files folder should go. Aware of OS differences and
 * network vs standalone installs.
 *)
function TApplicationSettings.GetFolderLocation(pathType: TFilePathType): string;
var
  applicationFolderName: string;
begin
  applicationFolderName := 'Recorder 6';
  // When standalone, there is no concept of a network location so we keep things local.
  if FStandalone and (pathType=fptNetworkData) then
    pathType := fptLocalData
  else if FStandalone and (pathType=fptNetworkDocs) then
    pathType := fptLocalDocs;
  case pathType of
    fptNetworkData, fptNetworkDocs, fptInstallFolder:
      result := ExtractFilePath(application.exename);
    fptLocalData:
      result := GetFolder(CSIDL_COMMON_APPDATA)+applicationFolderName + '\';
    fptLocalDocs:
      result := GetFolder(CSIDL_COMMON_DOCUMENTS)+applicationFolderName + '\';
  end;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TAutoApplicationSettings, Class_AutoApplicationSettings,
    ciMultiInstance, tmApartment);
  // Forces the short date format for the application
  ShortDateFormat:='dd/MM/yyyy';
end.
