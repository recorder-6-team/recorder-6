//==============================================================================
//  Unit:        Constants
//
//  Implements:  Various constant values and message strings used throughout
//               the application
//
//  Description:
//
//  Author:
//  Created:
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 142 $
//    $Date: 31.05.10 16:37 $
//    $Author: Simonlewis $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit Constants;

interface

uses
  Messages, Graphics;

resourcestring
  // taxon search restriction constants
  ResStr_CurrentChecklist = 'Current Checklist';
  ResStr_Unrestricted     = 'Unrestricted';
  ResStr_PreferredLists   = 'Preferred Checklists';
  ResStr_Preferred_Taxa   = 'Preferred Taxa';
  ResStr_Recommended_Full = 'Recommended';
  // Error messages
  ResStr_DBPathMissing      = 'Database Path not in registry - application cannot start';
  ResStr_DBPassMissing      = 'Database Password not in registry - application cannot start';
  ResStr_DTDPathMissin      = 'DTD Path not in registry - application cannot start';
  ResStr_AddinPathMissing   = 'Addin Path not in registry - application cannot start';
  ResStr_SiteIDMissing      = 'Site ID not in registry - application cannot start';
  ResStr_AddinIndexInvalid  = 'An addin has requested an invalid validation index';
  ResStr_AnotherUserUpdated = 'Record updated by another user';
  ResStr_AlreadyRunningOnThisMachine =
      'Recorder is already running on this machine. Application cannot start.';
  ResStr_CantRevoke         = 'Not all drag drop registration could be revoked';
  ResStr_DragDropRegFailed  = 'Could not register the following control for drag drop : ';
  ResStr_NotComponent       = 'Cannot look for an object in the drop component list';
  ResStr_ApplicationStartingError = 'An error occurred whilst starting the application.';

  {$IFDEF DEBUG} // this message never displayed in normal operation
  ResStr_ComponentCantPaste = 'Component not capable of paste operation :';
  ResStr_ComponentCantCopy  = 'Component not capable of copy operation :';
  {$ENDIF}

  ResStr_NotRegistered    = 'Component not registered to drag from';

  ResStr_ParentNotForm    = 'Internal Error.  A sub-form can only be created by a form object.';
  ResStr_NotBaseForm      = 'Form is not a base form : ';
  ResStr_NotRTFControl    = 'Current control is not a Rich Edit control.';
  ResStr_NotComAction     = 'Attempt to initiate an Add-in action failed because action of wrong type.';
  ResStr_MapNotConfigured = 'The Map Configuration screen cannot be opened until a base map is selected.';

  ResStr_CloseConfirm     = 'This will close all currently open windows.  Do you wish to continue?';

  ResStr_Editing          = 'Please close all windows before using the database utilities.';
  ResStr_RecordMissing    = 'The record was not found in table : ';
  ResStr_NoPrimaryKey     = 'The primary key cannot be located for the table ';
  ResStr_MultipleFieldKey = 'Trying to obtain single key field for a dataset with a multiple field primary key : ';
  ResStr_InitDAO          = 'Error occurred during DAO initialisation.';
  ResStr_WrongPassword    = 'The database password is incorrect.  The application cannot start.';

  ResStr_NoMenuWithName    = 'A COM Addin has called a menu option which does not exist : %s';
  ResStr_LinkSourceNotBase = 'Attempt to setup a Return Data link from non-base form';
  ResStr_TaxonPrivate  = 'Private Taxon';
  ResStr_ReadFail       = 'Unable to read from the database.';
  ResStr_CreateFail     = 'Unable to create new record.';
  ResStr_AddFail        = 'Unable to add new item in %s table.';
  ResStr_TreeAddFail    = 'Unable to add new item. No Parent.';
  ResStr_DelFail        = 'Unable to delete an item from the database.';
  ResStr_LocateFail     = 'Unable to locate the record corresponding to the current key.';
  ResStr_InitRecFail    = 'Unable to read an item from the database.';
  ResStr_WriteRecFail   = 'Unable to save item changes to the database.';
  ResStr_OpenFail       = 'Unable to open the database.';
  ResStr_CloseFail      = 'Unable to close the database.';
  ResStr_SetSpatialRef  = 'Unable to set spatial reference.';

  ResStr_LostDBConnection =
      'Whilst attempting to gain exclusive access to the database, another user '
      + 'took exclusive access of the database.  Therefore, the database connection cannot '
      + 'be regained.  The application will now close - please restart when the other '
      + 'user has completed their operation.  The error was described as'#13#10;

  ResStr_CannotAccessRecord = 'Unable to access the record.';
  ResStr_CannotEditRecord   = 'Unable to edit the record.';
  ResStr_CannotDeleteRecord = 'Unable to delete the record.';
  ResStr_CannotProceed      = 'Unable to proceed - A record is being edited.';

  ResStr_BadRecCard =
      'Unable to read content of record card ''%s''.'#13
      + 'This file is an invalid record card file, or its content has been corrupted.';

  ResStr_BadRucksack =
      'Unable to read content of rucksack ''%s''.'#13
      + 'This file is an invalid rucksack file, or its content has been corrupted.';

  ResStr_TaxonItems          = 'There are no taxa in the currently selected checklist.';
  ResStr_BiotopeItems        = 'There are no biotopes in the currently selected classification.';
  ResStr_AdminAreaItems        = 'There are no admin areas in the currently selected admin area type.';
  ResStr_TaxOccItems         =
      'There are no taxon occurrences in the database to relate the currently entered one with.';
  ResStr_NoReferenceItems    = 'There are no documents in the database.';
  ResStr_NoIndividualItems   = 'There are no individuals in the database.';
  ResStr_SampleNoRecorders   = 'This Recorder cannot be deleted, because they are the only Recorder on one or more of the Samples';
  ResStr_AddSampleRecorders   = 'Do you wish to add these Recorder(s) to all Samples for this Event ?';
  ResStr_CascadeDeterminer = 'One or more Recorders have been changed. Do you wish to update the determiners for these ?';
  ResStr_NoOrganisationItems = 'There are no organisations in the database.';
  ResStr_NoNameItems         = 'There are no individuals/organisations in the database.';
  ResStr_NoLocationItems     = 'There are no locations in the database.';
  ResStr_RelatedSampleItems  =
      'There are no samples in the database to relate the currently entered one with.';
  ResStr_NearGridRefItems    =
      'There are some locations within 2km of the spatial reference you have entered.  '
      + 'Would you like to view a list of these locations?  '
      + 'Note, selecting an item from the list will discard the new entry you are making.';

  ResStr_CustodyChanged =
      'Unable to save changes to %s record. '#13#10
      + 'The custody of this record has been changed to a different Site ID.';

  ResStr_IncompatibleReport =
      'The %s report you have selected is not compatible with the data you are trying to report.';

  ResStr_RegistryProblem   = 'A problem occurred opening registry keys for COM addins';
  ResStr_InvalidAddin      = 'A Registered Add-in is not valid and cannot be used.';
  ResStr_AddinImageProblem = 'A problem occurred loading the image for an add-in';

  ResStr_BadAddin = 'An addin could not be loaded for the following reason.  ';
  ResStr_NoHelp   = 'No help is available for this topic';
  ResStr_SelectBoundaryToDeleteFrom =
      'Please select the boundary you wish to delete from the selected boundary, then Press F8.';

  // Only individuals are allowed to be recorders.
  ResStr_NoOrgRecorders = 'You cannot specify an organisation as a survey event recorder.';

  ResStr_MinCharSearch = 'Please type %d characters or more to search on';

  ResStr_FilteringInProgress = 'Filtering in progress...';
  ResStr_NoFilterMatch       = 'There are no %s matching the filter criteria for the currently selected %s.';

  //resourcestring to be used with InvalidDate procedure
  ResStr_FactDate          = 'The Fact Date ';
  ResStr_TheAgreementDate  = 'The Agreement Date ';
  ResStr_TheDate           = 'The Date ';
  ResStr_StartDate         = 'The Start Date ';
  ResStr_EndDate           = 'The End Date ';
  ResStr_FoundationDate    = 'The Foundation Date ';
  ResStr_DateOfBirth       = 'The Date of Birth ';
  ResStr_DateOfDeath       = 'The Date of Death ';
  ResStr_SampleDate        = 'The Sample Date ';
  ResStr_DeterminationDate = 'The Determination Date ';
  ResStr_AccessionDate     = 'The Accession Date ';

  ResStr_The               = 'The ';
  ResStr_None              = '<None>';
  ResStr_Recorder          = 'Recorder';

  //resourcestring to be used with createDialog
  ResStr_FindAdminArea        = 'Find Admin Area';
  ResStr_FindBiotopeTerm      = 'Find Biotope Term';
  ResStr_FindDeterminerName   = 'Find Determiner Name';
  ResStr_FindDocument         = 'Find Document';
  ResStr_FindRecorderName     = 'Find Recorder Name';
  ResStr_FindParty            = 'Find Party';
  ResStr_FindIndOrgName       = 'Find Individual/Organisation Name';
  ResStr_FindTaxon            = 'Find Taxon';
  ResStr_FindTaxonName        = 'Find Taxon Name';
  ResStr_FindKeyword          = 'Find Keyword';
  ResStr_FindLocationName2km  = 'Find Location Names within 2km';
  ResStr_FindLocation         = 'Find Location';
  ResStr_FindLocationName     = 'Find Location Name';
  ResStr_FindSpecies          = 'Find Species';
  ResStr_FindRelatedSample    = 'Find Related Sample';
  ResStr_FindRelatedOccurence = 'Find Related Occurence';
  ResStr_FindSurveyTag        = 'Find Survey Tag';
  ResStr_FindIndividualName   = 'Find Individual Name';

  ResStr_FindSurvey           = 'Find Survey';
  ResStr_FindEvent            = 'Find Event';
  ResStr_FindSample           = 'Find Sample';
  ResStr_FindBiotope          = 'Find Biotope';
  ResStr_FindOccurrence       = 'Find Occurrence';

  ResStr_SurveyTag            = 'Survey Tag';
  ResStr_SurveyTags           = 'Tags';
  ResStr_Survey               = 'Survey';
  ResStr_Surveys              = 'Surveys';
  ResStr_Event                = 'Event';
  ResStr_BiotopeOcc           = 'Biotope Occurrence';
  ResStr_TaxonOcc             = 'Taxon Occurrence';
  ResStr_SurveyEvent          = 'Survey Event';
  ResStr_SurveyEvents         = 'Survey Events';
  ResStr_Sample               = 'Sample';
  ResStr_TaxonOrBiotopeOcc    = 'Taxon or Biotope Occurrences';
  ResStr_NoKeywords           = 'No keywords were found.';
  ResStr_NoSurveyTags         = 'No survey tags were found.';
  ResStr_NoSurveys            = 'No surveys were found.';

  ResStr_ERROR        = 'ERROR';
  ResStr_ERRORMessage = 'ERROR: %s';

  ResStr_TaxonDetermination   = 'Taxon Determination';
  ResStr_BiotopeDetermination = 'Biotope Determination';

  ResStr_NotImplemented         = 'Not implemented';
  ResStr_SurveyEventRecorder    = 'Survey Event Recorder';
  ResStr_ManagementAim          = 'Management Aim';
  ResStr_PotentialThreat        = 'Potential Threat';
  ResStr_DamageOccurence        = 'Damage Occurrence';
  ResStr_LocationName           = 'Location Name';
  ResStr_LocationUse            = 'Location Use';
  ResStr_Tenure                 = 'Tenure';
  ResStr_ContactNumbers         = 'Contact Numbers';
  ResStr_Address                = 'Address';
  ResStr_Communication          = 'Communication';
  ResStr_Association            = 'Association';
  ResStr_Author                 = 'Author';
  ResStr_Editor                 = 'Editor';
  ResStr_ReferenceNumber        = 'Reference Number';
  ResStr_RelatedSample          = 'Related Sample';
  ResStr_TaxonStatus            = 'Taxon Status';
  ResStr_TaxonFact              = 'Taxon Fact';
  ResStr_TaxonName              = 'Taxon Name';
  ResStr_TaxonOccurenceRelation = 'Taxon Occurence Relation';
  ResStr_Specimen               = 'Specimen';
  ResStr_PrivateItem            = 'Item';
  ResStr_PrivateDetail          = 'Detail';
  ResStr_PrivateType            = 'Type';
  ResStr_SystemManager          = 'System Manager';
  ResStr_FullEditOwn            = 'Full Edit (Own data only)';
  ResStr_FullEdit               = 'Full Edit';
  ResStr_AddOnly                = 'Add Only';
  ResStr_RecordCardsOnly        = 'Record Cards Only';
  ResStr_ReadOnly               = 'Read Only';
  ResStr_Unknown                = 'Unknown';
  ResStr_Rucksack               = ' Rucksack';
  ResStr_Location               = 'Location';
  ResStr_Feature                = 'Feature';
  ResStr_Document               = 'Document';

  //Resourcestring for captions
  ResStr_Cap_RunReportTemplate         = 'R&un Report Template ';
  ResStr_Cap_EditReportTemp            = 'Edit Report Templ&ate ';
  ResStr_Cap_RunSnapshot               = '&Run Snapshot ';
  ResStr_Cap_EditSnapshot              = 'Edit S&napshot ';
  ResStr_Cap_Ok                        = 'Ok';
  ResStr_Cap_Cancel                    = 'Cancel';
  ResStr_Cap_MapWindow                 = 'Map Window - %s';
  ResStr_Cap_Location                  = 'Location: %s';
  ResStr_Cap_Year                      = 'Year';
  ResStr_Cap_Author                    = 'Author';
  ResStr_Cap_Title                     = 'Title';
  ResStr_Cap_Filters                   = 'Filters';
  ResStr_Cap_HideItemDetails           = 'Hide &Item Details';
  ResStr_Cap_ShowItemDetails           = 'Show &Item Details';
  ResStr_Cap_TaxaSelection             = 'Taxa Selection';
  ResStr_Cap_IncludeAllTaxa            = 'Include &All Taxa';
  ResStr_Cap_AllTaxaFromList           = 'All &Taxa from list';
  ResStr_Cap_BiotopesSelection         = 'Biotopes Selection';
  ResStr_Cap_IncludeAllBiotopes        = 'Include &All Biotopes';
  ResStr_Cap_AllBiotopesFromList       = 'All Bi&otopes from list';
  ResStr_Cap_CheckList                 = 'Check &List:';
  ResStr_Cap_FindTaxon                 = '&Find Taxon:';
  ResStr_Cap_Classification            = 'C&lassification';
  ResStr_Cap_FindBiotope               = '&Find Biotope:';
  ResStr_Cap_FinishPolygonAdd          = 'Finish &Polygon && add to %s';
  ResStr_Cap_FinishLineAdd             = 'Finish &Line && add to %s';
  ResStr_Cap_SetBoundaryToSubtract     = 'Set boundary to sub&tract from';
  ResStr_Cap_SetBoundaryToSubtractFrom = 'Set boundary to subtract from';
  ResStr_Cap_SubtractBoundaryLink      = 'Sub&tract Boundary from Boundary';
  ResStr_Cap_SubtractBoundary          = 'Subtract Boundary from Boundary';
  ResStr_Cap_SubtractBoundClickable    = 'Sub&tract boundary';
  ResStr_SubtractBound                 = 'Subtract boundary';
  ResStr_Cap_None                      = '&None';
  ResStr_Cap_Default                   = '&Default';
  ResStr_Cap_AvailableOn               = 'Available on:';

  // Used as grid column titles.
  ResStr_AgreementDate     = 'Agreement Date';
  ResStr_Authority         = 'Authority';
  ResStr_BaseMap           = 'Base Map';
  ResStr_Biotope           = 'Biotope';
  ResStr_Constraints       = 'Constraints';
  ResStr_Damage            = 'Damage';
  ResStr_Date              = 'Date';
  ResStr_DateFrom          = 'Date From';
  ResStr_DateTo            = 'Date To';
  ResStr_Default           = 'Default';
  ResStr_Designation       = 'Designation';
  ResStr_Determiner        = 'Determiner';
  ResStr_DisplayName       = 'Display Name';
  ResStr_From              = 'From';
  ResStr_GeoArea           = 'Geo Area';
  ResStr_Initialised       = 'Initialised';
  ResStr_InternalMap       = 'Internal Map';
  ResStr_Language          = 'Language';
  ResStr_Locality          = 'Locality';
  ResStr_MapSystem         = 'Map System';
  ResStr_Name              = 'Name';
  ResStr_NameCode          = 'Name Code';
  ResStr_Number            = 'Number';
  ResStr_OccurrenceOf      = 'Occurrence of';
  ResStr_PointType         = 'Point Type';
  ResStr_Postcode          = 'Postcode';
  ResStr_Prefix            = 'Prefix';
  ResStr_RelatedOccurrence = 'Related Occurrence';
  ResStr_RelatedWith       = 'Related With';
  ResStr_Relation          = 'Relation';
  ResStr_Role              = 'Role';
  ResStr_SampleRef         = 'Sample Ref';
  ResStr_SampleType        = 'Sample Type';
  ResStr_SpecimenNumber    = 'Specimen Number';
  ResStr_SpecimenType      = 'Specimen Type';
  ResStr_Square            = 'Square';
  ResStr_Taxon             = 'Taxon';
  ResStr_Threat            = 'Threat';
  ResStr_Title             = 'Title';
  ResStr_To                = 'To';
  ResStr_Type              = 'Type';
  ResStr_Use               = 'Use';
  ResStr_Version           = 'Version';
  ResStr_With              = 'With';
  ResStr_NewItem           = 'New Item';
  ResStr_PlacesForOccurrences = 'Places for Occurrences';
  ResStr_OccurrencesForPlaces = 'Occurrences for Places';
  ResStr_FilterSHPFilesOnly   = 'SHP files only|*.shp';
  ResStr_TextFilterCondition  = '"is &Equal to","is &Not equal to","&Starts with",&Contains';
  ResStr_RichTextFilterCondition = '&Contains';
  ResStr_BooleanFilterCondition  = '"is &Equal to","is &Not equal to"';
  ResStr_NumberFilterCondition =
      '"is &Equal to","is &Not equal to","is &Greater than","is &Less than","is &Between"';
  ResStr_SpatialRefFilterConditionOS =
      '"is &Equal to","is &Not equal to","&Starts with","is &In Grid Square"';
  ResStr_SpatialRefFilterConditionNON =
      '"is &Equal to","is &Not equal to","&Starts with"';

  // Used for Taxon Search Display
  ResStr_Taxon_Can_Not_Expand = ' (can not expand) ';
type
  TEditMode = (emView, emAdd, emEdit, emDelete, emNone);

  //Access level
  TUserAccessLevel = (ualReadOnly, ualRecorder, ualAddOnly, ualFullUser, ualAdmin);

  TSQLSvrTimestamp = OLEVariant;

const
  // Messages used in the application
  // Our own user message -needed because Drag Drop cannot be setup until after
  //       everything else is ready
  DRAG_THRESHOLD = 5;

  WM_SETDRAGDROP                = WM_APP + 100;

  WM_SHOW_TIP_WINDOW            = WM_APP + 102;
  WM_LAST_SESSION_WINDOWS       = WM_APP + 103;
  WM_TRANSFER_DONE              = WM_APP + 104;
  WM_DISCARD_OBSERVATION        = WM_APP + 105;
  WM_DISCARD_LOCATION           = WM_APP + 106;
  WM_REFRESH_TERM_LISTS         = WM_APP + 107;
  WM_UPDATE_MENU_ICONS          = WM_APP + 108;
  WM_REFRESH_SPATIAL_REF_SYSTEM = WM_APP + 109;
  WM_UPDATE_SURVEY_COMBO        = WM_APP + 110;
  WM_REFRESH_LOCATION_TYPE      = WM_APP + 111;
  WM_REFRESH_NAMES              = WM_APP + 112;
  WM_REFRESH_DOCUMENTS          = WM_APP + 113;
  WM_REFRESH_LOCATION_DETAILS   = WM_APP + 114;
  WM_REFRESH_TAXON_DIC          = WM_APP + 115;
  WM_REFRESH_BIOTOPE_DIC        = WM_APP + 116;
  WM_REFRESH_SOURCES            = WM_APP + 117;
  WM_REFRESH_COLOURS            = WM_APP + 118;
  WM_IMPORTED_COMPLETE          = WM_APP + 119;
  WM_RUN_WIZARD                 = WM_APP + 120;
  WM_SHOW_DETAILS               = WM_APP + 121; // for dictionary details update
  WM_START_SEARCH               = WM_APP + 122; // for find dialog
  WM_HIDE_COMTOOLBAR            = WM_APP + 123; // for OLE Container
  WM_SUPPRESS_DRAGDROP          = WM_APP + 124;
  WM_CHECK_MDI_BUTTONS          = WM_APP + 125;
  WM_ASSOCIATE_BOUNDARY         = WM_APP + 126; //for Map screen
  WM_REFRESH_SPATIAL_REF        = WM_APP + 127;
  WM_UPDATE_SURVEY_DATE         = WM_APP + 128; // For LocationDetails Last Survey Date
  WM_REFRESH_RECORD_COUNT       = WM_APP + 129; // for import wizard
  WM_REFILTER                   = WM_APP + 130; // for import wizard
  WM_DORETURNDATA               = WM_APP + 131;
  WM_DELETE_NODE                = WM_APP + 132; //for Observations screen
  WM_REFRESH_OBSERVATIONS       = WM_APP + 133;
  WM_UNCHECKED_DELETE_NODE      = WM_APP + 134; //for Observations screen
  WM_UPDATE_SAMPLE_LOAD_CARD    = WM_APP + 135;
  WM_PASS_LOCATION              = WM_APP + 136; //for linking between boundary import and locations

  //============================================================================
  // Application Settings

  // General options default values
  DEFAULT_SHOW_LAST_SESSION_WINDOWS  = True;
  DEFAULT_SHOW_MENU_ICONS            = True;
  DEFAULT_GRADUATED_MENUS            = False;
  DEFAULT_SHOW_MAIN_TOOLBAR          = True;
  DEFAULT_SHOW_CONTEXT_TOOLBAR       = True;
  DEFAULT_SHOW_WELCOME_AT_START      = True;
  DEFAULT_SHOW_COMMON_NAMES          = False;
  DEFAULT_SHOW_ENTERED_NAMES         = False;
  DEFAULT_SHOW_AUTHORS               = False;
  DEFAULT_FULL_TRANSLATION           = True;
  DEFAULT_CUT_YEAR                   = 40;
  DEFAULT_AUTO_SCHEME_EMAIL          = True;
  DEFAULT_EXPORT_CONFIDENTIAL_OCC    = False;
  DEFAULT_REMEMBER_FILTERS           = False;
  DEFAULT_CONFIDENTIAL_ACCESS_LEVEL  = ualFullUser;
  DEFAULT_REFERENCES_SEARCH_KEYWORDS = False;
  DEFAULT_PARTIAL_TAXON_SEARCH       = False;
  DEFAULT_AUTO_COMPLETE_SEARCH       = False;
  DEFAULT_USE_PREFERRED_TAXA         = True;
  DEFAULT_INCLUDE_LOCATION_SPATIAL_REF = False;
  DEFAULT_INCLUDE_LOCATION_FILE_CODE = False;
  DEFAULT_ORGANISE_SURVEYS_BY_TAG    = False;
  DEFAULT_USE_OLD_IMPORT_WIZARD      = False;
  DEFAULT_IGNORE_REMEMBERED_MATCHES  = False;
  // Appearance default values
  DEFAULT_DRAG_SOURCE_COLOUR      = clBlue;
  DEFAULT_DRAG_DEST_COLOUR        = clRed;
  DEFAULT_MANDATORY_COLOUR        = clYellow;
  DEFAULT_BACKGROUND_PICTURE      = '';
  DEFAULT_DISABLE_DRAG_DROP_FRAME = False;
  DEFAULT_SHOW_TOOL_TIPS          = True;
  DEFAULT_USE_ORIGINAL_ICONS          = False;

  //Sundry Default Values

  DEFAULT_ADD_DOCS_TO_OCCURRENCE = False;

  // Grid Ref default value
  DEFAULT_SPATIAL_REF_SYSTEM    = 'OSGB';
  DEFAULT_GRID_REFS_AS_SQUARES  = False;

  NONE_RECORD_KEY = 'NBNSYS0000000001';
  NONE_RECORD     = 'None';

  // Standard Report names
  ST_PLACES_FOR_OCC_REPORT = '<Locations for Occurrences>';
  ST_OCCS_FOR_PLACE_REPORT = '<Occurrences for Locations>';

  MIN_CHARS           = 2;
  RUCKSACK_SEARCH_CODE_MIN_CHARS = 1;
  //============================================================================

  { IR 3/12/99
  Constants used to show a place card is a default place card... not a com addin }
  PLACECARD_STANDARD_HEADER_NAME = 'Standard Header';
  PLACECARD_STANDARD_HEADER_CLSID = '{D3393981-A966-11D3-B59C-005004AE7C43}';

  //============================================================================
  // Filter/ComplexFilter
  TYPE_TEXT       = 'TEXT';
  TYPE_RICH_TEXT  = 'RICH_TEXT';
  TYPE_MEMO       = 'MEMO';
  TYPE_BOOLEAN    = 'BOOLEAN';  // Used by the ComplexFilter
  TYPE_INTEGER    = 'INTEGER';  // Used by the ComplexFilter
  TYPE_NUMBER     = 'NUMBER';
  TYPE_VAGUE_DATE = 'VAGUE_DATE';
  TYPE_SPATIAL_REF= 'SPATIAL_REF';

  FILTER_DICTIONARY   = '%s Filter';
  FILTER_LOCATION     = 'Location Filter';
  FILTER_INDIVIDUAL   = 'Individual Filter';
  FILTER_ORGANISATION = 'Organisation Filter';
  FILTER_REFERENCE    = 'Reference Filter';
  FILTER_OBSERVATION  = 'Observation Filter';

  //============================================================================
  STATE_UNCHECKED  = 1;
  STATE_CHECKED    = 2;

  //============================================================================
  BoolString   : array[Boolean] of String = ('FALSE', 'TRUE');
  SQLBoolString: array[Boolean] of Char = ('0', '1'); // True/False disliked by SQL Server

  //============================================================================
  // Registry constants
  REG_KEY_RECORDER          = '\Software\Dorset Software\Recorder 6';
  REG_KEY_ADDIN             = REG_KEY_RECORDER + '\Installed Addins';
  REG_KEY_SETTINGS          = REG_KEY_RECORDER + '\Settings';
  REG_KEY_STANDARD_TOOLBAR  = REG_KEY_SETTINGS + '\Standard';
  REG_KEY_CONTEXT_TOOLBAR   = REG_KEY_SETTINGS + '\Context';
  REG_KEY_FORMS             = REG_KEY_RECORDER + '\Forms';
  REG_KEY_MAP_BROWSER       = REG_KEY_FORMS + '\MapBrowser';
  REG_KEY_FAVOURITE_REPORTS = 'Software\Dorset Software\Recorder 6\Favourite Reports';
  REG_KEY_LOCATION_SEARCH_COLS = REG_KEY_SETTINGS + '\Location Search Columns';

  REG_INSTALLED = 'Installed';
  REG_CLASS_ID  = 'ClsId';

  BITMAP_SIZE = 16;  // Addin glyph size.

  //============================================================================
  // If the Database provider shows up in a message string, use this constant
  STR_DATABASE = 'Access';
  // Maximum query size allowed in Access, 64k
  INT_MAXIMUM_QUERY_SIZE = 64000;

  STR_RUCKSACK     = 'Current Rucksack';
  STR_RUCKSACK_EXP = 'Current Rucksack (Expanded)';

  //============================================================================
  // Table names
  TN_ADDRESS                    = 'ADDRESS';
  TN_ADMIN_AREA                 = 'ADMIN_AREA';
  TN_ADMIN_BOUNDARY             = 'ADMIN_BOUNDARY';
  TN_BIOTOPE                    = 'BIOTOPE';
  TN_BIOTOPE_DETERMINATION      = 'BIOTOPE_DETERMINATION';
  TN_BIOTOPE_FACT               = 'BIOTOPE_FACT';
  TN_BIOTOPE_LIST_ITEM          = 'BIOTOPE_LIST_ITEM';
  TN_BIOTOPE_OCCURRENCE         = 'BIOTOPE_OCCURRENCE';
  TN_BIOTOPE_OCCURRENCE_DATA    = 'BIOTOPE_OCCURRENCE_DATA';
  TN_BIOTOPE_OCCURRENCE_SOURCES = 'BIOTOPE_OCCURRENCE_SOURCES';
  TN_CONCEPT                    = 'CONCEPT';
  TN_CONTACT_NUMBER             = 'CONTACT_NUMBER';
  TN_COMMUNICATION              = 'COMMUNICATION';
  TN_DAMAGE_OCCURRENCE          = 'DAMAGE_OCCURRENCE';
  TN_DETERMINATION_TYPE         = 'DETERMINATION_TYPE';
  TN_DETERMINER_ROLE            = 'DETERMINER_ROLE';
  TN_EXPORT_FILTER_TAG          = 'EXPORT_FILTER_TAG';
  TN_GRID_SQUARE                = 'GRID_SQUARE';
  TN_INDIVIDUAL                 = 'INDIVIDUAL';
  TN_LAND_PARCEL                = 'LAND_PARCEL';
  TN_LOCATION                   = 'LOCATION';
  TN_LOCATION_ADMIN_AREAS       = 'LOCATION_ADMIN_AREAS';
  TN_LOCATION_BOUNDARY          = 'LOCATION_BOUNDARY';
  TN_LOCATION_DATA              = 'LOCATION_DATA';
  TN_LOCATION_DESIGNATION       = 'LOCATION_DESIGNATION';
  TN_LOCATION_FEATURE           = 'LOCATION_FEATURE';
  TN_LOCATION_NAME              = 'LOCATION_NAME';
  TN_LOCATION_USE               = 'LOCATION_USE';
  TN_LOCATION_RELATION          = 'LOCATION_RELATION';
  TN_MEASUREMENT_CONTEXT        = 'MEASUREMENT_CONTEXT';
  TN_MEASUREMENT_QUALIFIER      = 'MEASUREMENT_QUALIFIER';
  TN_MEASUREMENT_TYPE_CONTEXT   = 'MEASUREMENT_TYPE_CONTEXT';
  TN_MEASUREMENT_UNIT           = 'MEASUREMENT_UNIT';
  TN_MEASUREMENT_UNIT_VALUE     = 'Measurement_Unit_Value';
  TN_MANAGEMENT_AIM             = 'MANAGEMENT_AIM';
  TN_POTENTIAL_THREAT           = 'POTENTIAL_THREAT';
  TN_NAME                       = 'NAME';
  TN_NAME_RELATION              = 'NAME_RELATION';
  TN_MEASUREMENT_TYPE           = 'MEASUREMENT_TYPE';
  TN_ORGANISATION               = 'ORGANISATION';
  TN_RECORD_TYPE                = 'RECORD_TYPE';
  TN_REFERENCE                  = 'REFERENCE';
  TN_SAMPLE                     = 'SAMPLE';
  TN_SAMPLE_ADMIN_AREAS         = 'SAMPLE_ADMIN_AREAS';
  TN_SAMPLE_DATA                = 'SAMPLE_DATA';
  TN_SAMPLE_RECORDER            = 'SAMPLE_RECORDER';
  TN_SAMPLE_SOURCES             = 'SAMPLE_SOURCES';
  TN_SAMPLE_TYPE                = 'SAMPLE_TYPE';
  TN_SAMPLE_RELATION            = 'SAMPLE_RELATION';
  TN_SOURCE                     = 'SOURCE';
  TN_SOURCE_FILE                = 'SOURCE_FILE';
  TN_SURVEY                     = 'SURVEY';
  TN_SURVEY_EVENT               = 'SURVEY_EVENT';
  TN_SURVEY_EVENT_RECORDER      = 'SURVEY_EVENT_RECORDER';
  TN_SURVEY_TAG                 = 'SURVEY_TAG';
  TN_TAXON                      = 'TAXON';
  TN_TAXON_DESIGNATION          = 'TAXON_DESIGNATION';
  TN_TAXON_DETERMINATION        = 'TAXON_DETERMINATION';
  TN_TAXON_FACT                 = 'TAXON_FACT';
  TN_TAXON_GROUP                = 'TAXON_GROUP';
  TN_TAXON_LIST_ITEM            = 'TAXON_LIST_ITEM';
  TN_TAXON_OCCURRENCE           = 'TAXON_OCCURRENCE';
  TN_TAXON_OCCURRENCE_DATA      = 'TAXON_OCCURRENCE_DATA';
  TN_TAXON_OCCURRENCE_SOURCES   = 'TAXON_OCCURRENCE_SOURCES';
  TN_TAXON_OCCURRENCE_RELATION  = 'TAXON_OCCURRENCE_RELATION';
  TN_THESAURUS_FACT             = 'THESAURUS_FACT';
  TN_TENURE                     = 'TENURE';
  TN_TERM                       = 'TERM';

  //Primary Key names
  PK_BIOTOPE_LIST_ITEM          = 'BIOTOPE_LIST_ITEM_KEY';
  PK_LOCATION                   = 'LOCATION_KEY';
  PK_LOCATION_BOUNDARY          = 'LOCATION_BOUNDARY_KEY';
  PK_MAP_SHEET                  = 'MAP_SHEET_KEY';
  PK_OCCURRENCE                 = 'OCCURRENCE_KEY';
  PK_SAMPLE                     = 'SAMPLE_KEY';
  PK_SAMPLE_TYPE                = 'SAMPLE_TYPE_KEY';
  PK_TAXON_LIST_ITEM            = 'TAXON_LIST_ITEM_KEY';
  PK_SURVEY_EVENT               = 'SURVEY_EVENT_KEY';

  //Foreign Key names (for FKs with names different from the PK they point to)
  FK_BASE_MAP_KEY               = 'BASE_MAP_KEY';

  ACG_KEYWORDS                  = 'SYSTEM0100000000';
  ACG_SURVEY_TAGS               = 'SYSTEM0100000001';

  CONCEPT_KEY_COMMON            = 'SYSTEM000000000L';
  MEANING_KEY_FACT              = 'SYSTEM000000026E';

  IMPORT_REJECTS_FILE           = 'ImportRejects.txt';

  // Default password used for new users.
  DEFAULT_USER_PASSWORD         = 'password';

//==============================================================================
implementation

// Nothing to implement here

//==============================================================================
end.
