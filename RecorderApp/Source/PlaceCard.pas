//==============================================================================
//  Unit:        PlaceCard
//
//  Implements:  TfrmPlaceCard
//               TAccessorClass
//               TTaxonHintWindow
//
//  Description: Presents the basic recording card functionality.
//               TAccessorClass is used to gain access to protected methods and
//               properties of the TStringGrid class
//
//  Possible improvements:
//               Reset the Biotope List Item key every time the Biotope edit box
//               changes, unless a biotope is successfully found.  Then, don't
//               need to perform recheck at end - should validate data slightly
//               quicker
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     Eric Salmon - 8 March 2002
//               Cleaned up the CleanUp code to take advantage of master/detail
//               relationships between table.
//               CCN EHS2 - Implement handling of multiple measurement columns.
//               Bring up the Map screen for Spatial Ref column when F2 or return
//               on partial/invalid Spatial Ref.
//               Bring up Biotope Dictionary screen for Biotope column on F2, and
//               Find dialog on return on a partially entered name.
//               Bring up Individual/Organisation screen for Determiner column
//               on F2.
//               For each distinct Spatial Reference, create a Sample record for
//               the species listed.
//               For specified Biotopes, create new Biotope Occurrence within the
//               same Sample as the Taxon Occurrence.
//
//  Last Revision Details:
//    $Revision: 319 $
//    $Date: 8/06/10 15:27 $
//    $Author: Andrewkemp $
//
//==============================================================================

{$I '..\..\Third Party\Dorset Software Services\DssVcl32\DelphiVersions.Inc'}

unit PlaceCard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Grids, ExtCtrls, BaseChildUnit, Buttons, Mask, Menus,
  VagueDateEdit, BaseFormUnit, CompositeComponent, SpatialRef, Recorder2000_TLB,
  DataClasses, DBListCombo, DropStruct, DropSource, DropTarget, ValidationData,
  Exceptionform, OLETools, DBGlyphCtrls, PlaceCardData, VagueDate, OnlineHelp,
  Constants, GeneralFunctions, ImageListButton, DB, Variants, ADODB, Measurements,
  LocationInfoFrame, Cascade, Contnrs, ActnList;

type
  EPlacesError = class(TExceptionPath);
  EDatabaseWriteError = class(EPlacesError);

  TAccessorClass = class(TStringGrid); // gain access to protected properties

  TTaxonHintWindow = class(THintWindow) // Gain access to the Paint procedure
  private
    FDelimiterCount: integer;
    FHintStrings: TStringList;
    procedure SetDelimiterCount(const Value: integer);
    procedure DrawHintText(const FontStyle: TFontStyles; const S: string;
      var R: TRect);
    function GetHintStrings: TStringList;
    function GetHintText: string;
    function PreviousColumnNames: string;
    function CurrentColumnName: string;
    function FollowingColumnNames: string;
  protected
    procedure Paint; override;
    property HintStrings: TStringList read GetHintStrings;
  public
    destructor Destroy; override;
    property DelimiterCount: integer read FDelimiterCount write SetDelimiterCount;
    property HintText: string read GetHintText;
  end;

  TfrmPlaceCard = class(TBaseChild)
    pnlHeader : TPanel;
    pnlList : TPanel;
    eReference : TEdit;
    shpReference: TShape;
    sgSpecies : TStringGrid;
    pnlButtons : TPanel;
    lblReference: TLabel;
    pnlButtons2 : TPanel;
    mnuEdit : TMenuItem;
    mnuEditCut : TMenuItem;
    mnuEditCopy : TMenuItem;
    mnuEditPaste : TMenuItem;
    mnuEditBold : TMenuItem;
    mnuEditItalic : TMenuItem;
    mnuEditUnderline : TMenuItem;
    N1 : TMenuItem;
    cmbAccuracy : TComboBox;
    lblTotalSpecies : TLabel;
    cmbSubstrate : TDBListCombo;
    cmbProvenance : TComboBox;
    cmbRecordtype : TDBListCombo;
    lblSurvey: TLabel;
    cmbSurvey : TDBListCombo;
    shpTaxonAdd: TShape;
    eTaxon: TEdit;
    lblAddSpecies: TLabel;
    pmReOrder: TPopupMenu;
    mnuOriginalOrder: TMenuItem;
    cmbQualifier: TComboBox;
    bbSave: TImageListButton;
    bbReset: TImageListButton;
    bbReferenceFind: TImageListButton;
    bbFindTaxon: TImageListButton;
    mnuRemoveRow: TMenuItem;
    cmbRestrictedData: TComboBox;
    pnlLeft: TPanel;
    shpRecorders: TShape;
    lbRecorders: TListBox;
    bbRecorderRemove: TImageListButton;
    bbRecorderAdd: TImageListButton;
    bbRecorderFind: TImageListButton;
    lblRecorders: TLabel;
    lblSampleType: TLabel;
    cmbSampleType: TDBListCombo;
    fraLocationInfo: TfraLocationInfo;
    pnlRight: TPanel;
    shpBiotope: TShape;
    eBiotope: TEdit;
    bbBiotopeFind: TImageListButton;
    lblBiotope: TLabel;
    lblComments: TLabel;
    reComments: TRichEdit;
    eAdminArea: TEdit;
    lblAdminArea: TLabel;
    lblDate: TLabel;
    eDate: TVagueDateEdit;
    ActionList1: TActionList;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bbRecorderFindClick(Sender: TObject);
    procedure bbReferenceFindClick(Sender: TObject);
    procedure bbBiotopeFindClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure bbResetClick(Sender: TObject);
    procedure bbRecorderAddClick(Sender: TObject);
    procedure bbRecorderRemoveClick(Sender: TObject);
    procedure eReferenceKeyPress(Sender: TObject; var Key: Char);
    procedure sgSpeciesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormActivate(Sender: TObject);
    procedure cmbAccuracyChange(Sender: TObject);
    procedure sgSpeciesClick(Sender: TObject);
    procedure ChangeEditState(Sender: TObject);
    procedure cmbSampleTypeDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure eBiotopeKeyPress(Sender: TObject; var Key: Char);
    procedure cmbSubstrateChange(Sender: TObject);
    procedure cmbProvenanceChange(Sender: TObject);
    procedure cmbRecordTypeChange(Sender: TObject);
    procedure cmbSampleTypeChange(Sender: TObject);
    procedure reCommentsChange(Sender: TObject);
    procedure sgSpeciesSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
    procedure lbRecordersKeyPress(Sender: TObject; var Key: Char);
    procedure reCommentsEnter(Sender: TObject);
    procedure reCommentsExit(Sender: TObject);
    procedure eDateExit(Sender: TObject);
    procedure cmbKeyPress(Sender: TObject; var Key: Char);
    procedure sgSpeciesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sgSpeciesDblClick(Sender: TObject);
    procedure sgSpeciesTopLeftChanged(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: Char);
    procedure mnuOriginalOrderClick(Sender: TObject);
    procedure bbFindTaxonClick(Sender: TObject);
    procedure cmbQualifierChange(Sender: TObject);
    procedure sgSpeciesSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure sgSpeciesComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgSpeciesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgSpeciesKeyPress(Sender: TObject; var Key: Char);
    procedure eTaxonEnter(Sender: TObject);
    procedure eTaxonExit(Sender: TObject);
    procedure eTaxonChange(Sender: TObject);
    procedure mnuRemoveRowClick(Sender: TObject);
    procedure lbRecordersKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cmbRestrictedDataChange(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure pnlHeaderResize(Sender: TObject);
    procedure sgSpeciesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FClosingForm : Boolean;
    FSurveyEventKey    : TKeyString;
    FSampleKey         : TKeyString;  // Sample in header
    FSampleKeys        : TStringList; // Samples in grid
    FReferenceKey      : TKeyString;
    FBiotopeListItemKey: TKeyString;  // Biotope in header
    FBiotopeOccKeys    : TStringList; // Biotopes in grid
    FTaxonOccKeys      : TStringList; // Selected Taxa in grid
    FNewSurveyEvent : Boolean;
    FSERKeys : TStringList;
    FTaxonListKey: string;
    FTotalSpecies : Integer;
    FEditing : Boolean;
    FRecordingCardPath : string;
    FSampleType : TKeyString;
    FIsComHeader : boolean;
    FComHeader : TOleProxy;
    FdmPlaceCard : TdmPlaceCard;
    FClearingGrid: boolean;
    FSpatialRef : string;
    FLocation : string;
    FSpeciesValues : TStringList;
    FEditingCell : boolean;
    FGridClicked: boolean;
    FEditedCol: integer;
    FEditedRow: integer;
    FSortColumn: integer;
    FSortDescending : boolean;
    FFromRucksack: boolean;
    FUsedPreferredTaxaList : TStringList; // keep track of the actual list item keys we added to prevent duplicates
    FLoading : boolean; // prevent unecessary event handlers during card load
    FAdditionalTaxa: TStringList;
    FRemovedTaxa: TStringList;
    FUserColumnWidths: Boolean;
    FTaxaAbbreviations: TStringList;
    FProtectSpatialRef: Boolean;
    FAllowedList: Boolean; // track if the list if allowed for data entry
    FRapidTaxonEntryHintWindow: TTaxonHintWindow;
    FLoadedFromSample: Boolean;
    FLoadedSampleValues: TStringList;
    FColsResized: Boolean;
    FDefaultSurveyKey: string;
    FDefaultTaxonGroupKey: string;
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
    procedure WMRefreshTermLists(var Msg: TMessage); message WM_REFRESH_TERM_LISTS;
    procedure WMRefreshSpatialRefSystem(var Msg: TMessage); message WM_REFRESH_SPATIAL_REF_SYSTEM;
    procedure WMUpdateSurveyCombo(var Msg: TMessage); message WM_UPDATE_SURVEY_COMBO;
    procedure WMRefreshNames(var Msg: TMessage); message WM_REFRESH_NAMES;
    procedure WMRefreshLocationDetails(var Msg: TMessage); message WM_REFRESH_LOCATION_DETAILS;
    procedure WMRefreshBiotopeDic(var Msg: TMessage); message WM_REFRESH_BIOTOPE_DIC;
    procedure WMRefreshTaxonDic(var Msg: TMessage); message WM_REFRESH_TAXON_DIC;
    procedure WMRefreshColours(var Msg: TMessage); message WM_REFRESH_COLOURS;
    procedure WMRefreshDocuments(var Msg: TMessage); message WM_REFRESH_DOCUMENTS;
    function AddNewTaxonToGrid(const AKey: string; AClearValues, ADuplicatesAllowed: Boolean): Integer;
    procedure CreateSurveyEvent;
    procedure AddTaxonToGrid(const iItemKey: TKeyString;
        const iItemText, iCommonName, iCodeNumber: String; IsCritical: Boolean);
    procedure AssignIndexToGrid(ATaxaKeys: TStringList);
    function CellOrNull(const ACol, ARow: Integer): string;
    function CellOrValue(ACol, ARow: Integer; const AValue: string): string;
    procedure CheckTaxon;
    procedure CleanUp;
    procedure ClearData;
    function ColByName(const AColumnName: string): Integer;
    procedure CreateSampleRecord(const ASpatialRef: String; AMainSample: boolean = false);
    procedure CreateBiotopeOccurrence(const ASampleKey, ABLIKey, ADeterminerKey: TKeyString;
        const ADetDate: String; const AComment: String = '');
    procedure CreateSpeciesOccurrences;
    procedure DropBiotope(const Sender: TObject; const iFormat: integer;
        const iSourceData: TKeyList; const iTextStrings: TStringList; var ioHandled: boolean);
    procedure DropRecorder(const Sender: TObject; const iFormat: integer;
        const iSourceData: TKeyList; const iTextStrings: TStringList; var ioHandled: boolean);
    procedure DropReference(const Sender: TObject; const iFormat: integer;
        const iSourceData: TKeyList; const iTextStrings: TStringList; var ioHandled: boolean);
    procedure DropSpecies(const Sender: TObject; const iFormat: integer;
        const iSourceData: TKeyList; const iTextStrings: TStringList; var ioHandled: boolean);
    procedure DropTaxon(const Sender: TObject; const iFormat: integer;
        const iSourceData: TKeyList; const iTextStrings: TStringList; var ioHandled: boolean);
    function FindTaxonInList(AName: String): String;
    function FindTaxonInListUsingPartialTaxonSearch(const AName: String; ASpacePos: Integer): String;
    function GetLastSurvey : TKeyString;
    procedure GetListOfTaxa;
    procedure GotAddedTaxon(const iItemKey: TKeyString; AClearValues: Boolean = False);
    procedure GridBiotopeUpdate(KeyList: TKeyList);
    procedure GridDeterminerUpdate(KeyList: TKeyList);
    procedure GridSpatialRefUpdate(KeyList: TKeyList);
    procedure InitialiseKeyVariables;
    function IsFixedCol(ACol: Integer): Boolean;
    function IsReadOnlyCell(ACol, ARow: Integer): Boolean;
    function IsCellCustodian(ACol, ARow: Integer): Boolean;
    function KeyOrNull(const AListCombo: TDBListCombo; ACol, ARow: Integer;
        const ADefault: String = ''): String;
    procedure MoveGridCell(const iToTheRight: Boolean);
    procedure ReadRecordingCard;
    procedure ReadSurveyForCard;
    procedure ReadTaxonGroupForCard;
    procedure RetrieveTaxa(iTaxaKeys: TStringList);
    function ReturnKeyDown : boolean;
    procedure ReSortGrid;
    procedure SaveGridLayout;
    procedure SetActiveCellState(const AChar:char);
    procedure SetColWidth(ACol: Integer; const AText: String);
    procedure SetEditing(Value: Boolean);
    procedure SetLastSurvey(const Value: TKeyString);
    procedure SetLocation(const Value: string);
    procedure SetRecordingCardPath(const Value: String);
    procedure SetSampleType(const Value: TKeyString);
    procedure SetSpatialRef(const Value: string);
    procedure SetTotalSpecies(Value: Integer);
    procedure SortGrid(ACol: Integer);
    property TotalSpecies: integer read FTotalSpecies write SetTotalSpecies;
    procedure UpdateSpeciesValues(const AColName: TColumnName; const ARow:integer);
    procedure UpdateTaxon(KeyList: TKeyList);
    procedure ValidateCardDate;
    function ValidCircaMeasurement(measurementKeys: TMeasurementKeys; const AValue: String): Boolean;
    function ValidEstimateMeasurement(measurementKeys: TMeasurementKeys; const AValue: String): Boolean;
    function ValidExactMeasurement(measurementKeys: TMeasurementKeys; const AValue: String): Boolean;
    function ValidRestrictedData(measurementKeys: TMeasurementKeys; const AValue: String): Boolean;
    function CheckGridDeterminers(const ColIdx:integer): Boolean;
    function CheckGridDeterminationDates(const ColIdx:integer): Boolean;
    function CheckCount: Boolean;
    function CheckGridBiotope(const ColIdx: Integer): Boolean;
    function CheckGridMeasurementColumns: Boolean;
    procedure CheckGridSpatialRef(const ColIdx: Integer);
    procedure SaveCard;
    function GetAppropriateSample(const ARow: integer; const ASampleList: TStringList;
        var ABiotopeName: string; const ADeterminerKey, ADetDate: string): string;
    procedure CreateTaxonOccurrenceRecord(const ATaxOccKey, ASampleKey: String; ARow: Integer;
        const ABiotope: String);
    property LastSurvey: TKeyString read GetLastSurvey write SetLastSurvey;
    function GetRowTLIKey(Index: Integer): TkeyString;
    procedure SetRowTLIKey(Index: Integer; const Value: TKeyString);
    function GetRowChecked(Index: Integer): Boolean;
    procedure SetRowChecked(Index: Integer; Value: Boolean);
    procedure CheckRow(ARow: integer);
    function GetRowTaxonOccKey(Index: Integer): TKeyString;
    procedure SetRowTaxonOccKey(Index: Integer; const Value: TKeyString);
    function GetRowFromSample(Index: Integer): Boolean;
    procedure SetRowFromSample(Index: Integer; Value: Boolean);
    function GetRowCustodian(Index: Integer): TKeyString;
    procedure SetRowCustodian(Index: Integer; const Value: TKeyString);
    procedure fraLocationInfoUpdateLocation(Sender: TObject);
    procedure BuildRapidEntryTaxonHint;
    procedure SetTaxonEntryDelimiterCount(const Value: integer);
    function GetTaxonEntryDelimiterCount: integer;
    procedure DisplayRapidTaxonEntryHint();
    function CountDelimiters: integer;
    procedure ParseAndInputNewTaxonData;
    procedure ParseNewTaxonData(const Items: TStringList);
    function AddNewTaxon(const Data: string; const AClearValues: Boolean = False): Boolean;
    procedure InputNewTaxonData(const Items: TStringList);
    function MatchListContents: Boolean;
    function GridEditingComboBox(const ACol: integer): TComboBox;
    procedure AddRecorder(const AText, AKey: string);
    procedure RemoveTaxonFromGrid;
    function GetTaxonKeyFromSearchCode(const ASearchCode: string): TKeyString;
    procedure ClearSpeciesValues;
    function DataRestrictedMeasurement(ACol: Integer): Boolean;
    function HasColumnName(col: Integer): Boolean;
    function PopulateColumns(rowIdx: Integer; data: _Recordset): Boolean;
    procedure UpdateSample;
    procedure UpdateTaxonOccurrence(row: Integer);
    procedure UpdateMeasurements(row: Integer; const taxonOccurrentKey: TKeyString);
    property RowTLIKey[Index: Integer]: TKeyString read GetRowTLIKey write SetRowTLIKey;
    property RowChecked[Index: Integer]: Boolean read GetRowChecked write SetRowChecked;
    property RowTaxonOccKey[Index: Integer]: TKeyString read GetRowTaxonOccKey write SetRowTaxonOccKey;
    property RowFromSample[Index: Integer]: Boolean read GetRowFromSample write SetRowFromSample;
    property RowCustodian[Index: Integer]: TKeyString read GetRowCustodian write SetRowCustodian;
    property TaxonEntryDelimiterCount: integer read GetTaxonEntryDelimiterCount
        write SetTaxonEntryDelimiterCount;
    procedure InitVariables;
    procedure DoSizing;
  protected
    function AddTaxonNamesToList: Integer;
    procedure SetupDestinationControls; override;
    procedure UpdateObservations(updateCaptions: Boolean = False);
    function GetCurrentControlEditMode: TEditMode; override;
  public
    constructor Create(AOwner: TComponent); reintroduce; overload;
    constructor Create(AOwner: TComponent; iClsID: TGUID); reintroduce; overload;
    destructor Destroy; override;
    procedure BiotopeUpdate(KeyList: TKeyList);
    procedure LoadFromSample(const ASurveyKey, AEventKey, ASampleKey: TKeyString;
        const ASampleCaption: String);
    procedure PreviewScreen; override;   // Called by File/Preview
    procedure PrintScreen; override;    // Called by File/Print
    procedure RecorderUpdate(KeyList: TKeyList);
    procedure ReferenceUpdate(KeyList: TKeyList);
    procedure SetDisplaySystem;
    procedure ValidateCard;
    procedure UpdateMapWindowSelector; override;
    property Editing:boolean read FEditing write SetEditing;
    property PassedSpatialRef: string read FSpatialRef write SetSpatialRef;
    property PassedLocation: string read FLocation write SetLocation;
    property RecordingCardPath: string read FRecordingCardPath write SetRecordingCardPath;
    property SampleType: TKeyString read FSampleType write SetSampleType;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, Map, Find, Maintbar, NewPlaceCard, GeneralData, ApplicationSettings,
  JNCCDatasets, Observations, SpatialRefFuncs, COMObj, Registry, StrUtils, DatabaseAccessADO,
  Types, BaseADODataModule, SampleData, TaxonOccurrData, Finder;

resourcestring
  SPlaceCard_RequireQualifier =
      'A spatial reference qualifier must be entered to support the spatial reference.';
  SPlaceCard_SelectOne          = 'At least one must be selected.';
  SPlaceCard_BuildingRecordCard = 'Building record card...';
  SPlaceCard_SpeciesPresent     = '%s species present.';
  SPlaceCard_Circa              = 'Circa';
  SPlaceCard_UnselectSpecies    = 'Do you really want to un-select this species?';
  SPlaceCard_SaveChangesConfirm =
      'Do you want to save the observations you have entered on this record card? Please note that '
      + 'if you select No then the information entered will be lost.';
  SPlaceCard_SaveCardChangesConfirm =
      'Do you want to save the changes to the record card template?';
  SPlaceCard_ValidVagueDates   = 'Enter valid vague dates or leave blank.';
  SPlaceCard_NoKeysAvailable   = 'No more keys are available in %s';
  SPlaceCard_SelectSurvey      = 'Please select a Survey.';
  SPlaceCard_SelectValidSpatialRef    = 'Please select a valid Spatial Reference or leave blank.';
  SPlaceCard_RetrievingTaxaFromList   = 'Retrieving taxa from list...';
  SPlaceCard_SingleOccurrenceRecorded = 'A taxon occurrence has been saved.';
  SPlaceCard_MultiOccurrenceRecorded  = ' taxon occurrences have been saved.';
  SPlaceCard_InvalidBiotope =
      'A Biotope found in the grid is invalid. Select a valid biotope or leave blank.';
  SPlaceCard_CleanUpFailed =
      'A failure occurred during saving.  Data may have been changed in the database that could not be undone.';
  SPlaceCard_RecorderNameMissing   = 'A Recorder Name is missing. ';
  SPlaceCard_SpatialRefNotWithin   = 'A spatial reference found in the grid does not fall within ';
  SPlaceCard_SpatialRefNotInGridSq =
      'A Spatial Reference found in the grid does not fall in the specified location''s grid squares. ';
  SPlaceCard_SpatialRefNotInSample =
      'A Spatial Reference found in the grid does not fall in the specified sample''s Spatial Ref. ';
  SPlaceCard_SpatialRefInGridInvalid = 'A Spatial Reference found in the grid was not recognised. ';
  SPlaceCard_SaveError =
      'An error occurred attempting to save the data.  Another user might be '
      + 'entering some data. Please wait a little while before trying to save again'#13#13
      + 'The database has not been changed.  Message: ';
  SPlaceCard_SaveFailure = 'Record card information could not be saved for the following reason: ';
  SPlaceCard_SomeTaxaIgnored =
      'One or more taxa could not be added to the record card.  ' +
      'This is either because several taxa share the same preferred ' +
      'name or because the checklist is not installed.';
  SPlaceCard_SaveCardChangesCannotSave =
      'The changes you have made to the record card template cannot be saved as you do not have sufficient permissions.';

  ResStr_CannotShowRecordingCard =
      'This recording card was created using a blocked list. It will not be shown.';

  ResStr_IncompleteSurveyEvent =
      'the partially saved data. There may be incomplete survey event or sample data in the system.';

  ResStr_DatabaseQuerying = 'Querying Database...';
  ResStr_UnableToSave     = 'Unable to save this record without a Survey selected. ';

  ResStr_SelectTaxaToSave =
      'Before saving the information on this card, please select one or more '
      + 'taxa from the list to create observations for.';
  ResStr_SurveyNotEqualDefaultConfirm =
      'The survey selected is not the same as the default. Continue with save ?';
  Restr_SurveyNotEqualDefault =
       'Not saved. The survey selected is not the same as the default';
  ResStr_TaxonGroupNotEqualDefaultConfirm =
      'The taxon selected is not in the default taxon group. Continue with save ?';

  ResStr_ObservationDateMissing = 'The Observation Date is missing. Enter a vague date value.';
  ResStr_DataOutsideSurvey      = 'The Date is outside the selected Survey''s range. ';
  ResStr_SampleTypeMissing      = 'The Sample Type is missing. Select a type from the list.';
  ResStr_InvalidBiotope         = 'The Biotope is invalid. Select a valid Biotope or leave blank.';
  ResStr_InvalidDocument        = 'The Document is invalid. Select a valid Document or leave blank.';

  ResStr_InvalidDeterminerName =
      'The Determiner Names must be valid names. Enter a valid name or select one from '
      + 'the existing names, or leave blank.';

  ResStr_InvalidDerterminationDate =
      'The Dates of Determination must be valid vague dates and must occur on or after the date of recording. If a ' +
      'vague determination date has been entered, note that the start of this vague date must not precede the start ' +
      'of the observation date.';

  ResStr_SettingCount =
      '"Count" and "Count Of" must both be set when either one is set. There is some data missing.' ;

  ResStr_InvalidMeasurementValues = 'Some measurement values are invalid.  Please correct them.';
  ResStr_TooManyDelimiters =
      'There were more data items in the input text than columns available.'#10#13
      + 'Additional data items have been ignored.';
  ResStr_NoListMatchFound         = '"%s" does not match any items in the list of attributes for %s.';
  ResStr_MultipleListMatchesFound = '"%s" matches more than one item in the list of attributes for %s.';
  ResSTR_ColumnName               = '%s Name';

  ResStr_RecordingCardMissingColumns =
      'There are measurements from the selected observations that cannot be displayed using'#13#10
      + 'this recording card because it is missing the relevant measurement columns.';

  ResStr_BtnCancel = 'Cancel';

  ResStr_AssociatedBiotope = 'Related to Biotope Occurrence for "%s".';

const
  LAST_KEY                      = 'ZZZZZZZZ';
  MAX_PREVIOUS_COLUMNS_IN_HINT  = 1;
  MAX_FOLLOWING_COLUMNS_IN_HINT = 4;

type
  TRowData = class(TKeyData)
    Checked: Boolean;
    TaxonOccKey: TKeyString;
    FromSample: Boolean;
    Custodian: TKeyString;
  end;

  TCellData = class(TKeyData)
    Custodian: TKeyString;
  end;

//==============================================================================
constructor TfrmPlaceCard.Create(AOwner: TComponent);
var lCursor: TCursor;
begin
  inherited Create(AOwner);
  //Set up spatial reference component
  SetDisplaySystem;

  FIsComHeader       := False;
  InitVariables;
  lCursor := HourglassCursor;
  try
    //Create data module
    FdmPlaceCard := TdmPlaceCard.Create(Self);
    FdmPlaceCard.SetColumnNames(nil, nil);

    cmbSubstrate.Active  := True;
    cmbRecordType.Active := True;
    cmbSampleType.Active := True;
    dmGeneralData.PopulateQualifierCombo(cmbQualifier, NONE_ABUNDANCE_QUALIFIER_KEY);
    sgSpecies.Cells[2, 0] := ResStr_ColumnScientificName;
    sgSpecies.Objects[2, 0] :=
        FdmPlaceCard.MappedColumns.Objects[FdmPlaceCard.IndexOfMappedColumn(COL_SCIENTIFIC_NAME, True)];

    cmbSurvey.KeyValue := LastSurvey;
    if cmbSurvey.ItemIndex = -1 then
      cmbSurvey.KeyValue := '';

    Editing := False;
    SetRequiredFieldsColourState(True, [cmbSurvey, eDate, cmbSampleType, lbRecorders]);
    SetReadOnlyFieldsColourState(True, [eAdminArea]);
    fraLocationInfo.SetColour(MergeColours(AppSettings.MandatoryColour, clWindow, 25));

    cmbSurvey.Active := True;
    bbRecorderRemove.Enabled := False;
    SendMessage(Handle, WM_UPDATE_MENU_ICONS, 0, 0);
    fraLocationInfo.OnUpdateLocation := fraLocationInfoUpdateLocation;
    fraLocationInfo.OnChangedContent := fraLocationInfoUpdateLocation;
  finally
    DefaultCursor(lCursor);
  end;
  //Help Setup
  Self.HelpContext    := IDH_PLACECARD;
  mnuEdit.HelpContext := IDH_EDITMENU;

  lblTotalSpecies.Caption := Format(SPlaceCard_SpeciesPresent, ['0']);
  UpdateMapWindowSelector;
  FRapidTaxonEntryHintWindow := TTaxonHintWindow.Create(Self);
  FRapidTaxonEntryHintWindow.Color := clInfoBk;
  FEditMode := emEdit;
  DoSizing;
end;  // Create

//------------------------------------------------------------------------------
{ overloaded constructor which replaces components on Top panel with a COM
     addin }
constructor TfrmPlaceCard.Create(AOwner: TComponent; iClsID: TGUID);
begin
  inherited Create(AOwner);
  FIsComHeader      := True;
  InitVariables;
  pnlHeader.Visible := False;
  FComHeader        := TOleProxy.Create(Self, iClsID);
  FComHeader.Align  := alTop;
  FComHeader.Parent := Self;
  FComHeader.Height := pnlHeader.Height;
  FdmPlaceCard:= TdmPlaceCard.Create(Self);
  FdmPlaceCard.SetColumnNames(nil, nil);
  cmbSurvey.KeyValue := LastSurvey;

  FTaxaAbbreviations := TStringList.Create;

  //Help Setup
  Self.HelpContext    := IDH_PLACECARD;
  mnuEdit.HelpContext := IDH_EDITMENU;

  UpdateMapWindowSelector;
  FRapidTaxonEntryHintWindow := TTaxonHintWindow.Create(Self);
  FRapidTaxonEntryHintWindow.Color := clInfoBk;
  FEditMode := emEdit;
  DoSizing;
end;  // Create

procedure TfrmPlaceCard.DoSizing;
var
  availableWidth: integer;
begin
  availableWidth := pnlHeader.Width - cmbSurvey.Left - 8;
  cmbSurvey.Width := availableWidth;
  eReference.Width := availableWidth - bbReferenceFind.Width - 1;
  shpReference.Width := eReference.Width + 2;
  bbReferenceFind.Left := shpReference.Left + shpReference.Width;
end;

(*==============================================================================
 * Set up some variables that are necessarry no matter which constructor we use.
 *)
procedure TfrmPlaceCard.InitVariables();
begin
  FSpeciesValues     := TStringList.Create;
  FEditingCell       := False;
  FAdditionalTaxa    := TStringList.Create;
  FRemovedTaxa       := TStringList.Create;
  FTaxaAbbreviations := TStringList.Create;
  FColsResized       := False;
end;

//==============================================================================
procedure TfrmPlaceCard.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(true);
  frmMain.SetContextToolbar(Self, mnuEdit, 0, []);
  dmFormActions.UpdateRTFMenu(reComments.Focused);
  if eTaxon.Focused then
    DisplayRapidTaxonEntryHint;
end;  // FormActivate

{-------------------------------------------------------------------------------
}
procedure TfrmPlaceCard.FormDeactivate(Sender: TObject);
begin
  inherited;
  FRapidTaxonEntryHintWindow.ReleaseHandle;
end;

//==============================================================================
procedure TfrmPlaceCard.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  CanClose := False;
  if Editing or fraLocationInfo.eSpatialRef.Modified then begin
    case MessageDlg(SPlaceCard_SaveChangesConfirm, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes : begin
                bbSaveClick(nil);
                CanClose := True;
              end;
      mrNo  : begin
                FClosingForm := True;
                CanClose     := True;
                FClosingForm := False;
              end;
    end;
    Editing := False;
    fraLocationInfo.eSpatialRef.Modified := False;
  end else
    CanClose := True;
  // User wants to close form, save grid layout (column widths)
  if CanClose then SaveGridLayout;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmPlaceCard.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
end;  // FormClose

//==============================================================================
destructor TfrmPlaceCard.Destroy;
var i: Integer;
begin
  // Clear all objects from the grid
  with sgSpecies do begin
    for i := FixedRows to RowCount - 1 do begin
      Objects[0, i].Free;
      Objects[0, i] := nil;
    end;
  end;

  // Clear all objects from recorder listbox
  with lbRecorders.Items do
    for i := 0 to Count - 1 do Objects[i].Free;

  // Clear all objects from Qualifier combo
  for i := 0 to cmbQualifier.Items.Count - 1 do
    cmbQualifier.Items.Objects[i].Free;

  FreeAndNil(FSpeciesValues);
  FreeAndNil(FAdditionalTaxa);
  FreeAndNil(FRemovedTaxa);
  FreeAndNil(FTaxaAbbreviations);

  // Doesn't matter if already Nil
  FreeAndNil(FSampleKeys);
  FreeAndNil(FBiotopeOccKeys);
  FreeAndNil(FTaxonOccKeys);
  FreeAndNil(FSERKeys);

  FreeAndNil(FLoadedSampleValues);
  // FdmPlaceCard is owned by the form, which is then in charge of getting rid of it.
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TfrmPlaceCard.WMTransferDone(var Msg: TMessage);
begin
  Show;
  Editing := True;
end;  // WMTransferDone

//==============================================================================
{ Setup anything we can drag or drop }
procedure TfrmPlaceCard.SetupDestinationControls;
begin
  fraLocationInfo.RegisterDragDrop(Self);
  RegisterDropComponent(eBiotope,   DropBiotope,  [TN_BIOTOPE_LIST_ITEM],   [CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(lbRecorders,DropRecorder, [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA]);
  RegisterDropComponent(eReference, DropReference,[TN_REFERENCE],           [CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(sgSpecies,  DropSpecies,  [TN_TAXON_LIST_ITEM],     [CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(eTaxon,     DropTaxon,    [TN_TAXON_LIST_ITEM],     [CF_JNCCDATA, CF_TEXT]);
  // Register the grid for standard Copy/Paste functionalities
  RegisterCopyPasteComponent(sgSpecies);
end;  // SetupDestinationControls

//==============================================================================
procedure TfrmPlaceCard.SetEditing(Value: boolean);
begin
  if Value <> FEditing then
  begin
    FEditing := Value;
    bbReset.Enabled := FEditing;
  end;
end;  // SetEditing

//==============================================================================
procedure TfrmPlaceCard.SetTotalSpecies(Value: Integer);
begin
  FTotalSpecies := Value;
  lblTotalSpecies.Caption := Format(SPlaceCard_SpeciesPresent, [IntToStr(TotalSpecies)]);
end;  // SetTotalSpecies

//==============================================================================
procedure TfrmPlaceCard.SetRecordingCardPath(const Value: String);
var
  cardName: String;
begin
  FRecordingCardPath := Value;
  //Get filename without path
  cardName := ExtractFileName(Value);
  //Get filename without extension
  Caption := Copy(cardName, 1, Length(cardName) - Length(ExtractFileExt(cardName)));
  Refresh;
  ReadRecordingCard;
  ReadSurveyForCard;
  ReadTaxonGroupForCard;
end;  // SetRecordingCardPath
//==============================================================================
procedure TfrmPlaceCard.ReadSurveyForCard;
var lCardFile: TextFile;
    lCardName: String;
    lCurrent : String;
    lCursor  : TCursor;
begin
  lCursor    := HourglassCursor;
  FLoading   := True;
  //Open file
  lCardName := ExtractWithoutExt(FRecordingCardPath);
  AssignFile(lCardFile, FRecordingCardPath);
  try
    Reset(lCardFile);
    //Read general
    while not eof(lCardFile) do begin
      ReadLn(lCardFile, lCurrent);
        If lCurrent = '<Survey>' then begin
          ReadLn(lCardFile, lCurrent);
          FDefaultSurveyKey := lCurrent;
          ReadLn(lCardFile, lCurrent);
          if lCurrent <> '</Survey>' then
            raise ECRDError.CreateNonCritical(Format(ResStr_BadRecCard, [lCardName]));
          cmbSurvey.KeyValue := FDefaultSurveyKey;
        end;
    end;

  finally
    //Close file
    CloseFile(lCardFile);
    DefaultCursor(lCursor);
    FLoading := False;
  end;

end;
//==============================================================================
procedure TfrmPlaceCard.ReadTaxonGroupForCard;
var lCardFile: TextFile;
    lCardName: String;
    lCurrent : String;
    lCursor  : TCursor;
begin
  lCursor    := HourglassCursor;
  FLoading   := True;
  //Open file
  lCardName := ExtractWithoutExt(FRecordingCardPath);
  AssignFile(lCardFile, FRecordingCardPath);
  try
    Reset(lCardFile);
    //Read general
    while not eof(lCardFile) do begin
      ReadLn(lCardFile, lCurrent);
        If lCurrent = '<TaxonGroup>' then begin
          ReadLn(lCardFile, lCurrent);
          FDefaultTaxonGroupKey := lCurrent;
          ReadLn(lCardFile, lCurrent);
          if lCurrent <> '</TaxonGroup>' then
            raise ECRDError.CreateNonCritical(Format(ResStr_BadRecCard, [lCardName]));
        end;
    end;

  finally
    //Close file
    CloseFile(lCardFile);
    DefaultCursor(lCursor);
    FLoading := False;
  end;

end;

//==============================================================================
//    Procedure Name: SetColWidth    Author: GAD    Date: 2/8/99
//           Purpose: Determines the width of the header text and sets the
//                    col width accordingly.
//                    Count and count of widths are special cases.
//        Parameters: iCol: integer - column to set.
//      Side Effects: Alters sgSpecies column widths!
//------------------------------------------------------------------------------
procedure TfrmPlaceCard.SetColWidth(ACol: Integer; const AText: String);
var
  newWidth: Integer;
begin
  // Stop resizing if some column widths were read from the file
  if not FUserColumnWidths then
    //Make sure we are working with the right font!
    with sgSpecies do begin
      Canvas.Font := Self.Font;
      //See how big the caption will be in this font and set the colwidth to that.
      if CompareText(AText, ResStr_ColumnCountOf) = 0 then  // special case
        ColWidths[ACol] := 130
      else begin
        newWidth := Canvas.TextWidth(AText + 'X'); // Extra bit for margin
        if (newWidth > ColWidths[ACol]) or (AText = ResStr_ColumnCountOf) then
          ColWidths[ACol] := newWidth;
      end;
    end;
end;  // SetColWidth

//==============================================================================
// Procedure Name: AddTaxonToGrid     Author: GAD    Date: 20/7/99
//        Purpose: Adds a new taxon to the stringgrid
//     Parameters: iItemText: string - The Taxon's scientific name.
//                 iItemKey: TKeyString - The Taxon's List_Item_Key.
//                 iCommonName: string - The Taxon's common name!
//   Side Effects: Adds a row to sgSpecies.
//------------------------------------------------------------------------------
procedure TfrmPlaceCard.AddTaxonToGrid(const iItemKey: TKeyString;
  const iItemText, iCommonName, iCodeNumber: String; IsCritical: Boolean);
var
  liCol: Integer;
begin
  with sgSpecies do begin
    // If the final row is populated, add a new row.
    if Cells[2, RowCount - 1] <> '' then
      RowCount := RowCount + 1;
    Row := RowCount - 1;
    // Scientific Name is on column 2 and nowhere else
    if IsCritical then Cells[1, RowCount - 1] := '!'
                  else Cells[1, RowCount - 1] := '';
    Cells[2, RowCount - 1] := iItemText;
    SetColWidth(2, iItemText);

    // If we have a Common Name column, add it too
    liCol := ColByName(COL_COMMON_NAME);
    if liCol > -1 then begin
      Cells[liCol, RowCount - 1] := iCommonName;
      SetColWidth(liCol, iCommonName);
    end;

    // If we have a Code Number column, add that
    liCol := ColByName(COL_CODE_NUMBER);
    if liCol > -1 then begin
      Cells[liCol, RowCount - 1] := iCodeNumber;
      SetColWidth(liCol, iCodeNumber);
    end;
    RowTLIKey[RowCount-1] := iItemKey;
  end;
end;  // AddTaxonToGrid

(*==============================================================================
 * Checks that the record card *.crd file can be written to. If so, then checks
 * if either the column widths or the species on the list have been changed,
 * and gives the user the chance to save these changes.
 *)
procedure TfrmPlaceCard.SaveGridLayout;
var lFileLines: TStringList;
    lIdx, i   : Integer;
    lCursor   : TCursor;
    lAttrs    : Integer;
    testHandle: Integer;
begin
  // Check the file does not have the read-only attribute set.
  lAttrs := FileGetAttr(FRecordingCardPath);
  // and we have write access, by temporarily opening the file to write
  testHandle := FileOpen(FRecordingCardPath, fmOpenWrite);
  if (testHandle<>-1) then FileClose(testHandle);
  if ((lAttrs and SysUtils.faReadOnly) <> 0) or (testHandle=-1) then
  begin
     if ((FAdditionalTaxa.Count > 0) or (FRemovedTaxa.Count > 0) or FColsResized) then
       ShowInformation(SPlaceCard_SaveCardChangesCannotSave);
     Exit;
  end;

  //  Don't save if the list is not allowed for data entry, as it will not have loaded
  if (not FAllowedList) then Exit;

  lCursor   := HourglassCursor;
  lFileLines:= TStringList.Create;
  try
    lFileLines.LoadFromFile(FRecordingCardPath);

    // Add/Update "Column Widths" section, after "Columns" AND "Column Names"
    lIdx := lFileLines.IndexOf('<Column Widths>');  // Might not exist, if first time round
    if lIdx = -1 then lIdx := lFileLines.IndexOf('</Column Names>') + 1;  // Might not exist either
    if lIdx =  0 then lIdx := lFileLines.IndexOf('</Columns>') + 1;       // Always exists

    // Remove all previous widths
    if lFileLines[lIdx] = '<Column Widths>' then begin
      Inc(lIdx);
      while lFileLines[lIdx] <> '</Column Widths>' do
        lFileLines.Delete(lIdx);
    end else begin
      // Insert section
      lFileLines.Insert(lIdx, '</Column Widths>');
      lFileLines.Insert(lIdx, '<Column Widths>');
      Inc(lIdx);
    end;

    for i := sgSpecies.ColCount - 1 downto 0 do
      lFileLines.Insert(lIdx, IntToStr(sgSpecies.ColWidths[i]));
    // only ask to save if they have added or removed taxa or resized columns
    if ((FAdditionalTaxa.Count > 0) or (FRemovedTaxa.Count > 0) or FColsResized) and
       (MessageDlg(SPlaceCard_SaveCardChangesConfirm, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      lIdx := lFileLines.IndexOf('</Taxon>');
      // If section missing, add it. This can happen if card uses a dictionary
      if lIdx = -1 then begin
        lIdx := lFileLines.IndexOf('</Column Widths>');
        Inc(lIdx);
        lFileLines.Insert(lIdx, '</Taxon>');
        lFileLines.Insert(lIdx, '<Taxon>');
        Inc(lIdx);
      end;
      // Now add the new taxa
      for i := 0 to FAdditionalTaxa.Count - 1 do
        lFileLines.Insert(lIdx, FAdditionalTaxa[i]);
      // Remove any taxa that are no longer present
      for i := lIdx downto lFileLines.IndexOf('<Taxon>') do begin
        if FRemovedTaxa.IndexOf(lFileLines[i]) >= 0 then
          lFileLines.Delete(i);
      end;    // for i := lIdx to lFileLines.Count - 1
      // Save back to file.
      lFileLines.SaveToFile(FRecordingCardPath);
    end;
    // Clear both lists to prevent users receiving a double prompt
    //  if they close the entire app when this window is open,
    //  and has added or removed taxa
    FAdditionalTaxa.Clear;
    FRemovedTaxa.Clear;
    FColsResized := False;
  finally
    lFileLines.Free;
    DefaultCursor(lCursor);
  end;
end;

//==============================================================================
procedure TfrmPlaceCard.ReadRecordingCard;
var lCardFile: TextFile;
    lCardName: String;
    lCurrent : String;
    lTaxaKeys: TStringList;
    lColIdx  : Integer;
    lCursor  : TCursor;
begin
  lCursor      := HourglassCursor;
  FLoading     := True;
  lTaxaKeys    := TStringList.Create;
  FAllowedList := true;
  //Open file
  lCardName := ExtractWithoutExt(FRecordingCardPath);
  AssignFile(lCardFile, FRecordingCardPath);
  try
    Reset(lCardFile);
    //Read general
    ReadLn(lCardFile, lCurrent);
    if lCurrent <> '<General>' then
      raise ECRDError.CreateNonCritical(Format(ResStr_BadRecCard, [lCardName]));

    ReadLn(lCardFile, lCurrent);
    if lCurrent = 'Rucksack' then
      FFromRucksack := True
    else
    if lCurrent = 'TaxonDict' then begin
      ReadLn(lCardFile, FTaxonListKey);
      FFromRucksack := False;
      // Check if we are allowed use this list for data entry
      FAllowedList := FdmPlaceCard.CheckTaxonDictionary(FTaxonListKey);
      if not FAllowedList then
      begin
        MessageDlg(ResStr_CannotShowRecordingCard, mtWarning, [mbOK], 0);
        self.Close;
        Exit;
      end;
    end else
      raise ECRDError.CreateNonCritical(Format(ResStr_BadRecCard, [lCardName]));

    // Build grid columns
    with sgSpecies do begin
      while lCurrent <> '</General>' do begin
        if (lCurrent = 'Common') then begin
          ColCount := ColCount + 1;
          Cells[ColCount - 1, 0] := Format(ResStr_ColumnName, [lCurrent]);
          SetColWidth(ColCount - 1, Cells[ColCount - 1, 0]);
        end;
        ReadLn(lCardFile, lCurrent);
      end;

      //Read columns
      ReadLn(lCardFile, lCurrent);
      if lCurrent <> '<Columns>' then
        raise ECRDError.CreateNonCritical(Format(ResStr_BadRecCard, [lCardName]));

      ReadLn(lCardFile, lCurrent);
      while lCurrent <> '</Columns>' do begin
        // Standard column?
        lColIdx := FdmPlaceCard.IndexOfMappedColumn(lCurrent, True);
        if lColIdx <> -1 then begin
          FSpeciesValues.Add(lCurrent + '=');
          ColCount := ColCount + 1;
          Cells[ColCount - 1, 0]   := lCurrent;
          Objects[ColCount - 1, 0] := FdmPlaceCard.MappedColumns.Objects[lColIdx];
          SetColWidth(ColCount - 1, Cells[ColCount - 1, 0]);
        end;
        // Measurement column?
        lColIdx := FdmPlaceCard.IndexOfMappedColumn(lCurrent, False);
        if lColIdx <> -1 then begin
          FSpeciesValues.Add(lCurrent + '=');
          ColCount := ColCount + 1;
          Cells[ColCount - 1, 0]   := lCurrent;
          Objects[ColCount - 1, 0] := FdmPlaceCard.MappedColumns.Objects[lColIdx];
          SetColWidth(ColCount - 1, Cells[ColCount - 1, 0]);
        end;
        ReadLn(lCardFile, lCurrent);
      end;

      ReadLn(lCardFile, lCurrent);
      // Any renamed columns?
      if CompareText(lCurrent, '<Column Names>') = 0 then begin
        ReadLn(lCardFile, lCurrent);
        lColIdx := 3;
        while CompareText(lCurrent, '</Column Names>') <> 0 do begin
          Cells[lColIdx, 0] := lCurrent;
          Inc(lColIdx);
          ReadLn(lCardFile, lCurrent);
        end;
        ReadLn(lCardFile, lCurrent);
      end;

      // Any column widths to read from the file?
      FUserColumnWidths := false;
      if CompareText(lCurrent, '<Column Widths>') = 0 then begin
        FUserColumnWidths := true;
        ReadLn(lCardFile, lCurrent);
        lColIdx := 0;
        while CompareText(lCurrent, '</Column Widths>') <> 0 do begin
          ColWidths[lColIdx] := StrToInt(lCurrent);
          Inc(lColIdx);
          ReadLn(lCardFile, lCurrent);
        end;
        ReadLn(lCardFile, lCurrent);
      end;
    end;  // with sgSpecies

    // Read Taxon keys
    LockWindowUpdate(sgSpecies.Handle);
    try
      //Read rows
      // If from dictionary, get those first
      if not FFromRucksack then
        GetListOfTaxa;
      // If from rucksack, or additional taxa present, process them
      if lCurrent = '<Taxon>' then begin
        ReadLn(lCardFile, lCurrent);
        while lCurrent <> '</Taxon>' do begin
          // Build comma delimited string of taxon keys in quotes.
          // Ignore duplicates BUT DON'T SORT
          if lTaxaKeys.IndexOf('''' + lCurrent + '''') = -1 then
            lTaxaKeys.Add('''' + lCurrent + '''');
          ReadLn(lCardFile, lCurrent);
        end;
        //Now retrieve all the names in one go and populate the grid
        RetrieveTaxa(lTaxaKeys);
      end else
        // If from rucksack, but no taxa found, wrong card format
        if FFromRucksack then
          raise ECRDError.CreateNonCritical(Format(ResStr_BadRecCard, [lCardName]));
      // Assign original sort order
      AssignIndexToGrid(lTaxaKeys);
      ReSortGrid;
    finally
      LockWindowUpdate(0);
    end;

    if not FIsComHeader then
      cmbSurvey.SetFocus;  // Restore focus on first control
    Editing := False;
  finally
    lTaxaKeys.Free;
    //Close file
    CloseFile(lCardFile);
    DefaultCursor(lCursor);
    FLoading := False;
  end;

  // CCN207 - ZENTRUM Delimited rapid data entry
  BuildRapidEntryTaxonHint;
end;  // ReadRecordingCard

//==============================================================================
// Procedure Name: AssignIndexToGrid
//        Purpose: Associates each non-fixed grid row with its original zero-
//                 based row index that it has prior to the grid being sorted by
//                 a column, by storing this value in the Objects collection of
//                 column 1. Used to restore the original order of
//                 the grid rows at a later time.
//     Parameters: ATaxaKeys: TStringList - The list of taxon keys from the
//                 taxon dictionary or rucksack.
//------------------------------------------------------------------------------
procedure TfrmPlaceCard.AssignIndexToGrid(ATaxaKeys: TStringList);
var i: integer;
begin
  with sgSpecies do
    for i := RowCount - 1 downto FixedRows do begin
      // if from rucksack, use rucksack position
      if (ATaxaKeys.Count > 0) and (RowCount = ATaxaKeys.Count + 1) then
        //Objects[1, i] := Ptr(ATaxaKeys.IndexOf('''' + RowTLIKey[i] + ''''))
        Objects[1, i] := TObject(ATaxaKeys.IndexOf('''' + RowTLIKey[i] + ''''))
      else
        Objects[1, i] := TObject(i - 1);
    end;
end;  // AssignIndexToGrid

//==============================================================================
procedure TfrmPlaceCard.GetListOfTaxa;
var lTaxonListItemKeys: TStringList;
begin
  frmMain.SetStatus(ResStr_DatabaseQuerying);
  frmMain.Refresh;
  FdmPlaceCard.StartqryTaxaDetails(FTaxonListKey);
  lTaxonListItemKeys := TStringList.Create;
  lTaxonListItemKeys.Sorted := true;
  lTaxonListItemKeys.Duplicates := dupIgnore;
  LockWindowUpdate(sgSpecies.Handle);
  try
    with FdmPlaceCard.qryTaxaDetails do try
      Open;
      frmMain.SetStatus(SPlaceCard_RetrievingTaxaFromList);
      while not Eof do begin
        lTaxonListItemKeys.Add('''' + FieldByName('TAXON_LIST_ITEM_KEY').AsString + '''');
        Next;
      end;
    finally
      Close;
    end;
    RetrieveTaxa(lTaxonListItemKeys);
  finally
    lTaxonListItemKeys.Free;
    LockWindowUpdate(0);
  end;
  frmMain.ProgressBar.Reset;
  frmMain.SetStatus('');
end;  // GetListOfTaxa

//==============================================================================
procedure TfrmPlaceCard.RetrieveTaxa(iTaxaKeys: TStringList);
var
  liCurrent   : Integer;
  liMaxInBlock: Integer;
  liAddedCount: Integer;
const
  MAX_IN_LIST = 2000;
begin
  FUsedPreferredTaxaList := TStringList.Create; // to track duplication
  FUsedPreferredTaxaList.Sorted := True;
  FUsedPreferredTaxaList.Duplicates := dupError;
  try
    liAddedCount := 0;
    if iTaxaKeys.Count>0 then begin
      //make sure everything looks pretty
      frmMain.SetStatus(SPlaceCard_BuildingRecordCard);
      frmMain.Refresh;
      try
        //Do de bizness
        if iTaxaKeys.Count < MAX_IN_LIST then begin
          // easy way - use only one query
          FdmPlaceCard.InitTaxaNames(iTaxaKeys);
          frmMain.ProgressBar.Position := 50;
          Inc(liAddedCount, AddTaxonNamesToList);
          frmMain.ProgressBar.Position := 100;
        end else begin
          // need to handle blocks of taxa, as could break 64K query limit!
          liCurrent := 0;
          while liCurrent < iTaxaKeys.Count - 1 do begin
            if liCurrent + MAX_IN_LIST > iTaxaKeys.Count - 1 then
              liMaxInBlock := iTaxaKeys.Count - 1
            else
              liMaxInBlock := liCurrent + MAX_IN_LIST;
            FdmPlaceCard.InitTaxaNamesInRange(iTaxaKeys, liCurrent, liMaxInBlock);
            frmMain.Progressbar.Position := (liCurrent + liMaxInBlock) * 50 div iTaxaKeys.Count;
            Inc(liAddedCount, AddTaxonNamesToList);
            frmMain.Progressbar.Position := liMaxInBlock * 100 div iTaxaKeys.Count;
            liCurrent := liMaxInBlock + 1;
          end;
        end;
      finally
        frmMain.ProgressBar.Reset;
        frmMain.SetStatus('');
      end;
    end;
    if liAddedCount < iTaxaKeys.Count then
      MessageDlg(SPlaceCard_SomeTaxaIgnored, mtInformation, [mbOk], 0);
  finally
    FUsedPreferredTaxaList.Free;
  end; // try
  // Focus on first row
  sgSpecies.Row := 1;
end;  // RetrieveTaxa

//==============================================================================
{ Procedure to take all taxon names from the current query and put them into the
     list.  Returns the number of succesful additions }
function TfrmPlaceCard.AddTaxonNamesToList: Integer;
var
  count, idx: Integer;
  itemName, abbr: String;
begin
  count := 0;
  with FdmPlaceCard.qryTaxaNames do
  begin
    First;
    while not Eof do
      try
        { Used FUsedPreferredTaxaList to ensure we add items only once }
        FUsedPreferredTaxaList.Add(FieldByName('ListItemKey').AsString);
        itemName := FieldByName('ItemName').AsString;
        abbr     := FieldByName('Abbreviation').AsString;

        AddTaxonToGrid(
            FieldByName('OriginalKey').AsString,
            itemName,
            FieldByName('CommonName').AsString,
            FieldByName('CodeNumber').AsString,
            FieldByName('Critical').AsInteger = 3);

        // Could several items with same abbreviation (yeah, dumb)
        idx := FTaxaAbbreviations.IndexOfName(abbr);
        if idx = -1 then
          FTaxaAbbreviations.AddObject(abbr + '=' + itemName, TObject(0))
        else begin
          // Use some marker to indicate multiple entries for same abbr.
          FTaxaAbbreviations.Objects[idx] := TObject(1);
          FTaxaAbbreviations.AddObject(abbr + '=' + itemName, TObject(1));
        end;
        Inc(count);
        Next;
      except on EStringListError do // Second instance of an item so ignore
        Next;
      end;
    Close;
  end; //with qryTaxaNames
  Result := count;
end;  // AddTaxonNamesToList

//==============================================================================
procedure TfrmPlaceCard.ChangeEditState(Sender: TObject);
var
  editOk: Boolean;
begin
  inherited;
  if FLoadedFromSample then begin
    editOk := False;
    if (Custodian <> AppSettings.SiteID) then
      ShowInformation(ResStr_NotAllowedToEditOtherSitesData)
    else
    if AppSettings.RestrictFullEdit then
      ShowInformation(ResStr_NotAllowedToEditOtherPersonsData)
    else
      editOk := True;
  end else
    editOk := True;

  if editOk then begin
    Editing := True;
    if fraLocationInfo.eLocation.Text = '' then
      eAdminArea.Text := '';
    // If changing Biotope, blank ListItemKey, so that we can check it's a valid one
    if Sender = eBiotope then FBiotopeListItemKey := '';
  end;
end;  // ChangeEditState

//==============================================================================
procedure TfrmPlaceCard.eDateExit(Sender: TObject);
begin
  inherited;
  if not FClosingForm then ValidateCardDate;
end;  // eDateExit

//------------------------------------------------------------------------------
procedure TfrmPlaceCard.ValidateCardDate;
begin
  if eDate.Text <> '' then begin
    ValidateValue(CheckVagueDate(eDate.Text), InvalidDate(ResStr_SampleDate, true, false), eDate);
    eDate.Text := VagueDateToString(eDate.VagueDate);
  end;
end;  // ValidateCardDate

//------------------------------------------------------------------------------
procedure TfrmPlaceCard.cmbSampleTypeDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  lCaptionTopSpacing, lIndex : integer;
begin
  inherited;
  with cmbSampleType do begin
    Canvas.FillRect(Rect);
    lCaptionTopSpacing:=(ItemHeight-Canvas.TextHeight('A')) div 2;
    { Get the index of the image }
    lIndex := FdmPlaceCard.GetGlyphIndex(items[Index]);
    { Draw its glyph if it has one }
    if lIndex > -1 then
      dmFormActions.ilSampleTypes.Draw(Canvas, Rect.Left + 2, Rect.Top, lIndex, true);
    { display the text }
    Canvas.TextOut (Rect.Left + dmFormActions.ilSampleTypes.Width + 6,
                    Rect.Top + lCaptionTopSpacing,
                    Items[Index]);
  end;
end;  // DrawListItem

//==============================================================================
procedure TfrmPlaceCard.eBiotopeKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if (Key = #13) and (FBiotopeListItemKey = '') then begin
    FdmPlaceCard.CheckBiotope(FBiotopeListItemKey, eBiotope);
    Key := #0;
  end;
end;  // eBiotopeKeyPress

//------------------------------------------------------------------------------
procedure TfrmPlaceCard.DropBiotope(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: boolean);
begin
  if Assigned(iSourceData) then
    if iSourceData.Header.ItemCount > 0 then begin
      // Set text first, because of OnChange event that clears the key!
      eBiotope.Text := dmGeneralData.GetBiotopeCodeName(iSourceData.Items[0].KeyField1);
      FBiotopeListItemKey := iSourceData.Items[0].KeyField1;
    end;
end;  // DropBiotope

//------------------------------------------------------------------------------
procedure TfrmPlaceCard.bbBiotopeFindClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actBiotopeDiction.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, BiotopeUpdate);
end;  // bbBiotopeFindClick

//------------------------------------------------------------------------------
procedure TfrmPlaceCard.BiotopeUpdate(KeyList: TKeyList);
begin
  if Assigned(KeyList) then
    try
      if KeyList.Header.ItemCount > 0 then begin
        // Set text first, because of OnChange event that clears the key!
        eBiotope.Text:=dmGeneralData.GetBiotopeCodeName(KeyList.Items[0].KeyField1);
        FBiotopeListItemKey:= KeyList.Items[0].KeyField1;
      end;
    finally
      KeyList.Free;
    end;
end;  // BiotopeUpdate

//==============================================================================
procedure TfrmPlaceCard.DropRecorder(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: boolean);
var
  KeyList : TKeyList;
  stItem : string;
  lCount : integer;
begin
  KeyList := iSourceData;
  if (KeyList <> nil) then
    for lCount:=0 to KeyList.Header.ItemCount-1 do
      if CompareText(KeyList.Items[lCount].KeyField2,'Individual')=0 then begin
        stItem:=dmGeneralData.GetIndividualName(KeyList.Items[lCount].KeyField1);
        AddRecorder(stItem, KeyList.Items[lCount].KeyField1);
      end;
end;  // DropRecorder

//------------------------------------------------------------------------------
procedure TfrmPlaceCard.bbRecorderFindClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actNames.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, RecorderUpdate);
end;  // bbRecorderFindClick

//------------------------------------------------------------------------------
procedure TfrmPlaceCard.bbRecorderAddClick(Sender: TObject);
var
  lFind : TdlgFind;
begin
  lFind := TdlgFind.CreateDialog(nil,ResStr_FindRecorderName, ftIndividual);
  with lFind do begin
    try
      SetSearchText('',true);
      if not eSearchText.NoSourceItems then begin
        if ShowModal = mrOK then
          AddRecorder(ItemText, ItemKey);
      end else
        MessageDlg(ResStr_NoIndividualItems, mtInformation, [mbOK], 0);
    finally
      Release;
    end;
  end;
end;  // bbRecorderAddClick

//------------------------------------------------------------------------------
procedure TfrmPlaceCard.bbRecorderRemoveClick(Sender: TObject);
var lIdx:integer;
begin
  with lbRecorders do
    if SelCount>0 then begin
      lIdx:=Items.Count-1;
      while lIdx>-1 do begin
        if Selected[lIdx] then begin
          Items.Objects[lIdx].Free;
          Items.Delete(lIdx);
        end;
        Dec(lIdx);
      end;
      Editing := True;
      bbRecorderRemove.Enabled:=Items.Count>0;
    end;
end;  // bbRecorderRemoveClick

//------------------------------------------------------------------------------
procedure TfrmPlaceCard.RecorderUpdate(KeyList: TKeyList);
var
  stItem : string;
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then
      if CompareText(KeyList.Items[0].KeyField2, 'Individual') = 0 then
      begin
        stItem:=dmGeneralData.GetIndividualName(KeyList.Items[0].KeyField1);
        AddRecorder(stItem, KeyList.Items[0].KeyField1);
      end else
        MessageDlg(ResStr_NoOrgRecorders, mtInformation, [mbOk], 0);
  finally
    KeyList.Free;
  end;
end;  // RecorderUpdate

//==============================================================================
procedure TfrmPlaceCard.DropReference(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: boolean);
var
  KeyList : TKeyList;
begin
  KeyList := iSourceData;
  if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then begin
    //Get key
    FReferenceKey  :=KeyList.Items[0].KeyField1;
    eReference.Text:=dmGeneralData.GetReferenceText(FReferenceKey);
  end;
end;  // DropReference

//==============================================================================
procedure TfrmPlaceCard.eReferenceKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    FdmPlaceCard.CheckReference(FReferenceKey,eReference);
    Key:=#0;
  end;
end;  // eReferenceKeyPress

//------------------------------------------------------------------------------
procedure TfrmPlaceCard.bbReferenceFindClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actDocuments.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, ReferenceUpdate);
end;  // bbReferenceFindClick

//------------------------------------------------------------------------------
procedure TfrmPlaceCard.ReferenceUpdate(KeyList: TKeyList);
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then begin
      //Get key
      FReferenceKey  :=KeyList.Items[0].KeyField1;
      eReference.Text:=dmGeneralData.GetReferenceText(FReferenceKey);
    end;
  finally
    KeyList.Free;
  end;
end;  // ReferenceUpdate

//==============================================================================
function TfrmPlaceCard.AddNewTaxonToGrid(const AKey: String;
  AClearValues, ADuplicatesAllowed: Boolean): Integer;
var
  taxonName, codeNumber,
  commonName                : String;
  preferredKey              : String;
  i, colIdx                 : Integer;
  isCritical, needToAddTaxon: Boolean;
begin
  needToAddTaxon := True;
  with dmDatabase.ExecuteSQL(Format(
      'SELECT Preferred_Name, Common_Name, Recommended_Taxon_List_Item_Key FROM Index_Taxon_Name '
      + 'WHERE Taxon_List_Item_Key=''%s'' AND System_Supplied_Data=1', [AKey]), true) do
  begin
    if not (Eof or Bof) then begin
      commonName   := VarToStr(Fields['Common_Name'].Value);
      taxonName    := VarToStr(Fields['Preferred_Name'].Value);
      preferredKey := VarToStr(Fields['Recommended_Taxon_List_Item_Key'].Value);
    end else
      raise EPlacesError.Create('AddNewTaxonToGrid - key ' + AKey + ' not found.');
  end;

  with sgSpecies do begin
    // Look to see whether taxon already present in list
    i := Cols[ColByName(COL_SCIENTIFIC_NAME)].IndexOf(taxonName);
    if i > 0 then
      if not (RowChecked[i] and ADuplicatesAllowed) then begin
        needToAddTaxon := False;
        Row := i;                                  // Select row for taxon
        if AClearValues then ClearSpeciesValues;   // Remove previously selected values
        if not RowChecked[i] then CheckTaxon;      // Check off occurrence if not selected
      end;

    if needToAddTaxon then begin
      with dmDatabase.ExecuteSQL(
          'SELECT Validation_Level, Lst_Itm_Code '
          + 'FROM Taxon_List_Item TLI, Taxon_Version TV '
          + 'WHERE TV.Taxon_Version_Key = TLI.Taxon_Version_Key '
          + '  AND TLI.Taxon_List_Item_Key = ''' + preferredKey + ''' ', True) do
      begin
        if not VarIsNull(Fields['Validation_Level'].Value) then
          isCritical := Fields['Validation_Level'].Value = 3
        else
          isCritical := false;
        codeNumber := VarToStr(Fields['Lst_Itm_Code'].Value);
      end; // with dmGeneralData.qryAllPurpose

      // If last row in table contains a record, then add a new row
      if Cells[2, RowCount - 1] <> '' then RowCount := RowCount + 1;

      RowTLIKey[RowCount - 1] := AKey;
      if isCritical then Cells[1, RowCount - 1] := '!'
                    else Cells[1, RowCount - 1] := '';

      Cells[2, RowCount - 1] := taxonName; // Add Taxon Name

      // Add Common Name, if column present
      colIdx := ColByName(COL_COMMON_NAME);
      if colIdx <> -1 then begin
        Cells[colIdx, RowCount - 1] := commonName;
        SetColWidth(colIdx, commonName);
      end;

      // Add Code Number, if column present
      colIdx := ColByName(COL_CODE_NUMBER);
      if colIdx <> -1 then begin
        Cells[colIdx, RowCount - 1] := codeNumber;
        SetColWidth(colIdx, codeNumber);
      end;

      Row := RowCount - 1; // Select row
      CheckTaxon;          // Tick off the occurrence
      Objects[1, RowCount - 1] := TObject(RowCount - 2); // Update list of objects with that added
    end; // if needToAddTaxon

    Result := Row;
  end; //with sgSpecies
end;  // AddNewTaxonToGrid

//==============================================================================
// Procedure Name: RemoveTaxonFromGrid
//        Purpose: Removes the currently selected row from the species grid.
//------------------------------------------------------------------------------
procedure TfrmPlaceCard.RemoveTaxonFromGrid;
var
  additionalTaxaIndex, lCol, OriginalIndex, i: integer;
begin
  with sgSpecies do begin
    if (Row > 0) then begin
      // WHY IS TOTAL UPDATED ONLY WHEN THE ROW IS CHECKED?
      if RowChecked[sgSpecies.Row] then
        TotalSpecies := TotalSpecies - 1;

      additionalTaxaIndex := FAdditionalTaxa.IndexOf(RowTLIKey[sgSpecies.Row]);
      if (additionalTaxaIndex >= 0) then
        // If the removed taxon already appears in the list of added taxa, remove it
        FAdditionalTaxa.Delete(additionalTaxaIndex)
      else
        // Otherwise add it to the list of removed taxa
        FRemovedTaxa.Add(RowTLIKey[sgSpecies.Row]);

      // Remove any objects (except for column 1) from the relevant grid row
      // Ignore column 1 because it just contains Integers cast as TObject and
      // no memory has been allocated to have to free?

      // Determine the "original index" of the row to be deleted - this is the
      // index that it had when first displayed, before any sorting by columns
      // may have been done.
      OriginalIndex := Integer(Objects[1, Row]);

      // Clear objects that are not on first column.
      for lCol := 0 to ColCount - 1 do begin
        if (lCol <> 1) then begin
          Objects[lCol, Row].Free;
          Objects[lCol, Row] := nil;
        end;
      end;

      // Remove the row from the grid itself
      DelLineInGrid(sgSpecies);

      // Decrement the "original index" stored for each row that itself has an
      // original index greater than the row to be deleted. This ensures that
      // all these indices remain consecutive.
      for i := FixedRows to RowCount - 1 do begin
        if (Integer(Objects[1, i]) > OriginalIndex) then
          Objects[1, i] := TObject(Integer(Objects[1, i]) - 1);
      end;
    end;

    // The object for the row beyond the last should no longer be assigned
    // since it is now the same object as the previous row.
    sgSpecies.Objects[0, sgSpecies.RowCount] := nil;
  end;
end;

//==============================================================================
// Drop a taxon onto the grid adds it to the bottom of the list
procedure TfrmPlaceCard.DropSpecies(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: boolean);
var lCol: Integer;
    lText: String;
    lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    if (iFormat = CF_JNCCDATA) and (iSourceData.Header.ItemCount > 0) and
       SameText(iSourceData.Header.TableName, TN_TAXON_LIST_ITEM) then
    begin
      ioHandled := True;
      GotAddedTaxon(iSourceData.Items[0].KeyField1);
    end else
    if (iFormat = CF_TEXT) and (iTextStrings.Count > 0) then begin
      ioHandled := True;
      lCol := ColByName(COL_COMMON_NAME);
      if lCol = -1 then lCol := 2;
      with sgSpecies do
        if Col > lCol then begin
          if (Col <> ColByName(COL_SUBSTRATE)) and (Col <> ColByName(COL_RECORD_TYPE)) then begin
            if Col = ColByName(COL_COMMENT) then
              lText := iTextStrings.Text
            else
              lText := iTextStrings[0];

            // Replace #13 with space and remove #10.
            Cells[Col,Row] := Trim(
                StringReplace(
                    StringReplace(lText, #13, ' ', [rfReplaceall]),
                    #10, '', [rfReplaceAll]));
            CheckRow(Row);
            UpdateSpeciesValues(TColumnName(Objects[Col, 0]), Row);
            if (Col = ColByName(COL_ACCURACY)) and (cmbAccuracy.Visible) then
              cmbAccuracy.Text := iTextStrings[0];
            if (Col = ColByName(COL_COUNT_OF)) and (cmbQualifier.Visible) then
              cmbQualifier.Text := iTextStrings[0];
            if (Col = ColByName(COL_PROVENANCE)) and (cmbProvenance.Visible) then
              cmbProvenance.Text := iTextStrings[0];
            Editing := True;
          end;
        end;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // DropSpecies
                  
//==============================================================================
// When taxa are dropped onto the Add Taxa edit box, add them to the grid
// and check them
procedure TfrmPlaceCard.DropTaxon(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
var
  i : integer;
begin
  for i := 0 to iSourceData.Header.ItemCount-1 do begin
    GotAddedTaxon(iSourceData.Items[i].KeyField1);
  end;
end;  // DropTaxon

//==============================================================================
procedure TfrmPlaceCard.UpdateSpeciesValues(const AColName: TColumnName; const ARow:integer);
var lCol: Integer;
    lstUpCase: String;
begin
  // Don't save values for [Count], [Count Of], [Accuracy], and any measurement columns.
  lstUpCase := UpperCase(AColName.OriginalName);
  if (Pos(COL_COUNT, lstUpCase) = 0) and (lstUpCase <> COL_ACCURACY) and AColName.IsStandard then
  begin
    lCol := ColByName(AColName.OriginalName);
    if lCol <> -1 then
      FSpeciesValues.Values[AColName.OriginalName] := sgSpecies.Cells[lCol, ARow];
  end;
end;  // UpdateSpeciesValues;

//==============================================================================
// Procedure Name: SortGrid
//        Purpose: Sorts rows in the species grid alphabetically, according to
//                 the column with the specified index. The sort direction,
//                 ascending or descending, is controlled by the flag
//                 FSortDescending.
//     Parameters: ACol: Integer - The index of the column to sort by.
//------------------------------------------------------------------------------
procedure TfrmPlaceCard.SortGrid(ACol: Integer);
var
  lRows: TList;
  lListIndex,lRowIndex: Integer;
  lNewRow: TStringList;
  lCursor: TCursor;

  //----------------------------------------------------------------------------
  procedure SelectionSort(SortList: PPointerList; const ALeft, ARight: Integer);
  var
    i, j, lMinIdx: integer;
    pBuffer: Pointer;
  begin
    for i := ALeft to Pred(ARight) do begin
      lMinIdx := i;
      // Find the smallest in remaining set of items
      for j := Succ(i) to ARight do
        if CompareText(TStrings(SortList^[j])[ACol], TStrings(SortList^[lMinIdx])[ACol]) < 0 then
          lMinIdx := j;
      // Swap if needed
      if lMinIdx <> i then begin
        pBuffer := SortList^[i];
        SortList^[i] := SortList^[lMinIdx];
        SortList^[lMinIdx] := pBuffer;
      end;
    end;
  end;  // SelectionSort
  //----------------------------------------------------------------------------
begin
  with sgSpecies do begin
    lRows   := TList.Create;
    lCursor := HourglassCursor;
    LockWindowUpdate(Handle);
    try
      FSortDescending := (FSortColumn = ACol) and (not FSortDescending);
      // Prepare the list for the sort
      for lRowIndex := FixedRows to RowCount - 1 do
      begin
        lNewRow := TStringList.Create;
        lNewRow.Assign(Rows[lRowIndex]);
        lRows.Add(lNewRow);
      end;
      // Sort the grid
      SelectionSort(lRows.List, 0, lRows.Count - 1);
      // Put the result back in the grid in correct order
      if FSortDescending then
        for lListIndex := 0 to lRows.Count - 1 do
          Rows[lRows.Count - lListIndex].Assign(TStrings(lRows.Items[lListIndex]))
      else
        for lListIndex := 0 to lRows.Count - 1 do
          Rows[lListIndex + 1].Assign(TStrings(lRows.Items[lListIndex]));
      FSortColumn := ACol;
    finally
      // free list objects
      for lListIndex := 0 to lRows.Count -1 do
        TObject(lRows.Items[lListIndex]).Free;
      // free list
      lRows.Free;
      DefaultCursor(lCursor);
      LockWindowUpdate(0);
    end;
  end;
end;  // SortGrid


//==============================================================================
// Procedure Name: ReSortGrid
//        Purpose: Restores the original order of rows in the species grid, if
//                 this has been changed by sorting alphabetically by a column,
//                 or has not yet been set because the grid was just populated.
//                 Rucksack items will assume their original order in the case
//                 that some items have been removed.
//                 IMPORTANT: Requires that Objects collection of column 1
//                 contains the zero-based indices of their parent rows, and
//                 that these indices are CONSECUTIVE.
//------------------------------------------------------------------------------
procedure TfrmPlaceCard.ReSortGrid;
var i, TargetIndex: Integer;
    TempRow: TStringList;
begin
  LockWindowUpdate(Handle);
  FSortDescending := False;
  FSortColumn := -1;
  TempRow := TStringList.Create;

  with sgSpecies do begin
    i := FixedRows;

    while i < RowCount do begin
      TargetIndex := Integer(Objects[1, i]) + 1;

      // Move the row to its target index, if not there already.
      if (TargetIndex <> i) then begin
        TempRow.Assign(Rows[TargetIndex]);
        Rows[TargetIndex].Assign(Rows[i]);
        Rows[i].Assign(TempRow);
        // If a change was made, go back to the start - otherwise we may miss
        // a row that's been set to an index we've already passed.
        // Efficiency of this approach could probably be improved, but avoiding
        // recursion here for memory reasons.
        i := FixedRows;
      end else begin
        i := i + 1;
      end;
    end;
  end;

  FreeAndNil(TempRow);
  LockWindowUpdate(0);
end;


//==============================================================================
procedure TfrmPlaceCard.CheckTaxon;
var
  i: Integer;
  clear: Boolean;
begin
  with sgSpecies do
    if not RowChecked[Row] then begin
      // Selecting
      CheckRow(Row);
      if RowFromSample[Row] then begin
        // Restore to originally loaded values.
        for i := 2 to ColCount - 1 do
          if not IsReadOnlyCell(i, Row) and (Objects[i, Row] is TCellData) then
            Cells[i, Row] := TCellData(Objects[i, Row]).ItemAdditional
      end else
        // Set the cells to the latest row's values.
        for i := 2 to ColCount - 1 do
          if HasColumnName(i) and not IsReadOnlyCell(i, Row) then
            Cells[i, Row] := FSpeciesValues.Values[TColumnName(Objects[i, 0]).OriginalName];
      Editing := True;
    end else
    if ((Col in [0, 1]) or IsCellCustodian(Col, Row)) and
       ((RowCustodian[Row] = '') or (RowCustodian[Row] = AppSettings.SiteID)) then
    begin
      // Clearing selection
      clear := True;
      // Find out if there is something to clear, and if so ask for confirmation.
      for i := 2 to ColCount - 1 do
        if not IsReadOnlyCell(i, Row) and (Cells[i, Row] <> '') then begin
          clear := False;
          Break;
        end;

      // Found something. Ask if really want to unselect and discard values.
      if not clear then
        clear := MessageDlg(SPlaceCard_UnselectSpecies, mtConfirmation,[mbYes, mbNo], 0) = mrYes;

      if clear then
      begin
        RowChecked[Row] := False;
        for i := 2 to ColCount - 1 do
          if not IsReadOnlyCell(i, Row) then
            Cells[i, Row] := '';
        TotalSpecies := TotalSpecies - 1;
      end;
      Editing := True;
    end else
      ShowInformation(ResStr_NotAllowedToEditOtherSitesData)
end;  // CheckTaxon

//==============================================================================
procedure TfrmPlaceCard.SetActiveCellState(const AChar:Char);
begin
  if not FEditingCell then
    with sgSpecies do begin
      // Clicked in Checkbox/Exclamation mark/Scient/Common names
      if FGridClicked and IsReadOnlyCell(Col, Row) then begin
        FGridClicked := False;
        CheckTaxon;
      end else begin
        CheckRow(Row);
        if (not IsReadOnlyCell(Col, Row)) and
           (Col <> ColByName(COL_ACCURACY)) and (Col <> ColByName(COL_SUBSTRATE)) and
           (Col <> ColByName(COL_PROVENANCE)) and (Col <> ColByName(COL_RECORD_TYPE)) then
        begin
          if AChar >= #32 then
            Cells[Col, Row] := AChar
          else
          if AChar <> #0  then
            Cells[Col, Row] := '';

          FEditingCell := True;
          Options      := Options + [goEditing];
        end
        else if Col = ColByName(COL_COUNT_OF) then    cmbQualifier.SetFocus
        else if Col = ColByName(COL_ACCURACY) then    cmbAccuracy.SetFocus
        else if Col = ColByName(COL_SUBSTRATE) then   cmbSubstrate.SetFocus
        else if Col = ColByName(COL_PROVENANCE) then  cmbProvenance.SetFocus
        else if Col = ColByName(COL_RECORD_TYPE) then cmbRecordType.SetFocus
        else if DataRestrictedMeasurement(Col) then   cmbRestrictedData.SetFocus;
      end;
      FEditedCol := Col;
      FEditedRow := Row;
    end;
end;  // SetActiveCellState

//==============================================================================
{ IsFixedCell - returns true if the column is a non-editable information column }
function TfrmPlaceCard.IsFixedCol(ACol: Integer): Boolean;
begin
  // Any fixed col, 0 is the checkbox column and 1 is the Critical column
  // xxx in [...] doesn't work with a -1 in the set!!! So do it differently
  Result := (ACol in [0, 1])
      or (ACol = ColByName(COL_CODE_NUMBER))
      or (ACol = ColByName(COL_SCIENTIFIC_NAME))
      or (ACol = ColByName(COL_COMMON_NAME))
      or (FLoadedFromSample and (ACol = ColByName(COL_SPATIAL_REF)));
end;  // IsFixedCol

//==============================================================================
function TfrmPlaceCard.IsReadOnlyCell(ACol, ARow: Integer): Boolean;
begin
  Result := IsFixedCol(ACol)
            or (RowFromSample[ARow]
              and (
                // Whole columns
                ((ACol = ColByName(COL_DETERMINER))
                  or (ACol = ColByName(COL_DATE_OF_DETERMINATION))
                  or (ACol = ColByName(COL_BIOTOPE))
                )
                // Cells for taxon occurrences
                or ((RowCustodian[ARow] <> AppSettings.SiteID)
                  and ((ACol = ColByName(COL_COMMENT))
                    or (ACol = ColByName(COL_RECORD_TYPE))
                    or (ACol = ColByName(COL_PROVENANCE))
                    or (ACol = ColByName(COL_SUBSTRATE))
                  )
                )
                // Cells for measurements
                or not IsCellCustodian(ACol, ARow)
              )
            );
end;  // IsReadOnlyCell

//==============================================================================
function TfrmPlaceCard.IsCellCustodian(ACol, ARow: Integer): Boolean;
begin
  with sgSpecies do
    Result := (ACol > 1)
              and (not Assigned(Objects[ACol, ARow])
                or ((Objects[ACol, ARow] is TCellData)
                  and ((TCellData(Objects[ACol, ARow]).Custodian = AppSettings.SiteID)
                    or (TCellData(Objects[ACol, ARow]).Custodian = '')
                  )
                )
              );
end;  // IsCellCustodian

//==============================================================================
procedure TfrmPlaceCard.sgSpeciesClick(Sender: TObject);
var lCol:integer;
begin
  inherited;
  if not (FClearingGrid or FLoading) then
    with sgSpecies do
      if Cells[2, 1] <> '' then begin  // If there is something in the grid
        lCol := ColByName(COL_COMMON_NAME);
        if lCol = -1 then lCol := 2;     // common name or latin name col
        // If you move outside the cell you are editing, record changes
        if (Row > 0) and ((Col <> FEditedCol) or (Row <> FEditedRow)) then
        begin
          if FEditingCell and HasColumnName(FEditedCol) then  // something changed so update
            UpdateSpeciesValues(TColumnName(Objects[FEditedCol, 0]), FEditedRow);

          if not IsReadOnlyCell(Col, Row) then begin // editable cell
            { Show the editor and select the text }
            FEditingCell := True;
            Options      := Options + [goEditing];
            FEditedCol   := Col;
            FEditedRow   := Row;
            // Check the InPlaceEditor is here before accessing it!!!!!
            if TAccessorClass(sgSpecies).InPlaceEditor <> nil then
              with TAccessorClass(sgSpecies).InPlaceEditor do begin
                SelStart  := 0;
                SelLength := Length(Text); // select the text
              end; // with inplaceeditor
          end else begin // non-editable cell
            Options      := Options - [goEditing];
            FEditedCol   := 0;
            FEditedRow   := 0;
            FEditingCell := False;
          end;
        end;
        if FGridClicked then begin
          FGridClicked := False;
          if Col in [0..lCol] then CheckTaxon;
        end;
        // Get the InPlaceEditor on the grid, and select all the text by default
        if Col > 2 then begin
          sgSpecies.EditorMode := True;
          if TAccessorClass(sgSpecies).InPlaceEditor <> nil then  // Check it's there first!!!
            with TAccessorClass(sgSpecies).InPlaceEditor do begin
              SelStart  := 0;
              SelLength := Length(Text);
            end;
        end;
      end;
end;  // sgSpeciesClick

//==============================================================================
procedure TfrmPlaceCard.sgSpeciesDblClick(Sender: TObject);
var liRow, liCol: LongInt;
    lMousePos   : TPoint;
begin
  inherited;
  // If nothing in grid, don't do anything.
  if sgSpecies.Cells[2, 1] = '' then Exit;

  lMousePos := sgSpecies.ScreenToClient(Mouse.CursorPos);
  sgSpecies.MouseToCell(lMousePos.X, lMousePos.Y, liCol, liRow);
  // Do nothing if column header was double-clicked
  if liRow > 0 then begin
    FGridClicked := True;
    SetActiveCellState(#0);
  end;
end;  // sgSpeciesDblClick

//==============================================================================
{ Handles the drawing of checkboxes and combo boxes in correct location on the
   data entry grid }
procedure TfrmPlaceCard.sgSpeciesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
var lcmbTemp: TCombobox;
    x, y    : Integer;
begin
  inherited;
  with sgSpecies do begin
    if Cells[2, 1] <> '' then
      if ((gdFocused in State) or (gdSelected in State))
         and (ARow > 0) and not IsReadOnlyCell(ACol, ARow) then
      begin
        Col := ACol;
        Row := ARow;
        lcmbTemp   := nil;
        if ACol = ColByName(COL_COUNT_OF)    then lcmbTemp := cmbQualifier  else
        if ACol = ColByName(COL_ACCURACY)    then lcmbTemp := cmbAccuracy   else
        if ACol = ColByName(COL_PROVENANCE)  then lcmbTemp := cmbProvenance else
        if ACol = ColByName(COL_SUBSTRATE)   then lcmbTemp := cmbSubstrate  else
        if ACol = ColByName(COL_RECORD_TYPE) then lcmbTemp := cmbRecordType else
        if DataRestrictedMeasurement(ACol)   then lcmbTemp := cmbRestrictedData;

        if lcmbTemp <> nil then begin
          EditorMode := False;
          if lcmbTemp.Style = csDropDown then
            lcmbTemp.Text := Cells[ACol, ARow]
          else
            lcmbTemp.ItemIndex := lcmbTemp.Items.IndexOf(Cells[ACol, ARow]);
          lcmbTemp.SetBounds(Rect.Left + 2, Rect.Top + 2,
                             Rect.Right - Rect.Left + 2, Rect.Bottom - Rect.Top + 2);
        end else
        if (ACol > 2) and (not sgSpecies.EditorMode) then 
          sgSpecies.EditorMode := True;
      end;

    // Show readonly columns in different shade. Also if Determination columns loaded from sample.
    if (ARow > 0) and IsReadOnlyCell(ACol, ARow) then
    begin
      Canvas.Brush.Color := MergeColours(clWindow, clBtnFace, 50);
      if RowFromSample[ARow] then begin
        Canvas.Brush.Color := MergeColours(Canvas.Brush.Color, clHighlight, 88);
        if ((ACol = 0) and (RowCustodian[ARow] <> AppSettings.SiteID))
        or ((ACol > 1) and not IsCellCustodian(ACol, ARow)) then
          Canvas.Brush.Color := MergeColours(clWindow, clHighlight, 50);
      end;
      Canvas.FillRect(Rect);
    end;

    // Check box column
    if ACol = 0 then begin
      if ARow > 0 then begin
        x := Rect.Left + (ColWidths[0] - 13) div 2;
        y := Rect.Top + (RowHeights[ARow] - 13) div 2;
        DrawCheckBox(Canvas, x, y, RowChecked[ARow]);
      end;
    end else
    // '!' column
    if ACol = 1 then begin
      // Existing '!' painted over already. Ready for the new red one
      if (Cells[1, ARow] = '!') then begin
        { The taxon name is invalid, this seems to flag it }
        Canvas.Font.Style := [fsBold];
        Canvas.Font.Color := clRed;
        Canvas.TextOut(Rect.Left + (ColWidths[1] - Canvas.TextWidth('!')) div 2, Rect.Top + 2, '!');
      end;
    end else
    // Species name column
    if (ACol = 2) and (ARow > 0) then begin
      { Italicise species names for taxa }
      Canvas.Font.Style := [];
      if (Pos(' ', Cells[2, ARow]) > 0) then begin // space denotes a species name
        Canvas.Font.Style := [fsItalic];
        Canvas.Font.Name  := 'Arial';
      end else
        Canvas.Font.Name := Font.Name;
      DrawChoppedText(Cells[2, ARow], Canvas, Rect, 2);
    end else
    if ACol > 2 then begin // skips 0 and 1, Taxon Name, Checkbox and "!"
      Canvas.FillRect(Rect);
      DrawChoppedText(Cells[ACol, ARow], Canvas, Rect, 2);
    end;

    if FEditingCell and HasColumnName(Col) then
      UpdateSpeciesValues(TColumnName(Objects[Col, 0]), Row);
  end;
end;  // sgSpeciesDrawCell

//==============================================================================
procedure TfrmPlaceCard.sgSpeciesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var lComCol,lCol,lRow:integer;
begin
  inherited;
  sgSpecies.MouseToCell(X, Y, lCol,lRow);
  lComCol := ColByName(COL_CODE_NUMBER);
  if lComCol = -1 then lComCol := ColByName(COL_COMMON_NAME);
  if lComCol = -1 then lComCol := 2;

  if (lRow = 0) and (lCol in [2..lComCol]) then
    SortGrid(lCol)
  else
    FGridClicked := lRow = sgSpecies.Row;  // Solve partially showed rows problem
end;  // sgSpeciesMouseDown

//==============================================================================
procedure TfrmPlaceCard.sgSpeciesTopLeftChanged(Sender: TObject);
var
  lDummy : boolean; // this is an unused var parameter
begin
  inherited;
  sgSpeciesSelectCell(nil, sgSpecies.Col, sgSpecies.Row, lDummy);
end;  // sgSpeciesTopLeftChanged

//==============================================================================
procedure TfrmPlaceCard.sgSpeciesSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  inherited;
  Editing      := True;
  FEditingCell := True;
end;  // sgSpeciesSetEditText

//==============================================================================
procedure TfrmPlaceCard.cmbAccuracyChange(Sender: TObject);
var
  lCol: Integer;
begin
  inherited;
  lCol := ColByName(COL_ACCURACY);
  if lCol <> -1 then
    with sgSpecies do begin
      Cells[lCol, Row] := cmbAccuracy.Text;
      Editing := True;
      CheckRow(Row);
    end;
end;  // cmbAccuracyChange

//==============================================================================
procedure TfrmPlaceCard.cmbQualifierChange(Sender: TObject);
var
  lCol: Integer;
begin
  inherited;
  lCol := ColByName(COL_COUNT_OF);
  if lCol <> -1 then
    with sgSpecies do begin
      Cells[lCol, Row] := cmbQualifier.Items[cmbQualifier.ItemIndex];
      Editing := True;
      CheckRow(Row);
    end;
end;  // cmbQualifierChange

//==============================================================================
procedure TfrmPlaceCard.cmbProvenanceChange(Sender: TObject);
var
  lCol: Integer;
begin
  inherited;
  lCol := ColByName(COL_PROVENANCE);
  if lCol <> -1 then
    with sgSpecies do begin
      if cmbProvenance.Text = ResStr_None then
        Cells[lCol, Row] := ''
      else
        Cells[lCol, Row] := cmbProvenance.Text;
      UpdateSpeciesValues(TColumnName(Objects[lCol, 0]), Row);
      Editing := True;
      CheckRow(Row);
    end;
end;  // cmbProvenanceChange

//==============================================================================
procedure TfrmPlaceCard.cmbSubstrateChange(Sender: TObject);
var lCol: Integer;
begin
  inherited;
  lCol := ColByName(COL_SUBSTRATE);
  if lCol <> -1 then
    with sgSpecies do begin
      if (cmbSubstrate.EmptyItem) and (cmbSubstrate.ItemIndex < 1) then
        Cells[lCol, Row] := ''
      else
        Cells[lCol, Row] := cmbSubstrate.Text;
      UpdateSpeciesValues(TColumnName(Objects[lCol, 0]), Row);
      Editing := True;
      CheckRow(Row);
    end;
end;  // cmbSubstrateChange

//==============================================================================
procedure TfrmPlaceCard.cmbRecordTypeChange(Sender: TObject);
var lCol: Integer;
begin
  inherited;
  lCol := ColByName(COL_RECORD_TYPE);
  if lCol <> -1 then
    with sgSpecies do begin
      if (cmbRecordType.EmptyItem) and (cmbRecordType.ItemIndex < 1) then
        Cells[lCol, Row] := ''
      else
        Cells[lCol, Row] := cmbRecordType.Text;
      UpdateSpeciesValues(TColumnName(Objects[lCol, 0]), Row);
      Editing := True;
      CheckRow(Row);
    end;
end;  // cmbRecordTypeChange

//==============================================================================
procedure TfrmPlaceCard.cmbRestrictedDataChange(Sender: TObject);
begin
  inherited;
  with sgSpecies do begin
    Cells[Col, Row] := cmbRestrictedData.Text;
    CheckRow(Row);
  end;
  Editing := True;
end;

//==============================================================================
procedure TfrmPlaceCard.bbSaveClick(Sender: TObject);
begin
  inherited;
  { Check for a COM Header }
  if FIsComHeader then
  begin
    { Read sample key from the SaveHeaderInfo method of the classes }
    FSampleKey := (FComHeader.ControlInterface as IRecordCardHeader).SaveHeaderInfo;
    if FSampleKey = '' then // problem occurred
    begin
      MessageDlg(
          SPlaceCard_SaveFailure + (FComHeader.ControlInterface as IRecordCardHeader).GetLastError,
          mtWarning, [mbOk], 0);
      Exit; // don't save data
    end;
  end else begin
    // Normal card
    ValidateCard;
    // All validated, save to database
    if FLoadedFromSample then begin
      UpdateSample;
      Editing := False;
      fraLocationInfo.eSpatialRef.Modified := False;
      Close;
    end else
      SaveCard;
  end;
end;  // bbSaveClick

//==============================================================================
procedure TfrmPlaceCard.bbResetClick(Sender: TObject);
begin
  inherited;
  if FLoadedFromSample then
    Close
  else begin
    ClearData;
    cmbSurvey.SetFocus;
    Editing := False;
  end;
end;  // bbResetClick

//==============================================================================
procedure TfrmPlaceCard.CreateSurveyEvent;
var i: Integer;
    lVagueDate: TVagueDate;
    ltfRepeat: Boolean;
begin
  //Add survey event
  with FdmPlaceCard do
  begin
    with qryInsertSurveyEvent do
    begin
      repeat
        ltfRepeat := False;
        FSurveyEventKey := dmGeneralData.GetNextKey(TN_SURVEY_EVENT, 'Survey_Event_Key', True);
        try
          Parameters.ParamByName('SURVEY_EVENT_KEY').Value := FSurveyEventKey;
          Parameters.ParamByName('SURVEY_KEY').Value       := cmbSurvey.KeyValue;
          FdmPlaceCard.InitLocalityParams(Parameters, fraLocationInfo);
          lVagueDate := StringToVagueDate(eDate.Text);
          Parameters.ParamByName('VAGUE_DATE_START').Value := Trunc(lVagueDate.StartDate);
          Parameters.ParamByName('VAGUE_DATE_END').Value   := Trunc(lVagueDate.EndDate);
          Parameters.ParamByName('VAGUE_DATE_TYPE').Value  := lVagueDate.DateTypeString;
          Parameters.ParamByName('Entered_By').Value       := AppSettings.UserID;
          ExecSQL
        except
          on E:Exception do begin
            if dmDatabase.CheckError(E, dbePrimaryKeyViolation) then
            begin
              dmGeneralData.RepairLastKeyTable(TN_SURVEY_EVENT, 'SURVEY_EVENT_KEY');
              if RightStr(AnsiString(FSurveyEventKey), 8) = LAST_KEY then
                raise EDatabaseWriteError.Create(Format(SPlaceCard_NoKeysAvailable,[TN_SURVEY_EVENT]));
              ltfRepeat := true;
            end
            else
              Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Survey Event]');
          end;
        end;
      until not ltfRepeat;

      //Add Survey Event Recorders
      FSERKeys.Clear;
      for i:= 0 to lbRecorders.Items.Count - 1 do
        repeat
          ltfRepeat := false;
          try
            FSERKeys.Add(dmGeneralData.GetNextKey(TN_SURVEY_EVENT_RECORDER, 'SE_Recorder_Key', True));
            dmDatabase.ExecuteSQL(Format(
                'INSERT INTO Survey_Event_Recorder ('
                + 'SE_Recorder_Key, Survey_Event_Key, Name_Key, Recorder_Role_Key, Entered_By) '
                + 'VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'')',
                [FSERKeys[FSERKeys.Count -1], FSurveyEventKey,
                TKeyData(lbRecorders.Items.Objects[i]).ItemKey, RECORDER_RECORDER_ROLE_KEY,
                AppSettings.UserID]));
          except
            on E:Exception do begin
              if dmDatabase.CheckError(E, dbePrimaryKeyViolation) then
              begin
                dmGeneralData.RepairLastKeyTable(TN_SURVEY_EVENT_RECORDER, 'SE_RECORDER_KEY');
                if RightStr(FSERKeys[FSERKeys.Count - 1] , 8) = LAST_KEY then
                  raise EDatabaseWriteError.Create(Format(SPlaceCard_NoKeysAvailable, [TN_SURVEY_EVENT_RECORDER]));
                FSERKeys.Delete(FSERKeys.Count - 1);
                ltfRepeat := true;
              end
              else
                Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Survey Event Recorder]');
            end;
          end;
        until not ltfRepeat
    end;
  end;  // with FdmPlaceCard
end;  // CreateSurveyEvent

//==============================================================================
procedure TfrmPlaceCard.CreateSampleRecord(const ASpatialRef: String; AMainSample: boolean = false);
var i: Integer;
    lSampleKey, lSampleSourceKey: TKeyString;
    lVagueDate : TVagueDate;
    lLatLong: TLatLong;
    lSystem : string;
begin
  // default sample record need only be created once
  if AMainSample and (FSampleKey <> '') then
    Exit;

  with FdmPlaceCard.qryInsertSample do
  begin
    lSampleKey := dmGeneralData.GetNextKey(TN_SAMPLE, 'Sample_Key', True);
    try
      Parameters.ParamByName('Sample_Key').Value       := lSampleKey;
      Parameters.ParamByName('Sample_Reference').Value := '';
      lVagueDate := StringToVagueDate(eDate.Text);
      Parameters.ParamByName('Vague_Date_Start').Value := Trunc(lVagueDate.StartDate);
      Parameters.ParamByName('Vague_Date_End').Value   := Trunc(lVagueDate.EndDate);
      Parameters.ParamByName('Vague_Date_Type').Value  := lVagueDate.DateTypeString;

      { If a spatial reference is entered }
      if ASpatialRef <> '' then begin
        if ASpatialRef = fraLocationInfo.eSpatialRef.EnteredRef then
          lSystem := fraLocationInfo.eSpatialRef.EnteredSystem
        else begin
          lSystem := DetermineSpatialRefSystem(ASpatialRef);
          if lSystem = ResStr_SystemUnknown then
            raise EPlacesError.Create(SPlaceCard_SpatialRefInGridInvalid);
        end;
        lLatLong := ConvertToLatLong(ASpatialRef, lSystem);
        Parameters.ParamByName('Spatial_Ref').Value           := ASpatialRef;
        Parameters.ParamByName('Spatial_Ref_System').Value    := lSystem;
        Parameters.ParamByName('Lat').Value                   := lLatLong.Lat;
        Parameters.ParamByName('Long').Value                  := lLatLong.Long;
        Parameters.ParamByName('Spatial_Ref_Qualifier').Value := fraLocationInfo.eSpatialRef.Qualifier;
      end else
        FdmPlaceCard.NullSpatialParameters(Parameters);
      Parameters.ParamByName('Outstanding_Card').Value := 1;
      Parameters.ParamByName('Sample_Type_Key').Value  := cmbSampleType.KeyValue;
      FdmPlaceCard.SetLocationKeyParameter(Parameters.ParamByName('Location_Key'), fraLocationInfo);
      Parameters.ParamByName('Survey_Event_Key').Value := FSurveyEventKey;
      Parameters.ParamByName('Location_Name').Value    := fraLocationInfo.eLocationName.Text;
      Parameters.ParamByName('Comment').Value          := GetRTFText(reComments);
      Parameters.ParamByName('Entered_By').Value       := AppSettings.UserID;
      ExecSQL;
    except
      on E:Exception do
        Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Sample]');
    end;

    try
      for i := 0 to FSERKeys.Count - 1 do
        dmDatabase.RunStoredProc(
            'usp_SampleRecorder_Insert',
            ['@SampleKey', lSampleKey,
             '@SERecorderKey', FSERKeys[i],
             '@EnteredBy', AppSettings.UserID]);
    except
      on E:Exception do
        Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Sample Recorder]');
    end;

    // Save reference as internal reference if there is any.
    if FReferenceKey <> '' then
      try
        lSampleSourceKey := dmGeneralData.GetNextKey(TN_SAMPLE_SOURCES, 'Source_Link_Key', True);
        Connection.Execute (Format(
            'INSERT INTO Sample_Sources(Source_Link_Key, Sample_Key, Source_Key, Original) '
            + 'VALUES (''%s'', ''%s'', ''%s'', 0)',
            [lSampleSourceKey, lSampleKey, FReferenceKey]));
      except
        on E:Exception do
          Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Sample Sources]');
      end;
  end;
  // Main sample - not a sample created for a spatial reference in the grid
  if AMainSample then
    FSampleKey := lSampleKey;
  // Keep list of all samples
  if not Assigned(FSampleKeys) then FSampleKeys := TStringList.Create;
  FSampleKeys.Add(lSampleKey);
end;  // CreateSampleRecord

//==============================================================================
procedure TfrmPlaceCard.CreateBiotopeOccurrence(const ASampleKey, ABLIKey, ADeterminerKey: TKeyString;
  const ADetDate: String; const AComment: String = '');
var
  lBiotopeOccKey, lBiotopeDetKey: TKeyString;
  lVagueDate: TVagueDate;
begin
  // Add the new occurrence and determination record
  lBiotopeOccKey := dmGeneralData.GetNextKey(TN_BIOTOPE_OCCURRENCE, 'Biotope_Occurrence_Key', True);
  // Save key for clean up, if anything goes wrong
  if FBiotopeOccKeys = nil then FBiotopeOccKeys := TStringList.Create;
  FBiotopeOccKeys.Add(lBiotopeOccKey);
  // Create record
  try
    dmDatabase.ExecuteSQL(Format(
        'INSERT INTO Biotope_Occurrence ('
        + 'Biotope_Occurrence_Key, Comment, Digitised, Checked, Verified, '
        + 'Sample_Key, Entered_By) '
        + 'VALUES (''%s'', %s, 0, 0, 0, ''%s'', ''%s'')',
        [lBiotopeOccKey, QuotedStr(AComment), ASampleKey, AppSettings.UserID]));
  except
    on E:Exception do
      Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Biotope Occurrence]');
  end;

  lBiotopeDetKey := dmGeneralData.GetNextKey(TN_BIOTOPE_DETERMINATION, 'BIOTOPE_DETERMINATION_KEY', True);
  try
    //Now do determination
    lVagueDate := StringToVagueDate(ADetDate);
    dmDatabase.ExecuteSQL(Format(
        'INSERT INTO Biotope_Determination ('
        + 'Biotope_Determination_Key, Biotope_Occurrence_Key, Biotope_List_Item_Key, '
        + 'Determination_Type_Key, Determiner_Role_Key, Determiner, Preferred, '
        + 'Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_By) '
        + 'VALUES ( ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', 1, %d, %d, ''%s'', ''%s'')',
        [lBiotopeDetKey, lBiotopeOccKey, ABLIKey, ORIGINAL_DETERMINATION_TYPE_KEY,
         ORIGINAL_DETERMINER_ROLE_KEY, ADeterminerKey, Trunc(lVagueDate.StartDate),
         Trunc(lVagueDate.EndDate), lVagueDate.DateTypeString, AppSettings.UserID]));
  except
    on E:Exception do
      Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Biotope Determination]');
  end;
end;  // CreateBiotopeOccurrence

{-------------------------------------------------------------------------------
  Description : Create species occurrences for each entry in the grid.  Also
      creates additional samples and biotope occurrences as required.
  Created : Unknown}
procedure TfrmPlaceCard.CreateSpeciesOccurrences;
var liRow: Integer;
    lSampleKey, lTaxonOccKey,
    lDeterminerKey, lTaxonDeterminationKey: TKeyString;
    lVagueDate: TVagueDate;
    lstDate, lBiotopeName: String;
    lSampleList: TStringList; // list of samples created - 'BLIKey/SpatialRef=SampleKey' format
begin
  lSampleList := TStringList.Create;
  try
    // Record  the default sample to link occurrences to
    if FSampleKey <> '' then
      lSampleList.Add(
          FBiotopeListItemKey + '/' + fraLocationInfo.eSpatialRef.EnteredRef + '=' + FSampleKey);

    //For each species in the grid
    for liRow := sgSpecies.FixedRows to sgSpecies.RowCount - 1 do
      // RowFromSample items are handled in UpdateSample and only relevant when card is loaded from sample.
      if RowChecked[liRow] and not RowFromSample[liRow] then
      begin
        //--------------------------------------------------------------------
        // TAXON DETERMINER FROM GRID, OR USE DEFAULT ONE
        //--------------------------------------------------------------------
        // Get the determiner's name and key, for Determinations (Biotope and Taxon)
        if CellOrNull(ColByName(COL_DETERMINER), liRow) <> '' then
          lDeterminerKey := TCellData(sgSpecies.Objects[ColByName(COL_DETERMINER), liRow]).ItemKey
        else
          lDeterminerKey := TKeyData(lbRecorders.Items.Objects[0]).ItemKey;
        // Get date, and if nothing found, use the main one
        lstDate := CellOrNull(ColByName(COL_DATE_OF_DETERMINATION), liRow);
        if lstDate = '' then lstDate := VagueDateToString(eDate.VagueDate);
        lSampleKey := GetAppropriateSample(liRow, lSampleList, lBiotopeName, lDeterminerKey, lstDate);

        //--------------------------------------------------------------------
        // TAXON OCCURRENCES
        //--------------------------------------------------------------------
        //Create a taxon occurrence record
        lTaxonOccKey := dmGeneralData.GetNextKey(TN_TAXON_OCCURRENCE, 'Taxon_Occurrence_Key', True);
        // Save key for cleanup, if anything goes wrong
        if FTaxonOccKeys = nil then FTaxonOccKeys := TStringList.Create;
        FTaxonOccKeys.Add(lTaxonOccKey);
        // Create records
        try
          CreateTaxonOccurrenceRecord(lTaxonOccKey, lSampleKey, liRow, lBiotopeName);
        except
          on E:Exception do
            Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Taxon Occurrence]');
        end;
        // Now insert the documents if AddDocsToOccurrence is set to true
        If (AppSettings.AddDocsToOccurrence) then begin;
        try
           dmDatabase.RunStoredProc('usp_TaxonOccurrenceSource_Insert',
           ['@TaxonOccurrenceKey', lTaxonOccKey]);
        except
          on E:Exception do
           Raise EDatabaseWriteError.Create(ResStr_WriteRecFail + ' ' + E.Message + ' [Taxon Occurrence Sources]');
      end;
  end;
        //Now insert determination
        lVagueDate := StringToVagueDate(lstDate);
        lTaxonDeterminationKey :=
            dmGeneralData.GetNextKey(TN_TAXON_DETERMINATION, 'Taxon_Determination_Key', True);
        try
          dmDatabase.ExecuteSQL(Format(
              'INSERT INTO Taxon_Determination('
              + 'Taxon_Determination_Key, Taxon_Occurrence_Key, Taxon_List_Item_Key, '
              + 'Determination_Type_Key, Determiner_Role_Key, Determiner, Preferred, '
              + 'Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_By) '
              + 'VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', 1, %d, %d, ''%s'', ''%s'' ) ',
              [lTaxonDeterminationKey, lTaxonOccKey, RowTLIKey[liRow],
              ORIGINAL_DETERMINATION_TYPE_KEY, ORIGINAL_DETERMINER_ROLE_KEY, lDeterminerKey,
              Trunc(lVagueDate.StartDate), Trunc(lVagueDate.EndDate), lVagueDate.DateTypeString,
              AppSettings.UserID]));
        except
          on E:Exception do
            Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Taxon Determination]');
        end;

        //Create measurement records
        UpdateMeasurements(liRow, lTaxonOccKey);
      end;  // if RowChecked[]
  finally
    lSampleList.Free;
  end;
end;  // CreateSpeciesOccurrences

{-------------------------------------------------------------------------------
  Description : Creates a taxon occurrence record for the occurrence identified
              by ARow
  Created : 28/04/2003 }
procedure TfrmPlaceCard.CreateTaxonOccurrenceRecord(const ATaxOccKey, ASampleKey: String;
  ARow: Integer; const ABiotope: String);
var
  lstComment,
  lstSubstrateKey,
  lstRecordTypeKey: String;
begin
  lstComment := CellOrNull(ColByName(COL_COMMENT), ARow);
  if ABiotope <> '' then begin
    if lstComment <> '' then lstComment := lstComment + #13#10;
    lstComment := lstComment + Format(ResStr_AssociatedBiotope, [ABiotope]);
  end;
  lstSubstrateKey  := KeyOrNull(cmbSubstrate, ColByName(COL_SUBSTRATE), ARow, NONE_RECORD_KEY);
  lstRecordTypeKey := KeyOrNull(cmbRecordType, ColByName(COL_RECORD_TYPE), ARow, NONE_RECORD_KEY);
  dmDatabase.ExecuteSQL(Format(
      'INSERT INTO Taxon_Occurrence ( '
      + 'Taxon_Occurrence_Key, Comment,  Checked, Verified, Sample_Key, Provenance, '
      + 'Zero_Abundance, Confidential, Substrate_Key, Record_Type_Key, Entered_By) '
      + 'VALUES ( ''%s'', %s, 0, 0, ''%s'', ''%s'', 0, 0, ''%s'', ''%s'', ''%s'')',
      [ATaxOccKey, QuotedStr(lstComment), ASampleKey, CellOrNull(ColByName(COL_PROVENANCE), ARow),
       lstSubstrateKey, lstRecordTypeKey, AppSettings.UserID]));
end;

{-------------------------------------------------------------------------------
  Description : Returns the sample key appropriate to the occurrence identified
        by the row in the grid.  Existing samples must be supplied in the
        ASampleList param in format 'BLI_KEY/Spatial_ref=Sample_Key'.
        Creates a sample if necessary and adds it to the list - returns the name
        in ABiotopeName if a biotope was created for the sample.
  Created : 28/04/2003 }
function TfrmPlaceCard.GetAppropriateSample(const ARow: Integer; const ASampleList: TStringList;
  var ABiotopeName: String; const ADeterminerKey, ADetDate: String): String;
var
  spatialRef, biotopeKey: String;
  col: Integer;
begin
  col := ColByName(COL_SPATIAL_REF);
  // find the spatial ref this occurrence should go against
  if (col > -1) and (sgSpecies.Cells[col, ARow] <> '') then
    spatialRef := DelocaliseSpatialRef(sgSpecies.Cells[col, ARow])
  else
    spatialRef := fraLocationInfo.eSpatialRef.EnteredRef;

  // find the biotope to put this occurrence against
  col := ColByName(COL_BIOTOPE);
  if (col > -1) and  (sgSpecies.Cells[col, ARow] <> '') then begin
    biotopeKey   := TCellData(sgSpecies.Objects[col, ARow]).ItemKey;
    ABiotopeName := sgSpecies.Cells[col, ARow];
  end else begin
    biotopeKey   := FBiotopeListItemKey;
    ABiotopeName := '';
  end;

  // now find the appropriate sample, or create one
  if ASampleList.IndexOfName(biotopeKey + '/' + spatialRef) > -1 then
    Result := ASampleList.Values[biotopeKey + '/' + spatialRef]
  else begin
    CreateSampleRecord(
        spatialRef,
        (spatialRef = fraLocationInfo.eSpatialRef.EnteredRef) and (ABiotopeName = ''));
    Result := FSampleKeys[FSampleKeys.Count - 1];
    ASampleList.Add(biotopeKey + '/' + spatialRef + '=' + Result);
    if biotopeKey <> '' then
      CreateBiotopeOccurrence(Result, biotopeKey, ADeterminerKey, ADetDate, '');
  end;
end;  // GetAppropriateSample

//==============================================================================
// Procedure Name: ColByName
//        Purpose: Gets the index of the species grid column that has the
//                 specified original name, or -1 if it does not exist.
//     Parameters: AColumnName: String - The original name of the required col.
//------------------------------------------------------------------------------
function TfrmPlaceCard.ColByName(const AColumnName: string): Integer;
var i: Integer;
begin
  //Set pessimistic result
  Result := -1;
  //Try to find column
  with sgSpecies do
    for i := 0 to ColCount - 1 do
      if HasColumnName(i) and SameText(TColumnName(Objects[i, 0]).OriginalName, AColumnName) then
      begin
        Result := i;
        Exit;
      end;
end;  // ColByName

//==============================================================================
function TfrmPlaceCard.CellOrNull(const ACol, ARow: Integer): string;
begin
  with sgSpecies do
    if (ACol >= FixedCols) and (ACol < ColCount) and
       (ARow >= FixedRows) and (ARow < RowCount) then
      Result:= Cells[ACol, ARow]
    else
      Result:= '';
end;  // CellOrNull

//==============================================================================
function TfrmPlaceCard.KeyOrNull(const AListCombo: TDBListCombo; ACol, ARow: Integer;
  const ADefault: String = ''): String;
begin
  with sgSpecies do
    if (ACol >= FixedCols) and (ACol < ColCount) and
       (ARow >= FixedRows) and (ARow < RowCount) then
    begin
      if Cells[ACol, ARow] = '' then
        Result := ADefault
      else
        with AListCombo do begin
          ItemIndex := Items.IndexOf(Cells[ACol, ARow]);
          Result    := KeyValue;
        end;
    end else
      Result := ADefault;
end;  // KeyOrNull

//==============================================================================
function TfrmPlaceCard.CellOrValue(ACol, ARow: Integer; const AValue: string): string;
begin
  with sgSpecies do
  begin
    if (ACol >= FixedCols) and (ACol < ColCount) and
       (ARow >= FixedRows) and (ARow < RowCount) then
      Result:= Cells[ACol, ARow]
    else
      Result:= AValue;
  end;
end;  // CellOrValue

//==============================================================================
procedure TfrmPlaceCard.ClearData;
var lRowIdx, lColIdx: Integer;
    lColName: TObject;
begin
  // see Mantis 271 - clear the comments as well as the grid
  reComments.Clear;
  FClearingGrid := True;
  try
    with sgSpecies do begin
      for lRowIdx := FixedRows to RowCount - 1 do
        RowChecked[lRowIdx] := False;

      for lColIdx := 0 to ColCount - 1 do
        if not IsFixedCol(lColIdx) then begin
          // Clear the cell if editable column one in one go, keeping only the column header
          lColName            := Objects[lColIdx, 0];
          Cols[lColIdx].Text  := Cells[lColIdx, 0];
          Objects[lColIdx, 0] := lColName;

          for lRowIdx := FixedRows to RowCount - 1 do begin
            // Clear objects that are not on first row
            Objects[lColIdx, lRowIdx].Free;
            Objects[lColIdx, lRowIdx] := nil;
          end;
        end;

      Row := 1;
      Col := 0;
      // Clear all values from that list too
      for lColIdx := 2 to ColCount - 1 do
        if HasColumnName(lColIdx)  and not IsFixedCol(lColIdx) then
          FSpeciesValues.Values[TColumnName(Objects[lColIdx, 0]).OriginalName] := '';
    end;
  finally
    FClearingGrid := False;
    TotalSpecies  := 0;
    fraLocationInfo.eSpatialRef.Modified := False;
    FProtectSpatialRef := False;
  end;
end;  // ClearData

//==============================================================================
procedure TfrmPlaceCard.cmbSampleTypeChange(Sender: TObject);
begin
  inherited;
  Editing := true;
end;  // cmbSampleTypeChange

//==============================================================================
procedure TfrmPlaceCard.reCommentsChange(Sender: TObject);
begin
  inherited;
  Editing := true;
end;  // reCommentsChange

//==============================================================================
procedure TfrmPlaceCard.CleanUp;
var lFields : TStringList;
begin
  lFields := TStringList.Create;
  //Tables must be cleaned up in this order;
  try
    try
      // Taxon_Occurrence, Taxon_Determination and Taxon_Occurrence_Data
      if Assigned(FTaxonOccKeys) then begin
        if FTaxonOccKeys.Count>0 then begin
          lFields.AddStrings(FTaxonOccKeys);
          lFields.Insert(0, 'Taxon_Occurrence_Key');
          // Do detail records in Taxon_Determination
          FdmPlaceCard.CleanupTable(TN_TAXON_DETERMINATION, lFields);
          // Do detail records in Taxon_Occurrence_Data
          FdmPlaceCard.CleanupTable(TN_TAXON_OCCURRENCE_DATA, lFields);
          // Now the master records in Taxon_Occurrence
          FdmPlaceCard.CleanupTable(TN_TAXON_OCCURRENCE, lFields);
          lFields.Clear;
        end;
      end;

      // Biotope_Occurrence, Biotope_Determination and Biotope_Occurrence_Data
      if Assigned(FBiotopeOccKeys) then
      begin
        lFields.AddStrings(FBiotopeOccKeys);
        lFields.Insert(0, 'Biotope_Occurrence_Key');
        // Do detail records in Biotope_Determination
        FdmPlaceCard.CleanupTable(TN_BIOTOPE_DETERMINATION, lFields);
        // Do detail records in Biotope_Occurrence_Data
        FdmPlaceCard.CleanupTable(TN_BIOTOPE_OCCURRENCE_DATA, lFields);
        // Now the master records in Biotope_Occurrence
        FdmPlaceCard.CleanupTable(TN_BIOTOPE_OCCURRENCE, lFields);
        lFields.Clear;
      end;

      // Sample, Sample_Sources and Sample_Recorder
      if (FSampleKey <> '') then
      begin
        // If we have any extra samples, apart from the "main" one
        if Assigned(FSampleKeys) then
          lFields.AddStrings(FSampleKeys);
        lFields.Insert(0, FSampleKey);
        lFields.Insert(0, 'Sample_Key');
        // Do detail records in Sample_Sources
        FdmPlaceCard.CleanupTable(TN_SAMPLE_SOURCES, lFields);
        // Do detail records in Sample_Recorder
        FdmPlaceCard.CleanupTable(TN_SAMPLE_RECORDER, lFields);
        // Now the master records in Sample
        FdmPlaceCard.CleanupTable(TN_SAMPLE, lFields);
        lFields.Clear;
      end;

      if FNewSurveyEvent then
      begin
        // Survey_Event and Survey_Event_Recorders
        if FSurveyEventKey <> '' then
        begin
          lFields.Add('Survey_Event_Key');
          lFields.Add(FSurveyEventKey);
          // Do detail records in Survey_Event_Recorder
          FdmPlaceCard.CleanupTable(TN_SURVEY_EVENT_RECORDER, lFields);
          // Now the master records in Survey_Event
          FdmPlaceCard.CleanupTable(TN_SURVEY_EVENT, lFields);
          lFields.Clear;
        end;
      end;
    except on E:ETableCleanupError do
      MessageDlg(SPlaceCard_CleanUpFailed +
                 ResStr_IncompleteSurveyEvent, mtWarning, [mbOK], 0);
    end;
  finally
    lFields.Free;
  end;
end;  // CleanUp

//==============================================================================
procedure TfrmPlaceCard.InitialiseKeyVariables;
begin
  FSurveyEventKey := '';
  if not Assigned(FSERKeys) then FSERKeys := TStringList.Create
                            else FSERKeys.Clear;
  FSampleKey := '';
  // Clear stringlists used when saving data
  if Assigned(FSampleKeys)     then FSampleKeys.Clear;
  if Assigned(FBiotopeOccKeys) then FBiotopeOccKeys.Clear;
  if Assigned(FTaxonOccKeys)   then FTaxonOccKeys.Clear;
end;  // InitialiseKeyVariables

//==============================================================================
procedure TfrmPlaceCard.lbRecordersKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key = #13 then
    bbRecorderAddClick(Self);
end;  // lbRecordersKeyPress

//==============================================================================
procedure TfrmPlaceCard.SetSampleType(const Value: TKeyString);
begin
  FSampleType := Value;
  if FSampleType <> '' then cmbSampleType.KeyValue := FSampleType;
end;  // SetSampleType

//==============================================================================
procedure TfrmPlaceCard.SetSpatialRef(const Value: string);
begin
  if Value <> '' then
    fraLocationInfo.eSpatialRef.Values := dmGeneralData.SetSpatialRef(Value, '', AppSettings.SpatialRefSystem);
end;  // TfrmPlaceCard.SetSpatialRef

//==============================================================================
procedure TfrmPlaceCard.UpdateObservations(updateCaptions: Boolean = False);
var
  obsForm: TfrmObservations;
  i: Integer;
begin
  inherited;
  obsForm := TfrmObservations(frmMain.GetForm(TfrmObservations));
  if obsForm <> nil then begin
    obsForm.RefreshSurvey(cmbSurvey.KeyValue, FSurveyEventKey, FSampleKey);
    if Assigned(FSampleKeys) then
      for i := 0 to FSampleKeys.Count - 1 do
        obsForm.RefreshSurvey(cmbSurvey.KeyValue, FSurveyEventKey, FSampleKeys[i]);
    obsForm.SelectNode(TN_SAMPLE, FSampleKey);

    // This is for when there is a cascade change, after loaded from sample and saved.
    if updateCaptions then
      with TdmSample.Create(nil) do
        try
          UpdateEventNodeCaption(obsForm.SelectedItem.Parent);
          UpdateSampleNodesCaption(obsForm.SelectedItem.Parent);
        finally
          Free;
        end;
  end;
end;  // UpdateObservations

//==============================================================================
procedure TfrmPlaceCard.reCommentsEnter(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(true);
end;  // reCommentsEnter

//==============================================================================
procedure TfrmPlaceCard.reCommentsExit(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(false);
end;  // reCommentsExit

//==============================================================================
procedure TfrmPlaceCard.SetLocation(const Value: string);
begin
  FLocation := Value;
  if FLocation <> '' then
    fraLocationInfo.eLocation.Text := FLocation;
end;  // SetLocation

//==============================================================================
procedure TfrmPlaceCard.WMRefreshTermLists(var Msg: TMessage);
var
  i: Integer;
begin
  cmbSampleType.Active := false;
  cmbSampleType.Active := true;
  cmbSubstrate.Active  := false;
  cmbSubstrate.Active  := true;
  cmbRecordType.Active := false;
  cmbRecordType.Active := true;
  for i := 0 to sgSpecies.ColCount - 1 do
    if HasColumnName(i) then
      if Assigned(TColumnName(sgSpecies.Objects[i, 0]).MeasurementKeys) then
        TColumnName(sgSpecies.Objects[i, 0]).MeasurementKeys.RefreshRestrictedValues;
end;  // WMRefreshTermLists

//==============================================================================
procedure TfrmPlaceCard.SetDisplaySystem;
begin
  fraLocationInfo.eSpatialRef.DisplaySystem := AppSettings.SpatialRefSystem;
end;  // SetDisplaySystem

//==============================================================================
procedure TfrmPlaceCard.WMRefreshSpatialRefSystem(var Msg: TMessage);
begin
  SetDisplaySystem;
end;  // WMRefreshSpatialRefSystem

//==============================================================================
procedure TfrmPlaceCard.WMUpdateSurveyCombo(var Msg: TMessage);
begin
  cmbSurvey.Active := False;
  cmbSurvey.Active := True;
end;  // WMUpdateSurveyCombo

//==============================================================================
procedure TfrmPlaceCard.WMRefreshNames(var Msg: TMessage);
var i, idxCol: Integer;
    detKey: TKeyString;
    detName: String;
begin
  // Update the recorders
  with lbRecorders do
    for i := 0 to Items.Count - 1 do
      Items[i] := dmGeneralData.GetIndividualName(TKeyData(Items.Objects[i]).ItemKey);
  // Update the determiners in the grid as well
  idxCol := ColByName(COL_DETERMINER);
  if (TotalSpecies > 0) and (idxCol <> -1) then
    with sgSpecies do
      for i := FixedRows to RowCount - 1 do
        if RowChecked[i] and (Cells[idxCol, i] <> '') then begin
          if dmGeneralData.CheckIndividual(Cells[idxCol, i], detKey, detName) then
            Cells[idxCol, i] := detName
          else
            Cells[idxCol, i] := '';
        end;
end;  // WMRefreshNames

//==============================================================================
procedure TfrmPlaceCard.WMRefreshLocationDetails(var Msg: TMessage);
begin
  fraLocationInfo.Refresh;
end;  // WMRefreshLocationDetails

//==============================================================================
procedure TfrmPlaceCard.WMRefreshBiotopeDic(var Msg: TMessage);
begin
  eBiotope.Text := dmGeneralData.GetBiotopeCodeName(FBiotopeListItemKey);
end;  // WMRefreshBiotopeDic

//==============================================================================
procedure TfrmPlaceCard.WMRefreshColours(var Msg: TMessage);
begin
  if FLoadedFromSample then
    SetReadOnlyFieldsColourState(True,
      [cmbSurvey, cmbSampleType, lbRecorders, eBiotope, reComments, eReference])
  else
    SetRequiredFieldsColourState(True, [cmbSurvey, eDate, cmbSampleType, lbRecorders]);
  fraLocationInfo.SetColour(MergeColours(AppSettings.MandatoryColour, clWindow, 25));
  // Trick Windows into repainting the listbox PROPERLY!!!!!
  lbRecorders.Visible := False;
  lbRecorders.Visible := True;
  Refresh;
end;  // WMRefreshColours

//==============================================================================
procedure TfrmPlaceCard.WMRefreshTaxonDic(var Msg: TMessage);
var i, idxCol: Integer;
    taxonName: String;
begin
  idxCol := ColByName(COL_COMMON_NAME);
  with sgSpecies do begin
    for i := RowCount - 1 downto FixedRows do begin
      taxonName := dmGeneralData.GetTaxonName(RowTLIKey[i]);
      if taxonName = '' then begin
        Row := i;
        if RowChecked[Row] then TotalSpecies := TotalSpecies - 1;
        Objects[0, Row].Free;
        DelLineInGrid(sgSpecies);
      end else begin
        Cells[2, i] := taxonName;
        if idxCol > -1 then begin
          taxonName := dmGeneralData.GetCommonName(RowTLIKey[i]);
          Cells[idxCol, i] := taxonName;
        end;
      end;
    end;
    Refresh;
  end;
end;  // WMRefreshTaxonDic

//==============================================================================
procedure TfrmPlaceCard.WMRefreshDocuments(var Msg: TMessage);
begin
  eReference.Text:=dmGeneralData.GetReferenceText(FReferenceKey);
end;  // WMRefreshDocuments

//==============================================================================
procedure TfrmPlaceCard.cmbKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
  if Key in [#13, #10] then begin// Return or shift return
    MoveGridCell(Key=#13);
    Key := #0;
  end;
end;  // cmbKeyPress

//==============================================================================
{ MoveGridCell - navigates the current cell to the right or left in the grid.
     If you hit the end, goes to the next/previous row where one is available }
procedure TfrmPlaceCard.MoveGridCell(const iToTheRight: Boolean);
begin
  if iToTheRight then begin  // Return
    sgSpecies.SetFocus;
    with sgSpecies do
      if Col < ColCount - 1 then
        Col := Col + 1
      else
      if Row < RowCount - 1 then begin
        Row := Row + 1;
        Col := 2;
      end;
  end else begin  // Shift-Return
    sgSpecies.SetFocus;
    with sgSpecies do
      if Col > 0 then
        Col := Col - 1
      else
      if Row > FixedRows then begin // don't move into title row
        Row := Row - 1;
        Col := ColCount-1;
      end;
  end;
end;  // MoveGridCell

//==============================================================================
function TfrmPlaceCard.FindTaxonInList(AName: String): String;
var i, lColIdx, lSpacePos                    : Integer;
    lDoSci, lDoCommon, lFound, lPreviousMatch: Boolean;
begin
  Result := '';
  if AName <> '' then begin
    while (AName <> '') and (AName[1] = '*') do
      AName := Copy(AName, 2, 255);

    // Maybe looking using abbreviation...
    i := FTaxaAbbreviations.IndexOfName(AName);
    if (i <> -1) and (Integer(FTaxaAbbreviations.Objects[i]) = 0) then
      AName := FTaxaAbbreviations.Values[AName];  // Get SciName from the list

    // CCN208 - ZENTRUM - Recording Card Add Taxon Search logic
    lPreviousMatch := False;
    lSpacePos := Pos(' ', AName);
    if (AppSettings.PartialTaxonSearch) and (lSpacePos > 0) then begin
      Result := FindTaxonInListUsingPartialTaxonSearch(AName, lSpacePos);
    end else begin
      // Check if the taxon can possibly be already in the grid
      // And if it is, do a more thorough search to find it.
      lDoSci    := Pos(LowerCase(AName), LowerCase(sgSpecies.Cols[2].CommaText)) > 0;
      lDoCommon := false;
      lColIdx   := ColByName(COL_COMMON_NAME);
      if not lDoSci and (lColIdx <> -1) then
        lDoCommon := Pos(AName, sgSpecies.Cols[lColIdx].CommaText) > 0;

      // Do we need to search the grid?
      if lDoSci or lDoCommon then begin
        // If no common name column, re-use the variable for Sci-Name instead
        if not lDoCommon then lColIdx := 2;
        // Check each item in turn
        for i := sgSpecies.FixedRows to sgSpecies.RowCount - 1 do begin
          lFound := AnsiStartsText(AName, sgSpecies.Cells[lColIdx, i]);
          if lFound then begin
            if (lPreviousMatch) then begin
              Result := '';
              Break;
            end else begin
              Result := RowTLIKey[i];
              lPreviousMatch := True;
            end;
          end;
        end;
      end;
    end;    // if (AppSettings.PartialTaxonSearch)
  end;
end;  // FindTaxonInList

//==============================================================================
function TfrmPlaceCard.FindTaxonInListUsingPartialTaxonSearch(const AName: String;
  ASpacePos: Integer): String;
var
  item: TSourceItem;
  i, scienceIdx, commonIdx: Integer;
begin
  // Search scientific name, and if present, common name too.
  scienceIdx := ColByName(COL_SCIENTIFIC_NAME);
  commonIdx  := ColByName(COL_COMMON_NAME);
  Result     := '';
  with sgSpecies do
    for i := FixedRows to RowCount - 1 do
    begin
      if commonIdx = -1 then
        item := TSourceItem.Create('', Cells[scienceIdx, i], [])
      else
        item := TSourceItem.Create('', Cells[scienceIdx, i], [Cells[commonIdx, i]]);

      try
        if item.IsTextInItems(AName, smAlways, True) then
          if Result <> '' then
          begin
            Result := '';
            Exit;
          end else
            Result := RowTLIKey[i];
      finally
        FreeAndNil(item);
      end;
    end;
end;  // FindTaxonInListUsingPartialTaxonSearch

//==============================================================================
procedure TfrmPlaceCard.eTaxonKeyPress(Sender: TObject; var Key: Char);
var
  lFoundTaxonKey: TKeyString;
begin
  inherited;
  if Key = #13 then begin
    if (Length(AppSettings.RapidEntryDelimiter) > 0) then begin
      ParseAndInputNewTaxonData;
    end else begin
      lFoundTaxonKey := GetTaxonKeyFromSearchCode(eTaxon.Text);
      if (lFoundTaxonKey = '') then
        lFoundTaxonKey := FindTaxonInList(eTaxon.Text);
      if lFoundTaxonKey = '' then
        lFoundTaxonKey := FdmPlaceCard.FindTaxon(eTaxon.Text);
      if lFoundTaxonKey <> '' then
        GotAddedTaxon(lFoundTaxonKey);
      Key := #0;
    end;    // if (Length(AppSettings.RapidEntryDelimiter) > 0)
  end;
end; // eTaxonKeyPress

//==============================================================================
{ Set the number of delimiters entered by the user when using fast add mode. }
procedure TfrmPlaceCard.eTaxonChange(Sender: TObject);
begin
  if (Length(AppSettings.RapidEntryDelimiter) = 1) then
    TaxonEntryDelimiterCount := CountDelimiters;
end;

//==============================================================================
{ Parse the new data entered using the Rapid Taxon Entry delimiter and add to grid. }
procedure TfrmPlaceCard.ParseAndInputNewTaxonData;
var
  Items: TStringList;
  SearchCodeKey: TKeyString;
  TaxonInGrid: Boolean;
begin
  Items := TStringList.Create;
  try
    ParseNewTaxonData(Items);     // Populate string list with data
    ClearSpeciesValues;          // Remove any previous entries
    if (Items.Count > 0) then begin
      SearchCodeKey := GetTaxonKeyFromSearchCode(Trim(Items[0]));
      if (SearchCodeKey = '') then begin
        TaxonInGrid := AddNewTaxon(Trim(Items[0]), True);
      end else begin
        GotAddedTaxon(SearchCodeKey, True);
        TaxonInGrid := True;
      end;    // if (SearchCodeKey = '')

      if (TaxonInGrid) then
        InputNewTaxonData(Items);   // Add new data to grid
    end;    // if (Items.Count > 0)

    sgSpecies.Col := 0;
    eTaxon.SetFocus;              // Prepare to accept next data entry
  finally
    Items.Free;
  end;
end;

//==============================================================================
{ Parse the new data entered using the Rapid Taxon Entry delimiter and add to string list. }
procedure TfrmPlaceCard.ParseNewTaxonData(const Items: TStringList);
var
  DelimiterPos, NextDelimiterPos: integer;
  Data: string;
begin
  Items.Clear;
  DelimiterPos := Pos(AppSettings.RapidEntryDelimiter, eTaxon.Text);
  if (DelimiterPos = 0) then begin
    Items.Add(eTaxon.Text);
  end else begin
    Data := Copy(eTaxon.Text, 1, DelimiterPos - 1);
    Items.Add(Data);
    while DelimiterPos > 0 do begin
      NextDelimiterPos := PosEx(AppSettings.RapidEntryDelimiter, eTaxon.Text, Succ(DelimiterPos));
      if (NextDelimiterPos = 0) then
        Data := Copy(eTaxon.Text, Succ(DelimiterPos), Length(eTaxon.Text))
      else
        Data := Copy(eTaxon.Text, Succ(DelimiterPos), NextDelimiterPos - DelimiterPos - 1);
      Items.Add(Data);
      DelimiterPos := NextDelimiterPos;
    end;    // while DelimiterPos > 0
  end;    // if (DelimiterPos = 0)
end;    // TfrmPlaceCard.ParseNewTaxonData;

//==============================================================================
{ Check whether the supplied Search Code matches one for the current rucksack.
  If so, return the corresponding taxon key, otherwise return an empty string.
}
function TfrmPlaceCard.GetTaxonKeyFromSearchCode(const ASearchCode: string): TKeyString;
var
  KeyList: TEditableKeyList;
  i: integer;
begin
  Result := '';
  if (Assigned(AppSettings.CurrentRucksack)) then begin
    KeyList := AppSettings.CurrentRucksack.TaxonSearchCodeList;
    for i := 0 to Pred(KeyList.Header.ItemCount) do
      if AnsiSameText(ASearchCode, KeyList.Items[i].KeyField2) then begin
        Result := KeyList.Items[i].KeyField1;
        Break;
      end;    // if AnsiSameText(ASearchCode, KeyList.Items[i].KeyField2)
  end;    // if (Assigned(AppSettings.CurrentRucksack))
end;    // TfrmPlaceCard.GetTaxonKeyFromSearchCode

//==============================================================================
{ Insert new taxon into the grid. }
function TfrmPlaceCard.AddNewTaxon(const Data: string; const AClearValues: Boolean = False): Boolean;
var
  lFoundTaxonKey: TKeyString;
begin
  lFoundTaxonKey := '';
  if (Length(Data) > 0) then begin
    lFoundTaxonKey := FindTaxonInList(Data);
    if lFoundTaxonKey = '' then
      lFoundTaxonKey := FdmPlaceCard.FindTaxon(Data);
    if lFoundTaxonKey <> '' then
      GotAddedTaxon(lFoundTaxonKey, AClearValues);
  end;    // if (Length(Data) > 0)
  Result := lFoundTaxonKey <> '';
end;

{-------------------------------------------------------------------------------
  Add a recorder to the list box
}
procedure TfrmPlaceCard.AddRecorder(const AText, AKey: string);
var
  lKeyData: TKeyData;
begin
  if lbRecorders.Items.IndexOf(AText) = -1 then
  begin
    lKeyData := TKeyData.Create;
    lKeyData.ItemKey:= AKey;
    lbRecorders.Items.AddObject(AText,lKeyData);
    Editing := True;
    bbRecorderRemove.Enabled := True;
  end;
end;

//==============================================================================
{ Add the new taxon data to the grid. }
procedure TfrmPlaceCard.InputNewTaxonData(const Items: TStringList);
var
  i, liColIndex: integer;
begin
  liColIndex := 3;
  for i := 1 to Pred(Items.Count) do begin
    // Ignore any fixed columns that cannot accept user input
    while IsFixedCol(liColIndex) do
      Inc(liColIndex);

    if (liColIndex < sgSpecies.ColCount) then begin
      sgSpecies.Col := liColIndex;
      sgSpecies.Cells[sgSpecies.Col, sgSpecies.Row] := Items[i];
      if ReturnKeyDown and MatchListContents and HasColumnName(sgSpecies.Col) then
        UpdateSpeciesValues(TColumnName(sgSpecies.Objects[sgSpecies.Col, 0]), sgSpecies.Row)
      else
        // Remove the entered data since it failed to match anything
        sgSpecies.Cells[sgSpecies.Col, sgSpecies.Row] := '';
    end;    // if (liColIndex < sgSpecies.ColCount)
    Inc(liColIndex);
  end;    // for i := 1 to Pred(Items.Count)
  if (liColIndex > sgSpecies.ColCount) then
    MessageDlg(ResStr_TooManyDelimiters, mtInformation, [mbOK], 0);
end;

//==============================================================================
{ Finds the best match in the relevant dropdown list and selects it.
  If no match is found, displays a warning message and returns False.
}
function TfrmPlaceCard.MatchListContents: Boolean;
var
  MatchText, ColumnTitle: string;
  DropDownList: TComboBox;
  i: integer;
  PartialMatch: Boolean;
begin
  Result := True;
  MatchText := sgSpecies.Cells[sgSpecies.Col, sgSpecies.Row];
  if (Length(MatchText) > 0) then begin
    DropDownList := GridEditingComboBox(sgSpecies.Col);
    if (Assigned(DropDownList)) then begin
      PartialMatch := False;
      Result := DropDownList.Items.IndexOf(MatchText) >= 0;

      if (Result) then begin
        // Set grid text to precise value from the drop down list (correct case)
        sgSpecies.Cells[sgSpecies.Col, sgSpecies.Row] := DropDownList.Items[DropDownList.Items.IndexOf(MatchText)];
      end else if (DropDownList.Style = csDropDown) then begin
        // Accept any text for dropdowns using a true combo-box style
        Result := True;
      end else begin
        for i := 0 to Pred(DropDownList.Items.Count) do
          if (AnsiStartsText(MatchText, DropDownList.Items[i])) then
            if (PartialMatch) then begin
              Result := False;
              Break;
            end else begin
              PartialMatch := True;
              Result := True;
              sgSpecies.Cells[sgSpecies.Col, sgSpecies.Row] := DropDownList.Items[i];
            end;    // if (PartialMatch)
      end;    // if (Result)

      if (not Result) then begin
        ColumnTitle := sgSpecies.Cells[sgSpecies.Col, 0];
        if (PartialMatch) then
          MessageDlg(Format(ResStr_MultipleListMatchesFound, [MatchText, ColumnTitle]), mtInformation, [mbOK], 0)
        else
          MessageDlg(Format(ResStr_NoListMatchFound, [MatchText, ColumnTitle]), mtInformation, [mbOK], 0);
      end;    // if (not Result)
    end;    // if (Assigned(DropDownList))
  end;    // if (Length(MatchText) > 0)
end;

//==============================================================================
{ Returns the combo box used for editing values in the specified column. }
function TfrmPlaceCard.GridEditingComboBox(const ACol: integer): TComboBox;
begin
  if (ACol = ColByName(COL_SUBSTRATE)) then
    Result := cmbSubstrate
  else if (ACol = ColByName(COL_RECORD_TYPE)) then
    Result := cmbRecordtype
  else if (ACol = ColByName(COL_COUNT_OF)) then
    Result := cmbQualifier
  else if (ACol = ColByName(COL_PROVENANCE)) then
    Result := cmbProvenance
  else if (ACol = ColByName(COL_ACCURACY)) then
    Result := cmbAccuracy
  else if DataRestrictedMeasurement(ACol) then
    Result := cmbRestrictedData
  else
    Result := nil;
end;

//==============================================================================
{ Adds a taxon to the grid, checks it, and sorts the grid correctly.  Finally,
    clears eTaxon.Text }
procedure TfrmPlaceCard.GotAddedTaxon(const iItemKey: TKeyString; AClearValues: Boolean = False);
var loTaxonRef: TObject;
    liIdx     : Integer;
    lRowCount     : Integer;
begin
  dmGeneralData.CheckTaxonAllowsDataEntry(iItemKey);
  lRowCount := sgSpecies.RowCount;
  AddNewTaxonToGrid(iItemKey, AClearValues, False);
  // Taxon located in grid already, so get object on current row
  loTaxonRef := sgSpecies.Objects[0, sgSpecies.Row];
  // Same trick as when Copy/Paste-Drag/Drop in grid. Do sort twice
  // to get back to current order direction
  if FSortColumn <> -1 then begin
    FSortDescending := not FSortDescending;
    SortGrid(FSortColumn);
  end;
  // Reselect the added taxon
  with sgSpecies do
    for liIdx := FixedRows to RowCount - 1 do
      if Objects[0, liIdx] = loTaxonRef then begin
        Row := liIdx;
        Break;
      end;
  eTaxon.Text := ''; // clear for next taxa
  eTaxon.SetFocus;   // and refocus
  // The taxon wasn't in the grid so remember it in case it needs to be saved
  if lRowCount <> sgSpecies.RowCount then begin
    liIdx := FRemovedTaxa.IndexOf(iItemKey);
    if (liIdx >= 0) then
      // If the added taxon already appears in the list of removed taxa, remove it
      FRemovedTaxa.Delete(liIdx)
    else
      // Otherwise add it to the list of added taxa
      FAdditionalTaxa.Add(iItemKey);
  end;    // if lRowCount <> sgSpecies.RowCount
end; //GotAddedTaxon

//==============================================================================
procedure TfrmPlaceCard.mnuOriginalOrderClick(Sender: TObject);
begin
  inherited;
  ReSortGrid;
end; //mnuOriginalOrderCLick

//==============================================================================
procedure TfrmPlaceCard.mnuRemoveRowClick(Sender: TObject);
begin
  inherited;
  RemoveTaxonFromGrid;
end;  //mnuRemoveRowClick

//==============================================================================
procedure TfrmPlaceCard.bbFindTaxonClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actTaxonDiction.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateTaxon);
end;  //bbFindTaxonClick

//==============================================================================
procedure TfrmPlaceCard.UpdateTaxon(KeyList:TKeyList);
var
  lTaxonKey: TKeyString;
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then begin
      lTaxonKey:=KeyList.Items[0].KeyField1;
      //check to see if taxon is in group
      If (Not dmGeneralData.IsGroupCorrect(ltaxonkey,FDefaultTaxonGroupKey))
        and (FDefaultTaxonGroupKey <> 'All') then
        if MessageDlg(ResStr_TaxonGroupNotEqualDefaultConfirm, mtInformation, [mbYes, mbNo], 0) = mrYes then
          GotAddedTaxon(lTaxonKey);
    end;
  finally
    KeyList.Free;
  end;
end;  //UpdateTaxon

//==============================================================================
procedure TfrmPlaceCard.PreviewScreen;
begin
  { Implemented abstract method even though it does nothing }
end;

//==============================================================================
procedure TfrmPlaceCard.PrintScreen;
begin
  { Implemented abstract method even though it does nothing }
end;

//==============================================================================
{ Focus the relevant combo box as we move onto a cell }
procedure TfrmPlaceCard.sgSpeciesSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  comboBox: TComboBox;
  item: TMeasurementKeys;
begin
  inherited;
  // Don't do unnecessary stuff during load
  if not FLoading then begin
    // Hide all combos by default.
    cmbAccuracy.Visible       := False;
    cmbQualifier.Visible      := False;
    cmbProvenance.Visible     := False;
    cmbSubstrate.Visible      := False;
    cmbRecordType.Visible     := False;
    cmbRestrictedData.Visible := False;

    if not IsReadOnlyCell(ACol, ARow) then begin
      with sgSpecies do begin
        comboBox := nil;  // non-combo column
        if ACol = ColByName(COL_COUNT_OF) then
          comboBox := cmbQualifier
        else
        if ACol = ColByName(COL_ACCURACY) then
          comboBox := cmbAccuracy
        else
        if ACol = ColByName(COL_SUBSTRATE) then
          comboBox := cmbSubstrate
        else
        if ACol = ColByName(COL_PROVENANCE) then
          comboBox := cmbProvenance
        else
        if ACol = ColByName(COL_RECORD_TYPE) then
          comboBox := cmbRecordType
        else begin
          item := nil;
          if HasColumnName(ACol) and not TColumnName(Objects[ACol, 0]).IsStandard then
            item := TColumnName(Objects[ACol, 0]).MeasurementKeys;
          if Assigned(item) and (item.RestrictedValues.Count > 0) then
          begin
            comboBox := cmbRestrictedData;
            comboBox.Items.CommaText := item.RestrictedValues.CommaText;
            comboBox.ItemIndex       := comboBox.Items.IndexOf(Cells[ACol, ARow]);
          end;
        end;
      end;
      
      if Assigned(comboBox) and (sgSpecies.Cells[2, 1] <> '')  then begin
        // and show the one we need
        comboBox.Visible   := True;
        Self.ActiveControl := comboBox;
      end else
        Self.ActiveControl := sgSpecies;
    end;
  end;
end;  // sgSpeciesSelectCell

//==============================================================================
{ Trap left and right arrow, return and ctrl-return, to provide navigation in
    the combo boxes on the species grid }
procedure TfrmPlaceCard.sgSpeciesComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lCellMove: Integer;
begin
  inherited;
  lCellMove := 0; // No movement by default
  with TComboBox(Sender) do
    // Right arrow
    if Key = VK_RIGHT then begin
      if (Style = csDropDownList) or        // Cannot edit, so always navigate within grid.
         (SelStart = Length(Text)) or       // Editable, but caret at right limit of text.
         (SelLength = Length(Text)) then    // Editable, all text selected.
        lCellMove := 1; // Cell to the right.
    end else
    // Left arrow
    if Key = VK_LEFT then begin
      if (Style = csDropDownList) or        // Cannot edit, so always navigate within grid.
         (SelStart = 0) or                  // Editable, but caret at left limit of text.
         (SelLength = Length(Text)) then    // Editable, all text selected.
        lCellMove := -1; // Cell to the left.
    end else
    // DEL or Backspace on a combo
    if (Key = VK_DELETE) or (Key = VK_BACK) then begin
      sgSpecies.Cells[sgSpecies.Col, sgSpecies.Row] := ''; // blank the cell
      if (Style = csDropDownList) then
        ItemIndex := -1; // blank the combo by clearing selection
    end else
      CheckRow(sgSpecies.Row);

  if lCellMove <> 0 then begin
    MoveGridCell(lCellMove = 1);
    Key := 0; // no further action on this keypress
  end; // if
end;  // sgSpeciesComboKeyDown

//==============================================================================
{ Trap left and right arrow to provide navigation in the normal cells on the
     species grid.  Also cause the delete or bkspace key to clear the cell.
     Return and CtrlReturn also navigate left and right }
procedure TfrmPlaceCard.sgSpeciesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lCellMove   : Integer;
  lInplaceEdit: TInplaceEdit;
begin
  inherited;
  lCellMove := 0; // No movement by default
  lInplaceEdit := TAccessorClass(Sender).InplaceEditor;
  with TStringGrid(Sender) do begin
    if Key = VK_F2 then begin
      if (Col = ColByName(COL_DETERMINER)) and not RowFromSample[Row] then begin
        dmFormActions.actNames.Execute;
        SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, GridDeterminerUpdate);
      end else
      if Col = ColByName(COL_SPATIAL_REF) then begin
        dmFormActions.actMapWindow.Execute;
        SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, GridSpatialRefUpdate);
      end else
      if Col = ColByName(COL_BIOTOPE) then begin
        dmFormActions.actBiotopeDiction.Execute;
        SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, GridBiotopeUpdate);
      end;
    end else
    // Right arrow
    if Key = VK_RIGHT then begin
      if not (goEditing in Options) then                            // cannot edit, so always navigate within grid
        lCellMove := 1 // cell to the right
      else
      if (lInplaceEdit.SelStart = Length(lInplaceEdit.Text)) or     // editable, but caret at right limit of text
         (lInplaceEdit.SelLength = Length(lInplaceEdit.Text)) then  // editable, all text selected
        lCellMove := 1; // cell to the right
    end else
    // Left arrow
    if Key = VK_LEFT then begin
      if not (goEditing in Options) then                            // cannot edit, so always navigate within grid
        lCellMove := -1 // cell to the left
      else
      if (lInplaceEdit.SelStart = 0) or                             // editable, but caret at left limit of text
         (lInplaceEdit.SelLength = Length(lInplaceEdit.Text)) then  // editable, all text selected
        lCellMove := -1; // cell to the left
    end else
    // Return key always moves, irrespective of edit text
    if Key = VK_RETURN then begin
      if not (ssCtrl in Shift) then
        lCellMove := 1
      else
        lCellMove := -1;
      if not ReturnKeyDown then
        lCellMove := 0; // keypress rejected because cell content invalid
    end else
    // del or backspace on an editable column, but not whilst editing
    if ((Key = VK_DELETE) or (Key = VK_BACK)) and
       not (IsReadOnlyCell(Col, Row) or (goEditing in Options)) then
      sgSpecies.Cells[Col, Row] := '' // blank the cell
    else
    // Check for F11 shortcut to insert current user or date
    if (Key = VK_F11) and (goEditing in Options) and
       (Col = ColByName(COL_DATE_OF_DETERMINATION)) then
    begin
      Cells[Col, Row] := DateToStr(Now);
      CheckRow(Row);
    end else
    if (Key = VK_F11) and (goEditing in Options) and
       (Col = ColByName(COL_DETERMINER)) then
    begin
      Cells[Col, Row] := AppSettings.Username;
      CheckRow(Row);
    end else
    // Else, if Space bar pressed on fixed col, check/uncheck taxon
    if IsReadOnlyCell(Col, Row) and (Key = VK_SPACE) then begin
      FGridClicked := True;
      SetActiveCellState(#0);
    end;

    if lCellMove <> 0 then begin
      if FEditingCell and HasColumnName(Col) then
        UpdateSpeciesValues(TColumnName(Objects[Col, 0]), Row);
      MoveGridCell(lCellMove = 1);
      Key := 0; // no further action on this keypress
    end;
  end;
end;  // sgSpeciesKeyDown

//==============================================================================
{ When ReturnKeyDown occurs, displays Find dialog if required to complete the
   text.  Result is true if determiner accepted, so the cell navigation
   should proceed }
function TfrmPlaceCard.ReturnKeyDown: Boolean;
var dlgFind     : TdlgFind;
    key         : TKeyString;
    itemCaption : String;
    lSpatialRef : TValidSpatialRef;
    liCol, liRow: Integer;
    lstValue    : String;
begin
  Result := True; // default
  with sgSpecies do
    if Cells[Col, Row] <> '' then begin
      liCol := Col;
      liRow := Row;
      // Update species selection column
      CheckRow(liRow);
      // Find recorder if on appropriate column and some text entered
      if liCol = ColByName(COL_DETERMINER) then begin
        if dmGeneralData.CheckIndividual(Cells[liCol, liRow], key, itemCaption) then
          Cells[liCol, liRow] := itemCaption
        else begin
          dlgFind := TdlgFind.CreateDialog(nil, ResStr_FindDeterminerName, ftIndividual);
          with dlgFind do begin
            try
              SetSearchText(Cells[liCol, liRow], true);  // True forces '*' if text is empty
              if not eSearchText.NoSourceItems then begin
                if ShowModal = mrOk then
                  Cells[liCol, liRow] := ItemText
                else
                  Result := False;
              end else begin
                MessageDlg(ResStr_NoIndividualItems, mtInformation, [mbOK], 0);
                Result := False;
              end;
            finally
              Free;
            end;
          end; // with dlgFind
        end;
      end else // determiner col and some text entered
      // Check biotope
      if liCol = ColByName(COL_BIOTOPE) then
      begin
        lstValue := Cells[liCol, liRow];
        if dmGeneralData.CheckBiotope(lstValue, key, itemCaption) then
          Cells[liCol, liRow] := itemCaption
        else begin
          Cells[liCol, liRow] := '';
          Result := false;
        end;
      end else
      // Check Spatial Ref is valid, and if not, get the map up
      if liCol = ColByName(COL_SPATIAL_REF) then
      begin
        lSpatialRef := ValidSpatialRef(Cells[liCol, liRow], AppSettings.SpatialRefSystem);
        if lSpatialRef.Valid then
          Cells[liCol, liRow] := lSpatialRef.FormattedSR
        else begin
          Cells[liCol, liRow] := '';
          dmFormActions.actMapWindow.Execute;
          SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, GridSpatialRefUpdate);
          Result := false;
        end;
      end;
    end;
end;  // ReturnKeyDown

//==============================================================================
procedure TfrmPlaceCard.sgSpeciesKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  with sgSpecies do
    // Only procees if the grid contains at least one taxon
    if Cells[2, 1] <> '' then
      if not (Key in [#10, #13]) and not IsReadOnlyCell(Col, Row) then
        CheckRow(Row);
end;  // sgSpeciesKeyPress

{-------------------------------------------------------------------------------
}
procedure TfrmPlaceCard.GridDeterminerUpdate(KeyList: TKeyList);
begin
  try
    if (KeyList <> nil) and (KeyList.Header.ItemCount > 0) then begin
      // Setup new value, and re-sync the grid
      with sgSpecies do begin
        Cells[ColByName(COL_DETERMINER), Row] := dmGeneralData.GetIndividualName(KeyList.Items[0].KeyField1);
        CheckRow(Row);
      end;
      Editing := true;
    end;
  finally
    KeyList.Free;
  end;
end;  // GridDeterminerUpdate

//------------------------------------------------------------------------------
procedure TfrmPlaceCard.GridBiotopeUpdate(KeyList: TKeyList);
begin
  try
    if (KeyList <> nil) and (KeyList.Header.ItemCount > 0) then begin
      // Setup new value, and re-sync the grid
      with sgSpecies do begin
        Cells[ColByName(COL_BIOTOPE), Row] := dmGeneralData.GetBiotopeCodeName(KeyList.Items[0].KeyField1);
        CheckRow(Row);
      end;
      Editing := true;
    end;
  finally
    KeyList.Free;
  end;
end;  // GridBiotopeUpdate

//------------------------------------------------------------------------------
procedure TfrmPlaceCard.GridSpatialRefUpdate(KeyList: TKeyList);
begin
  try
    if (KeyList <> nil)  and (KeyList.Header.ItemCount > 0) then
      if CompareText(KeyList.Header.TableName, 'SPATIAL_REF') = 0 then
        if KeyList.Items[0].KeyField2 <> '' then
          with sgSpecies do begin
            Cells[ColByName(COL_SPATIAL_REF), Row] := KeyList.Items[0].KeyField2;
            CheckRow(Row);
          end;
  finally
    KeyList.Free;
  end;
end;  // GridSpatialRefUpdate

//==============================================================================
function TfrmPlaceCard.ValidExactMeasurement(measurementKeys: TMeasurementKeys; const AValue: String): Boolean;
var liMinusPos: Integer;
begin
  // Valid data by default, but check further if numerical data
  Result := True;
  if Assigned(measurementKeys) and (measurementKeys.DataType = 'N') then begin
    liMinusPos := Pos('-', AValue);
    Result     := (IsFloat(AValue) or IsInt(AValue)) and (liMinusPos <= 1);
  end;
end;  // ValidExactMeasurement

//------------------------------------------------------------------------------
function TfrmPlaceCard.ValidEstimateMeasurement(measurementKeys: TMeasurementKeys; const AValue: String): Boolean;
var liMinusPos: Integer;
begin
  // Valid data by default, but check further if numerical data
  Result := True;
  if Assigned(measurementKeys) and (measurementKeys.DataType = 'N') then begin
    liMinusPos := Pos('-', AValue);
    Result := ((AValue[1] in ['<', '>', '~']) and
               (IsFloat(Trim(Copy(AValue, 2, 255))) or IsInt(Trim(Copy(AValue, 2, 255))))) or
              ((AValue[Length(AValue)] = '?') and
               (IsFloat(Copy(AValue, 1, Length(AValue) - 1)) or IsInt(Copy(AValue, 1, Length(AValue) - 1)))) or
              ((liMinusPos > 1) and
               (IsFloat(Copy(AValue, 1, liMinusPos - 1)) or IsInt(Copy(AValue, 1, liMinusPos - 1))) and
               (IsFloat(Copy(AValue, liMinusPos + 1, 255)) or IsInt(Copy(AValue, liMinusPos + 1, 255))));
  end;
end;  // ValidEstimateMeasurement

//------------------------------------------------------------------------------
function TfrmPlaceCard.ValidCircaMeasurement(measurementKeys: TMeasurementKeys; const AValue: String): Boolean;
begin
  // Valid data by default, but check further if numerical data
  Result := True;
  if Assigned(measurementKeys) and (measurementKeys.DataType = 'N') then
    Result := (UpCase(AValue[1]) = 'C') and
              (IsFloat(Trim(Copy(AValue, 2, 255))) or IsInt(Trim(Copy(AValue, 2, 255))));
end;  // ValidCircaMeasurement

//------------------------------------------------------------------------------
function TfrmPlaceCard.ValidRestrictedData(measurementKeys: TMeasurementKeys; const AValue: String): Boolean;
begin
  Result := True;
  if Assigned(measurementKeys) then
    Result := (measurementKeys.RestrictedValues.Count = 0) or
              (measurementKeys.RestrictedValues.IndexOf(AValue) <> -1);
end;  // ValidRestrictedData

//==============================================================================
{This writes a boolean registry key saying which survey has
previously been used.}
procedure TfrmPlaceCard.SetLastSurvey(const Value: TKeyString);
begin
  with TRegistry.Create do
    try
      if OpenKey(REG_KEY_SETTINGS, True) then begin
        WriteString('Place Card Survey', Value);
        CloseKey;
      end;
    finally
      Free;
    end;
end;

//==============================================================================
{This reads a boolean registry key saying which survey has
previously been used.}
function TfrmPlaceCard.GetLastSurvey: TKeyString;
begin
  Result := '';
  with TRegistry.Create do
    try
      if OpenKey(REG_KEY_SETTINGS, True) then begin
        if ValueExists('Place Card Survey') then
           Result := ReadString('Place Card Survey');
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function TfrmPlaceCard.CheckGridDeterminers(const ColIdx:integer): Boolean;
var lRowIdx  : Integer;
    detkey   : TKeyString;
    detname  : string;
    loKeyData: TCellData;
begin
  Result:=true;
  with sgSpecies do
    for lRowIdx := FixedRows to RowCount - 1 do begin
      // Trim spaces
      Cells[ColIdx, lRowIdx] := Trim(Cells[ColIdx, lRowIdx]);
      if Cells[ColIdx, lRowIdx] <> '' then
        if not dmGeneralData.CheckIndividual(Cells[ColIdx, lRowIdx], detkey, detname) then begin
          Result := false;
          Row := lRowIdx;
          Break;
        end else begin
          // When saving Determiner, it's quicker to get the Key from here
          // than querying the database again to get it.
          // If no key saved for this row, create one
          if Objects[ColIdx, lRowIdx] = nil then begin
            loKeyData := TCellData.Create;
            Objects[ColIdx, lRowIdx] := loKeyData;
          end;
          // Set ItemKey value to found Name Key
          TCellData(Objects[ColIdx, lRowIdx]).ItemKey := detkey;
          Cells[ColIdx, lRowIdx] := detname;
        end;
    end;
end;  // CheckGridDeterminers

//=============================================================================
// Validates that all determination vague dates in the grid are acceptable.
//
function TfrmPlaceCard.CheckGridDeterminationDates(const ColIdx: Integer): Boolean;
var lRowIdx: Integer;
    determinationDate: TVagueDate;
begin
  Result := True;
  with sgSpecies do begin
    for lRowIdx := FixedRows to RowCount - 1 do begin
      // Trim spaces
      Cells[ColIdx, lRowIdx] := Trim(Cells[ColIdx, lRowIdx]);
      // if nothing in cell, do nothing, all OK
      if (Cells[ColIdx, lRowIdx] <> '') and not IsReadOnlyCell(ColIdx, lRowIdx) then begin
        // If cell content is valid vague date, carry on with further checks
        if IsVagueDate(Cells[ColIdx, lRowIdx]) then begin
          determinationDate := StringToVagueDate(Cells[ColIdx, lRowIdx]);
          // Check vague date is correct and display properly in grid
          if dmValidation.CheckDeterminationDateAgainstSampleDate(eDate.VagueDate, determinationDate) then begin
            Cells[ColIdx, lRowIdx] := VagueDateToString(determinationDate);
          end
          else begin
            // Not a good vague date
            Result := False;
            Row    := lRowIdx;
            Break;
          end;
        end
        else begin
          // Cell content isn't a proper vague date
          Result := False;
          Row    := lRowIdx;
          Break;
        end;
      end;
    end;
  end;
end;  // CheckGridDeterminationDates

function TfrmPlaceCard.CheckCount: Boolean;
var lRowIdx, lCountIdx, lCountOfIdx: Integer;
    lstCount, lstCountOf           : String;
begin
  Result:=true;
  with sgSpecies do begin
    lCountIdx   := ColByName(COL_COUNT);
    lCountOfIdx := ColByName(COL_COUNT_OF);
    for lRowIdx := FixedRows to RowCount - 1 do begin
      if RowChecked[lRowIdx] then begin // if taxon ticked
        // Trim spaces
        lstCount := Trim(Cells[lCountIdx,lRowIdx]);
        Cells[lCountIdx,lRowIdx] := lstCount;
        lstCountOf := Trim(Cells[lCountOfIdx,lRowIdx]);
        Cells[lCountOfIdx,lRowIdx] := lstCountOf;
        if ((lstCount <> '') and (lstCountOf = '')) or
           ((lstCount = '') and (lstCountOf <> '')) then begin
          Result := false;
          Row := lRowIdx;
          Break;
        end;
      end;
    end;
  end;
end;  // CheckCount

function TfrmPlaceCard.CheckGridBiotope(const ColIdx: Integer): Boolean;
var liRow    : Integer;
    lKey     : TKeyString;
    lstValue : String;
    loKeyData: TCellData;
begin
  Result := true;
  with sgSpecies do
    for liRow := FixedRows to RowCount - 1 do begin
      // Trim spaces
      Cells[ColIdx, liRow] := Trim(Cells[ColIdx, liRow]);
      if Cells[ColIdx, liRow] <> '' then begin
        lstValue := Cells[ColIdx, liRow];
        lKey := '';
        if not FdmPlaceCard.CheckBiotope(lKey, lstValue) then begin
          Result := False;
          Row := liRow;
          Break;
        end else begin
          // If no key saved for this row, create one
          if Objects[ColIdx, liRow] = nil then begin
            loKeyData := TCellData.Create;
            Objects[ColIdx, liRow] := loKeyData;
          end;
          // Set ItemKey value to found BLI Key
          TCellData(Objects[ColIdx, liRow]).ItemKey := lKey;
        end;
      end;
    end;
end;  // CheckGridBiotope

function TfrmPlaceCard.CheckGridMeasurementColumns: Boolean;
var c, r: Integer;
    lstValue: String;
    measurementKeys: TMeasurementKeys;
begin
  Result := true;
  with sgSpecies do
    for c := 0 to ColCount - 1 do begin
      if HasColumnName(c) and not TColumnName(Objects[c, 0]).IsStandard then
      begin
        measurementKeys := TColumnName(Objects[c, 0]).MeasurementKeys;
        for r := FixedRows to RowCount - 1 do begin
          lstValue := Trim(Cells[c, r]);
          Cells[c, r] := lstValue;
          // if Taxon ticked and something in cell, check valid data
          if RowChecked[r] and (lstValue <> '') and
             ((not ValidExactMeasurement(measurementKeys, lstValue) and
               not ValidEstimateMeasurement(measurementKeys, lstValue) and
               not ValidCircaMeasurement(measurementKeys, lstValue)) or
              not ValidRestrictedData(measurementKeys, lstValue)) then
          begin
            Result := false;
            Row := r;
            Exit;
          end;
        end;
      end;
    end;
end;  // CheckGridMeasurementColumns

{-------------------------------------------------------------------------------
  Description : Validates that all spatial references in the grid are
              recognised
  Created : 25/04/2003 }
procedure TfrmPlaceCard.CheckGridSpatialRef(const ColIdx: Integer);
var liRow  : Integer;
    lSR    : TValidSpatialRef;
    lResult: Boolean;
    lValidationResult: TValidationResult;
    lSRSystem: String;
begin
  with sgSpecies do
    for liRow := FixedRows to RowCount - 1 do begin
      // Trim spaces
      Cells[ColIdx, liRow] := Trim(Cells[ColIdx, liRow]);
      if Cells[ColIdx, liRow] <> '' then begin
        lSR := ValidSpatialRef(
            DelocaliseSpatialRef(Cells[ColIdx, liRow]),
            AppSettings.SpatialRefSystem);
        if not lSR.Valid then begin
          Row := liRow;
          // Use VallidateValue to cause exception
          ValidateValue(false, SPlaceCard_SpatialRefInGridInvalid +
                               SPlaceCard_SelectValidSpatialRef, sgSpecies);
        end else begin
          lSRSystem := DetermineSpatialRefSystem(
              DelocaliseSpatialRef(Cells[ColIdx, liRow]));
          lValidationResult := dmValidation.CheckEventInSurvey(
              cmbSurvey.KeyValue,
              lSR.EnteredFormattedSR,
              lSRSystem,
              fraLocationInfo.eLocation.Key);
          if not lValidationResult.Success then
          begin
            sgSpecies.Row := liRow;
            sgSpecies.Col := ColIdx;
          end;
          ValidateValue(lValidationResult.Success, lValidationResult.Message, sgSpecies);

          if fraLocationInfo.eLocation.Key <> '' then begin
            lResult := dmValidation.CheckSRefInLocation(
                fraLocationInfo.eLocation.Key,
                lSR.EnteredFormattedSR,
                lSRSystem);
            if not lResult then begin
              Row := liRow;
              ValidateValue(false, SPlaceCard_SpatialRefNotInGridSq +
                                   SPlaceCard_SelectValidSpatialRef, sgSpecies);
            end;
          end else
          if fraLocationInfo.eSpatialRef.EnteredRef <> '' then
            if not CheckSRefInSRef(
                fraLocationInfo.eSpatialRef.EnteredRef,
                lSR.EnteredFormattedSR,
                fraLocationInfo.eSpatialRef.EnteredSystem,
                lSRSystem) then
            begin
              Row := liRow;
              ValidateValue(false, SPlaceCard_SpatialRefNotInSample +
                                   SPlaceCard_SelectValidSpatialRef, sgSpecies);
            end;
        end;
      end;
    end;
end;  // CheckGridSpatialRef

{-------------------------------------------------------------------------------
  Description : Perform a full validation of the card.
  Raises a non-critical exception and focus the control if anything found.
  Created : 25/04/2003 }
procedure TfrmPlaceCard.ValidateCard;
begin
  ValidateValue(cmbSurvey.Text <> '', ResStr_UnableToSave + SPlaceCard_SelectSurvey, cmbSurvey);
  ValidateValue(TotalSpecies > 0, ResStr_SelectTaxaToSave, sgSpecies);
  If cmbSurvey.KeyValue <> FDefaultSurveyKey then
     If MessageDlg(ResStr_SurveyNotEqualDefaultConfirm, mtConfirmation, [mbYes,mbNo], 0)=mrNo then
       ValidateValue(cmbSurvey.KeyValue = FDefaultSurveyKey,Restr_SurveyNotEqualDefault, cmbSurvey);

  ValidateValue(TotalSpecies > 0, ResStr_SelectTaxaToSave, sgSpecies);
  fraLocationInfo.Validate(cmbSurvey.KeyValue);

  // Check the date and ensure a proper format
  ValidateValue(eDate.Text <> '', ResStr_ObservationDateMissing, eDate);
  ValidateCardDate;
  ValidateValue(
      dmValidation.CheckEventDateAgainstSurvey(cmbSurvey.KeyValue, eDate.VagueDate),
      ResStr_DataOutsideSurvey,
      eDate);
  // Check other fields
  ValidateValue(cmbSampleType.Text <> '', ResStr_SampleTypeMissing, cmbSampleType);
  ValidateValue(
      (eBiotope.Text = '') or (FBiotopeListItemKey <> '')
      or ((FBiotopeListItemKey = '') and FdmPlaceCard.CheckBiotope(FBiotopeListItemKey, eBiotope)),
      ResStr_InvalidBiotope,
      eBiotope);
  ValidateValue(
      lbRecorders.Items.Count > 0,
      SPlaceCard_RecorderNameMissing + SPlaceCard_SelectOne,
      bbRecorderAdd);
  ValidateValue(
      (eReference.Text = '') or FdmPlaceCard.CheckReference(FReferenceKey, eReference),
      ResStr_InvalidDocument,
      eReference);
  // Check various values in grid
  if ColByName(COL_DETERMINER) <> -1 then
    ValidateValue(
        CheckGridDeterminers(ColByName(COL_DETERMINER)),
        ResStr_InvalidDeterminerName,
        sgSpecies);
  if ColByName(COL_DATE_OF_DETERMINATION) <> -1 then
    ValidateValue(
        CheckGridDeterminationDates(ColByName(COL_DATE_OF_DETERMINATION)),
        ResStr_InvalidDerterminationDate + #13#10#13#10 + SPlaceCard_ValidVagueDates,
        sgSpecies);
  if ColByName(COL_COUNT) <> -1 then
    ValidateValue(CheckCount, ResStr_SettingCount, sgSpecies);
  ValidateValue(CheckGridMeasurementColumns, ResStr_InvalidMeasurementValues, sgSpecies);
  if ColByName(COL_BIOTOPE) <> -1 then
    ValidateValue(CheckGridBiotope(ColByName(COL_BIOTOPE)), SPlaceCard_InvalidBiotope, sgSpecies);
  if ColByName(COL_SPATIAL_REF) <> -1 then
    CheckGridSpatialRef(ColByName(COL_SPATIAL_REF));
end;  // ValidateCard

{-------------------------------------------------------------------------------
  Description : Saves the card contents to the database.
  Created : 28/04/2003 }
procedure TfrmPlaceCard.SaveCard;
var lCursor: TCursor;
    i: Integer;
begin
  lCursor := HourglassCursor;
  try
    try
      InitialiseKeyVariables;

      FSurveyEventKey := FdmPlaceCard.GetSurveyEventKey(
          cmbSurvey.KeyValue,
          fraLocationInfo.eLocation.Key,
          fraLocationInfo.eLocationName.Text,
          fraLocationInfo.eSpatialRef,
          lbRecorders.Items,
          eDate.Text,
          FSERKeys);

      if (FSurveyEventKey = '') and (fraLocationInfo.eLocationName.Text <> '') then
        FSurveyEventKey := FdmPlaceCard.GetSurveyEventKey(
            cmbSurvey.KeyValue,
            fraLocationInfo.eLocation.Key,
            '',
            fraLocationInfo.eSpatialRef,
            lbRecorders.Items,
            eDate.Text,
            FSERKeys);

      //Line below may not seem necessary but this variable is used in "CleanUp"
      FNewSurveyEvent := FSurveyEventKey = '';
      if FNewSurveyEvent then
        CreateSurveyEvent;

      // "Main" biotope occurrence, goes with "main" sample
      if eBiotope.Text <> '' then begin
        // "Main" Sample record
        CreateSampleRecord(fraLocationInfo.eSpatialRef.EnteredRef, True);
        CreateBiotopeOccurrence(
            FSampleKey,
            FBiotopeListItemKey,
            TKeyData(lbRecorders.Items.Objects[0]).ItemKey,
            eDate.Text);
      end;
      // All selected species in the grid
      CreateSpeciesOccurrences;

      if TotalSpecies = 1 then
        MessageDlg(SPlaceCard_SingleOccurrenceRecorded, mtInformation, [mbOK], 0)
      else
        MessageDlg(IntToStr(TotalSpecies) + SPlaceCard_MultiOccurrenceRecorded, mtInformation, [mbOK], 0);

      // Update observations screen with new stuff
      UpdateObservations;
      if FSampleKey <> '' then
        dmGeneralData.CheckZOForSample(FSampleKey);
      if Assigned(FSampleKeys) then
        for i := 0 to FSampleKeys.Count - 1 do
          dmGeneralData.CheckZOForSample(FSampleKeys[i]);

      if not FLoadedFromSample then begin
        ClearData;
        cmbSurvey.SetFocus;
        LastSurvey := cmbSurvey.KeyValue;
      end;



    except
      on E:EDatabaseWriteError do begin
        Cleanup;
        DefaultCursor(lCursor);
        MessageDlg(SPlaceCard_SaveError + E.Message, mtInformation, [mbOK], 0);
      end;
    end;
  finally
    Editing := False;
    DefaultCursor(lCursor);
  end;
end;  // SaveCard

{-------------------------------------------------------------------------------
}
procedure TfrmPlaceCard.UpdateMapWindowSelector;
begin
  fraLocationInfo.UpdateMapWindowSelector;
end;

{-------------------------------------------------------------------------------
  Accessor to retrieve the taxon key for a row
}
function TfrmPlaceCard.GetRowTLIKey(Index: Integer): TKeyString;
begin
  if Assigned(sgSpecies.Objects[0, Index]) then
    Result := TKeyData(sgSpecies.Objects[0, Index]).ItemKey
  else
    Result := '';
end;

{-------------------------------------------------------------------------------
  Accessor - adds an object to store the taxon key to the grid column 0
}
procedure TfrmPlaceCard.SetRowTLIKey(Index: Integer; const Value: TKeyString);
var
  lNewRowData: TRowData;
begin
  if Assigned(sgSpecies.Objects[0, Index]) then
    TRowData(sgSpecies.Objects[0, Index]).ItemKey := Value
  else begin
    //Now create ourselves an object for the key.
    lNewRowData := TRowData.Create;
    lNewRowData.ItemKey := Value;
    //Put the object into the grid's repository (good word huh? ;-)
    sgSpecies.Objects[0, Index] := lNewRowData;
  end;
end;

{-------------------------------------------------------------------------------
  Accessor
}
function TfrmPlaceCard.GetRowChecked(Index: Integer): Boolean;
begin
  if Assigned(sgSpecies.Objects[0, Index]) then
    Result := TRowData(sgSpecies.Objects[0,Index]).Checked
  else
    Result := False;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TfrmPlaceCard.SetRowChecked(Index: Integer; Value: Boolean);
var
  lNewRowData: TRowData;
begin
  if Assigned(sgSpecies.Objects[0, Index]) then
    TRowData(sgSpecies.Objects[0, Index]).Checked := Value
  else begin
    // create ourselves an object to hold data about the row.
    lNewRowData := TRowData.Create;
    lNewRowData.Checked := Value;
    //Put the object into the grid's repository (good word huh? ;-)
    sgSpecies.Objects[0, Index] := lNewRowData;
  end;
  // Ensure redraw
  sgSpecies.Invalidate;
end;

{-------------------------------------------------------------------------------
  Check a given row, if not already checked, and keep track of species count
}
procedure TfrmPlaceCard.CheckRow(ARow: integer);
begin
  if not RowChecked[ARow] then begin
    RowChecked[ARow] := True;
    TotalSpecies := TotalSpecies + 1;
  end;
end;

{-------------------------------------------------------------------------------
}
function TfrmPlaceCard.GetRowTaxonOccKey(Index: Integer): TKeyString;
begin
  if Assigned(sgSpecies.Objects[0, Index]) then
    Result := TRowData(sgSpecies.Objects[0,Index]).TaxonOccKey
  else
    Result := '';
end;

{-------------------------------------------------------------------------------
}
procedure TfrmPlaceCard.SetRowTaxonOccKey(Index: Integer; const Value: TKeyString);
var
  lNewRowData: TRowData;
begin
  if Assigned(sgSpecies.Objects[0, Index]) then
    TRowData(sgSpecies.Objects[0, Index]).TaxonOccKey := Value
  else begin
    // create ourselves an object to hold data about the row.
    lNewRowData := TRowData.Create;
    lNewRowData.TaxonOccKey := Value;
    //Put the object into the grid's repository (good word huh? ;-)
    sgSpecies.Objects[0, Index] := lNewRowData;
  end;
  // Ensure redraw
  sgSpecies.Invalidate;
end;

{-------------------------------------------------------------------------------
  Accessor
}
function TfrmPlaceCard.GetRowFromSample(Index: Integer): Boolean;
begin
  if Assigned(sgSpecies.Objects[0, Index]) then
    Result := TRowData(sgSpecies.Objects[0,Index]).FromSample
  else
    Result := False;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TfrmPlaceCard.SetRowFromSample(Index: Integer; Value: Boolean);
var
  lNewRowData: TRowData;
begin
  if Assigned(sgSpecies.Objects[0, Index]) then
    TRowData(sgSpecies.Objects[0, Index]).FromSample := Value
  else begin
    // create ourselves an object to hold data about the row.
    lNewRowData := TRowData.Create;
    lNewRowData.FromSample := Value;
    //Put the object into the grid's repository (good word huh? ;-)
    sgSpecies.Objects[0, Index] := lNewRowData;
  end;
  // Ensure redraw
  sgSpecies.Invalidate;
end;

{-------------------------------------------------------------------------------
}
function TfrmPlaceCard.GetRowCustodian(Index: Integer): TKeyString;
begin
  if Assigned(sgSpecies.Objects[0, Index]) then
    Result := TRowData(sgSpecies.Objects[0,Index]).Custodian
  else
    Result := '';
end;

{-------------------------------------------------------------------------------
}
procedure TfrmPlaceCard.SetRowCustodian(Index: Integer; const Value: TKeyString);
var
  lNewRowData: TRowData;
begin
  if Assigned(sgSpecies.Objects[0, Index]) then
    TRowData(sgSpecies.Objects[0, Index]).Custodian := Value
  else begin
    // create ourselves an object to hold data about the row.
    lNewRowData := TRowData.Create;
    lNewRowData.Custodian := Value;
    //Put the object into the grid's repository (good word huh? ;-)
    sgSpecies.Objects[0, Index] := lNewRowData;
  end;
  // Ensure redraw
  sgSpecies.Invalidate;
end;

{-------------------------------------------------------------------------------
  Synch the admin areas box when the location changes
}
procedure TfrmPlaceCard.fraLocationInfoUpdateLocation(Sender: TObject);
begin
  if fraLocationInfo.eLocation.Key='' then
    eAdminArea.Text := ''
  else
    eAdminArea.Text := FdmPlaceCard.GetAdminAreas(fraLocationInfo.eLocation.Key);
end;

{-------------------------------------------------------------------------------
  Determines the hint for the eTaxon control when the Rapid Entry Delimiter
    has been set.
}
procedure TfrmPlaceCard.BuildRapidEntryTaxonHint;
var
  col: Integer;
begin
  FRapidTaxonEntryHintWindow.HintStrings.Clear;
  if (Length(AppSettings.RapidEntryDelimiter) = 1) then
    for col := 2 to sgSpecies.ColCount - 1 do
      if (col = 2) or (not IsFixedCol(col)) then
        if (Length(sgSpecies.Cells[col, 0]) > 0) then
          FRapidTaxonEntryHintWindow.HintStrings.Add(sgSpecies.Cells[col, 0]);
end;

{-------------------------------------------------------------------------------
  Displays the special hint for rapidly entering a new taxon.
}
procedure TfrmPlaceCard.eTaxonEnter(Sender: TObject);
begin
  if (Length(AppSettings.RapidEntryDelimiter) = 1) then begin
    TaxonEntryDelimiterCount := CountDelimiters;
    DisplayRapidTaxonEntryHint;
  end;    // if (Length(AppSettings.RapidEntryDelimiter) = 1)
end;

{-------------------------------------------------------------------------------
  Gets rid of the special hint for rapidly entering a new taxon.
}
procedure TfrmPlaceCard.eTaxonExit(Sender: TObject);
begin
  FRapidTaxonEntryHintWindow.ReleaseHandle;
end;

{-------------------------------------------------------------------------------
  Accessor method - sets how many rapid taxon entry column delimiters
  have been entered by the user.
}
procedure TfrmPlaceCard.SetTaxonEntryDelimiterCount(const Value: integer);
begin
  if (FRapidTaxonEntryHintWindow.DelimiterCount <> Value) then begin
    FRapidTaxonEntryHintWindow.DelimiterCount := Value;
    if (ActiveControl = eTaxon) then begin
      FRapidTaxonEntryHintWindow.ReleaseHandle;
      DisplayRapidTaxonEntryHint;
    end;    // if (ActiveControl = eTaxon)
  end;    // if (FRapidTaxonEntryHintWindow.DelimiterCount <> Value)
end;

{-------------------------------------------------------------------------------
  Accessor method - gets how many rapid taxon entry column delimiters
  have been entered by the user.
}
function TfrmPlaceCard.GetTaxonEntryDelimiterCount: integer;
begin
  Result := FRapidTaxonEntryHintWindow.DelimiterCount;
end;

{-------------------------------------------------------------------------------
  Displays the special hint for rapidly entering a new taxon.
}
procedure TfrmPlaceCard.DisplayRapidTaxonEntryHint;
var
  HintRect: TRect;
  HintUpperLeft, HintLowerRight: TPoint;
begin
  if (Length(AppSettings.RapidEntryDelimiter) = 1) then begin
    // Calculate where the hint needs to appear
    HintUpperLeft.X := eTaxon.Left;
    HintUpperLeft.Y := eTaxon.Top + eTaxon.Height;
    HintRect.TopLeft := eTaxon.ClientToScreen(HintUpperLeft);

    // Calculate how big the hint window needs to be
    HintLowerRight.X := HintRect.Left
      + FRapidTaxonEntryHintWindow.Canvas.TextWidth(FRapidTaxonEntryHintWindow.PreviousColumnNames + FRapidTaxonEntryHintWindow.FollowingColumnNames)
      + ((FRapidTaxonEntryHintWindow.Canvas.TextWidth(FRapidTaxonEntryHintWindow.CurrentColumnName) * 14) div 10) + 10;
    HintLowerRight.Y := HintRect.Top
      + FRapidTaxonEntryHintWindow.Canvas.TextHeight(FRapidTaxonEntryHintWindow.HintText);

    HintRect.BottomRight := HintLowerRight;

    // Display the hint
    if (Length(FRapidTaxonEntryHintWindow.HintText) > 0) then
      FRapidTaxonEntryHintWindow.ActivateHint(HintRect, FRapidTaxonEntryHintWindow.HintText);
  end;    // if (Length(AppSettings.RapidEntryDelimiter) = 1)
end;

{-------------------------------------------------------------------------------
  Counts how many delimiters the user has entered in the taxon edit box.
}
function TfrmPlaceCard.CountDelimiters: integer;
var
  i: integer;
begin
  Result := 0;
  if (Length(AppSettings.RapidEntryDelimiter) = 1) then begin
    i := Pos(AppSettings.RapidEntryDelimiter, eTaxon.Text);
    while i > 0 do begin
      Inc(Result);
      i := PosEx(AppSettings.RapidEntryDelimiter, eTaxon.Text, i + 1);
    end;    // while i > 0
  end;    // if (Length(AppSettings.RapidEntryDelimiter) = 1)
end;

{-------------------------------------------------------------------------------
  CCN248 - F11 shortcut to add current user as a recorder
}
procedure TfrmPlaceCard.lbRecordersKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if Key=VK_F11 then
    AddRecorder(AppSettings.Username, AppSettings.UserId);
end;

{-------------------------------------------------------------------------------
  Clear the current values in FSpeciesValues to prevent auto-fill occuring
}
procedure TfrmPlaceCard.ClearSpeciesValues;
var lCol, lColIdx: Integer;
begin
  // Do this so we can keep the common names if Sc-Names are there too.
  lCol := ColByName(COL_COMMON_NAME);
  if lCol = -1 then lCol := 2;
  // Retain Species Codes if present
  if ColByName(COL_CODE_NUMBER) <> -1 then
    lCol := ColByName(COL_CODE_NUMBER);

  // Clear all values from list
  for lColIdx := lCol + 1 to sgSpecies.ColCount - 1 do
    if HasColumnName(lColIdx) then
      FSpeciesValues.Values[TColumnName(sgSpecies.Objects[lColIdx, 0]).OriginalName] := '';
end;

{-------------------------------------------------------------------------------
}
function TfrmPlaceCard.DataRestrictedMeasurement(ACol: Integer): Boolean;
var
  item: TMeasurementKeys;
begin
  Result := False;
  item   := nil;
  if HasColumnName(ACol) and not TColumnName(sgSpecies.Objects[ACol, 0]).IsStandard then
    item := TColumnName(sgSpecies.Objects[ACol, 0]).MeasurementKeys;
  if Assigned(item) and (item.RestrictedValues.Count > 0) then begin
    cmbRestrictedData.Items.CommaText := item.RestrictedValues.CommaText;
    Result := True;
  end;
end;

{-------------------------------------------------------------------------------
}
function TfrmPlaceCard.HasColumnName(col: Integer): Boolean;
begin
  Result := Assigned(sgSpecies.Objects[col, 0]) and (sgSpecies.Objects[col, 0] is TColumnName);
end;

{-------------------------------------------------------------------------------
}
function TfrmPlaceCard.GetCurrentControlEditMode: TEditMode;
begin
  if Assigned(ActiveControl) and not (ActiveControl is TStringGrid) then
    Result := emEdit
  else
    Result := emNone;
end;  // GetCurrentControlEditMode

{-------------------------------------------------------------------------------
  Populate the card from the sample details and its occurrences.
}
procedure TfrmPlaceCard.LoadFromSample(const ASurveyKey, AEventKey, ASampleKey: TKeyString;
  const ASampleCaption: String);
var
  rs: _Recordset;
  data: TKeyData;
  rowIdx: Integer;
  allMeasurementsLoaded: Boolean;
begin
  Caption := Format('%s - [%s]', [Caption, ASampleCaption]);

  // Start with disabling all the controls where values can't be changed.
  cmbSurvey.ReadOnly       := True;
  cmbSampleType.ReadOnly   := True;
  eBiotope.ReadOnly        := True;
  bbBiotopeFind.Enabled    := False;
  lbRecorders.OnKeyDown    := nil;    // Just unlink the handlers so the list can stay enabled.
  lbRecorders.OnKeyPress   := nil;
  bbRecorderFind.Enabled   := False;
  bbRecorderAdd.Enabled    := False;
  bbRecorderRemove.Enabled := False;
  reComments.ReadOnly      := True;
  eReference.ReadOnly      := True;
  bbReferenceFind.Enabled  := False;
  bbReset.Caption          := ResStr_BtnCancel;
  bbReset.Hint             := '';

  // Since they can't be edited, they don't need the mandatory colour.
  SetReadOnlyFieldsColourState(True,
      [cmbSurvey, cmbSampleType, lbRecorders, eBiotope, reComments, eReference]);

  // Load Sample data
  cmbSurvey.KeyValue := ASurveyKey;
  FSurveyEventKey    := AEventKey;
  FSampleKey         := ASampleKey;

  // Get most of the sample details here.
  rs := dmDatabase.GetRecordset('usp_SampleDetails_Get', ['@SampleKey', ASampleKey]);
  with rs do begin
    FCustodian := VarToStr(Fields['Custodian'].Value);

    fraLocationInfo.eLocation.Key      := VarToStr(Fields['Location_Key'].Value);
    fraLocationInfo.eLocation.Text     := VarToStr(Fields['Location'].Value);
    fraLocationInfo.eLocationName.Text := VarToStr(Fields['Location_Name'].Value);

    fraLocationInfo.eSpatialRef.Qualifier     := VarToStr(Fields['Spatial_Ref_Qualifier'].Value);
    fraLocationInfo.eSpatialRef.EnteredRef    := VarToStr(Fields['Spatial_Ref'].Value);
    fraLocationInfo.eSpatialRef.EnteredSystem := VarToStr(Fields['Spatial_Ref_System'].Value);
    fraLocationInfo.eSpatialRef.DisplayRef    := GetDisplaySpatialRef(
        AppSettings.SpatialRefSystem,
        VarToStr(Fields['Spatial_Ref'].Value),
        VarToStr(Fields['Spatial_Ref_System'].Value),
        Fields['Lat'].Value,
        Fields['Long'].Value,
        '');
    eDate.Text := VagueDateToString(dmGeneralData.GetVagueDateFromRecordset(rs));

    cmbSampleType.KeyValue := Fields['Sample_Type_Key'].Value;
    reComments.Text        := VarToStr(Fields['Comment'].Value);

    // For cascade update checks on save.
    FLoadedSampleValues := TStringList.Create;
    FLoadedSampleValues.Values['Location_Key']          := VarToStr(Fields['Location_Key'].Value);
    FLoadedSampleValues.Values['Spatial_Ref']           := VarToStr(Fields['Spatial_Ref'].Value);
    FLoadedSampleValues.Values['Spatial_Ref_Qualifier'] := VarToStr(Fields['Spatial_Ref_Qualifier'].Value);
    FLoadedSampleValues.Values['Location_Name']         := VarToStr(Fields['Location_Name'].Value);
    FLoadedSampleValues.Values['Vague_Date_Start']      := VarToStr(Fields['Vague_Date_Start'].Value);
    FLoadedSampleValues.Values['Vague_Date_End']        := VarToStr(Fields['Vague_Date_End'].Value);
    FLoadedSampleValues.Values['Vague_Date_Type']       := VarToStr(Fields['Vague_Date_Type'].Value);

    Close;
  end;

  eDate.ReadOnly := not HaveCustody;
  fraLocationInfo.ReadOnly := not HaveCustody;

  // Get the sample recorders. And prepare FSERKeys at the same time, needed if adding new stuff.
  if not Assigned(FSERKeys) then FSERKeys := TStringList.Create;
  rs := dmDatabase.GetRecordset('usp_SampleRecoders_Select_ForSample', ['@SampleKey', ASampleKey]);
  with rs do begin
    while not Eof do begin
      data := TKeyData.Create;
      data.ItemKey := Fields['Name_Key'].Value;
      lbRecorders.Items.AddObject(Fields['Name'].Value, data);
      FSERKeys.Add(Fields['Key'].Value);
      MoveNext;
    end;
    Close;
  end;

  // Get the biotopes for the sample and take the first one to display on the card.
  rs := dmDatabase.GetRecordset('usp_BiotopeOccurrences_Select_ForSample', ['@SampleKey', ASampleKey]);
  with rs do begin
    if not Eof then begin
      eBiotope.Text := Fields['Item_Name'].Value;
      FBiotopeListItemKey := Fields['Biotope_List_Item_Key'].Value;
    end;
    Close;
  end;

  // Get Document refs and display first one.
  rs := dmDatabase.ExecuteSQL(Format(
      'SELECT TOP 1 Source_Key FROM Sample_Sources WHERE Sample_Key = ''%s''', [ASampleKey]), True);
  if not rs.Eof then 
    eReference.Text := dmGeneralData.GetReferenceText(rs.Fields['Source_Key'].Value);
  rs.Close;


  // Get all the taxon occurrences (with preferred determinations).
  allMeasurementsLoaded := True;
  rs := dmDatabase.GetRecordset('usp_TaxonOccurrences_Select_ForSample', ['@SampleKey', ASampleKey]);
  with rs do begin
    while not Eof do begin
      // Add to grid, if not already there.
      rowIdx := AddNewTaxonToGrid(Fields['Taxon_List_Item_Key'].Value, False, True);
      // Need to populate the columns now
      RowTaxonOccKey[rowIdx] := Fields['Taxon_Occurrence_Key'].Value;
      RowCustodian[rowIdx]   := Fields['Custodian'].Value;
      RowFromSample[rowIdx]  := True;
      allMeasurementsLoaded := PopulateColumns(rowIdx, rs) and allMeasurementsLoaded;
      MoveNext;
    end;
    Close;
  end;

  FLoadedFromSample                    := True;
  Editing                              := False;
  fraLocationInfo.eSpatialRef.Modified := False;
  bbReset.Enabled                      := True;  // Changing Editing disabled it.
  fraLocationInfo.eLocation.SetFocus;

  if not allMeasurementsLoaded then
    ShowInformation(ResStr_RecordingCardMissingColumns);
end;  // LoadFromSample

{-------------------------------------------------------------------------------
  Go through the columns on the card and try and load data from the occurrences.
  Data will be coming from various places depending on column being handled.
}
function TfrmPlaceCard.PopulateColumns(rowIdx: Integer; data: _Recordset): Boolean;
var
  colIdx, i: Integer;
  rs: _Recordset;
  measurementKeys: TMeasurementKeys;
  oneLoaded: Boolean;

  procedure SetCellAndData(col: Integer; rec: _Recordset; const cellText: String; const itemKey:
      String = '');
  var
    cellData: TCellData;
  begin
    if Assigned(sgSpecies.Objects[col, rowIdx]) then
      cellData := TCellData(sgSpecies.Objects[col, rowIdx])
    else
      cellData := TCellData.Create;
    cellData.Custodian      := rec.Fields['Custodian'].Value;
    cellData.ItemAdditional := cellText;  // Remember original value.
    if itemKey <> '' then cellData.ItemKey := itemKey;
    sgSpecies.Cells[col, rowIdx]   := cellText;
    sgSpecies.Objects[col, rowIdx] := cellData;
  end;

begin
  // Use the same recordset for most columns, since it's the same record.
  if (ColByName(COL_DETERMINER) > -1) or (ColByName(COL_DATE_OF_DETERMINATION) > -1) then begin
    colIdx := ColByName(COL_DETERMINER);
    if colIdx > -1 then begin
      SetCellAndData(
          colIdx,
          data,
          dmGeneralData.GetName(data.Fields['Determiner'].Value),
          data.Fields['Determiner'].Value);
    end;
    colIdx := ColByName(COL_DATE_OF_DETERMINATION);
    if colIdx > -1 then begin
      SetCellAndData(colIdx, data, VagueDateToString(dmGeneralData.GetVagueDateFromRecordset(data)));
    end;
  end;

  colIdx := ColByName(COL_RECORD_TYPE);
  if (colIdx > -1) and not VarIsNull(data.Fields['Record_Type_Key'].Value) then begin
    cmbRecordtype.KeyValue := data.Fields['Record_Type_Key'].Value;
    SetCellAndData(colIdx, data, cmbRecordType.Text);
  end;

  colIdx := ColByName(COL_SUBSTRATE);
  if (colIdx > -1) and not VarIsNull(data.Fields['Substrate_Key'].Value) then begin
    cmbSubstrate.KeyValue := data.Fields['Substrate_Key'].Value;
    SetCellAndData(colIdx, data, cmbSubstrate.Text);
  end;

  colIdx := ColByName(COL_COMMENT);
  if (colIdx > -1) and not VarIsNull(data.Fields['Comment'].Value) then
    SetCellAndData(colIdx, data, dmGeneralData.ConvertRichTextToText(data.Fields['Comment'].Value));

  colIdx := ColByName(COL_PROVENANCE);
  if (colIdx > -1) and not VarIsNull(data.Fields['Provenance'].Value) then
    SetCellAndData(colIdx, data, data.Fields['Provenance'].Value);

  // Load measurements related to the occurrence
  Result := True;
  colIdx := ColByName(COL_COUNT_OF);
  rs  := dmDatabase.GetRecordset(
      'usp_TaxonOccurrenceData_Select_ForTaxonOccurrence',
      ['@OccurrenceKey', data.Fields['Taxon_Occurrence_Key'].Value]);
  with rs do begin
    // For each measurement, locate and populate the corresponding column on the grid, if possible.
    while not Eof do begin
      oneLoaded := False;
      for i := 0 to sgSpecies.ColCount - 1 do
        if HasColumnName(i) and not TColumnName(sgSpecies.Objects[i, 0]).IsStandard then
        begin
          measurementKeys := TColumnName(sgSpecies.Objects[i, 0]).MeasurementKeys;
          if SameText(measurementKeys.ItemKey, Fields['Measurement_Qualifier_Key'].Value) and
             SameText(measurementKeys.ItemAdditional, Fields['Measurement_Unit_Key'].Value) then
          begin
            SetCellAndData(i, rs, Fields['Data'].Value, Fields['Data_Key'].Value);
            oneLoaded := True;
            Break;
          end;
        end;

      // Count/Count Of/Accuracy columns. Always check for "own" measurements, in case the
      // first one read wasn't owned.
      if not oneLoaded
         and (colIdx > -1)
         and SameText(COUNT_MEASUREMENT_UNIT_KEY, Fields['Measurement_Unit_Key'].Value)
         and ((sgSpecies.Cells[colIdx, rowIdx] = '')
           or (not IsCellCustodian(colIdx, rowIdx)
             and (Fields['Custodian'].Value = AppSettings.SiteID)
           )
         ) then
      begin
        SetCellAndData(colIdx, rs, Fields['Qualifier_Short_Name'].Value, Fields['Data_Key'].Value);
        SetCellAndData(ColByName(COL_COUNT), rs, Fields['Data'].Value, Fields['Data_Key'].Value);
        SetCellAndData(ColByName(COL_ACCURACY), rs, VarToStr(Fields['Accuracy'].Value), Fields['Data_Key'].Value);
        oneLoaded := True;
      end;
      Result := oneLoaded and Result;

      MoveNext;
    end;
    Close;
  end;
end;  // PopulateColumns

{-------------------------------------------------------------------------------
  Validate date and spatial ref and update sample(s) accordingly.
}
procedure TfrmPlaceCard.UpdateSample;
var
  cascadeSiblings: Boolean;
  cascadeType: TCascadeType;
  dmSample: TdmSample;
  dmTaxonOcc: TdmTaxonOccurrences;
  oldValues, newValues: TStringList;
  i: Integer;
  updateSQL: String;
begin
  dmSample   := nil;
  dmTaxonOcc := nil;
  oldValues  := nil;
  newValues  := nil;
  try
    dmSample   := TdmSample.Create(nil);
    dmTaxonOcc := TdmTaxonOccurrences.Create(nil);
    oldValues  := TStringList.Create;
    newValues  := TStringList.Create;
    updateSQL  := '';

    // Work out if location or date info has changed.
    if FloadedSampleValues.Values['Location_Key'] <> fraLocationInfo.eLocation.Key then
    begin
      oldValues.Values['Location_Key'] := FloadedSampleValues.Values['Location_Key'];
      newValues.Values['Location_Key'] := fraLocationInfo.eLocation.Key;
      updateSQL := Format(
          ' Location_Key = %s, ',
          [IIf(fraLocationInfo.eLocation.Key = '', 'NULL', #39 + fraLocationInfo.eLocation.Key) + #39]);
    end;

    if FloadedSampleValues.Values['Spatial_Ref'] <> fraLocationInfo.eSpatialRef.EnteredRef then
    begin
      oldValues.Values['Spatial_Ref'] := FloadedSampleValues.Values['Spatial_Ref'];
      newValues.Values['Spatial_Ref'] := fraLocationInfo.eSpatialRef.EnteredRef;
      updateSQL := Format(
          '%s Spatial_Ref_System = ''%s'', Spatial_Ref = ''%s'', ',
          [updateSQL,
           fraLocationInfo.eSpatialRef.EnteredSystem,
           newValues.Values['Spatial_Ref']]);
    end;

    if FloadedSampleValues.Values['Spatial_Ref_Qualifier'] <> fraLocationInfo.eSpatialRef.Qualifier then
    begin
      oldValues.Values['Spatial_Ref_Qualifier'] := FloadedSampleValues.Values['Spatial_Ref_Qualifier'];
      newValues.Values['spatial_Ref_Qualifier'] := fraLocationInfo.eSpatialRef.Qualifier;
      updateSQL := Format(
          '%s Spatial_Ref_Qualifier = ''%s'', ',
          [updateSQL, fraLocationInfo.eSpatialRef.Qualifier]);
    end;

    if FloadedSampleValues.Values['Location_Name'] <> fraLocationInfo.eLocationName.Text then
    begin
      oldValues.Values['Location_Name'] := FloadedSampleValues.Values['Location_Name'];
      newValues.Values['Location_Name'] := fraLocationInfo.eLocationName.Text;
      updateSQL := Format(
          '%s Location_Name = ''%s'', ',
          [updateSQL, fraLocationInfo.eLocationName.Text]);
    end;

    if (FloadedSampleValues.Values['Vague_Date_Start'] <> IntToStr(Trunc(eDate.StartDate)))
    or (FloadedSampleValues.Values['Vague_Date_End']   <> IntToStr(Trunc(eDate.EndDate)))
    or (FloadedSampleValues.Values['Vague_Date_Type']  <> eDate.DateTypeString) then
    begin
      oldValues.Values['Vague_Date_Start'] := FloadedSampleValues.Values['Vague_Date_Start'];
      oldValues.Values['Vague_Date_End']   := FloadedSampleValues.Values['Vague_Date_End'];
      oldValues.Values['Vague_Date_Type']  := FloadedSampleValues.Values['Vague_Date_Type'];
      newValues.Values['Vague_Date_Start'] := IntToStr(Trunc(eDate.StartDate));
      newValues.Values['Vague_Date_End']   := IntToStr(Trunc(eDate.EndDate));
      newValues.Values['Vague_Date_Type']  := eDate.DateTypeString;
      updateSQL := Format(
          '%s Vague_Date_Start = %d, Vague_Date_End = %d, Vague_Date_Type = ''%s'', ',
          [updateSQL, Trunc(eDate.StartDate), Trunc(eDate.EndDate), eDate.DateTypeString]);
    end;

    cascadeType := dmSample.ValidateCascadeChanges(
        FSurveyEventKey, FSampleKey, oldValues, fraLocationInfo, eDate, cascadeSiblings);

    // Update main sample, if needed.
    if HaveCustody and (updateSQL <> '') then
      dmDatabase.ExecuteSQL(Format(
          'UPDATE Sample SET %s Changed_By = ''%s'', Changed_Date = GetDate() '
          + 'WHERE Sample_Key = ''%s'' ',
          [updateSQL, AppSettings.UserID, FSampleKey]));

    // Go through the taxa in the grid.
    for i := sgSpecies.FixedRows to sgSpecies.RowCount - 1 do
      // Taxon Occ Key if loaded from sample.
      if RowTaxonOccKey[i] <> '' then
        // Remove occurrences that have been deselected.
        if not RowChecked[i] then
          dmTaxonOcc.DeleteRecord(RowTaxonOccKey[i])
        else begin
          // Update occurrences.
          if (RowCustodian[i] = AppSettings.SiteID) then UpdateTaxonOccurrence(i);

          // Update measurements.
          UpdateMeasurements(i, RowTaxonOccKey[i]);
        end;

    // Add any new occurrences. Process as normal card.
    CreateSpeciesOccurrences;

    // Process other samples if required.
    if oldValues.Count > 0 then
      dmSample.CascadeSampleChanges(
          FSurveyEventKey, FSampleKey, oldValues, newValues, cascadeType, cascadeSiblings);

    UpdateObservations(True);
  finally
    newValues.Free;
    oldValues.Free;
    dmTaxonOcc.Free;
    dmSample.Free;
  end;
end;  // UpdateSample

{-------------------------------------------------------------------------------
  Update one existing taxon occurrence record.
}
procedure TfrmPlaceCard.UpdateTaxonOccurrence(row: Integer);
var
  updateSQL, value: String;
  col: Integer;

  // Determine the cell and whether its content has changed.
  function CellChanged(const columnTitle: String): Boolean;
  var oldval: string;
  begin
    // The object is not filled in for cells where the original val is null. Treat these as empty strings
    // for the purposes of detecting changes.
    if (col > -1) and assigned(sgSpecies.Objects[col, row]) then
      oldval:=TCellData(sgSpecies.Objects[col, row]).ItemAdditional
    else
      oldval:='';
    col := ColByName(columnTitle);
    Result :=
        (col > -1)
        and (sgSpecies.Cells[col, row] <> oldval)
  end;

begin
  updateSQL := '';
  if CellChanged(COL_RECORD_TYPE) then begin
    value := KeyOrNull(cmbRecordtype, col, row, NONE_RECORD_KEY);
    updateSQL := Format(' Record_Type_Key = ''%s'', ', [value]);
  end;

  if CellChanged(COL_SUBSTRATE) then begin
    value := KeyOrNull(cmbSubstrate, col, row, NONE_RECORD_KEY);
    updateSQL := Format('%s Substrate_Key = ''%s'',', [updateSQL, value]);
  end;

  if CellChanged(COL_COMMENT) then
    updateSQL := Format(
        '%s Comment = ''%s'',',
        [updateSQL, StringReplace(sgSpecies.Cells[col, row], #39, #39#39, [rfReplaceAll])]);

  if CellChanged(COL_PROVENANCE) then
    updateSQL := Format(
        '%s Provenance = ''%s'',',
        [updateSQL, StringReplace(sgSpecies.Cells[col, row], #39, #39#39, [rfReplaceAll])]);

  // Only update if there is something to do.
  if updateSQL <> '' then begin
    updateSQL := Format(
        'UPDATE Taxon_Occurrence '
        + 'SET %s Changed_By = ''%s'', Changed_Date = GetDate() '
        + 'WHERE Taxon_Occurrence_Key = ''%s''',
        [updateSQL, AppSettings.UserID, RowTaxonOccKey[row]]);
    dmDatabase.ExecuteSQL(updateSQL);
  end;
end;  // UpdateTaxonOccurrence

{-------------------------------------------------------------------------------
  Update/insert/delete taxon occurrence measurements.
}
procedure TfrmPlaceCard.UpdateMeasurements(row: Integer; const taxonOccurrentKey: TKeyString);
var
  col, colCountOf: Integer;
  measurementKeys: TMeasurementKeys;
  accuracy, data: String;

  // Determine whether the cell content was changed.
  function ChangedContent(column: Integer): Boolean;
  begin
    Result :=
        Assigned(sgSpecies.Objects[column, row]) and
        (sgSpecies.Cells[column, row] <> TCellData(sgSpecies.Objects[column, row]).ItemAdditional);
  end;

begin
  try
    for col := 0 to sgSpecies.ColCount - 1 do
    begin
      data := sgSpecies.Cells[col, row];

      if HasColumnName(col)
         and not TColumnName(sgSpecies.Objects[col, 0]).IsStandard then
      begin
        measurementKeys := TColumnName(sgSpecies.Objects[col, 0]).MeasurementKeys;

        // Work out the value to save in the Accuracy field.
        if data <> '' then
          if ValidExactMeasurement(measurementKeys, data) then
            accuracy := ResStr_Exact
          else
          if ValidCircaMeasurement(measurementKeys, data) then begin
            accuracy := SPlaceCard_Circa;
            data     := LowerCase(data);
          end else
            // Anything else falls into Estimate
            accuracy := ResStr_Estimate;

        // Delete measurement if no data in grid.
        if Assigned(sgSpecies.Objects[col, row]) and (data = '') then
          dmDatabase.RunDeleteStoredProc(
              'usp_TaxonOccurrenceData_Delete',
              ['@Key', TCellData(sgSpecies.Objects[col, row]).ItemKey])
        else
        // Insert if no object but some data.
        if not Assigned(sgSpecies.Objects[col, row]) and (data <> '') then
          dmDatabase.RunInsertStoredProc(
              TN_TAXON_OCCURRENCE_DATA,
              'usp_TaxonOccurrenceData_Insert',
              ['@TaxonOccurrenceKey', taxonOccurrentKey,
               '@Data',               data,
               '@Accuracy',           accuracy,
               '@QualifierKey',       measurementKeys.ItemKey,
               '@UnitKey',            measurementKeys.ItemAdditional,
               '@EnteredBy',          AppSettings.UserID],
              '@Key')
        else
        // Update if custodian AND content different from original.
        if IsCellCustodian(col, row) and ChangedContent(col) then
          dmDatabase.RunUpdateStoredProc(
              'usp_TaxonOccurrenceData_Update',
              ['@Key',          TCellData(sgSpecies.Objects[col, row]).ItemKey,
               '@Data',         data,
               '@Accuracy',     accuracy,
               '@QualifierKey', measurementKeys.ItemKey,
               '@UnitKey',      measurementKeys.ItemAdditional,
               '@ChangedBy',    AppSettings.UserID]);
      end
    end;

    // Count/Count Of/Accuracy handled together.
    colCountOf := ColByName(COL_COUNT_OF);
    if colCountOf > -1 then
    begin
      data := '';
      if sgSpecies.Cells[colCountOf, row] <> '' then
        with cmbQualifier.Items do
          data := TKey(Objects[IndexOf(sgSpecies.Cells[colCountOf, row])]).Key;

      // Since Count and Count Of are BOTH either empty or not, check Count Of. Accuracy is different.
      // Delete measurement if no data in grid.
      if Assigned(sgSpecies.Objects[colCountOf, row])
         and (sgSpecies.Cells[colCountOf, row] = '') then
        dmDatabase.RunDeleteStoredProc(
            'usp_TaxonOccurrenceData_Delete',
            ['@Key', TCellData(sgSpecies.Objects[colCountOf, row]).ItemKey])
      else
      // Insert if no object but some data (Accuracy can be null, so no check on that).
      if not Assigned(sgSpecies.Objects[colCountOf, row]) and (sgSpecies.Cells[colCountOf, row] <> '') then
        dmDatabase.RunInsertStoredProc(
            TN_TAXON_OCCURRENCE_DATA,
            'usp_TaxonOccurrenceData_Insert',
            ['@TaxonOccurrenceKey', taxonOccurrentKey,
             '@Data',               CellOrValue(ColByName(COL_COUNT), row, '1'),
             '@Accuracy',           CellOrNull(ColByName(COL_ACCURACY), row),
             '@QualifierKey',       data,
             '@UnitKey',            COUNT_MEASUREMENT_UNIT_KEY,
             '@EnteredBy',          AppSettings.UserID],
            '@Key')
      else
      // Update if custodian AND any content changed from original.
      if IsCellCustodian(colCountOf, row)
         and (ChangedContent(colCountOf)
           or ChangedContent(ColByName(COL_COUNT))
           or ChangedContent(ColByName(COL_ACCURACY))) then
        dmDatabase.RunUpdateStoredProc(
            'usp_TaxonOccurrenceData_Update',
            ['@Key',          TCellData(sgSpecies.Objects[colCountOf, row]).ItemKey,
             '@Data',         CellOrValue(ColByName(COL_COUNT), row, '1'),
             '@Accuracy',     CellOrNull(ColByName(COL_ACCURACY), row),
             '@QualifierKey', data,
             '@UnitKey',      COUNT_MEASUREMENT_UNIT_KEY,
             '@ChangedBy',    AppSettings.UserID]);
    end;
  except
    on E:Exception do
      Raise EDatabaseWriteError.Create(E.Message + ' - Taxon Occurrence Data', E);
  end;
end;  // UpdateMeasurements

{===============================================================================
{ TTaxonHintWindow }
{-------------------------------------------------------------------------------
  Displays the hint, with the name of the column whose data is
  currently being entered displayed in bold text.
}
procedure TTaxonHintWindow.Paint;
var
  r: TRect;
  prefix, boldText, suffix: string;
begin
  r := ClientRect;
  Inc(r.Left, 2);
  Inc(r.Top, 2);
  Canvas.Font.Color := Screen.HintFont.Color;

  prefix := PreviousColumnNames;
  if (Length(prefix) > 0) then DrawHintText([], prefix, r);

  boldText := CurrentColumnName;
  if (Length(boldText) > 0) then DrawHintText([fsBold], boldText, r);

  suffix := FollowingColumnNames;
  if (Length(suffix) > 0) then DrawHintText([], suffix, r);
end;    //  TTaxonHintWindow.Paint

{-------------------------------------------------------------------------------
  Clean up.
}
destructor TTaxonHintWindow.Destroy;
begin
  FHintStrings.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Draws the specified text to the hint window using the font style,
  and increments the rectangle ready for the next string to be written.
}
procedure TTaxonHintWindow.DrawHintText(const FontStyle: TFontStyles;
  const S: string; var R: TRect);
begin
  Canvas.Font.Style := FontStyle;
  DrawText(Canvas.Handle, PChar(S), -1, R, DT_LEFT or DT_NOPREFIX or
    DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
  Inc(R.Left, Canvas.TextWidth(S));
  Canvas.Font.Style := [];
end;

{-------------------------------------------------------------------------------
  Accessor method - sets how many rapid taxon entry column delimiters
  have been entered by the user.
}
procedure TTaxonHintWindow.SetDelimiterCount(const Value: integer);
begin
  if (FDelimiterCount <> Value) then begin
    FDelimiterCount := Value;
    Invalidate;
  end;    // if (FDelimiterCount <> Value)
end;

{-------------------------------------------------------------------------------
  Accessor method - gets the entire list of columns names that can be displayed
    in the hint, depending on how many delimiters have been entered.
}
function TTaxonHintWindow.GetHintStrings: TStringList;
begin
  if (not Assigned(FHintStrings)) then
    FHintStrings := TStringList.Create;
  Result := FHintStrings;
end;

{-------------------------------------------------------------------------------
  Accessor method - gets the actual list of columns names to be displayed
    in the hint, taking into account the number of delimiters entered.
}
function TTaxonHintWindow.GetHintText: string;
var
  i: integer;
begin
  if (DelimiterCount > MAX_PREVIOUS_COLUMNS_IN_HINT) then Result := '...'
  else Result := '';

  for i := DelimiterCount - MAX_PREVIOUS_COLUMNS_IN_HINT to DelimiterCount + MAX_FOLLOWING_COLUMNS_IN_HINT do begin
    if (i >= 0) and (i < HintStrings.Count) then
      if (Length(Result) > 0) then
        Result := Result + AppSettings.RapidEntryDelimiter + HintStrings[i]
      else
        Result := HintStrings[i];
  end;    // for i := DelimiterCount - MAX_PREVIOUS_COLUMNS_IN_HINT to DelimiterCount + MAX_FOLLOWING_COLUMNS_IN_HINT

  if (DelimiterCount < HintStrings.Count - MAX_FOLLOWING_COLUMNS_IN_HINT)
      and (Length(Result) > 0) then
    Result := Result + '...';

  if (Result = '...') then Result := '';
end;

{-------------------------------------------------------------------------------
  Returns a string containing the name of the column whose
    data value is currently being entered.
}
function TTaxonHintWindow.CurrentColumnName: string;
begin
  if (Length(AppSettings.RapidEntryDelimiter) = 0) or (HintStrings.Count <= DelimiterCount) then begin
    Result := '';
  end else begin
    Result := HintStrings[DelimiterCount]
  end;    // if (Length(AppSettings.RapidEntryDelimiter) = 0) or (HintStrings.Count <= DelimiterCount)
end;

{-------------------------------------------------------------------------------
  Returns a string containing the names of any columns still
    awaiting data values, separated by the delimiter.
}
function TTaxonHintWindow.FollowingColumnNames: string;
var
  i: integer;
begin
  if (Length(AppSettings.RapidEntryDelimiter) = 0) then begin
    Result := '';
  end else begin
    for i := DelimiterCount + 1 to DelimiterCount + MAX_FOLLOWING_COLUMNS_IN_HINT do
      if (i < HintStrings.Count) then
        Result := Result + AppSettings.RapidEntryDelimiter + HintStrings[i];
    if (DelimiterCount < Pred(HintStrings.Count) - MAX_FOLLOWING_COLUMNS_IN_HINT) then
      Result := Result + '...';
  end;    // if (Length(AppSettings.RapidEntryDelimiter) = 0)
end;

{-------------------------------------------------------------------------------
  Returns a string containing the names of any columns with
    data values, separated by the delimiter.
}
function TTaxonHintWindow.PreviousColumnNames: string;
var
  i: integer;
begin
  if (Length(AppSettings.RapidEntryDelimiter) = 0) then begin
    Result := '';
  end else begin
    if (DelimiterCount > MAX_PREVIOUS_COLUMNS_IN_HINT)
      and (HintStrings.Count > MAX_PREVIOUS_COLUMNS_IN_HINT + MAX_FOLLOWING_COLUMNS_IN_HINT + 1) then
      Result := '...';
    for i := DelimiterCount - MAX_PREVIOUS_COLUMNS_IN_HINT to Pred(DelimiterCount) do
      if (i >= 0) then begin
        if (i < Pred(HintStrings.Count)) then
          Result := Result + HintStrings[i] + AppSettings.RapidEntryDelimiter
        else if (i = Pred(HintStrings.Count)) then
          Result := Result + HintStrings[i];
      end;    // if (i >= 0)
  end;    // if (Length(AppSettings.RapidEntryDelimiter) = 0)
end;

procedure TfrmPlaceCard.pnlHeaderResize(Sender: TObject);
var
  availableWidth, halfWidth: Integer;
begin
  inherited;

  if Self.Width > 400 then
  begin
    // Sets the size of the two frames
    availableWidth := pnlHeader.Width - 8 * 3;
    halfWidth := availableWidth div 2;
    pnlLeft.Width := halfWidth;
    pnlRight.Left := pnlLeft.Left + halfWidth + 8;
    pnlRight.Width := halfWidth;
  end;
end;

(*==============================================================================
 * Trap mouse up on the grid to detect any column width changes. This lets us
 * only offer a save message when appropriate.
 *)
procedure TfrmPlaceCard.sgSpeciesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if TAccessorClass(sgSpecies).FGridState=gsColSizing then
    FColsResized := true;
end;

end.
