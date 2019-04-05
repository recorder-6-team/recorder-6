//==============================================================================
//  Unit:        LocationDetails
//
//  Implements:  TfrmLocationDetails
//
//  Description: This form is docked on the Locations screen to allow details
//               of a selected location to be viewed and/or edited.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 281 $
//    $Date: 8/04/10 16:53 $
//    $Author: Andrewkemp $
//
//==============================================================================

{$I '..\..\Third Party\Dorset Software Services\DssVcl32\DelphiVersions.Inc'}

unit LocationDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseFormUnit, StdCtrls, ComCtrls, ExtCtrls, Buttons, Mask, Grids, Menus, Math,
  ActnList, VagueDateEdit, CompositeComponent, Measurements, Sources, Finder,
  CheckLst, LocationDetailsData, Db, DataClasses, VagueDate, GeneralFunctions,
  DBCtrls, DBListCombo, DropTarget, ExceptionForm, SpatialRef, BaseDockedForm,
  SpatialRefFuncs, TextInput, ValidationData, OnlineHelp, Find, Constants, Contnrs,
  GridSquareItem, ImageListButton, DatabaseAccessADO, FormActions, Variants,
  ToolWin, ADODb, GridSquareExtractor, Recorder2000_TLB,
  AddinCompositeComponent, AddinLinkedControls;

resourcestring
  ResStr_SiteNotVisited = 'Site not visited';
  ResStr_SiteStatus = 'Site is active';
  ResStr_SiteStatusNot = 'Site is not active';
  ResStr_ConfirmObservationSpatialRefUpdate =
      'Do you want to update all existing observations attached to this location ' +
      'and spatial reference to the new spatial reference?';
  ResStr_NoLocationName       = 'No Location Name selected. At least one must be selected.';
  ResStr_LocationTypeRequired = 'A Location Type is required for every location.';
  ResStr_IncludeSpatialRef    = 'The details must include a central spatial reference.';
  ResStr_EnterSRQualifier  =
      'A spatial reference qualifier must be entered to support the spatial reference.';
  ResStr_InvalidSpatialRef =
      'The Spatial Reference must be within the grid squares of the parent site.';

  ResStr_InfoMissing =
      'Some information is missing or has been duplicated. '
      + 'Enter both the Square and its Type, and make sure the Square is unique.';

  ResStr_MustContainSpecifiedSR = 'At least one square must contain the specified Spatial Reference.';
  ResStr_GridSquareWithinParent = 'All grid squares must be within the grid squares of the parent site.';
  ResStr_InfoMissingNumAndMap   = 'Some information is missing. Enter both the Number and the Map System.';
  ResStr_LocationNameExists     = 'This Location Name already exists in the list.';
  ResStr_DeleteDesignation      = 'Are you sure you want to delete this Designation?';
  ResStr_EndBeforeStartDate     = 'The End Date cannot come before the Start Date.';
  ResStr_DesignationRequired    = 'A Site is required for every Designation. Select a value from the list.';
  ResStr_AuthorityNameRequired  = 'An Authority Name is required for every Designation.';
  ResStr_InvalidAuthorityName   = 'The Authority Name is invalid. Enter a valid name.';
  ResStr_InvalidGridSquare      = 'Are you sure you want to delete this Grid Square?';
  ResStr_DeleteLandParcel       = 'Are you sure you want to delete this Land Parcel?';
  ResStr_DeleteBoundary         = 'Are you sure you want to delete this Boundary?';
  ResStr_VersionNumMissing      = 'The Version number is missing. Enter a valid number.';
  ResStr_InvalidVersionNumber   = 'The Version number is invalid. Enter a valid number.';
  ResStr_IfFileSetObjectIDSet   = 'If a map file is selected, an Object ID must also be selected.';
  ResStr_VersionNumLessThan     = 'The Version number must be less than 32768.';
  ResStr_InvalidVagueDate       = 'The vague date you have entered is not valid';
  ResStr_StartContainEndDate    = 'The Start Date cannot contain the End Date.';
  ResStr_EndContainStartDate    = 'The End Date cannot contain the Start Date.';
  ResStr_NoBoundarySelected     = 'No boundary was selected from the map browser screen.';
  ResStr_DeleteUse              = 'Are you sure you want to delete this Use?';
  ResStr_UseNameRequired        = 'A Use Name is required for every Use.';
  ResStr_StartDateRequired      = 'A Start Date is required for every Use.';
  ResStr_DeleteTenure           = 'Are you sure you want to delete this Tenure?';
  ResStr_TenureNameRequired     = 'A Tenure Name is required for every Tenure.';
  ResStr_InvalidTenureName      = 'The Tenure Name is invalid. Enter a valid Name.';

  ResStr_TenureTypeRequired =
      'A Tenure Type is required for every Tenure. Enter a type or select one from the list.';

  ResStr_StartdateRequiredForTenure = 'A Start Date is required for every Tenure.';
  ResStr_SRSystemNotRecognised      =  'The spatial reference is not recognised.';

  ResStr_LocationNames  = 'Location Names';
  ResStr_LocationName   = 'Location Name:';

  ResStr_DeleteName     = 'Are you sure you want to delete this Name?';
  ResStr_DeleteAdmin    = 'Are you sure you want to delete this Administrative Area?';
  ResStr_DeleteRelation = 'Are you sure you want to delete this Relation?';
  ResStr_InvalidCaller  =  'Internal error - invalid caller to extract grid squares';
  ResStr_LinkedTo       = 'Key field is %s';
  ResStr_ObjectIDNotExist = 'Object is not present in the map file.'#13#10#13#10
                          + 'This data has still been saved.';
  ResStr_MapFileNotFound = 'Map File does not exist.'#13#10#13#10
                          + 'This data has still been saved.';

type
  ELocationDetailsError = class(TExceptionPath);

  TfrmLocationDetails = class(TfrmBaseDockedForm)
    dlgOpen : TOpenDialog;
    pmMapWindow: TPopupMenu;
    pmMapWindowForBoundary: TPopupMenu;
    pnlDetails: TPanel;
    pnlInner: TPanel;
    bbSave: TImageListButton;
    bbCancel: TImageListButton;
    pcLocationDetails: TPageControl;
    tsGeneral: TTabSheet;
    Bevel1: TBevel;
    Label3: TLabel;
    Label4: TLabel;
    lblPreferredNamePrompt: TLabel;
    Label22: TLabel;
    Label13: TLabel;
    lblPreferredName: TLabel;
    lblLocationNames: TLabel;
    Label7: TLabel;
    lblLastSurveyDate: TLabel;
    SpatialRef: TSpatialRef;
    clbLocationNames: TCheckListBox;
    dbcmbLocationType: TDBLookupComboBox;
    dbeFileCode: TDBEdit;
    dbreLocDescription: TDBRichEdit;
    bbLocNameAdd: TImageListButton;
    bbLocNameEdit: TImageListButton;
    bbLocNameDel: TImageListButton;
    tsDesignations: TTabSheet;
    tsMeasurements: TTabSheet;
    Measurements: TMeasurements;
    tsGeoInfo: TTabSheet;
    pcGeoInfo: TPageControl;
    tsAdminAreas: TTabSheet;
    Shape3: TShape;
    sgAdminAreas: TStringGrid;
    bbAdminDel: TImageListButton;
    bbAdminAdd: TImageListButton;
    tsGridSquares: TTabSheet;
    pnlExtractingGridSquares: TPanel;
    sgGridSquares: TStringGrid;
    cmbSquareType: TComboBox;
    bbSquareAdd: TImageListButton;
    bbSquareDel: TImageListButton;
    btnExtractGridSquares: TImageListButton;
    tsLandParcel: TTabSheet;
    sgLandParcels: TStringGrid;
    bbLandDel: TImageListButton;
    bbLandAdd: TImageListButton;
    tsBoundaries: TTabSheet;
    sgBoundaries: TStringGrid;
    gbBoundaryDetails: TGroupBox;
    Label21: TLabel;
    Label20: TLabel;
    Label5: TLabel;
    Label2: TLabel;
    Label14: TLabel;
    eBoundaryVersion: TEdit;
    eBoundaryFrom: TVagueDateEdit;
    eBoundaryTo: TVagueDateEdit;
    bbBoundaryAccept: TImageListButton;
    bbBoundaryDiscard: TImageListButton;
    btnGetGISObject: TImageListButton;
    btnGetGISObjectDropDown: TButton;
    bbBoundaryAdd: TImageListButton;
    bbBoundaryEdit: TImageListButton;
    bbBoundaryDel: TImageListButton;
    tsOtherInfo: TTabSheet;
    pcOtherInfo: TPageControl;
    tsRelations: TTabSheet;
    Shape2: TShape;
    sgRelations: TStringGrid;
    cmbRelation: TComboBox;
    bbRelationAdd: TImageListButton;
    bbRelationDel: TImageListButton;
    tsUses: TTabSheet;
    tsTenure: TTabSheet;
    sgTenure: TStringGrid;
    gbTenureDetails: TGroupBox;
    Label50: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label25: TLabel;
    eTenureFrom: TVagueDateEdit;
    eTenureTo: TVagueDateEdit;
    cmbTenureType: TDBListCombo;
    bbTenureAccept: TImageListButton;
    bbTenureDiscard: TImageListButton;
    eTenureName: TNameLinkedEdit;
    bbTenureAdd: TImageListButton;
    bbTenureEdit: TImageListButton;
    bbTenureDel: TImageListButton;
    tsApproach: TTabSheet;
    lblAccessRestrict: TLabel;
    lblHowToGet: TLabel;
    dbreApproach: TDBRichEdit;
    dbreRestrictions: TDBRichEdit;
    tsSources: TTabSheet;
    Sources: TSources;
    lblLocationDisp: TLabel;
    pnlDesignationsTop: TPanel;
    bbDesignationAdd: TImageListButton;
    sgDesignations: TStringGrid;
    bbDesignationEdit: TImageListButton;
    bbDesignationDel: TImageListButton;
    splDesignations: TSplitter;
    pnlDesignationsBottom: TPanel;
    gbDesignationDetails: TGroupBox;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label1: TLabel;
    eDesRefCode: TEdit;
    reDesignComments: TRichEdit;
    dbcmbSiteStatus: TDBListCombo;
    eDesignationFrom: TEdit;
    eDesignationTo: TEdit;
    bbDesignationDiscard: TImageListButton;
    bbDesignationAccept: TImageListButton;
    eDesAuthority: TNameLinkedEdit;
    pnlUsesTop: TPanel;
    sgUses: TStringGrid;
    bbUseAdd: TImageListButton;
    bbUseEdit: TImageListButton;
    bbUseDel: TImageListButton;
    splUses: TSplitter;
    pnlUsesBottom: TPanel;
    gbUseDetails: TGroupBox;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    lblUsesComments: TLabel;
    eUse: TEdit;
    eUseFrom: TVagueDateEdit;
    eUseTo: TVagueDateEdit;
    reUsePotential: TRichEdit;
    reUseComments: TRichEdit;
    bbUseAccept: TImageListButton;
    bbUseDiscard: TImageListButton;
    cmbMapFile: TComboBox;
    btnMapFileBrowse: TButton;
    cmbGISObjectID: TComboBox;
    lblLinkedField: TLabel;
    lblStatus: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboBoxExit(Sender: TObject);
    procedure btnGetGISObjectClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure bbLandAddClick(Sender: TObject);
    procedure bbLandDelClick(Sender: TObject);
    procedure bbSquareAddClick(Sender: TObject);
    procedure bbSquareDelClick(Sender: TObject);
    procedure sgDesignationsClick(Sender: TObject);
    procedure bbDesignationAddClick(Sender: TObject);
    procedure bbDesignationEditClick(Sender: TObject);
    procedure bbDesignationDelClick(Sender: TObject);
    procedure bbDesignationAcceptClick(Sender: TObject);
    procedure bbDesignationDiscardClick(Sender: TObject);
    procedure bbAdminAddClick(Sender: TObject);
    procedure bbAdminDelClick(Sender: TObject);
    procedure sgGridSquaresDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sgRelationsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure cmbSquareTypeChange(Sender: TObject);
    procedure sgRelationsClick(Sender: TObject);
    procedure sgRelationsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure bbRelationAddClick(Sender: TObject);
    procedure bbRelationDelClick(Sender: TObject);
    procedure cmbRelationChange(Sender: TObject);
    procedure EnterRTF(Sender: TObject);
    procedure ExitRTF(Sender: TObject);
    procedure SpatialRefGetFromMap(Sender: TObject);
    procedure pcDetailsChanging(Sender: TObject; var AllowChange: Boolean);
    procedure clbLocationNamesClickCheck(Sender: TObject);
    procedure bbLocNameAddClick(Sender: TObject);
    procedure bbLocNameEditClick(Sender: TObject);
    procedure bbLocNameDelClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure sgBoundariesClick(Sender: TObject);
    procedure bbBoundaryAddClick(Sender: TObject);
    procedure bbBoundaryEditClick(Sender: TObject);
    procedure bbBoundaryDelClick(Sender: TObject);
    procedure bbBoundaryAcceptClick(Sender: TObject);
    procedure bbBoundaryDiscardClick(Sender: TObject);
    procedure sgUsesClick(Sender: TObject);
    procedure bbUseAddClick(Sender: TObject);
    procedure bbUseEditClick(Sender: TObject);
    procedure bbUseDelClick(Sender: TObject);
    procedure bbUseAcceptClick(Sender: TObject);
    procedure bbUseDiscardClick(Sender: TObject);
    procedure sgTenureClick(Sender: TObject);
    procedure bbTenureAddClick(Sender: TObject);
    procedure bbTenureEditClick(Sender: TObject);
    procedure bbTenureDelClick(Sender: TObject);
    procedure bbTenureAcceptClick(Sender: TObject);
    procedure bbTenureDiscardClick(Sender: TObject);
    procedure sgGridSquaresClick(Sender: TObject);
    procedure sgLandParcelsClick(Sender: TObject);
    procedure sgLandParcelsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure pnlDetailsResize(Sender: TObject);
    procedure DrawCellChoppedText(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure eDesignationFromExit(Sender: TObject);
    procedure eDesignationToExit(Sender: TObject);
    procedure eBoundaryFromExit(Sender: TObject);
    procedure eBoundaryToExit(Sender: TObject);
    procedure eUseFromExit(Sender: TObject);
    procedure eUseToExit(Sender: TObject);
    procedure eTenureFromExit(Sender: TObject);
    procedure eTenureToExit(Sender: TObject);
    procedure SpatialRefExit(Sender: TObject);
    procedure pcLocationDetailsChange(Sender: TObject);
    procedure pcOtherInfoChange(Sender: TObject);
    procedure pcGeoInfoChange(Sender: TObject);
    procedure clbLocationNamesClick(Sender: TObject);
    procedure sgGridSquaresSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
    procedure sgAdminAreasClick(Sender: TObject);
    procedure btnGetGISObjectDropDownClick(Sender: TObject);
    procedure SpatialRefInvalidSpatialRef(Sender: TObject; var Handled: Boolean);
    procedure btnExtractGridSquaresClick(Sender: TObject);
    procedure eTenureNameGetData(Sender: TObject);
    procedure eTenureNameFindData(Sender: TObject);
    procedure eDesAuthorityFindData(Sender: TObject);
    procedure eDesAuthorityGetData(Sender: TObject);
    procedure tsApproachShow(Sender: TObject);
    procedure pnlUsesBottomResize(Sender: TObject);
    procedure btnMapFileBrowseClick(Sender: TObject);
    procedure cmbMapFileChange(Sender: TObject);
  private
    FdmLocationDetails : TdmLocationDetails;
    FDrillForm : TBaseForm;
    FLocNameList : TLocationNameList;
    FDesignationList : TDesignationList;
    FCurrentDes : TDesignationItem;
    FAdminAreaList : TAdminAreaList;
    FGridSquareList : TGridSquareList;
    FLandParcelList : TLandParcelList;
    FBoundaryList : TBoundaryList;
    FCurrentBoundary : TBoundaryItem;
    FMapSheetKey: String;
    FRelationList : TRelationList;
    FUseList : TUseList;
    FCurrentUse : TUseItem;
    FTenureList : TTenureList;
    FCurrentTenure : TTenureItem;
    FAddItem : Boolean;
    FDeletingItem : Boolean;
    FClosingForm : Boolean;
    FLocationKey : TKeyString;
    FParentKey   : TKeyString;
    FNearGridRefs : TObjectList;
    FCheckedIndex : Integer;
    FGridSquareSR : String;
    FRow          : Integer;
    FExpectingSpatialRef: Boolean;
    FGridSquareExtractor: TGridSquareExtractor;
    FOrigEnteredRef: string;
    FGridSquareInfoList: TObjectList;
    FLastFilePath: string;
    FObjectIDExists: boolean;
    FFileExists: boolean;
    procedure RemoveExternalFile;
    procedure EnableDetails(const NewMode:Constants.TEditMode);
    procedure SetDrillForm(const Value: TBaseForm);
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
    procedure FreeObjects;
    procedure SetupObjects;
    procedure RefreshLocationNames;
    procedure BlankDesignation;
    procedure SaveDesignation;
    procedure DropAuthorityName(const Sender: TObject;
      const iFormat: Integer; const iSourceData: TKeyList;
      const iTextStrings: TStringList; var ioHandled: Boolean);
    procedure DropAdminArea(const Sender: TObject;
      const iFormat : Integer; const iSourceData: TKeyList;
      const iTextStrings : TStringList; var ioHandled : Boolean);
    procedure DropLocation(const Sender: TObject;
      const iFormat : Integer; const iSourceData: TKeyList;
      const iTextStrings : TstringList; var ioHandled : Boolean);
    procedure DropRelation(const Sender: TObject;
      const iFormat : Integer; const iSourceData: TKeyList;
      const iTextStrings : TStringList; var ioHandled : Boolean);
    procedure DropTenure(const Sender: TObject;
      const iFormat : Integer; const iSourceData: TKeyList;
      const iTextStrings : TStringList; var ioHandled : Boolean);
    procedure BlankBoundary;
    procedure SaveBoundary;
    procedure BlankUse;
    procedure SaveUse;
    procedure BlankTenure;
    procedure SaveTenure;
    procedure UpdateDesAuthority(KeyList: TKeyList);
    procedure UpdateTenure(KeyList: TKeyList);
    procedure UpdateAdminArea(KeyList: TKeyList);
    procedure UpdateBoundary(KeyList: TKeyList);
    procedure SetDesignationColour(const tfDetailsOn: Boolean);
    procedure SetBoundaryColour(const tfDetailsOn: Boolean);
    procedure SetUseColour(const tfDetailsOn: Boolean);
    procedure SetTenureColour(const tfDetailsOn: Boolean);
    function CheckGridSquares: Boolean;
    function CheckRefInParent: Boolean;
    function CheckRefInSquares:Boolean;
    function CheckSquaresInParent: Boolean;
    function CheckLandParcels: Boolean;
    function CheckExistence(AGrid:TStringGrid; const AKey: TKeyString): Boolean;
    procedure MapForGISObjectClick(Sender: TObject);
    procedure UpdateSpatialRef(KeyList: TKeyList);
    procedure Populate2KMList(iLatLong: TLatLong);
    function ValidateGridSquareSR(const iColumn, iRow : Integer) : String;
    procedure ValidateDesignationFromDate;
    procedure ValidateDesignationToDate;
    procedure ValidateBoundaryFromDate;
    procedure ValidateBoundaryToDate;
    procedure ValidateUseFromDate;
    procedure ValidateUseToDate;
    procedure ValidateTenureFromDate;
    procedure ValidateTenureToDate;
    procedure SetLocationNameButtons;
    function BoundaryMapFileName: String;
    property GridSquareSR: String read FGridSquareSR write FGridSquareSR;
    function CheckExistingObservations: boolean;
    procedure MapForSpatialRefClick(Sender: TObject);
    procedure WMUpdateSurveyDate(var AMessage: TMessage); message WM_UPDATE_SURVEY_DATE;
    function  GetLocationStatus: Integer;
    procedure InitialiseGridSquareInfoList;
    procedure GridSquareItemChange(Sender: TObject; Item: TDataItem; Status: TDataItemStatus);
    procedure SetPanelSize;
    procedure ResizeApproach;
    procedure RefreshMapFileCombo;
    procedure RefreshObjectIDCombo(mapSheet: TMapSheet);
    function GetMapSheetIndex(comboBox: TComboBox; value: TMapSheet): integer;
    procedure ClearCombo(comboBox: TComboBox);
    procedure SetcmbGISObjectID(objectID: string);
    procedure RefreshLabel;
  protected
    procedure SetupDestinationControls; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure AddBoundary(AMapSheetKey, AObjectStaticID: String);
    function GetKeyList: TKeyList; override;
    procedure UpdateRTFMenu; override;
    procedure AddRecord(const ALocDetKey:TKeyString);
    procedure EditRecord;
    procedure DisplayRecord(const ALocDetKey: TKeyString);
    function DeleteRecord(const ALocDetKey: TKeyString): Boolean;
    procedure RefreshLists;
    procedure SetDisplaySystem;
    procedure RefreshNames;
    procedure RefreshColours;
    procedure RefreshBoundaries;
    procedure ShowMetaData; override;
    procedure FindOnMap; override;
    procedure UpdateMapWindowSelector; override;
    property DrillForm:TBaseForm read FDrillForm write SetDrillForm;
    property LocationKey:TKeyString read FLocationKey write FLocationKey;
    property ParentKey:TKeyString read FParentKey write FParentKey;
    property NearGridRefs: TObjectList read FNearGridRefs write FNearGridRefs;
    property ExpectingSpatialRef: Boolean read FExpectingSpatialRef;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  Maintbar, Locations, Map, GeneralData, ApplicationSettings, ExtractGridSquares,
  Search, MapData, MetaDataPopup, JNCCDataSets, ComAddinUnit, BoundaryFieldSelect,
  MapServerLink, ms5;

const
  BOUNDARY_VERSION = '1';

//==============================================================================
constructor TfrmLocationDetails.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetupObjects;
  SetDisplaySystem;

  dlgOpen.InitialDir := AppSettings.MapFilePath;
  FLastFilePath      := AppSettings.MapFilePath;

  clbLocationNames.Clear;
  SetGridColumnTitles(sgDesignations, [ResStr_Designation, ResStr_Authority, ResStr_From, ResStr_To]);
  SetGridColumnTitles(sgBoundaries,   [ResStr_Version, ResStr_From, ResStr_To]);
  SetGridColumnTitles(sgLandParcels,  [ResStr_Number, ResStr_MapSystem]);
  SetGridColumnTitles(sgGridSquares,  [ResStr_Square, ResStr_Type]);
  SetGridColumnTitles(sgAdminAreas,   [ResStr_Name, ResStr_Type]);
  SetGridColumnTitles(sgRelations,    [ResStr_RelatedWith, ResStr_Relation]);
  SetGridColumnTitles(sgUses,         [ResStr_Use, ResStr_From, ResStr_To]);
  SetGridColumnTitles(sgTenure,       [ResStr_Name, ResStr_Type, ResStr_From, ResStr_To]);

  SwitchToDetails(sgDesignations, bbDesignationAdd, bbDesignationEdit, bbDesignationDel,
                  bbSave, bbCancel, gbDesignationDetails, False);
  SwitchToDetails(sgBoundaries, bbBoundaryAdd, bbBoundaryEdit, bbBoundaryDel,
                  bbSave, bbCancel, gbBoundaryDetails, False);
  SwitchToDetails(sgUses, bbUseAdd, bbUseEdit, bbUseDel,
                  bbSave, bbCancel, gbUseDetails, False);
  SwitchToDetails(sgTenure, bbTenureAdd, bbTenureEdit, bbTenureDel,
                  bbSave, bbCancel, gbTenureDetails, False);
  // Set pages
  SetFirstPage(pcGeoInfo);
  SetFirstPage(pcOtherInfo);
  SetFirstPage(pcLocationDetails);

  FDeletingItem:=False;
  EnableDetails(emView);

  //Help Setup
  Self.HelpContext           := IDH_LOCATION;
  tsGeneral.HelpContext      := IDH_LOCATIONGENERAL;
  tsDesignations.HelpContext := IDH_LOCATIONDESIG;
  tsMeasurements.HelpContext := IDH_LOCATIONMEASURES;
  tsGeoInfo.HelpContext      := IDH_LOCATIONAAS;
  tsOtherInfo.HelpContext    := IDH_LOCATIONRELATION;
  tsSources.HelpContext      := IDH_LOCATIONSOURCES;
  tsAdminAreas.HelpContext   := IDH_LOCATIONAAS;
  tsGridSquares.HelpContext  := IDH_LOCATIONGRIDSQUS;
  tsLandParcel.HelpContext   := IDH_LOCATIONLANDP;
  tsBoundaries.HelpContext   := IDH_LOCATIONBOUNDS;
  tsRelations.HelpContext    := IDH_LOCATIONRELATION;
  tsUses.HelpContext         := IDH_LOCATIONUSES;
  tsTenure.HelpContext       := IDH_LOCATIONTENURE;
  tsApproach.HelpContext     := IDH_LOCATIONAPPROACH;
  pcLocationDetails.HelpContext := IDH_LOCATIONGENERAL;
  pcGeoInfo.HelpContext      := IDH_LOCATIONAAS;
  pcOtherInfo.HelpContext    := IDH_LOCATIONRELATION;

  RefreshMapFileCombo;
end;  // Create

//==============================================================================
procedure TfrmLocationDetails.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Action:=caFree;

  ClearCombo(cmbMapFile);
end;  // FormClose

//==============================================================================
procedure TfrmLocationDetails.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  CanClose := False;
  if EditMode <> emView then begin
    Beep;
    case ConfirmSaveAndClose of
      mrYes : begin
                if bbDesignationAccept.Enabled then bbDesignationAcceptClick(nil) else
                if bbBoundaryAccept.Enabled    then bbBoundaryAcceptClick(nil)    else
                if bbUseAccept.Enabled         then bbUseAcceptClick(nil)         else
                if bbTenureAccept.Enabled      then bbTenureAcceptClick(nil);
                bbSaveClick(nil);
                CanClose := True;
              end;
      mrNo  : begin
                FClosingForm:=True;
                if bbDesignationDiscard.Enabled then bbDesignationDiscardClick(nil) else
                if bbBoundaryDiscard.Enabled    then bbBoundaryDiscardClick(nil)    else
                if bbUseDiscard.Enabled         then bbUseDiscardClick(nil)         else
                if bbTenureDiscard.Enabled      then bbTenureDiscardClick(nil);
                bbCancelClick(nil);
                CanClose     := True;
                FClosingForm := False;
              end;
    end;
  end else
    CanClose := True;
end;  // FormCloseQuery

//==============================================================================
destructor TfrmLocationDetails.Destroy;
begin
  FreeObjects;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TfrmLocationDetails.SetupObjects;
begin
  // Data Module
  FdmLocationDetails:=TdmLocationDetails.Create(nil);
  // Location Name List
  FLocNameList:=TLocationNameList.Create(FdmLocationDetails.qryLocNames,'Location_Name_Key',
                                         clbLocationNames.Items,TLocationNameItem);
  // Designation
  FDesignationList:=TDesignationList.Create(FdmLocationDetails.qryDesignation,
                                            'Designation_Key',sgDesignations,TDesignationItem);
  // Setup Measurement properties
  Measurements.Init(dmDatabase.LocalDatabase, TN_LOCATION_DATA,
                    'Location_Data_Key', 'Location_Key',
                    AppSettings.UserID, AppSettings.UserAccessLevel,
                    dmGeneralData.GetNextKey,
                    AppSettings.SiteID, AppSettings.RestrictFullEdit);
  //Geo Info
  FGridSquareInfoList := TObjectList.Create();
  InitialiseGridSquareInfoList;
  FAdminAreaList := TAdminAreaList.Create(FdmLocationDetails.qryAdminAreas, 'ADMIN_AREA_KEY', sgAdminAreas, TAdminAreaItem);
  FGridSquareList:= TGridSquareList.Create(FdmLocationDetails.qryGridSquares, 'GRID_SQUARE_KEY', sgGridSquares, TGridSquareItem);
  FGridSquareList.OnDataItemChange := GridSquareItemChange;
  FLandParcelList:= TLandParcelList.Create(FdmLocationDetails.qryLandParcels, 'LAND_PARCEL_KEY', sgLandParcels, TLandParcelItem);
  FBoundaryList  := TBoundaryList.Create(FdmLocationDetails.qryBoundaries, 'LOCATION_BOUNDARY_KEY', sgBoundaries, TBoundaryItem);

  //Other Info
  FRelationList:= TRelationList.Create(FdmLocationDetails.qryRelations, 'LOCATION_RELATION_KEY', sgRelations, TRelationItem);
  FUseList     := TUseList.Create(FdmLocationDetails.qryUses, 'LOCATION_USE_KEY', sgUses, TUseItem);
  FTenureList  := TTenureList.Create(FdmLocationDetails.qryTenures, 'TENURE_KEY', sgTenure, TTenureItem);

  // Sources
  Sources.Init(dmDatabase.LocalDatabase, 'Location_Sources',
               AppSettings.UserAccessLevel, dmGeneralData.GetNextKey,
               RegisterDropComponent, AppSettings.SiteID, AppSettings.ExternalFilePath);
  Sources.OnFindInternal:=dmGeneralData.SourcesFindInternal;
  Sources.OnAddInternal :=dmGeneralData.SourcesAddInternal;
  Sources.OnShowSource :=dmGeneralData.SourcesShowInternal;

  cmbTenureType.Active:= True;

  // Near Grid Refs
  FNearGridRefs := TObjectList.Create;
end;  // SetupObjects

//==============================================================================
procedure TfrmLocationDetails.FreeObjects;
begin
  FreeAndNil(FLocNameList);
  FreeAndNil(FDesignationList);
  FreeAndNil(FAdminAreaList);
  FreeAndNil(FGridSquareList);
  FreeAndNil(FBoundaryList);
  FreeAndNil(FRelationList);
  FreeAndNil(FUseList);
  FreeAndNil(FTenureList);
  FreeAndNil(FLandParcelList);
  // Data Module
  FreeAndNil(FdmLocationDetails);
  // Near Grid Ref List
  FreeAndNil(FNearGridRefs);

  FreeAndNil(FGridSquareInfoList);
end;  // FreeObjects

//==============================================================================
procedure TfrmLocationDetails.DisplayRecord(const ALocDetKey:TKeyString);
var lCursor : TCursor;
begin
  FLocationKey:= ALocDetKey;
  lCursor:=HourglassCursor;
  try
    with FdmLocationDetails do begin
      // There seems to be a bug when filtering the Locations,
      // the datasources turn to NIL. Hence the simple fix below.
      dbeFileCode.DataSource      :=dsLocation;
      dbcmbLocationType.DataSource:=dsLocation;
      dbcmbLocationType.ListSource:=dsLocationType;

      with qryLocation do begin
        Close;
        ResetDBRichEditControls([dbreLocDescription, dbreApproach, dbreRestrictions]);
        Parameters.ParamByName('KeyParameter').Value:=ALocDetKey;
        Open;
        FCustodian := FieldByName('Custodian').AsString;
        SpatialRef.Qualifier    := FieldByName('Spatial_Ref_Qualifier').AsString;
        SpatialRef.EnteredRef   := FieldByName('Spatial_Ref').AsString;
        FOrigEnteredRef         := SpatialRef.EnteredRef;
        SpatialRef.EnteredSystem:= FieldByName('Spatial_Ref_System').AsString;
        SpatialRef.DisplayRef := GetDisplaySpatialRef(
            AppSettings.SpatialRefSystem,
            FieldByName('Spatial_Ref').AsString,
            FieldByName('Spatial_Ref_System').AsString,
            FieldByName('Lat').Value,
            FieldByName('Long').Value,
            '');
      end;
      // Last survey date - async label
      lblLastSurveyDate.Caption := '';

      // Location Names
      qryLocNames.Parameters.ParamByName('KeyParameter').Value:=ALocDetKey;
      FLocNameList.Refresh;
      RefreshLocationNames;
      GetLocationStatus;
      // Designation
      dbcmbSiteStatus.Active:=True;
      qryDesignation.Parameters.ParamByName('KeyParameter').Value:=ALocDetKey;
      FDesignationList.Refresh;
      sgDesignationsClick(nil);

      // Measurements
      Measurements.KeyValue:=ALocDetKey;
      Measurements.Refresh;

      //Geo Info
      qryAdminAreas.Parameters.ParamByName('Key').Value:= ALocDetKey;
      FAdminAreaList.LocationKey:= ALocDetKey;
      FAdminAreaList.Refresh;

      qryGridSquares.Parameters.ParamByName('Key').Value:= ALocDetKey;
      FGridSquareList.LocationKey:= ALocDetKey;
      FGridSquareList.Refresh;

      qryLandParcels.Parameters.ParamByName('Key').Value:= ALocDetKey;
      FLandParcelList.LocationKey:= ALocDetKey;
      FLandParcelList.Refresh;

      qryBoundaries.Parameters.ParamByName('Key').Value:= ALocDetKey;
      FBoundaryList.LocationKey:= ALocDetKey;
      FBoundaryList.Refresh;
      sgBoundariesClick(Self);

      //Other Info
      qryRelations.Parameters.ParamByName('Key').Value:= ALocDetKey;
      FRelationList.LocationKey:= ALocDetKey;
      FRelationList.Refresh;

      qryUses.Parameters.ParamByName('Key').Value:= ALocDetKey;
      FUseList.LocationKey:= ALocDetKey;
      FUseList.Refresh;
      sgUsesClick(Self);

      qryTenures.Parameters.ParamByName('Key').Value:= ALocDetKey;
      FTenureList.LocationKey:= ALocDetKey;
      FTenureList.Refresh;
      sgTenureClick(Self);
    end;
    // Sources
    Sources.SourcesFromKey:=ALocDetKey;
    Sources.RefreshLists;
    // Additional Pages
    ChangeAdditionalPage(pcLocationDetails);
    // Notify COM Addins
    NotifyDataItemChange;
    PostMessage(Self.Handle, WM_UPDATE_SURVEY_DATE, 0, 0);
  finally
    // The query may not be open when reviewing a batch update. Make sure it is.
    FdmLocationDetails.qryLocationType.Close;
    FdmLocationDetails.qryLocationType.Open;
    // Must be called or else the box does not update on filter or show all
    dbcmbLocationType.Repaint;
    DefaultCursor(lCursor);
  end;
end;  // DisplayRecord

{-------------------------------------------------------------------------------
  Populate the last survey date field synch.
}
procedure TfrmLocationDetails.WMUpdateSurveyDate(var AMessage: TMessage);
var
  vagueDate : TVagueDate;
  rs: _Recordset;
begin
  lblLastSurveyDate.Caption := ResStr_SiteNotVisited;
  rs := dmDatabase.GetRecordset('usp_LastSurveyEvent_ForLocation_Select',['@Key', FLocationKey]);
  if not rs.Eof then begin
    vagueDate := dmGeneralData.GetVagueDateFromRecordset(rs);
    if vagueDate.DateTypeString <> '' then
      lblLastSurveyDate.Caption := VagueDateToString(vagueDate);
  end;
  rs.Close;
end;
{-------------------------------------------------------------------------------
  Populate the Status (is location active)
}
function TfrmLocationDetails.GetLocationStatus(): integer;
var
  rs: _Recordset;
  lSql: String;
begin
  result := 0;
  lblStatus.Caption :=  ResStr_SiteStatus;
  lSql := 'SELECT [dbo].[ufn_Location_Expired] (''' + FLocationKey + ''')';
  rs := dmDatabase.ExecuteSql(lSql,True);
  if not rs.eof then
  begin
    if rs.Fields[0].Value = '1' then begin
      lblStatus.Caption :=  ResStr_SiteStatusNot;
      result := 1;
    end;
    rs.Close;
  end;
end;
//==============================================================================
procedure TfrmLocationDetails.AddRecord(const ALocDetKey:TKeyString);
begin
  inherited;
  pcLocationDetails.ActivePage:=tsGeneral;
  // Get next sequential key
  LocationKey := ALocDetKey;
  FCustodian := AppSettings.SiteID;
  SpatialRef.Clear;
  FOrigEnteredRef := '';

  FdmLocationDetails.qryLocation.Append;
  ResetDBRichEditControls([dbreLocDescription, dbreApproach, dbreRestrictions]);
  dmGeneralData.SetNameIDAndDate(FdmLocationDetails.qryLocation,'Entered_By','Entry_Date');

  // Location Names
  FdmLocationDetails.qryLocNames.Parameters.ParamByName('KeyParameter').Value:='';
  FLocNameList.LocationKey:=LocationKey;
  FLocNameList.Refresh;
  // Designation
  FdmLocationDetails.qryDesignation.Parameters.ParamByName('KeyParameter').Value:='';
  FDesignationList.LocationKey:=LocationKey;
  FDesignationList.Refresh;
  // Measurements
  Measurements.KeyValue:=LocationKey;
  Measurements.Refresh;
  //Geo Info
  FdmLocationDetails.qryAdminAreas.Parameters.ParamByName('Key').Value:= ALocDetKey;
  FAdminAreaList.LocationKey:= ALocDetKey;
  FAdminAreaList.Refresh;

  FdmLocationDetails.qryGridSquares.Parameters.ParamByName('Key').Value:= ALocDetKey;
  FGridSquareList.LocationKey:= ALocDetKey;
  FGridSquareList.Refresh;

  FdmLocationDetails.qryLandParcels.Parameters.ParamByName('Key').Value:= ALocDetKey;
  FLandParcelList.LocationKey:= ALocDetKey;
  FLandParcelList.Refresh;

  FdmLocationDetails.qryBoundaries.Parameters.ParamByName('Key').Value:= ALocDetKey;
  FBoundaryList.LocationKey:= ALocDetKey;
  FBoundaryList.Refresh;

  //Other Info
  FdmLocationDetails.qryRelations.Parameters.ParamByName('Key').Value:= ALocDetKey;
  FRelationList.LocationKey:= ALocDetKey;
  FRelationList.Refresh;

  FdmLocationDetails.qryUses.Parameters.ParamByName('Key').Value:= ALocDetKey;
  FUseList.LocationKey:= ALocDetKey;
  FUseList.Refresh;

  FdmLocationDetails.qryTenures.Parameters.ParamByName('Key').Value:= ALocDetKey;
  FTenureList.LocationKey:= ALocDetKey;
  FTenureList.Refresh;
  // Sources
  Sources.SourcesFromKey:=ALocDetKey;
  Sources.RefreshLists;
  { Add COM addin page }
  AddAdditionalPages;
  // Activate the appropriate controls
  EnableDetails(emAdd);
  // Trigger New Location Name action
  bbLocNameAddClick(nil);
end;  // AddRecord

//==============================================================================
function TfrmLocationDetails.DeleteRecord(const ALocDetKey: TKeyString): Boolean;
begin
  Result:= FdmLocationDetails.DeleteRecord(ALocDetKey);
end;  // DeleteRecord

//==============================================================================
procedure TfrmLocationDetails.EditRecord;
begin
  DisplayRecord(LocationKey);
  if dmGeneralData.HasFullEditAccess(TN_LOCATION, 'Location_Key', LocationKey) and
      HaveCustody then
    try
      FdmLocationDetails.qryLocation.Edit;
      dmGeneralData.SetNameIDAndDate(FdmLocationDetails.qryLocation,'Changed_By','Changed_Date');
      EnableDetails(Constants.emEdit);
    except
      on E:Exception do
        if dmDatabase.CheckError(E, dbeRecordLocked) then begin
          MessageDlg(ResStr_CannotEditRecord + #13#13 +
                     dmDatabase.GetErrorMessage(E.Message, dbeRecordLocked),
                     mtInformation, [mbOk], 0);
          TfrmLocations(DrillForm).tvLocations.SetFocus;
        end else
          Raise;
    end  // Try...Except
  else
    EnableDetails(Constants.emEdit);
end;  // EditRecord

//==============================================================================
procedure TfrmLocationDetails.bbSaveClick(Sender: TObject);
var lCursor  : TCursor;
    iCount   : Integer;
    lChecked : Boolean;
    lValidRef: TValidSpatialRef;
    lCurrentTab, lCurrentSubTab : TTabSheet;
    lCustodian : String;
    lLocationStatus : Integer;
begin
  inherited;
  lCurrentTab := pcLocationDetails.ActivePage;
  lCurrentSubTab := nil;
  if lCurrentTab = tsGeoInfo then
    lCurrentSubTab := pcGeoInfo.ActivePage
  else if lCurrentTab = tsOtherInfo then
    lCurrentSubTab := pcOtherInfo.ActivePage;

  // Location Names
  pcLocationDetails.ActivePage := tsGeneral;
  if Sender=nil then begin
    lValidRef := ValidSpatialRef(SpatialRef.SpatialRef, SpatialRef.DisplaySystem);
    ValidateValue(lValidRef.Valid,ResStr_SRSystemNotRecognised,
                  SpatialRef.ControlSpatialRef);
  end;

  // Find if a location name has been checked
  lChecked := False;
  for iCount := 0 to clbLocationNames.Items.Count-1 do
    lChecked := lChecked or clbLocationNames.Checked[iCount];
  ValidateValue(lChecked,ResStr_NoLocationName,bbLocNameAdd);
  ValidateValue(dbcmbLocationType.Text <> '',
                ResStr_LocationTypeRequired,
                dbcmbLocationType);

  ValidateValue(SpatialRef.EnteredRef <> '',
                ResStr_IncludeSpatialRef,
                SpatialRef.ControlSpatialRef);

  ValidateValue(SpatialRef.Qualifier <> '',
                ResStr_EnterSRQualifier,
                SpatialRef.ControlQualifier);

  ValidateValue(CheckRefInParent,
                ResStr_InvalidSpatialRef,
                SpatialRef.ControlSpatialRef);

  // Measurements
  pcLocationDetails.ActivePage:=tsMeasurements;
  ValidateValue(Measurements.CheckGrid, ResStr_MeasurementMissingOrInvalid, Measurements.Grid);

  // Geo.Info. Grid Squares
  pcLocationDetails.ActivePage:=tsGeoInfo;
  pcGeoInfo.ActivePage        :=tsGridSquares;
  ValidateValue(CheckGridSquares,ResStr_InfoMissing,sgGridSquares);
  ValidateValue(CheckRefInSquares,
                ResStr_MustContainSpecifiedSR, sgGridSquares);
  ValidateValue(CheckSquaresInParent,
                ResStr_GridSquareWithinParent,sgGridSquares);

  // Geo.Info. Land Parcel
  pcGeoInfo.ActivePage:=tsLandParcel;
  ValidateValue(CheckLandParcels, ResStr_InfoMissingNumAndMap,
                sgLandParcels);
  pcLocationDetails.ActivePage := lCurrentTab;

  { Call to validate COM addin page... }
  if not CheckAdditionalPageCanSave then
    Exit;

  if lCurrentTab=tsGeoInfo then
    pcGeoInfo.ActivePage := lCurrentSubTab
  else if lCurrentTab=tsOtherInfo then
    pcOtherInfo.ActivePage := lCurrentSubTab;
  if not CheckExistingObservations then
    Exit;
  lCursor:=HourglassCursor;
  try
    lCustodian := dmGeneralData.Custodian(TN_LOCATION, 'Location_Key', LocationKey);
    if ((AppSettings.UserAccessLevel > ualAddOnly) and (lCustodian = AppSettings.SiteID))
       or (EditMode = emAdd) then
    begin
      if FdmLocationDetails.qryLocation.State<>dsBrowse then
        with FdmLocationDetails.qryLocation do
        begin
          FieldByName('Spatial_Ref').AsString := SpatialRef.EnteredRef;
          FieldByName('Spatial_Ref_System').AsString := SpatialRef.EnteredSystem;
          FieldByName('Spatial_Ref_Qualifier').AsString := SpatialRef.Qualifier;
          if EditMode = emAdd then begin
            FieldByName('Location_Key').AsString := LocationKey;
            if ParentKey <> '' then
              FieldByName('Parent_Key').AsString := ParentKey;
          end;
          Post;
        end;
    end else
    if (EditMode = Constants.emEdit) and (lCustodian <> FCustodian) then begin
      FdmLocationDetails.qryLocation.Cancel;
      MessageDlg(Format(ResStr_CustodyChanged, ['Location']), mtWarning, [mbOk], 0);
    end;
    // Location Names
    FLocNameList.LocationKey:=LocationKey;
    FLocNameList.Update;
    // Designation
    FDesignationList.LocationKey:=LocationKey;
    FDesignationList.Update;
    sgDesignationsClick(nil);
    // Measurements
    Measurements.KeyValue:=LocationKey;
    Measurements.UpdateList;
    //Geo Info
    FAdminAreaList.Update;
    FGridSquareList.Update;
    FLandParcelList.Update;
    FBoundaryList.Update;
    //Other Info
    FRelationList.Update;
    FUseList.Update;
    FTenureList.Update;

    // Sources
    Sources.Post;
    // Additional Pages
    SaveAdditionalPages;
    lLocationStatus := GetLocationStatus;
    if DrillForm<>nil then begin
      with clbLocationNames do
        for iCount:=0 to Items.Count-1 do
          if Checked[iCount] then
            with TfrmLocations(DrillForm) do begin
              SetSite(Items[iCount],dbeFileCode.Text,SpatialRef.SpatialRef,LocationKey);
              tvLocations.Selected:=SelectedItem;
              if Not AppSettings.UseOriginalIcons then begin
                If tvLocations.Selected.ImageIndex In[1,2] then
                  tvLocations.Selected.ImageIndex := lLocationStatus + 1
                else if tvLocations.Selected.ImageIndex In[3,4]then
                  tvLocations.Selected.ImageIndex := lLocationStatus + 3;
                tvLocations.Selected:=SelectedItem;
              end;
              Break;
            end;
    end;
    EnableDetails(emView);
    DisplayRecord(LocationKey);
  finally
    DefaultCursor(lCursor);
    //Send message to all open forms that the location detail has changed
    frmMain.BroadcastMessage(WM_REFRESH_LOCATION_DETAILS);
  end;
  // If boundaries have changed, might need to let the rest of the app know about it.
  AppSettings.UpdateMapWindowSelectors;
end;  // bbSaveClick


{-------------------------------------------------------------------------------
 If the spatial reference has changed, and any records exist in Survey_Event or
     Sample linked to this location which point to the previous spatial
     reference exactly, and are in the custody of this site, then the user is
     displayed a message with Yes, No and Cancel option buttons.
 If the user selects Yes, then the Survey_Event and Sample records are updated
 to reflect the new spatial reference.  If the user clicks Cancel then the save
 operation is aborted.  If the user clicks No, then the Save operation proceeds
 without updating Survey_Event or Sample records.
 Returns false if the save should be aborted
}
function TfrmLocationDetails.CheckExistingObservations: boolean;
var
  lCount: integer;
  lResult : word;
  lLatLong: TLatLong;
begin
  Result := True;
  if (EditMode = Constants.emEdit) and (FOrigEnteredRef <> SpatialRef.EnteredRef) then begin
    lCount := dmDatabase.GetStoredProcOutputParam(
        'usp_ObservationsLinkedToLocationCentroidCount_Get',
        ['@Key', FLocationKey,
         '@Spatial_Ref', FOrigEnteredRef],
        '@Count');
    if lCount > 0 then begin
      lResult := ConfirmYesNoCancel(ResStr_ConfirmObservationSpatialRefUpdate);
      if lResult = mrCancel then
        Result := false // abort save operation
      else if lResult = mrYes then begin
        lLatLong := ConvertToLatLong(SpatialRef.EnteredRef, SpatialRef.EnteredSystem);
        dmDatabase.RunStoredProc('usp_ObservationsLinkedToLocationCentroid_Update',
            ['@Key', FLocationKey,
             '@Old_Spatial_Ref', FOrigEnteredRef,
             '@Spatial_Ref', SpatialRef.EnteredRef,
             '@Spatial_Ref_System', SpatialRef.EnteredSystem,
             '@Spatial_Ref_Qualifier', SpatialRef.Qualifier,
             '@Lat', lLatLong.Lat,
             '@Long', lLatLong.Long]);
        frmMain.BroadcastMessage(WM_REFRESH_SPATIAL_REF);
      end;
    end;
  end;
end;

//==============================================================================
procedure TfrmLocationDetails.bbCancelClick(Sender: TObject);
var tfDiscardNew : Boolean;
begin
  inherited;
  tfDiscardNew:=FdmLocationDetails.qryLocation.State=dsInsert;
  FdmLocationDetails.qryLocation.Cancel;
  // Additional Pages
  CancelAdditionalPages;
  EnableDetails(emView);

  // If closing form, no need to do anything further. Otherwise, refresh screens.
  if not FClosingForm then
    if tfDiscardNew and (DrillForm<>nil) then
      PostMessage(TfrmLocations(DrillForm).Handle,WM_Discard_Location,0,0)
    else begin
      with TfrmLocations(DrillForm) do
        tvLocations.Selected:=SelectedItem;
      DisplayRecord(LocationKey);
    end;
end;  // bbCancelClick

//==============================================================================
procedure TfrmLocationDetails.EnableDetails(const NewMode:Constants.TEditMode);
var tfOn:Boolean;
//----------------------------------------------------------------------------
  procedure SetGridAndButtons(AGrid:TStringGrid; AddButton:TButton; tfEditing:Boolean=True);
  begin
    with AGrid do
      if tfOn then begin
        if tfEditing then Options:=Options+[goEditing];
        Options:=Options-[goRowSelect];
      end else begin
        Options:=Options-[goEditing];
        Options:=Options+[goRowSelect];
      end;
    AddButton.Enabled:=tfOn;
  end;  // SetGridAndButtons
//----------------------------------------------------------------------------
begin
  FEditMode:=NewMode;
  tfOn:=EditMode<>emView;

  SetRequiredFieldsColourState(tfOn, [clbLocationNames, dbcmbLocationType,
                                      SpatialRef.ControlSpatialRef,
                                      SpatialRef.ControlQualifier]);

  // General
  bbLocNameAdd.Enabled:=tfOn;
  SetLocationNameButtons;
  bbLocNameDel.Enabled :=bbLocNameEdit.Enabled;
  if FdmLocationDetails.qryLocation.State in [dsEdit, dsInsert] then
    SpatialRef.EditMode:=NewMode
  else
    SpatialRef.EditMode:=emView;

  // Designations
  bbDesignationAdd.Enabled :=tfOn;
  sgDesignationsClick(nil);
  // Measurements
  Measurements.EditMode:=NewMode;
  // Goe Info - Admin Area
  SetGridAndButtons(sgAdminAreas,bbAdminAdd,False);  // No direct grid editing
  sgAdminAreasClick(nil);
  // Geo Info - Grid Squares
  SetGridAndButtons(sgGridSquares,bbSquareAdd);  // Direct editing in grid allowed
  sgGridSquaresClick(nil);
  btnExtractGridSquares.Enabled := tfOn and (FBoundaryList.Count>0);
  // Geo Info - Land Parcel
  SetGridAndButtons(sgLandParcels,bbLandAdd);  // Direct editing in grid allowed
  sgLandParcelsClick(nil);
  // Geo Info - Boundaries
  bbBoundaryAdd.Enabled :=tfOn;
  sgBoundariesClick(nil);
  // Other Info - Relations
  SetGridAndButtons(sgRelations,bbRelationAdd,False);  // no direct grid editing
  sgRelationsClick(nil);
  // Other Info - Uses
  bbUseAdd.Enabled :=tfOn;
  sgUsesClick(nil);
  // Other Info - Tenure
  bbTenureAdd.Enabled :=tfOn;
  sgTenureClick(nil);
  // Popup Menus in RichEdit
  if tfOn then begin
    dbreLocDescription.PopupMenu:=dmFormActions.pmRTF;
    dbreApproach.PopupMenu      :=dmFormActions.pmRTF;
    dbreRestrictions.PopupMenu  :=dmFormActions.pmRTF;
  end else begin
    dbreLocDescription.PopupMenu:=nil;
    dbreApproach.PopupMenu      :=nil;
    dbreRestrictions.PopupMenu  :=nil;
  end;
  // Sources
  Sources.EditMode:=NewMode;

  bbSave.Enabled  :=tfOn;
  bbCancel.Enabled:=tfOn;

  //Additional pages
  SetAdditionalPagesState(tfOn);

  // Upate Main menu
  if DrillForm<>nil then TfrmLocations(DrillForm).SetMenuState(not tfOn);

  // Hide comboboxes on grids, whatever the edit mode
  cmbSquareType.Visible:=False;
  cmbRelation.Visible  :=False;
end;  // EnableDetails

//==============================================================================
{ Select the location name add/edit button state }
procedure TfrmLocationDetails.SetLocationNameButtons;
var lIdx: Integer;
begin
  inherited;
  with clbLocationNames do
    // Nothing in the list
    if FLocNameList.ItemCount=0 then begin
      bbLocNameEdit.Enabled:=False;
      bbLocNameDel.Enabled :=False;
    end else begin
      // There is something in the list
      lIdx:=ItemIndex;
      if lIdx=-1 then lIdx:=0;  // Make sure something is selected.
      with TLocationNameItem(Items.Objects[lIdx]) do
        bbLocNameEdit.Enabled := AppSettings.AllowEdit(EditMode) and
            (Added or dmGeneralData.HaveCustody(TN_LOCATION_NAME, 'LOCATION_NAME_KEY', ItemKey));
       with TLocationNameItem(Items.Objects[lIdx]) do
        bbLocNameDel.Enabled := AppSettings.AllowEdit(EditMode) and
       (Added or True);


    end;
end;  // clbLocationNameClick


//==============================================================================
procedure TfrmLocationDetails.RefreshLocationNames;
var iCount : Integer;
begin
  lblPreferredName.Caption:= '';
  with clbLocationNames do
    for iCount:=0 to Items.Count-1 do begin
      Checked[iCount]:=TLocationNameItem(Items.Objects[iCount]).Preferred;
      if Checked[iCount] then begin
        lblPreferredName.Caption:=DuplicateCharacters(Items[iCount], '&') ;
        lblLocationDisp.Caption:=GetTextWithinLimit(Canvas, lblPreferredName.Caption,
                                 pnlDetails.Width-lblLocationDisp.Left-8);
        FCheckedIndex := iCount;
      end;
    end;
end;  // RefreshLocationNames


//==============================================================================
procedure TfrmLocationDetails.clbLocationNamesClickCheck(Sender: TObject);
var
  liCount : Integer;
begin
  inherited;
  with clbLocationNames do
    if Items.Count>0 then
      if EditMode<>emView then begin
        { Ensure all other items are unchecked }
        for liCount:= 0 to Items.Count-1 do begin
          TLocationNameItem(Items.Objects[liCount]).Preferred:=(liCount = ItemIndex);
          Checked[liCount]:=(liCount = ItemIndex);
        end;
        lblPreferredName.Caption:=Items[ItemIndex];
        FCheckedIndex := ItemIndex;
      end else
        { disallow change of checked state - not in edit mode }
        for liCount:= 0 to Items.Count-1 do
          Checked[liCount]:=(liCount = FCheckedIndex);
end;  // clbLocationNameClickCheck


//==============================================================================
procedure TfrmLocationDetails.clbLocationNamesClick(Sender: TObject);
begin
  inherited;
  SetLocationNameButtons;
end;  // clbLocationNameClick

//==============================================================================
procedure TfrmLocationDetails.bbLocNameAddClick(Sender: TObject);
var lIdx     :Integer;
    lFound   :Boolean;
    lDataItem:TLocationNameItem;
begin
  inherited;
  with TdlgTextInput.Create(nil) do
    try
      Caption            :=ResStr_LocationNames;
      lblSubject.Caption :=ResStr_LocationName;
      eInitials.Visible  :=False;
      lblInitials.Visible:=False;

      if ShowModal=mrOk then begin
        lFound:=False;
        for lIdx:=0 to clbLocationNames.Items.Count-1 do
          if CompareText(clbLocationNames.Items[lIdx],eName.Text)=0 then begin
            lFound:=True;
            Break;
          end;
        if lFound then
          MessageDlg(ResStr_LocationNameExists,mtInformation,[mbOk],0)
        else begin
          lDataItem:=TLocationNameItem.CreateNew(FLocNameList);
          lDataItem.LocationName:=eName.Text;
          lDataItem.Preferred   :=(FLocNameList.ItemCount=0);
          FLocNameList.AddNew(lDataItem);
          RefreshLocationNames;
        end;
      end;
    finally
      Free;
    end;
  clbLocationNamesClick(nil);
end;  // bbLocNameAddClick

//==============================================================================
procedure TfrmLocationDetails.bbLocNameEditClick(Sender: TObject);
var lIdx, lItemIndex:Integer;
    lFound          :Boolean;
    lDataItem       :TLocationNameItem;
begin
  inherited;
  if clbLocationNames.ItemIndex<>-1 then
    with TdlgTextInput.Create(nil) do
      try
        Caption            :=ResStr_LocationNames;
        lblSubject.Caption :=ResStr_LocationName;
        eInitials.Visible  :=False;
        lblInitials.Visible:=False;

        lItemIndex:=clbLocationNames.ItemIndex;
        lDataItem :=TLocationNameItem(clbLocationNames.Items.Objects[lItemIndex]);
        eName.Text:=lDataItem.LocationName;

        if ShowModal=mrOk then begin
          lFound:=False;
          for lIdx:=0 to clbLocationNames.Items.Count-1 do
            if (CompareText(clbLocationNames.Items[lIdx],eName.Text)=0) and
               (lItemIndex<>lIdx) then begin
              lFound:=True;
              Break;
            end;
          if lFound then
            MessageDlg(ResStr_LocationNameExists,mtInformation,[mbOk],0)
          else begin
            lDataItem.LocationName:=eName.Text;
            clbLocationNames.Checked[lItemIndex]:=lDataItem.Preferred;
            // If preferred name modified, update label
            if clbLocationNames.Checked[lItemIndex] then lblPreferredName.Caption:=eName.Text;
          end;
        end;
      finally
        Free;
      end;
  clbLocationNamesClick(nil);
end;  // bbLocNameEditClick

//==============================================================================
procedure TfrmLocationDetails.bbLocNameDelClick(Sender: TObject);
var tfPref : Boolean;
begin
  inherited;
  with clbLocationNames do
    if ItemIndex<>-1 then
      if MessageDlg(ResStr_DeleteName,
                    mtConfirmation,[mbNo,mbYes],0)=mrYes then
      begin
        tfPref:=TLocationNameItem(Items.Objects[ItemIndex]).Preferred;
        FLocNameList.DeleteItem(ItemIndex);
        if tfPref and (Items.Count>0) then begin
          TLocationNameItem(Items.Objects[0]).Preferred:=True;
          Checked[0]:=True;
          lblPreferredName.Caption:=Items[0];
        end;
      end;
  clbLocationNamesClick(nil);
end;  // bbLocNameDeleteClick

//==============================================================================
procedure TfrmLocationDetails.DrawCellChoppedText(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  inherited;
  with TStringGrid(Sender) do begin
    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
  end;
end;  // DrawCellChoppedText

//==============================================================================
procedure TfrmLocationDetails.sgDesignationsClick(Sender: TObject);
begin
  inherited;
  with sgDesignations do
    FCurrentDes:=TDesignationItem(Objects[0,Row]);
  if FCurrentDes=nil then begin
    bbDesignationEdit.Enabled:=False;
    bbDesignationDel.Enabled :=False;
    BlankDesignation;
  end else
    with FCurrentDes do begin
      dbcmbSiteStatus.KeyValue:=SiteStatusKey;
      eDesRefCode.Text        :=RefCode;
      eDesAuthority.Text      :=Authority;
      eDesAuthority.Key       :=AuthorityKey;
      eDesignationFrom.Text   :=DateFrom;
      eDesignationTo.Text     :=DateTo;
      // Get back to the beginning of the stream before reading it.
      Comment.Position:=0;
      reDesignComments.Lines.LoadFromStream(Comment);

      EnableDetailsAddEditButtons(TN_LOCATION_DESIGNATION, 'Designation_Key', ItemKey,
          Custodian, EditMode, Added, bbDesignationEdit, bbDesignationDel, True);
    end;
end;  // sgDesignationsClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.BlankDesignation;
begin
  dbcmbSiteStatus.ItemIndex:=0;
  eDesRefCode.Text         :='';
  eDesAuthority.Text       :='';
  eDesignationFrom.Text    :='';
  eDesignationTo.Text      :='';
  reDesignComments.Clear;
end;  // BlankDesignation

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.SaveDesignation;
begin
  with FCurrentDes do begin
    SiteStatusKey:=dbcmbSiteStatus.KeyValue;
    SiteStatus   :=dbcmbSiteStatus.Text;
    RefCode      :=eDesRefCode.Text;
    Authority    :=eDesAuthority.Text;
    AuthorityKey :=eDesAuthority.Key;    
    DateFrom     :=eDesignationFrom.Text;
    DateTo       :=eDesignationTo.Text;
    Comment.Position:=0;
    reDesignComments.Lines.SaveToStream(Comment);
  end;
  sgDesignations.Refresh;
end;  // SaveDesignation

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.SetDesignationColour(const tfDetailsOn:Boolean);
begin
  SetRequiredFieldsColourState(tfDetailsOn,[dbcmbSiteStatus,eDesAuthority]);
end;  // SetDesignationColour

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbDesignationAddClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgDesignations, bbDesignationAdd, bbDesignationEdit, bbDesignationDel,
                  bbSave, bbCancel, gbDesignationDetails, True);
  SetDesignationColour(True);
  FCurrentDes:=TDesignationItem.CreateNew(FDesignationList);
  BlankDesignation;
  FAddItem:=True;
end;  // bbDesignationAddClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbDesignationEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgDesignations, bbDesignationAdd, bbDesignationEdit, bbDesignationDel,
                  bbSave, bbCancel, gbDesignationDetails, True);
  SetDesignationColour(True);
  FAddItem:=False;
end;  // bbDesignationEditClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbDesignationDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteDesignation, mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    FDesignationList.DeleteItem(sgDesignations.Row);
    sgDesignationsClick(nil);
  end;
end;  // bbDesignationDelClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbDesignationAcceptClick(Sender: TObject);
begin
  inherited;
  // Sender is nil if called from CloseQuery method.
  if Sender=nil then begin
    ValidateDesignationFromDate;
    ValidateDesignationToDate;
  end else begin
    // Check that the end date follows the start date
    if (eDesignationFrom.Text <> '') and (eDesignationTo.Text <> '') then
      ValidateValue((StrToDate(eDesignationFrom.Text) <= StrToDate(eDesignationTo.Text)),
                     ResStr_EndBeforeStartDate,
                     eDesignationTo);
  end;

  ValidateValue(dbcmbSiteStatus.Text<>'',ResStr_DesignationRequired,dbcmbSiteStatus);
  ValidateValue(eDesAuthority.Text<>'',ResStr_AuthorityNameRequired,eDesAuthority);
  ValidateValue(dmGeneralData.CheckName(eDesAuthority),ResStr_InvalidAuthorityName,eDesAuthority);

  SaveDesignation;
  if FAddItem then begin
    FDesignationList.AddNew(FCurrentDes);
    sgDesignations.Row:=sgDesignations.RowCount-1;
  end;
  SwitchToDetails(sgDesignations, bbDesignationAdd, bbDesignationEdit, bbDesignationDel,
                  bbSave, bbCancel, gbDesignationDetails, False);
  SetDesignationColour(False);
  sgDesignationsClick(nil);
end;  // bbDesignationAcceptClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbDesignationDiscardClick(Sender: TObject);
begin
  inherited;
  if FAddItem then FCurrentDes.Free;
  SwitchToDetails(sgDesignations, bbDesignationAdd, bbDesignationEdit, bbDesignationDel,
                  bbSave, bbCancel, gbDesignationDetails, False);
  SetDesignationColour(False);
  sgDesignationsClick(nil);
end;  // bbDesignationDiscardClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.UpdateDesAuthority(KeyList:TKeyList);
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then begin
      eDesAuthority.Text:=dmGeneralData.GetName(KeyList.Items[0].KeyField1);
      eDesAuthority.Key:=KeyList.Items[0].KeyField1;
    end;
  finally
    KeyList.Free;
  end;
end;  // UpdateDesAuthority

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.eDesignationFromExit(Sender: TObject);
var lPos:TPoint;
begin
  inherited;
  lPos := bbDesignationDiscard.ScreenToClient(Mouse.CursorPos);
  if not (FClosingForm or
          ((lPos.X in [0..bbDesignationDiscard.Width]) and (lPos.Y in [0..bbDesignationDiscard.Height]))) then
    ValidateDesignationFromDate;
end;  // eDesignationFromExit

procedure TfrmLocationDetails.ValidateDesignationFromDate;
begin
  if eDesignationFrom.Text <> '' then begin
    // Use VagueDate functions to deal with other date separators, like "."
    try
      eDesignationFrom.Text := VagueDateToString(StringToVagueDate(eDesignationFrom.Text));
    except
    end;
    // And validate (NB: 1753-01-01 is the minimum value for SQL Server's
    // datetime datatype)
    ValidateValue(IsDate(eDesignationFrom.Text) and
        (StrToDate(eDesignationFrom.Text) >= EncodeDate(1753, 1, 1)) and
        (StrToDate(eDesignationFrom.Text) <= Date),
        InvalidDate(ResStr_StartDate,False,False),eDesignationFrom);
  end;
end;  // ValidateDesignationFromDate

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.eDesignationToExit(Sender: TObject);
var lPos:TPoint;
begin
  inherited;
  lPos:=bbDesignationDiscard.ScreenToClient(Mouse.CursorPos);
  if not (FClosingForm or
          ((lPos.X in [0..bbDesignationDiscard.Width]) and (lPos.Y in [0..bbDesignationDiscard.Height]))) then
    ValidateDesignationToDate;
end;  // eDesignationToDate

procedure TfrmLocationDetails.ValidateDesignationToDate;
begin
  if (eDesignationTo.Text <> '') then begin
    // Use VagueDate functions to deal with other date separators, like "."
    try
      eDesignationTo.Text := VagueDateToString(StringToVagueDate(eDesignationTo.Text));
    except
    end;
    // And validate
    ValidateValue(IsDate(eDesignationTo.Text) and (StrToDate(eDesignationTo.Text)<=Date),
                  InvalidDate(ResStr_EndDate,False,True),eDesignationTo);
    ValidateValue(IsDate(eDesignationTo.Text) and
        (StrToDate(eDesignationTo.Text) >= EncodeDate(1753, 1, 1)) and
        (StrToDate(eDesignationTo.Text) <= Date),
        InvalidDate(ResStr_FromDate,False,False),eDesignationFrom);
    if (eDesignationFrom.Text <> '') then
      ValidateValue((StrToDate(eDesignationFrom.Text) <= StrToDate(eDesignationTo.Text)),
                     ResStr_EndBeforeStartDate,
                     eDesignationTo);
  end;
end;  // eDesignationToExit

//==============================================================================
procedure TfrmLocationDetails.sgAdminAreasClick(Sender: TObject);
begin
  inherited;
  if FAdminAreaList.ItemCount=0 then
    bbAdminDel.Enabled:=False
  else
    with TAdminAreaItem(sgAdminAreas.Objects[0,sgAdminAreas.Row]) do
      EnableDetailsAddEditButtons(TN_LOCATION_ADMIN_AREAS,
          'Location_Admin_Areas_Key', ItemKey,
          Custodian, EditMode, Added, nil, bbAdminDel, True);
end;  // sgAdminAreasClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbAdminAddClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actAdminAreaDiction.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateAdminArea);
end;  // bbAdminAddClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.UpdateAdminArea(KeyList:TKeyList);
var lDataItem : TAdminAreaItem;
  lTempList : TStringList;
  lCount : Integer;
  lExists : Boolean;
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then begin
      // Check the item is not already there
      lExists:=False;
      with sgAdminAreas do
        for lCount:=1 to RowCount-1 do
          lExists:=lExists or ((Objects[0,lCount]<>nil) and
                   (TAdminAreaItem(Objects[0,lCount]).AdminAreaKey=KeyList.Items[0].KeyField1));

      if not lExists then begin
        lDataItem:=TAdminAreaItem.CreateNew(FAdminAreaList);
        with lDataItem do begin
          AdminAreaKey:=KeyList.Items[0].KeyField1;

          //Find display fields
          lTempList:= TStringList.Create;
          try
            dmGeneralData.GetRecordStrings(lTempList, TN_ADMIN_AREA, AdminAreaKey);
            if lTempList.Values['SHORT_CODE'] = '' then
              Name:= lTempList.Values['ITEM_NAME']
            else
              Name:= lTempList.Values['SHORT_CODE'] + ', ' + lTempList.Values['ITEM_NAME'];
            dmGeneralData.GetRecordStrings(lTempList, 'ADMIN_TYPE', lTempList.Values['ADMIN_TYPE_KEY']);
            AdminAreaType:= lTempList.Values['SHORT_NAME'];
          finally
            lTempList.Free;
          end;
        end;
        FAdminAreaList.AddNew(lDataItem);
      end;
    end;
  finally
    KeyList.Free;
  end;
  sgAdminAreasClick(nil);
end;  // UpdateAdminArea

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbAdminDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteAdmin, mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    FAdminAreaList.DeleteItem(sgAdminAreas.Row);
    sgAdminAreasClick(nil);
  end;
end;  // bbAdminDelClick

//==============================================================================
procedure TfrmLocationDetails.sgGridSquaresClick(Sender: TObject);
var lDataItem: TGridSquareItem;
begin
  inherited;
  if (EditMode = emView) then begin
    bbSquareDel.Enabled   := False;
    cmbSquareType.Visible := False;
  end else
    with sgGridSquares do begin
      // Add a new default item only if event triggered by user
      if (FGridSquareList.ItemCount = 0) and (Sender is TStringGrid) then begin
        // List empty, create a default first item and set focus on first cell
        lDataItem        := TGridSquareItem.CreateNew(FGridSquareList);
        lDataItem.UserId := AppSettings.UserId;  
        lDataItem.SpatialRefSystem := AppSettings.SpatialRefSystem;
        lDataItem.EnteredRefSystem := AppSettings.SpatialRefSystem;
        FGridSquareList.AddNew(lDataItem);
        Row := RowCount - 1;
        Col := 0;
      end;
      lDataItem := TGridSquareItem(Objects[0, Row]);
      if lDataItem = nil then
        bbSquareDel.Enabled := False
      else begin
        with lDataItem do  begin
          EnableDetailsAddEditButtons(TN_GRID_SQUARE, 'Grid_Square_Key', ItemKey,
              Custodian, EditMode, Added, nil, bbSquareDel, True);
        if bbSquareDel.Enabled or (lDataItem.ItemKey = '') then begin
          if Col = 1 then Options := Options - [goEditing]
                     else Options := Options + [goEditing]
        end else
          Options := Options - [goEditing];
        cmbSquareType.Visible := False;
      end;
      end;
    end;
end;  // sgGridSquaresClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.sgGridSquaresDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var lDataItem      : TGridSquareItem;
    tfShowComboBox : Boolean;
    lValidSR       : TValidSpatialRef;
    lFOrmattedSR   : String;
begin
  inherited;
  with sgGridSquares do begin
    if (EditMode <> emView) and (FGridSquareList.ItemCount > 0) then begin
      if ARow > 0 then begin
        lDataItem      := TGridSquareItem(Objects[0, Row]);
        tfShowComboBox := bbSquareDel.Enabled or (lDataItem.ItemKey = '');
      end else
        tfShowComboBox:=False;

      if not FDeletingItem then begin
        if tfShowComboBox and (Col = 1) then begin
          { If in the second column, then draw the combobox. }
          if (gdFocused in State) and (Col = 1) and (Row > 0) then
          begin
            cmbSquareType.Left      := Rect.Left + Left + 1;
            cmbSquareType.Top       := Rect.Top + Top + 1;
            cmbSquareType.Width     := Rect.Right - Rect.Left + 2;
            cmbSquareType.ItemIndex := cmbSquareType.Items.IndexOf(Cells[Col, Row]);
            cmbSquareType.Visible   := True;
          end;
          { If not, then hide the combobox }
          if (gdFocused in State) and (Col <> 1) then
            cmbSquareType.Visible := False;
        end;
        { Has a new grid square been entered? }
        if (ACol = 0) and  (ARow = FRow) and (GridSquareSR <> '')  then
        begin
          lFormattedSR := ValidateGridSquareSR(0, FRow);
          lValidSR     := ValidSpatialRef(GridSquareSR, AppSettings.SpatialRefSystem);
          GridSquareSR := '';
        end; // if a new grid square has been entered

        lDataItem := TGridSquareItem(Objects[0, ARow]);  // Returns nil if no objects
        if lDataItem <> nil then begin  // Check we have someting here before going on
          if lDataItem.UserId = '' then
            lDataItem.UserId := AppSettings.UserId;
          if DelocaliseSpatialRef(Cells[0, ARow]) <> lDataItem.EnteredRef then
          begin
            lDataItem.EnteredRef := lValidSR.EnteredFormattedSR;
            lDataItem.SpatialRef := lValidSR.FormattedSR;
          end;
          // This is to ensure that the EnteredRefSystem is properly set
          if lDataItem.EnteredRef <> '' then
            lDataItem.EnteredRefSystem := DetermineSpatialRefSystem(lDataItem.EnteredRef)
          else
            lDataItem.EnteredRefSystem := '';
        end;
      end else
        cmbSquareType.Visible := False;
    end;
    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol,ARow],Canvas,Rect, 2);
    if (ACol = 0) and (ARow > 0) then
      DrawChoppedText(lFormattedSR, Canvas, Rect, 2);
  end;
end;  // sgGridSquaresDrawCell


{------------------------------------------------------------------------------
    Procedure Name: sgGridSquaresSetEditText
            Called: User moving from editting a cell
           Purpose: Validation
------------------------------------------------------------------------------}
procedure TfrmLocationDetails.sgGridSquaresSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: String);
begin
  GridSquareSR := DelocaliseSpatialRef(Value);
  FRow:=ARow;  // Little problem with goEditing in Options or not and DrawCell
end;          

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbSquareAddClick(Sender: TObject);
var lNewGridSquare: TGridSquareItem;
begin
  inherited;
  with sgGridSquares do
    if Cells[0, RowCount - 1] <> '' then begin
      lNewGridSquare                  := TGridSquareItem.CreateNew(FGridSquareList);
      lNewGridSquare.UserId           := AppSettings.UserId;
      lNewGridSquare.SpatialRefSystem := AppSettings.SpatialRefSystem;
      lNewGridSquare.EnteredRefSystem := AppSettings.SpatialRefSystem;
      FGridSquareList.AddNew(lNewGridSquare);
      Row := RowCount - 1;
      Col := 0;
    end;
  sgGridSquaresClick(nil);
  sgGridSquares.SetFocus;
end;  // bbSquareAddClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbSquareDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_InvalidGridSquare,
                mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    FDeletingItem:=True;
    FGridSquareList.DeleteItem(sgGridSquares.Row);
    FDeletingItem:=False;
  end;
  sgGridSquaresClick(nil);
end;  // bbSquareDelClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.cmbSquareTypeChange(Sender: TObject);
var lDataItem : TGridSquareItem;
begin
  inherited;
  lDataItem := TGridSquareItem(sgGridSquares.Objects[0, sgGridSquares.Row]);
  with cmbSquareType do begin
    lDataItem.SpatialRefSizeName := Text;
    lDataItem.SpatialRefSize     := TGridSquareInfo(Items.Objects[ItemIndex]).Size;
  end;
end;  // cmbSquareTypeChange

{------------------------------------------------------------------------------
    Procedure Name: ValidateGridSquareSR
            Called: sgGridSquare.OnDraw event.
           Purpose: To check that any spatial refernces in the Grid Squares
                    TStringGrid are valid, and to return a formatted version or
                    to raise an exception.
      Side Effects: Calls RemoveSubstrings, DecodeFromLetters, ValidInteger,
                    OSIGSpatialRefToEN
------------------------------------------------------------------------------}
function TfrmLocationDetails.ValidateGridSquareSR(const iColumn, iRow:Integer): String;
var
  lValidSR : TValidSpatialRef;
begin
  if GridSquareSR <> '' then
  begin
    lValidSR := ValidSpatialRef(GridSquareSR, AppSettings.SpatialRefSystem);
    if not lValidSR.Valid then
    begin
      GridSquareSR := '';
      raise ELocationError.CreateValidation(lValidSR.Error, sgGridSquares);
    end else begin
      Result := LocaliseSpatialRef(lValidSR.FormattedSR);
      sgGridSquares.Cells[ iColumn, iRow ] := Result;
    end;
  end; // if <> ''
end;

//==============================================================================
procedure TfrmLocationDetails.sgLandParcelsClick(Sender: TObject);
var lDataItem : TLandParcelItem;
begin
  inherited;
  if (EditMode=emView) then
    bbLandDel.Enabled:=False
  else
    with sgLandParcels do begin
      // Add a new default item only if event triggered by user
      if (FLandParcelList.ItemCount=0) and (Sender is TStringGrid) then begin
        // List empty, create a default first item and set focus on first cell
        lDataItem:= TLandParcelItem.CreateNew(FLandParcelList);
        FLandParcelList.AddNew(lDataItem);
        Row:=RowCount-1;
        Col:=0;
      end;
      lDataItem := TLandParcelItem(Objects[0, Row]);
      if lDataItem = nil then
        bbLandDel.Enabled := False
      else begin
        with lDataItem do
          EnableDetailsAddEditButtons(TN_LAND_PARCEL, 'Land_Parcel_Key', ItemKey,
                Custodian, EditMode, Added, nil, bbLandDel, True);

        if bbLandDel.Enabled or (lDataItem.ItemKey = '') then
          Options := Options + [goEditing]
        else
          Options := Options - [goEditing];
      end;
    end;
end;  // sgLandParcelsClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.sgLandParcelsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var lDataItem : TLandParcelItem;
begin
  inherited;
  with sgLandParcels do begin
    if (FLandParcelList.ItemCount>0) and not FDeletingItem then begin
      lDataItem:=TLandParcelItem(Objects[0,ARow]);  // Returns nil if no objects

      if goEditing in Options then
        if lDataItem<>nil then begin  // Check we have someting here before going on
          if Cells[0,ARow]<>lDataItem.Number    then lDataItem.Number   :=Cells[0,ARow];
          if Cells[1,ARow]<>lDataItem.MapSystem then lDataItem.MapSystem:=Cells[1,ARow];
        end;
    end;
    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
  end;
end;  // sgLandParcelsDrawCell

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbLandAddClick(Sender: TObject);
var lNewLandParcel : TLandParcelItem;
begin
  inherited;
  with sgLandParcels do
    if Cells[0,RowCount-1]<>'' then begin
      lNewLandParcel:= TLandParcelItem.CreateNew(FLandParcelList);
      FLandParcelList.AddNew(lNewLandParcel);
      Row:=RowCount-1;
      Col:=0;
    end;
  sgLandParcelsClick(nil);
end;  // bbLandAddClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbLandDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteLandParcel,mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    FDeletingItem:=True;
    FLandParcelList.DeleteItem(sgLandParcels.Row);
    FDeletingItem:=False;
  end;
  sgLandParcelsClick(nil);
end;  // bbLandDelClick

//==============================================================================
procedure TfrmLocationDetails.sgBoundariesClick(Sender: TObject);
begin
  inherited;
  with sgBoundaries do
    FCurrentBoundary:=TBoundaryItem(Objects[0,Row]);
  if FCurrentBoundary=nil then begin
    bbBoundaryEdit.Enabled:=False;
    bbBoundaryDel.Enabled :=False;
    BlankBoundary;
  end else
  //Populate the details fields with information from the list
    with FCurrentBoundary do
    begin
      eBoundaryFrom.Text    := FromDate;
      eBoundaryTo.Text      := ToDate;
      eBoundaryVersion.Text := IntToStr(Version);
      FObjectIDExists       := True;
      RemoveExternalFile;
      if Assigned(MapSheet) then begin
        FMapSheetKey := MapSheet.MapSheetKey;
        if MapSheet.IsInternal then begin
          cmbMapFile.ItemIndex := cmbMapFile.Items.IndexOf(BoundaryMapFileName);
          try
            SetcmbGISObjectID(IntToStr(ObjectID));
            RefreshLabel;
          except
            on EConvertError do
              raise ELocationError.CreateNonCritical(ResStr_DeleteLandParcel);
          end;
        end else begin
          // Add the external file to the combo box and select it.
          cmbMapFile.Items.AddObject(MapSheet.FileName, TMapSheet.CopyFrom(MapSheet));
          cmbMapFile.ItemIndex := cmbMapFile.Items.Count - 1;

          // Sets the selected ID to the value of the object ID.
          SetcmbGISObjectID(ObjectKey);
          RefreshLabel;
        end;
      end else begin
        cmbMapFile.ItemIndex     := -1;
        cmbGISObjectID.ItemIndex := -1;
      end;
      EnableDetailsAddEditButtons(TN_LOCATION_BOUNDARY, 'Location_Boundary_Key', ItemKey,
          Custodian, EditMode, Added, bbBoundaryEdit, bbBoundaryDel, True);
    end;
end;  // sgBoundariesClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.BlankBoundary;
begin
  eBoundaryFrom.Text   := '';
  eBoundaryTo.Text     := '';
  eBoundaryVersion.Text:= '';
  cmbMapFile.ItemIndex := -1;
  FMapSheetKey         := '';
  RefreshObjectIDCombo(nil);
end;  // BlankBoundary

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.SaveBoundary;
begin
  with FCurrentBoundary do begin
    FromDate    := eBoundaryFrom.Text;
    ToDate      := eBoundaryTo.Text;
    Version     := StrToInt(eBoundaryVersion.Text);
    if Assigned(MapSheet) then
      MapSheet.Free;
    if cmbMapFile.ItemIndex = -1 then
      MapSheet  := nil
    else begin
      MapSheet  := TMapSheet.CopyFrom(TMapSheet(cmbMapFile.Items.Objects[cmbMapFile.ItemIndex]));
      if MapSheet.IsInternal then begin
        ObjectID := StrToInt(cmbGISObjectID.Items[cmbGISObjectID.ItemIndex]);
      end else begin
        ObjectKey := cmbGISObjectID.Items[cmbGISObjectID.ItemIndex];
      end;
    end;
  end;
  sgBoundaries.Refresh;
end;  // SaveBoundary

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.SetBoundaryColour(const tfDetailsOn:Boolean);
begin
  SetRequiredFieldsColourState(tfDetailsOn,[eBoundaryVersion]);
end;  // SetBoundaryColour

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbBoundaryAddClick(Sender: TObject);
begin
  inherited;
  cmbMapFile.ItemIndex := -1;
  RefreshMapFileCombo;
  //Add new boundary
  SwitchToDetails(sgBoundaries, bbBoundaryAdd, bbBoundaryEdit, bbBoundaryDel,
                  bbSave, bbCancel, gbBoundaryDetails, True);
  btnGetGISObject.Enabled         := AppSettings.AvailableMaps.Count > 0;
  btnGetGISObjectDropDown.Enabled := btnGetGISObject.Enabled;
  SetBoundaryColour(True);
  BlankBoundary;
  FAddItem:= True;
  FCurrentBoundary:= TBoundaryItem.CreateNew(FBoundaryList);
end;  // bbBoundaryAddClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbBoundaryEditClick(Sender: TObject);
var
  objectID: string;
begin
  inherited;
  SwitchToDetails(sgBoundaries, bbBoundaryAdd, bbBoundaryEdit, bbBoundaryDel,
                  bbSave, bbCancel, gbBoundaryDetails, True);
  btnGetGISObject.Enabled         := AppSettings.AvailableMaps.Count > 0;
  btnGetGISObjectDropDown.Enabled := btnGetGISObject.Enabled;
  SetBoundaryColour(True);
  FAddItem:= False;
  if (cmbMapFile.ItemIndex <> -1) and Assigned(cmbMapFile.Items.Objects[cmbMapFile.ItemIndex]) then
  begin
    objectID := cmbGISObjectID.Items[cmbGISObjectID.ItemIndex];
    RefreshObjectIDCombo(TMapSheet(cmbMapFile.Items.Objects[cmbMapFile.ItemIndex]));
    SetcmbGISObjectID(objectID);
  end else
    RefreshObjectIDCombo(nil);
end;  // bbBoundaryEditClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbBoundaryDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteBoundary,mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    FBoundaryList.DeleteItem(sgBoundaries.Row);
    sgBoundariesClick(nil);
  end;
end;  // bbBoundaryDelClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbBoundaryAcceptClick(Sender: TObject);
var lValue, lErrCode : Integer;
begin
  inherited;
  // Sender is nil if called from CloseQuery method.
  if Sender = nil then begin
    ValidateBoundaryFromDate;
    ValidateBoundaryToDate;
  end;

  eBoundaryVersion.Text := Trim(eBoundaryVersion.Text);
  Val(eBoundaryVersion.Text, lValue, lErrCode);
  ValidateValue(eBoundaryVersion.Text <> '', ResStr_VersionNumMissing, eBoundaryVersion);
  ValidateValue((lValue > 0) and (lErrCode = 0),
                ResStr_InvalidVersionNumber, eBoundaryVersion);
  ValidateValue(lValue <= 32767, ResStr_VersionNumLessThan, eBoundaryVersion);

  // If the map is set, must select an ID.
  ValidateValue((cmbMapFile.ItemIndex = -1) or (cmbGISObjectID.ItemIndex <> -1),
                ResStr_IfFileSetObjectIDSet);
  if (cmbMapFile.ItemIndex <> -1) and
     not TMapSheet(cmbMapFile.Items.Objects[cmbMapFile.ItemIndex]).IsInternal then
    if not FFileExists then
      ShowInformation(ResStr_MapFileNotFound)
    else if not FObjectIDExists then
      ShowInformation(ResStr_ObjectIDNotExist);

  SaveBoundary;
  if FAddItem then begin
    FBoundaryList.AddNew(FCurrentBoundary);
    sgBoundaries.Row := sgBoundaries.RowCount - 1;
  end;
  SwitchToDetails(sgBoundaries, bbBoundaryAdd, bbBoundaryEdit, bbBoundaryDel,
                  bbSave, bbCancel, gbBoundaryDetails, False);
  btnGetGISObject.Enabled         := False;
  btnGetGISObjectDropDown.Enabled := False;
  SetBoundaryColour(False);
  sgBoundariesClick(nil);
end;  // bbBoundaryAcceptClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbBoundaryDiscardClick(Sender: TObject);
begin
  if FAddItem then FCurrentBoundary.Free;
  SwitchToDetails(sgBoundaries, bbBoundaryAdd, bbBoundaryEdit, bbBoundaryDel,
                  bbSave, bbCancel, gbBoundaryDetails, False);
  btnGetGISObject.Enabled         := False;
  btnGetGISObjectDropDown.Enabled := False;
  SetBoundaryColour(False);
  sgBoundariesClick(nil);
end;  // bbBoundaryDiscardClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.eBoundaryFromExit(Sender: TObject);
var lPos:TPoint;
begin
  inherited;
  lPos:=bbBoundaryDiscard.ScreenToClient(Mouse.CursorPos);
  if not (FClosingForm or
          ((lPos.X in [0..bbBoundaryDiscard.Width]) and (lPos.Y in [0..bbBoundaryDiscard.Height]))) then
    ValidateBoundaryFromDate;
end;  // eBoundaryFromExit

procedure TfrmLocationDetails.ValidateBoundaryFromDate;
begin
  if eBoundaryFrom.Text<>'' then begin
    ValidateValue(IsVagueDate(eBoundaryFrom.Text),
                  ResStr_InvalidVagueDate,eBoundaryFrom);
    ValidateValue(CheckVagueDate(eBoundaryFrom.Text),
                  InvalidDate(ResStr_StartDate,True,False),eBoundaryFrom);
    eBoundaryFrom.Text:=VagueDateToString(eBoundaryFrom.VagueDate);
  end;
end;  // ValidateBoundaryFromDate

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.eBoundaryToExit(Sender: TObject);
var lPos:TPoint;
begin
  inherited;
  lPos:=bbBoundaryDiscard.ScreenToClient(Mouse.CursorPos);
  if not (FClosingForm or
          ((lPos.X in [0..bbBoundaryDiscard.Width]) and (lPos.Y in [0..bbBoundaryDiscard.Height]))) then
    ValidateBoundaryToDate;
end;  // eBoundaryToExit

procedure TfrmLocationDetails.ValidateBoundaryToDate;
begin
  if eBoundaryTo.Text<>'' then begin
    ValidateValue(IsVagueDate(eBoundaryTo.Text),
                  ResStr_InvalidVagueDate,eBoundaryTo);
    ValidateValue(CheckVagueDate(eBoundaryTo.Text),
                  InvalidDate(ResStr_EndDate,True,True),eBoundaryTo);
    eBoundaryTo.Text:=VagueDateToString(eBoundaryTo.VagueDate);
    ValidateValue(not IsVagueDateInVagueDate(eBoundaryTo.VagueDate, eBoundaryFrom.VagueDate),
                  ResStr_StartContainEndDate,eBoundaryTo);
    ValidateValue(not IsVagueDateInVagueDate(eBoundaryFrom.VagueDate, eBoundaryTo.VagueDate),
                  ResStr_EndContainStartDate,eBoundaryTo);
    ValidateValue(CompareVagueDateToVagueDate(eBoundaryTo.VagueDate,eBoundaryFrom.VagueDate) >= 0,
                  ResStr_EndBeforeStartDate,eBoundaryTo);
  end;
end;  // eBoundaryToExit

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.btnGetGISObjectClick(Sender: TObject);
begin
  // Default map requested.
  FExpectingSpatialRef := False;
  dmFormActions.actMapWindow.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateBoundary);
end;  // btnGISObjectFindClick

{-------------------------------------------------------------------------------
}
procedure TfrmLocationDetails.btnGetGISObjectDropDownClick(Sender: TObject);
var
  lPos: TPoint;
begin
  inherited;
  lPos := btnGetGISObject.ClientToScreen(Point(0, btnGetGISObject.Height));
  pmMapWindowForBoundary.Popup(lPos.X, lPos.Y);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmLocationDetails.MapForGISObjectClick(Sender: TObject);
begin
  // Map selected from dropdown menu.
  FExpectingSpatialRef := False;
  dmFormActions.MapWindowMenuClick(Sender);
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateBoundary);
end;

//------------------------------------------------------------------------------
// This is being used to pass a TLocation from the Map to the location
// details form.  The header of the keylist is being used to store
// the map file name, the first item on the list is the static/object ID
// and the second is the central spatial reference.
procedure TfrmLocationDetails.UpdateBoundary(KeyList: TKeyList);
begin
  try
    if (KeyList <> nil) and (KeyList.Header.ItemCount > 0) then
    begin
      try
        // This should be an Object ID, i.e. a number.
        StrToInt(KeyList.Items[0].KeyField1);
      except
        on EConvertError do
          raise ELocationError.CreateNonCritical(ResStr_NoBoundarySelected);
      end;
      RemoveExternalFile;
      // Expected MapSheetKey returned through TableName property.
      FMapSheetKey      := KeyList.Header.TableName;
      cmbMapFile.ItemIndex := cmbMapFile.Items.IndexOf(BoundaryMapFileName);

      if cmbMapFile.ItemIndex = -1 then
        RefreshObjectIDCombo(nil)
      else
        RefreshObjectIDCombo(TMapSheet(cmbMapFile.Items.Objects[cmbMapFile.ItemIndex]));

      SetcmbGISObjectID(KeyList.Items[0].KeyField1);

      if SpatialRef.DisplayRef = '' then
      begin
        SpatialRef.EnteredRef := KeyList.Items[0].KeyField2;
        SpatialRef.DisplayRef := KeyList.Items[0].KeyField2;
        SpatialRef.Qualifier  := ResStr_InternalMap;
      end;
    end;
  finally
    KeyList.Free;
  end;
end;  // UpdateBoundary

//==============================================================================
procedure TfrmLocationDetails.sgRelationsClick(Sender: TObject);
begin
  inherited;
  if (EditMode = emView) or (FRelationList.ItemCount = 0) then begin
    bbRelationDel.Enabled := False;
    cmbRelation.Visible   := False;
  end else
    with TRelationItem(sgRelations.Objects[0,sgRelations.Row]) do begin
      EnableDetailsAddEditButtons(TN_LOCATION_RELATION, 'Location_Relation_Key',
          ItemKey, Custodian, EditMode, Added, nil, bbRelationDel, True);
      cmbRelation.Visible   := False;
    end;
end;  // sgRelationsClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.sgRelationsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var lDataItem:TRelationItem;
begin
  inherited;
  with sgRelations do begin
    if (EditMode<>emView) and (FRelationList.ItemCount>0) and
       (ACol=1) and (ARow=Row) then begin
      lDataItem:=TRelationItem(Objects[0,ARow]);
      if (gdFocused in State) and (bbRelationDel.Enabled or (lDataItem.ItemKey='')) then begin
        cmbRelation.Left     :=Rect.Left+Left+1;
        cmbRelation.Top      :=Rect.Top+Top+1;
        cmbRelation.Width    :=Rect.Right-Rect.Left+2;
        cmbRelation.ItemIndex:=cmbRelation.Items.IndexOf(Cells[Col,Row]);
        cmbRelation.Visible  :=True;
      end;

      if (gdFocused in State) and (Col<>1) then cmbRelation.Visible:=False;
    end;
    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
  end;
end;  // sgRelationsDrawCell

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.sgRelationsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var lRelationItem : TRelationItem;
begin
  inherited;
  //Update list
  if ACol=1 then begin
    lRelationItem:=TRelationItem(sgRelations.Objects[0,sgRelations.Row]);
    lRelationItem.Relationship:= Value;
  end;
end;  // sgRelationsSetEditText

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbRelationAddClick(Sender: TObject);
var lNewRelation : TRelationItem;
  lKey : string;
  lFind : TdlgFind;
begin
  inherited;
  lFind := TdlgFind.CreateDialog(nil,ResStr_FindLocation,ftlocation);
  with lFind do begin
    try
      SetSearchText('');
      if not eSearchText.NoSourceItems then begin
        if ShowModal=mrOk then begin
          lKey:=ItemKey;
          lNewRelation:= TRelationItem.CreateNew(FRelationList);
          // Get Preferred location
          lNewRelation.Location2Name:=dmGeneralData.GetLocationNameFromLocNameKey(lKey);
          lNewRelation.Location2Key :=lKey;
          lNewRelation.Relationship :=cmbRelation.Items[0];
          FRelationList.AddNew(lNewRelation);
        end;
      end else begin
        MessageDlg(ResStr_NoLocationItems, mtInformation, [mbOK], 0);
      end;
    finally
      Free;
    end;
  end;
  sgRelationsClick(nil);
end;  // bbRelationAddClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbRelationDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteRelation, mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    FRelationList.DeleteItem(sgRelations.Row);
    sgRelationsClick(nil);
  end;
end;  // bbRelationDelClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.cmbRelationChange(Sender: TObject);
begin
  inherited;
  with sgRelations do Cells[1,Row]:=cmbRelation.Text;
  sgRelationsSetEditText(Self, sgRelations.Col, sgRelations.Row, cmbRelation.Text);
end;  // cmbRelationChange

//==============================================================================
procedure TfrmLocationDetails.sgUsesClick(Sender: TObject);
begin
  inherited;
  with sgUses do
    FCurrentUse:=TUseItem(Objects[0,Row]);
  if FCurrentUse=nil then begin
    bbUseEdit.Enabled:=False;
    bbUseDel.Enabled :=False;
    BlankUse;
  end else
  //Populate the details fields with information from the list
    with FCurrentUse do begin
      eUse.Text:= Use;
      eUseFrom.Text:= FromDate;
      eUseTo.Text:= ToDate;

      //Comment - rich edit
      Comment.Position:= 0;
      reUseComments.Lines.LoadFromStream(Comment);

      //Potential - rich edit
      Potential.Position:= 0;
      reUsePotential.Lines.LoadFromStream(Potential);

      EnableDetailsAddEditButtons(TN_LOCATION_USE, 'Location_Use_Key', ItemKey,
          Custodian, EditMode, Added, bbUseEdit, bbUseDel, True);
    end;
end;  // sgUsesClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.BlankUse;
begin
  eUse.Text    :='';
  eUseFrom.Text:='';
  eUseTo.Text  :='';
  reUsePotential.Clear;
  reUseComments.Clear;
end;  // BlankUse

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.SaveUse;
begin
  with FCurrentUse do begin
    Use     :=eUse.Text;
    FromDate:=eUseFrom.Text;
    ToDate  :=eUseTo.Text;

    //Comment - rich edit
    Comment.Position:= 0;
    reUseComments.Lines.SaveToStream(Comment);

    //Potential - rich edit
    Potential.Position:= 0;
    reUsePotential.Lines.SaveToStream(Potential);
  end;
  sgUses.Refresh;
end;  // SaveUse

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.SetUseColour(const tfDetailsOn:Boolean);
begin
  SetRequiredFieldsColourState(tfDetailsOn,[eUse,eUseFrom]);
end;  // SetUseColour

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbUseAddClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgUses, bbUseAdd, bbUseEdit, bbUseDel,
                  bbSave, bbCancel, gbUseDetails, True);
  SetUseColour(True);
  FCurrentUse:=TUseItem.CreateNew(FUseList);
  BlankUse;
  FAddItem:=True;
end;  // bbUseAddClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbUseEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgUses, bbUseAdd, bbUseEdit, bbUseDel,
                  bbSave, bbCancel, gbUseDetails, True);
  SetUseColour(True);
  FAddItem:=False;
end;  // bbUseEditClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbUseDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteUse,
                mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    FUseList.DeleteItem(sgUses.Row);
    sgUsesClick(nil);
  end;
end;  // bbUseDelClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbUseAcceptClick(Sender: TObject);
begin
  inherited;
  // Sender is nil if called from CloseQuery method.
  if Sender=nil then begin
    ValidateUseFromDate;
    ValidateUseToDate;
  end;

  ValidateValue(eUse.Text<>'',ResStr_UseNameRequired,eUse);
  ValidateValue(eUseFrom.Text<>'',ResStr_StartDateRequired,eUseFrom);

  SaveUse;
  if FAddItem then begin
    FUseList.AddNew(FCurrentUse);
    sgUses.Row:=sgUses.RowCount-1;
  end;
  SwitchToDetails(sgUses, bbUseAdd, bbUseEdit, bbUseDel,
                  bbSave, bbCancel, gbUseDetails, False);
  SetUseColour(False);
  sgUsesClick(nil);
end;  // bbUseAcceptClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbUseDiscardClick(Sender: TObject);
begin
  inherited;
  if FAddItem then FCurrentUse.Free;
  SwitchToDetails(sgUses, bbUseAdd, bbUseEdit, bbUseDel,
                  bbSave, bbCancel, gbUseDetails, False);
  SetUseColour(False);
  sgUsesClick(nil);
end;  // bbUseDiscardClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.eUseFromExit(Sender: TObject);
var lPos:TPoint;
begin
  inherited;
  lPos:=bbUseDiscard.ScreenToClient(Mouse.CursorPos);
  if not (FClosingForm or
          ((lPos.X in [0..bbUseDiscard.Width]) and (lPos.Y in [0..bbUseDiscard.Height]))) then
    ValidateUseFromDate;
end;  // eUseFromExit

procedure TfrmLocationDetails.ValidateUseFromDate;
begin
  if eUseFrom.Text<>'' then begin
    ValidateValue(IsVagueDate(eUseFrom.Text),ResStr_InvalidVagueDate,eUseFrom);
    ValidateValue(CheckVagueDate(eUseFrom.Text),InvalidDate(ResStr_StartDate,True,False),eUseFrom);
    eUseFrom.Text:=VagueDateToString(eUseFrom.VagueDate);
  end;
end;  // ValidateUseFromDate

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.eUseToExit(Sender: TObject);
var lPos:TPoint;
begin
  inherited;
  lPos:=bbUseDiscard.ScreenToClient(Mouse.CursorPos);
  if not (FClosingForm or
          ((lPos.X in [0..bbUseDiscard.Width]) and (lPos.Y in [0..bbUseDiscard.Height]))) then
    ValidateUseToDate;
end;  // eUseToExit

procedure TfrmLocationDetails.ValidateUseToDate;
begin
  if eUseTo.Text<>'' then begin
    ValidateValue(IsVagueDate(eUseTo.Text),ResStr_InvalidVagueDate,eUseTo);
    ValidateValue(CheckVagueDate(eUseTo.Text),InvalidDate(ResStr_EndDate,True,True),eUseTo);
    eUseTo.Text:=VagueDateToString(eUseTo.VagueDate);
    ValidateValue(not IsVagueDateInVagueDate(eUseTo.VagueDate, eUseFrom.VagueDate),
                  ResStr_StartContainEndDate,eUseTo);
    ValidateValue(not IsVagueDateInVagueDate(eUseFrom.VagueDate, eUseTo.VagueDate),
                  ResStr_EndContainStartDate,eUseTo);
    ValidateValue(CompareVagueDateToVagueDate(eUseTo.VagueDate,eUseFrom.VagueDate) >= 0,
                  ResStr_EndBeforeStartDate,eUseTo);
  end;
end;  // eUseTo

//==============================================================================
procedure TfrmLocationDetails.sgTenureClick(Sender: TObject);
begin
  inherited;
  with sgTenure do
    FCurrentTenure:=TTenureItem(Objects[0,Row]);
  if FCurrentTenure=nil then begin
    bbTenureEdit.Enabled:=False;
    bbTenureDel.Enabled :=False;
    BlankTenure;
  end else
  //Populate the details fields with information from the list
    with FCurrentTenure do begin
      eTenureName.Text       := OwnerName;
      eTenureName.Key        := OwnerKey;
      cmbTenureType.KeyValue := TypeKey;
      eTenureFrom.Text       := FromDate;
      eTenureTo.Text         := ToDate;
      EnableDetailsAddEditButtons(TN_TENURE, 'Tenure_Key', ItemKey,
          Custodian, EditMode, Added, bbTenureEdit, bbTenureDel, True);
    end;
end;  // sgTenureClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.BlankTenure;
begin
  eTenureName.Text      :='';
  cmbTenureType.KeyValue:='';
  eTenureFrom.Text      :='';
  eTenureTo.Text        :='';
end;  // BlankTenure

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.SaveTenure;
begin
  with FCurrentTenure do begin
    OwnerName:=eTenureName.Text;
    OwnerKey :=eTenureName.Key;
    TypeKey  :=cmbTenureType.KeyValue;
    TypeName :=cmbTenureType.Text;
    FromDate :=eTenureFrom.Text;
    ToDate   :=eTenureTo.Text;
  end;
  sgTenure.Refresh;
end;  // SaveTenure

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.SetTenureColour(const tfDetailsOn:Boolean);
begin
  SetRequiredFieldsColourState(tfDetailsOn,[eTenureName,cmbTenureType,eTenureFrom]);
end;  // SetTenureColour

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbTenureAddClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgTenure, bbTenureAdd, bbTenureEdit, bbTenureDel,
                  bbSave, bbCancel, gbTenureDetails, True);
  SetTenureColour(True);
  FCurrentTenure:= TTenureItem.CreateNew(FTenureList);
  BlankTenure;
  FAddItem:= True;
end;  // bbTenureAddClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbTenureEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgTenure, bbTenureAdd, bbTenureEdit, bbTenureDel,
                  bbSave, bbCancel, gbTenureDetails, True);
  SetTenureColour(True);
  FAddItem:= False;
end;  // bbTenureEditClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbTenureDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteTenure,
                mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    FTenureList.DeleteItem(sgTenure.Row);
    sgTenureClick(nil);
  end;
end;  // bbTenureDelClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbTenureAcceptClick(Sender: TObject);
begin
  inherited;
  // Sender is nil if called from CloseQuery method.
  if Sender=nil then begin
    ValidateTenureFromDate;
    ValidateTenureToDate;
  end;

  ValidateValue(eTenureName.Text<>'',ResStr_TenureNameRequired,eTenureName);
  ValidateValue(dmGeneralData.CheckName(eTenureName),ResStr_InvalidTenureName,eTenureName);
  ValidateValue(cmbTenureType.Text<>'',ResStr_TenureTypeRequired,cmbTenureType);
  ValidateValue(eTenureFrom.Text<>'',ResStr_StartdateRequiredForTenure,eTenureFrom);

  SaveTenure;
  if FAddItem then begin
    FTenureList.AddNew(FCurrentTenure);
    sgTenure.Row:=sgTenure.RowCount-1;
  end;
  SwitchToDetails(sgTenure, bbTenureAdd, bbTenureEdit, bbTenureDel,
                  bbSave, bbCancel, gbTenureDetails, False);
  SetTenureColour(False);
  sgTenureClick(nil);
end;  // bbTenureAcceptClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.bbTenureDiscardClick(Sender: TObject);
begin
  inherited;
  if FAddItem then FCurrentTenure.Free;
  SwitchToDetails(sgTenure, bbTenureAdd, bbTenureEdit, bbTenureDel,
                  bbSave, bbCancel, gbTenureDetails, False);
  SetTenureColour(False);
  sgTenureClick(nil);
end;  // bbTenureDiscardClick

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.UpdateTenure(KeyList:TKeyList);
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then begin
      eTenureName.Text:=dmGeneralData.GetName(KeyList.Items[0].KeyField1);
      eTenureName.Key:=KeyList.Items[0].KeyField1;
    end;
  finally
    KeyList.Free;
  end;
end;  // UpdateTenure

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.eTenureFromExit(Sender: TObject);
var lPos:TPoint;
begin
  inherited;
  lPos:=bbTenureDiscard.ScreenToClient(Mouse.CursorPos);
  if not (FClosingForm or
          ((lPos.X in [0..bbTenureDiscard.Width]) and (lPos.Y in [0..bbTenureDiscard.Height]))) then
    ValidateTenureFromDate;
end;  // eTenureFromExit

{-------------------------------------------------------------------------------
}
procedure TfrmLocationDetails.ValidateTenureFromDate;
begin
  if eTenureFrom.Text<>'' then begin
    ValidateValue(IsVagueDate(eTenureFrom.Text),ResStr_InvalidVagueDate, eTenureFrom);
    ValidateValue(CheckVagueDate(eTenureFrom.Text),
                  InvalidDate(ResStr_StartDate,True,False),eTenureFrom);
    eTenureFrom.Text:=VagueDateToString(eTenureFrom.VagueDate);
  end;
end;  // eTenureFromExit

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.eTenureToExit(Sender: TObject);
var lPos:TPoint;
begin
  inherited;
  lPos:=bbTenureDiscard.ScreenToClient(Mouse.CursorPos);
  if not (FClosingForm or
          ((lPos.X in [0..bbTenureDiscard.Width]) and (lPos.Y in [0..bbTenureDiscard.Height]))) then
    ValidateTenureToDate;
end;  // eTenureToExit

procedure TfrmLocationDetails.ValidateTenureToDate;
begin
  if eTenureTo.Text<>'' then begin
    ValidateValue(IsVagueDate(eTenureTo.Text),ResStr_InvalidVagueDate, eTenureTo);
    ValidateValue(CheckVagueDate(eTenureTo.Text),
                  InvalidDate(ResStr_EndDate,True,True),eTenureTo);
    eTenureTo.Text:=VagueDateToString(eTenureTo.VagueDate);
    ValidateValue(not IsVagueDateInVagueDate(eTenureTo.VagueDate, eTenureFrom.VagueDate),
                  ResStr_StartContainEndDate, eTenureTo);
    ValidateValue(not IsVagueDateInVagueDate(eTenureFrom.VagueDate, eTenureTo.VagueDate),
                  ResStr_EndContainStartDate ,eTenureTo);
    ValidateValue(CompareVagueDateToVagueDate(eTenureTo.VagueDate,eTenureFrom.VagueDate) >= 0,
                  ResStr_EndBeforeStartDate,eTenureTo);
  end;
end;  // eTenureToExit

//==============================================================================
procedure TfrmLocationDetails.WMTransferDone(var Msg: TMessage);
begin
  if DrillForm<>nil then TfrmLocations(DrillForm).Show;
end;  // WMTransferDone

//==============================================================================
procedure TfrmLocationDetails.SetDrillForm(const Value: TBaseForm);
begin
  FDrillForm := Value;
end;  // SetDrillForm

//==============================================================================
procedure TfrmLocationDetails.ComboBoxExit(Sender: TObject);
begin
  inherited;
  TComboBox(Sender).Visible:=False;
end;  // ComboBoxExit

//==============================================================================
procedure TfrmLocationDetails.UpdateRTFMenu;
begin
  if DrillForm<>nil then
    dmFormActions.UpdateRTFMenu(((DrillForm.ActiveControl is TDBRichEdit) and
                               (FdmLocationDetails.qryLocation.State in [dsEdit,dsInsert])) or
                              (DrillForm.ActiveControl is TRichEdit))
  else
    dmFormActions.UpdateRTFMenu(False);
end;  // UpdateRTFMenu

//==============================================================================
procedure TfrmLocationDetails.EnterRTF(Sender: TObject);
begin
  inherited;
  if Sender is TRichEdit then
    dmFormActions.UpdateRTFMenu(not TRichEdit(Sender).ReadOnly)
  else if Sender is TDBRichEdit then
    dmFormActions.UpdateRTFMenu(FdmLocationDetails.qryLocation.State in [dsEdit,dsInsert])
  else
    dmFormActions.UpdateRTFMenu(True);
end;  // EnterRTF

//==============================================================================
procedure TfrmLocationDetails.ExitRTF(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(False);
end;  // ExitRTF

//==============================================================================
// ON EXIT FOR SPATIAL REF
{ Validates value, saves it in its original, but formatted form to the
  FEnteredSR field and converts it }
procedure TfrmLocationDetails.SpatialRefExit(Sender: TObject);
var ltfDisplayNearby : Boolean;
    lKey       : TKeyString;
    lText      : String;
    lFind      : TdlgFind;
    lCentralLatLong : TLatLong;
    lSavePos,lCancelPos : TPoint;
begin
  inherited;
  lSavePos  :=bbSave.ScreenToClient(Mouse.CursorPos);
  lCancelPos:=bbCancel.ScreenToClient(Mouse.CursorPos);

  // Check only if not clicked Cancel
  if not ((lCancelPos.X in [0..bbCancel.Width]) and (lCancelPos.Y in [0..bbCancel.Height])) then
    // check for locations within 2km of newly entered one.
    if FdmLocationDetails.qryLocation.State = dsInsert then
    begin
      ltfDisplayNearby := False;
      if SpatialRef.DisplayRef <> '' then
      begin
        lCentralLatLong := ConvertToLatLong(SpatialRef.EnteredRef, SpatialRef.EnteredSystem);
        FNearGridRefs.Clear;
        Populate2KMList(lCentralLatLong);
        if FNearGridRefs.Count > 0 then
        begin
          if MessageDlg(ResStr_NearGridRefItems, mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
            lFind := TdlgFind.CreateDialog(nil,ResStr_FindLocationName2km,ftNearGridRef);
            with lFind do
              try
                eSearchText.SearchMode:=smAlways;
                SetSearchText('', True);  // ensure all possible matches are displayed
                if ShowModal=mrOk then begin
                  // Searching is for a location name, so convert to a location key
                  lKey :=dmGeneralData.GetLocKeyFromLocNameKey(ItemKey);
                  lText := ItemText;  // and text
                  ltfDisplayNearby := True;
                end;
              finally
                Free;
              end;
          end;
        end;
        // Nearby location wanted, so cancel this new one and find the selected one
        if ltfDisplayNearby then begin
          bbCancelClick(nil);
          Application.ProcessMessages;  //Ensure WM_DISCARD_LOCATION has been processed
          TfrmLocations(DrillForm).LocateNode(lText, lKey, True);
        end else
        // Don't want nearby location, so save if Save clicked
        if (lSavePos.X in [0..bbSave.Width]) and (lSavePos.Y in [0..bbSave.Height]) then
          bbSaveClick(nil);
      end;
    end;
end;  // SpatialRefExit

//------------------------------------------------------------------------------
procedure TfrmLocationDetails.SpatialRefGetFromMap(Sender: TObject);
begin
  inherited;
  FExpectingSpatialRef := True;
  dmFormActions.actMapWindow.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild),Self,UpdateSpatialRef);
end;  // SpatialRefGetFromMap

//------------------------------------------------------------------------------

{ When "Returning Data" from the map. }

procedure TfrmLocationDetails.UpdateSpatialRef(KeyList:TKeyList);
begin
  try
    if (KeyList <> nil)  and (KeyList.Header.ItemCount>0) then
    begin
      If (CompareText(KeyList.Header.TableName, 'SPATIAL_REF') = 0) then begin
        // use the last item in the list - this is the exact reference
        if KeyList.Items[KeyList.Header.ItemCount-1].KeyField2 <> ResStr_BeyondSystemLimits then
        begin
          SpatialRef.Values := dmGeneralData.SetSpatialRef(
              KeyList.Items[KeyList.Header.ItemCount-1].KeyField2,
              '',
              AppSettings.SpatialRefSystem);
        end else
          raise ELocationDetailsError.CreateNonCritical(ResStr_OutsideSystem);
      end;
    end;
  finally
    KeyList.Free;
  end; // try..finally
end;  // UpdateSpatialRef

//==============================================================================
procedure TfrmLocationDetails.pcDetailsChanging(Sender: TObject; var AllowChange: Boolean);
begin
  inherited;
  AllowChange:=not FEditDetails;
end;  // pcDetailsChanging

//==============================================================================
procedure TfrmLocationDetails.SetupDestinationControls;
begin
  RegisterDropComponent(eDesAuthority, DropAuthorityName,
                        [TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION],
                        [CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(sgAdminAreas, DropAdminArea,
                        [TN_ADMIN_AREA], [CF_JNCCDATA]);
  RegisterDropComponent(sgRelations, DropRelation, [TN_LOCATION], [CF_JNCCDATA]);
  RegisterDropComponent(eTenureName, DropTenure,
                        [TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION],
                        [CF_JNCCDATA, CF_TEXT]);
  if SpatialRef<>nil then
    RegisterDropComponent(SpatialRef.ControlSpatialRef, DropLocation,
                          ['SPATIAL_REF'], [CF_JNCCDATA, CF_TEXT]);
end;  // SetupDestinationControls

//==============================================================================
procedure TfrmLocationDetails.DropAuthorityName(const Sender: TObject;
  const iFormat : Integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : Boolean);
begin
  if (EditMode <> emView) and FEditDetails then
    ioHandled := dmGeneralData.DropLinkedEditText(
        eDesAuthority,
        iFormat,
        iSourceData,
        dmGeneralData.GetName,
        iTextStrings)
  else
    ioHandled := True;
end;  // DropAuthorityName

//==============================================================================
procedure TfrmLocationDetails.DropAdminArea(const Sender: TObject;
  const iFormat : Integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : Boolean);
var
  lNewAdminArea : TAdminAreaItem;
  lTempList : TStringList;
begin
  if (EditMode<>emView) and ((AppSettings.UserAccessLevel>ualAddOnly) or
                             (FdmLocationDetails.qryLocation.State=dsInsert)) then
  begin
    if iSourceData.Header.ItemCount>0 then
      if iFormat=CF_JNCCDATA then begin
        ioHandled:= True;
        if not CheckExistence(sgAdminAreas,iSourceData.Items[0].KeyField1) then begin
          lNewAdminArea:= TAdminAreaItem.CreateNew(FAdminAreaList);
          with lNewAdminArea do begin
            AdminAreaKey:= iSourceData.Items[0].KeyField1;

            //Find display fields
            lTempList:= TStringList.Create;
            try
              dmGeneralData.GetRecordStrings(lTempList, TN_ADMIN_AREA, AdminAreaKey);
              if lTempList.Values['SHORT_CODE'] = '' then
                Name:= lTempList.Values['ITEM_NAME']
              else
                Name:= lTempList.Values['SHORT_CODE'] + ', ' + lTempList.Values['ITEM_NAME'];
              dmGeneralData.GetRecordStrings(lTempList, 'ADMIN_TYPE', lTempList.Values['ADMIN_TYPE_KEY']);
              AdminAreaType:= lTempList.Values['SHORT_NAME'];
            finally
              lTempList.Free;
            end;
          end;
          FAdminAreaList.AddNew(lNewAdminArea);
        end;
      end;
  end else
    ioHandled:=True;
  sgAdminAreasClick(nil);
end;  // DropAdminArea

//==============================================================================
procedure TfrmLocationDetails.DropRelation(const Sender: TObject;
  const iFormat: Integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: Boolean);
var
  lNewRelation : TRelationItem;
  lKey : string;
begin
  if (EditMode<>emView) and ((AppSettings.UserAccessLevel>=ualAddOnly) or
                             (FdmLocationDetails.qryLocation.State=dsInsert)) then
  begin
    if iSourceData.Header.ItemCount>0 then
      if iFormat=CF_JNCCDATA then begin
        ioHandled:= True;
        lKey:=iSourceData.Items[0].KeyField1;
        if not CheckExistence(sgRelations,lKey) then begin
          lNewRelation:= TRelationItem.CreateNew(FRelationList);
          lNewRelation.Location2Name:=dmGeneralData.GetLocationName(lKey);
          lNewRelation.Location2Key :=lKey;
          lNewRelation.Relationship :=cmbRelation.Items[0];
          FRelationList.AddNew(lNewRelation);
          sgRelations.Row:=sgRelations.RowCount-1;
          sgRelationsClick(nil);
        end;
      end;
  end else
    ioHandled:=True;
end;  // DropRelation

//==============================================================================
procedure TfrmLocationDetails.DropTenure(const Sender: TObject;
  const iFormat: Integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: Boolean);
begin
  if (EditMode <> emView) and FEditDetails then
    ioHandled := dmGeneralData.DropLinkedEditText(
        eTenureName,
        iFormat,
        iSourceData,
        dmGeneralData.GetName,
        iTextStrings)
  else
    ioHandled := True;
end;  // DropTenure

//==============================================================================
procedure TfrmLocationDetails.pnlDetailsResize(Sender: TObject);
begin
  inherited;
  lblLocationDisp.Caption := GetTextWithinLimit(
      Canvas, lblPreferredName.Caption, pnlDetails.Width - lblLocationDisp.Left - 8);

  SetPanelSize;
end;

{-------------------------------------------------------------------------------
  Sets the size of the main panel and its contents.
}
procedure TfrmLocationDetails.SetPanelSize;
var
  deltaWidth, deltaHeight: Integer;
begin
  // Works out the change in height and width of the panel.
  deltaWidth  := pnlDetails.Width - pnlInner.Width;
  deltaHeight := pnlDetails.Height - pnlInner.Height;

  // For some reason, the automatic resizing doesn't work properly. Using a
  // second panel within the main panel and sizing it manually solves this, but
  // is a bit of a hack.
  pnlInner.Width  := pnlDetails.Width;
  pnlInner.Height := pnlDetails.Height;

  // Custom controls do not have anchor properties and must be resized manually.
  eDesAuthority.Width := eDesAuthority.Width + deltaWidth;
  eTenureName.Width   := eTenureName.Width + deltaWidth;
  Sources.Width       := Sources.Width + deltaWidth;
  Sources.Height      := Sources.Height + deltaHeight;
  Measurements.Width  := Measurements.Width + deltaWidth;
  Measurements.Height := Measurements.Height + deltaHeight;
  SpatialRef.Width    := SpatialRef.Width + deltaWidth;

  ResizeApproach;
end;  // scbLocationDetailsResize

{-------------------------------------------------------------------------------
  When the approach tab sheet is shown, sizes its items to the correct dimensions.
}
procedure TfrmLocationDetails.tsApproachShow(Sender: TObject);
begin
  ResizeApproach;
end;

{-------------------------------------------------------------------------------
  Sets up the size of the items in the Approach tab sheet.
}
procedure TfrmLocationDetails.ResizeApproach;
var
  availableHeight, halfHeight: Integer;
begin
  availableHeight         := tsApproach.Height
                            - 16 * 2   // Labels
                            - 4 * 3;   // Other gaps.

  halfHeight              := availableHeight div 2;
  dbreApproach.Height     := halfHeight;
  lblAccessRestrict.Top   := dbreApproach.Top + dbreApproach.Height + 4;
  dbreRestrictions.Top    := lblAccessRestrict.Top + 16;
  dbreRestrictions.Height := halfHeight;
end;

//==============================================================================
{ Populates a KeyList to return data to other forms with the
  Table Name :'LOCATION'
  KeyField1 : Location_Key }
function TfrmLocationDetails.GetKeyList: TKeyList;
var lNewKeyList : TEditableKeyList;
begin
  //Return an editable key list with the selected nodes key
  lNewKeyList:= TEditableKeyList.Create;
  lNewKeyList.SetTable(TN_LOCATION);
  lNewKeyList.AddItem(LocationKey,'');
  Result:= lNewKeyList;
end;  // GetKeyList

//==============================================================================
function TfrmLocationDetails.CheckGridSquares:Boolean;
var lCount: Integer;
begin
  Result := True;
  if FGridSquareList.ItemCount > 0 then
    with sgGridSquares do
      for lCount := RowCount - 1 downto FixedRows do
        Result := Result
            and (TGridSquareItem(Objects[0, lCount]).EnteredRef <> '')
            and (TGridSquareItem(Objects[0, lCount]).SpatialRefSizeName <> '')
            and (Cols[0].IndexOf(Cells[0, lCount]) = lCount);  // Check square appears only once
end;  // CheckGridSquares

//==============================================================================
function TfrmLocationDetails.CheckRefInParent:Boolean;
var lCurrentLocKey:TKeyString;
begin
  Result:=False;
  if ParentKey='' then
    Result:=True
  else with FdmLocationDetails.qryGridSquares do
  begin
    lCurrentLocKey := Parameters.ParamByName('Key').Value;
    Parameters.ParamByName('Key').Value:= ParentKey;
    Open;
    try
      if Eof then
        Result:=True
      else begin
        { Need to do check }
        First;
        while not Eof do begin
          if CheckSRefInSRef(FieldByName('SPATIAL_REF').AsString,
                             SpatialRef.EnteredRef,
                             FieldByName('SPATIAL_REF_SYSTEM').AsString,
                             SpatialRef.EnteredSystem) then
          begin
            Result:= True;  // Found at least one good square
            Break; // data loop
          end;
          Next;
        end;  // while
      end;
    finally
      Close;
      { Reset the key value }
      Parameters.ParamByName('Key').Value:= lCurrentLocKey;
    end; // try
  end; // with
end;  // CheckRefInParent

//==============================================================================
function TfrmLocationDetails.CheckRefInSquares:Boolean;
var
  lCount: Integer;
  item: TGridSquareItem;
begin
  Result := False;
  if FGridSquareList.ItemCount = 0 then
    Result := True
  else
    with sgGridSquares do
      for lCount := RowCount - 1 downto FixedRows do begin
        item := TGridSquareItem(Objects[0, lCount]);
        // Disable checking if a square exists for addin reference systems, since
        // we don't know the size of the square so can't be sure
        if AppSettings.ComAddins.SpatialSystems.IndexOf(item.SpatialRefSystem) = -1 then
        begin
          if CheckSRefInSRef(item.EnteredRef,
                             SpatialRef.EnteredRef,
                             item.SpatialRefSystem,
                             SpatialRef.EnteredSystem) then
          begin
            Result := True;  // Found at least one good square
            Break; // data loop, not FOR loop
          end;
        end else begin
          // addin square present, so we have to assume it might be Ok
          Result := True;
          Break;
        end;
      end;
end;  // CheckRefInSquares

//==============================================================================
{ Validate that any grid squares are within those specified for the parent
     location if they exist }
function TfrmLocationDetails.CheckSquaresInParent:Boolean;
var lCount : Integer;
    lSquareValid : Boolean;
    lCurrentLocKey : TKeyString;
begin
  Result := True;
  if ParentKey <> '' then with FdmLocationDetails.qryGridSquares do
  begin
    lCurrentLocKey := Parameters.ParamByName('Key').Value;
    Parameters.ParamByName('Key').Value := ParentKey;
    Open;
    try
      if (not Eof) and (FGridSquareList.ItemCount > 0) then begin
        for lCount := sgGridSquares.RowCount - 1 downto sgGridSquares.FixedRows do
        begin
          First;
          lSquareValid := False;
          while not Eof do begin // loop dataset checking for a hit
            if CheckSRefInSRef(FieldByName('SPATIAL_REF').AsString,
                               TGridSquareItem(sgGridSquares.Objects[0, lCount]).EnteredRef,
                               FieldByName('SPATIAL_REF_SYSTEM').AsString,
                               TGridSquareItem(sgGridSquares.Objects[0, lCount]).SpatialRefSystem) then
            begin
              lSquareValid := True;  // Found at least one good square
              Break; // while loop, not FOR loop
            end;
            Next;
          end;  // while
          if not lSquareValid then begin
            Result := False;
            Break; // from for loop - have a failure, no need to continue
          end;
        end;  // for
      end; // if need to do check
    finally
      Close;
      { Reset the key value }
      Parameters.ParamByName('Key').Value:= lCurrentLocKey;
    end; // try
  end; // with
end;  // CheckSquaresInParent

//==============================================================================
function TfrmLocationDetails.CheckLandParcels:Boolean;
var lCount : Integer;
begin
  Result:=True;
  if FLandParcelList.ItemCount>0 then
    with sgLandParcels do
      for lCount:=FixedRows to RowCount-1 do
        Result:=Result and (TLandParcelItem(Objects[0,lCount]).Number<>'') and
                           (TLandParcelItem(Objects[0,lCount]).MapSystem<>'');
end;  // CheckLandParcels

//==============================================================================
function TfrmLocationDetails.CheckExistence(AGrid:TStringGrid; const AKey:TKeyString):Boolean;
var lCount : Integer;
begin
  Result:=False;
  if AGrid=sgAdminAreas then begin
    if FAdminAreaList.ItemCount>0 then
      with AGrid do
        for lCount:=FixedRows to RowCount-1 do
          Result:=Result or (TAdminAreaItem(Objects[0,lCount]).AdminAreaKey=AKey);
  end else begin
    if FRelationList.ItemCount>0 then
      with AGrid do
        for lCount:=FixedRows to RowCount-1 do
          Result:=Result or (TRelationItem(Objects[0,lCount]).Location2Key=AKey);
  end;
end;  // CheckExistence

//==============================================================================
procedure TfrmLocationDetails.Populate2KMList(iLatLong: TLatLong);
var
  lSWLatLong, lNELatLong : TLatLong; // bounding box on which to search
  lEastNorth, lSWEastNorth, lNEEastNorth : TMapCoord;
  lLocationKey : TKeyString;
  lNearGridRef : TKeyData;
  lFormatSettings: TFormatSettings;
const
  NEAR_GAP = 2000; // 2km
begin
  // get a position in metres for the location
  lEastNorth := ConvertLatLongToDummyEastNorth(iLatLong.Long,
                                                iLatLong.Lat);
  // find bounding box 2km around centre - ie total 4km wide
  lSWEastNorth.x := lEastNorth.x - NEAR_GAP;
  lSWEastNorth.y := lEastNorth.y - NEAR_GAP;
  lNEEastNorth.x := lEastNorth.x + NEAR_GAP;
  lNEEastNorth.y := lEastNorth.y + NEAR_GAP;
  // now convert to a latlong bounding box
  lSWLatLong := ConvertDummyEastNorthToLatLong(lSWEastNorth);
  lNELatLong := ConvertDummyEastNorthToLatLong(lNEEastNorth);
  // and setup a query to detect locations in this box
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, lFormatSettings);
  lFormatSettings.DecimalSeparator := '.';

  with dmDatabase.ExecuteSQL('SELECT Location_Key FROM Location ' +
                             'WHERE Lat>=' + FloatToStr(lSWLatLong.Lat, lFormatSettings) +
                             ' AND Lat<=' + FloatToStr(lNELatLong.Lat, lFormatSettings) +
                             ' AND Long>=' + FloatToStr(lSWLatLong.Long, lFormatSettings) +
                             ' AND Long<=' + FloatToStr(lNELatLong.Long, lFormatSettings),
                             True) do
  begin
    try
      while not Eof do begin
        lLocationKey := Fields['LOCATION_KEY'].Value;
        lNearGridRef := TKeyData.Create;
        lNearGridRef.ItemKey := lLocationKey;
        FNearGridRefs.Add(lNearGridRef);
        MoveNext;
      end; // whiie
    finally
      Close;
    end; // try
  end; // with dmDatabase
end;  // Populate2KMList

//==============================================================================
procedure TfrmLocationDetails.RefreshLists;
begin
  dbcmbSiteStatus.Active:=False;
  dbcmbSiteStatus.Active:=True;
  dbcmbLocationType.ListSource.Dataset.Active:=False;
  dbcmbLocationType.ListSource.Dataset.Active:=True;
  cmbTenureType.Active:=False;
  cmbTenureType.Active:=True;
  Measurements.RefreshLists;
end;  // RefreshLists

//==============================================================================
procedure TfrmLocationDetails.SetDisplaySystem;
begin
  SpatialRef.DisplaySystem := AppSettings.SpatialRefSystem;
end;

//==============================================================================
procedure TfrmLocationDetails.pcLocationDetailsChange(Sender: TObject);
begin
  inherited;
  pcLocationDetails.HelpContext := pcLocationDetails.ActivePage.HelpContext;
end;

//==============================================================================
procedure TfrmLocationDetails.pcOtherInfoChange(Sender: TObject);
begin
  inherited;
  tsOtherInfo.HelpContext := pcOtherInfo.ActivePage.HelpContext;
  pcOtherInfo.HelpContext := pcOtherInfo.ActivePage.HelpContext;
  pcLocationDetails.HelpContext := pcOtherInfo.ActivePage.HelpContext;
end;

//==============================================================================
procedure TfrmLocationDetails.pcGeoInfoChange(Sender: TObject);
begin
  inherited;
  tsGeoInfo.HelpContext := pcGeoInfo.ActivePage.HelpContext;
  pcGeoInfo.HelpContext := pcGeoInfo.ActivePage.HelpContext;
  pcLocationDetails.HelpContext := pcGeoInfo.ActivePage.HelpContext;
end;

//==============================================================================
//==============================================================================
{ Called when a location is dropped from the Map onto the SpatialRef Component }
procedure TfrmLocationDetails.DropLocation(const Sender: TObject;
  const iFormat: Integer; const iSourceData: TKeyList;
  const iTextStrings: TstringList; var ioHandled: Boolean);
var
  lKeyList : TKeyList;
begin
  lKeyList := iSourceData;
  if (iSourceData <> nil) and (iSourceData.Header.ItemCount > 0) then
    if (iFormat = CF_JNCCDATA) and
       (iSourceData.Header.TableName = 'SPATIAL_REF') then begin
      { Overwrite the Spatial ref returned from the database with the one
        passed from the map. }
      SpatialRef.Values := dmGeneralData.SetSpatialRef(lKeyList.Items[0].KeyField2,
                            '', AppSettings.SpatialRefSystem);
    end;
end;  // DropLocation

//==============================================================================
procedure TfrmLocationDetails.RefreshColours;
begin
  SetRequiredFieldsColourState(EditMode<>emView,[clbLocationNames, dbcmbLocationType,
                                                 SpatialRef.ControlSpatialRef,
                                                 SpatialRef.ControlQualifier]);
  SetRequiredFieldsColourState(bbDesignationAccept.Enabled,[dbcmbSiteStatus,eDesAuthority]);
  SetRequiredFieldsColourState(bbUseAccept.Enabled,[eUse,eUseFrom]);
  SetRequiredFieldsColourState(bbTenureAccept.Enabled,[eTenureName,cmbTenureType,eTenureFrom]);
  SetRequiredFieldsColourState(bbBoundaryAccept.Enabled,[eBoundaryVersion]);
end;  // RefreshColours

//==============================================================================
{ Update the boundary information on the current detail page }
procedure TfrmLocationDetails.RefreshBoundaries;
begin
  FBoundaryList.Refresh;
  sgBoundariesClick(Self);
end;  // RefreshBoundaries

//==============================================================================
procedure TfrmLocationDetails.RefreshNames;
var lIdx       :Integer;
    lDesItem   :TDesignationItem;
    lTenureItem:TTenureItem;
begin
  // Refresh Designations
  if FCurrentDes <> nil then
  begin
     eDesAuthority.Text   :=dmGeneralData.GetName(FCurrentDes.AuthorityKey);
     FCurrentDes.Authority:=eDesAuthority.Text;
  end;
  with FDesignationList do
    for lIdx:=0 to Count-1 do begin
      lDesItem          := TDesignationItem(Items[lIdx]);
      lDesItem.Authority:=dmGeneralData.GetName(lDesItem.AuthorityKey);
      RefreshItemDisplay(lDesItem);
    end;

  // Refresh Tenures
  if FCurrentTenure <> nil then
  begin
    eTenureName.Text        :=dmGeneralData.GetName(FCurrentTenure.OwnerKey);
    FCurrentTenure.OwnerName:=eTenureName.Text;
  end;
  with FTenureList do
    for lIdx:=0 to Count-1 do begin
      lTenureItem:=TTenureItem(Items[lIdx]);
      lTenureItem.OwnerName:=dmGeneralData.GetName(lTenureItem.OwnerKey);
      RefreshItemDisplay(lTenureItem);
    end;
end;  // RefreshNames

//==============================================================================
procedure TfrmLocationDetails.ShowMetaData;
begin
  with TdlgMetaDataPopup.Create(nil) do
  try
    ShowStandard(ResStr_Location, lblLocationDisp.Caption,
       FdmLocationDetails.qryLocation.FieldByName('Location_Key').AsString,
       FdmLocationDetails.qryLocation);
  finally
    Free;
  end;
end;

//==============================================================================
{ Description : Show the map centred on a location.  Also zoom the map if the
              location has a polygon }
procedure TfrmLocationDetails.FindOnMap;
begin
  TfrmMap.CentreOnLocation(FLocationKey, lblPreferredName.Caption);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmLocationDetails.UpdateMapWindowSelector;
begin
  AppSettings.UpdateMapMenu(Self, pmMapWindow.Items, True, MapForSpatialRefClick);
  AppSettings.UpdateMapMenu(Self, pmMapWindowForBoundary.Items, True, MapForGISObjectClick);
  btnGetGISObject.Enabled         := btnGetGISObject.Enabled and
                                     (AppSettings.AvailableMaps.Count > 0);
  btnGetGISObjectDropDown.Enabled := btnGetGISObject.Enabled;
  SpatialRef.UpdateMapWindowSelector;
end;

//==============================================================================
function TfrmLocationDetails.BoundaryMapFileName: String;
begin
  with dmDatabase.ExecuteSQL(
    'SELECT 	MS2.Dataset_Sheet_Name + '' ('' + MS1.Dataset_Sheet_Name + '')'' AS MapName ' +
    'FROM	Map_Sheet MS1 ' +
    'JOIN	Map_Sheet MS2 ON MS2.Base_Map_Key = MS1.Base_Map_Key AND MS2.Sheet_Type = 0 ' +
    'WHERE	MS1.Map_Sheet_Key = ''' + FMapSheetKey + '''', True) do
  begin
    if not Eof then Result := VarToStr(Fields['MapName'].Value)
               else Result := '';
    Close;
  end;
end;

procedure TfrmLocationDetails.AddBoundary(AMapSheetKey, AObjectStaticID: String);
var
  i: integer;
begin
  bbBoundaryAddClick(nil);
  eBoundaryFrom.Text    := DateToStr(Now);
  eBoundaryVersion.Text := BOUNDARY_VERSION;
  FMapSheetKey          := AMapSheetKey;

  // Find the map sheet with the specified map sheet key.
  FFileExists := False;
  cmbMapFile.ItemIndex := -1;
  RefreshMapFileCombo;
  for i := 0 to cmbMapFile.Items.Count - 1 do
    if TMapSheet(cmbMapFile.Items.Objects[i]).MapSheetKey = AMapSheetKey then begin
      cmbMapFile.ItemIndex := i;
      FFileExists := True;
      Break;
    end;

  RefreshObjectIDCombo(TMapSheet(cmbMapFile.Items.Objects[cmbMapFile.ItemIndex]));

  SetcmbGISObjectID(AObjectStaticID);

  bbBoundaryAcceptClick(nil);
end;

{-------------------------------------------------------------------------------
  Sets cmbCISObjectID to the specified objectID. If the objectID is not found, adds
  it to the list and then selects it.
}
procedure TfrmLocationDetails.SetcmbGISObjectID(objectID: string);
begin
  cmbGISObjectID.ItemIndex := cmbGISObjectID.Items.IndexOf(objectID);

  // If the ItemIndex is -1, the item was not in the list, so add and select it.
  if cmbGISObjectID.ItemIndex = -1 then begin
    cmbGISObjectID.Items.Add(objectID);
    cmbGISObjectID.ItemIndex := cmbGISObjectID.Items.Count - 1;
    FObjectIDExists := False;
  end else
    FObjectIDExists := True;
end;

procedure TfrmLocationDetails.SpatialRefInvalidSpatialRef(Sender: TObject; var Handled: Boolean);
var
  lPos: TPoint;
begin
  lPos := bbCancel.ScreenToClient(Mouse.CursorPos);

  // Check if clicked Cancel
  Handled := (lPos.X in [0..bbCancel.Width]) and (lPos.Y in [0..bbCancel.Height]);
end;

{-------------------------------------------------------------------------------
  Show a popup menu when the user clicks Extract Grid Squares button
}
procedure TfrmLocationDetails.btnExtractGridSquaresClick(Sender: TObject);
begin
  inherited;
  with TdlgExtractGridSquares.Create(nil) do
    try
      pcLocationDetails.Enabled := False;
      bbSave.Enabled            := False;
      sgGridSquares.Visible     := False; // displays panel so grid doesn't flicker

      BoundaryList   := FBoundaryList;
      GridSquareList := FGridSquareList;
      if ShowModal = mrOK then
        FGridSquareExtractor := Extractor;
    finally
      Free;
      
      pcLocationDetails.Enabled := True;
      bbSave.Enabled            := True;
      sgGridSquares.Visible     := True;
    end; // try
end;

{-------------------------------------------------------------------------------
}
procedure TfrmLocationDetails.MapForSpatialRefClick(Sender: TObject);
begin
  // Map selected from dropdown menu.
  FExpectingSpatialRef := True;
  dmFormActions.MapWindowMenuClick(Sender);
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateSpatialRef);
end;

{-------------------------------------------------------------------------------
  If there are any addins with map formats that have non-standard grid squares,
  retrieve them.
}
procedure TfrmLocationDetails.InitialiseGridSquareInfoList;
var
  i, j: Integer;
  lBaseMapScaleList: IBaseMapScaleList;
  lRefSystem: String;
begin
  // Add the default sizes. No Ref systems as it applies to OSGB/OSNI, maybe others?
  FGridSquareInfoList.Add(TGridSquareInfo.Create('', 100000, '100km'));
  FGridSquareInfoList.Add(TGridSquareInfo.Create('', 10000, '10km'));
  FGridSquareInfoList.Add(TGridSquareInfo.Create('', 1000, '1km'));
  FGridSquareInfoList.Add(TGridSquareInfo.Create('', 100, 'Hectare'));
  FGridSquareInfoList.Add(TGridSquareInfo.Create('', 2000, 'Tetrad'));

  // Get everything needed from extra SpatialRef addins.
  for i:=0 to AppSettings.ComAddins.SpatialSystemInterfaces.Count-1 do
  begin
    // Get Ref System associated with sizes about to be retrieved.
    lRefSystem := '';
    if Supports(AppSettings.ComAddins.SpatialSystemInterfaces[i], IID_ISpatialReference) then
      lRefSystem :=
          (AppSettings.ComAddins.SpatialSystemInterfaces[i] as ISpatialReference).SpatialRefSystem;

    // Retrieve all sizes for current system and add them to the list.
    if Supports(AppSettings.ComAddins.SpatialSystemInterfaces[i],
                IID_IBaseMapScaleList, lBaseMapScaleList) then
    begin
      lBaseMapScaleList := AppSettings.ComAddins.SpatialSystemInterfaces[i] as IBaseMapScaleList;

      for j := 0 to lBaseMapScaleList.GridScaleCount - 1 do
        FGridSquareInfoList.Add(
            TGridSquareInfo.Create(
                lRefSystem,
                Floor(lBaseMapScaleList.GridScaleX[j]),
                lBaseMapScaleList.GridScaleCaption[j]));
    end;
  end;

  // And now add all the types to the box.
  cmbSquareType.Clear;
  for i := 0 to FGridSquareInfoList.Count - 1 do
    if cmbSquareType.Items.IndexOf(TGridSquareInfo(FGridSquareInfoList[i]).SizeName) = -1 then
      cmbSquareType.Items.AddObject(
          TGridSquareInfo(FGridSquareInfoList[i]).SizeName,
          FGridSquareInfoList[i]);
end;  // InitialiseGridSquareInfoList;

{-------------------------------------------------------------------------------
}
procedure TfrmLocationDetails.GridSquareItemChange(
    Sender: TObject;
    Item: TDataItem;
    Status: TDataItemStatus);
var
  squareItem: TGridSquareItem;
  sizeName : String;
  intf: ISpatialReference;

  function FindMatch(const ASystem: String; ASize: Integer): String;
  var
    j: Integer;
    info: TGridSquareInfo;
  begin
    Result := '';
    for j := 0 to FGridSquareInfoList.Count - 1 do begin
      info := TGridSquareInfo(FGridSquareInfoList[j]);
      if SameText(ASystem, info.SpatialRefSystem) and (ASize = info.Size) then begin
        Result := info.SizeName;
        Break;
      end;
    end;
  end;

begin
  // Need to set the size name when new item is added.
  if Status = disNew then begin
    squareItem := TGridSquareItem(Item);
    // Try to find a match with the system specified.
    sizeName := FindMatch(squareItem.SpatialRefSystem, squareItem.SpatialRefSize);
    // If no match found, try with equivalent system, if there is one.
    if (sizeName = '') and
       (AppSettings.ComAddins.SpatialSystems.IndexOf(squareItem.SpatialRefSystem) <> -1) then
    begin
      intf := CurrentSpatialRefInterface(squareItem.SpatialRefSystem);
      if Supports(intf, IID_ISpatialReference6) then
        sizeName := FindMatch(
            (intf as ISpatialReference6).EquivalentBaseMapSystem,
            squareItem.SpatialRefSize);
    end;
    // If no match found, try with default types.
    if sizeName = '' then
      sizeName := FindMatch('', squareItem.SpatialRefSize);
    // Supposedly, there should now be a match.
    if sizeName <> '' then squareItem.SpatialRefSizeName := sizeName;
  end;
end;

{-------------------------------------------------------------------------------
  Get data F9 link for Tenure
}
procedure TfrmLocationDetails.eTenureNameGetData(Sender: TObject);
begin
  inherited;
  dmFormActions.actNames.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild),Self,UpdateTenure);
end;

{-------------------------------------------------------------------------------
  Find data setup for Tenure
}
procedure TfrmLocationDetails.eTenureNameFindData(Sender: TObject);
begin
  inherited;
  if not dmGeneralData.CheckName(eTenureName) then
    MessageDlg(ResStr_InvalidTenureName, mtInformation, [mbOK], 0);
end;

{-------------------------------------------------------------------------------
  Find data setup for Designation Authority
}
procedure TfrmLocationDetails.eDesAuthorityFindData(Sender: TObject);
begin
  inherited;
  if not dmGeneralData.CheckName(eDesAuthority) then
    MessageDlg(ResStr_InvalidAuthorityName, mtInformation, [mbOK], 0);
end;

{-------------------------------------------------------------------------------
  Get data F9 link for Designation Authority
}
procedure TfrmLocationDetails.eDesAuthorityGetData(Sender: TObject);
begin
  inherited;
  dmFormActions.actNames.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateDesAuthority);
end;

{-------------------------------------------------------------------------------
  When the panel on the bottom of the Uses tab page is resized, resizes its
  components to fit.
}
procedure TfrmLocationDetails.pnlUsesBottomResize(Sender: TObject);
var
  availableHeight, halfHeight: Integer;
begin
  inherited;
  availableHeight := gbUseDetails.Height - eUseFrom.Top - eUseFrom.Height -4 * 3;
  halfHeight := availableHeight div 2;
  reUsePotential.Height := halfHeight;
  reUseComments.Top := reUsePotential.Top + halfHeight + 4;
  reUseComments.Height := halfHeight;
  lblUsesComments.Top := reUseComments.Top + 3;
end;

{-------------------------------------------------------------------------------
  Refreshes the available items in the MapFile combo box.
}
procedure TfrmLocationDetails.RefreshMapFileCombo;
var
  oldValue: TMapSheet;
  oldText, fileName: string;
  index: integer;
begin
  // Stores the selected value
  if cmbMapFile.ItemIndex = -1 then
    oldValue := nil
  else begin
    oldValue := TMapSheet.CopyFrom(TMapSheet(cmbMapFile.Items.Objects[cmbMapFile.ItemIndex]));
    oldText := cmbMapFile.Items[cmbMapFile.ItemIndex];
  end;

  ClearCombo(cmbMapFile);

  with dmDatabase.ExecuteSQL('EXECUTE usp_Polygon_Layers_Get', True) do begin
    try
      while not EOF do begin
        if Fields.Item['Dataset_Sheet_Filename'].Value = Null then
          fileName := ''
        else
          fileName := Fields['Dataset_Sheet_Filename'].Value;

        cmbMapFile.Items.AddObject(
                Format('%s (%s)',
                       [Fields['Display_Name'].Value,
                        Fields['Sheet_Name'].Value]),
                TMapSheet.CreateInternal(
                       fileName,
                       Fields['Map_Sheet_Key'].Value,
                       Fields['Base_Map_Key'].Value));
        MoveNext;
      end;
    finally
      Close;
    end;
  end;

  // Re-selects the original selected value if it is in the list.
  if Assigned(oldValue) then begin
    index := GetMapSheetIndex(cmbMapFile, oldValue);
    if index = -1 then begin
      cmbMapFile.Items.AddObject(oldText, oldValue);
      index := cmbMapFile.Items.Count - 1;
    end else
      FreeAndNil(oldValue);

    cmbMapFile.ItemIndex := index; 

    RefreshObjectIDCombo(TMapSheet(cmbMapFile.Items.Objects[cmbMapFile.ItemIndex]));
  end else
    RefreshObjectIDCombo(nil);
end;

{-------------------------------------------------------------------------------
  Gets the index of the specified map sheet in the combo box object list.
}
function TfrmLocationDetails.GetMapSheetIndex(comboBox: TComboBox; value: TMapSheet): integer;
var
  i: integer;
  listItem: TMapSheet;
begin
  Result := -1;
  for i := 0 to comboBox.Items.Count - 1 do
    if Assigned(comboBox.Items.Objects[i]) then
    begin
      listItem := TMapSheet(comboBox.Items.Objects[i]);
      if (listItem.IsInternal = value.IsInternal) and (
            ((value.IsInternal = True ) and (listItem.MapSheetKey = value.MapSheetKey)) or
            ((value.IsInternal = False) and (listItem.FileName    = value.FileName   ))
                                                      ) then
      begin
        Result := i;
        Exit;
      end;
    end;
end;

{-------------------------------------------------------------------------------
  Opens a dialog allowing the user to select an external map browser file.
}
procedure TfrmLocationDetails.btnMapFileBrowseClick(Sender: TObject);
begin
  inherited;
  if FLastFilePath <> '' then
    dlgOpen.InitialDir := FLastFilePath;

  if dlgOpen.Execute = True then
  begin
    FLastFilePath := ExtractFilePath(dlgOpen.FileName);

    with TdlgBoundaryFieldSelect.Create(self) do
    begin
      try
        FileName := dlgOpen.FileName;
        if ShowModal = mrOK then
        begin
          RemoveExternalFile;
          cmbMapFile.Items.AddObject(
                  dlgOpen.FileName,
                  TMapSheet.CreateExternal(
                        dlgOpen.FileName,
                        cmbBoundaryAttribute.Items[cmbBoundaryAttribute.ItemIndex]));

          cmbMapFile.ItemIndex := cmbMapFile.Items.Count - 1;

          RefreshObjectIDCombo(TMapSheet(cmbMapFile.Items.Objects[cmbMapFile.ItemIndex]));
        end;
      finally
        Free;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  When the combo box is changed, delete all file names from the list except for the
  currently selected item.
}
procedure TfrmLocationDetails.cmbMapFileChange(Sender: TObject);
begin
  inherited;
  RefreshMapFileCombo;
end;

{-------------------------------------------------------------------------------
  Refreshes the contents of the ObjectID combo box.
}
procedure TfrmLocationDetails.RefreshObjectIDCombo(mapSheet: TMapSheet);
var
  mapServerLink: TMapServerLink;
  sheetID, itemCount, i, objectID, attributeIndex: integer;
  idValue: array [0..50] of char;

  {-----------------------------------------------------------------------------
    Gets the attribute index of the specified attribute.
  }
  function GetAttributeIndex: integer;
  var
    x, attributeCount: integer;
    attributeName: array [0..10] of char;
  begin
    Result := -1;
    MapCheck(MapDataGetNumAttributeFields(mapServerLink.MapHandle, sheetID, attributeCount));
    for x := 0 to attributeCount - 1 do begin
      MapCheck(
          MapDataGetAttributeFieldName(mapServerLink.MapHandle, sheetID, x, attributeName, 10));
      if mapSheet.IDField = attributeName then begin
        Result := x;
        Exit;
      end;
    end;
  end;

begin
  cmbGISObjectID.Clear;  
  FObjectIDExists := True;
  if Assigned(mapSheet) then begin
    mapServerLink := TMapServerLink.Create(nil);
    try
      if mapSheet.IsInternal then begin
        mapServerLink.ActiveDataset := mapSheet.BaseMapKey + '.gds';

        sheetID :=
            mapServerLink.SheetIDByFileName(AppSettings.ObjectSheetFilePath + mapSheet.FileName);

        // Add in the ObjectIDs for each item in the sheet
        MapCheck(MapGetNumObjects(mapServerLink.MapHandle, sheetID, itemCount));
        for i := 0 to itemCount - 1 do begin
          MapCheck(MapGetObjectStaticID(mapServerLink.MapHandle, sheetID, i, objectID));
          cmbGISObjectID.Items.Add(IntToStr(objectID));
        end;
      end else begin
        // Use the default map as the active dataset.
        mapServerLink.ActiveDataset := AppSettings.AvailableMaps.DefaultMap.BaseMapKey + '.gds';

        if FileExists(mapSheet.FileName) then begin
          // Create a temporary sheet for the external data.
          if CompareText(ExtractFileExt(mapSheet.FileName), '.gsf') = 0 then
            MapCheck(MapAttachSheet(mapServerLink.MapHandle,
                                    PChar(mapSheet.FileName)),
                     mapSheet.FileName)
          else
            MapCheck(MapImport(mapServerLink.MapHandle,
                               PChar(mapSheet.FileName),
                               PChar(AppSettings.MapFilePath),
                               0,
                               False),
                     mapSheet.FileName);

          // The most recent sheet will be the sheet we added.
          sheetID := mapServerLink.SheetTotal - 1;
          try
            attributeIndex := GetAttributeIndex;

            // Add items from the ID field the user specified.
            MapCheck(MapGetNumObjects(mapServerLink.MapHandle, sheetID, itemCount));
            for i := 0 to itemCount - 1 do begin
              MapCheck(MapDataGetAttribute(mapServerLink.MapHandle,sheetID, attributeIndex,
                                           i, idValue, 50));
              cmbGISObjectID.Items.Add(idValue);
            end;
          finally
            // Remove the temporary sheet.
            if sheetID <> -1 then begin
              MapCheck(MapSelectSheet(mapServerLink.MapHandle, sheetID, False));
              MapCheck(MapDetachSheet(mapServerLink.MapHandle, sheetID));
            end;
          end;
          FFileExists := True;
        end else
          FFileExists := False;
      end;
    finally
      mapServerLink.Free;
    end;
  end;
  RefreshLabel;
end;

{-------------------------------------------------------------------------------
  Refreshes the linked field label on the boundaries tab.
}
procedure TfrmLocationDetails.RefreshLabel;
var
  mapSheet: TMapSheet;
begin
  lblLinkedField.Visible := False;
  if (cmbMapFile.ItemIndex <> -1) and Assigned(cmbMapFile.Items.Objects[cmbMapFile.ItemIndex]) then
  begin
    mapSheet := TMapSheet(cmbMapFile.Items.Objects[cmbMapFile.ItemIndex]);

    if not mapSheet.IsInternal then begin
      lblLinkedField.Visible := True;
      lblLinkedField.Caption := Format(ResStr_LinkedTo, [mapSheet.IDField]);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Frees all the objects in a combo box then clears the list.
}
procedure TfrmLocationDetails.ClearCombo(comboBox: TComboBox);
var
  i: integer;
begin
  with comboBox do begin
    for i := 0 to Items.Count - 1 do
      if Assigned(Items.Objects[i]) then
        Items.Objects[i].Free;

    Clear;
  end;
end;

{-------------------------------------------------------------------------------
  Removes the last item from cmbMapFile, if it is an external file.
}
procedure TfrmLocationDetails.RemoveExternalFile;
var
  lastIndex: integer;
begin
  with cmbMapFile.Items do begin
    lastIndex := Count - 1;
    if (lastIndex > -1) and not TMapSheet(Objects[lastIndex]).IsInternal then begin
      Objects[lastIndex].Free;
      Delete(lastIndex);
    end;
  end;
end;

end.

