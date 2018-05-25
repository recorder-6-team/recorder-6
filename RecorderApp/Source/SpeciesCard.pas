//==============================================================================
//  Unit:        SpeciesCard
//
//  Implements:  TfrmSpeciesCard
//
//  Description: Allows user to enter a single species record.
//                             4
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     Eric Salmon - 8 March 2002
//               Cleaned up the CleanUp code to take advantage of master/detail
//               relationships between table.
//
//  Last Revision Details:
//    $Revision: 205 $
//    $Date: 12/11/09 17:47 $
//    $Author: Simonlewis $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit SpeciesCard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Grids, ExtCtrls, BaseChildUnit, Buttons, VagueDateEdit,
  Menus, BaseFormUnit, CompositeComponent, SpatialRef, Constants, DBListCombo,
  DataClasses, ExceptionForm, DropStruct, DropSource, DropTarget, PlaceCardData,
  DBGlyphCtrls, ValidationData, OnlineHelp, GeneralFunctions, TaxonOccurrData,
  ImageListButton, Db, ADODB, LocationInfoFrame, AddinCompositeComponent,
  AddinLinkedControls, XMLDoc, XMLIntf;

resourcestring
  ResStr_DocumentElementInvalid = 'The XML file %s is invalid because the top level element '+
      'is expected to be called %s, but it is currently set to %s. Note that XML is case sensitive.';

type
  ESpeciesError = class(TExceptionPath);
  EDatabaseWriteError = class(ESpeciesError);
  ECustomCardError = class(TExceptionPath);

  TfrmSpeciesCard = class(TBaseChild)
    pnlButtons : TPanel;
    mnuEdit : TMenuItem;
    mnuEditCut : TMenuItem;
    mnuEditCopy : TMenuItem;
    mnuEditPaste : TMenuItem;
    N2 : TMenuItem;
    mnuEditBold : TMenuItem;
    mnuEditItalic : TMenuItem;
    mnuEditUnderline : TMenuItem;
    pnlButtons2 : TPanel;
    pnlSpecies : TPanel;
    pnlMain: TPanel;
    bbReset: TImageListButton;
    bbSave: TImageListButton;
    pmMapWindow: TPopupMenu;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pnlCtrlSurvey: TPanel;
    Label18: TLabel;
    cmbSurvey: TDBListCombo;
    pnlCtrlTaxon: TPanel;
    Label16: TLabel;
    eTaxon: TEdit;
    bbTaxonFind: TImageListButton;
    shpTaxon: TShape;
    pnlCtrlDocument: TPanel;
    Label7: TLabel;
    shpReference: TShape;
    bbReferenceFind: TImageListButton;
    eReference: TEdit;
    pnlCtrlLocationInfo: TPanel;
    fraLocationInfo: TfraLocationInfo;
    pnlCtrlSampleType: TPanel;
    cmbSampleType: TDBListCombo;
    Label17: TLabel;
    pnlCtrlCount: TPanel;
    Label10: TLabel;
    sgCount: TStringGrid;
    bbDetailsAdd: TImageListButton;
    bbDetailsDel: TImageListButton;
    cmbQualifier: TComboBox;
    cmbAccuracy: TComboBox;
    pnlCtrlRecorders: TPanel;
    shpRecorders: TShape;
    Label6: TLabel;
    lbRecorders: TListBox;
    bbRecorderFind: TImageListButton;
    bbRecorderAdd: TImageListButton;
    bbRecorderRemove: TImageListButton;
    pnlCtrlDate: TPanel;
    Label1: TLabel;
    eDate: TVagueDateEdit;
    pnlCtrlAdminAreas: TPanel;
    Label5: TLabel;
    eAdminArea: TEdit;
    pnlCtrlDeterminer: TPanel;
    Label12: TLabel;
    eDeterminer: TNameLinkedEdit;
    pnlCtrlBiotope: TPanel;
    lblBiotope: TLabel;
    shpBiotope: TShape;
    eBiotope: TEdit;
    bbBiotopeFind: TImageListButton;
    pnlCtrlProvenance: TPanel;
    Label11: TLabel;
    cmbProvenance: TComboBox;
    pnlCtrlRecordType: TPanel;
    Label14: TLabel;
    cmbRecordType: TDBListCombo;
    pnlCtrlSubstrate: TPanel;
    Label15: TLabel;
    cmbSubstrate: TDBListCombo;
    pnlCtrlComments: TPanel;
    reComments: TRichEdit;
    Label8: TLabel;
    pnlCtrlDateOfDetermination: TPanel;
    lblDeterminationDate: TLabel;
    eDeterminationDate: TVagueDateEdit;
    pnlCtrlSpecimenComment: TPanel;
    lblSpecimenComment: TLabel;
    reSpecimenComment: TRichEdit;
    pnlCtrlSpecimenNumber: TPanel;
    lblSpecimenNumber: TLabel;
    eSpecimenNumber: TEdit;
    pnlCtrlSpecimenType: TPanel;
    lblSpecimenType: TLabel;
    cmbSpecimenType: TDBListCombo;
    pnlCtrlSpecimenLocation: TPanel;
    lblSpecimenLocation: TLabel;
    eSpecimenLocation: TEdit;
    procedure eReferenceKeyPressed(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bbSaveClick(Sender: TObject);
    procedure bbResetClick(Sender: TObject);
    procedure bbReferenceFindClick(Sender: TObject);
    procedure bbBiotopeFindClick(Sender: TObject);
    procedure bbAdminAreaFindClick(Sender: TObject);    
    procedure bbRecorderFindClick(Sender: TObject);
    procedure bbRecorderAddClick(Sender: TObject);
    procedure bbRecorderRemoveClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure sgCountDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure cmdGridExit(Sender: TObject);
    procedure sgCountClick(Sender: TObject);
    procedure bbDetailsAddClick(Sender: TObject);
    procedure bbDetailsDelClick(Sender: TObject);
    procedure ChangeEditState(Sender: TObject);
    procedure cmbSampleTypeDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure bbTaxonFindClick(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: Char);
    procedure eBiotopeKeyPress(Sender: TObject; var Key: Char);
    procedure eAdminAreaSearchKeyPress(Sender: TObject; var Key: Char);
    procedure cmbProvenanceChange(Sender: TObject);
    procedure cmbCountAttributeChange(Sender: TObject);
    procedure cmbRecordTypeChange(Sender: TObject);
    procedure cmbSubstrateChange(Sender: TObject);
    procedure lbRecordersKeyPress(Sender: TObject; var Key: Char);
    procedure sgCountSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure reCommentsChange(Sender: TObject);
    procedure reCommentsEnter(Sender: TObject);
    procedure reCommentsExit(Sender: TObject);
    procedure eDateExit(Sender: TObject);
    procedure eDeterminationDateExit(Sender: TObject);
    procedure sgCountKeyPress(Sender: TObject; var Key: Char);
    procedure sgCountSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure eReferenceDblClick(Sender: TObject);
    procedure eDeterminerGetData(Sender: TObject);
    procedure eDeterminerFindData(Sender: TObject);
    procedure lbRecordersKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure eSpecimenNumberChange(Sender: TObject);
    procedure pnlSpeciesResize(Sender: TObject);
  private
    FEditing : boolean;
    FClosingForm:boolean;
    FSurveyEventKey : TKeyString;
    FSampleKey : TKeyString;
    FTaxonListItemKey : TKeyString;
    FTaxonOccKey : TKeyString;
    FTaxonDetKey : TKeyString;
    FtfHaveData  : boolean; // have we inserted data that needs to be cleaned?
    FReferenceKey : TKeyString;
    FdmPlaceCard : TdmPlaceCard;
    FdmTaxonOcc :TdmTaxonOccurrences;
    FSERKeys : TStringList;
    FBiotopeOccKey : TKeyString;
    FBiotopeListItemKey : TKeyString;
    FAdminAreaKeys: TStringList;
    FAdminAreaTypes: TStringList;
    FBiotopeDetKey : TKeyString;
    FBiotopeOccDataKey : TKeyString;
    FNewSurveyEvent : boolean;
    FCustomCard : boolean;
    FControlsUsed : TStringList;
    FMeasurementControls: TStringList;
    FSrefInSampleRef: string;
    FSearchingForAdminTypeIndex: integer;

    procedure PositionCombo(parentGrid: TStringGrid; comboBox: TComboBox; targetColumn, targetRow: integer);
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
    procedure WMRefreshTermLists(var Msg:TMessage); message WM_REFRESH_TERM_LISTS;
    procedure WMRefreshSpatialRefSystem(var Msg: TMessage); message WM_REFRESH_SPATIAL_REF_SYSTEM;
    procedure WMUpdateSurveyCombo(var Msg: TMessage); message WM_UPDATE_SURVEY_COMBO;
    procedure WMRefreshNames(var Msg: TMessage); message WM_REFRESH_NAMES;
    procedure WMRefreshLocationDetails(var Msg: TMessage); message WM_REFRESH_LOCATION_DETAILS;
    procedure WMRefreshBiotopeDic(var Msg: TMessage); message WM_REFRESH_BIOTOPE_DIC;
    procedure WMRefreshTaxonDic(var Msg: TMessage); message WM_REFRESH_TAXON_DIC;
    procedure WMRefreshColours(var Msg: TMessage); message WM_REFRESH_COLOURS;
    procedure WMRefreshDocuments(var Msg: TMessage); message WM_REFRESH_DOCUMENTS;
    procedure SetEditing(const Value: boolean);
    procedure CreateSampleRecord;
    procedure CreateBiotopeOccurrence;
    procedure CreateSpeciesOccurrence;
    procedure CreateSpecimen(const taxonOccurrenceKey : string);
    procedure AddSurveyEvent;
    procedure DropTaxon(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TstringList; var ioHandled : boolean);
    procedure DropReference(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TstringList; var ioHandled : boolean);
    procedure DropRecorder(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TstringList; var ioHandled : boolean);
    procedure DropBiotope(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TstringList; var ioHandled : boolean);
    procedure DropAdminArea(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TstringList; var ioHandled : boolean);
    procedure DropDeterminer(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TstringList; var ioHandled : boolean);
    procedure ResetCard;
    function GetDeterminerKey: string;
    procedure Cleanup;
    procedure InitialiseKeyVariables;
    function DeterminerComplete(var lstDetCheckMsg: string; var DetCheckComponent: TWinControl): boolean;
    procedure ValidateCardDate;
    procedure ValidateDeterminationDate;
    function TaxonInLocalDatabase(ATaxonKey : TKeyString; ATaxonName : string): boolean;
    function GetLastSurvey: TKeyString;
    procedure SetLastSurvey(const Value: TKeyString);
    property LastSurvey: TKeyString read GetLastSurvey write SetLastSurvey;
    procedure AddRecorder(AText, AKey: string);
    procedure fraLocationInfoUpdateLocation(Sender: TObject);
    procedure fraLocationInfoChangedContent(Sender: TObject);
    procedure LoadCustomCard;
    procedure LoadPanel(node: IXMLNode; parentPanel: TPanel);
    function CreateMeasurementPanel(node: IXmlNode; const entity: string): TPanel;
    function CreateAdminAreaSearchPanel(node: IXmlNode): TPanel;
    procedure SaveCustomMeasurements;
    function FindLabel(parentPanel: TWinControl): TLabel;
    procedure SetControlWidth(ctrl, parentPanel: TControl; ctrlWidth: string);
    function GetSampleReference: string;
  protected
    procedure SetupDestinationControls; override;
    procedure UpdateObservations;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ReferenceUpdate(KeyList: TKeyList);
    procedure TaxonUpdate(KeyList: TKeyList);
    procedure BiotopeUpdate(KeyList: TKeyList);
    procedure AdminAreaUpdate(KeyList: TKeyList);
    procedure RecorderUpdate(KeyList: TKeyList);
    procedure DeterminerUpdate(KeyList: TKeyList);
    procedure SetDisplaySystem;
    procedure UpdateMapWindowSelector; override;
    property Editing: boolean read FEditing write SetEditing;
    destructor Destroy; override;
  end;

var
  frmSpeciesCardFileToLoad: string;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, Map, Find, Maintbar, GeneralData, ApplicationSettings,
  JNCCDatasets, VagueDate, Observations, HierarchyNodes, SpatialRefFuncs, Registry,
  StrUtils, DatabaseAccessADO, Types;

resourcestring
  ResStr_SaveChangesToRecordCard =  'Do you want to save the changes you have made to this record card?';

  ResStr_CannotSaveRecordSelect = 'Unable to save this record without a Survey selected.  '+
                                  'Please select a Survey.';

  ResStr_InvalidOrMissingTaxonName =  'The Taxon Name is missing or is invalid.  '+
                                      'Enter a valid name.';

  ResStr_InvalidDocumentSelect =  'The Document is invalid. Select a valid Document or leave blank.';
  ResStr_MissingRecorderNameSelect =  'A Recorder Name is missing. '+
                                      'At least one must be selected.' ;
  ResStr_MissingDateEnter = 'The Date is missing. Enter a date value.';
  ResStr_DateOutsideRange = 'The Date is outside the selected Survey''s range. ';
  ResStr_MissingSampleTypeSelect =  'The Sample Type is missing. Select a type from the list.';
  ResStr_InvalidBiotopeSelect = 'The Biotope is invalid. Select a valid Biotope or leave blank.';
  ResStr_InvalidAdminAreaSelect = 'The Admin Area is invalid. Select a valid Admin Area or leave blank.';

  ResStr_SetCountAndCountOf = 'Count and Count Of must both be set when '+
                              'either one is set. There is some data missing.';
  ResStr_InvalidCountQualifier = 'Invalid Count qualifier.';

  ResStr_SuccessfullyRecordedEvent = 'All data successfully recorded under a new survey event.';
  ResStr_SuccessfullyRecordedExistingEvent =  'All data successfully recorded as part of an existing survey event.';

  ResStr_ExceptionWhenSaving =  'An exception occurred attempting to save the data.  Another user might be ' +
                                'entering some data. Please wait a little while before trying to save again'#13#13+
                                'Residual data was successfully cleaned up.  Message: ';

  ResStr_DetDateAfterObsDate = 'The determination date has to occur on or after the observation date of %s. If a ' +
      'vague determination date has been entered, note that the start of this vague date must not precede the start ' +
      'of the observation date.';

  ResStr_UnableToMatchDeterminer = 'Unable to match the determiner to any known individuals. ' +
                                   'Please try a different determiner.';

  ResStr_SpecifyDeterminerForDate = 'For a determination date to be accepted, a determiner must ' +
                                    'also be specified.';

  ResStr_SpecifyDetDateForDeterminer =  'When a determiner is specified, a determination date must ' +
                                        'also be entered.';

  ResStr_SpecimenTypeMissing = 'Specimen Type is missing. Please select a Specimen Type.';
  ResStr_MustEnterSpecimenNumber = 'If you enter a Specimen Location or Specimen Comment, ' +
                                   'you must also enter a Specimen Number and Specimen Type.';

  ResStr_FailureToSave =  'A failure occurred during saving and I was unable to clean up ' +
                          'the partially saved data. There may be incomplete survey event ' +
                          'or sample data in the system.';

  ResStr_CountGridCaptions = 'Count,"Count Of",Accuracy';
  ResStr_AdminArea = 'Admin area';
  ResStr_GetAdminArea = 'Get admin area';

const
  NO_RESIZE = -1;

//==============================================================================
constructor TfrmSpeciesCard.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  { Create a place card data module specifically for our use }
  FdmPlaceCard := TdmPlaceCard.Create(Self);
  FdmTaxonOcc  := TdmTaxonOccurrences.Create(nil);
  FCustomCard := false;
  FSrefInSampleRef := '';
  FMeasurementControls := TStringList.Create;
  FAdminAreaKeys := TStringList.Create;
  FAdminAreaTypes := TStringList.Create;
  LoadCustomCard;

  sgCount.Rows[0].CommaText := ResStr_CountGridCaptions;

  cmbSampleType.Active   := True;
  cmbRecordType.Active   := True;
  cmbSubstrate.Active    := True;
  cmbSpecimenType.Active := True;

  ResetCard;
  SetDisplaySystem;
  dmGeneralData.PopulateQualifierCombo(cmbQualifier, NONE_ABUNDANCE_QUALIFIER_KEY); // abundance type
  fraLocationInfo.eSpatialRef.Modified:=false;
  Editing := false;
  SetRequiredFieldsColourState(true,[cmbSurvey, eTaxon, eDate, cmbSampleType, lbRecorders]);
  fraLocationInfo.SetColour(MergeColours(AppSettings.MandatoryColour, clWindow, 25));

  cmbSurvey.Active := true;
  SendMessage(Handle,WM_UPDATE_MENU_ICONS, 0, 0);

  //Help Setup
  mnuEdit.HelpContext := IDH_EDITMENU;
  Self.HelpContext    := IDH_SPECIESCARD;
  fraLocationInfo.OnUpdateLocation := fraLocationInfoUpdateLocation;
  fraLocationInfo.OnChangedContent := fraLocationInfoChangedContent;

  UpdateMapWindowSelector;
  FEditMode := Constants.emEdit;

  // Set this explicitly here because setting in DFM is ignored.
  Constraints.MinWidth := 200;
end;  // Create

//==============================================================================
procedure TfrmSpeciesCard.FormCreate(Sender: TObject);
begin
  inherited;
  // Since the form has now a fixed size, need to make sure it is from the start.
  Height := Constraints.MaxHeight;
  Width  := Constraints.MaxWidth;

  // Set this explicitly here because setting in DFM is ignored.
  Constraints.MinWidth := 200;
end;

//==============================================================================
procedure TfrmSpeciesCard.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(true);
  frmMain.SetContextToolbar(Self,mnuEdit,0,[]);
  dmFormActions.UpdateRTFMenu(reComments.Focused);
end;  // FormActivate

//==============================================================================
procedure TfrmSpeciesCard.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=false;
  if Editing or fraLocationInfo.eSpatialRef.Modified then
    case MessageDlg(ResStr_SaveChangesToRecordCard, mtConfirmation,[mbYes,mbNo,mbCancel],0) of
      mrYes : begin
                bbSaveClick(nil);
                CanClose:=true;
              end;
      mrNo  : begin
                FClosingForm:=true;
                bbResetClick(nil);
                CanClose:=true;
                FClosingForm:=false;
              end;
    end
  else
    CanClose:=true;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmSpeciesCard.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
end;  // FormClose

//==============================================================================
destructor TfrmSpeciesCard.Destroy;
var iCount : integer;
begin
  with lbRecorders.Items do begin
    for iCount:=0 to Count-1 do
      Objects[iCount].Free;
    Clear;
  end;

  { Clean up qualifier keys }
  with cmbQualifier.Items do begin
    for iCount := 0 to Count - 1 do
      Objects[iCount].Free;
    Clear;
  end;

  FreeAndNil(FSERKeys);
  FreeAndNil(FdmPlaceCard);
  FreeAndNil(FdmTaxonOcc);
  FreeAndNil(FAdminAreaKeys);
  FreeAndNil(FAdminAreaTypes);
  if assigned(FControlsUsed) then
    FControlsUsed.Free;
  if assigned(FMeasurementControls) then
    FMeasurementControls.Free;
  inherited Destroy;
end;  // Destroy

procedure TfrmSpeciesCard.LoadCustomCard;
var
  xmlDoc: IXMLDocument;
  node: IXMLNode;
  i: integer;
  rowPanel: TPanel;
  lastBottom: integer;
  mandatoryControls: TStringList;
begin
  if frmSpeciesCardFileToLoad = '' then
    exit;
  FCustomCard := true;
  xmlDoc := NewXMLDocument;
  xmlDoc.LoadFromFile(AppSettings.CustomSpeciesCardPath + frmSpeciesCardFileToLoad);
  if xmlDoc.DocumentElement.NodeName <> 'species_card' then
    raise ECustomCardError.CreateNonCritical(Format(ResStr_DocumentElementInvalid,
        [AppSettings.CustomSpeciesCardPath + frmSpeciesCardFileToLoad, 'species_card',
        xmlDoc.DocumentElement.NodeName]));
  pnlMain.Visible := false;
  node := xmlDoc.DocumentElement;
  if node.HasAttribute('title') then
    self.Caption := node.Attributes['title'];
  if node.HasAttribute('sref_in_sample_ref') then
    FSrefInSampleRef := node.Attributes['sref_in_sample_ref'];
  lastBottom := 0;
  FControlsUsed := TStringList.Create;
  for i := 0 to node.ChildNodes.Count-1 do begin
    if node.ChildNodes.Nodes[i].NodeName = 'row' then begin
      rowPanel := TPanel.Create(self);
      rowPanel.parent := pnlSpecies;
      // force to anchor to the bottom of the list
      rowPanel.top := lastBottom + 1;
      rowPanel.align := alTop;
      rowPanel.height := 24;
      rowPanel.BevelOuter := bvNone;
      LoadPanel(node.ChildNodes.Nodes[i], rowPanel);
      lastBottom := rowPanel.Top + rowPanel.Height;
    end;
  end;
  for i:= 0 to FAdminAreaTypes.Count-1 do begin
    RegisterDropComponent(TEdit(FAdminAreaTypes.Objects[i]), DropAdminArea, [TN_ADMIN_AREA], [CF_JNCCDATA, CF_TEXT]);
    TEdit(FAdminAreaTypes.Objects[i]).Name := 'adminArea' + IntToStr(i);
  end;
  mandatoryControls := TStringList.Create;
  try
    mandatoryControls.commaText := 'survey,taxon,location_info,recorders,date,sample_type';
    for i := 0 to mandatoryControls.Count-1 do begin
      if FControlsUsed.IndexOf(mandatoryControls[i])=-1 then
        raise ECustomCardError.CreateNonCritical('The card definition is invalid - ' + mandatoryControls[i] + ' required');
    end;
  finally
    mandatoryControls.Free;
  end;
  pnlSpeciesResize(self);
end;

(**
 * Find the label on a control panel for a custom species card so that we can resize it etc.
 *)
function TfrmSpeciesCard.FindLabel(parentPanel: TWinControl): TLabel;
var i: integer;
begin
  result := nil;
  for i:=0 to parentPanel.ControlCount-1 do
    if parentPanel.Controls[i] is TLabel then
      result := TLabel(parentPanel.Controls[i]);
  if not assigned(result) then
    raise ECustomCardError.Create('Missing label on panel');
end;

procedure TfrmSpeciesCard.SetControlWidth(ctrl, parentPanel: TControl; ctrlWidth: string);
begin
  if (Pos('px', ctrlWidth)>0) then begin
    ctrlWidth := StringReplace(ctrlWidth, 'px', '', []);
    // fixed width in pixels
    ctrl.Width := StrToInt(ctrlWidth);
    // flag the column so it is not resized
    ctrl.Tag := NO_RESIZE;
  end else
  begin
    // remove % if specified (optional)
    ctrlWidth := StringReplace(ctrlWidth, '%', '', []);
    ctrl.Width := (parentPanel.Width * StrToInt(ctrlWidth)) div 100;
    // tag the width into the panel's properties so we can detect and resize it
    ctrl.Tag := StrToInt(ctrlWidth);
  end;
end;

procedure TfrmSpeciesCard.LoadPanel(node: IXMLNode; parentPanel: TPanel);
var
  i, j, ctrl, origWidth: integer;
  colPanel, ctrlPanel: TPanel;
  container: TWinControl;
  loadingCols, loadingCtrls: boolean;
  lastEdge: integer;
  pnlLabel: TLabel;
  labelCaption: string;
begin
  loadingCols := false;
  loadingCtrls := false;
  lastEdge := 0;
  for i := 0 to node.ChildNodes.Count-1 do begin
    if node.ChildNodes.Nodes[i].NodeName = 'col' then begin
      if loadingCtrls then
        raise ECustomCardError.Create('Cannot mix controls and columns inside a single block');
      loadingCols := true;
      colPanel := TPanel.Create(self);
      colPanel.parent := parentPanel;
      // ensure it aligns in the right order
      colPanel.left := lastEdge + 1;
      colPanel.align := alLeft;
      //if i = node.ChildNodes.Count-1 then
      //  colPanel.align := alClient;
      colPanel.height := 24; // minimum - will expand as we go
      if node.ChildNodes[i].HasAttribute('width') then
        SetControlWidth(colPanel, parentPanel, node.ChildNodes[i].Attributes['width'])
      else
        colPanel.Width := parentPanel.Width div node.ChildNodes.Count;
      colPanel.BevelOuter := bvNone;
      LoadPanel(node.ChildNodes.Nodes[i], colPanel);
      if parentPanel.Height < colPanel.Top + colPanel.Height then
        parentPanel.Height := colPanel.Top + colPanel.Height;
      lastEdge := colPanel.Left + colPanel.Width;
    end
    else begin
      if loadingCols then
        raise ESpeciesError.Create('Cannot mix controls and columns inside a single block');
      loadingCtrls := true;
      ctrlPanel := nil;
      FControlsUsed.Add(node.ChildNodes.Nodes[i].NodeName);
      if node.ChildNodes.Nodes[i].NodeName = 'survey' then
        ctrlPanel := pnlCtrlSurvey
      else if node.ChildNodes.Nodes[i].NodeName = 'taxon' then
        ctrlPanel := pnlCtrlTaxon
      else if node.ChildNodes.Nodes[i].NodeName = 'document' then
        ctrlPanel := pnlCtrlDocument
      else if node.ChildNodes.Nodes[i].NodeName = 'location_info' then
        ctrlPanel := pnlCtrlLocationInfo
      else if node.ChildNodes.Nodes[i].NodeName = 'sample_type' then
        ctrlPanel := pnlCtrlSampleType
      else if node.ChildNodes.Nodes[i].NodeName = 'count' then
        ctrlPanel := pnlCtrlCount
      else if node.ChildNodes.Nodes[i].NodeName = 'determiner' then
        ctrlPanel := pnlCtrlDeterminer
      else if node.ChildNodes.Nodes[i].NodeName = 'specimen_number' then
        ctrlPanel := pnlCtrlSpecimenNumber
      else if node.ChildNodes.Nodes[i].NodeName = 'specimen_type' then
        ctrlPanel := pnlCtrlSpecimenType
      else if node.ChildNodes.Nodes[i].NodeName = 'specimen_location' then
        ctrlPanel := pnlCtrlSpecimenLocation
      else if node.ChildNodes.Nodes[i].NodeName = 'recorders' then
        ctrlPanel := pnlCtrlRecorders
      else if node.ChildNodes.Nodes[i].NodeName = 'date' then
        ctrlPanel := pnlCtrlDate
      else if node.ChildNodes.Nodes[i].NodeName = 'biotope' then
        ctrlPanel := pnlCtrlBiotope
      else if node.ChildNodes.Nodes[i].NodeName = 'admin_areas' then
        ctrlPanel := pnlCtrlAdminAreas
      else if node.ChildNodes.Nodes[i].NodeName = 'admin_area_search' then
        ctrlPanel := CreateAdminAreaSearchPanel(node.ChildNodes.Nodes[i])
      else if node.ChildNodes.Nodes[i].NodeName = 'provenance' then
        ctrlPanel := pnlCtrlProvenance
      else if node.ChildNodes.Nodes[i].NodeName = 'record_type' then
        ctrlPanel := pnlCtrlRecordType
      else if node.ChildNodes.Nodes[i].NodeName = 'substrate' then
        ctrlPanel := pnlCtrlSubstrate
      else if node.ChildNodes.Nodes[i].NodeName = 'date_of_determination' then
        ctrlPanel := pnlCtrlDateOfDetermination
      else if node.ChildNodes.Nodes[i].NodeName = 'specimen_comment' then
        ctrlPanel := pnlCtrlSpecimenComment
      else if node.ChildNodes.Nodes[i].NodeName = 'comments' then
        ctrlPanel := pnlCtrlComments
      else if node.ChildNodes.Nodes[i].NodeName = 'sample_measurement' then
        ctrlPanel := CreateMeasurementPanel(node.ChildNodes.Nodes[i], 'sample')
      else if node.ChildNodes.Nodes[i].NodeName = 'taxon_measurement' then
        ctrlPanel := CreateMeasurementPanel(node.ChildNodes.Nodes[i], 'taxon');
      if ctrlPanel <> nil then begin
        ctrlPanel.visible := true;
        if parentPanel.Height < lastEdge + ctrlPanel.Height then begin
          if parentPanel.Parent.Height < parentPanel.Top + lastEdge + ctrlPanel.Height then
            parentPanel.Parent.Height := parentPanel.Top + lastEdge + ctrlPanel.Height;
          if parentPanel.align <> alLeft then
            parentPanel.Height := lastEdge + ctrlPanel.Height;
        end;
        ctrlPanel.align := alNone;
        if node.ChildNodes[i].HasAttribute('label_caption') then begin
          labelCaption := node.ChildNodes[i].Attributes['label_caption'];
          for ctrl := 0 to ctrlPanel.ControlCount-1 do begin
            if ctrlPanel.Controls[ctrl] is TLabel then
              TLabel(ctrlPanel.Controls[ctrl]).Caption := labelCaption;
          end;
        end;
        ctrlPanel.parent := parentPanel;
        // insert below other controls then lock it to the top
        ctrlPanel.top := lastEdge + 1;
        ctrlPanel.align := alTop;
        lastEdge := ctrlPanel.Top + ctrlPanel.Height;
        // do we have a label width setting?
        if node.ChildNodes.Nodes[i].HasAttribute('label_width') then begin
          // If a single control panel just contains a frame then we need to look inside that.
          if (ctrlPanel.ControlCount=1) and (ctrlPanel.Controls[0] is TFrame) then
            container := TFrame(ctrlPanel.Controls[0])
          else
            container := ctrlPanel;
          pnlLabel := FindLabel(container);
          origWidth := pnlLabel.Width;
          SetControlWidth(pnlLabel, ctrlPanel, node.ChildNodes.Nodes[i].Attributes['label_width']);
          // adjust the other controls
          for j := 0 to container.ControlCount-1 do begin
            if (not (container.Controls[j] is TLabel)) and (akLeft in container.Controls[j].Anchors) then begin
              container.Controls[j].Left := container.Controls[j].Left + pnlLabel.Width - origWidth;
              container.Controls[j].Width := container.Controls[j].Width + origWidth - pnlLabel.Width;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Create a new panel for recording a sample or taxon measurement
}
function TfrmSpeciesCard.CreateMeasurementPanel(node: IXmlNode; const entity: string): TPanel;
var
  panel: TPanel;
  ctrlLabel: TLabel;
  ctrlInput: TEdit;
  accuracy: string;
begin
  if not node.HasAttribute('measurement_type_key') then
    raise ECustomCardError.CreateNonCritical('The card definition is invalid - measurements require a measurement_type_key');
  if not node.HasAttribute('measurement_unit_key') then
    raise ECustomCardError.CreateNonCritical('The card definition is invalid - measurements attributes require a measurement_unit_key');
  if not node.HasAttribute('measurement_qualifier_key') then
    raise ECustomCardError.CreateNonCritical('The card definition is invalid - measurements attributes require a measurement_qualifier_key');
  if not node.HasAttribute('label') then
    raise ECustomCardError.CreateNonCritical('The card definition is invalid - measurements attributes require a label');
  panel := TPanel.Create(self);
  panel.BevelOuter := bvNone;
  panel.Align := alTop;
  panel.Height := 26;
  ctrlLabel := TLabel.Create(self);
  ctrlLabel.Parent := panel;
  ctrlLabel.Caption := node.Attributes['label'];
  ctrlLabel.Left := 6;
  ctrlLabel.Top := 6;
  ctrlInput := TEdit.Create(self);
  ctrlInput.Parent := panel;
  ctrlInput.Left := 103;
  ctrlInput.Top := 2;
  ctrlInput.Width := 145;
  result := panel;
  accuracy := '';
  if node.HasAttribute('accuracy') then
    accuracy := node.Attributes['accuracy'];
  FMeasurementControls.AddObject(entity + ':' + node.Attributes['measurement_type_key'] +
      ':' + node.Attributes['measurement_unit_key'] +
      ':' + node.Attributes['measurement_qualifier_key'] +
      ':' + accuracy, ctrlInput);
end;

function TfrmSpeciesCard.CreateAdminAreaSearchPanel(node: IXmlNode): TPanel;
var
  panel: TPanel;
  ctrlLabel: TLabel;
  ctrlInput: TEdit;
  shp: TShape;
  btn: TImageListButton;
begin
  if not node.HasAttribute('admin_type_key') then
    raise ECustomCardError.CreateNonCritical('The card definition is invalid - ' +
        'admin_area_search elements require an admin_type_key');
  panel := TPanel.Create(self);
  panel.BevelOuter := bvNone;
  panel.Align := alTop;
  panel.Height := 26;
  ctrlLabel := TLabel.Create(self);
  with ctrlLabel do begin
    Parent := panel;
    Caption := ResStr_AdminArea;
    Left := 6;
    Top := 8;
  end;
  shp := TShape.Create(self);
  with shp do begin
    Parent := panel;
    Left := 101;
    Top := 2;
    Height := 23;
    Width := panel.width-128;
    Anchors := [akLeft, akTop, akRight];
    Pen.Color := clRed;
  end;
  ctrlInput := TEdit.Create(self);
  with ctrlInput do begin
    Parent := panel;
    Left := 102;
    Top := 3;
    Width := panel.width-130;
    Anchors := [akLeft, akTop, akRight];
    // tag the index of this admin type control
    Tag := FAdminAreaKeys.Count;
    OnChange := ChangeEditState;
    OnKeyPress := eAdminAreaSearchKeyPress;
  end;
  btn := TImageListButton.Create(self);
  with btn do begin
    Parent := panel;
    Anchors := [akTop, akRight];
    Height := 23;
    Hint := ResStr_GetAdminArea;
    ImageIndex := 5;
    ImageList := dmFormActions.ilButtons;
    Left := panel.Width - 27;
    Top := 2;
    Width := 21;
    OnClick := bbAdminAreaFindClick;
    // tag the index of this admin type control
    Tag := FAdminAreaKeys.Count;
  end;
  result := panel;
  // Create an entry in the lists of admin area keys and types for this control
  FAdminAreaKeys.Add('');
  FAdminAreaTypes.AddObject(node.Attributes['admin_type_key'], ctrlInput);
end;

{-------------------------------------------------------------------------------
  Add a new recorder to the recorders list box
}
procedure TfrmSpeciesCard.AddRecorder(AText, AKey: string);
var
  lKeyData: TKeyData;
begin
  if lbRecorders.Items.IndexOf(AText) = -1 then begin
    //Make new key item
    lKeyData:= TKeyData.Create;
    lKeyData.ItemKey:= AKey;
    //Insert new row
    lbRecorders.Items.AddObject(AText, lKeyData);
    Editing:=true;
    bbRecorderRemove.Enabled:=true;
  end;
end;

//==============================================================================
{ Setup anything we can drag or drop }
procedure TfrmSpeciesCard.SetupDestinationControls;
begin
  RegisterDropComponent(eTaxon, DropTaxon,
                        [TN_TAXON_LIST_ITEM], [CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(eReference, DropReference,
                        [TN_REFERENCE], [CF_JNCCDATA, CF_TEXT]);
  fraLocationInfo.RegisterDragDrop(Self);
  RegisterDropComponent(lbRecorders, DropRecorder,
                        [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA]);
  RegisterDropComponent(eBiotope, DropBiotope,
                        [TN_BIOTOPE_LIST_ITEM], [CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(eDeterminer, DropDeterminer,
                        [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA, CF_TEXT]);
end;

//==============================================================================
procedure TfrmSpeciesCard.SetEditing(const Value: boolean);
begin
  if Value <> FEditing then
  begin
    FEditing:= Value;
    bbSave.Enabled := FEditing;
    bbReset.Enabled := FEditing;
  end;
end;  // SetEditing

//==============================================================================
procedure TfrmSpeciesCard.ChangeEditState(Sender: TObject);
begin
  inherited;
  Editing:=true;
  if fraLocationInfo.eLocation.Text = '' then
    eAdminArea.Text := '';
  // If changing Biotope, blank ListItemKey, so that we can check it's a valid one
  if Sender = eBiotope then
    FBiotopeListItemKey := '';
  // If changing AdminArea, blank AdminAreadKey, so that we can check it's a valid one
  if (Sender is TEdit) and (Copy(TEdit(Sender).Name, 1, 9) = 'adminArea') then
    FAdminAreaKeys[TEdit(Sender).Tag] := '';
end;  // ChangeEditState

//==============================================================================
procedure TfrmSpeciesCard.WMTransferDone(var Msg: TMessage);
begin
  Show;
  Editing:=true;
end;  // WMTransferDone

//==============================================================================
procedure TfrmSpeciesCard.bbSaveClick(Sender: TObject);
var
  ltfDeterminerCheck : boolean;
  lstDetCheckMsg     : string;
  lDetCheckComponent : TWinControl;
  lCursor            : TCursor;
  i                  : integer;
  adminAreaKey, adminTypeKey: TKeyString;
  adminCheck         : Boolean;
  //----------------------------------------------------------------------------
  function CheckCount:boolean;
  var lRowIdx : integer;
  begin
    Result:=true;
    with sgCount do
      for lRowIdx:=1 to RowCount-1 do
        if ((Cells[0,lRowIdx]<>'') and (Cells[1,lRowIdx]='')) or
           ((Cells[0,lRowIdx]='')  and (Cells[1,lRowIdx]<>'')) then begin
          Result:=false;
          Row:=lRowIdx;
          Break;
        end;
  end;  // CheckCount
  //----------------------------------------------------------------------------
  function CheckCountQualifier:boolean;
  var lRowIdx : integer;
  begin
    Result:=true;
    with sgCount do
      for lRowIdx:=1 to RowCount-1 do
        if (Cells[1,lRowIdx]<>'') and (cmbQualifier.Items.IndexOf(Cells[1,lRowIdx]) = -1) then begin
          Result:=false;
          Row:=lRowIdx;
          Break;
        end;
  end;  // CheckCountQualifier
  //----------------------------------------------------------------------------
begin
  inherited;
  ValidateValue(cmbSurvey.KeyValue<>'',ResStr_CannotSaveRecordSelect,cmbSurvey);
  ValidateValue((eTaxon.Text<>'') and dmGeneralData.CheckTaxon(eTaxon, FTaxonListItemKey),ResStr_InvalidOrMissingTaxonName,eTaxon);
  ValidateValue((eReference.Text='') or FdmPlaceCard.CheckReference(FReferenceKey,eReference),
                ResStr_InvalidDocumentSelect, eReference);
  ValidateValue((eSpecimenNumber.Text = '') or (cmbSpecimenType.Text <> ''),
                ResStr_SpecimenTypeMissing, cmbSpecimenType);
  ValidateValue((eSpecimenNumber.Text <> '') or
                ((eSpecimenLocation.Text = '') and (reSpecimenComment.Text = '')),
                ResStr_MustEnterSpecimenNumber, eSpecimenNumber);
  ValidateValue(lbRecorders.Items.Count>0,ResStr_MissingRecorderNameSelect,bbRecorderAdd);
  // Check the date is filled in, then ensure a proper format
  ValidateValue(eDate.Text<>'', ResStr_MissingDateEnter, eDate);
  ValidateCardDate;
  try
    ValidateValue(dmValidation.CheckEventDateAgainstSurvey(cmbSurvey.KeyValue, eDate.VagueDate),
                  ResStr_DateOutsideRange, eDate);
  except
    on e: EValidationDataError do
      raise TExceptionPath.CreateNonCritical(e.Message);
  end;
  fraLocationInfo.Validate(cmbSurvey.KeyValue);
  ValidateValue(cmbSampleType.Text<>'',
                ResStr_MissingSampleTypeSelect,cmbSampleType);
  ValidateValue((eBiotope.Text = '') or (FBiotopeListItemKey <> '') or
                ((FBiotopeListItemKey = '') and FdmPlaceCard.CheckBiotope(FBiotopeListItemKey, eBiotope)),
                ResStr_InvalidBiotopeSelect, eBiotope);
  for i := 0 to FAdminAreaTypes.Count - 1 do begin
    adminAreaKey := TKeyString(FAdminAreaKeys[i]);
    adminTypeKey := TKeyString(FAdminAreaTypes[i]);
    adminCheck := (TEdit(FAdminAreaTypes.Objects[i]).Text <> '') and (FAdminAreaKeys[i] = '')
      and FdmPlaceCard.CheckAdminArea(
        adminAreaKey,
        TEdit(FAdminAreaTypes.Objects[i]),
        adminTypeKey
    );
    ValidateValue((TEdit(FAdminAreaTypes.Objects[i]).Text = '') or (FAdminAreaKeys[i] <> '') or adminCheck,
                ResStr_InvalidAdminAreaSelect, TEdit(FAdminAreaTypes.Objects[i]));
  end;
  ValidateValue(CheckCount,ResStr_SetCountAndCountOf,sgCount);
  ValidateValue(CheckCountQualifier,ResStr_InvalidCountQualifier,sgCount);
  ltfDeterminerCheck := DeterminerComplete(lstDetCheckMsg, lDetCheckComponent);
  ValidateValue(ltfDeterminerCheck,lstDetCheckMsg,lDetCheckComponent);
  ValidateDeterminationDate;

  lCursor:=HourglassCursor;
  try
    InitialiseKeyVariables;
    if not assigned(FSERKeys) then FSERKeys := TStringList.Create;
    FSERKeys.Clear;
    FSurveyEventKey := FdmPlaceCard.GetSurveyEventKey(cmbSurvey.KeyValue,fraLocationInfo.eLocation.Key,
                       fraLocationInfo.eLocationName.Text,
                       fraLocationInfo.eSpatialRef,lbRecorders.Items,eDate.Text, FSERKeys);
    if (FSurveyEventKey = '') and (fraLocationInfo.eLocationName.Text<>'') then
      FSurveyEventKey := FdmPlaceCard.GetSurveyEventKey(cmbSurvey.KeyValue,fraLocationInfo.eLocation.Key,
                       '', fraLocationInfo.eSpatialRef,lbRecorders.Items,eDate.Text, FSERKeys);
    //Line below may not seem necessary but this variable is used in "cleanup"
    FNewSurveyEvent := FSurveyEventKey = '';
    if FNewSurveyEvent then
      AddSurveyEvent;

    FSampleKey := FdmPlaceCard.GetSampleKey(Trunc(eDate.VagueDate.StartDate),
                  Trunc(eDate.VagueDate.EndDate), eDate.VagueDate.DateTypeString,
                  FSurveyEventKey, cmbSampleType.KeyValue,
                  fraLocationInfo.eLocationName.Text, fraLocationInfo.eSpatialRef,
                  FMeasurementControls);
    if FSampleKey = '' then
      CreateSampleRecord;

    if eBiotope.Text <> '' then
      CreateBiotopeOccurrence;

    CreateSpeciesOccurrence;
    SaveCustomMeasurements;
    LastSurvey:=cmbSurvey.KeyValue;
    ResetCard;
    Editing:=false;
    cmbSurvey.SetFocus;
    DefaultCursor(lCursor);
    if FNewSurveyEvent then
      Messagedlg(ResStr_SuccessfullyRecordedEvent, mtInformation, [mbOk], 0)
    else
      Messagedlg(ResStr_SuccessfullyRecordedExistingEvent, mtInformation, [mbOk], 0);
    UpdateObservations;
  except on E:EDatabaseWriteError do
    begin
      Cleanup;
      DefaultCursor(lCursor);
      MessageDlg(ResStr_ExceptionWhenSaving +
                 E.Message, mtInformation, [mbOK], 0);
      Editing := true;
    end;
  end; //try
end;  // bbSaveClick

//==============================================================================
procedure TfrmSpeciesCard.bbResetClick(Sender: TObject);
begin
  inherited;
  //Reset mode
  if fraLocationInfo.eSpatialRef.Modified and FClosingForm then
    fraLocationInfo.eSpatialRef.SpatialRef:='';
  ResetCard;
  Editing:=false;
  cmbSurvey.SetFocus;
end;  // bbResetClick


//==============================================================================
procedure TfrmSpeciesCard.eTaxonKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  FTaxonListItemKey := '';
  if Key=#13 then begin
    if dmGeneralData.CheckTaxon(eTaxon, FTaxonListItemKey) then
      try
        dmGeneralData.CheckTaxonAllowsDataEntry(FTaxonListItemKey);
      except
        on EGeneralData do begin
          eTaxon.Text := '';
          FTaxonListItemKey := '';
          raise;
        end;
      end;
    Key:=#0;
  end;
end;  // eTaxonKeyPress

//==============================================================================
procedure TfrmSpeciesCard.DropTaxon(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TstringList; var ioHandled: boolean);
var
  lTaxonName : string;
  lTaxonKey  : TKeyString;
begin
  if (iSourceData<>nil) and (iSourceData.Header.ItemCount>0) then begin
    lTaxonKey  := iSourceData.Items[0].KeyField1;
    dmGeneralData.CheckTaxonAllowsDataEntry(lTaxonKey);
    if AppSettings.DisplayTaxonCommonNames then
      lTaxonName := dmGeneralData.GetBestTaxonName(lTaxonKey)
    else
      lTaxonName := dmGeneralData.GetTaxonName(lTaxonKey);
    { Assign Taxon key and edit box text only if the taxon is in local database }
    if TaxonInLocalDatabase(lTaxonKey,lTaxonName) then begin
      FTaxonListItemKey := lTaxonKey;
      eTaxon.Text       := lTaxonName;
    end;
  end;
end;  // DropTaxon

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.bbTaxonFindClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actTaxonDiction.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, TaxonUpdate);
end;  // bbTaxonFindClick

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.TaxonUpdate(KeyList: TKeyList);
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then begin
      dmGeneralData.CheckTaxonAllowsDataEntry(KeyList.Items[0].KeyField1);
      FTaxonListItemKey:= KeyList.Items[0].KeyField1;
      eTaxon.Text:= dmGeneralData.GetTaxonName(FTaxonListItemKey);
    end;
  finally
    KeyList.Free;
  end;
end;  // TaxonUpdate

//==============================================================================
procedure TfrmSpeciesCard.eDateExit(Sender: TObject);
begin
  inherited;
  if not FClosingForm then ValidateCardDate;
end;  // eDateExit

procedure TfrmSpeciesCard.ValidateCardDate;
begin
  if eDate.Text<>'' then begin
    ValidateValue(CheckVagueDate(eDate.Text),
                  InvalidDate(ResStr_SampleDate,true,false),eDate);
    eDate.Text:=VagueDateToString(eDate.VagueDate);
  end;
end;  // ValidateCardDate

//==============================================================================
procedure TfrmSpeciesCard.eReferenceKeyPressed(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    FdmPlaceCard.CheckReference(FReferenceKey,eReference);
    Key:=#0;
  end;
end;  // eReferenceKeyPressed

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.DropReference(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TstringList; var ioHandled: boolean);
begin
  if (iSourceData<>nil) and (iSourceData.Header.ItemCount>0) then begin
    //Get key
    FReferenceKey  :=iSourceData.Items[0].KeyField1;
    eReference.Text:=dmGeneralData.GetReferenceText(FReferenceKey);
  end;
end;  // DropReference

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.bbReferenceFindClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actDocuments.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, ReferenceUpdate);
end;  // bbReferenceFindClick

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.ReferenceUpdate(KeyList: TKeyList);
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
end;  // ReferenceUpdate;

//==============================================================================
procedure TfrmSpeciesCard.eDeterminationDateExit(Sender: TObject);
begin
  inherited;
  if not FClosingForm then ValidateDeterminationDate;
end;  // eDeterminationDateExit

//==============================================================================
// Checks the determination date, either on control exist, or form save.
//
procedure TfrmSpeciesCard.ValidateDeterminationDate;
begin
  if eDeterminationDate.Text <> '' then begin
    ValidateValue(
      CheckVagueDate(eDeterminationDate.Text),
      InvalidDate(ResStr_DeterminationDate, true, false),
      eDeterminationDate
    );
    if eDate.Text <> '' then begin
      ValidateValue(
        dmValidation.CheckDeterminationDateAgainstSampleDate(eDate.VagueDate, eDeterminationDate.VagueDate),
        Format(ResStr_DetDateAfterObsDate, [eDate.Text]),
        eDeterminationDate
      );
    end;
    // Standardise the input date formatting.
    eDeterminationDate.Text := VagueDateToString(eDeterminationDate.VagueDate);
  end;
end;  // ValidateDeterminationDate

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.DropRecorder(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TstringList; var ioHandled: boolean);
var
  stItem : string;
  lCount : integer;
begin
  if (iSourceData<>nil) then
    for lCount:=0 to iSourceData.Header.ItemCount-1 do
      if CompareText(iSourceData.Items[lCount].KeyField2, TN_INDIVIDUAL) = 0 then begin
        stItem := dmGeneralData.GetIndividualName(iSourceData.Items[lCount].KeyField1);
        AddRecorder(stItem, iSourceData.Items[lCount].KeyField1);
      end;
end;  // DropRecorder

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.DropDeterminer(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TstringList; var ioHandled: boolean);
begin
  ioHandled := dmGeneralData.DropLinkedEditText(
      eDeterminer,
      iFormat,
      iSourceData,
      dmGeneralData.GetIndividualName,
      iTextStrings);
end;  // DropDeterminer

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.bbRecorderFindClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actNames.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, RecorderUpdate);
end;  // bbRecorderFindClick

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.bbRecorderAddClick(Sender: TObject);
var
  lFind : TdlgFind;
begin
  lFind := TdlgFind.CreateDialog(Self,ResStr_FindRecorderName, ftIndividual);
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
procedure TfrmSpeciesCard.bbRecorderRemoveClick(Sender: TObject);
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
      Editing:=true;
      bbRecorderRemove.Enabled:=Items.Count>0;
    end;
end;  // bbRecorderRemoveClick

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.RecorderUpdate(KeyList: TKeyList);
var
  stItem : string;
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then
      if CompareText(KeyList.Items[0].KeyField2, TN_INDIVIDUAL) = 0 then
      begin
        stItem:=dmGeneralData.GetIndividualName(KeyList.Items[0].KeyField1);
        AddRecorder(stItem, KeyList.Items[0].KeyField1);
      end else
        MessageDlg(ResStr_NoOrgRecorders, mtInformation, [mbOk], 0);
  finally
    KeyList.Free;
  end;
end;  // RecorderUpdate

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.DeterminerUpdate(KeyList: TKeyList);
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then begin
      //Get key
      eDeterminer.Key:= KeyList.Items[0].KeyField1;
      //Get determiner name
      eDeterminer.Text:= dmGeneralData.GetIndividualName(eDeterminer.Key);
    end;
  finally
    KeyList.Free;
  end;
end;  // DeterminerUpdate

//==============================================================================
procedure TfrmSpeciesCard.cmbSampleTypeDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  lCaptionTopSpacing : integer;
  lIndex : integer;
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
  end;  // with
end;  // cmbSampleTypeDrawItem

//==============================================================================
procedure TfrmSpeciesCard.eBiotopeKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if (Key = #13) and (FBiotopeListItemKey = '') then begin
    FdmPlaceCard.CheckBiotope(FBiotopeListItemKey,eBiotope);
    Key := #0;
  end;
end;  // eBiotopeKeyPress

//==============================================================================
procedure TfrmSpeciesCard.eAdminAreaSearchKeyPress(Sender: TObject; var Key: Char);
var
  adminAreaKey, adminTypeKey: TKeyString;
begin
  inherited;
  adminAreaKey := FAdminAreaKeys[TEdit(Sender).Tag];
  adminTypeKey := FAdminAreaTypes[TEdit(Sender).Tag];
  if (Key = #13) and (FAdminAreaKeys[TEdit(Sender).Tag] = '') then begin
    FdmPlaceCard.CheckAdminArea(adminAreaKey, TEdit(Sender), adminTypeKey);
    Key := #0;
  end;
end;  // eAdminAreaSearchKeyPress

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.DropBiotope(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TstringList; var ioHandled: boolean);
begin
  if Assigned(iSourceData) then
    if iSourceData.Header.ItemCount > 0 then begin
      // Set text first, because of OnChange event that clears the key!
      eBiotope.Text := dmGeneralData.GetBiotopeCodeName(iSourceData.Items[0].KeyField1);
      FBiotopeListItemKey := iSourceData.Items[0].KeyField1;
    end;
end;  // DropBiotope

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.DropAdminArea(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TstringList; var ioHandled: boolean);
var
  edit: TEdit;
begin
  if Assigned(iSourceData) then
    if iSourceData.Header.ItemCount > 0 then begin
      edit := TEdit(TClipboardCapability(Sender).DropControl);
      // Set text first, because of OnChange event that clears the key!
      edit.Text := dmGeneralData.GetAdminAreaName(iSourceData.Items[0].KeyField1);
      FAdminAreaKeys[edit.Tag] := iSourceData.Items[0].KeyField1;
    end;
end;  // DropAdminArea

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.bbBiotopeFindClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actBiotopeDiction.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, BiotopeUpdate);
end;  // bbBiotopeFindClick

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.bbAdminAreaFindClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actAdminAreaDiction.Execute;
  FSearchingForAdminTypeIndex := TImageListButton(Sender).Tag;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, AdminAreaUpdate);
end;  // bbAdminAreaFindClick

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.BiotopeUpdate(KeyList: TKeyList);
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

//------------------------------------------------------------------------------
procedure TfrmSpeciesCard.AdminAreaUpdate(KeyList: TKeyList);
begin
  if Assigned(KeyList) then
    try
      if KeyList.Header.ItemCount > 0 then begin
        // Set text first, because of OnChange event that clears the key!
        TEdit(FAdminAreaTypes.Objects[FSearchingForAdminTypeIndex]).Text:=dmGeneralData.GetAdminAreaName(KeyList.Items[0].KeyField1);
        FAdminAreaKeys[FSearchingForAdminTypeIndex] := KeyList.Items[0].KeyField1;
      end;
    finally
      KeyList.Free;
    end;
end;  // AdminAreaUpdate

//==============================================================================
{ Draw combo boxes for qualifier and accuracy data entry in count grid }
procedure TfrmSpeciesCard.sgCountDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  inherited;
  with sgCount do begin
    { hide data entry combos that should not be visible }
    if (gdFocused in State) and (Col<>2) then
      cmbAccuracy.Visible:=false
    else if cmbAccuracy.Visible then
      PositionCombo(sgCount, cmbAccuracy, Col, Row);
    if (gdFocused in State) and (Col<>1) then
      cmbQualifier.Visible:=false
    else if cmbQualifier.Visible then
      PositionCombo(sgCount, cmbQualifier, Col, Row);

    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
  end;
end;  // sgCountDrawCell

//==============================================================================
procedure TfrmSpeciesCard.sgCountClick(Sender: TObject);
begin
  inherited;
  with sgCount do
    if Col=2 then Options:=Options-[goEditing]
             else Options:=Options+[goEditing];
end;  // sgCountClick

//==============================================================================
procedure TfrmSpeciesCard.cmdGridExit(Sender: TObject);
begin
  inherited;
  TComboBox(Sender).Visible:=false;
end;  // cmbGridExit

//==============================================================================
procedure TfrmSpeciesCard.bbDetailsAddClick(Sender: TObject);
begin
  inherited;
  AddLineInGrid(sgCount);
  Editing:=true;
  ActiveControl := sgCount; // focus grid
end;  // bbDetailsAddClick

//==============================================================================
procedure TfrmSpeciesCard.bbDetailsDelClick(Sender: TObject);
begin
  inherited;
  DelLineInGrid(sgCount);
  Editing:=true;
  with sgCount do
    bbDetailsDel.Enabled:=(RowCount>2) or ((RowCount=2) and (Rows[1].CommaText<>',,'));
end;  // bbDetailsDelClick

//==============================================================================
procedure TfrmSpeciesCard.AddSurveyEvent;
var
  i : Integer;
begin
  //Add survey event
  with FdmPlaceCard do
  begin
    with qryInsertSurveyEvent do
    begin
      FSurveyEventKey:=dmGeneralData.GetNextKey(TN_SURVEY_EVENT, 'Survey_Event_Key');
      Parameters.ParamByName('SURVEY_EVENT_KEY').Value        := FSurveyEventKey;
      Parameters.ParamByName('SURVEY_KEY').Value              := cmbSurvey.KeyValue;
      FdmPlaceCard.InitLocalityParams(Parameters, fraLocationInfo);
      Parameters.ParamByName('VAGUE_DATE_START').Value        := Trunc(eDate.VagueDate.StartDate);
      Parameters.ParamByName('VAGUE_DATE_END').Value          := Trunc(eDate.VagueDate.EndDate);
      Parameters.ParamByName('VAGUE_DATE_TYPE').Value         := eDate.VagueDate.DateTypeString;
      Parameters.ParamByName('Entered_By').Value := AppSettings.UserID;
      ExecSQL;

      //Add Survey Event Recorders
      FSERKeys.Clear;
      for i:= 0 to lbRecorders.Items.Count -1 do
      begin
        try
          FSERKeys.Add(dmGeneralData.GetNextKey(TN_SURVEY_EVENT_RECORDER, 'SE_Recorder_Key'));
          Connection.Execute('INSERT INTO Survey_Event_Recorder (' +
              'SE_Recorder_Key, Survey_Event_Key, Name_Key, Recorder_Role_Key, ' +
              'Entered_By) ' +
              'VALUES (' +
              '''' + FSERKeys[FSERKeys.Count -1] + ''', ''' + FSurveyEventKey +
              ''', ''' + TKeyData(lbRecorders.Items.Objects[i]).ItemKey +
              ''', ''' + RECORDER_RECORDER_ROLE_KEY +
              ''', ''' + AppSettings.UserID +
              ''')');
        except
          on E:Exception do begin
            Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Survey Event Recorder]');
          end;
        end;
      end;
    end;
  end;
end;  // AddSurveyEvent

//==============================================================================
procedure TfrmSpeciesCard.CreateSampleRecord;
var
  liCount   : Integer;
begin
  with FdmPlaceCard.qryInsertSample do begin
    FSampleKey := dmGeneralData.GetNextKey(TN_SAMPLE, 'Sample_Key');
    try
      Parameters.ParamByName('Sample_Key').Value       := FSampleKey;
      Parameters.ParamByName('SAMPLE_REFERENCE').Value := GetSampleReference;
      Parameters.ParamByName('VAGUE_DATE_START').Value     := Trunc(eDate.VagueDate.StartDate);
      Parameters.ParamByName('Vague_Date_End').Value := Trunc(eDate.VagueDate.EndDate);
      Parameters.ParamByName('Vague_Date_Type').Value := eDate.VagueDate.DateTypeString;
      FdmPlaceCard.InitLocalityParams(Parameters, fraLocationInfo);
      Parameters.ParamByName('OUTSTANDING_CARD').Value     := 1;
      Parameters.ParamByName('SAMPLE_TYPE_KEY').Value       := cmbSampleType.KeyValue;
      Parameters.ParamByName('SURVEY_EVENT_KEY').Value      := FSurveyEventKey;
      Parameters.ParamByName('Entered_By').Value := AppSettings.UserID;
      ExecSQL;
    except
      on E:Exception do begin
        Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Sample]');
      end;
    end;

    try
      for liCount := 0 to FSERKeys.Count - 1 do
        Connection.Execute('INSERT INTO Sample_Recorder ' +
              '(SAMPLE_KEY, SE_RECORDER_KEY, Entered_By)' +
              'VALUES(''' + FSampleKey + ''', ''' + FSERKeys[liCount] + ''' , ''' +
              AppSettings.UserID + ''')');
    except
      on E:Exception do begin
        Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Sample Recorder]');
      end;
    end;

    // Save reference as internal reference if there is any.
    if FReferenceKey <> '' then begin
      try
        Connection.Execute ('INSERT INTO Sample_Sources(Source_Link_Key, ' +
            'Sample_Key, Source_Key, Original)' +
            'VALUES (''' + dmGeneralData.GetNextKey('Sample_Sources', 'Source_Link_Key') +
            ''', ''' + FSampleKey + ''', ''' + FReferenceKey + ''', 0)');
      except
        on E:Exception do begin
          Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Sample Sources]');
        end;
      end;
    end;  // if FReferenceKey<>''
    // Save link to admin area(s) if present
    for liCount := 0 to FAdminAreaTypes.Count-1 do begin
      if FAdminAreaKeys[liCount] <> '' then begin
        try
          Connection.Execute ('INSERT INTO Sample_Admin_Areas(Sample_Admin_Areas_Key, Admin_Area_Key, ' +
              'Sample_Key, Entered_By)' +
              'VALUES (''' + dmGeneralData.GetNextKey('Sample_Admin_Areas', 'Sample_Admin_Areas_Key') +
              ''', ''' + FAdminAreaKeys[liCount] + ''', ''' + FSampleKey + ''', ''' + AppSettings.UserID + ''')');
        except
          on E:Exception do begin
            Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Sample Admin Areas]');
          end;
        end;
      end;
    end;
  end;  // with FdmPlaceCard.tblSampleStuff
end;  // CreateSampleRecord

(**
 * Gets the sample reference to store. Normally blank, but if using a custom card
 * with sref_in_sample_ref set, then stores the spatial reference in the sample reference
 * converted into whichever system is specified in the attribute's value.
 *)
function TfrmSpeciesCard.GetSampleReference: string;
var
  sref, system: string;
begin
  if FSrefInSampleRef='' then
    result := ''
  else begin
    sref := fraLocationInfo.eSpatialRef.EnteredRef;
    system := fraLocationInfo.eSpatialRef.EnteredSystem;
    result := Copy(ConvertSystems(sref, system, FSrefInSampleRef), 1, 15);
  end;
end;

//==============================================================================
procedure TfrmSpeciesCard.CreateBiotopeOccurrence;
var lVagueDate: TVagueDate;
begin
  with FdmPlaceCard do begin
    // Add the new occurrence and determination record
    with qryInsertBiotopeOccur do begin
      FBiotopeOccKey := dmGeneralData.GetNextKey(TN_BIOTOPE_OCCURRENCE, 'Biotope_Occurrence_Key');
      // Create record
      try
      SQL.Text := 'INSERT INTO Biotope_Occurrence (' +
            'Biotope_Occurrence_Key, Comment, Digitised, Checked, Verified, Sample_Key, ' +
            'Entered_By) ' +
            'VALUES(''' + FBiotopeOccKey +
            ''', '''''+
            ', 0, 0, 0, ''' + FSampleKey +
            ''', ''' + AppSettings.UserID +
            ''')';
        ExecSQL;
        //Now do determination
        if eDeterminationDate.Text = '' then lVagueDate := eDate.VagueDate
                                        else lVagueDate := eDeterminationDate.VagueDate;

        FBiotopeDetKey := dmGeneralData.GetNextKey(TN_BIOTOPE_DETERMINATION, 'Biotope_Determination_Key');
        SQL.Text := 'INSERT INTO Biotope_Determination(' +
            'Biotope_Determination_Key, Biotope_Occurrence_Key, ' +
            'Biotope_List_Item_Key, Determination_Type_Key, ' +
            'Determiner_Role_Key, Determiner, Preferred, Vague_Date_Start, ' +
            'Vague_Date_End, Vague_Date_Type, Entered_By) ' +
            'VALUES(';
        SQL.Text := SQL.Text + '''' +
            FBiotopeDetKey +
            ''', ''' + FBiotopeOccKey +
            ''', ''' + FBiotopeListItemKey +
            ''', ''' + ORIGINAL_DETERMINATION_TYPE_KEY +
            ''', ''' + ORIGINAL_DETERMINER_ROLE_KEY +
            ''', ''' + TKeyData(lbRecorders.Items.Objects[0]).ItemKey +
            ''', 1, ' + IntToStr(Trunc(lVagueDate.StartDate)) +
            ', ' + IntToStr(Trunc(lVagueDate.EndDate)) +
            ', ''' + lVagueDate.DateTypeString +
            ''', ''' + AppSettings.UserID +
            ''')';
        ExecSQL;
      except
        on E:Exception do begin
          Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Biotope Occurrence/Determination]');
        end;
      end;
    end; // with qryInsertBiotopeOccur
  end;  // with FdmPlaceCard
end;  // CreateBiotopeOccurrence

//==============================================================================
procedure TfrmSpeciesCard.CreateSpeciesOccurrence;
var I         : Integer;
    lKeyObject: TKey;
    lVagueDate: TVagueDate;
begin
  //Open tables
  with FdmPlaceCard do begin

    //Create a taxon occurrence record
    FTaxonOccKey:= dmGeneralData.GetNextKey(TN_TAXON_OCCURRENCE, 'Taxon_Occurrence_Key');
    FTaxonDetKey:= dmGeneralData.GetNextKey(TN_TAXON_DETERMINATION, 'Taxon_Determination_Key');
    with qryInsertTaxonOccur do begin
      try
        SQL.Text :=  'INSERT INTO Taxon_Occurrence (' +
            'Taxon_Occurrence_Key, Comment,  Checked, Verified, Sample_Key, ' +
            'Provenance, Zero_Abundance, Confidential, Substrate_Key, Record_Type_Key, ' +
            'Entered_By)' +
            'VALUES(''' + FTaxonOccKey +
            ''', ' + QuotedStr(reComments.Lines.Text) +
            ', 0, 0, ''' + FSampleKey +
            ''', ''' + IfThen(cmbProvenance.Text = '', NONE_RECORD, cmbProvenance.Text) +
            ''', ' + IntToStr(Ord(dmGeneralData.CheckZeroAbundance(FTaxonOccKey))) +
            ', 0, ''' + cmbSubstrate.KeyValue +
            ''', ''' + cmbRecordType.KeyValue +
            ''', ''' + AppSettings.UserID +
            ''')';
        ExecSQL;

        // If a specimen number has been entered, add a specimen.
        if eSpecimenNumber.Text <> '' then
          CreateSpecimen(FTaxonOccKey);

        //Now insert determination
        if eDeterminationDate.Text = '' then lVagueDate := eDate.VagueDate
                                        else lVagueDate := eDeterminationDate.VagueDate;
        SQL.Text := 'INSERT INTO Taxon_Determination(' +
            'Taxon_Determination_Key, Taxon_Occurrence_Key, Taxon_List_Item_Key, ' +
            'Determination_Type_Key, ' +
            'Determiner_Role_Key, Determiner, Preferred, Vague_Date_Start, ' +
            'Vague_Date_End, Vague_Date_Type, Entered_By) ' +
            'VALUES(''' + FTaxonDetKey +
            ''', ''' + FTaxonOccKey +
            ''', ''' + FTaxonListItemKey +
            ''', ''' + ORIGINAL_DETERMINATION_TYPE_KEY +
            ''', ''' + ORIGINAL_DETERMINER_ROLE_KEY +
            ''', ''' + GetDeterminerKey +
            ''', 1, ' + IntToStr(Trunc(lVagueDate.StartDate)) +
            ', ' + IntToStr(Trunc(lVagueDate.EndDate)) +
            ', ''' + lVagueDate.DateTypeString +
            ''', ''' + AppSettings.UserID +
            ''')';
        ExecSQL;
      except
        on E:Exception do begin
          Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Taxon Occurrence/Determination]');
        end;
      end;
    end;// with qryInsertTaxonOccur
    with qryInsertTaxOccurData do begin
      try
        for i:= sgCount.FixedRows to sgCount.RowCount - 1 do
          if sgCount.Cells[0, i] <> '' then begin
            //For each count in the grid
            FtfHaveData := True;
            Parameters.ParamByName('SiteID').Value := AppSettings.SiteID;
                  // Set other values
            Parameters.ParamByName('TAXON_OCCURRENCE_KEY').Value := FTaxonOccKey;
            Parameters.ParamByName('DATA').Value := sgCount.Cells[0, i];
            if sgCount.Cells[1, i] = '' then
              Parameters.ParamByName('MEASUREMENT_QUALIFIER_KEY').Value := NONE_ABUNDANCE_QUALIFIER_KEY // None
            else begin
              // find measurement key from cmbQualifiers
              with cmbQualifier.Items do
                lKeyObject := TKey(Objects[IndexOf(sgCount.Cells[1, i])]);
              Parameters.ParamByName('MEASUREMENT_QUALIFIER_KEY').Value := lKeyObject.Key;
            end;
            if sgCount.Cells[2, i] <> ResStr_None then //just in case.
              Parameters.ParamByName('ACCURACY').Value                 := sgCount.Cells[2, i];
            Parameters.ParamByName('MEASUREMENT_UNIT_KEY').Value := COUNT_MEASUREMENT_UNIT_KEY;
            Parameters.ParamByName('Entered_By').Value := AppSettings.UserID;
            ExecSQL;
          end;
      except
        on E:Exception do begin
          Raise EDatabaseWriteError.Create(ResStr_CreateFail + ' ' + E.Message + ' [Taxon Occurrence Data]');
        end;
      end;
    end; // with qryInsertTaxOccurData
    with qryInsertTaxonOccur do begin
      try
        SQL.Text := 'UPDATE Taxon_Occurrence SET Zero_Abundance = ' +
            IntToStr(Ord(dmGeneralData.CheckZeroAbundance(FTaxonOccKey))) +
            'WHERE Taxon_Occurrence_Key = ''' + FTaxonOccKey + '''';
        ExecSQL;
      except
        on E:Exception do begin
          raise EDatabaseWriteError.Create(ResStr_WriteRecFail, E);
        end;
      end;
    end; // with qryInsertTaxonOccur

  end;  // with FdmPlaceCard
  // Now insert the documents if AddDocsToOccurrence is set to true
  If (AppSettings.AddDocsToOccurrence) then begin;
    try
      dmDatabase.RunStoredProc('usp_TaxonOccurrenceSource_Insert',
         ['@TaxonOccurrenceKey', FTaxonOccKey]);
    except
      on E:Exception do
        Raise EDatabaseWriteError.Create(ResStr_WriteRecFail + ' ' + E.Message + ' [Taxon Occurrence Sources]');
      end;
  end;

end;  // CreateSpeciesOccurrence

(*******************************************************************************
 * Use any custom measurement controls that have values to create
 * measurement records.
 *)
procedure TfrmSpeciesCard.SaveCustomMeasurements;
var i: integer;
    def: string;
    ctrl: TEdit;
    defTokens: TStringList;
    query: TJNCCQuery;
    existing: boolean;
begin
  defTokens := TStringList.Create;
  defTokens.Delimiter := ':';
  try
    for i := 0 to FMeasurementControls.Count-1 do begin
      ctrl := TEdit(FMeasurementControls.Objects[i]);
      if ctrl.Text<>'' then begin
        def := FMeasurementControls[i];
        defTokens.delimitedText := def;
        if defTokens[0]='sample' then begin
          dmGeneralData.qryAllPurpose.SQL.Text := Format(
              'SELECT 1 FROM %s_Data WHERE Sample_Key=''%s'' AND Data=''%s'' ' +
              'AND Accuracy=''%s'' AND Measurement_Unit_Key=''%s'' AND Measurement_Qualifier_Key=''%s''',
              [defTokens[0], FSampleKey, StringReplace(ctrl.Text, '''', '''''', [rfReplaceAll]),
              StringReplace(defTokens[4], '''', '''''', [rfReplaceAll]), defTokens[2], defTokens[3]]);
          dmGeneralData.qryAllPurpose.Open;
          existing := dmGeneralData.qryAllPurpose.RecordCount>0;
          dmGeneralData.qryAllPurpose.Close;
        end else
          // only samples can be re-used when saving several forms
          existing := false;
        if (not existing) then
        begin
          if defTokens[0]='sample' then begin
            query := FdmPlaceCard.qryInsertSampleData;
            query.Parameters.ParamByName('Sample_Key').Value := FSampleKey;
          end
          else begin
            query := FdmPlaceCard.qryInsertTaxOccurData;
            query.Parameters.ParamByName('Taxon_Occurrence_Key').Value := FTaxonOccKey;
          end;
          query.Parameters.ParamByName('SiteID').Value := AppSettings.SiteID;
          query.Parameters.ParamByName('Data').Value := ctrl.Text;
          query.Parameters.ParamByName('Measurement_Unit_Key').Value := defTokens[2];
          query.Parameters.ParamByName('Measurement_Qualifier_Key').Value := defTokens[3];
          query.Parameters.ParamByName('Accuracy').Value := defTokens[4];
          query.Parameters.ParamByName('Entered_By').Value := AppSettings.UserID;
          query.ExecSQL;
        end;
      end;
    end;
  finally
    defTokens.free;
  end;
end;

{-------------------------------------------------------------------------------
  Creates a new specimen record.
}
procedure TfrmSpeciesCard.CreateSpecimen(const taxonOccurrenceKey : string);
var
  specimenKey : string;
  location : string;
  commentStream : TStringStream;
  commentString : string;
  sql : string;
begin
  commentStream := TStringStream.Create('');

  try
    try
      // Interpret a blank specimen location as a 'null' value.
      if eSpecimenLocation.Text = '' Then
        location := 'NULL'
      else
        location := QuotedStr(eSpecimenLocation.Text);

      specimenKey := dmGeneralData.GetNextKey('SPECIMEN', 'Specimen_Key');
      reSpecimenComment.Lines.SaveToStream(commentStream);

      // Quotes in the RTF for SQL need escaping by making them double.
      // Skip the null termination character of the stream (size - 1).
      commentStream.Position := 0;
      commentString := StringReplace(
          StringReplace(
              commentStream.ReadString(commentStream.Size - 1),
              #13#10, '', [rfReplaceAll]),
          #39, #39#39, [rfReplaceAll]);

      sql := 'INSERT INTO Specimen (' +
             'SPECIMEN_KEY, NUMBER, SPECIMEN_TYPE_KEY, LOCATION, ' +
             'TAXON_OCCURRENCE_KEY, ENTERED_BY, COMMENT) ' +
             'VALUES(''' + specimenKey + ''','
                         + QuotedStr(eSpecimenNumber.Text) + ','
                         + '''' + cmbSpecimenType.KeyValue + ''','
                         + location + ','
                         + '''' + taxonOccurrenceKey + ''','
                         + '''' + AppSettings.UserID + ''','
                         + '''' + commentString + ''')';
      dmDatabase.ExecuteSQL(sql, False);
    except
      on E:Exception do
        raise EDatabaseWriteError.Create(ResStr_WriteRecFail + #13#10#13#10 + E.Message, E);
    end;
  finally
    commentStream.Free;
  end;
end;

//==============================================================================
procedure TfrmSpeciesCard.cmbProvenanceChange(Sender: TObject);
begin
  inherited;
  with cmbProvenance do
    if ItemIndex = 0 then Text := '';
  Editing := true;
end;  // cmbProvenanceChange

//==============================================================================
{ Update the grid with the contents of the combo box, either for Qualifier or
     Accuracy }
procedure TfrmSpeciesCard.cmbCountAttributeChange(Sender: TObject);
begin
  inherited;
  with sgCount do begin
    if Sender is TComboBox then
      Cells[Col,Row]:=TComboBox(Sender).Text;
    bbDetailsDel.Enabled:=(RowCount>2) or ((RowCount=2) and (Rows[1].CommaText<>',,'));
  end;
  Editing:=true;
end;  // cmbAccuracyChange or cmbQualifierChange

//==============================================================================
procedure TfrmSpeciesCard.cmbRecordTypeChange(Sender: TObject);
begin
  inherited;
  if cmbRecordType.ItemIndex = 0 then
    cmbRecordType.Text := '';
  Editing:=true;
end;  // cmbRecordTypeChange

//==============================================================================
procedure TfrmSpeciesCard.cmbSubstrateChange(Sender: TObject);
begin
  inherited;
  if cmbSubstrate.ItemIndex = 0 then
    cmbSubstrate.Text := '';
  Editing:=true;
end;  // cmbSubstrateChange

//==============================================================================
procedure TfrmSpeciesCard.ResetCard;
var i: integer;
begin
  with sgCount do begin
    for i := 1 to RowCount - 1 do
      Rows[i].Clear;
    RowCount:=2;
  end;

  cmbSurvey.KeyValue := LastSurvey;
  if cmbSurvey.ItemIndex = -1 then
    cmbSurvey.KeyValue := '';

  bbDetailsDel.Enabled    := false;
  bbRecorderRemove.Enabled:= lbRecorders.Items.Count>0;
  cmbProvenance.Text      := NONE_RECORD;
  cmbProvenance.ItemIndex := 0;
  cmbRecordType.KeyValue  := NONE_RECORD_KEY;
  cmbSubstrate.KeyValue   := NONE_RECORD_KEY;
  eDeterminer.Text        := '';
  eDeterminationDate.Text := '';
  eBiotope.Text           := '';
  for i := 0 to FAdminAreaTypes.Count-1 do
    TEdit(FAdminAreaTypes.Objects[i]).Text   := '';
  reComments.Text         := '';
  eSpecimenNumber.Text    := '';
  cmbSpecimenType.ClearSelection;
  reSpecimenComment.Clear;
  fraLocationInfo.eSpatialRef.Modified     := false;
end;  // ResetCard

//==============================================================================
function TfrmSpeciesCard.GetDeterminerKey: string;
begin
  if eDeterminer.Text = '' then
    Result := TKeyData(lbRecorders.Items.Objects[0]).ItemKey
  else
    Result := eDeterminer.Key;
end;  // GetDeterminerKey

//==============================================================================
procedure TfrmSpeciesCard.lbRecordersKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
  if Key = #13 then
    bbRecorderAddClick(Self);
end;  // lbRecordersKeyPress

//==============================================================================
procedure TfrmSpeciesCard.sgCountSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  inherited;
  Editing := true;
  bbDetailsDel.Enabled:=true;
end;  // sgCountSetEditText

//==============================================================================
procedure TfrmSpeciesCard.reCommentsChange(Sender: TObject);
begin
  inherited;
  Editing := true;
end;  // reCommentsChange

//==============================================================================
procedure TfrmSpeciesCard.Cleanup;
var lFields: TStringList;
begin
  lFields := TStringList.Create;
  //Tables must be cleaned up in this order;
  Try
    Try
      //Taxon_Occurrence_data;
      if FtfHaveData then
      begin
        lFields.Add('Taxon_Occurrence_Key');
        lFields.Add(FTaxonOccKey);
        // Do detail records in Taxon_Occurrence_Data
        FdmPlaceCard.CleanupTable(TN_TAXON_OCCURRENCE_DATA, lFields);
        // Do detail records in Taxon_Determination
        FdmPlaceCard.CleanupTable(TN_TAXON_DETERMINATION, lFields);
        // Now the master records in Taxon_Occurrence
        FdmPlaceCard.CleanupTable(TN_TAXON_OCCURRENCE, lFields);
        lFields.Clear;
      end;

      // Biotope_Occurrence, Biotope_Determination and Biotope_Occurrence_Data
      if FBiotopeOccDataKey <> '' then
      begin
        lFields.Add('Biotope_Occurrence_Key');
        lFields.Add(FBiotopeOccKey);
        // Do detail records in Biotope_Occurrence_Data
        FdmPlaceCard.CleanupTable(TN_BIOTOPE_OCCURRENCE_DATA, lFields);
        // Do detail records in Biotope_Determination
        FdmPlaceCard.CleanupTable(TN_BIOTOPE_DETERMINATION, lFields);
        // Now the master records in Biotope_Occurrence
        FdmPlaceCard.CleanupTable(TN_BIOTOPE_OCCURRENCE, lFields);
        lFields.Clear;
      end;

      // Sample, Sample_Sources and Sample_Recorder
      if (FSampleKey <> '') then
      begin
        lFields.Add('Sample_Key');
        lFields.Add(FSampleKey);
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
        // Survey_Evetn and Survey_Event_Recorders
        lFields.Add('Survey_Event_Key');
        lFields.Add(FSurveyEventKey);
        // Do detail records in Survey_Event_Recorder
        FdmPlaceCard.CleanupTable(TN_SURVEY_EVENT_RECORDER, lFields);
        // Now the master records in Survey_Event
        FdmPlaceCard.CleanupTable(TN_SURVEY_EVENT, lFields);
        lFields.Clear;
      end;
    except on E:ETableCleanupError do
      MessageDlg(ResStr_FailureToSave, mtWarning, [mbOK], 0);
    end;
  Finally
    lFields.Free;
  end;
end;  // CleanUp

//==============================================================================
procedure TfrmSpeciesCard.InitialiseKeyVariables;
begin
  FSurveyEventKey := '';
  if assigned(FSERKeys) then
    FSERKeys.clear;
  FSampleKey := '';
  FBiotopeOccKey := '';
  FBiotopeDetKey := '';
  FBiotopeOccDataKey := '';
  FTaxonOccKey := '';
  FTaxonDetKey := '';
  FtfHaveData := False;
end;  // InitialiseKeyVariables

//==============================================================================
function TfrmSpeciesCard.DeterminerComplete(var lstDetCheckMsg: string;
  var DetCheckComponent: TWinControl): boolean;
begin
  if (eDeterminer.Text <> '') and (eDeterminationDate.Text = '') then
  begin
    Result := false;
    lstDetCheckMsg := ResStr_SpecifyDetDateForDeterminer;
    DetCheckComponent := eDeterminationDate;
  end else
  if (eDeterminer.Text = '') and (eDeterminationDate.Text <> '') then
  begin
    Result := false;
    lstDetCheckMsg := ResStr_SpecifyDeterminerForDate;
    DetCheckComponent := eDeterminer;
  end else
  if (eDeterminer.Text <> '') and (eDeterminationDate.Text <> '') then
  begin
    if dmGeneralData.CheckIndividual(eDeterminer) then
      Result := true
    else
    begin
      Result := false;
      lstDetCheckMsg := ResStr_UnableToMatchDeterminer;
      DetCheckComponent := eDeterminer;
    end;
  end else
    Result := true;
end;  // DeterminerComplete

//==============================================================================
procedure TfrmSpeciesCard.UpdateObservations;
var
  lForm : TForm;
begin
  inherited;
  lForm := frmMain.GetForm(TfrmObservations, false, false);
  if lForm <> nil then
    TfrmObservations(lForm).RefreshSurvey(cmbSurvey.KeyValue,FSurveyEventKey,FSampleKey);
end;  // UpdateObservations


//==============================================================================
procedure TfrmSpeciesCard.reCommentsEnter(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(true);
end;  // reCommentsEnter

//==============================================================================
procedure TfrmSpeciesCard.reCommentsExit(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(false);
end;  // reCommentsExit

//==============================================================================
procedure TfrmSpeciesCard.WMRefreshTermLists(var Msg: TMessage);
begin
  cmbSampleType.Active := false;
  cmbSampleType.Active := true;
  cmbSubstrate.Active  := false;
  cmbSubstrate.Active  := true;
  cmbRecordType.Active := false;
  cmbRecordType.Active := true;
end;  // WMRefreshTermLists

//==============================================================================
procedure TfrmSpeciesCard.SetDisplaySystem;
begin
  fraLocationInfo.eSpatialRef.DisplaySystem := AppSettings.SpatialRefSystem;
end;

//==============================================================================
procedure TfrmSpeciesCard.WMRefreshSpatialRefSystem(var Msg: TMessage);
begin
  SetDisplaySystem;
end;

//==============================================================================
procedure TfrmSpeciesCard.WMUpdateSurveyCombo(var Msg: TMessage);
begin
  cmbSurvey.Active := False;
  cmbSurvey.Active := True;
end;

//==============================================================================
procedure TfrmSpeciesCard.WMRefreshNames(var Msg: TMessage);
var liIndex: integer;
begin
  eDeterminer.Text:= dmGeneralData.GetIndividualName(eDeterminer.Key);

  with lbRecorders do
    for liIndex := 0 to Items.Count-1 do
      Items[liIndex] := dmGeneralData.GetIndividualName(TKeyData(Items.Objects[liIndex]).ItemKey);
end;

//==============================================================================
procedure TfrmSpeciesCard.WMRefreshLocationDetails(var Msg: TMessage);
begin
  fraLocationInfo.Refresh;
end;

//==============================================================================
procedure TfrmSpeciesCard.WMRefreshBiotopeDic(var Msg: TMessage);
begin
  eBiotope.Text := dmGeneralData.GetBiotopeCodeName(FBiotopeListItemKey);
end;

//==============================================================================
procedure TfrmSpeciesCard.WMRefreshColours(var Msg: TMessage);
begin
  SetRequiredFieldsColourState(true,[cmbSurvey,eTaxon,eDate,cmbSampleType,lbRecorders]);
  Repaint;
end;

//==============================================================================
procedure TfrmSpeciesCard.WMRefreshTaxonDic(var Msg: TMessage);
begin
  eTaxon.Text:= dmGeneralData.GetTaxonName(FTaxonListItemKey);
end;

//==============================================================================
procedure TfrmSpeciesCard.WMRefreshDocuments(var Msg: TMessage);
begin
  eReference.Text:=dmGeneralData.GetReferenceText(FReferenceKey);
end;

//==============================================================================
{ Checks whether taxon is in the local database or on CD }
function TfrmSpeciesCard.TaxonInLocalDatabase(ATaxonKey: TKeyString;
  ATaxonName: string): boolean;
begin
  if (dmGeneralData.CheckTaxonInstalled(ATaxonKey, ATaxonName) = 1) then
    Result := True
  else // I don't think this should happen, but if it does...
    Result := False;
end; // TaxonInLocalDatabase

//==============================================================================
procedure TfrmSpeciesCard.sgCountKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  If Key=#13 then begin // return key
    if sgCount.Col < 2 then
      sgCount.Col := sgCount.Col + 1
    else begin
      sgCount.Col :=0;
      ActiveControl := sgCount;
    end;
    Key:=#0;
  end;
end;

//==============================================================================
procedure TfrmSpeciesCard.sgCountSelectCell(Sender: TObject; ACol, ARow: Integer;
    var CanSelect: Boolean);
var
  lComboBox: TComboBox;
begin
  inherited;
  { Default - hide combo boxes }
  cmbQualifier.Visible := False;
  cmbAccuracy.Visible  := False;

  if (ARow > 0) and (ACol > 0) then begin
    if ACol = 1 then begin
      lComboBox := cmbQualifier;
      // select correct item
      cmbQualifier.ItemIndex := cmbQualifier.Items.IndexOf(sgCount.Cells[ACol,ARow]);
    end else begin
      lComboBox        := cmbAccuracy;
      cmbAccuracy.Text := sgCount.Cells[ACol,ARow];
    end;
    PositionCombo(sgCount, lComboBox, ACol, ARow);
    lComboBox.Visible := True;
    ActiveControl     := lComboBox;
  end; // if focused and not in title row
end;

{-------------------------------------------------------------------------------
  Positions a combo box at the specified position in the supplied grid.
}
procedure TfrmSpeciesCard.PositionCombo(parentGrid: TStringGrid; comboBox: TComboBox;
    targetColumn, targetRow: integer);
var
  selectedCell: TRect;
begin
  // Sorry about all the + 1 and - 1 stuff, apparently when they made delphi
  // they decided to make 'CellRect' more of an estimate than an exact value!
  selectedCell    := parentGrid.CellRect(targetColumn, targetRow);
  comboBox.Left   := selectedCell.Left + parentGrid.Left + 1;
  comboBox.Top    := selectedCell.Top + parentGrid.Top + 1;
  comboBox.Width  := selectedCell.Right - selectedCell.Left + 3;
  comboBox.Height := selectedCell.Bottom - selectedCell.Top - 1;
end;

//==============================================================================

{This writes a boolean registry key saying which survey has
previously been used.}
procedure TfrmSpeciesCard.SetLastSurvey(const Value: TKeyString);
var
  lReg: TRegistry;
begin
 lReg:=TRegistry.Create;
  with lReg do
    try
      if OpenKey(REG_KEY_SETTINGS, True) then begin
        WriteString('Species Card Survey',Value);
        CloseKey;
      end;
    finally
      Free;
    end;
end;

//==============================================================================
{This reads a boolean registry key saying which survey has
previously been used.}
function TfrmSpeciesCard.GetLastSurvey: TkeyString;
var
  lReg: TRegistry;
begin
  Result:='';
  lReg:=TRegistry.Create;
  with lReg do
    try
      if OpenKey(REG_KEY_SETTINGS, True) then begin
        if lReg.ValueExists('Species Card Survey') then
           Result:=lReg.ReadString('Species Card Survey');
        CloseKey;
      end;
    finally
      Free;
    end;
end;

//==============================================================================
procedure TfrmSpeciesCard.eReferenceDblClick(Sender: TObject);
begin
  inherited;
  if (FReferenceKey <> '') and (eReference.Text <> '') then
    dmGeneralData.SourcesShowInternal(Sender, FReferenceKey);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSpeciesCard.UpdateMapWindowSelector;
begin
  fraLocationInfo.UpdateMapWindowSelector;
end;

{-------------------------------------------------------------------------------
  Synch the admin areas box when the location changes
}
procedure TfrmSpeciesCard.fraLocationInfoUpdateLocation(Sender: TObject);
begin
  if fraLocationInfo.eLocation.Key='' then
    eAdminArea.Text := ''
  else
    eAdminArea.Text := FdmPlaceCard.GetAdminAreas(fraLocationInfo.eLocation.Key);
end;

{-------------------------------------------------------------------------------
  if location info changes, update the save button state
}
procedure TfrmSpeciesCard.fraLocationInfoChangedContent(Sender: TObject);
begin
  Editing := true;
end;

{-------------------------------------------------------------------------------
  F9 return data link for determiner control
}
procedure TfrmSpeciesCard.eDeterminerGetData(Sender: TObject);
begin
  inherited;
  dmFormActions.actNames.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, DeterminerUpdate);
end;

{-------------------------------------------------------------------------------
  Find dialog for determiner control
}
procedure TfrmSpeciesCard.eDeterminerFindData(Sender: TObject);
begin
  inherited;
  dmGeneralData.CheckIndividual(eDeterminer);
end;

{-------------------------------------------------------------------------------
  F11 hotkey inserts current user into Recorders list
}
procedure TfrmSpeciesCard.lbRecordersKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key=VK_F11 then
    AddRecorder(AppSettings.UserName, AppSettings.UserId);
end;

{-------------------------------------------------------------------------------
  If the specimen number is blank, the specimen type is cleared as well.
}
procedure TfrmSpeciesCard.eSpecimenNumberChange(Sender: TObject);
begin
  inherited;
  if eSpecimenNumber.Text = '' then
    cmbSpecimenType.ClearSelection;
  ChangeEditState(Sender);
end;

{-------------------------------------------------------------------------------
  When the main panel is resized, resize its contents to fit.
}
procedure TfrmSpeciesCard.pnlSpeciesResize(Sender: TObject);
var
  halfWidth, i : Integer;

begin
  inherited;
  if FCustomCard then begin
    for i := 0 to ComponentCount-1 do begin
      if (Components[i] is TPanel) then begin
        if (TPanel(Components[i]).Align = alLeft) and (TPanel(Components[i]).Parent.ControlCount > 1) then begin
          // tags indicate percentage width panels, otherwise we equally divide the parent space
          if TPanel(Components[i]).Tag>0 then
            TPanel(Components[i]).Width := TPanel(Components[i]).Parent.Width * TPanel(Components[i]).Tag div 100
          else if TPanel(Components[i]).Tag <> NO_RESIZE then
            TPanel(Components[i]).Width := TPanel(Components[i]).Parent.Width div
                TPanel(Components[i]).Parent.ControlCount;
        end;
      end;
    end;
  end else
  begin
    // Enforce a minimum width of 670 pixels for the main panel - if the form is
    // made smaller, it will hide part of panel but won't shrink controls further.
    pnlMain.Width  := Max(670, pnlSpecies.Width);
    pnlMain.Height := pnlSpecies.Height;
    halfWidth      := pnlMain.width div 2;
    pnlLeft.Width  := halfWidth;
    pnlRight.Left  := pnlLeft.Left + pnlLeft.Width;
    pnlRight.Width := halfWidth;
  end;
  // fix up some controls that don't have anchors
  fraLocationInfo.eLocation.Width := fraLocationInfo.Width - fraLocationInfo.eLocation.Left - 6;
  fraLocationInfo.eLocationName.Width := fraLocationInfo.Width - fraLocationInfo.eLocationName.Left - 6;
  fraLocationInfo.eSpatialRef.Width := fraLocationInfo.Width - fraLocationInfo.eSpatialRef.Left - 6;
  eDeterminer.Width := pnlCtrlDeterminer.Width - eDeterminer.Left - 7;
end;

end.
