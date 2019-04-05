//==============================================================================
//  Unit:        TaxonOccur
//
//  Implements:  TfrmTaxonOccurrences
//
//  Description: This form is docked on the Observations screen to allow details
//               of a selected tqaxon occurrence to be viewed and/or edited.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 223 $
//    $Date: 8/04/10 17:22 $
//    $Author: Andrewkemp $
//
//==============================================================================

unit TaxonOccur;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons, Menus, Grids, Mask, VagueDateEdit,
  BaseFormUnit, CompositeComponent, Sources, Measurements, DropStruct,
  DropSource, DataClasses, TaxonOccurrData, DeterminationData, Db, DBCtrls,
  DBListCombo, VagueDate, BaseDockedForm, ValidationData, Constants, OnlineHelp,
  HierarchyNodes, GeneralFunctions, ImageListButton, DatabaseAccessADO,
  SpatialRefFuncs, BaseOccurrenceDetail, Variants, AddinCompositeComponent,
  AddinLinkedControls;

type
  TfrmTaxonOccurrences = class(TfrmBaseOccurrenceDetail)
    pnlDetails: TPanel;
    pnlInner: TPanel;
    lblTaxonName: TLabel;
    pcTaxonOccurrence: TPageControl;
    tsGeneral: TTabSheet;
    bvlGeneral: TBevel;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    Label28: TLabel;
    lblVerificationStatus: TLabel;
    lblProvenance: TLabel;
    Label1: TLabel;
    Label18: TLabel;
    cmbProvenance: TComboBox;
    dbeSurveyorRef: TDBEdit;
    dbreComments: TDBRichEdit;
    dbcbChecked: TDBCheckBox;
    dbcbConfidential: TDBCheckBox;
    mmCount: TMemo;
    dbcmbRecordType: TDBListCombo;
    dbcmbSubstrate: TDBListCombo;
    cmbDeterminationType: TDBListCombo;
    tsDeterminations: TTabSheet;
    tsMeasurements: TTabSheet;
    Measurements: TMeasurements;
    tsRelatedOccurrences: TTabSheet;
    tsSpecimens: TTabSheet;
    gbSpecimenDetails: TGroupBox;
    Label26: TLabel;
    Label27: TLabel;
    Label13: TLabel;
    Label29: TLabel;
    Label16: TLabel;
    bbSpecimenAccept: TImageListButton;
    bbSpecimenDiscard: TImageListButton;
    eSpecNumber: TEdit;
    eSpecDate: TVagueDateEdit;
    reSpecComments: TRichEdit;
    dbcmbSpecType: TDBListCombo;
    eSpecLocation: TEdit;
    tsSources: TTabSheet;
    Sources: TSources;
    bbSave: TImageListButton;
    bbCancel: TImageListButton;
    pnlDetsTop: TPanel;
    sgDeterminations: TStringGrid;
    bbDetAdd: TImageListButton;
    bbDetEdit: TImageListButton;
    bbDetDel: TImageListButton;
    pnlDetsBottom: TPanel;
    gbDetDetails: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Shape1: TShape;
    Label9: TLabel;
    Label10: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Shape4: TShape;
    bbDetAccept: TImageListButton;
    bbDetDiscard: TImageListButton;
    reDetComments: TRichEdit;
    eTaxon: TEdit;
    eWork: TEdit;
    cbPreferred: TCheckBox;
    eDetDate: TVagueDateEdit;
    dbcmbRole: TDBListCombo;
    dbcmbType: TDBListCombo;
    bbFindTaxon: TImageListButton;
    bbFindRef: TImageListButton;
    eDeterminer: TNameLinkedEdit;
    splDets: TSplitter;
    pnlRelatedTop: TPanel;
    Shape5: TShape;
    sgRelOcc: TStringGrid;
    bbRelOccAdd: TImageListButton;
    bbRelOccEdit: TImageListButton;
    bbRelOccDel: TImageListButton;
    pnlRelatedBottom: TPanel;
    gbRelOccDetails: TGroupBox;
    Label11: TLabel;
    Label12: TLabel;
    Label4: TLabel;
    Shape3: TShape;
    bbRelOccDiscard: TImageListButton;
    bbRelOccAccept: TImageListButton;
    eRelatedTaxon: TEdit;
    reOccComments: TRichEdit;
    dbcmbRelType: TDBListCombo;
    splRelated: TSplitter;
    pnlSpecimensTop: TPanel;
    sgSpecimens: TStringGrid;
    bbSpecimenAdd: TImageListButton;
    bbSpecimenEdit: TImageListButton;
    bbSpecimenDel: TImageListButton;
    splSpecimens: TSplitter;
    lblReviewSummary: TLabel;
    tsPrivate: TTabSheet;
    sgPrivateOcc: TStringGrid;
    gbPrivateDetails: TGroupBox;
    Label20: TLabel;
    Label21: TLabel;
    bbPrivateAccept: TImageListButton;
    bbPrivateDiscard: TImageListButton;
    ePrivateItemName: TEdit;
    ePrivateDetail: TEdit;
    bbPrivateAdd: TImageListButton;
    bbPrivateEdit: TImageListButton;
    bbPrivateDel: TImageListButton;
    rePrivateComment: TRichEdit;
    Label17: TLabel;
    Label19: TLabel;
    dbcmbPrivateType: TDBListCombo;
    Label22: TLabel;
    ePrivateItemDate: TEdit;
    ePrivateItemValue: TEdit;
    Label23: TLabel;
    lblMetadata: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbSpecimenAddClick(Sender: TObject);
    procedure bbSpecimenAcceptClick(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: Char);
    procedure bbFindTaxonClick(Sender: TObject);
    procedure bbRelatedTaxonFindClick(Sender: TObject);
    procedure bbRelOccAddClick(Sender: TObject);
    procedure bbRelOccAcceptClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure EnterRTF(Sender: TObject);
    procedure ExitRTF(Sender: TObject);
    procedure eWorkKeyPress(Sender: TObject; var Key: Char);
    procedure bbDetDiscardClick(Sender: TObject);
    procedure bbDetAcceptClick(Sender: TObject);
    procedure bbRelOccEditClick(Sender: TObject);
    procedure bbRelOccDelClick(Sender: TObject);
    procedure bbRelOccDiscardClick(Sender: TObject);
    procedure bbSpecimenDiscardClick(Sender: TObject);
    procedure bbSpecimenEditClick(Sender: TObject);
    procedure bbSpecimenDelClick(Sender: TObject);
    procedure bbDetAddClick(Sender: TObject);
    procedure bbDetEditClick(Sender: TObject);
    procedure bbDetDelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bbCancelClick(Sender: TObject);
    procedure sgDeterminationsDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgDeterminationsClick(Sender: TObject);
    procedure sgRelOccClick(Sender: TObject);
    procedure sgRelOccDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sgSpecimensClick(Sender: TObject);
    procedure sgSpecimensDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure bbFindRefClick(Sender: TObject);
    procedure eRelatedTaxonKeyPress(Sender: TObject; var Key: Char);
    procedure eDetDateExit(Sender: TObject);
    procedure eSpecDateExit(Sender: TObject);
    procedure pcTaxonOccurrenceChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure pcTaxonOccurrenceChange(Sender: TObject);
    procedure dbcbCheckedClick(Sender: TObject);
    procedure eWorkDblClick(Sender: TObject);
    procedure dbCheckBoxMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cmbDeterminationTypeChange(Sender: TObject);
    procedure eDeterminerGetData(Sender: TObject);
    procedure eDeterminerFindData(Sender: TObject);
    procedure pnlDetailsResize(Sender: TObject);
    procedure sgPrivateOccClick(Sender: TObject);
    procedure bbPrivateAddClick(Sender: TObject);
    procedure bbPrivateEditClick(Sender: TObject);
    procedure bbPrivateDelClick(Sender: TObject);
    procedure bbPrivateAcceptClick(Sender: TObject);
    procedure bbPrivateDiscardClick(Sender: TObject);
    procedure sgPrivateOccDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure dbcmbPrivateTypeChange(Sender: TObject);

  private
    FdmTaxonOcc       :TdmTaxonOccurrences;
    FdmDetermination  :TdmDetermination;
    FDrillForm        :TBaseForm;
    FSampleKey        :TKeyString;
    FTaxonOccKey      :TKeyString;
    FVerified         :Byte;

    FDeterminationList:TDeterminationList;
    FCurrentDet      :TDeterminationItem;
    FPreferredDet    :TDeterminationItem;
    FDetTaxonKey     :TKeyString;
    FDetWorkKey      :TKeyString;

    FRelOccList   :TRelOccList;
    FCurrentRelOcc:TRelOccItem;
    FRelTaxonKey  :TKeyString;

    FSpecimenList:TSpecimenList;
    FPrivateDataList:TPrivateDataList;

    FCurrentSpec :TSpecimenItem;
    FCurrentPrivateData :TPrivateDataItem;
    FAddItem :boolean;
    FProvenance:string;
    FClosingForm:boolean;

    procedure EnableDetails(const NewMode:Constants.TEditMode);
    procedure SetDrillForm(const Value: TBaseForm);
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
    procedure DropDetTaxonName(const Sender: TObject;
      const iFormat: integer; const iSourceData: TKeyList;
      const iTextStrings: TStringList; var ioHandled: boolean);
    procedure DropDeterminerName(const Sender: TObject;
      const iFormat: integer; const iSourceData: TKeyList;
      const iTextStrings: TStringList; var ioHandled: boolean);
    procedure DropWorkData(const Sender: TObject; const iFormat: integer;
      const iSourceData: TKeyList; const iTextStrings: TStringList;
      var ioHandled: boolean);
    procedure DropGridOcc(const Sender: TObject; const iFormat: integer;
      const iSourceData: TKeyList; const iTextStrings: TStringList;
      var ioHandled: boolean);
    procedure DropDetailOcc(const Sender: TObject; const iFormat: integer;
      const iSourceData: TKeyList; const iTextStrings: TStringList;
      var ioHandled: boolean);
    procedure BlankDetDetails;
    procedure SaveDetDetails;
    procedure BlankRelOccDetails;
    procedure SaveRelOccDetails;
    procedure BlankSpecDetails;
    procedure BlankPrivateDetails;
    procedure SaveSpecDetails;
    procedure SavePrivateDetails;
    function  CheckTaxOcc:boolean;
    function  CheckWork:boolean;
    procedure SetupObjects;
    procedure FreeObjects;
    procedure UpdateReference(KeyList: TKeyList);
    procedure UpdateDeterminer(KeyList: TKeyList);
    procedure UpdateTaxon(KeyList: TKeyList);
    procedure SelectPreferredDetermination;
    procedure SetDeterminationColour(const tfDetailsOn: boolean);
    procedure SetRelOccColour(const tfDetailsOn: boolean);
    procedure SetSpecimensColour(const tfDetailsOn: boolean);
    procedure SetPrivateColour(const tfDetailsOn: boolean);
    procedure SetVerified(value: Byte);
    function CheckValidation: Byte;
    procedure RefreshComboList(ACombo: TDBListCombo);
    procedure UpdateVerificationStatus;
    procedure ValidateDeterminationDate;
    procedure ValidateSpecimenDate;
    property Verified: Byte read FVerified write SetVerified;
    function GetReviewStatusCode :integer;
    function GetReviewStatus (const statuscode : integer) : String;
    function GetReviewStatusSummary(const statuscode : integer) : String;
    function GetTypeDescription : string;
    function GetCompetencyLevel : integer;
  protected
    procedure SetupDestinationControls; override;
  public
    constructor Create(AOwner:TComponent); override;
    function GetKeyList: TKeyList; override;
    procedure UpdateRTFMenu; override;
    procedure AddRecord(const ASampleKey:TKeyString);
    procedure EditRecord;
    procedure DisplayRecord(const AOccurrenceKey: TKeyString); override;
    procedure DeleteRecord(const ATaxonOccKey: TKeyString);
    procedure ChangeChecked(const ANewState: boolean);
    procedure GetInternalSources(keyList: TEditableKeyList);
    procedure RefreshTaxonName;
    procedure RefreshNames;
    procedure RefreshColours;
    procedure ShowMetadata; override;
    procedure FindOnMap; override;
    procedure RefreshLists;
    property DrillForm:TBaseForm read FDrillForm write SetDrillForm;
    property SampleKey:TKeyString read FSampleKey write FSampleKey;
    property TaxonOccKey:TKeyString read FTaxonOccKey write FTaxonOccKey;
end;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, Maintbar, Observations, Find, DropTarget, RichEdit, ActiveX, Map,
  GeneralData, ApplicationSettings, MetadataPopup, ExceptionForm, PlaceCardData;

const
  NOT_VERIFIED    = 0;
  VERIFIED_FAILED = 1;
  VERIFIED_PASSED = 2;


resourcestring
  ResStr_DeterminationRequired =  'At least one Determination is required. Enter at least one determination.';
  ResStr_NotVerified = 'Not verified';
  ResStr_FailedVerification = 'Failed/pending verification';
  ResStr_PassedVerification = 'Passed verification';
  ResStr_DeleteDetermination = 'Are you sure you want to delete this Determination?';
  ResStr_TaxonNameRequired =  'A Taxon Name is required for every Determination.';
  ResStr_DeterminerNameRequired = 'A Determiner Name is required for every Determination.';
  ResStr_DeterminerNameInvalid =  'The Determiner Name is invalid. Enter a valid name.';
  ResStr_DeterminerMustHaveRole = 'Every determiner must have a role specified.';
  ResStr_DeterminerMustHaveType = 'Every determiner must have a type selected.';
  ResStr_DeterminationDateRequired =  'A Determination Date is required for every Determination.';
  ResStr_InvalidWorkDocument = 'The Work Document is invalid. Select a valid Work Document or leave blank.';
  ResStr_InvalidVagueDate = 'The vague date you have entered is not valid';
  ResStr_DeleteRelatedOccurence = 'Are you sure you want to delete this Related Occurrence?';
  ResStr_RecordRequiredForOcc = 'A record is required for every Related Occurrence.';
  ResStr_InvalidTaxonName = 'The Taxon Name is invalid. Enter a valid Taxon Name.';
  ResStr_RelationTypeMissing =  'The Relationship Type is missing. Select a value from the list.';
  ResStr_DeleteSpecimen = 'Are you sure you want to delete this Specimen?';
  ResStr_DeletePrivate = 'Are you sure you want to delete this Private Entry?';
  ResStr_SpecimenNumRequired =  'A Specimen Number is required for every Specimen.';
  ResStr_SpecimenTypeRequired = 'A Specimen Type is required for every Specimen. Select one from the list.';
  ResStr_PrivateTypeRequired = 'A Type is required for every Private entry. Select one from the list.';
  ResStr_PrivateItemRequired = 'An Item Name is required for every Private entry.';
  ResStr_PrivateValue = 'Must be a number with no punctuation.';
  ResStr_TheItemDate = 'The item date';
  ResStr_Not_Reviewed = 'Not Reviewed';
  ResStr_Reviewed = 'Review Complete';
  ResStr_Review_Unactioned = 'Review Unactioned';
  ResStr_Not_Reviewed_Summary = 'Not Reviewed';
  ResStr_Reviewed_Summary = 'Reviewed';
  ResStr_Review_Unactioned_Summary = 'Review Unactioned';

//==============================================================================
constructor TfrmTaxonOccurrences.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  SetupObjects;
  SetGridColumnTitles(sgDeterminations, ['', ResStr_Taxon, ResStr_Determiner, ResStr_Role, ResStr_Type, ResStr_Date]);
  SetGridColumnTitles(sgRelOcc,         [ResStr_RelatedOccurrence, ResStr_Type]);
  SetGridColumnTitles(sgSpecimens,      [ResStr_SpecimenNumber, ResStr_SpecimenType]);
  SetGridColumnTitles(sgPrivateOcc,      [ResStr_PrivateType, ResStr_PrivateItem,ResStr_PrivateDetail]);
  SwitchToDetails(sgDeterminations,bbDetAdd,bbDetEdit,bbDetDel,
                  bbSave, bbCancel, gbDetDetails,false);
  SwitchToDetails(sgRelOcc, bbRelOccAdd, bbRelOccEdit, bbRelOccDel,
                  bbSave, bbCancel, gbRelOccDetails, false);
  SwitchToDetails(sgSpecimens,bbSpecimenAdd,bbSpecimenEdit,bbSpecimenDel,
                  bbSave, bbCancel, gbSpecimenDetails,false);
  SwitchToDetails(sgPrivateOcc,bbPrivateAdd,bbPrivateEdit,bbPrivateDel,
                  bbSave, bbCancel, gbPrivateDetails,false);
  EnableDetails(Constants.emView);
  pcTaxonOccurrence.ActivePage:=tsGeneral;

  //Help Setup
  pcTaxonOccurrence.HelpContext    := IDH_TAXONGENERAL;
  tsGeneral.HelpContext            := IDH_TAXONGENERAL;
  tsDeterminations.HelpContext     := IDH_TAXONDETS;
  tsMeasurements.HelpContext       := IDH_TAXONMEASURES;
  tsRelatedOccurrences.HelpContext := IDH_TAXONRELATEDOCCS;
  tsSpecimens.HelpContext          := IDH_TAXONSPECIMENS;
  tsSources.HelpContext            := IDH_TAXONSOURCES;
  SetReadOnlyFieldsColourState(true, [mmCount]);
end;  // Create

//==============================================================================
procedure TfrmTaxonOccurrences.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FreeObjects;
  Action := caFree;
end;  // FormClose

//==============================================================================
procedure TfrmTaxonOccurrences.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  CanClose:=false;
  if EditMode<>emView then begin
    Beep;
    case ConfirmSaveAndClose of
      mrYes : begin
                if bbDetAccept.Enabled then bbDetAcceptClick(nil) else
                if bbRelOccAccept.Enabled then bbRelOccAcceptClick(nil) else
                if bbSpecimenAccept.Enabled then bbSpecimenAcceptClick(nil) else
                if bbPrivateAccept.Enabled then bbPrivateAcceptClick(nil);
                bbSaveClick(nil);
                CanClose:=true;
              end;
      mrNo  : begin
                FClosingForm:=true;
                if bbDetdiscard.Enabled then bbDetDiscardClick(nil) else
                if bbRelOccDiscard.Enabled then bbRelOccDiscardClick(nil) else
                if bbSpecimenDiscard.Enabled then bbSpecimenDiscardClick(nil) else
                if bbPrivateDiscard.Enabled then bbPrivateDiscardClick(nil);
                bbCancelClick(nil);
                CanClose:=true;
                FClosingForm:=false;
              end;
    end;
  end else
    CanClose:=true;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmTaxonOccurrences.SetupObjects;
begin
  // Data Module
  FdmTaxonOcc:=TdmTaxonOccurrences.Create(nil);
  FdmTaxonOcc.qryTaxonOcc.Parameters.ParamByName('KeyParameter').Value:='';
  FdmDetermination:=TdmDetermination.Create(nil);
  FdmDetermination.qryTaxonDet.Parameters.ParamByName('KeyParameter').Value:='';
  // Setup Determination
  FDeterminationList:=TDeterminationList.Create(FdmDetermination.qryTaxonDet,'Taxon_Determination_Key',
                                                sgDeterminations,TDeterminationItem);
  FDeterminationList.Taxon:=true;
  // Setup Measurement properties
  Measurements.Init(dmDatabase.LocalDatabase, 'Taxon_Occurrence_Data',
                    'Taxon_Occurrence_Data_Key', 'Taxon_Occurrence_key',
                    AppSettings.UserID, AppSettings.UserAccessLevel,
                    dmGeneralData.GetNextKey,
                    AppSettings.SiteID, AppSettings.RestrictFullEdit);
  // Setup Related occurrences
  FRelOccList:=TRelOccList.Create(FdmTaxonOcc.qryRelOcc,'Taxon_Occurrence_Relation_Key',
                                  sgRelOcc,TRelOccItem,FdmTaxonOcc);
  // Specimens
  FSpecimenList:=TSpecimenList.Create(FdmTaxonOcc.qrySpecimen,'Specimen_Key',sgSpecimens,TSpecimenItem);

  //Private
  FPrivateDataList:=TPrivateDataList.Create(FdmTaxonOcc.qryTaxonPrivateDetail,'Taxon_Private_Data_Key',sgPrivateOcc,TPrivateDataItem);

  // Sources
  Sources.Init(dmDatabase.LocalDatabase, 'Taxon_Occurrence_Sources',
               AppSettings.UserAccessLevel, dmGeneralData.GetNextKey,
               RegisterDropComponent, AppSettings.SiteID, AppSettings.ExternalFilePath);
  Sources.OnFindInternal:=dmGeneralData.SourcesFindInternal;
  Sources.OnAddInternal :=dmGeneralData.SourcesAddInternal;
  Sources.OnShowSource :=dmGeneralData.SourcesShowInternal;
end;  // SetupObjects

//==============================================================================
procedure TfrmTaxonOccurrences.FreeObjects;
begin
  FDeterminationList.Free;
  FDeterminationList := nil;
  FRelOccList.Free;
  FRelOccList := nil;
  FdmDetermination.Free;
  FdmDetermination:=nil;
  FdmTaxonOcc.Free;
  FdmTaxonOcc:=nil;
end;  // FreeObjects

//==============================================================================
procedure TfrmTaxonOccurrences.DisplayRecord(const AOccurrenceKey: TKeyString);
var iCount :integer;
    lCursor:TCursor;
 ScrollMessage:TWMVScroll;
 lDetTypeKey: string;
begin
  lCursor:=HourglassCursor;
  TaxonOccKey :=AOccurrenceKey;
  try
    with FdmTaxonOcc do
      with qryTaxonOcc do begin
        Close;
        ResetDBRichEditControls([dbreComments]);
        Parameters.ParamByName('KeyParameter').Value:=AOccurrenceKey;
        Open;
        FCustodian := FieldByName('Custodian').AsString;
        SampleKey  := FieldByName('Sample_Key').AsString;
        //New List boxes on General Tab.
        dbcmbRecordType.KeyValue := FieldByName('Record_type_key').AsString;
        dbcmbSubstrate.KeyValue  := FieldByName('Substrate_key').AsString;
        FProvenance              := FieldByName('Provenance').AsString;
        cmbProvenance.Text       := FProvenance;
        Verified                 := FieldByName('Verified').AsInteger;
        FCheckedState            := dbcbChecked.Checked;
      end;
    lblTaxonName.Caption:=DuplicateCharacters(FdmTaxonOcc.GetNameFromOcc(AOccurrenceKey), Char('&'));
     // Determination
    dbcmbRole.Active:=true;
    dbcmbType.Active:=true;
    FdmDetermination.qryTaxonDet.Parameters.ParamByName('KeyParameter').Value:=AOccurrenceKey;
    FDeterminationList.OccKey:=AOccurrenceKey;
    FDeterminationList.Refresh;
    sgDeterminationsClick(nil);

    if AOccurrenceKey <> '' then begin
      if pcTaxonOccurrence.ActivePage = tsDeterminations then
        lDetTypeKey := FCurrentDet.DetTypeKey
      else
        lDetTypeKey := dmGeneralData.GetPreferredTaxonDetType(AOccurrenceKey);
    end;
    FdmDetermination.qryDetType.Parameters.ParamByName('KeyParameter').Value := lDetTypeKey;
    RefreshComboList(cmbDeterminationType);
    cmbDeterminationType.KeyValue := lDetTypeKey;
    // Measurements
    Measurements.KeyValue:=AOccurrenceKey;
    Measurements.Refresh;
    // Count list
    mmCount.Lines.Clear;
    with Measurements.Grid do
      for iCount:=1 to RowCount-1 do
        if (CompareText(Cells[0,iCount],'Abundance')=0) and
           (CompareText(Cells[3,iCount],'Count')=0) then
          if Cells[4,iCount]='' then
            mmCount.Lines.Add(Cells[2,iCount]+', '+Cells[1,iCount])
          else
            mmCount.Lines.Add(Cells[2,iCount]+', '+Cells[1,iCount]+', ('+Cells[4,iCount]+')');
  //  scroll to first line (nabbed from internet)
     ScrollMessage.Msg:=WM_VScroll;
     for iCount := mmCount.Lines.Count DownTo 0 do
     begin
        ScrollMessage.ScrollCode:=sb_LineUp;
        ScrollMessage.Pos:=0;
        mmCount.Dispatch(ScrollMessage);
     end;    // Related Occurrences
    dbcmbRelType.Active:=true;
    FdmTaxonOcc.qryRelOcc.Parameters.ParamByName('KeyParameter').Value:=AOccurrenceKey;
    FRelOccList.OccKey:=AOccurrenceKey;
    FRelOccList.Refresh;
    sgRelOccClick(nil);
    // Specimen
    dbcmbSpecType.Active:=true;
    FdmTaxonOcc.qrySpecimen.Parameters.ParamByName('KeyParameter').Value:=AOccurrenceKey;
    FSpecimenList.OccKey:=AOccurrenceKey;
    FSpecimenList.Refresh;
    sgSpecimensClick(nil);
     // Private
    dbcmbPrivateType.Active:=true;
    FdmTaxonOcc.qryTaxonPrivateDetail.Parameters.ParamByName('KeyParameter').Value:=AOccurrenceKey;
    FPrivateDataList.OccKey:=AOccurrenceKey;
    FPrivateDataList.Refresh;
    sgPrivateOccClick(nil);
    // Sources
    Sources.SourcesFromKey:=AOccurrenceKey;
    Sources.RefreshLists;
    // Additional Pages
    ChangeAdditionalPage(pcTaxonOccurrence);
    // Notify COM Addins
    NotifyDataItemChange;
    //Have to set active last or appear on screen with text in
    dbcmbRecordType.Active := True;
    dbcmbSubstrate.Active := True;
    cmbDeterminationType.Active := True;
  finally
    DefaultCursor(lCursor);
  end;
end;  // DisplayRecord

//==============================================================================
procedure TfrmTaxonOccurrences.ChangeChecked(const ANewState: boolean);
begin
  DoCheckedStateChange('Taxon', TaxonOccKey, ANewState);
end;  // ChangeChecked

//==============================================================================
procedure TfrmTaxonOccurrences.AddRecord(const ASampleKey:TKeyString);
begin
  inherited;
  // Next Key
  SampleKey     := ASampleKey;
  eDetDate.Text := VagueDateToString(dmGeneralData.GetVagueDateFromRecordset(
      dmDatabase.GetRecordset('usp_SampleDetails_Get', ['@SampleKey', ASampleKey])));
  TaxonOccKey   := dmGeneralData.GetNextKey('Taxon_Occurrence','Taxon_Occurrence_Key');
  FCustodian    := AppSettings.SiteID;
  Verified      := NOT_VERIFIED;  // not validated by default
  // Determinations
  FdmDetermination.qryDetType.Parameters.ParamByName('KeyParameter').Value:='';
  FdmDetermination.qryDetRole.Parameters.ParamByName('KeyParameter').Value:='';
  FDeterminationList.OccKey:=TaxonOccKey;
  FDeterminationList.Refresh;
  sgDeterminationsClick(nil);
  // Measurements
  Measurements.KeyValue:=TaxonOccKey;
  Measurements.Refresh;
  // Related Occurrences
  FdmTaxonOcc.qryRelOcc.Parameters.ParamByName('KeyParameter').Value:='';
  FRelOccList.OccKey:=TaxonOccKey;
  FRelOccList.Refresh;
  // Specimens
  FdmTaxonOcc.qrySpecimen.Parameters.ParamByName('KeyParameter').Value:='';
  FSpecimenList.OccKey:=TaxonOccKey;
  FSpecimenList.Refresh;
  // Private
  FdmTaxonOcc.qryTaxonPrivateDetail.Parameters.ParamByName('KeyParameter').Value:='';
  FPrivateDataList.OccKey:=TaxonOccKey;
  FPrivateDataList.Refresh;
 
  with FdmTaxonOcc.qryTaxonOcc do begin
    Append;
    // Set combo boxes
    dbcmbSubstrate.KeyValue:=NONE_RECORD_KEY;
    FieldByName('Substrate_Key').AsString:=NONE_RECORD_KEY;
    dbcmbRecordType.KeyValue:=NONE_RECORD_KEY;
    FieldByName('Record_Type_Key').AsString:=NONE_RECORD_KEY;
    cmbProvenance.Text:=NONE_RECORD;
    FProvenance       :=NONE_RECORD;
    dbcbConfidential.Checked := False;
    dbcbChecked.Checked      := False;
  end;
   // Private
  FdmTaxonOcc.qryTaxonPrivateDetail.Parameters.ParamByName('KeyParameter').Value:='';
  FPrivateDataList.OccKey:=TaxonOccKey;
  FPrivateDataList.Refresh;
  // Comment
  ResetDBRichEditControls([dbreComments]);
  dmGeneralData.SetNameIDAndDate(FdmTaxonOcc.qryTaxonOcc,'Entered_By','Entry_Date');
  // Sources
  Sources.SourcesFromKey:=TaxonOccKey;
  Sources.RefreshLists;
  { Add COM addin page }
  AddAdditionalPages;
  EnableDetails(Constants.emAdd);
  pcTaxonOccurrence.ActivePage:=tsDeterminations;
  bbDetAddClick(nil);
  eTaxon.SetFocus;
end;  // AddRecord

//==============================================================================
procedure TfrmTaxonOccurrences.DeleteRecord(const ATaxonOccKey: TKeyString);
begin
  FdmTaxonOcc.DeleteRecord(TaxonOccKey);
end;  // DeleteRecord

//==============================================================================
procedure TfrmTaxonOccurrences.EditRecord;
var
  lDet: TDeterminationItem;
begin
  DisplayRecord(TaxonOccKey);
  if dmGeneralData.HasFullEditAccess(TN_TAXON_OCCURRENCE, 'Taxon_Occurrence_Key', TaxonOccKey) and
      HaveCustody then
    try
      with FdmTaxonOcc.qryTaxonOcc do begin
        SampleKey  :=FieldByName('Sample_Key').AsString;
        //TaxonOccKey:=FieldByName('Taxon_Occurrence_Key').AsString;
        //I would be interested to hear the justification for that line ever having been there.
        //I'm not totally convinced by the one above it either.
        Edit;
      end;
      dmGeneralData.SetNameIDAndDate(FdmTaxonOcc.qryTaxonOcc,'Changed_By','Changed_Date');
      EnableDetails(Constants.emEdit);
    except
      on E:Exception do
        if dmDatabase.CheckError(E, dbeRecordLocked) then begin
          MessageDlg(ResStr_CannotEditRecord + #13#13 +
                     dmDatabase.GetErrorMessage(E.Message, dbeRecordLocked),
                     mtInformation, [mbOk], 0);
          TfrmObservations(DrillForm).tvObservations.SetFocus;
        end else
          Raise;
    end  // Try...Except
  else
    EnableDetails(Constants.emEdit);

  SelectPreferredDetermination;
  if Assigned(FPreferredDet) and (pcTaxonOccurrence.ActivePage = tsGeneral) then
    lDet := FPreferredDet
  else
    lDet := FCurrentDet;

  with FdmDetermination do begin
    qryDetType.Parameters.ParamByName('KeyParameter').Value := lDet.DetTypeKey;
    qryDetRole.Parameters.ParamByName('KeyParameter').Value := lDet.RoleKey;
    RefreshComboList(cmbDeterminationType);
    RefreshComboList(dbcmbRole);
    RefreshComboList(dbcmbType);
  end;
end;  // EditRecord

//==============================================================================
procedure TfrmTaxonOccurrences.bbSaveClick(Sender: TObject);
var iCount     : integer;
    lDataItem  : TDeterminationItem;
    lCurrentTab: TTabSheet;
    lCursor    : TCursor;
    lCustodian : String;
    ZeroAbundance : Boolean;
begin
  inherited;
  lCurrentTab:=pcTaxonOccurrence.ActivePage;
  // General
  pcTaxonOccurrence.ActivePage:=tsGeneral;
  // Determinations
  pcTaxonOccurrence.ActivePage:=tsDeterminations;
  if bbDetAccept.Enabled then
    ValidateValue(FDeterminationList.ItemCount>0,ResStr_DeterminationRequired,eTaxon)
  else
    ValidateValue(FDeterminationList.ItemCount>0,ResStr_DeterminationRequired,bbDetAdd);

  // Measurements
  pcTaxonOccurrence.ActivePage:=tsMeasurements;
  ValidateValue(Measurements.CheckGrid, ResStr_MeasurementMissingOrInvalid, Measurements.Grid);
  { Call to validate COM addin page... }
  if not CheckAdditionalPageCanSave then Exit;

  pcTaxonOccurrence.ActivePage:=lCurrentTab;
  lCursor:=HourglassCursor;
  try
    lCustodian := dmGeneralData.Custodian(TN_TAXON_OCCURRENCE, 'Taxon_Occurrence_Key', TaxonOccKey);
    if ((AppSettings.UserAccessLevel > ualAddOnly) and (lCustodian = AppSettings.SiteID)
       and (FdmTaxonOcc.qryTaxonOcc.State=dsEdit))
       or (FdmTaxonOcc.qryTaxonOcc.State=dsInsert) then
    begin
      with FdmTaxonOcc.qryTaxonOcc do begin
        //Mantis 496
        FieldByName('Provenance').AsString:=cmbProvenance.Text;

        if dbcmbRecordType.Text <> '' then
          FieldByName('RECORD_TYPE_KEY').AsString := dbcmbRecordType.KeyValue;
        if dbcmbSubstrate.Text <> '' then
          FieldByName('SUBSTRATE_KEY').AsString := dbcmbSubstrate.KeyValue;
        if State=dsInsert then begin
          FieldByName('Taxon_Occurrence_Key').AsString:=TaxonOccKey;
          FieldByName('Sample_Key').AsString          :=SampleKey;
        end;
        Verified := CheckValidation;
        FieldByName('Verified').AsInteger := Verified;
        Post;
      end  // with
    end else
    if (AppSettings.UserAccessLevel > ualAddOnly) or (EditMode = emAdd) then
      if (EditMode = Constants.emEdit) and (lCustodian <> FCustodian) then begin
        FdmTaxonOcc.qryTaxonOcc.Cancel;
        MessageDlg(Format(ResStr_CustodyChanged, ['Taxon Occurrence']), mtWarning, [mbOk], 0);
      end;
      // editing someone else's occurrence - must still check validation
      with FdmTaxonOcc.qryTaxonOcc do begin
        Edit;  // won't be in edit mode in this case
        Verified := CheckValidation;
        FieldByName('Verified').AsInteger := Verified;
        Post;
      end; // with

    // Determinations
    FDeterminationList.OccKey:=TaxonOccKey;
    FDeterminationList.Update;
    sgDeterminationsClick(nil);
    // Measurements
    Measurements.KeyValue:=TaxonOccKey;
    Measurements.UpdateList;
    // Check for zero abundance
    ZeroAbundance := dmGeneralData.CheckZeroAbundance(TaxonOccKey);
    FdmTaxonOcc.qryTaxonOcc.Last;
    FdmTaxonOcc.qryTaxonOcc.Edit;
    FdmTaxonOcc.qryTaxonOcc.FieldByName('ZERO_ABUNDANCE').AsBoolean := ZeroAbundance;
    FdmTaxonOcc.qryTaxonOcc.Post;
    // Related Occurrences
    FRelOccList.OccKey:=TaxonOccKey;
    FRelOccList.Update;
    sgRelOccClick(nil);
    // Specimens
    FSpecimenList.OccKey:=TaxonOccKey;
    FSpecimenList.Update;
    sgSpecimensClick(nil);
     // Private
    FPrivateDataList.OccKey:=TaxonOccKey;
    FPrivateDataList.Update;
    sgPrivateOccClick(nil);
    // Sources
    Sources.Post;
    // Additional Pages
    ChangeAdditionalPage(pcTaxonOccurrence);  // Required after add new occurrence
    SaveAdditionalPages;

    // Update Taxon Occurrence node in Observation tree
    if DrillForm<>nil then
      for iCount:=0 to FDeterminationList.Count-1 do begin
        lDataItem:=TDeterminationItem(FDeterminationList.Items[iCount]);
        if not lDataItem.Deleted and lDataItem.Preferred then begin
          with TfrmObservations(DrillForm) do begin
            // Update node with new information
            if FCheckedState<>dbcbChecked.Checked then begin
              ChangeChecked(dbcbChecked.Checked);
              FCheckedState:=dbcbChecked.Checked;
              if FCheckedState then TTaxonOccNode(SelectedItem.Data).StateImage:=STATE_CHECKED
                  else TTaxonOccNode(SelectedItem.Data).StateImage:=STATE_UNCHECKED;
            end;
            // Images 0to5 are the existing images used where UseOriginalIcons is true
            // New icons start at 6 and there are three blocks one for each of the verified indicators (0,1,2)
            // Each block has 4 possible icon. Verified X 4 therefore gives start of each block of 4
            // ord(dbcbConfidential.Checked) (values 0 or 1) identifies the confidential element in each block
            // (2 *ord(ZeroAbundance) (2 x values 0 or 1) idenfifies the zero abundance element within the block
            // For each verified indicator we have 4 possible icons eg.
            // Verified Indicator of 0 not confidential or zero abundance = 6 +(0 *4) + 0 + (2 * 0) = 6
            // Verified Indicator of 0  confidential but not zero abundance = 6 +(0 *4) + 1 + (2 * 0) = 7
            // Verified Indicator of 0  not confidential but zero abundance = 6 +(0 *4) + 0 + (2 * 1) = 8
            // Verified Indicator of 0  confidential and zero abundance = 6 +(0 *4) + 1 + (2 * 1) = 9

            if Not AppSettings.UseOriginalIcons then
            TTaxonOccNode(SelectedItem.Data).ImageIndex :=  6 + (verified * 4) + ord(dbcbConfidential.Checked) + (2 *ord(ZeroAbundance));

            TTaxonOccNode(SelectedItem.Data).Confidential := dbcbConfidential.Checked;
            SetItemTextAndKey(lDataItem.ItemName,TaxonOccKey);
            tvObservations.Selected:=SelectedItem;
          end;
          Break;
        end;
      end;
    EnableDetails(Constants.emView);
    DisplayRecord(TaxonOccKey);
  finally
    DefaultCursor(lCursor);
  end;
end;  // bbSaveClick

//==============================================================================
procedure TfrmTaxonOccurrences.bbCancelClick(Sender: TObject);
var tfDiscardNew:boolean;
begin
  inherited;
  tfDiscardNew:=FdmTaxonOcc.qryTaxonOcc.State=dsInsert;
  FdmTaxonOcc.qryTaxonOcc.Cancel;
  // Additional Pages
  CancelAdditionalPages;
  EnableDetails(Constants.emView);

  // If closing form, no need to do anything further. Otherwise, refresh screens.
  if not FClosingForm then
    if tfDiscardNew and (DrillForm<>nil) then
      PostMessage(TfrmObservations(DrillForm).Handle,WM_Discard_Observation,0,0)
    else begin
      with TfrmObservations(DrillForm) do
        tvObservations.Selected:=SelectedItem;
      DisplayRecord(TaxonOccKey);
    end;
end;  // bbCancelClick

//==============================================================================
procedure TfrmTaxonOccurrences.EnableDetails(const NewMode:Constants.TEditMode);
var
  tfOn:boolean;
begin
  // Set FEditMode here as it is needed in some Click events triggered from here
  FEditMode:=NewMode;
  tfOn:=EditMode<>emView;

  // On General page
  dbcmbRecordType.ReadOnly := not (FdmTaxonOcc.qryTaxonOcc.State in [dsEdit,dsInsert]);
  cmbProvenance.Enabled    := tfOn and (FdmTaxonOcc.qryTaxonOcc.State in [dsEdit,dsInsert]);
  dbcmbSubstrate.ReadOnly  := dbcmbRecordType.ReadOnly;
  cmbDeterminationType.ReadOnly := NewMode <> emAdd; // Always enabled for new occurrences.

  // On Determination page
  bbDetAdd.Enabled := tfOn;
  sgDeterminationsClick(nil);
  // Rethink readonly state depending on preferred det. (Still ok for Add mode though).
  SelectPreferredDetermination;
  if Assigned(FPreferredDet) then
    cmbDeterminationType.ReadOnly := (NewMode = emView);

  // On Measurements page
  Measurements.EditMode:=NewMode;
  // On Related Occurrences page
  bbRelOccAdd.Enabled :=tfOn;
  sgRelOccClick(nil);
   // On Specimens page
  bbSpecimenAdd.Enabled :=tfOn;
  sgSpecimensClick(nil);
   // On Ext Ref Page
  bbPrivateAdd.Enabled :=tfOn;
  sgPrivateOccClick(nil);
  // On Sources page
  Sources.EditMode:=NewMode;

  bbSave.Enabled  :=tfOn;
  bbCancel.Enabled:=tfOn;

  // Set RTF popup menu
  if tfOn then dbreComments.PopupMenu:=dmFormActions.pmRTF
          else dbreComments.PopupMenu:=nil;

  // Additional Pages
  SetAdditionalPagesState(tfOn);

  // Upate Main menu
  if DrillForm<>nil then TfrmObservations(DrillForm).SetMenuState(not tfOn);
end;  // EnableDetails


//==============================================================================
procedure TfrmTaxonOccurrences.bbRelatedTaxonFindClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actTaxonDiction.Execute;
end;  // bbRelatedTaxonFindClick

//==============================================================================
procedure TfrmTaxonOccurrences.SetVerified(value: Byte);
begin
  FVerified := value;
  UpdateVerificationStatus;
end;

//==============================================================================
procedure TfrmTaxonOccurrences.UpdateVerificationStatus;

var
  lReviewStatusCode :integer;
begin
  lReviewStatusCode := getReviewStatusCode;
  case FVerified  of
    NOT_VERIFIED:    lblVerificationStatus.Caption := ResStr_NotVerified + getReviewStatus(lReviewStatusCode) ;
    VERIFIED_FAILED: lblVerificationStatus.Caption := ResStr_FailedVerification + getReviewStatus(lReviewStatusCode);
    VERIFIED_PASSED: lblVerificationStatus.Caption := ResStr_PassedVerification + getReviewStatus(lReviewStatusCode);
  end;
  lblReviewSummary.Caption :=  getReviewStatusSummary(lReviewStatusCode);

end;  // UpdateVerificationStatus
  
//==============================================================================
function TfrmTaxonOccurrences.CheckValidation: Byte;
begin
  Result := NOT_VERIFIED;
  SelectPreferredDetermination;
  if Assigned(FPreferredDet) then
    Result := FPreferredDet.Verified;
end;  // CheckValidation

//==============================================================================
procedure TfrmTaxonOccurrences.sgDeterminationsDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var xPos,yPos:integer;
    lDataItem:TDeterminationItem;
{    lCNLang: string;}
begin
  inherited;
  with sgDeterminations do begin
    if (ACol=0) and (ARow>0) then begin
      Canvas.FillRect(Rect);
      xPos:=Rect.Left+(ColWidths[0]-13) div 2;
      yPos:=Rect.Top+(RowHeights[Row]-13) div 2;
      lDataItem:=TDeterminationItem(Rows[ARow].Objects[0]);
      if lDataItem=nil then
        DrawCheckBox(Canvas,xPos,yPos,false)
      else
        DrawCheckBox(Canvas,xPos,yPos,lDataItem.Preferred);
    end else begin
      Canvas.FillRect(Rect);
      DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
    end;
  end;
end;  // sgDeterminationDrawCell

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.sgDeterminationsClick(Sender: TObject);
var
  lFullEditAccess: boolean;
begin
  inherited;
  with sgDeterminations do
    FCurrentDet:=TDeterminationItem(Objects[0,Row]);
  if FCurrentDet=nil then begin
    // Nothing in the grid, prevent editing/deleting regardless of access privileges
    bbDetEdit.Enabled:=false;
    bbDetDel.Enabled :=false;
    BlankDetDetails;
  end else begin
    with FCurrentDet do begin
      FdmDetermination.qryDetType.Parameters.ParamByName('KeyParameter').Value := DetTypeKey;
      FdmDetermination.qryDetRole.Parameters.ParamByName('KeyParameter').Value := RoleKey;
      RefreshComboList(dbcmbRole);
      RefreshComboList(dbcmbType);
      eTaxon.Text := ItemName;
      FDetTaxonKey       :=ItemNameKey;
      eDeterminer.Text   :=Determiner;
      eDeterminer.Key    :=DeterminerKey;
      cbPreferred.Checked:=Preferred;
      dbcmbRole.KeyValue :=RoleKey;
      dbcmbType.KeyValue :=DetTypeKey;
      eDetDate.Text      :=Date;
      eWork.Text         :=Work;
      FDetWorkKey        :=WorkKey;
      // Get back to the beginning of the stream before reading it.
      Comment.Position:=0;
      reDetComments.Lines.LoadFromStream(Comment);

      // ItemKey is empty for newly added items. Save to actually get a value
      lFullEditAccess := dmGeneralData.HasFullEditAccess(TN_TAXON_DETERMINATION,
            'Taxon_Determination_Key', FCurrentDet.ItemKey);
      bbDetEdit.Enabled := AppSettings.AllowEdit(EditMode) and
        (Added or ((Custodian = AppSettings.SiteID)
        and lFullEditAccess));
      bbDetDel.Enabled  := AppSettings.AllowEdit(EditMode) and lFullEditAccess
        and (Added or (Custodian = AppSettings.SiteID));
    end;
  end;
end;  // sgDeterminationsClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.BlankDetDetails;
var
  i: Integer;
  lDataItem: TDeterminationItem;
begin
  FdmDetermination.qryDetType.Parameters.ParamByName('KeyParameter').Value:='';
  FdmDetermination.qryDetRole.Parameters.ParamByName('KeyParameter').Value:='';
  RefreshComboList(dbcmbRole);
  RefreshComboList(dbcmbType);
  FDetTaxonKey       :='';
  eTaxon.Text        :='';
  eDeterminer.Text   :='';  // reset to blank for all first determination
  eDeterminer.Key    :='';
  // will be updated if multiple sample recorders
  for i:=0 to FDeterminationList.Count-1 do begin
    lDataItem:=TDeterminationItem(FDeterminationList.Items[i]);
    if not lDataItem.Deleted and lDataItem.Preferred then begin
      FDetTaxonKey   := lDataItem.ItemNameKey;
      eTaxon.Text    := lDataItem.ItemName;
      //default to the person entering the determination
      eDeterminer.Text := dmGeneralData.GetName(AppSettings.UserID);
      eDeterminer.Key  := AppSettings.UserID;
    end;
  end;
  FDetWorkKey        :='';
  cbPreferred.Checked:=True;
  dbcmbRole.KeyValue  := ORIGINAL_DETERMINER_ROLE_KEY;
  dbcmbType.KeyValue  := ORIGINAL_DETERMINATION_TYPE_KEY;
  eWork.Text         :='';
  reDetComments.Clear;
end;  // BlankDeterminationDetails

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.SaveDetDetails;
var iCount:integer;
begin
  with FCurrentDet do begin
    ItemNameKey  :=FDetTaxonKey;
    ItemName     :=eTaxon.Text;
    DeterminerKey:=eDeterminer.Key;
    Determiner   :=eDeterminer.Text;
    Work         :=eWork.Text;
    if Work='' then WorkKey:=''
               else WorkKey:=FDetWorkKey;
    Date         :=eDetDate.Text;
    RoleKey      :=dbcmbRole.KeyValue;
    Role         :=dbcmbRole.Text;
    DetTypeKey   :=dbcmbType.KeyValue;
    DetType      :=dbcmbType.Text;
    Verified     :=dmGeneralData.GetDetTypeVerified(DetTypeKey);
    Custodian    :=AppSettings.SiteID;
    Comment.Position:=0;
    reDetComments.Lines.SaveToStream(Comment);

    // If new preferred, uncheck the others
    if cbPreferred.Checked then begin
      cmbDeterminationType.KeyValue := DetTypeKey;
      for iCount:=0 to FDeterminationList.Count-1 do
        TDeterminationItem(FDeterminationList.Items[iCount]).Preferred:=false;
    end;
    // Set Preferred if checkbox checked or if only one item in grid
    Preferred:=cbPreferred.Checked or
               ((FDeterminationList.ItemCount=1) and not FAddItem) or
               ((FDeterminationList.ItemCount=0) and FAddItem);
  end;
  sgDeterminations.Refresh;
end;  // SaveDetDetails

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.SelectPreferredDetermination;
var
  i: Integer;
  item: TDeterminationItem;
begin
  FPreferredDet := nil;
  for i := sgDeterminations.FixedRows to sgDeterminations.RowCount - 1 do begin
    item := TDeterminationItem(sgDeterminations.Objects[0, i]);
    if (item <> nil) and item.Preferred then begin
      FPreferredDet := item;
      Exit;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.SetDeterminationColour(const tfDetailsOn:boolean);
begin
  SetRequiredFieldsColourState(tfDetailsOn,[eTaxon,eDeterminer,dbcmbRole,dbcmbType,eDetDate]);
end;  // SetDeterminationColour

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbDetAddClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgDeterminations, bbDetAdd, bbDetEdit, bbDetDel,
                  bbSave, bbCancel, gbDetDetails, true);
  SetDeterminationColour(true);
  FCurrentDet:=TDeterminationItem.CreateNew(FDeterminationList);
  BlankDetDetails;
  FAddItem:=true;
end;  // bbDetAddClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbDetEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgDeterminations, bbDetAdd, bbDetEdit, bbDetDel,
                  bbSave, bbCancel, gbDetDetails, true);
  SetDeterminationColour(true);
  cbPreferred.Enabled:=not cbPreferred.Checked;
  FAddItem:=false;
end;  // bbDetEditClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbDetDelClick(Sender: TObject);
var lDataItem:TDeterminationItem;
    tfPref   :boolean;
begin
  inherited;
  if MessageDlg(ResStr_DeleteDetermination,
                mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    with sgDeterminations do begin
      lDataItem:=TDeterminationItem(Objects[0,Row]);
      if lDataItem<>nil then begin
        tfPref:=lDataItem.Preferred;
        FDeterminationList.DeleteItem(Row);
        // Refresh grid
        sgDeterminationsClick(nil);
        if tfPref and (FDeterminationList.ItemCount>0) then begin
          // Set Preferred to first item in grid
          TDeterminationItem(Objects[0,1]).Preferred:=true;
        end;
      end;
    end;
    // Refresh grid
    sgDeterminationsClick(nil);
  end;
end;  // bbDetDelClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbDetAcceptClick(Sender: TObject);
begin
  inherited;
  // Sender is nil if called from CloseQuery method.
  if Sender=nil then ValidateDeterminationDate;

  ValidateValue(eTaxon.Text<>'',ResStr_TaxonNameRequired,eTaxon);
  ValidateValue(dmGeneralData.CheckTaxon(eTaxon, FDetTaxonKey),ResStr_InvalidTaxonName,eTaxon);
  ValidateValue(eDeterminer.Text<>'',ResStr_DeterminerNameRequired,eDeterminer);
  ValidateValue(dmGeneralData.CheckIndividual(eDeterminer),ResStr_DeterminerNameInvalid,eDeterminer);
  ValidateValue(dbcmbRole.Text <> '',ResStr_DeterminerMustHaveRole,dbcmbRole);
  ValidateValue(dbcmbType.Text <> '',ResStr_DeterminerMustHaveType,dbcmbType);
  ValidateValue(eDetDate.Text<>'',ResStr_DeterminationDateRequired,eDetDate);
  ValidateValue(dmValidation.CheckDeterminationDateAgainstSample(SampleKey, eDetDate.VagueDate),
                ResStr_DetDateAgainstSample, eDetDate);
  ValidateValue((eWork.Text='') or CheckWork,ResStr_InvalidWorkDocument,eWork);

  SaveDetDetails;
  if FAddItem then FDeterminationList.AddNew(FCurrentDet);
  SwitchToDetails(sgDeterminations,bbDetAdd,bbDetEdit,bbDetDel,
                  bbSave, bbCancel, gbDetDetails,false);
  SetDeterminationColour(false);
  sgDeterminationsClick(nil);
  // Refresh det type if needed
  SelectPreferredDetermination;
  if Assigned(FPreferredDet) then begin
    cmbDeterminationType.KeyValue := FPreferredDet.DetTypeKey;
    Verified := FPreferredDet.Verified;
  end;
end;  // bbDetAcceptClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbDetDiscardClick(Sender: TObject);
begin
  inherited;
  if FAddItem then FCurrentDet.Free;
  SwitchToDetails(sgDeterminations,bbDetAdd,bbDetEdit,bbDetDel,
                  bbSave, bbCancel, gbDetDetails,false);
  SetDeterminationColour(false);
  sgDeterminationsClick(nil);
end;  // bbDetDiscardClick

//------------------------------------------------------------------------------
function TfrmTaxonOccurrences.CheckTaxOcc:boolean;
var stTaxOccKey : TKeyString;
    stTaxOcc    : string;
    tfShowFinder: boolean;
    lFind       : TdlgFind;
begin
  Result:=true;
  tfShowFinder := False;
  stTaxOcc:=eRelatedTaxon.Text;
  if FRelTaxonKey <> '' then begin
    if UpperCase(FdmTaxonOcc.GetNameFromOcc(FRelTaxonKey)) <> UpperCase(stTaxOcc) then
      tfShowFinder := True
    else
      stTaxOccKey := FRelTaxonKey;
  end else
    tfShowFinder := True;

  if tfShowFinder then begin
    lFind := TdlgFind.CreateDialog(nil,ResStr_FindRelatedOccurence,ftTaxOcc);
    with lFind do begin
      try
        if FindUnique(stTaxOcc) then begin
          stTaxOccKey := ItemKey;
          stTaxOcc    := FdmTaxonOcc.GetNameFromOcc(stTaxOccKey);
        end else
        if not eSearchText.NoSourceItems then begin
          if ShowModal=mrOk then begin
            stTaxOccKey := ItemKey;
            stTaxOcc    := FdmTaxonOcc.GetNameFromOcc(stTaxOccKey);
          end else begin
            Result := false;
          end;
        end else begin
          MessageDlg(ResStr_TaxOccItems, mtInformation, [mbOK], 0);
          Result:=false;
        end;
      finally
        Release;
      end;
    end;
  end;
  eRelatedTaxon.Text:=stTaxOcc;
  FRelTaxonKey      :=stTaxOccKey;
end;  // CheckTaxOcc

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.eTaxonKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  FDetTaxonKey := '';
  if Key=#13 then begin
    dmGeneralData.CheckTaxon( eTaxon, FDetTaxonKey );
    Key:=#0;
  end;
end;  // eTaxonKeyPress

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbFindTaxonClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actTaxonDiction.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateTaxon);
end;  // bbFindTaxonClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.UpdateTaxon(KeyList:TKeyList);
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then begin
      FDetTaxonKey:=KeyList.Items[0].KeyField1;
      eTaxon.Text :=dmGeneralData.GetTaxonName(FDetTaxonKey);
    end;
  finally
    KeyList.Free;
  end;
end;  // UpdateTaxon

{-------------------------------------------------------------------------------
  Drop handler for determiner
}
procedure TfrmTaxonOccurrences.UpdateDeterminer(KeyList:TKeyList);
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then begin
      eDeterminer.Text :=dmGeneralData.GetIndividualName(KeyList.Items[0].KeyField1);
      eDeterminer.Key  :=KeyList.Items[0].KeyField1;
    end;
  finally
    KeyList.Free;
  end;
end;  // UpdateDeterminer

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.eDetDateExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbDetDiscard.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbDetDiscard.Width]) and (lCancelPos.Y in [0..bbDetDiscard.Height]) then
    bbDetDiscardClick(nil)
  else if not FClosingForm then
    ValidateDeterminationDate;
end;  // eDetDateExit

procedure TfrmTaxonOccurrences.ValidateDeterminationDate;
begin
  if eDetDate.Text<>'' then begin
    ValidateValue(IsVagueDate(eDetDate.Text),
                  ResStr_InvalidVagueDate,eDetDate);
    ValidateValue(CheckVagueDate(eDetDate.Text),
                  InvalidDate(ResStr_DeterminationDate,true,false),eDetDate);
    ValidateValue(dmValidation.CheckDeterminationDateAgainstSample(SampleKey, eDetDate.VagueDate),
                  ResStr_DetDateAgainstSample, eDetDate);
    eDetDate.Text:=VagueDateToString(eDetDate.VagueDate);
  end;
end;  // eDetDateExit

//------------------------------------------------------------------------------
function TfrmTaxonOccurrences.CheckWork: Boolean;
var lFind : TdlgFind;
begin
  Result := true;
  lFind  := TdlgFind.CreateDialog(nil, ResStr_FindDocument, ftReference);
  with lFind do
    try
      if FindUnique(eWork.Text) then begin
        FDetWorkKey := ItemKey;
        eWork.Text  := ItemText;
      end else
      if not eSearchText.NoSourceItems then begin
        if ShowModal = mrOk then begin
          FDetWorkKey := ItemKey;
          eWork.Text  := ItemText;
        end else
          Result := false;
      end else begin
        MessageDlg(ResStr_NoReferenceItems, mtInformation, [mbOK], 0);
        Result := false;
      end;
    finally
      Release;
    end;
end;  // CheckWork

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.eWorkKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=#13 then begin
    CheckWork;
    Key:=#0;
  end;
end;  // eWorkKeyPress

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbFindRefClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actDocuments.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild),Self,UpdateReference);
end;  // bbFindRefClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.UpdateReference(KeyList:TKeyList);
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then begin
      FDetWorkKey:=KeyList.Items[0].KeyField1;
      eWork.Text :=dmGeneralData.GetReferenceText(FDetWorkKey);
    end;
  finally
    KeyList.Free;
  end;
end;  // UpdateReference

//==============================================================================
procedure TfrmTaxonOccurrences.sgRelOccDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  inherited;
  with sgRelOcc do begin
    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
  end;
end;  // sgRelOccDrawCell

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.sgRelOccClick(Sender: TObject);
begin
  inherited;
  with sgRelOcc do
    FCurrentRelOcc:=TRelOccItem(Objects[0,Row]);
  if FCurrentRelOcc=nil then begin
    // Nothing in the grid, prevent editing/deleting regardless of access privileges
    bbRelOccEdit.Enabled:=false;
    bbRelOccDel.Enabled :=false;
    BlankRelOccDetails;
  end else
    with FCurrentRelOcc do begin
      eRelatedTaxon.Text   :=TaxonName;
      FRelTaxonKey         :=OccurrenceKey;
      dbcmbRelType.KeyValue:=RelTypeKey;
      // Get back to the beginning of the stream before reading it.
      Comment.Position:=0;
      reOccComments.Lines.LoadFromStream(Comment);
      EnableDetailsAddEditButtons(TN_TAXON_OCCURRENCE_RELATION,
          'Taxon_Occurrence_Relation_Key', ItemKey,
          Custodian, EditMode, Added, bbRelOccEdit, bbRelOccDel, True);
    end;
end;  // sgRelOccClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.BlankRelOccDetails;
begin
  eRelatedTaxon.Text    :='';
  dbcmbRelType.ItemIndex:=-1;
  reOccComments.Clear;
end;  // BlankRelOccDetails

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.SaveRelOccDetails;
begin
  with FCurrentRelOcc do begin
    TaxonName    :=eRelatedTaxon.Text;
    OccurrenceKey:=FRelTaxonKey;
    RelTypeKey   :=dbcmbRelType.KeyValue;
    RelType      :=dbcmbRelType.Text;
    // Get back to the beginning of the stream before reading it.
    Comment.Position:=0;
    reOccComments.Lines.SaveToStream(Comment);
  end;
  sgRelOcc.Refresh;
end;  // SaveRelOccDetails

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.SetRelOccColour(const tfDetailsOn:boolean);
begin
  SetRequiredFieldsColourState(tfDetailsOn,[eRelatedTaxon,dbcmbRelType]);
end;  // SetRelOccColour

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbRelOccAddClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgRelOcc, bbRelOccAdd, bbRelOccEdit, bbRelOccDel,
                  bbSave, bbCancel, gbRelOccDetails, true);
  SetRelOccColour(true);
  FCurrentRelOcc:=TRelOccItem.CreateNew(FRelOccList);
  BlankRelOccDetails;
  FAddItem:=true;
end;  // bbOccAddClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbRelOccEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgRelOcc, bbRelOccAdd, bbRelOccEdit, bbRelOccDel,
                  bbSave, bbCancel, gbRelOccDetails, true);
  SetRelOccColour(true);
  FAddItem:=false;
end;  // bbRelOccEditClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbRelOccDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteRelatedOccurence,
                mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    with sgRelOcc do
      if TRelOccItem(Objects[0,Row])<>nil then
        FRelOccList.DeleteItem(Row);
    sgRelOccClick(nil);
  end;
end;  // bbRelOccDelClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbRelOccAcceptClick(Sender: TObject);
begin
  inherited;
  ValidateValue(eRelatedTaxon.Text<>'',ResStr_RecordRequiredForOcc,eRelatedTaxon);
  ValidateValue(CheckTaxOcc, ResStr_InvalidTaxonName,eRelatedTaxon);
  ValidateValue(dbcmbRelType.Text<>'',ResStr_RelationTypeMissing, dbcmbRelType);

  SaveRelOccDetails;
  if FAddItem then FRelOccList.AddNew(FCurrentRelOcc);
  SwitchToDetails(sgRelOcc, bbRelOccAdd, bbRelOccEdit, bbRelOccDel,
                  bbSave, bbCancel, gbRelOccDetails, false);
  SetRelOccColour(false);
  sgRelOccClick(nil);
end;  // bbOccAcceptClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbRelOccDiscardClick(Sender: TObject);
begin
  inherited;
  if FAddItem then FCurrentRelOcc.Free;
  SwitchToDetails(sgRelOcc, bbRelOccAdd, bbRelOccEdit, bbRelOccDel,
                  bbSave, bbCancel, gbRelOccDetails, false);
  SetRelOccColour(false);
  sgRelOccClick(nil);
end;  // bbOccDiscardClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.eRelatedTaxonKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=#13 then begin
    CheckTaxOcc;
    Key:=#0;
  end;
end;  // eRelatedTaxonKeyPress

//==============================================================================
procedure TfrmTaxonOccurrences.sgSpecimensDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  inherited;
  with sgSpecimens do begin
    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
  end;
end;  // sgSpecimensDrawCell

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.sgSpecimensClick(Sender: TObject);
begin
  inherited;
  with sgSpecimens do
    FCurrentSpec:=TSpecimenItem(Objects[0,Row]);
  if FCurrentSpec=nil then begin
    // Nothing in the grid, prevent editing/deleting regardless of access privileges
    bbSpecimenEdit.Enabled:=false;
    bbSpecimenDel.Enabled :=false;
    BlankSpecDetails;
  end else
    with FCurrentSpec do begin
      eSpecNumber.Text:=Number;
      eSpecDate.Text  :=AccessionDate;
      dbcmbSpecType.KeyValue:=SpecTypeKey;
      eSpecLocation.Text:=Location;
      // Get back to the beginning of the stream before reading it.
      Comment.Position:=0;
      reSpecComments.Lines.LoadFromStream(Comment);
      // ItemKey is empty for newly added items. Save to actually get a value
      bbSpecimenEdit.Enabled := AppSettings.AllowEdit(EditMode) and
                                (Added or (Custodian = AppSettings.SiteID));
      bbSpecimenDel.Enabled  := AppSettings.AllowEdit(EditMode) and
                                (Added or (Custodian = AppSettings.SiteID));
    end;
end;  // sgSpecimensClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.BlankSpecDetails;
begin
  eSpecNumber.Text:='';
  eSpecDate.Text  :='';
  dbcmbSpecType.ItemIndex:=-1;
  eSpecLocation.Text:='';
  reSpecComments.Clear;
end;  // BlankSpecDetails

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.BlankPrivateDetails;
begin
  ePrivateItemName.Text:='';
  ePrivateDetail.Text  :='';
  dbcmbPrivateType.ItemIndex:=-1;
  ePrivateItemDate.Text := '';
  ePrivateItemValue.Text := '';
  rePrivateComment.Clear;
  lblMetaData.caption := '';
end;  // BlankPrivateDetails

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.SaveSpecDetails;
begin
  with FCurrentSpec do begin
    Number       :=eSpecNumber.Text;
    AccessionDate:=eSpecDate.Text;
    SpecTypeKey  :=dbcmbSpecType.KeyValue;
    SpecType     :=dbcmbSpecType.Text;
    Location     :=eSpecLocation.Text;
    // Get back to the beginning of the stream before reading it.
    Comment.Position:=0;
    reSpecComments.Lines.SaveToStream(Comment);
  end;
  sgSpecimens.Refresh;
end;  // SaveSpecDetails

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.SetSpecimensColour(const tfDetailsOn:boolean);
begin
  SetRequiredFieldsColourState(tfDetailsOn,[eSpecNumber,dbcmbSpecType]);
end;  // SetSpecimensColour

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.SetPrivateColour(const tfDetailsOn:boolean);
begin
  SetRequiredFieldsColourState(tfDetailsOn,[dbcmbPrivateType,ePrivateItemName]);
end;  // SetPrivateColour


//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbSpecimenAddClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgSpecimens,bbSpecimenAdd,bbSpecimenEdit,bbSpecimenDel,
                  bbSave, bbCancel, gbSpecimenDetails,true);
  SetSpecimensColour(true);
  FCurrentSpec:=TSpecimenItem.CreateNew(FSpecimenList);
  BlankSpecDetails;
  FAddItem:=true;
end;  // sbSpecimenAddClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbSpecimenEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgSpecimens,bbSpecimenAdd,bbSpecimenEdit,bbSpecimenDel,
                  bbSave, bbCancel, gbSpecimenDetails,true);
  SetSpecimensColour(true);
  FAddItem:=false;
end;  // bbSpecimenEditClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbSpecimenDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteSpecimen,
                mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    with sgSpecimens do
      if TSpecimenItem(Objects[0,Row])<>nil then
        FSpecimenList.DeleteItem(Row);
    sgSpecimensClick(nil);
  end;
end;  // bbSpecimenDelClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.bbSpecimenAcceptClick(Sender: TObject);
begin
  inherited;
  // Sender is nil if called from CloseQuery method.
  if Sender=nil then ValidateSpecimenDate;

  ValidateValue(eSpecNumber.Text<>'',ResStr_SpecimenNumRequired,eSpecNumber);
  ValidateValue(dbcmbSpecType.Text<>'',ResStr_SpecimenTypeRequired, dbcmbSpecType);

  SaveSpecDetails;
  if FAddItem then FSpecimenList.AddNew(FCurrentSpec);
  SwitchToDetails(sgSpecimens,bbSpecimenAdd,bbSpecimenEdit,bbSpecimenDel,
                  bbSave, bbCancel, gbSpecimenDetails,false);
  SetSpecimensColour(false);
  sgSpecimensClick(nil);
end;  // bbSpecimenAcceptClick


procedure TfrmTaxonOccurrences.bbSpecimenDiscardClick(Sender: TObject);
begin
  inherited;
  if FAddItem then FCurrentSpec.Free;
  SwitchToDetails(sgSpecimens,bbSpecimenAdd,bbSpecimenEdit,bbSpecimenDel,
                  bbSave, bbCancel, gbSpecimenDetails,false);
  SetSpecimensColour(false);
  sgSpecimensClick(nil);
end;  // bbSpecimenDiscardClick

//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.eSpecDateExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbSpecimenDiscard.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbSpecimenDiscard.Width]) and (lCancelPos.Y in [0..bbSpecimenDiscard.Height]) then
    bbSpecimenDiscardClick(nil)
  else if not FClosingForm then
    ValidateSpecimenDate;
end;  // eSpecDateExit

procedure TfrmTaxonOccurrences.ValidateSpecimenDate;
begin
  if eSpecDate.Text<>'' then begin
    ValidateValue(IsVagueDate(eSpecDate.Text),
                  ResStr_InvalidVagueDate,eSpecDate);
    ValidateValue(CheckVagueDate(eSpecDate.Text),
                 InvalidDate(ResStr_AccessionDate,true,true),eSpecDate);
    eSpecDate.Text:=VagueDateToString(eSpecDate.VagueDate);
  end;
end;  // ValidateSpecimenDate

//==============================================================================
procedure TfrmTaxonOccurrences.SetDrillForm(const Value: TBaseForm);
begin
  FDrillForm := Value;
end;  // SetDrillForm

//==============================================================================
procedure TfrmTaxonOccurrences.WMTransferDone(var Msg: TMessage);
begin
  if DrillForm<>nil then TfrmObservations(DrillForm).Show;
end;  // WMTransferDone

//==============================================================================
procedure TfrmTaxonOccurrences.UpdateRTFMenu;
begin
  if Assigned(DrillForm) then
    dmFormActions.UpdateRTFMenu(((DrillForm.ActiveControl is TDBRichEdit) and
                               (FdmTaxonOcc.qryTaxonOcc.State in [dsEdit,dsInsert])) or
                              (DrillForm.ActiveControl is TRichEdit));
end;  // UpdateRTFMenu

//==============================================================================
procedure TfrmTaxonOccurrences.EnterRTF(Sender: TObject);
begin
  inherited;
  if Sender is TDBRichEdit then
    dmFormActions.UpdateRTFMenu(FdmTaxonOcc.qryTaxonOcc.State in [dsEdit,dsInsert])
  else
    dmFormActions.UpdateRTFMenu(true);
end;  // EnterRTF

//==============================================================================
procedure TfrmTaxonOccurrences.ExitRTF(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(false);
end;  // ExitRTF

//==============================================================================
{ Overridden method to set up all drag drop destination controls }
procedure TfrmTaxonOccurrences.SetupDestinationControls;
begin
  RegisterDropComponent(eTaxon, DropDetTaxonName,
                        [TN_TAXON_LIST_ITEM], [CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(eDeterminer, DropDeterminerName,
                        [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(eWork, DropWorkData,
                        [TN_REFERENCE], [CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(sgRelOcc, DropGridOcc,
                        [TN_TAXON_OCCURRENCE], [CF_JNCCDATA]);
  RegisterDropComponent(eRelatedTaxon, DropDetailOcc,
                        [TN_TAXON_OCCURRENCE], [CF_JNCCDATA, CF_TEXT]);
end;

//==============================================================================
procedure TfrmTaxonOccurrences.DropDetTaxonName(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
begin
  if (EditMode<>emView) and FEditDetails then begin
    if iSourceData.Header.ItemCount>0 then
      if (iFormat = CF_JNCCDATA) then begin
        ioHandled:= true;
        FDetTaxonKey:=iSourceData.Items[0].KeyField1;
        eTaxon.Text :=dmGeneralData.GetTaxonName(FDetTaxonKey);
      end else
        ioHandled:=false;
  end else
    ioHandled:=true;
end;  // DropDetTaxonName

//==============================================================================
procedure TfrmTaxonOccurrences.DropDeterminerName(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
begin
  if (EditMode <> emView) and FEditDetails then
    ioHandled := dmGeneralData.DropLinkedEditText(
        eDeterminer,
        iFormat,
        iSourceData,
        dmGeneralData.GetIndividualName,
        iTextStrings)
  else
    ioHandled := True;
end;  // DropDeterminerName

//==============================================================================
procedure TfrmTaxonOccurrences.DropWorkData(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
begin
  if (EditMode<>emView) and FEditDetails then begin
    if iSourceData.Header.ItemCount>0 then
      if (iFormat = CF_JNCCDATA) then begin
        ioHandled:= true;
        FDetWorkKey:=iSourceData.Items[0].KeyField1;
        eWork.Text :=dmGeneralData.GetReferenceText(FDetWorkKey);
      end else
        ioHandled:=false;
  end else
    ioHandled:=true;
end;  // DropWorkData

//==============================================================================
procedure TfrmTaxonOccurrences.DropGridOcc(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
begin
  if (EditMode<>emView) and not FEditDetails then begin
    if iSourceData.Header.ItemCount>0 then
      if (iFormat = CF_JNCCDATA) then begin
        ioHandled:=true;
        dbcmbRelType.ItemIndex:=0;
        FCurrentRelOcc:=TRelOccItem.CreateNew(FRelOccList);
        with FCurrentRelOcc do begin
          OccurrenceKey:=iSourceData.Items[0].KeyField1;
          TaxonName    :=FdmTaxonOcc.GetNameFromOcc(iSourceData.Items[0].KeyField1);
          RelTypeKey   :=dbcmbRelType.KeyValue;
          RelType      :=dbcmbRelType.Text;
        end;
        FRelOccList.AddNew(FCurrentRelOcc);
        sgRelOccClick(nil);
      end else
        ioHandled:=false;
  end else
    ioHandled:=true;
end;  // DropGridOcc

//==============================================================================
procedure TfrmTaxonOccurrences.DropDetailOcc(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
begin
  if (EditMode<>emView) and FEditDetails then begin
    if iSourceData.Header.ItemCount>0 then
      if (iFormat = CF_JNCCDATA) then begin
        ioHandled:=true;
        if FAddItem then dbcmbRelType.ItemIndex:=0;  // Leave the type if modifying
        FRelTaxonKey      :=iSourceData.Items[0].KeyField1;
        eRelatedTaxon.Text:=FdmTaxonOcc.GetNameFromOcc(iSourceData.Items[0].KeyField1);
        reOccComments.Clear;
      end else
        ioHandled:=false;
  end else
    ioHandled:=true;
end;  // DropDetailOcc

//==============================================================================
function TfrmTaxonOccurrences.GetKeyList: TKeyList;
var lNewKeyList:TEditableKeyList;
begin
  //Return an editable key list with the selected nodes key
  lNewKeyList:= TEditableKeyList.Create;
  lNewKeyList.SetTable(TN_TAXON_OCCURRENCE);
 	lNewKeyList.AddItem(TaxonOccKey,'');
  Result:= lNewKeyList;
end;  // GetKeyList

//==============================================================================
procedure TfrmTaxonOccurrences.pcTaxonOccurrenceChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  inherited;
  AllowChange:=not FEditDetails;
end;  // pcTaxonOccurrenceChanging

{-------------------------------------------------------------------------------
  Update the contents of any term-list linked combo boxes, called when the
     termlist screen broadcasts a refresh.
}
procedure TfrmTaxonOccurrences.RefreshLists;
begin
  RefreshComboList(dbcmbRecordType);
  RefreshComboList(dbcmbSubstrate);
  RefreshComboList(cmbDeterminationType);
  RefreshComboList(dbcmbRole);
  RefreshComboList(dbcmbType);
  RefreshComboList(dbcmbRelType);
  RefreshComboList(dbcmbSpecType);
  RefreshComboList(dbcmbPrivateType);
  Measurements.RefreshLists;
end;  // Refreshlists

//==============================================================================
procedure TfrmTaxonOccurrences.pcTaxonOccurrenceChange(Sender: TObject);
var
  lDetTypeKey: String;
begin
  inherited;
  SelectPreferredDetermination;
  pcTaxonOccurrence.HelpContext := pcTaxonOccurrence.ActivePage.HelpContext;
  if pcTaxonOccurrence.ActivePage = tsGeneral then begin
    if Assigned(FPreferredDet) then
      lDetTypeKey := FPreferredDet.DetTypeKey
    else
      lDetTypeKey := dmGeneralData.GetPreferredTaxonDetType(FTaxonOccKey);
    FdmDetermination.qryDetType.Parameters.ParamByName('KeyParameter').Value := lDetTypeKey;
    RefreshComboList(cmbDeterminationType);
    cmbDeterminationType.KeyValue := lDetTypeKey;
  end else if (pcTaxonOccurrence.ActivePage = tsDeterminations) and Assigned(FCurrentDet) then begin
    FdmDetermination.qryDetType.Parameters.ParamByName('KeyParameter').Value := FCurrentDet.DetTypeKey;
    RefreshComboList(dbcmbType);
    RefreshComboList(dbcmbRole);
    dbcmbType.KeyValue := FCurrentDet.DetTypeKey;
  end;
end;

//==============================================================================
procedure TfrmTaxonOccurrences.dbcbCheckedClick(Sender: TObject);
var lForm:TForm;
begin
  inherited;
  lForm:=frmMain.GetForm(TfrmObservations);
  if lForm<>nil then
    lForm.Repaint;
end;

//==============================================================================
procedure TfrmTaxonOccurrences.RefreshTaxonName;
var
  liIndex: integer;
  lCurrItem: TDeterminationItem;
begin
  eTaxon.Text :=dmGeneralData.GetTaxonName(FDetTaxonKey);
  with FDeterminationList do begin
    for liIndex := 0 to Count-1 do begin
      lCurrItem := TDeterminationItem(Items[liIndex]);
      lCurrItem.ItemName:=dmGeneralData.GetTaxonName(lCurrItem.ItemNameKey);
      RefreshItemDisplay(lCurrItem);
    end;
  end;
end;

//==============================================================================
procedure TfrmTaxonOccurrences.RefreshNames;
var
  liIndex: integer;
  lCurrItem : TDeterminationItem;
begin
  eDeterminer.Text:=dmGeneralData.GetIndividualName(eDeterminer.Key);
  with FDeterminationList do begin
    for liIndex := 0 to Count-1 do begin
      lCurrItem := TDeterminationItem(Items[liIndex]);
      lCurrItem.Determiner:=dmGeneralData.GetName(lCurrItem.DeterminerKey);
      RefreshItemDisplay(lCurrItem);
    end;
  end;
end;

//==============================================================================
procedure TfrmTaxonOccurrences.RefreshColours;
begin
  if EditMode<>emView then begin
    SetRequiredFieldsColourState(bbDetAccept.Enabled, [eTaxon,eDeterminer,dbcmbRole,dbcmbType,eDetDate]);
    SetRequiredFieldsColourState(bbRelOccAccept.Enabled, [eRelatedTaxon,dbcmbRelType]);
    SetRequiredFieldsColourState(bbSpecimenAccept.Enabled,[eSpecNumber,dbcmbSpecType]);
    SetRequiredFieldsColourState(bbPrivateAccept.Enabled,[ePrivateItemName,dbcmbPrivateType]);
  end;
end;

//==============================================================================
procedure TfrmTaxonOccurrences.ShowMetadata;
begin
  with TdlgMetaDataPopup.Create(nil) do
  try
    ShowStandard(ResStr_TaxonOcc, lblTaxonName.Caption,
       FdmTaxonOcc.qryTaxonOcc.FieldByName('Taxon_Occurrence_Key').AsString,
       FdmTaxonOcc.qryTaxonOcc);
  finally
    Free;
  end;
end;

//==============================================================================
{ Description : Show the map and scroll to centre this occurrence.  If the
              map is unavailable, then reset it. }
procedure TfrmTaxonOccurrences.FindOnMap;
var lLocationKey, lSpatialRef, lSRefSys: string;
begin
  with dmDatabase.ExecuteSQL('SELECT Location_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long ' +
                             'FROM Sample WHERE Sample_Key = ''' + FSampleKey + '''', True) do
  begin
    if not Eof then begin
      lLocationKey := VarToStr(Fields['Location_Key'].Value);
      lSpatialRef  := VarToStr(Fields['Spatial_Ref'].Value);
      lSRefSys     := VarToStr(Fields['Spatial_Ref_System'].Value);
    end;
    Close;
  end;
  TfrmMap.CentreOnPlace(lLocationKey, lblTaxonName.Caption, lSpatialRef, lSRefSys);
end;

{ Description : Gets the Review Status Code }

function TfrmTaxonOccurrences.GetReviewStatusCode : integer;
begin
  Result := 0;
  If  GetCompetencyLevel <> 0 then begin
    with dmDatabase.ExecuteSQL('SELECT  [dbo].[ufn_ReturnReviewStatus](''' + FTaxonOccKey  + ''') AS ReviewStatus',True) do
    begin
      if not Eof then
        Result :=  Fields['ReviewStatus'].Value
      else
        Result :=  0;
      Close;
    end;
  end;
end;


function TfrmTaxonOccurrences.GetCompetencyLevel : integer;
begin

  with dmDatabase.ExecuteSQL('SELECT  DATA FROM SETTING WHERE NAME = ''Competency''',True) do
  begin
    if not Eof then
        Result :=  Fields['DATA'].Value
    else
      Result :=  0;
    Close;
  end;
end;
{-------------------------------------------------------------------------------
}
 { Description : Gets the review status given the code}
function TfrmTaxonOccurrences.GetReviewStatus(const statuscode : integer) : String;

begin
   case StatusCode of
      0:  Result:= '';
      1:  Result:=  '/' + ResStr_Not_Reviewed;
      2:  Result := '/' + ResStr_Review_Unactioned;
      3:  Result := '/' + ResStr_Reviewed;
   end;
end;
{-------------------------------------------------------------------------------
}

{ Description : Gets the review status summary given the code}
function TfrmTaxonOccurrences.GetReviewStatusSummary (const statuscode : integer) : String;

begin
   case StatusCode of
      0:  Result := '';
      1:  Result :=  ResStr_Not_Reviewed_Summary;
      2:  Result := ResStr_Review_Unactioned_Summary;
      3:  Result := ResStr_Reviewed_Summary ;
   end;
end;
{-------------------------------------------------------------------------------
}




procedure TfrmTaxonOccurrences.eWorkDblClick(Sender: TObject);
begin
  inherited;
  if FDetWorkKey<>'' then
    dmGeneralData.SourcesShowInternal(Sender, FDetWorkKey);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmTaxonOccurrences.dbCheckBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  //pretend a key has been pressed.
  PostMessage(TWinControl(Sender).Handle, WM_KEYDOWN, VK_Return,0);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmTaxonOccurrences.cmbDeterminationTypeChange(Sender: TObject);
begin
  inherited;
  SelectPreferredDetermination;

  if (not Assigned(FPreferredDet)) or (cmbDeterminationType.Text <> FPreferredDet.DetType) then
  begin
    if Assigned(FPreferredDet) then
      eTaxon.Text := FPreferredDet.ItemName;
    pcTaxonOccurrence.ActivePage := tsDeterminations;
    bbDetAddClick(Sender); //Mimics the add button click, creating a new record.

    eDeterminer.Text    := AppSettings.UserName;
    eDetDate.Text       := DateToStr(Date);
    cbPreferred.Checked := True;
    dbcmbRole.KeyValue  := DETERMINATION_CONFIRMATION_ROLE_KEY;
    dbcmbType.ItemIndex := dbcmbType.Items.IndexOf(cmbDeterminationType.Text);
  end;
end;

{-------------------------------------------------------------------------------
  Setup F9 link to Determiner edit box.
}
procedure TfrmTaxonOccurrences.eDeterminerGetData(Sender: TObject);
begin
  inherited;
  dmFormActions.actNames.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateDeterminer);
end;

{-------------------------------------------------------------------------------
  Setup find dialog for Determiner edit box
}
procedure TfrmTaxonOccurrences.eDeterminerFindData(Sender: TObject);
begin
  inherited;
  dmGeneralData.CheckIndividual(eDeterminer);
end;

{-------------------------------------------------------------------------------
  By de-activating then re-activating a DB combo, its contents get re-populated
}
procedure TfrmTaxonOccurrences.RefreshComboList(ACombo: TDBListCombo);
begin
  ACombo.Active := not ACombo.Active;
  ACombo.Active := not ACombo.Active;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmTaxonOccurrences.GetInternalSources(keyList: TEditableKeyList);
var
  i: Integer;
begin
  Sources.GetInternalSources(keyList);
  with sgDeterminations do
    for i := FixedRows to RowCount - 1 do
      if TDeterminationItem(Objects[0, i]).WorkKey <> '' then
        keyList.AddItem(TDeterminationItem(Objects[0, i]).WorkKey, TN_SOURCE);
end;

{-------------------------------------------------------------------------------
  When the
}
procedure TfrmTaxonOccurrences.pnlDetailsResize(Sender: TObject);
var
  deltaWidth, deltaHeight, width, halfWidth : Integer;
begin
  inherited;
  // Works out the change in height and width of the panel.
  deltaWidth  := pnlDetails.Width - pnlInner.Width;
  deltaHeight := pnlDetails.Height - pnlInner.Height;

  // For some reason, the automatic resizing doesn't work properly. Using a
  // second panel within the main panel and sizing it manually solves this, but
  // is a bit of a hack.
  pnlInner.Width  := pnlDetails.Width;
  pnlInner.Height := pnlDetails.Height;

  // Custom controls do not have anchor properties and must be resized manually.
  eDeterminer.Width   := eDeterminer.Width + deltaWidth;
  Sources.Width       := Sources.Width + deltaWidth;
  Sources.Height      := Sources.Height + deltaHeight;
  Measurements.Width  := Measurements.Width + deltaWidth;
  Measurements.Height := Measurements.Height + deltaHeight;

  width := bvlGeneral.Width - dbcmbSubstrate.Left + bvlGeneral.Left
                            - lblProvenance.Width
                            - 4 * 2 //Internal spacing
                            - 8;    //Space to bevel edge
  halfWidth := width div 2;

  dbcmbSubstrate.Width := halfWidth;
  lblProvenance.Left   := dbcmbSubstrate.Left + dbcmbSubstrate.Width + 4;
  cmbProvenance.Left   := lblProvenance.Left + lblProvenance.Width + 4;
  cmbProvenance.Width  := halfWidth;
end;

procedure TfrmTaxonOccurrences.sgPrivateOccClick(Sender: TObject);
begin
  inherited;
  with sgPrivateOcc do
    FCurrentPrivateData:=TPrivateDataItem(Objects[0,Row]);
  if FCurrentPrivatedata =nil then begin
    // Nothing in the grid, prevent editing/deleting regardless of access privileges
    bbPrivateEdit.Enabled:=false;
    bbPrivateDel.Enabled :=false;
    BlankPrivateDetails;
  end else
    with FCurrentPrivateData do begin
      ePrivateItemName.Text :=PrivateItemName;
      ePrivateItemDate.Text := PrivateDate;
      ePrivateItemValue.Text := PrivateValue;
      ePrivateDetail.Text := PrivateDetail;
      dbcmbPrivateType.KeyValue := PrivateTypeKey;
      PrivateComment.Position:=0;
      rePrivateComment.Lines.LoadFromStream(PrivateComment);
      // ItemKey is empty for newly added items. Save to actually get a value
      bbPrivateEdit.Enabled := AppSettings.AllowEdit(EditMode) and
                                (Added or (Custodian = AppSettings.SiteID));
      bbPrivateDel.Enabled  := AppSettings.AllowEdit(EditMode) and
                                (Added or (Custodian = AppSettings.SiteID));
      lblMetaData.caption := GetTypeDescription;
    end;

end;

procedure TfrmTaxonOccurrences.bbPrivateAddClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgPrivateOcc,bbPrivateAdd,bbPrivateEdit,bbPrivateDel,
                  bbSave, bbCancel, gbPrivateDetails,true);
  SetPrivateColour(true);
  FCurrentPrivateData:=TPrivateDataItem.CreateNew(FPrivateDataList);
  BlankPrivateDetails;
  FAddItem:=true;
end;

procedure TfrmTaxonOccurrences.bbPrivateEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgPrivateOcc,bbPrivateAdd,bbPrivateEdit,bbPrivateDel,
                  bbSave, bbCancel, gbPrivateDetails,true);
  SetPrivateColour(true);
  FAddItem:=false;
end;

procedure TfrmTaxonOccurrences.bbPrivateDelClick(Sender: TObject);
begin
  inherited;
  inherited;
  if MessageDlg(ResStr_DeletePrivate,
                mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    with sgPrivateOcc do
      if TPrivateDataItem(Objects[0,Row])<>nil then
        FPrivateDataList.DeleteItem(Row);
    sgPrivateOccClick(nil);
  end;
end;

procedure TfrmTaxonOccurrences.bbPrivateAcceptClick(Sender: TObject);

begin
  inherited;
  // Sender is nil if called from CloseQuery method.
  ValidateValue(dbcmbPrivateType.Text<>'',ResStr_PrivateTypeRequired, dbcmbPrivateType);
  ValidateValue(ePrivateItemName.Text<>'',ResStr_PrivateItemRequired, ePrivateItemName);
  if ePrivateItemDate.Text<>'' then begin
    try
      ePrivateItemDate.Text := VagueDateToString(StringToVagueDate(ePrivateItemDate.Text));
    except
    end;
    // And validate
    ValidateValue(IsDate(ePrivateItemDate.Text),
                  InvalidStandardDate(ResStr_TheItemDate,true));
    ePrivateItemDate.Text:=DateToStr(StrToDate(ePrivateItemDate.Text));
  end;
  SavePrivateDetails;
  if FAddItem then FPrivateDataList.AddNew(FCurrentPrivateData);
  SwitchToDetails(sgPrivateOcc,bbPrivateAdd,bbPrivateEdit,bbPrivateDel,
                  bbSave, bbCancel, gbPrivateDetails,false);
  SetPrivateColour(false);
  sgPrivateOccClick(nil);
end;

procedure TfrmTaxonOccurrences.bbPrivateDiscardClick(Sender: TObject);
begin
  inherited;
  if FAddItem then FCurrentPrivateData.Free;
  SwitchToDetails(sgPrivateOcc,bbPrivateAdd,bbPrivateEdit,bbPrivateDel,
                  bbSave, bbCancel, gbPrivateDetails,false);
  SetPrivateColour(false);
  sgPrivateOccClick(nil);
end;
//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.SavePrivateDetails;
begin
  with FCurrentPrivateData do begin
    PrivateItemName  :=ePrivateItemName.Text;
    PrivateDate   := ePrivateItemDate.Text;
    PrivateValue :=  ePrivateItemValue.Text;
    PrivateDetail:= ePrivateDetail.Text;
    PrivateType     :=dbcmbPrivateType.Text;
    PrivateTypeKey := dbcmbPrivateType.KeyValue;
     // Get back to the beginning of the stream before reading it.
    PrivateComment.Position:=0;
      rePrivateComment.Lines.SaveToStream(PrivateComment);
  end;
  sgPrivateOCC.Refresh;
end;
//------------------------------------------------------------------------------
procedure TfrmTaxonOccurrences.sgPrivateOccDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  inherited;
   with sgPrivateOcc do begin
    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
  end;
end;




procedure TfrmTaxonOccurrences.dbcmbPrivateTypeChange(Sender: TObject);
begin
  inherited;
  lblMetaData.caption := GetTypeDescription;
end;


function TfrmTaxonOccurrences.GetTypeDescription : string;
begin
  with dmDatabase.ExecuteSQL('SELECT dbo.ufn_RtfToPlaintext(Description) AS Description ' +
                             ' FROM Taxon_Private_Type WHERE Taxon_Private_Type_key = ''' + dbcmbPrivateType.keyvalue + '''', True) do
  begin
    if not Eof then
     Result := VarToStr(Fields['Description'].Value)
    else
      Result :=  '';
    Close;
  end;
end;

end.
