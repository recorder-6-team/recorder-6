//==============================================================================
//  Unit:        FeatureDetails
//
//  Implements:  TfrmFeatureDetails
//
//  Description: This form is docked on the Locations screen to allow details
//               of a selected feature to be viewed and/or edited.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 116 $
//    $Date: 11/03/09 11:16 $
//    $Author: Ericsalmon $
//
//  $History: FeatureDetails.pas $
//  
//  *****************  Version 116  *****************
//  User: Ericsalmon   Date: 11/03/09   Time: 11:16
//  Updated in $/JNCC/Development/Build/Source
//  18655. Bug fix.
//
//  *****************  Version 115  *****************
//  User: Pauldavies   Date: 19/01/09   Time: 12:14
//  Updated in $/JNCC/Development/Build/Source
//  Added splitters, resized buttons and other minor alignment changes.
//  
//  *****************  Version 114  *****************
//  User: Pauldavies   Date: 2/01/09    Time: 16:48
//  Updated in $/JNCC/Development/Build/Source
//  Incident ID: 18366
//  CCN 272
//  Added support for resizing.
//  
//  *****************  Version 113  *****************
//  User: Qingsun      Date: 18/09/08   Time: 10:04
//  Updated in $/JNCC/Development/Build/Source
//  
//  *****************  Version 112  *****************
//  User: Johndurman   Date: 23/05/08   Time: 15:51
//  Updated in $/JNCC/Development/Build/Source
//  VI 17177 - CCN263 - Limiting deletion of record attributes where the
//  current system is not the custodian
//  
//  *****************  Version 111  *****************
//  User: Ericsalmon   Date: 26/03/08   Time: 18:05
//  Updated in $/JNCC/Development/Build/Source
//  VI16723. Fixed hardcoded "Internal Map" string problem. Also changed
//  hardcoded gird column titles to individual resource strings.
//  
//  *****************  Version 110  *****************
//  User: Johnvanbreda Date: 11/02/08   Time: 16:31
//  Updated in $/JNCC/Development/Build/Source
//  CCN248 - F11 hotkey basic work
//  
//  *****************  Version 109  *****************
//  User: Rickyshrestha Date: 3/01/08    Time: 14:35
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardocded strings to resourestring
//  
//  *****************  Version 108  *****************
//  User: Rickyshrestha Date: 27/12/07   Time: 13:46
//  Updated in $/JNCC/Development/Build/Source
//  Replaced some hardcoded strings to resourcestring
//  
//  *****************  Version 107  *****************
//  User: Rickyshrestha Date: 20/12/07   Time: 13:16
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 106  *****************
//  User: Rickyshrestha Date: 20/12/07   Time: 11:01
//  Updated in $/JNCC/Development/Build/Source
//  Changed some constants to resourcestring
//  
//  *****************  Version 105  *****************
//  User: Rickyshrestha Date: 12/12/07   Time: 16:40
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestrings
//  ResStr_FeatureMissing
//    ResStr_ManagementAim
//    ResStr_NameRequired
//    ResStr_AuthorityRequired
//    ResStr_InvalidAuthority
//    ResStr_DateInPast
//    ResStr_AgreementDateRequired
//    ResStr_ThreatTypeMissing
//    ResStr_ThreatMissing
//    ResStr_DeleteOccurence
//    ResStr_DamageOccurenceDate
//    ResStr_DamageDescription
//    ResStr_InvalidVagueDate
//  
//  *****************  Version 104  *****************
//  User: Johnvanbreda Date: 20/01/06   Time: 14:30
//  Updated in $/JNCC/Development/Build/Source
//  IR10812
//  Review of security
//
//  *****************  Version 103  *****************
//  User: Johnvanbreda Date: 20/12/05   Time: 12:02
//  Updated in $/JNCC/Development/Build/Source
//  CCN132
//  Restructuring custodian field handling
//  
//  *****************  Version 102  *****************
//  User: Ericsalmon   Date: 12/12/05   Time: 13:59
//  Updated in $/JNCC/Development/Build/Source
//  CCN132. Full use restriction to own data.
//  
//  *****************  Version 101  *****************
//  User: Ericsalmon   Date: 18/10/04   Time: 15:21
//  Updated in $/JNCC/Development/Build/Source
//  ID 7029. Discarding new item was causing datamodule to be re-created
//  and not destroyed. Fixed.
//  
//  *****************  Version 100  *****************
//  User: Ericsalmon   Date: 12/10/04   Time: 11:20
//  Updated in $/JNCC/Development/Build/Source
//  Fix related to incident 7433. Key in additional tabs is refreshed
//  before a save, so that a new record's key is correctly passed on to the
//  addin.
//  
//  *****************  Version 99  *****************
//  User: Andrewkemp   Date: 29/06/04   Time: 17:23
//  Updated in $/JNCC/Development/Build/Source
//  VI 4571
//  When registering drop controls include CF_TEXT as an acceptable format
//  for text boxes and other appropriate controls.
//  
//  *****************  Version 98  *****************
//  User: Ericsalmon   Date: 5/04/04    Time: 10:07
//  Updated in $/JNCC/Development/Build/Source
//  Optimisation.
//  
//  *****************  Version 97  *****************
//  User: Ericsalmon   Date: 19/02/04   Time: 11:59
//  Updated in $/JNCC/Development/Build/Source
//  ID 3802. Drag/Drop and Cut/Paste on trees.
//  
//  *****************  Version 96  *****************
//  User: Ericsalmon   Date: 27/01/04   Time: 18:04
//  Updated in $/JNCC/Development/Build/Source
//  Development.
//  
//  *****************  Version 95  *****************
//  User: Ericsalmon   Date: 8/12/03    Time: 15:44
//  Updated in $/JNCC/Development/Build/Source
//  Removed CheckToDeleteRecord function. Unnecessary with SQL Server.
//  
//  *****************  Version 94  *****************
//  User: Ericsalmon   Date: 26/02/03   Time: 11:50
//  Updated in $/JNCC/Source
//  IR 504 - Change of custodian check.
//  
//==============================================================================

unit FeatureDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseFormUnit, ExtCtrls, StdCtrls, Grids, VagueDateEdit, Buttons, ComCtrls,
  Menus, CompositeComponent, Sources, FeatureDetailsData, Mask, DataClasses,
  Db, DBCtrls, DBListCombo, ExceptionForm, DropTarget, VagueDate, Constants,
  BaseDockedForm, ValidationData, OnlineHelp, ImageListButton, GeneralFunctions,
  DatabaseAccessADO, AddinCompositeComponent, AddinLinkedControls;

type
  EFeatureDetailsError = class(TExceptionPath);

  TfrmFeatureDetails = class(TfrmBaseDockedForm)
    pnlDetails: TPanel;
    pnlInner: TPanel;
    bbSave: TImageListButton;
    bbCancel: TImageListButton;
    pcFeatureDetails: TPageControl;
    tsGeneral: TTabSheet;
    Bevel1: TBevel;
    Label9: TLabel;
    Label10: TLabel;
    Label29: TLabel;
    Label44: TLabel;
    dbreComments: TDBRichEdit;
    dbeFeatureName: TDBEdit;
    dblcFeatureGrading: TDBLookupComboBox;
    cmbFeatureType: TDBListCombo;
    tsAims: TTabSheet;
    gbAimDetails: TGroupBox;
    Label21: TLabel;
    Label19: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    bbAimAccept: TImageListButton;
    bbAimDiscard: TImageListButton;
    eAimName: TEdit;
    eNextAppraisal: TEdit;
    eAgreementDate: TEdit;
    reAimDescription: TRichEdit;
    eAimAuthority: TNameLinkedEdit;
    tsPotentialThreats: TTabSheet;
    gbPotentialDetails: TGroupBox;
    Label25: TLabel;
    Label26: TLabel;
    Label12: TLabel;
    bbPotentialAccept: TImageListButton;
    bbPotentialDiscard: TImageListButton;
    eThreat: TEdit;
    reThreatComments: TRichEdit;
    cmbThreatType: TDBListCombo;
    tsDamages: TTabSheet;
    gbDamageDetails: TGroupBox;
    Label35: TLabel;
    Label43: TLabel;
    bbDamageAccept: TImageListButton;
    bbDamageDiscard: TImageListButton;
    eOccurrenceDate: TVagueDateEdit;
    reDamageComments: TRichEdit;
    tsSources: TTabSheet;
    Sources: TSources;
    lblFeature: TLabel;
    Label2: TLabel;
    pnlManagementTop: TPanel;
    sgAims: TStringGrid;
    bbAimAdd: TImageListButton;
    bbAimEdit: TImageListButton;
    bbAimDel: TImageListButton;
    splManagment: TSplitter;
    splThreats: TSplitter;
    pnlThreatsTop: TPanel;
    sgPotentialThreats: TStringGrid;
    bbPotentialAdd: TImageListButton;
    bbPotentialEdit: TImageListButton;
    bbPotentialDel: TImageListButton;
    splDamage: TSplitter;
    pnlDamageTop: TPanel;
    bbDamageAdd: TImageListButton;
    bbDamageEdit: TImageListButton;
    bbDamageDel: TImageListButton;
    sgDamageOccurrences: TStringGrid;
    eFeatureFrom: TEdit;
    eFeatureTo: TEdit;
    Label32: TLabel;
    Label31: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbSaveClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure bbAimAddClick(Sender: TObject);
    procedure bbAimAcceptClick(Sender: TObject);
    procedure bbPotentialAddClick(Sender: TObject);
    procedure bbPotentialAcceptClick(Sender: TObject);
    procedure bbDamageAddClick(Sender: TObject);
    procedure bbDamageAcceptClick(Sender: TObject);
    procedure pcFeatureDetailsChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cmbFeatureTypeChange(Sender: TObject);
    procedure bbAimEditClick(Sender: TObject);
    procedure bbAimDelClick(Sender: TObject);
    procedure bbAimDiscardClick(Sender: TObject);
    procedure sgAimsClick(Sender: TObject);
    procedure bbPotentialEditClick(Sender: TObject);
    procedure bbPotentialDelClick(Sender: TObject);
    procedure sgPotentialThreatsClick(Sender: TObject);
    procedure bbPotentialDiscardClick(Sender: TObject);
    procedure bbDamageEditClick(Sender: TObject);
    procedure bbDamageDelClick(Sender: TObject);
    procedure sgDamageOccurrencesClick(Sender: TObject);
    procedure bbDamageDiscardClick(Sender: TObject);
    procedure EnterRTF(Sender: TObject);
    procedure ExitRTF(Sender: TObject);
    procedure pnlDetailsResize(Sender: TObject);
    procedure sgAimsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sgPotentialThreatsDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgDamageOccurrencesDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure eNextAppraisalExit(Sender: TObject);
    procedure eAgreementDateExit(Sender: TObject);
    procedure eOccurrenceDateExit(Sender: TObject);
    procedure pcFeatureDetailsChange(Sender: TObject);
    procedure dbComboKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dbComboKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dbComboClick(Sender: TObject);
    procedure eAimAuthorityFindData(Sender: TObject);
    procedure eAimAuthorityGetData(Sender: TObject);
    procedure eFeatureFromExit(Sender: TObject);
    procedure eFeatureToExit(Sender: TObject);
  private
    FdmFeatureDetails:TdmFeatureDetails;
    FDrillForm  :TBaseForm;
    FClosingForm:boolean;
    FAddItem    :Boolean;

    FAimList      :TAimList;
    FCurrentAim   :TAimItem;
    FThreatList   :TThreatList;
    FCurrentThreat:TThreatItem;
    FDamageList   :TDamageList;
    FCurrentDamage:TDamageItem;

    FParentKey: TKeyString;
    FFeatureKey: TKeyString;

    FKeyDown: Boolean;//indicates whether a key has been pressed on a combo box
    procedure EnableDetails(const NewMode:Constants.TEditMode);
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
    procedure SetDrillForm(const Value: TBaseForm);
    procedure SetupObjects;
    procedure FreeObjects;
		procedure DropAimAuthority(const Sender: TObject;
		  const iFormat : integer; const iSourceData: TKeyList;
		  const iTextStrings : TStringList; var ioHandled : boolean);
    procedure BlankAim;
    procedure SaveAim;
    procedure BlankThreat;
    procedure SaveThreat;
    procedure BlankDamage;
    procedure SaveDamage;
    procedure UpdateAimAuthority(KeyList: TKeyList);
    procedure SetAimColour(const tfDetailsOn: boolean);
    procedure SetThreatColour(const tfDetailsOn: boolean);
    procedure SetDamageColour(const tfDetailsOn: boolean);
    procedure ValidateAgreementDate;
    procedure ValidateAppraisalDate;
    procedure ValidateOccurrenceDate;
    procedure ValidateFeatureFromDate;
    procedure ValidateFeatureToDate;
  protected
    procedure SetupDestinationControls; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetKeyList: TKeyList; override;
    procedure UpdateRTFMenu; override;
    procedure AddRecord(const AFeatDetKey:TKeyString);
    procedure EditRecord;
    procedure DisplayRecord(const AFeatDetKey:TKeyString);
    function DeleteRecord(const AFeatDetKey:TKeyString): Boolean;
    procedure RefreshLists;
    procedure RefreshColours;
    procedure RefreshNames;
    procedure ShowMetaData; override;
    property DrillForm:TBaseForm read FDrillForm write SetDrillForm;
    property FeatureKey:TKeyString read FFeatureKey write FFeatureKey;
    property ParentKey:TKeyString read FParentKey write FParentKey;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, Maintbar, Locations, GeneralData, ApplicationSettings, Find,
  MetadataPopup;

resourcestring
  ResStr_FeatureMissing = 'The Feature Name is missing. Enter a value.';
  ResStr_ManagementAim =  'Are you sure you want to delete this Management Aim?';
  ResStr_NameRequired = 'A Name is required for every Aim.';
  ResStr_AuthorityRequired = 'An Authority Name is required for every Aim.';
  ResStr_InvalidAuthority = 'The Authority Name is invalid. Enter a valid Name.';
  ResStr_DateInPast = 'The Next Appraisal Date cannot be in the past.';
  ResStr_AgreementDateRequired = 'An Agreement Date is required for every Aim.';
  ResStr_ThreatTypeMissing = 'The Threat Type is missing. Select a value from the list.';
  ResStr_ThreatMissing = 'The Threat is missing. Enter a value.';
  ResStr_DeleteOccurence = 'Are you sure you want to delete this Damage Occurrence?';
  ResStr_DamageOccurenceDate = 'A Date is required for every Damage Occurrence.';
  ResStr_DamageDescription =  'A Damage description is required for every Damage Occurrence.';
  ResStr_InvalidVagueDate = 'The vague date you have entered is not valid';
  ResStr_EndBeforeStartDate     = 'The End Date cannot come before the Start Date.';
  ResStr_EnterValidNextAppraisalDate =  'The Next Appraisal Date must be a '+
                                        'valid standard date.';

  ResStr_DeletePotentialThreat =  'Are you sure you want to delete this Potential Threat?';

//==============================================================================
constructor TfrmFeatureDetails.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetupObjects;

  SetGridColumnTitles(sgAims, [ResStr_Name, ResStr_AgreementDate]);
  SetGridColumnTitles(sgPotentialThreats, [ResStr_Type, ResStr_Threat]);
  SetGridColumnTitles(sgDamageOccurrences, [ResStr_Date, ResStr_Damage]);

  SwitchToDetails(sgAims, bbAimAdd, bbAimEdit, bbAimDel,
                  bbSave, bbCancel, gbAimDetails, false);
  SwitchToDetails(sgPotentialThreats, bbPotentialAdd, bbPotentialEdit, bbPotentialDel,
                  bbSave, bbCancel, gbPotentialDetails, false);
  SwitchToDetails(sgDamageOccurrences, bbDamageAdd, bbDamageEdit, bbDamageDel,
                  bbSave, bbCancel, gbDamageDetails, false);
  EnableDetails(Constants.emView);
  pcFeatureDetails.ActivePage:=tsGeneral;

  cmbFeatureType.Active := True;
  cmbThreatType.Active  := True;

  //Help Setup
  pcFeatureDetails.HelpContext   := IDH_FEATUREGENERAL;
  tsGeneral.HelpContext          := IDH_FEATUREGENERAL;
  tsAims.HelpContext             := IDH_FEATUREAIMS;
  tsPotentialThreats.HelpContext := IDH_FEATURETHREATS;
  tsDamages.HelpContext          := IDH_FEATUREDAMAGES;
  tsSources.HelpContext          := IDH_FEATURESOURCES;

  FKeyDown := false;
end;  // Create

//==============================================================================
procedure TfrmFeatureDetails.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Action:=caFree;
end;  // FormClose

//==============================================================================
procedure TfrmFeatureDetails.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  CanClose:=false;
  if EditMode<>emView then begin
    Beep;
    case ConfirmSaveAndClose of
      mrYes : begin
                if bbAimAccept.Enabled then bbAimAcceptClick(nil) else
                if bbPotentialAccept.Enabled then bbPotentialAcceptClick(nil) else
                if bbDamageAccept.Enabled then bbDamageAcceptClick(nil);
                bbSaveClick(nil);
                CanClose:=true;
              end;
      mrNo  : begin
                FClosingForm:=true;
                if bbAimDiscard.Enabled then bbAimDiscardClick(nil) else
                if bbPotentialDiscard.Enabled then bbPotentialDiscardClick(nil) else
                if bbDamageDiscard.Enabled then bbDamageDiscardClick(nil);
                bbCancelClick(nil);
                CanClose:=true;
                FClosingForm:=false;
              end;
    end;
  end else
    CanClose:=true;
end;  // FormCloseQuery

//==============================================================================
destructor TfrmFeatureDetails.Destroy;
begin
  FreeObjects;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TfrmFeatureDetails.SetupObjects;
begin
  // Data Module
  FdmFeatureDetails:=TdmFeatureDetails.Create(nil);

  FAimList:= TAimList.Create(FdmFeatureDetails.qryManagementAim, 'MANAGEMENT_AIM_KEY', sgAims, TAimItem);
  FThreatList:= TThreatList.Create(FdmFeatureDetails.qryThreats, 'POTENTIAL_THREAT_KEY', sgPotentialThreats, TThreatItem);
  FDamageList:= TDamageList.Create(FdmFeatureDetails.qryDamages, 'DAMAGE_OCCURRENCE_KEY', sgDamageOccurrences, TDamageItem);

  // Sources
  Sources.Init(dmDatabase.LocalDatabase, 'Location_Feature_Sources',
               AppSettings.UserAccessLevel, dmGeneralData.GetNextKey,
               RegisterDropComponent, AppSettings.SiteID, AppSettings.ExternalFilePath);
  Sources.OnFindInternal:=dmGeneralData.SourcesFindInternal;
  Sources.OnAddInternal :=dmGeneralData.SourcesAddInternal;
  Sources.OnShowSource :=dmGeneralData.SourcesShowInternal;
end;  // SetupObjects

//==============================================================================
procedure TfrmFeatureDetails.FreeObjects;
begin
  FdmFeatureDetails.Free;
  FdmFeatureDetails:=nil;
end;  // FreeObjects

//==============================================================================
procedure TfrmFeatureDetails.DisplayRecord(const AFeatDetKey:TKeyString);
var lCursor:TCursor;
begin
  lCursor:=HourglassCursor;
  FFeatureKey:=AFeatDetKey;
  try
    with FdmFeatureDetails do begin
      with qryFeature do begin
        Close;
        ResetDBRichEditControls([dbreComments]);
        Parameters.ParamByName('KeyParameter').Value:=AFeatDetKey;
        Open;
        eFeatureFrom.Text   := FieldByName('Date_From').AsString;
        eFeatureTo.Text     := FieldByName('Date_To').AsString;
        //Syncronize grading and type
        FCustodian := FieldByName('Custodian').AsString;
        qryTypeLocate.Parameters.ParamByName('Key').Value:= FieldByName('FEATURE_GRADING_KEY').AsString;
        qryTypeLocate.Open;
        cmbFeatureType.KeyValue:= qryTypeLocate.FieldByName('LOCATION_FEATURE_TYPE_KEY').AsString;
        cmbFeatureTypeChange(nil);
        qryTypeLocate.Close;
        lblFeature.Caption:=GetTextWithinLimit(Canvas,DuplicateCharacters(dbeFeatureName.Text+' - '+cmbFeatureType.Text, '&'),
                                               pnlDetails.Width-lblFeature.Left-8);
      end;
      //Management Aims
      qryManagementAim.Parameters.ParamByName('Key').Value:= AFeatDetKey;
      FAimList.FeatureKey:= AFeatDetKey;
      FAimList.Refresh;
      sgAimsClick(Self);

      //Potential Threats
      qryThreats.Parameters.ParamByName('Key').Value:= AFeatDetKey;
      FThreatList.FeatureKey:= AFeatDetKey;
      FThreatList.Refresh;
      sgPotentialThreatsClick(Self);

      //Damage Occurrences
      qryDamages.Parameters.ParamByName('Key').Value:= AFeatDetKey;
      FDamagelist.FeatureKey:= AFeatDetKey;
      FDamageList.Refresh;
      sgDamageOccurrencesClick(Self);
    end;
    // Sources
    Sources.SourcesFromKey:=AFeatDetKey;
    Sources.RefreshLists;
    // Additional Pages
    ChangeAdditionalPage(pcFeatureDetails);
    // Notify COM Addins
    NotifyDataItemChange;
  finally
    DefaultCursor(lCursor);
  end;
end;  // DisplayRecord

//==============================================================================
procedure TfrmFeatureDetails.AddRecord(const AFeatDetKey:TKeyString);
begin
  inherited;
  pcFeatureDetails.ActivePage:=tsGeneral;
  FeatureKey := AFeatDetKey;
  FCustodian := AppSettings.SiteID;
  with FdmFeatureDetails.qryFeature do begin
    Append;
    FieldByName('Feature_Grading_Key').AsString:=NONE_RECORD_KEY;
  end;
  ResetDBRichEditControls([dbreComments]);
  dmGeneralData.SetNameIDAndDate(FdmFeatureDetails.qryFeature,'Entered_By','Entry_Date');
  cmbFeatureType.KeyValue    :=NONE_RECORD_KEY;
  cmbFeatureTypeChange(nil);

  //Management Aims
  FdmFeatureDetails.qryManagementAim.Parameters.ParamByName('Key').Value:= AFeatDetKey;
  FAimList.FeatureKey:= AFeatDetKey;
  FAimList.Refresh;

  //Potential Threats
  FdmFeatureDetails.qryThreats.Parameters.ParamByName('Key').Value:= AFeatDetKey;
  FThreatList.FeatureKey:= AFeatDetKey;
  FThreatList.Refresh;

  //Damage Occurrences
  FdmFeatureDetails.qryDamages.Parameters.ParamByName('Key').Value:= AFeatDetKey;
  FDamagelist.FeatureKey:= AFeatDetKey;
  FDamageList.Refresh;
  // Sources
  Sources.SourcesFromKey:=AFeatDetKey;
  Sources.RefreshLists;
  { Add COM addin page }
  AddAdditionalPages;
  EnableDetails(Constants.emAdd);
end;  // AddRecord

//==============================================================================
function TfrmFeatureDetails.DeleteRecord(const AFeatDetKey:TKeyString): Boolean;
begin
  Result:= FdmFeatureDetails.DeleteRecord(AFeatDetKey);
end;  // DeleteRecord

//==============================================================================
procedure TfrmFeatureDetails.EditRecord;
begin
  DisplayRecord(FeatureKey);
  if dmGeneralData.HasFullEditAccess(TN_LOCATION_FEATURE, 'Location_Feature_Key', FeatureKey) and
      HaveCustody then
    try
      FdmFeatureDetails.qryFeature.Edit;
      dmGeneralData.SetNameIDAndDate(FdmFeatureDetails.qryFeature,'Changed_By','Changed_Date');
      EnableDetails(Constants.emEdit);
    except
      on E:Exception do
        if dmDatabase.CheckError(E, dbeRecordLocked) then begin
          MessageDlg(ResStr_CannotEditRecord + #13#13 +
                     dmDatabase.GetErrorMessage(E.Message, dbeRecordLocked),
                     mtInformation, [mbOk], 0);
          TfrmLocations(DrillForm).tvLocations.SetFocus;
          FEditMode := emView;
        end else
          Raise;
    end  // Try...Except
  else
    EnableDetails(Constants.emEdit);
end;  // bbEditClick

//==============================================================================
procedure TfrmFeatureDetails.bbSaveClick(Sender: TObject);
var lCurrentTab: TTabSheet;
    lCursor    : TCursor;
    lCustodian : String;
begin
  inherited;
  lCurrentTab:=pcFeatureDetails.ActivePAge;
  pcFeatureDetails.ActivePage:=tsGeneral;
  ValidateValue(dbeFeatureName.Text<>'',ResStr_FeatureMissing, dbeFeatureName);
  ValidateFeatureFromDate;
  ValidateFeatureToDate;

  pcFeatureDetails.ActivePage:=lCurrentTab;

  { Call to validate COM addin page... }
  if not CheckAdditionalPageCanSave then
    Exit;

  lCursor:=HourglassCursor;
  try
    lCustodian := dmGeneralData.Custodian(TN_LOCATION_FEATURE, 'Location_Feature_Key', FeatureKey);
    if ((AppSettings.UserAccessLevel > ualAddOnly) and (lCustodian = AppSettings.SiteID))
       or (EditMode = emAdd) then
    begin
      with FdmFeatureDetails.qryFeature do begin
        if State in [dsEdit, dsInsert] then begin
          if cmbFeatureType.Text <> '' then
            FieldByName('Feature_Grading_Key').AsString := dblcFeatureGrading.KeyValue
          else
            FieldByName('Feature_Grading_Key').AsString := NONE_RECORD_KEY;

          if EditMode = emAdd then begin
            FieldByName('Location_Feature_Key').AsString := FeatureKey;
            FieldByName('Location_Key').AsString         := ParentKey;
          end;
          FieldByName('DATE_FROM').Text:=eFeatureFrom.Text;
          FieldByName('DATE_TO').Text:=eFeatureTo.Text;
          Post;
        end;
      end;
    end else
    if (EditMode = Constants.emEdit) and (lCustodian <> FCustodian) then begin
      FdmFeatureDetails.qryFeature.Cancel;
      MessageDlg(Format(ResStr_CustodyChanged, ['Location Feature']), mtWarning, [mbOk], 0);
    end;
    // Save lists
    FAimList.Update;
    FThreatList.Update;
    FDamageList.Update;
    // Sources
    Sources.Post;
    // Additional Pages
    ChangeAdditionalPage(pcFeatureDetails);  // Required after add new feature.
    SaveAdditionalPages;

    if DrillForm<>nil then
      with TfrmLocations(DrillForm) do begin
        SetFeature(dbeFeatureName.Text+' - '+cmbFeatureType.Text,FeatureKey);
        tvLocations.Selected:=SelectedItem;
      end;
    EnableDetails(Constants.emView);
    DisplayRecord(FeatureKey);
  finally
    DefaultCursor(lCursor);
  end;
end;  // bbSaveClick

//==============================================================================
procedure TfrmFeatureDetails.bbCancelClick(Sender: TObject);
var tfDiscardNew:boolean;
begin
  inherited;
  tfDiscardNew:=FdmFeatureDetails.qryFeature.State=dsInsert;
  FdmFeatureDetails.qryFeature.Cancel;
  // Additional Pages
  CancelAdditionalPages;
  EnableDetails(Constants.emView);

  // If closing form, no need to do anything further. Otherwise, refresh screens.
  if not FClosingForm then
    if tfDiscardNew and (DrillForm<>nil) then
      PostMessage(TfrmLocations(DrillForm).Handle,WM_Discard_Location,0,0)
    else begin
      with TfrmLocations(DrillForm) do
        tvLocations.Selected:=SelectedItem;
      DisplayRecord(FeatureKey);
    end;
end;  // bbCancelClick

//==============================================================================
procedure TfrmFeatureDetails.EnableDetails(const NewMode:Constants.TEditMode);
var tfOn:boolean;
begin
  FEditMode:=NewMode;
  tfOn:=EditMode<>emView;

  SetRequiredFieldsColourState(tfOn,[dbeFeatureName]);
  cmbFeatureType.ReadOnly:=not (FdmFeatureDetails.qryFeature.State in [dsEdit,dsInsert]);

  // Management Aims
  bbAimAdd.Enabled :=tfOn;
  sgAimsClick(nil);
  // Potantial Threats
  bbPotentialAdd.Enabled :=tfOn;
  sgPotentialThreatsClick(nil);
  // Damage Occurrences
  bbDamageAdd.Enabled :=tfOn;
  sgDamageOccurrencesClick(nil);
  // RTF menus
  if tfOn then dbreComments.PopupMenu:=dmFormActions.pmRTF
          else dbreComments.PopupMenu:=nil;

  // Sources
  Sources.EditMode:=NewMode;

  bbSave.Enabled  :=tfOn;
  bbCancel.Enabled:=tfOn;

  //Additional pages
  SetAdditionalPagesState(tfOn);

  // Upate Main menu
  if DrillForm<>nil then TfrmLocations(DrillForm).SetMenuState(not tfOn);
end;  // EnableDetails


//==============================================================================
procedure TfrmFeatureDetails.sgAimsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  inherited;
  with sgAims do begin
    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
  end;
end;  // sgAimsDrawCell

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.sgAimsClick(Sender: TObject);
begin
  inherited;
  with sgAims do
    FCurrentAim:=TAimItem(Objects[0,Row]);

  if FCurrentAim=nil then begin
    bbAimEdit.Enabled:=false;
    bbAimDel.Enabled :=false;
    BlankAim;
  end else
    //Populate the details fields with information from the list
    with FCurrentAim do begin
      eAimName.Text     :=ItemName;
      eAimAuthority.Key :=Authority;
      eAimAuthority.Text:=dmGeneralData.GetName(Authority);

      //Description - rich edit
      Description.Position:= 0;
      reAimDescription.Lines.LoadFromStream(Description);

      eNextAppraisal.Text:= AppraisalDate;
      eAgreementDate.Text:= DateTimeToStr(AgreementDate);
      EnableDetailsAddEditButtons(TN_MANAGEMENT_AIM,
          'Management_Aim_Key', ItemKey,
          Custodian, EditMode, Added, bbAimEdit, bbAimDel, True);
    end;
end;  // sgAimsClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.BlankAim;
begin
  eAimName.Text      :='';
  eAimAuthority.Text :='';
  reAimDescription.Clear;
  eNextAppraisal.Text:='';
  eAgreementDate.Text:='';
end;  // BlankAim

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.SaveAim;
begin
  with FCurrentAim do begin
    ItemName:=eAimName.Text;
    Authority:=eAimAuthority.Key;

    //Description - rich edit
    Description.Position:= 0;
    reAimDescription.Lines.SaveToStream(Description);

    AppraisalDate:= eNextAppraisal.Text;
    if IsDate(eAgreementDate.Text) then
      AgreementDate:=StrToDateTime(eAgreementDate.Text)
    else
      AgreementDate:=0;
  end;
  sgAims.Refresh;
end;  // SaveAim

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.SetAimColour(const tfDetailsOn:boolean);
begin
  SetRequiredFieldsColourState(tfDetailsOn,[eAimName,eAimAuthority,eAgreementDate]);
end;  // SetAimColour

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbAimAddClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgAims, bbAimAdd, bbAimEdit, bbAimDel,
                  bbSave, bbCancel, gbAimDetails,true);
  SetAimColour(true);
  FCurrentAim:= TAimItem.CreateNew(FAimList);
  BlankAim;
  FAddItem:= True;
end;  // bbAimAddClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbAimEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgAims, bbAimAdd, bbAimEdit, bbAimDel,
                  bbSave, bbCancel, gbAimDetails,true);
  SetAimColour(true);
  FAddItem:= False;
end;  // bbAimEditClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbAimDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_ManagementAim, mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    FAimList.DeleteItem(sgAims.Row);
    sgAimsClick(nil);
  end;
end;  // bbAimDelClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbAimAcceptClick(Sender: TObject);
begin
  inherited;
  if Sender=nil then begin
    // Sender is nil if called from CloseQuery method.
    ValidateAppraisalDate;
    if eAgreementDate.Focused then ValidateAgreementDate;
  end;
  
  ValidateValue(eAimName.Text<>'',ResStr_NameRequired, eAimName);
  ValidateValue(eAimAuthority.Text<>'',ResStr_AuthorityRequired, eAimAuthority);
  ValidateValue(dmGeneralData.CheckName(eAimAuthority),ResStr_InvalidAuthority, eAimAuthority);
  if eNextAppraisal.Text<>'' then
    ValidateValue(StrToDate(eNextAppraisal.Text)>=Date,ResStr_DateInPast, eNextAppraisal);
  ValidateValue(eAgreementDate.Text<>'',ResStr_AgreementDateRequired, eAgreementDate);

  SaveAim;
  if FAddItem then FAimList.AddNew(FCurrentAim);
  SwitchToDetails(sgAims, bbAimAdd, bbAimEdit, bbAimDel,
                  bbSave, bbCancel, gbAimDetails, false);
  SetAimColour(false);
  sgAimsClick(nil);
end;  // bbAimAcceptClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbAimDiscardClick(Sender: TObject);
begin
  inherited;
  if FAddItem then FCurrentAim.Free;
  SwitchToDetails(sgAims, bbAimAdd, bbAimEdit, bbAimDel,
                  bbSave, bbCancel, gbAimDetails, false);
  SetAimColour(false);
  sgAimsClick(nil);
end;  // bbAimDiscard

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.UpdateAimAuthority(KeyList:TKeyList);
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then begin
      eAimAuthority.Key :=KeyList.Items[0].KeyField1;
      eAimAuthority.Text:=dmGeneralData.GetName(KeyList.Items[0].KeyField1);
    end;
  finally
    KeyList.Free;
  end;
end;  // UpdateAimAuthority

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.eNextAppraisalExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbAimDiscard.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbAimDiscard.Width]) and (lCancelPos.Y in [0..bbAimDiscard.Height]) then
    bbAimDiscardClick(nil)
  else if not FClosingForm then
    ValidateAppraisalDate;
end;  // eNextAppraisalExit

procedure TfrmFeatureDetails.ValidateAppraisalDate;
begin
  if eNextAppraisal.Text<>'' then begin
    try
      // Use VagueDate functions to deal with other date separators, like "."
      // If conversion fails, not a valid date and trapped after handler
      eNextAppraisal.Text := VagueDateToString(StringToVagueDate(eNextAppraisal.Text));
    except
    end;
    // And validate
    ValidateValue(IsDate(eNextAppraisal.Text),ResStr_EnterValidNextAppraisalDate,eNextAppraisal);
    eNextAppraisal.Text:=DateToStr(StrToDate(eNextAppraisal.Text));
  end;
end;  // eNextAppraisalExit

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.eAgreementDateExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbAimDiscard.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbAimDiscard.Width]) and (lCancelPos.Y in [0..bbAimDiscard.Height]) then
    bbAimDiscardClick(nil)
  else if not FClosingForm then
    ValidateAgreementDate;
end;  // eAgreementDateExit

procedure TfrmFeatureDetails.ValidateAgreementDate;
begin
  if eAgreementDate.Text<>'' then begin
    try
      // Use VagueDate functions to deal with other date separators, like "."
      // If conversion fails, not a valid date and trapped after handler
      eAgreementDate.Text := VagueDateToString(StringToVagueDate(eAgreementDate.Text));
    except
    end;
    // And validate
    ValidateValue(IsDate(eAgreementDate.Text) and (StrToDate(eAgreementDate.Text)<=Date),
                  InvalidDate(ResStr_TheAgreementDate,false,false),eAgreementDate);
    eAgreementDate.Text:=DateToStr(StrToDate(eAgreementDate.Text));
  end;
end;  // eAgreementDateExit

//==============================================================================
procedure TfrmFeatureDetails.sgPotentialThreatsDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  inherited;
  with sgPotentialThreats do begin
    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
  end;
end;  // sgPotentialThreatsDrawCell

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.sgPotentialThreatsClick(Sender: TObject);
begin
  inherited;
  with sgPotentialThreats do
    FCurrentThreat:=TThreatItem(Objects[0,Row]);
  if FCurrentThreat=nil then begin
    bbPotentialEdit.Enabled:=false;
    bbPotentialDel.Enabled :=false;
    BlankThreat;
  end else
    //Populate the details fields with information from the list
    with FCurrentThreat do begin
      cmbThreatType.KeyValue:=TypeKey;
      eThreat.Text:=Threat;

      //Comments - rich edit
      Comments.Position:= 0;
      reThreatComments.Lines.LoadFromStream(Comments);
      EnableDetailsAddEditButtons(TN_POTENTIAL_THREAT,
          'Potential_Threat_Key', ItemKey,
          Custodian, EditMode, Added, bbPotentialEdit, bbPotentialDel, True);
    end;
end;

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.BlankThreat;
begin
  cmbThreatType.ItemIndex:= -1;
  eThreat.Text:= '';
  reThreatComments.Lines.Clear;
end;  // BlankThreat

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.SaveThreat;
begin
  with FCurrentThreat do begin
    TypeKey :=cmbThreatType.KeyValue;
    TypeName:=cmbThreatType.Text;
    Threat  :=eThreat.Text;

    //Comments - rich edit
    Comments.Position:= 0;
    reThreatComments.Lines.SaveToStream(Comments);
  end;
  sgPotentialThreats.Refresh;
end;  // SaveThreat

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.SetThreatColour(const tfDetailsOn:boolean);
begin
  SetRequiredFieldsColourState(tfDetailsOn,[cmbThreatType,eThreat]);
end;  // SetThreatColour

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbPotentialAddClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgPotentialThreats, bbPotentialAdd, bbPotentialEdit, bbPotentialDel,
                  bbSave, bbCancel, gbPotentialDetails, true);
  SetThreatColour(true);
  BlankThreat;
  FAddItem:= True;
  FCurrentThreat:= TThreatItem.CreateNew(FThreatList);
end;  // bbPotentialAddClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbPotentialEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgPotentialThreats, bbPotentialAdd, bbPotentialEdit, bbPotentialDel,
                  bbSave, bbCancel, gbPotentialDetails, true);
  SetThreatColour(true);
  FAddItem:= False;
end;  // bbPotentialEditClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbPotentialDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeletePotentialThreat,
                mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    FThreatList.DeleteItem(sgPotentialThreats.Row);
    sgPotentialThreatsClick(nil);
  end;
end;  // bbPotentialDelClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbPotentialAcceptClick(Sender: TObject);
begin
  inherited;
  ValidateValue(cmbThreatType.Text<>'',ResStr_ThreatTypeMissing, cmbThreatType);
  ValidateValue(eThreat.Text<>'',ResStr_ThreatMissing, eThreat);

  SaveThreat;
  if FAddItem then FThreatList.AddNew(FCurrentThreat);
  SwitchToDetails(sgPotentialThreats, bbPotentialAdd, bbPotentialEdit, bbPotentialDel,
                  bbSave, bbCancel, gbPotentialDetails, false);
  SetThreatColour(false);
  sgPotentialThreatsClick(nil);
end;  // bbPotentialAcceptClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbPotentialDiscardClick(Sender: TObject);
begin
  inherited;
  if FAddItem then FCurrentThreat.Free;
  SwitchToDetails(sgPotentialThreats, bbPotentialAdd, bbPotentialEdit, bbPotentialDel,
                  bbSave, bbCancel, gbPotentialDetails, false);
  SetThreatColour(false);
  sgPotentialThreatsClick(nil);
end;  // bbPotentialDiscardClick

//==============================================================================
procedure TfrmFeatureDetails.sgDamageOccurrencesDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  inherited;
  with sgDamageOccurrences do begin
    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
  end;
end;  // sgDamageOccurrencesDrawCell

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.sgDamageOccurrencesClick(Sender: TObject);
begin
  inherited;
  with sgDamageOccurrences do
    FCurrentDamage:=TDamageItem(Objects[0,Row]);
  if FCurrentDamage=nil then begin
    bbDamageEdit.Enabled:=false;
    bbDamageDel.Enabled :=false;
    BlankDamage;
  end else
    //Populate the details fields with information from the list
    with FCurrentDamage do begin
      eOccurrenceDate.Text:= Date;
      //Comments - rich edit
      Comments.Position:= 0;
      reDamageComments.Lines.LoadFromStream(Comments);
      EnableDetailsAddEditButtons(TN_DAMAGE_OCCURRENCE,
          'Damage_Occurrence_Key', ItemKey,
          Custodian, EditMode, Added, bbDamageEdit, bbDamageDel, True);
    end;
end;  // sgDamageOccurrencesClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.BlankDamage;
begin
  eOccurrenceDate.Text:= '';
  reDamageComments.Lines.Clear;
end;  // BlankDamage

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.SaveDamage;
begin
  with FCurrentDamage do begin
    Date:= eOccurrenceDate.Text;
    //Comments - rich edit
    Comments.Position:= 0;
    reDamageComments.Lines.SaveToStream(Comments);
    CommentString:= dmGeneralData.ConvertRichTextToText(reDamageComments); 
  end;
  sgDamageOccurrences.Refresh;
end;  // SaveDamage

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.SetDamageColour(const tfDetailsOn:boolean);
begin
  SetRequiredFieldsColourState(tfDetailsOn,[eOccurrenceDate,reDamageComments]);
end;  // SetDamageColour

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbDamageAddClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgDamageOccurrences, bbDamageAdd, bbDamageEdit, bbDamageDel,
                  bbSave, bbCancel, gbDamageDetails, true);
  SetDamageColour(true);
  BlankDamage;
  FAddItem:= True;
  FCurrentDamage:= TDamageItem.CreateNew(FDamageList);
end;  // bbDamageAddClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbDamageEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgDamageOccurrences, bbDamageAdd, bbDamageEdit, bbDamageDel,
                  bbSave, bbCancel, gbDamageDetails, true);
  SetDamageColour(true);
  FAddItem:= False;
end;  // bbDamageEditClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbDamageDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteOccurence,
                mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    FDamageList.DeleteItem(sgDamageOccurrences.Row);
    sgDamageOccurrencesClick(nil);
  end;
end;  // bbDamageDelClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbDamageAcceptClick(Sender: TObject);
begin
  inherited;
  // Sender is nil if called from CloseQuery method.
  if Sender=nil then ValidateOccurrenceDate;

  ValidateValue(eOccurrenceDate.Text<>'',ResStr_DamageOccurenceDate, eOccurrenceDate);
  ValidateValue(reDamageComments.Text<>'',ResStr_DamageDescription, reDamageComments);

  SaveDamage;
  if FAddItem then FDamageList.AddNew(FCurrentDamage);
  SwitchToDetails(sgDamageOccurrences, bbDamageAdd, bbDamageEdit, bbDamageDel,
                  bbSave, bbCancel, gbDamageDetails, false);
  SetDamageColour(false);
  sgDamageOccurrencesClick(nil);
end;  // bbDamageAcceptClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.bbDamageDiscardClick(Sender: TObject);
begin
  inherited;
  if FAddItem then FCurrentDamage.Free;
  SwitchToDetails(sgDamageOccurrences, bbDamageAdd, bbDamageEdit, bbDamageDel,
                  bbSave, bbCancel, gbDamageDetails, false);
  SetDamageColour(false);
  sgDamageOccurrencesClick(nil);
end;  // bbDamageDiscardClick

//------------------------------------------------------------------------------
procedure TfrmFeatureDetails.eOccurrenceDateExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbDamageDiscard.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbDamageDiscard.Width]) and (lCancelPos.Y in [0..bbDamageDiscard.Height]) then
    bbDamageDiscardClick(nil)
  else if not FClosingForm then
    ValidateOccurrenceDate;
end;  // eOccurrenceDateExit

procedure TfrmFeatureDetails.ValidateOccurrenceDate;
begin
  if eOccurrenceDate.Text<>'' then begin
    ValidateValue(IsVagueDate(eOccurrenceDate.Text),
                  ResStr_InvalidVagueDate, eOccurrenceDate);
    ValidateValue(CheckVagueDate(eOccurrenceDate.Text),InvalidDate(ResStr_TheDate,true,false),eOccurrenceDate);
    eOccurrenceDate.Text:=VagueDateToString(eOccurrenceDate.VagueDate);
  end;
end;  // ValidateOccurrenceDate

//==============================================================================
procedure TfrmFeatureDetails.WMTransferDone(var Msg: TMessage);
begin
  if DrillForm<>nil then TfrmLocations(DrillForm).Show;
end;  // WMTransferDone

//==============================================================================
procedure TfrmFeatureDetails.SetDrillForm(const Value: TBaseForm);
begin
  FDrillForm := Value;
end;  // SetLocationForm

//==============================================================================
procedure TfrmFeatureDetails.pcFeatureDetailsChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  inherited;
  AllowChange:=not FEditDetails;
end;  // pcFeatureDetailsChanging

//==============================================================================
procedure TfrmFeatureDetails.cmbFeatureTypeChange(Sender: TObject);
begin
  inherited;
  //Update the grading combo box
  with FdmFeatureDetails do
    try
      qryGrading.Close;
      qryGrading.Parameters.ParamByName('Key').Value:=cmbFeatureType.KeyValue;
      qryGrading.Open;
      qryGrading.First;
      // Update grading as the combo stays empty if this is not done.
      if qryFeature.State in [dsInsert,dsEdit] then
        qryFeature.FieldByName('Feature_Grading_Key').AsString:=
                       qryGrading.FieldByName('Feature_Grading_Key').AsString;
    except
      on E:Exception do
        Raise EFeatureDetailsError.Create(ResStr_LocateFail + ' - LOCATION_FEATURE_GRADING', E);
    end;
end;  // cmbFeatureTypeChange

//==============================================================================
procedure TfrmFeatureDetails.SetupDestinationControls;
begin
  RegisterDropComponent(eAimAuthority, DropAimAuthority,
                        ['NAME','INDIVIDUAL', 'ORGANISATION'],
                        [CF_JNCCDATA, CF_TEXT]);
end;  // SetupDestinationControls

//==============================================================================
procedure TfrmFeatureDetails.DropAimAuthority(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: boolean);
begin
  if (EditMode <> emView) and FEditDetails then
    ioHandled := dmGeneralData.DropLinkedEditText(
        eAimAuthority,
        iFormat,
        iSourceData,
        dmGeneralData.GetName,
        iTextStrings)
  else
    ioHandled := True;
end;  // DropAimAuthority

//==============================================================================
procedure TfrmFeatureDetails.UpdateRTFMenu;
begin
  dmFormActions.UpdateRTFMenu(((DrillForm.ActiveControl is TDBRichEdit) and
                               (FdmFeatureDetails.qryFeature.State in [dsEdit,dsInsert])) or
                              (DrillForm.ActiveControl is TRichEdit));
end;  // UpdateRTFMenu

//==============================================================================
procedure TfrmFeatureDetails.EnterRTF(Sender: TObject);
begin
  inherited;
  if Sender is TRichEdit then
    dmFormActions.UpdateRTFMenu(not TRichEdit(Sender).ReadOnly)
  else if Sender is TDBRichEdit then
    dmFormActions.UpdateRTFMenu(FdmFeatureDetails.qryFeature.State in [dsEdit,dsInsert])
  else
    dmFormActions.UpdateRTFMenu(true);
end;  // EnterRTF

//==============================================================================
procedure TfrmFeatureDetails.ExitRTF(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(false);
end;  // ExitRTF

//==============================================================================
procedure TfrmFeatureDetails.pnlDetailsResize(Sender: TObject); 
var
  deltaWidth, deltaHeight : Integer;
begin
  inherited;
  lblFeature.Caption:=GetTextWithinLimit(Canvas,dbeFeatureName.Text+' - '+cmbFeatureType.Text,
                                         pnlDetails.Width-lblFeature.Left-8);
  // Works out the change in height and width of the panel.
  deltaWidth := pnlDetails.Width - pnlInner.Width;
  deltaHeight := pnlDetails.Height - pnlInner.Height;

  // For some reason, the automatic resizing doesn't work properly. Using a
  // second panel within the main panel and sizing it manually solves this, but
  // is a bit of a hack.
  pnlInner.Width := pnlDetails.Width;
  pnlInner.Height := pnlDetails.Height;

  // Custom controls do not have anchor properties and must be resized manually.
  eAimAuthority.Width := eAimAuthority.Width + deltaWidth;
  Sources.Width := Sources.Width + deltaWidth;
  Sources.Height := Sources.Height + deltaHeight;
end;  // pnlFeatureDetailsResize

//==============================================================================
function TfrmFeatureDetails.GetKeyList: TKeyList;
var lNewKeyList:TEditableKeyList;
begin
  //Return an editable key list with the selected nodes key
  lNewKeyList:= TEditableKeyList.Create;
  lNewKeyList.SetTable('LOCATION_FEATURE');
 	lNewKeyList.AddItem(FeatureKey,'');
  Result:= lNewKeyList;
end;  // GetKeyList

//==============================================================================
procedure TfrmFeatureDetails.RefreshLists;
begin
  cmbFeatureType.Active:=false;
  cmbFeatureType.Active:=true;
  FdmFeatureDetails.qryGrading.Refresh;
  cmbThreatType.Active:=false;
  cmbThreatType.Active:=true;
end;  // RefreshLists

//==============================================================================
procedure TfrmFeatureDetails.pcFeatureDetailsChange(Sender: TObject);
begin
  inherited;
  pcFeatureDetails.HelpContext := pcFeatureDetails.ActivePage.HelpContext;
end;

//==============================================================================
procedure TfrmFeatureDetails.RefreshColours;
begin
  SetRequiredFieldsColourState(EditMode<>emView,[dbeFeatureName]);
  SetRequiredFieldsColourState(bbAimAccept.Enabled,[eAimName,eAimAuthority,eAgreementDate]);
  SetRequiredFieldsColourState(bbPotentialAccept.Enabled,[cmbThreatType,eThreat]);
  SetRequiredFieldsColourState(bbDamageAccept.Enabled,[eOccurrenceDate,reDamageComments]);
end;

//==============================================================================
procedure TfrmFeatureDetails.RefreshNames;
var lIdx :integer;
    lItem:TAimItem;
begin
  // Refresh Aims
  if FCurrentAim<> nil then
    eAimAuthority.Text:=dmGeneralData.GetName(FCurrentAim.Authority);
  with FAimList do
    for lIdx:=0 to Count-1 do begin
      lItem:=TAimItem(Items[lIdx]);
      RefreshItemDisplay(lItem);
    end;
end;

//==============================================================================
procedure TfrmFeatureDetails.ShowMetaData;
begin
  with TdlgMetaDataPopup.Create(nil) do
  try
    ShowStandard(ResStr_Feature, lblFeature.Caption,
       FdmFeatureDetails.qryFeature.FieldByName('Location_Feature_Key').AsString,
       FdmFeatureDetails.qryFeature);
  finally
    Free;
  end;
end;

//==============================================================================
//These functions control whether BaseChild warns the user that the data
//in the combo boxes is not
//editable because the data is from another site.
procedure TfrmFeatureDetails.dbComboKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  FKeyDown :=true;
end;

procedure TfrmFeatureDetails.dbComboKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  FKeyDown :=false;
end;

procedure TfrmFeatureDetails.dbComboClick(Sender: TObject);
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
procedure TfrmFeatureDetails.eAimAuthorityFindData(Sender: TObject);
begin
  inherited;
  dmGeneralData.CheckName(eAimAuthority);
end;

{-------------------------------------------------------------------------------
  F9 return data link for Aim authority
}
procedure TfrmFeatureDetails.eAimAuthorityGetData(Sender: TObject);
begin
  inherited;
  dmFormActions.actNames.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateAimAuthority);
end;

procedure TfrmFeatureDetails.ValidateFeatureFromDate;
begin
   if eFeatureFrom.Text <> '' then begin
    // Use VagueDate functions to deal with other date separators, like "."
    try
      eFeatureFrom.Text := VagueDateToString(StringToVagueDate(eFeatureFrom.Text));
    except
    end;
    // And validate (NB: 1753-01-01 is the minimum value for SQL Server's
    // datetime datatype)
    ValidateValue(IsDate(eFeatureFrom.Text) and
        (StrToDate(eFeatureFrom.Text) >= EncodeDate(1753, 1, 1)) and
        (StrToDate(eFeatureFrom.Text) <= Date),
        InvalidDate(ResStr_StartDate,False,False),eFeatureFrom);
  end;
end;

procedure TfrmFeatureDetails.ValidateFeatureToDate;
begin
  if (eFeatureTo.Text <> '') then begin
    // Use VagueDate functions to deal with other date separators, like "."
    try
      eFeatureTo.Text := VagueDateToString(StringToVagueDate(eFeatureTo.Text));
    except
    end;
    // And validate
    ValidateValue(IsDate(eFeatureTo.Text) and (StrToDate(eFeatureTo.Text)<=Date),
                  InvalidDate(ResStr_EndDate,False,True),eFeatureTo);
    ValidateValue(IsDate(eFeatureTo.Text) and
        (StrToDate(eFeatureTo.Text) >= EncodeDate(1753, 1, 1)) and
        (StrToDate(eFeatureTo.Text) <= Date),
        InvalidDate(ResStr_EndDate,False,False),eFeatureTo);
    if (eFeatureFrom.Text <> '') then
      ValidateValue((StrToDate(eFeatureFrom.Text) <= StrToDate(eFeatureTo.Text)),
                     ResStr_EndBeforeStartDate,
                     eFeatureTo);
  end;
end;

procedure TfrmFeatureDetails.eFeatureFromExit(Sender: TObject);
var lPos:TPoint;
begin
  inherited;
  lPos := bbCancel.ScreenToClient(Mouse.CursorPos);
  if not (FClosingForm or
          ((lPos.X in [0..bbCancel.Width]) and (lPos.Y in [0..bbCancel.Height]))) then
    ValidateFeatureFromDate;
end;  // eFeatureFromExit

procedure TfrmFeatureDetails.eFeatureToExit(Sender: TObject);
var lPos:TPoint;
begin
  inherited;
  lPos := bbCancel.ScreenToClient(Mouse.CursorPos);
  if not (FClosingForm or
          ((lPos.X in [0..bbCancel.Width]) and (lPos.Y in [0..bbCancel.Height]))) then
    ValidateFeatureToDate;
end;  // eFeatureToDate


end.
