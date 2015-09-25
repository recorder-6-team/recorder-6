//==============================================================================
//  Unit:        BiotopeOccur
//
//  Implements:  TfrmBiotopeOccurrences
//
//  Description: This form is docked on the Observations screen to allow details
//               of a selected biotope occurrence to be viewed and/or edited.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 178 $
//    $Date: 8/04/10 16:49 $
//    $Author: Andrewkemp $
//
//==============================================================================

unit BiotopeOccur;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, BaseFormUnit, Menus, ComCtrls, Mask, Grids,
  VagueDateEdit, Sources, CompositeComponent, Measurements, Db, DBListCombo,
  DeterminationData, DataClasses, DBCtrls, BiotopeOccurData, DropStruct,
  DropSource, VagueDate, BaseDockedForm, ValidationData, OnlineHelp,
  HierarchyNodes, Constants, GeneralFunctions, ImageListButton, DatabaseAccessADO,
  BaseOccurrenceDetail, Variants, SpatialRefFuncs, AddinCompositeComponent,
  AddinLinkedControls;

type
  TfrmBiotopeOccurrences = class(TfrmBaseOccurrenceDetail)
    pnlDetails: TPanel;
    pnlInner: TPanel;
    lblBiotope: TLabel;
    pcBiotopeOccurrence: TPageControl;
    tsGeneral: TTabSheet;
    Bevel1: TBevel;
    Label5: TLabel;
    Label2: TLabel;
    lblVerificationStatus: TLabel;
    Label18: TLabel;
    dbreComments: TDBRichEdit;
    dbeSurveyorRef: TDBEdit;
    dbcbChecked: TDBCheckBox;
    dbcbDigitised: TDBCheckBox;
    cmbDeterminationType: TDBListCombo;
    tsDeterminations: TTabSheet;
    gbDetDetails: TGroupBox;
    Label1: TLabel;
    Label6: TLabel;
    Label3: TLabel;
    Shape1: TShape;
    Label9: TLabel;
    Label4: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Shape4: TShape;
    bbFindBiotope: TImageListButton;
    bbDetDiscard: TImageListButton;
    bbDetAccept: TImageListButton;
    reDetComments: TRichEdit;
    eBiotope: TEdit;
    eWork: TEdit;
    cbPreferred: TCheckBox;
    eDetDate: TVagueDateEdit;
    dbcmbRole: TDBListCombo;
    dbcmbType: TDBListCombo;
    bbFindRef: TImageListButton;
    eDeterminer: TNameLinkedEdit;
    tsMeasurements: TTabSheet;
    Measurements: TMeasurements;
    tsSources: TTabSheet;
    Sources: TSources;
    bbSave: TImageListButton;
    bbCancel: TImageListButton;
    pnlDetTop: TPanel;
    sgDeterminations: TStringGrid;
    bbDetAdd: TImageListButton;
    bbDetEdit: TImageListButton;
    bbDetDel: TImageListButton;
    splDeterminations: TSplitter;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbFindBiotopeClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure bbFindWorkClick(Sender: TObject);
    procedure bbDetAddClick(Sender: TObject);
    procedure bbDetEditClick(Sender: TObject);
    procedure bbDetDelClick(Sender: TObject);
    procedure eBiotopeKeyPress(Sender: TObject; var Key: Char);
    procedure eWorkKeyPress(Sender: TObject; var Key: Char);
    procedure sgDeterminationsDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgDeterminationsClick(Sender: TObject);
    procedure bbDetAcceptClick(Sender: TObject);
    procedure bbDetDiscardClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bbCancelClick(Sender: TObject);
    procedure pnlDetailsResize(Sender: TObject);
    procedure eDetDateExit(Sender: TObject);
    procedure pcBiotopeOccurrenceChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure pcBiotopeOccurrenceChange(Sender: TObject);
    procedure EnterRTF(Sender: TObject);
    procedure ExitRTF(Sender: TObject);
    procedure eWorkDblClick(Sender: TObject);
    procedure dbCheckBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cmbDeterminationTypeChange(Sender: TObject);
    procedure eDeterminerGetData(Sender: TObject);
    procedure eDeterminerFindData(Sender: TObject);
  private
    FdmBiotopeOcc     :TdmBiotopeOccurrences;
    FdmDetermination  :TdmDetermination;
    FDrillForm        :TBaseForm;
    FSampleKey        :TKeyString;
    FBiotopeOccKey    :TKeyString;

    FDeterminationList:TDeterminationList;
    FCurrentDet       :TDeterminationItem;
    FPreferredDet     :TDeterminationItem;
    FDetBiotopeKey    :TKeyString;
    FDetBioCode       :string;
    FDetWorkKey       :TKeyString;

    FAddItem     :boolean;
    FBiotopeDesc :string;
    FClosingForm :boolean;
    FVerified    :Byte;
    procedure EnableDetails(const NewMode:Constants.TEditMode);
    procedure SetDrillForm(const Value: TBaseForm);
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
    procedure BlankDetDetails;
    procedure SaveDetDetails;
    procedure UpdateBiotopeDescription;
    function CheckBiotope:boolean;
    function CheckWork:boolean;
    procedure FreeObjects;
    procedure SetupObjects;
    function GetDisplayDesc: string;
    procedure DropDetBiotopeName(const Sender: TObject;
      const iFormat: integer; const iSourceData: TKeyList;
      const iTextStrings: TStringList; var ioHandled: boolean);
    procedure DropDeterminerName(const Sender: TObject;
      const iFormat: integer; const iSourceData: TKeyList;
      const iTextStrings: TStringList; var ioHandled: boolean);
    procedure DropWorkData(const Sender: TObject; const iFormat: integer;
      const iSourceData: TKeyList; const iTextStrings: TStringList;
      var ioHandled: boolean);
    procedure UpdateBiotope(KeyList: TKeyList);
    procedure UpdateDeterminer(KeyList: TKeyList);
    procedure UpdateReference(KeyList: TKeyList);
    procedure SetDeterminationColour(const tfDetailsOn: boolean);
    procedure ValidateDeterminationDate;
    procedure SetVerified;
    function CheckValidation: Byte; 
    procedure SelectPreferredDetermination;
    procedure RefreshComboList(ACombo: TDBListCombo);
  protected
    procedure SetupDestinationControls; override;
  public
    constructor Create(AOwner:TComponent); override;
    function GetKeyList: TKeyList; override;
    procedure AddRecord(const ASampleKey:TKeyString);
    procedure EditRecord;
    procedure DisplayRecord(const AOccurrenceKey: TKeyString); override;
    procedure DeleteRecord(const ABiotopeOccKey: TKeyString);
    procedure ChangeChecked(const ANewState: boolean);
    procedure GetInternalSources(keyList: TEditableKeyList);
    procedure RefreshLists;
    procedure RefreshBiotopeName;
    procedure RefreshNames;
    procedure RefreshColours;
    procedure FindOnMap; override;
    procedure ShowMetadata; override;
    procedure UpdateRTFMenu; override;
    property DrillForm:TBaseForm read FDrillForm write SetDrillForm;
    property SampleKey:TKeyString read FSampleKey write FSampleKey;
    property BiotopeOccKey:TKeyString read FBiotopeOccKey write FBiotopeOccKey;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, GeneralData, FormActions, Maintbar, Observations, Find,
  DropTarget, MetadataPopup, ExceptionForm, Map, PlaceCardData;

resourcestring
  ResStr_DeterminationRequired = 'At least one Determination is required. Enter at least one determination.';
  ResStr_DeleteDetermination =  'Are you sure you want to delete this Determination?';
  ResStr_BiotopeNameRequired =  'A Biotope Name is required for every Determination.';
  ResStr_BiotopeNameInvalid = 'The Biotope Name is invalid. Enter a valid Biotope Name.';
  ResStr_DeterminerNameRequired = 'A Determiner Name is required for every Determination.';
  ResStr_DeterminerNameInvalid = 'The Determiner Name is invalid. Enter a valid name.';
  ResStr_RoleSpecified = 'Every determiner must have a role specified.';
  ResStr_TypeSelected = 'Every determiner must have a type selected.';
  ResStr_DeterminationDateRequired = 'A Determination Date is required for every Determination.';
  ResStr_WorkInvalid = 'The Work Document is invalid. Select a valid Work Document or leave blank.';
  ResStr_VagueDate = 'The vague date you have entered is not valid';
  ResStr_DeterminationDate = 'The Determination Date ';
  ResStr_NotVerified = 'Not verified';
  ResStr_FailedVerification = 'Failed/pending verification';
  ResStr_PassedVerification = 'Passed verification';
  ResStr_BiotopeOccurence = 'Biotope Occurrence';
  
//==============================================================================
constructor TfrmBiotopeOccurrences.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  SetupObjects;
  SetGridColumnTitles(
      sgDeterminations,
      ['', ResStr_Biotope, ResStr_Determiner, ResStr_Role, ResStr_Type, ResStr_Date]);

  SwitchToDetails(sgDeterminations,bbDetAdd,bbDetEdit,bbDetDel,
                  bbSave, bbCancel, gbDetDetails,false);
  EnableDetails(Constants.emView);
  pcBiotopeOccurrence.ActivePage:=tsGeneral;

  //Help Setup
  pcBiotopeOccurrence.HelpContext := IDH_BIOTOPEGENERAL;
  tsGeneral.HelpContext           := IDH_BIOTOPEGENERAL;
  tsMeasurements.HelpContext      := IDH_BIOTOPEMEASURES;
  tsDeterminations.HelpContext    := IDH_BIOTOPEDETS;
  tsSources.HelpContext           := IDH_BIOTOPESOURCES;
end;  // Create

//==============================================================================
procedure TfrmBiotopeOccurrences.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  FreeObjects;
  Action := caFree;
end;  // FormClose

//==============================================================================
procedure TfrmBiotopeOccurrences.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  CanClose:=false;
  if EditMode<>emView then begin
    Beep;
    case ConfirmSaveAndClose of
      mrYes : begin
                if bbDetAccept.Enabled then bbDetAcceptClick(nil);
                bbSaveClick(nil);
                CanClose:=true;
              end;
      mrNo  : begin
                FClosingForm:=true;
                if bbDetDiscard.Enabled then bbDetDiscardClick(nil);
                bbCancelClick(nil);
                CanClose:=true;
                FClosingForm:=false;
              end;
    end;
  end else
    CanClose:=true;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmBiotopeOccurrences.SetupObjects;
begin
  // Data Module
  FdmBiotopeOcc:=TdmBiotopeOccurrences.Create(nil);
  FdmBiotopeOcc.qryBiotopeOcc.Parameters.ParamByName('KeyParameter').Value:='';
  FdmDetermination:=TdmDetermination.Create(Self);
  FdmDetermination.qryTaxonDet.Parameters.ParamByName('KeyParameter').Value:='';
  // Setup Determination
  FDeterminationList:=TDeterminationList.Create(FdmDetermination.qryBiotopeDet,'Biotope_Determination_Key',
                                                sgDeterminations,TDeterminationItem);
  FDeterminationList.Taxon:=false;
  // Setup Measurement properties
  Measurements.Init(dmDatabase.LocalDatabase, TN_BIOTOPE_OCCURRENCE_DATA,
                    'Biotope_Occurrence_Data_Key', 'Biotope_Occurrence_key',
                    AppSettings.UserID, AppSettings.UserAccessLevel,
                    dmGeneralData.GetNextKey,
                    AppSettings.SiteID, AppSettings.RestrictFullEdit);
  // Sources
  Sources.Init(dmDatabase.LocalDatabase, TN_BIOTOPE_OCCURRENCE_SOURCES,
               AppSettings.UserAccessLevel, dmGeneralData.GetNextKey,
               RegisterDropComponent, AppSettings.SiteID, AppSettings.ExternalFilePath);
  Sources.OnFindInternal:=dmGeneralData.SourcesFindInternal;
  Sources.OnAddInternal :=dmGeneralData.SourcesAddInternal;
  Sources.OnShowSource :=dmGeneralData.SourcesShowInternal;
end;  // SetupObjects

//==============================================================================
procedure TfrmBiotopeOccurrences.FreeObjects;
begin
  FDeterminationList.Free;
  FDeterminationList := nil;
  FdmDetermination.Free;
  FdmDetermination := nil;
  FdmBiotopeOcc.Free;
  FdmBiotopeOcc := nil;
end;  // FreeObjects

//==============================================================================
function TfrmBiotopeOccurrences.GetDisplayDesc:string;
var stText,stClass:string;
begin
  stClass:=Copy(FBiotopeDesc,Pos('[',FBiotopeDesc),255);
  stText :=Copy(FBiotopeDesc,1,Pos('[',FBiotopeDesc)-1);
  stText :=GetTextWithinLimit(Canvas, DuplicateCharacters(stText, Char('&')),
                              pnlDetails.Width - lblBiotope.Left - Canvas.TextWidth(stClass) - 12);
  Result:=stText+stClass;
  lblBiotope.Width:=Canvas.TextWidth(Result);
end;  // GetDisplayDesc

//==============================================================================
procedure TfrmBiotopeOccurrences.UpdateBiotopeDescription;
var iIndex:integer;
    lKey  :TKeyString;
begin
  if FDeterminationList.ItemCount>0 then begin
    // Find the preferred determination and get the BLI key from it
    with sgDeterminations do
      for iIndex:=1 to RowCount-1 do
        if TDeterminationItem(Objects[0,iIndex]).Preferred then begin
          lKey:=TDeterminationItem(Objects[0,iIndex]).ItemNameKey;
          Break;
        end;
    // Use the BLI key to get the description
    FBiotopeDesc:=dmGeneralData.GetBiotopeCodeName(lKey)+
                  ' ['+FdmBiotopeOcc.GetClassification(lKey)+']';
    if FDetBioCode<>'' then
      FBiotopeDesc:=FDetBioCode+', '+FBiotopeDesc;
  end else
    FBiotopeDesc:='';
  lblBiotope.Caption:=GetDisplayDesc;
end;  // UpdateBiotopeDescription

//==============================================================================
procedure TfrmBiotopeOccurrences.DisplayRecord(const AOccurrenceKey: TKeyString);
var lCursor:TCursor;
 lDetTypeKey: string;
begin
  lCursor :=HourglassCursor;
  FBiotopeOccKey:=AOccurrenceKey;
  try
    with FdmBiotopeOcc do
      with qryBiotopeOcc do begin
        Close;
        ResetDBRichEditControls([dbreComments]);
        Parameters.ParamByName('KeyParameter').Value:=AOccurrenceKey;
        Open;
        SampleKey  := FieldByName('Sample_Key').AsString;
        FCustodian := FieldByName('Custodian').AsString;
        FCheckedState := dbcbChecked.Checked;
        FVerified     := FieldByName('Verified').AsInteger;
      end;   
    // Determination
    dbcmbRole.Active:=true;
    dbcmbType.Active:=true;
    FdmDetermination.qryBiotopeDet.Parameters.ParamByName('KeyParameter').Value:=AOccurrenceKey;
    FDeterminationList.OccKey:=AOccurrenceKey;
    FDeterminationList.Refresh;
    sgDeterminationsClick(Self);
    UpdateBiotopeDescription;

    if AOccurrenceKey <> '' then begin
      if pcBiotopeOccurrence.ActivePage = tsDeterminations then
        lDetTypeKey := FCurrentDet.DetTypeKey
      else
        lDetTypeKey := dmGeneralData.GetPreferredBiotopeDetType(AOccurrenceKey);
    end;
    FdmDetermination.qryDetType.Parameters.ParamByName('KeyParameter').Value := lDetTypeKey;
    RefreshLists;
    cmbDeterminationType.KeyValue := lDetTypeKey;
    // Measurements
    Measurements.KeyValue:=AOccurrenceKey;
    Measurements.Refresh;
    // Sources
    Sources.SourcesFromKey:=AOccurrenceKey;
    Sources.RefreshLists;
    // Additional Pages
    ChangeAdditionalPage(pcBiotopeOccurrence);
    // Notify COM Addins
    NotifyDataItemChange;
    cmbDeterminationType.Active := True;
  finally
    DefaultCursor(lCursor);
  end;
end;  // DisplayRecord

{-------------------------------------------------------------------------------
  By de-activating then re-activating a DB combo, its contents get re-populated
}
procedure TfrmBiotopeOccurrences.RefreshComboList(ACombo: TDBListCombo);
begin
  ACombo.Active := not ACombo.Active;
  ACombo.Active := not ACombo.Active;
end;

//==============================================================================
procedure TfrmBiotopeOccurrences.ChangeChecked(const ANewState: boolean);
begin
  DoCheckedStateChange('Biotope', BiotopeOccKey, ANewState);
end;  // ChangeChecked

//==============================================================================
procedure TfrmBiotopeOccurrences.AddRecord(const ASampleKey:TKeyString);
begin
  inherited;
  // Next Key
  SampleKey     := ASampleKey;
  eDetDate.Text := VagueDateToString(dmGeneralData.GetVagueDateFromRecordset(
      dmDatabase.GetRecordset('usp_SampleDetails_Get', ['@SampleKey', ASampleKey])));
  BiotopeOccKey := dmGeneralData.GetNextKey(TN_BIOTOPE_OCCURRENCE,'Biotope_Occurrence_Key');
  FCustodian    := AppSettings.SiteID;
  FVerified     := 0;  // not validated by default
  // Determinations      
  FdmDetermination.qryDetType.Parameters.ParamByName('KeyParameter').Value:='';
  FdmDetermination.qryDetRole.Parameters.ParamByName('KeyParameter').Value:='';
  RefreshLists;
  FDeterminationList.OccKey:=BiotopeOccKey;
  FDeterminationList.Refresh;
  sgDeterminationsClick(Self);
  // Measurements
  Measurements.KeyValue:=BiotopeOccKey;
  Measurements.Refresh;
  FdmBiotopeOcc.qryBiotopeOcc.Append;
  ResetDBRichEditControls([dbreComments]);
  dbcbChecked.Checked   := False;
  dbcbDigitised.Checked := False;
  dmGeneralData.SetNameIDAndDate(FdmBiotopeOcc.qryBiotopeOcc,'Entered_By','Entry_Date');

  FBiotopeDesc:='';
  lblBiotope.Caption:='';
  // Sources
  Sources.SourcesFromKey:=BiotopeOccKey;
  Sources.RefreshLists;
  { Add COM addin page }
  AddAdditionalPages;
  EnableDetails(Constants.emAdd);
  pcBiotopeOccurrence.ActivePage:=tsDeterminations;
  bbDetAddClick(nil);
  eBiotope.SetFocus;
end;  // AddRecord

//==============================================================================
procedure TfrmBiotopeOccurrences.DeleteRecord(const ABiotopeOccKey: TKeyString);
begin
  FdmBiotopeOcc.DeleteRecord(ABiotopeOccKey);
end;  // DeleteRecord

//==============================================================================
procedure TfrmBiotopeOccurrences.EditRecord;
var
  lDet: TDeterminationItem;
begin
  DisplayRecord(BiotopeOccKey);
  if dmGeneralData.HasFullEditAccess(TN_BIOTOPE_OCCURRENCE, 'Biotope_Occurrence_Key', BiotopeOccKey) and
     HaveCustody then
    try
      FdmBiotopeOcc.qryBiotopeOcc.Edit;
      dmGeneralData.SetNameIDAndDate(FdmBiotopeOcc.qryBiotopeOcc,'Changed_By','Changed_Date');
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
  if Assigned(FPreferredDet) and (pcBiotopeOccurrence.ActivePage = tsGeneral) then
    lDet := FPreferredDet
  else
    lDet := FCurrentDet;

  with FdmDetermination do begin
    qryDetType.Parameters.ParamByName('KeyParameter').Value := lDet.DetTypeKey;
    qryDetRole.Parameters.ParamByName('KeyParameter').Value := lDet.RoleKey;
    RefreshLists;
  end;
end;  // EditRecord

//==============================================================================
procedure TfrmBiotopeOccurrences.bbSaveClick(Sender: TObject);
var iCount     : integer;
    lDataItem  : TDeterminationItem;
    lCurrentTab: TTabSheet;
    lCursor    : TCursor;
    lCustodian : String;
begin
  inherited;
  lCurrentTab:=pcBiotopeOccurrence.ActivePage;
  // General
  pcBiotopeOccurrence.ActivePage:=tsGeneral;
  // Measurement
  pcBiotopeOccurrence.ActivePage:=tsMeasurements;
  ValidateValue(Measurements.CheckGrid, ResStr_MeasurementMissingOrInvalid, Measurements.Grid);
  // Determinations
  pcBiotopeOccurrence.ActivePage:=tsDeterminations;
  if bbDetAccept.Enabled then
    ValidateValue(FDeterminationList.ItemCount>0, ResStr_DeterminationRequired, eBiotope)
  else
    ValidateValue(FDeterminationList.ItemCount>0, ResStr_DeterminationRequired, bbDetAdd);
  pcBiotopeOccurrence.ActivePage:=lCurrentTab;

  { Call to validate COM addin page... }
  if not CheckAdditionalPageCanSave then Exit;

  lCursor:=HourglassCursor;
  try
    lCustodian := dmGeneralData.Custodian(TN_BIOTOPE_OCCURRENCE, 'Biotope_Occurrence_Key', BiotopeOccKey);
    if ((AppSettings.UserAccessLevel > ualAddOnly) and (lCustodian = AppSettings.SiteID))
       or (EditMode = emAdd) then
    begin
      with FdmBiotopeOcc.qryBiotopeOcc do begin
        if EditMode = emAdd then begin
          FieldByName('Biotope_Occurrence_Key').AsString:=BiotopeOccKey;
          FieldByName('Sample_Key').AsString            :=SampleKey;
        end;
        if State <> dsBrowse then begin
          FVerified:=CheckValidation;
          FieldByName('Verified').AsInteger:=FVerified;
          Post;
        end;
      end;  // with
    end else
    if (EditMode = Constants.emEdit) and (lCustodian <> FCustodian) then begin
      FdmBiotopeOcc.qryBiotopeOcc.Cancel;
      MessageDlg(Format(ResStr_CustodyChanged, ['Biotope Occurrence']), mtWarning, [mbOk], 0);
    end;
    // Determinations
    FDeterminationList.OccKey:=BiotopeOccKey;
    FDeterminationList.Update;
    sgDeterminationsClick(Self);
    UpdateBiotopeDescription;
    // Measurements
    Measurements.KeyValue:=BiotopeOccKey;
    Measurements.UpdateList;
    // Sources
    Sources.Post;
    // Additional Pages
    SaveAdditionalPages;

    if DrillForm<>nil then
      for iCount:=0 to FDeterminationList.Count-1 do begin
        lDataItem:=TDeterminationItem(FDeterminationList.Items[iCount]);
        if not lDataItem.Deleted and lDataItem.Preferred then begin
          with TfrmObservations(DrillForm) do begin
            if FCheckedState<>dbcbChecked.Checked then begin
              ChangeChecked(dbcbChecked.Checked);
              FCheckedState:=dbcbChecked.Checked;
              if FCheckedState then TBiotopeOccNode(SelectedItem.Data).StateImage:=STATE_CHECKED
                               else TBiotopeOccNode(SelectedItem.Data).StateImage:=STATE_UNCHECKED;
            end;
            SetItemTextAndKey(Copy(FBiotopeDesc,1,Pos('[',FBiotopeDesc)-1),BiotopeOccKey);
            tvObservations.Selected:=SelectedItem;
          end;
          Break;
        end;
      end;
    EnableDetails(Constants.emView);
    DisplayRecord(BiotopeOccKey);
  finally
    DefaultCursor(lCursor);
  end;
end;  // bbSaveClick

//==============================================================================
procedure TfrmBiotopeOccurrences.bbCancelClick(Sender: TObject);
var tfDiscardNew:boolean;
begin
  inherited;
  tfDiscardNew:=FdmBiotopeOcc.qryBiotopeOcc.State=dsInsert;
  FdmBiotopeOcc.qryBiotopeOcc.Cancel;
  // Additional Pages
  CancelAdditionalPages;
  EnableDetails(emView);

  // If closing form, no need to do anything further. Otherwise, refresh screens.
  if not FClosingForm then
    if tfDiscardNew and (DrillForm<>nil) then
      PostMessage(TfrmObservations(DrillForm).Handle,WM_DISCARD_OBSERVATION,0,0)
    else begin
      with TfrmObservations(DrillForm) do
        tvObservations.Selected:=SelectedItem;
      DisplayRecord(BiotopeOccKey);
    end;
end;  // bbCancelClick

//==============================================================================
procedure TfrmBiotopeOccurrences.EnableDetails(const NewMode:Constants.TEditMode);
var
  tfOn:boolean;
begin
  FEditMode:=NewMode;
  tfOn:=EditMode<>emView;
  cmbDeterminationType.ReadOnly := NewMode <> emAdd;

  // On Determination page
  bbDetAdd.Enabled :=tfOn;
  sgDeterminationsClick(nil);
  // Rethink readonly state depending on preferred det. (Still ok for Add mode though).
  SelectPreferredDetermination;
  if Assigned(FPreferredDet) then
    cmbDeterminationType.ReadOnly := (NewMode = emView);
  // On Measurements page
  Measurements.EditMode:=NewMode;
  // On Sources page
  Sources.EditMode:=NewMode;

  bbSave.Enabled  :=tfOn;
  bbCancel.Enabled:=tfOn;

  // Additional Pages
  SetAdditionalPagesState(tfOn);

  // Set RTF popup menu
  if tfOn then dbreComments.PopupMenu:=dmFormActions.pmRTF
          else dbreComments.PopupMenu:=nil;

  // Upate Main menu
  if DrillForm<>nil then TfrmObservations(DrillForm).SetMenuState(not tfOn);
end;  // EnableDetails


//==============================================================================
procedure TfrmBiotopeOccurrences.sgDeterminationsClick(Sender: TObject);
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
      RefreshLists;
      eBiotope.Text      :=ItemName;
      FDetBiotopeKey     :=ItemNameKey;
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
      lFullEditAccess := dmGeneralData.HasFullEditAccess(TN_BIOTOPE_DETERMINATION,
            'Biotope_Determination_Key', FCurrentDet.ItemKey);
      bbDetEdit.Enabled := AppSettings.AllowEdit(EditMode) and
        (Added or ((Custodian = AppSettings.SiteID)
        and lFullEditAccess));
      bbDetDel.Enabled  := AppSettings.AllowEdit(EditMode) and lFullEditAccess
        and (Added or (Custodian = AppSettings.SiteID));
    end;
  end;
  SetVerified;
end;  // sgDeterminationsClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.sgDeterminationsDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var xPos,yPos:integer;
    lDataItem:TDeterminationItem;
begin
  inherited;
  with sgDeterminations do begin
    if (ACol=0) and (ARow>0) then begin
      Canvas.FillRect(Rect);
      xPos:=Rect.Left+(ColWidths[0]-13) div 2;
      yPos:=Rect.Top+(RowHeights[Row]-13) div 2;
      lDataItem:=TDeterminationItem(Objects[0,ARow]);
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
procedure TfrmBiotopeOccurrences.BlankDetDetails;  
var
  i: Integer;
  lDataItem: TDeterminationItem;
begin     
  FdmDetermination.qryDetType.Parameters.ParamByName('KeyParameter').Value:='';
  FdmDetermination.qryDetRole.Parameters.ParamByName('KeyParameter').Value:='';
  RefreshLists;
  FDetBiotopeKey     :='';
  eBiotope.Text      :='';
  eDeterminer.Text   :='';  // reset to blank if the first determination
  eDeterminer.Key    :='';
  for i:=0 to FDeterminationList.Count-1 do begin
    lDataItem:=TDeterminationItem(FDeterminationList.Items[i]);
    if not lDataItem.Deleted and lDataItem.Preferred then begin
      FDetBiotopeKey := lDataItem.ItemNameKey;
      eBiotope.Text  := lDataItem.ItemName;
      // default to the person entering the determination
      eDeterminer.Text   :=dmGeneralData.GetName(AppSettings.UserID);
      eDeterminer.Key    :=AppSettings.UserID;
    end;
  end;
  FDetWorkKey         := '';
  cbPreferred.Checked := True;
  dbcmbRole.KeyValue  := ORIGINAL_DETERMINER_ROLE_KEY;
  dbcmbType.KeyValue  := ORIGINAL_DETERMINATION_TYPE_KEY;
  eWork.Text          := '';
  reDetComments.Clear;
end;  // BlankDeterminationDetails

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.SaveDetDetails;
var iCount:integer;
begin
  with FCurrentDet do begin
    ItemNameKey  :=FDetBiotopeKey;
    ItemName     :=eBiotope.Text;
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
    Preferred    :=cbPreferred.Checked;
    Verified     :=dmGeneralData.GetDetTypeVerified(DetTypeKey);
    Comment.Position:=0;
    reDetComments.Lines.SaveToStream(Comment);

    // If new preferred, uncheck the others
    if cbPreferred.Checked then begin
      cmbDeterminationType.KeyValue := DetTypeKey;
      for iCount:=0 to FDeterminationList.Count-1 do
        TDeterminationItem(FDeterminationList.Items[iCount]).Preferred:=false;
    end;
    Preferred:=cbPreferred.Checked or
               ((FDeterminationList.ItemCount=1) and not FAddItem) or
               ((FDeterminationList.ItemCount=0) and FAddItem);
    if Preferred then begin
      FBiotopeDesc:=dmGeneralData.GetBiotopeCodeName(FDetBiotopeKey)+
                    ' ['+FdmBiotopeOcc.GetClassification(ItemNameKey)+']';
      lblBiotope.Caption:=GetDisplayDesc;
    end;
  end;
  sgDeterminations.Refresh;
end;  // SaveDetDetails

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.SetDeterminationColour(const tfDetailsOn:boolean);
begin
  SetRequiredFieldsColourState(tfDetailsOn,[eBiotope,eDeterminer,dbcmbRole,dbcmbType,eDetDate]);
end;  // SetDeterminationColour

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.bbDetAddClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgDeterminations,bbDetAdd,bbDetEdit,bbDetDel,
                  bbSave, bbCancel, gbDetDetails,true);
  SetDeterminationColour(true);
  FCurrentDet:=TDeterminationItem.CreateNew(FDeterminationList);
  BlankDetDetails;
  FAddItem:=true;
end;  // bbDetAddClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.bbDetEditClick(Sender: TObject);
begin
  inherited;
  SwitchToDetails(sgDeterminations,bbDetAdd,bbDetEdit,bbDetDel,
                  bbSave, bbCancel, gbDetDetails,true);
  SetDeterminationColour(true);
  cbPreferred.Enabled:=not cbPreferred.Checked;
  FAddItem:=false;
end;  // bbDetEditClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.bbDetDelClick(Sender: TObject);
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
        sgDeterminationsClick(Self);
        if tfPref and (FDeterminationList.ItemCount>0) then begin
          // Set Preferred to first item in grid
          TDeterminationItem(Objects[0,1]).Preferred:=true;
          // Refresh grid with new preferred
          sgDeterminationsClick(Self);
        end;
      end;
    end;
    sgDeterminationsClick(nil);
  end;
end;  // bbDetDelClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.bbDetAcceptClick(Sender: TObject);
begin
  inherited;
  // Sender is nil if called from CloseQuery method.
  if Sender=nil then ValidateDeterminationDate;

  ValidateValue(eBiotope.Text<>'',ResStr_BiotopeNameRequired,eBiotope);
  ValidateValue(CheckBiotope,ResStr_BiotopeNameInvalid,eBiotope);
  ValidateValue(eDeterminer.Text<>'',ResStr_DeterminerNameRequired,eDeterminer);
  ValidateValue(dmGeneralData.CheckIndividual(eDeterminer),ResStr_DeterminerNameInvalid,eDeterminer);
  ValidateValue(dbcmbRole.Text <> '',ResStr_RoleSpecified,dbcmbRole);
  ValidateValue(dbcmbType.Text <> '',ResStr_TypeSelected,dbcmbType);
  ValidateValue(eDetDate.Text<>'',ResStr_DeterminationDateRequired,eDetDate);
  ValidateValue(dmValidation.CheckDeterminationDateAgainstSample(SampleKey, eDetDate.VagueDate),
                ResStr_DetDateAgainstSample, eDetDate);
  ValidateValue((eWork.Text='') or CheckWork,ResStr_WorkInvalid,eWork);

  SaveDetDetails;
  if FAddItem then FDeterminationList.AddNew(FCurrentDet);
  SwitchToDetails(sgDeterminations,bbDetAdd,bbDetEdit,bbDetDel,
                  bbSave, bbCancel, gbDetDetails,false);
  SetDeterminationColour(false);
  sgDeterminationsClick(nil);
  // Refresh det type if needed
  SelectPreferredDetermination;
  if Assigned(FPreferredDet) then
    cmbDeterminationType.KeyValue := FPreferredDet.DetTypeKey;
end;  // bbDetAcceptClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.bbDetDiscardClick(Sender: TObject);
begin
  inherited;
  if FAddItem then FCurrentDet.Free;
  SwitchToDetails(sgDeterminations,bbDetAdd,bbDetEdit,bbDetDel,
                  bbSave, bbCancel, gbDetDetails,false);
  SetDeterminationColour(false);
  sgDeterminationsClick(nil);
end;  // bbDetDiscardClick

//------------------------------------------------------------------------------
function TfrmBiotopeOccurrences.CheckBiotope:boolean;
var lFind :TdlgFind;
begin
  Result := true;
  lFind  := TdlgFind.CreateDialog(nil, ResStr_FindBiotopeTerm, ftBiotope);
  with lFind do
    try
      if FindUnique(eBiotope.Text) then begin
        FDetBiotopeKey := ItemKey;
        eBiotope.Text  := ItemText;
      end else if not eSearchText.NoSourceItems then begin
        if ShowModal=mrOk then begin
          FDetBiotopeKey := ItemKey;
          eBiotope.Text  := ItemText;
        end else
          Result := false;
      end else begin
        MessageDlg(ResStr_BiotopeItems, mtInformation, [mbOK], 0);
        Result := false;
      end;
    finally
      Release;
    end;
end;  // CheckBiotope

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.eBiotopeKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
  if Key=#13 then begin
    CheckBiotope;
    Key:=#0;
  end;
end;  // eBiotopeKeyPress

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.bbFindBiotopeClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actBiotopeDiction.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateBiotope);
end;  // bbFindBiotopeClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.UpdateBiotope(KeyList:TKeyList);
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then begin
      FDetBiotopeKey:=KeyList.Items[0].KeyField1;
      eBiotope.Text:=dmGeneralData.GetBiotopeCodeName(FDetBiotopeKey);
    end;
  finally
    KeyList.Free;
  end;
end;  // UpdateBiotope

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.UpdateDeterminer(KeyList:TKeyList);
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
procedure TfrmBiotopeOccurrences.eDetDateExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbDetDiscard.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbDetDiscard.Width]) and (lCancelPos.Y in [0..bbDetDiscard.Height]) then
    bbDetDiscardClick(nil)
  else if not FClosingForm then 
    ValidateDeterminationDate;
end;  // eDetDateExit

procedure TfrmBiotopeOccurrences.ValidateDeterminationDate;
begin
  if eDetDate.Text<>'' then begin
     ValidateValue(IsVagueDate(eDetDate.Text),
                  ResStr_VagueDate,eDetDate);
    ValidateValue(CheckVagueDate(eDetDate.Text),
                  InvalidDate(ResStr_DeterminationDate ,true,false),eDetDate);
    ValidateValue(dmValidation.CheckDeterminationDateAgainstSample(SampleKey, eDetDate.VagueDate),
                  ResStr_DetDateAgainstSample, eDetDate);
    eDetDate.Text:=VagueDateToString(eDetDate.VagueDate);
  end;
end;  // ValidateDeterminationDate

//------------------------------------------------------------------------------
function TfrmBiotopeOccurrences.CheckWork: Boolean;
var lFind: TdlgFind;
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
procedure TfrmBiotopeOccurrences.eWorkKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=#13 then begin
    CheckWork;
    Key:=#0;
  end;
end;  // eWorkKeyPress

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.bbFindWorkClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actDocuments.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild),Self,UpdateReference);
end;  // bbFindWorkClick

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.UpdateReference(KeyList:TKeyList);
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
procedure TfrmBiotopeOccurrences.SetDrillForm(const Value: TBaseForm);
begin
  FDrillForm := Value;
end;  // SetDrillForm

//==============================================================================
procedure TfrmBiotopeOccurrences.WMTransferDone(var Msg: TMessage);
begin
  if DrillForm<>nil then TfrmObservations(DrillForm).Show;
end;  // WMTransferDone

//==============================================================================
procedure TfrmBiotopeOccurrences.UpdateRTFMenu;
begin
  if Assigned(DrillForm) then
    dmFormActions.UpdateRTFMenu(((DrillForm.ActiveControl is TDBRichEdit) and
                               (FdmBiotopeOcc.qryBiotopeOcc.State in [dsEdit,dsInsert])) or
                              (DrillForm.ActiveControl is TRichEdit));
end;  // UpdateRTFMenu

//==============================================================================
procedure TfrmBiotopeOccurrences.EnterRTF(Sender: TObject);
begin
  inherited;
  if Sender is TRichEdit then
    dmFormActions.UpdateRTFMenu(not TRichEdit(Sender).ReadOnly)
  else if Sender is TDBRichEdit then
    dmFormActions.UpdateRTFMenu(FdmBiotopeOcc.qryBiotopeOcc.State in [dsEdit,dsInsert])
  else
    dmFormActions.UpdateRTFMenu(true);
end;  // EnterRTF

//==============================================================================
procedure TfrmBiotopeOccurrences.ExitRTF(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(false);
end;  // ExitRTF

//==============================================================================
procedure TfrmBiotopeOccurrences.SetupDestinationControls;
begin
  RegisterDropComponent(eBiotope, DropDetBiotopeName,
                        [TN_BIOTOPE_LIST_ITEM], [CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(eDeterminer, DropDeterminerName,
                        [TN_NAME,TN_INDIVIDUAL], [CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(eWork, DropWorkData,
                        [TN_REFERENCE], [CF_JNCCDATA, CF_TEXT]);
end;  // SetupDestinationControls

//==============================================================================
procedure TfrmBiotopeOccurrences.DropDetBiotopeName(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
begin
  if (EditMode <> emView) and FEditDetails then begin
    if iSourceData.Header.ItemCount>0 then
      if (iFormat = CF_JNCCDATA) then begin
        ioHandled:= true;
        FDetBiotopeKey:=iSourceData.Items[0].KeyField1;
        eBiotope.Text:=dmGeneralData.GetBiotopeCodeName(FDetBiotopeKey);
      end else
        ioHandled:=false;
  end else
    ioHandled:=true;
end;  // DropDetBiotopeName

//==============================================================================
procedure TfrmBiotopeOccurrences.DropDeterminerName(const Sender: TObject;
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
procedure TfrmBiotopeOccurrences.DropWorkData(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
begin
  if (EditMode <> emView) and FEditDetails then begin
    if iSourceData.Header.ItemCount>0 then
      if (iFormat = CF_JNCCDATA) then begin
        ioHandled := true;
        FDetWorkKey := iSourceData.Items[0].KeyField1;
        eWork.Text  := dmGeneralData.GetReferenceText(FDetWorkKey);
      end else
        ioHandled := false;
  end else
    ioHandled:=true;
end;  // DropWorkData

//==============================================================================
procedure TfrmBiotopeOccurrences.pnlDetailsResize(Sender: TObject);
var
  deltaWidth, deltaHeight : Integer;
begin
  inherited;
  lblBiotope.Caption:=GetDisplayDesc;

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
end;  // pnlBiotopeDetailsResize

//==============================================================================
function TfrmBiotopeOccurrences.GetKeyList: TKeyList;
var lNewKeyList:TEditableKeyList;
begin
  //Return an editable key list with the selected nodes key
  lNewKeyList:= TEditableKeyList.Create;
  lNewKeyList.SetTable(TN_BIOTOPE_OCCURRENCE);
 	lNewKeyList.AddItem(BiotopeOccKey,'');
  Result:= lNewKeyList;
end;  // GetKeyList

//==============================================================================
procedure TfrmBiotopeOccurrences.pcBiotopeOccurrenceChanging(
  Sender: TObject; var AllowChange: Boolean);
begin
  inherited;
  AllowChange:=not FEditDetails;
end;  // pcBiotopeOccurrenceChanging

//==============================================================================
procedure TfrmBiotopeOccurrences.RefreshLists;
begin
  RefreshComboList(cmbDeterminationType);
  RefreshComboList(dbcmbRole);
  RefreshComboList(dbcmbType);
  Measurements.RefreshLists;
end;  // RefreshLists

//==============================================================================
procedure TfrmBiotopeOccurrences.pcBiotopeOccurrenceChange(Sender: TObject);
var
  lDetTypeKey: String;
begin
  inherited; 
  SelectPreferredDetermination;
  pcBiotopeOccurrence.HelpContext := pcBiotopeOccurrence.ActivePage.HelpContext;
  if pcBiotopeOccurrence.ActivePage = tsGeneral then begin
    if Assigned(FPreferredDet) then
      lDetTypeKey := FPreferredDet.DetTypeKey
    else
      lDetTypeKey := dmGeneralData.GetPreferredBiotopeDetType(FBiotopeOccKey);
    FdmDetermination.qryDetType.Parameters.ParamByName('KeyParameter').Value := lDetTypeKey;
    RefreshLists;
    cmbDeterminationType.KeyValue := lDetTypeKey;
  end else if (pcBiotopeOccurrence.ActivePage = tsDeterminations) and Assigned(FCurrentDet) then begin
    sgDeterminationsClick(nil);
  end;
end;  // pcBiotopeOccurrenceChange

//==============================================================================
procedure TfrmBiotopeOccurrences.RefreshBiotopeName;
var
  liIndex: integer;
  lCurrItem : TDeterminationItem;
begin
  eBiotope.Text:=dmGeneralData.GetBiotopeCodeName(FDetBiotopeKey);
  with FDeterminationList do begin
    for liIndex := 0 to Count-1 do begin
      lCurrItem := TDeterminationItem(Items[liIndex]);
      lCurrItem.ItemName:=dmGeneralData.GetBiotopeCodeName(lCurrItem.ItemNameKey);
      RefreshItemDisplay(lCurrItem);
    end;
  end;
end;  // RefreshBiotopeName

//==============================================================================
procedure TfrmBiotopeOccurrences.RefreshNames;
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
end;  // RefreshNames

//==============================================================================
procedure TfrmBiotopeOccurrences.RefreshColours;
begin
  SetRequiredFieldsColourState((EditMode<>emView) and bbDetAccept.Enabled,
                               [eBiotope,eDeterminer,dbcmbRole,dbcmbType,eDetDate]);
end;  // RefreshColours

//==============================================================================
procedure TfrmBiotopeOccurrences.ShowMetadata;
begin
  with TdlgMetaDataPopup.Create(nil) do
  try
    ShowStandard(ResStr_BiotopeOccurence, lblBiotope.Caption,
       FdmBiotopeOcc.qryBiotopeOcc.FieldByName('Biotope_Occurrence_Key').AsString,
       FdmBiotopeOcc.qryBiotopeOcc);
  finally
    Free;
  end;
end;

//==============================================================================
{ Description : Show the map and scroll to centre this occurrence.  If the
              map is unavailable, then reset it. }
procedure TfrmBiotopeOccurrences.FindOnMap;
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
  TfrmMap.CentreOnPlace(lLocationKey, lblBiotope.Caption, lSpatialRef, lSRefSys);
end;


//==============================================================================
{ Description : Set the state of the validation label
  Creataed : 27/09/2002 }
procedure TfrmBiotopeOccurrences.SetVerified;
begin
  case FVerified  of
    0 : lblVerificationStatus.Caption := ResStr_NotVerified;
    1 : lblVerificationStatus.Caption := ResStr_FailedVerification;
    2 : lblVerificationStatus.Caption := ResStr_PassedVerification;
  end;
end;  // SetVerified

//==============================================================================
{ Description : Check the validation status according to the preferred
     determination. }
function TfrmBiotopeOccurrences.CheckValidation:byte;
//var lCount: Integer;
begin
  Result := 0;
  SelectPreferredDetermination;
  if Assigned(FPreferredDet) then
    Result := FPreferredDet.Verified;
end;  // CheckValidation 

//------------------------------------------------------------------------------
procedure TfrmBiotopeOccurrences.SelectPreferredDetermination;
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

{-------------------------------------------------------------------------------
}
procedure TfrmBiotopeOccurrences.eWorkDblClick(Sender: TObject);
begin
  inherited;
  if FDetWorkKey<>'' then
    dmGeneralData.SourcesShowInternal(Sender, FDetWorkKey);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmBiotopeOccurrences.dbCheckBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  //Pretend a key has been pressed.
begin
  PostMessage(TWinControl(Sender).Handle, WM_KEYDOWN, VK_Return,0);
end;


{-------------------------------------------------------------------------------
}
procedure TfrmBiotopeOccurrences.cmbDeterminationTypeChange(Sender: TObject);
var
  newDeterminationType : String;
begin
  inherited;
  SelectPreferredDetermination;
  newDeterminationType := cmbDeterminationType.Text;

  if (not Assigned(FPreferredDet)) or (newDeterminationType <> FPreferredDet.DetType) then
  begin 
    if Assigned(FPreferredDet) then
      eBiotope.Text := FPreferredDet.ItemName;
      
    pcBiotopeOccurrence.ActivePage := tsDeterminations;
    bbDetAddClick(Sender); //Mimics the add button click, creating a new record.

    eDeterminer.Text := AppSettings.UserName;
    eDetDate.Text := DateToStr(Date);
    cbPreferred.Checked := True;
    dbcmbRole.KeyValue := DETERMINATION_CONFIRMATION_ROLE_KEY;
    dbcmbType.ItemIndex := dbcmbType.Items.IndexOf(newDeterminationType);
  end;
end;

{-------------------------------------------------------------------------------
  Setup F9 return data link for Determiner
}
procedure TfrmBiotopeOccurrences.eDeterminerGetData(Sender: TObject);
begin
  inherited;
  dmFormActions.actNames.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateDeterminer);
end;

{-------------------------------------------------------------------------------
  Set up Find dialog for the Determiner box
}
procedure TfrmBiotopeOccurrences.eDeterminerFindData(Sender: TObject);
begin
  inherited;
  dmGeneralData.CheckIndividual(eDeterminer);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmBiotopeOccurrences.GetInternalSources(keyList: TEditableKeyList);
var
  i: Integer;
begin
  Sources.GetInternalSources(keyList);
  with sgDeterminations do
    for i := FixedRows to RowCount - 1 do
      if TDeterminationItem(Objects[0, i]).WorkKey <> '' then
        keyList.AddItem(TDeterminationItem(Objects[0, i]).WorkKey, TN_SOURCE);
end;

end.
