//==============================================================================
//  Unit:        SampleDetails
//
//  Implements:  TfrmSampleDetails
//
//  Description: This form is docked on the Observations screen to allow details
//               of a selected sample to be viewed and/or edited.
//
//  Author:      Paul Thomas
//  Created:     10 Nov 1999
//
//  Last Revision Details:
//    $Revision: 133 $
//    $Date: 8/04/10 17:20 $
//    $Author: Andrewkemp $
//
//==============================================================================

{$I '..\..\Third Party\Dorset Software Services\DssVcl32\DelphiVersions.Inc'}

unit SampleDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  BaseFormUnit, Grids, StdCtrls, ExtCtrls, ActnList, Menus, Buttons, ComCtrls,
  CheckLst, CompositeComponent, Measurements, Sources, SampleData, DBGlyphCtrls,
  DataClasses, DBCtrls, DropTarget, VagueDate, SpatialRef, Mask, BaseDockedForm,
  ExceptionForm, ValidationData, OnlineHelp, Constants, GeneralFunctions,
  ImageListButton, DatabaseAccessADO, {$IFDEF DELPHI7UP} Variants, {$ENDIF}
  VagueDateEdit, LocationInfoFrame, RapTree, ADODB, ResourceStrings,
  Cascade;

type
  ESampleDetails = class(TExceptionPath);
  TfrmSampleDetails = class(TfrmBaseDockedForm)
    pmMapWindow: TPopupMenu;
    pnlDetails: TPanel;
    pnlInner: TPanel;
    lblSample: TLabel;
    bbCancel: TImageListButton;
    bbSave: TImageListButton;
    pcSampleDetails: TPageControl;
    tsGeneral: TTabSheet;
    Bevel1: TBevel;
    lblSampleRefDisp: TLabel;
    lblSampleType: TLabel;
    lblSampleDate: TLabel;
    lblStartTime: TLabel;
    lblDuration: TLabel;
    lblComment: TLabel;
    dbeSampleRef: TDBEdit;
    dbcmbSampleType: TDBGlyphLookupComboBox;
    dbreComments: TDBRichEdit;
    dbeDuration: TDBEdit;
    eStartTime: TEdit;
    eSampleDate: TVagueDateEdit;
    fraLocationInfo: TfraLocationInfo;
    tsSampleRecorder: TTabSheet;
    Bevel2: TBevel;
    lblRecorders: TLabel;
    clbRecorders: TCheckListBox;
    tsMeasurements: TTabSheet;
    Measurements: TMeasurements;
    tsRelations: TTabSheet;
    Bevel3: TBevel;
    Label16: TLabel;
    shpRelatedSamples: TShape;
    bbSampleAdd: TImageListButton;
    bbSampleDel: TImageListButton;
    sgRelatedSamples: TStringGrid;
    tsSources: TTabSheet;
    Sources: TSources;
    tsAdminAreas: TTabSheet;
    sgAdminAreas: TStringGrid;
    Shape3: TShape;
    bbAdminAdd: TImageListButton;
    bbAdminDel: TImageListButton;
    tsPrivate: TTabSheet;
    Label8: TLabel;
    Label1: TLabel;
    ePrivateLocation: TEdit;
    ePrivateCode: TEdit;
    Bevel4: TBevel;
    Label2: TLabel;
    Label3: TLabel;
    eUnparsed: TEdit;
    Label4: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbSaveClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure bbSampleAddClick(Sender: TObject);
    procedure bbSampleDelClick(Sender: TObject);
    procedure EnterRTF(Sender: TObject);
    procedure ExitRTF(Sender: TObject);
    procedure clbRecordersClickCheck(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pnlDetailsResize(Sender: TObject);
    procedure eStartTimeExit(Sender: TObject);
    procedure pcSampleDetailsChange(Sender: TObject);
    procedure sgRelatedSamplesClick(Sender: TObject);
    procedure dbComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure dbComboKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure dbComboClick(Sender: TObject);
    procedure SpatialRefInvalidSpatialRef(Sender: TObject; var Handled: Boolean);
    procedure eSampleDateExit(Sender: TObject);
    procedure sgAdminAreasClick(Sender: TObject);
    procedure bbAdminAddClick(Sender: TObject);
    procedure bbAdminDelClick(Sender: TObject);
    procedure fraLocationInfoeLocationNameChange(Sender: TObject);
    procedure fraLocationInfoeLocationChange(Sender: TObject);
  private
    FDrillForm:TBaseForm;
    FdmSample :TdmSample;
    FEventKey: TKeyString;
    FSampleKey:TKeyString;
    FRelatedSampleList:TRelatedSampleList;
    FClosingForm:boolean;
    FKeyDown: Boolean;
    FSampleAdminAreaList: TSampleAdminAreaList;
    procedure EnableDetails(const NewMode:TEditMode);
    procedure SetDrillForm(const Value: TBaseForm);
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
    procedure SetupObjects;
    procedure FreeObjects;
    procedure DropRelatedSample(const Sender: TObject;
      const iFormat: integer; const iSourceData: TKeyList;
      const iTextStrings: TStringList; var ioHandled: boolean);
    function CheckRelatedSample(const AKey: TKeyString): boolean;
    procedure ValidateStartTime;
    procedure ValidateSampleDetailInput;
    procedure SetTitleLabel;
    procedure UpdateAdminArea(KeyList:TKeyList);
    procedure DropAdminArea(const Sender: TObject; const iFormat : Integer; const iSourceData: TKeyList;
        const iTextStrings : TStringList; var ioHandled : Boolean);
    function AdminAreaCheckExistence(const AKey:TKeyString):Boolean;
  protected
    procedure SetupDestinationControls; override;
  public
    constructor Create(AOwner:TComponent); override;
    function GetKeyList: TKeyList; override;
    procedure UpdateRTFMenu; override;
    procedure AddRecord(const AEventKey:TKeyString);
    procedure EditRecord;
    procedure DisplayRecord(const ASampleKey: TKeyString);
    procedure DeleteRecord(const ASampleKey: TKeyString);
    procedure RefreshLists;
    procedure RefreshColours;
    procedure SetDisplaySystem;
    procedure RefreshSampleLocation;
    procedure RefreshSpatialRef;
    procedure RefreshNames;
    procedure ShowMetadata; override;
    procedure FindOnMap; override;
    procedure UpdateMapWindowSelector; override;
    property DrillForm:TBaseForm read FDrillForm write SetDrillForm;
    property EventKey:TKeyString read FEventKey write FEventKey;
    property SampleKey:TKeyString read FSampleKey write FSampleKey;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, Maintbar, FormActions, Observations, TaxonOccur,
  BiotopeOccur, Find, GeneralData, SpatialRefFuncs, MetadataPopup, Map,
  BaseChildUnit, HierarchyNodes, ADOInt;

resourcestring
  ResStr_ErrorInSampleDetails = 'Error occurred in Sample Details constructor';
  ResStr_SampleListSetup =  'Related sample list setup';
  ResStr_ErrorInSampleDetailsPhase =  'Error occurred in Sample Details SetupObjects phase ';
  ResStr_Measurements = 'measurements';
  ResStr_Sources =  'sources';
  ResStr_NoValidSampleKey = 'No valid sample key can be obtained. Unable to proceed.';
  ResStr_SampleDateRequired = 'A Sample Date is required for every Sample.';
  ResStr_SampleTypeRequired = 'A Sample Type is required for every Sample. Select one from the list.';
  ResStr_NoRecorderSelected = 'No Recorder selected. At least one Recorder must be selected.';
  ResStr_CannotLocateRow =  'Row cannot be located for updating';
  ResStr_DeleteRelatedSample =  'Are you sure you want to delete this Related Sample?';
  ResStr_InvalidTime =  'The Time is not valid. Please enter a '+
                        'valid time or leave blank.';
  ResStr_InvalidVagueDate = 'The vague date you have entered is not valid';
  ResStr_DataModuleSetp = 'Data module setup';
  ResStr_OutsideSurveyBoundingBox = 'The spatial reference for the sample falls outside the bounding '+
      'box defined for the survey.';
  ResStr_OutsideSurveyDateRange = 'The date for the sample falls outside the range of allowed dates '+
      'for the survey.';
  ResStr_DeleteAdmin = 'Are you sure you want to delete this Administrative Area?';
  ResStr_SurveyMustBeTemp = 'Unparsed recorders may only be used on temporary surveys';
//==============================================================================
function IsTime(const ATime:string):boolean;
begin
  Result:=true;
  try
    StrToTime(ATime);
  except
    on EConvertError do Result:=false;
  end;
end;  // IsTime

//==============================================================================
constructor TfrmSampleDetails.Create(AOwner:TComponent);
begin
  try
    inherited Create(AOwner);
    DrillForm:=nil;
    SetGridColumnTitles(sgRelatedSamples, [ResStr_SampleRef, ResStr_Date, ResStr_Type]);
    SetupObjects;
    SetDisplaySystem;
    EnableDetails(emView);
    pcSampleDetails.ActivePage:=tsGeneral;
  except
    on E:Exception do
      raise ESampleDetails.Create(ResStr_ErrorInSampleDetails, E);
  end;

  SetGridColumnTitles(sgAdminAreas,   [ResStr_Name, ResStr_Type]);

  //Help Setup
  pcSampleDetails.HelpContext := IDH_SAMPLEGENERAL;
  tsGeneral.HelpContext       := IDH_SAMPLEGENERAL;
  tsSampleRecorder.HelpContext:= IDH_SAMPLERECORDER;
  tsMeasurements.HelpContext  := IDH_SAMPLEMEASURES;
  tsRelations.HelpContext     := IDH_SAMPLERELATIONS;
  tsSources.HelpContext       := IDH_SAMPLESOURCES;
end;  // Create

//==============================================================================
procedure TfrmSampleDetails.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FreeObjects;
  Action := caFree;
end;  // FormClose

//==============================================================================
procedure TfrmSampleDetails.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  CanClose:=false;
  if EditMode<>emView then begin
    Beep;
    case ConfirmSaveAndClose of
      mrYes : begin
                bbSaveClick(nil);
                CanClose:=true;
              end;
      mrNo  : begin
                FClosingForm:=true;
                bbCancelClick(nil);
                CanClose:=true;
                FClosingForm:=false;
              end;
    end;
  end else
    CanClose:=true;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmSampleDetails.SetupObjects;
var
  lPhase : string; // for advanced error tracking
begin
  lPhase := ResStr_DataModuleSetp;
  try
    // Data Module
    FdmSample:=TdmSample.Create(nil);
    FdmSample.qryRelatedSample.Parameters.ParamByName('KeyParameter').Value:='';
    // Related Samples
    lPhase := ResStr_SampleListSetup;
    FRelatedSampleList:=TRelatedSampleList.Create(
                                   FdmSample.qryRelatedSample,'Sample_Key_2',
                                   sgRelatedSamples,TRelatedSampleItem,FdmSample);
    FRelatedSampleList.SampleKey:='';
    // Setup Measurement properties
    lPhase := ResStr_Measurements;
    Measurements.Init(dmDatabase.LocalDatabase, 'Sample_Data',
                      'Sample_Data_Key', 'Sample_Key',
                      AppSettings.UserID, AppSettings.UserAccessLevel,
                      dmGeneralData.GetNextKey,
                      AppSettings.SiteID, AppSettings.RestrictFullEdit);
    // Sources
    lPhase := ResStr_Sources;
    Sources.Init(dmDatabase.LocalDatabase, 'Sample_Sources',
                 AppSettings.UserAccessLevel, dmGeneralData.GetNextKey,
                 RegisterDropComponent, AppSettings.SiteID, AppSettings.ExternalFilePath);
    Sources.OnFindInternal:=dmGeneralData.SourcesFindInternal;
    Sources.OnAddInternal :=dmGeneralData.SourcesAddInternal;
    Sources.OnShowSource :=dmGeneralData.SourcesShowInternal;

    FSampleAdminAreaList := TSampleAdminAreaList.Create(FdmSample.qryAdminAreas, 'ADMIN_AREA_KEY',
                                            sgAdminAreas, TSampleAdminAreaItem);
  except
    on E:Exception do
      raise ESampleDetails.Create(ResStr_ErrorInSampleDetailsPhase+ lPhase, E);
  end;
end;  // SetupObjects

//==============================================================================
procedure TfrmSampleDetails.FreeObjects;
var iCount:integer;
begin
  // Objects in Recorders
  with clbRecorders.Items do begin
    for iCount:=0 to Count-1 do begin
      TKeyData(Objects[iCount]).Free;
      Objects[iCount]:=nil;
    end;
    Clear;
  end;

  // Related Samples
  FRelatedSampleList.Free;
  FRelatedSampleList := nil;
  // Data Module
  FdmSample.Free;
  FdmSample:=nil;
end;  // FreeObjects

//==============================================================================
procedure TfrmSampleDetails.DisplayRecord(const ASampleKey: TKeyString);
var
  lCursor: TCursor;
begin
  lCursor    := HourglassCursor;
  FSampleKey := ASampleKey;
  try
    with FdmSample do begin
      with qrySample do begin
        Close;
        ResetDBRichEditControls([dbreComments]);
        Parameters.ParamByName('KeyParameter').Value := ASampleKey;
        Open;
        FCustodian := FieldByName('Custodian').AsString;
        EventKey   := FieldByName('Survey_Event_Key').AsString;
        fraLocationInfo.eLocation.Key := FieldByName('Location_Key').AsString;
        fraLocationInfo.eSpatialRef.Qualifier     := FieldByName('Spatial_Ref_Qualifier').AsString;
        fraLocationInfo.eSpatialRef.EnteredRef    := FieldByName('Spatial_Ref').AsString;
        fraLocationInfo.eSpatialRef.EnteredSystem := FieldByName('Spatial_Ref_System').AsString;
        fraLocationInfo.eSpatialRef.DisplayRef    := GetDisplaySpatialRef(
            AppSettings.SpatialRefSystem,
            FieldByName('Spatial_Ref').AsString,
            FieldByName('Spatial_Ref_System').AsString,
            FieldByName('Lat').Value,
            FieldByName('Long').Value,
            '');

        eSampleDate.Text := FieldByName('Vague_Date_Start').Text;

        if FieldByName('Time').IsNull then
          eStartTime.Text := ''
        else
          eStartTime.Text := FormatDateTime('hh:mm', FieldByName('Time').AsDateTime);
        fraLocationInfo.eLocation.Text := dmGeneralData.GetLocationName(fraLocationInfo.eLocation.Key);
        fraLocationInfo.eLocationName.Text := FieldByName('Location_Name').AsString;
        //Michael Weideli  Mantis 450 Mantis 451
        ePrivateLocation.Text := FieldByName('Private_Location').AsString;
        ePrivateCode.Text := FieldByName('Private_Code').AsString;
        eUnparsed.Text := FieldByName('Recorders').AsString;
     
      end;
      // Deal with Recorders
      RefreshRecorders(ASampleKey, qrySample.FieldByName('Survey_Event_Key').AsString, clbRecorders);
      // Prepare query for Related Samples
      qryRelatedSample.Parameters.ParamByName('KeyParameter').Value := ASampleKey;
      // Sample key value for the admin areas query.
      qryAdminAreas.Parameters.ParamByName('Key').Value:= ASampleKey;
      FSampleAdminAreaList.SampleKey:= ASampleKey;
      FSampleAdminAreaList.Refresh;
    end;
    SetTitleLabel;
    // Deal with related samples
    FRelatedSampleList.SampleKey := ASampleKey;
    FRelatedSampleList.Refresh;
    // Measurements
    Measurements.KeyValue := ASampleKey;
    Measurements.Refresh;
    // Sources
    Sources.SourcesFromKey := ASampleKey;
    Sources.RefreshLists;

    // Additional Pages
    ChangeAdditionalPage(pcSampleDetails);
    // Notify COM Addins
    NotifyDataItemChange;
  finally
    DefaultCursor(lCursor);
  end;
end;  // DisplayRecord

//==============================================================================
procedure TfrmSampleDetails.AddRecord(const AEventKey: TKeyString);
begin
  inherited;
  pcSampleDetails.ActivePage := tsGeneral;
  // Get next sequential key
  EventKey := AEventKey;
  fraLocationInfo.eSpatialRef.Clear;
  SampleKey  := dmGeneralData.GetNextKey('Sample', 'Sample_Key');
  FCustodian := AppSettings.SiteID;
  with FdmSample do
  begin
    with qryEvent do
    begin
      Parameters.ParamByName('Key').Value:=EventKey;
      Open;
      fraLocationInfo.eSpatialRef.Qualifier := FieldByName('Spatial_Ref_Qualifier').AsString;
      fraLocationInfo.eSpatialRef.EnteredRef := FieldByName('Spatial_Ref').AsString;
      fraLocationInfo.eSpatialRef.EnteredSystem := FieldByName('Spatial_Ref_System').AsString;
      fraLocationInfo.eSpatialRef.DisplayRef := GetDisplaySpatialRef(
          AppSettings.SpatialRefSystem,
          FieldByName('Spatial_Ref').AsString,
          FieldByName('Spatial_Ref_System').AsString,
          FieldByName('Lat').Value,
          FieldByName('Long').Value,
          '');
      fraLocationInfo.eLocation.Key  :=FieldByName('Location_Key').AsString;
      fraLocationInfo.eLocation.Text :=dmGeneralData.GetLocationName(fraLocationInfo.eLocation.Key);
      fraLocationInfo.eLocationName.Text := FieldByName('Location_Name').AsString;
    end;
    with qrySample do begin
      Append;
      FieldByName('Vague_Date_Start').Text:=qryEvent.FieldByName('Vague_Date_Start').Text;
      eSampleDate.Text := qryEvent.FieldByName('Vague_Date_Start').Text;
      FieldByName('Location_Key').AsString:=fraLocationInfo.eLocation.Key;
      FieldByName('Spatial_Ref_System').Text := fraLocationInfo.eSpatialRef.EnteredSystem;
      FieldByName('Spatial_Ref').AsString :=
          fraLocationInfo.eSpatialRef.EnteredRef;
      FieldByName('Spatial_Ref_Qualifier').AsString := fraLocationInfo.eSpatialRef.Qualifier;
      FieldByName('Location_Name').AsString := fraLocationInfo.eLocationName.Text;
    end;
    dmGeneralData.SetNameIDAndDate(qrySample,'Entered_By','Entry_Date');
    qryEvent.Close; 
    qryAdminAreas.Parameters.ParamByName('Key').Value:= SampleKey;
  end;
  ResetDBRichEditControls([dbreComments]);

  // Deal with Recorders
  FdmSample.RefreshRecorders('',EventKey,clbRecorders);
  // if only one recorder, then select automatically
  if clbRecorders.Items.Count=1 then
    clbRecorders.Checked[0] := true;
  // Measurements
  Measurements.KeyValue:=SampleKey;
  Measurements.Refresh;
  // Related Samples
  FdmSample.qryRelatedSample.Parameters.ParamByName('KeyParameter').Value:='';
  FRelatedSampleList.SampleKey:=SampleKey;
  FRelatedSampleList.Refresh;
  // Sources
  Sources.SourcesFromKey:=SampleKey;
  Sources.RefreshLists;
  { Add COM addin page }
  AddAdditionalPages;
  EnableDetails(emAdd);
end;  // AddRecord

//==============================================================================
procedure TfrmSampleDetails.DeleteRecord(const ASampleKey:TKeyString);
begin
  FdmSample.DeleteRecord(ASampleKey);
end;  // DeleteRecord

//==============================================================================
procedure TfrmSampleDetails.EditRecord;
begin
  DisplayRecord(SampleKey);
  if dmGeneralData.HasFullEditAccess('Sample', 'Sample_Key', SampleKey) and
      HaveCustody then
    try
      FdmSample.qrySample.Edit;
      dmGeneralData.SetNameIDAndDate(FdmSample.qrySample,'Changed_By','Changed_Date');
      EnableDetails(emEdit);
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
    EnableDetails(emEdit);
end;  // EditRecord

//==============================================================================
procedure TfrmSampleDetails.bbSaveClick(Sender: TObject);
var lNodeText     : string;
    lCurrentTab   : TTabSheet;
    lCursor       : TCursor;
    lCustodian    : String;
    lOldValues, lNewValues: TStringList;
    lSampleTypeKey: TKeyString;
    lCascadeType: TCascadeType;
    lCascadeSiblings: Boolean;
begin
  inherited;
  lCurrentTab := pcSampleDetails.ActivePage;
  
  if Sender = nil then  begin   // Sender is nil if called from CloseQuery method.
    eSampleDateExit(nil);
    ValidateStartTime;
  end;

  (* step 1: validate all user inputs *)
  ValidateSampleDetailInput;
  pcSampleDetails.ActivePage := lCurrentTab;

  (*step 2: display sample cascade screen *)
  lOldValues := TStringList.Create;
  lNewValues := TStringList.Create;
  try
    // Determine whether the values of controls that require cascading have changed.
    with FdmSample.qrySample do begin
      if FieldByName('Location_Key').AsString <> fraLocationInfo.eLocation.Key then begin
        lOldValues.Values['Location_Key'] := FieldByName('Location_Key').AsString;
        lNewValues.Values['Location_Key'] := fraLocationInfo.eLocation.Key;
      end;

      if FieldByName('Spatial_Ref').AsString <> fraLocationInfo.eSpatialRef.EnteredRef then begin
        lOldValues.Values['Spatial_Ref'] := FieldByName('Spatial_Ref').AsString;
        lNewValues.Values['Spatial_Ref'] := fraLocationInfo.eSpatialRef.EnteredRef;
      end;

      if FieldByName('Spatial_Ref_Qualifier').AsString <> fraLocationInfo.eSpatialRef.Qualifier then
      begin
        lOldValues.Values['Spatial_Ref_Qualifier'] := FieldByName('Spatial_Ref_Qualifier').AsString;
        lNewValues.Values['Spatial_Ref_Qualifier'] := fraLocationInfo.eSpatialRef.Qualifier;
      end;

      if FieldByName('Location_Name').AsString <> fraLocationInfo.eLocationName.Text then begin
        lOldValues.Values['Location_Name'] := FieldByName('Location_Name').AsString;
        lNewValues.Values['Location_Name'] := fraLocationInfo.eLocationName.Text
      end;

      if FieldByName('Vague_Date_Start').Text  <> eSampleDate.Text then
      begin
        lOldValues.Values['Vague_Date_Start'] := FieldByName('Vague_Date_Start').AsString;
        lOldValues.Values['Vague_Date_End']   := FieldByName('Vague_Date_End').AsString;
        lOldValues.Values['Vague_Date_Type']  := FieldByName('Vague_Date_Type').AsString;
        lNewValues.Values['Vague_Date_Start'] := IntToStr(Trunc(eSampleDate.StartDate));
        lNewValues.Values['Vague_Date_End']   := IntToStr(Trunc(eSampleDate.EndDate));
        lNewValues.Values['Vague_Date_Type']  := eSampleDate.DateTypeString;
      end;
    end;  //end with FdmSample.qrySample

    lCascadeType := FdmSample.ValidateCascadeChanges(
        EventKey, SampleKey, lOldValues, fraLocationInfo, eSampleDate, lCascadeSiblings);

    (*step 3: post changes to database *)
    If not CheckAdditionalPageCanSave then Exit;

    lCursor := HourglassCursor;
    try
      lCustodian := dmGeneralData.Custodian('Sample', 'Sample_Key', SampleKey);
      if ((AppSettings.UserAccessLevel > ualAddOnly) and (lCustodian = AppSettings.SiteID)) or (EditMode = emAdd) then
      begin
        with FdmSample.qrySample do begin
          if State in [dsEdit, dsInsert] then begin
            if fraLocationInfo.eLocation.Key <> '' then
              FieldByName('Location_Key').AsString := fraLocationInfo.eLocation.Key
            else
              FieldByName('Location_Key').Value    := Null;

            FieldByName('Spatial_Ref_System').AsString    := fraLocationInfo.eSpatialRef.EnteredSystem;
            FieldByName('Spatial_Ref').AsString           := fraLocationInfo.eSpatialRef.EnteredRef;
            FieldByName('Spatial_Ref_Qualifier').AsString := fraLocationInfo.eSpatialRef.Qualifier;
            FieldByName('Location_Name').AsString         := fraLocationInfo.eLocationName.Text;
            FieldByName('Vague_Date_Start').Text          := eSampleDate.Text;
            FieldByName('Time').AsString                  := eStartTime.Text;
            // Michael weideli   Mantis 450 Mantis 451
            FieldByName('Private_Location').AsString      := ePrivateLocation.Text;
            FieldByName('Private_Code').AsString          := ePrivateCode.Text;
            FieldByName('Recorders').AsString             := eUnparsed.Text;
            if State = dsInsert then
            begin
              FieldByName('Sample_Key').AsString        := SampleKey;
              FieldByName('Survey_Event_Key').AsString  := EventKey;
              FieldByName('Outstanding_Card').AsInteger := 0;
            end;

            try
              Post;
              if (lOldValues.Count > 0) then begin
                FdmSample.CascadeSampleChanges(
                    EventKey, SampleKey, lOldValues, lNewValues, lCascadeType, lCascadeSiblings);
                FdmSample.UpdateEventNodeCaption(TfrmObservations(DrillForm).SelectedItem.Parent);
                TfrmObservations(DrillForm).tvObservations.Refresh;
              end;
            except
              on E:EDatabaseError do begin
                // The following error could happen if the sample query affects a timestamp row
                if CompareText(Copy(E.Message, 1, 34), ResStr_CannotLocateRow) = 0 then
                  ShowInformation(E.Message)
                else
                  raise;
              end;
            end; // try
          end;
        end;  // with qrySample
      end else
      if (EditMode = emEdit) and (lCustodian <> FCustodian) then begin
        FdmSample.qrySample.Cancel;
        MessageDlg(Format(ResStr_CustodyChanged, ['Sample']), mtWarning, [mbOk], 0);
      end;
      lSampleTypeKey := FdmSample.qrySample.FieldByName('Sample_Type_Key').AsString;

      // Recorders
      FdmSample.UpdateRecorders(SampleKey,clbRecorders);
      // Related samples
      FRelatedSampleList.SampleKey:=SampleKey;
      FRelatedSampleList.Update;
      // Measurements
      Measurements.KeyValue:=SampleKey;
      Measurements.UpdateList;
      // Sources
      Sources.Post;
      // Admin areas
      FSampleAdminAreaList.SampleKey := SampleKey;
      FSampleAdminAreaList.Update;
      // Additional Pages
      SaveAdditionalPages;
      if DrillForm <> nil then begin
        lNodeText := Format(
            '%s - %s - %s',
            [eSampleDate.Text, fraLocationInfo.LocalityLabel, dbcmbSampleType.Text]);

        if dbeSampleRef.Text <> '' then
          lNodeText := Format('%s - %s', [dbeSampleRef.Text, lNodeText]);

        with TfrmObservations(DrillForm) do begin
          SetItemTextAndKey(lNodeText, SampleKey, lSampleTypeKey);
          tvObservations.Selected := SelectedItem;
        end;

        // Ensure the caption for all sample nodes has been updated
        //  (including those with multiple names for their location)
        if lCascadeSiblings then begin
          FdmSample.UpdateSampleNodesCaption(TfrmObservations(DrillForm).SelectedItem.Parent);
          TfrmObservations(DrillForm).tvObservations.Refresh;
        end;
      end;
      EnableDetails(emView);
      DisplayRecord(SampleKey);
    finally
      DefaultCursor(lCursor);
    end;
  finally
    FreeAndNil(lNewValues);
    FreeAndNil(lOldValues);
  end;
end;  // bbSaveClick

//==============================================================================
procedure TfrmSampleDetails.bbCancelClick(Sender: TObject);
var tfDiscardNew:boolean;
begin
  inherited;
  tfDiscardNew:=FdmSample.qrySample.State=dsInsert;
  FdmSample.qrySample.Cancel;
  // Additional Pages
  CancelAdditionalPages;
  EnableDetails(emView);

  // If closing form, no need to do anything further. Otherwise, refresh screens.
  if not FClosingForm then
    if tfDiscardNew and (DrillForm<>nil) then
      PostMessage(TfrmObservations(DrillForm).Handle,WM_Discard_Observation,0,0)
    else begin
      with TfrmObservations(DrillForm) do
        tvObservations.Selected:=SelectedItem;
      DisplayRecord(SampleKey);
    end;
end;  // bbCancelClick

//==============================================================================
procedure TfrmSampleDetails.EnableDetails(const NewMode:TEditMode);
var tfOn:boolean;
begin
  FEditMode:=NewMode;
  tfOn:=EditMode<>emView;

  RefreshColours;
  // General
  fraLocationInfo.ReadOnly := not (FdmSample.qrySample.State in [dsEdit,dsInsert]);
  eSampleDate.ReadOnly := fraLocationInfo.ReadOnly;
  eStartTime.ReadOnly  :=fraLocationInfo.ReadOnly;
  // Michael weideli    Mantis 450 Mantis 451
  ePrivateLocation.ReadOnly := fraLocationInfo.ReadOnly;
  ePrivateCode.ReadOnly :=  fraLocationInfo.ReadOnly;
  eUnparsed.ReadOnly :=  fraLocationInfo.ReadOnly;
 // Related Sample
  bbSampleAdd.Enabled  :=tfOn;
  sgRelatedSamplesClick(nil);
  // Other
  Measurements.EditMode :=NewMode;
  Sources.EditMode      :=NewMode;

  clbRecorders.Enabled  :=tfOn;
  // RTF popup menus
  if tfOn then dbreComments.PopupMenu:=dmFormActions.pmRTF
          else dbreComments.PopupMenu:=nil;

  bbSave.Enabled  :=tfOn;
  bbCancel.Enabled:=tfOn;
  // Admin areas
  bbAdminAdd.Enabled :=tfOn;

  sgAdminAreasClick(nil);
  // Additional Pages
  SetAdditionalPagesState(tfOn);

  if DrillForm<>nil then
    TfrmObservations(DrillForm).SetMenuState(not tfOn);
end;  // EnableDetails

//==============================================================================
procedure TfrmSampleDetails.clbRecordersClickCheck(Sender: TObject);
var lIdx:integer;
begin
  inherited;
  with clbRecorders do begin
    lIdx := ItemIndex;
    // If can't edit, cancel change
    if EditMode<>emAdd then begin
      if not (AppSettings.AllowEdit(EditMode) and
          dmGeneralData.HaveCustody('SURVEY_EVENT_RECORDER', 'SE_RECORDER_KEY',
                                 TKeyData(Items.Objects[lIdx]).ItemKey) and
          dmGeneralData.HasFullEditAccess('SURVEY_EVENT_RECORDER', 'SE_RECORDER_KEY',
                                 TKeyData(Items.Objects[lIdx]).ItemKey)) then
      begin
        Checked[lIdx] := not Checked[lIdx];
        PostMessage(TWinControl(Sender).Handle, WM_KEYDOWN, VK_Return,0);
      end;
    end;
  end;
end;  // clbRecordersClickCheck

//==============================================================================
function TfrmSampleDetails.CheckRelatedSample(const AKey:TKeyString):boolean;
var lCount:integer;
begin
  Result:=false;
  if FRelatedSampleList.ItemCount>0 then
    with sgRelatedSamples do
      for lCount:=FixedRows to RowCount-1 do
        Result:=TRelatedSampleItem(Objects[0,lCount]).ItemKey=AKey;
end;  // CheckRelatedSample

//==============================================================================
procedure TfrmSampleDetails.sgRelatedSamplesClick(Sender: TObject);
begin
  inherited;
  if FRelatedSampleList.ItemCount=0 then
    bbSampleDel.Enabled:=false
  else
    with TRelatedSampleItem(sgRelatedSamples.Objects[0,sgRelatedSamples.Row]) do
      EnableDetailsAddEditButtons(TN_SAMPLE_RELATION, 'Sample_Relation_Key', SampleRelationKey,
          Custodian, EditMode, Added, nil, bbSampleDel, True);
end;

//------------------------------------------------------------------------------
procedure TfrmSampleDetails.bbSampleAddClick(Sender: TObject);
var
  lDataItem:TRelatedSampleItem;
  lFind : TdlgFind;
begin
  inherited;
  lFind := TdlgFind.CreateDialog(nil,ResStr_FindRelatedSample,ftSample);
  with lFind do begin
    try
      SetSearchText('');
      if not eSearchText.NoSourceItems then begin
        if ShowModal=mrOk then begin
          // Add if doesn't already exist in the grid
          if not CheckRelatedSample(ItemKey) and (ItemKey<>SampleKey) then begin
            lDataItem:=TRelatedSampleItem.CreateNew(FRelatedSampleList);
            lDataItem.SetProperties(ItemKey);
            FRelatedSampleList.AddNew(lDataItem);
            sgRelatedSamplesClick(nil);
          end;
        end;
      end else
        MessageDlg(ResStr_RelatedSampleItems, mtInformation, [mbOK], 0);
    finally
      Free;
    end;
  end;
end;  // bbSampleAddClick

//==============================================================================
procedure TfrmSampleDetails.bbSampleDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteRelatedSample, mtConfirmation, [mbNo,mbYes], 0) = mrYes then
  begin
    FRelatedSampleList.DeleteItem(sgRelatedSamples.Row);
    sgRelatedSamplesClick(nil);
  end;
end;  // bbSampleDelClick

//==============================================================================
procedure TfrmSampleDetails.SetupDestinationControls;
begin
  RegisterDropComponent(sgRelatedSamples, DropRelatedSample,
                        ['SAMPLE'],[CF_JNCCDATA]);
  RegisterDropComponent(sgAdminAreas, DropAdminArea,
                         [TN_ADMIN_AREA], [CF_JNCCDATA]);
  fraLocationInfo.RegisterDragDrop(self);
end;  // SetupDestinationControls

//==============================================================================
procedure TfrmSampleDetails.DropRelatedSample(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
var lDataItem:TRelatedSampleItem;
    lKey     :TKeyString;
begin
  if EditMode<>emView then begin
    if iSourceData.Header.ItemCount>0 then
      if iFormat = CF_JNCCDATA then begin
        ioHandled:=true;
        lKey     :=iSourceData.Items[0].KeyField1;
        // Add if doesn't already exist in the grid
        if not CheckRelatedSample(lKey) and (lKey<>SampleKey) then begin
          lDataItem:=TRelatedSampleItem.CreateNew(FRelatedSampleList);
          lDataItem.SetProperties(lKey);
          FRelatedSampleList.AddNew(lDataItem);
          sgRelatedSamplesClick(nil);
        end;
      end else
        ioHandled:= false;
  end else
    ioHandled:=true;
end;  // DropRelatedSample

//==============================================================================
procedure TfrmSampleDetails.SetDrillForm(const Value: TBaseForm);
begin
  FDrillForm := Value;
end;  // SetDrillForm

//==============================================================================
procedure TfrmSampleDetails.WMTransferDone(var Msg: TMessage);
begin
  if DrillForm<>nil then TfrmObservations(DrillForm).Show;
end;  // WMTransferDone

//==============================================================================
procedure TfrmSampleDetails.UpdateRTFMenu;
begin
  if Assigned(DrillForm) then
    dmFormActions.UpdateRTFMenu((DrillForm.ActiveControl is TDBRichEdit) and
                              (FdmSample.qrySample.State in [dsEdit,dsInsert]));
end;  // UpdateRTFMenu

//==============================================================================
procedure TfrmSampleDetails.EnterRTF(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(FdmSample.qrySample.State in [dsEdit,dsInsert]);
end;  // EnterRTF

//==============================================================================
procedure TfrmSampleDetails.ExitRTF(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(false);
end;  // ExitRTF

//==============================================================================
procedure TfrmSampleDetails.pnlDetailsResize(Sender: TObject);
var
  deltaWidth, deltaHeight : Integer;
begin
  SetTitleLabel;

  // Works out the change in height and width of the panel.
  deltaWidth := pnlDetails.Width - pnlInner.Width;
  deltaHeight := pnlDetails.Height - pnlInner.Height;

  // For some reason, the automatic resizing doesn't work properly. Using a
  // second panel within the main panel and sizing it manually solves this, but
  // is a bit of a hack.
  pnlInner.Width := pnlDetails.Width;
  pnlInner.Height := pnlDetails.Height; 

  Sources.Width := Sources.Width + deltaWidth;
  Sources.Height := Sources.Height + deltaHeight;    
  Measurements.Width := Measurements.Width + deltaWidth;
  Measurements.Height := Measurements.Height + deltaHeight;
end;  // scbpnlSampleDetailsResize

//==============================================================================
function TfrmSampleDetails.GetKeyList: TKeyList;
var lNewKeyList:TEditableKeyList;
begin
  //Return an editable key list with the selected nodes key
  lNewKeyList:= TEditableKeyList.Create;
  lNewKeyList.SetTable('SAMPLE');
 	lNewKeyList.AddItem(SampleKey,'');
  Result:= lNewKeyList;
end;  // GetKeyList

//==============================================================================
procedure TfrmSampleDetails.eStartTimeExit(Sender: TObject);
var lPos: TPoint;
begin
  inherited;
  lPos := bbCancel.ScreenToClient(Mouse.CursorPos);
  if not (((lPos.X in [0..bbCancel.Width]) and (lPos.Y in [0..bbCancel.Height])) or FClosingForm) then
    ValidateStartTime;
end;  // eStartTimeExit

procedure TfrmSampleDetails.ValidateStartTime;
begin
  if eStartTime.Text<>'' then begin
    ValidateValue(IsTime(eStartTime.Text), ResStr_InvalidTime, eStartTime);
    eStartTime.Text := FormatDateTime('hh:mm', StrToTime(eStartTime.Text));
  end;
end;  // ValidateStartTime

//==============================================================================
procedure TfrmSampleDetails.RefreshLists;
begin
  // only way I could find to get DBCOmboBoxes to Refresh
  FdmSample.qrySampleType.Active := False;
  FdmSample.qrySampleType.Active := True;
  Measurements.RefreshLists;
  Measurements.Refresh;
end;  // Refreshlists

//==============================================================================
procedure TfrmSampleDetails.SetDisplaySystem;
begin
  fraLocationInfo.eSpatialRef.DisplaySystem := AppSettings.SpatialRefSystem;
end;

//==============================================================================
procedure TfrmSampleDetails.pcSampleDetailsChange(Sender: TObject);
begin
  inherited;
  pcSampleDetails.HelpContext := pcSampleDetails.ActivePage.HelpContext;
end;
//==============================================================================
procedure TfrmSampleDetails.RefreshSampleLocation;
begin
  fraLocationInfo.eLocation.Text := dmGeneralData.GetLocationName(fraLocationInfo.eLocation.Key);
end;

//==============================================================================
procedure TfrmSampleDetails.RefreshSpatialRef;
begin
  // Spatial reference updated so update details
  if EditMode=emView then
    DisplayRecord(FSampleKey);
end;

//==============================================================================
procedure TfrmSampleDetails.RefreshNames;
begin
  with FdmSample do
    RefreshRecorders(FSampleKey,qrySample.FieldByName('Survey_Event_Key').AsString,clbRecorders);
end;

//==============================================================================
procedure TfrmSampleDetails.RefreshColours;
begin
  SetRequiredFieldsColourState(EditMode <> emView, [eSampleDate, dbcmbSampleType, clbRecorders]);
  if EditMode <> emView then
    fraLocationInfo.SetColour(MergeColours(AppSettings.MandatoryColour, clWindow, 25))
  else
    fraLocationInfo.SetColour(clWindow);
end;

//==============================================================================
procedure TfrmSampleDetails.ShowMetadata;
begin
  with TdlgMetaDataPopup.Create(nil) do
  try
    ShowStandard(ResStr_Sample, lblSample.Caption,
       FdmSample.qrySample.FieldByName('Sample_Key').AsString,
       FdmSample.qrySample);
  finally
    Free;
  end;
end;

//==============================================================================
{ Display the map window panned to show a sample }
procedure TfrmSampleDetails.FindOnMap;
begin
  fraLocationInfo.FindOnMap;
end;

//==============================================================================
//These functions control whether BaseChild warns the user that the data
//in the combo boxes is not
//editable because the data is from another site.
procedure TfrmSampleDetails.dbComboKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  FKeyDown :=true;
end;

procedure TfrmSampleDetails.dbComboKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  FKeyDown :=false;
end;

procedure TfrmSampleDetails.dbComboClick(Sender: TObject);
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

{-------------------------------------------------------------------------------
}
procedure TfrmSampleDetails.UpdateMapWindowSelector;
begin
  fraLocationInfo.UpdateMapWindowSelector;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSampleDetails.SpatialRefInvalidSpatialRef(Sender: TObject; var Handled: Boolean);
var
  lPos: TPoint;
begin
  lPos := bbCancel.ScreenToClient(Mouse.CursorPos);

  // Check if clicked Cancel
  Handled := (lPos.X in [0..bbCancel.Width]) and (lPos.Y in [0..bbCancel.Height]) or FClosingForm;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSampleDetails.eSampleDateExit(Sender: TObject);
var
  lPos: TPoint;
begin
  lPos := bbCancel.ScreenToClient(Mouse.CursorPos);
  if not (((lPos.X in [0..bbCancel.Width]) and (lPos.Y in [0..bbCancel.Height])) or FClosingForm) then
  begin
    ValidateValue(IsVagueDate(eSampleDate.Text),
                  ResStr_InvalidVagueDate, eSampleDate);
    ValidateValue(CheckVagueDate(eSampleDate.Text),
                  Format(ResStr_Future_VagueDate, [eSampleDate.Text]), eSampleDate);
    eSampleDate.Text := VagueDateToString(eSampleDate.VagueDate);
  end;
end;


{-------------------------------------------------------------------------------
  Set the text for the little label at the top and clip it
}
procedure TfrmSampleDetails.SetTitleLabel;
var
  lText: String;
begin
  lText := FdmSample.GetNodeCaption(
      dbeSampleRef.Text,
      eSampleDate.Text,
      fraLocationInfo.eLocation.Text,
      fraLocationInfo.eLocationName.Text,
      fraLocationInfo.eSpatialRef.DisplayRef,
      dbcmbSampleType.Text,
      True);
  lblSample.Caption := GetTextWithinLimit(
      Canvas,
      DuplicateCharacters(lText, '&'),
      pnlDetails.Width - lblSample.Left - 8);
end;


{-------------------------------------------------------------------------------
  validate all user inputs for sample details
}
procedure TfrmSampleDetails.ValidateSampleDetailInput;
var
  lIndex   : integer;
  lChecked : boolean;
  lEventKey: string;
begin
   // General tab
  pcSampleDetails.ActivePage:=tsGeneral;
  ValidateValue(eSampleDate.Text <> '',ResStr_SampleDateRequired, eSampleDate);
  ValidateValue(dbcmbSampleType.Text <> '',ResStr_SampleTypeRequired,dbcmbSampleType);
  lEventKey := TfrmObservations(DrillForm).GetSurveyKeyForEvent(FEventKey);
  // if unparsed is not blank then the survey must be marked temporary
  if eUnparsed.text <> '' then
    ValidateValue(dmValidation.CheckIsTemporary(lEventKey).Success,
      ResStr_SurveyMustBeTemp,
      eUnparsed);
  // Make sure it stays inside its survey irrespective of cascading
  with fraLocationInfo do begin
    Validate;
    { Check spatial ref for Sample Location lies within survey bounding box }
    ValidateValue(
        dmValidation.CheckEventInSurvey(
            lEventKey,
            eSpatialRef.DisplayRef,
            eSpatialRef.DisplaySystem,
            '').Success,
        ResStr_OutsideSurveyBoundingBox,
        eSpatialRef.ControlSpatialRef);
  end;
  ValidateValue(
      dmValidation.CheckEventDateAgainstSurvey(lEventKey, eSampleDate.VagueDate),
      ResStr_OutsideSurveyDateRange,
      eSampleDate);
  // Recorders tab
  pcSampleDetails.ActivePage := tsSampleRecorder;
  lChecked := False;
  for lIndex := 0 to clbRecorders.Items.Count - 1 do
    lChecked := lChecked or clbRecorders.Checked[lIndex];
  ValidateValue(lChecked, ResStr_NoRecorderSelected, clbRecorders);
  // Measurements tab
  pcSampleDetails.ActivePage:=tsMeasurements;
  ValidateValue(Measurements.CheckGrid, ResStr_MeasurementMissingOrInvalid, Measurements.Grid);
end;

{-------------------------------------------------------------------------------
  When the AdminAreas string grid is clicked, enables or disables the delete button
  based on whether or not the selected item can be deleted at this time.
}
procedure TfrmSampleDetails.sgAdminAreasClick(Sender: TObject);
begin
  inherited;
  if FSampleAdminAreaList.ItemCount=0 then
    bbAdminDel.Enabled:=false
  else
    with TSampleAdminAreaItem(sgAdminAreas.Objects[0,sgAdminAreas.Row]) do
      EnableDetailsAddEditButtons(TN_SAMPLE_ADMIN_AREAS, 'Sample_Admin_Areas_Key', AdminAreaKey,
          Custodian, EditMode, Added, nil, bbAdminDel, True);
end;

procedure TfrmSampleDetails.bbAdminAddClick(Sender: TObject);
begin
  dmFormActions.actAdminAreaDiction.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateAdminArea);
end;

{-------------------------------------------------------------------------------
  Updates the admin area string grid with one or more passed admin areas.
}
procedure TfrmSampleDetails.UpdateAdminArea(KeyList:TKeyList);
var lDataItem : TSampleAdminAreaItem;
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
                   (TSampleAdminAreaItem(Objects[0,lCount]).AdminAreaKey=KeyList.Items[0].KeyField1));

      if not lExists then begin
        lDataItem:=TSampleAdminAreaItem.CreateNew(FSampleAdminAreaList);
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
        FSampleAdminAreaList.AddNew(lDataItem);
      end;
    end;
  finally
    KeyList.Free;
  end;
  sgAdminAreasClick(nil);
end;  // UpdateAdminArea

{-------------------------------------------------------------------------------
  When the delete button is clicked, removes the currently selected item from the list.
}
procedure TfrmSampleDetails.bbAdminDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_DeleteAdmin, mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
    FSampleAdminAreaList.DeleteItem(sgAdminAreas.Row);
    sgAdminAreasClick(nil);
  end;
end;

{-------------------------------------------------------------------------------
  Handles an admin area being dropped on the admin area grid.
}
procedure TfrmSampleDetails.DropAdminArea(const Sender: TObject;
  const iFormat : Integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : Boolean);
var
  lNewAdminArea : TSampleAdminAreaItem;
  lTempList : TStringList;
begin
  if (EditMode<>emView) and ((AppSettings.UserAccessLevel>ualAddOnly) or
                             (FdmSample.qrySample.State=dsInsert)) then
  begin
    if iSourceData.Header.ItemCount>0 then
      if iFormat=CF_JNCCDATA then begin
        ioHandled:= True;
        if not AdminAreaCheckExistence(iSourceData.Items[0].KeyField1) then begin
          lNewAdminArea:= TSampleAdminAreaItem.CreateNew(FSampleAdminAreaList);
          with lNewAdminArea do begin
            AdminAreaKey:= iSourceData.Items[0].KeyField1;

            //Find display fields
            lTempList:= TStringList.Create;
            try
              dmGeneralData.GetRecordStrings(lTempList, 'ADMIN_AREA', AdminAreaKey);
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
          FSampleAdminAreaList.AddNew(lNewAdminArea);
        end;
      end;
  end else
    ioHandled:=True;
  sgAdminAreasClick(nil);
end;  // DropAdminArea

{-------------------------------------------------------------------------------
  Checks whether a key exists in a grid.
}
function TfrmSampleDetails.AdminAreaCheckExistence(const AKey:TKeyString):Boolean;
var lCount : Integer;
begin
  Result:=False;
  if FSampleAdminAreaList.ItemCount>0 then
    with sgAdminAreas do
      for lCount:=FixedRows to RowCount-1 do
        Result:=Result or (TSampleAdminAreaItem(Objects[0,lCount]).AdminAreaKey=AKey);
end;  // AdminAreaCheckExistence

procedure TfrmSampleDetails.fraLocationInfoeLocationNameChange(
  Sender: TObject);
begin
  inherited;
  fraLocationInfo.DataChange(Sender);

end;

procedure TfrmSampleDetails.fraLocationInfoeLocationChange(
  Sender: TObject);
begin
  inherited;
  fraLocationInfo.DataChange(Sender);

end;

end.
