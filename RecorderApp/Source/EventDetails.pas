//==============================================================================
//  Unit:        EventDetails
//
//  Implements:  TfrmEventDetails
//
//  Description: This form is docked on the Observations screen to allow details
//               of a selected event to be viewed and/or edited.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 225 $
//    $Date: 8/04/10 16:51 $
//    $Author: Andrewkemp $
//
//==============================================================================

{$I '..\..\Third Party\Dorset Software Services\DssVcl32\DelphiVersions.Inc'}

{ Event Details unit - Recorder 2000
     Contains details page for Survey Events. }
unit EventDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseFormUnit, Grids, StdCtrls, ExtCtrls, ActnList, Buttons, Menus, Mask,
  DBCtrls, ComCtrls, CompositeComponent, Sources, Droptarget, DataClasses,
  EventData, Db, DBListCombo, SpatialRef, VagueDate, Constants, BaseDockedForm,
  ExceptionForm, ValidationData, OnlineHelp, GeneralFunctions, ImageListButton,
  DatabaseAccessADO, Variants, VagueDateEdit, DataStringGrid,
  LocationInfoFrame, ControlStringGrid, ResourceStrings;

resourcestring
  ResStr_RecorderRequired = 'A recorder must be selected and have a valid role before you can save the event.';
  ResStr_UncheckRecorder = 'This recorder is used in one or more Samples. Make sure the name is '#13#10 +
            'unchecked in the Recorders check list of the appropriate Samples before '#13#10 +
            'attempting the deletion again.';
  ResStr_InvalidVagueDate = 'The vague date you have entered is not valid';
  ResStr_EventDate = 'The Event Date ';

  ResStr_InvalidOwnerName = 'The owner''s name ''%s'' you entered is invalid.'#13 +
                            'Please make sure that the name is that of either an individual ' +
                            'or an organisation.';

  ResStr_EventDataRequired =  'An Event Date is required for every Event.';
  ResStr_DeleteRecord = 'Are you sure you want to delete this Recorder?';
  ResStr_SpatialRef = 'Spatial Reference';
  ResStr_LocationName = 'Location Name';
  ResStr_Location = 'Location';


type
  EEventDetailsError = class(TExceptionPath);

  TOwnersGrid = class(TControlStringGrid)
  private
    FOwnershipGridManager: TDataStringGrid;
    procedure SetOwnershipGridManager(const Value: TDataStringGrid);
  protected
    procedure ColWidthsChanged; override;
  public
    property OwnershipGridManager: TDataStringGrid read FOwnershipGridManager write SetOwnershipGridManager;
  end;

  TfrmEventDetails = class(TfrmBaseDockedForm)
    pmMapWindow: TPopupMenu;
    pnlDetails: TPanel;
    pnlInner: TPanel;
    dblblEventDate: TDBText;
    lblEvent: TLabel;
    pcEventDetails: TPageControl;
    tsGeneral: TTabSheet;
    Bevel1: TBevel;
    lblEventDate: TLabel;
    lblEventWeather: TLabel;
    shpLocationName: TShape;
    dbeEventWeather: TDBEdit;
    eEventDate: TVagueDateEdit;
    fraLocationInfo: TfraLocationInfo;
    tsOwnership: TTabSheet;
    Label2: TLabel;
    Bevel2: TBevel;
    tsSources: TTabSheet;
    Sources: TSources;
    bbSave: TImageListButton;
    bbCancel: TImageListButton;
    pnlCommentsRecorders: TPanel;
    pnlComment: TPanel;
    lblComment: TLabel;
    dbreComments: TDBRichEdit;
    pnlRecorders: TPanel;
    shpRecorders: TShape;
    Label4: TLabel;
    sgRecorders: TStringGrid;
    bbRecorderFind: TImageListButton;
    bbRecorderAdd: TImageListButton;
    bbRecorderRemove: TImageListButton;
    cmbRecorderRole: TComboBox;
    bbRecorderReplace: TImageListButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbSaveClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure bbRecorderFindClick(Sender: TObject);
    procedure bbRecorderAddClick(Sender: TObject);
    procedure bbRecorderRemoveClick(Sender: TObject);
    procedure bbRecorderReplaceClick(Sender: TObject);
    procedure sgRecordersDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure cmbRecorderRoleExit(Sender: TObject);
    procedure cmbRecorderRoleChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure EnterRTF(Sender: TObject);
    procedure ExitRTF(Sender: TObject);
    procedure sgRecordersTopLeftChanged(Sender: TObject);
    procedure pnlDetailsResize(Sender: TObject);
    procedure pcEventDetailsChange(Sender: TObject);
    procedure sgRecordersClick(Sender: TObject);
    procedure eEventDateExit(Sender: TObject);
    procedure tsOwnershipShow(Sender: TObject);
    procedure SpatialRefInvalidSpatialRef(Sender: TObject;
      var Handled: Boolean);
    procedure fraLocationInfoeSpatialRefChange(Sender: TObject);
    procedure sgRecordersKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FDrillForm    :TBaseForm;
    FdmEvent      :TdmEvent;
    FEventRecorderList:TEventRecorderList;
    FEventKey: TKeyString;
    FSurveyKey: TKeyString;
    FClosingForm: Boolean;
    FOwnershipPopulated: boolean;
    FOwnershipGridManager: TDataStringGrid;
    sgOwners: TControlStringGrid;
    procedure EnableDetails(const NewMode:TEditMode);
    procedure SetDrillForm(const Value: TBaseForm);
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
    procedure SetupObjects;
    procedure FreeObjects;
    procedure UpdateRecorders(KeyList: TKeyList);
    procedure ChangeRecorders(KeyList: TKeyList);
    procedure UpdateEventOwner(KeyList: TKeyList);
    procedure AddRecorder(const ANameKey: TKeyString);
    procedure ModifyRecorder(const ANameKey: TKeyString);
    procedure DropRecorder(const Sender: TObject; const iFormat: integer;
      const iSourceData: TKeyList; const iTextStrings: TStringList;
      var ioHandled: boolean);
    function RecorderUsedInSample(const AKey: TKeyString): boolean;
    function MultipleRecordersInSample(const AKey: TKeyString): boolean;
    function Survey_Event_Recorder_Name_Changed(const ASERKey,ANameKey: string ): boolean;
    function Survey_Event_Recorder_Name_Added(const ASEKey: string; AStringList: TStringList): boolean;
    procedure DeleteOwnership(ioRowKey: string);
    procedure UpdateOwnership(var ioRowKey: string; iData: TStringList);
    procedure FindSurveyEventOwnerName(const AInitialText: string;
      var AText, AKey: string);
    procedure GetSurveyEventOwnerName;
    procedure ValidateOwnershipNames;
    procedure CascadeEventChanges(AChanges, APreviousValues : TStringList; EventKey: TKeyString);
    procedure CascadeRecorderToSamples(const ASEKey : string);
    procedure CascadeDeterminer(const AKeyList:TStringList);
    procedure UpdateSamples(AChanges, AChangeValues, APreviousValues: TStringList; ASetAll: boolean);
    function GetEventValue(aKey: string): string;
    procedure UpdateSamplesInInterface;
    function EventHasSamples: Boolean;
  protected
    procedure SetupDestinationControls; override;
  public
    constructor Create(Aowner : TComponent); override;
    destructor Destroy; override;
    function GetKeyList: TKeyList; override;
    procedure AddRecord(const ASurveyKey:TKeyString);
    procedure EditRecord;
    procedure DisplayRecord(const AEventKey: TKeyString);
    procedure DeleteRecord(const AEventKey: TKeyString);
    procedure RefreshLists;
    procedure SetDisplaySystem;
    procedure RefreshNames;
    procedure RefreshEventLocation;
    procedure RefreshSpatialRef;
    procedure RefreshColours;
    procedure FindOnMap; override;
    procedure ShowMetadata; override;
    procedure UpdateRTFMenu; override;
    procedure UpdateMapWindowSelector; override;
    property DrillForm:TBaseForm read FDrillForm write SetDrillForm;
    property SurveyKey:TKeyString read FSurveyKey write FSurveyKey;
    property EventKey:TKeyString read FEventKey write FEventKey;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, GeneralData, FormActions, Observations, Locations,
  MainTBar, Find, SpatialRefFuncs, MetadataPopup, Map, Cascade,
  ADODb, HierarchyNodes;


type
  TStringGridAccessor = class(TStringGrid);

//==============================================================================
constructor TfrmEventDetails.Create(Aowner: TComponent);
begin
  inherited Create(AOwner);
  sgOwners := TOwnersGrid.Create(self);
  with sgOwners do begin
    Parent := tsOwnership;
    SetBounds(12, 28, 341, 233);
    ColCount := 2;
    DefaultRowHeight := 18;
    FixedCols := 0;
    Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing];
    TabOrder := 0;
    sgOwners.Anchors := [akTop, akRight, akBottom, akLeft];
  end;
  DrillForm:=nil;
  SetGridColumnTitles(sgRecorders, [ResStr_Name, ResStr_Role]);
  SetupObjects;
  SetDisplaySystem;
  EnableDetails(emView);
  pcEventDetails.ActivePage:=tsGeneral;
  FOwnershipPopulated := False;

  //Help Setup
  pcEventDetails.HelpContext := IDH_EVENTGENERAL;
  tsGeneral.HelpContext      := IDH_EVENTGENERAL;
  tsOwnership.HelpContext    := IDH_EVENTOWNERS;
  tsSources.HelpContext      := IDH_EVENTSOURCES;
end;  // Create

//==============================================================================
procedure TfrmEventDetails.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
end;  // FormClose

//==============================================================================
procedure TfrmEventDetails.FormCloseQuery(Sender: TObject;
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
                FClosingForm := True;
                bbCancelClick(nil);
                CanClose:=true;
                FClosingForm := False;
              end;
    end;
  end else
    CanClose:=true;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmEventDetails.SetupObjects;
begin
  // Data Module
  FdmEvent:=TdmEvent.Create(nil);
  FdmEvent.qryRecorder.Parameters.ParamByName('KeyParameter').Value:='';
  // Recorders
  FEventRecorderList:=TEventRecorderList.Create(FdmEvent.qryRecorder,'SE_Recorder_Key',
                                                sgRecorders,TEventRecorderItem,FdmEvent);
  FEventRecorderList.EventKey:='';
  FCustodian := '';
  // Get the roles for the recorders
  RefreshLists;
  // Sources
  Sources.Init(dmDatabase.LocalDatabase, 'Survey_Event_Sources',
               AppSettings.UserAccessLevel, dmGeneralData.GetNextKey,
               RegisterDropComponent, AppSettings.SiteID, AppSettings.ExternalFilePath);
  Sources.OnFindInternal:=dmGeneralData.SourcesFindInternal;
  Sources.OnAddInternal :=dmGeneralData.SourcesAddInternal;
  Sources.OnShowSource :=dmGeneralData.SourcesShowInternal;
end;  // SetupObjects

//==============================================================================
procedure TfrmEventDetails.Refreshlists;
var iCount :integer;
    lNewKey:TKeyData;
begin
  // Objects in Role list sorted by long_name
  with cmbRecorderRole.Items do begin
    for iCount:=0 to Count-1 do Objects[iCount].Free;
    Clear;
    with FdmEvent.qryRole do begin
      open;
      while not Eof do begin
        lNewKey:=TKeyData.Create;
        lNewKey.ItemKey:=FieldByName('Recorder_Role_Key').AsString;
        AddObject(FieldByName('Short_Name').AsString,lNewKey);
        Next;
      end;
      close;
    end;
  end;
  //Need to refresh role list if not in edit mode
  FEventRecorderList.Refresh;
end;  // RefreshLists

//==============================================================================
procedure TfrmEventDetails.FreeObjects;
var iCount:integer;
begin
  // Objects in Role list
  with cmbRecorderRole.Items do begin
    for iCount := 0 to Count - 1 do Objects[iCount].Free;
    Clear;
  end;

  FreeAndNil(FEventRecorderList);
  FreeAndNil(FdmEvent);
  FreeAndNil(FOwnershipGridManager);
  // Not sure why but it is necessary to kill focus on the grid before the grid
  // starts destroying. Otherwise occasional access violations occur when you
  // close the form. Eg enter an invalid owner name and type, try to close the
  // form, press Yes to save -> validation message. Now close the form again,
  // press no to save -> window closes with access violation without this next line.
  sgOwners.Enabled := False;
end;  // FreeObjects

//==============================================================================
procedure TfrmEventDetails.DisplayRecord(const AEventKey: TKeyString);
var lCursor:TCursor;
begin
  lCursor:=HourglassCursor;
  FEventKey    :=AEventKey;
  try
    with FdmEvent do begin
      with qryEvent do begin
        Close;
        ResetDBRichEditControls([dbreComments]);
        Parameters.ParamByName('KeyParameter').Value:= AEventKey;
        Open;
        FCustodian := FieldByName('Custodian').AsString;
        SurveyKey := FieldByName('Survey_Key').AsString;
        fraLocationInfo.eLocation.Key := FieldByName('Location_Key').AsString;
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

        fraLocationInfo.eLocation.Text := dmGeneralData.GetLocationName(
            fraLocationInfo.eLocation.Key);
        fraLocationInfo.eLocationName.Text  := FieldByName('Location_Name').AsString;
        eEventDate.Text     := FieldByName('Vague_Date_Start').Text;
        lblEvent.Caption:=GetTextWithinLimit(Canvas,DuplicateCharacters(
            fraLocationInfo.eLocation.Text,Char('&')),
            pnlDetails.Width - lblEvent.Left - 8);
      end;
      // Set Survey_Event_Key parameter
      qryRecorder.Parameters.ParamByName('KeyParameter').Value:=AEventKey;
    end;
    // Deal with Recorders
    FEventRecorderList.EventKey:=AEventKey;
    FEventRecorderList.Refresh;
    // Sources
    Sources.SourcesFromKey:=AEventKey;
    Sources.RefreshLists;
    // Additional Pages
    ChangeAdditionalPage(pcEventDetails);
    FOwnershipPopulated := False;
    if pcEventDetails.ActivePage = tsOwnership then
      tsOwnershipShow(nil);
    // Notify COM Addins
    NotifyDataItemChange;
  finally
    DefaultCursor(lCursor);
  end;
end;  // DisplayRecord

//==============================================================================
procedure TfrmEventDetails.AddRecord(const ASurveyKey:TKeyString);
begin
  inherited;
  pcEventDetails.ActivePage:=tsGeneral;
  // Get next sequential key
  SurveyKey:=ASurveyKey;
  EventKey :=dmGeneralData.GetNextKey('Survey_Event','Survey_Event_Key');
  FCustodian := AppSettings.SiteID;
  FdmEvent.qryEvent.Append;
  ResetDBRichEditControls([dbreComments]);
  dmGeneralData.SetNameIDAndDate(FdmEvent.qryEvent,'Entered_By','Entry_Date');
  fraLocationInfo.eLocation.Text :='';
  fraLocationInfo.eLocationName.Text := '';
  fraLocationInfo.eSpatialRef.Clear;
  FdmEvent.qryRecorder.Parameters.ParamByName('KeyParameter').Value:='';
  // Deal with Recorders
  FEventRecorderList.EventKey:=EventKey;
  FEventRecorderList.Refresh;
  // Sources
  Sources.SourcesFromKey:=EventKey;
  Sources.RefreshLists;
  { Add COM addin page }
  AddAdditionalPages;
  // Now we have an event key, we must let any addin pages know what it is
  NotifyDataItemChange;
  EnableDetails(emAdd);
end;  // AddRecord

//==============================================================================
procedure TfrmEventDetails.DeleteRecord(const AEventKey:TKeyString);
begin
  FdmEvent.DeleteRecord(AEventKey);
end;  // DeleteRecord

//==============================================================================
procedure TfrmEventDetails.EditRecord;
begin
  DisplayRecord(EventKey);
  if dmGeneralData.HasFullEditAccess('Survey_Event', 'Survey_Event_Key', EventKey) and
     HaveCustody then
    try
      FdmEvent.qryEvent.Edit;
      dmGeneralData.SetNameIDAndDate(FdmEvent.qryEvent,'Changed_By','Changed_Date');
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
procedure TfrmEventDetails.bbSaveClick(Sender: TObject);
var lCurrentTab : TTabSheet;
    ltfRecRole  : boolean;
    i           : integer;
    lCursor     : TCursor;
    lCustodian  : String;
    lChangedED  : TStringList; //Stores the list of changes made to the event
    lPreviousSR  : TStringList; //Stores the list of previous Sample Recorder key
    lPreviousSER  : TStringList; //Stores the list of previous Survey Event Recorder Key
    lPreviousValues: TStringList; //Stores the previous values of the changed fields
begin
  inherited;
  lCurrentTab:=pcEventDetails.ActivePage;
  pcEventDetails.ActivePage:=tsGeneral;

  { Sender is nil if called from CloseQuery method. }
  if Sender = nil then begin
    eEventDateExit(nil);

  end;
  fraLocationInfo.Validate(SurveyKey);
  ValidateValue(eEventDate.Text<>'',
                ResStr_EventDataRequired, eEventDate);

  if eEventDate.Text <> '' then
    ValidateValue(dmValidation.CheckEventDateAgainstSurvey(SurveyKey,StringToVagueDate(eEventDate.Text)),
                  ResStr_EventDateAgainstSurvey, eEventDate);

  ltfRecRole := false;
  with sgRecorders do
    for i := 1 to RowCount - 1 do
      ltfRecRole := ltfRecRole or ((Objects[0,i]<>nil) and (Cells[1,i]<>''));

  ValidateValue(ltfRecRole,ResStr_RecorderRequired,bbRecorderFind);

  if Assigned(FOwnershipGridManager) then begin
    pcEventDetails.ActivePage := tsOwnership;
    FOwnershipGridManager.Validate;
    ValidateOwnershipNames;
  end;

  pcEventDetails.ActivePage:=lCurrentTab;

  { Call to validate COM addin page... }
  if not CheckAdditionalPageCanSave then Exit;

  lCursor:=HourglassCursor;
  lChangedED := TStringList.Create;
  lPreviousValues := TStringList.Create;
  lPreviousSR := TStringList.Create;
  lPreviousSER := TStringList.Create;
  try
    lCustodian := dmGeneralData.Custodian('Survey_Event', 'Survey_Event_Key', EventKey);
    if ((AppSettings.UserAccessLevel>ualAddOnly) and (lCustodian = AppSettings.SiteID))
       or (EditMode=emAdd) then
    begin
      with FdmEvent.qryEvent do begin
        if State in [dsEdit, dsInsert] then begin
          if fraLocationInfo.eLocation.Key <> '' then
          begin
            if (not (FieldByName('Location_Key').AsString = fraLocationInfo.eLocation.Key)) then
            begin
              lPreviousValues.Add(FieldByName('Location_Key').AsString);
              FieldByName('Location_Key').AsString := fraLocationInfo.eLocation.Key;
              lChangedED.Add('Location_Key');
            end;
          end
          else
            FieldByName('Location_Key').Value           := Null;

          //Checking other fields
          FieldByName('Spatial_Ref_System').AsString    := fraLocationInfo.eSpatialRef.EnteredSystem;

          if FieldByName('Spatial_Ref').AsString
              <> fraLocationInfo.eSpatialRef.EnteredRef then
          begin
            lPreviousValues.Add(FieldByName('Spatial_Ref').AsString);
            FieldByName('Spatial_Ref').AsString :=
                fraLocationInfo.eSpatialRef.EnteredRef;
            lChangedED.Add('Spatial_Ref');
          end;

          if (not (FieldByName('Spatial_Ref_Qualifier').AsString = fraLocationInfo.eSpatialRef.Qualifier)) then
          begin
            lPreviousValues.Add(FieldByName('Spatial_Ref_Qualifier').AsString);
            FieldByName('Spatial_Ref_Qualifier').AsString := fraLocationInfo.eSpatialRef.Qualifier;
            lChangedED.Add('Spatial_Ref_Qualifier');
          end;

          if (not (FieldByName('Location_Name').AsString = fraLocationInfo.eLocationName.Text)) then
          begin
            lPreviousValues.Add(FieldByName('Location_Name').AsString);
            FieldByName('Location_Name').AsString         := fraLocationInfo.eLocationName.Text;
            lChangedED.Add('Location_Name');
          end;

          if (not (FieldByName('Vague_Date_Start').Text = eEventDate.Text)) then
          begin
            lPreviousValues.Add(FieldByName('Vague_Date_Start').AsString);
            lChangedED.Add('Vague_Date_Start');
            lPreviousValues.Add(FieldByName('Vague_Date_End').AsString);
            lChangedED.Add('Vague_Date_End');
            lPreviousValues.Add(FieldByName('Vague_Date_Type').AsString);
            lChangedED.Add('Vague_Date_Type');
            FieldByName('Vague_Date_Start').Text          := eEventDate.Text;
          end;

          if EditMode = emAdd then
          begin
            FieldByName('Survey_Key').AsString       := SurveyKey;  // Survey key to link to survey table
            FieldByName('Survey_Event_Key').AsString := EventKey;   // New Event key
          end;
          Post;
          //Cascade changes with user permission if not in add mode
          if not (EditMode = emAdd) then
            if (lChangedED.Count > 0) then
              CascadeEventChanges(lChangedED, lPreviousValues, EventKey);
        end;
      end;
    end else
    if (EditMode = emEdit) and (lCustodian <> FCustodian) then begin
      FdmEvent.qryEvent.Cancel;
      MessageDlg(Format(ResStr_CustodyChanged, [ResStr_SurveyEvent]), mtWarning, [mbOk], 0);
    end;
    //Store Sample Recorder (SER Key and the Name key)
    with dmGeneralData.qryAllPurpose do begin
      SQL.Text := ' SELECT Distinct SR.SE_Recorder_Key,SER.Name_Key ' +
                  ' FROM Sample_Recorder SR ' +
                  ' INNER JOIN Survey_Event_Recorder SER  ON ' +
                  ' SR.SE_Recorder_Key = SEr.SE_Recorder_Key ' +
                  ' WHERE SER.Survey_Event_Key = ''' + EventKey +'''';
      Open;
        While not eof do begin
          lPreviousSR.Add(Fields[0].Value + '=' + Fields[1].Value);
          lPreviousSER.Add(Fields[0].Value);
        next;
        end;
      Close;
    end;
    // Event recorder
    FEventRecorderList.EventKey := EventKey;
    FEventRecorderList.Update;
    //Checks to see if any new Recorders. If so ask user if wishes to cascade these to the Sample Recorder
    //If option is chosen then all Recorders will be added to all samples not just the new Recorders.
    if Survey_Event_Recorder_Name_Added(EventKey,lPreviousSER) then begin
      if MessageDlg(ResStr_AddSampleRecorders, mtConfirmation,[mbNo,mbYes],0)=mrYes then
        CascadeRecorderToSamples(EventKey);
    end;
    //Cascade Determiner
    // Check to see if any changes have been made to Names for a SER key
    // If so then user may opt to Cascade these changes to the Determiner
    for i := 0 to lPreviousSR.Count-1 do
    begin
      if Survey_Event_Recorder_Name_Changed(lPreviousSR.Names[i],lPreviousSR.ValueFromIndex[i]) then begin
        if MessageDlg(ResStr_CascadeDeterminer, mtConfirmation,[mbNo,mbYes],0)=mrYes then
          CascadeDeterminer(lPreviousSR);
        break;
      end;
    end;

    // ownership tab
    if Assigned(FOwnershipGridManager) then
      FOwnershipGridManager.Save;
    // Sources
    Sources.Post;
    // Additional Pages
    SaveAdditionalPages;

    if DrillForm <> nil then
      with TfrmObservations(DrillForm) do begin
        SetItemTextAndKey(eEventDate.Text+' - '+fraLocationInfo.LocalityLabel,EventKey);
        tvObservations.Selected:=SelectedItem;
      end;
    EnableDetails(emView);
    DisplayRecord(EventKey);
  finally
    DefaultCursor(lCursor);
    FreeAndNil(lChangedED);
    FreeAndNil(lPreviousValues);
    FreeAndNil (lPreviousSR);
    FreeAndNil (lPreviousSER);
  end;
end;  // bbSaveClick
{-------------------------------------------------------------------------------

 Gets the list of all the samples under the parameter EventKey and ask the user
 to update the sample data according to the changes to event
}
procedure TfrmEventDetails.CascadeEventChanges(AChanges, APreviousValues: TStringList; EventKey: TKeyString);
var
  lDlgOption: TdlgCascade;
  lChangedDetails: string;
  j: integer;
  lChangeToList: TStringList;
begin
  if (AChanges.Count > 0) then
    lChangedDetails := TdmEvent.GetChangedCascadeFields(AChanges);  // call staic method GetChangedCascadeFields defined in EventData
  //Create the list of values to update to
  lChangeToList := TStringList.Create;
  for j:= 0 to AChanges.Count - 1 do
  begin
    lChangeToList.Add(GetEventValue(AChanges.Strings[j]));
  end;

  //Ask the user what to do
  if EventHasSamples then begin
    lDlgOption := TdlgCascade.CreateDialog(self, lChangedDetails, False, False);
    if (lDlgOption.ShowModal = mrOk) then
    begin
      if lDlgOption.ChosenOption = ctCascadeAll then
        //Pass it to UpdateSamples to update it in the database
        UpdateSamples(AChanges, lChangeToList, APreviousValues, true) //true means to set all the samples under that event
      else if lDlgOption.ChosenOption = ctCascadeEqual then
        UpdateSamples(AChanges, lChangeToList, APreviousValues, false); //false means to set only the equal samples under that event

      if lDlgOption.ChosenOption <> ctNothing then
        UpdateSamplesInInterface;
    end;    // if (lDlgOption.ShowModal = mrOk)
  end;    // if EventHasSamples
end;    // TfrmEventDetails.CascadeEventChanges

//==============================================================================
//  Returns true if the current event contains one or more samples
function TfrmEventDetails.EventHasSamples: Boolean;
var
  lExpanded: Boolean;
begin
  Result := False;
  if Assigned(DrillForm) then begin
    lExpanded := TfrmObservations (DrillForm).SelectedItem.Expanded;
    if not lExpanded then
      TfrmObservations (DrillForm).SelectedItem.Expand(false);
    Result := TfrmObservations (DrillForm).SelectedItem.Count > 0;
    if not lExpanded then
      TfrmObservations (DrillForm).SelectedItem.Collapse(False);
  end;    // if Assigned(DrillForm)
end;    // TfrmEventDetails.EventHasSamples

//==============================================================================
procedure TfrmEventDetails.UpdateSamplesInInterface;
var
  i: Integer;
begin
  if (TfrmObservations (DrillForm).SelectedItem.Expanded)
      or (TNodeObject(TfrmObservations (DrillForm).SelectedItem.Data).ChildrenPopulated) then
  begin
    with TfrmObservations(DrillForm) do begin
      //Freeing the child nodes of this event
      for i := 0 to SelectedItem.Count - 1 do
        //Index is always 0 here because when the item at 0 is freed, another sample takes its place
        SelectedItem.Item[0].Free;
      //Updating the Event in the interface by forcing a re-population
      TNodeObject(SelectedItem.Data).ChildrenPopulated := False;
      tvObservations.Items.AddChild(SelectedItem, '-');
      SelectedItem.Expand(false);
    end; // with TfrmObservations(DrillForm)
  end;
end;

//==============================================================================
function TfrmEventDetails.GetEventValue(AKey: string): string;
begin
  Result := FdmEvent.qryEvent.FieldByName(AKey).AsString;
end;

//==============================================================================
procedure TfrmEventDetails.UpdateSamples(AChanges, AChangeValues, APreviousValues: TStringList; ASetAll: boolean);
var
  i, Next: Integer;
begin
  Next := 0;
  if (ASetAll) then begin
    for i:= 0 to AChanges.Count - 1 do
      // Vague dates use three fields to store their data - set them all at once
      if i >= Next then
        if AnsiSameText(AChanges.Strings[i], 'Vague_Date_Start') then begin
          dmDatabase.RunStoredProc('usp_Sample_Cascade_' + AChanges.Strings[i] + '_FromEvent',
              ['@EventKey', FEventKey, '@StartValue', AChangeValues.Strings[i],
              '@PreviousStartValue', NULL, '@EndValue', AChangeValues.Strings[i + 1],
              '@PreviousEndValue', NULL, '@TypeValue', AChangeValues.Strings[i + 2],
              '@PreviousTypeValue', NULL, '@CurrentSample', NULL]);
          Next := i + 3;
        end else
          dmDatabase.RunStoredProc('usp_Sample_Cascade_' + AChanges.Strings[i] + '_FromEvent',
              ['@EventKey', FEventKey, '@Value', AChangeValues.Strings[i], '@PreviousValue',
              NULL, '@CurrentSample', NULL])
  end else begin
    for i:= 0 to AChanges.Count - 1 do
      // Vague dates use three fields to store their data - set them all at once
      if i >= Next then
        if AnsiSameText(AChanges.Strings[i], 'Vague_Date_Start') then begin
          dmDatabase.RunStoredProc('usp_Sample_Cascade_' + AChanges.Strings[i] + '_FromEvent',
              ['@EventKey', FEventKey, '@StartValue', AChangeValues.Strings[i],
              '@PreviousStartValue', APreviousValues.Strings[i], '@EndValue', AChangeValues.Strings[i + 1],
              '@PreviousEndValue', APreviousValues.Strings[i+1], '@TypeValue', AChangeValues.Strings[i + 2],
              '@PreviousTypeValue', APreviousValues.Strings[i + 2], '@CurrentSample', NULL]);
          Next := i + 3;
        end else
          dmDatabase.RunStoredProc('usp_Sample_Cascade_' + AChanges.Strings[i] + '_FromEvent',
              ['@EventKey', FEventKey, '@Value', AChangeValues.Strings[i], '@PreviousValue',
              APreviousValues.Strings[i], '@CurrentSample', NULL]);
  end;    // if (ASetAll)
end;



//==============================================================================
procedure TfrmEventDetails.bbCancelClick(Sender: TObject);
var tfDiscardNew:boolean;
begin
  inherited;
  tfDiscardNew:=FdmEvent.qryEvent.State=dsInsert;
  FdmEvent.qryEvent.Cancel;
  if Assigned(FOwnershipGridManager) then
    FOwnershipGridManager.Refresh;
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
      DisplayRecord(EventKey);
    end;
end;  // bbCancelClick

//==============================================================================
procedure TfrmEventDetails.EnableDetails(const NewMode:TEditMode);
var tfOn:boolean;
begin
  FEditMode:=NewMode;
  tfOn:=EditMode<>emView;

  Refreshcolours;
  fraLocationInfo.ReadOnly := not (FdmEvent.qryEvent.State in [dsEdit,dsInsert]);
  eEventDate.ReadOnly := fraLocationInfo.ReadOnly;

  with sgRecorders do
    if tfOn then Options:=Options-[goRowSelect]
            else Options:=Options+[goRowSelect];
  bbRecorderFind.Enabled  :=tfOn;
  bbRecorderAdd.Enabled   :=tfOn;
  bbRecorderReplace.Enabled :=tfOn;
  sgRecordersClick(nil);
  Sources.EditMode        :=NewMode;

  if tfOn then dbreComments.PopupMenu:=dmFormActions.pmRTF
          else dbreComments.PopupMenu:=nil;

  bbSave.Enabled  :=tfOn;
  bbCancel.Enabled:=tfOn;

  // Additional Pages
  SetAdditionalPagesState(tfOn);
  if Assigned(FOwnershipGridManager) then
    FOwnershipGridManager.Enabled := tfOn;

  if DrillForm<>nil then TfrmObservations(DrillForm).SetMenuState(not tfOn);
  cmbRecorderRole.Visible:=false;
end;  // EnableDetails

//==============================================================================
procedure TfrmEventDetails.SetupDestinationControls;
begin
  RegisterDropComponent(sgRecorders, DropRecorder,
                        ['NAME','INDIVIDUAL'], [CF_JNCCDATA, CF_TEXT]);
  fraLocationInfo.RegisterDragDrop(self);
end;  // SetupDestinationControls

//==============================================================================
procedure TfrmEventDetails.sgRecordersClick(Sender: TObject);
begin
  inherited;
  if FEventRecorderList.ItemCount=0 then
    bbRecorderRemove.Enabled:=false
  else
    with TEventRecorderItem(sgRecorders.Objects[0,sgRecorders.Row]) do
      bbRecorderRemove.Enabled := AppSettings.AllowEdit(EditMode) and CanDelete;
end;  // sgRecordersClick

//------------------------------------------------------------------------------
procedure TfrmEventDetails.sgRecordersDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var lDataItem:TEventRecorderItem;
begin
  inherited;
  with sgRecorders do begin
    if (EditMode<>emView) and (Cells[0,Row]<>'') then begin
      lDataItem:=TEventRecorderItem(Objects[0,Row]);
      // Make sure combobox appears when allowed OR when new item in grid
      if (AppSettings.AllowEdit(EditMode) and
          (lDataItem.Added or lDataItem.CanEdit))
          and (gdFocused in State) and (Col = 1) and (Row > 0) then
      begin
        cmbRecorderRole.Left     :=Rect.Left+Left+1;
        cmbRecorderRole.Top      :=Rect.Top+Top+1;
        cmbRecorderRole.Width    :=Rect.Right-Rect.Left+2;
        cmbRecorderRole.ItemIndex:=cmbRecorderRole.Items.IndexOf(Cells[Col,Row]);
        cmbRecorderRole.Visible  :=True;
      end else
        cmbRecorderRole.Visible:=false;
    end;
    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
  end;
end;  // sgRecordersDrawCell

//------------------------------------------------------------------------------
procedure TfrmEventDetails.sgRecordersTopLeftChanged(Sender: TObject);
begin
  inherited;
  cmbRecorderRole.Visible:=false;
end;  // sgRecordersTopLeftChanged

//------------------------------------------------------------------------------
procedure TfrmEventDetails.AddRecorder(const ANameKey:TKeyString);
var lstItem   :string;
    lDataItem:TEventRecorderItem;
begin
  lstItem:=dmGeneralData.GetIndividualName(ANameKey);
  // name not already in list, and name not an organisation
  if (sgRecorders.Cols[0].IndexOf(lstItem)=-1) and (lstItem<>'') then begin
    lDataItem:=TEventRecorderItem.CreateNew(FEventRecorderList);
    lDataItem.NameKey:=ANameKey;
    lDataItem.RoleKey:=TKeyData(cmbRecorderRole.Items.Objects[0]).ItemKey;
    FEventRecorderList.AddNew(lDataItem);
    // Focus on the row with the new recorder.
    sgRecorders.Row:=sgRecorders.RowCount-1;
    sgRecorders.Col:=0;
    sgRecordersClick(nil);
  end
  else if lstItem='' then // organisation so show message
    MessageDlg(ResStr_NoOrgRecorders, mtInformation, [mbOk], 0);
end;  // AddRecorder
//------------------------------------------------------------------------------
procedure TfrmEventDetails.ModifyRecorder(const ANameKey:TKeyString);
var lstItem   :string;
     var lDataItem:TEventRecorderItem;
 begin
  inherited;
  lstItem:=dmGeneralData.GetIndividualName(ANameKey);
  if (sgRecorders.Cols[0].IndexOf(lstItem)=-1) and (lstItem<>'') then begin
    lDataItem:=TEventRecorderItem(sgRecorders.Rows[sgRecorders.Row].Objects[0]);
    lDataItem.NameKey:=ANameKey;
  end
  else if lstItem='' then // organisation so show message
    MessageDlg(ResStr_NoOrgRecorders, mtInformation, [mbOk], 0);
end;  // AddRecorder
//------------------------------------------------------------------------------
procedure TfrmEventDetails.bbRecorderAddClick(Sender: TObject);
var
  lFind : TdlgFind;
begin
  lFind := TdlgFind.CreateDialog(nil,ResStr_FindRecorderName,ftIndividual);
  with lFind do begin
    try
      SetSearchText('',true);
      if not eSearchText.NoSourceItems then begin
        if ShowModal=mrOk then
          AddRecorder(ItemKey);
      end else
        MessageDlg(ResStr_NoIndividualItems, mtInformation, [mbOK], 0);
    finally
      Release;
    end;
  end;
end;  // bbRecorderAddClick

//------------------------------------------------------------------------------
function TfrmEventDetails.RecorderUsedInSample(const AKey:TKeyString):boolean;
begin
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text:='SELECT * FROM Sample_Recorder WHERE SE_Recorder_Key = '''+AKey+'''';
    Open;
    Result:=not Eof;
    Close;
  end;
end;  // RecorderUsedInSample
//------------------------------------------------------------------------------
function TfrmEventDetails.MultipleRecordersInSample(const AKey:TKeyString):boolean;
// Determine if all Sample with this Recorder have mutiple entries
// If not we can't delete the Recorder
var
  sampleKeys : TStringList;
  i : integer;
begin
  sampleKeys := TStringList.Create;
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := 'Select Sample_Key from Sample_Recorder WHERE SE_Recorder_Key = '''+AKey+'''';
    Open;
    While not eof do begin
      sampleKeys.Add(('''' + Fields[0].Value + ''''));
      Next;
    end;
    Close;
  end;
  Result := true;
  for i := 0 to sampleKeys.Count -1 do begin
    with dmGeneralData.qryAllPurpose do begin
      SQL.Text := 'Select count(*) as SRCount from Sample_Recorder WHERE ' +
                  ' Sample_Key = ' + sampleKeys[i] + ' AND SE_Recorder_Key <>  ''' + AKey+'''';
      Open;
      If Fields[0].Value = 0 then  Result := false;
      Close;
    end;
    if Result = false then Break
  end;
  sampleKeys.Free;

end;  // Multiple Sample

//------------------------------------------------------------------------------
procedure TfrmEventDetails.bbRecorderRemoveClick(Sender: TObject);
begin
  inherited;
  with sgRecorders do begin
    if not TEventRecorderItem(Objects[0,Row]).CanDelete then
      raise EEventDetailsError.CreateNonCritical(ResStr_NotAllowedToDeleteOtherPersonsData);
    if not RecorderUsedInSample(TEventRecorderItem(Objects[0,Row]).ItemKey) then begin
      if MessageDlg(ResStr_DeleteRecord,
                    mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
        FEventRecorderList.DeleteItem(Row);
        cmbRecorderRole.Visible:=false;
        sgRecordersClick(nil);
      end;
    end else
      If MultipleRecordersInSample(TEventRecorderItem(Objects[0,Row]).ItemKey) then begin
        if MessageDlg(ResStr_DeleteRecord,
                    mtConfirmation,[mbNo,mbYes],0)=mrYes then begin
          FEventRecorderList.DeleteItem(Row);
          cmbRecorderRole.Visible:=false;
          sgRecordersClick(nil);
        end;
      end
      else
        MessageDlg(ResStr_SampleNoRecorders, mtInformation, [mbOK], 0);

  end;
end;  // bbRecorderRemoveClick

//---------------------------------------------------------------------------
procedure TfrmEventDetails.bbRecorderReplaceClick(Sender: TObject);
begin
  inherited;
  with sgRecorders do begin
    if not TEventRecorderItem(Objects[0,Row]).CanDelete then
      raise EEventDetailsError.CreateNonCritical(ResStr_NotAllowedToDeleteOtherPersonsData);
  end;
  dmFormActions.actNames.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, ChangeRecorders);
end;  // bbRecorderReplaceClick

//------------------------------------------------------------------------------
procedure TfrmEventDetails.bbRecorderFindClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actNames.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateRecorders);
end;  // bbRecorderFindClick

//------------------------------------------------------------------------------
procedure TfrmEventDetails.UpdateRecorders(KeyList:TKeyList);
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then
      AddRecorder(KeyList.Items[0].KeyField1);
  finally
    KeyList.Free;
  end;
end;  // UpdateRecorders
//------------------------------------------------------------------------------
procedure TfrmEventDetails.ChangeRecorders(KeyList:TKeyList);
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then
      ModifyRecorder(KeyList.Items[0].KeyField1);
  finally
    KeyList.Free;
  end;
end;  // UpdateRecorders

//------------------------------------------------------------------------------
procedure TfrmEventDetails.DropRecorder(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
var lCount:integer;
begin
  if EditMode<>emView then begin
    if iFormat=CF_JNCCDATA then begin
      ioHandled:=true;
      for lCount:=0 to iSourceData.Header.ItemCount-1 do
        if CompareText(iSourceData.Items[lCount].KeyField2,'Individual')=0 then
          AddRecorder(iSourceData.Items[lCount].KeyField1);
    end;
  end else
    ioHandled:=true;
end;  // DropRecorder

//------------------------------------------------------------------------------
procedure TfrmEventDetails.cmbRecorderRoleExit(Sender: TObject);
begin
  inherited;
  cmbRecorderRole.Visible:=false;
end;  // cmbRecorderRoleExit

//------------------------------------------------------------------------------
procedure TfrmEventDetails.cmbRecorderRoleChange(Sender: TObject);
var lDataItem:TEventRecorderItem;
begin
  inherited;
  lDataItem:=TEventRecorderItem(sgRecorders.Rows[sgRecorders.Row].Objects[0]);
  lDataItem.RoleKey:=TKeyData(cmbRecorderRole.Items.Objects[cmbRecorderRole.ItemIndex]).ItemKey;
end;  // cmbRecorderRoleChange

//==============================================================================
procedure TfrmEventDetails.SetDrillForm(const Value: TBaseForm);
begin
  FDrillForm := Value;
end;  // SetDrillForm

//==============================================================================
procedure TfrmEventDetails.WMTransferDone(var Msg: TMessage);
begin
  if DrillForm<>nil then TfrmObservations(DrillForm).Show;
end;  // WMTransferDone

//==============================================================================
procedure TfrmEventDetails.UpdateRTFMenu;
begin
  if Assigned(DrillForm) then
    dmFormActions.UpdateRTFMenu((DrillForm.ActiveControl is TDBRichEdit) and
                             (FdmEvent.qryEvent.State in [dsEdit,dsInsert]));
end;  // UpdateRTFMenu

//==============================================================================
procedure TfrmEventDetails.EnterRTF(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(FdmEvent.qryEvent.State in [dsEdit,dsInsert]);
end;  // EnterRTF

//==============================================================================
procedure TfrmEventDetails.ExitRTF(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(false);
end;  // ExitRTF

//==============================================================================
procedure TfrmEventDetails.pnlDetailsResize(Sender: TObject);
var
  deltaWidth, deltaHeight, availableHeight, thirdHeight : Integer;
begin
  inherited;
  lblEvent.Caption:=GetTextWithinLimit(Canvas, fraLocationInfo.eLocation.Text,
                                       pnlDetails.Width - lblEvent.Left - 8);
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

  availableHeight := pnlCommentsRecorders.Height - 4;
  thirdHeight := availableHeight div 3;
  pnlComment.Height := thirdHeight;
  pnlRecorders.Top := pnlComment.Top + pnlComment.Height + 4;
  pnlRecorders.Height := thirdHeight * 2;
end;  // pnlDetailsResize

//==============================================================================
{ Builds the key list to return data }
function TfrmEventDetails.GetKeyList: TKeyList;
var lNewKeyList:TEditableKeyList;
begin
  //Return an editable key list with the selected nodes key
  lNewKeyList:= TEditableKeyList.Create;
  lNewKeyList.SetTable('SURVEY_EVENT');
 	lNewKeyList.AddItem(EventKey,'');
  Result:= lNewKeyList;
end;  // GetKeyList

//==============================================================================
procedure TfrmEventDetails.SetDisplaySystem;
begin
  fraLocationInfo.eSpatialRef.DisplaySystem := AppSettings.SpatialRefSystem;
end;

//==============================================================================
procedure TfrmEventDetails.pcEventDetailsChange(Sender: TObject);
begin
  inherited;
  pcEventDetails.HelpContext := pcEventDetails.ActivePage.HelpContext;
end;

//==============================================================================
procedure TfrmEventDetails.RefreshEventLocation;
begin
  fraLocationInfo.eLocation.Text := dmGeneralData.GetLocationName(
      fraLocationInfo.eLocation.Key);
end;

{-------------------------------------------------------------------------------
  Refresh the record, because another screen has updated the spatial ref.
}
procedure TfrmEventDetails.RefreshSpatialRef;
begin
  if EditMode=emView then
    DisplayRecord(FEventKey)
end;

//==============================================================================
procedure TfrmEventDetails.RefreshNames;
var liIndex: integer;
    lCurrItem : TEventRecorderItem;
begin
  with FEventRecorderList do
    for liIndex := 0 to Count-1 do begin
      lCurrItem := TEventRecorderItem(Items[liIndex]);
      RefreshItemDisplay(lCurrItem);
    end;
end;

//==============================================================================
procedure TfrmEventDetails.RefreshColours;
begin
  SetRequiredFieldsColourState(EditMode <> emView, [eEventDate, sgRecorders]);
  if EditMode <> emView then
    fraLocationInfo.SetColour(MergeColours(AppSettings.MandatoryColour, clWindow, 25))
  else
    fraLocationInfo.SetColour(clWindow);
end;

//==============================================================================
procedure TfrmEventDetails.ShowMetadata;
begin
  with TdlgMetaDataPopup.Create(nil) do
  try
    ShowStandard(ResStr_Event, lblEvent.Caption,
       FdmEvent.qryEvent.FieldByName('Survey_Event_Key').AsString,
       FdmEvent.qryEvent);
  finally
    Free;
  end;
end;

//==============================================================================
{ Display the map window panned to show an event }
procedure TfrmEventDetails.FindOnMap;
begin
  fraLocationInfo.FindOnMap;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmEventDetails.UpdateMapWindowSelector;
begin
  fraLocationInfo.UpdateMapWindowSelector;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmEventDetails.eEventDateExit(Sender: TObject);
var
  lPos: TPoint;
begin
  lPos := bbCancel.ScreenToClient(Mouse.CursorPos);
  if not (((lPos.X in [0..bbCancel.Width]) and (lPos.Y in [0..bbCancel.Height])) or FClosingForm) then
  begin
    ValidateValue(IsVagueDate(eEventDate.Text),
                 ResStr_InvalidVagueDate, eEventDate);
    ValidateValue(CheckVagueDate(eEventDate.Text),
                  InvalidDate(ResStr_EventDate, True, False), eEventDate);
    eEventDate.Text := VagueDateToString(eEventDate.VagueDate);
  end;
end;

{-------------------------------------------------------------------------------
  Set up the ownership grid when the tab is first shown
}
procedure TfrmEventDetails.tsOwnershipShow(Sender: TObject);
var
  lRow: integer;
  lColumnWidth: integer;
const
  OWNERSHIP_KEY = 'Survey_Event_Owner_Type_Key';
begin
  inherited;
  if (not FOwnershipPopulated) and Assigned(FdmEvent) then begin
    // Clear the grid
    for lRow := 1 to sgOwners.RowCount - 1 do
      sgOwners.Rows[lRow].CommaText := '';

    with FdmEvent.qryOwnership do begin
      Close;
      Sql.Text := 'exec usp_SurveyEventOwners_Select_ForSurveyEvent ''' + FEventKey + '''';
      lColumnWidth := (sgOwners.Width - GetSystemMetrics(SM_CXVSCROLL)) div 2;
      FdmEvent.qryOwnershipShort_Name.DisplayWidth := lColumnWidth;
      FdmEvent.qryOwnershipDisplayName.DisplayWidth := lColumnWidth;
      Open;
    end;
    if not Assigned(FOwnershipGridManager) then begin
      FOwnershipGridManager := TDataStringGrid.Create(sgOwners, FdmEvent.qryOwnership,
                                                      'Survey_Event_Owner_Key');
      with FOwnershipGridManager do begin
        OnDeleteRow := DeleteOwnership;
        OnUpdateRow := UpdateOwnership;
        ImageList   := dmFormActions.ilButtons;
        GetButtonImageIndex := 5;
        // CCN248 - F11 hotkey inserts current user
        SetupLinkedEdit('Name_Key', 0, FindSurveyEventOwnerName, GetSurveyEventOwnerName,
            AppSettings.UserId, AppSettings.UserName);
        // Enable locking of rows belonging to different custodian
        SiteID := AppSettings.SiteID;
        UserID := AppSettings.UserId;
        RestrictFullEdit := AppSettings.RestrictFullEdit;
        AddOnly := AppSettings.UserAccessLevel <= ualAddOnly;
        PopulateGrid;
        Enabled := EditMode <> emView;
      end;
      TOwnersGrid(sgOwners).OwnershipGridManager := FOwnershipGridManager;
    end else
      FOwnershipGridManager.Refresh;
    FOwnershipPopulated := True;
  end; // if
end;

{-------------------------------------------------------------------------------
  Find dialog handler for event owners grid
}
procedure TfrmEventDetails.FindSurveyEventOwnerName(const AInitialText: string;
    var AText, AKey: string);
var
  lKey: TKeyString;
begin
  // set key as a keystring to pass correct param type
  lKey := AKey;
  dmGeneralData.CheckName(AInitialText, lKey, AText);
  AKey := lKey;
end;

{-------------------------------------------------------------------------------
  Initialise a return data linked for names and the Event Owners grid
}
procedure TfrmEventDetails.GetSurveyEventOwnerName;
begin
  dmFormActions.actNames.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateEventOwner);
end;


{-------------------------------------------------------------------------------
  When the departments data string grid notifies a deletion event, record the
      key so that the record can be deleted when the data is saved.
}
procedure TfrmEventDetails.DeleteOwnership(ioRowKey: string);
begin
  dmDatabase.RunStoredProc('usp_SurveyEventOwner_Delete', ['@Key', ioRowKey]);
end;

{-------------------------------------------------------------------------------
  Update a survey event ownership record.
}
procedure TfrmEventDetails.UpdateOwnership(var ioRowKey: string;
  iData: TStringList);
begin
  if ioRowKey='' then
    ioRowKey := dmDatabase.GetStoredProcOutputParam('usp_SurveyEventOwner_Insert',
        ['@SurveyEventKey', FEventKey,
        '@NameKey', iData.Values['Name_Key'],
        '@SurveyEventOwnerTypeKey', iData.Values['Survey_Event_Owner_Type_Key'],
        '@EnteredBy', AppSettings.UserID],
        '@Key')
  else
    dmDatabase.RunStoredProc('usp_SurveyEventOwner_Update',
        ['@SurveyEventOwnerKey', ioRowKey,
        '@NameKey', iData.Values['Name_Key'],
        '@SurveyEventOwnerTypeKey', iData.Values['Survey_Event_Owner_Type_Key'],
        '@ChangedBy', AppSettings.UserID]);
end;

{-------------------------------------------------------------------------------
  Return data handler for Event Owners grid
}
procedure TfrmEventDetails.UpdateEventOwner(KeyList: TKeyList);
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then
      FOwnershipGridManager.UpdateLinkedValue(
          dmGeneralData.GetName(Keylist.Items[0].KeyField1),
          KeyList.Items[0].KeyField1);
  finally
    KeyList.Free;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmEventDetails.ValidateOwnershipNames;
var
  lRow: Integer;
  lName: String;
  lKey: TKeyString;
begin
  with sgOwners do
    for lRow := RowCount - 1 downto 1 do begin
      if TOwnersGrid(sgOwners).OwnershipGridManager.RowContainsData(lRow) then begin
        lName := Cells[0, lRow];
        // Make sure there is a key there
        if not Assigned(Objects[0, lRow]) then
          Objects[0, lRow] := TLinkedKey.Create;
        lKey := TLinkedKey(Objects[0, lRow]).Key;
        if lKey='' then
          ValidateValue(dmGeneralData.CheckName(lName, lKey, lName),
                     Format(ResStr_InvalidOwnerName, [Cells[0, lRow]]), sgOwners);
        // If validation fails, method bombs out before the next lines.
        Cells[0, lRow] := lName;
        TLinkedKey(Objects[0, lRow]).Key := lKey;
      end else begin
        TOwnersGrid(sgOwners).Row := lRow;
        TOwnersGrid(sgOwners).OwnershipGridManager.DeleteFromGrid(nil);
      end;
    end;
end;

{ TOwnersGrid }

{-------------------------------------------------------------------------------
  When the user resizes a col, make sure the edit control repositions
}
procedure TOwnersGrid.ColWidthsChanged;
begin
  inherited;
  if Assigned(FOwnershipGridManager) then
    FOwnershipGridManager.PositionEditControl(Col, Row);
end;


{-------------------------------------------------------------------------------
}
procedure TOwnersGrid.SetOwnershipGridManager(
  const Value: TDataStringGrid);
begin
  FOwnershipGridManager := Value;
end;

{-------------------------------------------------------------------------------
  Destroy objects
}
destructor TfrmEventDetails.Destroy;
begin
  FreeObjects;
  inherited;
end;

procedure TfrmEventDetails.SpatialRefInvalidSpatialRef(Sender: TObject;
  var Handled: Boolean);
var
  lPos: TPoint;
begin
  lPos := bbCancel.ScreenToClient(Mouse.CursorPos);
  // Check if clicked Cancel
  Handled := (lPos.X in [0..bbCancel.Width]) and
      (lPos.Y in [0..bbCancel.Height]) or FClosingForm;
end;

{-------------------------------------------------------------------------------
  Click handler for map menu items on the spatial reference control
}
procedure TfrmEventDetails.fraLocationInfoeSpatialRefChange(
  Sender: TObject);
begin
  inherited;
  fraLocationInfo.DataChange(Sender);

end;

{-------------------------------------------------------------------------------
  Handle F11 keypress to fill in current user
}
procedure TfrmEventDetails.sgRecordersKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key=VK_F11 then
    AddRecorder(AppSettings.UserId);
end;

{-------------------------------------------------------------------------------
  Cascades a Survey Event Recorders into all samples for the event
}
procedure TfrmEventDetails.CascadeRecorderToSamples(const ASEKey: string);
begin
 dmDatabase.RunStoredProc('usp_Cascade_Sample_Recorders', ['@Key', ASEKey]);
end;
{-------------------------------------------------------------------------------
  Cascades any changed Survey Event Recorders into the TDET determiner
}
procedure TfrmEventDetails.CascadeDeterminer(const AKeyList: TStringList );
var i:integer;
begin
  for i := 0 to AKeYList.Count-1 do
    dmDatabase.RunStoredProc('usp_Cascade_To_Determination', ['@SERKey', AKeyList.Names[i],'@NameKey',AKeyList.ValueFromIndex[i]]);
end;
//==============================================================================
function TfrmEventDetails.Survey_Event_Recorder_Name_Changed(const ASERKey,ANameKey: string ): boolean;
begin
  // A bit of a fudge here to avoid using 'EXISTS' so that dmGeneralData.qryAllPurpose can be used
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := ' SELECT * FROM Survey_Event_Recorder SER ' +
                ' WHERE SER.SE_Recorder_Key = ''' + ASERKey +'''' +
                ' AND SER.Name_key <> ''' + ANameKey + '''' +
                ' AND 1 = (Select 1 from Survey_Event_Recorder WHERE ' +
                ' SE_Recorder_Key = ''' + ASERKey + ''')';
    Open;
      Result := not eof;
    Close;
  end;
end;

//==============================================================================
function TfrmEventDetails.Survey_Event_Recorder_Name_Added(const ASEKey: string; AStringList:TStringList): boolean;
begin
  // Dont offer option if there are no samples
  Result := false;
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := ' SELECT SE_Recorder_Key FROM Survey_Event_Recorder  ' +
                ' INNER JOIN SAMPLE  on SAMPLE.Survey_Event_Key' +
                ' = Survey_Event_Recorder.Survey_Event_Key ' +
                ' WHERE Survey_Event_Recorder.Survey_Event_key = ''' + ASEKey +'''';
    Open;
      while not eof do begin
       if AStringList.IndexOf(Fields[0].Value) = -1 then begin
         Result:=true;
       end;
       next;
      end;
    Close;
  end;
end;
end.
