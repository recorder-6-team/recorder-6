//==============================================================================
//  Unit:        DataExport
//
//  Implements:  TdlgDataExport
//
//  Description: Allows users to export data as any of the installed formats.
//               NBN Data(XML) and DMAP are the formats provided in this build,
//               but the software can be configured to export data to any format
//               that supports the software's Export COM interface.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 69 $
//    $Date: 1/10/09 9:48 $
//    $Author: Simonlewis $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit DataExport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  Buttons, StdCtrls, ExtCtrls, ApplicationSettings, COMClasses, GeneralFunctions,
  JNCCDatasets, DataClasses, OnlineHelp, ThreadedDataOutput, ThreadedDatabaseOutput,
  SQLConstants, ImageListButton, ADODB, DatabaseAccessADO, ComCtrls, ExportFilters;

resourcestring
  ResStr_ExportingConfidentialOccurrences =
      'Please note that the option to export '
      + 'confidential occurrences is enabled so this export file may contain '
      + 'confidential information.';
  ResStr_ExportingNBNData = 'Exporting NBN Data...';
  ResStr_TodaysDataOnly = 'This will only export data that has been entered or modified today!';
  ResStr_SelectedDataAllInvalid =
      'The export has been aborted because all the selected data was invalid.';
  ResStr_TempSurveysNothingToExport =
      'You are trying to export surveys marked as temporary. Temporary survey data cannot be exported.';
  ResStr_TempSurveysSomethingToExport =
      'You are trying to export some records from surveys marked as temporary. Temporary survey data cannot be exported '+
          'and will be excluded from the export.';

type
  { Enum for each filter mode available, only applicable when exporting from a
       filter }
  TFilterSetting = (fsSinceLastExport, fsSinceDate, fsAll);

  TdlgDataExport = class(TForm)
    bvlFrame: TBevel;
    lblExportType: TLabel;
    cmbExportType: TComboBox;
    lblDestination: TLabel;
    eDestination: TEdit;
    dlgSave: TSaveDialog;
    cbIncludeObservations: TCheckBox;
    cbIncludeSubSites: TCheckBox;
    bbOK: TImageListButton;
    bbCancel: TImageListButton;
    rgExportFilterSettings: TRadioGroup;
    dtpFilterDate: TDateTimePicker;
    cbReassignCustody: TCheckBox;
    bbExportTo: TButton;
    cbNoValidation: TCheckBox;
    cbExportPrivate: TCheckBox;
    cbExportAddresses: TCheckBox;
    lblExportConfidential: TLabel;
    procedure bbCancelClick(Sender: TObject);
    procedure bbExportToClick(Sender: TObject);
    procedure bbOkClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure cmbExportTypeChange(Sender: TObject);
    procedure eDestinationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rgExportFilterSettingsClick(Sender: TObject);
  private
    hlpExport: TOnlineHelp;
    FCancelled, FHideCheckBox:boolean;
    FKeyList : TEditableKeyList;
    FExportIntf : IUnknown;
    FDataOutput : TThreadedDataOutput;
    FDataBaseOutput: TThreadedDataBaseOutput;
    FFilter: TExportFilter;
    FFullFilePath: String;
    FExcludeGroup: String;
    FKeyListCreated: Boolean;
    FValidationKeyList: TKeyList;
    FOldCursor: TCursor;
    FKeyListFilter: TEditableKeyList;
    FInitialList: TEditableKeyList;
    FValidationMessages: TStringList;
    FDataOutputCompleteRun: Boolean;
    FExportStart: TDateTime;
    procedure PopulateExportList;
    procedure DoNBNExport(const AExportType: String; AInvalidKeys: TKeyList; AFromExportFilter: Boolean);
    function BuildCOMKeyList(AKeyList: TKeyList): TCOMKeyList;
    procedure CheckColumnSelectionRequired;
    function CheckMetadata : boolean;
    function GetSubsitesVisible: boolean;
    procedure SetCheckBoxPositions;
    procedure SetCustodyCheckBox;
    procedure PickupSubsites;
    procedure PickupSurveys;
    procedure ConstructorStuff;
    function FilterSetting : TFilterSetting;
    function FilterDate : TDateTime;
    function NeedAFilter: Boolean;
    procedure ValidateAndExport(const exportType: String);
    procedure DataOutputComplete(Sender: TObject);
    procedure ShowInvalidRecords;
    procedure ExcludeTemporarySurveys;
    procedure EnableDisableCustody(EnableDisable : boolean);
  public
    constructor Create(AOwner : TComponent; AKeyList, AValidationKeyList: TKeyList;
        AHideCheckBox : boolean); reintroduce; overload;
    constructor Create(AOwner: TComponent; AFilter:TExportFilter); reintroduce; overload;
    destructor Destroy; override;
    property FullFilePath: String read FFullFilePath;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  Maintbar, ExportField, ComObj, Recorder2000_TLB, Metadata, GeneralData,
  FormActions, ExceptionForm, Constants, CustodyTransferExport, ExternalFilter,
  FilterResult;

const
  NBN_DATA_EXPORT     = 'NBN Data';
  NBN_EXT             = 'xml';
  NBN_DATABASE_EXPORT = 'NBN Access Database (zipped)';
  NBN_DB_EXT          = 'zip';

  ALL_FILES_FILTER    = '|All files|*.*';

resourcestring
  SDataExport_FindingInformationToExport = 'Finding information to export...';

  ResStr_FileExists      = 'The file selected already exists.  Do you want to replace it?';
  ResStr_NoFilter        = 'Request for Filter Date failed - there is no filter available';
  ResStr_NothingToExport = 'No records were identified using the filter.  Export aborted.';
  ResStr_SinceLastExport = 'Export records changed since day of last export using filter';
  ResStr_SinceDate       = 'Export records changed since';
  ResStr_All             = 'Export all records for filter';
  ResStr_SelectDataColumnForExport = 'Select Data Column for %s Export';
  ResStr_Exporting = 'Exporting Data (Press ESC to cancel)';
  ResStr_DisplayingInvalidRecordsForReview =
      'The records that were excluded from the export because '
      + 'they were invalid are now being displayed for you to review.';
  ResStr_ExportAborted = 'Export aborted.';
  ResStr_NoDataForCustodyReassignment =
      'No data was found to which you have the right to re-assign custodianship.'#13#10
      + 'Do you want to proceed anyway?';
  ResStr_ExportConfidential =  'Confidential records will be exported ';
  ResStr_NotExportConfidential = 'Confidential records will not be exported ';
//==============================================================================
{ Constructor - stores the input key list for later use }
constructor TdlgDataExport.Create(AOwner : TComponent; AKeyList,
    AValidationKeyList: TKeyList; AHideCheckBox : boolean);
var lCursor: TCursor;
begin
  inherited Create(AOwner);
  rgExportFilterSettings.Visible := False;
  Height := Height - rgExportFilterSettings.Height - 8; // Form is large in design mode to include filter date stuff
  { We must have an editable key list because of the way NBN export works,
  but the input key list can be any sort }
  FKeyListCreated := not (AKeyList is TEditableKeylist);
  if AKeyList is TEditableKeyList then
    FKeyList := TEditableKeyList(AKeyList)
  else begin
    FKeyList := TEditableKeyList.Create;
    // There can be a lot of keys to transfer from one list to the other
    lCursor := HourglassCursor;
    try
      FKeyList.Assign(AKeyList);
    finally
      DefaultCursor(lCursor);
    end;
  end;
  FValidationKeyList := AValidationKeyList;
  FHideCheckBox := AHideCheckBox;
  cbIncludeObservations.Visible := not FHideCheckBox;
  cbIncludeSubsites.Visible := GetSubsitesVisible;
  ExcludeTemporarySurveys;
  ConstructorStuff;
end;  // Create

//==============================================================================
{ Constructor used when exporting using an Export Filter }
constructor TdlgDataExport.Create(AOwner: TComponent; AFilter: TExportFilter);
begin
  inherited Create(AOwner);
  FKeyListCreated := false;
  Caption := Caption + ' (' + AFilter.FilterName + ')';
  rgExportFilterSettings.Visible := True;
  dtpFilterDate.Visible := True;
  FFilter := AFilter;
  FFilter.UserAccessLevel := AppSettings.UserAccessLevel;
  FFilter.ExportConfidentialOccurrences := AppSettings.ExportConfidentialOccurrences;
  FHideCheckbox := True;
  cbIncludeObservations.Visible := False;
  cbIncludeSubsites.Visible := False;
  dtpFilterDate.DateTime := Date;

  if FFilter.HasLastExportDate then begin
    rgExportFilterSettings.Items.Add(ResStr_SinceLastExport);
    dtpFilterDate.Top := rgExportFilterSettings.Top+40;
  end else
    dtpFilterDate.Top := rgExportFilterSettings.Top+22;

  rgExportFilterSettings.Items.Add(ResStr_SinceDate);
  rgExportFilterSettings.Items.Add(ResStr_All);
  rgExportFilterSettings.ItemIndex := 0;
  ConstructorStuff;
end;

(**
 * Removes any surveys of type temporary_Survey from the export if explicitly included.
 *)
procedure TdlgDataExport.ExcludeTemporarySurveys;
var
  i: integer;
  surveyKeys: string;
  surveyCount, deleted: integer;
  rs: _Recordset;
  
begin
  if (CompareText(FKeyList.Header.TableName, 'SURVEY')=0) or (CompareText(FKeyList.Header.TableName, 'MIXED')=0) then begin
    surveyCount := 0;
    deleted := 0;
    surveyKeys := '';
    for i:=0 to FKeyList.Header.ItemCount-1 do begin
      if (FKeyList.Items[i].KeyField2='') or (CompareText(FKeyList.Items[i].KeyField2, 'SURVEY')=0) then begin
        if surveyKeys<>'' then
          surveyKeys := surveyKeys + ',';
        surveyKeys := surveyKeys + '''' + FKeyList.Items[i].KeyField1 + '''';
        surveyCount := surveyCount+1;
      end;
    end;
    if surveyCount>0 then begin
      rs := dmDatabase.ExecuteSQL('SELECT Survey_Key, Temporary_Survey FROM Survey WHERE Survey_Key IN (' +
          surveyKeys + ') AND Temporary_Survey = 1', true);
      if rs.RecordCount>0 then begin
        rs.MoveFirst;
        while not rs.EOF do begin
          for i:=0 to FKeyList.Header.ItemCount-1 do begin
            if ((FKeyList.Items[i].KeyField2='') or (CompareText(FKeyList.Items[i].KeyField2, 'SURVEY')=0)) and
               (FKeyList.Items[i].KeyField1=rs.Fields[0].Value) then begin
                FKeyList.DeleteItem(i);
                deleted := deleted + 1;
            end;
          end;
          rs.MoveNext;
        end;
        if surveyCount-deleted=0 then
          raise TExceptionPath.CreateNonCritical(ResStr_TempSurveysNothingToExport)
        else if deleted>0 then
          ShowInformation(ResStr_TempSurveysSomethingToExport);
      end;
    end;
  end;

end;

{ Common code to both constructors }
procedure TdlgDataExport.ConstructorStuff;
begin
  SetCheckBoxPositions;
  SetCustodyCheckBox;
  PopulateExportList;
  with cmbExportType do
    ItemIndex:=Items.IndexOf(NBN_DATA_EXPORT);
  { Make sure this selection is handled correctly }
  cmbExportTypeChange(Self);
  FCancelled:=false;
  FFullFilePath := '';
end;

//==============================================================================
procedure TdlgDataExport.FormCreate(Sender: TObject);
begin
  hlpExport := TOnlineHelp.Create(Self.Handle);
  OnHelp := hlpExport.OnHelpReplacement;
  HelpContext := IDH_DATAEXPORT;
end;  // FormCreate

//==============================================================================
procedure TdlgDataExport.FormDestroy(Sender: TObject);
begin
  hlpExport.Free;
end;  // FormDestroy

//==============================================================================
{ Destructor just does some cleanup }
destructor TdlgDataExport.Destroy;
begin
  if (not FDataOutputCompleteRun) then begin
    // Ensure that the if DataOutputComplete method is called once per export
    //  If the export is cancelled, this destructor gets called before the
    //  thread's OnTerminate event is raised.
    if Assigned(FDataOutput) then begin
      FDataOutput.OnTerminate := nil;
      DataOutputComplete(Self);
    end;    // if Assigned(FDataOutput)
    if Assigned(FDataBaseOutput) then begin
      FDataBaseOutput.OnTerminate := nil;
      DataOutputComplete(Self);
    end;    // if Assigned(FDataBaseOutput)
  end;    // if (not FDataOutputCompleteRun)

  if FKeyListCreated then
    FKeyList.Free;
  inherited Destroy;
end;

//==============================================================================
{ Determine if the subsites checkbox should display - depends on the presence of
    a location in the export key list }
function TdlgDataExport.GetSubsitesVisible : boolean;
var
  i : integer;
begin
  Result := False;
  for i := 0 to FKeyList.Header.ItemCount - 1 do
  begin
    if SameText(FKeyList.ItemTable[i], TN_LOCATION) then begin
      Result := True;
      Break; // from loop
    end;
  end;
end;

//==============================================================================
{ Call the save file dialog to allow selection of a file/directory. }
procedure TdlgDataExport.bbExportToClick(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    eDestination.Text:=dlgSave.FileName;
    bbOk.SetFocus;
  end;
  if GetDriveType(PChar(GetCurrentDir))=DRIVE_REMOVABLE then
    SetCurrentDir(ExtractFilePath(Application.Exename));
end;  // bbExportToClick

//==============================================================================
procedure TdlgDataExport.bbCancelClick(Sender: TObject);
begin
  if FDataOutput <> nil then
    FDataOutput.Cancelled := True;
  if FDataBaseOutput <> nil then
    FDataBaseOutput.Cancelled := true;

  FFullFilePath := '';
  frmMain.TaskFinished;
  FCancelled:=true;
  Close;
end;  // bbCancelClick

//==============================================================================
procedure TdlgDataExport.bbOkClick(Sender: TObject);
var
   lstItemName: String;
begin

  lstItemName := cmbExportType.Items[cmbExportType.ItemIndex];
  { if the file has no extension, then give it the default }
  if (ExtractFileExt(eDestination.Text)='') then
    if lstItemName = NBN_DATA_EXPORT then
      eDestination.Text := eDestination.Text + '.' + NBN_EXT
    else
    if lstItemName = NBN_DATABASE_EXPORT then
      eDestination.Text := eDestination.Text + '.' + NBN_DB_EXT;

  FFullFilePath := eDestination.Text;

  if cbExportAddresses.checked = false then FExcludeGroup := 'A'
    else FExcludeGroup := '';

  { Confirm on replace }
  if FileExists(eDestination.Text) then
    if MessageDlg(ResStr_FileExists, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    begin
      ModalResult := mrNone;
      Exit; // from procedure without doing anything
    end;

  if AppSettings.ExportConfidentialOccurrences then
    ShowInformation(ResStr_ExportingConfidentialOccurrences);
  frmMain.SetStatus(SDataExport_FindingInformationToExport);
  FExportStart := Now; // trap date/time before export starts, so no record are ever missed
  try
    {If exporting using an Export Filter, then FKeyList is yet nil}
    if not Assigned(FFilter) or NeedAFilter then begin
      // Add subsites, in case reassigning custody, we want them all now!
      PickupSubsites;
      // Add Survey for Selected Survey tag (Concept)
      PickupSurveys;
      // This is no longer run here
      //if cbReassignCustody.Checked then CustodyWantsReassigning;
      // Custody is reassigned in DataBaseExport
      // Validate selected data, and only proceed with export if all ok.
      ValidateAndExport(lstItemName);
    end;
  finally
    frmMain.SetStatus('');
  end;
end;  // bbOKClick

//==============================================================================
{ If required, then find surveys for survey tags
    and add the survey keys to the export list }
procedure TdlgDataExport.PickupSurveys;
  //----------------------------------------------------------------------------
Var
  i : integer;
  Concept_rs : _Recordset;
begin
  for i := 0 to FKeyList.Header.ItemCount - 1 do begin
    if SameText(FKeyList.ItemTable[i], TN_Concept) then begin
      Concept_rs:= dmDatabase.ExecuteSQL('SELECT Survey_Key FROM Survey_Tag ' +
        ' Where Concept_Key = ''' + FKeyList.items[i].KeyField1 + '''',true);
      FKeyList.ConvertToMixedData;
      if Concept_rs.RecordCount>0 then begin
        Concept_rs.MoveFirst;
        while not  Concept_rs.EOF do begin
          FKeyList.AddItem(Concept_rs.Fields['Survey_Key'].Value,'SURVEY');
          Concept_rs.MoveNext;
        end;
      end;
    end;
  end;
end;  // PickupSurveys

//==============================================================================
{ If required, then find subsites of each location in the key list and add them
    to the export list }
procedure TdlgDataExport.PickupSubsites;
var
  lInitialCount, i : integer;
  //----------------------------------------------------------------------------
  procedure AddSubSiteKeys(const iLocationKey: String);
  var
    lqrySubSiteKeys:TADOQuery;
  begin
    lqrySubSiteKeys := TADOQuery.Create(nil);
    try
      dmDatabase.SetDatabaseLocal([lqrySubSiteKeys]);
      // Find its sub sites, if any
      with lqrySubSiteKeys do begin
        // Set the query properties
        SetStandardQuery(lqrySubsiteKeys, ssLocationChildren, iLocationKey, AppSettings.UserId);
        Open;
        FKeylist.AddQueryResults(lqrySubSiteKeys);
        First;
        while not Eof do begin
          // Get all sub sites for this sub site
          AddSubSiteKeys(FieldByName('ItemKey').AsString);
          Next;
        end;
        Close;
      end;  // with
    finally
      lqrysubSiteKeys.Free;
    end; // try
  end;
  //----------------------------------------------------------------------------
begin
  if cbIncludeSubsites.Visible and cbIncludeSubsites.Checked then begin
    { The keylist will change size so use a local var to define limits of a for loop }
    lInitialCount := FKeyList.Header.ItemCount;
    for i := 0 to lInitialCount - 1 do
      if SameText(FKeyList.ItemTable[i], TN_LOCATION) then
        AddSubsiteKeys(FKeyList.items[i].KeyField1);
  end; // if including subsites
end;  // PickupSubsites

//==============================================================================
procedure TdlgDataExport.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#27 then begin
    FCancelled:=true;
    Key:=#0;
  end;
end;  // FormKeyPress

//==============================================================================
procedure TdlgDataExport.PopulateExportList;
var
  i : integer;
begin
  cmbExportType.Items.Add(NBN_DATA_EXPORT); // always available
  cmbExportType.Items.Add(NBN_DATABASE_EXPORT); // always available
  with AppSettings.ComAddins.ExportFilters do
    for i := 0 to Count-1 do
      cmbExportType.Items.Add(Names[i]);
end;  // PopulateExportList

//==============================================================================
procedure TdlgDataExport.DoNBNExport(const AExportType: String; AInvalidKeys: TKeyList;
  AFromExportFilter: Boolean);
begin
  if not CheckMetadata then
  begin
    bbOk.Enabled := true;
    Exit;
  end;
  FOldCursor := HourglassCursor;
  frmMain.SetStatus(ResStr_ExportingNBNData);
  if AExportType = NBN_DATA_EXPORT then begin
    FDataOutput := TThreadedDataOutput.Create(eDestination.Text,
        FKeyList, AInvalidKeys, DataOutputComplete,
        (cbIncludeObservations.State = cbChecked) and (not cbReassignCustody.Checked),
        AFromExportFilter);
  end else begin
    FDataBaseOutput := TThreadedDatabaseOutput.Create(dmDatabase,
        eDestination.Text, frmMain.SetStatus, frmMain.ProgressBar,
        FKeyList, AInvalidKeys, DataOutputComplete,
        (cbIncludeObservations.State = cbChecked) and (not cbReassignCustody.Checked),
        AFromExportFilter,cbReassignCustody.Checked,cbExportPrivate.checked,FExcludeGroup);
  end;
end;  // DoNBNExport

//==============================================================================
{ Transfers the keylist of export data items into a COM key list so that an
     addin can be informed which items to export }
function TdlgDataExport.BuildCOMKeyList(AKeyList: TKeyList): TCOMKeyList;
var
  lCOMKeyList : TCOMKeyList;
begin
  lCOMKeyList := TCOMKeyList.Create(AKeyList);
  Result := lCOMKeyList;
end;  // BuildCOMKeyList

//==============================================================================
{ cmbExportTypeChange.  The Com export object is loaded and the appropriate
     file extensions loaded.  The IncludeObservations checkbox must be displayed
     only for the NBNdata export selection in the combo.  }
procedure TdlgDataExport.cmbExportTypeChange(Sender: TObject);
var
  lItemName : string;
  lstGuid : string;
  lstExtension : string;
begin
  //Mantis 343
  EnableDisableCustody(false);
  if cmbExportType.ItemIndex=-1 then
    bbOk.Enabled:=False
  else begin
    lItemName := cmbExportType.Items[cmbExportType.ItemIndex];
    if lItemName = NBN_DATA_EXPORT then
    begin
      dlgSave.DefaultExt := NBN_EXT;
      dlgSave.Filter := NBN_DATA_EXPORT + '|' + '*.' + NBN_EXT + ALL_FILES_FILTER;
    end else
    if lItemName = NBN_DATABASE_EXPORT then
    begin
      EnableDisableCustody(true);
      dlgSave.DefaultExt := NBN_DB_EXT;
      dlgSave.Filter := NBN_DATABASE_EXPORT + '|' + '*.' + NBN_DB_EXT + ALL_FILES_FILTER;
    end else begin
      with AppSettings.ComAddins.ExportFilters do
          lstGuid :=Values[lItemName];
      FExportIntf := CreateComObject( StringToGUID(lstGuid) );
      lstExtension := (FExportIntf as IFilter).DefaultFileExtension;
      dlgSave.DefaultExt := lstExtension;
      dlgSave.Filter := (FExportIntf as IRecorderAddin).Name + '|' +
                        '*.' + lstExtension + ALL_FILES_FILTER;
    end;
    if not FHideCheckBox then
      cbIncludeObservations.Visible := (lItemName = NBN_DATA_EXPORT) or (lItemName = NBN_DATABASE_EXPORT);
  end;
end;  // cmbExportTypeChange

//==============================================================================
{ Spaces out the 2 checkboxes nicely depending on which is visible }
procedure TdlgDataExport.SetCheckBoxPositions;
begin
  // Both visible.
  if cbIncludeObservations.Visible and cbIncludeSubsites.Visible then begin
    cbIncludeObservations.Top := Height - 70;
    cbIncludeSubsites.Top     := Height - 50;
  end else begin
    // Only one or none visible
    cbIncludeObservations.Top := Height - 54;
    cbIncludeSubsites.Top     := Height - 54;
    bvlFrame.Height           := bvlFrame.Height + 12;
    cbReassignCustody.Top     := cbReassignCustody.Top + 12;
    Height                    := Height - 12;
  end;
  cbNoValidation.Top :=  cbReassignCustody.Top ;
  cbExportPrivate.Top :=  cbReassignCustody.Top ;
  cbExportAddresses.Top :=  cbReassignCustody.Top + 25;
  lblExportConfidential.Top :=  cbExportAddresses.Top;
  if AppSettings.ExportConfidentialOccurrences then
    lblExportConfidential.caption  := ResStr_ExportConfidential
  else
    lblExportConfidential.caption := ResStr_NotExportConfidential;

end;  // SetCheckBoxPositions

//==============================================================================
procedure TdlgDataExport.SetCustodyCheckBox;
begin
  cbReassignCustody.Visible := AppSettings.UserAccessLevel = ualAdmin;
  if not cbReassignCustody.Visible then
     Height := Height - 20;
  cbNoValidation.height :=  cbReassignCustody.Height;
  cbExportPrivate.height :=  cbReassignCustody.Height;
end;

//==============================================================================
{ Enable/disable bbOK depending on if we have a file name to export to }
procedure TdlgDataExport.eDestinationChange(Sender: TObject);
begin
  bbOk.Enabled := not (eDestination.Text='');
end;  // eDestinationChange

//==============================================================================
{ Checks if the com object supports IExportByColumn.  If so, then the dialog
     is displayed which requests selection of a single column. }
procedure TdlgDataExport.CheckColumnSelectionRequired;
var
  lExportByColumn : IExportByColumn;
  ldlgExportField : TdlgExportField;
  lResult         : TModalResult;
begin
  try
    lExportByColumn := FExportIntf as IExportByColumn;
  except
    on E:EIntfCastError do
      Exit; // export by column not supported
  end;
  ldlgExportField := TdlgExportfield.Create(Self);
  with ldlgExportField do begin
    try
      lblTitle.Caption := Format(ResStr_SelectDataColumnForExport, [cmbExportType.Items[cmbExportType.ItemIndex]]);
      lResult := ShowModal;
      if lResult = mrOK then begin
        lExportByColumn.MeasurementUnitKey := MeasurementUnitKey;
        lExportByColumn.NumberOfRanges     := NumberOfRanges;
        lExportByColumn.MinValue           := MinValue;
        lExportByColumn.MaxValue           := MaxValue;
      end;
    finally
      Release;
    end; // try..finally
  end;
end;  // CheckColumnSelectionRequired

//==============================================================================
{ If any metadata required before the export, displays a form to request the
    information.  Returns true if data entered and valid, or data already
    available. }
function TdlgDataExport.CheckMetadata: boolean;
begin
  dmGeneralData.qryMetadata.Open;
  try
    if dmGeneralData.qryMetadata.RecordCount = 0 then
      Result := true
    else
      with TdlgMetadata.Create(nil) do
        try
          Result := (Showmodal = mrOK);
        finally
          Free;
        end;
  finally
    dmGeneralData.qryMetadata.Close;
  end;
end;  // CheckMetaData

//==============================================================================
{ Description : Enable the date picker if the correct radio option is selected
  Created : 02/12/2002 }
procedure TdlgDataExport.rgExportFilterSettingsClick(Sender: TObject);
begin
  dtpFilterDate.Enabled := FilterSetting = fsSinceDate;
end;

//==============================================================================
{ Description : function to grab the current filter setting from the radio
    group.   Accounts for the fact that items can be moved.
  Created : 2/12/2002 }
function TdlgDataExport.FilterSetting: TFilterSetting;
begin
  with rgExportFilterSettings do begin
  if Items[rgExportFilterSettings.ItemIndex] = ResStr_SinceLastExport then
    Result := fsSinceLastExport
  else
  if Items[rgExportFilterSettings.ItemIndex] = ResStr_SinceDate then
    Result := fsSinceDate
  else
    Result := fsAll;
  end; // with
end;

//==============================================================================
{ Description : function that will return the date that should be supplied to
     an export filter.  Raises an exception if there isn't one
  Created 02/12/2002 }
function TdlgDataExport.FilterDate: TDateTime;
begin
  Result := 0;
  if not Assigned(FFilter) then
    raise TExceptionPath.Create(ResStr_NoFilter);
  if FilterSetting = fsAll then
    Result := 0 // use 1899 - since no records could have been entered before then
  else
  if FilterSetting = fsSinceLastExport then
    Result := FFilter.LastExportDate
  else
  if FilterSetting = fsSinceDate then
    Result := dtpFilterDate.DateTime;
end;

//==============================================================================
function TdlgDataExport.NeedAFilter: Boolean;
var lCursor: TCursor;
begin
  FreeAndNil(FKeyList);
  Result := True;
  if FilterDate = Date then
    if DefMessageDlg(ResStr_TodaysDataOnly,
                     mtWarning, mbOKCancel, mbCancel, 0) = idCancel then begin
      ModalResult := mrNone;
      Result := False
    end;
  if Result then begin
    lCursor := HourglassCursor;
    try
      FKeyList := FFilter.KeyList(FilterDate);
      // use the same keylist as a list of validation items when using a filter
      FValidationKeyList := FKeyList;
    finally
      DefaultCursor(lCursor)
    end;
    if FKeyList.Header.ItemCount=0 then
      raise TExceptionPath.CreateNonCritical(ResStr_NothingToExport);
  end;
end;

//==============================================================================
procedure TdlgDataExport.ValidateAndExport(const exportType: String);
var
  i, idx: integer;
begin
  FKeyListFilter := TEditableKeyList.Create();
  FKeyListFilter.SetTable(MIXED_DATA);
  FInitialList := TEditableKeyList.Create();
  FInitialList.SetTable(MIXED_DATA);
  FValidationMessages := TStringList.Create;
  bbOk.Enabled := False;
  if NOT cbNoValidation.checked then
    dmFormActions.RevalidateNBNData(FValidationKeyList, FKeyListFilter, FValidationMessages);

  // Save initial list of keys to use with filter later.
  if FKeyListFilter.Header.ItemCount > 0 then
    FInitialList.Assign(FKeyListFilter);
  FKeyList.ConvertToMixedData;
  // make sure any invalid items in the original list of export keys are excluded
  for i := 0 to FKeyListFilter.Header.ItemCount - 1 do begin
    idx := FKeyList.IndexOf(FKeyListFilter.Items[i].KeyField1, FKeyListFilter.ItemTable[i]);
    if idx <> -1 then
      FKeyList.DeleteItem(idx);
  end;
  if FKeyList.Header.ItemCount>0 then
    try
      if (FKeyListFilter.Header.ItemCount = 0) or
         (ConfirmYesNo(ResStr_ConfirmExportWithoutInvalidRecords) = mrYes) then
      begin
        // Standard export or Add-in?
        if SameText(exportType, NBN_DATA_EXPORT) or SameText(exportType,  NBN_DATABASE_EXPORT) then
          DoNBNExport(exportType, FKeyListFilter, Assigned(FFilter))
        else begin
          // addin
          CheckColumnSelectionRequired;
          frmMain.Status.Panels[0].Text := ResStr_Exporting;
          if Supports(FExportIntf, IID_IExportFilter6) then
            (FExportIntf as IExportFilter6).ExportFileWithFilteredInvalids(
                BuildComKeylist(FKeyList),
                BuildComKeylist(FKeyListFilter),
                eDestination.Text)
          else
            (FExportIntf as IExportFilter).ExportFile(
                BuildComKeylist(FKeyList),
                eDestination.Text);
        end;
      end;
    finally
      frmMain.TaskFinished;
    end
  else
    // nothing to export because the exported node(s) were invalid
    ShowInformation(ResStr_SelectedDataAllInvalid);
end;

{-------------------------------------------------------------------------------
  Called when either of the FDataOutput thread or FDatabaseOutput thread
    has completed its operations
-------------------------------------------------------------------------------}
procedure TdlgDataExport.DataOutputComplete(Sender: TObject);
begin
  FDataOutputCompleteRun := True;
  try
    DefaultCursor(FOldCursor);
    frmMain.TaskFinished;
    ModalResult := mrOk;
    if Assigned(FFilter) then
      FFilter.UpdateLastExportDate(FExportStart);
    ShowInvalidRecords;


  finally
    FDataOutput := nil;         // Should already have been freed by the thread
    FDataBaseOutput := nil;     // Should already have been freed by the thread
    FreeAndNil(FKeyListFilter);
    FreeAndNil(FInitialList);
    FreeAndNil(FValidationMessages);
  end;
end;

{-------------------------------------------------------------------------------
  Display any invalid records
-------------------------------------------------------------------------------}
procedure TdlgDataExport.ShowInvalidRecords;
var
  filter: TListFilter;
begin
  if FInitialList.Header.ItemCount > 0 then begin
    filter := TListFilter.Create;
    try
      if filter.LoadFilter(FInitialList, FValidationMessages) then begin
        ShowInformation(ResStr_DisplayingInvalidRecordsForReview);
        filter.ApplyFilter;
      end;
    finally
      filter.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Control display of No Validation and Custody Change Box
-------------------------------------------------------------------------------}
procedure TdlgDataExport.EnableDisableCustody(EnableDisable : boolean);

begin
  cbReassignCustody.enabled := EnableDisable;
  cbReassignCustody.checked := false;
  cbNoValidation.enabled := EnableDisable;
  cbNoValidation.checked := false;
  cbExportPrivate.Enabled := EnableDisable;
  cbExportPrivate.checked := false;
  cbExportAddresses.Enabled := EnableDisable;
  cbExportAddresses.checked  := true;
end;
end.

