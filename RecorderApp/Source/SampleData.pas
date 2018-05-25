//==============================================================================
//  Unit:        SampleData
//
//  Implements:  TdmSampleData
//               TRelatedSampleItem
//               TRelatedSampleList
//
//  Description: Implements data access functionality for the sample details
//               screen.
//
//               TRelatedSampleItem and TRelatedSampleList
//               Helper classes used on the Related Sample tab of the details
//               screen.
//
//  Author:      Paul Thomas
//  Created:     23 April 1999
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 47 $
//    $Date: 8/04/10 17:19 $
//    $Author: Andrewkemp $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

{$I '..\..\Third Party\Dorset Software Services\DssVcl32\DelphiVersions.Inc'}

unit SampleData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  BaseData, DataClasses, JNCCDatasets, Grids, CheckLst, ExceptionForm, VagueDate,
  Constants, GeneralFunctions, ADODB, DatabaseAccessADO, BaseSampleEventData, RapTree
  {$IFDEF DELPHI7UP}, Variants {$ENDIF}, Cascade, LocationInfoFrame, VagueDateEdit;

type
  ESampleError = class(TExceptionPath);

  TdmSample = class(TBaseSampleEventDataModule)  // devide from base class who contains GetChangedCascadeFields function
    qrySample: TJNCCQuery;
    dsSample: TDataSource;
    qryLocationName: TJNCCQuery;
    qryRelatedSample: TJNCCQuery;
    qryRecorder: TJNCCQuery;
    qrySampleType: TJNCCQuery;
    dsSampleType: TDataSource;
    qryRelSampProps: TJNCCQuery;
    qryEvent: TJNCCQuery;
    qryAdminAreas: TJNCCQuery;
  private
    function ValidateSiblingSampleDates(const eventKey, sampleKey: TKeyString;
        newEventDate: TVagueDate; checkAllSamples: Boolean): Boolean;
    function GetCascadeType(const eventKey: TKeyString; changes: TStringList;
        var updateSiblings: Boolean): TCascadeType;
    procedure UpdateEvent(const eventKey, sampleKey: TKeyString; newValues, oldValues: TStringList;
        setAll: Boolean);
    procedure UpdateSamples(const eventKey, sampleKey: TKeyString; newValues, oldValues: TStringList;
        setAll: Boolean);
    procedure UpdateDeterminations(const sampleKey: TKeyString; newValues, oldValues: TStringList);
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
    procedure DeleteRecord(const ASampleKey:TKeyString);
    procedure RefreshRecorders(const ASampleKey,AEventKey: TKeyString; AList:TCheckListBox);
    procedure UpdateRecorders(const ASampleKey:TKeyString; AList:TCheckListBox);
    function ValidateCascadeChanges(const eventKey, sampleKey: TKeyString; changes: TStringList;
        locationInfo: TfraLocationInfo; sampleDate: TVagueDateEdit; var updateSiblings: Boolean): TCascadeType;
    procedure CascadeSampleChanges(const eventKey, sampleKey: TKeyString; oldValues, newValues: TStringList;
        cascadeType: TCascadeType; updateSiblings: Boolean);
    function GetNodeCaption(const sampleRef, itemDate, location, locationName, spatialRef,
        sampleType: String; isSample: Boolean): String;
    procedure SetNodeCaption(node: TFlyNode; rsData: _Recordset; isSample: Boolean);
    procedure UpdateEventNodeCaption(eventNode: TFlyNode);
    procedure UpdateSampleNodesCaption(eventNode: TFlyNode);
  end;

  

  //============================================================================
  TSampleAdminAreaItem = class(TGridDataItem)
  private
    FAdminAreaType: String;
    FName: String;
    FAdminAreaKey: string;
    procedure SetAdminAreaType(const Value: String);
    procedure SetName(const Value: String);
    procedure SetAdminAreaKey(const Value: string);
  protected
    procedure InitFromRecord(iDataset: TDataset); override;
    procedure WriteToRecord(iDataset: TDataset); override;
    function GetCell(const iX: integer): string; override;
  public
    property AdminAreaKey: string read FAdminAreaKey write SetAdminAreaKey;
    property Name: String read FName write SetName;
    property AdminAreaType: String read FAdminAreaType write SetAdminAreaType;
  end;  // TSampleAdminAreaItem

  //----------------------------------------------------------------------------
  TSampleAdminAreaList = class(TGridDataList)
  private
    FSampleKey: string;
    procedure SetSampleKey(const Value: string);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    property SampleKey: string read FSampleKey write SetSampleKey;
  end;  // TSampleAdminAreaList

  TRelatedSampleItem = class(TGridDataItem)
  private
    FReference    :string;
    FDate         :string;
    FSampleTypeKey:TKeyString;
    FSampleType   :string;
    FRelationKey  :TKeyString;
  protected
    procedure InitFromRecord( iDataset : TDataset ); override;
    procedure WriteToRecord( iDataset : TDataset ); override;
    function GetCell( const iX : integer ): string; override;
  public
    procedure SetProperties(const ASampleKey:TKeyString);
    property Reference:string read FReference;
    property Date:string read FDate;
    property SampleTypeKey:TKeyString read FSampleTypeKey;
    property SampleType:string read FSampleType;
    property SampleRelationKey:TKeyString read FRelationKey;
  end;  // TRelatedSampleItem

  TRelatedSampleList = class(TGridDataList)
  private
    FSampleKey       :TKeyString;
    FSampleDataModule:TdmSample;
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    constructor Create( iDataset : TDataset; const iKeyFieldName : string;
      iStringGrid: TStringGrid; iItemClass : TItemClass; iSampleDataModule:TdmSample);
    procedure GridDrawCell(Sender: TObject; ACol,ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    property SampleKey:TKeyString read FSampleKey write FSampleKey;
    property SampleDataModule:TdmSample read FSampleDataModule;
  end;  // TRelatedSampleList

//==============================================================================
implementation

{$R *.DFM}

uses
  GeneralData, ApplicationSettings, FormActions, ValidationData, HierarchyNodes,
  SpatialRefFuncs;

resourcestring
  ResStr_InheritedCreate =  'inherited Create';
  ResStr_OpenSampleType = 'Open sample type';
  ResStr_ErrorInPhase = 'An error occurred in dmSampleData constructor phase ';
  ResStr_AdminArea =  'Administrative Area';

//==============================================================================
{ TdmSample }
//------------------------------------------------------------------------------
constructor TdmSample.Create(AOwner: TComponent);
var lPhase : string; // for error handling
begin
  lPhase := ResStr_InheritedCreate;
  try
    inherited Create(AOwner);
    lPhase := ResStr_OpenSampleType;
    qrySampleType.Open;
  except on E:Exception do
    raise ESampleError.Create(ResStr_ErrorInPhase + lPhase, E);
  end;
end;  // Create

//==============================================================================
procedure TdmSample.DeleteRecord(const ASampleKey: TKeyString);
begin
  with dmGeneralData do begin
    ExecuteSQL('DELETE FROM Sample_Recorder WHERE Sample_Key = '''+ASampleKey+'''',
               ResStr_DelFail+' - ');
    DelSources('Sample_Sources','Sample_Key',ASampleKey);

    ExecuteSQL(Format('DELETE FROM Sample_Admin_Areas WHERE Sample_Key = ''%s''', [ASampleKey]),
               ResStr_DelFail + ' - ');
    ExecuteSQL('DELETE FROM Sample_Relation WHERE Sample_Key_1 = '''+ASampleKey+'''',
               ResStr_DelFail+' - ');
    ExecuteSQL('DELETE FROM Sample_Data WHERE Sample_Key = '''+ASampleKey+'''',
               ResStr_DelFail+' - ');
    ExecuteSQL('DELETE FROM Sample WHERE Sample_Key = '''+ASampleKey+'''',
               ResStr_DelFail+' - ');
  end;
end;  // DeleteRecord

//==============================================================================
procedure TdmSample.RefreshRecorders(const ASampleKey,AEventKey: TKeyString; AList:TCheckListBox);
var iCount    :integer;
    lNewKey   :TKeyData;
    stFullName:string;
begin
  qryRecorder.Close;
  with qryRecorder.SQL do begin
    Clear;
    Add('SELECT I.Forename ,I.Surname, I.Initials, R.Short_Name, ' +
               'S.Sample_Key, SER.SE_Recorder_Key ');
    Add('FROM ((Survey_Event_Recorder AS SER ' +
        'INNER JOIN Individual AS I ' +
        'ON         SER.Name_Key = I.Name_Key) ');
    Add('INNER JOIN Recorder_Role AS R ' +
        'ON         SER.Recorder_Role_Key = R.Recorder_Role_Key) ');
    Add('LEFT JOIN  (SELECT * FROM Sample_Recorder WHERE Sample_Key = ''' + ASampleKey + ''') AS S ' +
        'ON         SER.SE_Recorder_Key = S.SE_Recorder_Key ');
    Add('WHERE SER.Survey_Event_Key = ''' + AEventKey + ''' ');
  end;
  // Update the Recorder query parameter
  qryRecorder.Open;

  for iCount:=0 to AList.Items.Count-1 do
    TKeyData(AList.Items.Objects[iCount]).Free;
  AList.Items.Clear;

  with qryRecorder do begin
    First;
    while not Eof do begin
      lNewKey:=TKeyData.Create;
      lNewKey.ItemKey:=FieldByName('SE_Recorder_Key').AsString;
      if FieldByName('Forename').AsString='' then
        stFullName:=FieldByName('Initials').AsString+' '+FieldByName('Surname').AsString
      else
        stFullName:=FieldByName('Forename').AsString+' '+FieldByName('Surname').AsString;
      AList.Items.AddObject(Trim(stFullName)+', '+FieldByName('Short_Name').AsString,lNewKey);
      AList.Checked[AList.Items.Count-1]:=not FieldByName('Sample_Key').IsNull;
      Next;
    end;
    Close;
  end;
end;  // RefreshRecorders

//==============================================================================
procedure TdmSample.UpdateRecorders(const ASampleKey:TKeyString; AList:TCheckListBox);
var lKey  : TKeyString;
    lqrySR: TJnccQuery;
    i     : Integer;
begin
  lqrySR := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqrySR]);
    with lqrySR do begin
      SQL.Text := 'SELECT * FROM Sample_Recorder WHERE Sample_Key = ''' + ASampleKey + '''';
      SQL.Add('');
      for i := 0 to AList.Items.Count - 1 do begin
        lKey := TKeyData(Alist.Items.Objects[i]).ItemKey;
        SQL[1] := 'AND SE_Recorder_Key = ''' + lKey + '''';
        Open;
        // If record found but recorder not in the list, delete record
        if (not Eof) and (not AList.Checked[i]) then
          Delete
        // else if no record found but recorder in the list, append record
        else if Eof and AList.Checked[i] then
          AppendRecord([ASampleKey, lKey, AppSettings.UserID, Date]);
        Close;
      end;
    end;
  finally
    lqrySR.Free;
  end;
end;  // UpdateRecorders

//==============================================================================
destructor TdmSample.Destroy;
begin
  qrySample.Close;
  qryLocationName.Close;
  qryRecorder.Close;
  qryRelatedSample.Close;
  qrySampleType.Close;
  inherited Destroy;
end;  // Destroy

{-------------------------------------------------------------------------------
 Gets the cascade option selected by the user, if any relevant fields have been updated.
}
function TdmSample.ValidateCascadeChanges(const eventKey, sampleKey: TKeyString;
    changes: TStringList; locationInfo: TfraLocationInfo; sampleDate: TVagueDateEdit;
    var updateSiblings: Boolean): TCascadeType;
var
  eventDateMatchesSampleDate: Boolean;
  validResult: TValidationResult;
begin
  // Get user preference for cascading any updates
  Result := GetCascadeType(eventKey, changes, updateSiblings);

  with locationInfo do
    if ((eLocation.Text <> '') or (eLocationName.Text = '') or (eSpatialRef.DisplayRef <> '')) and
       (Result = ctNothing) then
    begin
      // check location/sref stays consistent
      { Check spatial ref for Sample Location lies within that for event }
      validResult := dmValidation.CheckSampleInEvent(
          eventKey,
          eSpatialRef.EnteredRef,
          eLocation.Key,
          eSpatialRef.EnteredSystem);
      ValidateValue(validResult.Success, validResult.Message, eSpatialRef.ControlSpatialRef);
    end;

  eventDateMatchesSampleDate := dmDatabase.GetStoredProcOutputParam(
      'usp_SampleSameDateAsSurvey', ['@SampleKey', sampleKey], '@SameDates');

  if (Result = ctNothing) or (changes.IndexOfName('Vague_Date_Start') = -1) or
     ((Result = ctCascadeEqual) and (not eventDateMatchesSampleDate)) then
    // New sample's date might mismatch event's so check it.
    ValidateValue(
        dmValidation.CheckSampleDateAgainstEvent(eventKey, StringToVagueDate(sampleDate.Text)),
        ResStr_SampleDateAgainstEvent,
        sampleDate);

  if updateSiblings and (Result = ctCascadeEqual) then
    // date cascading to siblings that currently match, so check the ones that currently don't match
    ValidateValue(
        ValidateSiblingSampleDates(eventKey, sampleKey, StringToVagueDate(sampleDate.Text), False),
        ResStr_OtherSampleDateAgainstEvent,
        sampleDate)
  else
  if (not updateSiblings) and (Result = ctCascadeAll) then
    // date cascading to event only, so check all siblings dates remain in the event
    ValidateValue(
        ValidateSiblingSampleDates(eventKey, sampleKey, StringToVagueDate(sampleDate.Text), True),
        ResStr_OtherSampleDateAgainstEvent,
        sampleDate);
end;

{-------------------------------------------------------------------------------
 Gets the cascade option selected by the user, if any relevant fields have been updated.
}
function TdmSample.GetCascadeType(const eventKey: TKeyString; changes: TStringList;
  var updateSiblings: Boolean): TCascadeType;
var
  changedDetails: String;
  eventContainsOtherSamples: Boolean;
begin
  // Now bring up the options.
  Result         := ctNothing;
  updateSiblings := False;

  if changes.Count > 0 then begin
    // Determines whether the survey event for this sample contains any other samples.
    with dmDatabase.GetRecordset(
        'usp_Samples_Select_ForEvent', ['@EventKey', eventKey, '@SortOrder', '0']) do
    begin
      // Clearly, if only one sample, it's the current one, so no others.
      eventContainsOtherSamples := RecordCount > 1;
      Close;
    end;

    changedDetails := TdmSample.GetChangedCascadeFields(changes);
    with TdlgCascade.CreateDialog(nil, changedDetails, True, eventContainsOtherSamples) do
      try
        if ShowModal = mrOk then begin
          Result         := ChosenOption;
          updateSiblings := ckbUpdateSamples.Checked;
        end;
      finally
        Free;
      end;
  end;   
end;  // GetCascadeType

{-------------------------------------------------------------------------------
  Checks that all sibling samples will still have valid dates when the new event date is applied.
  If checkAllSamples is false, only those samples whose date is different to the
    current sample's existing date are checked, since the others will be updated.
}
function TdmSample.ValidateSiblingSampleDates(const eventKey, sampleKey: TKeyString;
    newEventDate: TVagueDate; checkAllSamples: Boolean): Boolean;
var
  rsSamples, rsCurrentSample, rsEvent: _Recordset;
  sampleDate, currentSampleDate, eventDate, finalEventDate: TVagueDate;
begin
  Result := True;
  finalEventDate := newEventDate;
  if not checkAllSamples then
  begin
    rsCurrentSample   := dmDatabase.GetRecordset('usp_SampleDetails_Get', ['@SampleKey', sampleKey]);
    currentSampleDate := dmGeneralData.GetVagueDateFromRecordset(rsCurrentSample);

    // If current event date doesn't match sample, it won't be changed, so use existing
    rsEvent   := dmDatabase.GetRecordset('usp_EventDetails_Get', ['@EventKey', eventKey]);
    eventDate := dmGeneralData.GetVagueDateFromRecordset(rsEvent);
    if not AreVagueDatesEqual(eventDate, currentSampleDate) then
      finalEventDate := eventDate;
  end;  // if not checkAllSamples

  rsSamples := dmDatabase.GetRecordset(
      'usp_Samples_Select_ForEvent', ['@EventKey', eventKey, '@SortOrder', '0']);

  if Assigned(rsSamples) then begin
    while not rsSamples.Eof do begin
      if rsSamples.Fields['Sample_Key'].Value <> sampleKey then begin
        sampleDate := dmGeneralData.GetVagueDateFromRecordset(rsSamples);

        if checkAllSamples or (not AreVagueDatesEqual(sampleDate, currentSampleDate)) then begin
          Result :=
              IsVagueDateInVagueDate(sampleDate, finalEventDate) or
              AreVagueDatesEqual(sampleDate, finalEventDate);
          if not Result then Break;
        end;
      end;
      rsSamples.MoveNext;
    end;
    rsSamples.Close;
  end;
end;  // ValidateSiblingSampleDates

{-------------------------------------------------------------------------------
 Gets the list of all the samples under the parameter EventKey and ask the user
 to update the sample data according to the changes to event
}
procedure TdmSample.CascadeSampleChanges(const eventKey, sampleKey: TKeyString; oldValues,
    newValues: TStringList; cascadeType: TCascadeType; updateSiblings: Boolean);
begin
  if cascadeType = ctCascadeAll then
  begin
    // Pass it to UpdateEvent - True means to set all the samples under that event
    UpdateEvent(eventKey, sampleKey, newValues, oldValues, True);
    UpdateDeterminations(sampleKey, newValues, nil);
    if updateSiblings then
      UpdateSamples(eventKey, sampleKey, newValues, oldValues, True);
  end else
  if cascadeType = ctCascadeEqual then
  begin
    // False means to set only the equal samples under that event
    UpdateEvent(eventKey, sampleKey, newValues, oldValues, False);
    UpdateDeterminations(sampleKey, newValues, oldValues);

    if updateSiblings then
      UpdateSamples(eventKey, sampleKey, newValues, oldValues, False);
  end;
end;    // CascadeSampleChanges

{-------------------------------------------------------------------------------
}
procedure TdmSample.UpdateEvent(const eventKey, sampleKey: TKeyString; newValues, oldValues:
    TStringList; setAll: Boolean);
var
  i, next: Integer;
begin
  next := 0;
  if setAll then begin
    for i:= 0 to newValues.Count - 1 do begin
      // Vague dates use three fields to store their data - set them all at once
      if i >= next then begin
        if AnsiSameText(newValues.Names[i], 'Vague_Date_Start') then begin
          dmDatabase.RunStoredProc(
              'usp_Event_Cascade_Vague_Date_Start_FromSample',
              ['@EventKey',           eventKey,
               '@StartValue',         newValues.ValueFromIndex[i],
               '@PreviousStartValue', NULL,
               '@EndValue',           newValues.ValueFromIndex[i + 1],
               '@PreviousEndValue',   NULL,
               '@TypeValue',          newValues.ValueFromIndex[i + 2],
               '@PreviousTypeValue',  NULL]);
          next := i + 3;
        end else if AnsiSameText(newValues.Names[i], 'Spatial_Ref') then
          dmDatabase.RunStoredProc(
              'usp_Event_Cascade_Spatial_Ref_FromSample',
              ['@SampleKey',     sampleKey,
               '@PreviousValue', NULL])
        else
          dmDatabase.RunStoredProc(
              'usp_Event_Cascade_' + newValues.Names[i] + '_FromSample',
              ['@EventKey',      eventKey,
               '@Value',         newValues.ValueFromIndex[i],
               '@PreviousValue', NULL]);
      end;
    end;
  end else begin
    for i:= 0 to newValues.Count - 1 do begin
      // Vague dates use three fields to store their data - set them all at once
      if i >= next then begin
        if AnsiSameText(newValues.Names[i], 'Vague_Date_Start') then begin
          dmDatabase.RunStoredProc(
              'usp_Event_Cascade_Vague_Date_Start_FromSample',
              ['@EventKey',           eventKey,
               '@StartValue',         newValues.ValueFromIndex[i],
               '@PreviousStartValue', oldValues.ValueFromIndex[i],
               '@EndValue',           newValues.ValueFromIndex[i + 1],
               '@PreviousEndValue',   oldValues.ValueFromIndex[i + 1],
               '@TypeValue',          newValues.ValueFromIndex[i + 2],
               '@PreviousTypeValue',  oldValues.ValueFromIndex[i + 2]]);
          next := i + 3;
        end else
          if AnsiSameText(newValues.Names[i], 'Spatial_Ref') then
          dmDatabase.RunStoredProc(
              'usp_Event_Cascade_Spatial_Ref_FromSample',
              ['@SampleKey',     sampleKey,
               '@PreviousValue', oldValues.ValueFromIndex[i]])
        else
          dmDatabase.RunStoredProc(
               'usp_Event_Cascade_' + newValues.Names[i] + '_FromSample',
               ['@EventKey',      eventKey,
               '@Value',         newValues.ValueFromIndex[i],
               '@PreviousValue', NULL]);
      end;
    end;
  end;    // if (setAll)
end;  // UpdateEvent

{-------------------------------------------------------------------------------
}
procedure TdmSample.UpdateSamples(const eventKey, sampleKey: TKeyString; newValues, oldValues:
    TStringList; setAll: Boolean);
var
  i, next: Integer;
begin
  next := 0;
  if setAll then begin
    for i:= 0 to newValues.Count - 1 do
      // Vague dates use three fields to store their data - set them all at once
      if i >= next then
        if AnsiSameText(newValues.Names[i], 'Vague_Date_Start') then begin
          dmDatabase.RunStoredProc(
              'usp_Sample_Cascade_Vague_Date_Start_FromEvent',
              ['@EventKey',           eventKey,
               '@StartValue',         newValues.ValueFromIndex[i],
               '@PreviousStartValue', NULL,
               '@EndValue',           newValues.ValueFromIndex[i + 1],
               '@PreviousEndValue',   NULL,
               '@TypeValue',          newValues.ValueFromIndex[i + 2],
               '@PreviousTypeValue',  NULL,
               '@CurrentSample',      sampleKey]);
          next := i + 3;
        end else
          dmDatabase.RunStoredProc(
              'usp_Sample_Cascade_' + newValues.Names[i] + '_FromEvent',
              ['@EventKey',      eventKey,
               '@Value',         newValues.ValueFromIndex[i],
               '@PreviousValue', NULL,
               '@CurrentSample', sampleKey])
  end else begin
    for i:= 0 to newValues.Count - 1 do
      // Vague dates use three fields to store their data - set them all at once
      if i >= next then
        if AnsiSameText(newValues.Names[i], 'Vague_Date_Start') then begin
          dmDatabase.RunStoredProc(
              'usp_Sample_Cascade_Vague_Date_Start_FromEvent',
              ['@EventKey',           eventKey,
               '@StartValue',         newValues.ValueFromIndex[i],
               '@PreviousStartValue', oldValues.ValueFromIndex[i],
               '@EndValue',           newValues.ValueFromIndex[i + 1],
               '@PreviousEndValue',   oldValues.ValueFromIndex[i + 1],
               '@TypeValue',          newValues.ValueFromIndex[i + 2],
               '@PreviousTypeValue',  oldValues.ValueFromIndex[i + 2],
               '@CurrentSample',      sampleKey]);
          next := i + 3;
        end else
          dmDatabase.RunStoredProc(
              'usp_Sample_Cascade_' + newValues.Names[i] + '_FromEvent',
              ['@EventKey',      eventKey,
               '@Value',         newValues.ValueFromIndex[i],
               '@PreviousValue', oldValues.ValueFromIndex[i],
               '@CurrentSample', sampleKey]);
  end;
end;  // UpdateSamples

{-------------------------------------------------------------------------------
  This procedure is called to update the determination dates for the sample
}
procedure TdmSample.UpdateDeterminations(const sampleKey: TKeyString; newValues, oldValues:
    TStringList);
var
  i, next: Integer;
  params: Array of Variant;
begin
  next   := 0;
  params := nil;
  if Assigned(oldValues) then begin
    for i:= 0 to newValues.Count - 1 do
      // Vague dates use three fields to store their data - set them all at once
      if i >= next then
        if AnsiSameText(newValues.Names[i], 'Vague_Date_Start') then
        begin
          params := VarArrayOf(
              ['@EventKey',           NULL,
               '@StartValue',         newValues.ValueFromIndex[i],
               '@PreviousStartValue', oldValues.ValueFromIndex[i],
               '@EndValue',           newValues.ValueFromIndex[i + 1],
               '@PreviousEndValue',   oldValues.ValueFromIndex[i + 1],
               '@TypeValue',          newValues.ValueFromIndex[i + 2],
               '@PreviousTypeValue',  oldValues.ValueFromIndex[i + 2],
               '@CurrentSample',      sampleKey]);
          dmDatabase.RunStoredProc(
              'usp_Taxon_Determination_Cascade_Vague_Date_Start_FromEvent', params);
          dmDatabase.RunStoredProc(
              'usp_Biotope_Determination_Cascade_Vague_Date_Start_FromEvent', params);
          next := i + 3;
        end;
  end else begin
    for i:= 0 to newValues.Count - 1 do 
      // Vague dates use three fields to store their data - set them all at once
      if i >= next then
        if AnsiSameText(newValues.Names[i], 'Vague_Date_Start') then
        begin
          params := VarArrayOf(
              ['@EventKey',           NULL,
               '@StartValue',         newValues.ValueFromIndex[i],
               '@PreviousStartValue', NULL,
               '@EndValue',           newValues.ValueFromIndex[i + 1],
               '@PreviousEndValue',   NULL,
               '@TypeValue',          newValues.ValueFromIndex[i + 2],
               '@PreviousTypeValue',  NULL,
               '@CurrentSample',      sampleKey]);
          dmDatabase.RunStoredProc(
              'usp_Taxon_Determination_Cascade_Vague_Date_Start_FromEvent', params);
          dmDatabase.RunStoredProc(
              'usp_Biotope_Determination_Cascade_Vague_Date_Start_FromEvent', params);
          next := i + 3;
        end;
  end;  // if Assigned(oldValues)
end;  // UpdateDeterminations

{-------------------------------------------------------------------------------
}
procedure TdmSample.UpdateEventNodeCaption(eventNode: TFlyNode);
var
  eventData: TEventNode;
  rsEvent: _Recordset;
begin
  eventData := TEventNode(eventNode.Data);
  rsEvent   := dmDatabase.GetRecordset('usp_EventDetails_Get', ['@EventKey', eventData.ItemKey]);
  if rsEvent.RecordCount > 0 then begin
    SetNodeCaption(eventNode, rsEvent, False);
    rsEvent.Close;
  end;
end;  // UpdateEventNodeCaption

{-------------------------------------------------------------------------------
  This procedure is called to update the samples on the heirarchy tree after
  updating it in the database
}
procedure TdmSample.UpdateSampleNodesCaption(eventNode: TFlyNode);
var
  i, index: Integer;
  sampleData: TSampleNode;
  rsSample: _Recordset;
begin
  index := 0;
  for i := 0 to eventNode.Count - 1 do
    if i >= index then begin
      sampleData := TSampleNode(eventNode.Item[i].Data);
      rsSample   := dmDatabase.GetRecordset('usp_SampleDetails_Get',
          ['@SampleKey', sampleData.ItemKey]);

      // Handle multiple records for the same sample - multiple location names
      index := i;
      while (not rsSample.Eof) and (index < eventNode.Count) do begin
        // Exit loop if next node is for a different sample
        if sampleData.ItemKey <> TSampleNode(eventNode.Item[index].Data).ItemKey then
          Break;

        SetNodeCaption(eventNode.Item[index], rsSample, True);
        rsSample.MoveNext;
        Inc(index);
      end;
      rsSample.Close;
    end;
end;  // UpdateSampleNodesCaption

{-------------------------------------------------------------------------------
}
procedure TdmSample.SetNodeCaption(node: TFlyNode; rsData: _Recordset; isSample: Boolean);
var
  sampleReference, sampleType: String;
begin
  if isSample then begin
    sampleReference := VarToStr(rsData.Fields['Sample_Reference'].Value);
    sampleType      := VarToStr(rsData.Fields['Sample_Type'].Value);
  end else begin
    sampleReference := '';
    sampleType      := '';
  end;
  // Setting the node caption
  node.Caption := GetNodeCaption(
      sampleReference,
      VagueDateToString(dmGeneralData.GetVagueDateFromRecordset(rsData)),
      VarToStr(rsData.Fields['Location'].Value),
      VarToStr(rsData.Fields['Location_Name'].Value),
      VarToStr(rsData.Fields['Spatial_Ref'].Value),
      sampleType,
      isSample);
end;  // SetNodeCaption

{-------------------------------------------------------------------------------
  Build the caption for an event or sample node on the Observations screen.
}
function TdmSample.GetNodeCaption(const sampleRef, itemDate, location, locationName, spatialRef,
    sampleType: String; isSample: Boolean): String;
begin
  Result := itemDate;

  // Checking for the next item to put in node caption.
  // The order is Location, Location Name, Spatial Ref.
  if location <> '' then
    Result := Result + ' - ' + location
  else
  if spatialRef <> '' then
    Result := Result + ' - ' + LocaliseSpatialRef(spatialRef)
  else
  if locationName <> '' then
    Result := Result + ' - ' + locationName;

  // If it is sample it also needs the sample type at the end.
  if isSample then begin
    Result := Result + ' - ' + sampleType;
    // also include the sample reference if available.
    if sampleRef <> '' then
      Result := sampleRef + ' - ' + Result;
  end;
end;  // GetNodeCaption

//==============================================================================
{ TRelatedSampleItem }
//------------------------------------------------------------------------------
function TRelatedSampleItem.GetCell(const iX: integer): string;
begin
  case iX of
    0 : Result:=Reference;   // Reference
    1 : Result:=Date;        // Date
    2 : Result:=SampleType;  // Sample Type
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TRelatedSampleItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FItemKey      :=FieldByName('Sample_Key_2').AsString;
      FDate         :=FieldByName('Vague_Date_Start').Text;
      FReference    :=FieldByName('Sample_Reference').AsString;
      FSampleTypeKey:=FieldByName('Sample_Type_Key').AsString;
      FSampleType   :=FieldByName('Short_Name').AsString;
      FRelationKey  :=FieldByName('Sample_Relation_Key').AsString;
    except on E:Exception do
      raise ESampleError.Create(ResStr_InitRecFail+' - SAMPLE_RELATION',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TRelatedSampleItem.WriteToRecord(iDataset: TDataset);
begin
  // Do nothing, as Related Samples can only be added or deleted, NOT modified.
  // But we need this here because of the abstract declaration.
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TRelatedSampleItem.SetProperties(const ASampleKey: TKeyString);
begin
  with TRelatedSampleList(OwnerList).SampleDataModule.qryRelSampProps do begin
    Close;
    Parameters.ParamByName('KeyParameter').Value:=ASampleKey;
    Open;
    FItemKey:=ASampleKey;
    FDate         :=FieldByName('Vague_Date_Start').Text;
    FReference    :=FieldByName('Sample_Reference').AsString;
    FSampleTypeKey:=FieldByName('Sample_Type_Key').AsString;
    FSampleType   :=FieldByName('Short_Name').AsString;
    Close;
  end;
end;  // SetProperties

//==============================================================================
{ TRelatedSampleList }
//------------------------------------------------------------------------------
constructor TRelatedSampleList.Create(iDataset:TDataset; const iKeyFieldName:string;
  iStringGrid:TStringGrid; iItemClass:TItemClass; iSampleDataModule:TdmSample);
begin
  FSampleDataModule:=iSampleDataModule;
  iStringGrid.OnDrawCell:=GridDrawCell;
  inherited Create(iDataSet,iKeyFieldName,iStringGrid,TRelatedSampleItem);
end;  // Create

//------------------------------------------------------------------------------
function TRelatedSampleList.ItemsDisplayName: String;
begin
  Result := ResStr_RelatedSample;
end;

//------------------------------------------------------------------------------
procedure TRelatedSampleList.GridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var x,y,lIndex:integer;
    lDataItem:TRelatedSampleItem;
begin
  inherited;
  with TStringGrid(Sender) do
    if (Rows[ARow].Objects[0]<>nil) and (ACol=2) and (ARow>0) then begin
      lDataItem:=TRelatedSampleItem(Rows[ARow].Objects[0]);

      Canvas.FillRect(Rect);
      x:=Rect.Left+2;
      y:=Rect.Top+(DefaultRowHeight-dmFormActions.ilSampleTypes.Height) div 2;
      lIndex:=dmFormActions.GetSampleImageIndex(lDataItem.SampleTypeKey);

      dmFormActions.ilSampleTypes.Draw(Canvas,x,y,lIndex);
      // Display the text. Use Classes.Rect for the function, as Rect is a local var.
      DrawChoppedText(Cells[ACol,ARow],Canvas,
                      Classes.Rect(x+dmFormActions.ilSampleTypes.Width+2,Rect.Top,
                                   Rect.Right,Rect.Bottom),2);
    end else begin
      // Normal cells, normal chopping.
      Canvas.FillRect(Rect);
      DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
    end;
end;  // GridDrawCell

//------------------------------------------------------------------------------
procedure TRelatedSampleList.DoAdditions;
var i           : integer;
    lDataItem   : TRelatedSampleItem;
    lqryAddition: TJnccQuery;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Sample_Relation WHERE Sample_Relation_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := TRelatedSampleItem(ItemsToAdd[i]);
          Append;
          Fieldbyname('Sample_Relation_Key').AsString :=
              dmGeneralData.GetNextKey('Sample_Relation', 'Sample_Relation_Key');
          FieldByName('Sample_Key_1').AsString        :=SampleKey;
          FieldByName('Sample_Key_2').AsString        :=lDataItem.ItemKey;
          FieldByName('Entered_By').AsString          :=AppSettings.UserID;
          FieldByName('Position_Number').AsInteger    :=0;
          Post;
        end;
        Close;
      except on E:Exception do
        raise ESampleError.Create(Format(ResStr_AddFail, ['SAMPLE_RELATION']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TRelatedSampleList.ProcessUpdate;
var qrySR:TJnccQuery;
begin
  qrySR:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qrySR]);
    DoAdditions;
    // No modifications
    DeleteFromTable(qrySR,'Sample_Relation','Sample_Key_2');
  finally
    qrySR.Free;
  end;
end;  // ProcessUpdate

//==============================================================================
{ TSampleAdminAreaItem }
//------------------------------------------------------------------------------
function TSampleAdminAreaItem.GetCell(const iX: integer): string;
begin
  case iX of
    0: Result := FName;
    1: Result := FAdminAreaType;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TSampleAdminAreaItem.InitFromRecord(iDataset: TDataset);
var lTempList: TStringList;
begin
  with iDataSet do
    try
      FAdminAreaKey := FieldByName('Admin_Area_Key').AsString;
      //Set the key in two places as we need ItemKey to be able to do modifications
      FItemKey      := Fieldbyname('Sample_Admin_Areas_Key').AsString;

      //Get display fields from other tables
      lTempList := TStringList.Create;
      try
        dmGeneralData.GetRecordStrings(lTempList, 'ADMIN_AREA', FAdminAreaKey);
        if lTempList.Values['SHORT_CODE'] = '' then
          FName := lTempList.Values['ITEM_NAME']
        else
          FName := lTempList.Values['SHORT_CODE'] + ', ' + lTempList.Values['ITEM_NAME'];
        dmGeneralData.GetRecordStrings(lTempList, 'ADMIN_TYPE', lTempList.Values['ADMIN_TYPE_KEY']);
        FAdminAreaType := lTempList.Values['SHORT_NAME'];
      finally
        lTempList.Free;
      end;
    except on E:Exception do
      raise ESampleError.Create(ResStr_InitRecFail + ' - SAMPLE_ADMIN_AREAS',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
// Doesn't seemed to be called at all, and shouldn't ever be called really
procedure TSampleAdminAreaItem.WriteToRecord(iDataset: TDataset);
begin
//  with iDataSet do
//    try
//      FieldByName('Admin_Area_Key').AsString:= FAdminAreaKey;
//    except on Exception do
//      raise ESampleError.Create(EST_WRITE_REC_FAIL);
//    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TSampleAdminAreaItem.SetAdminAreaKey(const Value: string);
begin
  FAdminAreaKey := Value;
  SetModified;
end;  // SetAdminAreaKey

//------------------------------------------------------------------------------
procedure TSampleAdminAreaItem.SetAdminAreaType(const Value: String);
begin
  FAdminAreaType := Value;
  SetModified;
end;  // SetAdminAreaType

//------------------------------------------------------------------------------
procedure TSampleAdminAreaItem.SetName(const Value: String);
begin
  FName := Value;
  SetModified;
end;  // SetName

//==============================================================================
{ TSampleAdminAreaList }
function TSampleAdminAreaList.ItemsDisplayName: String;
begin
  Result := ResStr_AdminArea;
end;

//------------------------------------------------------------------------------
procedure TSampleAdminAreaList.DoAdditions;
var lDataItem   : TSampleAdminAreaItem;
    i           : integer;
    lqryAddition: TJnccQuery;
begin
  if ItemsToAdd.Count = 0 then Exit;
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Sample_Admin_Areas WHERE Sample_Admin_Areas_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := TSampleAdminAreaItem(ItemsToAdd[i]);
          Append;
          Fieldbyname('Sample_Admin_Areas_Key').AsString :=
              dmGeneralData.GetNextKey('Sample_Admin_Areas','Sample_Admin_Areas_Key');
          FieldByName('Admin_Area_Key').AsString := lDataItem.FAdminAreaKey;
          FieldByName('Sample_Key').AsString     := FSampleKey;
          FieldByName('Entry_Date').AsDateTime   := Now;
          FieldByName('Entered_By').AsString     := AppSettings.UserID;
          FieldByName('Custodian').AsString      := AppSettings.SiteID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise ESampleError.Create(Format(ResStr_AddFail, ['SAMPLE_ADMIN_AREAS']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TSampleAdminAreaList.ProcessUpdate;
var qryDel: TJnccQuery;
    lCount: Integer;
    lKeys : String;
begin
  DoAdditions;
  DoModifications;
  qryDel := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    with qryDel do begin
      if ItemsToDelete.Count > 0 then begin
        lKeys := '';
        for lCount := 0 to ItemsToDelete.Count - 1 do
          lKeys  := lKeys + ',''' + TSampleAdminAreaItem(ItemsToDelete[lCount]).AdminAreaKey + '''';
        lKeys[1] := ' '; // Remove first ','
        SQL.Text := Format('DELETE FROM Sample_Admin_Areas WHERE Admin_Area_Key IN (%s) ' +
                           'AND Sample_Key = ''%s''', [lKeys, FSampleKey]);
        ExecSQL;
      end; // if ItemsToDelete.Count>0
    end;
  finally
    qryDel.Free;
  end;
end;  // ProcessUpdate

//------------------------------------------------------------------------------
procedure TSampleAdminAreaList.SetSampleKey(const Value: string);
begin
  FSampleKey := Value;
end;  // SetSampleKey

//==============================================================================
end.

