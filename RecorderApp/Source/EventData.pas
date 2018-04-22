//==============================================================================
//  Unit:        EventData
//
//  Implements:  TdmEvent
//               TEventRecorderItem
//               TEventRecorderList
//
//  Description: Implements data access functionality for the event details screen.
//
//               TEventRecorderItem and TEventRecorderList
//               Helper classes used on the General tab of the details screen.
//
//  Author:      Paul Thomas
//  Created:     23 April 1999
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 39 $               
//    $Date: 23/05/08 16:09 $
//    $Author: Qingsun $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit EventData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  BaseData, JNCCDatasets, DataClasses, Grids, VagueDate, ExceptionForm,
  Constants, ADODB, DatabaseAccessADO, BaseSampleEventData;

type
  EEventError = class(TExceptionPath);

  TdmEvent = class(TBaseSampleEventDataModule)  // devide from base class who contains GetChangedCascadeFields function
    qryEvent: TJNCCQuery;
    dsEvent: TDataSource;
    qryRecorder: TJNCCQuery;
    qryLocationName: TJNCCQuery;
    tblRole: TJNCCTable;
    qrySurveyDates: TJNCCQuery;
    qryOwnership: TJNCCQuery;
    qryOwnershipTypes: TJNCCNamesQuery;
    qryOwnershipSurvey_Event_Owner_Type_Key: TStringField;
    qryOwnershipName_Key: TStringField;
    qryOwnershipCustodian: TStringField;
    qryOwnershipSurvey_Event_Owner_Key: TStringField;
    qryOwnershipDisplayName: TStringField;
    qryOwnershipShort_Name: TStringField;
    qryOwnershipEntered_By: TStringField;
    qrySampleRecorder: TJNCCQuery;
    qryRole: TJNCCQuery;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
    procedure DeleteRecord(const AEventKey:TKeyString);
  end;  // TdmEvent

  //============================================================================
  TEventRecorderItem = class(TGridDataItem)
  private
    FRoleKey:TKeyString;
    FNameKey:TKeyString;
    FCanEdit: boolean;
    FCanDelete: boolean;
    function GetName: string;
    function GetRole: string;
    procedure SetNameKey(const Value: TKeyString);
    procedure SetRoleKey(const Value: TKeyString);
  protected
    procedure InitFromRecord(iDataset : TDataset); override;
    procedure WriteToRecord(iDataset : TDataset); override;
    function GetCell(const iX : integer): string; override;
  public
    constructor CreateNew(aOwner: TCacheDataList); override;
    property RoleKey: TKeyString read FRoleKey write SetRoleKey;
    property NameKey: TKeyString read FNameKey write SetNameKey;
    property Name: string read GetName;
    property Role: string read GetRole;
    property CanEdit: boolean read FCanEdit;
    property CanDelete: boolean read FCanDelete;
    constructor CreateFromRecord(aOwner: TCacheDataList; iDataSet: TDataSet); override;
  end;  // TEventRecorderItem

  //----------------------------------------------------------------------------
  TEventRecorderList=class(TGridDataList)
  private
    FEventDataModule:TdmEvent;
    FEventKey       :TKeyString;
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    constructor Create(iDataset: TDataset; const iKeyFieldName: string;
      iStringGrid: TStringGrid; iItemClass: TItemClass;
      iEventDataModule:TdmEvent);
    property EventDataModule:TdmEvent read FEventDataModule;
    property EventKey:TKeyString read FEventKey write FEventKey;
  end;  // TEventRecorderList
//==============================================================================
implementation

{$R *.DFM}

uses
  GeneralData, ApplicationSettings;

//==============================================================================
constructor TdmEvent.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  tblRole.Open;
end;  // Create

//==============================================================================
// Delete records from detail tables SURVEY_EVENT_RECORDER and
// SURVEY_EVENT_SOURCES, then from the master table SURVEY_EVENT.
procedure TdmEvent.DeleteRecord(const AEventKey: TKeyString);
begin

   dmGeneralData.DelSources('Survey_Event_Sources','Survey_Event_Key',AEventKey);
   dmDatabase.RunDeleteStoredProc('usp_SurveyEvent_Delete', ['@Key', AEventKey]);

end;  // DeleteRecord

//==============================================================================
destructor TdmEvent.Destroy;
begin
  tblRole.Close;
  qryEvent.Close;
  qryRecorder.Close;
  qryLocationName.Close;
  qrySampleRecorder.Close;
  inherited Destroy;
end;  // Destroy

//==============================================================================
// TEventRecorderItem
//------------------------------------------------------------------------------
procedure TEventRecorderItem.InitFromRecord(iDataSet: TDataSet);
begin
  with iDataSet do
    try
      FItemKey:=FieldByName('SE_Recorder_Key').AsString;
      FNameKey:=FieldByName('Name_Key').AsString;
      FRoleKey:=FieldByName('Recorder_Role_Key').AsString;
    except on E:Exception do
      raise EEventError.Create(ResStr_InitRecFail+' - SURVEY_EVENT_RECORDER',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TEventRecorderItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FieldByName('Name_Key').AsString:=FNameKey;
      FieldByName('Recorder_Role_Key').AsString:=FRoleKey;
      FieldByName('Changed_By').AsString :=AppSettings.UserID;
      FieldByName('Changed_Date').AsDateTime :=Now;
    except on E:Exception do
      raise EEventError.Create(ResStr_WriteRecFail+' - SURVEY_EVENT_RECORDER',E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
function TEventRecorderItem.GetName: string;
begin
  Result:=dmGeneralData.GetIndividualName(FNameKey);
end;  // GetName

//------------------------------------------------------------------------------
function TEventRecorderItem.GetRole: string;
begin
  Result := '';
  with TEventRecorderList(OwnerList).FEventDataModule.tblRole do
    if Locate('RECORDER_ROLE_KEY', FRoleKey, []) then
      Result := FieldByName('Short_Name').AsString;
end;  // GetRole

//------------------------------------------------------------------------------
procedure TEventRecorderItem.SetNameKey(const Value: TKeyString);
begin
  FNameKey:=Value;
  SetModified;
end;  // SetNameKey

//------------------------------------------------------------------------------
procedure TEventRecorderItem.SetRoleKey(const Value: TKeyString);
begin
  FRoleKey := Value;
  SetModified;
end;  // SetRoleKey

//------------------------------------------------------------------------------
function TEventRecorderItem.GetCell(const iX:integer):string;
begin
  case iX of
    0 : Result := Name;  // Display Name in column 0
    1 : Result := Role;  // Display Role in column 1
  else
    Result := ResStr_NotImplemented;
  end;
end;  // GetCell

//==============================================================================
// TEventRecorderList
//------------------------------------------------------------------------------
constructor TEventRecorderList.Create(iDataset: TDataset; const iKeyFieldName: string;
  iStringGrid: TStringGrid; iItemClass: TItemClass; iEventDataModule: TdmEvent);
begin
  FEventDataModule:=iEventDataModule;  // Link to DataModule object to use tblRole
  inherited Create(iDataSet,iKeyFieldName,iStringGrid,iItemClass);
end;  // Create

//------------------------------------------------------------------------------
function TEventRecorderList.ItemsDisplayName: String;
begin
  Result := ResStr_SurveyEventRecorder;
end;

//------------------------------------------------------------------------------
procedure TEventRecorderList.DoAdditions;
var i           : Integer;
    lDataItem   : TEventRecorderItem;
    lqryAddition: TJnccQuery;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Survey_Event_Recorder WHERE SE_Recorder_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := TEventRecorderItem(ItemsToAdd[i]);
          Append;
          FieldByName('SE_Recorder_Key').AsString   :=
              dmGeneralData.GetNextKey('Survey_Event_Recorder','SE_Recorder_Key');
          FieldByName('Name_Key').AsString          := lDataItem.NameKey;
          FieldByName('Survey_Event_Key').AsString  := EventKey;
          FieldByName('Recorder_Role_Key').AsString := lDataItem.RoleKey;
          FieldByName('Entered_By').AsString        := AppSettings.UserID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise EEventError.Create(Format(ResStr_AddFail, ['SURVEY_EVENT_RECORDER']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TEventRecorderList.ProcessUpdate;
var qrySER:TJnccQuery;
begin
  DoAdditions;
  DoModifications;
  qrySER:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qrySER]);
    DeleteFromTable(qrySER,'Sample_Recorder','SE_Recorder_Key');
    DeleteFromTable(qrySER,'Survey_Event_Recorder','SE_Recorder_Key');
  finally
    qrySER.Free;
  end;
end;  // ProcessUpdate

{-------------------------------------------------------------------------------
}
constructor TEventRecorderItem.CreateFromRecord(aOwner: TCacheDataList; iDataSet: TDataSet);
begin
  inherited;
  FCanDelete := (AppSettings.UserAccessLevel >= ualFullUser) and
             (Custodian = AppSettings.SiteID) and
             ((not AppSettings.RestrictFullEdit) or
             (iDataset.FieldByName('Entered_By').AsString=AppSettings.UserID));
  FCanEdit := FCanDelete;
end;

{-------------------------------------------------------------------------------
  New event recorder item, can always delete or edit
}
constructor TEventRecorderItem.CreateNew(aOwner: TCacheDataList);
begin
  inherited;
  FCanEdit := true;
  FCanDelete := true;
end;

end.
