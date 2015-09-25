//==============================================================================
//  Unit:        DictionaryDataGeneric
//
//  Implements:  TSourceItem
//               TBaseStatusItem
//               TBaseFactsItem
//
//  Description: Contains helper classes used in dictionary editors to manage
//               data before committing changes to the database. 
//
//  Author:      Eric Salmon
//  Created:     10 Dec 2001
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 5 $
//    $Date: 27/12/07 15:07 $
//    $Author: Rickyshrestha $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

{$I '..\..\Third Party\Dorset Software Services\DssVcl32\DelphiVersions.Inc'}

unit DictionaryDataGeneric;

interface

uses
  SysUtils, Classes, DataClasses, Db, Constants, ValidationData
  {$IFDEF DELPHI7UP}, Variants {$ENDIF};

type
  // Grid data item with some code to handle sources fields
  TSourceItem = class(TGridDataItem)
  protected
    FSourceKey: TKeyString;
    FSystemSupplied: Boolean;
    // Accessor methods
    procedure SetSourceKey(const Value: TKeyString);
  public
    property SourceKey: TKeyString read FSourceKey write SetSourceKey;
    property SystemSupplied: Boolean read FSystemSupplied;
  end;

  //============================================================================
  // base item for the status tabs of the dictionary edit screens
  TBaseStatusItem = class(TSourceItem)
  private
    FStatusType    : String;
    FDateFrom      : TDateTime;
    FDateTo        : TDateTime;
    FGeographicArea: String;
    FConstraints   : String;
    FDetails       : TMemoryStream;
    FStatusTypeKey: TKeyString;
    procedure SetStatusType(const AStatusType: String);
    procedure SetDateFrom(const ADateFrom: TDateTime);
    procedure SetDateTo(const ADateTo: TDateTime);
    procedure SetDateFromAsString(const ADateFrom: String);
    procedure SetDateToAsString(const ADateTo: String);
    procedure SetConstraints(const AConstraints: String);
    procedure SetGeographicArea(const AGeographicArea: String);
    procedure SetStatusTypeKey(const Value: TKeyString);

    function GetDateFromAsString: String;
    function GetDateToAsString: String;
  protected
    procedure InitFromRecord(iDataset: TDataset); override;
    procedure WriteToRecord(iDataset: TDataset); override;
    function GetCell(const iX: Integer): String; override;
  public
    constructor CreateNew(AOwner:TCacheDataList); override;
    destructor Destroy; override;
    property StatusType: String read FStatusType write SetStatusType;
    property DateFromAsString: String read GetDateFromAsString write SetDateFromAsString;
    property DateToAsString: String read GetDateToAsString write SetDateToAsString;
    property DateFrom: TDateTime read FDateFrom write SetDateFrom;
    property DateTo: TDateTime read FDateTo write SetDateTo;
    property GeographicArea: String read FGeographicArea write SetGeographicArea;
    property Constraints: String read FConstraints write SetConstraints;
    property Details: TMemoryStream read FDetails; // write SetDetails;
    property StatusTypeKey: TKeyString read FStatusTypeKey write SetStatusTypeKey;
  end;

  //============================================================================
  // base item for the facts tabs of the dictionary edit screens
  TBaseFactsItem = class(TSourceItem)
  private
    FFactType: String;
    FTitle: String;
    FDate: String;
    FFacts: String;
    procedure SetDate(const Value: String);
    procedure SetFacts(const Value: String);
    procedure SetFactType(const Value: String);
    procedure SetTitle(const Value: String);
  protected
    procedure InitFromRecord(iDataset: TDataset); override;
    procedure WriteToRecord(iDataset: TDataset); override;
    function GetCell(const iX: Integer): String; override;
  public
    property FactType: String read FFactType write SetFactType;
    property Date: String read FDate write SetDate;
    property Title: String read FTitle write SetTitle;
    property Facts: String read FFacts write SetFacts;
  end;

//==============================================================================
implementation

uses
  ApplicationSettings;
  
//==============================================================================
{ TSourceItem }

procedure TSourceItem.SetSourceKey(const Value: TKeyString);
begin
  FSourceKey := Value;
  SetModified;
end;  // SetSourceKey

//==============================================================================
//==============================================================================
{ TBaseFactsItem }
function TBaseFactsItem.GetCell(const iX: Integer): String;
begin
  case iX of
    0 : Result := FFactType;
    1 : Result := FDate;
    2 : Result := FTitle;
  else
    Result := ResStr_NotImplemented;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TBaseFactsItem.SetDate(const Value: String);
begin
  FDate := Value;
  SetModified;
end;  // SetDate

//------------------------------------------------------------------------------
procedure TBaseFactsItem.SetFacts(const Value: String);
begin
  FFacts := Value;
  SetModified;
end;  // SetFacts

//------------------------------------------------------------------------------
procedure TBaseFactsItem.SetFactType(const Value: String);
begin
  FFactType := Value;
  SetModified;
end;  // SetFactType

//------------------------------------------------------------------------------
procedure TBaseFactsItem.SetTitle(const Value: String);
begin
  FTitle := Value;
  SetModified;
end;  // SetTitle

//------------------------------------------------------------------------------
procedure TBaseFactsItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FItemKey        := FieldByName('FactKey').AsString;
      FFactType       := FieldByName('Type').AsString;
      FTitle          := FieldByName('Title').Asstring;
      FFacts          := FieldByName('Data').Asstring;
      FSystemSupplied := Fieldbyname('SystemSuppliedData').AsBoolean;
      FSourceKey      := Fieldbyname('SourceKey').AsString;
      // do not alias the vague date fields
      FDate           := FieldByName('FACT_VAGUE_DATE_START').Text;
    except on Exception do
      raise EDataListError.Create(ResStr_InitRecFail + ' - FACT');
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TBaseFactsItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      // Update code here
      FieldByName('Data').AsString  := FFacts;
      FieldByName('Title').AsString := FTitle;
      FieldByName('Type').AsString  := FFactType;
      if FSourceKey = '' then
        FieldByName('SourceKey').Value := Null
      else
        FieldByName('SourceKey').AsString := FSourceKey;
      FieldByName('ChangedBy').AsString     := AppSettings.UserID;
      FieldByName('ChangedDate').AsDateTime := Now;
      // do not alias the vague date fields
      FieldByName('FACT_VAGUE_DATE_START').Text := FDate;
    except on Exception do
      raise EDataListError.Create(ResStr_WriteRecFail + ' - FACT');
    end;
end;  // WriteToRecord

//==============================================================================
//==============================================================================
{ TBaseStatusItem }
constructor TBaseStatusItem.CreateNew(AOwner:TCacheDataList);
begin
  inherited CreateNew(AOwner);
  FDetails := TMemoryStream.Create;
end;

//------------------------------------------------------------------------------
destructor TBaseStatusItem.Destroy;
begin
  FDetails.Free;
  inherited Destroy;
end;  // Destroy

//------------------------------------------------------------------------------
function TBaseStatusItem.GetCell(const iX: Integer): String;
begin
  case iX of
    0 : Result := FStatusType;
    1 : Result := DateFromAsString;
    2 : Result := DateToAsString;
    3 : Result := FGeographicArea;
    4 : Result := FConstraints;
  else
    Result := ResStr_NotImplemented;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TBaseStatusItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FItemKey    := FieldByName('DesignationKey').AsString;
      FStatusType := FieldByName('ShortName').AsString;

      if FieldByName('DateFrom').IsNull then
        FDateFrom := -1
      else
        FDateFrom := FieldByName('DateFrom').AsDateTime;

      if FieldByName('DateTo').IsNull then
        FDateTo := -1
      else
        FDateTo := FieldByName('DateTo').AsDateTime;

      FGeographicArea := FieldByName('GeogArea').AsString;
      FConstraints    := FieldByName('StatusConstraint').AsString;
      FDetails := TMemoryStream.Create;
      TMemoField(FieldByName('Details')).SaveToStream(FDetails);
      FStatusTypeKey  := Fieldbyname('DesignationTypeKey').AsString;
      FSourceKey      := Fieldbyname('SourceKey').AsString;
      FSystemSupplied := Fieldbyname('SystemSuppliedData').AsBoolean;
    except on Exception do
      raise EDataListError.Create(ResStr_InitRecFail + ' - DESIGNATION (Status)');
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TBaseStatusItem.SetConstraints(const AConstraints: String);
begin
  FConstraints := AConstraints;
  SetModified;
end;  // SetConstraints

//------------------------------------------------------------------------------
procedure TBaseStatusItem.SetDateFrom(const ADateFrom: TDateTime);
begin
  FDateFrom := ADateFrom;
  SetModified;
end;  // SetDateFrom

//------------------------------------------------------------------------------
procedure TBaseStatusItem.SetDateTo(const ADateTo: TDateTime);
begin
  FDateTo := ADateTo;
  SetModified;
end;  // SetDateTo

//------------------------------------------------------------------------------
procedure TBaseStatusItem.SetDateFromAsString(const ADateFrom: String);
begin
  if (ADateFrom<>'') and IsDate(ADateFrom) then
  begin
    FDateFrom := StrToDate(ADateFrom);
    SetModified;
  end
  else
    FDateFrom := -1;
end;  // SetDateFromAsString

//------------------------------------------------------------------------------
procedure TBaseStatusItem.SetDateToAsString(const ADateTo: String);
begin
  if (ADateTo<>'') and IsDate(ADateTo) then
  begin
    FDateTo := StrToDate(ADateTo);
    SetModified;
  end
  else
    FDateTo := -1;
end;  // SetDateToAsString

//------------------------------------------------------------------------------
function TBaseStatusItem.GetDateFromAsString: String;
begin
  if FDateFrom >=0 then
    Result := DateToStr(FDateFrom)
  else
    Result := '';
end;  // GetDateFromAsString

//------------------------------------------------------------------------------
function TBaseStatusItem.GetDateToAsString: String;
begin
  if FDateTo >=0 then
    Result := DateToStr(FDateTo)
  else
    Result := '';
end;  // GetDateToAsString

//------------------------------------------------------------------------------
procedure TBaseStatusItem.SetGeographicArea(const AGeographicArea: String);
begin
  FGeographicArea := AGeographicArea;
  SetModified;
end;  // SetGeographicArea

//------------------------------------------------------------------------------
procedure TBaseStatusItem.SetStatusType(const AStatusType: String);
begin
  FStatusType := AStatusType;
  SetModified;
end;  // SetStatusType

//------------------------------------------------------------------------------
procedure TBaseStatusItem.SetStatusTypeKey(const Value: TKeyString);
begin
  FStatusTypeKey := Value;
  SetModified;
end;  // SetStatusTypeKey

//------------------------------------------------------------------------------
procedure TBaseStatusItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      // Update code here
      if FDateFrom > 0 then
        FieldByName('DateFrom').AsDateTime := FDateFrom
      else
        FieldByName('DateFrom').Clear;

      if FDateTo > 0 then
        FieldByName('DateTo').AsDateTime := FDateTo
      else
        FieldByName('DateTo').Clear;

      FieldByName('GeogArea').AsString           := FGeographicArea;
      FieldByName('StatusConstraint').AsString   := FConstraints;
      FDetails.Position := 0;
      TMemoField(FieldByName('Details')).LoadFromStream(FDetails);
      FieldByName('DesignationTypeKey').AsString := FStatusTypeKey;
      if FSourceKey = '' then
        FieldByName('SourceKey').Value := Null
      else
        FieldByName('SourceKey').AsString := FSourceKey;
      FieldByName('ChangedBy').AsString     := AppSettings.UserID;
      FieldByName('ChangedDate').AsDateTime := Now;
    except on Exception do
      raise EDataListError.Create(ResStr_WriteRecFail + ' - DESIGNATION (Status)');
    end;
end;  // WriteToRecord

//==============================================================================
end.
