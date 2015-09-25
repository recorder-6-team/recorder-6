//==============================================================================
//  Unit:        MeasurementsData
//
//  Implements:  TdmMeasurements
//               TMeasureItem
//               TMeasureList
//
//  Description: Implements data access functionality for the Measurements
//               component.
//
//               TMeasureItem and TMeasureList
//               Helper classes.
//
//  Author:      Eric Salmon
//  Created:     14 May 1999
//
//  Changes:     Eric Salmon - 27/03/2002
//               Implementation changes concerning the handling of the database
//               object, makes it easier to deal with either TtaDatabase or
//               TADOConnection.
//
//  Last Revision Details:
//    $Revision: 22 $
//    $Date: 18/03/08 16:16 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 1999
//
//==============================================================================

unit MeasurementsData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DataClasses, Grids, Db, JNCCDatasets,ADODB;

type
  TGetNextKey = function (const ATableName,AKeyFieldName:string):TKeyString of object;

  TdmMeasurements = class(TDataModule)
    qryMeasure: TJNCCQuery;
    qryMeasureType: TJNCCQuery;
    qryMeasureUnit: TJNCCQuery;
    dsMeasureType: TDataSource;
    dsMeasureUnit: TDataSource;
    qryMeasureQualifier: TJNCCQuery;
    dsMeasureQualifier: TDataSource;
    qryUpdate: TJNCCQuery;
    qryMeasureData: TJNCCQuery;
    dsMeasureData: TDataSource;
  private
    FDatabase: TADOConnection;
    procedure SetDatabase(const ADataset: TCustomADODataset);
  public
    procedure UpdateDatasets(const ADatabase: TADOConnection);
  end;  // TdmMeasurements

  //----------------------------------------------------------------------------
  TMeasureItem = class(TGridDataItem)
  private
    FData: string;
    FAccuracy: string;
    FMeasureUnit: string;
    FMeasureUnitKey: TKeyString;
    FMeasureType: string;
    FMeasureTypeKey: TKeyString;
    FQualifier: string;
    FQualifierKey: TKeyString;
    FEnteredBy: string;
    procedure SetAccuracy(const Value: string);
    procedure SetData(const Value: string);
    procedure SetMeasureTypeKey(const Value: TKeyString);
    procedure SetMeasureUnitKey(const Value: TKeyString);
    procedure SetQualifierKey(const Value: TKeyString);
    procedure SetEnteredBy(const Value: string);
  protected
    procedure InitFromRecord(iDataset : TDataset); override;
    procedure WriteToRecord(iDataset : TDataset); override;
    function GetCell( const iX : integer): string; override;
  public
    property MeasureTypeKey:TKeyString read FMeasureTypeKey write SetMeasureTypeKey;
    property MeasureType:string read FMeasureType write FMeasureType;
    property MeasureUnitKey:TKeyString read FMeasureUnitKey write SetMeasureUnitKey;
    property MeasureUnit:string read FMeasureUnit write FMeasureUnit;
    property QualifierKey: TKeyString read FQualifierKey write SetQualifierKey;  // ES-28/09/01
    property Qualifier:string read FQualifier write FQualifier;
    property Data:string read FData write SetData;
    property Accuracy:string read FAccuracy write SetAccuracy;
    property EnteredBy: string read FEnteredBy write SetEnteredBy;
  end;  // TMeasureItem

  //----------------------------------------------------------------------------
  TMeasureList = class(TGridDataList)
  private
    FdmMeasurements:TdmMeasurements;
    FKeyValue: TKeyString;
    FDataTableName: string;
    FDataTableKeyField: string; // used to store the name of the key field for the data table
    FParentTableKeyField: string; // used to store the name of the key field for the parent table of the data table
    FUserID: TKeyString;
    FNextKey:TGetNextKey;
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure DoModifications; override;
    procedure ProcessUpdate; override;
  public
    constructor Create(iDataset : TDataset; const iKeyFieldName : string;
      iStringGrid: TStringGrid; iItemClass : TItemClass;
      iMeasureDataModule: TdmMeasurements);
    function CheckItem(const iItem:TObject):TMeasureItem;
    property KeyValue:TKeyString read FKeyValue write FKeyValue;
    property DataTableName:string read FDataTableName write FDataTableName;
    property DataTableKeyField:string read FDataTableKeyField write FDataTableKeyField;
    property ParentTableKeyField:string read FParentTableKeyField write FParentTableKeyField;
    property UserID:TKeyString read FUserID write FUserID;
    property NextKey:TGetNextKey read FNextKey write FNextKey;
  end;  // TMeasureList

//==============================================================================
implementation

{$R *.DFM}

uses
  GeneralFunctions;

resourcestring
  ResStr_CannotUpdateRec    = 'Cannot update record.';
  ResStr_MeasureDisplayName = '%s Measurement Data';
  ResStr_Unknown            = 'Unknown';

//==============================================================================
{ TdmMeasurements }
procedure TdmMeasurements.UpdateDatasets(const ADatabase: TADOConnection);
var i : integer;
begin
  FDatabase := ADatabase;
  for i := 0 to ComponentCount-1 do
    { Check for relevant datasets }
    if Components[i] is TCustomADODataset then SetDatabase(TCustomADODataset(Components[i]));
end;  // UpdateDataSets

//------------------------------------------------------------------------------
procedure TdmMeasurements.SetDatabase(const ADataset: TCustomADODataset);
begin
    ADataset.Connection := FDatabase;
end;  // SetDatabase

//==============================================================================
//==============================================================================
{ TMeasureItem }
procedure TMeasureItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FItemKey       := FieldByName('Data_Key').AsString;
      FData          := FieldByName('Data').AsString;
      FQualifier     := FieldByName('Qualifier_Short_Name').AsString;    // ES-28/09/01
      FQualifierKey  := FieldByName('Measurement_Qualifier_Key').AsString;    // ES-28/09/01
      FAccuracy      := FieldByName('Accuracy').AsString;
      FMeasureUnit   := FieldByName('Unit_Short_Name').AsString;
      FMeasureType   := FieldByName('Type_Short_Name').AsString;
      FMeasureUnitKey:= FieldByName('Measurement_Unit_Key').AsString;
      FMeasureTypeKey:= FieldByName('Measurement_Type_Key').AsString;
      FEnteredBy     := FieldByName('Entered_By').AsString;
    except on Exception do
      raise EDataListError.Create(ResStr_BadStructure + 'DATA_KEY, DATA, ' +
                                  'MEASUREMENT_QUALIFIER_KEY, QUALIFIER_SHORT_NAME, ' +
                                  'ACCURACY, UNIT_SHORT_NAME, TYPE_SHORT_NAME, ' +
                                  'MEASUREMENT_UNIT_KEY, MEASUREMENT_TYPE_KEY, ' +
                                  'CHANGED_BY, CHANGED_DATE');
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TMeasureItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      if FAccuracy='' then FAccuracy := ResStr_Unknown;
      FieldByName('Data').AsString                     := FData;
      FieldByName('Measurement_Qualifier_Key').AsString:= FQualifierKey;  // ES-28/09/01
      FieldByName('Accuracy').AsString                 := FAccuracy;
      FieldByName('Measurement_Unit_Key').AsString     := FMeasureUnitKey;
      FieldByName('Changed_By').AsString               := TMeasureList(OwnerList).UserID;
      FieldByName('Changed_Date').AsDateTime           := Now;
    except on Exception do
      raise EDataListError.Create(ResStr_CannotUpdateRec);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
function TMeasureItem.GetCell(const iX: integer): string;
begin
  case iX of
    0 : Result:=MeasureType;
    1 : Result:=Qualifier;
    2 : Result:=Data;
    3 : Result:=MeasureUnit;
    4 : Result:=Accuracy;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TMeasureItem.SetAccuracy(const Value: string);
begin
  FAccuracy := Value;
  SetModified;
end;  // SetAccuracy

//------------------------------------------------------------------------------
procedure TMeasureItem.SetData(const Value: string);
begin
  FData := Value;
  SetModified;
end;  // SetData

//------------------------------------------------------------------------------
procedure TMeasureItem.SetMeasureTypeKey(const Value: TKeyString);
begin
  FMeasureTypeKey := Value;
  SetModified;
end;  // SetMeasureTypeKey

//------------------------------------------------------------------------------
procedure TMeasureItem.SetMeasureUnitKey(const Value: TKeyString);
begin
  FMeasureUnitKey := Value;
  SetModified;
end;  // SetMeasureUnitKey

//------------------------------------------------------------------------------
procedure TMeasureItem.SetQualifierKey(const Value: TKeyString);  // ES-28/09/01
begin
  FQualifierKey := Value;
  SetModified;
end;  // SetQualifierKey

{-------------------------------------------------------------------------------
}
procedure TMeasureItem.SetEnteredBy(const Value: string);
begin
  FEnteredBy := Value;
end;

{-------------------------------------------------------------------------------
}
{-------------------------------------------------------------------------------
}

//==============================================================================
{ TMeasureList }
function TMeasureList.ItemsDisplayName: String;
begin
  Result := Format(
      ResStr_MeasureDisplayName,
      [ReadableFormat(Copy(FDataTableName, 1, Pos('_DATA', FDataTableName) - 1))]);
end;

//------------------------------------------------------------------------------
constructor TMeasureList.Create(iDataset: TDataset; const iKeyFieldName: string;
  iStringGrid: TStringGrid; iItemClass: TItemClass; iMeasureDataModule: TdmMeasurements);
begin
  FdmMeasurements:=iMeasureDataModule;
  inherited Create(iDataSet,iKeyFieldName,iStringGrid,TMeasureItem);
end;  // Create

//------------------------------------------------------------------------------
procedure TMeasureList.DoAdditions;
var lqryAddition: TJnccQuery;
    lNewKey     : TKeyString;
    i           : integer;
    lDataItem   : TMeasureItem;
begin
  if ItemsToAdd.Count > 0 then begin
    lqryAddition := TJnccQuery.Create(nil);
    try
      FdmMeasurements.SetDatabase(lqryAddition);
      with lqryAddition do begin
        SQL.Text := 'SELECT * FROM ' + FDataTableName + ' WHERE ' + FDataTableKeyField + ' = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := TMeasureItem(ItemsToAdd[i]);
          if Assigned(FNextKey) then
            lNewKey := FNextKey(FDataTableName,FDataTableKeyField);
          Append;
          if lDataItem.Accuracy = '' then lDataItem.Accuracy := ResStr_Unknown;
          FieldByName(FDataTableKeyField).AsString          := lNewKey;
          FieldByName(FParentTableKeyField).AsString        := FKeyValue;
          FieldByName('Data').AsString                      := lDataItem.Data;
          FieldByName('Measurement_Qualifier_Key').AsString := lDataItem.QualifierKey;   // ES-28/09/01
          FieldByName('Accuracy').AsString                  := lDataItem.Accuracy;
          FieldByName('Measurement_Unit_Key').AsString      := lDataItem.MeasureUnitKey;
          FieldByName('Entered_By').AsString                := UserID;
          Post;
        end;
        Close;
      end;
    finally
      lqryAddition.Free;
    end;
  end;
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TMeasureList.DoModifications;
var lDataItem       :TMeasureItem;
  //----------------------------------------------------------------------------
  function GetModifiedItem(const AKey:TKeyString):TMeasureItem;
  var iCount:integer;
  begin
    Result:=nil;
    for iCount:=0 to ItemsToModify.Count-1 do
      if TMeasureItem(ItemsToModify.Items[iCount]).ItemKey = AKey then begin
        Result:=TMeasureItem(ItemsToModify.Items[iCount]);
        Exit;
      end;
  end;  // GetModifiedItem
  //----------------------------------------------------------------------------
begin
  if ItemsToModify.Count > 0 then
    with FdmMeasurements.qryUpdate do begin
      if not Active then Open;
      First;
      while not EOF do begin
        lDataItem := GetModifiedItem(FieldByName(FDataTableKeyField).AsString);
        if (lDataItem <> nil) then begin
          Edit;
          lDataItem.WriteToRecord(FdmMeasurements.qryUpdate);
          Post;
        end;
        Next;
      end; // while
      Close;
    end; // with
end;  // DoModifications

//------------------------------------------------------------------------------
procedure TMeasureList.ProcessUpdate;
var qryData:TJnccQuery;
begin
  qryData:=TJnccQuery.Create(nil);
  try
    FdmMeasurements.SetDatabase(qryData);
    DoAdditions;
    DoModifications;
    DeleteFromTable(qryData, FDataTableName, FDataTableKeyField);
  finally
    qryData.Free;
  end;
end;  // ProcessUpdate

//------------------------------------------------------------------------------
function TMeasureList.CheckItem(const iItem:TObject):TMeasureItem;
var lDataItem:TMeasureItem;
begin
  if iItem=nil then begin
    lDataItem:=TMeasureItem.CreateNew(Self);
    AddNew(lDataItem);
  end else
    lDataItem:=TMeasureItem(iItem);
  Result:=lDataItem;
end;  // CheckItem

//==============================================================================
end.
