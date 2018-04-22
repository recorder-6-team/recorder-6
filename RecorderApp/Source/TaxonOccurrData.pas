//==============================================================================
//  Unit:        TaxonOccurrData
//
//  Implements:  TdmTaxonOccurrences
//               TRelOccItem
//               TRelOccList
//               TSpecimenItem
//               TSpecimenList
//               TPrivateDataItem
//               TPrivateDataList

//  Description: Implements data access functionality for the taxon occurrence
//               screen.
//
//               TRelOccItem and TRelOccList
//               Helper classes used on the Related Occurrences tab of the
//               details screen.
//
//               TPrivateDataItem and TPrivateDataList
//               Helper classes used on the External Ref tab of the details screen.
//
//  Author:      Paul Thomas
//  Created:     23 April 1999
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 38 $
//    $Date: 27/12/07 16:10 $
//    $Author: Rickyshrestha $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit TaxonOccurrData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  BaseData, JNCCDatasets, DataClasses, Grids, Constants, ExceptionForm,
  ADODB, DatabaseAccessADO;

type
  ETaxonOccurrenceError= class(TExceptionPath);

  TdmTaxonOccurrences = class(TBaseDataModule)
    qryTaxonOcc: TJNCCQuery;
    dsTaxonOcc: TDataSource;
    qryRecordType: TJNCCQuery;
    qrySubstrate: TJNCCQuery;
    dsRecordType: TDataSource;
    dsSubstrate: TDataSource;
    qryTLIKeyDELTHIS: TJNCCQuery;
    qryRelationType: TJNCCQuery;
    dsRelationType: TDataSource;
    qryRelOcc: TJNCCQuery;
    qrySpecimen: TJNCCQuery;
    qrySpecType: TJNCCQuery;
    dsSpecType: TDataSource;
    qryNameFromOcc: TJNCCQuery;
    qryTaxonPrivateDetail: TJNCCQuery;
    dsPrivateType: TDataSource;
    qryPrivateType: TJNCCQuery;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;
    procedure DeleteRecord(const ATaxOccKey:TKeyString);
    function GetNameFromOcc(const AOccKey: TKeyString): string;
  end;  // TdmTaxonOccurrences

  //============================================================================
  TRelOccItem = class(TGridDataItem)
  private
    FOccurrenceKey:TKeyString;
    FTaxonName    :string;
    FRelTypeKey   :TKeyString;
    FRelType      :string;
    FComment      :TMemoryStream;
    procedure SetRelType(const Value: string);
    procedure SetRelTypeKey(const Value: TKeyString);
    procedure SetOccurrenceKey(const Value: TKeyString);
    procedure SetTaxonName(const Value: string);
  protected
    procedure InitFromRecord( iDataset : TDataset ); override;
    procedure WriteToRecord( iDataset : TDataset ); override;
    function GetCell( const iX : integer ): string; override;
  public
    constructor CreateNew(AOwner:TCacheDataList); override;
    destructor Destroy; override;
    property OccurrenceKey:TKeyString read FOccurrenceKey write SetOccurrenceKey;
    property TaxonName:string read FTaxonName write SetTaxonName;
    property RelTypeKey:TKeyString read FRelTypeKey write SetRelTypeKey;
    property RelType:string read FRelType write SetRelType;
    property Comment:TMemoryStream read FComment;
  end;  // TRelOccItem

  //----------------------------------------------------------------------------
  TRelOccList = class(TGridDataList)
  private
    FOccKey: TKeyString;
    FdmTaxOcc:TdmTaxonOccurrences;
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    constructor Create( iDataSet : TDataSet; const iKeyFieldName : string;
      iStringGrid: TStringGrid; iItemClass : TItemClass; iDataModule:TdmTaxonOccurrences );
    property OccKey:TKeyString read FOccKey write FOccKey;
    property dmTaxOcc:TdmTaxonOccurrences read FdmTaxOcc;
  end;  // TRelOccList

  //============================================================================
  TSpecimenItem = class(TGridDataItem)
  private
    FComment: TMemoryStream;
    FNumber: string;
    FAccessionDate: string;
    FSpecTypeKey: TKeyString;
    FSpecType: string;
    FLocation: string;
    procedure SetNumber(const Value: string);
    procedure SetAccessionDate(const Value: string);
    procedure SetSpecTypeKey(const Value: TKeyString);
    procedure SetSpecType(const Value: string);
    procedure SetLocation(const Value: string);
  protected
    procedure InitFromRecord( iDataset : TDataset ); override;
    procedure WriteToRecord( iDataset : TDataset ); override;
    function GetCell( const iX : integer ): string; override;
  public
    constructor CreateNew(AOwner:TCacheDataList); override;
    destructor Destroy; override;
    property Number:string read FNumber write SetNumber;
    property AccessionDate:string read FAccessionDate write SetAccessionDate;
    property SpecTypeKey:TKeyString read FSpecTypeKey write SetSpecTypeKey;
    property SpecType:string read FSpecType write SetSpecType;
    property Location:string read FLocation write SetLocation;
    property Comment:TMemoryStream read FComment;
  end;  // TSpecimenItem

  //----------------------------------------------------------------------------
  TSpecimenList = class(TGridDataList)
  private
    FOccKey: TKeyString;
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    property OccKey:TKeyString read FOccKey write FOccKey;
  end;  // TSpecimenList

 //============================================================================
  TPrivateDataItem = class(TGridDataItem)
  private
    FPrivateComment: TMemoryStream;
    FPrivateItemName: string;
    FPrivateDate: string;
    FPrivateValue:string;
    FPrivateDetail: string;
    FPrivateTypeKey: TKeyString;
    FPrivateType: string;
    procedure SetPrivateItemName(const Value: string);
    procedure SetPrivateDate(const Value: string);
    procedure SetPrivateValue(const Value: string);
    procedure SetPrivateDetail(const Value: string);
    procedure SetPrivateTypeKey(const Value: TKeyString);
    procedure SetPrivateType(const Value: string);
  protected
    procedure InitFromRecord( iDataset : TDataset ); override;
    procedure WriteToRecord( iDataset : TDataset ); override;
    function GetCell( const iX : integer ): string; override;
  public
    constructor CreateNew(AOwner:TCacheDataList); override;
    destructor Destroy; override;
    property PrivateItemName:string read FPrivateItemName write SetPrivateItemName;
    property PrivateDate:string read FPrivateDate write SetPrivateDate;
    property PrivateValue:string read FPrivateValue write SetPrivateValue;
    property PrivateDetail:string read FPrivateDetail write SetPrivateDetail;
    property PrivateTypeKey:TKeyString read FPrivateTypeKey write SetPrivateTypeKey;
    property PrivateType:string read FPrivateType write SetPrivateType;
    property PrivateComment:TMemoryStream read FPrivateComment;
  end;  // TPrivateDataItem

//----------------------------------------------------------------------------
  TPrivateDataList = class(TGridDataList)
  private
    FOccKey: TKeyString;
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    property OccKey:TKeyString read FOccKey write FOccKey;
  end;  // TPrivateDataList

implementation

{$R *.DFM}

uses
  GeneralData, ApplicationSettings;

//==============================================================================
const

  SQL_TLI_KEY_FOR_OCC = 'SELECT TAXON_LIST_ITEM_KEY '+
                        'FROM TAXON_DETERMINATION '+
                        'WHERE (TAXON_DETERMINATION_KEY = :KeyParameter) '+
                        'AND (PREFERRED = TRUE) ';

  SQL_TAXON_NAME_FROM_OCC =
        'SELECT ITN.Preferred_Name ' +
        'FROM Taxon_Determination TD INNER JOIN Index_Taxon_Name ITN ' +
        '     ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key ' +
        'WHERE TD.Taxon_Occurrence_Key = :KeyParameter ' +
        'AND TD.Preferred = True';

//==============================================================================
constructor TdmTaxonOccurrences.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  qryRecordType.Open;
  qrySubstrate.Open;
end;  // TdmTaxonOccurrences.Create

//------------------------------------------------------------------------------
destructor TdmTaxonOccurrences.Destroy;
begin
  qryRecordType.Close;
  qrySubstrate.Close;
  inherited Destroy;
end;  // TdmTaxonOccurrences.Destroy

//------------------------------------------------------------------------------
function TdmTaxonOccurrences.GetNameFromOcc(const AOccKey:TKeyString):string;
{var
  lTLIKey : TKeyString;}
begin
  Result:='';
{ Un-comment all this code in order to display taxon common names }
(*
  if AppSettings.DisplayTaxonCommonNames then begin
    with qryNameFromOcc do begin
      Close;
      SQL.Clear;
      SQL.Text := 'SELECT TAXON_LIST_ITEM_KEY '+
                  'FROM TAXON_DETERMINATION '+
                  'WHERE (TAXON_OCCURRENCE_KEY = '''+AOccKey+''') '+
                  'AND (PREFERRED = TRUE) ';
      try
        Open;
        First;
        if not EOF then
          lTLIKey := FieldByName('TAXON_LIST_ITEM_KEY').AsString
        else begin
          Result := '';
          Exit;
        end;
      finally
        Close;
      end; // try..finally
    end; // with qryNameFromOcc
    Result := dmGeneralData.GetBestTaxonName(lTLIKey);
  end else begin
*)
  with qryNameFromOcc do begin
    Close;
    SQL.Clear;
    SQL.Text := SQL_TAXON_NAME_FROM_OCC;
    Parameters.ParamByName('KeyParameter').Value:=AOccKey;
    try
      Open;
      First;
      if not EOF then
        Result := FieldByName('Preferred_Name').AsString;
    finally
      Close;
    end; // try..finally
  end // with

end;  // TdmTaxonOccurrences.GetNameFromOcc

//------------------------------------------------------------------------------
procedure TdmTaxonOccurrences.DeleteRecord(const ATaxOccKey: TKeyString);
begin
  with dmGeneralData do begin
    ExecuteSQL('DELETE FROM Taxon_Private_Data WHERE Taxon_Occurrence_Key = '''+ATaxOccKey+'''',
               ResStr_DelFail+' - TAXON_PRIVATE_DATA');
    ExecuteSQL('DELETE FROM Taxon_Occurrence_Relation WHERE Taxon_Occurrence_Key_1 = '''+ATaxOccKey+'''',
               ResStr_DelFail+' - TAXON_OCCURRENCE_RELATION');
    DelSources('Taxon_Occurrence_Sources','Taxon_Occurrence_Key',ATaxOccKey);
    ExecuteSQL('DELETE FROM Taxon_Occurrence_Data WHERE Taxon_Occurrence_Key = '''+ATaxOccKey+'''',
               ResStr_DelFail+' - TAXON_OCCURRENCE_DATA');
    ExecuteSQL('DELETE FROM Taxon_Determination WHERE Taxon_Occurrence_Key = '''+ATaxOccKey+'''',
               ResStr_DelFail+' - TAXON_DETERMINATION');
    ExecuteSQL('DELETE FROM Specimen WHERE Taxon_Occurrence_Key = '''+ATaxOccKey+'''',
               ResStr_DelFail+' - SPECIMEN');
    ExecuteSQL('DELETE FROM Taxon_Occurrence WHERE Taxon_Occurrence_Key = '''+ATaxOccKey+'''',
               ResStr_DelFail+' - TAXON_OCCURRENCE');
  end;
end;  // TdmTaxonOccurrences.DeleteRecord

//==============================================================================
{ TRelOccItem }
constructor TRelOccItem.CreateNew(AOwner: TCacheDataList);
begin
  inherited CreateNew(AOwner);
  FComment:=TMemoryStream.Create;
end;  // TRelOccItem.CreateNew

//------------------------------------------------------------------------------
destructor TRelOccItem.Destroy;
begin
  FComment.Free;
  inherited Destroy;
end;  // TRelOccItem.Destroy

//------------------------------------------------------------------------------
function TRelOccItem.GetCell(const iX: integer): string;
begin
  case iX of
    0 : begin
          if FTaxonName='' then  // Get the taxon name if not already set
            FTaxonName:=TRelOccList(OwnerList).dmTaxOcc.GetNameFromOcc(FOccurrenceKey);
          Result:=FTaxonName;
        end;
    1 : Result:=FRelType;
  end;
end;  // TRelOccItem.GetCell

//------------------------------------------------------------------------------
procedure TRelOccItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FItemKey      :=FieldByName('Taxon_Occurrence_Relation_Key').AsString;
      FOccurrenceKey:=FieldByName('Taxon_Occurrence_Key_2').AsString;
      // FTaxonName is set when displaying it in the associated grid
      FRelTypeKey   :=FieldByName('Relationship_Type_Key').AsString;
      FRelType      :=FieldByName('Short_Name').AsString;
      FComment:=TMemoryStream.Create;
		  TMemoField(FieldByName('Comment')).SaveToStream(FComment);
    except on E:Exception do
      raise ETaxonOccurrenceError.Create(ResStr_InitRecFail+' - TAXON_OCCURRENCE_RELATION',E);
    end;
end;  // TRelOccItem.InitFromRecord

//------------------------------------------------------------------------------
procedure TRelOccItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FieldByName('Taxon_Occurrence_Key_2').AsString:=FOccurrenceKey;
      FieldByName('Relationship_Type_Key').AsString:=FRelTypeKey;
      FComment.Position:= 0;
		  TMemoField(FieldByName('Comment')).LoadFromStream(FComment);
      FieldByName('Changed_By').AsString    :=AppSettings.UserID;
      FieldByName('Changed_Date').AsDateTime:=Now;
    except on E:Exception do
      raise ETaxonOccurrenceError.Create(ResStr_WriteRecFail+' - TAXON_OCCURRENCE_RELATION',E);
    end;
end;  // TRelOccItem.WriteToRecord

//------------------------------------------------------------------------------
procedure TRelOccItem.SetRelType(const Value: string);
begin
  FRelType := Value;
  SetModified;
end;  // TRelOccItem.SetRelType

procedure TRelOccItem.SetRelTypeKey(const Value: TKeyString);
begin
  FRelTypeKey := Value;
  SetModified;
end;  // TRelOccItem.SetRelTypeKey

procedure TRelOccItem.SetOccurrenceKey(const Value: TKeyString);
begin
  FOccurrenceKey := Value;
  SetModified;
end;  // TRelOccItem.SetTaxonKey

procedure TRelOccItem.SetTaxonName(const Value: string);
begin
  FTaxonName := Value;
  SetModified;
end;  // TRelOccItem.SetTaxonName

//==============================================================================
{ TRelOccList }

constructor TRelOccList.Create(iDataSet: TDataSet; const iKeyFieldName: string;
  iStringGrid: TStringGrid; iItemClass: TItemClass; iDataModule: TdmTaxonOccurrences);
begin
  FdmTaxOcc:=iDataModule;
  inherited Create(iDataSet,iKeyFieldName,iStringGrid,iItemClass);
end;  // Create

//------------------------------------------------------------------------------
function TRelOccList.ItemsDisplayName: String;
begin
  Result := ResStr_TaxonOccurenceRelation;
end;

//------------------------------------------------------------------------------
procedure TRelOccList.DoAdditions;
var lqryAddition: TJnccQuery;
    i           : integer;
    lDataItem   : TRelOccItem;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Taxon_Occurrence_Relation WHERE Taxon_Occurrence_Relation_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := TRelOccItem(ItemsToAdd[i]);
          Append;
          FieldByName('Taxon_Occurrence_Relation_Key').AsString :=
              dmGeneralData.GetNextKey('Taxon_Occurrence_Relation', 'Taxon_Occurrence_Relation_Key');
          FieldByName('Taxon_Occurrence_Key_1').AsString        := OccKey;
          FieldByName('Taxon_Occurrence_Key_2').AsString        := lDataItem.OccurrenceKey;
          FieldByName('Relationship_Type_Key').AsString         := lDataItem.RelTypeKey;
          lDataItem.Comment.Position := 0;
          TMemoField(FieldByName('Comment')).LoadFromStream(lDataItem.Comment);
          FieldByName('Entered_By').AsString := AppSettings.UserID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise ETaxonOccurrenceError.Create(Format(ResStr_AddFail, ['TAXON_OCCURRENCE_RELATION']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // TRelOccList.DoAdditions

//------------------------------------------------------------------------------
procedure TRelOccList.ProcessUpdate;
var qryDel:TJnccQuery;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    DeleteFromTable(qryDel,'Taxon_Occurrence_Relation','Taxon_Occurrence_Relation_Key');
  finally
    qryDel.Free;
  end;
end;  // TRelOccList.ProcessUpdate

//==============================================================================
{ TSpecimenItem }

constructor TSpecimenItem.CreateNew(AOwner: TCacheDataList);
begin
  inherited CreateNew(AOwner);
  FComment:=TMemoryStream.Create;
end;  // TSpecimenItem.CreateNew

//------------------------------------------------------------------------------
destructor TSpecimenItem.Destroy;
begin
  FComment.Free;
  inherited Destroy;
end;  // TSpecimenItem.Destroy

//------------------------------------------------------------------------------
function TSpecimenItem.GetCell(const iX: integer): string;
begin
  case iX of
    0 : Result:=FNumber;
    1 : Result:=FSpecType;
  end;
end;  // TSpecimenItem.GetCell

//------------------------------------------------------------------------------
procedure TSpecimenItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FItemKey      :=FieldByName('Specimen_Key').AsString;
      FNumber       :=FieldByName('Number').AsString;
      FAccessionDate:=FieldByName('Accession_Vague_Date_Start').Text;
      FSpecTypeKey  :=FieldByName('Specimen_Type_Key').AsString;
      FSpecType     :=FieldByName('Short_Name').AsString;
      FLocation     :=FieldByName('Location').AsString;
      FComment:=TMemoryStream.Create;
		  TMemoField(FieldByName('Comment')).SaveToStream(FComment);
    except on E:Exception do
      raise ETaxonOccurrenceError.Create(ResStr_InitRecFail+' - SPECIMEN',E);
    end;
end;  // TSpecimenItem.InitFromRecord

//------------------------------------------------------------------------------
procedure TSpecimenItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FieldByName('Number').AsString                :=FNumber;
      FieldByName('Accession_Vague_Date_Start').Text:=AccessionDate;
      FieldByName('Specimen_Type_Key').AsString     :=SpecTypeKey;
      FieldByName('Location').AsString              :=Location;
      FComment.Position:= 0;
		  TMemoField(FieldByName('Comment')).LoadFromStream(FComment);
      FieldByName('Changed_By').AsString    :=AppSettings.UserID;
      FieldByName('Changed_Date').AsDateTime:=Now;
    except on E:Exception do
      raise ETaxonOccurrenceError.Create(ResStr_WriteRecFail+' - SPECIMEN',E);
    end;
end;  // TSpecimenItem.WriteToRecord

//------------------------------------------------------------------------------
procedure TSpecimenItem.SetNumber(const Value: string);
begin
  FNumber := Value;
  SetModified;
end;  // TSpecimenItem.SetNumber

procedure TSpecimenItem.SetAccessionDate(const Value: string);
begin
  FAccessionDate := Value;
  Setmodified;
end;  // TSpecimenItem.SetAccessionDate

procedure TSpecimenItem.SetSpecType(const Value: string);
begin
  FSpecType := Value;
  SetModified;
end;  // TSpecimenItem.SetSpecType

procedure TSpecimenItem.SetSpecTypeKey(const Value: TKeyString);
begin
  FSpecTypeKey := Value;
  SetModified;
end;  // TSpecimenItem.SetSpecTypeKey

procedure TSpecimenItem.SetLocation(const Value: string);
begin
  FLocation := Value;
  SetModified;
end;  // TSpecimenItem.SetSpecTypeKey

//==============================================================================
{ TSpecimenList }
function TSpecimenList.ItemsDisplayName: String;
begin
  Result := ResStr_Specimen;
end;

//------------------------------------------------------------------------------
procedure TSpecimenList.DoAdditions;
var lqryAddition: TJnccQuery;
    i           : integer;
    lDataItem   : TSpecimenItem;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Specimen WHERE Specimen_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := TSpecimenItem(ItemsToAdd[i]);
          Append;
          FieldByName('Specimen_Key').AsString           :=
              dmGeneralData.GetNextKey('Specimen', 'Specimen_Key');
          FieldByName('Taxon_Occurrence_Key').AsString   := OccKey;
          FieldByName('Number').AsString                 := lDataItem.Number;
          FieldByName('Accession_Vague_Date_Start').Text := lDataItem.AccessionDate;
          FieldByName('Specimen_Type_Key').AsString      := lDataItem.SpecTypeKey;
          FieldByName('Location').AsString               := lDataItem.Location;
          lDataItem.Comment.Position:= 0;
          TMemoField(FieldByName('Comment')).LoadFromStream(lDataItem.Comment);
          FieldByName('Entered_By').AsString := AppSettings.UserID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise ETaxonOccurrenceError.Create(Format(ResStr_AddFail, ['SPECIMEN']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // TSpecimenList.DoAdditions

//------------------------------------------------------------------------------
procedure TSpecimenList.ProcessUpdate;
var qryDel:TJnccQuery;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    DeleteFromTable(qryDel,'Specimen','specimen_Key');
  finally
    qryDel.Free;
  end;
end;  // TSpecimenList.ProcessUpdate

//==============================================================================
//==============================================================================
{ TPrivateDataItem }
constructor TPrivateDataItem.CreateNew(AOwner: TCacheDataList);
begin
  inherited CreateNew(AOwner);
  FPrivateComment:=TMemoryStream.Create;
end;  // TPrivateDataItem.CreateNew

//------------------------------------------------------------------------------
destructor TPrivateDataItem.Destroy;
begin
  FPrivateComment.Free;
  inherited Destroy;
end;  // TPrivateDataItem.Destroy

//------------------------------------------------------------------------------
function TPrivateDataItem.GetCell(const iX: integer): string;
begin
  case iX of
    0 : Result:=FPrivateType;
    1 : Result:=FPrivateItemName;
    2 : Result:=FPrivateDetail;

  end;
end;  // TPrivateDataItem.GetCell

//------------------------------------------------------------------------------
procedure TPrivateDataItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FItemKey        :=FieldByName('Taxon_Private_Data_Key').AsString;
      FPrivateItemName    :=FieldByName('Item_Name').AsString;
      FPrivateDetail  :=FieldByName('Detail').Text;
      FPrivateType :=FieldByName('short_name').Text;
      FPrivateTypeKey  :=FieldByName('Taxon_Private_Type_Key').AsString;
      FPrivateDate := FieldByName('Item_Date').AsString;
      FPrivateValue := FieldByName('Item_Value').AsString;
      FPrivateComment:=TMemoryStream.Create;
		  TMemoField(FieldByName('Comment')).SaveToStream(FPrivateComment);
    except on E:Exception do
      raise ETaxonOccurrenceError.Create(ResStr_InitRecFail+' - PRIVATE',E);
    end;
end;  // TPrivateDataItem.InitFromRecord
//------------------------------------------------------------------------------
procedure TPrivateDataItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FieldByName('Item_Name').AsString:=FPrivateItemName;
      FieldByName('Item_Date').AsString:=FPrivateDate;
      FieldByName('Item_Value').AsString:=FPrivateValue;
      FieldByName('Detail').Text:= FPrivateDetail;
      FieldByName('Taxon_Private_Type_Key').AsString     :=FPrivateTypeKey;
      FPrivateComment.Position:= 0;
      TMemoField(FieldByName('Comment')).LoadFromStream(FPrivateComment);
      FieldByName('Changed_By').AsString    :=AppSettings.UserID;
      FieldByName('Changed_Date').AsDateTime:=Now;
    except on E:Exception do
      raise ETaxonOccurrenceError.Create(ResStr_WriteRecFail+' - PRIVATE ',E);
    end;
end;  // TPrivateDataItem.WriteToRecord

//------------------------------------------------------------------------------
procedure TPrivateDataItem.SetPrivateItemName(const Value: string);
begin
  FPrivateItemName := Value;
  SetModified;
end;  // TPrivateDataItem.SetDetail

//------------------------------------------------------------------------------
procedure TPrivateDataItem.SetPrivateDate(const Value: string);
begin
  FPrivateDate := Value;
  SetModified;
end;  // TPrivateDataItem.SetDetail
//------------------------------------------------------------------------------
procedure TPrivateDataItem.SetPrivateValue(const Value: string);
begin
  FPrivateValue := Value;
  SetModified;
end;  // TPrivateDataItem.SetDetail
//------------------------------------------------------------------------------

procedure TPrivateDataItem.SetPrivateDetail (const Value: string);
begin
  FPrivateDetail := Value;
  Setmodified;
end;  // TPrivateDataItem.SetDetail

//==============================================================================
{ TPrivateDataList }
function TPrivateDataList.ItemsDisplayName: String;
begin
  Result := ResStr_TaxonPrivate;
end;

//------------------------------------------------------------------------------
procedure TPrivateDataList.DoAdditions;
var lqryAddition: TJnccQuery;
    i           : integer;
    lDataItem   : TPrivateDataItem;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Taxon_Private_Data WHERE Taxon_Private_Data_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := TPrivateDataItem(ItemsToAdd[i]);
          Append;
          FieldByName('Taxon_Private_Data_Key').AsString           :=
              dmGeneralData.GetNextKey('Taxon_Private_Data', 'Taxon_Private_Data_Key');
          FieldByName('Taxon_Occurrence_Key').AsString   := OccKey;
          FieldByName('Item_Name').AsString        := lDataItem.PrivateItemName;
          FieldByName('Item_Date').AsString        := lDataItem.PrivateDate;
          FieldByName('Item_Value').AsString        := lDataItem.PrivateValue;
          FieldByName('Detail').Text := lDataItem.PrivateDetail;
          FieldByName('Taxon_Private_Type_Key').AsString      := lDataItem.PrivateTypeKey;
          lDataItem.PrivateComment.Position:= 0;
            TMemoField(FieldByName('Comment')).LoadFromStream(lDataItem.PrivateComment);
          FieldByName('Entered_By').AsString := AppSettings.UserID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise ETaxonOccurrenceError.Create(Format(ResStr_AddFail, ['Private']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // TPrivateDataList.DoAdditions

//------------------------------------------------------------------------------
procedure TPrivateDataList.ProcessUpdate;
var qryDel:TJnccQuery;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    DeleteFromTable(qryDel,'Taxon_Private_Data','Taxon_Private_Data_Key');
  finally
    qryDel.Free;
  end;
end;  // TPrivateDataList.ProcessUpdate

//==============================================================================
procedure TPrivateDataItem.SetPrivateType(const Value: string);
begin
  FPrivateType := Value;
  SetModified;
end;  // TPrivateDataItem.SetPrivateType

procedure TPrivateDataItem.SetPrivateTypeKey(const Value: TKeyString);
begin
  FPrivateTypeKey := Value;
  SetModified;
end;  // TPrivateDataItem.SetPrivateTypeKey

end.
